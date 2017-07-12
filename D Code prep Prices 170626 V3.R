########################################################
## (C) 2017 by Boris Vaillant, Quantitative Consulting
## 
## License is granted to to use this code under the MIT License
## (see file License_MIT.txt )
##
## Part D: Loading and preparing the price data
##
## This code processes the original price data from www.tankerkoenig.de
## The data is assumed to be accessible via a Postgres connection. The earlier
## dataset from "tankerkoenig" is assumed to be in a db called benzin_2015, the later 
## dataset in a db called benzin_2016. You can bypass using Postgres by setting
## the variable IhavePostgres = FALSE. In this case, data from a sample of 1000 stations,
## provided in "D DataIn" is used.
##
## In a first step, regular time series are produced for batches of 
## stations. We take some pains to make this first regular series "rectangular",
## i.e. there will be an entry, possibly NA, for every TimeID X StationID
## In a second step, we calculate average conmpetitor prices,
## using the "distance data frame" from Code Part C.
##
## TODO: The full procedure may run 5-10 hours on an average laptop
## It should be possible to speed this up considerably, by
## a) optimising the Postgres query extracting the station batches
## b) using sparse matrix methods from the "Matrix" package for the calculation
## of competitor prices
##
## Two datasets are produced
## 1) D_pricesAgg_XX.rds containing daily data for 2016
## 2) D_pricesGridMay_XX.rds containing 10min data for May 2016
##
## For the time being, the later parts (E, F, G) of the program will look for 
## the daily data in  D_pricesAgg_30_KEEP.rds, which is already provided in "Z DataOut"

## Clear all -- only if you have saved your other projects
## rm(list=ls(all=TRUE)) 

library(tidyverse)
library(lubridate)
library(stringr) 

## check whether the cleaning parameters for prices make sense:
fillPricesNA <- function(x) ifelse(is.na(x) | x < 700 | x > 3500, NA, x)

## this path setting assumes that the working directory is set to the project folder
PATHDATA = "./D DataIn/"
PATHOUT = "./Z DataOut/"

## --------------------------------------------------
## load station master and opening hours
stationsAll <- readRDS( paste0(PATHOUT, "C_stationsAll_final.rds"))
stationsHours <-  readRDS( paste0(PATHOUT, "B StationsOpeningTimes.rds")) 

## the following piece needs to know whether you have installed the 2 price dbs from
## www.tankerkoenig.de  as benzin_2015 and benzin_2016 in Postgres
## (if FALSE, the program will fall back to the prepared rds-files in ./DataIn/)
## Leaving all the parameters as pre-defined will run through with the fallback 
IhavePostgres = FALSE

## Chunking parameter
NSize = 200 # (best < 300 for a 8GB Laptop)
set.seed(1234)
if(IhavePostgres) {
  statInclude <-  stationsAll$stid 
  } else {
    statInclude <- sample(stationsAll$stid, 1000, replace = FALSE )
  }

loadConfig <- tibble(
  stid = statInclude,
  Chunk = as.integer(0:(length(statInclude)-1)/ NSize)) %>% 
  group_by(Chunk) %>%
  nest()

NConfig <- nrow(loadConfig)
## -------------------------------------------------------------
## setting up grid of times that will have to be imputed

## Imputation parameter
resImpute = 10 # minutes; grid for initial imputation 

## time interval for the data
## keep these times *fixed* (filter wanted times below)
startdate = ymd_hms("2014-06-08 09:00:00", tz = "Europe/Berlin")
enddate = ymd_hms("2017-01-01 01:00:00", tz = "Europe/Berlin")

## time interval for the buffers
stoptimeend = enddate +  as.difftime(resImpute, units = "mins") 
stoptimestart = startdate -  as.difftime(resImpute, units = "mins") 

## times template 
prtimes <-  tibble(
  TimeID = 0:as.integer(difftime(stoptimeend, stoptimestart , units = "mins")/resImpute),
  TimeInterval = stoptimestart + as.difftime(TimeID * 10, units = "mins"),
  TimeID2 = as.integer(difftime(TimeInterval, stoptimestart , units = "mins")/resImpute), # fits
  Wday = as.integer((wday(TimeInterval) + 5) %% 7 + 1 ),
  WdayNam = wday(TimeInterval, label = TRUE), 
  Week = as.integer(isoweek(TimeInterval)),
  ## e.g. Hour = 15 means, tick came in between 15.00h and 16.00h
  Hour = hour(TimeInterval),
  StundeCounter = Hour + 1L, ## Logic used by the traffic counter data
  DatumCounter = as.integer((year(TimeInterval) - 2000) * 10000L +  
                              month(TimeInterval) * 100L + day(TimeInterval)),
  Minute = minute(TimeInterval),
  MinuteInDay = as.integer(Hour * 60L + Minute),
  One = 1L
)

## ------------------------------------------
## setup load from Postgres

if(IhavePostgres) {
  # Connect to local PostgreSQL via dplyr
  # db from https://creativecommons.tankerkoenig.de/#history
  localdba <- src_postgres(dbname = 'benzin_2015',
                          host = 'localhost',
                          port = 5432,
                          user = 'b1',
                          password = 'Dgr1wäst55')

  # Connect to local PostgreSQL via dplyr
  # db from https://creativecommons.tankerkoenig.de/#history
  localdbb <- src_postgres(dbname = 'benzin_2016',
                           host = 'localhost',
                           port = 5432,
                           user = 'b1',
                           password = 'Dgr1wäst55')
}

st = Sys.time()
nchunk = 2

for(nchunk in 1:NConfig) {
  
  keepstations <- unlist(loadConfig$data[[nchunk]])
  cat("Chunk ", nchunk, " started at ", difftime(Sys.time(),st) , " ")

  if(IhavePostgres) {
    pricesA <- tbl(localdba, "gas_station_information_history") %>%
      select(stid, date , e10,  e5, diesel, changed) %>%
      filter(stid %in% keepstations) %>%
      collect(n = Inf) %>% 
      mutate(source = 1L)
    
    pricesB <- tbl(localdbb, "gas_station_information_history") %>%
      select(stid, date , e10, e5, diesel, changed) %>%
      filter(stid %in% keepstations) %>%
      collect(n = Inf) %>% 
      mutate(source = 2L)
    
    } else 
    { ## fallback
    if(all(file.exists(c(paste0(PATHDATA, "pricesA_Sample_Chk_", nchunk, ".rds"),
                          paste0(PATHDATA, "pricesB_Sample_Chk_", nchunk, ".rds")))))
      {
      pricesA <- readRDS(paste0(PATHDATA, "pricesA_Sample_Chk_", nchunk, ".rds")) 
      pricesB <- readRDS(paste0(PATHDATA, "pricesB_Sample_Chk_", nchunk, ".rds")) 
    } else next
  }

  pricesA <- pricesA %>% mutate(source = 1L)
  pricesB <- pricesB %>% mutate(source = 2L)
  prices <- bind_rows(pricesA, pricesB) 
 
  # Example data for the presentation
  if(nchunk == 1) saveRDS(pricesA,  paste0(PATHOUT, "D_OrigPrice_Example.rds"))
  
  ## prices in A cannot be later than prices in B 
  prices <- prices  %>% 
    group_by(stid) %>%
    mutate(MinDateB = min(date + as.difftime(10000 * (source == 1), units = "days")) ) %>%
    filter(!(source == 1 & date >= MinDateB))
  
  rm(pricesA, pricesB)
  gc()

  ## stoppers are added at start and end, to ensure
  ## that all prices are imputed (possibly as NA) over the same period
  ## of time 
  keepDF <- loadConfig$data[[nchunk]]
  endstopper <- keepDF %>% 
    mutate(date = stoptimeend,
           changed = -999L)
  startstopper <- keepDF %>% 
    mutate(date = stoptimestart,
           changed = -999L)
  
  prices <- 
    bind_rows(startstopper, 
              filter(prices, date >= startdate, date <= enddate),
              endstopper) %>%
    mutate(date = with_tz(date, tz = "Europe/Berlin")) 
  
  ## keep only stations with info in stationsAll
  ## create TimeID as in the prtimes table
  prices <- prices %>% 
    left_join(select(stationsAll, stid, StationID, brandClInt)) %>%
    filter(!is.na(StationID)) %>%
    mutate( TimeInterval = floor_date(date, paste0(resImpute, "mins")),
            TimeID = as.integer(difftime(TimeInterval, stoptimestart, units = "mins") / resImpute)) 

  ## if there are multiple entries for a TimeID only keep the latest price
  prices <- prices %>%
    ungroup() %>% 
    arrange(StationID, TimeID, desc(date))%>% 
    group_by(StationID, TimeID) %>%  
    slice(1) ## work around for recent top_n problem

  # keychecks etc  - not used in production
  # dim(prices); prices %>% group_by(StationID, TimeID) %>% summarise(n()) %>% dim()
  # check that stoppers are well separated in time
  # table(pmax(max(prices$TimeID)-3, prices$TimeID))
  # table(pmin(min(prices$TimeID)+3, prices$TimeID))

  ## stations template
  prstations <- prices %>% 
    group_by(StationID, brandClInt) %>%
    summarise(minTime = min(TimeID),
              maxTime = max(TimeID)) %>% 
    mutate(One = 1L)
  
  ## identify gap length between price ticks. Rules:
  ## clean prices a bit
  prices <- prices %>% ungroup() %>%
    mutate_at(vars(e10, e5, diesel), funs(fillPricesNA)) %>%
    arrange(StationID, TimeID) %>%  
    ## should not be necessary,  but keep for safety 
    group_by(StationID) %>%
    mutate(
      TimeAfter = lead(TimeID, 1L),
      TimeAfter = ifelse(is.na(TimeAfter), TimeID + 1L, TimeAfter),
      ForwardGap = TimeAfter - TimeID) # for locf
      # TimeBefore = lag(TimeID, 1L),
      # TimeBefore = ifelse(is.na(  TimeBefore), TimeID - 1L, TimeBefore),
      #  BackGap = TimeID - TimeBefore, # for nocb)
    
  ## Create template of expected data by building the square (!) ~ 5sec 
  pricesGrid <-
    select(prstations, StationID, One, brandClInt) %>%
    left_join(select(prtimes, TimeID, One, DatumCounter, StundeCounter, Wday, MinuteInDay)) %>%
    ungroup() %>%
    select(-One)

  ## dim(pricesGrid); dim(prstations)*dim(prtimes) # should be exact rectangle
  ## get memory status # gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6
  
  ## fill template with locf (or nocb if needed)
  pricesGrid <- pricesGrid %>%
    mutate(
      lastE10 = rep(prices$e10, prices$ForwardGap),
      lastTimeID = rep(prices$TimeID, prices$ForwardGap),
      lastE5 = rep(prices$e5, prices$ForwardGap),
      lastDiesel = rep(prices$diesel, prices$ForwardGap),
      #  nextChanged = rep(prices$changed, prices$BackGap), ## example for nocb
      lastGapLength =  rep(prices$ForwardGap, prices$ForwardGap)
    )

  ## get memory status # gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6

  ## add info on opening hours (~ 5 Sec)
  dim(pricesGrid)
  pricesGrid <- pricesGrid %>% 
    left_join(select(stationsHours, StationID, Wday = weekdayX , 
                     startInt1, endInt1, startInt2, endInt2 )) 
  dim(pricesGrid) # should not blow up 
  
  pricesGrid <- pricesGrid %>% 
      mutate(
        open=(!is.na(startInt1) & 
                ((startInt1 <= MinuteInDay & endInt1 >= MinuteInDay) |
                   (startInt2 <= MinuteInDay & endInt2 >= MinuteInDay))) ) %>%
      select(-startInt1, -startInt2, -endInt1, -endInt2) 
  
  saveRDS(filter(pricesGrid, DatumCounter >= 160501, DatumCounter <= 160528),
          paste0(PATHOUT, "Tempfiles/Temp_D_pricesGridMay16_Chk_", nchunk, "_of_", NConfig,".rds"))
  
  # Example data for the presentation
  if(nchunk == 1) 
    saveRDS(filter(pricesGrid, DatumCounter >= 1605011, DatumCounter <= 160514),  
            paste0(PATHOUT, "D_GridPrice_Example.rds"))
  
  ## Calculation of aggregate for further study (~ 1min)
  ## exclude night hours for the aggregated data 
  pricesAgg <- pricesGrid %>% 
    filter(StundeCounter >= 5, StundeCounter <= 21, open) %>%
    group_by(StationID, DatumCounter) %>%
    summarise(medDiesel = as.integer(median(lastDiesel, na.rm = TRUE)),
              medE5 = as.integer(median(lastE5, na.rm = TRUE)),
              medE10 = as.integer(median(lastE10, na.rm = TRUE))) %>%
    ungroup()
  
  saveRDS(pricesAgg, paste0(PATHOUT,"Tempfiles/Temp_D_pricesAgg_Chk_", nchunk, "_of_", NConfig,".rds"))
  
  rm(pricesGrid, pricesAgg)  
  gc()
}  

## ------------------------------------------------------
## reconstruction of data sets and calculation of competition prices
## Note: competitor prices are only realistic if we work with the full 
## set of stations. Running this with the sample is only a "demo"

## ------------------------------------------------------
## A) (daily) aggregated data set
## (code for the 10min data is similar and not provided here)

## reconstruct complete aggregate data set
pricesAgg <- 
  tibble(filPath = paste0(PATHOUT,"Tempfiles/Temp_D_pricesAgg_Chk_", 
                          1:NConfig, "_of_", NConfig, ".rds")) %>%
  mutate(filLoad = map(filPath, readRDS)) %>%
  select(-filPath) %>%
  unnest()

## keycheck etc - this is a "rectangle"
dim(pricesAgg)
n_distinct(pricesAgg$StationID) * n_distinct(pricesAgg$DatumCounter)
summary(pricesAgg)

## calculate weights from distance matrix
distDF <- readRDS(paste0(PATHOUT, "C_distDF.rds")) %>%
  ungroup() %>%
  arrange(StationID, distcomp) %>%
  group_by(StationID) %>% 
  slice(1:10) %>% 
  mutate(weight = 1 / (100 + distcomp) / sum(1 / (100 + distcomp))) %>%
  ungroup() 

## get memory status # 
gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6

## use chunking (for distDF this time) as before:
## Chunking parameter
NSizeComp = 200
statIncludeComp <-   stationsAll %>% filter(stid %in% statInclude) %>% 
  select(StationID) %>% unlist()

CompConfig <- tibble(
  StationID = statIncludeComp,
  Chunk = as.integer(0:(length(statIncludeComp) - 1) / NSizeComp)) %>% 
  group_by(Chunk) %>%
  nest()

NConfigComp <- nrow(CompConfig)

st = Sys.time()
nchunk = 2

for(nchunk in 1:NConfigComp) {
  
  cat("Chunk ", nchunk, " started at ", difftime(Sys.time(),st) , " ")
  
  keepstations <- unlist(CompConfig$data[[nchunk]])
  distDFComp <- distDF %>% filter(StationID %in% keepstations)
  
  ## average competitor prices on aggregate basis
  compAgg <- distDFComp %>% 
    select( distcomp, StationID, CompID, weight) %>% 
    inner_join(select(pricesAgg, 
                     CompID = StationID, DatumCounter, medE5, medE10, medDiesel)) %>%
    group_by(StationID, DatumCounter) %>%
    summarise(CompMeanE5 = as.integer(sum(medE5 * weight, na.rm=TRUE) / 
                                        sum(weight * (!is.na(medE5)), na.rm=TRUE)),
              CompMeanE10 = as.integer(sum(medE10 * weight, na.rm=TRUE) / 
                                         sum(weight * (!is.na(medE10)), na.rm=TRUE)),
              CompMeanDiesel = as.integer(sum(medDiesel * weight, na.rm=TRUE) / 
                                            sum(weight * (!is.na(medDiesel)), na.rm=TRUE)))
  
  saveRDS(compAgg, paste0(PATHOUT,"Tempfiles/Temp_D_compAgg_Chk_",
                          nchunk, "_of_", NConfigComp,".rds"))

  rm(compAgg)  
  gc()
}

## reconstruct complete compAgg
compAgg <- 
  tibble(filPath = paste0(PATHOUT,"Tempfiles/Temp_D_compAgg_Chk_",
                          1:NConfigComp, "_of_", NConfigComp, ".rds")) %>%
  mutate(filLoad = map(filPath, readRDS)) %>%
  select(-filPath) %>%
  unnest() %>%
  mutate_at(vars(CompMeanE5, CompMeanE10, CompMeanDiesel), funs(fillPricesNA))

dim(compAgg)

dim(pricesAgg)
pricesAgg <- pricesAgg %>% left_join(compAgg)
dim(pricesAgg)

saveRDS(pricesAgg, paste0(PATHOUT,"D_pricesAgg_", NConfigComp,".rds"))

# rm(compAgg, pricesAgg)
## get memory status 
gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6
