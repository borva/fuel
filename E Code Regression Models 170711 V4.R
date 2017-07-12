########################################################
## (C) 2017 by Boris Vaillant, Quantitative Consulting
## 
## License is granted to to use this code under the MIT License
## (see file License_MIT.txt )
##
## Part E: Some regression models
##
## This part 
## 1) collects the data prepared so far, 
## 2) calculates a number of additional metrics
## 3) loads and cleans daily brent oil prices (including imputation for Saturdays and 
## Sundays, where the price is not quoted)
## 4) runs a number of different models (without using much of a multi-model
## infrastructure)
## 5) cleans, collects and saves the results in "E_ResultDF.rds"
##
## Again, the parts of the code that need more resources (think ~ 200GB RAM, and 5hrs 
## for the splm on daily data 2016) have been commented out, and the results
## made available in "E_ResultDF_KEEP.rds"
##
## TODO: check whether caret package can be useful here

## Clear all -- only if you have saved your other projects
## rm(list=ls(all=TRUE)) 

library(tidyverse)
library(lubridate)
library(stringr) 
library(splines)

## this path setting assumes that the working directory is set to the project folder
PATHDATA = "./E DataIn/"
PATHOUT = "./Z DataOut/"

## load data from previous stages
stationsAll <- readRDS( paste0(PATHOUT, "C_stationsAll_final.rds"))
pricesAgg <- readRDS( paste0(PATHOUT,"D_pricesAgg_30_KEEP.rds"))

## introduce scaled versions for explanatory variables
scale_01 <- function(x) 
  as.integer(1000L * (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))

stationsAll <- stationsAll  %>% ungroup() %>%
  mutate(hotels =  `Geöffnete Beherbergungsbetriebe_Anzahl` / Flaeche,
         stuetze = `Empfänger von sozialen MindSichLeist_Anzahl` / Insgesamt,
         compdens =`all< 02048m`,
         kreisgadm = as.numeric(id)) %>% 
  mutate_at(vars(StatsKKm2, popdens, compdens, hotels),
            funs("Sc" = scale_01))

## load brent and impute weekend prices
brent <- read_csv(paste0(PATHDATA,"/Brent_EUR_Ct.csv")) %>% 
  mutate(brDate = mdy(Date)) %>% 
  arrange(brDate) %>% ## ! separate mutate seems necessary
  mutate(brTimeID = as.integer(difftime(brDate, min(brDate) , units = "days")),
         TimeAfter = lead(brTimeID, 1L),
         TimeAfter = ifelse(is.na(TimeAfter), brTimeID + 1L, TimeAfter),
         ForwardGap = TimeAfter - brTimeID)  # for locf

brentGrid <-  tibble(
  brTimeID = 0:max(brent$brTimeID),
  date = min(brent$brDate) + as.difftime(brTimeID, units = "days"),
  Brent =  as.integer(rep(brent$`Closing Price` / 15.9, brent$ForwardGap)))

saveRDS(brentGrid, paste0(PATHOUT, "E_Brent.rds"))

# --------------------------------------------
## daily time series preparation

dim(pricesAgg)
pricesAgg <- pricesAgg %>% 
  mutate(date = ymd(DatumCounter),
         Wday = as.integer((wday(date) + 5) %% 7 + 1),
         Week = as.integer(isoweek(date)))  %>% 
  left_join(select(stationsAll, StationID, hotels_Sc,  popdens_Sc, compdens_Sc, 
                   StatsKKm2_Sc, brandCl ,lng, lat, isBAB, closeBAB, kreisgadm))  %>%
  left_join(select(brentGrid, date, Brent))
dim(pricesAgg)

summary(pricesAgg)
gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6

## select only 2016
pricesAgg <- pricesAgg %>%  
  filter(DatumCounter > 159999 & DatumCounter < 170000, lng > 5, lat > 5, !is.na(kreisgadm)) %>%
  select(-date, -medDiesel, -medE5)
dim(pricesAgg)

## ---------------------------------------------------------
## regression section

RESULT <- NULL

## simple linear model
mod1 <- lm(medE10 ~ 1 + 
             Brent +
             StatsKKm2_Sc +
             ns(lat, df = 6):ns(lng, df = 6)  + 
             isBAB + closeBAB + brandCl +
             hotels_Sc + 
             popdens_Sc + compdens_Sc +
             factor(Wday),
           data = pricesAgg)

RESULT[[1]] <- broom::tidy(summary(mod1)) %>% mutate(Name = "LM")
rm(mod1)
gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6
RESULT[[1]]

## simple linear model with competitor price
mod2 <- lm(medE10 ~ 1 + 
             Brent +
             StatsKKm2_Sc +
             CompMeanE10 +
             ns(lat, df = 6):ns(lng, df = 6)  + 
             isBAB + closeBAB + brandCl +
             hotels_Sc + 
             popdens_Sc + compdens_Sc +
             factor(Wday),
           data = pricesAgg)

RESULT[[2]] <- broom::tidy(summary(mod2)) %>% mutate(Name = "LM + Comp")
rm(mod2)
gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6
RESULT[[2]]

## simple linear model with competitor price X brand
mod3 <- lm(medE10 ~ 1 + 
             Brent +
             StatsKKm2_Sc +
             CompMeanE10*brandCl +
             ns(lat, df = 6):ns(lng, df = 6)  + 
             isBAB + closeBAB + 
             hotels_Sc + 
             popdens_Sc + compdens_Sc +
             factor(Wday),
           data = pricesAgg)

RESULT[[3]] <- broom::tidy(summary(mod3)) %>% mutate(Name = "LM + Comp x Brand")
rm(mod3)
gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6

## fixed effects for kreise and days
library(lfe)
mod4 <- felm(medE10 ~ 1 + 
               isBAB + closeBAB + brandCl 
             |  factor(DatumCounter) + factor(kreisgadm),
             data = pricesAgg)

temp <- getfe(mod4)
temp$term = rownames(temp)
temp$estimate <- temp$effect

RESULT[[4]] <- bind_rows( broom::tidy(mod4), select(temp, term, estimate)) %>%
  mutate(Name = "FELM")
rm(mod4)
gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6

####################################################

## need no NAs + maintain balanced panel for plm and splm
##
## Note: run on 1/2 year to go through on 8GB laptop
##


pricesAgg <- pricesAgg %>%  
  filter(DatumCounter > 159999 & DatumCounter < 160700) %>%
  group_by(StationID) %>%
  mutate( CT = n(),
          keep = !( any(is.na(medE10)) |
                      any(is.na(popdens_Sc)) |
                      any(is.na(compdens_Sc)) |  
                      any(is.na(hotels_Sc )) |  
                      any(is.na(StatsKKm2_Sc)) |
                      any(is.na(CompMeanE10))) )

## check for "balancedness"
temp <- table(pricesAgg$StationID)
table(temp) 

## some Stations not open on Sundays
## -> keep only stations with 182 open days
## (this needs to be adapted for other selections!)

pricesAgg <- pricesAgg %>%
  ungroup() %>%
  filter(keep , CT == 182 ) %>% 
  mutate(StationID2 = as.integer(as.factor(StationID)),
         Day = as.integer(as.factor(DatumCounter))) 

## Panel balance OK
dim(pricesAgg)
n_distinct(pricesAgg$StationID2)*n_distinct(pricesAgg$DatumCounter)

########################################################################
## panel models
if(FALSE) { ## long runtimes
  
  gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6

  library(plm)

  mod5 <-plm(medE10 ~ 1 + 
             Brent +
             StatsKKm2_Sc +
             brandCl +
             ns(lat, df = 6):ns(lng, df = 6)  + 
             isBAB + closeBAB + 
             hotels_Sc + 
             popdens_Sc + compdens_Sc +
             factor(Wday), 
           data = pricesAgg, index= c("StationID2", "Day"),
           model = "random", effect ="individual")

  RESULT[[5]] <- broom::tidy(mod5) %>% mutate(Name = "PLM Random Indiv 2016H1")
  rm(mod5)
  gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6
  RESULT[[5]]


  mod6 <-plm(medE10 ~ 1 + 
             Brent +
             CompMeanE10 +
             StatsKKm2_Sc +
             brandCl +
             ns(lat, df = 6):ns(lng, df = 6)  + 
             isBAB + closeBAB + 
             hotels_Sc + 
             popdens_Sc + compdens_Sc +
             factor(Wday), 
           data = pricesAgg, index= c("StationID2", "Day"),
           model = "random", effect ="individual")

  RESULT[[6]] <- broom::tidy(mod6) %>% mutate(Name = "PLM Random Indiv + Comp 2016H1")
  rm(mod6)
  gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6

###############################################
## spatial panel methods

  ## only run on a large machine (e.g. AWS or similar)
  ## 12 months of data ~ 5hrs and 200GB RAM
  library(plm)
  library(splm)
  
  ## prepare cometitor matrix
  transDF <- pricesAgg %>% ungroup() %>%
  distinct(StationID, StationID2)

  transDF2 <- transDF
  names(transDF2) <- c("CompID", "CompID2")

  ## re-index for compatibility with spdep
  distDF <- readRDS( "./toAWS/Tempfile C distDF.rds") %>%
   arrange(StationID, distcomp) %>%
   group_by(StationID) %>% 
   slice(1:10) %>%
   mutate(weight = 1/(100+distcomp)/sum(1/(100+distcomp))) %>%
   ungroup() 

  nbdf <- distDF %>% 
    left_join(transDF) %>%
    left_join(transDF2) %>%
    select(StationID2, CompID2, weight) %>%
    filter(!is.na(StationID2), !is.na(CompID2)) %>%
    arrange(StationID2, CompID2) %>%
    group_by(StationID2) %>%
    mutate(weight = weight/sum(weight) )
  
  ## OK: test <- nbdf %>% ungroup() %>% distinct(CompID2) %>% anti_join(nbdf)
  dim(nbdf)

  nbdf <- nbdf %>%
    nest(CompID2, weight) %>%
    as.list() 
  
  mynb <- transpose(nbdf[[2]])
  class(mynb[[1]]) <- c("nb")
  mynblist <- nb2listw(neighbours = mynb[[1]], glist =mynb[[2]],
                       style = "W") 
  
  ## select size of data set here
  pricesAggP <- pdata.frame(
    filter(pricesAgg, DatumCounter > 160700 & DatumCounter < 160800),
    index =  c("StationID2", "Day"))
  
  st <- system.time(
    mod7 <-spgm(medE10 ~ 1 + 
                  Brent +
                  StatsKKm2_Sc +
                  brandCl +
                  ns(lat, df = 6):ns(lng, df = 6)  + 
                  isBAB + closeBAB + 
                  hotels_Sc + 
                  popdens_Sc + compdens_Sc +
                  factor(Wday), 
                data = pricesAggP, 
                index= c("StationID2", "Day"),
                listw =mynblist,
                model = "random", 
                lag = TRUE, 
                spatial.error = FALSE))
  st

  RESULT[[7]] <- broom::tidy(summary(mod7)$Coef) %>% 
    mutate(Name = "SPLM Random Indiv + Lag - SErr")

  rm(mod7)
  gc(); sort( sapply(ls(),function(x){object.size(get(x))}))/10^6
  
}

resultDF <- bind_rows(RESULT)
saveRDS(resultDF, paste0(PATHOUT, "E_ResultDF.rds")) 
## _KEEP version contains models for full 2016

