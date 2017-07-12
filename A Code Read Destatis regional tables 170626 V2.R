########################################################
## (C) 2017 by Boris Vaillant, Quantitative Consulting
## 
## License is granted to to use this code under the MIT License
## (see file License_MIT.txt )
##
## Part A: Automatic process to aggregate data from the Destatis Regional database
##
## All original data files (folder "A DataIn") are from
## https://www.regionalstatistik.de/genesis/online/
## and are free to use when citing the source
##
## This code takes the csv-files in folder A_DataIn and consolidates
## them in a single file A_destatis_KreisData.rds.
## It then reads the "Gemeindeverzeichnis", extracts the lowest hierarchy
## and writes a cleaned version to A_destatis_GV.rds
## Outputs are written to folder "Z DataOut"



## Clear all -- only if you have saved your other projects
## rm(list=ls(all=TRUE)) 

library(tidyverse)
library(stringr) ## should be included in tidyverse, but doesn't seem to be ...

## this path setting assumes that the working directory is set to the project folder
PATHDATA = "./A DataIn/"
PATHOUT = "./Z DataOut/"

## list and store csv-files in working directory (including full path)
tablesToLoad <- tibble(fileFullPath =grep(".csv", sort(dir(PATHDATA, full.names = TRUE )), value = TRUE),
                       fileName = str_extract(fileFullPath,"\\d+[^/]+\\.csv"))

tablesToLoad

## first investigation - read in data as (shortened) lines of text
collectData <- tablesToLoad %>% 
  mutate(textLines =
           map(fileFullPath, readLines, encoding ="latin1") %>%  
           map(substr,  start= 1, stop= 100)
         ) 

## have a look at the beginnings and ends of each table
collectData %>% .$textLines %>% map(head, 10) ## -> data starts after ";;" lines
collectData %>% .$textLines %>% map(tail, 10) ## -> data stops at "___" line

## extract ";" or "_ positions for each table
collectData  <- collectData  %>% 
  mutate( indexStart = map(textLines, ~grep("^;",. )), ## indexes the lines starting with ;)
          posStart =  map_int(indexStart, max),
          posEnd = map(textLines, ~grep("^_",. )) %>% map_int(min)-posStart-1L)

## load header only and construct theoretical column "Long" names
## from the text info above the data
collectData   <- collectData  %>% 
  mutate( header = pmap( list(file=fileFullPath,   n_max =posStart-1),  
                        read_csv2, locale = locale(encoding = "latin1"), 
                        na ="NA", skip=1, col_names =FALSE),
          LongNames = map(header, . %>% map(paste0, collapse="_")) %>% 
            map(unlist) %>% map(as.vector)) ## CHECK for purrr versions

origColNames <- collectData %>% .$LongNames
names(origColNames) <- collectData$fileName

## look at the proposed long names
dump("origColNames","")
 
## then edit the resulting text (possibly with the help of Ctrl+Shift+A 
## to reformat the result in RStudio) 

## and create code like the following for MyNames:
collectData$MyNames <-  list(
  `173-01-4.csv` = c(
    "CharDate",
    "KreisCode","KreisName",
    "Flaeche"
  ),
  `173-01-4.csv` = c(
    "KreisCode","KreisName",
    "e2015_Insgesamt",
    "e2015_männlich",
    "e2015_weiblich",
    "e2014_Insgesamt",
    "e2014_männlich",
    "e2014_weiblich"
  ),
    `368-01-4.csv` = c(
      "CharDate", "KreisCode", "KreisName",
      "Lohn- und Einkommensteuerpflichtige_Anzahl",
      "Gesamtbetrag der Einkünfte_Tsd. EUR",
      "Lohn- und Einkommensteuer_Tsd. EUR"
    ),
    `400-51-4-B.csv` = c(
      "CharDate", "KreisCode", "KreisName", "MetricName_1", "MetricUnit_1", 
      "Baulandverkäufe_Insgesamt",
      "Baulandverkäufe_baureifes Land"
    ),
    `426-71-4.csv` = c(
      "CharDate", "KreisCode", "KreisName",
      "BIP_Tsd. EUR",
      "BIP je Erwerbstätigen_EUR",
      "BIP je Einwohner_EUR",
      "BWS Insgesamt_Tsd. EUR",
      "BWS Land- und Forstwirtschaft, Fischerei (A)_Tsd. EUR",
      "BWS Produzierendes Gewerbe ohne Baugewerbe (B-E)_Tsd. EUR",
      "BWS Verarbeitendes Gewerbe (C)_Tsd. EUR",
      "BWS Baugewerbe (F)_Tsd. EUR",
      "BWS Handel,Verkehr,Gastgewerbe,Informa-/Kommunikation_Tsd. EUR",
      "BWS Fin-,Vers.-,Unt.-dienstl.,Grundst.-/Wohnungswesen_Tsd. EUR",
      "BWS öffentl. u. sonst. Dienstl.,Erziehung, Gesundheit_Tsd. EUR"
    ),
    `469-11-4.csv` = c(
      "CharDate", "KreisCode", "KreisName",
      "Geöffnete Beherbergungsbetriebe_Anzahl",
      "Angebotene Gästebetten_Anzahl",
      "Gästeübernachtungen_Anzahl",
      "Gästeankünfte_Anzahl"
    ),
    `641-41-4.csv` = c(
      "CharDate", "KreisCode", "KreisName",
      "KFZ_Insgesamt",
      "KFZ_Pkw",
      "KFZ_Lkw",
      "KFZ_Zugmaschinen",
      "KFZ_Krafträder"
    ),
    `661-31-4.csv` = c(
      "CharDate", "KreisCode", "KreisName",
      "Empfänger von sozialen MindSichLeist_Anzahl",
      "Davon_ALG II_zusammen_Anzahl",
      "Davon_ALG II_erwerbsf. Leistungsb._Anzahl",
      "Davon_ALG II_nicht erwerbsf. Leistungsb. (Sozialgeld)_Anzahl",
      "Davon_Hilfe z. Lebensunt. außerh. v. Einr. n. d. SGB XII__Anzahl",
      "Davon_Grundsicher. im Alter u.b. Erwerbsm. n. d. SGB XII__Anzahl",
      "Davon_Regelleist. n.d. Asylbewerber LG__Anzahl"
    ),
    `666-51-4.csv` = c(
      "CharDate", "KreisCode", "KreisName",
      "verfügbares Einkommen der privaten Haushalte_Tsd. EUR",
      "verfüg. Einkommen der priv. Haushalte je Einwohner_EUR"
    )
  )

## make first load of full data with these names ....
collectData   <- collectData  %>% 
  mutate(firstLoad = pmap( list(file=fileFullPath, skip=posStart, col_names=MyNames, n_max =posEnd),  
                        read_csv2,  
                        na = c("", "NA", "-"), 
                        locale = locale(encoding = "latin1",
                                        decimal_mark = ",", 
                                        grouping_mark = ".")),
         firstSpecs = firstLoad %>% map(spec))

## save specs in order to further adjust column definitions
origSpecs <- collectData  %>% .$firstSpecs
names(origSpecs) <- collectData$fileName

origSpecs

## better not use dump in this case, but edit the output of 
## origSpecs directly to create code like the following for MyTypes
collectData$MyTypes <-list(
  `171-01-4.csv` =cols(
    .default = col_number(), 
    CharDate = col_character(),
    KreisCode = col_character(),## due to leading 0 etc this *must* be character
    KreisName = col_character() 
  ),
  `173-01-4.csv` =cols(
    .default = col_number(), 
    KreisCode = col_character(),## due to leading 0 etc this *must* be character
    KreisName = col_character() 
  ),
`368-01-4.csv` = cols(
  .default = col_number(), 
  CharDate = col_character(), ## as there are many variations of Date, this is character, too
  KreisCode = col_character(),
  KreisName = col_character()
),
`400-51-4-B.csv` =cols(
  .default = col_number(), 
  CharDate = col_character(),
  KreisCode = col_character(),
  KreisName = col_character(),
  MetricName_1 = col_character(),
  MetricUnit_1 = col_character()
),
`426-71-4.csv` =cols(
  .default = col_number(), 
  CharDate = col_character(),
  KreisCode = col_character(),
  KreisName = col_character()
),
`469-11-4.csv` = cols(
  .default = col_number(), 
  CharDate = col_character(),
  KreisCode = col_character(),
  KreisName = col_character()
),
`641-41-4.csv`= cols(
  .default = col_number(), 
  CharDate = col_character(),
  KreisCode = col_character(),
  KreisName = col_character()
),
`661-31-4.csv`= cols(
  .default = col_number(), 
  CharDate = col_character(),
  KreisCode = col_character(),
  KreisName = col_character()
),
`666-51-4.csv`= cols(
  .default = col_number(), 
  CharDate = col_character(),
  KreisCode = col_character(),
  KreisName = col_character()
))

## now load final full data with these specs  ....
## to reduce problem reports, we identify the different NA characters
collectData   <- collectData  %>% 
  mutate(finalLoad = 
           pmap( list(file = fileFullPath, 
                      col_types = MyTypes, col_names = MyNames,
                      skip = posStart, n_max = posEnd),  
                 read_csv2, 
                 na = c("", "NA", "-", ".", "...", "x"), 
                 locale = locale(encoding = "latin1",
                                 decimal_mark = ",",  
                                 grouping_mark = ".")))

## check data load
collectData
collectData  %>% .$finalLoad %>% map(head,5) ## starts with Flensburg or Deutschland
collectData  %>% .$finalLoad %>% map(tail,5) ## ends with Altenburger Land
collectData  %>% .$finalLoad %>% map(problems) ## no remaining problems

## keep and arrange the relevant data by binding all rows together
dat <- collectData %>% 
  select(fileName, finalLoad)  %>% 
  unnest() 

## this effectively creates all columns for all data sets
dim(dat)

## but we identify common columns and move to long format right away:
## (note special format of `400-51-4-B.csv` requires special treatment of
## "MetricName_1","MetricUnit_1")

## key for making a long data frame 
keepKey <- c("CharDate", "KreisCode", "KreisName", "MetricName_1","MetricUnit_1")  

repNA <- function(x) ifelse(is.na(x), "", x)
dat <- dat %>% 
  mutate( MetricName_1 = repNA(MetricName_1),
          MetricUnit_1 = repNA(MetricUnit_1)) %>% 
  ## move to long format
  gather( key = MetricName_2, value = Value_2,
          -one_of(c("fileName", keepKey)), na.rm = TRUE)

## create clean Year and MetricName columns 
## applies a number of rules specific to this data only
## also, I selected several years in the original data pulls
## and will now restrict to the most recent one
dat <- dat %>%
  mutate( Year = as.integer(ifelse(!is.na(CharDate), 
                                   str_extract(CharDate, "201\\d{1}"),                         
                                   str_extract(MetricName_2, "201\\d{1}"))),
          MetricName_2 = str_replace(MetricName_2, "e201\\d{1}_", ""),
          MetricName = str_trim(paste(MetricName_1, MetricUnit_1, MetricName_2), side = "both")) %>%
  group_by(fileName) %>% 
  filter(is.na(Year) | Year == max(Year)) %>% 
  ungroup() 

## need a dirty operation for Hamburg (02 -> 02000) and Berlin (11 -> 11000)
## it has proved useful to keep both versions in the data
dat <- dat %>% mutate(KreisCode = str_replace(KreisCode,fixed("000"),"")) ## no 000

adddat <- dat %>% filter(nchar(dat$KreisCode) == 2) %>%
  mutate(KreisCode = paste0(KreisCode,"000")) ## only 000
dat <- bind_rows(dat, adddat) 

## KreisCode (~ nuts3)  should have a unique name
# also -- this is detail you may ignore if you are using this code for your own purposes --
## we need to keep an alternative version of codes 
## due to the recent merger of Göttingen 03152 and Osterrode 03156 into 03159

dat <-dat %>%  
  arrange(KreisCode, KreisName) %>%
  group_by(KreisCode) %>%
  mutate(KreisName = first(KreisName),
         KreisCode2GV = str_replace(KreisCode,fixed("000"),""),
         KreisCode2GV = ifelse(KreisCode2GV %in% c("03152","03156"), "03159", KreisCode2GV))

## now move to the cleaned wide version
datWide <- dat %>% select(KreisCode, KreisCode2GV, KreisName, MetricName, Value_2) %>%
  spread(key = MetricName, value = Value_2)  %>%
  ungroup()

## check that KreisCode is indeed key
dim(datWide)
datWide %>% summarise(n_distinct(KreisCode))
table(nchar(datWide$KreisCode)) ## 5 are the Kreise


## save as tsv (for use outside of R) and as rds
## to continue the workshop
write_tsv(datWide, paste0(PATHOUT,"A_destatis_KreisData.tsv"))
saveRDS(datWide, paste0(PATHOUT,"A_destatis_KreisData.rds"))

## -----------------------------------------------------------
## Read Gemeindeverzeichnis

## this data set provides information about surface area and population
## at a much more granular level

library(readxl)  

gv <- read_excel( ## works amazingly well, also in Linux
  paste0(PATHDATA,"AuszugGV1QAktuell.xlsx"), sheet = 2, 
  col_names = c("SatzArt",	"TextKennz",
                "Land", "RB", "Kreis", "VB", "Gem", "GemeindeName", "Fläche", "Stand", 
                "insgesamt","männlich", "weiblich", "je km2", "PLZ", "Long", "Lat", 
                "ReiseGebCode", "ReiseGebTxt", "BesiedlungCode", "BesiedlungTxt"),
  na = "", skip = 6) 

## this is a multi-level data set. We keep only the
## most granular level
## also, col_types in readxl do not yet work 100% as expected 
## we need separate parsing:
gv <- gv %>% 
  mutate_if(is.character, repNA) %>%
  filter(GemeindeName!="" , SatzArt == 60) %>%
  mutate(Long = parse_number(Long, locale= locale(decimal_mark = ",")),
         Lat = parse_number(Lat, locale= locale(decimal_mark = ",")),
         KreisCodeGV = paste0(Land, RB, Kreis, VB, Gem))


## -----------------------------------------------------------
## save as tsv (for use outside of R) 
## and as rds to continue the workshop
write_tsv(gv, paste0(PATHOUT,"A_destatis_GV.tsv"))
saveRDS(gv, paste0(PATHOUT,"A_destatis_GV.rds"))
