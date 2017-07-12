########################################################
## (C) 2017 by Boris Vaillant, Quantitative Consulting
## 
## License is granted to to use this code under the MIT License
## (see file License_MIT.txt )
##
## Part B: Cleaning of the station master table
##
## This code takes the file all_stations.txt.gz,
## cleans long / lat, extracts brand information, and
## identifies parts of the text, indicating whether a station is a higway
## station or not. The result is written to "B StationsAll.rds"
## It also parses the json containing the information about the
## opening hours writing the result "B StationsOpeningTimes.rds"
## Outputs are written to folder "Z DataOut"


## Clear all -- only if you have saved your other projects
## rm(list=ls(all=TRUE)) 

library(tidyverse)
library(stringr) 

## this path setting assumes that the working directory is set to the project folder
PATHDATA = "./B DataIn/"
PATHOUT = "./Z DataOut/"

## -------------------------------------------------------------
## load station data
## from separate delivery M. Kurz / www.tankerkoenig.de 30.5.2017
## can be used under CC4.0

stationsAll <- read_delim(file= paste0(PATHDATA,"/all_stations.txt.gz"), delim ="|",
                   trim_ws = TRUE,
                   locale = locale(decimal_mark = "."),
                   col_types = cols(
                     `id`= col_character(),
                     `version`= col_integer(),
                     `version_time`= col_datetime(),
                     `name`= col_character(),
                     `brand`= col_character(),
                     `street`= col_character(),
                     `house_number`= col_character(),
                     `post_code`= col_integer(),
                     `place`= col_character(),
                     `public_holiday_identifier`= col_character(),
                     `lat`= col_double(),
                     `lng`= col_double(),
                     `price_in_import`= col_datetime(),
                     `price_changed`= col_datetime(),
                     `open_ts`= col_character(),
                     `ot_json`= col_character(),
                     `station_in_import`= col_character()
                   ))

problems(stationsAll) ## OK

# remove delimiting rows and create a simpler ID
# NOTE: this ID is kept fixed will be universally used
# also in the following stages of the analysis

stationsAll <- stationsAll %>% 
  slice(2:(nrow(.)-1)) %>%
  rename(stid = id) %>%
  arrange(stid) %>% 
  mutate(StationID = row_number())


## -------------------------------------------------------------
## clean coordinates 

## switch lat / long for a few stations; set missings to 0
longlim = c(5, 20) ## very rough box around Germany
latlim = c(45, 60)

na2zero <- function(x) ifelse(is.na(x), 0.0, x)

stationsAll <- stationsAll %>%
  mutate(
    lat = na2zero(lat),
    lng = na2zero(lng),
    issue = lat != 0 & (lat < latlim[1] | lat > latlim[2] |
                          lng < longlim[1] | lng > longlim[2]),
    lat2 = ifelse(issue, lng, lat),
    lng2 = ifelse(issue, lat, lng),
    issue2 = lat != 0 & (lat2 < latlim[1] | lat2 > latlim[2] |
                           lng2 < longlim[1] | lng2 > longlim[2]),
    lat = lat2,
    lng = lng2
  )

## manual correction for one station
stationsAll$lat[stationsAll$stid=="a2380ebc-1b74-4ac0-9db0-9a90512513d6"] <- 53.1613
stationsAll$lng[stationsAll$stid=="a2380ebc-1b74-4ac0-9db0-9a90512513d6"] <- 12.4637

## -------------------------------------------------------------
## clean attributes / brands  

stationsAll <- stationsAll %>% 
  mutate(
    brand2 = brand %>% str_to_lower() %>% str_replace_na() %>% str_trim(side = "both"),
    name2 = name %>% str_to_lower() %>% str_replace_na() %>% str_trim(side = "both")
  )

## table of translations and groupings to apply to brand
keysdf <-  tribble(
  ~kword, ~kgroup,
  "bft",      "BfT",
  "raiffeisen",   "Raiffeisen", 
  "freie",  "Others", 
  "avia","Avia",
  "kaufland",  "Supermarkt", 
  "real,-",  "Supermarkt",
  "shell",  "Shell", 
  "esso",  "Esso", 
  "total", "Total", 
  "jet", "Jet", 
  "agip", "Agip", 
  "supermarkt",  "Supermarkt", 
  "v-markt", "Supermarkt",
  "v markt", "Supermarkt",
  "edeka",  "Supermarkt", 
  "rewe", "Supermarkt", 
  "globus", "Supermarkt", 
  "markant", "Supermarkt", 
  "aral",  "Aral", 
  "star",  "Star", 
  "hem", "HEM",
  "marktkauf", "Supermarkt", 
  "e center",  "Supermarkt", 
  "e-center",  "Supermarkt", 
  #  "oil\!", "Oil!", 
  "omv",  "OMV", 
  "westfalen","Westfalen")

## ensure keywords are at start or end of a word
keysdf <- keysdf %>% mutate(kreg = paste0("\\b", kword,"\\b"))
regstring <- paste(keysdf$kreg, collapse="|")

stationsAll <- stationsAll %>% 
  mutate(brand3 = brand2 %>% str_extract(regstring),
         name3 = name2 %>% str_extract(regstring),
         brand4 = ifelse((is.na(brand3) & !is.na(name3))|
                           (brand3=="freie" & name3=="bft"), name3, brand3 ))

dim(stationsAll)
stationsAll <- stationsAll %>% 
  left_join(select(keysdf, brand4 = kword, brandCl = kgroup)) %>%
  mutate(brandCl = as.factor(ifelse(is.na(brandCl), "Others", brandCl)),
         brandClInt = as.integer(brandCl))

dim(stationsAll) ## never join anything handmade without checking dimensions :)

## add size class of brands
temp <- c(5, 10, 50, 100, 500, 1000, 5000 )

stationsAll <- stationsAll %>% 
  group_by(brand2, brandCl) %>% 
  mutate(Count= n()) %>% 
  ungroup() %>%
  mutate(Count = ifelse(is.na(brand2)| brand2 %in% c("", "NA"), 1, Count), 
         sizeCl = cut( Count, breaks = c(0, temp), labels = paste0("<", temp), right = FALSE),
         sizeClInt = as.integer(sizeCl)) %>%  
  select(-name3, -brand3, -brand4, -Count)

## show overview of brands 
stationsAll %>% group_by(brand2, brandCl, sizeCl) %>% 
  summarise(Count = n()) %>% arrange(desc(Count))


## -------------------------------------------------------------
## Highway marker

## parse addresses for signs of highways

kwBAB <- paste(c("\\bautobahn",   "\\bbat\\b",  "\\bbab\\b"), collapse="|")
kwDir <- paste(c("\\bnord\\b",   "\\bsued\\b", "\\bsüd\\b", "\\bost\\b", "\\bwest\\b"), collapse="|")
kwAX <- "\\ba[ ]*\\d+\\b"

## create a (wholly made up) score, based on how many of these matches are found
stationsAll <- stationsAll %>% 
  mutate(BABtxtscore =
           grepl(kwBAB, str_to_lower(stationsAll$street)) +
           grepl(kwDir, str_to_lower(stationsAll$street)) +
           grepl(kwAX, str_to_lower(stationsAll$street))*2 +
           grepl(kwBAB, str_to_lower(stationsAll$brand)) +
           grepl(kwAX, str_to_lower(stationsAll$brand))*2 +
           grepl(kwDir, str_to_lower(stationsAll$brand)) +
           grepl(kwBAB, str_to_lower(stationsAll$place)) +
           grepl(kwAX, str_to_lower(stationsAll$place))*2 +
           grepl(kwDir, str_to_lower(stationsAll$place)))

table(stationsAll$BABtxtscore)

## this is the input for a manual review of names
## and places, the result of which is the following vector
## (NOTE: this refers to the StationID defined above -
## changing the master data input would destroy this!)

BABresult <- c( 4554, 7840, 4080, 12336, 917, 1141, 1203, 1385, 1621, 1701, 2070, 
                2545, 2571, 2997, 3984, 4310, 4359, 4603, 4719, 4780, 6082, 6170, 
                6542, 8545, 8620, 8665, 8826, 8973, 9422, 10082, 10549, 10818, 11677, 
                12155, 12159, 12343, 13549, 13614, 13888, 13932, 13935, 14216, 14558, 
                14701, 14796, 3234, 9327, 10068, 10866, 11795, 13591, 13951, 4363, 12228, 
                7314, 8212, 9038, 10405, 14020, 1364, 1529, 1564, 1594, 1612, 1736, 2173, 
                2413, 2419, 2564, 2631, 2981, 3243, 3361, 3371, 3393, 3395, 3406, 3441, 
                3473, 3737, 3742, 4011, 4104, 4247, 4405, 4433, 4621, 4733, 4755, 4766, 
                5829, 5925, 6342, 6405, 6482, 6700, 7538, 7751, 7767, 7785, 7906, 7995, 
                8117, 8143, 8233, 8259, 8280, 8332, 8358, 8377, 8434, 8473, 8525, 8639, 
                8666, 8833, 8857, 9261, 9268, 9818, 10131, 10334, 10851, 10958, 11014, 
                11157, 11273, 11477, 11557, 11686, 11740, 12380, 12413, 12509, 12565, 
                12663, 12956, 13048, 13053, 13561, 13578, 13824, 14008, 14203, 14219, 
                14611, 14692, 14697, 14795, 1444, 2249, 2479, 4820, 6649, 7955, 8410, 
                10277, 10724, 11113, 11787, 13040, 14200, 4560, 11603, 12171, 13438, 
                14124, 14737, 9279, 9588, 1125, 1608, 2780, 2788, 2995, 3376, 3383, 
                3593, 3879, 4604, 5876, 7021, 7110, 7168, 7453, 7857, 8503, 8814, 8955, 
                11023, 11621, 12119, 12790, 12839, 13815, 13978, 14106, 14265, 14471, 
                14639, 14880, 14899, 5937, 7859, 13572, 432, 1858, 11653, 7095, 1476, 
                1514, 1634, 1762, 2442, 3615, 3696, 3994, 4901, 5776, 5973, 6153, 6515, 
                6605, 6827, 6915, 7297, 7619, 7879, 8083, 8517, 8603, 9075, 9185, 10262, 
                10272, 10824, 10873, 10928, 11488, 11655, 11750, 12459, 12463, 13129, 
                13737, 14274, 14382, 14841, 14854, 14858, 1347, 6495, 7016, 7623, 8649, 
                9756, 12549, 14192, 14401, 1077, 1156, 1960, 5923, 11019, 8067, 941, 1006, 
                1226, 1239, 1405, 1522, 1554, 1810, 2089, 2175, 2311, 2440, 2696, 2973, 
                2988, 3012, 3043, 3335, 3598, 3747, 3781, 3882, 3927, 4164, 4177, 4204, 
                4279, 4932, 5871, 5872, 6061, 6113, 6339, 6351, 6583, 6916, 7030, 7045, 
                7234, 7408, 7522, 7753, 7889, 8312, 8442, 8464, 9110, 9150, 9194, 9466, 
                9514, 9581, 9741, 9759, 9844, 10112, 10170, 10268, 10363, 10479, 10617, 
                10943, 10989, 11271, 11282, 11333, 11634, 11788, 12345, 12351, 12493, 
                12626, 12658, 12705, 12850, 12864, 13012, 13489, 13503, 13512, 13686, 
                13755, 13844, 14125, 14335, 14586, 14624, 14670, 14733, 1933, 4651, 4842, 
                6058, 8564, 10310, 10419, 12213, 12391)

stationsAll <- stationsAll %>% 
  mutate(isBAB = (StationID %in%  BABresult)*1L)

## -------------------------------------------------------------
## opening hours

## parse json for opening hours
## (has duplicated lines for each separate definition of opening times)

dim(stationsAll)
stationsHours <-stationsAll %>% 
  select(stid, StationID, ot_json) %>%
  mutate(parse_json = map(ot_json, jsonlite::fromJSON, flatten = FALSE),
         days = map(parse_json, `[[`, c("openingTimes")) %>% map( `[[`, "applicable_days"),
         periods = map(parse_json, `[[`, c("openingTimes")) %>%    map( `[[`, "periods") %>% map(bind_rows)
  ) %>% 
  select(-parse_json)%>% 
  unnest() %>%
  mutate(startInt =  as.integer(as.difftime(startp, format = "%H:%M", units = "mins")/10)*10L,
         endInt =  as.integer(as.difftime(endp, format = "%H:%M", units = "mins")/10)*10L,
         endInt = ifelse(endInt <= 1, 1440L, endInt))

## translate bit codes for days
## first create a "bit translation" data frame
Nbit = 8
repBit <- function(stelle, nbit = Nbit) {
  tibble(days = 1:(2^nbit - 1), 
         weekdayX = as.integer(stelle + 1), ## 1 should be Monday
         digbit = as.integer(rep(rep(c(0L, 1L), rep(2^stelle, 2)), 2^(nbit - 1 - stelle))[-1]))
}
bitdf <- bind_rows(lapply((Nbit-1):0, repBit)) %>% filter(digbit>0) %>%
  select(-digbit)
bitdf

## match bit translation to the opening hours
dim(stationsHours)
stationsHours <- stationsHours %>% left_join(bitdf)
dim(stationsHours) ## 107511 last time I checked

## the following requires that there are max 2 entries per day
## (which is the case in the current data set)
stationsHours <- stationsHours %>% group_by(StationID, weekdayX) %>%
  mutate(countrow = row_number(),
         startInt1 = first(startInt),
         startInt2 = last(startInt),
         endInt1 = first(endInt),
         endInt2 = last(endInt)) %>%
  filter(countrow ==1) %>%
  ungroup()
dim(stationsHours)

table(stationsHours$weekdayX) # Sun = 7

## open: ca 100 empty ot_json -> always open or always closed?
temp <- stationsHours %>% ungroup() %>% distinct(StationID)
stationsAll %>% anti_join(temp)

## --------------------------------------------------------------
## save outputs

saveRDS(stationsAll, paste0(PATHOUT, "B StationsAll.rds"))
saveRDS(stationsHours, paste0(PATHOUT, "B StationsOpeningTimes.rds"))
