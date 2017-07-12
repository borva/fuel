########################################################
## (C) 2017 by Boris Vaillant, Quantitative Consulting
## 
## License is granted to to use this code under the MIT License
## (see file License_MIT.txt )
##
## Part C: Geo matching of station data
##
## 1) This code reads in the NUTS 3 - shape polygons from www.gadm.org
## (hosted at "http://biogeo.ucdavis.edu", note that this has a non-commercial license,
## so you need to pull this data yourself)
## 2) matches the long / lat in stationAll to the corresponding NUTS 3 - area
## and adds a number of socio-economic explanatory variables from A_destatis_KreisData.rds
## to stationsAll
## 3) In the next step, the program calculates closeBAB", an indicator whether a 
## station is in a (roughly) 500m radius from a motorway-link
## This step takes around 10min on an average laptop and is only performed,
## if the file C_stationsAll_w_Highway_KEEP.rds is removed from "Z DataOut"
## 4) The distance matrix between stations is then calculated. Based on this
## we calculate a number of metrics describing local competitor density and
## write the "distance data frame" to C_distDF.rds 
## 5) Finally, using the same methodology, we calculate distances between stations
## and "Gemeinden" from A_destatis_GV.rds. The area and population from the closest
## 3 Gemeinde are then used to calculate the population density around each station
##
## Results are written to C_stationsAll_final.rds
## Note - the calculation of the distance matrices is memory intensive.
## This code runs OK - sometimes with some swapping - on 8GB RAM 
##
## TODO: 
## * Replace my manually constructed OSM- highway shapefile by an appropriate
## call to Overpass using the package osmdata
## * introduce sf

## Clear all -- only if you have saved your other projects
## rm(list=ls(all=TRUE)) 

library(tidyverse)
library(stringr) 

## this path setting assumes that the working directory is set to the project folder
PATHDATA = "./C DataIn/"
PATHOUT = "./Z DataOut/"

## ------------------------------------------------------
## Prepare geodata
library(sp)

if(!file.exists(paste0(PATHOUT, "C_kreisShapes.rds"))) {
  ## runs, if you do not have the gadm Shapefiles 
  ## for German administrative areas (level 2, NUTS3) "Kreise"
  ## from  www.gadm.org/  !this cannot be used for commercial purposes!
  ## online download - please ensure you are connected
  kreisShapes <- readRDS(gzcon(url(
    "http://biogeo.ucdavis.edu/data/gadm2.8/rds/DEU_adm2.rds")))
  saveRDS(kreisShapes, paste0(PATHOUT, "C_kreisShapes.rds"))
}
  
kreisShapes <- readRDS( paste0(PATHOUT, "C_kreisShapes.rds"))
proj4string(kreisShapes)

## kreisShapes is an S4 object:
slotNames(kreisShapes)
head(kreisShapes@data)
dim(kreisShapes@data) # 403 15

## ----------------------------------------------------
## combine with general statistics per Kreis from WS A

kreisStats <- readRDS(file = paste0(PATHOUT,"A_destatis_KreisData.rds")) %>% 
  mutate(One = 1L)

## create a combined Kreis data table from all sources
kreisAll <- kreisShapes@data %>% 
  select(kreisID = CCA_2) %>%
  mutate(id = as.character(row_number()-1)) %>%
  left_join(kreisStats, by= c("kreisID"= "KreisCode"))

dim(kreisAll)

## check matches
summary(kreisAll$One) ## one missing -> Bodensee -> OK

## --------------------------------------------------------------------
## match stations to Kreise and join 

stationsAll <- readRDS(file = paste0(PATHOUT,"B StationsAll.rds"))

## ensure station long / lat have exactly thje same projection string
## as the kreisShapes

pointsDF <- stationsAll %>% select(long = lng, lat = lat)
pointsSP <- SpatialPoints(pointsDF, proj4string=CRS(proj4string(kreisShapes)))
kreisMatch <- over(pointsSP, kreisShapes)
summary(kreisMatch) 

## add Kreis Marker to Stations 
stationsAll <- bind_cols(stationsAll, select(kreisMatch, kreisID=CCA_2))

# few missing matches would require manual correction -- we do not correct these
stationsAll %>% filter(is.na(kreisID))
# tankpool24 Straße von Malakka 26388 Wilhelmshaven N 53.58208 E 8.13587 ## on seashore
# Clemens Tenhagen BFT Weselerstr.17 47665 Sonsbeck N 51.36410 E 6.22430 ## according to Google already in NL

## First result of match: Stations Count per Kreis etc
addStation2Kreis <- stationsAll %>% 
  filter(!is.na(kreisID)) %>%
  group_by(kreisID) %>% 
  summarise(NrStations = n()) ## more later

kreisAll <- kreisAll %>% 
  left_join(addStation2Kreis) %>%
  mutate(
    Stats100KInh = as.integer(NrStations / Insgesamt * 100000),
    StatsKKm2 = as.integer(NrStations / `Flaeche` * 1000),
    Stats100KKFZ = as.integer(NrStations / KFZ_Insgesamt * 100000))

dim(kreisAll)
summary(kreisAll)

## join Kreis Infos to Stations (check join is OK)
dim(stationsAll)
stationsAll <- stationsAll %>% left_join(kreisAll)
dim(stationsAll)

## -----------------------------------------------------------------------
## for each station, find distance to next highway link
## and create the "closeBAB" indicator

if(!file.exists(paste0(PATHOUT, "C_stationsAll_w_Highway.rds"))) { 
  ## this section creates the "closeBAB" highway info and saves the result
  ## delete the mentioned file to make this run explicitly
  ## runs 10-15 min.
  
  library(rgdal)
  ## data source: OSM - thanks to http://download.geofabrik.de
  highwayShapes <- readOGR(paste0(PATHDATA, "highways_shp"), "highways", p4s = NULL)
  proj4string(highwayShapes) #longlat OK
  
  ## ensure station long / lat have exactly the same projection string
  ## as the highwayShapes
  pointsDF <- stationsAll %>% select(long = lng, lat = lat)
  pointsSP <- SpatialPoints(pointsDF, proj4string = CRS(proj4string(highwayShapes)))
  
  ##  Calculate distance of all stations to all highways
  library(rgeos)
  indexLink = highwayShapes@data$type == "motorway_link"
  
  ## rgeos::gWithinDistance works in cartesian coordinates only
  ## consider projecting to something more rectangular in Germany?
  system.time( ## 600s
    linkMatch <- gWithinDistance(pointsSP, highwayShapes[indexLink,], 
                                    dist = 0.005, byid = TRUE) ##~500m around HW Links
  ) 
  
  stationsAll$Linkgeoscore <- colSums(linkMatch)
  rm(linkMatch, highwayShapes)
  gc()
  
  stationsAll <- stationsAll %>% 
    mutate(closeBAB = (!isBAB &  Linkgeoscore >0)*1L)
  
  ## save stationsAll, rename to "_KEEP" if you want to use this file
  ## going forward
  saveRDS(stationsAll, paste0(PATHOUT, "C_stationsAll_w_Highway.rds")) 
}

stationsAll <- readRDS( paste0(PATHOUT, "C_stationsAll_w_Highway_KEEP.rds")) 

## -----------------------------------------------------
## distance "matrix" between stations

library(geosphere)

pointsDF <- stationsAll %>% select(long = lng, lat = lat)
pointsSP <- SpatialPoints(pointsDF, proj4string=CRS("+proj=longlat +datum=WGS84"))

system.time( ## for full matrix ~1min
  distvec <- as.vector(as.integer(distm(pointsSP, fun = distHaversine)))
) # distance in meter

NStations <- length(stationsAll$StationID)

## check memory
gc(); sort(sapply(ls(), function(x){ object.size(get(x)) })) / 1000000

## Create the main "distance dataframe"
## Note: 8GB RAM should just be OK,
## final result (after throwing away long distances) will be very small
distDF <- tibble(distcomp = distvec,
                 StationID = rep(stationsAll$StationID, NStations),
                 CompID = rep(stationsAll$StationID, rep(NStations, NStations)),
                 BrandInt = rep(stationsAll$brandClInt, NStations),
                 CompBrandInt = rep(stationsAll$brandClInt, rep(NStations, NStations)))  %>%
  filter(distcomp <= 16383, StationID != CompID) %>%
  mutate(distRing =  pmax(512L, 2^as.integer(log2(1 + distcomp) + 0.999)),
         distRingTxt = paste0("< ", substr(100000 + distRing, 2, 6), "m"))

## free space and check memory again
rm(distvec)
gc(); sort(sapply(ls(), function(x){ object.size(get(x)) })) / 1000000

## look at distribution of distance classes
table(distDF$distRingTxt) 

## calculate number of competitors per full distance class
## "all" = any other station, "own" = stations from own brandCl
adddistDF <- distDF %>% ungroup() %>%
  mutate(myCt = CompBrandInt == BrandInt) %>%
  group_by(StationID, distRing, distRingTxt) %>%
  summarise(Ct = n(),
            myCt = sum(myCt, na.rm =TRUE)) %>%
  ungroup() %>%
  arrange(StationID, distRing) %>%
  group_by(StationID) %>%
  mutate(Ct = cumsum(Ct), # sum over the rings gives full disc
         myCt = cumsum(myCt)) %>%
  ungroup() %>%
  select( -distRing )

## keycheck
dim(adddistDF); adddistDF %>% group_by(StationID, distRingTxt) %>% summarise(n()) %>% dim()

adddistDF1 <- adddistDF %>%
  mutate(distRingTxt = paste0("all", distRingTxt)) %>% 
  select(-myCt) %>%
  spread(key = distRingTxt, value = Ct, fill = 0.0)

adddistDF2 <- adddistDF %>%
  mutate(distRingTxt = paste0("own", distRingTxt)) %>% 
  select(-Ct) %>%
  spread(key = distRingTxt, value = myCt, fill = 0.0)

stationsAll <- stationsAll %>% left_join(adddistDF1) %>% left_join(adddistDF2)

saveRDS(distDF, paste0(PATHOUT, "C_distDF.rds"))
saveRDS(kreisAll, paste0(PATHOUT, "C_kreisAll.rds"))

##----------------------------------------------------
## Use Gemeindeverzeichnis to get population density etc at station level

# pointsDF <- stationsAll %>% select(long = lng, lat = lat)
pointsSP <- SpatialPoints(pointsDF, proj4string = CRS("+proj=longlat +datum=WGS84"))

gv <-  readRDS( paste0(PATHOUT, "A_destatis_GV.rds"))
gv <- gv %>% filter(!is.na(Long )) %>%
  ungroup() %>%
  mutate(GemeindeID = row_number(),
         kreis5 = paste0(Land,RB, Kreis))

pointsGvDF <- select(gv, long = Long, lat = Lat)
pointsGvSP <- SpatialPoints(pointsGvDF, proj4string = CRS("+proj=longlat +datum=WGS84"))

### Distances
system.time( ## for full matrix 1min
  distGvvec <- as.integer(1+distm(pointsSP, pointsGvSP, fun = distHaversine))
) # distance  meter

NStations <- length(stationsAll$StationID)
NGv <- length(gv$GemeindeID)

## Distance "matrix" between Stations and Gemeinden
distGvDF <- tibble(distcomp = distGvvec,
                 StationID = rep(stationsAll$StationID, NGv),
                 GemeindeID = rep(gv$GemeindeID,rep(NStations, NGv))) %>%
  filter(distcomp <= 50000)

## free space and check memory again
rm(distGvvec)
gc(); sort(sapply(ls(), function(x){ object.size(get(x)) })) / 1000000

dim(distGvDF)
distGvDF <- distGvDF %>% 
  left_join(select(gv, GemeindeID, insgesamt, Fläche))
dim(distGvDF) # match OK

## calculate population density as average from 3 closest Gemeinde
distGvDF <- distGvDF %>% ungroup() %>%
  arrange(StationID, distcomp) %>%
  group_by(StationID) %>%
  slice(1:3) %>%
  summarise(popdens = sum(insgesamt, na.rm=TRUE) / sum(Fläche, na.rm=TRUE))
  
dim(distGvDF)
stationsAll <- stationsAll %>% left_join(distGvDF)

## -------------------------------------
## save result
saveRDS(stationsAll, paste0(PATHOUT, "C_stationsAll_final.rds"))
