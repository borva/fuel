########################################################
## (C) 2017 by Boris Vaillant, Quantitative Consulting
## 
## License is granted to to use this code under the MIT License
## (see file License_MIT.txt )
##
## Part E: First simple analyses and graphs
##

# Clear all -- only if you have saved your other projects
# rm(list=ls(all=TRUE)) 

library(tidyverse)
library(lubridate)
library(stringr) ## should be included in tidyverse, but doesn't seem to be ...

## this path setting assumes that the working directory is set to the project folder
PATHDATA = "./F DataIn/"
PATHOUT = "./Z DataOut/"

## --------------------------------------------
## daily time series

## load data from previous stage
pricesAgg <- readRDS( paste0(PATHOUT,"D_pricesAgg_30_KEEP.rds"))
brent <- readRDS( paste0(PATHOUT,"E_Brent.rds"))

pricesAgg <- pricesAgg %>% 
  mutate(date = ymd(DatumCounter),
         Wday = as.integer((wday(date) + 5) %% 7 + 1),
         Week = as.integer(isoweek(date)))

showDay <- pricesAgg %>%
  group_by(date, Wday) %>%
  summarise( medE10 = median(medE10, na.rm = TRUE) / 10,
             medE5 = median(medE5, na.rm = TRUE) / 10,
             medDiesel = median(medDiesel, na.rm = TRUE) / 10) %>%
  gather(key ="Type", value ="Price in Ct", medE10, medE5, medDiesel)

brent <- brent %>%
  mutate(Type = "Brent",
         Wday = as.integer((wday(date)+5) %% 7 +1 ),
         `Price in Ct` = as.integer(Brent)/10) %>%
  select(date,  Wday,   Type, `Price in Ct`)

showDay <- bind_rows(showDay, brent)

gg1 <- ggplot(showDay, aes(x = date,  y= `Price in Ct`, color = Type)) + 
  geom_step()
gg1

## --------------------------------------------
## intra-week variation

showWDay <- showDay %>% group_by(Wday, Type) %>%
  summarise(Price = mean(`Price in Ct`)) %>%
  group_by( Type) %>%
  mutate(`Delta in Ct` = Price - mean(Price))

gg2 <- ggplot(filter(showWDay, Type != "Brent"), 
       aes(x = Wday, y = `Delta in Ct`, fill = Type  )) + 
  geom_col()  + facet_wrap(~Type)
gg2

## -----------------------------------------------------
## Overview of station Brands

## load data from previous stage
stationsAll <- readRDS( paste0(PATHOUT, "C_stationsAll_final.rds"))

stationsView <- stationsAll %>% filter(!is.na(ot_json)) %>%
  group_by(brandCl) %>% summarise(Number = n()) %>%
  ungroup() %>%
  arrange(Number) %>% 
  mutate(brandFact = factor(brandCl, levels= as.vector(brandCl)[(-2+(1:length(brandCl)))%%length(brandCl)+1]),
         NumberX = ifelse(Number > 4000, 0, Number),
         hjustnum = ifelse(NumberX >= 400 , 1.3, -0.3 )
  )

gg3 <- ggplot(stationsView, aes(x=brandFact, y= NumberX, label = Number, hjust = hjustnum)) +
  geom_col() + ylim(0, 2500) +
  coord_flip() +  geom_text( ) +
  labs(y= "Number of Stations", x="",
       caption= "from Station Master Data MTS-K & www.tankerkoenig.de")
gg3

## -----------------------------------------
## distribution of population densities

showPopdens <- stationsAll %>% 
  ungroup() %>%
  filter(!is.na(popdens), popdens > 1) %>%
  select(popdens) %>%
  arrange(popdens) %>%
  mutate(`Population density inhab. / km2` = popdens, 
         `cumul. number of stations` = row_number())

ggPop <- ggplot(showPopdens)  + 
  geom_step( aes(x = `cumul. number of stations`, 
                 y= `Population density inhab. / km2`) ,
             color = "darkorange")
ggPop

## ----------------------------------------
## competitor distances

distDF <- readRDS(paste0(PATHOUT, "C_distDF.rds"))

NStations <- nrow(stationsAll)

distClasses <- distDF %>% 
  filter(distRing < 16385) %>%
  mutate(distRing = pmax(512, distRing),
         `Distance class` = paste0("< ",substr(100000 + distRing,2,6),"m")) %>%
  group_by(`Distance class`) %>%
  summarise(`Avg # of competitors` = round(n() / NStations,1)) %>%
  mutate( hjustnum = ifelse(`Avg # of competitors` >= 20 , 1.3, -0.3 ))

gg4 <- ggplot(distClasses, aes(x = `Distance class`, 
                               y = `Avg # of competitors` , 
                               hjust = hjustnum, 
                               label = `Avg # of competitors`)) + 
  geom_col() +  geom_text() + coord_flip()
gg4


## --------------------------------------- 
## map of highway stations

babs <- stationsAll %>% filter(isBAB==1 | closeBAB==1) %>%
  mutate(BAB = ifelse(isBAB==1, "on BAB", "close to BAB")) %>%
  select(lng, lat, BAB)

## the map is not distributed in the project file,
## please ensure you have internet connection
## and license conditions are met
if(!file.exists(paste0(PATHOUT, "F_DEmap.rds"))) {
  library(ggmap)
  demap <- qmap(location = "germany", zoom = 6, 
                size = c(600,800), maptype = c("roadmap"), scale = 2) 
  saveRDS(demap, paste0(PATHOUT, "F_DEmap.rds"))
}

demap <- readRDS(paste0(PATHOUT, "F_DEmap.rds"))
dehwmap <- demap +  
  xlim(5.90, 14.99)+ ylim(47.41, 55.02) + 
  geom_point(data = babs, aes(x = lng, y = lat, color = BAB), size = 0.7)+
  scale_color_manual(values = c("on BAB" = "darkorange", "close to BAB" = "grey")) +
  guides(color = guide_legend(override.aes = list(size = 3)))+
  theme(legend.position= c(0.2, 0.9),
        legend.background = element_rect(colour = "darkgrey", fill = "white"),
        legend.title = element_blank())

dehwmap


## ----------------------------------------
## prepare regression coefficients

resultDF <- readRDS(paste0(PATHOUT, "E_ResultDF_KEEP.rds")) %>%
  filter(!(Name %in% c("FELM", "LM + Comp x Brand")))

## again, build a grid to fill in variables = 0
## note: may switch this back to expand.grid()

coefgrid <- resultDF %>% ungroup() %>% distinct(term) %>% mutate(One = 1L)
namegrid <- resultDF %>% ungroup() %>% distinct(Name) %>% mutate(One = 1L)

resultGrid <- namegrid %>% left_join(coefgrid)

## this table steers the grouping and cleaning of the regressionb coefficients 
keysdf <-  tribble(
  ~kword, ~kgroup, ~kname, ~kremove, ~kshow, ~kfactor, ~kunit,
  "ns",   "Geography", "",  "XX", TRUE, 0.1, "Ct",
  "Brent",   "Price In", "Brent",  ".", TRUE, 1, "Factor",
  "Wday",  "Time", "Day of Week", "factor\\(\\w+\\)[\\.]*",  TRUE, 0.1, "Ct",
  "Week",  "Time", "Week", "factor\\(\\w+\\)\\.",  FALSE, 0.1, "Ct",
  "CompMean", "Price In", "Competitor Price", ".",  FALSE, 1, "Factor",
  "lambda", "Price In", "Competitor Price", ".",  TRUE, 1, "Factor",
   "popdens_Sc", "Market", "Population Density", ".", TRUE, 100, "Ct",
  "compdens_Sc",  "Market","close Competitors", ".", TRUE, 100, "Ct",
  "brandCl",  "Brand", "", "brandCl", TRUE, 0.1, "Ct",
  "hotels_Sc", "Market", "Hotels", ".", TRUE, 100, "Ct",
  "isBAB", "Location","on BAB", ".", TRUE, 0.1, "Ct",
  "closeBAB", "Location", "close to BAB", ".", TRUE, 0.1, "Ct",
  "StatsKKm2_Sc", "Market", "Station Density", ".", TRUE, 100, "Ct",
  "Intercept",  "Market", "Intercept", ".", FALSE, 0.1, "Ct")

## ensure keywords are at start or end of a word
keysdf <- keysdf %>% mutate(kreg = paste0("\\b", kword))
regstring <- paste(keysdf$kreg, collapse="|")

## add cleaning and grouping to the terms
resultGrid <- resultGrid %>%  
  mutate(CoefKey = term %>% str_extract(regstring))%>% 
  left_join(select(keysdf, CoefKey = kword, CoefCl = kgroup, 
                   CoefShort = kname, kremove, kshow, kfactor)) %>%
  mutate(CoefDetail = str_replace_all(term, kremove,""),
         Term = paste(CoefShort, CoefDetail)) %>%
  filter(!is.na(kshow) & kshow == TRUE) 

## keycheck
n_distinct(resultGrid$Name) * n_distinct(resultGrid$Term)
dim(resultGrid)

## add estimates and diagnostics back tothe table,
## calculate appropriate units
resultGrid <- resultGrid %>%  left_join(resultDF) %>%
  mutate(Effect = estimate*kfactor,
         Effect = ifelse(is.na(Effect), 0, Effect)) %>%
  arrange(Name, CoefCl, Term)
dim(resultGrid)

## plot
resultGG <- resultGrid %>% 
  filter(CoefCl != "Geography" ) %>%
  mutate(CoefCl2 = CoefCl) %>%
  nest(-Name, -CoefCl) %>%
  mutate(gg = map(data, ~ggplot(.) +
                    geom_col(aes(x = factor(Term, levels= rev(levels(factor(Term)))), 
                                 y =  Effect, fill = CoefCl2)) +
                    coord_flip() + xlab("") +
                                   ggtitle(.$CoefCl2) +
                    scale_fill_discrete(guide = FALSE) ))

resultGG
resultGG$gg[[2]]

## ---------------------------------
## Spline Visual

library(splines)
N = 100
lnggrid <- seq(5.80, 15.00, length.out = N)
latgrid <- seq(47.00, 55.10, length.out = N)

## again, people would probably rather use expand.grid()
geosimDF <- tibble(lng = rep(lnggrid, N),
                   lat = rep(latgrid, rep(N, N)))

## needs to be restricted to points in Germany, only
library(sp)
kreisShapes <- readRDS(file = paste0(PATHOUT,"C_kreisShapes.rds"))
pointsDF <- geosimDF %>% select(long = lng,lat = lat)
pointsSP <- SpatialPoints(pointsDF, proj4string = CRS(proj4string(kreisShapes)))
kreisMatch <- over(pointsSP, kreisShapes)
geosimDF <- geosimDF %>% filter(!is.na(kreisMatch$ID_0))

## recalculation of total contribution of splines:
matsimDF <-  model.matrix(~  -1 + ns(lat, df = 6):ns(lng, df = 6) ,
                            data = geosimDF)
## need to make sure, we have names alphabetically ordered
matsimDF <- matsimDF[, order(colnames(matsimDF), sort(colnames(matsimDF)))]

resultGeo <- resultGrid %>% filter(CoefCl =="Geography") %>%
  mutate(Effect = Effect * (p.value < 0.5)) %>%
  select(Effect, Name, term) %>%
  nest(-Name)

dim(matsimDF)
resultGeo

resultGeo <- resultGeo %>%
  mutate(testOK = map(data, ~all(.$term == colnames(matsimDF))),
         predGeo = map(data, ~as.vector(matsimDF %*% .$Effect)),
         predGeoClean = map(predGeo, ~pmin(pmax(., quantile(., probs = 0.02)),
                             quantile(., probs = 0.98))),
         geoCoord = map(data, ~ geosimDF),
         `Delta Ct` = map(predGeoClean, ~(. - mean(.))))

resultGeo$testOK ## needs to be all true, otherwise check ordering of names
resultGeo

## load base map
demap <- readRDS(paste0(PATHOUT, "F_DEmap.rds"))

resultMap <- resultGeo %>% select(Name, geoCoord, `Delta Ct`) %>% 
  unnest() %>%
  nest(-Name) %>%
  mutate( deCtmap = map(data,  ~demap +  
  xlim(5.90, 14.99)+ ylim(47.41, 55.02) + 
  geom_tile(data = ., 
            mapping = aes(x=lng, y=lat, fill = `Delta Ct`),
            alpha = 0.7) +
  scale_fill_gradient2(low = "#797AB4", mid = "white",
                       high = "#E24E26", midpoint = 0, space = "Lab",
                       na.value = "grey50", guide = "colourbar") +
    xlim(5.90, 14.99 ) +          
    ylim(47.41, 55.02 ) + 
    guides(color = guide_legend(override.aes = list(size=3)))+  
    theme(legend.position= c(0.1, 0.8),
        legend.background = element_rect(colour = "grey", fill = "white"))
  ))

resultMap
resultMap$deCtmap[[6]]

saveRDS(resultMap, paste0(PATHOUT,"F_resultMap.rds"))
saveRDS(resultGG, paste0(PATHOUT,"F_resultGG.rds"))
saveRDS(resultGrid, paste0(PATHOUT,"F_resultGrid.rds"))
