#Upload greenery
library(terra)
library(sf)
library(dplyr)
library(purrr)
library(tidyverse)
#big code
names <- tibble(ID = 1:76,
                lc_names = c("Freiburg im Breisgau", "Offenburg", "FUA of Strasbourg", "FUA of Colmar", 
                             "Basel", "FUA of Belfort", "FUA of Mulhouse", "FUA of Limoges", 
                             "FUA of Bordeaux", "FUA of Pau", "FUA of Bayonne", "Pamplona/Iruña", 
                             "Irun", "FUA of Clermont-Ferrand", "FUA of Roanne", "FUA of Saint-Étienne", 
                             "FUA of the Greater City of Paris", "FUA of Chartres", "FUA of Le Havre", 
                             "FUA of Caen", "FUA of Rennes", "FUA of Cherbourg-en-Cotentin", 
                             "FUA of Le Mans", "FUA of Dijon", "FUA of Troyes", "FUA of Lorient", 
                             "FUA of Saint-Brieuc", "FUA of Brest", "FUA of Nantes", "FUA of Saint-Nazaire", 
                             "FUA of Orléans", "FUA of Bourges", "FUA of Poitiers", "FUA of Tours", 
                             "Charleroi (greater city)", "FUA of Reims", "FUA of Nancy", "FUA of Besançon", 
                             "Geneva", "FUA of Rouen", "FUA of Toulouse", "FUA of Béziers", 
                             "FUA of Perpignan", "FUA of Montpellier", "FUA of Martigues", 
                             "FUA of Avignon", "FUA of Nîmes", "Saarbrücken", "FUA of Metz", 
                             "Luxembourg", "Mouscron", "Mons (greater city)", "Kortrijk", 
                             "FUA of Lille", "FUA of Amiens", "FUA of Lens", "FUA of Douai", 
                             "FUA of Arras", "FUA of Calais", "FUA of Saint-Quentin", "FUA of Dunkerque", 
                             "FUA of Valenciennes", "FUA of Boulogne-sur-mer", "FUA of Angers", 
                             "FUA of La Rochelle", "FUA of Aix-en-Provence", "FUA of Nice", 
                             "FUA of Fréjus", "FUA of Marseille", "FUA of Toulon", "FUA of Grenoble", 
                             "FUA of Lyon", "FUA of Chambéry", "FUA of Annecy", "FUA of Valence", 
                             "FUA of Annemasse"))

names_wind <- data.frame(NAME_3 =c("Freiburg im Breisgau", "Offenburg", "FUA of Strasbourg", "FUA of Colmar", 
                                   "Basel", "FUA of Belfort", "FUA of Mulhouse", "FUA of Limoges", 
                                   "FUA of Bordeaux", "FUA of Pau", "FUA of Bayonne", "Pamplona/Iruña", 
                                   "Irun", "FUA of Clermont-Ferrand", "FUA of Roanne", "FUA of Saint-Étienne", 
                                   "FUA of the Greater City of Paris", "FUA of Chartres", "FUA of Le Havre", 
                                   "FUA of Caen", "FUA of Rennes", "FUA of Cherbourg-en-Cotentin", 
                                   "FUA of Le Mans", "FUA of Dijon", "FUA of Troyes", "FUA of Lorient", 
                                   "FUA of Saint-Brieuc", "FUA of Brest", "FUA of Nantes", "FUA of Saint-Nazaire", 
                                   "FUA of Orléans", "FUA of Bourges", "FUA of Poitiers", "FUA of Tours", 
                                   "Charleroi (greater city)", "FUA of Reims", "FUA of Nancy", "FUA of Besançon", 
                                   "Geneva", "FUA of Rouen", "FUA of Toulouse", "FUA of Béziers", 
                                   "FUA of Perpignan", "FUA of Montpellier", "FUA of Martigues", 
                                   "FUA of Avignon", "FUA of Nîmes", "Saarbrücken", "FUA of Metz", 
                                   "Luxembourg", "Mouscron", "Mons (greater city)", "Kortrijk", 
                                   "FUA of Lille", "FUA of Amiens", "FUA of Lens", "FUA of Douai", 
                                   "FUA of Arras", "FUA of Calais", "FUA of Saint-Quentin", "FUA of Dunkerque", 
                                   "FUA of Valenciennes", "FUA of Boulogne-sur-mer", "FUA of Angers", 
                                   "FUA of La Rochelle", "FUA of Aix-en-Provence", "FUA of Nice", 
                                   "FUA of Fréjus", "FUA of Marseille", "FUA of Toulon", "FUA of Grenoble", 
                                   "FUA of Lyon", "FUA of Chambéry", "FUA of Annecy", "FUA of Valence", 
                                   "FUA of Annemasse"))

wind_name <- data.frame(ID=1:76, 
                        lc_names =c("Freiburg im Breisgau", "Offenburg", "FUA of Strasbourg", "FUA of Colmar", 
                                    "Basel", "FUA of Belfort", "FUA of Mulhouse", "FUA of Limoges", 
                                    "FUA of Bordeaux", "FUA of Pau", "FUA of Bayonne", "Pamplona/Iruña", 
                                    "Irun", "FUA of Clermont-Ferrand", "FUA of Roanne", "FUA of Saint-Étienne", 
                                    "FUA of the Greater City of Paris", "FUA of Chartres", "FUA of Le Havre", 
                                    "FUA of Caen", "FUA of Rennes", "FUA of Cherbourg-en-Cotentin", 
                                    "FUA of Le Mans", "FUA of Dijon", "FUA of Troyes", "FUA of Lorient", 
                                    "FUA of Saint-Brieuc", "FUA of Brest", "FUA of Nantes", "FUA of Saint-Nazaire", 
                                    "FUA of Orléans", "FUA of Bourges", "FUA of Poitiers", "FUA of Tours", 
                                    "Charleroi (greater city)", "FUA of Reims", "FUA of Nancy", "FUA of Besançon", 
                                    "Geneva", "FUA of Rouen", "FUA of Toulouse", "FUA of Béziers", 
                                    "FUA of Perpignan", "FUA of Montpellier", "FUA of Martigues", 
                                    "FUA of Avignon", "FUA of Nîmes", "Saarbrücken", "FUA of Metz", 
                                    "Luxembourg", "Mouscron", "Mons (greater city)", "Kortrijk", 
                                    "FUA of Lille", "FUA of Amiens", "FUA of Lens", "FUA of Douai", 
                                    "FUA of Arras", "FUA of Calais", "FUA of Saint-Quentin", "FUA of Dunkerque", 
                                    "FUA of Valenciennes", "FUA of Boulogne-sur-mer", "FUA of Angers", 
                                    "FUA of La Rochelle", "FUA of Aix-en-Provence", "FUA of Nice", 
                                    "FUA of Fréjus", "FUA of Marseille", "FUA of Toulon", "FUA of Grenoble", 
                                    "FUA of Lyon", "FUA of Chambéry", "FUA of Annecy", "FUA of Valence", 
                                    "FUA of Annemasse"))


greenery <- rast("/Volumes/Seagate/Work/Tickets/twelfth/greenery/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
#Pv data
solar <- list.files("/Volumes/Seagate/Work/Tickets/twelfth/greenery/France_GISdata_LTAy_YearlyMonthlyTotals_GlobalSolarAtlas-v2_GEOTIFF/monthly", full.names=TRUE)
solar <- rast("/Volumes/Seagate/Work/Tickets/twelfth/greenery/France_GISdata_LTAy_YearlyMonthlyTotals_GlobalSolarAtlas-v2_GEOTIFF/GHI.tif")
#French regions
france <- vect("/Users/emiljanmrizaj/Downloads/ref-urau-2020-100k/URAU_RG_100K_2020_3035_FUA.shp")
france <- france[, 7]
#wind

#wind <- list.files("/Volumes/Seagate/Work/Tickets/twelfth/greenery/ORD45090", full.names=TRUE)
#wind2 <- rast("wind_all.tif")
#dd <- project(france, wind2)
#plot(crop(wind2$win_1, dd))
#France solar
france_wind <- read.csv("opsd-renewable_power_plants-2020-08-25/renewable_power_plants_FR.csv")
france_wind <- france_wind[france_wind$energy_source_level_2 == "Wind",]
france_wind <- france_wind[,c(1, 10, 11, 12, 14)]
france_wind <- france_wind[complete.cases(france_wind),]
wind_f <- st_read("wind_count.shp")
wind_f <- wind_f[,c(9, 15, 20)]
wind_f <- as.data.frame(wind_f)
wind_f$geometry<-NULL
wind_f<-wind_f[complete.cases(wind_f),]
wind_f <- aggregate(. ~ NAME_3, wind_f, mean)
##Wind raster
#wind <- rast("/Users/emiljanmrizaj/Downloads/fg_ens_mean_0.25deg_reg_v23.1e.nc")
#raster::writeRaster(wind1, "wind_europe.tif",overwrite=TRUE)
#wind_extract <- wind %>% terra::extract(france, weights=TRUE) 
#<------------- TIF METHOD ---------------------->
#wind <- rast("wind_europe.tif")
#france <- project(france, wind)
#wind <- crop(wind, france)
#wind_extract <- terra::extract(wind, france, weights=TRUE)
#wind_extract <- wind_extract[complete.cases(wind_extract),]
#wind_extract <- wind_extract %>% inner_join(names, by = "ID") %>%
#  arrange(ID) %>% select(-ID)
#with the .tif method
#wind_extract[,1][wind_extract$wind_europe == min(wind_extract$wind_europe)]<-1
#wind_extract <- wind_extract %>% group_by(lc_names) %>% summarise(across(starts_with("wind"), weighted.mean))
#agg_wind<-wind_extract %>% mutate(loc = str_match(lc_names, 'Paris')) %>% 
#  mutate(loc = ifelse(is.na(loc), lc_names, loc)) %>% 
#  mutate(loc = as.vector(loc)) %>% # str_match returns a matrix
#  group_by(loc) %>%
#  summarise(avg_wind = mean(wind_europe))
#names(agg_wind)[1]<-"NAME_3"
#wind_extract[,-1] <- missRanger(wind_extract[,-1], pmm.k = 3, num.trees = 100)


#with the .nc method
wind <- rast("/Users/emiljanmrizaj/Downloads/fg_ens_mean_0.25deg_reg_v24.0e.nc")
#france <- merge(france, names_wind, by="NAME_3")
france <- project(france, wind)
wind <- crop(wind, france)
wind_extract <- terra::extract(wind, france, weights=TRUE)
wind_extract[,-1][is.na(wind_extract[, -1])]<-0
wind_extract <- wind_extract %>% 
  mutate(across(.cols = where(is.numeric), 
                ~ replace(., . == 0, sample(.[. != 0], length(.[. == 0]), replace=T))))
#wind_extract <- wind_extract[complete.cases(wind_extract),]
wind_extract <- wind_extract %>% inner_join(wind_name, by = "ID") %>%
  arrange(ID) %>% select(-ID)

wind_extract <- wind_extract %>% group_by(lc_names) %>% summarise(across(starts_with("fg"), weighted.mean))
wind_extract <- data.frame(wind_extract)

test <- wind_extract %>% select_if(~max(.)>7)

wind_extract<-test %>% 
  mutate(across(.cols = where(is.numeric), 
                ~ replace(., . < 7, sample(.[. >7], length(.[. < 7]), replace=T))))

w_1 <- data.frame(wind_speed = rowMeans(wind_extract[,2:length(wind_extract)]))
w_1$cities <-wind_extract$lc_names
#agg_wind<-w_1 %>% mutate(loc = str_match(cities, 'Paris')) %>% 
#  mutate(loc = ifelse(is.na(loc), cities, loc)) %>% 
#  mutate(loc = as.vector(loc)) %>% # str_match returns a matrix
#  group_by(loc) %>%
#  summarise(avg_wind = mean(wind_speed))
agg_wind <- w_1
names(agg_wind)[2]<-"NAME_2"

#To filter the wind again follow these steps:
#Load into Qgis, merged the wind data and calculate the total number of points in polygon
#spatial join with the polygon after
#clean the zeros and filter everything with zero
#Now you can start creating sample values from the 0s
wind_stuff <- st_read("/Volumes/Seagate/Work/Tickets/twelfth/filter_wind.shp")
wind_stuff <- as.data.frame(wind_stuff)
wind_stuff$geometry <- NULL
wind_stuff <- wind_stuff[,c(7,12, 29, 30, 31, 32)]
wind_stuff[is.na(wind_stuff)]<-0
wind_stuff$ht_max <- as.numeric(wind_stuff$ht_max)
wind_stuff$ht_mat <- as.numeric(wind_stuff$ht_mat)
wind_stuff$ht_nacelle <- as.numeric(wind_stuff$ht_nacelle)
wind_stuff$diam_rotor <- as.numeric(wind_stuff$diam_rotor)
wind_stuff[,4][wind_stuff$ht_mat == "NaN"]<-0
wind_stuff[,5][wind_stuff$ht_nacelle == "NaN"]<-0
wind_stuff[,6][wind_stuff$diam_rotor == "NaN"]<-0
wind_stuff[wind_stuff$NUMPOINTS != 0, -c(1, 2)] <- wind_stuff[wind_stuff$NUMPOINTS != 0, -c(1, 2)] %>% 
  mutate(across(.cols = where(is.numeric), 
                ~ replace(., . == 0, sample(.[. != 0 & . > 50], length(.[. == 0]), replace=T))))
wind_stuff<-aggregate(. ~ NAME_2 + NUMPOINTS, wind_stuff, mean)

#testrun
wind_run <- st_read("sum_count_wind.shp")
#¢wind_run<-wind_run[,c(9, 15, 20, 40),]
wind_run <- as.data.frame(wind_run)
wind_run$geometry<-NULL
wind_run<-wind_run[complete.cases(wind_run$electrical),]
wind_run[,4][is.na(wind_run$diam_rotor)]<-0
wind_run[,4][wind_run$diam_rotor == "NaN"]<-0
wind_run$diam_rotor <- as.numeric(wind_run$diam_rotor)
wind_run <- wind_stuff[, -c(1, 2)] %>% 
  mutate(across(.cols = where(is.numeric), 
                ~ replace(., . == 0, sample(.[. != 0 & . > 50], length(.[. == 0]), replace=T))))
wind_run<-aggregate(. ~ NAME_3 + NUMPOINTS, wind_stuff, mean)
agg_wind <- inner_join(wind_stuff, agg_wind, by="NAME_2")
power <- function(wind_speed,rotor_sweep, numbers){
  x = wind_speed/(2.237)
  #air_density = 1.23
  y = pi*(rotor_sweep/2)^2
  z = (3.2 * y *(x^3) * numbers)
  return(z)
}
agg_wind <- agg_wind %>% mutate(power_kWh = power(agg_wind$wind_speed, agg_wind$diam_rotor, agg_wind$NUMPOINTS))
agg_wind <- agg_wind[,-c(3:7)]
#w_wind <- read.csv("w_test.csv")
#w_wind$X <- NULL

wind_run[wind_run$NAME_3 == "Rennes",][,4] <- 90
agg_wind <- inner_join(wind_stuff, agg_wind, by="NAME_3")
power <- function(wind_speed,rotor_sweep, numbers){
  x = wind_speed/(2.237)
  #air_density = 1.23
  y = pi*(rotor_sweep/2)^2
  z = (3.2 * y *(x^3) * numbers)
  return(z)
}

agg_wind <- agg_wind %>% mutate(power_kWh = power(agg_wind$avg_wind, agg_wind$diam_rotor, agg_wind$NUMPOINTS))
#agg_wind2 <- agg_wind[, -c(2:5)]
#agg_wind2 <- aggregate(. ~ NAME_3, agg_wind2, sum)
#agg_wind3 <- agg_wind[,c(1, 3)] %>% distinct() %>% arrange()
#agg_wind <- inner_join(agg_wind2, agg_wind3, by="NAME_3")

#solar
france_solar <- read.csv("opsd-renewable_power_plants-2020-08-25/renewable_power_plants_FR.csv")
france_solar<- france_solar[france_solar$energy_source_level_2 == "Solar",]
france_solar <- france_solar[,c(1, 10, 11, 12, 14)]
france_solar <- france_solar[complete.cases(france_solar),]
solar_f <- st_read("count_solar.shp")
solar_f <- solar_f[,c(9, 14, 17)]
solar_f <- as.data.frame(solar_f)
solar_f$geometry <- NULL
solar_f[, 2][is.na(solar_f$elctrc_)]<-0
solar_f<-aggregate(. ~ NAME_3+NUMPOINTS, solar_f, mean)
solar_f<-solar_f %>% mutate(loc = str_match(NAME_3, 'Paris')) %>% 
  mutate(loc = ifelse(is.na(loc), NAME_3, loc)) %>% 
  mutate(loc = as.vector(loc)) %>% select(-1)
solar_f<-aggregate(. ~ loc, solar_f, mean)

# <----- You can get solar also
#Get the heat in homes
#Things to do
#<------ Test out the extra data downloaded; Get a total percentage of the data
#Represent the data as a sum



#Data manipulation
france_data <- project(france, solar)
france<-france[,-c(1, 2, 4, 5, 6, 7, 8, 10, 11, 12, 13)]
sol_extract <- solar %>% terra::extract(france, weights=TRUE) 
sol_extract<-sol_extract[complete.cases(sol_extract),]
data_values <- sol_extract %>% inner_join(names, by = "ID") %>%
  arrange(ID) %>% select(-ID)

agg_france <- data_values %>% group_by(lc_names) %>% summarise(across(starts_with("GHI"), weighted.mean))
#Summarise for Paris
#agg_france<-agg_france %>% mutate(loc = str_match(lc_names, 'Paris')) %>% 
#  mutate(loc = ifelse(is.na(loc), lc_names, loc)) %>% 
#  mutate(loc = as.vector(loc)) %>% # str_match returns a matrix
#  group_by(loc) %>%
#  summarise(avg_GHI = mean(GHI))
agg_france <- inner_join(agg_france, department, by="lc_names")

#greenery
france <- project(france, greenery)
green_crop <- crop(greenery, france)
green_extract <- terra::extract(greenery, france)
green_extract<-green_extract %>% count(ID, LABEL3)
green_extract<-green_extract[complete.cases(green_extract),]
green_extract$factor_id <- as.numeric(factor(green_extract$LABEL3))
green_extract$LABEL3 <- as.character(green_extract$LABEL3)
green_extract$LABEL3 <- as.factor(green_extract$LABEL3)


land <- c("Agro-forestry areas", 
          "Broad-leaved forest", "Complex cultivation patterns", 
          "Coniferous forest", "Fruit trees and berry plantations","Land principally occupied by agriculture, with significant areas of natural vegetation", "Mixed forest", 
          "Natural grasslands", "Non-irrigated arable land", 
          "Olive groves", "Pastures","Permanently irrigated land", "Rice fields","Sclerophyllous vegetation",
          "Sparsely vegetated areas", "Transitional woodland-shrub", 
          "Vineyards","Green urban areas")

water <- c("Bare rocks", "Beaches, dunes, sands",  "Coastal lagoons",  "Estuaries", 
           "Glaciers and perpetual snow", 
           "Inland marshes", "Intertidal flats", "Moors and heathland", 
           "Peat bogs", 
           "Salines", "Salt marshes", "Sea and ocean","Water bodies", "Water courses")

other <- c(
  "Airports",  
  "Burnt areas", "Construction sites", "Continuous urban fabric", 
  "Discontinuous urban fabric", "Dump sites", "Industrial or commercial units", 
  "Mineral extraction sites", "NODATA", 
  "Port areas",  "Road and rail networks and associated land", "Sport and leisure facilities" )

green_extract$landcover_class <- ifelse(green_extract$LABEL3 %in% land,"Agriculture/Land" ,ifelse(green_extract$LABEL3 %in% water, "WaterBodies",ifelse(green_extract$LABEL3 %in% other, "Other", "NA" ) ))

pland_pred <- green_extract %>% 
  count(ID, landcover_class) %>% 
  group_by(ID) %>% 
  mutate(pland = n / sum(n)) %>% 
  ungroup() %>% 
  select(-n) %>% 
  # remove NAs after tallying so pland is relative to total number of cells
  filter(!is.na(landcover_class))

green_france <- inner_join(pland_pred,names, by="ID")
green_france = green_france %>% pivot_wider(names_from = landcover_class, values_from = pland)
sun_land <- inner_join(agg_france, green_france, by = 'lc_names')
