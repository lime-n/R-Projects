#select for specific disaster

disasterlocations <- disasterlocations %>% unnest_wider(centroid)
names(disasterlocations)[16:17]<-c("Longitude", "Latitude")
disasterlocations$disasterno<-paste0(disasterlocations$disasterno,"-",disasterlocations$iso3)
disaster_type1 <- disasterlocations[,c(2, 8,9, 14:18)]
disaster_type1<-disaster_type1[!duplicated(disaster_type1[,c(2, 5)]),]


dis_locations <- st_as_sf(disaster_type1, coords=c("Longitude", "Latitude"), crs=4326)
write_sf(dis_locations, "dis_locations.shp", layer_options = "ENCODING=UTF-8")
#


#upload the data
area_cities <- st_read("geom_dis.shp")


a_area <- area_cities[,c(1, 3, 10)]
data_area <- st_join(dis_locations,a_area)

data_dis <- read_csv("data_disaster.csv")
data_dis <- disasteryear
data_dis$X1<-NULL
data_dis$`Unnamed: 0`<-NULL
data_dis$year <- NULL
data_dis$`Insured Damages ('000 US$)`<-NULL
data_dis$`Reconstruction Costs ('000 US$)`<-NULL
data_dis[,2:7][is.na(data_dis[,2:7])]<-0
agg_disaster<-aggregate(. ~ disasterno + `Disaster Subtype` + `Disaster Subgroup`, data_dis, sum)
agg_disaster <- agg_disaster[rowSums(agg_disaster[,4:9])>0,]

names(area_cities)[8]<-"disasterno"
test_year <- inner_join(area_cities,agg_disaster, by="disasterno")
centroid_data <- st_centroid(test_year)
st_write(centroid_data, "centroid_disaster.shp")

Work_cent<- as.data.frame(centroid_data)
Work_cent <- Work_cent[,-c(2,4,5,6 ,20)]
Work_cent$area <- Work_cent$area/1e+6
Work_cent$perimeter <- NULL
#take the sum to accumulate data
names(Work_cent)[1:3] <- c("country", "city", "disastertype")
names(Work_cent)[5]<-"continent"
names(Work_cent)[7:8] <- c("Disaster_Subtype", "Disaster_Subgroup")
Work_cent$disasterno <- NULL
Work_cent1 <- aggregate(. ~ area + country + city + disastertype + continent + Disaster_Subtype+Disaster_Subgroup, Work_cent, sum)

Work_cent1 <- Work_cent1 %>% 
  group_by(disastertype, Disaster_Subtype,Disaster_Subgroup,country, `Total Damages ('000 US$)`, `Total Affected`, `No Homeless`, `No Affected`,`No Injured`,`Total Deaths`) %>% 
  mutate(Proportion_of_area = area/sum(area)) %>% 
  ungroup() %>% 
  mutate(across(8:13,
                ~.*Proportion_of_area, 
                .names = "{col}_")) %>% 
  select(-`Total Damages ('000 US$)`, -`Total Affected`, -`No Homeless`, -`No Affected`,-`No Injured`,-`Total Deaths`) %>%
  ungroup()

Work_cent1<-Work_cent1[!duplicated(Work_cent1[,c(9:13)]),]
Work_cent1$`No Homeless_` <- NULL

earthquakes <- Work_cent1[Work_cent1$disastertype == "earthquake",-10]
earthquakes<-earthquakes[earthquakes$`Total Damages ('000 US$)_`>0 & earthquakes$`No Affected_`>0 &earthquakes$`No Injured_`>0 &earthquakes$`Total Deaths_`>0,]

flood <- Work_cent1[Work_cent1$disastertype == "flood",-10]
flood<-flood[flood$`Total Damages ('000 US$)_`>0 &  flood$`No Affected_`>0 &flood$`No Injured_`>0 &flood$`Total Deaths_`>0,]

storm <- Work_cent1[Work_cent1$disastertype == "storm",-10]
storm<-storm[storm$`Total Damages ('000 US$)_`>0 &  storm$`No Affected_`>0 &storm$`No Injured_`>0 &storm$`Total Deaths_`>0,]

landslide <- Work_cent1[Work_cent1$disastertype == "landslide",-10]
landslide<-landslide[landslide$`Total Damages ('000 US$)_`>0 &  landslide$`No Affected_`>0 &landslide$`No Injured_`>0 &landslide$`Total Deaths_`>0,]

volcanic <- Work_cent1[Work_cent1$disastertype == "volcanic activity",-10]
volcanic<-volcanic[volcanic$`Total Damages ('000 US$)_`>0 &  volcanic$`No Affected_`>0 &volcanic$`No Injured_`>0 &volcanic$`Total Deaths_`>0,]

mass_movement <- Work_cent1[Work_cent1$disastertype == "mass movement (dry)",-10]
mass_movement<-mass_movement[mass_movement$`Total Damages ('000 US$)_`>0 &  mass_movement$`No Affected_`>0 &mass_movement$`No Injured_`>0 &mass_movement$`Total Deaths_`>0,]

drought <- Work_cent1[Work_cent1$disastertype == "drought",-10]
drought<-drought[drought$`Total Damages ('000 US$)_`>0 & drought$`No Affected_`>0 &drought$`No Injured_`>0 &drought$`Total Deaths_`>0,]

temperature <- Work_cent1[Work_cent1$disastertype == "extreme temperature",-10]
temperature<-temperature[temperature$`Total Damages ('000 US$)_`>0 &  temperature$`No Affected_`>0 &temperature$`No Injured_`>0 &temperature$`Total Deaths_`>0,]

readr::write_excel_csv(earthquakes, "earthquake.csv",)

readr::write_excel_csv(flood, "flood.csv")

readr::write_excel_csv(storm, "storm.csv")

readr::write_excel_csv(landslide, "landslide.csv")

readr::write_excel_csv(temperature, "temperature.csv")

readr::write_excel_csv(volcanic, "volcanic.csv")


Work_eu <- Work_cent1[Work_cent1$continent == "Europe",]

earthquakes <- Work_eu[Work_eu$disastertype == "earthquake",-10]
earthquakes<-earthquakes[earthquakes$`Total Damages ('000 US$)_`>0 & earthquakes$`No Affected_`>0 &earthquakes$`No Injured_`>0 &earthquakes$`Total Deaths_`>0,]

flood <- Work_eu[Work_eu$disastertype == "flood",-10]
flood<-flood[flood$`Total Damages ('000 US$)_`>0 & flood$`No Affected_`>0 &flood$`No Injured_`>0 &flood$`Total Deaths_`>0,]

storm <- Work_eu[Work_eu$disastertype == "storm",-10]
storm<-storm[storm$`Total Damages ('000 US$)_`>0 & storm$`No Affected_`>0 &storm$`No Injured_`>0 &storm$`Total Deaths_`>0,]

landslide <- Work_eu[Work_eu$disastertype == "landslide",-10]
landslide<-landslide[landslide$`Total Damages ('000 US$)_`>0 & landslide$`No Affected_`>0 &landslide$`No Injured_`>0 &landslide$`Total Deaths_`>0,]

volcanic <- Work_eu[Work_eu$disastertype == "volcanic activity",-10]
volcanic<-volcanic[volcanic$`Total Damages ('000 US$)_` >0 & volcanic$`Total Affected_`>0 & volcanic$`No Affected_` >0 & volcanic$`No Injured_`>0 & volcanic$`Total Deaths_` >0,]

mass_movement <- Work_eu[Work_eu$disastertype == "mass movement (dry)",-10]
mass_movement<-mass_movement[mass_movement$`Total Damages ('000 US$)_` >0 & mass_movement$`Total Affected_`>0 & mass_movement$`No Affected_` >0 & mass_movement$`No Injured_`>0 & mass_movement$`Total Deaths_` >0,]

drought <- Work_eu[Work_eu$disastertype == "drought",-10]
drought<-drought[drought$`Total Damages ('000 US$)_`>0 & drought$`No Affected_`>0 &drought$`No Injured_`>0 &drought$`Total Deaths_`>0,]

temperature <- Work_eu[Work_eu$disastertype == "extreme temperature",-10]
temperature<-temperature[temperature$`Total Damages ('000 US$)_`>0 & temperature$`No Affected_`>0 &temperature$`No Injured_`>0 &temperature$`Total Deaths_`>0,]


readr::write_excel_csv(earthquakes, "earthquake.eu.csv",)

readr::write_excel_csv(flood, "flood.eu.csv")

readr::write_excel_csv(storm, "storm.eu.csv")

readr::write_excel_csv(landslide, "landslide.eu.csv")

readr::write_excel_csv(temperature, "temperature.eu.csv")

data_disasters_test <- Work_cent1[Work_cent1$`Total Damages ('000 US$)_` >0 & Work_cent1$`Total Affected_`>0 & Work_cent1$`No Affected_` >0 & Work_cent1$`No Injured_`>0 & Work_cent1$`Total Deaths_` >0,]

data_disasters_test <- disasterlocations %>% count(country,continent,disastertype)
data_disasters_test <- data_disasters_test %>% pivot_wider(names_from = "disastertype", values_from = "n", values_fill=list(n=0))
write.csv(data_disasters_test, "data_disasters_test.csv")
