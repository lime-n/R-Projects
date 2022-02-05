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
test_year <- inner_join(area_cities,dis_year, by="disasterno")
centroid_data <- st_centroid(test_year)
st_write(centroid_data, "centroid_disaster.shp")

Work_cent<- as.data.frame(centroid_data)
Work_cent <- Work_cent[,-c(2,4,5,6 ,20)]
Work_cent$area <- Work_cent$area/1e+6
Work_cent$perimeter <- NULL
#take the sum to accumulate data
names(Work_cent)[1:3] <- c("country", "city", "disastertype")
names(Work_cent)[5]<-"continent"
names(Work_cent)[13:14] <- c("Disaster_Subtype", "Disaster_Subgroup")
Work_cent$disasterno <- NULL
Work_cent1 <- aggregate(. ~ area + country + city + disastertype + continent + Disaster_Subtype+Disaster_Subgroup, Work_cent, sum)

century1 <- Work_cent
century1$`No Homeless`<-NULL

century2 <- century1 %>% 
  group_by(disastertype, Disaster_Subtype,Disaster_Subgroup,country, `Total Damages ('000 US$)`, `Total Affected`,  `No Affected`,`No Injured`,`Total Deaths`) %>% 
  mutate(Proportion_of_area = area/sum(area)) %>% 
  ungroup() %>% 
  mutate(across(7:11,
                ~.*Proportion_of_area, 
                .names = "{col}_")) %>% 
  select(-`Total Damages ('000 US$)`, -`Total Affected`, -`No Affected`,-`No Injured`,-`Total Deaths`) %>%
  ungroup()

Work_cent1$Proportion_of_area <- NULL
Work_cent2 <- Work_cent1 %>% 
  group_by(disastertype, Disaster_Subtype,Disaster_Subgroup,country,  `Total Affected_`,  `No Affected_`) %>% 
  mutate(Proportion_of_area = area/sum(area)) %>% 
  ungroup() %>% 
  mutate(across(8:12,
                ~.*Proportion_of_area, 
                .names = "{col}_")) %>% 
  select(-`Total Damages ('000 US$)_`, -`Total Affected_`, -`No Affected_`,-`No Injured_`,-`Total Deaths_`) %>%
  ungroup()

Work_cent2<-Work_cent2[!duplicated(Work_cent2[,c(9:13)]),]
Work_cent1$`No Homeless_` <- NULL

#Try the data again
century4$area <- century4$area/1e+6

earthquakes <- century4[century4$disastertype == "earthquake",]
earthquakes<-earthquakes[earthquakes$`Total Damages ('000 US$)_` >0 & earthquakes$`Total Affected_`>0 & earthquakes$`No Affected_` >0 & earthquakes$`No Injured_`>0 & earthquakes$`Total Deaths_` >0,]

flood <- century4[century4$disastertype == "flood",]
flood<-flood[flood$`Total Damages ('000 US$)_` >0 & flood$`Total Affected_`>0 & flood$`No Affected_` >0 & flood$`No Injured_`>0 & flood$`Total Deaths_` >0,]

storm <- century4[century4$disastertype == "storm",]
storm<-storm[storm$`Total Damages ('000 US$)_` >0 & storm$`Total Affected_`>0 & storm$`No Affected_` >0 & storm$`No Injured_`>0 & storm$`Total Deaths_` >0,]

landslide <- century4[century4$disastertype == "landslide",]
landslide<-landslide[landslide$`Total Damages ('000 US$)_` >0 & landslide$`Total Affected_`>0 & landslide$`No Affected_` >0 & landslide$`No Injured_`>0 & landslide$`Total Deaths_` >0,]

volcanic <- century4[century4$disastertype == "volcanic activity",]
volcanic<-volcanic[volcanic$`Total Damages ('000 US$)_` >0 & volcanic$`Total Affected_`>0 & volcanic$`No Affected_` >0 & volcanic$`No Injured_`>0 & volcanic$`Total Deaths_` >0,]

mass_movement <- century4[century4$disastertype == "mass movement (dry)",]
mass_movement<-mass_movement[mass_movement$`Total Damages ('000 US$)_` >0 & mass_movement$`Total Affected_`>0 & mass_movement$`No Affected_` >0 & mass_movement$`No Injured_`>0 & mass_movement$`Total Deaths_` >0,]

drought <- century4[century4$disastertype == "drought",]
drought<-drought[drought$`Total Damages ('000 US$)_`>0 & drought$`Total Affected_`>0& drought$`No Affected_`>0 &drought$`No Injured_`>0 &drought$`Total Deaths_`>0,]

temperature <- century4[century4$disastertype == "extreme temperature",]
temperature<-temperature[temperature$`Total Damages ('000 US$)_` >0 & temperature$`Total Affected_`>0 & temperature$`No Affected_` >0 & temperature$`No Injured_`>0 & temperature$`Total Deaths_` >0,]

readr::write_excel_csv(earthquakes, "earthquake.csv",)

readr::write_excel_csv(flood, "flood.csv")

readr::write_excel_csv(storm, "storm.csv")

readr::write_excel_csv(landslide, "landslide.csv")

readr::write_excel_csv(temperature, "temperature.csv")

readr::write_excel_csv(volcanic, "volcanic.csv")

readr::write_excel_csv(mass_movement, "mass_movement.csv")


Work_eu <- Work_cent1[Work_cent1$continent == "Europe",]

eu_century <- century4[century4$continent == "Europe",]

earthquakes <- eu_century[eu_century$disastertype == "earthquake",]
earthquakes<-earthquakes[earthquakes$`Total Damages ('000 US$)_`>0 & earthquakes$`Total Affected_`>0& earthquakes$`No Affected_`>0 &earthquakes$`No Injured_`>0 &earthquakes$`Total Deaths_`>0,]

flood <- eu_century[eu_century$disastertype == "flood",]
flood<-flood[flood$`Total Damages ('000 US$)_`>0 & flood$`Total Affected_`>0& flood$`No Affected_`>0 &flood$`No Injured_`>0 &flood$`Total Deaths_`>0,]

storm <- eu_century[eu_century$disastertype == "storm",]
storm<-storm[storm$`Total Damages ('000 US$)_`>0 & storm$`Total Affected_`>0& storm$`No Affected_`>0 &storm$`No Injured_`>0 &storm$`Total Deaths_`>0,]

landslide <- eu_century[eu_century$disastertype == "landslide",]
landslide<-landslide[landslide$`Total Damages ('000 US$)_`>0 & landslide$`Total Affected_`>0& landslide$`No Affected_`>0 &landslide$`No Injured_`>0 &landslide$`Total Deaths_`>0,]

volcanic <- eu_century[eu_century$disastertype == "volcanic activity",]
volcanic<-volcanic[volcanic$`Total Damages ('000 US$)_` >0 & volcanic$`Total Affected_`>0& volcanic$`Total Affected_`>0 & volcanic$`No Affected_` >0 & volcanic$`No Injured_`>0 & volcanic$`Total Deaths_` >0,]

mass_movement <- eu_century[eu_century$disastertype == "mass movement (dry)",]
mass_movement<-mass_movement[mass_movement$`Total Damages ('000 US$)_` >0 & mass_movement$`Total Affected_`>0& mass_movement$`Total Affected_`>0 & mass_movement$`No Affected_` >0 & mass_movement$`No Injured_`>0 & mass_movement$`Total Deaths_` >0,]

drought <- eu_century[eu_century$disastertype == "drought",]
drought<-drought[drought$`Total Damages ('000 US$)_`>0 & drought$`Total Affected_`>0& drought$`No Affected_`>0 &drought$`No Injured_`>0 &drought$`Total Deaths_`>0,]

temperature <- eu_century[eu_century$disastertype == "extreme temperature",]
temperature<-temperature[temperature$`Total Damages ('000 US$)_`>0 & temperature$`Total Affected_`>0& temperature$`No Affected_`>0 &temperature$`No Injured_`>0 &temperature$`Total Deaths_`>0,]


readr::write_excel_csv(earthquakes, "earthquake.eu.csv",)

readr::write_excel_csv(flood, "flood.eu.csv")

readr::write_excel_csv(storm, "storm.eu.csv")

#readr::write_excel_csv(landslide, "landslide.eu.csv")

readr::write_excel_csv(temperature, "temperature.eu.csv")

data_disasters_test <- Work_cent1[Work_cent1$`Total Damages ('000 US$)_` >0 & Work_cent1$`Total Affected_`>0 & Work_cent1$`No Affected_` >0 & Work_cent1$`No Injured_`>0 & Work_cent1$`Total Deaths_` >0,]

data_disasters_test <- disasterlocations %>% count(country,continent,disastertype)
data_disasters_test <- data_disasters_test %>% pivot_wider(names_from = "disastertype", values_from = "n", values_fill=list(n=0))
write.csv(data_disasters_test, "data_disasters_test.csv")
#####Countries

earthquakes <- century5[century5$disastertype == "earthquake",]
earthquakes<-earthquakes[earthquakes$`Total Damages ('000 US$)_` >0 & earthquakes$`Total Affected_`>0 & earthquakes$`No Affected_` >0 & earthquakes$`No Injured_`>0 & earthquakes$`Total Deaths_` >0,]

flood <- century5[century5$disastertype == "flood",]
flood<-flood[flood$`Total Damages ('000 US$)_` >0 & flood$`Total Affected_`>0 & flood$`No Affected_` >0 & flood$`No Injured_`>0 & flood$`Total Deaths_` >0,]

storm <- century5[century5$disastertype == "storm",]
storm<-storm[storm$`Total Damages ('000 US$)_` >0 & storm$`Total Affected_`>0 & storm$`No Affected_` >0 & storm$`No Injured_`>0 & storm$`Total Deaths_` >0,]

landslide <- century5[century5$disastertype == "landslide",]
landslide<-landslide[landslide$`Total Damages ('000 US$)_` >0 & landslide$`Total Affected_`>0 & landslide$`No Affected_` >0 & landslide$`No Injured_`>0 & landslide$`Total Deaths_` >0,]

volcanic <- century5[century5$disastertype == "volcanic activity",]
volcanic<-volcanic[volcanic$`Total Damages ('000 US$)_` >0 & volcanic$`Total Affected_`>0 & volcanic$`No Affected_` >0 & volcanic$`No Injured_`>0 & volcanic$`Total Deaths_` >0,]

mass_movement <- century5[century5$disastertype == "mass movement (dry)",]
mass_movement<-mass_movement[mass_movement$`Total Damages ('000 US$)_` >0 & mass_movement$`Total Affected_`>0 & mass_movement$`No Affected_` >0 & mass_movement$`No Injured_`>0 & mass_movement$`Total Deaths_` >0,]

drought <- century5[century5$disastertype == "drought",]
drought<-drought[drought$`Total Damages ('000 US$)_`>0 & drought$`Total Affected_`>0& drought$`No Affected_`>0 &drought$`No Injured_`>0 &drought$`Total Deaths_`>0,]

temperature <- century5[century5$disastertype == "extreme temperature",]
temperature<-temperature[temperature$`Total Damages ('000 US$)_` >0 & temperature$`Total Affected_`>0 & temperature$`No Affected_` >0 & temperature$`No Injured_`>0 & temperature$`Total Deaths_` >0,]


readr::write_excel_csv(earthquakes, "country_earthquake.csv",)

readr::write_excel_csv(flood, "country_flood.csv")

readr::write_excel_csv(storm, "country_storm.csv")

readr::write_excel_csv(landslide, "country_landslide.csv")

readr::write_excel_csv(temperature, "country_temperature.csv")

readr::write_excel_csv(volcanic, "country_volcanic.csv")

readr::write_excel_csv(mass_movement, "country_mass_movement.csv")

