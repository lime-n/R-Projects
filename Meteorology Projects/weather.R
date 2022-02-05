#Gather stations names
library(dplyr)
library(tidyr)
library(sf)
a = data.table::fread("Stations/TG/stations.txt", sep="")
stations <- a[-c(1:16)]
sp <- stations %>% 
  mutate(x = strsplit(as.character(`EUROPEAN CLIMATE ASSESSMENT & DATASET (ECA&D), file created on 14-06-2021`), ",")) %>%
  unnest(x)
stations <- data.frame(matrix(sp$x, ncol =6, byrow=TRUE))

left_stations <- stations[1:301,]
right_stations <- stations[302:nrow(stations),]
right_stations <- right_stations[, c(2, 3, 4, 5, 6, 1)]
names(right_stations) <- c("X1", "X2", "X3", "X4", "X5", "X6")
stations <- rbind(left_stations, right_stations)
#correct the amnomaly values
stations[301,4] <- stations[301,5]
stations[301,5] <- stations[301,6]

#convert coordinates
stations <- stations[,c(2,3,4, 5)]
names(stations) <- c("City_station","Country_station", "Latitude", "Longitude")
stations$Latitude <- parzer::parse_lat(stations$Latitude)
stations$Longitude <- parzer::parse_lat(stations$Longitude)
stations$Country_station <- gsub(" ", "", stations$Country_station)
stations$City_station <- gsub(" ", "", stations$City_station)
stations<- stations[complete.cases(stations),]
stations_shp <- st_as_sf(stations, coords=c("Longitude", "Latitude"), crs=4326)


#Load EU countries
stp <- read_sf("Data/ref-lau-2019-01m/LAU_RG_01M_2019_4326.shp/LAU_RG_01M_2019_4326.shp")
#stp$CNTR_CODE[stp$CNTR_CODE %in% 
#                c("CZ" ,"AT", "AL" ,"DE", "BE" ,"EL" ,"DK" ,
#                  "EE" ,"BG", "ES" ,"CH" ,"FR" ,"FI", "CY",
#                  "HU" ,"HR", "IT" ,"IE" ,"IS", "PL", "NL",
#                  "NO", "LI", "LT" ,"LU" ,"LV", "MK", "MT",
#                  "PT", "RO","SK" ,"RS", "SE", "UK" ,"SI")] <- 
#  c("CZECHIA", "AUSTRIA", "ALBANIA", "GERMANY", "BELGIUM", 
#    "GREECE", "DENMARK", "ESTONIA", "BULGARIA", "SPAIN", 
#    "SWITZERLAND", "FRANCE", "FINLAND", "CYPRUS", "HUNGARY", 
#    "CROATIA", "ITALY", "IRELAND", "ICELAND", "POLAND", 
#    "NETHERLANDS", "NORWAY", "LIECHENSTEIN", "LITHUANIA",
#    "LUXEMBOURG", "LATVIA", "NORTH MACEDONIA", "MALTA", 
#    "PORTUGAL", "ROMANIA", "SLOVAKIA", "SERBIA", "SWEDEN",
#    "UNITED KINGDOM", "SLOVENIA")

stp$CNTR_CODE[stp$CNTR_CODE == "CZ"] <- "CZECHIA"
stp$CNTR_CODE[stp$CNTR_CODE == "AT"] <- "AUSTRIA"
stp$CNTR_CODE[stp$CNTR_CODE == "AL"] <- "ALBANIA"
stp$CNTR_CODE[stp$CNTR_CODE == "DE"] <- "GERMANY"
stp$CNTR_CODE[stp$CNTR_CODE == "BE"] <- "BELGIUM"
stp$CNTR_CODE[stp$CNTR_CODE == "EL"] <- "GREECE"
stp$CNTR_CODE[stp$CNTR_CODE == "DK"] <- "DENMARK"
stp$CNTR_CODE[stp$CNTR_CODE == "EE"] <- "ESTONIA"
stp$CNTR_CODE[stp$CNTR_CODE == "BG"] <- "BULGARIA"
stp$CNTR_CODE[stp$CNTR_CODE == "ES"] <- "SPAIN"
stp$CNTR_CODE[stp$CNTR_CODE == "CH"] <- "SWITZERLAND"
stp$CNTR_CODE[stp$CNTR_CODE == "FR"] <- "FRANCE"
stp$CNTR_CODE[stp$CNTR_CODE == "FI"] <- "FINLAND"
stp$CNTR_CODE[stp$CNTR_CODE == "CY"] <- "CYPRUS"
stp$CNTR_CODE[stp$CNTR_CODE == "HU"] <- "HUNGARY"
stp$CNTR_CODE[stp$CNTR_CODE == "HR"] <- "CROATIA"
stp$CNTR_CODE[stp$CNTR_CODE == "IT"] <- "ITALY"
stp$CNTR_CODE[stp$CNTR_CODE == "IE"] <- "IRELAND"
stp$CNTR_CODE[stp$CNTR_CODE == "IS"] <- "ICELAND"
stp$CNTR_CODE[stp$CNTR_CODE == "PL"] <- "POLAND"
stp$CNTR_CODE[stp$CNTR_CODE == "NL"] <- "NETHERLANDS"
stp$CNTR_CODE[stp$CNTR_CODE == "NO"] <- "NORWAY"
stp$CNTR_CODE[stp$CNTR_CODE == "LI"] <- "LIECHENSTEIN"
stp$CNTR_CODE[stp$CNTR_CODE == "LT"] <- "LITHUANIA"
stp$CNTR_CODE[stp$CNTR_CODE == "LU"] <- "LUXEMBOURG"
stp$CNTR_CODE[stp$CNTR_CODE == "LV"] <- "LATVIA"
stp$CNTR_CODE[stp$CNTR_CODE == "MK"] <- "NORTH MACEDONIA"
stp$CNTR_CODE[stp$CNTR_CODE == "MT"] <- "MALTA"
stp$CNTR_CODE[stp$CNTR_CODE == "PT"] <- "PORTUGAL"
stp$CNTR_CODE[stp$CNTR_CODE == "RO"] <- "ROMANIA"
stp$CNTR_CODE[stp$CNTR_CODE == "SK"] <- "SLOVAKIA"
stp$CNTR_CODE[stp$CNTR_CODE == "RS"] <- "SERBIA"
stp$CNTR_CODE[stp$CNTR_CODE == "SE"] <- "SWEDEN"
stp$CNTR_CODE[stp$CNTR_CODE == "UK"] <- "UNITED KINGDOM"
stp$CNTR_CODE[stp$CNTR_CODE == "SI"] <- "SLOVENIA"
####

write_sf(stp, "eu.shp", layer_options = "ENCODING=UTF-8")


#cities2 <- data.frame(cities=stp$LAU_NAME, countries=stp$CNTR_CODE, population = stp$POP_2019)
#load file
stp <- st_read("eu.shp")
#stp <- stp[,c(2, 4)]
#for populations
stp <- stp[,c(2, 4, 5)]
####
#Collect capital cities only and coerce as point data
#Austria = Wien = Vienna
Vienna <- stp[stp$LAU_NAME == "Wien",]
#Czechia = Praha = Prague
Prague <- stp[stp$LAU_NAME == "Praha",]; Prague <- Prague[1,]
#Albania - Tirana - Tirane
Tirane <- stp[stp$LAU_NAME == "Tiranë",]
#Germany = Berlin
Berlin <- stp[stp$LAU_NAME == "Berlin, Stadt",]
#Belgium - Brussels
Brussels <- stp[stp$LAU_NAME == "Bruxelles / Brussel",]
#Greece - Athens
Athens <- stp[stp$LAU_NAME == "Ψευδοδημοτική Κοινότητα Αθηναίων",]
#Denmark = Copenhagen
Copenhagen <- stp[stp$LAU_NAME == "København",]
#Estonia = Tallinn
Tallinn <- stp[stp$LAU_NAME == "Tallinn",]
#Bulgaria = Sofia
Sofia <- stp[stp$LAU_NAME == "Столична",]
#Spain = Madrid
Madrid <- stp[stp$LAU_NAME == "Madrid",]
#Switzerland = Bern
Bern = stp[stp$LAU_NAME == "Bern",]
#France = Paris
Paris = stp[stp$LAU_NAME == "Paris",]
#Finland = Helsinki
Helsinki = stp[stp$LAU_NAME == "Helsinki / Helsingfors",]
#Cyprus = Nicosia
Nicosia = stp[stp$LAU_NAME == "Λευκωσία",]
#Hungary = Budapest
Budapest = stp[stp$LAU_NAME == "Budapest",]
#Croatia = Zagreb
Zagreb <- stp[stp$LAU_NAME == "Grad Zagreb",]
#Italy = Rome
Rome <- stp[stp$LAU_NAME == "Roma",];Rome<- Rome[1,]
#Ireland = Dublin
Dublin <- st_read("Dublin+pop.shp")
Dublin <- Dublin[, c(2, 4, 5)]
#Iceland = Reykjavik
Reykjavik = stp[78571,]
#Poland = Warsaw
Warsaw = stp[stp$LAU_NAME == "Warszawa",]
#Netherlands = Amsterdam
Amsterdam = stp[stp$LAU_NAME == "Amsterdam",]
#Norway = Oslo
Oslo = stp[stp$LAU_NAME == "Oslo kommune",]
#Liechenstein = Vaduz
Vaduz = stp[stp$LAU_NAME == "Vaduz",]
#Lithuania = Vilnius
Vilnius = stp[stp$LAU_NAME == "Vilniaus miesto savivaldybė",]
#Luxembourg = luxembourg
Luxembourg = stp[stp$LAU_NAME == "Luxembourg",]
#Latvia = Riga
Riga = stp[stp$LAU_NAME == "Rīga",]
#North Macedonia = Skopje
Skopje = stp[stp$LAU_NAME %in% c("Aerodrom","Butel","Gazi Baba","Đorče Petrov",
                                 "Karpoš", "Kisela Voda", "Saraj", "Centar", "Čair", "Šuto Orizari"),]
#Portugal = Lisbon
Lisbon <- st_read("Lisbon_population.shp")
Lisbon <- Lisbon[, c(2, 4, 5)]
#Malta = Valletta
Valletta = stp[stp$LAU_NAME == "Valletta",]
#Romania = Bucharest
Bucharest = stp[stp$LAU_NAME == "Municipiul Bucureşti",]
#Slovakia = Bratislava
Bratislava = stp[stp$LAU_NAME %in% c("Bratislava - mestská časť Čunovo", "Bratislava - mestská časť Devín", 
                                     "Bratislava - mestská časť Devínska Nová Ves", "Bratislava - mestská časť Dúbravka", "Bratislava - mestská časť Jarovce",
                                     "Bratislava - mestská časť Karlova Ves", "Bratislava - mestská časť Lamač", "Bratislava - mestská časť Nové Mesto", 
                                     "Bratislava - mestská časť Petržalka", "Bratislava - mestská časť Podunajské Biskupice", 
                                     "Bratislava - mestská časť Rača", "Bratislava - mestská časť Rusovce", "Bratislava - mestská časť Ružinov", 
                                     "Bratislava - mestská časť Staré Mesto", "Bratislava - mestská časť Vajnory", "Bratislava - mestská časť Vrakuňa",
                                     "Bratislava - mestská časť Záhorská Bystrica"),]
#Serbia = Belgrade
Belgrade = stp[stp$LAU_NAME %in% c("Belgrade - Barajevo", "Belgrade - Čukarica", "Belgrade - Grocka", "Belgrade - Lazarevac",
                                   "Belgrade - Mladenovac", "Belgrade - Novi Beograd", "Belgrade - Obrenovac", "Belgrade - Palilula", "Belgrade - Rakovica", 
                                   "Belgrade - Savski venac", "Belgrade - Sopot", "Belgrade - Stari grad", "Belgrade - Surčin", "Belgrade - Voždovac",
                                   "Belgrade - Vračar", "Belgrade - Zemun", "Belgrade - Zvezdara"),]
#Sweden = Stockholm
Stockholm = stp[stp$LAU_NAME == "Stockholm",]
#UK = london
London = stp[stp$LAU_NAME == "City of London",]
#Slovenia = Ljublijana
Ljubljana = stp[stp$LAU_NAME == "Ljubljana",]

#merge the multiple polygons into one
bl <- sum(Belgrade$POP_2019)
Belgrade <- Belgrade %>% group_by(CNTR_CODE) %>% summarize() %>% ungroup()
br <- sum(Bratislava$POP_2019)
Bratislava <- Bratislava %>% group_by(CNTR_CODE) %>% summarize() %>% ungroup()
lb <- sum(Lisbon$POP_2019)
Lisbon <- Lisbon %>% group_by(CNTR_CODE) %>% summarize() %>% ungroup()
sk <- sum(Skopje$POP_2019)
Skopje <- Skopje %>% group_by(CNTR_CODE) %>% summarize() %>% ungroup()
db <- sum(Dublin$POP_2019)
Dublin <- Dublin %>% group_by(CNTR_CODE) %>% summarize() %>% ungroup()

Belgrade$POP_2019 <- bl
Bratislava$POP_2019 <- br
Lisbon$POP_2019 <- lb
Skopje$POP_2019 <- sk
Dublin$POP_2019 <- 544107

####


Vienna[1,2]$LAU_NAME <- "Vienna"
#Czechia = Praha = Prague
Prague[1,2]$LAU_NAME <- "Prague"
#Albania - Tirana - Tirane
Tirane[1,2]$LAU_NAME <- "Tirane"
#Germany = Berlin
Berlin[1,2]$LAU_NAME <- "Berlin"
#Belgium - Brussels
Brussels[1,2]$LAU_NAME <- "Brussels"
#Greece - Athens
Athens[1,2]$LAU_NAME <- "Athens"
#Denmark = Copenhagen
Copenhagen[1,2]$LAU_NAME <- "Copenhagen"
#Estonia = Tallinn
Tallinn[1,2]$LAU_NAME <- "Tallinn"
#Bulgaria = Sofia
Sofia[1,2]$LAU_NAME <- "Sofia"
#Spain = Madrid
Madrid[1,2]$LAU_NAME <- "Madrid"
#Switzerland = Bern
Bern[1,2]$LAU_NAME = "Bern"
#France = Paris
Paris[1,2]$LAU_NAME = "Paris"
#Finland = Helsinki
Helsinki[1,2]$LAU_NAME = "Helsinki"
#Cyprus = Nicosia
Nicosia[1,2]$LAU_NAME = "Nicosia"
#Hungary = Budapest
Budapest[1,2]$LAU_NAME = "Budapest"
#Croatia = Zagreb
Zagreb[1,2]$LAU_NAME <- "Zagreb"
#Italy = Rome
Rome[1,2]$LAU_NAME<- "Rome"
#Ireland = Dublin
Dublin$LAU_NAME <- "Dublin"
#Iceland = Reykjavik
Reykjavik[1,2]$LAU_NAME = "Reykjavik"
#Poland = Warsaw
Warsaw[1,2]$LAU_NAME = "Warsaw"
#Netherlands = Amsterdam
Amsterdam[1,2]$LAU_NAME = "Amsterdam"
#Norway = Oslo
Oslo[1,2]$LAU_NAME = "Oslo"
#Liechenstein = Vaduz
Vaduz[1,2]$LAU_NAME = "Vaduz"
#Lithuania = Vilnius
Vilnius[1,2]$LAU_NAME = "Vilnius"
#Luxembourg = luxembourg
Luxembourg[1,2]$LAU_NAME= "Luxembourg"
#Latvia = Riga
Riga[1,2]$LAU_NAME = "Riga"
#North Macedonia = Skopje
Skopje$LAU_NAME = "Skopje"
#Portugal = Lisbon
Lisbon$LAU_NAME = "Lisbon"
#Malta = Valletta
Valletta[1,2]$LAU_NAME = "Valletta"
#Romania = Bucharest
Bucharest[1,2]$LAU_NAME = "Bucharest"
#Slovakia = Bratislava
Bratislava$LAU_NAME <- "Bratislava" 
#Serbia = Belgrade
Belgrade$LAU_NAME <- "Belgrade"
#Sweden = Stockholm
Stockholm[1,2]$LAU_NAME = "Stockholm"
#UK = london
London[1,2]$LAU_NAME = "London"
#Slovenia = Ljublijana
Ljubljana[1,2]$LAU_NAME = "Ljubljana"

EU_capitals <- rbind(Vienna, Prague, Tirane,Berlin,Brussels, Athens,Copenhagen,Tallinn,
                     Sofia, Madrid, Bern, Paris,Helsinki, Nicosia, Budapest, Zagreb,Rome,Dublin,Reykjavik,
                     Warsaw, Amsterdam,Oslo,Vaduz,Vilnius,Luxembourg,Riga,Skopje,Lisbon,Valletta,Bucharest,
                     Bratislava,Belgrade,Stockholm,London,Ljubljana)

names(EU_capitals) <- c("Country", "Capital City", "Population", "geometry")
write_sf(EU_capitals, "EU_capitals.shp")
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
library(terra)
library(dplyr)
library(tidyr)
library(Hmisc)
eu_vect <- vect("EU_capitals.shp")
eu_vect$Popultn <- NULL
#cloud 
wet_day <- rast("Weather/cru_ts4.05.2011.2020.wet.dat.nc")
wet_day<-wet_day[[1:120]]
temp <- rast("Weather/cru_ts4.05.2011.2020.tmp.dat.nc")
temp<-temp[[1:120]]
precip <- rast("Weather/cru_ts4.05.2011.2020.pre.dat.nc")
precip<-precip[[1:120]]
frost_day <- rast("Weather/cru_ts4.05.2011.2020.frs.dat.nc")
frost_day<-frost_day[[1:120]]
cloud <- rast("Weather/cru_ts4.05.2011.2020.cld.dat.nc")
cloud<-cloud[[1:120]]

files <- list.files(path="ORD44063/", pattern=".nc", full.names=TRUE)
sunshine <- rast(files)
sunshine <- sunshine[[1:35]]



Weighted_data <- function(x){
  #reproject raster
  eu_vect <- project(eu_vect, x)
  #extractr with weights to later compute weighted mean and weighted SD
  if(nlyr(x) == 120){
    eu_extract <<- terra::extract(x[[1:120]], eu_vect, weights=TRUE, xy=TRUE)
    
    #Create an extra ID column to collect the names from the vector and merge them with Id in th raster
    eu_extract$ID2 <- eu_extract$ID
    
    vect_country <- tibble(ID = 1:35,
                           country=   c("AUSTRIA", "CZECHIA", "ALBANIA", "GERMANY", "BELGIUM", "GREECE", 
                                        "DENMARK", "ESTONIA", "BULGARIA", "SPAIN", "SWITZERLAND", "FRANCE", 
                                        "FINLAND", "CYPRUS", "HUNGARY", "CROATIA", "ITALY", "IRELAND", 
                                        "ICELAND", "POLAND", "NETHERLANDS", "NORWAY", "LIECHENSTEIN", 
                                        "LITHUANIA", "LUXEMBOURG", "LATVIA", "NORTH MACEDONIA", "PORTUGAL", 
                                        "MALTA", "ROMANIA", "SLOVAKIA", "SERBIA", "SWEDEN", "UNITED KINGDOM", 
                                        "SLOVENIA"))
    vect_city <- tibble(ID2 = 1:35,
                        capital_city = c("Vienna", "Prague", "Tirane", "Berlin", "Brussels", "Athens", 
                                         "Copenhagen", "Tallinn", "Sofia", "Madrid", "Bern", "Paris", 
                                         "Helsinki", "Nicosia", "Budapest", "Zagreb", "Rome", "Dublin", 
                                         "Reykjavik", "Warsaw", "Amsterdam", "Oslo", "Vaduz", "Vilnius", 
                                         "Luxembourg", "Riga", "Skopje", "Lisbon", "Valletta", "Bucharest", 
                                         "Bratislava", "Belgrade", "Stockholm", "London", "Ljubljana"))
    
    Values_cloud <- eu_extract %>% inner_join(vect_country, by = "ID") %>%
      arrange(ID) %>% select(-ID)
    
    #naming convetion
    names(Values_cloud)[1:120] <- paste0(paste(rep(c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 10),sep=" "),"-","wet", "_",rep(2011:2020, each=12) )
    
    #Assign and return variable
    Values_cloud <- Values_cloud %>% inner_join(vect_city, by = "ID2") %>%
      arrange(ID2) %>% select(-ID2)
    Values_cloud <<- Values_cloud[complete.cases(Values_cloud),]
    
    #extract the seasons
    #winter
    dec<-grep("dec",   names(Values_cloud), value=TRUE)
    jan<-grep("jan",   names(Values_cloud), value=TRUE)
    feb<-grep("feb",   names(Values_cloud), value=TRUE)
    weather_winter <- Values_cloud %>% select(dec, jan, feb)
    weather_winter$country <- Values_cloud$country
    weather_winter$capital_city <- Values_cloud$capital_city
    weather_winter$weight <- Values_cloud$weight
    weather_winter <- weather_winter[complete.cases(weather_winter),]
    #spring
    mar<-grep("mar",   names(Values_cloud), value=TRUE)
    apr<-grep("apr",   names(Values_cloud), value=TRUE)
    may<-grep("may",   names(Values_cloud), value=TRUE)
    weather_spring <- Values_cloud %>% select(mar, apr, may)
    weather_spring$country <- Values_cloud$country
    weather_spring$capital_city <- Values_cloud$capital_city
    weather_spring$weight <- Values_cloud$weight
    weather_spring <- weather_spring[complete.cases(weather_spring),]
    
    #summer
    jun<-grep("jun",   names(Values_cloud), value=TRUE)
    jul<-grep("jul",   names(Values_cloud), value=TRUE)
    aug<-grep("aug",   names(Values_cloud), value=TRUE)
    weather_summer <- Values_cloud %>% select(jun, jul, aug)
    weather_summer$country <- Values_cloud$country
    weather_summer$capital_city <- Values_cloud$capital_city
    weather_summer$weight <- Values_cloud$weight
    weather_summer <- weather_summer[complete.cases(weather_summer),]
    
    #autumn
    sep<-grep("sep",   names(Values_cloud), value=TRUE)
    oct<-grep("oct",   names(Values_cloud), value=TRUE)
    nov<-grep("nov",   names(Values_cloud), value=TRUE)
    weather_autumn <- Values_cloud %>% select(sep, oct, nov)
    weather_autumn$country <- Values_cloud$country
    weather_autumn$capital_city <- Values_cloud$capital_city
    weather_autumn$weight <- Values_cloud$weight
    weather_autumn <- weather_autumn[complete.cases(weather_autumn),]
    
    weather_winter <<- weather_winter
    weather_spring <<- weather_spring
    weather_summer <<- weather_summer
    weather_autumn <<- weather_autumn
    
    weather_winter <- weather_winter %>% pivot_longer(-c(31:33))
    weather_spring <- weather_spring %>% pivot_longer(-c(31:33))
    weather_summer <- weather_summer %>% pivot_longer(-c(31:33))
    weather_autumn <- weather_autumn %>% pivot_longer(-c(31:33))
    
    weather_winter$name <- NULL
    weather_spring$name <- NULL
    weather_summer$name <- NULL
    weather_autumn$name <- NULL
    
    names(weather_winter)[4] <- "winter"
    names(weather_spring)[4] <- "spring"
    names(weather_summer)[4] <- "summer"
    names(weather_autumn)[4] <- "autumn"
    
    weather_winter.mean<-weather_winter %>% group_by(country, capital_city) %>% summarise(across(starts_with("winter"),weighted.mean))
    weather_spring.mean<-weather_spring %>% group_by(country, capital_city) %>% summarise(across(starts_with("spring"),weighted.mean))
    weather_summer.mean<-weather_summer %>% group_by(country, capital_city) %>% summarise(across(starts_with("summer"),weighted.mean))
    weather_autumn.mean<-weather_autumn %>% group_by(country, capital_city) %>% summarise(across(starts_with("autumn"),weighted.mean))
    merged_weather_mean <<- Reduce(function(x, y) merge(x, y, all=TRUE), list(weather_winter.mean, weather_spring.mean, weather_summer.mean, weather_autumn.mean))
    
    weather_winter.std<-weather_winter %>% group_by(country, capital_city) %>% summarise(winter = sqrt(wtd.var(winter, weights=weight)))
    weather_spring.std<-weather_spring %>% group_by(country, capital_city) %>% summarise(spring = sqrt(wtd.var(spring, weights=weight)))
    weather_summer.std<-weather_summer %>% group_by(country, capital_city) %>% summarise(summer = sqrt(wtd.var(summer, weights=weight)))
    weather_autumn.std<-weather_autumn %>% group_by(country, capital_city) %>% summarise(autumn = sqrt(wtd.var(autumn, weights=weight)))
    merged_weather_sd <<- Reduce(function(x, y) merge(x, y, all=TRUE), list(weather_winter.std, weather_spring.std, weather_summer.std, weather_autumn.std))
    

  }else if(nlyr(x) == 35){
    eu_extract <<- terra::extract(x[[1:35]], eu_vect, weights=TRUE, xy=TRUE)
    #Create an extra ID column to collect the names from the vector and merge them with Id in th raster
    eu_extract$ID2 <- eu_extract$ID
    names(eu_extract) <- c("ID","feb_sun_2018-02","mar_sun_2018-03","apr_sun_2018-04","may_sun_2018-05","jun_sun_2018-06","jul_sun_2018-07","aug_sun_2018-08","sep_sun_2018-09","oct_sun_2018-10","nov_sun_2018-11","dec_sun_2018-12","jan_sun_2019-01","feb_sun_2019-02","mar_sun_2019-03","apr_sun_2019-04","may_sun_2019-05","jun_sun_2019-06","jul_sun_2019-07","aug_sun_2019-08","sep_sun_2019-09","oct_sun_2019-10","nov_sun_2019-11","dec_sun_2019-12","jan_sun_2020-01","feb_sun_2020-02","mar_sun_2020-03","apr_sun_2020-04","may_sun_2020-05","jun_sun_2020-06","jul_sun_2020-07","aug_sun_2020-08","sep_sun_2020-09","oct_sun_2020-10","nov_sun_2020-11","dec_sun_2020-12", "weight", "x", "y", "ID2")
    
    vect_country <- tibble(ID = 1:35,
                           country=   c("AUSTRIA", "CZECHIA", "ALBANIA", "GERMANY", "BELGIUM", "GREECE", 
                                        "DENMARK", "ESTONIA", "BULGARIA", "SPAIN", "SWITZERLAND", "FRANCE", 
                                        "FINLAND", "CYPRUS", "HUNGARY", "CROATIA", "ITALY", "IRELAND", 
                                        "ICELAND", "POLAND", "NETHERLANDS", "NORWAY", "LIECHENSTEIN", 
                                        "LITHUANIA", "LUXEMBOURG", "LATVIA", "NORTH MACEDONIA", "PORTUGAL", 
                                        "MALTA", "ROMANIA", "SLOVAKIA", "SERBIA", "SWEDEN", "UNITED KINGDOM", 
                                        "SLOVENIA"))
    vect_city <- tibble(ID2 = 1:35,
                        capital_city = c("Vienna", "Prague", "Tirane", "Berlin", "Brussels", "Athens", 
                                         "Copenhagen", "Tallinn", "Sofia", "Madrid", "Bern", "Paris", 
                                         "Helsinki", "Nicosia", "Budapest", "Zagreb", "Rome", "Dublin", 
                                         "Reykjavik", "Warsaw", "Amsterdam", "Oslo", "Vaduz", "Vilnius", 
                                         "Luxembourg", "Riga", "Skopje", "Lisbon", "Valletta", "Bucharest", 
                                         "Bratislava", "Belgrade", "Stockholm", "London", "Ljubljana"))
    
    Values_cloud <- eu_extract %>% inner_join(vect_country, by = "ID") %>%
      arrange(ID) %>% select(-ID)
    

    #Assign and return variable
    Values_cloud <- Values_cloud %>% inner_join(vect_city, by = "ID2") %>%
      arrange(ID2) %>% select(-ID2)
    Values_cloud <<- Values_cloud[complete.cases(Values_cloud),]
    
    #extract the seasons
    #winter
    dec<-grep("dec",   names(Values_cloud), value=TRUE)
    jan<-grep("jan",   names(Values_cloud), value=TRUE)
    feb<-grep("feb",   names(Values_cloud), value=TRUE)
    weather_winter <- Values_cloud %>% select(dec, jan, feb)
    weather_winter$country <- Values_cloud$country
    weather_winter$capital_city <- Values_cloud$capital_city
    weather_winter$weight <- Values_cloud$weight
    weather_winter <- weather_winter[complete.cases(weather_winter),]
    #spring
    mar<-grep("mar",   names(Values_cloud), value=TRUE)
    apr<-grep("apr",   names(Values_cloud), value=TRUE)
    may<-grep("may",   names(Values_cloud), value=TRUE)
    weather_spring <- Values_cloud %>% select(mar, apr, may)
    weather_spring$country <- Values_cloud$country
    weather_spring$capital_city <- Values_cloud$capital_city
    weather_spring$weight <- Values_cloud$weight
    weather_spring <- weather_spring[complete.cases(weather_spring),]
    
    #summer
    jun<-grep("jun",   names(Values_cloud), value=TRUE)
    jul<-grep("jul",   names(Values_cloud), value=TRUE)
    aug<-grep("aug",   names(Values_cloud), value=TRUE)
    weather_summer <- Values_cloud %>% select(jun, jul, aug)
    weather_summer$country <- Values_cloud$country
    weather_summer$capital_city <- Values_cloud$capital_city
    weather_summer$weight <- Values_cloud$weight
    weather_summer <- weather_summer[complete.cases(weather_summer),]
    
    #autumn
    sep<-grep("sep",   names(Values_cloud), value=TRUE)
    oct<-grep("oct",   names(Values_cloud), value=TRUE)
    nov<-grep("nov",   names(Values_cloud), value=TRUE)
    weather_autumn <- Values_cloud %>% select(sep, oct, nov)
    weather_autumn$country <- Values_cloud$country
    weather_autumn$capital_city <- Values_cloud$capital_city
    weather_autumn$weight <- Values_cloud$weight
    weather_autumn <- weather_autumn[complete.cases(weather_autumn),]
    
    weather_winter <<- weather_winter
    weather_spring <<- weather_spring
    weather_summer <<- weather_summer
    weather_autumn <<- weather_autumn
    
    weather_winter <- weather_winter %>% pivot_longer(-c(9:11))
    weather_spring <- weather_spring %>% pivot_longer(-c(10:12))
    weather_summer <- weather_summer %>% pivot_longer(-c(10:12))
    weather_autumn <- weather_autumn %>% pivot_longer(-c(10:12))
    
    weather_winter$name <- NULL
    weather_spring$name <- NULL
    weather_summer$name <- NULL
    weather_autumn$name <- NULL
    
    names(weather_winter)[4] <- "winter"
    names(weather_spring)[4] <- "spring"
    names(weather_summer)[4] <- "summer"
    names(weather_autumn)[4] <- "autumn"
    
    weather_winter.mean<-weather_winter %>% group_by(country, capital_city) %>% summarise(across(starts_with("winter"),weighted.mean))
    weather_spring.mean<-weather_spring %>% group_by(country, capital_city) %>% summarise(across(starts_with("spring"),weighted.mean))
    weather_summer.mean<-weather_summer %>% group_by(country, capital_city) %>% summarise(across(starts_with("summer"),weighted.mean))
    weather_autumn.mean<-weather_autumn %>% group_by(country, capital_city) %>% summarise(across(starts_with("autumn"),weighted.mean))
    merged_weather_mean <<- Reduce(function(x, y) merge(x, y, all=TRUE), list(weather_winter.mean, weather_spring.mean, weather_summer.mean, weather_autumn.mean))
    
    weather_winter.std<-weather_winter %>% group_by(country, capital_city) %>% summarise(winter = sqrt(wtd.var(winter, weights=weight)))
    weather_spring.std<-weather_spring %>% group_by(country, capital_city) %>% summarise(spring = sqrt(wtd.var(spring, weights=weight)))
    weather_summer.std<-weather_summer %>% group_by(country, capital_city) %>% summarise(summer = sqrt(wtd.var(summer, weights=weight)))
    weather_autumn.std<-weather_autumn %>% group_by(country, capital_city) %>% summarise(autumn = sqrt(wtd.var(autumn, weights=weight)))
    merged_weather_sd <<- Reduce(function(x, y) merge(x, y, all=TRUE), list(weather_winter.std, weather_spring.std, weather_summer.std, weather_autumn.std))

  }
  
 }
Weighted_data(wet_day)
names(Values_cloud)

#Wet day
wet_merged_mean <- merged_weather_mean
wet_merged_std <- merged_weather_sd
fwrite(wet_merged_mean, "wet_merged_mean.csv")
fwrite(wet_merged_std, "wet_merged_std.csv")

#Cloud
cloud_merged_mean <- merged_weather_mean
cloud_merged_std <- merged_weather_sd
fwrite(cloud_merged_mean, "cloud_merged_mean.csv")
fwrite(cloud_merged_std, "cloud_merged_std.csv")

#Precipitation
precip_merged_mean <- merged_weather_mean
precip_merged_std <- merged_weather_sd
fwrite(precip_merged_mean, "precip_merged_mean.csv")
fwrite(precip_merged_std, "precip_merged_std.csv")

#temp
temp_merged_mean <- merged_weather_mean
temp_merged_std <- merged_weather_sd
fwrite(temp_merged_mean, "temp_merged_mean.csv")
fwrite(temp_merged_std, "temp_merged_std.csv")

#frost day
frost_merged_mean <- merged_weather_mean
frost_merged_std <- merged_weather_sd
fwrite(frost_merged_mean, "frost_merged_mean.csv")
fwrite(frost_merged_std, "frost_merged_std.csv")

#sunshine
sunshine_merged_mean <- merged_weather_mean
sunshine_merged_std <- merged_weather_sd
fwrite(sunshine_merged_mean, "sunshine_merged_mean.csv")
fwrite(sunshine_merged_std, "sunshine_merged_std.csv")


xt %>% group_by(country, capital_city) %>% summarise(across(starts_with("winter"),weighted.mean))


Values_cloud$season <- ifelse(stations_precip$month %in% c("Mar", "Apr", "May"), "Spring",
                              ifelse(stations_precip$month %in% c("Jun", "Jul", "Aug"), "Summer",
                                     ifelse(stations_precip$month %in% c("Sep", "Oct", "Nov"), "Autumn",
                                            ifelse(stations_precip$month %in% c("Dec", "Jan", "Feb"), "Winter", NA)
                                     )
                              )
)

######3
library(data.table)
files <- list.files(path = "ECA_indexSS/", pattern="*.txt", full.names = TRUE)
all.txt <- lapply(files, function(x) transform(fread(x, nrows=1), 
                                               Station = sub('.*STATION, (.*?)\\(.*', '\\1',
                                                             paste0(readLines(x), collapse = '\n'))))
stations_precip <- do.call(rbind.data.frame, all.txt)

###

library(stringr)
stations_precip <- stations_precip[,-c(1, 2)]
stations_precip$Country_city <- str_extract(stations_precip$Station, "STATION [^\\(]*") %>%
  str_replace_all(" +", " ") %>% 
  str_replace("^STATION", "") %>% 
  str_trim() %>%
  as.data.frame()


stations_precip$Station <- str_trim(word(stations_precip$Station, 2, sep = ("DATA VALUES\n\n[0-9]"))) %>%
  str_split(" +") %>% 
  sapply(as.numeric)## separate 2nd part of string

stations <- stations_precip %>% unnest(Station)

splitted_stations <- split(stations, rep(1:21, 3))
split_combine <- do.call(cbind.data.frame, splitted_stations)
split_combine<-split_combine[, -c(seq(4, 42, 2))]
split_combine <- split_combine[,c(2, 1, 3:22)]
####
library(data.table)
library(dplyr)
library(tidyr)
stationPrecip <- fread("station_precip_num.csv")











write_sf(stp, "eu.shp")


#stuff before
p <- gsub(",", "", a)

#loop through to select only station names
str_names <- NULL
for(i in 1:length(p)){
  str_names[i]<-strsplit(p, "[+]")[[i]][1]
}
#remove whitespace
station_names.TG <- gsub(" ", "", str_names)

#create data frame
station_names.TG <- data.frame(stations=station_names.cc)
station_names.TG <- station_names.cc[!(station_names.cc$stations == ""),]
station_names.TG <- data.frame(stations=station_names.cc)
