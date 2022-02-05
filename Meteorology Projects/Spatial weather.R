library(tidyverse)
library(data.table)
library(terra)
library(sf)

stations <- fread("stations_precip.txt", sep='')
s <- stations[-c(1:18),]
sp <- s %>% 
  mutate(x = strsplit(as.character(`EUROPEAN CLIMATE ASSESSMENT & DATASET (ECA&D), file created on 26-06-2021`), ",")) %>%
  unnest(x)
stations <- data.frame(matrix(sp$x, ncol =6, byrow=TRUE))
#stations$X5<-sub(".",'', stations$X5)
#stations$X4<-sub(".",'', stations$X4)
stations1 <- stations[,c(3,4, 5)]
names(stations1) <- c("Station", "Latitude", "Longitude")
stations1$Latitude <- parzer::parse_lat(stations1$Latitude)
stations1$Longitude <- parzer::parse_lat(stations1$Longitude)
stations1$Station <-str_trim(stations1$Station, side = "right")
#New method
rr <- rast("Over_Gb/GBOverview.tif")
r <- RGB2col(rr, 1:3)
sta.vec <- vect(stations, geom=c("Longitude", "Latitude"), crs="epsg:4326")
sta.vec <- project(sta.vec, r)
r <- crop(r, sta.vec)
z <- aggregate(rast(r), c(20,10))
z <- rasterize(sta.vec, z, "Stations")
plot(r)
plot(z, add=T)

#List files
files <- list.files(path = "eca2/", pattern=".txt", full.names=TRUE)

#Get files by file name
all.txt <- lapply(files, function(x) transform(fread(x), 
                                               Station = sub('.*UNITED KINGDOM, (.*?)\\(.*', '\\1',
                                                      paste0(readLines(x), collapse = '\n'))))

stations_precip <- do.call(rbind.data.frame, all.txt)
stations_precip$DATE <- ymd(stations_precip$DATE)
stations_precip$year <- year(stations_precip$DATE)
stations_precip$month <- month(stations_precip$DATE)
stations_precip$month <- month.abb[stations_precip$month]
st.1 <- stations_precip[stations_precip$year > 2017,]

stations_precip$season <- ifelse(stations_precip$month %in% c("Mar", "Apr", "May"), "Spring",
                      ifelse(stations_precip$month %in% c("Jun", "Jul", "Aug"), "Summer",
                             ifelse(stations_precip$month %in% c("Sep", "Oct", "Nov"), "Autumn",
                                    ifelse(stations_precip$month %in% c("Dec", "Jan", "Feb"), "Winter", NA)
                             )
                      )
)
stations_precip <- stations_precip[stations_precip$year >2017,]
stations_precip <- stations_precip[, -c(1, 4)]
stations_precip <- stations_precip[stations_precip$RR != -9999,]
mean_precip <- aggregate(RR ~ season + year + Station, stations_precip, mean)
mean_precip <- mean_precip %>% pivot_wider(names_from = season, values_from = RR)
stations_2018 <- stations_precip[stations_precip$year == 2018,]
stations_2018$Station <-str_trim(stations_2018$Station, side = "right")
#2018 test
xy.merge <- base::merge(stations_2018, stations, allow.cartesian=TRUE)
xy.agg <- aggregate(RR ~ Station + season, xy.merge, mean)
xy.agg_merge <- merge(xy.agg,xy.merge[,c(1, 7, 8)], allow.cartesian=TRUE)
xy.agg_merge %>% distinct()

