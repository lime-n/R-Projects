library(sf)
library(dplyr)
library(readr)

#hospitals
wales <- read_csv("wales.csv")
scotland <- read_csv("scotland_postcode.csv")
england <- read_csv("eng_hospitals.csv")
hospital2 <- read_csv("Hospital2.csv")
england <- england[,c(3, 4)];scotland <- scotland[, c(3, 4)]
names(scotland) <- c("hospital", "postcode")
wales$X1 <- NULL
names(wales)<-c("hospital","postcode")
hospitals <- rbind(england, scotland, wales)
hospital <- rbind(hospitals, hospital2)
hospital <- unique(hospital)

postcode <- read_csv("Postcodes.csv")

names(postcode)[3]<-"postcode"
postcode <- postcode[,c(3, 9,10, 12, 11)]
library(readr)
fire_stations <- read_csv("fire_stations.csv")

hospitals<-merge(hospital, postcode, by="postcode")
fire_stations<-merge(fire_stations, postcode, by="postcode")

hospitals <- st_as_sf(hospitals, coords=c("longitude", "latitude"),crs=4326)
fire_stations <- st_as_sf(fire_stations, coords=c("longitude", "latitude"),crs=4326)

write_sf(hospitals, "hospitals.shp")
write_sf(fire_stations, "fire_stations.shp")


write_sf(shape_hospitals, "shape_hospitals.shp")







distance <- st_read('uk_distance_stations.shp')


time_arrival <- function(x){
  x/56.327
}
distance$arrival_time <- time_arrival(distance$HubDist)
#distance$arrival_time <- strftime(as.POSIXct(distance$arrival_time * 60 * 60, origin = Sys.Date(), tz = "GMT"), format = "%M:%S")

distance <- as.data.frame(distance)
names(distance) <- c("id", "brigade", "distance", "geometry", "arrival_time")
distance_stations <- aggregate(arrival_time ~ brigade + distance, distance, mean)

#
ujk_n$Districts <- gsub("\\.*District", "", ujk_n$Districts )
ujk_n$Districts  <-str_trim(ujk_n$Districts , side = "right")
ujk_n$Districts  <- gsub("(B", "", ujk_n$Districts, fixed=TRUE)
ujk_n$Districts <- gsub(")", "", ujk_n$Districts , fixed=TRUE)


