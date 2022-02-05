#Dataset filtering

library(tidyverse)
library(terra)
library(sf)
#List files and pick within the last ten years from 2009-2019

sl.seq <- function(i){
  seq(3, length(i), 4)
}

#function to get last ten values
last_ten <- function(i){
  i[(length(i)-10):max(length(i))]
}


x<-list.files(path="sun/seas/v20200731", pattern=".nc", full.names = TRUE)
y<-list.files(path="tasmax/seas/v20200731", pattern=".nc", full.names = TRUE)
z<-list.files(path="tasmin/seas/v20200731", pattern=".nc", full.names = TRUE)
c<-list.files(path="hurs/seas/v20200731", pattern=".nc", full.names = TRUE)
v<-list.files(path="rainfall/seas/v20200731", pattern=".nc", full.names = TRUE)
b<-list.files(path="sfcWind/seas/v20200731", pattern=".nc", full.names = TRUE)
n<-list.files(path="tas/seas/v20200731", pattern=".nc", full.names = TRUE)
m<-list.files(path="snowLying/seas/v20200731", pattern=".nc", full.names = TRUE)

#get the last ten values

x = last_ten(x)
y = last_ten(y)
z = last_ten(z)
c = last_ten(c)
v = last_ten(v)
b = last_ten(b)
n = last_ten(n)
m = last_ten(m)
#Rasterize the data and store their names
sun.rast <- rast(x)
max_airtemp <- rast(y)
min_airtemp <- rast(z)
relative_humidity <- rast(c)
total_precip <- rast(v)
wind_speed <- rast(b)
mean_airtemp <- rast(n)
snow <- rast(m)

#for season data
x <- names(sun.rast)
y <- names(max_airtemp)
z <- names(min_airtemp)
c <- names(relative_humidity)
v <- names(total_precip)
b <- names(wind_speed)
n <- names(mean_airtemp)
m <- names(snow)

#Name for the seasons
names(sun.rast) <- paste0("sunshine", "_",rep(c("Winter", "Autumn", "Summer", "Spring"), 11),rep(2009:2019, each=4))
names(max_airtemp) <- paste0("max_temp", "_",rep(c("Winter", "Autumn", "Summer", "Spring"), 11),rep(2009:2019, each=4))
names(min_airtemp) <- paste0("min_temp", "_",rep(c("Winter", "Autumn", "Summer", "Spring"), 11),rep(2009:2019, each=4))
names(relative_humidity) <- paste0("rel_humidity", "_",rep(c("Winter", "Autumn", "Summer", "Spring"), 11),rep(2009:2019, each=4))
names(total_precip) <- paste0("total_precip", "_",rep(c("Winter", "Autumn", "Summer", "Spring"), 11),rep(2009:2019, each=4))
names(snow) <- paste0("snow", "_",rep(c("Winter", "Autumn", "Summer", "Spring"), 11),rep(2009:2019, each=4))
names(wind_speed) <- paste0("wind_speed", "_",rep(c("Winter", "Autumn", "Summer", "Spring"), 11),rep(2009:2019, each=4))
names(mean_airtemp) <- paste0("mean_temp", "_",rep(c("Winter", "Autumn", "Summer", "Spring"), 11),rep(2009:2019, each=4))

#Write this into dataframe
sf_max.T <- as.data.frame(max_airtemp, xy=T)
sf_min.T <- as.data.frame(min_airtemp, xy=T)
sf_snow <- as.data.frame(snow, xy=T)
sf_relative_humidity <- as.data.frame(relative_humidity, xy=T)
sf_total_precip <- as.data.frame(total_precip, xy=T)
sf_wind_speed <- as.data.frame(wind_speed, xy=T)
sf_mean_airtemp<- as.data.frame(mean_airtemp, xy=T)
sf_sun.rast <- as.data.frame(sun.rast, xy=T)

#readshape file
sf_snow <- st_as_sf(sf_snow, coords=c("x", "y"), crs=4326)
sf_sun.rast <- st_as_sf(sf_sun.rast, coords=c("x", "y"), crs=4326)
sf_mean_airtemp <- st_as_sf(sf_mean_airtemp, coords=c("x", "y"), crs=4326)
sf_wind_speed <- st_as_sf(sf_wind_speed, coords=c("x", "y"), crs=4326)
sf_total_precip <- st_as_sf(sf_total_precip, coords=c("x", "y"), crs=4326)
sf_relative_humidity <- st_as_sf(sf_relative_humidity, coords=c("x", "y"), crs=4326)
sf_min.T <- st_as_sf(sf_min.T, coords=c("x", "y"), crs=4326)
sf_max.T <- st_as_sf(sf_max.T, coords=c("x", "y"), crs=4326)

#write to geopackage
write_sf(sf_max.T, "max_temp_seas.gpkg")
write_sf(sf_sun.rast, "sunshine_seas.gpkg")
write_sf(sf_mean_airtemp, "mean_airtemp_seas.gpkg")
write_sf(sf_wind_speed, "wind_speed_seas.gpkg")
write_sf(sf_total_precip, "total_precip_seas.gpkg")
write_sf(sf_relative_humidity, "rel_humidity_seas.gpkg")
write_sf(sf_min.T, "min_temp_seas.gpkg")
write_sf(sf_snow, "snowlying_seas.gpkg")


#Change names for monthly data
names(sun.rast) <- paste0("sunshine", "_",rep(2009:2019, each=12), "-", rep(1:12, 1))
names(max_airtemp) <-paste0("max_airtemp", "_",rep(2009:2019, each=12), "-", rep(1:12, 1))
names(min_airtemp) <-paste0("min_airtemp", "_",rep(2009:2019, each=12), "-", rep(1:12, 1))
names(relative_humidity) <- paste0("relative_humidity", "_",rep(2009:2019, each=12), "-", rep(1:12, 1))
names(total_precip) <-paste0("rainfall", "_",rep(2009:2019, each=12), "-", rep(1:12, 1))
names(wind_speed) <-paste0("wind_speed", "_",rep(2009:2019, each=12), "-", rep(1:12, 1))
names(mean_airtemp) <-paste0("mean_airtemp", "_",rep(2009:2019, each=12), "-", rep(1:12, 1))
names(snow) <- paste0("snow", "_",rep(2009:2019, each=12), "-", rep(1:12, 1))


#conver to data along with coordinates
sf_max.T <- as.data.frame(max_airtemp, xy=T)
sf_min.T <- as.data.frame(min_airtemp, xy=T)
sf_snow <- as.data.frame(snow, xy=T)
sf_relative_humidity <- as.data.frame(relative_humidity, xy=T)
sf_total_precip <- as.data.frame(total_precip, xy=T)
sf_wind_speed <- as.data.frame(wind_speed, xy=T)
sf_mean_airtemp<- as.data.frame(mean_airtemp, xy=T)
sf_sun.rast <- as.data.frame(sun.rast, xy=T)

#write as geopackage
sf_snow <- st_as_sf(sf_snow, coords=c("x", "y"), crs=4326)
sf_sun.rast <- st_as_sf(sf_sun.rast, coords=c("x", "y"), crs=4326)
sf_mean_airtemp <- st_as_sf(sf_mean_airtemp, coords=c("x", "y"), crs=4326)
sf_wind_speed <- st_as_sf(sf_wind_speed, coords=c("x", "y"), crs=4326)
sf_total_precip <- st_as_sf(sf_total_precip, coords=c("x", "y"), crs=4326)
sf_relative_humidity <- st_as_sf(sf_relative_humidity, coords=c("x", "y"), crs=4326)
sf_min.T <- st_as_sf(sf_min.T, coords=c("x", "y"), crs=4326)
sf_max.T <- st_as_sf(sf_max.T, coords=c("x", "y"), crs=4326)


#Write as geopackage
write_sf(sf_max.T, "max_temp.gpkg")
write_sf(sf_sun.rast, "sunshine.gpkg")
write_sf(sf_mean_airtemp, "mean_airtemp.gpkg")
write_sf(sf_wind_speed, "wind_speed.gpkg")
write_sf(sf_total_precip, "total_precip.gpkg")
write_sf(sf_relative_humidity, "rel_humidity.gpkg")
write_sf(sf_min.T, "min_temp.gpkg")
write_sf(sf_snow, "snowlying.gpkg")

#raster upload
writeRaster(snow, "snow_month.tif",filetype="GTiff", overwrite=TRUE)
writeRaster(wind_speed, "windspeed_month.tif",filetype="GTiff", overwrite=TRUE)
writeRaster(sun.rast, "sunshine_month.tif",filetype="GTiff", overwrite=TRUE)
writeRaster(total_precip, "rainfall_month.tif",filetype="GTiff", overwrite=TRUE)
writeRaster(min_airtemp, "min_air_temp_month.tif",filetype="GTiff", overwrite=TRUE)
writeRaster(max_airtemp, "max_air_temp_month.tif",filetype="GTiff", overwrite=TRUE)
writeRaster(mean_airtemp, "mean_airtemp_month.tif",filetype="GTiff", overwrite=TRUE)
writeRaster(relative_humidity, "relative_humidity_month.tif",filetype="GTiff", overwrite=TRUE)




#sunshine

#precipitation

#maximum air-temp

#minimum air-temp

#wind speed

#mean air temperature

#relative humidity

#ground forst


#snow lying cover
