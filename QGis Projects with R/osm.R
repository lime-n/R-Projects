#OSM
library(sf)
library(stringr)
library(dplyr)


osm_parks <- st_read("britain-and-ireland-latest.osm.pdf")
osm.playground <- osm_parks %>%
  filter(str_detect(other_tags, "playground"))
osm.fountain <- osm_parks %>%
  filter(str_detect(other_tags, "fountain"))
osm.golfcourse <- osm_parks %>%
  filter(str_detect(other_tags, "golf_course"))
osm.track <- osm_parks %>%
  filter(str_detect(other_tags, "track"))
osm.fishing <- osm_parks %>%
  filter(str_detect(other_tags, "fishing"))



osm.playground$leisure_amenity <- "playground"
osm.golfcourse$leisure_amenity <- "golfcourse"
osm.track$leisure_amenity <- "track"
osm.fishing$leisure_amenity <- "fishing"
osm.fountain$lesiure_amenity <- "fountain"


raster::bind(osm.playground, osm.fountain)

write_sf(osm.playground, "osm.playground.shp")
write_sf(osm.golfcourse, "osm.golfcourse.shp")
write_sf(osm.track, "osm.track.shp")
write_sf(osm.fishing, "osm.fishing.shp")
write_sf(osm.fountain, "osm.fountain.shp")
