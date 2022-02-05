library(sf)
merged <- read_sf("merged_scot_uk.shp")
uk <- read_sf("/Users/emiljanmrizaj/Downloads/bdline_essh_gb/Data/GB/district_borough_unitary_region.shp")
uk <- uk[, 1]
merged <- merged[,1]
dats <- st_join(uk, merged)
dats <- as.data.frame(dats)
dats <- dats %>% distinct(NAME.x, .keep_all = TRUE)
names(dats)<-c("districts", "regions")