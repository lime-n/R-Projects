library(sf)
library(terra)
library(dplyr)

sunshine <- read_sf("Season_climate/sunshine_seas.gpkg")
open_zoom <- read_sf("joined_park_points_sunshine.shp")

open_zoom1 <- open_zoom[, -c(1, 2, 3, 4, 9, 10)]




sl.seq <- function(i){
  seq(3, length(i), 4)
}

#x<-names(sunshine)

#sunshine_summer <-sunshine[,sl.seq(x)]

zoom_split <- open_zoom %>%
  bind_cols %>%
  st_cast(to = "POINT") %>%
  dplyr::mutate(
    X = sf::st_coordinates(geometry)[,1],
    Y = sf::st_coordinates(geometry)[,2]
  ) %>%
  sf::st_drop_geometry()


sunshine_summer <- sunshine_split[,sl.seq(x)]
sunshine_10yrs<-data.frame(sunshine=rowMeans(sunshine_summer))
sunshine_10yrs<-cbind(sunshine_10yrs, X=sunshine_split$X, Y=sunshine_split$Y)

shp_sunshine <- st_as_sf(sunshine_10yrs, coords = c("X", "Y"), crs=27700)


#join data
joined_park <- st_join(open_zoom, shp_sunshine)


library(sf)
library(terra)
library(dplyr)

sunshine <- read_sf("Season_climate/sunshine_seas.gpkg")
open_zoom <- read_sf("merged_line_sunshine.shp")

open_zoom1 <- open_zoom[, -c(1, 2, 3, 4, 9, 10)]




sl.seq <- function(i){
  seq(3, length(i), 4)
}

#x<-names(sunshine)

#sunshine_summer <-sunshine[,sl.seq(x)]

zoom80 <- parks_80 %>%
  bind_cols %>%
  st_cast(to = "POINT") %>%
  dplyr::mutate(
    X = sf::st_coordinates(geometry)[,1],
    Y = sf::st_coordinates(geometry)[,2]
  ) %>%
  sf::st_drop_geometry()


sunshine_summer <- sunshine_split[,sl.seq(x)]
sunshine_10yrs<-data.frame(sunshine=rowMeans(sunshine_summer))
sunshine_10yrs<-cbind(sunshine_10yrs, X=sunshine_split$X, Y=sunshine_split$Y)

shp_sunshine <- st_as_sf(sunshine_10yrs, coords = c("X", "Y"), crs=27700)


#join data
joined_park <- st_join(open_zoom, shp_sunshine)

################################################################################################################################
################################################################################################################################
#########################################START HERE######################################################
################################################################################################################################
################################################################################################################################

#zoom stuff
open_zoom <- read_sf("reprojected_centroid.shp")
open_zoom <- st_transform(open_zoom, crs=4326)
zoom_park <- open_zoom
zoom_park<-zoom_park[, -c(1, 2)]
open_zoom <- as.data.frame(open_zoom)
open_zoom <- data.frame(open_zoom)
open_zoom <- open_zoom[complete.cases(open_zoom$sunshine),]
open_zoom <- open_zoom[, -c(1, 2)]

names(open_zoom)[7:11]<-c("surfacewater_intersection_area","surfacewater_intersection_area_%", "woodland_intersection_area","woodland_intersection_area_%", "leisure_amenity")
names(zoom_park)[7:9]<-c("surfacewater_intersection_area_%", "woodland_intersection_area_%", "leisure_amenity")
#area
open_zoom$percent_area <- percent_rank(open_zoom$area)*100
#perimeter
open_zoom$percent_perimeter <- percent_rank(open_zoom$perimeter)*100
#area interval
open_zoom$area_interval <- findInterval(open_zoom$percent_area, seq(1, max(open_zoom$percent_area), max(open_zoom$percent_area)/100))
#area perimeter
open_zoom$perimeter_interval <- findInterval(open_zoom$percent_perimeter, seq(1, max(open_zoom$percent_perimeter), max(open_zoom$percent_perimeter)/100))
#Length
open_zoom$percent_length <- percent_rank(open_zoom$LENGTH)*100
#length interval
open_zoom$length_interval <- findInterval(open_zoom$percent_length, seq(1, max(open_zoom$percent_length), max(open_zoom$percent_length)/100))
#area interval
open_zoom$percent_sunshine <- percent_rank(open_zoom$sunshine)*100
#area perimeter
open_zoom$sunshine_interval <- findInterval(open_zoom$percent_sunshine, seq(1, max(open_zoom$percent_sunshine), max(open_zoom$percent_sunshine)/100))

#get unique values
open_zoom <- unique(open_zoom)
#remove duplicate entries risen from added sunshine hours
open_zoom <- open_zoom[!duplicated(open_zoom[, 1:7]),]

parks_80<- open_zoom[open_zoom$area_interval >=80, ]

parks_80$park_name <- c("Southampton Common", "Southampton Common","Highwoods Country Park", "Highwoods Country Park", 
                        "Byron's Pool LNR", "Bushy Park", "Colwick Country Park", "Wepre Park", 
                        "River Lee Country Park", "Brokerwood Country Park", "Lea Valley Country Park", 
                        "River Lee Country Park", "Tooting Common Lakeside Park",
                        "Hampstead Heath", "Glebe Farm Recreation Ground", "Royal Botanic Gardens", "Old Park", 
                        "Regents Park", "Albert Park", "Hyde Park", "Worcester Woods Park", 
                        "Wisley Common", "Garden Wisley", "Richmond Park", "Kensington Gardens", "Dunorlan Park", 
                        "Hampton Court Park", "Alexandra Park", "Lydiard Park", "Victoria Park", "Priory Park", 
                        "Sefton Park", "Haden Hill Park",
                        "Beveridge Park", "Peckham Rye Park", "Temple Newsam Park", "Temple Newsam Park", "Bedford Park",
                        "Seaton Park", 
                        "Haigh Country Park", "Howlands Park", "Handsworth Park", "Valley Gardens", "Burgess Park",
                        "Sean Devereux Park", "Cannon Hill Park", NA, "Exhibition Park*", "Battersea Park", 
                        "Southwark Park", "Home Park", "Ferry Meadows in Nene Park", "Barking Park", 
                        "Wilton Lodge Park", "Glynllifon Park", NA, "Hazlehead Park",
                        "Holyrood Park","Meadfowfield Park",  "Crystal Palace", "Hinchingbrooke Country Park",
                        "Summerhill Country Park","Humford Wood", "Attlee Park", "Humford Wood", "Senneleys Park",
                        "Victoria Park", "Country Park", "Knighton Park", NA, "Astley Park", "Nonsuch Park",
                        NA, "Aldenham Country Park", "Bury Hill Park",
                        "Capstone Farm Country Park", "The Rising Sun Country Park", NA, "Pheonix Park", 
                        "Town Park", "Alexandra Park", "Shorne Woods Country Park",
                        "Tolcross Park", "Shorne Woods Country Park", "Shorne Woods Country Park","Braunstone Park & Skatepark", 
                        "South Beach Park", "Dean Castle Country Park",
                        NA, "Laighhills Public Park", "Mugdock Country Park", "Mugdock Country Park", 
                        "Mugdock Country Park", "Cowpen Bewley Woodland Park", NA,
                        "Marble Hill Park", NA, "Belmore Playing Fields", "Springburn Park", NA, 
                        "Telford Park", "Croxteth Hall & Country Park", "Black Park Country Park", "Riverside Country Park", 
                        "Black Park Country Park", "Black Park Country Park", "Emberton Country Park", "Mouldon Hill Country Park",
                        "Cassiobury Park", "Wilton Park", "Severn Valley Country Park", "Severn Valley Country Park",
                        "Moors Valley Country Park and Forest", "Moors Valley Country Park and Forest","Moors Valley Country Park and Forest", 
                        "Hardwick Heath", "Ouse Valley Park", "Bestwood Country Park", "Garden Wisley", "Poolsbrook Country Park",
                        "Petersfield Lake Play Area", "Lanark Loch", "Cahfford Hundred Park", "Avery Hill Park", 
                        "Hogganfield Park", "Queen Elizabeth Country Park", "Tattenhoe Valley Park", "Southsea Common", 
                        "Farnham Park", "RSPB Lodmoor", NA, "Balloch Castle and Country Park", "Ballach Castle and Country Park", 
                        "Ballach Castle and Country Park", "Cannock Chase MTB Trails", "Cannock Chase MTB Trails", NA,
                        "Holmebrook Valley Country Park", "Alvaston Park", "Arrowe Country Park", "Arrowe Country Park", NA, NA, 
                        "Kingsbury Water Park", "Kingsbury Water Park", "Kingsbury Water Park", "Saltram Park", "Swanscombe Heritage Park", 
                        "Hardwick Park", "Hamilton Park", "Wetlands Walk", "Pontypool Park", NA, "Great Notley Country Park", "Haughton Country Park",
                        "Blackford Hill and Pond", "Great Denham Playground and Park", NA, "Aros Park", "Aros Park", 
                        "Pennington Flash Country Park", "Horton Country Park", NA, "North Loughton Valley Park", NA, "Central Park (Dagenham)", 
                        "Tweedbank Park", "Leverhulme Park", "The Helix: Home of the Kelpies", "Aros Park", "Valentines Park","Plean Country Park",
                        "Plean Country Park", "Plean Country Park", "Osterley Park", "Osterley Park", "Fermyn Woods Country Park",
                        "Fermyn Woods Country Park", "Leybourne Lakes Country Park", "Hardwick Country Park", "Fermyn Woods Country Park",
                        "Aros Park", "Roundhay Park", "Hobsons Park", "Tredegar Park", "Grange Park", NA, "Danson Park", "Melton Country Park",
                        "Bedfont Lakes Country Park", "Temple Newsham Park", "Bestwood Country Park", "Ravenswood Park", "Worsbrough Country Park", 
                        "Anglers Country Park", NA, NA, NA, "Darley Park", "Peterborough Sculpture Park", "Brockwell Park", 
                        "Upton Court Park", "West Stow Country Park", NA, "Colwick Country Park", "Hillfield Park", "Primrose Hill",
                        NA, "Rother Valley Country Park", NA, NA, NA, "Rother Valley Country Park", "Leazes Park", "Gnoll Country Park", 
                        NA, "Paddy Freeman's Park", "Brocks Hill Country Park", "Clapham Common", "Rivacre Valley Country Park", 
                        "Chevin Forest Park", NA, NA, NA, "Irchester Country Park", "Irchester Country Park", "Syon Park", "Sherdley Park", 
                        "Rushcliffe Country Park", NA, "Walshes Park", "Stanton Low Park", "Moor Park", "Castle Hill Country Park", "Mowsbury Park",
                        "Calendar Park", "Surrey Hills Park","Surrey Hills Park","Surrey Hills Park" )

#remove NAs
parks_80 <- parks_80[complete.cases(parks_80$park_name),]

#remove Duplicates
parks_80 <- parks_80[!duplicated(parks_80$park_name),]


################################################################################################################################
################################################################################################################################
#########################################END HERE######################################################
################################################################################################################################
################################################################################################################################



myIntervals <- c("0 - 270979", "270979 - 541958", "541958  - 812937", "812937  - 1083917","1083917  - 1354896","1354896  - 1625875",
                 "1625875  - 1896855","1896855  - 2167834","2167834 - 2438813","2438813 - 2709793","2709793 - 2980772", "2980772 - 3251751",
                 "3251751  - 3522730", "3522730  - 3793710","3793710  - 4064689","4064689  - 4335668",
                 "4335668  - 4606648","4606648  - 4877627","4877627 - 5148606","5148606  - 5419586","5419586 - 5690565", "5690565 - 5961544",
                 "5961544  - 6232523", "6232523  - 6503503","6503503  - 6774482","6774482  - 7045461",
                 "7045461  - 7316441","7316441  - 7587420","7587420 - 7858399","7858399  - 8129379","8129379 - 8400358", "8400358 - 8671337",
                 "3251751  - 3522730", "3522730 - 3793710","3793710  - 4064689","4064689 - 4335668",
                 "8671337  - 8942316","8942316  - 9213296","9213296 - 9484275","9484275  - 9755254", "9755254 - 10026234", "10026234 - 10297213", 
                 "10297213  - 10568192", "10568192  - 10839172","10839172  - 11110151","11110151  - 11381130",
                 "11381130  - 11652109","11652109  - 11923089","11923089 - 12194068","12194068  - 12465047","12465047 - 12736027", "12736027 - 13007006",
                 "13007006 - 13277985", "13277985 - 13548965")
open_zoom$area_range<- myIntervals[findInterval(open_zoom$area, c(0, 270979 , 541958, 812937, 1083917, 1354896, 1625875, 1896855, 2167834, 2438813,
                                                                  2709793,2980772,  3251751, 3522730, 3793710, 4064689, 4335668, 4606648, 4877627, 5148606,
                                                                  5419586, 5690565, 5961544, 6232523, 6503503, 6774482, 7045461, 7316441, 7587420, 7858399, 
                                                                  8129379, 8400358, 8671337, 8942316, 9213296, 9484275,9755254, 10026234,  10297213, 
                                                                  10568192,10839172,  11110151,  11381130, 11652109, 11923089, 12194068, 12465047,
                                                                  12736027, 13007006,13277985, 13548965 ))]

myIntervals <- c("0 - 4190", "4190 - 8381", "8381  - 12572", "12572 - 16762","16762  - 20953","20953  - 25144","25144  - 29334","29334  - 33525","33525 - 37716","37716  - 41907")
open_zoom$perimeter_range<- myIntervals[findInterval(open_zoom$perimeter, c(0, 4190 , 8381, 12572, 16762, 20953, 25144, 29334, 33525, 37716, 41907))]


data_zoom <- as.data.frame(open_zoom)


perimeter <- data.frame(x=open_zoom$perimeter)
max(perimeter[complete.cases(perimeter),])


perimeter <- data.frame(x=open_zoom$perimeter)
max(perimeter[complete.cases(perimeter),])


#area
zoom_count <- data_zoom %>% count(type, Name, area_range, perimeter_range) %>% arrange(area_range)
zoom_count<-zoom_count[complete.cases(zoom_count),]
zoom_count$area_range <- as.factor(zoom_count$area_range)
zoom_count$area_range2 <- factor(zoom_count$area_range, levels=levels(zoom_count$area_range)[order(as.numeric(gsub("( -.*)", "", levels(zoom_count$area_range))))])
zoom_count <- zoom_count[order(zoom_count$area_range2), ]
zoom_count$area_range2 <- NULL
zoom_count <- zoom_count %>% pivot_wider(names_from = area_range, values_from = n, values_fill=list(n=0))
#perimeter
zoom_count$perimeter_range <- as.factor(zoom_count$perimeter_range)
zoom_count$perimeter_range2 <- factor(zoom_count$perimeter_range, levels=levels(zoom_count$perimeter_range)[order(as.numeric(gsub("( -.*)", "", levels(zoom_count$perimeter_range))))])
zoom_count <- zoom_count[order(zoom_count$perimeter_range2), ]
zoom_count$perimeter_range2 <- NULL

#perimeter
zoom_count <- data_zoom %>% count(type, Name, perimeter_range) %>% arrange(perimeter_range)
zoom_count<-zoom_count[complete.cases(zoom_count),]
zoom_count$perimeter_range <- as.factor(zoom_count$perimeter_range)
zoom_count$perimeter_range2 <- factor(zoom_count$perimeter_range, levels=levels(zoom_count$perimeter_range)[order(as.numeric(gsub("( -.*)", "", levels(zoom_count$perimeter_range))))])
zoom_count <- zoom_count[order(zoom_count$perimeter_range2), ]
zoom_count$perimeter_range2 <- NULL


