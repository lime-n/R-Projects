#coastal climate
library(tidyverse)
library(terra)
library(sf)


#functions used

#Convert sunshine hours to H.M.S
d.hms <- function(i){
  hrs <- parse_number(as.character(i))
  hours <- floor(hrs)
  minutes <- floor((hrs %% 1) * 60)
  second <- round((((hrs %% 1) * 60) %% 1) * 60)
  sprintf("%02d:%02d:%02d", hours, minutes, second)
}

#function to select only those in summer
sl.seq <- function(i){
  seq(3, length(i), 4)
}

#function to get last ten values
last_ten <- function(i){
  i[(length(i)-10):max(length(i))]
}

################################################################################
#################################FIRST TICKET###################################
################################################################################
#Select files
x<-list.files(path="sun/seas/v20200731", pattern=".nc", full.names = TRUE)
x=x[91:101]
#load to raster
sun.rast <- rast(x)
#get names and select only summer month
z <- names(sun.rast)
sun.rast <- sun.rast[[sl.seq(z)]]

#get values
val.sun <- as.data.frame(sun.rast)
names(val.sun) <- c("Summer_2009","Summer_2010","Summer_2011","Summer_2012","Summer_2013","Summer_2014","Summer_2015","Summer_2016","Summer_2017","Summer_2018","Summer_2019")

#get the shapefile coastline areas
uk.sf <- read_sf("coastal.gpkg")
uk.sf <- st_transform(uk.sf, crs = 4326)

uk.vec <- vect(uk.sf[1])
uk.vec <- project(uk.vec, sun.rast)

#extract the values relative to coastline
extract.values <- extract(sun.rast, uk.vec, weights = TRUE)
names(extract.values) <- c("ID", "Summer_2009", "Summer_2010", "Summer_2011", "Summer_2012", "Summer_2013", "Summer_2014", "Summer_2015", "Summer_2016", "Summer_2017", "Summer_2018", "Summer_2019", "weights")
extract.values <- extract.values[complete.cases(extract.values),]
var_names <- tibble(ID = 1:109,
                    Coastal_Cities=c("Dorset", "North Tyneside District (B)", "Wirral District (B)", 
                                     "Abertawe - Swansea", "Angus", "Blackpool (B)", "Bournemouth, Christchurch and Poole", 
                                     "Bro Morgannwg - the Vale of Glamorgan", "Casnewydd - Newport", 
                                     "Castell-nedd Port Talbot - Neath Port Talbot", "City of Bristol (B)", 
                                     "City of Kingston upon Hull (B)", "Conwy - Conwy", "Sefton District (B)", 
                                     "South Tyneside District (B)", "Sunderland District (B)", "Dundee City", 
                                     "East Riding of Yorkshire", "Falkirk", "Sir Ddinbych - Denbighshire", 
                                     "Sir Gaerfyrddin - Carmarthenshire", "Sir y Fflint - Flintshire", 
                                     "Southend-on-Sea (B)", "South Gloucestershire", "The City of Brighton and Hove (B)", 
                                     "Torbay (B)", "West Lothian", "Isle of Wight", "Sir Ynys Mon - Isle of Anglesey", 
                                     "Gwynedd - Gwynedd", "Caerdydd - Cardiff", "Sir Ceredigion - Ceredigion", 
                                     "Sir Fynwy - Monmouthshire", "Sir Benfro - Pembrokeshire", "North Somerset", 
                                     "Inverclyde", "Medway (B)", "Midlothian", "North East Lincolnshire (B)", 
                                     "North Lincolnshire (B)", "Pen-y-bont ar Ogwr - Bridgend", "Redcar and Cleveland (B)", 
                                     "Argyll and Bute", "Aberdeenshire", "Fife", "Aberdeen City", 
                                     "City of Edinburgh", "East Lothian", "Moray", "City of Portsmouth (B)", 
                                     "City of Plymouth (B)", "South Ayrshire", "Cornwall", "Isles of Scilly", 
                                     "Highland", "County Durham", "Northumberland", "Dumfries and Galloway", 
                                     "Scottish Borders", "North Ayrshire", "Hartlepool (B)", "New Forest District", 
                                     "Eastleigh District (B)", "Gosport District (B)", "Fareham District (B)", 
                                     "South Holland District", "Boston District (B)", "Copeland District (B)", 
                                     "South Lakeland District", "Allerdale District (B)", "Barrow-in-Furness District (B)", 
                                     "North Devon District", "East Devon District", "Teignbridge District", 
                                     "Lewes District", "Rother District", "Wealden District", "Eastbourne District (B)", 
                                     "Hastings District (B)", "Rochford District", "Tendring District", 
                                     "Colchester District (B)", "Maldon District (B)", "Castle Point District (B)", 
                                     "Havant District (B)", "Folkestone and Hythe District", "Thanet District", 
                                     "Canterbury District (B)", "Dover District", "Swale District (B)", 
                                     "West Lancashire District (B)", "Lancaster District (B)", "Fylde District (B)", 
                                     "Wyre District (B)", "East Lindsey District", "Great Yarmouth District (B)", 
                                     "King's Lynn and West Norfolk District (B)", "Scarborough District (B)", 
                                     "Somerset West and Taunton District", "Sedgemoor District", "East Suffolk District", 
                                     "Babergh District", "Arun District", "Adur District", "Chichester District", 
                                     "Worthing District (B)", "North Norfolk District", "South Hams District", 
                                     "Torridge District"))



#Covert to a dataframe and get values
summer_values <- extract.values %>% inner_join(var_names, by = "ID") %>%
  arrange(ID) %>% select(-ID)

#ten_year.average <- aggregate(. ~ Coastal_Cities, summer_values, mean)
ten_year.average <- summer_values %>% group_by(Coastal_Cities) %>% summarise(across(starts_with("Sum"), weighted.mean))



#Filter white space and signs
ten_year.average$Coastal_Cities <- gsub("\\.*District", "", ten_year.average$Coastal_Cities)
ten_year.average$Coastal_Cities <- gsub("(B", "", ten_year.average$Coastal_Cities, fixed=TRUE)
ten_year.average$Coastal_Cities <- gsub(")", "", ten_year.average$Coastal_Cities, fixed=TRUE)
ten_year.average$Coastal_Cities <-str_trim(ten_year.average$Coastal_Cities, side = "right")
ten_year.average[,-1] <- round(ten_year.average[,-1], 0)
#ten_year.average[,-1] <- apply(ten_year.average[,-1],2, FUN=d.hms)
write.csv(ten_year.average, "Coastal_summer.csv")

#check the confidence interval of the data of years since 2009
hist(sqrt(ten_year.average$Summer_2009))


mean(ten_year.average$Summer_2009)

ci <- function(x){
z= rowMeans(x[,-1])-1.96*(apply(x[, -1], 1, sd)/length(x[,-1])); 
v =rowMeans(x[,-1])+1.96*(apply(x[, -1], 1, sd)/length(x[,-1])) ;
y=data.frame(lower_confidence = z, upper_confidence = v);
return(y)}
p <- ci(ten_year.average)

t <- NULL

for(i in nrow(age)){
  for(j in length(age))
    {
    t[[i]][[j]] <- age[[j]][i]
  } 
  
 

}



diff_ci <- function(x, y) { 
  
  for(i in nrow(x)) {
    for(j in length(x[, -1])){
  if(x[[j]][i] > ci(y)[1][[1]][i] | x[[j]][i] < ci(y)[2][[1]][i]){
    print(x)
  }
  
    } 
    
  }

}
diff_ci(ten_year.average[, -1], ten_year.average)

#Ttry this one
t=0
p=0
diff_ci <- function(x, y) { 
  
  for(i in nrow(x)) {
    for(j in length(x[, -1])){
      t = if(x[j] > ci(y)[1][[1]][i]){
        return(x)} 
      else if(p = x[j] < ci(y)[2][[1]][i]){
        return(x)
      }
      
    } 
    
  }
  e = data.frame(t, p)
  return(e)
}
diff_ci(ten_year.average[, -1], ten_year.average)

##or this one

diff_ci <- function(x, y) { 
  
  for(i in nrow(x)) {
    for(j in length(x[, -1])){
     if(x[j] > ci(y)[1][[1]][i] & x[j] < ci(y)[2][[1]][i])
      return(x)
    } 
    
  }
  return(x)
}

diff_ci(ten_year.average[, -1], ten_year.average)


################################################################################
###############SECOND TICKET###############
################################################################################
#coastal climate
library(tidyverse)
library(terra)
library(sf)
#List files and pick within the last ten years from 2009-2019
x<-list.files(path="sun/seas/v20200731", pattern=".nc", full.names = TRUE)
y<-list.files(path="tasmax/seas/v20200731", pattern=".nc", full.names = TRUE)
z<-list.files(path="tasmin/seas/v20200731", pattern=".nc", full.names = TRUE)
c<-list.files(path="hurs/seas/v20200731", pattern=".nc", full.names = TRUE)
v<-list.files(path="rainfall/seas/v20200731", pattern=".nc", full.names = TRUE)
b<-list.files(path="sfcWind/seas/v20200731", pattern=".nc", full.names = TRUE)

#get the last ten values

x = last_ten(x)
y = last_ten(y)
z = last_ten(z)
c = last_ten(c)
v = last_ten(v)
b = last_ten(b)

#Rasterize the data and store their names
sun.rast <- rast(x)
max_airtemp <- rast(y)
min_airtemp <- rast(z)
relative_humidity <- rast(c)
total_precip <- rast(v)
wind_speed <- rast(b)

x <- names(sun.rast)
y <- names(max_airtemp)
z <- names(min_airtemp)
c <- names(relative_humidity)
v <- names(total_precip)
b <- names(wind_speed)

#select only those in summer
sun.rast <- sun.rast[[sl.seq(x)]]
max_airtemp <- max_airtemp[[sl.seq(y)]]
min_airtemp <- min_airtemp[[sl.seq(z)]]
relative_humidity <- relative_humidity[[sl.seq(c)]]
total_precip <- total_precip[[sl.seq(v)]]
wind_speed <- wind_speed[[sl.seq(b)]]

#import shapefile, fectorize and project to raster
uk.sf <- read_sf("GB/district_borough_unitary_region.shp")
uk.sf <- st_transform(uk.sf, crs = 4326)
uk.vec <- vect(uk.sf[1])
uk.vec <- project(uk.vec, sun.rast)
uk.sf$NAME <- gsub("\\.*District", "", uk.sf$NAME)
uk.sf$NAME <- gsub("(B", "", uk.sf$NAME, fixed=TRUE)
uk.sf$NAME <- gsub(")", "", uk.sf$NAME, fixed=TRUE)
uk.sf$NAME <-str_trim(uk.sf$NAME, side = "right")

#Extract values relative to districts
extract.sunshine <- extract(sun.rast, uk.vec, weights = TRUE)
names(extract.sunshine) <- c("ID", "Sunshine_2009","Sunshine_2010", "Sunshine_2011", "Sunshine_2012", "Sunshine_2013", "Sunshine_2014", "Sunshine_2015", "Sunshine_2016", "Sunshine_2017", "Sunshine_2018", "Sunshine_2019", "weights")
extract.max_airtemp<- extract( max_airtemp, uk.vec, weights= TRUE)
names(extract.max_airtemp) <- c("ID", "Max_airtemp_2009","Max_airtemp_2010", "Max_airtemp_2011", "Max_airtemp_2012", "Max_airtemp_2013", "Max_airtemp_2014", "Max_airtemp_2015", "Max_airtemp_2016", "Max_airtemp_2017", "Max_airtemp_2018", "Max_airtemp_2019", "weights")
extract.min_airtemp <- extract(min_airtemp, uk.vec, weights= TRUE)
names(extract.min_airtemp) <- c("ID", "Min_airtemp_2009","Min_airtemp_2010", "Min_airtemp_2011", "Min_airtemp_2012", "Min_airtemp_2013", "Min_airtemp_2014", "Min_airtemp_2015", "Min_airtemp_2016", "Min_airtemp_2017", "Min_airtemp_2018", "Min_airtemp_2019", "weights")
extract.relative_humidity <- extract(relative_humidity, uk.vec, weights= TRUE)
names(extract.relative_humidity) <- c("ID", "Relative_humidity_2009","Relative_humidity_2010", "Relative_humidity_2011", "Relative_humidity_2012", "Relative_humidity_2013", "Relative_humidity_2014", "Relative_humidity_2015", "Relative_humidity_2016", "Relative_humidity_2017", "Relative_humidity_2018", "Relative_humidity_2019", "weights")
extract.total_precip <- extract(total_precip, uk.vec, weights= TRUE)
names(extract.total_precip) <- c("ID", "Total_preciptation_2009","Total_preciptation_2010", "Total_preciptation_2011", "Total_preciptation_2012", "Total_preciptation_2013", "Total_preciptation_2014", "Total_preciptation_2015", "Total_preciptation_2016", "Total_preciptation_2017", "Total_preciptation_2018", "Total_preciptation_2019", "weights")
extract.wind_speed <- extract(wind_speed, uk.vec, weights= TRUE)
names(extract.wind_speed) <- c("ID", "Wind_speed_2009","Wind_speed_2010", "Wind_speed_2011", "Wind_speed_2012", "Wind_speed_2013", "Wind_speed_2014", "Wind_speed_2015", "Wind_speed_2016", "Wind_speed_2017", "Wind_speed_2018", "Wind_speed_2019", "weights")


uk.names <- tibble(ID = 1:363,
       UK_Districts = c("Buckinghamshire", "Dorset", "Bolton", "Bradford", "Bury", 
                        "Calderdale", "City of Wolverhampton", "Coventry", "Doncaster", 
                        "Dudley", "Gateshead", "Kirklees", "Knowsley", "Leeds", "Liverpool", 
                        "Manchester", "Newcastle upon Tyne", "North Tyneside", "Oldham", 
                        "Rochdale", "Wigan", "Wirral", "Abertawe - Swansea", "Angus", 
                        "Bath and North East Somerset", "Bedford", "Blackburn with Darwen", 
                        "Blackpool", "Blaenau Gwent - Blaenau Gwent", "Bournemouth, Christchurch and Poole", 
                        "Bracknell Forest", "Bro Morgannwg - the Vale of Glamorgan", 
                        "Caerffili - Caerphilly", "Casnewydd - Newport", "Castell-nedd Port Talbot - Neath Port Talbot", 
                        "Central Bedfordshire", "Cheshire East", "Cheshire West and Chester", 
                        "City of Bristol", "City of Derby", "City of Kingston upon Hull", 
                        "City of Leicester", "City of Nottingham", "City of Peterborough", 
                        "City of Southampton", "City of Stoke-on-Trent", "Clackmannanshire", 
                        "Conwy - Conwy", "Sefton", "Sheffield", "Solihull", "South Tyneside", 
                        "Stockport", "St. Helens", "Sunderland", "Tameside", "Trafford", 
                        "Wakefield", "Walsall", "Salford", "Sandwell", "County of Herefordshire", 
                        "Darlington", "Dundee City", "East Ayrshire", "East Dunbartonshire", 
                        "East Renfrewshire", "East Riding of Yorkshire", "Falkirk", "Glasgow City", 
                        "Halton", "Shropshire", "Sir Ddinbych - Denbighshire", "Sir Gaerfyrddin - Carmarthenshire", 
                        "Sir y Fflint - Flintshire", "Slough", "Southend-on-Sea", "South Gloucestershire", 
                        "South Lanarkshire", "Stirling", "Stockton-on-Tees", "Swindon", 
                        "Telford and Wrekin", "The City of Brighton and Hove", "Thurrock", 
                        "Torfaen - Torfaen", "Torbay", "Warrington", "West Berkshire", 
                        "West Dunbartonshire", "West Lothian", "Wiltshire", "Windsor and Maidenhead", 
                        "Wokingham", "Wrecsam - Wrexham", "York", "Isle of Wight", "Sir Ynys Mon - Isle of Anglesey", 
                        "Gwynedd - Gwynedd", "Caerdydd - Cardiff", "Sir Ceredigion - Ceredigion", 
                        "Sir Fynwy - Monmouthshire", "Sir Benfro - Pembrokeshire", "North Somerset", 
                        "Inverclyde", "Luton", "Medway", "Merthyr Tudful - Merthyr Tydfil", 
                        "Middlesbrough", "Midlothian", "Milton Keynes", "North East Lincolnshire", 
                        "North Lanarkshire", "North Lincolnshire", "Pen-y-bont ar Ogwr - Bridgend", 
                        "Perth and Kinross", "Powys - Powys", "Reading", "Redcar and Cleveland", 
                        "Renfrewshire", "Rhondda Cynon Taf - Rhondda Cynon Taf", "Rutland", 
                        "Argyll and Bute", "Aberdeenshire", "Fife", "Aberdeen City", 
                        "City of Edinburgh", "East Lothian", "Birmingham", "Moray", "City of Portsmouth", 
                        "City of Plymouth", "South Ayrshire", "Cornwall", "Isles of Scilly", 
                        "Orkney Islands", "Shetland Islands", "Highland", "Na h-Eileanan an Iar", 
                        "Barnsley", "North Northamptonshire", "County Durham", "Northumberland", 
                        "Dumfries and Galloway", "Scottish Borders", "North Ayrshire", 
                        "Hartlepool", "Rotherham", "West Northamptonshire", "Fenland", 
                        "South Cambridgeshire", "East Cambridgeshire", "Huntingdonshire", 
                        "Cambridge", "High Peak", "Erewash", "North East Derbyshire", 
                        "Amber Valley", "Chesterfield", "Brentwood", "Epping Forest", 
                        "Uttlesford", "Chelmsford", "Harlow", "Cotswold", "Tewkesbury", 
                        "Gloucester", "Cheltenham", "Basingstoke and Deane", "New Forest", 
                        "Eastleigh", "East Hampshire", "Winchester", "Test Valley", "Hart", 
                        "Gosport", "Fareham", "Rushmoor", "Three Rivers", "Broxbourne", 
                        "Dacorum", "East Hertfordshire", "St. Albans", "Welwyn Hatfield", 
                        "North Hertfordshire", "Watford", "Stevenage", "Tunbridge Wells", 
                        "Sevenoaks", "Tonbridge and Malling", "Ashford", "Maidstone", 
                        "Dartford", "Gravesham", "Chorley", "Rossendale", "Preston", 
                        "Pendle", "Burnley", "Hinckley and Bosworth", "Melton", "Harborough", 
                        "Blaby", "Charnwood", "Oadby and Wigston", "West Lindsey", "South Kesteven", 
                        "South Holland", "Boston", "North Kesteven", "Lincoln", "Breckland", 
                        "Norwich", "Craven", "Bassetlaw", "Gedling", "Ashfield", "Newark and Sherwood", 
                        "Broxtowe", "Mansfield", "Vale of White Horse", "South Oxfordshire", 
                        "West Oxfordshire", "Oxford", "Mendip", "Cannock Chase", "Mid Suffolk", 
                        "Ipswich", "Waverley", "Woking", "Surrey Heath", "Runnymede", 
                        "Guildford", "Reigate and Banstead", "Mole Valley", "Elmbridge", 
                        "Epsom and Ewell", "Rugby", "Nuneaton and Bedworth", "Mid Sussex", 
                        "Crawley", "Malvern Hills", "Wyre Forest", "Wychavon", "Bromsgrove", 
                        "Worcester", "Redditch", "Broadland", "Copeland", "Carlisle", 
                        "South Lakeland", "Allerdale", "Eden", "Barrow-in-Furness", "South Derbyshire", 
                        "Bolsover", "Derbyshire Dales", "North Devon", "East Devon", 
                        "Teignbridge", "West Devon", "Mid Devon", "Exeter", "Lewes", 
                        "Rother", "Wealden", "Eastbourne", "Hastings", "Rochford", "Tendring", 
                        "Colchester", "Maldon", "Braintree", "Basildon", "Castle Point", 
                        "Forest of Dean", "Stroud", "Havant", "Hertsmere", "Folkestone and Hythe", 
                        "Thanet", "Canterbury", "Dover", "Swale", "West Lancashire", 
                        "Lancaster", "South Ribble", "Fylde", "Wyre", "Ribble Valley", 
                        "Hyndburn", "North West Leicestershire", "East Lindsey", "Great Yarmouth", 
                        "South Norfolk", "King's Lynn and West Norfolk", "Scarborough", 
                        "Selby", "Richmondshire", "Harrogate", "Ryedale", "Hambleton", 
                        "Rushcliffe", "Cherwell", "Somerset West and Taunton", "South Somerset", 
                        "Sedgemoor", "Staffordshire Moorlands", "South Staffordshire", 
                        "Lichfield", "Newcastle-under-Lyme", "Stafford", "East Staffordshire", 
                        "Tamworth", "East Suffolk", "Babergh", "West Suffolk", "Tandridge", 
                        "Spelthorne", "North Warwickshire", "Warwick", "Stratford-on-Avon", 
                        "Arun", "Adur", "Chichester", "Horsham", "Worthing", "Kingston upon Thames London Boro", 
                        "Croydon London Boro", "Bromley London Boro", "Hounslow London Boro", 
                        "Ealing London Boro", "Havering London Boro", "Hillingdon London Boro", 
                        "Harrow London Boro", "Brent London Boro", "Barnet London Boro", 
                        "Lambeth London Boro", "Southwark London Boro", "Lewisham London Boro", 
                        "Greenwich London Boro", "Bexley London Boro", "Enfield London Boro", 
                        "Waltham Forest London Boro", "Redbridge London Boro", "Sutton London Boro", 
                        "Richmond upon Thames London Boro", "Merton London Boro", "Wandsworth London Boro", 
                        "Hammersmith and Fulham London Boro", "Kensington and Chelsea London Boro", 
                        "City of Westminster London Boro", "Camden London Boro", "Tower Hamlets London Boro", 
                        "Islington London Boro", "Hackney London Boro", "Haringey London Boro", 
                        "Newham London Boro", "Barking and Dagenham London Boro", "City and County of the City of London", 
                        "North Norfolk", "South Hams", "Torridge"))

extract.sunshine<-extract.sunshine %>% inner_join(uk.names, by="ID") %>% arrange(ID) %>% select(-ID)
extract.max_airtemp<-extract.max_airtemp %>%inner_join(uk.names, by="ID") %>% arrange(ID) %>% select(-ID)
extract.min_airtemp<-extract.min_airtemp %>%inner_join(uk.names, by="ID") %>% arrange(ID) %>% select(-ID)
extract.relative_humidity<-extract.relative_humidity %>%inner_join(uk.names, by="ID") %>% arrange(ID) %>% select(-ID)
extract.total_precip<-extract.total_precip %>%inner_join(uk.names, by="ID") %>% arrange(ID) %>% select(-ID)
extract.wind_speed<-extract.wind_speed %>%inner_join(uk.names, by="ID") %>% arrange(ID) %>% select(-ID)

#Remove NAs
extract.sunshine <- extract.sunshine[complete.cases(extract.sunshine),]
extract.max_airtemp <- extract.max_airtemp[complete.cases(extract.max_airtemp),]
extract.min_airtemp <- extract.min_airtemp[complete.cases(extract.min_airtemp),]
extract.relative_humidity <- extract.relative_humidity[complete.cases(extract.relative_humidity),]
extract.total_precip <- extract.total_precip[complete.cases(extract.total_precip),]
extract.wind_speed <- extract.wind_speed[complete.cases(extract.wind_speed),]

#Get mean aggregation values
#agg.sunshine <- aggregate(. ~ UK_Districts, extract.sunshine, mean)
#agg.max_airtemp <- aggregate(. ~ UK_Districts, extract.max_airtemp, mean)
#agg.min_airtemp <- aggregate(. ~ UK_Districts, extract.min_airtemp, mean)
#agg.relative_humidity <- aggregate(. ~ UK_Districts, extract.relative_humidity, mean)
#agg.total_precip <- aggregate(. ~ UK_Districts, extract.total_precip, mean)
#agg.wind_speed<- aggregate(. ~ UK_Districts, extract.wind_speed, mean)

#Get weighted means instead
agg.sunshine <- extract.sunshine %>% group_by(UK_Districts) %>% summarise(across(starts_with("Sun"), weighted.mean))
agg.max_airtemp <- extract.max_airtemp %>% group_by(UK_Districts) %>% summarise(across(starts_with("Max"), weighted.mean))
agg.min_airtemp<- extract.min_airtemp %>% group_by(UK_Districts) %>% summarise(across(starts_with("Min"), weighted.mean))
agg.relative_humidity<- extract.relative_humidity %>% group_by(UK_Districts) %>% summarise(across(starts_with("Rel"), weighted.mean))
agg.total_precip<- extract.total_precip %>% group_by(UK_Districts) %>% summarise(across(starts_with("Tot"), weighted.mean))
agg.wind_speed<- extract.wind_speed %>% group_by(UK_Districts) %>% summarise(across(starts_with("Win"), weighted.mean))

#Find the lowest temperature, sunshine, humidity, and highest rainfall?

low.sunshine <- aggregate(. ~ UK_Districts, agg.sunshine, min)
low.max_airtemp <- aggregate(. ~ UK_Districts, agg.max_airtemp, min)
low.min_airtemp <- aggregate(. ~ UK_Districts, agg.min_airtemp, min)
low.relative_humidity <- aggregate(. ~ UK_Districts, agg.relative_humidity, min)
max.total_precip <- aggregate(. ~ UK_Districts, agg.total_precip, max)
max.wind_speed <- aggregate(. ~ UK_Districts, agg.wind_speed, max)

#Round to 1 decimal place
low.sunshine[,-1] <- round(agg.sunshine[,-1], 0)
low.max_airtemp[,-1] <- round(agg.max_airtemp[,-1], 1)
low.min_airtemp[,-1] <- round(agg.min_airtemp[,-1], 1)
low.relative_humidity[,-1] <- round(agg.relative_humidity[,-1], 1)
max.total_precip[,-1] <- round(agg.total_precip[,-1], 1)
max.wind_speed[,-1] <- round(agg.wind_speed[,-1], 1)

#or calculate hours minutes and seconds
#low.sunshine[,-1] <- apply(low.sunshine[,-1],2, FUN=d.hms)
#low.max_airtemp[,-1] <- apply(low.max_airtemp[,-1],2, FUN=d.hms)
#low.min_airtemp[,-1] <- apply(low.min_airtemp[,-1],2, FUN=d.hms)
#low.relative_humidity[,-1] <- apply(low.relative_humidity[,-1],2, FUN=d.hms)
#max.total_precip[,-1] <- apply(max.total_precip[,-1],2, FUN=d.hms)
#max.wind_speed[,-1] <- apply(max.wind_speed[,-1],2, FUN=d.hms)

#Remove environment clog
rm(z, x, y, c, v, b, last_ten, sun.rast, 
   max_airtemp, min_airtemp, relative_humidity, 
   total_precip, wind_speed, sl.seq, uk.sf, uk.vec, extract.sunshine, 
   extract.max_airtemp, extract.min_airtemp, extract.relative_humidity, 
   extract.total_precip, extract.wind_speed, uk.names, agg.sunshine, agg.max_airtemp, 
   agg.min_airtemp, agg.relative_humidity, agg.total_precip, agg.wind_speed)


#write them into csv files
write.csv(low.sunshine, "low.sunshine.csv")
write.csv(low.max_airtemp, "low.max_airtemp.csv")
write.csv(low.min_airtemp, "low.min_airtemp.csv")
write.csv(low.relative_humidity, "low.relative_humidity.csv")
write.csv(max.total_precip,"max.total_precip.csv")
write.csv(max.wind_speed,  "max.wind_speed.csv")



#upload new shapefile


library(readxl)
Final_weather_ticket_6 <- read_excel("Final weather ticket-6.xlsx")
sf <- Final_weather_ticket_6
names(sf)[1] <- "NAME"
sh.s <- inner_join(sf, uk.sf, by="NAME")
write_sf(sh.s, "uk_shape.shp")


uk.sf <- read_sf("coastal.gpkg")
uk.sf <- st_transform(uk.sf, crs = 4326)
coast1<- read_excel("Coastal_.xlsx")
names(coast1)[1] <- "NAME"
uk.sf <- uk.sf[1]

uk.sf$NAME <- gsub("\\.*District", "", uk.sf$NAME)
uk.sf$NAME <- gsub("(B", "", uk.sf$NAME, fixed=TRUE)
uk.sf$NAME <- gsub(")", "", uk.sf$NAME, fixed=TRUE)
uk.sf$NAME <-str_trim(uk.sf$NAME, side = "right")



coast <- inner_join(coast1, uk.sf, by = "NAME")
write_sf(coast, "coast_2.shp")





#This was practice
y <- list.files(path="sun/mon/v20200731", pattern=".nc", full.names = TRUE)
y.ras <- rast(y[1])

minus <- function(a, b){
  a-b
}

val.sun %>% mutate(minus = apply(1, FUN=minus))
