library(sf)
library(terra)
library(arrow)
library(stringr)
library(tidyverse)
join_rest <- st_read("join_gadm_restaurant.shp")
join_rest<-join_rest[,-c(1:11, 13:28)]
join_rest <- as.data.frame(join_rest)
restaurants <- data.table::fread("all_restaurants.csv")
data_test <- as.data.frame(join_rest)
data_test$geometry<- NULL
names(data_test)[2]<-"restaurant_geo"

europe_capitals <- st_read("europe_capitals.shp")
restaurants <- restaurants[complete.cases(restaurants[, c(3:4)]),]
res_shp <- st_as_sf(restaurants, coords = c("longitude", "latitude"), crs=4326)
europe_restaurants <- st_join(europe_capitals, res_shp)
data_test <- as.data.frame(europe_restaurants)


reviews <- read_feather("/Volumes/Seagate/Downloads folder/parsed_reviews.feather")
combined_reviews_all <- inner_join(reviews, data_test, by="restaurant_geo")

combined_reviews_all[,12][combined_reviews_all$NAME == "Vatican City",] <- 'Rome'
combined_reviews_all<-combined_reviews_all %>% distinct()
combined_reviews_all <- combined_reviews_all[!(duplicated(combined_reviews_all)),]

romance_words <- read_csv("/Users/emiljanmrizaj/Downloads/romance_words.csv")
romance_words$...1<- NULL

romance_words$col <- 1:nrow(romance_words)
test <- romance_words %>% pivot_longer(-c(10))
test2 <- cbind(test,data.frame(names=test$value,chr=apply(test,2,nchar)[,3]))
test2<-test2[test2$chr > 5,]

keys <- romance_words %>%
  summarize_all(function(x) str_c(x, collapse ="|")) %>%
  pivot_longer(everything())

reviwed_all<-combined_reviews_all %>% 
  crossing(keys)  %>%
  mutate(observed = 1*str_detect(review_text, value)) %>%
  select(-value) %>%
  pivot_wider(values_from = observed)

combined_reviews_all$romantic<- ifelse(str_detect(combined_reviews_all$review_text, romance_words$romantic), 1, 0) 
combined_reviews_all$beautiful<-ifelse(str_detect(combined_reviews_all$review_text, romance_words$beautiful), 1, 0)
combined_reviews_all$lovely<-ifelse(str_detect(combined_reviews_all$review_text, romance_words$lovely), 1, 0)
combined_reviews_all$cozy<-ifelse(str_detect(combined_reviews_all$review_text, romance_words$cozy), 1, 0)
combined_reviews_all$cute<-ifelse(str_detect(combined_reviews_all$review_text, romance_words$cute), 1, 0)
combined_reviews_all$charming<-ifelse(str_detect(combined_reviews_all$review_text, romance_words$charming), 1, 0)
combined_reviews_all$romance<-ifelse(str_detect(combined_reviews_all$review_text, romance_words$romance), 1, 0)
combined_reviews_all$intimate<-ifelse(str_detect(combined_reviews_all$review_text, romance_words$intimate), 1, 0)
combined_reviews_all$date<-ifelse(str_detect(combined_reviews_all$review_text, romance_words$date), 1, 0)

combined_reviews_total <- bind_rows(combined_reviews_all1,
                                    combined_reviews_all2,
                                    combined_reviews_all3,
                                    combined_reviews_all4,
                                    combined_reviews_all5,
                                    combined_reviews_all6,
                                    combined_reviews_all7,
                                    combined_reviews_all8,
                                    combined_reviews_all9) %>% mutate(across(everything(), ~ replace_na(.x, 0)))

combined_reviews_total$cute <- ifelse(combined_reviews_total$all == 5, 1, 0)
combined_reviews_total$charming <- ifelse(combined_reviews_total$all == 6, 1, 0)
combined_reviews_total$romance <- ifelse(combined_reviews_total$all == 7, 1, 0)
combined_reviews_total$intimate <- ifelse(combined_reviews_total$all == 8, 1, 0)
combined_reviews_total$date <- ifelse(combined_reviews_total$all == 9, 1, 0)


