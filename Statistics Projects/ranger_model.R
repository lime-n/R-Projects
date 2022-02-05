library(tidyverse)
weather2 <- readr::read_csv("/Users/emiljanmrizaj/Downloads/data_weather.csv")
weather2 <- weather2 %>% pivot_longer(-c(1))
weather2$name<-NULL
weather2_train <- weather2 %>% 
  split(if_else(runif(nrow(.)) <= 0.8, "train", "test"))
map_int(weather2_train, nrow)

weather_test <- weather2_train

weather2_train$train$value <- factor(weather2_train$train$value)
library(ranger)
rf <- ranger(formula =  value ~ ., 
             data = weather2_train$train,
             importance = "impurity",
             probability = TRUE,
             replace = TRUE, 
             sample.fraction = c(detection_freq, detection_freq))

rf_pred_train <- tibble(obs = occ_obs, pred = occ_pred)

#calibration_model <- scam(rf_pred_train$obs$value ~ s(pred, k = 5, bs = "mpi"), 
#                          gamma = 1.4,
#                          data = rf_pred_train)

pred_rf <- predict(rf, data = weather2, type = "response")
weather2$predictions <- pred_rf$predictions[, 2]
weather2$no_snow <- pred_rf$predictions[, 1]
weather2$value <- NULL
weather2<-weather2 %>% aggregate(. ~ Name, data=., mean)
