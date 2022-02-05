#Electrical vehicle testing
library(tidyverse)
electric <- read_csv("electric.csv")
names(electric) <- c("year", "code", "fuel", "co2", "co2_2", "veh_reg")
electric$fuel <- stringr::str_to_lower(electric$fuel)
electric_all <- electric %>% select(1, 2, 6) %>% aggregate(. ~ year+ code, data=., sum)
el <- electric[electric$fuel %in% "electric",]
el_merge <- merge(el, electric_all, by=c("year", "code"))
el_merge<-el_merge[, c(1, 2, 6,7)]
names(el_merge) <- c("year", "code","reg", "veh_reg")
el_merge$proportion <- (el_merge$reg/el_merge$veh_reg)*100
el_merge<- el_merge[, c(1, 2, 5)]
el_wider <- el_merge %>% pivot_wider(names_from = code, values_from = proportion)
el_wider <- el_wider %>% unnest()
el_wider <- el_wider %>% pivot_longer(-c(1))
el_wider <- el_wider %>% aggregate(. ~ year+name, data=., mean)
el_wider <- el_wider %>% pivot_wider(names_from = name, values_from = value)
el_wider[1, 13] <- el_wider[1, 32]
el_wider$UK <- NULL


#prediction
elec = ts(el_wider[, -1], start=2010, end=2020)
data_list <- apply(elec,2,function(xx)(forecast(ets(xx,model="ZAZ"))))
elec_cf <- NULL
for(i in 1:30){
  elec_cf[[i]] <- data_list[i][[1]]$mean[1:length(data_list[i][[1]]$mean)]
}
elec_cf <- elec_cf %>% unlist() %>% as.data.frame(.) %>% mutate(year = rep(2021:2030, 30))
elec_cft <- replicate(10, c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
                            "EE", "ES", "FI", "FR", "GB", "GR", "HR", "HU", "IE", "IS", "IT", 
                            "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", 
                            "SK"))  %>% as.data.frame() %>% pivot_longer(-c(0)) %>% select(-1) %>% cbind(elec_cf)
names(elec_cft) <- c("code", "prediction", "year")
elec_cft <- elec_cft %>% pivot_wider(names_from = code, values_from = prediction)
test_data <- rbind(el_wider, elec_cft)

