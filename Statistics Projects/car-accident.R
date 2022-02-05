#Car crash data
library(readxl)
library(readr)
#total cars was produced from the link provided in the methodology in google-drive
#it was extract from the website for motor vehicle totals and created as a .csv
total_cars <- read_excel("totalcars.xlsx")
#taken from the FARS data
vehicle <- read_csv("/Users/emiljanmrizaj/Downloads/FARS2018NationalCSV/Vehicle.CSV")
accident <- read_csv("/Users/emiljanmrizaj/Downloads/FARS2018NationalCSV/accident.CSV")
state_mile_average <- aggregate(MILEPT ~ STATENAME, accident, mean)
state_mile_sum <- aggregate(MILEPT ~ STATENAME, accident, sum)
state_accident_sum <- aggregate(VE_TOTAL ~ STATENAME, accident, sum)

#merge data
all_data <- merge(state_accident_sum, state_mile_average)
all_data <- merge(all_data, state_mile_sum, by="STATENAME")
names(all_data)<- c("States", "accidents", "average_miles", "total_miles")
all_data <- merge(all_data, total_cars, by="States")
y<-state_accident_sum[state_accident_sum$STATENAME == "District of Columbia",]
z<-state_mile_sum[state_mile_sum$STATENAME == "District of Columbia",]
state_mile_average$STATENAME[state_mile_average$STATENAME == "District of Columbia"] <- state_mile_sum$STATENAME[state_mile_sum$STATENAME == "District of Columbia"]
v <- state_mile_average[state_mile_average$STATENAME == "District of Columbia",]
w<-total_cars[33,]
vz <- merge(v, z, by="STATENAME")
vzy <- merge(vz, y)
vzyw <- merge(vzy, w)
vzyw <- vzyw[,-5]
names(vzyw) <- c("States" ,"average_miles", "total_miles", "accidents", "Total_vehicles")
all_data <- rbind(all_data, vzyw)
#counties
#avgmile_county_state <- aggregate(MILEPT ~ STATENAME + COUNTYNAME, accident, mean)
#accident_county_state <- aggregate(VE_TOTAL ~ STATENAME + COUNTYNAME, accident, sum)
#sumile_county_state <- aggregate(MILEPT ~ STATENAME + COUNTYNAME, accident, sum)
#all_data_counties <- merge(avgmile_county_state, accident_county_state, by=c("STATENAME", "COUNTYNAME"))
#all_data_counties <- merge(all_data_counties, sumile_county_state, by=c("STATENAME", "COUNTYNAME"))
#names(all_data_counties)<- c("States", "counties", "average_miles","accidents", "total_miles")

#vehicle and accident merge
#vehicle_accident <-merge(accident, vehicle, by=c("STATE", "ST_CASE"))
#vehicle_accident <- vehicle_accident[vehicle_accident$TRAV_SP !=  998,]
#vehicle_accident <- vehicle_accident[vehicle_accident$TRAV_SP !=  999,]
#remove large values
vehicle <- vehicle[vehicle$TRAV_SP !=  998,] 
vehicle <- vehicle[vehicle$TRAV_SP !=  999,] 

vehicle$TRAV_SP[vehicle$TRAV_SP ==  997] <- 150
vehicle2 <- vehicle[vehicle$TRAV_SP > 40,]
avgspeed_state <- aggregate(TRAV_SP ~ STATENAME, vehicle2, mean)
names(avgspeed_state) <- c("States", "avg_speed")
all_data <- merge(all_data, avgspeed_state, by="States")
x<- aggregate(PERSONS ~ STATENAME, accident, mean)
all_data$persons <- round(x$PERSONS, 0)
#accident rates
accident_rate1 <- (all_data$accidents)/(all_data$total_miles)

#accident rates by exposure (average speed)
accident_rate2 <- (all_data$accidents*all_data$avg_speed)/(all_data$total_miles)

#accident rates by average speed, total miles and total cars
accident_rate3 <- ((all_data$accidents*all_data$avg_speed)/(all_data$total_miles))/(all_data$Total_vehicles)

#accident rates by accident / total miles and number of cars
accident_rate4 <- (all_data$accidents)/(all_data$total_miles)/(all_data$Total_vehicles)

#calculation
x <-0
for(i in 1:length(accident_rate4)){
x[i]<-((accident_rate4[i]*1)^(1)*exp(-accident_rate4[i]*1))/factorial(1)
}
x
all_data <- cbind(all_data, cars_probability = x)
write_csv(all_data, "accidents-all.csv")

#ALTERNATIVELY!!!!!!!

#If you wanted to use multiple predictors in the data
#summarise the data that you need into averages, or sums
#however you would like to do it
#using all_data as an example

probability_1 <- ppois(1, lambda=predict(glm(accidents ~ average_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))
poiss_at_most_1 <- ppois(0, lambda=predict(glm(accidents ~ average_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))+ppois(1, lambda=predict(glm(accidents ~ average_miles+total_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))
poiss_at_most_2 <- ppois(0, lambda=predict(glm(accidents ~ average_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))+ppois(1, lambda=predict(glm(accidents ~ average_miles+total_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))+ppois(2, lambda=predict(glm(accidents ~ average_miles+total_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))
poiss_at_most_3 <- ppois(0, lambda=predict(glm(accidents ~ average_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))+ppois(1, lambda=predict(glm(accidents ~ average_miles+total_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))+ppois(2, lambda=predict(glm(accidents ~ average_miles+total_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))+ppois(3, lambda=predict(glm(accidents ~ average_miles+total_miles + Total_vehicles + avg_speed + persons, all_data, family=poisson(link=log)), lower.tail=TRUE))

poiss <- data.frame(probability_1,poiss_at_most_1, poiss_at_most_2, poiss_at_most_3)
all_data<-cbind(all_data, poiss_at_most_1, poiss_at_most_2,poiss_at_most_3)
all_data$Probability <- poiss$poiss
write.csv(poiss, "asset_2018_car_crash_average.csv")
#Counts statistics



###########################################################

ppois(1, lambda=predict(glm(VE_TOTAL ~ STATENAME + DAY_WEEK , accident, family=poisson(link=log)), lower.tail=TRUE))
#collects the probabilities from the glm
#1 represents 'k' in the wiki article for poisson distribution
#example plot
plot(ppois(1:3, lambda=predict(glm(accidents ~ total_miles + Total_vehicles + avg_speed, all_data, family=poisson(link=log)), lower.tail=TRUE)), type = "h", lwd=2, main = "Poisson distribution of car-accidents", ylab = "P(X = k)", xlab = "Events in each State")
