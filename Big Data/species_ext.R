library(tidyr)
library(data.table)
files <- list.files(path="bigdata/", pattern = ".csv", full.names = TRUE)
#define the column names in the data
cols <- c("family", "species","countryCode","stateProvince", "occurrenceStatus", "individualCount", 
          "decimalLatitude", "decimalLongitude", "eventDate", "year")
#define the column numbers
col_num <- c(8, 10,16,18, 19,20, 22, 23, 30, 33)
#
fil <- files[c(1:6)]
#combine the data together with rbindlist
result <- rbindlist(lapply(fil[1:2], function(x) setNames(
  fread(x, select = col_num, header = TRUE, sep="\t"), cols)), fill = TRUE) %>% dplyr::filter(year >= 2015) %>% tidyr::drop_na()
#Add 1st dataset to rest of them
#final <- rbind(data, result)

#add the rest
final <- rbind(final, result)

write.csv(result, "final_species3.csv")