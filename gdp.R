#gdp stuff
library(tidyverse)
em <- read_csv("/Users/emiljanmrizaj/Downloads/emdat_public_2021_12_02_query_uid-s3E6W6.csv")
em <- em[, c(11:12, 14)] %>% distinct()
gdp <- read_csv("/Users/emiljanmrizaj/Downloads/API_NY/gpd2.csv")
natural <-  read_csv("/Volumes/Seagate/online downloads/natural_test.csv")
natural<-merge(em, natural, by="Country")
names(gdp)[2]<-"ISO"
natural_gdp <- left_join(natural, gdp, by="ISO")
natural_gdp$relative_damage <- natural_gdp$`Total Damages, Inflation Adjusted (US$)`/natural_gdp$`2018`
readr::write_excel_csv(natural_gdp, "natural_gdp222.csv")
