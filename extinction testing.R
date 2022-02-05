#species aggregation
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ModelMetrics)
data_p <- fread("species_mean_sum.csv")
iucn <- fread("assessments2.csv")
iucn <- iucn[, 3]
names(iucn) <- "species"
data_species <- merge(iucn, data_p)
#convert to date
data_species$eventDate <- format(as.Date(data_species$eventDate), "%Y-%m")
data_species$eventDate <- ym(data_species$eventDate)
data_species$month <- month(data_species$eventDate)
#p <- data_species[, .(abundance = sum(abundance), count=sum(count)), by = .(species, eventDate, month, year)]
#subset the top 50 into the larger dataset
# filter the data based on counts something like 24+
st <- aggregate(geo_mean ~ species + year + month, data_species, sum)
#second permutation
#st2 <- st[!(st$species %in% first_permutation$species),]
#third permutation
#st3 <- st2[!(st2$species %in% second_permutation$species),]
#fourth permutation
#st4 <- st3[!(st3$species %in% third_permutation$species),]
#fifth permutation
#st5 <- st4[!(st4$species %in% fourth_permutation$species),]
#fifth permutation
#st6 <- st5[!(st5$species %in% fifth_permutation$species),]
#fifth permutation
#st7 <- st6[!(st6$species %in% sixth_permutation$species),]
#fifth permutation
#st8 <- st7[!(st7$species %in% seventh_permutation$species),]
#fifth permutation
#st9 <- st8[!(st8$species %in% eigth_permutation$species),]
#fifth permutation
#st10 <- st9[!(st9$species %in% eigth_permutation$species),]
#fifth permutation
#st11 <- st10[!(st10$species %in% eigth_permutation$species),]
#fifth permutation
#st12 <- st11[!(st11$species %in% eigth_permutation$species),]
#fifth permutation
#st13 <- st12[!(st12$species %in% eigth_permutation$species),]
#fifth permutation
#st14 <- st13[!(st13$species %in% eigth_permutation$species),]
#fifth permutation
#st15 <- st14[!(st14$species %in% eigth_permutation$species),]
#fifth permutation
#st16 <- st15[!(st15$species %in% eigth_permutation$species),]
#fifth permutation
#st17 <- st16[!(st16$species %in% eigth_permutation$species),]
#fifth permutation
#st18 <- st17[!(st17$species %in% eigth_permutation$species),]
#fifth permutation
#st19 <- st18[!(st18$species %in% eigth_permutation$species),]
#fifth permutation
#st20 <- st19[!(st19$species %in% eigth_permutation$species),]
#fifth permutation
#st22 <- st20[!(st20$species %in% eigth_permutation$species),]
#fifth permutation
#st22 <- st21[!(st21$species %in% eigth_permutation$species),]
#st23 <- st22[!(st22$species %in% eigth_permutation$species),]
#st24 <- st23[!(st23$species %in% eigth_permutation$species),]
#st25 <- st24[!(st24$species %in% eigth_permutation$species),]
#st26 <- st25[!(st25$species %in% eigth_permutation$species),]
st27 <- st26[!(st26$species %in% eigth_permutation$species),]
#st28 <- st27[!(st27$species %in% eigth_permutation$species),]

mim <- st27 %>% pivot_wider(names_from = species, values_from = geo_mean)
mim <- mim %>% arrange(year)
mim <- mim[mim$year != 2021,]
#select only for those with more than 50% of the data
mim <- mim[, which(colMeans(!is.na(mim)) > 0.5)]
#mim<-mim[-c(1:38),]
#mim <- mim[-c(2, 3, 6, 11, 14, 16, 18, 20, 23, 25, 28, 34, 36, 38),]
names(mim)[-c(1, 2)] <- gsub(" ", "_", names(mim[, -c(1, 2)]))
#impute values
mim[,-c(1, 2)] <- missRanger::missRanger(mim[,-c(1, 2)], pmm.k=3, num.trees = 100)


#calculating the differences in log10 geometric means
s.diff <- lapply(mim[, -c(1, 2)], diff)
c.col_sp <- do.call(cbind.data.frame, s.diff)
diff.sum <- data.frame(colMeans(c.col_sp[, , drop=FALSE]))
diff.sum <- tibble::rownames_to_column(diff.sum)
names(diff.sum) <- c("species", "difference")
asc.diff <- diff.sum %>% arrange(difference)
un_sp_cl <- data_species[,c(1,2)] %>% distinct()
asc.diff$species <- gsub("_", " ", asc.diff$species)
asc.diff <- inner_join(asc.diff, un_sp_cl, by="species")
asc.diff_all <- asc.diff[1:50,]
#first_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#second_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#third_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#fourth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#fifth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#sixth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#seventh_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#eigth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#ninth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#tenth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#eleventh_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#twelth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#thirteenth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#fourteenth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#fifteenth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#sixteenth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#seventeenth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#eighteenth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#nineteenth_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#twenty_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#twentyone_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#twentytwo_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#twentythree_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#twentyfour_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#twentyfive_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
#twentysix_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
twentyseven_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]
twentyeight_permutation <- asc.diff_all[!duplicated(asc.diff_all[,c(3)]),]

binded_all <- rbind(first_permutation,second_permutation,third_permutation,fourth_permutation,fifth_permutation,sixth_permutation,seventh_permutation,eigth_permutation
                    ,ninth_permutation,tenth_permutation,eleventh_permutation,twelth_permutation,thirteenth_permutation,fourteenth_permutation,fifteenth_permutation,
                    sixteenth_permutation,seventeenth_permutation,eighteenth_permutation,nineteenth_permutation,twenty_permutation
                    ,twentyone_permutation,twentytwo_permutation,twentythree_permutation,twentyfour_permutation,twentyfive_permutation, twentysix_permutation
                    ,twentyseven_permutation)

write.csv(binded_all, "top50_species.csv")

#monthly differences
c.col_sp$id <- 1:nrow(c.col_sp)
c.col_sp <- c.col_sp %>% pivot_longer(-c(129))
c.col_sp$name <- gsub("_", " ", c.col_sp$name)
c.col_sp <-c.col_sp[c.col_sp$name %in% asc.diff_all$species,]
c.col_sp <- c.col_sp %>% pivot_wider(names_from = name, values_from = value)
write_csv(c.col_sp, "species_diff.csv")

#select top 50
mim.piv <- mim.l %>% pivot_longer(-c(1, 2))
mim.piv$season <- ifelse(mim.piv$month %in% c(3, 4, 5), "Spring",
                         ifelse(mim.piv$month %in% c(6, 7, 8), "Summer",
                                ifelse(mim.piv$month %in% c(9, 10, 11), "Autumn",
                                       ifelse(mim.piv$month %in% c(12, 1, 2), "Winter", NA)
                                )
                         )
)
names(mim.piv) <- c("year", "month","species","geometric_mean","season")
mim.piv$species <- gsub("_"," ", mim.piv$species)
top_50<-mim.piv[mim.piv$species %in% asc.diff_all$species,]

top_50.wide <- top_50 %>% pivot_wider(names_from = species, values_from = geometric_mean)
#top_50.wide$month <- month(top_50.wide$eventDate)
nest_top_50<-top_50.wide %>% 
  nest(data = c("month", "eventDate", "season", "Anarhynchus_frontalis", "Charadrius_montanus", "Phoeniconaias_minor", 
                "Leucocarbo_chalconotus", "Calidris_tenuirostris", "Leucosticte_atrata", 
                "Larosterna_inca", "Leucosticte_australis", "Calcarius_ornatus", 
                "Amazona_viridigenalis", "Calidris_ferruginea", "Charadrius_bicinctus", 
                "Sterna_striata", "Anser_erythropus", "Limosa_lapponica", "Pycnonotus_taivanus", 
                "Gymnorhinus_cyanocephalus", "Emberiza_rustica", "Dendrocygna_arborea", 
                "Pelecanus_philippensis", "Lonchura_oryzivora", "Phoenicopterus_chilensis", 
                "Clangula_hyemalis", "Numenius_arquata", "Tringa_brevipes", "Acridotheres_javanicus", 
                "Hesperiphona_vespertina", "Carpornis_cucullata", "Numenius_madagascariensis", 
                "Fratercula_arctica", "Calidris_pusilla", "Chloropsis_cochinchinensis", 
                "Mycteria_leucocephala", "Bangsia_melanochlamys", "Lophura_swinhoii", 
                "Haematopus_ostralegus", "Hypopyrrhus_pyrohypogaster", "Nestor_meridionalis", 
                "Necrosyrtes_monachus", "Quiscalus_quiscula", "Alca_torda", "Iridosornis_porphyrocephalus", 
                "Euphagus_carolinus", "Threskiornis_melanocephalus", "Anhinga_melanogaster", 
                "Psittacula_alexandri", "Balearica_regulorum", "Charadrius_obscurus", 
                "Numenius_tahitiensis", "Amazona_leucocephala"))

x = c("month", "eventDate", "season", "Anarhynchus_frontalis", "Charadrius_montanus", "Phoeniconaias_minor", 
      "Leucocarbo_chalconotus", "Calidris_tenuirostris", "Leucosticte_atrata", 
      "Larosterna_inca", "Leucosticte_australis", "Calcarius_ornatus", 
      "Amazona_viridigenalis", "Calidris_ferruginea", "Charadrius_bicinctus", 
      "Sterna_striata", "Anser_erythropus", "Limosa_lapponica", "Pycnonotus_taivanus", 
      "Gymnorhinus_cyanocephalus", "Emberiza_rustica", "Dendrocygna_arborea", 
      "Pelecanus_philippensis", "Lonchura_oryzivora", "Phoenicopterus_chilensis", 
      "Clangula_hyemalis", "Numenius_arquata", "Tringa_brevipes", "Acridotheres_javanicus", 
      "Hesperiphona_vespertina", "Carpornis_cucullata", "Numenius_madagascariensis", 
      "Fratercula_arctica", "Calidris_pusilla", "Chloropsis_cochinchinensis", 
      "Mycteria_leucocephala", "Bangsia_melanochlamys", "Lophura_swinhoii", 
      "Haematopus_ostralegus", "Hypopyrrhus_pyrohypogaster", "Nestor_meridionalis", 
      "Necrosyrtes_monachus", "Quiscalus_quiscula", "Alca_torda", "Iridosornis_porphyrocephalus", 
      "Euphagus_carolinus", "Threskiornis_melanocephalus", "Anhinga_melanogaster", 
      "Psittacula_alexandri", "Balearica_regulorum", "Charadrius_obscurus", 
      "Numenius_tahitiensis", "Amazona_leucocephala")

#extras


#mim <- mim[ , apply(mim, 2, function(x) !any(is.na(x)))]


#take the log by taking the inverse log of the geometric means
#mim[mim == 0 ] <- 1
#mim.l <- lapply(mim[, -c(1, 2)], log10)
#mim.l<-do.call(cbind.data.frame, mim.l)
#mim.l <- cbind(mim[, c(1, 2)], mim.l)
#mim20 <- mim[,1:1000]

#split the years

#library(lubridate)

#top2019$year <- year(top2019$eventDate)
#top2020$year <- year(top2020$eventDate)#

#top2019 <- top2019[, c(54, 1:53)]
#top2020 <- top2020[, c(54, 1:53)]

nest_mim<-top_50.wide %>% 
  nest(data = x)

mim.2019 <- data.frame(nest_mim$data[1])
mim.2020 <- data.frame(nest_mim$data[2])



model <- nls(Psittacula_alexandri ~ SSlogis(month, a, b, c), data=top_50.wide)

lines(top_50.wide$month[1:12], predict(model))
model2 <- glm(Charadrius_mongolus ~ month, data=c.col_sp)
model <- lm(Charadrius_mongolus ~ month, data=c.col_sp)

plot(top_50.wide$month, top_50.wide$Psittacula_alexandri)
lines(fitted(model1))
model1 <- gam(Psittacula_alexandri ~ s(month, k=12) + s(as.factor(season)), data=top_50.wide,family=poisson, method='REML')
################################################################################
#####################END THIS HERE##############################################


#extract certain values
u <- data_species %>% filter(substr(species, 1, 1) %in% "U")



#Create seasons from months
mim.piv$season <- ifelse(mim.piv$month %in% c(3, 4, 5), "Spring",
                      ifelse(mim.piv$month %in% c(6, 7, 8), "Summer",
                             ifelse(mim.piv$month %in% c(9, 10, 11), "Autumn",
                                    ifelse(mim.piv$month %in% c(12, 1, 2), "Winter", NA)
                             )
                      )
)

################################################################################

#select those only in years 2019-2020
mon_species[, year := year(eventDate)]
m.m <- mon_species[mon_species$year %in% c(2019, 2020),]
mim <- m.m %>% pivot_wider(names_from = species, values_from = abundance)
mim <- mim[ , apply(mim, 2, function(x) !any(is.na(x)))]

#test
mim <- p[count > 50, -4] %>% pivot_wider(names_from = species, values_from = mean)
mim <- mim %>% arrange(eventDate)
#mim<-mim[-c(1:38),]
#mim <- mim[-c(2, 3, 6, 11, 14, 16, 18, 20, 23, 25, 28, 34, 36, 38),]
names(mim)[-c(1, 2)] <- gsub(" ", "_", names(mim[, -c(1, 2)]))
mim <- mim[ , apply(mim, 2, function(x) !any(is.na(x)))]




#ranger test
names(mim)[-c(1, 2)] <- gsub(" ", "_", names(mim[, -c(1, 2)]))
mim2 <- mim[, 1:5]
library(missRanger)
mimNA <- generateNA(mim2, seed = 5)
View(mimNA)
mimForest <- missRanger(mimNA, pmm.k=3, num.trees = 100)

#change names
names(mim)[-c(1, 2)] <- gsub(" ", "_", names(mim[, -c(1, 2)]))

x = list()
multi.nls <- function(x){
  for(i in length(names(x)[-c(1, 2)])){
    x[i] <- nls(i)
  }
}




#calculate imputation matrix
library(missRanger)
library(caret)
names(mim)[-c(1, 2)] <- gsub(" ", "", nm)

mn <- mim[1:12, 1:7]
mimNA <- mn
mimNA[, -c(1, 2)]<- generateNA(mimNA[, -c(1, 2)], seed=5)
confusionMatrix(as.factor(mim[, -c(1, 2)]), as.factor(mRan))


mRan <- missRanger(mimNA[, -c(1, 2)], pmm.k=4, num.trees = 100)

s <- mon_species[year %in% c(2019, 2020, 2021),,by=species ]
split_species <- split(s, s$species)

m.m <- mon_species[mon_species$year %in% c(2019, 2020),]






piv.wid <- mon_species %>% pivot_wider(names_from = species, values_from = abundance)
p.lon <- piv.wid %>% pivot_longer(-c(1:2))
piv.wid[complete.cases(piv.wid),]
