#Check tow placement relative to planned strata
require(ggplot2)
require(tidyverse)

load("Y:/Offshore/Assessment/Data/Survey_data/2024/Survey_summary_output/testing_results_summer2024_Oct2.RData")

bank <- "GBa"
yr <- 2024

tows <- surv.dat[[bank]][surv.dat[[bank]]$year==yr & surv.dat[[bank]]$state=="live", which(names(surv.dat[[bank]]) %in% c("year", "tow", "cruise", "bank", "Strata_ID", "Strata_ID_new", "slon", "slat", "elon", "elat"))]

sheets <- readxl::excel_sheets("Y:/Offshore/Survey/SurveyWG/2024/LE20SurveyStations2024_GBa.xlsx")

planned <- readxl::read_excel("Y:/Offshore/Survey/SurveyWG/2024/LE20SurveyStations2024_GBa.xlsx", sheet = "LE20stationsGBa2024")

if(bank == "GBb") {
  #planned$Strata_ID <- planned$Strata_ID+100
  gbbtows <- tows
}
if(bank == "GBa") {
  planned$Strata_ID #<- planned$Poly.ID
  gbatows <- tows
}
if(bank == "BBn") {
  #planned$Strata_ID <- planned$Strata_ID+200
  bbntows <- tows
}
if(bank == "BBs") {
  bbstows <- tows
  planned$Strata_ID <- planned$Strata_ID+300
  planned$Strata_ID[planned$Strata_ID %in% c(303, 304)] <- planned$Strata_ID[planned$Strata_ID %in% c(303, 304)]+1
}
if(bank == "Sab") {
  tows <- tows %>% dplyr::rename(Strata_ID = "Strata_ID_new")
  sabtows <- tows
  sabtows <- arrange(sabtows, tow)
  #planned$Strata_ID <- planned$Strata_ID+500
}

source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/github_spatial_import.R")
shp <- github_spatial_import("offshore_survey_strata", "offshore_survey_strata.zip")
windows()
ggplot() + 
  geom_sf(data=shp[shp$label=="GBa",], aes(fill=Strt_ID), alpha=0.25)+
  # geom_segment(data=tows[tows$tow %in% 191:192,], aes(x=slon, xend=elon, y=slat, yend=elat, colour=as.factor(Strata_ID))) +
  # geom_text(data=tows[tows$tow %in% 191:192,], aes(x=slon, y=slat, label=tow)) +
  geom_point(data=planned[planned$Station_no %in% 191:193,], aes(x=Longitude, y=Latitude, colour=as.factor(Strata_ID))) +
  geom_text(data=planned[planned$Station_no %in% 191:193,], aes(x=Longitude, y=Latitude, label=Station_no)) +
  ggtitle("planned")+
  xlim(-66.05, -65.9)+
  ylim(41.65, 41.8)
windows()
ggplot() + 
  geom_sf(data=shp[shp$label=="GBa",], aes(fill=Strt_ID), alpha=0.25)+
  geom_segment(data=tows[tows$tow %in% 191:193,], aes(x=slon, xend=elon, y=slat, yend=elat, colour=as.factor(Strata_ID))) +
  geom_text(data=tows[tows$tow %in% 191:193,], aes(x=slon, y=slat, label=tow)) +
  # geom_point(data=planned[planned$Station_no %in% 191:192,], aes(x=Longitude, y=Latitude, colour=as.factor(Strata_ID))) +
  # geom_text(data=planned[planned$Station_no %in% 191:192,], aes(x=Longitude, y=Latitude, label=Station_no)) +
  ggtitle("survey")+
  xlim(-66.05, -65.9)+
  ylim(41.65, 41.8)

compare <- dplyr::select(planned, Station_no, Strata_ID) %>%
  dplyr::rename(tow = Station_no,
                Strata_ID_planned = "Strata_ID") %>%
  dplyr::full_join(tows)

compare[which(!compare$Strata_ID == compare$Strata_ID_planned),]

strata_for_db <- rbind(gbatows, gbbtows)
strata_for_db <- rbind(bbntows, sabtows) #bbstows)

write.csv(strata_for_db, "Y:/Offshore/Assessment/Data/Survey_data/2022/Spring/strata_for_db.csv")
