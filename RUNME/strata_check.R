#Check tow placement relative to planned strata
require(ggplot2)
require(tidyverse)

load("Y:/Offshore/Assessment/Data/Survey_data/2022/Survey_summary_output/testing_results_spring2022_2.RData")

bank <- "BBn"
yr <- 2022

tows <- surv.dat[[bank]][surv.dat[[bank]]$year==yr & surv.dat[[bank]]$state=="live", which(names(surv.dat[[bank]]) %in% c("year", "tow", "cruise", "bank", "Strata_ID", "Strata_ID_new", "slon", "slat", "elon", "elat"))]

sheets <- readxl::excel_sheets("Y:/Offshore/Survey/SurveyWG/2022/LE15Stationlist_Spring2022_Final_SWG.xlsx")

planned <- readxl::read_excel("Y:/Offshore/Survey/SurveyWG/2022/LE15Stationlist_Spring2022_Final_SWG.xlsx", sheet = "BBn")

if(bank == "GBb") {
  planned$Strata_ID <- planned$Poly.ID+100
  gbbtows <- tows
}
if(bank == "GBa") {
  planned$Strata_ID <- planned$Poly.ID
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

ggplot() + geom_segment(data=tows, aes(x=slon, xend=elon, y=slat, yend=elat, colour=as.factor(Strata_ID))) +
  geom_point(data=planned, aes(x=Long.dd, y=Lat.dd, colour=as.factor(Strata_ID)))

compare <- dplyr::select(planned, `Station Number`, Strata_ID) %>%
  dplyr::rename(tow = `Station Number`,
                Strata_ID_planned = "Strata_ID") %>%
  dplyr::full_join(tows)

compare[which(!compare$Strata_ID == compare$Strata_ID_planned),]

strata_for_db <- rbind(gbatows, gbbtows)
strata_for_db <- rbind(bbntows, sabtows) #bbstows)

write.csv(strata_for_db, "Y:/Offshore/Assessment/Data/Survey_data/2022/Spring/strata_for_db.csv")
