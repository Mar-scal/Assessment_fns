### checking on sable depth

require(ggplot2)
install.packages("concaveman")
require(concaveman)
require(sf)

load("Y:/Offshore scallop/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")
max(all.surv.dat$depth[all.surv.dat$bank=="Sab"])

ggplot() + geom_histogram(data=all.surv.dat[all.surv.dat$bank=="Sab",], aes(depth)) + facet_wrap(~year)

stations_sp <- sp::SpatialPointsDataFrame(coords = data.frame(all.surv.dat$lon[all.surv.dat$bank=="Sab"],
                                                              all.surv.dat$lat[all.surv.dat$bank=="Sab"]), 
                                          data = data.frame(all.surv.dat[all.surv.dat$bank=="Sab",]), proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

stations_sf <- st_as_sf(stations_sp)

concavehull <- concaveman(stations_sf, concavity=2, length_threshold=0)

source("Y:/Offshore scallop/Assessment/Assessment_fns/Maps/ScallopMap.r")
ScallopMap("Sab", plot.bathy = T, un=un.ID, pw=pwd.ID, db.con = "ptran")
plot(all.surv.dat$lon[all.surv.dat$bank=="Sab"], all.surv.dat$lat[all.surv.dat$bank=="Sab"], add=T)

seeds <- c(62200, 62299, 62300, 62301, 62302, 62305, 62304, 62305, 62306, 62307, 62308, 62309, 62310, 62311, 62334, 62335, 62336)
stations <- NULL
for(i in 1:length(unique(seeds))){
  stations[[i]] <- read.csv(paste0("Y:/Offshore scallop/Assessment/Data/Survey_data/2020/Spring/Sab/", unique(seeds)[i], "/Preliminary_Survey_design_Tow_locations_Sab.csv"))
  stations[[i]]$seed <- unique(seeds)[i]
}

plot(stations[[1]]$X, stations[[1]]$Y, bg="red", pch=21, add=T)


require(leaflet)

library(htmlwidgets)

sab <- 
  leaflet() %>%
  setView(-62, 45, 5)%>%
  addProviderTiles(provider = providers$Esri.OceanBasemap) %>%
  addPolygons(data=concavehull) %>%
  #addCircles(lng = all.surv.dat[all.surv.dat$bank=="Sab",]$lon, 
  #           lat = all.surv.dat[all.surv.dat$bank=="Sab",]$lat, weight = 0.5) %>%
  addCircles(lng = stations[[14]]$X,
             lat = stations[[14]]$Y,
             color = "red",
             label = paste0(unique(stations[[15]]$seed)))

saveWidget(sab, file="sab.html")



