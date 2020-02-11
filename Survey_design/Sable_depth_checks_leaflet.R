### checking on sable depth

require(ggplot2)
#install.packages("concaveman")
require(concaveman)
require(sf)
require(sp)
require(leaflet)
library(htmlwidgets)
require(tidyverse)
require(rgdal)

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
points(all.surv.dat$lon[all.surv.dat$bank=="Sab"], all.surv.dat$lat[all.surv.dat$bank=="Sab"], add=T)

sab_survey_leaflet <- function(seed) {
  stations <- read.csv(paste0("Y:/Offshore scallop/Assessment/Data/Survey_data/2020/Spring/Sab/", seed, "/Preliminary_Survey_design_Tow_locations_Sab.csv"))
  stations$seed <- seed
  
  #plot(stations[[1]]$X, stations[[1]]$Y, bg="red", pch=21, add=T)
  
  sab <- 
    leaflet() %>%
    setView(-62, 45, 5)%>%
    addProviderTiles(provider = providers$Esri.OceanBasemap) %>%
    addPolygons(data=concavehull) %>%
    # addCircles(lng = all.surv.dat[all.surv.dat$bank=="Sab",]$lon, 
    #            lat = all.surv.dat[all.surv.dat$bank=="Sab",]$lat,
    #            label = paste0(all.surv.dat[all.surv.dat$bank=="Sab",]$year)) %>%
    addCircles(lng = stations$X,
               lat = stations$Y,
               color = "red",
               label = paste0(unique(stations$seed)))
  
  saveWidget(sab, file="Y:/Offshore scallop/Assessment/2020/Survey_Design/sab.html")
}

# list.files("Y:/Offshore scallop/Assessment/Data/Survey_data/2020/Spring/Sab/")
# [1] "62200" NO                                          "62299" NO                                          "62300" NO                                    
# [4] "62301" NO                                          "62302" NO                                        "62303" NO                                      
# [7] "62304" NO                                        "62305" NO                                        "62306" NO                                       
# [10] "62307" maybe                                     "62308" NO                                        "62309" maybe                                 
# [13] "62310" NO                                        "62311" NO                                       "62312" NO                                      
# [16] "62313" NO                                        "62314"  NO                                     "62315" good along south but clumpy elsewhere 
# [19] "62316" NO                                        "62317" NO                                        "62318" NO                                       
# [22] "62319" NO                                        "62320" no                                    "62321" NO                                       
# [25] "62322" almost                                    "62323" maybe, hole-y                             "62324" NO                                      
# [28] "62325" maybe ***                             "62326" no                                        "62327" no                                       
# [31] "62328" maybe                                    "62329" no                                        "62330" no                                       
# [34] "62331" best yet?                                "62332" no                                        "62333" maybe                                     
# [37] "62334"                                           "62335"                                           "62336"           

sab_survey_leaflet(62309)


# Browns
seeds <- list.files("Y:/Offshore scallop/Assessment/Data/Survey_data/2020/Spring/BBn/")[!is.na(as.numeric(list.files("Y:/Offshore scallop/Assessment/Data/Survey_data/2020/Spring/BBn/")))]
stations <- NULL
for(i in 1:length(seeds)){
  station <- read.csv(paste0("Y:/Offshore scallop/Assessment/Data/Survey_data/2020/Spring/BBn/", seeds[i], "/Preliminary_Survey_design_Tow_locations_BBn.csv"))
  station$seed <- seeds[i]
  stations <- rbind(stations, station)
}
ggplot() + geom_point(data=stations[stations$seed %in% c(62311, 62315, 62310),], aes(X, Y)) + facet_wrap(~seed, nrow=1) + coord_map()

62297
62305
62307
62309
62310


# Browns south
load("Y:/Offshore/Assessment/Data/Survey_data/2018/Survey_summary_output/Survey_all_results.Rdata")

bbs_dat <- pivot_longer(data = surv.Live$BBs[, c("year", "tow", "lon", "lat", "pre", "rec", "com", "tot")], cols=c("pre", "rec", "com", "tot"))

bbs_dat$name <- factor(bbs_dat$name, levels=c("pre", "rec","com", "tot"))

ggplot() + geom_point(data=bbs_dat[bbs_dat$year > 2015,], aes(lon, lat, size=value)) + facet_grid(name~year) + coord_map() 

bbs_stns <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2020/Spring/BBs/Preliminary_Survey_design_Tow_locations_BBs.csv")

bbs_strata <- st_read("Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore_survey_strata/BBs.shp")

bbs_strata <- st_cast(bbs_strata, to = "MULTIPOLYGON")

png("Y:/Offshore/Assessment/2020/Survey_Design/BBs/Historical abundances vs. 2020 allocation.png", width=14, height=8, units="in", res=400)
ggplot(bbs_strata) + 
  geom_sf(aes(fill=as.factor(ID)), colour=NA, alpha=0.5) +
  geom_point(data=bbs_dat[bbs_dat$year > 2015,], aes(lon, lat, size=value), alpha=0.5) + facet_grid(year~name)+
  geom_point(data=bbs_stns, aes(X, Y), shape=21, colour="black", fill="white") + 
  theme_bw() +
  scale_size_continuous(name="Abundance estimate in year") +
  guides(fill=F) +
  coord_sf(datum=NA)
dev.off()
