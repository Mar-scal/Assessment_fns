# Function to spatially plot offshore banks with historical survey data, fishery data, and proposed survey stations

plot_offshore_spatial(direct_data="Y:/Offshore/Assessment/", 
                      direct_fns = "C:/Users/keyserf/Documents/Github/",
                      survey_years=2019,
                      fishery_years=2019,
                      station_years=2020,
                      bank="Ger")

plot_offshore_spatial<- function(direct,
                                 survey_years,
                                 fishery_years, 
                                 station_years,
                                 bank) {
  
  # You need the following packages:
  require(leaflet)
  require(tidyverse)
  require(sf)
  
  # if survey_years isn't null, then get the data. This will always pull in the Survey_all_results data
  if(!is.null(survey_years)){
    load(paste0(direct_data, "Data/Survey_data/", max(as.numeric(survey_years)), "/Survey_summary_output/Survey_all_results.Rdata"))
    
    # this assumes you want to plot surv.Live stratified estimates per tow
    surv.Live.sub <- pivot_longer(surv.Live[[bank]][surv.Live[[bank]]$year %in% survey_years, c("year", "tow", "lon", "lat", "pre", "rec", "com", "tot")], cols=c("pre", "rec", "com", "tot"))
  }
  
  direct=direct1
  
  # if fishery_years isn't null, then get the data. This only works for >2008. Pulls in the new.log.dat
  if(!is.null(fishery_years)){
    source(paste0(direct_fns, "Assessment_fns/Fishery/logs_and_fishery_data.r"))
    logs_and_fish(loc="offshore", year = fishery_years, get.marfis = F, export = F, direct = direct_data)
    fishery <- new.log.dat[new.log.dat$bank==bank,]
  }
  
  # if station_years isn't null, then get the data. Pulls in the stations for the bank(s) you selected.
  if(!is.null(station_years)){
    stations <- NULL
    for(i in 1:length(banks)){
      bank <- read.csv(paste0(direct_data, "Data/Survey_data/", max(as.numeric(station_years)), 
                              "/Spring/", banks[i],"/Preliminary_Survey_design_Tow_locations_", banks[i], ".csv"))
      bank$bank <- banks[i]
      stations <- rbind(stations, bank)
    }
  }
  
  # now get the strata (if they exist)
  strata_banks <- c("GBa", "GBb", "BBn", "BBs", "Sab")
  if(any(strata_banks %in% banks)){
    strata_banks <- strata_banks[which(strata_banks %in% bank)]
    strata <- st_read(paste0(direct, "Data/Maps/approved/GIS_layers/offshore_survey_strata/", strata_banks[i], ".shp"))
  }
  if("Ger" %in% bank){
    strata <- st_read(paste0(direct_data, "Data/Maps/approved/Survey/German_WGS_84/WGS_84_German.shp"))
  }
  
  ggplot() + 
    theme_bw() + 
    geom_sf(data=st_cast(strata, "MULTIPOLYGON"), colour=NA) +
    geom_point(data=surv.Live.sub, aes(lon, lat, size=value)) +
    geom_point(data=stations, aes(X, Y)) +
    geom_point(data=)
    facet_wrap(~name)
    
}
