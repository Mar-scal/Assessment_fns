# Function to spatially plot offshore banks with historical survey data, fishery data, and proposed survey stations

load_offshore_spatial <- function(direct_data,
                                  direct_fns,
                                  survey,
                                  fishery, 
                                  load_years) {
  
  # if survey_years isn't null, then get the data. This will always pull in the Survey_all_results data
  if(survey == T){
    load(paste0(direct_data, "Data/Survey_data/", max(as.numeric(load_years)), "/Survey_summary_output/Survey_all_results.Rdata"))
  }    
  
  # if fishery_years isn't null, then get the data. This only works for >2008. Pulls in the new.log.dat
  if(fishery == T){
    source(paste0(direct_fns, "Assessment_fns/Fishery/logs_and_fishery_data.r"))
    logs_and_fish(loc="offshore", year = load_years, get.marfis = F, export = F, direct = direct_data)
  }
  
  return(list(surv.Live=surv.Live, new.log.dat=new.log.dat))
  
}


offshore_data <- load_offshore_spatial(direct_data="Y:/Offshore/Assessment/", 
                                       direct_fns = "C:/Users/keyserf/Documents/Github/",
                                       survey=T,
                                       fishery=T,
                                       load_years=2018)


plot_offshore_spatial<- function(direct_data,
                                 direct_fns,
                                 offshore_data,
                                 overlay_data,
                                 station_years,
                                 survey_years,
                                 fishery_years,
                                 bank) {
  
  # You need the following packages:
  require(leaflet)
  require(tidyverse)
  require(sf)
  
  # tidy up the survey data
  if(overlay_data=="survey"){
    # this assumes you want to plot surv.Live stratified estimates per tow
    surv.Live.sub <- pivot_longer(offshore_data$surv.Live[[bank]]
                                  [offshore_data$surv.Live[[bank]]$year %in% survey_years,
                                    c("year", "tow", "lon", "lat", "pre", "rec", "com", "tot")], 
                                  cols=c("pre", "rec", "com", "tot"))
  }
  
  # tidy up the fishery data
  if(overlay_data=="fishery"){
    fishery <- offshore_data$new.log.dat[offshore_data$new.log.dat$bank==bank,]
  }
  
  # if station_years isn't null, then get the data. Pulls in the stations for the bank(s) you selected.
  if(!is.null(station_years)){
    stations <- read.csv(paste0(direct_data, "Data/Survey_data/", max(as.numeric(station_years)), 
                                "/Spring/", bank,"/Preliminary_Survey_design_Tow_locations_", bank, ".csv"))
  }
  
  # now get the strata (if they exist)
  strata_banks <- c("GBa", "GBb", "BBn", "BBs", "Sab")
  if(any(strata_banks %in% bank)){
    strata_banks <- strata_banks[which(strata_banks %in% bank)]
    strata <- st_read(paste0(direct_data, "Data/Maps/approved/GIS_layers/offshore_survey_strata/", strata_banks, ".shp"))
  }
  if("Ger" %in% bank){
    strata <- st_read(paste0(direct_data, "Data/Maps/approved/Survey/German_WGS_84/WGS_84_German.shp"))
  }
  
  size.var <- NULL
  facet_var_x <- NULL
  facet_var_y <- NULL
  if(overlay_data=="fishery") {
    size_var <- "pro.repwt"
    plotdat <- fishery
  }
  
  if(overlay_data=="survey") {
    size_var <- "value"
    plotdat <- surv.Live.sub
  }
  
  plotdat <- st_as_sf(plotdat, coords = c("lon", "lat"), crs = 4326)
  
  if(length(survey_years) > 1 | length(fishery_years)>1){
    facet_var_x <- "year"
  }
  
  if(length(survey_years) > 1 & overlay_data == "survey"){
    facet_var_y <- "name"
  }
  
  if(length(survey_years) == 1 & overlay_data == "survey"){
    facet_var_x <- "name"
  }
  
  
  plotting <- function(size_var, facet_var_x, facet_var_y) {
    browser()  
    # for the point size variable
    mapping <- aes(size = .data[[size_var]])
    if (is.null(size_var)) {
      mapping$size <- NULL
    }
    
    # for the facetting by year or survey data type
    if (!is.null(facet_var_x) & is.null(facet_var_y)) {
      facet <- facet_wrap(vars(.data[[facet_var_x]]))}
    if (!is.null(facet_var_x) & !is.null(facet_var_y)) {
      facet <- facet_grid(rows=vars(.data[[facet_var_x]]), cols=vars(.data[[facet_var_y]]))}
    if (is.null(facet_var_x) & is.null(facet_var_y)) {
      facet <- NULL}
  
    # basemap <- ggplot() +
    #   theme_bw() +
    #   geom_sf(data=st_cast(strata, "MULTIPOLYGON"), colour=NA, aes(fill=as.factor(ID)), alpha=0.5)

    ggplot(plotdat) + 
      theme_bw() +
      #geom_sf(data=st_cast(strata, "MULTIPOLYGON"), colour=NA, aes(fill=as.factor(ID)), alpha=0.5)+
      geom_sf(mapping, alpha=0.5) +
      #geom_point(data=stations, aes(X, Y), colour="black", fill="white", shape=21) +
      facet
    
  }
  
  print(plotting(size_var=size_var, facet_var_x=facet_var_x, facet_var_y=facet_var_y))
  
}



plot_offshore_spatial(direct_data="Y:/Offshore/Assessment/", 
                      direct_fns = "C:/Users/keyserf/Documents/Github/",
                      offshore_data=offshore_data,
                      overlay_data="fishery",
                      survey_years=c(2016,2018),
                      fishery_years=2019,
                      station_years=2020,
                      bank="BBs")

