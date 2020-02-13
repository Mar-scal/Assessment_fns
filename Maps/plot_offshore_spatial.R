# Function to spatially plot offshore banks with historical survey data, fishery data, and proposed survey stations

load_offshore_spatial <- function(direct_data,
                                  direct_fns,
                                  survey,
                                  fishery, 
                                  survey_year,
                                  fishery_years) {
  
  # if survey_years isn't null, then get the data. This will always pull in the Survey_all_results data
  if(survey == T){
    load(paste0(direct_data, "Data/Survey_data/", survey_year, "/Survey_summary_output/Survey_all_results.Rdata"))
  }    
  
  # if fishery_years isn't null, then get the data. This only works for >2008. Pulls in the new.log.dat
  if(fishery == T){
    source(paste0(direct_fns, "Assessment_fns/Fishery/logs_and_fishery_data.r"))
    logs_and_fish(loc="offshore", year = fishery_years, get.marfis = F, export = F, direct = direct_data)
  }
  
  return(list(surv.Live=surv.Live, new.log.dat=new.log.dat))
  
}



plot_offshore_spatial<- function(direct_data,
                                 direct_fns,
                                 offshore_data,
                                 overlay_data,
                                 station_years,
                                 survey_years,
                                 fishery_years,
                                 size_class,
                                 bank, 
                                 plotly=F) {
  
  # You need the following packages:
  require(leaflet)
  require(tidyverse)
  require(sf)
  require(plotly)
  
  # tidy up the survey data if that's the data you want to overlay
  if(overlay_data=="survey"){
    # this assumes you want to plot surv.Live stratified estimates per tow
    surv.Live.sub <- pivot_longer(offshore_data$surv.Live[[bank]]
                                  [offshore_data$surv.Live[[bank]]$year %in% survey_years,
                                    c("year", "tow", "lon", "lat", "pre", "rec", "com", "tot")], 
                                  cols=c("pre", "rec", "com", "tot"))
  }
  
  # tidy up the fishery data if that's the data you want to overlay
  if(overlay_data=="fishery"){
    fishery <- offshore_data$new.log.dat[offshore_data$new.log.dat$bank==bank,]
  }
  
  # if station_years isn't null (i.e. you want to plot some stations), then get the data. Pulls in the stations for the bank(s) you selected.
  if(!is.null(station_years)){
    stations <- read.csv(paste0(direct_data, "Data/Survey_data/", max(as.numeric(station_years)), 
                                "/Spring/", bank,"/Preliminary_Survey_design_Tow_locations_", bank, ".csv"))
  }
  
  # now get the strata (if they exist)
  # the true strata are in the GIS_layers, but Ger "strata" (really bathymetry) are in a different location
  strata_banks <- c("GBa", "GBb", "BBn", "BBs", "Sab")
  if(any(strata_banks %in% bank)){
    strata_banks <- strata_banks[which(strata_banks %in% bank)]
    strata <- st_read(paste0(direct_data, "Data/Maps/approved/GIS_layers/offshore_survey_strata/", strata_banks, ".shp"))
  }
  if("Ger" %in% bank){
    strata <- st_read(paste0(direct_data, "Data/Maps/approved/Survey/German_WGS_84/WGS_84_German.shp"))
  }
  
  
  # so now the plan is to set up some dynamic variables for plotting the point size and facets relative to the data you decided to plot
  size.var <- NULL
  
  # if you're overlaying fishery data, then the point size is going to be the pro.repwt.  
  if(overlay_data=="fishery") {
    size_var <- "pro.repwt"
    plotdat <- fishery
    if(dim(plotdat)[1] == 0) stop("Requested data does not exist. Check offshore_data object to make sure this data exists.")
  }
  # if you're overlaying survey data, then the stratified estimate is going to be the pro.repwt. we're also going to facet by size class in this case.
  if(overlay_data=="survey") {
    size_var <- "value"
    plotdat <- surv.Live.sub
    if(!is.null(size_class)) plotdat <- plotdat[plotdat$name %in% size_class,]
    if(is.null(size_class)) size_class <- c("pre", "rec", "com", "tot")
    if(dim(plotdat)[1] == 0) stop("Requested data does not exist. Check offshore_data object to make sure this data exists.")
    plotdat$name <- factor(plotdat$name, levels=c("pre", 'rec', "com", "tot"))
  }
  
  # set up the basemap if there are strata, and a separate one if it's German, or if there are no strata
  if(bank %in% strata_banks){
    strata <- st_cast(strata, "MULTIPOLYGON")
    
    # set up the basemap. Keep the stations and points in normal dataframes because SF breaks the legends otherwise.
    basemap <- ggplot() +
      theme_bw() +
      geom_sf(data=strata, colour=NA, aes(fill=as.factor(ID)), alpha=0.5) +
      geom_point(data=stations, aes(X,Y), shape=21, colour="black", fill="white", size=2)
  }
  
  if(bank == "Ger"){
    strata <- st_cast(strata, "MULTIPOLYGON")
    
    # set up the basemap. Keep the stations and points in normal dataframes because SF breaks the legends otherwise.
    basemap <- ggplot() +
      theme_bw() +
      geom_sf(data=strata, colour=NA, alpha=0.5, fill="grey") +
      geom_point(data=stations, aes(X,Y), shape=21, colour="black", fill="white", size=2)
  }
  
  if(!bank == "Ger" & !bank %in% strata_banks){
    # set up the basemap. Keep the stations and points in normal dataframes because SF breaks the legends otherwise.
    basemap <- ggplot() +
      theme_bw() +
      geom_point(data=stations, aes(X,Y), shape=21, colour="black", fill="white", size=2)
  }
  
  # for the point size variable
  mapping <- aes(lon, lat, size = .data[[size_var]])
  if (is.null(size_var)) {
    mapping$size <- NULL
  }
  
  # now for the dynamic facetting. can't figure out how to make this go with the sf layers so I'm doing multiple versions
  # if you have more than one year of overlay data, facet by year
  if(overlay_data=="fishery"){
    finalplot <- basemap + 
      theme_bw() +
      geom_point(data=plotdat, mapping, alpha=0.25) +
      facet_wrap(~year) +
      scale_size_continuous(name=size_var, guide = "legend") +
      scale_fill_discrete(guide=FALSE) +
      ggtitle("Fishery data")
  }
  
  # if you're overlaying survey data, and the then we have to use a second facetting variable. 
  if(overlay_data == "survey" & (length(survey_years) <= length(size_class))){
    finalplot <- basemap + 
      theme_bw() +
      geom_point(data=plotdat, mapping, alpha=0.25) +
      facet_grid(year~name) +
      scale_size_continuous(name=size_var, guide = "legend")+
      scale_fill_discrete(guide=FALSE) +
      ggtitle("Survey data (stratified n per tow)")
  }
  if(overlay_data == "survey" & (length(survey_years) > length(size_class))){
    finalplot <- basemap + 
      theme_bw() +
      geom_point(data=plotdat, mapping, alpha=0.25) +
      facet_grid(name~year) +
      scale_size_continuous(name=size_var, guide = "legend")+
      scale_fill_discrete(guide=FALSE) +
      ggtitle("Survey data (stratified n per tow)")
  }
  
  if(plotly==F) print(finalplot)
  if(plotly==T) print(ggplotly(finalplot))
}



