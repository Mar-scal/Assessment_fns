fishery_footprint_history <- function(bnk, years, months=NULL, high=25, highcpue = 30, subarea=NULL, survey, other, fish.dat, direct_fns) {
  
  source(paste(direct_fns,"Fishery/fishery.dat.r",sep=""))
  source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))
  source(paste(direct_fns,"Survey_and_OSAC/gridPlot.r",sep=""))
  source(paste(direct_fns,"Maps/github_spatial_import.R",sep=""))
  
  require(sf)
  require(PBSmapping)
  require(maptools)
  
  # Read2 Get the survey boundary polygons for all banks.
  # survey.bound.polys<-read.csv(paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
  #                              header=T,stringsAsFactors = F)
  # newAreaPolys<-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep="")
  #                        ,stringsAsFactors = F,header=T)
  
  if(!is.null(subarea)) {
    if(subarea=="none") subarea <- NULL
  }
  
  survey <- survey[survey$ID==bnk,]
  if(!is.null(subarea)) other <- other[other$ID==subarea,]
  
  survey_polyset <- SpatialPolygons2PolySet(as_Spatial(st_geometry(survey)))
  if(!is.null(subarea)) other_polyset <- SpatialLines2PolySet(as_Spatial(st_geometry(other)))
  if(is.null(subarea)) bnk.survey.bound.poly <- survey_polyset
  if(!is.null(subarea)) bnk.survey.bound.poly <- other_polyset
  
  yr <- max(years)
  # bnk.survey.bound.poly <- bnk.survey.bound.poly[bnk.survey.bound.poly$startyear==max(bnk.survey.bound.poly$startyear),]
  # if(!is.null(subarea)) bnk.survey.bound.poly <- newAreaPolys[newAreaPolys$label==subarea,]
  lvls <-  c(10,50,100,500,1000,5000,10000,50000)   # Set the levels, not important for the moment...
  lvls.eff <- c(10,50,100,1000)
  grids = 1/60 # 1/60 = 1 minute grids
  fish.cells <- NULL
  eff.cells <- NULL
  wf.fish.cells <- NULL
  wf.eff.cells <- NULL
  ft.fish.cells <- NULL
  ft.eff.cells <- NULL
  if(is.null(months)) time <- data.frame(year=years)
  if(!is.null(months)) time <- arrange(expand.grid(year=years, month=months), year)
  
  for(k in 1:nrow(time))
  {
    # subset out subarea if necessary
    if(!is.null(subarea)) {
      bnk.survey.bound.poly_sf <- st_as_sf(PolySet2SpatialPolygons(as.PolySet(bnk.survey.bound.poly, projection="LL")))
      bnk.survey.bound.poly_sf <- st_transform(bnk.survey.bound.poly_sf, 4326)
      fish.dat_sf <- st_as_sf(fish.dat, coords=c("lon", "lat"), remove=F, crs=4326)
      fish.dat_sf <- st_intersection(fish.dat_sf, bnk.survey.bound.poly_sf)
      st_geometry(fish.dat_sf) <- NULL
      fish.dat <- fish.dat_sf
    }
    
    fish.dat$month <- month(fish.dat$date)
    
    if(is.null(months)){
      bnk.fish.dat <- subset(fish.dat, bank== bnk & year == time$year[k], select = c('ID','lon','lat','pro.repwt'))
      bnk.effort.dat <- subset(fish.dat, bank== bnk & year == time$year[k], select = c('ID','lon','lat','hm'))
      wf.fish.dat <- subset(fish.dat, bank== bnk & year == time$year[k] & fleet == "WF",select = c('ID','lon','lat','pro.repwt'))
      ft.fish.dat <- subset(fish.dat, bank== bnk & year == time$year[k] & fleet == "FT",select = c('ID','lon','lat','pro.repwt'))
      wf.effort.dat <- subset(fish.dat, bank== bnk & year == time$year[k] & fleet == "WF",select = c('ID','lon','lat','hm'))
      ft.effort.dat <- subset(fish.dat, bank== bnk & year == time$year[k] & fleet == "FT",select = c('ID','lon','lat','hm'))
    }
    
    if(!is.null(months)){
      bnk.fish.dat <- subset(fish.dat, bank== bnk & year == time[k,]$year & month == time[k,]$month, select = c('ID','lon','lat','pro.repwt'))
      bnk.effort.dat <- subset(fish.dat, bank== bnk & year == time[k,]$year & month == time[k,]$month,select = c('ID','lon','lat','hm'))
      wf.fish.dat <- subset(fish.dat, bank== bnk & year == time[k,]$year & month == time[k,]$month & fleet == "WF",select = c('ID','lon','lat','pro.repwt'))
      ft.fish.dat <- subset(fish.dat, bank== bnk & year == time[k,]$year & month == time[k,]$month& fleet == "FT",select = c('ID','lon','lat','pro.repwt'))
      wf.effort.dat <- subset(fish.dat, bank== bnk & year == time[k,]$year & month == time[k,]$month& fleet == "WF",select = c('ID','lon','lat','hm'))
      ft.effort.dat <- subset(fish.dat, bank== bnk & year == time[k,]$year & month == time[k,]$month& fleet == "FT",select = c('ID','lon','lat','hm'))
    }
    
    names(bnk.fish.dat)[1:4]<-c("EID","X","Y","Z")
    names(bnk.effort.dat)[1:4]<-c("EID","X","Y","Z")
    names(wf.fish.dat)[1:4]<-c("EID","X","Y","Z")
    names(wf.effort.dat)[1:4]<-c("EID","X","Y","Z")
    names(ft.fish.dat)[1:4]<-c("EID","X","Y","Z")
    names(ft.effort.dat)[1:4]<-c("EID","X","Y","Z")
    
    # Remove any NA's from the data and give a warning
    if(any(is.na(bnk.fish.dat)==T)) 
    {
      message(paste("Heads up", bnk[i], "has NA's in it, check your data!! I will remove and continue calculations for now"))
      bnk.fish.dat <- na.omit(bnk.fish.dat)
      bnk.effort.dat <- na.omit(bnk.effort.dat)
      ft.fish.dat <- na.omit(ft.fish.dat)
      ft.effort.dat <- na.omit(ft.effort.dat)
      wf.fish.dat <- na.omit(wf.fish.dat)
      wf.effort.dat <- na.omit(wf.effort.dat)
      
    } # if(any(is.na(bnk.fish.dat)==T)) 
    
    if(is.null(subarea)) area <- bnk
    if(!is.null(subarea)) area <- subarea
    
    #Get the total removals from each 1 minute cell if there was a fishery in a given year...
    if(nrow(bnk.fish.dat) > 0)
    {
      bank.spatial <- gridPlot(bnk.fish.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
      eff.spatial <- gridPlot(bnk.effort.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
      
      # source(paste(direct_fns,"/Maps/ScallopMap.r",sep="")) #Source3 
      # print(ScallopMap(bnk,poly.lst=bank.spatial[1:2],poly.border=bank.spatial[[2]]$border,xlab="",ylab="",
      #            title=paste0("Catch: ", area, " - ", years[k]), bathy.source="quick",
      #            plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct, direct_fns=direct_fns,cex.mn=2,dec.deg = F))
      # 
      # print(ScallopMap(bnk,poly.lst=eff.spatial[1:2],poly.border=eff.spatial[[2]]$border,xlab="",ylab="",
      #            title=paste0("Effort: ", area, " - ", years[k]), bathy.source="quick",
      #            plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct, direct_fns=direct_fns,cex.mn=2,dec.deg = F))
      
      
      #########  Now make a summary of the  fishery catch in each cell on each bank and for the WF and FT fleets.
      test <- NULL
      test.eff <- NULL
      for(j in 1:length(bank.spatial[[2]]$PID)) {test[[j]] <- subset(bank.spatial[[1]],PID %in% bank.spatial[[2]]$PID[j] & SID %in% bank.spatial[[2]]$SID[j])}
      for(j in 1:length(eff.spatial[[2]]$PID)) {test.eff[[j]] <- subset(eff.spatial[[1]],PID %in% eff.spatial[[2]]$PID[j] & SID %in% eff.spatial[[2]]$SID[j])}
      
      # Pull the data from list into a dataframe
      res <- do.call("rbind",test)
      res.eff <- do.call("rbind",test.eff)
      
      # Then punch that into a list with some other info included.
      if(is.null(months)) {
        fish.cells[[as.character(years[k])]] <- cbind(calcCentroid(res,rollup=3),catch=bank.spatial[[2]]$Z,
                                                      bank=rep(bnk,length(bank.spatial[[2]]$Z)),year=rep(years[k],length(bank.spatial[[2]]$Z)))
        eff.cells[[as.character(years[k])]] <- cbind(calcCentroid(res.eff,rollup=3),effort=eff.spatial[[2]]$Z,
                                                     bank=rep(bnk,length(eff.spatial[[2]]$Z)),year=rep(years[k],length(eff.spatial[[2]]$Z)))
      }
      if(!is.null(months)) {
        fish.cells[[as.character(paste0(time[k,]$year, ".", time[k,]$month))]] <- cbind(calcCentroid(res,rollup=3),catch=bank.spatial[[2]]$Z,
                                                      bank=rep(bnk,length(bank.spatial[[2]]$Z)),year=rep(time[k,]$year,length(bank.spatial[[2]]$Z)),month=rep(time[k,]$month,length(bank.spatial[[2]]$Z)))
        eff.cells[[as.character(paste0(time[k,]$year, ".", time[k,]$month))]] <- cbind(calcCentroid(res.eff,rollup=3),effort=eff.spatial[[2]]$Z,
                                                     bank=rep(bnk,length(eff.spatial[[2]]$Z)),year=rep(time[k,]$year,length(eff.spatial[[2]]$Z)),month=rep(time[k,]$month,length(bank.spatial[[2]]$Z)))
      }
    } # if(nrow(bnk.fish.dat) > 0)
    
    # Wet fishery...
    # if(nrow(wf.fish.dat) > 0)
    # {
    #   wf.bank.spatial <- gridPlot(wf.fish.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
    #   wf.eff.spatial <- gridPlot(wf.effort.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
    #   
    #   wf.test <- NULL
    #   wf.test.eff <- NULL
    #   for(j in 1:length(wf.bank.spatial[[2]]$PID)) wf.test[[j]] <- subset(wf.bank.spatial[[1]],PID %in% wf.bank.spatial[[2]]$PID[j] & 
    #                                                                         SID %in% wf.bank.spatial[[2]]$SID[j])
    #   for(j in 1:length(wf.eff.spatial[[2]]$PID))  wf.test.eff[[j]] <- subset(wf.eff.spatial[[1]],PID %in% wf.eff.spatial[[2]]$PID[j] & 
    #                                                                             SID %in% wf.eff.spatial[[2]]$SID[j])
    #   wf.res <- do.call("rbind",wf.test)
    #   wf.res.eff <- do.call("rbind",wf.test.eff)
    #   wf.fish.cells[[as.character(years[k])]] <- cbind(calcCentroid(wf.res,rollup=3),catch=wf.bank.spatial[[2]]$Z,
    #                                                    bank=rep(bnk,length(wf.bank.spatial[[2]]$Z)),year=rep(years[k],length(wf.bank.spatial[[2]]$Z)))
    #   wf.eff.cells[[as.character(years[k])]] <- cbind(calcCentroid(wf.res.eff,rollup=3),effort=wf.eff.spatial[[2]]$Z,
    #                                                   bank=rep(bnk,length(wf.eff.spatial[[2]]$Z)),year=rep(years[k],length(wf.eff.spatial[[2]]$Z)))
    # } # end if(nrow(wf.fish.dat) > 0)
    # 
    # # Freezers......
    # if(nrow(ft.fish.dat) > 0)
    # {
    #   ft.bank.spatial <- gridPlot(ft.fish.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
    #   ft.eff.spatial <- gridPlot(ft.effort.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
    #   ft.test <- NULL
    #   ft.test.eff <- NULL
    #   for(j in 1:length(ft.bank.spatial[[2]]$PID)) ft.test[[j]] <- subset(ft.bank.spatial[[1]],PID %in% ft.bank.spatial[[2]]$PID[j] & 
    #                                                                         SID %in% ft.bank.spatial[[2]]$SID[j])
    #   for(j in 1:length(ft.eff.spatial[[2]]$PID))  ft.test.eff[[j]] <- subset(ft.eff.spatial[[1]],PID %in% ft.eff.spatial[[2]]$PID[j] & 
    #                                                                             SID %in% ft.eff.spatial[[2]]$SID[j])
    #   
    #   ft.res <- do.call("rbind",ft.test)
    #   ft.res.eff <- do.call("rbind",ft.test.eff)
    #   ft.fish.cells[[as.character(years[k])]] <- cbind(calcCentroid(ft.res,rollup=3),catch=ft.bank.spatial[[2]]$Z,
    #                                                    bank=rep(bnk,length(ft.bank.spatial[[2]]$Z)),year=rep(years[k],length(ft.bank.spatial[[2]]$Z)))
    #   ft.eff.cells[[as.character(years[k])]] <- cbind(calcCentroid(ft.res.eff,rollup=3),effort=ft.eff.spatial[[2]]$Z,
    #                                                   bank=rep(bnk,length(ft.eff.spatial[[2]]$Z)),year=rep(years[k],length(ft.eff.spatial[[2]]$Z)))
    # } # end if(nrow(bnk.fish.dat) > 0)
    # 
  } # end for(k in 1:length(years))
  
  
  # And unwrap all this crap into something useful..
  spatial.fishery.history <- do.call("rbind",fish.cells)
  spatial.effort.history <-  do.call("rbind",eff.cells)
  # spatial.ft.fishery.history <- do.call("rbind",ft.fish.cells)
  # spatial.ft.effort.history <-  do.call("rbind",ft.eff.cells)
  # spatial.wf.fishery.history <- do.call("rbind",wf.fish.cells)
  # spatial.wf.effort.history <-  do.call("rbind",wf.eff.cells)
  
  
  if(is.null(months)) {
    colnames(spatial.fishery.history) <- c("PID","SID","lon","lat","catch","bank","year")
    colnames(spatial.effort.history) <- c("PID","SID","lon","lat","effort","bank","year")
  }
  
  if(!is.null(months)) {
    spatial.fishery.history$year <- paste0(spatial.fishery.history$year, ".", spatial.fishery.history$month)
    spatial.effort.history$year <- paste0(spatial.effort.history$year, ".", spatial.effort.history$month)
    colnames(spatial.fishery.history) <- c("PID","SID","lon","lat","catch","bank","year", "month")
    colnames(spatial.effort.history) <- c("PID","SID","lon","lat","effort","bank","year", "month")
  }
  # colnames(spatial.ft.fishery.history) <- c("PID","SID","lon","lat","catch","bank","year")
  # colnames(spatial.ft.effort.history) <- c("PID","SID","lon","lat","effort","bank","year")
  # colnames(spatial.wf.fishery.history) <- c("PID","SID","lon","lat","catch","bank","year")
  # colnames(spatial.wf.effort.history) <- c("PID","SID","lon","lat","effort","bank","year")
  
  # Turn catch into tonnage...
  spatial.fishery.history$catch <- spatial.fishery.history$catch/1000
  # spatial.ft.fishery.history$catch <- spatial.ft.fishery.history$catch/1000
  # spatial.wf.fishery.history$catch <- spatial.wf.fishery.history$catch/1000
  # 
  ## Now create some annual data summaries
  # Quick check that I'm getting what I want...
  tot.catch <- aggregate(catch~year,data = spatial.fishery.history,FUN=sum)
  tot.effort <- aggregate(effort~year,data = spatial.effort.history,FUN=sum)
  # tot.ft.catch <- aggregate(catch~year,data = spatial.ft.fishery.history,FUN=sum)
  # tot.ft.effort <- aggregate(effort~year,data = spatial.ft.effort.history,FUN=sum)
  # tot.wf.catch <- aggregate(catch~year,data = spatial.wf.fishery.history,FUN=sum)
  # tot.wf.effort <- aggregate(effort~year,data = spatial.wf.effort.history,FUN=sum)
  #
  # # Now how many cells were fished each year...
  cells.fished <- aggregate(catch~year,data = spatial.fishery.history,FUN=length)
  # cells.ft.fished <- aggregate(catch~year,data = spatial.ft.fishery.history,FUN=length)
  # cells.wf.fished <- aggregate(catch~year,data = spatial.wf.fishery.history,FUN=length)
  names(cells.fished) <- c("year","cells_fished")
  # names(cells.ft.fished) <- c("year","cells_fished")
  # names(cells.wf.fished) <- c("year","cells_fished")
  # considering any cells in which more than 10 tonnes was caught
  high.catch <- subset(spatial.fishery.history, catch >= high)
  # high.ft.catch <- subset(spatial.ft.fishery.history, catch >= high)
  # high.wf.catch <- subset(spatial.wf.fishery.history, catch >= high)
  # The number of high catch cells in a year.
  n.high <- arrange(full_join(aggregate(catch~ year,high.catch,length), data.frame(year=unique(cells.fished$year))), year, by="year")
  # ft.n.high <- arrange(full_join(aggregate(catch~ year,high.ft.catch,length), data.frame(year=years)), year, by="year")
  # wf.n.high <- arrange(full_join(aggregate(catch~ year,high.wf.catch,length), data.frame(year=years)), year, by="year")
  # Get the proportion of cells that experienced high catches...
  cells.fished$prop_high <- n.high$catch/cells.fished$cells_fished
  # cells.ft.fished$prop_high <- ft.n.high$catch/cells.ft.fished$cells_fished
  # cells.wf.fished$prop_high <- wf.n.high$catch/cells.wf.fished$cells_fished
  # Some basic summary stats...
  med.catch <- aggregate(catch~year,spatial.fishery.history,median)
  med.effort <- aggregate(effort~year,spatial.effort.history,median)
  # med.ft.catch <- aggregate(catch~year,spatial.ft.fishery.history,median)
  # med.ft.effort <- aggregate(effort~year,spatial.ft.effort.history,median)
  # med.wf.catch <- aggregate(catch~year,spatial.wf.fishery.history,median)
  # med.wf.effort <- aggregate(effort~year,spatial.wf.effort.history,median)
  prop.total.catch <- high.catch %>%
    group_by(year) %>%
    summarize(highcatch = sum(catch)) %>%
    left_join(tot.catch, by="year") %>%
    mutate(prop.total.catch=highcatch / catch)
  
  cumulative <- NULL
  for(i in unique(spatial.fishery.history$year)) {
    cumulativeyr <- spatial.fishery.history %>%
      filter(year==i) %>%
      arrange(-catch) %>%
      mutate(cumul = cumsum(catch))
    cumulative <- rbind(cumulative, cumulativeyr)
  }
  
  cumulative_crop <- left_join(cumulative, tot.catch, by="year") %>%
    mutate(catch50 = catch.y*0.75) %>%
    mutate(diff = catch50-cumul) %>%
    filter(diff>0)
  
  ncellsperc <- cumulative_crop %>%
    group_by(year) %>%
    summarize(numcells = n()) %>%
    left_join(cells.fished, by="year") %>%
    mutate(propcells75=numcells/cells_fished)
  
  spatial.rate.history <- spatial.fishery.history %>%
    left_join(spatial.effort.history, by=c("year", "lon", "lat", "bank", "PID", "SID"))  %>%
    mutate(cpue = catch*1000/effort)
  
  fishery_sf <- st_set_crs(st_as_sf(spatial.rate.history, coords=c("lon", "lat"), remove=F), 4326)
  
  if(!is.null(months)) fishery_sf$date <- ym(paste0(substr(fishery_sf$year, 1,4), "-", substr(fishery_sf$year, 6, nchar(fishery_sf$year))))
  if(is.null(months)) fishery_sf$date <- fishery_sf$year
  
  if(is.null(subarea)) {
    basemap <- ggplot() + 
      geom_sf(data=survey[survey$ID==bnk,], fill=NA) + 
      theme_bw()
  }
  
  if(!is.null(subarea)) {
    bound <- st_buffer(st_as_sfc(st_bbox(other[other$ID==subarea,])), 10)
    survey_plot <- st_crop(survey[survey$ID==bnk,], bound)
    basemap <- ggplot() + 
      geom_sf(data=survey_plot, fill=NA) + 
      geom_sf(data=other[other$ID==subarea,]) +
      theme_bw()
  }
  
  map_catch <- basemap +
    geom_sf(data=fishery_sf[fishery_sf$catch<=high,], colour="grey") + 
    geom_sf(data=fishery_sf[fishery_sf$catch>high,]) + 
    facet_wrap(~date) +
    ggtitle(paste0(area, ", ", "cells above ", high, "t"))
  
  map_cpue <- basemap + 
    geom_sf(data=fishery_sf[fishery_sf$cpue<=highcpue,], colour="grey") + 
    geom_sf(data=fishery_sf[fishery_sf$cpue>highcpue,]) + 
    facet_wrap(~date) +
    ggtitle(paste0(area, ", ", "cells above ", highcpue, "kg/hm"))

  ## Now take a summary of these summaries...
  annual.summary <- cbind(tot.catch,tot.effort$effort,cells.fished$cells_fished,cells.fished$prop_high,med.catch$catch,med.effort$effort, ncellsperc$numcells, ncellsperc$propcells75#, tot.ft.catch$catch,
                          # tot.ft.effort$effort,cells.ft.fished$cells_fished,cells.ft.fished$prop_high,med.ft.catch$catch,med.ft.effort$effort, tot.wf.catch$catch,
                          # tot.wf.effort$effort,cells.wf.fished$cells_fished,cells.wf.fished$prop_high,med.wf.catch$catch,med.wf.effort$effort
                          )
  
  names(annual.summary) <- c("year","tot_catch","tot_effort","cells_fished","prop_high","median_catch","median_effort", "num_cells_75perc_catch", "prop_cells_fished_75perc")#,
                             # "ft_tot_catch", "ft_tot_effort","ft_cells_fished","ft_prop_high","ft_median_catch","ft_median_effort",
                             # "wf_tot_catch",  "wf_tot_effort","wf_cells_fished","wf_prop_high","wf_median_catch","wf_median_effort")
  
  if(!is.null(months)) {
    annual.summary$month <- substr(annual.summary$year, 6, nchar(annual.summary$year))
    annual.summary$year <- substr(annual.summary$year, 1, 4)
    annual.summary$date <- ymd(paste0(annual.summary$year, "-", annual.summary$month, "-01"))
  }
  
  if(is.null(months)) {
    annual.summary$date <- ymd(paste0(annual.summary$year, "-01-01"))
  }
  
  annual.summary$num_high <- annual.summary$cells_fished*annual.summary$prop_high
  
  a <- ggplot() + geom_point(data=annual.summary, aes(date, median_catch)) + 
    geom_line(data=annual.summary, aes(date, median_catch)) + theme_bw() + ylab("Median catch\nper cell (t)")+
    scale_x_date(date_labels = "%b %Y") + theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  b <- ggplot() + geom_point(data=annual.summary, aes(date, median_effort)) + 
    geom_line(data=annual.summary, aes(date, median_effort)) + theme_bw() + ylab("Median effort\nper cell (hm)")+
    scale_x_date(date_labels = "%b %Y")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  c <- ggplot() + geom_point(data=annual.summary, aes(date, cells_fished)) + 
    geom_line(data=annual.summary, aes(date, cells_fished)) + 
    theme_bw() + ylab("Number of\ncells fished")+
    scale_x_date(date_labels = "%b %Y")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  d <- ggplot() + geom_point(data=annual.summary, aes(date, num_high)) + 
    geom_line(data=annual.summary, aes(date, num_high)) + theme_bw() + ylab("Number of\ncells above\nhigh threshold")+
    scale_x_date(date_labels = "%b %Y")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  e <- ggplot() + geom_point(data=annual.summary, aes(date, prop_high)) + 
    geom_line(data=annual.summary, aes(date, prop_high)) + theme_bw() + ylab("Proportion of\ncells above\nhigh threshold")+
    scale_x_date(date_labels = "%b %Y")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  fplot <- ggplot() + geom_point(data=annual.summary, aes(date, num_cells_75perc_catch)) + 
    geom_line(data=annual.summary, aes(date, num_cells_75perc_catch)) + theme_bw() +
    scale_x_date(date_labels = "%b %Y") + ylab("Number of\ncells with\n75% of catch")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  g <- ggplot() + geom_point(data=annual.summary, aes(date, prop_cells_fished_75perc)) + 
    geom_line(data=annual.summary, aes(date, prop_cells_fished_75perc)) + theme_bw() +
    scale_x_date(date_labels = "%b %Y") + ylab("Proportion of\ncells fished with\n75% of catch")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  
  require(patchwork)
  cell_stats <- a/b + plot_annotation(
    title = "Gridded area summary",
    subtitle = paste0(area, ", high threshold = ", high, "t")
  )
  
  threshold_stats <- c/d/e + plot_annotation(
    title = "Fishery footprint summary",
    subtitle = paste0(area, ", high threshold = ", high, "t")
  )
  
  perc_catch_stats <- fplot/g + plot_annotation(
    title = "Percentage of catch",
    subtitle = paste0(area, ", 75% of total catch"))
  
  medcatch <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
                                     c(median_catch, median_effort, cells_fished, prop_high, num_high)),
                        annual.summary[,c("date", "median_catch")], by="date")
  medcatch$name <- factor(medcatch$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  f <- ggplot() + geom_text(data=medcatch, aes(median_catch, value, label=date, colour=date)) +
    geom_path(data=medcatch, aes(median_catch, value)) + ylab(NULL)+
    facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
    theme_bw()+
    theme(strip.placement = "outside", strip.background = element_blank()) +
    scale_colour_continuous(guide="none")
  
  medeff <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
                                   c(median_catch, median_effort, cells_fished, prop_high, num_high)),
                      annual.summary[,c("date", "median_effort")], by="date")
  medeff$name <- factor(medeff$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  g <- ggplot() + geom_text(data=medeff, aes(median_effort, value, label=date, colour=date)) +
    geom_path(data=medeff, aes(median_effort, value)) +ylab(NULL)+
    facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
    theme_bw()+
    theme(strip.text = element_blank(), strip.placement = "outside", strip.background = element_blank()) +
    scale_colour_continuous(guide="none")
  
  cellsf <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
                                   c(median_catch, median_effort, cells_fished, prop_high, num_high)),
                      annual.summary[,c("date", "cells_fished")], by="date")
  cellsf$name <- factor(cellsf$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  h <- ggplot() + geom_text(data=cellsf, aes(cells_fished, value, label=date, colour=date)) +
    geom_path(data=cellsf, aes(cells_fished, value)) +ylab(NULL)+
    facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
    theme_bw()+
    theme(strip.text = element_blank(), strip.placement = "outside", strip.background = element_blank()) +
    scale_colour_continuous(guide="none")
  
  proph <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
                                  c(median_catch, median_effort, cells_fished, prop_high, num_high)),
                     annual.summary[,c("date", "prop_high")], by="date")
  proph$name <- factor(proph$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  i <- ggplot() + geom_text(data=proph, aes(prop_high, value, label=date, colour=date)) +
    geom_path(data=proph, aes(prop_high, value)) +ylab(NULL)+
    facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
    theme_bw()+
    theme(strip.text = element_blank(), strip.placement = "outside", strip.background = element_blank()) +
    scale_colour_continuous(guide="none")
  
  numh <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
                                 c(median_catch, median_effort, cells_fished, prop_high, num_high)),
                    annual.summary[,c("date", "num_high")], by="date")
  numh$name <- factor(numh$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  j <- ggplot() + geom_text(data=numh, aes(num_high, value, label=date, colour=date)) +
    geom_path(data=numh, aes(num_high, value)) +ylab(NULL)+
    facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
    theme_bw()+
    theme(strip.text = element_blank(), strip.placement = "outside", strip.background = element_blank())
  
  footprint_crosscor <- (f|g|h|i|j) + plot_annotation(
    title = "Fishery footprint cross-correlation",
    subtitle = paste0(area, ", high threshold = ", high, "t"))
  
  return(list(map_catch = map_catch, map_cpue = map_cpue, cell_stats=cell_stats, threshold_stats=threshold_stats, perc_catch_stats=perc_catch_stats, footprint_crosscor=footprint_crosscor, annual_summary=annual.summary))
}
