####################################################################
## File to run the error checks for the log and slip information directly from MARFIS.  This function only works for years we have
# data in MARFIS, so 2008 onwards...
###################################################################
# Update history
# Jan 2019:  File created
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
##
###############################################################################################################


###############################################################################################################
# Arguments


### May 16, 2016 added options for calling the database which I should have had in before...
# Arguments
# direct:       The root directory to work from  Default = "Y:/Offshore scallop/Assessment", 
# yrs:          The year(s0) you want to check, needs to be 2008 onwards or it'll break. Default is NULL which will pull everything form 2008 to current year
# marfis:       Do you want to check the marfis log or the local logs.  T/F, default is T which checks the marfis logs, if checking the local logs
#               set to F.  NOTE that this will check the logs associated with what you set "direct" to be!!!
# repo:         Where are you getting the functions and the GIS layers from.  Default is 'github' whic pulls in the latest from Github
#               while 'local' searches for the function in a subdirectory of your direct call above
# db.con:       The database to connect to.  Default ="ptran",
# un:           Your username to connect to SQL database.  Default = un.ID
# pw:           Your password to connect to SQL database.  Default = pwd.ID
# db.lib:       Do you want to use "ROracle" (the default) or "RODBC", the old way
# bank:         The bank you are interested in checking, can be one, multiple, or all banks at once.  Default = NULL which will use all data, to select
#               a bank you need to use the 3 digit codes (e.g. "GBa","BBn",)
# trips:        Do you want to just look at specfic trips.  Default is NULL (looks at everything for the year)
# dates:        Do you want to just look for trips that fished within a certain range.  Default is NULL. 
#               For one day enter like: dates="YYYY-MM-DD"
#               For a date range enter as: dates=c("YYYY-MM-DD", "YYYY-MM-DD") where the first date is the beginning and the second date is the end of the range.
# tow.time:     What is the range of tow times you want to check.  default = c(3,80) which will flag tow times < 3 and > 80.
# trip.tol:     What is the tolerance you want for comparing the slip weights with the trip weights from the logs.  Options include
#               a: 'exact' which will flag trips that aren't exact matches, this may flag a lot of trips that are only off due to rounding error
#               b:  enter a numeric value to search for trips in which the weight of the logs and slips differ by given amount.  For example
#                   trip.tol = 100 will flag trips in which the difference between the sum of the slip and sum of the trip weight is > 100 lbs.
#                   Default is 1 lb which is pretty much equivalent to rounding to the nearest whole number.
# spatial       Do you want to make spatial plots of the trips.  T/F, default = T.
# reg.2.plot    A few options here, the default is NULL.  What this does is to create a zoomed in plot focused on a bounding box around the
#               points in each tow.  If you want to plot a bank or a region just put that here, see pecjector for 
#               region names that work (e.g. "GBa","GBb", "SS", "BOF", etc) if you want to plot a specific area
#               you can enter the coordinates and the projection system, likely you'll never need to mess with the projection system
#               so adding something like this where you change the x and y values should do the trick.
#               data.frame(y = c(40,46),x = c(-68,-55),proj_sys = "+init=epsg:4326")
#               Note that for mutiple trips the NULL option will zoom in on each trip, whereas the non-NULL option will plot the
#               same region for every trip.
# plot          "shiny" if you want to run the shiny app. "pdf" if you want to print the pdfs. 
# export        Do you want to export the results to a xlsx spreadsheet.  Default is NULL which doesn't export anything.
#               export = "fish.dat" will put a file in the Assessment/yr/log_Checks/ folder with a name that attempts to
#               highlight what you have tested, alternatively you can enter your own name and save it wherever you'd like.
###############################################################################################################

log_checks <- function(direct, direct_fns, yrs = NULL , marfis=T, repo = "github",
                       db.con="ptran",un=NULL,pw=NULL,db.lib = "ROracle", 
                       bank = NULL ,trips = NULL, dates = NULL, vrnum = NULL,tow.time = c(3,80),trip.tol = 1 ,
                       spatial = T,reg.2.plot = NULL, plot = "shiny", export = NULL
                       
)
{
  # Load in the functions needed for this function to run.
  
  if(repo == "github")
  {
    funs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Fishery/logs_and_fishery_data.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/combo_shp.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Fishery/Log_spatial_checks/app.R")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
    for(fun in funs) 
    {
      download.file(fun,destfile = basename(fun))
      source(paste0(getwd(),"/",basename(fun)))
      file.remove(paste0(getwd(),"/",basename(fun)))
    } # end  for(fun in funs) 
  } # end if(repo == "github")
  
  if(repo == "local")
  {
    source(paste(direct_fns,"Fishery/logs_and_fishery_data.R",sep="")) #logs_and_fish is function call
    source(paste(direct_fns,"Maps/pectinid_projector_sf.R",sep="")) # The new scallopMap
    source(paste(direct_fns,"Maps/combo_shp.R",sep="")) # The new scallopMap
    source(paste0(direct_fns, "Fishery/Log_spatial_checks/app.R"))
  } # end if(repo== 'local')
  
  # The necesary library
  require(maptools)  || stop("Maptools, MAPtools, MAPTOOLS, install this package, please, now, HURRY!!!!")
  require(sp)  || stop("You shall not pass until you install the *sp* package... you've been warned...")
  require(rgeos)  || stop("Without *rgeos* package installed you won't be able to do anything...")
  require(lubridate) || stop("You need the *lubridate* package installed or you won't even know what year it is...")
  require(openxlsx)|| stop(" You need the openxlsx package if you want to export the results bub...")
  require(rgdal)|| stop("You shall not pass until you install the *rgdal* package... you've been warned...")
  require(sf)|| stop("You shall not pass until you install the *sf* package... you've been warned...")
  options(stringsAsFactors = F)
  
  # If we are running the shiny app we want to set the plot_package to be "ggplot2", otherwise we set it to NULL which is just base graphics
  plot_package <- ifelse(plot == "shiny","ggplot2","NULL")
  # We want to bring in the vessel information
  fleet.dat <- read.csv(paste0(direct,"Data/Offshore_Fleet.csv"))
  # Now we want to look at the portion of the fleet we have Gear size and number of rakes
  fleet.dat <- fleet.dat[!is.na(fleet.dat$num_rakes),]
  
  # If you didn't specify years we pick all years from 2008 to current (this won't work before 2008!)
  if(is.null(yrs)) yrs <- 2008:as.numeric(format(Sys.Date(),"%Y"))
  # Now bring in the fishery data you want to look at.
  logs_and_fish(loc="offshore",year = yrs,un=un,pw=pw,db.con=db.con,direct=direct, direct_fns=direct_fns,get.marfis = marfis,export=F,db.lib=db.lib)
  # the marfis data
  if(marfis==T) dat.log <- marfis.log
  if(marfis==F) dat.log <- new.log.dat

  dat.log$avgtime <- as.numeric(dat.log$avgtime)
  
  # extra time check column
  dat.log$watchtime <- (as.numeric(dat.log$numtow)*dat.log$avgtime)/60
  
  if(marfis==T) dat.slip <- marfis.slip
  if(marfis==F) dat.slip <- slip.dat
  
  # If you want to look by trip Number, this would also pull any logs with the trip number missing from that year
  miss.dat <- NULL
  if(!is.null(trips)) 
  {
    miss.dat[["trips"]] <- dat.log[is.na(dat.log$tripnum),]
    dat.log <- dat.log[!is.na(dat.log$tripnum),]
    dat.log <- dat.log[dat.log$tripnum %in% trips,]
  } # end if(!is.null(trips)) 
  # If you want to look by date fished, this would also pull any logs with the date fished missing from that year
  
  if(!is.null(dates)) 
  {
    miss.dat[["date_fished"]] <- dat.log[is.na(dat.log$fished),]
    dat.log <- dat.log[!is.na(dat.log$fished),]
    
    if(length(dates)>1) dat.log <- dat.log[dat.log$fished %in% ymd(dates[1]):ymd(dates[2]),]
    if(length(dates)==1) dat.log <- dat.log[dat.log$fished %in% ymd(dates),]
  } # end if(!is.null(dates)) 
  # If you want to look by vessel,this would also pull any logs with the vrnum missing from that year
  if(!is.null(vrnum)) 
  {
    miss.dat[["vrnum"]] <- dat.log[is.na(dat.log$vrnum),]
    dat.log <- dat.log[!is.na(dat.log$vrnum),]
    dat.log <- dat.log[dat.log$vrnum %in% vrnum,]
  } # end if(!is.null(vrnum))
  # If you want to look by bank this would also pull any logs with the bank missing
  if(!is.null(bank)) 
  {
    miss.dat[["bank"]] <- dat.log[is.na(dat.log$bank),]
    dat.log <- dat.log[!is.na(dat.log$bank),]
    dat.log <- dat.log[dat.log$bank %in% bank,]
  } #end if(!is.null(bank))
  
  # kill it and return an error message if too many parameters specified.
  if(dim(dat.log)[1] == 0) stop("No trips found with specified parameters.")
  
  
  # So we now subset our data to the trips/banks/dates we are interested in, we can now search for na's in these key fields for the remaining data.
  remove <- unique(c(which(is.na(dat.log$numrake)),
                     which(is.na(dat.log$lon_ent)),
                     which(is.na(dat.log$lat_ent)),
                     which(is.na(dat.log$lon)),
                     which(is.na(dat.log$lat)),
                     which(is.na(dat.log$sfa)), 
                     which(is.na(dat.log$watch)), 
                     which(is.na(dat.log$numtow)),
                     which(is.na(dat.log$avgtime)),
                     which(is.na(dat.log$weight)),
                     which(is.na(dat.log$fished)),
                     which(is.na(dat.log$nafo))))
  log.checks <- dat.log[remove,]
  
  # If any of the trips have roe-on we want to flag those
  roe.on <- dat.log[dat.log$roeon == "Y",]
  # If the tow time is < 3 or > 80 that's something we want to investigate further. Also flagging any watches that are longer than 6h. 
  tow.time.outliers <- dat.log[(dat.log$avgtime < tow.time[1] | dat.log$avgtime > tow.time[2] | dat.log$watchtime >= 6) & !is.na(dat.log$watchtime),]
  
  # Now before I get into the spatial bit I need to make an automated file name so I can save either/both a pdf or an xlsx
  if(!is.null(export) || spatial == T)
  {
    if(is.null(bank)) bank.name <- ""
    if(!is.null(bank)) bank.name <- paste0("_",paste(bank,collapse = "_"))
    if(is.null(trips)) trip.name <- ""
    if(!is.null(trips)) trip.name <- paste0("_",paste(trips,collapse = "_"))
    if(is.null(dates)) date.name <- ""
    if(!is.null(dates)) date.name <- paste0("_",paste(dates,collapse = "_"))
    if(is.null(vrnum)) vr.num <- ""
    if(!is.null(vrnum)) vr.num <- paste0("_",paste(vrnum,collapse = "_"))
    t.name <- paste(tow.time,collapse = "_")
    # Now make the file name
    if(spatial == F) f.name <- paste0(direct,max(yrs),"/Log_checks/Log_check",bank.name,trip.name,date.name,vr.num,
                                      "_avg_tow_time_range_",t.name,"_slip_vs_log_weight_tol_",trip.tol)
    if(spatial == T) f.name <- paste0(direct,max(yrs),"/Log_checks/Log_check",bank.name,trip.name,date.name,vr.num,
                                      "_avg_tow_time_range_",t.name,"_slip_vs_log_weight_tol_",trip.tol,"_spatial_checks")
  } # if(export == T || spatial == T)
  
  # I need to do something with the slips to make sure they agree with the weights for each trip
  trip.ids <- unique(dat.log$tripnum)
  num.trips <- length(trip.ids)
  
  # Now run through all the trips of interest
  weight.mismatch.logs <- NULL
  weight.mismatch.slips <- NULL
  gear.size.mismatch <- NULL
  num.rakes.mismatch <- NULL
  watches.outside.sa <- NULL
  watches.outside.nafo <- NULL
  # If makeing spatial plots crack open the pdf
  if(spatial ==T & plot == "pdf") {pdf(file = paste0(f.name,".pdf"),width=11,height = 11, onefile=T)}
  trip.log.all <- list()
  osa.all <- list()
  pr.all <- list()
  if(plot == "shiny") pect_ggplot.all <- list()
  
  #set this up so it doesn't happen over and over again for each trip in the loop
  if(spatial==T){
    # First we bring in the shapefiles if we haven't already...
    if(!exists("offshore.spa"))
    {
      
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to the temp directory you created above
      download.file("https://raw.githubusercontent.com/Mar-Scal/GIS_layers/master/offshore/offshore.zip", temp)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # This little all_layers function brings in all the shapefiles we have currently and makes sure theya are WGS 84 and lat-lon
      offshore.spa <- suppressMessages(combo.shp(temp2, make.sf = T))
      offshore.spa <- st_transform(offshore.spa, CRS("+init=epsg:4326"))
      
      # Pop this into the global environment so we don't make it a bunch of times..
      assign('offshore.spa',offshore.spa,pos=1)
      # We also want to bring in the survey strata for the banks with a survey strata
      # Figure out where your tempfiles are stored
    } # end  if(!exists("offshore.spa"))
    
    if(!exists("nafo.subs"))
    {
      
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to the temp directory you created above
      download.file("https://raw.githubusercontent.com/Mar-Scal/GIS_layers/master/NAFO/Subareas.zip", temp)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # This little all_layers function brings in all the shapefiles we have currently and makes sure theya are WGS 84 and lat-lon
      nafo.subs <- suppressMessages(combo.shp(temp2, make.sf = T))
      nafo.subs <- st_transform(nafo.subs, CRS("+init=epsg:4326"))
      # Pop this into the global environment so we don't make it a bunch of times..
      assign('nafo.subs',nafo.subs,pos=1)
      # Figure out where your tempfiles are stored
    } # end  if(!exists("offshore.spa"))
  }
  
  
  for(i in 1:num.trips) # this is going through trips
  {
    trip.log <- dat.log[dat.log$tripnum == trip.ids[i],]
    trip.slip <- dat.slip[dat.slip$tripnum == trip.ids[i],]
    # Get the slip and trip landings
    sum.slip <- sum(trip.slip$weight,na.rm=T) # This is in lbs
    sum.trip <- sum(trip.log$pro.repwt,na.rm=T)*2.2046 # Get this into lbs to match sum.slip
    # Now what is your comparison criteria for these.
    
    # exact criteria
    if(trip.tol == 'exact')
    {
      if(sum.slip != sum.trip) 
      {
        weight.mismatch.logs[[as.character(trip.ids[i])]]  <- trip.log
        weight.mismatch.slips[[as.character(trip.ids[i])]] <- trip.slip
      } # end if(sum.slip != trip.slip) 
    } # end if(trip.tol == 'exact')
    
    # If you specify a tolerance value you want to compare, default is 1 lb which is basically same a rounding
    if(is.numeric(trip.tol))
    {
     # If the difference between these 2 values is > trip.tol then output the slip.
      if(abs(sum.slip - sum.trip) > trip.tol)
      {
        weight.mismatch.logs[[as.character(trip.ids[i])]]  <- trip.log
        weight.mismatch.slips[[as.character(trip.ids[i])]] <- trip.slip
      } # end if(sum.slip != trip.slip) 
    } # end if(trip.tol == 'round')
    
    # check for extra rows that are full of NA's, and remove them if they exist:
    if(dim(trip.log[which(apply(trip.log, 1, function(x){any(is.na(x))})),])[1] >0 & length(which(any(!is.na(trip.log[which(apply(trip.log, 1, function(x){any(is.na(x))})),])))) == 0) {
      stop("Entirely NA in rows in data. Delete them in the CSV.")
    }
    
    # Check to ensure vessel gear size and number of rakes and gear size is correct.  I don't want to flag NA"s here as that would be redundant with above
    num.rakes <- fleet.dat$num_rakes[fleet.dat$ID  == unique(trip.log$vrnum)][1] # Add [1] For vessels with multiple entries (e.g. same number different name)
    gear.size <- fleet.dat$gear_size[fleet.dat$ID  == unique(trip.log$vrnum)][1] # Add [1] For vessels with multiple entries (e.g. same number different name)
    
    if(any(na.omit(trip.log$numrake) != num.rakes)) num.rakes.mismatch[[as.character(trip.ids[i])]] <- trip.log
    if(any(na.omit(trip.slip$gear.ft) != gear.size)) gear.size.mismatch[[as.character(trip.ids[i])]] <- trip.slip
    
    # We also want to make spatial plots, I'm going to test out pecjector for this..
    # and while I'm at it we may as well run simple spatial checks to make sure the data falls within the boundary we expect it to.
    
    if(spatial==T)
    {
      # Now we want to see if any of the data in the logs falls outside the region we think it is in.
      # For split trips I consider the first bank only, so this will pretty much flag all split trips, which I think is fine!
      trip.area <- na.omit(unique(trip.log$bank))[1]
      if(trip.area == "Mid") trip.area <- "Sab" # Set middle to Sable.
      # Now if we are in an area with a survey strata we want to flag any tows that fall outside the sfa
      # We need to remove any NA's (these would have been flagged above) or this breaks
      trip.log <- trip.log[!is.na(trip.log$lat),]
      trip.log <- trip.log[!is.na(trip.log$lon),]
      # SPB is a pain as it is two different pieces, so if looking at SPB I switch to look at this by SFA
      if(trip.area == "SPB")
      {
        #Pick it up here, I have logs that could be spread across 2-3 areas, so I think I need to loop through each of the SFA's and
        # make sure the tows are landing inside the SFA it says, if they are we flag them.  I'll have to make the rest of the below an else
        sfa.visited <- unique(trip.log$sfa)
        num.sfas <- length(sfa.visited)
        osa.spb <- NULL
        
        for(spb in 1:num.sfas)
        {
          trip.tmp <- trip.log[trip.log$sfa == sfa.visited[spb],]
          spb.area <- paste0("SFA",sfa.visited[spb],collapse = "")
          # Now if we are in an area with a survey strata we want to flag any tows that fall outside the the sfa
          # Turn the trip.log into a spatial object
          trip.tmp <- st_as_sf(trip.tmp, coords = c("lon", "lat"), remove=F)
          # project it, logs are all WGS84 as I understand it and Lat Lon
          trip.tmp <- st_set_crs(trip.tmp, value=4326)
          if(sfa.visited[spb] %in% c("10","11","12"))
          {
            osa.spb <- trip.tmp[which(suppressMessages(st_disjoint(trip.tmp,  offshore.spa[offshore.spa$ID %in% spb.area,], sparse=F))),]
          }# end if(sfa.visited %in% c("10","11","12"))
          # If the SFA is not one of these then it's mislabelled and needs fixed so output them all
          if(!sfa.visited[spb] %in% c("10","11","12"))
          {
            osa.spb <- trip.tmp
          } # end if(!sfa.visited %in% c("10","11","12"))
        } # end for(spb in 1:num.sfas)
        
        if(nrow(osa.spb)>0){
          # We need to turn the osa into a spatial object with projection
          osa<- st_as_sf(osa.spb, coords = c("lon", "lat"), remove=F)
          # project it, logs are all WGS84 as I understand it and Lat Lon
          osa <- st_set_crs(osa.spb, value=4326)
          st_geometry(osa) <- NULL  
          osa <- as.data.frame(osa)
        }
        
        st_geometry(osa.spb) <- NULL  
        osa.spb <- as.data.frame(osa.spb)
        
        if(nrow(osa.spb)==0){
          osa <- osa.spb
        }
        
        # Now get the data pulled together and plop it in an object with the rest of the missing trips.
        watches.outside.sa[[as.character(trip.ids[i])]] <- osa.spb
        # Need to make this osa and trip.log spatial beasts.
        # We need to turn the trip.log into a spatial object with projection
        trip.log <- st_as_sf(trip.log, coords = c("lon", "lat"), remove=F)
        # project it, logs are all WGS84 as I understand it and Lat Lon
        trip.log <- st_set_crs(trip.log, value=4326)
      } 
      else {
        # We  still need to turn the trip.log into a spatial object with projection
        trip.log <- st_as_sf(trip.log, coords = c("lon", "lat"), remove=F)
        # project it, logs are all WGS84 as I understand it and Lat Lon
        trip.log <- st_set_crs(trip.log, value=4326)
        osa <- trip.log[which(suppressMessages(st_disjoint(trip.log,  offshore.spa[offshore.spa$ID %in% trip.area,], sparse=F))),]
        st_geometry(osa) <- NULL
        osa <- as.data.frame(osa)
        watches.outside.sa[[as.character(trip.ids[i])]] <- osa

      } # end else which is everywhere outside SPB.
      
      # Now I need to do somethign similar for the NAFO sub-regions.  Because I don't want to flag every trip on GBa that
      # moves between the north and south I'm going to do this the slow way, for
      # every watch I'm going to compare the location with the NAFO region in the log, if it doesn't
      # match then we flag it.
      os.nafo <- NULL
      
      for(n in 1:nrow(trip.log)) # this is going through log records for one trip
      {
        
        # Get the nafo area from the log
        nafo.area <- trip.log$nafo[n]
        if(!is.na(nafo.area))
        {
          # So here's some stick handling to get the nafo sub areas to line up, the info is in there...
          # First we get the right NAFO region (3 vs 4 vs 5)
          nafo.reg <- nafo.subs[nafo.subs$level_0 %in% substr(nafo.area, 1,1),]
          # Next we have to determine what "level of sub area we are looking at, this little line is doing a lot!
          # But basically is taking the id info from the nafo.reg and picking out the first one that matches what 
          # we are looking for.  The id's are the internal identifier of the location of the approriate polygon
          # while the number of characters in the nafo.area object identify the level we want to look at inside the object
          # We only ever want to return the first polygon as this is the one at the appropriate level to the fishing activity listed.
          if(nchar(nafo.area)==4) sp.slot <- na.omit(nafo.reg[nafo.reg$level_3 %in% nafo.area,]$id)
          if(nchar(nafo.area)==3) sp.slot <- na.omit(nafo.reg[nafo.reg$level_2 %in% nafo.area,]$id)
          if(nchar(nafo.area)==2) sp.slot <- na.omit(nafo.reg[nafo.reg$level_1 %in% nafo.area,]$id)
          nafo.loc <- nafo.reg[nafo.reg$id  ==sp.slot,]
          # Then we need to match on the level that the nafo.area is, this is a little different, but
          # basically if our watch is flagged as in the wrong NAFO area then we keep it.
          
          if(suppressMessages(st_disjoint(trip.log[n,],nafo.loc,sparse=F))) {
            trip.log.tmp <- trip.log[n,]
            st_geometry(trip.log.tmp) <- NULL
            os.nafo[[as.character(n)]] <- trip.log.tmp
          }
        } # end if(!is.na(nafo.area))
        
      } # end for(n in 1:length(trip.log))
      
      # Tidy up the os.nafo list...
      
      #if(is.null(os.nafo)) os.nafo <- cbind(tmp@data,tmp@coords)
      if(length(os.nafo) >1) os.nafo.df <- do.call("rbind",os.nafo)
      if(length(os.nafo) == 1) os.nafo.df <- as.data.frame(os.nafo[[1]])
      if(!is.null(os.nafo)) {
        watches.outside.nafo[[as.character(trip.ids[i])]] <- as.data.frame(os.nafo.df)
        os.nafo <- os.nafo.df
      }
      
      # Now make the plot for each trip with all the points.  If a point falls outside the survey domain we give it a different sympbol and color
      if(is.null(reg.2.plot)) pr <- data.frame(y=c(st_bbox(trip.log)$ymin, st_bbox(trip.log)$ymax), 
                                               x=c(st_bbox(trip.log)$xmin, st_bbox(trip.log)$xmax),
                                               crs=st_crs(trip.log)$epsg)
      
      # This assumes that you are asking to plot a certain region that pecjector understands (e.g. "GBa","GB","SS", etc) or
      # a dataframe that looks like this...data.frame(y = c(40,46),x = c(-68,-55),proj_sys = "+init=epsg:4326")
      if(!is.null(reg.2.plot)) pr <- reg.2.plot
      
      # Now make the plot, add the points, if there is only 1 point the bounding box method doesn't work!
      if(nrow(trip.log) == 1 && is.null(reg.2.plot)) 
      {
        pect_plot <- pecjector(area = trip.area, plot=F, add_layer = list(eez='eez', nafo="sub", sfas="all"), repo=repo,direct=direct, 
                               direct_fns=direct_fns)
      } 
      else {
        pect_plot <- pecjector(area = pr, plot=F, add_layer = list(eez='eez', nafo="sub", sfas="all"), repo="github",direct=direct, 
                               direct_fns=direct_fns)
      }
      
      trip_plot <- pect_plot  + geom_sf(data=trip.log) + 
        ggtitle(paste0(trip.log$ves[1],"_",trip.log$vrnum[1],"_",min(trip.log$fished,na.rm=T),"-",max(trip.log$fished,na.rm=T))) +
        scale_x_continuous(expand=c(0.1, 0)) + scale_y_continuous(expand=c(0.1, 0))
      
      if(nrow(osa) > 0) trip_plot <- trip_plot + geom_point(data=osa, aes(lon, lat), colour="blue", shape=21)  # these are any points outside the survey domain
      if(!is.null(os.nafo))  trip_plot <- trip_plot + geom_point(data=os.nafo, aes(lon, lat), colour="red", shape=21)  # these are any points outside the expected nafo subregion.
      
      
      
      print(trip_plot)
      
      # plot(osa,add=T,pch=20,cex=2,col="blue") # These are any points outside the survey domain, if there are any
      
      # if(!is.null(os.nafo)) points(os.nafo$lon,os.nafo$lat,pch=21,cex=2,col="red") # These are any points outside the expected nafo subregion.
      
      print(paste0("Trip ID:",trip.ids[i],"  count=",i))
      
    } # end if (spatial==T)
    
    #} # end for(i in 1:num.trips)
    
    if(spatial==T & plot == "shiny"){
      # for use in shiny
      trip.log.all[[i]] <- trip.log
      osa.all[[i]] <- osa
      
      #manually expand pr for each trip
      if(pr$x[1] < pr$x[2])
      {
        if(pr$x[1] < 0) pr$x[1] <-pr$x[1] + 0.0025*pr$x[1]
        if(pr$x[1] > 0) pr$x[1] <- pr$x[1] - 0.0025*pr$x[1]
        # Now we could have xlim[2] being positive when xlim 1 is negative so this needs it's own if's
        if(pr$x[2] < 0) pr$x[2] <- pr$x[2] - 0.0025*pr$x[2]
        if(pr$x[2] > 0) pr$x[2] <- pr$x[2] + 0.0025*pr$x[2]
        
      } else{
        if(pr$x[1] < 0) pr$x[1] <- pr$x[1] - 0.0025*pr$x[1]
        if(pr$x[1] > 0) pr$x[1] <- pr$x[1] + 0.0025*pr$x[1]
        # Now we could have xlim[2] being positive when xlim 1 is negative so this needs it's own if's
        if(pr$x[2] < 0) pr$x[2] <- pr$x[2] + 0.0025*pr$x[2]
        if(pr$x[2] > 0) pr$x[2] <- pr$x[2] - 0.0025*pr$x[2]
      } # end else and the if tied to xlim
      # Now do the same for y's
      if(pr$y[1] < pr$y[2])
      {
        if(pr$y[1] < 0) pr$y[1] <- pr$y[1] + 0.005*pr$y[1]
        if(pr$y[1] > 0) pr$y[1] <- pr$y[1] - 0.005*pr$y[1]
        # Now we could have ylim[2] being positive when ylim 1 is negative so this needs it's own if's
        if(pr$y[2] < 0) pr$y[2] <- pr$y[2] - 0.005*pr$y[2]
        if(pr$y[2] > 0) pr$y[2] <- pr$y[2] + 0.005*pr$y[2]
        
      } else{
        if(pr$y[1] < 0) pr$y[1] <- pr$y[1] - 0.005*pr$y[1]
        if(pr$y[1] > 0) pr$y[1] <- pr$y[1] + 0.005*pr$y[1]
        # Now we could have ylim[2] being positive when ylim 1 is negative so this needs it's own if's
        if(pr$y[2] < 0) pr$y[2] <- pr$y[2] + 0.005*pr$y[2]
        if(pr$y[2] > 0) pr$y[2] <- pr$y[2] - 0.005*pr$y[2]
      } # end else and the if tied to ylim
      pr.all[[i]] <- pr
      
      pect_ggplot.all[[i]] <- trip_plot
    }
    
  } # end for(i in 1:num.trips)
 
  if(spatial==T & plot=="pdf") dev.off() # close PDF device
  
  # if we don't have any bad nafo watches we need to do this so the do.call later doesn't blow up.
  if(is.null(watches.outside.nafo)) watches.outside.nafo <- list(watches.outside.nafo)
  
  # Missing data is anything that is missing the vrnum, bank, date fished, or trip number
  missing.dat <- NA
  if(!is.null(miss.dat)) missing.dat <- do.call("rbind",miss.dat)
  # Mismatch for a trip for number of rakes and gear size
  num.rake.wrong <- NA
  if(!is.null(num.rakes.mismatch)) num.rake.wrong <- do.call("rbind",num.rakes.mismatch)
  # Gear size wrong anywhere
  gear.size.wrong <- NA
  if(!is.null(gear.size.mismatch)) gear.size.wrong <-  do.call("rbind",gear.size.mismatch)
  # The logs and slips in which the weight isn't matching
  weight.log.wrong <- NA
  if(!is.null(weight.mismatch.logs)) weight.log.wrong <- do.call("rbind",weight.mismatch.logs)
  weight.slip.wrong <- NA
  if(!is.null(weight.mismatch.slips)) weight.slip.wrong <- do.call("rbind",weight.mismatch.slips)
  
  if(spatial == T) {
     watches.outside.survey.bounds<- do.call("rbind",watches.outside.sa)
    watches.outside.nafo.bounds <- do.call("rbind",watches.outside.nafo)
  }
  # A list we need for exporting...
  
  if(spatial == F) dat.export <- list(log.checks = log.checks,missing.dat = missing.dat,num.rake.wrong = num.rake.wrong,
                                      gear.size.wrong = gear.size.wrong,weight.log.wrong = weight.log.wrong,weight.slip.wrong = weight.slip.wrong,
                                      tow.time.outliers = tow.time.outliers,roe.on = roe.on)
  
  if(spatial == T) dat.export <- list(log.checks = log.checks,missing.dat = missing.dat,num.rake.wrong = num.rake.wrong,
                                      gear.size.wrong = gear.size.wrong,weight.log.wrong = weight.log.wrong,weight.slip.wrong = weight.slip.wrong,
                                      tow.time.outliers = tow.time.outliers,roe.on = roe.on,watches.outside.survey.bounds = watches.outside.survey.bounds,
                                      watches.outside.nafo.bounds = watches.outside.nafo.bounds)
  
  # Now I want to make a file name that tells me exactly what I ran, this should be fun!
  if(!is.null(export))
  {
    # If you are specfiying the file name yourself
    if(export != "fish.dat") write.xlsx(dat.export,export)
    # If you want an automated file name the name gets created up top so it is available for the pdf figure.
    if(export == "fish.dat") write.xlsx(dat.export,paste0(f.name,".xlsx"))
    # Print the results to the screen and assign them to the global environment for a quick look 
    print(dat.export)
    assign('dat.export',dat.export,pos=1)
  } # end if(!is.null(export))
  
  if(plot== "shiny" && is.null(reg.2.plot)) {
    shinyapp(trip.log=trip.log.all, osa=osa.all, pr=pr.all, direct=direct, direct_fns=direct_fns, repo=repo, pect_ggplot = pect_ggplot.all)
  }
  
} # end function
