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
# 1 direct:     The root directory to work from  Default = "Y:/Offshore scallop/Assessment", 
# 2 yrs:        The year(s0) you want to check, needs to be 2008 onwards or it'll break. Default is NULL which will pull everything form 2008 to current year

#32 db.con:     The database to connect to.  Default ="ptran",
#33 un:         Your username to connect to SQL database.  Default = un.ID
#34 pw:         Your password to connect to SQL database.  Default = pwd.ID
#   db.lib:     Do you want to use "ROracle" (the default) or "RODBC", the old way
#   bank:       The bank you are interested in checking, can be one, multiple, or all banks at once.  Default = NULL which will use all data, to select
#               a bank you need to use the 3 digit codes (e.g. "GBa","BBn",)
#   trips:      Do you want to just look at specfic trips.  Default is NULL (looks at everything for the year)
#   dates:      Do you want to just look for trips that fished within a certain range.  Default is NULL. 
#               For one day enter like: dates="YYYY-MM-DD"
#               For a date range enter as: dates=c("YYYY-MM-DD", "YYYY-MM-DD") where the first date is the beginning and the second date is the end of the range.
#   tow.time.check:   What is the range of tow times you want to check.  default = c(3,80) which will flag tow times < 3 and > 80.
#   trip.tol:   What is the tolerance you want for comparing the slip weights with the trip weights from the logs.  Options include
#               a: 'exact' which will flag trips that aren't exact matches, this may flag a lot of trips that are only off due to rounding error
#               b:  enter a numeric value to search for trips in which the weight of the logs and slips differ by given amount.  For example
#                   trip.tol = 100 will flag trips in which the difference between the sum of the slip and sum of the trip weight is > 100 lbs.
#                   Default is 1 lb which is pretty much equivalent to rounding to the nearest whole number.
#   plot.trips  Do you want to make spatial plots of the trips.  T/F, default = T.
#   export      Do you want to export the results to a xlsx spreadsheet.  Default is NULL which doesn't export anything.
#               export = "fish.dat" will put a file in the fishery data/log_checks/yr folder with a name that attempts to
#               highlight what you have tested, alternatively you can enter your own name and save it wherever you'd like.
#   reg.2.plot  A few options here, the default is NULL.  What this does is to create a zoomed in plot focused on a bounding box around the
#               points in each tow.  If you want to plot a bank or a region just put that here, see pecjector for 
#               region names that work (e.g. "GBa","GBb", "SS", "BOF", etc) if you want to plot a specific area
#               you can enter the coordinates and the projection system, likely you'll never need to mess with the projection system
#               so adding something like this where you change the x and y values should do the trick.
#               data.frame(y = c(40,46),x = c(-68,-55),proj_sys = "+init=epsg:4326")
#               Note that for mutiple trips the NULL option will zoom in on each trip, whereas the non-NULL option will plot the
#               same region for every trip.
###############################################################################################################

log_checks <- function(direct = "Y:/Offshore scallop/Assessment/", yrs = NULL ,
                    un=NULL,pw=NULL,db.con="ptran",db.lib = "ROracle", export = NULL,
                    bank = NULL ,trips = NULL, dates = NULL, vrnum = NULL,tow.time.check = c(3,80),trip.tol = 1 ,spatial = T,
                    reg.2.plot = NULL
                  )
{
  
# Load in the functions needed for this function to run.
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep="")) #logs_and_fish is function call
source(paste(direct,"Assessment_fns/Maps/pectinid_projector.r",sep="")) # The new scallopMap
source(paste(direct,"Assessment_fns/Maps/combine_shapefile_layers.R",sep="")) # The new scallopMap
  
# The necesary library
require(maptools)  || stop("Maptools, MAPtools, MAPTOOLS, install this package, please, now, HURRY!!!!")
require(sp)  || stop("You shall not pass until you install the *sp* package... you've been warned...")
require(rgeos)  || stop("Without *rgeos* package installed you won't be able to do anything...")
require(lubridate) || stop("You need the *lubridate* package installed or you won't even know what year it is...")
require(openxlsx)|| stop(" You need the openxlsx package if you want to export the results bub...")
require(rgdal)
options(stringsAsFactors = F)

# We want to bring in the vessel information
fleet.dat <- read.csv(paste0(direct,"Data/Offshore_Fleet.csv"))
# Now we want to look at the portion of the fleet we have Gear size and number of rakes
fleet.dat <- fleet.dat[!is.na(fleet.dat$num_rakes),]

# If you didn't specify years we pick all years from 2008 to current (this won't work before 2008!)
if(is.null(yrs)) yrs <- 2008:as.numeric(format(Sys.Date(),"%Y"))
# Now bring in the fishery data you want to look at.
logs_and_fish(loc="offshore",year = yrs,un=un,pw=pw,db.con=db.con,direct=direct,get.marfis = T,export=F,db.lib=db.lib)
# the marfis data
dat.log <- marfis.log
dat.log$avgtime <- as.numeric(dat.log$avgtime)
dat.slip <- marfis.slip



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
# If the tow time is < 3 or > 80 that's something we want to investigate further.  
tow.time.outliers <- dat.log[dat.log$avgtime < tow.time.check[1] || dat.log$avgtime > tow.time.check[2] ,]

# Now before I get into the spatial bit I need to make an automated file name so I can save either/both a pdf or an xlsx
if(export == T || spatial == T)
{
    if(is.null(bank)) bank.name <- ""
    if(!is.null(bank)) bank.name <- paste0("_",paste(bank,collapse = "_"))
    if(is.null(trips)) trip.name <- ""
    if(!is.null(trips)) trip.name <- paste0("_",paste(trips,collapse = "_"))
    if(is.null(dates)) date.name <- ""
    if(!is.null(dates)) date.name <- paste0("_",paste(dates,collapse = "_"))
    if(is.null(vrnum)) vr.num <- ""
    if(!is.null(vrnum)) vr.num <- paste0("_",paste(vrnum,collapse = "_"))
    t.name <- paste(tow.time.check,collapse = "_")
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

# If makeing spatial plots crack open the pdf
if(spatial ==T) pdf(file = paste0(f.name,".pdf"),width=11,height = 11)
for(i in 1:num.trips)
{
  #browser()  
  trip.log <- dat.log[dat.log$tripnum == trip.ids[i],]
  trip.slip <- dat.slip[dat.slip$tripnum == trip.ids[i],]
  # Get the slip and trip landings
  sum.slip <- sum(trip.slip$weight) # This is in lbs
  sum.trip <- sum(trip.log$pro.repwt)*2.20462 # Get this into lbs to match sum.slip
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

  # Check to ensure vessel gear size and number of rakes and gear size is correct.  I don't want to flag NA"s here as that would be redundant with above
  num.rakes <- fleet.dat$num_rakes[fleet.dat$ID  == unique(trip.log$vrnum)][1] # Add [1] For vessels with multiple entries (e.g. same number different name)
  gear.size <- fleet.dat$gear_size[fleet.dat$ID  == unique(trip.log$vrnum)][1] # Add [1] For vessels with multiple entries (e.g. same number different name)
  #browser()
  if(any(na.omit(trip.log$numrake) != num.rakes)) num.rakes.mismatch[[as.character(trip.ids[i])]] <- trip.log
  if(any(na.omit(trip.slip$gear.ft) != gear.size)) gear.size.mismatch[[as.character(trip.ids[i])]] <- trip.slip

  # We also want to make spatial plots, I'm going to test out pecjector for this..
  # and while I'm at it we may as well run simple spatial checks to make sure the data falls within the boundary we expect it to.
  if(spatial==T)
  {
    # First we bring in the shapefiles if we haven't already...
    if(!exists("offshore.spa"))
    {
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to the temp directory you created above
      download.file("https://raw.githubusercontent.com/Dave-Keith/GIS_layers/master/offshore/offshore.zip", temp)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # This  little all_layers function brings in all the shapefiles we have currently
      offshore.spa <- all.layers(temp2)
      # We also want to bring in the survey strata for the banks with a survey strata
      # Figure out where your tempfiles are stored
    }
    # Bring in the boundaries for each area, this has all the areas of interest which is nice!
    if(!exists("offshore.survey.boundaries"))
    {
      temp <- tempfile()
      # Download this to the temp directory you created above
      download.file("https://raw.githubusercontent.com/Dave-Keith/GIS_layers/master/survey_boundaries/survey_boundaries.zip", temp)
      
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # This pulls in all the layers from the above location
      offshore.survey.boundaries <- all.layers(temp2)
    } # if(i==1)
    
    # Now we want to see if any of the data in the logs falls outside the region we think it is in.
    # For split trips I consider the first bank only, so this will pretty much flag all split trips, which I think is fine!
    trip.area <- na.omit(unique(trip.log$bank))[1]
    # Now if we are in an area with a survey strata we want to flag any tows that fall outside the survey strata.
    # Turn the trip.log into a spatial object
    # We need to remove any NA's (these would have been flagged above) or this breaks
    trip.log <- trip.log[!is.na(trip.log$lat),]
    trip.log <- trip.log[!is.na(trip.log$lon),]
    coordinates(trip.log) <- ~ lon + lat
    # project it, logs are all WGS84 as I understand it and Lat Lon
    proj4string(trip.log) <- CRS("+init=epsg:4326")
    osa <- trip.log[which(gDisjoint(trip.log,offshore.survey.boundaries[[trip.area]],byid=T)),]
    watches.outside.sa[[as.character(trip.ids[i])]] <- osa@data
    # Now make the plot for each trip with all the points.  If a point falls outside the survey domain we give it a different sympbol and color
    if(is.null(reg.2.plot)) pr <- data.frame(x = trip.log@bbox[1,],y = trip.log@bbox[2,],proj_sys = proj4string(trip.log))
    # This assumes that you are asking to plot a certain region that pecjector understands (e.g. "GBa","GB","SS", etc) or
    # a dataframe that looks like this...data.frame(y = c(40,46),x = c(-68,-55),proj_sys = "+init=epsg:4326")
    if(!is.null(reg.2.plot)) pr <- reg.2.plot
    # Now make the plot, add the points, if there is only 1 point the bounding box method doesn't work!
    if(nrow(trip.log@data) == 1 && is.null(reg.2.plot)) 
    {
      pecjector(area = trip.area,add_EEZ = T,add_sfas = "all",add_land = T)
    } 
    else {
      pecjector(area = pr,add_EEZ = T,add_sfas = "all",add_land = T)
      }
    
    plot(trip.log,add=T,pch=19,cex=0.5)
    plot(osa,add=T,pch=20,cex=2,col="blue") # These are any points outside the survey domain
    title(paste0(trip.log@data$ves[1],"_",trip.log@data$vrnum[1],"_",min(trip.log@data$fished,na.rm=T),"-",max(trip.log@data$fished,na.rm=T)),cex.main=1)
  } # end if(spatial==T)
  print(paste0("Trip ID:",trip.ids[i]))
} # end for(i in 1:num.trips)
dev.off()
#browser()
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

#browser()

if(spatial == T) watches.outside.survey.bounds <- do.call("rbind",watches.outside.sa)
# A list we need for exporting...


if(spatial == F) dat.export <- list(log.checks = log.checks,missing.dat = missing.dat,num.rake.wrong = num.rake.wrong,
                                    gear.size.wrong = gear.size.wrong,weight.log.wrong = weight.log.wrong,weight.slip.wrong = weight.slip.wrong,
                                    tow.time.outliers = tow.time.outliers,roe.on = roe.on)

if(spatial == T) dat.export <- list(log.checks = log.checks,missing.dat = missing.dat,num.rake.wrong = num.rake.wrong,
                                    gear.size.wrong = gear.size.wrong,weight.log.wrong = weight.log.wrong,weight.slip.wrong = weight.slip.wrong,
                                    tow.time.outliers = tow.time.outliers,roe.on = roe.on,watches.outside.survey.bounds = watches.outside.survey.bounds)

# Now I want to make a file name that tells me exactly what I ran, this should be fun!
if(!is.null(export))
{
   # If you are specfiying the file name yourself
  if(export != "fish.dat") write.xlsx(dat.export,export)
  # If you want an automated file name the name gets created up top so it is available for the pdf figure.
  if(export == "fish.dat") write.xlsx(dat.export,paste0(f.name,".xlsx"))
return(dat.export)
} # end if(!is.null(export))
  
} # end function
