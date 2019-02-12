# An attempt by DK to make the survey design scripts fit within the current structure, we are currently using 
# a variety of files for this which contain the same information as what we use for the survey/modelling and 
# if we leave it like this the duplication will make us look like chumps eventually...
#####  DK August 26th, 2016
# Update history
# Commented, checked  and revised by DK August 2016
# March 2017 : Added option for text points or not on GBa, and changed Ger to have 20 extra random stations so Ginette and Tech can "randomly" 
              # remove 20 of them...also fixed "zoomed in" GBa figures so the Northeast and Northwest labels were correct...
# June 2017:  Added an option for adding in extra stations to the figures.  These would need to be in the file extra.stations.csv, see below.
# July 2017:  Revised so that the seed chosen (either by user or by R) is plotted on the figures.  Tidied up the zoom figures on GBa so that the 
#             dot colors reflect the strata, allowed for the points on all figures to be the EID numbers (not the final survey cruise nubmers so use 
#             with caution), and changed the name of the csv with the survey locations to preliminary to differentiate it from the final csv
#             which is created after the cruise track has been determined for the survey.
# Jan 2018:   DK revised so that the minimum distance between stations on Sable is 2 km, it had historically been 3 km, but with the new
#             Haddock Box and loss of area on Sable it was basically impossible to pull off a survey of 100 stations...
# Feb 2018:   DK revised the German allocation so that something other that 80 tows worked...
# May 2018:  Revised the GBa sampling design to allocate more stations than we would if randomly assigning by area on the northern portion of GBa and tidied up the seedbox dates, now 
#            using our friend lubridate...
# Jan 2019:  DK revised to incorporate Banquereau stations, at this time we are proposing Ban stations to be fixed based on the 2012 stations.  Also had
#            to clean up for the revised Sable strataification due to WEBCA.  DK also made some other tweaks to the function, specifically how the
#            text points are dealt with, changed so the filenames easily incorporate the text point option and cut down on if() statements. We also added the ger.rep argument to allow backup repeat stations

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  None, this is a top level file whose output is either csv files or figures.
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 1: source(paste(direct,"Assessment_fns/Survey_design/alloc.poly_new.r",sep=""))
# 2: source(paste(direct,"Assessment_fns/Survey_design/Relief.plots.r",sep=""))
# 3: source(paste(direct,"Assessment_fns/Survey_design/genran.r",sep=""))
# 4: source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
###############################################################################################################

###############################################################################################################
# Arguments
# yr:            The year of interest for making survey design.  Default is the current year as.numeric(format(Sys.time(), "%Y"))
# direct:        The directory to load/save all the data, Default is the network offshore scallop "Y:/Offshore scallop/Assessment_fns/"
# export:        Do you want to export the survey design locations.  T/F Default is F
# seed:          If you want to reproduce results you can specify the random seed used for allocating these.  Default = NULL which will use R's 
#                internal random number generators.  
# plot:          Do you want to plot the results.  T/F Default is T
# fig:           Where do you want to plot the figures.  Three options, includes the default print to your "screen" optionally can do "pdf" or "png".
# legend:        Add a legend to the figure.  T/F Default  is T
# zoom:          Do you want to produce magnified maps for GBa tow locations.  T/F default is T
# banks:         What banks do you want to run this on.  Default banks are "BBs","BBn","GBa","GBb","Sab","Mid","GB","Ger" (note no option for Ban yet)
# relief.plots:  For German bank do you want to make the "relief plots", note these take a long time to make!!.  T/F and default is F
# digits:        For the relief plots this controls the smoothing of the surface.  Basically this says how many digits to retain in the X and Y locations
#                Default is 4 (which is very detailed, using 3 makes a very smooth surface.)
# point.style:   Do you want to have the points in the zoomed in GBa plots text of the numbers or just cute little cexy circles?  
#                Three options, Default = "points" which plots filled circles, "stn_num" puts in station numbers, and "both" does both
#                "both" uses the x.adj and y.adj proportion to place the ID next to the point.
# x.adj:         adjustment of ID placement relative to the full x-range (e.g. x.adj=0.02 will place ID 2% away from the point in the x direction.)
# y.adj:         adjustment of ID placement relative to the full y-range (e.g. y.adj=0.02 will place ID 2% away from the point in the y direction.)

# ger.new:       Number of new stations to generate on German bank, default is 60 (this must be <= 80, generally we only use 60 or 80), change the
#                alloc.poly number of stations below if you need > 80 new tows
# ger.rep:       Number of German repeat stations to assign. Default is 20 stations, but you can add more to be used as backup repeats
# add.extras:    Do we want to add the extra stations to the figures, the coordinates of these extra stations 
#                would need to be in the file Data/Survey_data/Extra_stations.csv.  T/F with a default of F.
##### SURVEY DESIGN

Survey.design <- function(yr = as.numeric(format(Sys.time(), "%Y")) ,direct = "Y:/Offshore scallop/Assessment/",export = F,seed = NULL, point.style = "points",
                          plot=T,fig="screen",legend=T, zoom = T,banks = c("BBs","BBn","GBa","GBb","Sab","Mid","GB","Ger"),
                          add.extras = F,relief.plots = F,digits=4,ger.new = 60, x.adj=0.02, y.adj=0.02,ger.rep=20, cables=F)
{
# Make sure data imported doesn't become a factor
options(stringsAsFactors=F)
# load required packages
require(PBSmapping) || stop("Install PBSmapping Package bub")
require(lubridate) || stop("Install the lubridate Package before it's too late!")
# load in the functions we need to do the survey design
# Note I put the survey design functions in a "Survey_Design" folder with the other functions, and putting the figures in the "Survey_Design" folder 
source(paste(direct,"Assessment_fns/Survey_design/alloc.poly.r",sep=""))
source(paste(direct,"Assessment_fns/Survey_design/Relief.plots.r",sep=""))
source(paste(direct,"Assessment_fns/Survey_design/genran.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
source(paste(direct,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep=""))

# Bring in flat files we need for this to work, they are survey polyset, survey information, extra staions and the seedboxes.
surv.polyset <- read.csv(paste(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv",sep=""),stringsAsFactors = F) #Read1
areas <- read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""),stringsAsFactors = F) #Read1
surv.polydata <- read.csv(paste(direct,"Data/Survey_data/survey_information.csv",sep=""),stringsAsFactors = F)#Read2
seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),
                     stringsAsFactors = F,header=T) # Read3
seedboxes$Open <- dmy(seedboxes$Open)
# DK created this file by extracting the extra tows from the survey results in years past
#Read4 The idea would be to add new extra stations to this over time so they are all maintained in one location
if(add.extras == T) extra.tows <- read.csv(paste(direct,"Data/Survey_data/Extra_stations.csv",sep=""),stringsAsFactors = F)
# Note the folder structure is using the "Survey_Data" directory, there are two essentially static files stored in the top level directory
# the file with information about fixed station location are in the top level folder (fixed_station_banks_towlst.csv)
# the extra stations for all the banks are also stored in the same location (Extra_stations.csv), my hope is this is the file in which
# all of the new extra stations are stored so that we have the details regarding the extra station locations for all banks in all years
# in one location.

# Define some variables
surv.poly <- NULL
polydata <- NULL
towlst <- NULL

# Close all open plot devices
if(plot == T && !is.null(dev.list())) dev.off(dev.list())
# Get the index for the loop.
num.banks <- length(banks)
#browser()
for(i in 1:num.banks)
{

  # Grab the bank
  bnk <- banks[i]
  # Grab any extra tows selected for that bank and make them a PBS mapping object
  if(add.extras ==F) extras <- data.frame(bill = NULL) # Did it this way to minimize the amount of code I need to change...
  if(add.extras == T) 
  {
    extras <- subset(extra.tows,year == yr & bank == banks[i],select = c("tow","lon","lat","bank"))
    names(extras) <- c("EID","X","Y","bank")
    extras$EID <- as.numeric(extras$EID)
    if(bnk != "Mid") key <-findPolys(extras, subset(areas,bank == bnk))
    if(bnk == "Mid") key <-findPolys(extras, subset(areas,bank == "Sab")) # This will put extras with Middle if any on Sable, but they won't show up on figure so ok.
    extras <- extras[extras$EID %in% key$EID,]
    attr(extras,"projection") <- "LL"
    extras$lon.deg.min <- round(convert.dd.dddd(x = extras$X, format = "deg.min"), 4)
    extras$lat.deg.min <- round(convert.dd.dddd(x = extras$Y, format = "deg.min"), 4)
    extras <- extras[,c("EID", "X", "Y", "lon.deg.min", "lat.deg.min", "bank")]
  } # end if(add.extras == T)
  # Grab any seedboxes that were closed during the current year.
  sb <- subset(seedboxes,Bank == bnk & Open >= ymd(paste(yr,"-01-01",sep="")))
  if(bnk == "GB") sb <- subset(seedboxes,Bank == "GBa" & Open >= ymd(paste(yr,"-01-01",sep="")))
  # Now for the banks that have survey strata we get the allocation.
  if(bnk %in% c("BBs","BBn","GBa","GBb","Sab"))
  {
    # Get the correct survey polygons
    surv.poly[[i]] <- subset(surv.polyset,label==bnk)
    surv.poly[[i]] <- subset(surv.poly[[i]], startyear == max(surv.poly[[i]]$startyear))
    attr(surv.poly[[i]],"projection")<-"LL"
    polydata[[i]] <- subset(surv.polydata,label==bnk)
    # For areas in which we have mutliple survey strata information... e.g. Sable which was changed due to WEBCA.
    polydata[[i]] <- polydata[[i]][polydata[[i]]$startyear == max(polydata[[i]]$startyear,na.rm=T),]
    # For GBa we actually want a different allocation scheme, I've set the number of tows in each strata to be what we had in 2016, this is similar to what we've
    # observed since 2010, but every year has varied slightly (in the north).
    # This scheme is based on a vague comment in the 2013 GBa Assessment about preferntially placing some tows in the northern portion of the bank.  Note that for the 2017 survey we 
    # had simply stratified by area based on disscusions with Ginette and a collective lack of knowledge about the details around the 2013 comment.
    if(bnk == "GBa") polydata[[i]]$allocation <- c(51,37,32,26,35,11,8) 
    # if the seed hasn't been set, set it...
    if(is.null(seed)) seed <- sample(1:2^15, 1)
    # Now allocate the tows, each bank has it's own settings for this allocation.
    #DK Note, I question the use of mindist of 3 on Sable, I'm sure there is good reason for it, but how much does this limit the 
    #actual randomization of the tow locations?
    # DK revised Sable to be a minimum distance of 2 km given that the Haddock box has removed a percentage of the bank...
    
    if(bnk == "BBn") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]], polydata[[i]]),ntows=100,pool.size=3,mindist=1,seed=seed)
    if(bnk == "BBs") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]], polydata[[i]]),ntows=25,seed=seed)
    if(bnk == "Sab") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]][surv.poly[[i]]$startyear==max(surv.poly[[i]]$startyear),], polydata[[i]]),ntows=100,pool.size=3,mindist=2,seed=seed)
    if(bnk == "GBb") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]], polydata[[i]]),ntows=30,pool.size=5,seed=seed)
    if(bnk == "GBa") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]], polydata[[i]]),ntows=200,pool.size=5,mindist=1,seed=seed)
      
    #get the deg dec minutes coordinates too
    if(!bnk == "Ger")  {
      writetows <- towlst[[i]]$Tows
      if(add.extras==T) {
        extras$Poly.ID <- NA
        extras$STRATA <- "extra"
        writetows <- rbind(towlst[[i]]$Tows, extras[,c("EID", "X", "Y", "Poly.ID", "STRATA")])
      }
      writetows$lon.deg.min <- round(convert.dd.dddd(x = writetows$X, format = "deg.min"), 4)
      writetows$lat.deg.min <- round(convert.dd.dddd(x = writetows$Y, format = "deg.min"), 4)
      writetows <- writetows[, c("EID", "X", "Y", "lon.deg.min", "lat.deg.min", "Poly.ID", "STRATA")]
    }
    
	  # if you want to save the tow lists you can export them to csv's.
  	if(export == T && bnk %in% c("BBn","BBs","GB","Mid","Sab", "Ban")) 
  	{
  	  write.csv(writetows,paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Preliminary_Survey_design_Tow_locations_", bnk, ".csv",sep=""),row.names=F) #Write1
  	} # end if(export == T && bnk %in% c("BBn","BBs","GB","Ger","Mid","Sab","Ban")) 
  	if(export == T && bnk %in% c("GBa","GBb")) 
  	{  
  	  write.csv(writetows,paste(direct,"Data/Survey_Data/",yr,"/Summer/",bnk,"/Preliminary_Survey_design_Tow_locations_", bnk, ".csv",sep=""),row.names=F) #Write2
  	} # end if(export == T && bnk %in% c("GBa,GBb")) 
	  
  	# Now if you want to make the plots do all of this.
  	if(plot == T)
  	{
  	  # Where do yo want the plot to go?
  	  if(fig=="screen") windows(11,8.5)
  	  if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", res=420,
  	                        height = 8.5,bg = "transparent")
  	  if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation_",bnk,"_",point.style,".pdf"),width = 11, 
  	                        height = 8.5,bg = "transparent")
  	  
  	  # Make the plot, add a title, the tow locations, any extra tows and any seedboxes + optionally a legend.
  	  ScallopMap(bnk,poly.lst=list(surv.poly[[i]][surv.poly[[i]]$startyear==max(surv.poly[[i]]$startyear),],polydata[[i]]),plot.bathy = T,plot.boundries = T,dec.deg = F)
  	  # For some reason I can't figure out Sable is overplotting the medium strata on top of the high and very high, this is my hack to fix....
  	  if(bnk == "Sab")
  	  {
  	    surv.poly[[i]] <- surv.poly[[i]][surv.poly[[i]]$startyear==max(surv.poly[[i]]$startyear),]
  	    addPolys(surv.poly[[i]][surv.poly[[i]]$PID==4,],col=polydata[[i]]$col[polydata[[i]]$PID==4],border=NA)
  	    addPolys(surv.poly[[i]][surv.poly[[i]]$PID==5,],col=polydata[[i]]$col[polydata[[i]]$PID==5],border=NA)
  	  }
  	  title(paste("Survey (",bnk,"-",yr,")",sep=""),cex.main=2,line=1)

  	  # So what do we want to do with the points, first plots the station numbers
  	  if(point.style == "stn_num") text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
  	  # This just plots the points
  	  if(point.style == "points") addPoints(towlst[[i]]$Tows,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])
  	  # This does both, if it doesn't look pretty change the x.adj and y.adj options
  	  if(point.style == "both" ) 
  	  {
  	    addPoints(towlst[[i]]$Tows,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])
  	    labs <- data.frame(X=towlst[[i]]$Tows$X,Y=towlst[[i]]$Tows$Y,text=towlst[[i]]$Tows$EID)
  	    x.range <- max(abs(labs$X)) - min(abs(labs$X))
  	    y.range <- max(abs(labs$Y)) - min(abs(labs$Y))
  	    labs$X.adj <- labs$X + x.adj*x.range
  	    labs$Y.adj <- labs$Y + y.adj*y.range
  	    text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
  	  } # end if(point.style == "both") 

  	  #if(point.style != "points") text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
  	  #if(point.style == "points") addPoints(towlst[[i]]$Tows,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])

  	  if(nrow(extras) > 0) {
  	    if(point.style == "points") addPoints(extras,pch=24, cex=1,bg="darkorange")
  	    if(point.style == "both" ) 
  	    {
  	      addPoints(extras,pch=24, cex=1,bg="darkorange")
  	      labs <- data.frame(X=extras$X,Y=extras$Y,text=extras$EID)
  	      labs$X.adj <- labs$X + x.adj*x.range
  	      labs$Y.adj <- labs$Y + y.adj*y.range
  	      text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
  	    }
  	  }
  	  if(cables==T){
  	    cables <- readOGR("Y:/Maps/Undersea_cables/AllKnownCables2015.shp")
  	    lines(cables, col="red", lty="dashed")
  	  }
  	  if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
  	  if(legend == T && bnk != "GBa" && bnk!= "GBb") legend('bottomright',legend=polydata[[i]]$PName,pch=21,pt.bg=polydata[[i]]$col,bty='n',cex=0.9, inset = .01)
  	  if(legend == T && bnk %in% c("GBa","GBb")) legend('bottomleft',legend=polydata[[i]]$PName,pch=21,pt.bg=polydata[[i]]$col,bty='n',cex=0.9, inset = .01)
  	  if(legend == T) legend('top',paste("Survey stations (n = ",length(towlst[[i]]$Tows$Y),")",sep=""),pch=21,bty='n',cex=0.9, inset = .01)
  	  if(nrow(extras) > 0 && legend == T) legend('topright',paste("Extra stations (n = ",nrow(extras),")",sep=""),
  	                                             pch=24,bty='n',cex=0.9, inset = .01,pt.bg = "darkorange")
  	  legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
  	  # Turn the device off if necessary.  
  	  if(fig != "screen") dev.off()
  	  
  	  # For Georges Bank in the summer we also create some maps that focus in on certain areas, if you set zoom = T this will happen.
  	  if(zoom == T && bnk == "GBa") 
  	  {
  	    # This looks closely at GBa south.
  	    if(fig=="screen") windows(11,8.5)
  	    if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", 
  	                          res=420,height = 8.5,bg = "transparent")
  	    if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".pdf"),width = 11, 
  	                          height = 8.5,bg = "transparent")
  	    ScallopMap(ylim=c(41.25,41.833),xlim=c(-66.6,-65.85),poly.lst=list(surv.poly[[i]],polydata[[i]]),plot.bathy = T,plot.boundries = T,dec.deg = F,
  	               title=paste("GBa August Survey South (",yr,")",sep=""),cex=1.2)
  	    
  	    # Note that the addPoints function has some sort of error in it as the bg color does not get assigned properly 
  	    # only happens when some of the points are missing from the figure so it is something with the subsetting in there...
  	    if(point.style == "points")  points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])
  	    if(point.style == "stn_num")  text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
  	    # This does both, if it doesn't look pretty change the x.adj and y.adj options
  	    if(point.style == "both") 
  	    {
  	      points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])
  	      labs <- data.frame(X=towlst[[i]]$Tows$X,Y=towlst[[i]]$Tows$Y,text=towlst[[i]]$Tows$EID)
  	      x.range <- max(abs(labs$X)) - min(abs(labs$X))
  	      y.range <- max(abs(labs$Y)) - min(abs(labs$Y))
  	      labs$X.adj <- labs$X + x.adj*x.range
  	      labs$Y.adj <- labs$Y + y.adj*y.range
  	      text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
  	    } # end if(point.style == "both") 


  	    if(nrow(extras) > 0) {
  	      if(point.style == "points") addPoints(extras,pch=24, cex=1,bg="darkorange")
  	      if(point.style == "both" ) 
  	      {
  	        addPoints(extras,pch=24, cex=1,bg="darkorange")
  	        labs <- data.frame(X=extras$X,Y=extras$Y,text=extras$EID)
  	        labs$X.adj <- labs$X + x.adj*x.range
  	        labs$Y.adj <- labs$Y + y.adj*y.range
  	        text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
  	      }
  	    }
  	    if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
  	    legend('bottomright',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
  	    # Turn the device off if necessary.  
  	    if(fig != "screen") dev.off()
  	    
  	    # This looks closely at GBa Northwest
  	    if(fig=="screen") windows(11,8.5)
  	    if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", 
  	                          res=420,height = 8.5,bg = "transparent")
  	    if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".pdf"),width = 11, 
  	                          height = 8.5,bg = "transparent")
  	    ScallopMap(ylim=c(41.833,42.2),xlim=c(-67.2,-66.6),bathy.source="usgs",isobath='usgs',bathcol=rgb(0,0,1,0.3),dec.deg = F,
  	               poly.lst=list(surv.poly[[i]],polydata[[i]]),title=paste("GBa August Survey Northwest (",yr,")",sep=""),cex=1.2)
  	    
  	    # Note that the addPoints function has some sort of error in it as the bg color does not get assigned properly 
  	    # only happens when some of the points are missing from the figure so it is something with the subsetting in there...
  	    if(point.style == "points") points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])
  	    if(point.style == "stn_num") text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
  	    # This does both, if it doesn't look pretty change the x.adj and y.adj options
  	    if(point.style == "both") 
  	    {
  	      points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])
  	      labs <- data.frame(X=towlst[[i]]$Tows$X,Y=towlst[[i]]$Tows$Y,text=towlst[[i]]$Tows$EID)
  	      x.range <- max(abs(labs$X)) - min(abs(labs$X))
  	      y.range <- max(abs(labs$Y)) - min(abs(labs$Y))
  	      labs$X.adj <- labs$X + x.adj*x.range
  	      labs$Y.adj <- labs$Y + y.adj*y.range
  	      text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
  	    } # end if(point.style == "both") 

  	    #if(point.style == "points") points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])

  	    if(nrow(extras) > 0) {
  	      if(point.style == "points") addPoints(extras,pch=24, cex=1,bg="darkorange")
  	      if(point.style == "both" ) 
  	      {
  	        addPoints(extras,pch=24, cex=1,bg="darkorange")
  	        labs <- data.frame(X=extras$X,Y=extras$Y,text=extras$EID)
  	       labs$X.adj <- labs$X + x.adj*x.range
  	        labs$Y.adj <- labs$Y + y.adj*y.range
  	        text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
  	      }
  	    }
  	    if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
  	    legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
  	    
  	    # Turn the device off if necessary.  
  	    if(fig != "screen") dev.off()
  	    
  	    # And this looks closely at GBa in the Northeast.
  	    if(fig=="screen") windows(11,8.5)
  	    if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", 
  	                          res=420,height = 8.5,bg = "transparent")
  	    if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".pdf"),width = 11, 
  	                          height = 8.5,bg = "transparent")  	    
  	    ScallopMap(ylim=c(41.833,42.2),xlim=c(-66.6,-66),bathy.source="usgs",isobath='usgs',bathcol=rgb(0,0,1,0.3),dec.deg=F,
  	               poly.lst=list(surv.poly[[i]],polydata[[i]]),title=paste("GBa August Survey Northeast (",yr,")",sep=""),cex=1.2)
  	    
  	    # Note that the addPoints function has some sort of error in it as the bg color does not get assigned properly 
  	    # only happens when some of the points are missing from the figure so it is something with the subsetting in there...
  	    if(point.style == "points") points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])
  	    if(point.style == "stn_num") text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
  	    # This does both, if it doesn't look pretty change the x.adj and y.adj options
  	    if(point.style == "both" ) 
  	    {
  	      points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])
  	      labs <- data.frame(X=towlst[[i]]$Tows$X,Y=towlst[[i]]$Tows$Y,text=towlst[[i]]$Tows$EID)
  	      x.range <- max(abs(labs$X)) - min(abs(labs$X))
  	      y.range <- max(abs(labs$Y)) - min(abs(labs$Y))
  	      labs$X.adj <- labs$X + x.adj*x.range
  	      labs$Y.adj <- labs$Y + y.adj*y.range
  	      text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
  	    } # end if(point.style == "both" ) 

  	    if(nrow(extras) > 0) {
  	      if(point.style == "points") addPoints(extras,pch=24, cex=1,bg="darkorange")
  	      if(point.style == "both" ) 
  	      {
  	        addPoints(extras,pch=24, cex=1,bg="darkorange")
  	        labs <- data.frame(X=extras$X,Y=extras$Y,text=extras$EID)
  	        labs$X.adj <- labs$X + x.adj*x.range
  	        labs$Y.adj <- labs$Y + y.adj*y.range
  	        text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
  	      }
  	    }
  	    if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
  	    legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
  	    
  	    # Turn the device off if necessary.  
  	    if(fig != "screen") dev.off()
  	  } # end if(zoom == T)
  	} # end if(plot == T)
  } # end if(bnk %in% c("BBs","BBn","GBa","GBb","Sab"))

######  Now do this for Middle and Georges Bank 
#####   Middle has fixed stations so we call in these 15 fixed stations from a flat file
#####   Georges Spring monitoring stations has 30 fixed stations also called from a flat file
#####   Banquereau stations 
  
  if(bnk %in% c("Mid","GB","Ban")) 
  {
    #Read5
    towlst[[i]] <-  subset(read.csv(paste(direct,"Data/Survey_data/fixed_station_banks_towlst.csv",sep="")),Bank == bnk)
    
    #get the deg dec minutes coordinates too
    towlst[[i]]$lon.deg.min <- round(convert.dd.dddd(x = towlst[[i]]$X, format = "deg.min"), 4)
    towlst[[i]]$lat.deg.min <- round(convert.dd.dddd(x = towlst[[i]]$Y, format = "deg.min"), 4)
    towlst[[i]] <- towlst[[i]][, c("EID", "X", "Y", "lon.deg.min", "lat.deg.min")]
    
    writetows <- towlst[[i]]
    
    if(add.extras==T) writetows <- rbind(writetows, extras[,!names(extras) %in% "bank"])
    
    writetows$Bank <- bnk
    writetows$Survey <- "spring"
    
    if(export == T) {
      write.csv(writetows[,c("EID", "X", "Y", "lon.deg.min", "lat.deg.min", "Bank", "Survey")],paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Preliminary_Survey_design_Tow_locations_", bnk, ".csv",sep=""),row.names=F) #Write1
    }
    
    attr(towlst[[i]],"projection") <- "LL"
    if(plot == T)
    {
      if(fig=="screen") windows(11,8.5)
      if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", res=420,
                            height = 8.5,bg = "transparent")
      if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".pdf"),width = 11, 
                            height = 8.5,bg = "transparent")
      ScallopMap(bnk,plot.bathy = T,plot.boundries = T,dec.deg=F)
      title(paste("Survey (",bnk,"-",yr,")",sep=""),cex.main=2,line=1)
      # Add the points, or text or both
      if(point.style == "points") addPoints(towlst[[i]],pch=21, cex=1)
      if(point.style == "stn_num") text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
      # This does both, if it doesn't look pretty change the x.adj and y.adj options
      if(point.style == "both" ) 
      {
        addPoints(towlst[[i]],pch=21, cex=1)
        labs <- data.frame(X=towlst[[i]]$X,Y=towlst[[i]]$Y,text=towlst[[i]]$EID)
        x.range <- max(abs(labs$X)) - min(abs(labs$X))
        y.range <- max(abs(labs$Y)) - min(abs(labs$Y))
        labs$X.adj <- labs$X + x.adj*x.range
        labs$Y.adj <- labs$Y + y.adj*y.range
        text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
      }
      
      if(nrow(extras) > 0) {
        if(point.style == "points") addPoints(extras,pch=24, cex=1,bg="darkorange")
        if(point.style == "both" ) 
        {
          addPoints(extras,pch=24, cex=1,bg="darkorange")
          labs <- data.frame(X=extras$X,Y=extras$Y,text=extras$EID)
          labs$X.adj <- labs$X + x.adj*x.range
          labs$Y.adj <- labs$Y + y.adj*y.range
          text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
        }
      }
      if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
      if(legend == T && bnk != "Ban") legend('bottomleft',paste("Fixed stations (n = ",length(towlst[[i]]$EID),")",sep=""),pch=21,bty='n',cex=0.9, inset = .01)
      if(legend == T && bnk == "Ban") legend('topright',paste("Exploratory repeat stations (n = ",length(towlst[[i]]$EID),")",sep=""),pch=21,bty='n',cex=0.9, inset = .01)
      if(nrow(extras) > 0 && legend == T) legend('bottomright',paste("Extra stations (n = ",nrow(extras),")",sep=""),
                                                 pch=24,bty='n',cex=0.9, inset = .01,pt.bg = "darkorange")
      if(fig != "screen") dev.off()
    }# end if(plot==T)
  } # end if(bnk %in% c("Mid","GB", "Ban")) 
  
# Finally this will run German bank.
if(bnk == "Ger")
  {
  #Read6 Be careful here, I'm assuming that when you run this you are looking at the Ger data from "last year" and looking to identify the
    # tows that you want to repeat for this year, if trying to reproduce something specific for past years you'd need to change yr
    # to the year that you are interested in (if going back before 2012 you may need to create this csv file)
    survey.dat <- read.csv(paste(direct,"Data/Survey_data/",(yr-1),"/Spring/Ger/Survey1985-",(yr-1),".csv",sep=""))
    lastyearstows <- subset(survey.dat,state=='live'& year==(yr-1),c('tow','slon','slat','stratum'))
    lastyearstows$stratum<-1
    #Read7 This contains the boundary polygon from the file "GerSurvPoly1.csv".
    Ger.polyset <- subset(read.csv(paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),stringsAsFactors = F),label==bnk)
    Ger.polyset$PID <- 1 # Force the PID to be 1, since it is a boundary there is only 1 unique PID...
    attr(Ger.polyset,"projection")<-"LL"

    # This gets us the tows for German bank, note we have ger.new new tows and 20 repeats when we call it this way.
    Ger.tow.lst<-alloc.poly(poly.lst=list(Ger.polyset, data.frame(PID=1,PName="Ger",border=NA,col=rgb(0,0,0,0.2),repeats=ger.rep)),
                            ntows=ger.new+20,pool.size=3,mindist=1,repeated.tows=lastyearstows,seed=seed)

    ger.tows <- sample(Ger.tow.lst$Tows$new.tows$EID,size=ger.new,replace=F)
    
    Ger.tow.lst$Tows$new.tows <- Ger.tow.lst$Tows$new.tows[Ger.tow.lst$Tows$new.tows$EID %in% ger.tows,]
    Ger.tow.lst$Tows$new.tows$EID <- 1:nrow(Ger.tow.lst$Tows$new.tows)
    
    # Rename and tidy up the data
    Ger.tow.lst$Tows$new.tows$STRATA="new"
    Ger.tow.lst$Tows$repeated.tows$STRATA="repeated"
    # any repeated tows above 20 get flagged as repeated-backup
    # pull all OTHER repeats as backups and plot/list separately
    names(lastyearstows) <- c("tow", "X", "Y", "stratum")
    Ger.repeat.backups <- plyr::join(lastyearstows, Ger.tow.lst$Tows$repeated.tows, type="full")
    Ger.repeat.backups <- dplyr::select(Ger.repeat.backups[is.na(Ger.repeat.backups$EID),], c("tow", "X", "Y"))
    #if(length(Ger.tow.lst$Tows$repeated.tows$STRATA) > 20) Ger.tow.lst$Tows$repeated.tows$STRATA[21:ger.rep] <- "repeated-backup"
    Ger.tow.lst$Tows$new.tows$Poly.ID=1
    Ger.tow.lst$Tows$repeated.tows$Poly.ID=24
    Ger.repeat.backups$Poly.ID<-24
    Ger.repeat.backups$STRATA = "repeated-backup"
    names(Ger.repeat.backups)[which(names(Ger.repeat.backups)=="tow")] <- "EID"
    Ger.repeat.backups$EID <- Ger.repeat.backups$EID + 2000
    Ger.tow.lst$Tows$backup.repeats <- Ger.repeat.backups
    #if(length(Ger.tow.lst$Tows$repeated.tows$STRATA) > 20) Ger.tow.lst$Tows$repeated.tows$Poly.ID[Ger.tow.lst$Tows$repeated.tows$STRATA=="repeated-backup"] <- 3
    Ger.tow.dat<-do.call("rbind",Ger.tow.lst$Tows)
    
    # Get degree decimal minutes
    Ger.tow.dat$lon.deg.min <- round(convert.dd.dddd(x = Ger.tow.dat$X, format = "deg.min"), 4)
    Ger.tow.dat$lat.deg.min <- round(convert.dd.dddd(x = Ger.tow.dat$Y, format = "deg.min"), 4)
    Ger.tow.dat <- Ger.tow.dat[,c("EID", "X", "Y", "lon.deg.min", "lat.deg.min", "Poly.ID", "STRATA")]
    
    #Write3 If you want to save the data here's where it will go
    if(export == T)  {
      write.csv(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),],paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Preliminary_Survey_design_Tow_locations_",bnk,".csv",sep=""),row.names=F)
      write.csv(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("repeated", "repeated-backup"),],paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Preliminary_Survey_design_Tow_locations_",bnk,"_repbackups.csv",sep=""),row.names=F)
    }
    # Plot this bad boy up if you want to do such things
    if(plot==T)
    {
      if(fig=="screen") windows(11,8.5)
      if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", res=420,
                            height = 8.5,bg = "transparent")
      if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".pdf"),width = 11, 
                            height = 8.5,bg = "transparent")  
      ScallopMap(bnk,plot.bathy = T,plot.boundries = T,dec.deg=F)
      # Add the German bank boundary and then add the survey points
      addPolys(Ger.polyset,border=NA,col=rgb(0,0,0,0.2))

      # Add points, station numbers, or both.
      if(point.style == "points") addPoints(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),],pch=Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$Poly.ID)
      #browser()
      if(point.style == "stn_num") text(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$X,
                                        Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$Y,
                                        label=Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$EID,col='black', cex=0.6)
      # This does both, if it doesn't look pretty change the x.adj and y.adj options
      if(point.style == "both") 
      {
        addPoints(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),],pch=Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$Poly.ID)
        labs <- data.frame(X=Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$X,
                           Y=Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$Y,
                           text=Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$EID)
        x.range <- max(abs(labs$X)) - min(abs(labs$X))
        y.range <- max(abs(labs$Y)) - min(abs(labs$Y))
        labs$X.adj <- labs$X + x.adj*x.range
        labs$Y.adj <- labs$Y + y.adj*y.range
        text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
      }
      title(paste("Survey (",bnk,"-",yr,")",sep=""),cex.main=2,line=1)
      # If there are extra tows or seedboxes plot them
      if(nrow(extras) > 0) {
        if(point.style == "points") addPoints(extras,pch=24, cex=1,bg="darkorange")
        if(point.style == "both" ) 
        {
          addPoints(extras,pch=24, cex=1,bg="darkorange")
          labs <- data.frame(X=extras$X,Y=extras$Y,text=extras$EID)
          labs$X.adj <- labs$X + x.adj*x.range
          labs$Y.adj <- labs$Y + y.adj*y.range
          text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
        }
      }
      if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
      # If the seed was set display this on the plot so you know later how you made that plot!!
      if(!is.null(seed)) legend('bottomleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")

      #browser()
      if(ger.rep<21) legend('top',legend=c('new','repeated'),bty='n',pch=unique(Ger.tow.dat$Poly.ID), inset = .02)
      if(ger.rep>20) legend('top',legend=c('new','repeated', 'repeated-backup'),bty='n',pch=unique(Ger.tow.dat$Poly.ID), inset = .02)

      # Turn off the plot device if not plotting to screen
      if(fig != "screen") dev.off()
      
      ### PLOTS BACKUP REPEATS
      if(fig=="screen") windows(11,8.5)
      if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_repeat.backups.png"),width = 11, units="in", res=420,
                            height = 8.5,bg = "transparent")
      if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_repeat.backups.pdf"),width = 11, 
                            height = 8.5,bg = "transparent")  
      ScallopMap(bnk,plot.bathy = T,plot.boundries = T,dec.deg=F)
      # Add the German bank boundary and then add the survey points
      addPolys(Ger.polyset,border=NA,col=rgb(0,0,0,0.2))
      addPoints(Ger.tow.dat[Ger.tow.dat$STRATA %in% "repeated-backup",],pch=Ger.tow.dat[Ger.tow.dat$STRATA %in% "repeated-backup",]$Poly.ID)
      addPoints(Ger.tow.dat[Ger.tow.dat$STRATA %in% "repeated",],pch=Ger.tow.dat[Ger.tow.dat$STRATA %in% "repeated",]$Poly.ID, bg="black")
      
      title(paste("Repeat backups (",bnk,"-",yr,")",sep=""),cex.main=2,line=1)
      # If there are extra tows or seedboxes plot them
      if(nrow(extras) > 0) {
        if(point.style == "points") addPoints(extras,pch=24, cex=1,bg="darkorange")
        if(point.style == "both" ) 
        {
          addPoints(extras,pch=24, cex=1,bg="darkorange")
          labs <- data.frame(X=extras$X,Y=extras$Y,text=extras$EID)
          labs$X.adj <- labs$X + x.adj*x.range
          labs$Y.adj <- labs$Y + y.adj*y.range
          text(labs$X.adj,labs$Y.adj,label=labs$text,col='black', cex=0.6)
        }
      }
      if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
      # If the seed was set display this on the plot so you know later how you made that plot!!
      if(!is.null(seed)) legend('bottomleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
      
      if(fig != "screen") dev.off()
    } # end if(plot==T)
    
    # Now if you want to make these new fangled relief plots... source(paste(direct,"Assessment_fns/Survey_design/Relief.plots.r",sep=""))
    if(relief.plots == T)  Relief.plots(Ger.tow.dat,fig = fig,digits=digits)
  }# end if(bnk== "Ger")
} # end for(i in 1:num.banks)
return(towlst[[i]]) # Return the last object in case I want to look at it...
} # end function