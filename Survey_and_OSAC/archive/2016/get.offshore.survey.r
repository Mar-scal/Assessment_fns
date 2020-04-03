################################################################################################################
##### GETS OFFSHORE DATA FROM ORACLE DATABASE
####  Commented and checked by DK starting on July 27, 2015.
# Update history
#Commented, checked  and revised by DK March 31, 2016
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary_data.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#      1: convert.dd.dddd.r
##
###############################################################################################################

###############################################################################################################
# ARGUMENTS
# db.con:   The database to connect to.  Default ="ptran",
# un:       Your username to connect to SQL database.  Default = un.ID
# pw:       Your password to connect to SQL database.  Default = pwd.ID
# direct:   The working directory to point to.  Default ="Y:Offshore scallop/Assessment"
###############################################################################################################

# where did the .ar come from in the below...
# source("fn/get.offshore.survey.ar.r")

### GETS OFFSHORE DATA FROM ORACLE DATABASE
### currently standardized live shell height frequency

# DK August 20, 2015, function call altered so DB credentials are entered directly into function call.
get.offshore.survey <- function(db.con ="ptran", un=un.ID , pw = pwd.ID,direct="Y:Offshore scallop/Assessment")
  {
	require(RODBC) || stop("Package RODBC cannot be found")
	
#	chan <- odbcConnect("bank", "hubleyb", "p35mghk")
#	chan <- odbcConnect("bank.canso3", "scaloff", "fgb256k")

  ### DK:  I believe I need this, but maybe not?
  source(paste(direct_fns,"Survey_and_OSAC/convert.dd.dddd.r",sep="")) #Source7
  
  #DK August 20, 2015 Note: Need this to open the channel, we need to get one more view, or more general access to the OSTOWS table
  # so that the .rProfile method works, for now the workaround would be to put the general (admin?) un/pw into your rprofile...
  #chan <- odbcConnect("ptran", uid=un , pwd=pw)
  chan <- odbcConnect("ptran", uid = un.ID,pwd = pwd.ID)
  
  #####################################################################################################################
  # Jessica has new views for these calls, ,all this prorating is not necessary anymore as she's taken care of it in SQL
  # Key is to import those tables and send it out of this file looking identical!  
  ######################################################################################################################
  
  #qu.strata <- "select * from SCALOFF.OSSTRATA"
  # DK Oct 29, 2015, don't need tow data either, we don't ever use it....
  qu.dead <- "select * from SCALOFF.OSDEADRES_VW"
  qu.live <- "select * from SCALOFF.OSLIVERES_VW"
  qu.sample <- "select * from SCALOFF.OSSAMPLES_VW"
  #qu.tow <- "select * from SCALOFF.OSTOWS"
  
  
  
  # Grab the SQL data from the respective database tables
  #strata <- sqlQuery(chan, qu.strata)
  dead <- sqlQuery(chan, qu.dead)
  live <- sqlQuery(chan, qu.live)
  samp <- sqlQuery(chan, qu.sample)
  #tow <- sqlQuery(chan, qu.tow)
  odbcCloseAll()
  
  
  ## FIRST UP DEAL WITH THE SHELL HEIGHT FREQUENCY DATA
  # Add the "state" of the scallop to the dead/live objects before combining
  live$state <- "live"
  dead$state <- "dead"
  
  # Put all the dead and live scallop data into one object, this is the same length as the SHF list in the old SurvDB object
  # The next step is to add in the remaining columns, likely a few merge's to pull that off.
  all <- rbind(dead,live)

  
  # Now convert The start and end lat/long data into decimal degrees.
  #Source1 source("fn/Survey/convert.dd.dddd.r")
  all$slat<-convert.dd.dddd(all$START_LAT)
  all$slon<-convert.dd.dddd(all$START_LON)
  all$elat<-convert.dd.dddd(all$END_LAT)
  all$elon<-convert.dd.dddd(all$END_LON)
  
  # Take the start/end postion and takes the mid-point of the tow as strata
  all$lon<-with(all,apply(cbind(elon,slon),1,mean))
  all$lat<-with(all,apply(cbind(elat,slat),1,mean))
  
  # Convert the depth from fathoms to meters.
  all$depth<-all$DEPTH_F*1.8288
  
  # Now we can merge "all" with the "tow" object and reproduce the original dataset. Don't actually need to do this anymore....
  #SHF <- merge(all,tow,all.x=T)
  SHF <- all
  
  # Now rearrange the data and select a subset that will be used elsewhere
  choose <- c("YEAR","CRUISE","MGT_AREA_CD","TOW_DATE","TOW_NO","STRATA_ID","slat","slon","elat","elon","depth",
              "state",paste('BIN',seq(0,195,5),sep='_'),"TOW_TYPE_ID","BOTTOM_TEMP")
  SHF <- SHF[,choose]
  # This is not the same size as the object exported from the old get.offshore.survey, but it is the same size as
  # what is needed for SurveySummary, if this is used elsewere the above subset is where it needs changed.
  
  
  ### Next up we make the position object, this is very simple.
  # This is formatted so that it matches the output from previous year's survey data
  pos=subset(all,state=="live",c('MGT_AREA_CD','TOW_NO','START_LAT','START_LON','END_LAT','END_LON','DEPTH_F',
                                 'YEAR','lon','lat','depth',"TOW_DATE"))
  names(pos) <- c("bank","tow","slat","slon","elat","elon","depth.f","year", "lon", "lat", "depth","TOW_DATE")
  
  
  ## FINALLY DEAL WITH THE SHELL HEIGHT FREQUENCY DATA
  #This is basically the OSSAMPLE_VW view with a few little tweaks
  
  # first we need to convert the locations into decimal degree and then calculate the mid-point of the tow as before.
  #Source1 source("fn/Survey/convert.dd.dddd.r")
  samp$slat<-convert.dd.dddd(samp$START_LAT)
  samp$slon<-convert.dd.dddd(samp$START_LON)
  samp$elat<-convert.dd.dddd(samp$END_LAT)
  samp$elon<-convert.dd.dddd(samp$END_LON)
  
  # Take the start/end postion and takes the mid-point of the tow as strata
  samp$lon<-with(samp,apply(cbind(elon,slon),1,mean))
  samp$lat<-with(samp,apply(cbind(elat,slat),1,mean))
  
  # Convert the depth from fathoms to meters.
  samp$depth<-samp$DEPTH_F*1.8288
  

  # Note that I have added in STRATA_ID, I belive this eventually should be what we use for strata and will make the "lon" and "lat" columns redundant
  # I have also removed "TOW_SEQ" as it is never used in SurveySummary, if we need it I will have to check back here!
  choose.samp <- c('MGT_AREA_CD','TOW_NO','START_LAT','START_LON','END_LAT','END_LON','DEPTH_F','YEAR','lon',
                   'lat','depth','CRUISE','SCALLOP_NUM','WET_MEAT_WGT','SHELL_HEIGHT','STRATA_ID','TOW_DATE')
  
  MWs <- samp[,choose.samp]
  
  list(SHF=SHF,MWs=MWs,pos=pos)
} # End get.offshore.survey DK version.