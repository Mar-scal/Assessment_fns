################################################################################################################
##### GETS OFFSHORE DATA FROM ORACLE DATABASE
####  Commented and checked by DK starting on July 27, 2015.
# Update history
#Commented, checked  and revised by DK March 31, 2016
# May 16, 2016:  Updated to work with 64 bit version of R.
# June 15, 2016:  Updated to allow for optoinal printing of the industry report.
# April 2018, Updated to use ROracle
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
# db.con:           The database to connect to.  Default ="ptran", if using 64 bit r use ptran64.
# un:               Your username to connect to SQL database.  Default = un.ID
# pw:               Your password to connect to SQL database.  Default = pwd.ID
# direct:           The working directory to point to.  Default ="Y:Offshore scallop/Assessment"
# industry.report:  Run the industry report.  T/F, default = F
###############################################################################################################

# where did the .ar come from in the below...
# source("fn/get.offshore.survey.ar.r")

### GETS OFFSHORE DATA FROM ORACLE DATABASE
### currently standardized live shell height frequency

# DK August 20, 2015, function call altered so DB credentials are entered directly into function call.
get.offshore.survey <- function(db.con ="ptran", un=un.ID , pw = pwd.ID,industry.report = F,direct="Y:Offshore scallop/Assessment/", ...)
{
	require(ROracle) || stop("Package ROracle cannot be found")
	
#	chan <- odbcConnect("bank", "hubleyb", "p35mghk")
#	chan <- odbcConnect("bank.canso3", "scaloff", "fgb256k")

  ### DK:  I believe I need this, but maybe not?
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep="")) #Source7
  
  #DK August 20, 2015 Note: Need this to open the channel, we need to get one more view, or more general access to the OSTOWS table
  # so that the .rProfile method works, for now the workaround would be to put the general (admin?) un/pw into your rprofile...
  # DK revised April 2018 to ROracle
  chan <-dbConnect(dbDriver("Oracle"),username=un, password=pw,db.con)
  
  #####################################################################################################################
  # Jessica has new views for these calls, ,all this prorating is not necessary anymore as she's taken care of it in SQL
  # Key is to import those tables and send it out of this file looking identical!  
  ######################################################################################################################
  db <- "HUMF" ### CHANGE HUMF TO SCALOFF!!!
  message("reminder that this is pulling data from HUMF views, not production SCALOFF")
  
  #qu.strata <- "select * from SCALOFF.OSSTRATA"
  # DK Oct 29, 2015, don't need tow data either, we don't ever use it.... 
  qu.dead <- paste0("select * from ", db, ".OSDEADRES_SS_VW")
  qu.live <- paste0("select * from ", db, ".OSLIVERES_SS_VW")
  qu.sample <- paste0("select * from ", db, ".OSSAMPLES_SS_VW")
  qu.dead.ice <- paste0("select * from ", db, ".OSDEADRES_ICE_VW")
  qu.live.ice <- paste0("select * from ", db, ".OSLIVERES_ICE_VW")
  qu.sample.ice <- paste0("select * from ", db, ".OSSAMPLES_ICE_VW")
  #qu.tow <- "select * from HUMF.OSTOWS"
  
  
  
  # Grab the SQL data from the respective database tables
  #strata <- sqlQuery(chan, qu.strata)
  # Revised to be ROracle query
  dead <- dbGetQuery(chan, qu.dead)
  live <- dbGetQuery(chan, qu.live)
  samp <- dbGetQuery(chan, qu.sample)
  deadice <- dbGetQuery(chan, qu.dead.ice)
  liveice <- dbGetQuery(chan, qu.live.ice)
  sampice <- dbGetQuery(chan, qu.sample.ice)
  #tow <- sqlQuery(chan, qu.tow)
  dbDisconnect(chan)
  
  dead$species <- "seascallop"
  live$species <- "seascallop"
  samp$species <- "seascallop"
  deadice$species <- "icelandic"
  liveice$species <- "icelandic"
  sampice$species <- "icelandic"
  
  dead <- rbind(dead, deadice)
  live <- rbind(live, liveice)
  samp <- rbind(samp, sampice)
  
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
              "state",paste('BIN',seq(0,195,5),sep='_'),"TOW_TYPE_ID","BOTTOM_TEMP", "species")
  SHF <- SHF[,choose]
  # This is not the same size as the object exported from the old get.offshore.survey, but it is the same size as
  # what is needed for SurveySummary, if this is used elsewere the above subset is where it needs changed.
  
  
  ### Next up we make the position object, this is very simple.
  # This is formatted so that it matches the output from previous year's survey data
  pos=subset(all,state=="live",c('MGT_AREA_CD','TOW_NO','START_LAT','START_LON','END_LAT','END_LON','DEPTH_F',
                                 'YEAR','lon','lat','depth',"TOW_DATE", "species"))
  names(pos) <- c("bank","tow","slat","slon","elat","elon","depth.f","year", "lon", "lat", "depth","TOW_DATE", "species")
  
  
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
  


  
  
  # Industry report
  if(industry.report == T)
  {
    ### read in OSSURVEYS, OSTOWS and OSHFREQ_SAMPLES
    chan <-dbConnect(dbDriver("Oracle"),username=un, password=pw,db.con)
    
    #####################################################################################################################
    # Jessica has new views for these calls, ,all this prorating is not necessary anymore as she's taken care of it in SQL
    # Key is to import those tables and send it out of this file looking identical!  
    ######################################################################################################################
    db <- "HUMF" ### CHANGE HUMF TO SCALOFF!!!
    message("reminder that this is pulling data from HUMF views, not production SCALOFF")
    
    #qu.strata <- "select * from SCALOFF.OSSTRATA"
    # DK Oct 29, 2015, don't need tow data either, we don't ever use it.... 
    qu.surveys <- paste0("select * from ", db, ".OSSURVEYS")
    qu.surveys<- dbGetQuery(chan, qu.surveys)
    
    survey_seq <- paste(as.character(unique(qu.surveys[qu.surveys$CRUISE==cruise,]$SURVEY_SEQ)), sep="' '", collapse=", ")
    qu.tows <- paste0("select * from ", db, ".OSTOWS WHERE SURVEY_SEQ in (", survey_seq, ")")
    qu.tows<- dbGetQuery(chan, qu.tows)
    
    tow_seq <- paste(as.character(unique(qu.tows$TOW_SEQ)), sep="' '", collapse=", ")
    qu.hfreq <- paste0("select * from ", db, ".OSHFREQSAMPLES WHERE TOW_SEQ in (", tow_seq, ")")
    qu.hfreq<- dbGetQuery(chan, qu.hfreq)
    
    hfreq_seq <- paste(as.character(unique(qu.hfreq$HFREQ_SAMPLE_SEQ)), sep="' '", collapse=", ")
    qu.heightfreq <- paste0("select * from ", db, ".OSHEIGHTFREQ WHERE HFREQ_SAMPLE_SEQ in (", hfreq_seq, ")")
    qu.heightfreq<- dbGetQuery(chan, qu.heightfreq)
    dbDisconnect(chan)
    
    surv_tows <- join(qu.tows, qu.surveys, type="left", by="SURVEY_SEQ")
    surv_tows_samp <- join(surv_tows, qu.hfreq, type="full", by="TOW_SEQ")
    surv_tows_samp_hf <- join(surv_tows_samp, qu.heightfreq, type="full", by="HFREQ_SAMPLE_SEQ")
    
    surv_tows_samp_hf$indreport_bin[surv_tows_samp_hf$BIN_ID <70] <- "0-70"
    surv_tows_samp_hf$indreport_bin[surv_tows_samp_hf$BIN_ID >65 & surv_tows_samp_hf$BIN_ID <100] <- "70-100"
    surv_tows_samp_hf$indreport_bin[surv_tows_samp_hf$BIN_ID >95] <- "100+"
    
    surv_tows_samp_hf$prorated_number <- surv_tows_samp_hf$NUMBER_IN_BIN / (surv_tows_samp_hf$SAMPLED/surv_tows_samp_hf$TOTAL)
    
    surv_tows_samp_hf$prorated_number[is.na(surv_tows_samp_hf$prorated_number)] <- 0
    
    industryreport_l <- ddply(.data=surv_tows_samp_hf, .(SURVEY_NAME, MGT_AREA_CD, TOW_NO, START_LAT, START_LON, END_LAT, END_LON, DEPTH_F, SPECIES_ID, LIVECODE, indreport_bin),
                            summarize,
                            total_in_bin=sum(prorated_number))
    
    industryreport_catchbaskets <- ddply(.data=surv_tows_samp_hf[surv_tows_samp_hf$CONTAINER_TYPE_ID ==1,], .(SURVEY_NAME, MGT_AREA_CD, TOW_NO, START_LAT, START_LON, END_LAT, END_LON, DEPTH_F, SPECIES_ID, LIVECODE),
                                         summarize,
                                         catchbaskets=unique(round((unique(TOTAL)/30) *4, 0)/4))
    
    industryreport_l <- join(industryreport_l, industryreport_catchbaskets[!is.na(industryreport_catchbaskets$catchbaskets),], type="left")
    
    industryreport <- dcast(industryreport_l, SURVEY_NAME + MGT_AREA_CD + TOW_NO + START_LAT + START_LON + END_LAT + END_LON + DEPTH_F + SPECIES_ID + catchbaskets ~ LIVECODE + indreport_bin, value.var="total_in_bin")
    
    industryreport$YEAR <- yr
    
    industryreport <- select(arrange(industryreport, SURVEY_NAME, SPECIES_ID, TOW_NO), SURVEY_NAME, MGT_AREA_CD, TOW_NO, START_LAT, START_LON, END_LAT, END_LON, DEPTH_F, SPECIES_ID, catchbaskets, `L_0-70`, `L_70-100`, `L_100+`, `D_0-70`, `D_70-100`, `D_100+`)
    
    industryreport <- industryreport[!is.na(industryreport$catchbaskets),]
    
    industryreport[is.na(industryreport)] <- 0

    # And make the CSV...
    write.csv(industryreport,paste(direct,"Data/Survey_data/",yr,"/IndustryReport_", Sys.Date(), ".csv",sep=""),row.names=F)
  }# End if(industry.report = T)
  
  # Note that I have added in STRATA_ID, I belive this eventually should be what we use for strata and will make the "lon" and "lat" columns redundant
  # I have also removed "TOW_SEQ" as it is never used in SurveySummary, if we need it I will have to check back here!
  choose.samp <- c('MGT_AREA_CD','TOW_NO','START_LAT','START_LON','END_LAT','END_LON','DEPTH_F','YEAR','lon',
                   'lat','depth','CRUISE','SCALLOP_NUM','WET_MEAT_WGT','SHELL_HEIGHT','STRATA_ID','TOW_DATE', "species")
  
  MWs <- samp[,choose.samp]
  
  list(SHF=SHF,MWs=MWs,pos=pos)
} # End get.offshore.survey DK version.