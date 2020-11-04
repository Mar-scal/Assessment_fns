################################################################################################################
##### This is the script for accessing the survey data and for producing the output data for analysis
#####  This along with Survey Summary figures replaces Survey Summary final
#####  DK December 11 2015.
# Update history
# Commented, checked  and revised by DK April 4th, 2016
# May 12th, updated to use the updated sql database back to 2002.  The method for selecting matched tows on German
# bank also modified, no longer using or creating flat files with the german tow list information in it.
# June 16th, revised to add the "season" variable so we could save results for the spring survey.  Some other minor tweaks
# related to the tow tracks as well.
# June 22nd, I removed the tow tracks from this, they are more a pain than anything...
#            I also changed the MIDDLE bank CF model to a glm model, the gam_d model seems to overestimate CF on the bank 
# July 8th:  Tidied up the GB survey results to exclude the non-repeated station tows.  Also did the same for the CF and MC data
#            that is used for the contour plots.  Should mostly effect the results for GB
# Aug 31st:  Added in some new save options for the Rdata files at the end of script to hopefully lessen the potential for overwriting the 
#            Completed saved data with partial results.  Added in a return function so results are returned to the R workspace after running
#            Also tidied up some of the commenting and Section Numbering.
# 2017:
# June-Aug: Added in binned results for the pre-recruits and the fully recruited scallop, allows you to look at various
#            results for specified bins.  See "bins" for the options.  Changed the strata function as well
# April 2018:  Noticed we had double loaded data for 2000 and 2001 as these are now in the database, fixed.  Also had to make some annoying changes to the
#              script as something changed with how the simple survey code was struggling with the year data due to some rbinds making it a character vector
#              instead of a numeric... Also started using lubridate for the dates on the seedboxes as the as.Date method broke for some reason...
# May 2018: FK implemented restratification for Sable due to WEBCA using a new function: survey.dat.restrat(). 
#                If you need to restratify a bank, check out the function file for instructions! FK also has a private github log of changes made. 
# Sept 2018: DK, attempting to add in the average size of scallop in a tow along with a growth potential for that tow, will then integrate
#            into a spatial picture of the size and growth potential in the survey figures script.  Also moved the write.csv bits to section 3.
# Oct 2018:  DK added ability to split up GBa into spatial districts.  This includes a number of spatial subsets found in Offshore.csv
#            we will need to add in the ability to do proper domain estimators for these sub-areas given the "very low" strata cuts across
#            the different regions.
################################################################################################################
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  None, this is a top level file whose output is either csv files or figures.
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files"), 
# see "Source_relation_SurveySummary.docx" for complete details
#   1:  import.survey.data.r
#   2:  get.offshore.survey.r
#   3:  import.hyd.data.r
#   4:  getdis.r
#   5:  shwt.lme.r
#   6:  condFac.r
#   7:  surv.by.tow.r
#   8:  simple.surv.r
#   9:  assign_strata.r
#  10:  survey.dat.r
#  11:  sprSurv.r
##
###############################################################################################################

###############################################################################################################
# Arguments
# 1:  direct:       The working directory to put figures are from which to grab data.  Default = "Y:/Offshore scallop/Assessment/", 
# 2:  yr.start:     Start year for getting the data.  1984 is default, we haven't generally used older data 
# 3:  yr:           End year.  Default is the current system year: as.numeric(format(Sys.time(), "%Y"))
# 4:  surveys:      Which banks + survey to pull the data for.  Defaults to 'all' which is all banks except Banquereau which isn't yet supported.
#                   This will only work if all 8 banks are surveyed in a given year, if one isn't you'll need to select all banks using the below.
#                   Options are any combo of c("Banspring", BBnspring" ,"BBsspring" ,"Gerspring", "Midspring", "Sabspring", "GBspring" ,"GBbsummer", "GBasummer","GBaspatial")
# 5:  survey.year:  If specified this will overwrite the above "yr" and select the final year of the data, Default = NULL.  This is mostly useful 
#                   if preprocessed = T (that is you already have data up to current year and don't want to re-run the processing step but
#                   want to create the survey data only up to a specific year).
# 6:  preprocessed: If you have already run the function and have the database data saved but want to recreate the processed survey data. Default =F
# 7:  un.ID:        Your SQL database username:  Default = un.ID (which I suggest you specify in your R profile so it is loaded by default)
# 8:  pwd.ID:       Your SQL database password:  Default = pwd.ID (if specified in your R profile it will be loaded by default)
# 9:  season:       For the spring survey we need to identify that we don't have all the results in yet.  When running the 
#                   When summer survey is complete you can also set this to the default of "both".  Used to determine name of saved results.
# 10: bins:         This allows you to pick specific shell height bins to look at.  Default is bins = "bank_default", which will set up bins 
#                   for each bank based on their recruit (RS) and commerical (CS) size bins, using this the bins will be <50, 50-70,70-RS,RS-CS,CS-120,120+
#                   The ideal bins will really be bank specific, these bins are based on growth found for the inshore scallop based on our 
#                   incremental growth results.  Once this is done for offshore we can update these to be something better and tailor it for each bank.
#                   if specifying your own bin it should look something like bin = c(50,70,90,110)
# 11: testing:      Allows for testing of the script when making changes.  Default = T which means the files will be saved as "testing.Rdata" in
#                   the current years data/Survey_data/ folder.  If set to F it will save as specified by options used.
# 12: spatial:      Do we want to do a simple spatial analysis for any area that we have sub-divided into finer regions.  For the moment
#                   we only have this available for GBa.
# 13: commercialsampling:  Do we want to include MW-SH data that were collected during year-round commercial fishing trips? Default is T, yes include all data.
#                    F means include only survey MW-SH samples.
# 14: nickname:     if testing = T, adds a nickname to your Rdata file for testing purposes (so that you don't overwrite one of the hard-coded versions)
###############################################################################################################

survey.data <- function(direct, direct_fns, yr.start = 1984, yr = as.numeric(format(Sys.time(), "%Y")) ,
                        surveys = "all", survey.year= NULL,preprocessed = F,un.ID=un.ID,pwd.ID=pwd.ID,db.con="ptran",
                        season = "both",bins = "bank_default",testing = T,spatial = T, commercialsampling = T, mwsh.test = F, nickname)
{  
  ##############################################################################################################
  ################################### SECTION 1 SECTION 1 SECTION 1 ############################################
  #################################### LOAD PACKAGES, LOAD FLAT FILES (where possible) #########################
  ################################### PRE-PROCESS DATA (where possible)               #########################
  ################################### SECTION 1 SECTION 1 SECTION 1 ############################################
  ##############################################################################################################
  
  ################################### Start Load Packages and Functions #########################################
  # Step 1:  Load all required packages and functions immediately
  #
  ##########################################################################################################
  # First load required packages
  require(PBSmapping)
  require(RColorBrewer)
  require(lubridate)
  require(BIOSurvey2)
  require(sp)
  require(maptools)
  ############################# GENERAL DATA ########################################################
  ############################# GENERAL DATA ########################################################
  # Enter here standard data which is used throughout this script.
  # Note that if you specifed surveys == "all" that runs these banks.
  if(length(surveys)==1 & any(surveys == "all")) surveys = c("BanSeaspring", "BanIcespring", "BBnspring" ,"BBsspring" ,"Gerspring", "Midspring", "Sabspring", "GBspring" ,"GBbsummer", "GBasummer")
  # The length of the loop to run
  num.surveys <- length(surveys)
  atow<-800*2.4384/10^6 # area of standard tow in km2
  
  ############################# END GENERAL DATA ########################################################
  
  ############################# LOAD FUNCTIONS ########################################################
  
  # Now load all functions in the program in one location.  All calls to these functions are linked via the commented source #
  # so we can easily tie the function call to the script for that function.
  # The  functions are in this directory unless explicitly specified
  # These 8 functions are pre-processing functions used to bring in and arrange various pieces of data
  source(paste(direct_fns,"Survey_and_OSAC/import.survey.data.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/get.offshore.survey.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/import.hyd.data.r",sep="")) 
  
  # These are the functions used to within the heart of the code to make stuff happen
  source(paste(direct_fns,"Survey_and_OSAC/getdis.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/shwt.lme.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/condFac.r",sep="")) 
  
  source(paste(direct_fns,"Survey_and_OSAC/assign_strata.r",sep=""),local=T) 
  
  source(paste(direct_fns,"Survey_and_OSAC/survey.dat.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/survey.dat.restrat.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/sprSurv.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/surv.by.tow.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/simple.surv.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/growth_potential.r",sep="")) 
  ################################## End Load Functions   #######################################################
  
  ################################## Update the run log   #######################################################
  # add an entry into the run log
  if(!file.exists(paste0(direct_fns, "Survey_and_OSAC/SurveySummaryRunLog.csv"))) runlog <- data.frame(X=NULL, runfunction=NULL, runassigned=NULL, rundefaults=NULL)
  if(file.exists(paste0(direct_fns, "Survey_and_OSAC/SurveySummaryRunLog.csv"))) runlog <- read.csv(paste0(direct_fns, "Survey_and_OSAC/SurveySummaryRunLog.csv"))
  runlog <- runlog[, !names(runlog) %in% "X"]
  rundate <- as.character(Sys.time())
  runfunction <- "data"
  runassigned <- paste(as.character(deparse(match.call())), collapse="")
  rundefaults <- paste(as.character(deparse(args(survey.data))), collapse="")
  runlog <- rbind(runlog, cbind(rundate, runfunction, runassigned, rundefaults))
  write.csv(runlog, file = paste0(direct_fns, "Survey_and_OSAC/SurveySummaryRunLog.csv"))
  ################################# End update the runlog #######################################################
  
  ################################### START LOAD & PRE-PROCESS DATA ############################################
  # Step 2:  Load Data: Load all input data here, 
  ##########################################################################################################
  
  # If we already have the data we can skip this and just load the data, saves about 2 minutes...
  if(preprocessed == F)
  {
    ################# LOAD GENERAL FLAT FILES ###################################################################
    
    #Read1 This is a huge flat file with decades of condition factor, shell weight, and meat height data in it up to 2012.
    ps.dat<-read.csv(paste(direct,"data/Condition/PSGB.csv",sep=""))
    
    #Read2 Polygons for all the offshore banks
    newAreaPolys<-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep="")
                           ,stringsAsFactors = F,header=T)
    #Read3 All the seedboxes ever.
    seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),
                         stringsAsFactors = F,header=T)
    seedboxes$Closed <- dmy(seedboxes$Closed)
    seedboxes$Open <- dmy(seedboxes$Open)
    # Dump the commments they are just messy..
    seedboxes <- seedboxes[,-grep("comment",names(seedboxes))]
    
    #Read4 Get the survey boundary polygons for all banks.
    survey.bound.polys<-read.csv(paste0(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv"), 
                                 header=T,stringsAsFactors = F)
    #Read5 Get the detailed survey polygons for all banks
    survey.detail.polys <- read.csv(paste0(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv"), 
                                    header=T,stringsAsFactors = F)
    #Read6 Get the survey information for each bank
    survey.info <- read.csv(paste(direct,"data/Survey_data/survey_information.csv",sep=""),
                            header=T,stringsAsFactors = F)
    
    #Read7 The boundary between Fully recruited and recruit size classes (RS = Recruit min, CS = Fully recruited (commercial) min)
    size.cats <- read.csv(paste(direct,"data/Size_categories_by_bank.csv",sep=""),
                          header=T,stringsAsFactors = F)
    ###############################################################################################################
    ################################## End LOAD FLAT FILES ################################## 
    
    
    ###############################################################################################################
    ################################## BEGIN DATA PRE-PROCESSING ################################## 
    #
    
    # This creates an object (and also exports the data to a csv file) containing the 
    # May and August survey data from 1984 until 2006  Given these files exist and are no longer being
    # updated why don't we just import the flat files and what are we exporting new flat files every time?
    # DK NOTE:  I was worried that we were mixing up fathoms and meters
    # with the next few pieces of data, but as best I can tell the data
    # in the flat files coming from import.survey.data are already in meters, e.g. look at plot(survMay.dat$depth~survMay.dat$year)
    # and plot plot(survAug.dat$depth~survAug.dat$year) in this latter one it does appear we stop sampling deep from 1990 onwards
    # which worries me about potential for biasing results a bit but nothing pathological I don't imagine.
    #Source1 source("fn/import.survey.data.r")
    # NOTE:  This function will go away once we have Offshore data loaded, should be spring 2016
    # Currently the data in the database is loaded back to 2000.
    # Import 2006 for BanIce data, then we'll remove back to 2000 in a few lines.
    
    survMay.dat<-import.survey.data(1984:2006,survey='May',explore=T,export=F,direct=direct, direct_fns=direct_fns)
    survAug.dat<-import.survey.data(1981:1999,survey='Aug',explore=T,export=F,direct=direct, direct_fns=direct_fns)
    
    print("import.survey.data done")
    
    # keep BanIce separate
    survBanIce.dat <- survMay.dat[survMay.dat$bank =="BanIce",]
    # take out BanIce
    survMay.dat <- survMay.dat[!survMay.dat$bank == "BanIce",]
    # take out years 2001-2006
    survMay.dat <- survMay.dat[survMay.dat$year<2001,]
    # take out 2000 for all banks except browns, GB
    survMay.dat <- survMay.dat[!(survMay.dat$bank %in% c("Ger", "Sab", "Mid", "Ban", "BBs") & survMay.dat$year==2000),]
    
    print("check years in import.survey.data to update for any additional historical data that has been loaded since last time.")
    
    # Here we are subseting these data and getting rid of totwt and baskets bearing and distance coefficient
    survMay.dat<-survMay.dat[which(!names(survMay.dat)%in%c("dis","brg",'totwt','baskets'))]
    survAug.dat<-survAug.dat[which(!names(survAug.dat)%in%c("dis","brg",'totwt','baskets'))]
    # Using month as an indicator of whether data is spring or summer, so these might be off by a month but will do the trick.
    survMay.dat$month <-  5
    survAug.dat$month <-  8
    # Make the dates Date format so we don't lose them at the merge.
    survAug.dat$date <- as.Date(survAug.dat$date,format = "%Y-%m-%d")
    survMay.dat$date <- as.Date(survMay.dat$date,format = "%Y-%m-%d")
    survMay.dat$species <- "seascallop"
    survAug.dat$species <- "seascallop"
    
    # From Whatever year we have the data loaded into SQL database we are getting the data directly from the SQL server.
    # As of Winter 2016 this was an ongoing process.
    #Source6 source("fn/get.offshore.survey.r") Get the data directly from SQL
    SurvDB<-get.offshore.survey(db.con = db.con, un=un.ID , pw = pwd.ID,direct=direct, direct_fns=direct_fns)
    # subset by yr to cut off data past specified yr
    
    SurvDB$SHF <- SurvDB$SHF[SurvDB$SHF$YEAR < (yr+1),]
    SurvDB$MWs <- SurvDB$MWs[SurvDB$MWs$YEAR < (yr+1),]
    SurvDB$pos <- SurvDB$pos[SurvDB$pos$year < (yr+1),]

    # SurvDB$SHF$bank[SurvDB$SHF$bank == "Ban" & SurvDB$SHF$species == "icelandic"] <- "BanIce"
    # SurvDB$MWs$bank[SurvDB$MWs$bank == "Ban" & SurvDB$MWs$species == "icelandic"] <- "BanIce"
    # SurvDB$pos$bank[SurvDB$pos$bank == "Ban" & SurvDB$pos$species == "icelandic"] <- "BanIce"
    
    print("get.offshore.survey done")
    
    ### Do any preprocessing to the BanIce data here
    if("BanIcespring" %in% surveys) {
     BanIceSurvey2012 <- read.csv(paste0(direct, "Data/Survey_data/2012/Spring/BanIceSurvey2012.csv"))
     BanIceSurvey2012 <- BanIceSurvey2012[,which(names(BanIceSurvey2012)=="year"):which(names(BanIceSurvey2012)=="random")]
     message("Note: the pre/rec/com estimates and HF data for tow 936 in .../2012/r/data/Ban/BanIceSurvey2012.csv (2006 and 2012 flat data)\nare WRONG and are based on incorrect bins. We will recalculate these later in this function.\nAlso note that BanIce tow 936 HF data was corrected in 2019.\nThe accurate raw HF data are in .../Data/2012/Spring/TE13BanIcehf.csv. NOT IN THE Y:/Alan/... FOLDER\nNOR IN Y:/Offshore scallop/Assessment/2012/r/data FOLDER.")
    }
    
    ### This grabs the data directly from the database and makes it it's own object, works for now
    ### since we don't use the data for anything but a plot on GB.
    temps <- subset(SurvDB$SHF,state == "live",
                    select = c("TOW_NO","BOTTOM_TEMP","YEAR","MGT_AREA_CD","CRUISE","slon","slat","elat","elon", "species"))
    names(temps) <- c("tow","mean.temp","year","bank","cruise","slon","slat","elat","elon", "species")
    # Remove the temperature data so the survey data from before temperature data was collected lines up nicely. (once db is updated back
    # to spring of 2011 this will become unncessary
    SurvDB$SHF <- SurvDB$SHF[,-which(names(SurvDB$SHF)=="BOTTOM_TEMP")]
    
    # Names for the MW and SHF data.
    hdr.mw <- c("bank","tow","slat","slon","elat","elon","depth.f","year",
                "lon","lat","depth","cruise","scalnum","wmw","sh","ID","month", "species")
    hdr.shf <- c("year","cruise","bank","date","tow","stratum","slat","slon","elat","elon","depth","state","h5","h10","h15",
                 "h20","h25","h30","h35","h40","h45","h50","h55","h60","h65","h70","h75",
                 "h80","h85","h90","h95","h100","h105","h110","h115","h120","h125","h130","h135","h140","h145","h150","h155",
                 "h160","h165","h170","h175","h180","h185","h190","h195","h200","random", "species")
    names(SurvDB$MWs)<-hdr.mw
    names(SurvDB$SHF)<-hdr.shf
    SurvDB$SHF$year <- as.numeric(SurvDB$SHF$year)
    SurvDB$SHF$month <- as.numeric(format(SurvDB$SHF$date, "%m"))
    SurvDB$pos$month <- as.numeric(format(SurvDB$pos$TOW_DATE, "%m"))
    SurvDB$MWs$month <- as.numeric(format(SurvDB$MWs$month, "%m"))
    # Replace the strata ID with a unique identifier.
    SurvDB$MWs$ID<-with(SurvDB$MWs,paste(cruise,tow,sep='.'))
    # Combine the Survey data.  This first one has all of the SHF data there is.
    all.surv.dat <- rbind(survMay.dat,survAug.dat,SurvDB$SHF) # Springsurv2011$SHF,
    # Extract the month so we can determine spring (< 7) vs. summer (> 6) surveys.  Really only necessary for GBa/GBb to be honest.
    all.surv.dat$survey[all.surv.dat$month < 7] <- "spring"
    all.surv.dat$survey[all.surv.dat$month > 6] <- "summer"
    # Make German and BBn all spring too, 2015 did occur in summer, but these are spring surveys so makes the below work easier.
    all.surv.dat$survey[all.surv.dat$bank %in% c("Ger","BBn")] <- "spring"
    
    # Make German and BBn all spring too, 2015 did occur in summer, but these are spring surveys so makes the below work easier.
    all.surv.dat$survey[all.surv.dat$bank %in% c("Ger","BBn")] <- "spring"
    
    # This makes sure that ALL the data have the lat/long calculated in the same way
    all.surv.dat$lon<-with(all.surv.dat,apply(cbind(elon,slon),1,mean))
    all.surv.dat$lat<-with(all.surv.dat,apply(cbind(elat,slat),1,mean))
    
    print("NEED TO REVISE import.hyd.data yrs everytime more historical data is added to database. We need to investigate potential duplication?!")
    
    # THe MW data is now from 2006 to current soon to be all data...
    MW.dat.new <- SurvDB$MWs
    MW.dat.new$bank[MW.dat.new$bank=="Ban" & MW.dat.new$species=="icelandic"] <- "BanIce"
    #Source7 	source("fn/import.hyd.data.r") 'Hydration' sampling, essentially this is the MW data that isn't yet in the SQL DB
    # NOTE:  This function will go away once we have Offshore data loaded, someday...
    MW.dat<-import.hyd.data(yrs=1982:2000, export=F,dirt=direct)
    # No hydration data was collected from Icelandic scallops on Banquereau until 2012, so this next line is unnecessary
    # if("BanIcespring" %in% surveys) MW.dat.BanIce <- import.hyd.data(yrs=2001:2006, Bank="BanIce", export=F, dirt=direct)
    
    # remove "commercial" survey tows that were done in the past
    if(commercialsampling == F) MW.dat <- MW.dat[!MW.dat$tow==0,]
    
    print("import.hyd.data done")
    
    # You're done with the SQL calls at this point so remove your username and password so they don't get saved...
    rm("un.ID","pwd.ID")
    
    # Now save the data so you don't have to do all that every time
    if(is.null(nickname)) save(list = ls(all.names = TRUE), file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.Rdata",sep=""))
    if(!is.null(nickname)) save(list = ls(all.names = TRUE), file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed_", nickname, ".Rdata",sep=""))
    
  } # end if(preprocessed == F)
  
  # Load the preprocessed data, this is a little stick handling to make sure I have survey's picked right when using this data.
  if(preprocessed == T) 
  {
    # You're done with the SQL calls at this point so remove your username and password so they don't get saved...
    # If they don't exist you'll get a warning but who cares...
    rm("un.ID","pwd.ID")
    
    # Also need to save some of the function arguements so they don't get overwritten  when loading the preprocessed data...
    tmp <- surveys		
    dirc <- direct
    s.year <- survey.year
    ssn <- season
    bins.tmp <- bins
    test <- testing
    mwsh <-mwsh.test
    
    if(!is.null(nickname)) load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed_", nickname, ".Rdata",sep=""))  
    if(is.null(nickname)) load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.Rdata",sep=""))  
    # Reset the arguement names and re-load the functions to ensure we have the latest versions
    direct <- dirc
    # These are the functions used to within the heart of the code to make stuff happen
    source(paste(direct_fns,"Survey_and_OSAC/getdis.r",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/shwt.lme.r",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/condFac.r",sep="")) 
    #source(paste(direct_fns,"Survey_and_OSAC/surv.by.tow.r",sep="")) 
    #source(paste(direct_fns,"Survey_and_OSAC/simple.surv.r",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/assign_strata.r",sep=""),local=T) 
    #source(paste(direct_fns,"Survey_and_OSAC/survey.dat.r",sep="")) 
    #source(paste(direct_fns,"Survey_and_OSAC/sprSurv.r",sep=""))
    
    source(paste(direct_fns,"Survey_and_OSAC/survey.dat.r",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/sprSurv.r",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/surv.by.tow.r",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/simple.surv.r",sep="")) 
    surveys <- tmp
    num.surveys <- length(surveys)
    survey.year <- s.year
    season <- ssn
    bins <- bins.tmp
    testing <- test
    mwsh.test <-mwsh
  } # end if(preprocessed == T) 
  
  
  ###############################################################################################################
  ################################## END DATA PRE-PROCESSING ################################## 
  ################################################################################################################
  ################################### END SECTION 1 END SECTION 1#################################################
  ################################### END LOAD & PRE-PROCESS DATA ###############################################
  ################################### END SECTION 1 END SECTION 1#################################################
  ###############################################################################################################
  
  
  
  
  
  ##############################################################################################################
  ################################### SECTION 2 SECTION 2 SECTION 2 ############################################
  # This is where all the analyses takes place.
  
  ################################### SECTION 2 SECTION 2 SECTION 2 ############################################
  ###############################################################################################################
  
  # Create a new column that ID's the Bank-Survey combo...
  # Make all the GB spring data just GB as we don't differentiate it here.
  all.surv.dat$bank[all.surv.dat$bank %in% c("GBa","GBb","GB") & all.surv.dat$survey == "spring"] <- "GB"
  # Incorporated BanIcespring and Banspring in January/July 2019 to prep for potential 2019 survey.
  all.surv.dat$bank[all.surv.dat$bank %in% "Ban" & all.surv.dat$species=="icelandic"] <- "BanIce"
  all.surv.dat$surv.bank <- paste0(all.surv.dat$bank,all.surv.dat$survey)
  # Remove GBsummer data are ambiguous as to whether they are from GBa or GBb so remove them too...
  all.surv.dat <- subset(all.surv.dat,surv.bank != "GBsummer")
  
  # We only survey BBs from time to time (maybe never once Fundian Channel happens), so make sure we have BBs data for the year of interest
  BBs.this.year <- nrow(all.surv.dat[all.surv.dat$surv.bank == "BBsspring" & all.surv.dat$year == survey.year,])
  # If there is no data remove BBs from the survey list and reduce the number of surveys accordingly
  if(BBs.this.year == 0) {surveys <- surveys[surveys != "BBsspring"]; num.surveys <- length(surveys)}
  
  # Prep the BanIce data here too
  if("BanIcespring" %in% surveys) BanIceSurvey2012$surv.bank <- paste0(BanIceSurvey2012$bank,"spring")
  
  # Now if we are going to run the spatial sub-areas we can nicely increase the number of survey
  spat.names <- NULL
  if(spatial == T)
  {
    # Make sure we have spatial data...
    if(nrow(newAreaPolys[newAreaPolys$subarea == "Y",])>0) 
    {
      spat.names <- newAreaPolys[newAreaPolys$subarea == "Y",c("label","bank","survey_target")]
      surveys <- c(surveys,unique(spat.names$label))
      num.surveys <- length(surveys)
    }
  } # end if(spatial == T)
  
  
  #Initialize some variables
  bank.dat <- NULL
  strata.mis.match <- NULL
  mw <- NULL
  SpatHtWt.fit <- NULL
  mw.dat.all <- NULL
  cf.data <- NULL
  surv.dat <- NULL
  surv.Clap <- NULL
  surv.Clap.Rand <- NULL
  surv.Live <- NULL
  surv.Rand <- NULL
  survey.obj <- NULL
  clap.survey.obj <- NULL
  SS.summary <- NULL
  SHF.summary <- NULL
  CF.current <- NULL
  #tow.dis <- NULL
  seedbox.obj <- NULL
  lined.survey.obj <- NULL
  merged.survey.obj <- NULL
  pot.grow <- NULL
  survey.strata.table <- NULL
  detail.surv.poly <- NULL
  bound.surv.poly <- NULL
  if(yr==2020) Ind2020 <- NULL
  # Now get the survey summary results for all the banks...
  #num.surveys = 1
  #surveys <- "GBa-Large_core"
  
  for(i in 1:num.surveys)
  {

    # first things first, if you're dealing with Icelandic scallops from Banquereau, go to the one-off script:
    if("BanIcespring" %in% surveys[i]){
      message("Running BanIce survey summary in BanIce_SurveySummary_data.R since BanIce 2006 and 2012 is in flat files.")
      BanIceSurvey_new <- all.surv.dat[all.surv.dat$year>2012 & all.surv.dat$bank=="BanIce",]
      BanIceMW_new <- MW.dat.new[MW.dat.new$year>2012 & MW.dat.new$bank=="BanIce",]
      
      source(paste0(direct_fns, "Survey_and_OSAC/BanIce_SurveySummary_data.R"))
      BanIce <- BanIce_SurveySummary_data(yr=yr, survey.year=survey.year, surveydata=BanIceSurvey2012,
                                          meatweightdata_2012 = paste0(direct, "Data/Survey_data/2012/Spring/TE13mtwt.csv"),
                                          positionsdata_2012=paste0(direct, "Data/Survey_data/2012/Spring/TE13positions.csv"),
                                          commercialsampling=commercialsampling, BanIceSurvey_new=BanIceSurvey_new, BanIceMW_new=BanIceMW_new, mwsh.test.ban = mwsh.test,
                                          bins="bank_default", RS= size.cats$RS[size.cats$Bank == "Ban"],CS = size.cats$CS[size.cats$Bank == "Ban"], 
                                          survey.bound.polys=survey.bound.polys, survey.detail.polys=survey.detail.polys)

      bank.dat[["BanIce"]] <- BanIce$bank.dat
      SpatHtWt.fit[["BanIce"]] <- BanIce$SpatHtWt.fit
      cf.data[["BanIce"]] <- BanIce$cf.data
      surv.dat[["BanIce"]] <- BanIce$surv.dat
      surv.Clap[["BanIce"]] <- BanIce$surv.Clap
      surv.Clap.Rand[["BanIce"]] <- BanIce$surv.Clap.Rand
      surv.Live[["BanIce"]] <- BanIce$surv.Live
      surv.Rand[["BanIce"]] <- BanIce$surv.Rand
      survey.obj[["BanIce"]] <- BanIce$survey.obj
      clap.survey.obj[["BanIce"]] <- BanIce$clap.survey.obj
      SS.summary[["BanIce"]] <- BanIce$SS.summary
      SHF.summary[["BanIce"]] <- BanIce$SHF.summary
      CF.current[["BanIce"]] <- BanIce$CF.current
      seedbox.obj[["BanIce"]] <- BanIce$seedbox.obj
      lined.survey.obj[["BanIce"]] <- BanIce$lined.survey.obj
      merged.survey.obj[["BanIce"]] <- BanIce$merged.survey.obj
      pot.grow[["BanIce"]] <- BanIce$pot.grow
      survey.strata.table[["BanIce"]] <- BanIce$survey.strata.table
      detail.surv.poly[["BanIce"]] <- BanIce$detail.surv.poly
      bound.surv.poly[["BanIce"]] <- BanIce$bound.surv.poly
      mw.dat.all[["BanIce"]] <- BanIce$mw.dat.all
      
      bnk <- "BanIce"
      bank.4.spatial <- "BanIce"
      bank.dat[["BanIce"]]$survey <- "Spring"

    }
    
    # In 2020, a minimal survey of BBn, GBa and GBb was conducted by industry. Survey design and sampling protocols were adjusted significantly. 
    # As such, we will diverge from this script to analyse these data. 
    if(any(c("BBnspring", "GBasummer", "GBbsummer") %in% surveys[i]) & yr == 2020){
      
      message("Running 2020 survey summary in Industry2020_SurveySummary_data.R since 2020 survey was industry-lead\nand used different survey design and protocols than usual.")
      
      bnk <- substr(surveys[i], 1, 3)
      if(is.null(survey.year)) survey.year <- yr
      
      # first things first, we need to grab the data for the tows that are repeats for the tows in the 2020 survey
      require(readxl)
      require(tidyverse)
      browser()
      if(bnk=="BBn") {
        repeat.list.full <- read_excel(paste0(direct, "Data/Survey_data/2020/LE12BBn2020towlist.xlsx"))
        repeat.list <- read_excel(paste0(direct, "Data/Survey_data/2020/LE12BBn2020towlist.xlsx"), skip = 1)
        names(repeat.list) <- paste0(names(repeat.list.full), "_", names(repeat.list))
        
        repeat.list <- dplyr::select(repeat.list, `LE12_Tow no....2`, `pastyear_LE09-07`, `pasttow_Tow no....12`)
        names(repeat.list) <- c("tow_2020", "year", "tow")
        
        
        IndSurvey2020 <- data.frame(all.surv.dat[all.surv.dat$year==2020 & all.surv.dat$bank %in% bnk,], tow_2020=NA)
        repeat.dat <- left_join(repeat.list, all.surv.dat[all.surv.dat$bank==bnk,])
        IndSurvey2020 <- full_join(IndSurvey2020, repeat.dat)
        Ind_MW_new <- NULL
        
        years <- min(IndSurvey2020$year):yr
        
        pastyear <- repeat.list %>%
          select(-tow) %>%
          rename(pastyear=year, tow=tow_2020) %>%
          mutate(year=2020)
        
        IndSurvey2020 <- left_join(IndSurvey2020, pastyear)
      }
      # for GBa and GBb:
      if(!bnk == "BBn") {
        repeat.list.full <- read_excel(paste0(direct, "Data/Survey_data/2020/LE11", bnk, "2020towlist.xlsx"))
        repeat.list <- read_excel(paste0(direct, "Data/Survey_data/2020/LE11", bnk, "2020towlist.xlsx"), skip = 1)
        names(repeat.list) <- paste0(names(repeat.list.full), "_", names(repeat.list))
        
        if(bnk=="GBb") repeat.list <- dplyr::select(repeat.list, `CruiseLE11_Tow no....2`, `pastyear_LE10`, `pasttow_Tow no....12`, `MW-SH_sample`)
        if(bnk=="GBa") repeat.list <- dplyr::select(repeat.list, `LE11_Tow no....2`, `pastyear_LE10-08`, `pasttow_Tow no....13`, `MW-SH_sample`)
        
        repeat.list <- repeat.list[complete.cases(repeat.list),]
        names(repeat.list) <- c("tow_2020", "year", "tow", "mwsh")
        repeat.list$year <- as.numeric(as.character(repeat.list$year))
        repeat.list.sub <- dplyr::select(repeat.list, -mwsh)
        IndSurvey2020 <- data.frame(all.surv.dat[all.surv.dat$year==2020 & all.surv.dat$bank %in% bnk,], tow_2020=NA)
        repeat.dat <- left_join(repeat.list.sub, all.surv.dat[all.surv.dat$bank==bnk,])
        IndSurvey2020 <- full_join(IndSurvey2020, repeat.dat)
        
        pastyear <- repeat.list.sub %>%
          select(-tow) %>%
          rename(pastyear=year, tow=tow_2020) %>%
          mutate(year=2020)
        
        IndSurvey2020 <- left_join(IndSurvey2020, pastyear)
        
        MW.dat.new$year <- as.numeric(as.character(MW.dat.new$year))
        Ind_MW_new <- MW.dat.new[MW.dat.new$year==2020 & MW.dat.new$bank== bnk,]
        repeat.dat.mw <- left_join(repeat.list[repeat.list$mwsh=="Yes",], MW.dat.new[MW.dat.new$bank==bnk,])
        Ind_MW_new <- full_join(Ind_MW_new, repeat.dat.mw)
        
        Ind_MW_new <- left_join(Ind_MW_new, pastyear)
        
        years <- min(IndSurvey2020$year):yr
      }
      
      source(paste0(direct_fns, "Survey_and_OSAC/Industry2020_SurveySummary_data.R"))
      Ind2020[[bnk]] <- Industry2020_SurveySummary_data(yr=yr, survey.year=survey.year, years=years, surveydata=IndSurvey2020,
                                          commercialsampling=commercialsampling, meatweightdata=Ind_MW_new,
                                          bins="bank_default", RS= size.cats$RS[size.cats$Bank == bnk],CS = size.cats$CS[size.cats$Bank == bnk], 
                                          survey.bound.polys=survey.bound.polys, survey.detail.polys=survey.detail.polys)
      
      bank.dat[[bnk]] <- Ind2020[[bnk]]$bank.dat
      SpatHtWt.fit[[bnk]] <- Ind2020[[bnk]]$SpatHtWt.fit
      cf.data[[bnk]] <- Ind2020[[bnk]]$cf.data
      surv.dat[[bnk]] <- Ind2020[[bnk]]$surv.dat
      surv.Clap[[bnk]] <- Ind2020[[bnk]]$surv.Clap
      surv.Clap.Rand[[bnk]] <- Ind2020[[bnk]]$surv.Clap.Rand
      surv.Live[[bnk]] <- Ind2020[[bnk]]$surv.Live
      surv.Rand[[bnk]] <- Ind2020[[bnk]]$surv.Rand
      survey.obj[[bnk]] <- Ind2020[[bnk]]$survey.obj
      clap.survey.obj[[bnk]] <- Ind2020[[bnk]]$clap.survey.obj
      SS.summary[[bnk]] <- Ind2020[[bnk]]$SS.summary
      SHF.summary[[bnk]] <- Ind2020[[bnk]]$SHF.summary
      CF.current[[bnk]] <- Ind2020[[bnk]]$CF.current
      seedbox.obj[[bnk]] <- Ind2020[[bnk]]$seedbox.obj
      lined.survey.obj[[bnk]] <- Ind2020[[bnk]]$lined.survey.obj
      merged.survey.obj[[bnk]] <- Ind2020[[bnk]]$merged.survey.obj
      pot.grow[[bnk]] <- Ind2020[[bnk]]$pot.grow
      survey.strata.table[[bnk]] <- Ind2020[[bnk]]$survey.strata.table
      detail.surv.poly[[bnk]] <- Ind2020[[bnk]]$detail.surv.poly
      bound.surv.poly[[bnk]] <- Ind2020[[bnk]]$bound.surv.poly
      mw.dat.all[[bnk]] <- Ind2020[[bnk]]$mw.dat.all
    
      bank.4.spatial <- bnk
      bank.dat[[bnk]]$survey <- "Summer" # note: BBn survey is normally in spring, but this survey occured in the summer. 
      
    }
    
    if(!"BanIcespring" %in% surveys[i] & !yr==2020){
      #  So first thing to do is get the data for the bank.... 
      if("BanIcespring" %in% surveys[i]) stop("Took a wrong turn at the else statement for BanIce... go back to jail.")
      # If we aren't dealing with spatial data do this to get the bank and the bank data...
      if(is.null(spat.names) || !(surveys[i] %in% spat.names$label)) 
      {
        if(surveys[i] %in% all.surv.dat$surv.bank) {
          bnk <- as.character(unique(subset(all.surv.dat,surv.bank == surveys[i])$bank))
          bank.dat[[bnk]] <- subset(all.surv.dat,surv.bank==surveys[i])
          # I also want to make this bank.4.spatial object here as well as it will allow me to use this throughout the file without
          # having to add in a billion if loops
          bank.4.spatial <- bnk
        }
        
      } # end if(is.null(spat.names) || !(surveys[i] %in% spat.names$label)) 
      
      # If we are dealing with spatial data do this...
      if(!is.null(spat.names) && surveys[i] %in% spat.names$label)  
      {
        bnk <- surveys[i]
        # Here we need to figure out what survey data we want to pull so we can extract the right data from the all.surv.dat file
        # This mostly is way overcomplicated because we dont' want to pull GBa spring with GBa summer
        bank.4.spatial <- unique(spat.names$bank)
        survey.4.spatial <- unique(spat.names$survey_target)
        bank.dat[[bnk]] <- subset(all.surv.dat,surv.bank == survey.4.spatial)
      } # end if(!is.null(spat.names) && surveys[i] %in% spat.names$label)  
      
      # Since we are missing 2015 we need to do this for GB spring survey...
      if(bnk == "GB") bank.dat[[bnk]] <- subset(all.surv.dat,surv.bank == surveys[i] & year != 2015)
      
      # Get the appropriate sizes for recrutis and commercial size
      RS <- size.cats$RS[size.cats$Bank == bank.dat[[bnk]]$bank[1]]
      CS <- size.cats$CS[size.cats$Bank == bank.dat[[bnk]]$bank[1]]
      if(bnk=="BanIce") RS <- size.cats$RS[size.cats$Bank == "Ban"]
      if(bnk=="BanIce") CS <- size.cats$CS[size.cats$Bank == "Ban"]
      if(is.null(survey.year)==F) yr <- survey.year
      if(is.null(survey.year)==T) yr <- max(bank.dat[[bnk]]$year,na.rm=T)
      years <- yr.start:yr
      # Because of change in RS and CS on GB we need to have this more nuanced for GB (I originally only had this for GBa, but it's gotta be for all GB...)
      # The CS and RS specified here actually 5 higher than the actual shell heights
      # CS = Shell height for knife-edge recriutment:  
      # Correctly specifying the years here really matters since the RS and CS are changing with time,
      # begs for a better method!!
      # 1981-1985 CS = 75, RS = 60
      # From 1986-1995 CS = 85, RS= 75
      # From 1996-current CS= 95, RS = 85
      # This is the Shell height for each shell height category. If it ever changes in the future this will need adjusted.
      # If we are still using this code in 2030 I'll be both old and worried...
      if(grepl("GB",bnk))
      {
        SH.dat <- data.frame(year = 1980:2030,CS = c(rep(75,6),rep(85,10),rep(95,2030-1995)),RS = c(rep(60,6),rep(75,10),rep(85,2030-1995)))
        CS <- SH.dat$CS[SH.dat$year %in% years]
        RS <- SH.dat$RS[SH.dat$year %in% years]
      } # End if(bnk == "GBa")
      
      # Now we can set up our more detailed SHF bins as well
      if(bins == "bank_default")
      {
        bin <- c(50,70,RS[length(RS)],CS[length(CS)],120) 
      }# end if{bins == "bank_default"}
      # If you have specified the bins then do this...
      if(bins != "bank_default") bin <- bins 
      
      # Remove years in which we don't have good data for specific banks, 1984 very problematic with clappers/live data.
      # One Browns South and North we also need to be particulatr with data # NOTE: May need to add Ban year cut-off here?
      if((bnk == "Ger" || bnk == "Mid" || bnk=="Ban" ||bnk=="BanIce") && yr.start < 1985) years <- 1985:yr
      if(bnk == "Sab" && yr.start < 1986) years <- 1986:yr # The first year for Sable is causing issues with condFac model.
      # Looks like it is due to the lme part of the model predicting mw of 9.5 (rest of years more like 12-13)
      
      if(bnk == "BBs"  && yr.start < 1988) years <- c(1985,1986,1988:yr)
      if(bnk == "BBn"  && yr.start < 1991) years <- c(1991:yr)
      bank.dat[[bnk]] <- subset(bank.dat[[bnk]] , year %in% years)
      
      # Now run the survey boundary and seedboxes in here.
      # Get the  bank survey boundary polygon when we are dealing with the entire bank
      if(is.null(spat.names) || !(surveys[i] %in% spat.names$label)) 
      {
        if(bnk=="BanIce") bound.poly.surv <- subset(survey.bound.polys,label=="Ban" & startyear == max(survey.bound.polys$startyear[survey.bound.polys$label=="Ban"]))
        if(!bnk=="BanIce") bound.poly.surv <- subset(survey.bound.polys,label==bnk & startyear == max(survey.bound.polys$startyear[survey.bound.polys$label==bnk]))
        attr(bound.poly.surv,"projection")<-"LL"
        
        #Read4 Read drooped #Detailed Survey polygons
        detail.poly.surv <- subset(survey.detail.polys,label==bnk)
        attr(detail.poly.surv,"projection")<-"LL"
        
        # Get the strata areas.
        strata.areas <- subset(survey.info,label==bnk,select =c("Strata_ID","towable_area","startyear"))
        #Read25 read removed... Get all the details of the survey strata
        surv.info <- subset(survey.info,label== bnk)
      } # end if(is.null(spat.names) || !(surveys[i] %in% spat.names$label))
      
      # If we are dealing with a spatial subset we need to do some fancy dancy-ness
      if(!is.null(spat.names) && surveys[i] %in% spat.names$label)  
      {
        # Get the boundary for the full area and the detailed survey data
        full.bound.poly <- as.PolySet(survey.bound.polys[survey.bound.polys$label == bank.4.spatial ,],projection="LL")
        full.bound.poly <- full.bound.poly[full.bound.poly$startyear == max(full.bound.poly$startyear,na.rm=T) ,]
        full.detail <- as.PolySet(survey.detail.polys[survey.detail.polys$label == bank.4.spatial,],projection="LL")
        full.detail <- full.detail[full.detail$startyear == max(full.detail$startyear,na.rm=T) ,]
        # I also want the full surv info object as there are some pieces I want to pull out of this.
        full.area.surv.info <- subset(survey.info,label== bank.4.spatial)
        # Get the boundary for our subset area.
        spat.bound <- as.PolySet(newAreaPolys[newAreaPolys$label == bnk,],projection = "LL")
        # Now clip out the boundary area and the detailed polygons
        bound.poly.surv <- joinPolys(full.bound.poly,spat.bound,"INT")
        detail.poly.surv <- joinPolys(full.detail,spat.bound,"INT")
        # Add back in the information that joinPolys strips away.
        detail.poly.surv$label <- full.detail$label[1]
        bound.poly.surv$label <- full.bound.poly$label[1]
        detail.poly.surv$Strata_ID <- detail.poly.surv$PID
        detail.poly.surv$startyear <- full.detail$startyear[1]
        bound.poly.surv$startyear <- full.bound.poly$startyear[1]
        
        # These are the strata ID's for a given sub-area, sweet
        strata.IDs <- unique(detail.poly.surv$PID)
        # calculate the area of the spatial subset
        
        area_km2 <- calcArea(detail.poly.surv,rollup=1)$area
        n.strata <- length(area_km2)
        # Now make a data frame and make sure it works if we have multiple start years, picking the most recent data may/may not be ideal
        surv.info <- full.area.surv.info[full.area.surv.info$Strata_ID %in% strata.IDs & 
                                           full.area.surv.info$startyear == max(full.area.surv.info$startyear),]
        # And let's replace the values with the proper values
        surv.info$area_km2 <- area_km2
        surv.info$towable_area <- surv.info$area_km2*1000*1000/800/(8/3.2808)
        #surv.info$PID <- 1:nrow(surv.info)
        #names(surv.info) <- names(full.area.survy.info)
        # subset to the strata areas.
        strata.areas <- subset(surv.info,select =c("Strata_ID","towable_area","startyear"))                                                               
      }
      # Save the survey strata table so we have it for later, this is mostly needed for when we have user defined areas carved out.
      survey.strata.table[[bnk]] <- surv.info
      detail.surv.poly[[bnk]] <- detail.poly.surv
      bound.surv.poly[[bnk]]<- bound.poly.surv
      
      # Give each tow a unique identifier.
      bank.dat[[bnk]]$ID<-paste(bank.dat[[bnk]]$year,bank.dat[[bnk]]$tow,sep='.')
      
      # Add 200 to the tow numbers on BBn for the years 1991:2000 to get the numbering to match. 
      # 2015 entered into the db as 1-100 as well. And beware of extras!
      if(bnk == "BBn")  bank.dat[[bnk]]$tow[bank.dat[[bnk]]$year %in% c(1991:2000, 2015) & bank.dat[[bnk]]$tow < 200] <- 
        bank.dat[[bnk]]$tow[bank.dat[[bnk]]$year %in% c(1991:2000, 2015) & bank.dat[[bnk]]$tow < 200] + 200
      
      # Assign the strat based on location, the "new_strata" column is used for processing later on in the function so I've retained it.
      # We used to write to the screen what percentage of strata were reassigned. But the strata are now entered as NULL so it'll always be 100%
      # German and Middle bank have no stratifcation scheme and we don't do this for GB spring which is fixed stations.
      # Sable has to be handled differently so that we assign the old and new strata (pre and post WEBCA). Note that since Sable has been restratified, some tows are now outside of the strata bounds and marked as NA.
      if(bnk == "Sab") 
      {
        strata.years <- unique(detail.poly.surv[detail.poly.surv$label==bnk,]$startyear)
        nrestrat <- length(strata.years)
        
        #this handles one restratification only...
        oldscheme <- assign.strata(bank.dat[[bnk]], detail.poly.surv[!detail.poly.surv$startyear == strata.years[nrestrat],])
        newscheme <- assign.strata(bank.dat[[bnk]], detail.poly.surv[detail.poly.surv$startyear == strata.years[nrestrat],])
        
        names(newscheme)[dim(newscheme)[2]] <- "Strata_ID_new"
        names(oldscheme)[dim(oldscheme)[2]] <- "Strata_ID_old"
        
        # bank.dat[[bnk]] <- rbind(oldscheme[oldscheme$year < strata.years[nrestrat],], newscheme[newscheme$year == strata.years[nrestrat]|newscheme$year > strata.years[nrestrat],])
        bank.dat[[bnk]] <- cbind(newscheme, data.frame(Strata_ID_old=oldscheme[,dim(oldscheme)[2]]))
      } # end if(bnk == "Sab") 
      
      if(bnk != "Ger" && bnk != "Mid"  && bnk != "GB" && bnk!= "Sab" && bnk!= "Ban" && bnk!="BanIce") bank.dat[[bnk]] <- assign.strata(bank.dat[[bnk]],detail.poly.surv)
      # above assigns strata to each tow. 
      
      print("assign.strata done")
      
      # MEAT WEIGHT DATA from 2011-current Get the mw data from 2011 to this year, this is if we aren't doing any spatial subsetting
      if(is.null(spat.names) || !(surveys[i] %in% spat.names$label)) 
      {
        if(bnk != "GB" && bnk != "GBa" && bnk != "GBb") mw[[bnk]] <- subset(MW.dat.new,bank==bnk)
        # GB spring in 2015 didn't have a true survey so it shouldn't be included.
        if(bnk=="GB")              mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GB","GBa","GBb") & month < 7)
        if(bnk=="GBa")             mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GBa") & month > 7)
        if(bnk=="GBb")             mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GBb") & month > 7)
      } # end  if(is.null(spat.names) || !(surveys[i] %in% spat.names$label)) 
      # Now if we have spatial meat weight data we need to subset it...
      # If we are dealing with a spatial subset we need to do some fancy dancy-ness
      if(!is.null(spat.names) && surveys[i] %in% spat.names$label)  
      {
        if(bank.4.spatial != "GB" && bank.4.spatial != "GBa" && bank.4.spatial != "GBb") mw[[bnk]] <- subset(MW.dat.new,bank==bank.4.spatial)
        # GB spring in 2015 didn't have a true survey so it shouldn't be included.
        if(bank.4.spatial=="GB")              mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GB","GBa","GBb") & month < 7)
        if(bank.4.spatial=="GBa")             mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GBa") & month > 7)
        if(bank.4.spatial=="GBb")             mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GBb") & month > 7)
        # now we need to remove all data outside our domain of interest and get the data in the same projection...
        coordinates(mw[[bnk]])<- ~ lon+lat
        proj4string(mw[[bnk]]) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        spat.bound.sp <- PolySet2SpatialPolygons(spat.bound)
        spat.bound.sp <- spTransform(spat.bound.sp,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
        locs <- over(mw[[bnk]],spat.bound.sp)
        mw[[bnk]] <- mw[[bnk]][which(locs == 1),]
        # I really just want the data back out, not all the jazzy spatial info, later I might, but we're not there yet...
        mw[[bnk]] <- cbind(mw[[bnk]]@data,mw[[bnk]]@coords)
      } # end  if(!is.null(spat.names) && surveys[i] %in% spat.names$label)  
      
      # For the most recent data
      mw.dm <- na.omit(subset(mw[[bnk]],year==yr))
      mw.dm$sh<-mw.dm$sh/100
      # MODEL - This is the meat weight Shell height realationship.  
      #MEAT WEIGHT SHELL HEIGHT RELATIONSHIP in current year 
      #Source5 source("fn/shwt.lme.r") note that the exponent is set as a parameter here b=3
      SpatHtWt.fit[[bnk]] <- shwt.lme(mw.dm,random.effect='tow',b.par=3)
      print("shwt.lme done")
      
      print("NEED TO REVISE import.hyd.data yrs and tow number corrections everytime more historical data is added to database. We need to investigate potential duplication?!")
      
      # just in case there are still remnant Commercial sampling station 0 tows that we need to get rid of (e.g. if you have pre-processed = T and are using an RData created before Jan2019 edits):
      if(commercialsampling==F) MW.dat <- MW.dat[!MW.dat$tow==0,]
      
      # here we are putting the MW hydration sampling from 2010 and earlier together with the data since 2010 and 
      # then we export it as a csv. NOTE: FK added Ban here
      if(bank.4.spatial %in% c("Mid", "Ban", "BanIce")) 
      {
        # MEAT WEIGHT DATA - hydration sampling, This will vary for certain banks, 
        # Create and export a MW-SH object
        if(bank.4.spatial == "Mid") {
          mw.dat.all[[bnk]] <- merge(
            subset(MW.dat,bank==bank.4.spatial & month %in% 5:6 & year > 1983,
                   c("tow","year","lon","lat","depth","sh","wmw")),
            subset(mw[[bnk]],select=c("tow","year","lon","lat","depth","sh","wmw")),all=T)
          if(commercialsampling==F) mw.dat.all[[bnk]] <- subset(mw.dat.all[[bnk]], tow>0)
        }
        if(bank.4.spatial %in% c("Ban", "BanIce")) {
          mw.dat.all[[bnk]] <- merge(
            subset(MW.dat,bank==bank.4.spatial & month %in% 4:6 & year > 1983,
                   c("tow","year","lon","lat","depth","sh","wmw")),
            subset(mw[[bnk]],select=c("tow","year","lon","lat","depth","sh","wmw")),all=T)
          if(commercialsampling==F) mw.dat.all[[bnk]] <- subset(mw.dat.all[[bnk]], tow>0)
        }
        
        mw.dat.all[[bnk]]$ID<-paste(mw.dat.all[[bnk]]$year,mw.dat.all[[bnk]]$tow,sep='.')
        
        # write this now for these banks
        write.csv(mw.dat.all[[bnk]],paste(direct,"Data/Survey_data/",yr,"/Spring/",bank.4.spatial,"/mw_Data.csv",sep=""),row.names=F) # Write1
        ## MODEL - This is the model used to esimate condition factor across Middle Bank
        #Source6 source("fn/condFac.r")
        # Due to the sparseness of the data for this bank the most complex model we can fit is a gam_d, 
        # data this is like far more complex still than the really allows for.
        # June 2016, I changed this to the glm model, the gam_d model seems to overestimate CF on the bank 
        
        cf.data[[bnk]]<-condFac(mw.dat.all[[bnk]],bank.dat[[bnk]],model.type='glm',dirct=direct)
        
        if(mwsh.test == T) {
          browser()
          source(paste0(direct_fns, "Survey_and_OSAC/mwsh.sensit.R"))
          mwshtest <- mwsh.sensit(mwdat=na.omit(mw.dat.all[[bnk]]), shfdat=bank.dat[[bnk]], bank=bnk, plot=F, 
                                  sub.size=NULL, sub.year=NULL, sub.tows=NULL, sub.samples=NULL, 
                                  direct=direct, seed=1234, direct_fns=direct_fns)
          cf.data[[bnk]] <- mwshtest$condmod
        }
      } # end if(bnk %in% c("Mid", "Ban"))
      
      if(!bank.4.spatial %in% c("Mid", "Ban", "BanIce"))
      {
        if(bank.4.spatial != "GB") mw.tmp <- subset(MW.dat,bank == bank.4.spatial)
        if(bank.4.spatial == "GB") mw.tmp <- subset(MW.dat,bank %in% c("GB","GBa","GBb"))
        mw.tmp$ID <- paste(mw.tmp$year,mw.tmp$tow,sep='.')
        # Grab the relavent Meat-Weight Shell height data and make a flat file from it
        if(bank.4.spatial %in% c("BBn","Ger","Sab","BBs","GB") & !yr == 2020) 
        {
          mw.dat.all[[bnk]] <- merge(
            subset(mw.tmp, 
                   month %in% 5:6 & year %in% years,
                   c("ID","year","lon","lat","depth","sh","wmw","tow")),
            subset(mw[[bnk]],select=c("ID","year","lon","lat","depth","sh","wmw","tow")),
            all=T)
          if(commercialsampling==F) mw.dat.all[[bnk]] <- subset(mw.dat.all[[bnk]], tow>0)
        }
        
        # Grab the relavent Meat-Weight Shell height data for the summer 
        if(bank.4.spatial %in% c("GBa","GBb")) 
        {
          mw.dat.all[[bnk]] <- merge(
            subset(mw.tmp, 
                   month > 7 & year %in% years,
                   c("ID","year","lon","lat","depth","sh","wmw","tow")),
            subset(mw[[bnk]],select=c("ID","year","lon","lat","depth","sh","wmw","tow")),
            all=T)
          if(commercialsampling==F) mw.dat.all[[bnk]] <- subset(mw.dat.all[[bnk]], tow>0)
        }
        
      } # end if(bnk == "Sab" | bnk == "Ger") 
      #		mw.dat.all[[bnk]] <- subset(mw.dat.all[[bnk]], year != 2015)
      
      ## MODEL - This is the model used to esimate condition factor across the bank for all banks but Middle/Ban
      if(!bank.4.spatial %in% c("Mid", "Ban", "BanIce")) 
      {
        # Note that I was getting singular convergence issues for the below sub-area so I simplified the model...
        if(bnk == "GBa-Large_core")  cf.data[[bnk]] <- condFac(na.omit(mw.dat.all[[bnk]]),bank.dat[[bnk]],model.type='glm',dirct=direct)
        if(bnk != "GBa-Large_core")  cf.data[[bnk]] <- condFac(na.omit(mw.dat.all[[bnk]]),bank.dat[[bnk]],model.type='gam_f',dirct=direct)
      }
      
      if(mwsh.test == T) {
        
        browser()
        source(paste0(direct_fns, "Survey_and_OSAC/mwsh.sensit.R"))
        mwshtest <- mwsh.sensit(mwdat=na.omit(mw.dat.all[[bnk]]), shfdat=bank.dat[[bnk]], bank=bnk, plot=F, 
                                sub.size=NULL, sub.year=NULL, sub.tows=NULL, sub.samples=NULL, 
                                direct=direct, seed=1234, direct_fns=direct_fns)
        cf.data[[bnk]] <- mwshtest$condmod
      }
      
      # Because of the lined survey on German we want to differentiate between the lined and unlined CF data
      if(bank.4.spatial == "Ger")
      {
        # In case R decides to treat year as a factor....
        if(class(cf.data[[bnk]]$CFyrs$year) == 'factor') cf.data[[bnk]]$CFyrs$year <- as.numeric(levels(cf.data[[bnk]]$CFyrs$year))[cf.data[[bnk]]$CFyrs$year]
        if(class(cf.data[[bnk]]$CFyrs$year) != 'factor') cf.data[[bnk]]$CFyrs$year <- as.numeric(cf.data[[bnk]]$CFyrs$year)
        cf.data[[bnk]]$CFyrs$CF2[cf.data[[bnk]]$CFyrs$year > 2007] <- cf.data[[bnk]]$CFyrs$CF[cf.data[[bnk]]$CFyrs$year>2007]
        cf.data[[bnk]]$CFyrs$CF[cf.data[[bnk]]$CFyrs$year > 2007] <- NA
      } # end if(bnk == "Ger")
      
      print("condFac done")
      
      # Fill the years without any data with NA's (this helps with plotting data.). Appended NA rows are fine (no need to be interspersed)
      if(mwsh.test == F) cf.data[[bnk]]$CFyrs <-merge(cf.data[[bnk]]$CFyrs,data.frame(year=1983:yr),all=T)
      
      # Output the predictions for the bank
      surv.dat[[bnk]] <- cf.data[[bnk]]$pred.dat
      
      # Pull out the ID and condition factor
      tmp.dat<-subset(cf.data[[bnk]]$CF.data,select=c("ID","CF"))
      # Rename CF to CFh
      names(tmp.dat)[2]<-"CFh"
      # merge the two data sets, keeping all x values
      surv.dat[[bnk]]<-merge(surv.dat[[bnk]],tmp.dat,all.x=T)
      # Replace any NA's in CFh with the original Condition Factor.
      surv.dat[[bnk]]$CFh[is.na(surv.dat[[bnk]]$CFh)]<-surv.dat[[bnk]]$CF[is.na(surv.dat[[bnk]]$CFh)]
      
      # Calculate the biomass of the Pre-recruits, Recruits and the Commerical Scallops in each tow on the bank
      # Here we have added the ability to calculate the biomass of specific bins of interest. Also for
      # GBa note that b/c of changes in sizes over time the user specified bins won't 
      # necessarily add up to the pre,rec, or com totals as the bins won't necessaryily be the same thing
      # e.g. in 1984 RS =60 and CS=75, so the pre's will be < 60 while the user specified bins are constrained
      # to use just the current RS size (unless of course you specify something yourself).
      #Source7 source("...surv.by.tow.r") surv.by.tow calculates number or biomass of pre, rec and com size scallops in each tow
      if(bank.4.spatial %in% c("Ban", "BanIce", "Mid","Ger","BBn","GB","GBa","GBb")) 
      {
        surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS,type = "ALL",mw.par = "CF",user.bins = bin)
        #surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS, type='B', mw.par="CF")
      } # end if(bnk %in% c("Mid","Ger","BBn","GB","GBa","GBb"))
      
      if(bank.4.spatial == "Sab" || bnk == "BBs" ) 
      {
        surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS,type="ALL",mw.par ="CFh",user.bins = bin)
        #surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS, type='B', mw.par="CFh")
      } # end if(bnk == "Sab" || bnk == "BBs" ) 
      
      print("surv.by.tow done")
      
      # On Georges spring we need to tidy up some of the randoms..
      if(bank.4.spatial == "GB")
      {
        surv.dat[[bnk]]$random[surv.dat[[bnk]]$year < 2013] <-4
        surv.dat[[bnk]]$random[surv.dat[[bnk]]$tow %in% c(1:24,301:324)] <-3
        surv.dat[[bnk]]$random[surv.dat[[bnk]]$tow > 12 & surv.dat[[bnk]]$year == 1988] <- 4
      } # end if(bnk == "GB")
      
      # Subset the data into the clappers (dead) and live scallops.  Use only random survey tows for Clappers...
      # For GB spring the survey of interest are the comparative tows...
      surv.Clap[[bnk]]<-subset(surv.dat[[bnk]],state=='dead')
      surv.Live[[bnk]]<-subset(surv.dat[[bnk]],state=='live')
      if(bank.4.spatial != "GB") surv.Rand[[bnk]]<-subset(surv.dat[[bnk]],state=='live' & random==1)		
      if(bank.4.spatial != "GB") surv.Clap.Rand[[bnk]]<-subset(surv.dat[[bnk]],state=='dead'& random==1)
      if(bank.4.spatial == "GB") surv.Clap.Rand[[bnk]] <- subset(surv.dat[[bnk]],state=='dead'& random==3)
      if(bank.4.spatial == "GB") surv.Rand[[bnk]]<-subset(surv.dat[[bnk]],state=='live' & random==3)	
      #########  Now calculate the clappers...
      
      # Clappers the banks for each size class
      surv.Clap.Rand[[bnk]]$clap.prop<-surv.Clap.Rand[[bnk]]$tot/(surv.Rand[[bnk]]$tot+surv.Clap.Rand[[bnk]]$tot)*100
      surv.Clap.Rand[[bnk]]$clap.prop[is.na(surv.Clap.Rand[[bnk]]$clap.prop)]<-0
      surv.Clap.Rand[[bnk]]$clap.propCom<-surv.Clap.Rand[[bnk]]$com/(surv.Rand[[bnk]]$com+surv.Clap.Rand[[bnk]]$com)*100
      surv.Clap.Rand[[bnk]]$clap.propCom[is.na(surv.Clap.Rand[[bnk]]$clap.propCom)]<-0
      surv.Clap.Rand[[bnk]]$clap.propRec<-surv.Clap.Rand[[bnk]]$rec/(surv.Rand[[bnk]]$rec+surv.Clap.Rand[[bnk]]$rec)*100
      surv.Clap.Rand[[bnk]]$clap.propRec[is.na(surv.Clap.Rand[[bnk]]$clap.propRec)]<-0
      surv.Clap.Rand[[bnk]]$clap.propPre<-surv.Clap.Rand[[bnk]]$pre/(surv.Rand[[bnk]]$pre+surv.Clap.Rand[[bnk]]$pre)*100
      surv.Clap.Rand[[bnk]]$clap.propPre[is.na(surv.Clap.Rand[[bnk]]$clap.propPre)]<-0
      surv.Clap.Rand[[bnk]]$clap.prop[is.na(surv.Clap.Rand[[bnk]]$clap.prop)]<-0
      # Do the same for all the tows...
      surv.Clap[[bnk]]$clap.prop<-surv.Clap[[bnk]]$tot/(surv.Live[[bnk]]$tot+surv.Clap[[bnk]]$tot)*100
      surv.Clap[[bnk]]$clap.prop[is.na(surv.Clap[[bnk]]$clap.prop)]<-0
      surv.Clap[[bnk]]$clap.propCom<-surv.Clap[[bnk]]$com/(surv.Live[[bnk]]$com+surv.Clap[[bnk]]$com)*100
      surv.Clap[[bnk]]$clap.propCom[is.na(surv.Clap[[bnk]]$clap.propCom)]<-0
      surv.Clap[[bnk]]$clap.propRec<-surv.Clap[[bnk]]$rec/(surv.Live[[bnk]]$rec+surv.Clap[[bnk]]$rec)*100
      surv.Clap[[bnk]]$clap.propRec[is.na(surv.Clap[[bnk]]$clap.propRec)]<-0
      surv.Clap[[bnk]]$clap.propPre<-surv.Clap[[bnk]]$pre/(surv.Live[[bnk]]$pre+surv.Clap[[bnk]]$pre)*100
      surv.Clap[[bnk]]$clap.propPre[is.na(surv.Clap[[bnk]]$clap.propPre)]<-0
      surv.Clap[[bnk]]$clap.prop[is.na(surv.Clap[[bnk]]$clap.prop)]<-0
      
      
      # Using the Live scallops only make the Middle Bank survey object
      # Simple survey updated to enable the caluclation for user specified sH bins.
      # Now we can subset the clappers, randome, live and surv dat objects, once we do this I think we are golden...
      
      if(!is.null(spat.names) && surveys[i] %in% spat.names$label)  
      {
        # now we need to remove all data outside our domain of interest and get the data in the same projection...
        coordinates(surv.Clap[[bnk]])<- ~ lon+lat
        proj4string(surv.Clap[[bnk]]) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        coordinates(surv.Live[[bnk]])<- ~ lon+lat
        proj4string(surv.Live[[bnk]]) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        coordinates(surv.Rand[[bnk]])<- ~ lon+lat
        proj4string(surv.Rand[[bnk]]) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        coordinates(surv.dat[[bnk]])<- ~ lon+lat
        proj4string(surv.dat[[bnk]]) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        
        # spat.bound.sp would have been created above...
        locs.c <- over(surv.Clap[[bnk]],spat.bound.sp)
        locs.l <- over(surv.Live[[bnk]],spat.bound.sp)
        locs.r <- over(surv.Rand[[bnk]],spat.bound.sp)
        locs.d <- over(surv.dat[[bnk]],spat.bound.sp)
        
        # No cut out the survey data for each...
        surv.Clap[[bnk]] <- surv.Clap[[bnk]][which(locs.c == 1),]
        surv.Live[[bnk]] <- surv.Live[[bnk]][which(locs.l == 1),]
        surv.Rand[[bnk]] <- surv.Rand[[bnk]][which(locs.r == 1),]
        # For some reason surv.dat isn't working...???
        #tmp <- surv.dat[[bnk]][which(locs.d == 1),]
        
        # I really just want the data back out, not all the jazzy spatial info, later I might, but we're not there yet...
        surv.Clap[[bnk]] <- cbind(surv.Clap[[bnk]]@data,surv.Clap[[bnk]]@coords)
        surv.Live[[bnk]] <- cbind(surv.Live[[bnk]]@data,surv.Live[[bnk]]@coords)
        surv.Rand[[bnk]] <- cbind(surv.Rand[[bnk]]@data,surv.Rand[[bnk]]@coords)
        #surv.dat[[bnk]] <- cbind(tmp@data,tmp@coords)
      } # end  if(!is.null(spat.names) && surveys[i] %in% spat.names$label)  
      
      
      #Source15 source("fn/simple.surv.r") prepare survey index data obj
      if(bank.4.spatial %in% c("Mid", "Ban", "BanIce")) 
      {
        survey.obj[[bnk]] <- simple.surv(surv.Live[[bnk]][surv.Live[[bnk]]$random==1,],years=years,user.bins=bin)
        survey.obj[[bnk]][[1]]$CF <- sapply(1:length(years),
                                            function(x){with(subset(surv.Live[[bnk]][surv.Live[[bnk]]$random==1,],year == years[x]),
                                                             weighted.mean(CF,com.bm,na.rm=T))})
        if(bank.4.spatial == "Mid") clap.survey.obj[[bnk]]<-simple.surv(surv.Clap.Rand[[bnk]][surv.Clap.Rand[[bnk]]$random==1,],years=years)
        if(bank.4.spatial %in% c("Ban", "BanIce")) message("Using surv.Clap instead of surv.Clap.Rand for Ban"); clap.survey.obj[[bnk]]<-simple.surv(surv.Clap[[bnk]][surv.Clap.Rand[[bnk]]$random==1,],years=years)
      } #end 	if(bnk == "Mid")  
      # And here is Georges Bank spring survey results
      if(bank.4.spatial == "GB")  
      {
        # Only look at the repeated tows for GB!!
        survey.obj[[bnk]] <- simple.surv(subset(surv.Rand[[bnk]], random %in% c(1,3)),years=years,user.bins=bin)
        survey.obj[[bnk]][[1]]$CF <- sapply(1:length(years),
                                            function(x){with(subset(surv.Rand[[bnk]],year == years[x]),
                                                             weighted.mean(CF,com.bm,na.rm=T))})
        clap.survey.obj[[bnk]]<-simple.surv(subset(surv.Clap.Rand[[bnk]],random %in% c(1,3)), years=years)
      } # end 	if(bnk == "GB")  
      
      print("simple.surv for non-German banks done")
      
      # For German bank we have the matched survey design which leads to some unique processing to get survey results.
      if(bank.4.spatial == "Ger")
      {
        
        # Survey results
        # On German we have a repeated measures survey design (we repeat stations each year) 
        # and in 2008 there were both lined and unlined tows, it's all very comnfusing, but tow ID's of
        # 451 and greater were all lined, less than 451 were unlined...
        lined.dat<-rbind(subset(surv.Live[[bnk]],year==2008 & tow >=451),subset(surv.Live[[bnk]],year > 2008))
        surv.Clap.Rand[[bnk]]<-rbind(subset(surv.Clap.Rand[[bnk]],year==2008 & tow >=451),subset(surv.Clap.Rand[[bnk]],year > 2008))
        ### WHICH TOWS ARE MATCHED WITH WHICH!!!!
        #Some funky code to get the matched tows...
        ger.tows <- NULL
        ger.years <- 2009:yr # Pick the years for which we've had repeated tows.
        for(b in 1:length(ger.years))
        {
          # Get the tows for the current year.
          new.ger.tows <- subset(surv.Live[[bnk]], year == ger.years[b],c("tow","slon","slat","lat","lon","elon","elat","random","year"))
          # Get the EID's into the correct format...
          new.ger.tows$EID <- 1:nrow(new.ger.tows)
          # Now get the tows for the previous year, in 2009 we select only the lined tows from the 2008 survey (this is what was done in the past).
          if(ger.years[b] == 2009) last.ger.tows <- subset(surv.Live[[bnk]], year==ger.years[b]-1 & tow >= 451)
          if(ger.years[b] > 2009) last.ger.tows <- subset(surv.Live[[bnk]], year==ger.years[b]-1)
          # Now get all the tows that appear to overlap and they are our matched tows, search on end lat/lon, then start lat/lon
          # and finish with mean lat/lon, the mean lat/lon is the final search criteria so overwrites the other two which I think makes the most sense
          # as you could start/finish in opposite directions but you should be closest in the middle so the mean lat/lon should give the best results
          # The other two are helpful for filling in a few odds and ends that don't match up.
          # This seems to do the trick, in 2010 there is one multiple match but it retains the correct tow so we're all good.
          for(k in 1:nrow(new.ger.tows))
          {
            if(any(floor(last.ger.tows$elat*100) == floor(new.ger.tows$elat[k]*100) & 
                   floor(last.ger.tows$elon*100) == floor(new.ger.tows$elon[k]*100)))
              new.ger.tows$EID[k] <- last.ger.tows$tow[floor(last.ger.tows$elat*100) == floor(new.ger.tows$elat[k]*100) & 
                                                         floor(last.ger.tows$elon*100) == floor(new.ger.tows$elon[k]*100)]
            if(any(ceiling(last.ger.tows$elat*100) == ceiling(new.ger.tows$elat[k]*100) & 
                   ceiling(last.ger.tows$elon*100) == ceiling(new.ger.tows$elon[k]*100)))
              new.ger.tows$EID[k] <- last.ger.tows$tow[ceiling(last.ger.tows$elat*100) == ceiling(new.ger.tows$elat[k]*100) & 
                                                         ceiling(last.ger.tows$elon*100) == ceiling(new.ger.tows$elon[k]*100)]
            if(any(round(last.ger.tows$elat,digits=2) == round(new.ger.tows$elat[k],digits=2) & 
                   round(last.ger.tows$elon,digits=2) == round(new.ger.tows$elon[k],digits=2)))
              new.ger.tows$EID[k] <- last.ger.tows$tow[round(last.ger.tows$elat,digits=2) == round(new.ger.tows$elat[k],digits=2) & 
                                                         round(last.ger.tows$elon,digits=2) == round(new.ger.tows$elon[k],digits=2)]
            if(any(floor(last.ger.tows$slat*100) == floor(new.ger.tows$slat[k]*100) & 
                   floor(last.ger.tows$slon*100) == floor(new.ger.tows$slon[k]*100)))
              new.ger.tows$EID[k] <- last.ger.tows$tow[floor(last.ger.tows$slat*100) == floor(new.ger.tows$slat[k]*100) & 
                                                         floor(last.ger.tows$slon*100) == floor(new.ger.tows$slon[k]*100)]
            if(any(ceiling(last.ger.tows$slat*100) == ceiling(new.ger.tows$slat[k]*100) & 
                   ceiling(last.ger.tows$slon*100) == ceiling(new.ger.tows$slon[k]*100)))
              new.ger.tows$EID[k] <- last.ger.tows$tow[ceiling(last.ger.tows$slat*100) == ceiling(new.ger.tows$slat[k]*100) & 
                                                         ceiling(last.ger.tows$slon*100) == ceiling(new.ger.tows$slon[k]*100)]
            if(any(round(last.ger.tows$slat,digits=2) == round(new.ger.tows$slat[k],digits=2) & 
                   round(last.ger.tows$slon,digits=2) == round(new.ger.tows$slon[k],digits=2)))
              new.ger.tows$EID[k] <- last.ger.tows$tow[round(last.ger.tows$slat,digits=2) == round(new.ger.tows$slat[k],digits=2) & 
                                                         round(last.ger.tows$slon,digits=2) == round(new.ger.tows$slon[k],digits=2)]
            if(any(floor(last.ger.tows$lat*100) == floor(new.ger.tows$lat[k]*100) & 
                   floor(last.ger.tows$lon*100) == floor(new.ger.tows$lon[k]*100)))
              new.ger.tows$EID[k] <- last.ger.tows$tow[floor(last.ger.tows$lat*100) == floor(new.ger.tows$lat[k]*100) & 
                                                         floor(last.ger.tows$lon*100) == floor(new.ger.tows$lon[k]*100)]
            if(any(ceiling(last.ger.tows$lat*100) == ceiling(new.ger.tows$lat[k]*100) & 
                   ceiling(last.ger.tows$lon*100) == ceiling(new.ger.tows$lon[k]*100)))
              new.ger.tows$EID[k] <- last.ger.tows$tow[ceiling(last.ger.tows$lat*100) == ceiling(new.ger.tows$lat[k]*100) & 
                                                         ceiling(last.ger.tows$lon*100) == ceiling(new.ger.tows$lon[k]*100)]
            if(any(round(last.ger.tows$lat,digits=2) == round(new.ger.tows$lat[k],digits=2) & 
                   round(last.ger.tows$lon,digits=2) == round(new.ger.tows$lon[k],digits=2)))
              new.ger.tows$EID[k] <- last.ger.tows$tow[round(last.ger.tows$lat,digits=2) == round(new.ger.tows$lat[k],digits=2) & 
                                                         round(last.ger.tows$lon,digits=2) == round(new.ger.tows$lon[k],digits=2)]
          } # end for(k in 1:nrow(new.ger.tows))
          
          # Now this won't be perfect, should get most but not all of them so check the results over.
          # In 2013 we aren't seeing the match from 2012 for two tows so I've selected the matched tows by hand.
          if(ger.years[b] == 2013)
          {
            new.ger.tows$EID[which(new.ger.tows$tow == 451)] <- 426 # 425 also a good candidate but this does the trick.
            new.ger.tows$EID[which(new.ger.tows$tow == 472)] <- 464 # Best match I can see, the lat is off a bit but depth and lon both very close.
          } # end if(ger.years[b] == 2013)
          
          # In 2017 tow 432 was not a good repeat thus we are treating it as an exploratory tow only and it is excluded from this analysis..
          if(ger.years[b] == 2017) new.ger.tows <- new.ger.tows[new.ger.tows$tow != 432,]
          # Now replace all the ones that were not matched tows with a small number.
          new.ger.tows$EID[new.ger.tows$random == 1] <- 1:length(new.ger.tows$EID[new.ger.tows$random==1] )
          new.ger.tows$stratum <- new.ger.tows$random
          # the the sprSurv function below we need the "strautm" of the repeat tows to be set to 2.
          new.ger.tows$stratum[new.ger.tows$stratum == 3] <- 2
          new.ger.tows <- subset(new.ger.tows,select = c("tow","EID","lon","lat","stratum","year"))
          lined.dat$stratum[which(lined.dat$year == ger.years[b])] <- new.ger.tows$stratum
          # Update the list with the new matched tows
          ger.tows <- rbind(ger.tows,new.ger.tows)
        } # end for(b in 1:length(ger.years))
        
        # All the 2008 lined tows are given a stratum of 2, because that's how it was done...
        lined.dat$stratum[which(lined.dat$year == 2008)] <- 2
        # This gets us the overall estimates for the bank, but it doesn't get the shell height frequency data we need
        # but in 2011 the survey design was not set up for this so we'll need to grab that data from the lined.survey.obj
        # If this gives you NA's (for any year other than 2011) than you have something wrong in the tow list selection.
        spr.survey.obj <- sprSurv(lined.dat[-which(lined.dat$random %in% c(2,4,5)),],2008:yr,ger.tows,chng=T,user.bins=bin)
        
        print("sprSurv done")
        
        # prepare survey index data obj
        #Source15 source("fn/simple.surv.r")
        unlined.dat<-rbind(subset(surv.Live[[bnk]],year<2007),subset(surv.Live[[bnk]],year == 2007 & tow <= 431),
                           subset(surv.Live[[bnk]],year == 2008 & tow < 451))
        # Going to use the unlined survey as the survey.obj, the matched/lined survey specialness will get it's own names
        survey.obj[[bnk]]<-simple.surv(unlined.dat,years=1983:2008,user.bins=bin)
        # The lined survey object is only used for 2011 and also is used for the SHF plots since the spr.survey.obj does not 
        # recover the SHF's.
        lined.survey.obj<-simple.surv(lined.dat[-which(lined.dat$random %in% c(2,4,5)),],years=2008:yr,user.bins=bin)
        clap.survey.obj[[bnk]]<-simple.surv(surv.Clap.Rand[[bnk]],years=2008:yr,user.bins=bin)
        
        print("simple.surv for German done")
        
        # The total lined survey object, in 2011 it seems we didn't do repeat tows. This is the object that should be used
        # to look at time series for Germaan as it has the properly caluclated data tied together
        # But it doesn't have the SHF type of data so anything using the SHF data has to use either lined.surve.obj (since 2008)
        # or the survey.obj (before 2008).
        merged.survey.obj<-merge(subset(spr.survey.obj$out.obj[[1]],year!=2011),subset(lined.survey.obj[[1]],year==2011),all=T)
        
        # matched survey results.
        matched.tows<-rbind(subset(surv.Live[[bnk]], year == (yr-1) & tow %in% spr.survey.obj$out.obj[[2]]$tow.y1),
                            subset(surv.Live[[bnk]], year == yr & tow %in% spr.survey.obj$out.obj[[2]]$tow.y2))
        matched.survey.obj<-simple.surv(matched.tows, years=(yr-1):yr,user.bins=bin)
        # This is used below to generate summarys of the survey data on the bank for the most recent year.
        SS.summary[[bnk]] <- merged.survey.obj
        SS.summary[[bnk]]$bank <- bank.4.spatial
        SHF.summary[[bnk]] <- as.data.frame(cbind(lined.survey.obj[[1]]$year,lined.survey.obj[[2]]$n.yst))
        SHF.summary[[bnk]]$bank <- bank.4.spatial
        # Add in the RS and CS to the "model.dat" bit of the lined survey object which I'm using for the breakdown plot later.
        lined.survey.obj$model.dat$CS <- CS
        lined.survey.obj$model.dat$RS <- RS
      }# end if(bnk == "Ger")
      
      # Get the survey estimates for the banks for which we have strata. 
      if(bank.4.spatial != "Ger" && bank.4.spatial != "Mid" && bank.4.spatial != "GB" && bank.4.spatial != "Ban"  && bank.4.spatial != "BanIce") 
      {
        
        if(bank.4.spatial=="Sab")  
        {
          survey.obj[[bnk]] <- survey.dat.restrat(shf=surv.Rand[[bnk]], RS=RS, CS=CS, #RS=80 CS=90
                                                  bk=bank.4.spatial, areas=strata.areas, mw.par="CF",user.bins = bin)	# bin = c(50, 70, 80, 90, 120)
          clap.survey.obj[[bnk]] <- survey.dat.restrat(shf=surv.Clap.Rand[[bnk]],htwt.fit=SpatHtWt.fit[[bnk]], RS=RS, CS= CS, 
                                                       bk=bank.4.spatial, areas=strata.areas, mw.par="CF",user.bins = bin)		
          print("survey.dat.restrat done")
        } # end if(bnk=="Sab")
        
        if(bank.4.spatial !="Sab")
        {  
          survey.obj[[bnk]] <- survey.dat(surv.Rand[[bnk]], RS=RS, CS=CS, 
                                          bk=bank.4.spatial, areas=strata.areas, mw.par="CF",user.bins = bin)	
          clap.survey.obj[[bnk]] <- survey.dat(surv.Clap.Rand[[bnk]],SpatHtWt.fit[[bnk]], RS=RS, CS= CS, 
                                               bk=bank.4.spatial, areas=strata.areas, mw.par="CF",user.bins = bin)
          print("survey.dat done")
        } # end if(bnk!="Sab")
        
        survey.obj[[bnk]][[1]]$CF <- na.omit(sapply(1:length(years),
                                                    function(x){with(subset(surv.Rand[[bnk]],year == years[x]),
                                                                     weighted.mean(CF,com.bm,na.rm=T))}))
        survey.obj[[bnk]][[1]]$clappers<-clap.survey.obj[[bnk]][[1]]$N
        survey.obj[[bnk]][[1]]$clappersR<-clap.survey.obj[[bnk]][[1]]$NR
        
      } # end if(bank.4.spatial != "Ger" && bank.4.spatial != "Mid" && bank.4.spatial != "GB" && bank.4.spatial != "Ban")
      
      # Mostly due to GB, but I want to have the CS and RS for each year of the calculations here...
      survey.obj[[bnk]][[1]]$CS <- CS
      survey.obj[[bnk]][[1]]$RS <- RS
      clap.survey.obj[[bnk]][[1]]$CS <- CS
      clap.survey.obj[[bnk]][[1]]$RS <- RS
      
      # Now get the rest of the Survey summary and SHF summaries for the banks, later we'll export as csv's.
      if(bank.4.spatial != "Ger")
      {
        
        SS.summary[[bnk]] <- survey.obj[[bnk]][[1]]
        SS.summary[[bnk]]$bank <- bank.4.spatial
        # Same for the SHF data.
        SHF.summary[[bnk]] <- as.data.frame(cbind(survey.obj[[bnk]][[1]]$year,survey.obj[[bnk]][[2]]$n.yst))
        SHF.summary[[bnk]]$bank <- bank.4.spatial
      } # end if(bnk != "Ger")
      
      # Give the SS.summary headers nice names and output the results to the appropriate folder
      #names(SS.summary[[bnk]]) <- c("year","n","FR.BM","CV.FR.BM","R.BM","CV.R.BM","Pre.BM","CV.Pre.BM",
      #                            "FR_N", "CV.FR.N",  "R.N","CV.R.N","Pre.N", "CV.Pre.N","bank")
      
      # MEAT COUNT & CONDITION FACTOR requires some processing
      if(!bank.4.spatial %in% c("Ban", "BanIce")) CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank == bnk & year==yr,c('tow','lon','lat')),
                                                                                   SpatHtWt.fit[[bnk]]$fit))
      if(bank.4.spatial %in% c("Ban")) CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank == bnk & year==yr & species=="seascallop",c('tow','lon','lat')),
                                                                        SpatHtWt.fit[[bnk]]$fit))
      if(bank.4.spatial %in% c("BanIce")) CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank == bnk & year==yr & species=="icelandic",c('tow','lon','lat')),
                                                                           SpatHtWt.fit[[bnk]]$fit))
      if(bank.4.spatial == "GB") CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank %in% c("GB","GBa","GBb") & year==yr & month < 7,
                                                                         c('tow','lon','lat')),SpatHtWt.fit[[bnk]]$fit))
      if(bank.4.spatial == "GBa") CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank == bank.4.spatial & year==yr & month > 6,
                                                                          c('tow','lon','lat')),SpatHtWt.fit[[bnk]]$fit))
      if(bank.4.spatial == "GBb") CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank == bank.4.spatial & year==yr & month > 6,
                                                                          c('tow','lon','lat')), SpatHtWt.fit[[bnk]]$fit))
      names(CF.current[[bnk]])[4]<-"CF"
      # For German we want all the tows here, both the random and the repeats.
      if(bank.4.spatial == "Ger") CF.current[[bnk]]<-merge(CF.current[[bnk]],subset(surv.Live[[bnk]],year==yr,c('year','tow','lon','lat',"com","com.bm")))
      # If not German we only want the 'random' tows
      if(bank.4.spatial != "Ger") CF.current[[bnk]]<-merge(CF.current[[bnk]],subset(surv.Rand[[bnk]],year==yr,c('year','tow','lon','lat',"com","com.bm")))
      
      # Meat count per 500g
      CF.current[[bnk]]$meat.count <- 0.5/(CF.current[[bnk]]$com.bm/CF.current[[bnk]]$com)
      
      # The seedbox calculations		
      # Bring in the seeboxes for the latest year
      sb <- subset(seedboxes,Bank == bnk & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep="") | Bank == bnk & Active=="Yes")
      sb <- subset(sb, Active=="Yes")
      if(bank.4.spatial == "GB")  sb <- subset(seedboxes,Bank %in% c("GBa","GBb") & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep="")| Bank == bnk & Active=="Yes")
      
      # If there were any seeboxes closed in this year get the results from the box(es)
      if(length(sb[,1]) > 0)
      {
        # Here we pull out the data from within the seedboxes, this could be used
        # too look at results from any seedbox of interest as long as we have it's name (but if so use the seebox object
        # as BBboxes is subset to just be currently closed boxes on BBn)
        #Source15. #source("fn/simple.surv.r")
        sb[,c("X", "Y")] <- apply(sb[,c("X", "Y")], 2, function(x) as.numeric(x))
        boxes <- as.PolySet(sb,projection = "LL")
        # Note that we are grabbing all samples from within a box and not just the random tows.
        box.dat <- data.frame(EID=1:nrow(surv.Live[[bnk]]),X=surv.Live[[bnk]]$lon,Y=surv.Live[[bnk]]$lat)
        box.names <- unique(boxes$SCALLOP_Group_ID)
        # In case we have multipe boxes closed...
        for(m in 1:length(box.names))
        {  
          key <-findPolys(box.dat, subset(boxes,SCALLOP_Group_ID == box.names[m]))
          seedbox.obj[[bnk]][[m]] <- simple.surv(surv.Live[[bnk]][1:nrow(surv.Live[[bnk]]) %in% key$EID,],years=years,user.bins = bin)
          seedbox.obj[[bnk]][[m]]$model.dat$RS <- RS
          seedbox.obj[[bnk]][[m]]$model.dat$CS <- CS
          seedbox.obj[[bnk]][[m]]$box.tow.data <- surv.Live[[bnk]][1:nrow(surv.Live[[bnk]]) %in% key$EID,]
        } # end for(m in 1:length(box.names))
      } #end if(length(boxes[,1]) > 0))
      
      # Now let's calculate the average size and growth potential by bank, use surv.Live b/c we want to look at this for all tows.
      
      pot.grow[[bnk]] <- grow.pot(dat= surv.Live[[bnk]],mwsh.fit = SpatHtWt.fit[[bnk]],bank = bank.4.spatial)
      
      # Set biomass and condition to NA for years with no detailed sampling data
      survey.obj[[bnk]]$model.dat[!survey.obj[[bnk]]$model.dat$year %in% unique(cf.data[[bnk]]$CFyrs$year), 
                                  c("I","I.cv", "IR","IR.cv", "IPR", "IPR.cv", "CF", "w.bar", "w.k")] <- NA
      survey.obj[[bnk]]$shf.dat$w.yst[!survey.obj[[bnk]]$model.dat$year %in% unique(cf.data[[bnk]]$CFyrs$year)] <- NA
      
      if(bnk == 'Ger') {
        merged.survey.obj[!merged.survey.obj$year %in% unique(cf.data[[bnk]]$CFyrs$year), 
                          c("I","I.cv", "IR","IR.cv", "IPR", "IPR.cv", "CF", "w.bar", "w.k")] <- NA
        lined.survey.obj$model.dat[!lined.survey.obj$model.dat$year %in% unique(cf.data[[bnk]]$CFyrs$year), 
                                   c("I","I.cv", "IR","IR.cv", "IPR", "IPR.cv")] <- NA
      }
      
    } # end "else" for all surveys other than BanIceSpring
    
    
    ###############################################################################################################
    ########## End Section 2  ########## End Section 2  ########## End Section 2  ########## End Section 2
    ########## End Section 2 ########## End Section 2   ########## End Section 2  ########## End Section 2
    ###############################################################################################################
    
    
    
    #########################################################################################################################
    ########## Section 3  ########## Section 3  ########## Section 3  ########## Section 3  ########## Section 3  ###########
    ########## Section 3  ########## Section 3  ########## Section 3  ########## Section 3  ########## Section 3  ###########
    #########################################################################################################################
    # Now that we've done everything we can save all the results.  Pay attention to how this is structured so you can understand 
    # where/what the data are saved as given what you've run.  For example "SurveySummary_figures" only works when you have run the
    # Spring/Summer/both for all the banks included in the survey (it loads Survey_all_results, Survey_spring_results, or Survey_summer_results).
    #Write2 Output some of the summary data from the survey.
    
    if(bnk %in% c("BBn" ,"BBs" ,"Ger", "Mid", "Ban", "BanIce", "Sab", "GB" ,"GBb", "GBa"))
    {
      #Write2 Output some of the summary data from the survey.
      write.csv(SS.summary[[bnk]],
                file = paste(direct,"Data/Survey_data/",yr,"/",na.omit(unique(bank.dat[[bnk]]$survey)),"/",bank.4.spatial,"/Annual_summary",
                             yr,".csv",sep=""),row.names = F)
      #Write3
      write.csv(SHF.summary[[bnk]],
                file = paste(direct,"Data/Survey_data/",yr,"/",na.omit(unique(bank.dat[[bnk]]$survey)),"/",bank.4.spatial,"/Annual_SHF_summary",
                             yr,".csv",sep=""),row.names = F)
      #Write4
      if(!bnk == "BanIce") write.csv(mw.dat.all[[bnk]],paste(direct,"Data/Survey_data/",yr,"/",na.omit(unique(bank.dat[[bnk]]$survey)),"/",bank.4.spatial,
                                        "/mw_Data.csv",sep=""),row.names=F)
      #Write5 - Output the raw survey data in it's entirety
      write.table(surv.dat[[bnk]],
                  paste(direct,"Data/Survey_data/",yr,"/",na.omit(unique(bank.dat[[bnk]]$survey)),"/",bank.4.spatial,
                        "/Survey",min(years),"-",max(years),".csv",sep=""),sep=',',row.names=F)
    } # end if(bnk %in% c("Banspring", BBnspring" ,"BBsspring" ,"Gerspring", "Midspring", "Sabspring", "GBspring" ,"GBbsummer", "GBasummer"))
  } # end loop
  
  # If we have included all the surveyed banks save it as this.  This assumes we get 5 banks done in the spring and 2 banks in the summer.
  # This may need adjusted if we had a weird survey year (such as 2015).
  
  # If I'm just testing
  if(testing == T & is.null(nickname)) save(list = ls(all.names = TRUE), 
                                            file = paste(direct,"Data/Survey_data/",yr,
                                                         "/Survey_summary_output/testing_results.Rdata",sep=""))
  if(testing == T & !is.null(nickname)) save(list = ls(all.names = TRUE), 
                                             file = paste(direct,"Data/Survey_data/",yr,
                                                          "/Survey_summary_output/testing_results_", nickname, ".Rdata",sep=""))
  if(testing == F)
  {
    if(season == "both" && num.surveys >=7)	save(list = ls(all.names = TRUE), 
                                                 file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))
    # If running everything save the final survey .obj for all banks.
    if(season == "both" && num.surveys >=7)	save(survey.obj,file=paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_object.Rdata",sep=""))
    
    # This will save the results indicating that we don't have all the information for the banks, you may want this if just looking at a couple
    # banks for some reason
    if(season == "both" && num.surveys <7)	save(list = ls(all.names = TRUE), 
                                                file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Selected_Survey_results.Rdata",sep=""))
    # Also if you want to save the survey objects
    if(season == "both" && num.surveys <7)	save(survey.obj,
                                                file=paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Selected_Survey_objects.Rdata",sep=""))
    
    # Now if just running the spring...
    # If running the spring only save it as this.  Again assumes we do at least 5 banks in the spring (Sab,Ger,BBn,GB,Mid)
    if(season == "spring" && num.surveys >=5)  save(list = ls(all.names = TRUE),
                                                    file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))
    # If fewer than 5 banks selected save the data as this
    if(season == "spring" && num.surveys <5) save(list = ls(all.names = TRUE),
                                                  file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Selected_spring_survey_results.Rdata",sep=""))
    # If for some reason you just want the summer results save this here (note if you specify summer but still specify a bank that was sampled
    # during the spring you'll end up with spring data mixed in here.)
    if(season == "summer" && surveys %in% c("GBasummer", "GBbsummer"))		  save(list = ls(all.names = TRUE),
                                                                               file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_summer_results.Rdata",sep=""))
    
    if(season == "summer" && num.surveys !=2)		  save(list = ls(all.names = TRUE),
                                                      file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Selected_summer_survey_results.Rdata",sep=""))
  } # end if(testing ==F) 
  
  return(list(survey.obj = survey.obj,
              SHF.summary = SHF.summary,
              SS.summary = SS.summary,
              CF.current = CF.current,
              cf.data = cf.data,
              clap.survey.obj = clap.survey.obj,
              lined.survey.obj = lined.survey.obj,
              merged.survey.obj = merged.survey.obj,
              seedbox.obj = seedbox.obj,
              pot.grow = pot.grow,
              survey.strata.table = survey.strata.table))
  
  ##################################################################################################################################
  ########## End Section 3 ########## End Section 3 ########## End Section 3 ########## End Section 3 ########## End Section 3 #####
  ########## End Section 3 ########## End Section 3 ########## End Section 3 ########## End Section 3 ########## End Section 3 #####
  ##################################################################################################################################
  
} # end function

