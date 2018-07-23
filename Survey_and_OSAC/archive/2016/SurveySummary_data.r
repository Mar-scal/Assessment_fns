################################################################################################################
##### This is the script for accessing the survey data and for producing the output data for analysis
#####  This along with Survey Summary figures replaces Survey Summary final
#####  DK December 11 2015.
# Update history
# Commented, checked  and revised by DK April 4th, 2016
# May 12th, updated to use the updated sql database back to 2002.  The method for selecting matched tows on German
# bank also modified, no longer using or creating flat files with the german tow list information in it.
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
#   9:  restratwp.r
#  10:  survey.dat.r
#  11:  sprSurv.r
##
###############################################################################################################

###############################################################################################################
# Arguments
# 1:  direct:       The working directory to put figures are from which to grab data.  Default = "Y:/Offshore scallop/Assessment/", 
# 2:  yr.start:     Start year for getting the data.  1984 is default, we haven't generally used older data 
# 3:  yr:           End year.  Default is the current system year: as.numeric(format(Sys.time(), "%Y"))
# 4:  surveys:      Which banks+survey to pull the data for.  Defaults to all banks except Banquereau which isn't yet supported
#                   Options are "BBnspring" ,"BBsspring" ,"Gerspring", "Midspring", "Sabspring", "GBspring" ,"GBbsummer", "GBasummer"
# 5:  survey.year:  If specified this will overwrite the above "yr" and select the final year of the data, Default = NULL.  This is mostly useful 
#                   if preprocessed = T (that is you already have data up to current year and don't want to re-run the processing step but
#                   want to create the survey data only up to a specific year).
# 6:  preprocessed: If you have already run the function and have the database data saved but want to recreate the processed survey data. Default =F
# 7:  un.ID:        Your SQL database username:  Default = un.ID (which I suggest you specify in your R profile so it is loaded by default)
# 8:  pwd.ID:       Your SQL database password:  Default = pwd.ID (if specified in your R profile it will be loaded by default)
###############################################################################################################



survey.data <- function(direct = "Y:/Offshore scallop/Assessment/", yr.start = 1984, yr = as.numeric(format(Sys.time(), "%Y")) ,
                        surveys = c("BBnspring" ,"BBsspring" ,"Gerspring", "Midspring", "Sabspring", "GBspring" ,"GBbsummer", "GBasummer"),
                                    survey.year= NULL,preprocessed = F,un.ID=un.ID,pwd.ID=pwd.ID)
{  
##############################################################################################################
################################### SECTION 1 SECTION 1 SECTION 1 ############################################
#################################### LOAD PACKAGES, LOAD FLAT FILES (where possible) #########################
################################### PRE-PROCESS DATA (where possible)               #########################
################################### SECTION 1 SECTION 1 SECTION 1 ############################################
##############################################################################################################
#


################################### Start Load Packages and Functions #########################################
# Step 1:  Load all required packages and functions immediately
#
##########################################################################################################
# First load required packages
require(PBSmapping)
require(RColorBrewer)
############################# GENERAL DATA ########################################################
############################# GENERAL DATA ########################################################
# Enter here standard data which is used throughout this script.
# The length of the loop to run
num.surveys <- length(surveys)
atow<-800*2.4384/10^6 # area of standard tow in km2
############################# END GENERAL DATA ########################################################

############################# LOAD FUNCTIONS ########################################################

# Now load all functions in the program in one location.  All calls to these functions are linked via the commented source #
# so we can easily tie the function call to the script for that function.
# The  functions are in this directory unless explicitly specified
# These 8 functions are pre-processing functions used to bring in and arrange various pieces of data
source(paste(direct,"Assessment_fns/Survey_and_OSAC/import.survey.data.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/get.offshore.survey.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/import.hyd.data.r",sep="")) 

# These are the functions used to within the heart of the code to make stuff happen
source(paste(direct,"Assessment_fns/Survey_and_OSAC/getdis.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/shwt.lme.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/condFac.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/surv.by.tow.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/simple.surv.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/restratwp.r",sep=""),local=T) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/survey.dat.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/sprSurv.r",sep="")) 


################################## End Load Functions   #######################################################

################################### START LOAD & PRE-PROCESS DATA ############################################
# Step 2:  Load Data: Load all input data here, 
##########################################################################################################

# If we already have the data we can skip this and just load the data, saves about 2 minutes...
if(preprocessed == F)
{
################# LOAD GENERAL FLAT FILES ###################################################################

#Read2 This is a huge flat file with decades of condition factor, shell weight, and meat height data in it up to 2012.
ps.dat<-read.csv(paste(direct,"data/Condition/PSGB.csv",sep=""))

# Polygons for all the offshore banks
newAreaPolys<-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep="")
                       ,stringsAsFactors = F,header=T)
#Read30 All the seedboxes ever.
seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes.csv",sep=""),
                     stringsAsFactors = F,header=T)
seedboxes$Closed <- as.Date(seedboxes$Closed)
seedboxes$Open <- as.Date(seedboxes$Open)
# Dump the commments they are just messy..
seedboxes <- seedboxes[,-grep("comment",names(seedboxes))]

# Get the survey boundary polygons for all banks.
survey.bound.polys<-read.csv(paste(direct,"data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
                             header=T,stringsAsFactors = F)
# Get the detailed survey polygons for all banks
survey.detail.polys <- read.csv(paste(direct,"data/Maps/approved/Survey/survey_detail_polygons.csv",sep=""),
                                header=T,stringsAsFactors = F)
# Get the survey information for each bank
survey.info <- read.csv(paste(direct,"data/Survey_data/survey_information.csv",sep=""),
                        header=T,stringsAsFactors = F)

# The boundary between Fully recruited and recruit size classes (RS = Recruit min, CS = Fully recruited (commercial) min)
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
    survMay.dat<-import.survey.data(1984:2001,survey='May',explore=T,export=F,dirc=direct)
    survAug.dat<-import.survey.data(1981:2001,survey='Aug',explore=T,export=F,dirc=direct)
		
    # Here we are subseting these data and getting rid of totwt and baskets bearing and distance coefficient
    survMay.dat<-survMay.dat[which(!names(survMay.dat)%in%c("dis","brg",'totwt','baskets'))]
    survAug.dat<-survAug.dat[which(!names(survAug.dat)%in%c("dis","brg",'totwt','baskets'))]
    # Using month as an indicator of whether data is spring or summer, so these might be off by a month but will do the trick.
    survMay.dat$month <-  5
    survAug.dat$month <-  8
    
    
		# From Whatever year we have the data loaded into SQL database we are getting the data directly from the SQL server.
		# As of Winter 2016 this was an ongoing process.
    #Source6 source("fn/get.offshore.survey.r") Get the data directly from SQL
		SurvDB<-get.offshore.survey(db.con ="ptran", un=un.ID , pw = pwd.ID,direct=direct)
		### This grabs the data directly from the database and makes it it's own object, works for now
		### since we don't use the data for anything but a plot on GB.
		temps <- subset(SurvDB$SHF,state == "live",
		                select = c("TOW_NO","BOTTOM_TEMP","YEAR","MGT_AREA_CD","CRUISE","slon","slat","elat","elon"))
		names(temps) <- c("tow","mean.temp","year","bank","cruise","slon","slat","elat","elon")
		# Remove the temperature data so the survey data from before temperature data was collected lines up nicely. (once db is updated back
		# to spring of 2011 this will become unncessary
		SurvDB$SHF <- SurvDB$SHF[,-which(names(SurvDB$SHF)=="BOTTOM_TEMP")]
		
		# Extract the month so we can determine spring (< 7) vs. summer (> 6) surveys.  Really only necessary for GBa/GBb to be honest.
		hdr.mw <- c("bank","tow","slat","slon","elat","elon","depth.f","year",
		            "lon","lat","depth","cruise","scalnum","wmw","sh","ID","month")
		hdr.shf <- c("year","cruise","bank","date","tow","stratum","slat","slon","elat","elon","depth","state","h5","h10","h15",
		             "h20","h25","h30","h35","h40","h45","h50","h55","h60","h65","h70","h75",
		             "h80","h85","h90","h95","h100","h105","h110","h115","h120","h125","h130","h135","h140","h145","h150","h155",
		             "h160","h165","h170","h175","h180","h185","h190","h195","h200","random")
		names(SurvDB$MWs)<-hdr.mw
		names(SurvDB$SHF)<-hdr.shf
		SurvDB$SHF$month <- as.numeric(format(SurvDB$SHF$date, "%m"))
		SurvDB$pos$month <- as.numeric(format(SurvDB$pos$TOW_DATE, "%m"))
		SurvDB$MWs$month <- as.numeric(format(SurvDB$MWs$month, "%m"))
		# Replace the strata ID with a unique identifier.
		SurvDB$MWs$ID<-with(SurvDB$MWs,paste(cruise,tow,sep='.'))
		# Combine the Survey data.  This first one has all of the SHF data there is.
		all.surv.dat <- rbind(survMay.dat,survAug.dat,SurvDB$SHF) # Springsurv2011$SHF,
		all.surv.dat$survey[all.surv.dat$month < 7] <- "spring"
		all.surv.dat$survey[all.surv.dat$month > 6] <- "summer"
		# Make German and BBn all spring too, 2015 did occur in summer, but these are spring surveys so makes the below work easier.
		all.surv.dat$survey[all.surv.dat$bank %in% c("Ger","BBn")] <- "spring"
		
		# This makes sure that ALL the data have the lat/long calculated in the same way
		all.surv.dat$lon<-with(all.surv.dat,apply(cbind(elon,slon),1,mean))
		all.surv.dat$lat<-with(all.surv.dat,apply(cbind(elat,slat),1,mean))
		
		# THe MW data is now from 2006 to current soon to be all data...
		MW.dat.new <- SurvDB$MWs
		
		#Source7 	source("fn/import.hyd.data.r") 'Hydration' sampling, essentially this is the MW data that isn't yet in the SQL DB
		# NOTE:  This function will go away once we have Offshore data loaded, should be spring 2016
		MW.dat<-import.hyd.data(yrs=1982:2005, export=F,dirt=direct)

	# You're done with the SQL calls at this point so remove your username and password so they don't get saved...
	rm("un.ID","pwd.ID")
		
# Now save the data so you don't have to do all that every time
		save(list = ls(all.names = TRUE), file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.Rdata",sep=""))
		
} # end if(preprocessed == F)

# Load the preprocessed data, this is a little stick handling to make sure I have survey's picked right when using this data.
if(preprocessed == T) 
  {
    # You're done with the SQL calls at this point so remove your username and password so they don't get saved...
    # If they don't exist you'll get a warning but who cares...
    rm("un.ID","pwd.ID")
    tmp <- surveys		
    dirc <- direct
    load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.Rdata",sep=""))  
    direct <- dirc
    # These are the functions used to within the heart of the code to make stuff happen
    source(paste(direct,"Assessment_fns/Survey_and_OSAC/getdis.r",sep="")) 
    source(paste(direct,"Assessment_fns/Survey_and_OSAC/shwt.lme.r",sep="")) 
    source(paste(direct,"Assessment_fns/Survey_and_OSAC/condFac.r",sep="")) 
    source(paste(direct,"Assessment_fns/Survey_and_OSAC/surv.by.tow.r",sep="")) 
    source(paste(direct,"Assessment_fns/Survey_and_OSAC/simple.surv.r",sep="")) 
    source(paste(direct,"Assessment_fns/Survey_and_OSAC/restratwp.r",sep=""),local=T) 
    source(paste(direct,"Assessment_fns/Survey_and_OSAC/survey.dat.r",sep="")) 
    source(paste(direct,"Assessment_fns/Survey_and_OSAC/sprSurv.r",sep="")) 
    surveys <- tmp
    num.surveys <- length(surveys)
  } # end if(preprocessed == T) 
	

###############################################################################################################
################################## END DATA PRE-PROCESSING ################################## 
################################################################################################################
################################### END SECTION 1 END SECTION 1#################################################
################################### END LOAD & PRE-PROCESS DATA ###############################################
################################### END SECTION 1 END SECTION 1#################################################
###############################################################################################################
		

# Create a new column that ID's the Bank-Survey combo...
# Make all the GB spring data just GB as we don't differentiate it here.
all.surv.dat$bank[all.surv.dat$bank %in% c("GBa","GBb","GB") & all.surv.dat$survey == "spring"] <- "GB"
all.surv.dat$surv.bank <- paste0(all.surv.dat$bank,all.surv.dat$survey)
# Fix up BBn and German because of the timing of the 
# Remove Banquereau since it is done so infrequently, and GBsummer data are ambiguous as to whether they are
# from GBa or GBb so remove them too...
all.surv.dat <- subset(all.surv.dat,surv.bank != "GBsummer" & surv.bank != "BanIcespring" & surv.bank != "Banspring")

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
tow.dis <- NULL
seedbox.obj <- NULL


# Now get the survey summary results for all the banks...
for(i in 1:num.surveys)
{
#  So first thing to do is get the data for the bank....
bnk <- as.character(unique(subset(all.surv.dat,surv.bank == surveys[i])$bank))
bank.dat[[bnk]] <- subset(all.surv.dat,surv.bank==surveys[i])
if(bnk == "GB") bank.dat[[bnk]] <- subset(all.surv.dat,surv.bank == surveys[i] & year != 2015)
RS <- size.cats$RS[size.cats$Bank == bnk]
CS <- size.cats$CS[size.cats$Bank == bnk]
if(is.null(survey.year)==F) yr <- survey.year
if(is.null(survey.year)==T) yr <- max(bank.dat[[bnk]]$year,na.rm=T)
years <- yr.start:yr
# Because of change in RS and CS on GBa we need to have this more nuanced for GBa
# The CS and RS specified here actually 5 higher than the actual shell heights
# CS = Shell height for knife-edge recriutment:  
# Correctly specifying the years here really matters since the RS and CS are changing with time,
# begs for a better method!!
# 1981-1985 CS = 75, RS = 60
# From 1986-1995 CS = 85, RS= 75
# From 1996-current CS= 95, RS = 85
# This is the Shell height for each shell height category. If it ever changes in the future this will need adjusted.
# If we are still using this code in 2030 I'll be both old and worried...
if(bnk == "GBa")
  {
    SH.dat <- data.frame(year = 1980:2030,CS = c(rep(75,6),rep(85,10),rep(95,2030-1995)),RS = c(rep(60,6),rep(75,10),rep(85,2030-1995)))
    CS <- SH.dat$CS[SH.dat$year %in% years]
    RS <- SH.dat$RS[SH.dat$year %in% years]
  }# End if(bnk == "GBa")
# Remove years in which we don't have good data for specific banks, 1984 very problematic with clappers/live data.
# One Browns South and North we also need to be particulatr with data
if((bnk == "Ger" || bnk == "Mid" || bnk == "Sab") && yr.start < 1985) years <- 1985:yr
if(bnk == "BBs"  && yr.start < 1988) years <- c(1985,1986,1988:yr)
if(bnk == "BBn"  && yr.start < 1991) years <- c(1991:yr)
bank.dat[[bnk]] <- subset(bank.dat[[bnk]] , year %in% years)

#####################################  TOW TRACKS, THIS SECTION MAY NEED ADJUSTED EACH YEAR ##########################
# The tow tracks remain messy, but I don't see a way around this
# Next we will input all of the tow tracks for the banks, this is messy, but can't see a way around it..
# TOW TRACK DATA MIDDLE BANK
# Need to input the number of tows, file location, weighting scheme, seconds between readings (rule) 
# #Source10 source("fn/getdis.r")  
if(bnk == "Mid") tow.dis[[bnk]] <-dist.coef(1:15,path=paste(direct,"data/Tow_tracks/",yr,"/Spring/Middle/",sep=""),w=c(1:10,9:1),
                  rule=8,smooth=T,plt=F,meh=1000,direct=direct)
# TOW TRACK DATA SABLE BANK
# Need to input the number of tows, file location, weighting scheme, seconds between readings
#Source10 source("fn/getdis.r") 
if(bnk == "Sab") tow.dis[[bnk]] <- dist.coef(1:100,path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/Sable/",sep=""),
                                           w=c(1:10,9:1),rule=8,smooth=T,plt=F,direct=direct)

# TOW TRACK DATA GERMAN BANK
# Need to input the number of tows, file location, weighting scheme, seconds between readings
#Source10 source("fn/getdis.r") 
if(bnk == "Ger") tow.dis[[bnk]]<-dist.coef(c(404,405,444,446,447,458,472,c(407:415),c(417:423),c(425:431),
                    c(433:436),c(439:442),c(449:451),c(454:456),c(460:468), c(474:480),1:20),
                  path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/German/",sep=""), w=c(1:10,9:1),rule=8,smooth=T,plt=F,direct=direct)

# I don't have BBs example will need to add perhaps whenever we get back out there.

# Tow tracks for BBn

# TOW TRACK DATA BBn BANK Need to input the number of tows, file location, weighting scheme, seconds between readings
#Source10 source("fn/getdis.r") 

if(bnk == "BBn")
  {
reg<-dist.coef(c(1:100),path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/BBn/",sep=""),
                  w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000,direct = direct)
extra<-dist.coef(c(950:970),path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/BBnextras/",sep=""),
                         w=c(1:10,9:1),rule=8,smooth=T,plt=F,direct=direct)

tow.dis[[bnk]] <- as.data.frame(rbind(reg,extra))
  } # end if(bnk == "BBn")

#Source10 source("fn/getdis.r") Calculate the tow track distances for Georges Spring survey.  
if(bnk == "GB") tow.dis[[bnk]] <- dist.coef(subset(bank.dat[[bnk]],year==yr & state=='live')$tow,
                    path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/GB/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F,direct=direct)


#Source10 source("fn/getdis.r") Tow distances in GBa and GBb and with extra tows
if(bnk == "GBa")
  {
    reg<-dist.coef(c(1:200),path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBa/",sep=""),
                  w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000,direct=direct)
    extra<-dist.coef(c(910:915),
                       path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBa/",sep=""),
                       w=c(1:10,9:1),rule=8,smooth=T, plt=F,meh=1000,direct=direct)
    tow.dis[[bnk]] <- as.data.frame(rbind(reg,extra))
    
  } # end if(bnk == "GBa")


if(bnk == "GBb") tow.dis[[bnk]] <- dist.coef(301:330,path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBb/",sep=""),
                  w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000,direct=direct)

#####################################  END TOW TRACKS, THIS SECTION MAY NEED ADJUSTED EACH YEAR ##########################

    # Now run the survey boundary and seedboxes in here.
    # Get the  bank survey boundary polygon this replaces #Read3
    bound.poly.surv <- subset(survey.bound.polys,label==bnk) 
    attr(bound.poly.surv,"projection")<-"LL"
    
    # Sable Bank Polygons	
    #Read4 Read drooped #Detailed Survey polygons
    detail.poly.surv <- subset(survey.detail.polys,label==bnk)
    attr(detail.poly.surv,"projection")<-"LL"
    
    # Get the strata areas.
    strata.areas <- subset(survey.info,label==bnk,select =c("PID","towable_area"))
    #Read25 read removed... Get all the details of the survey strata
    surv.info <- subset(survey.info,label== bnk)
    # Give each tow a unique identifier.
    bank.dat[[bnk]]$ID<-paste(bank.dat[[bnk]]$year,bank.dat[[bnk]]$tow,sep='.')

    
    
    #Source20 source("fn/restratwp.r",local=T)
    # Reassign the strat based on location and write to the screen what percentage of strata were reassigned.
    # German and Middle bank have no stratifcation scheme 
    if(bnk == "BBn")  bank.dat[[bnk]]$tow[ bank.dat[[bnk]]$year %in% 1991:2008] <-
                                         bank.dat[[bnk]]$tow[bank.dat[[bnk]]$year %in% 1991:2008]+200

    if(bnk != "Ger" && bnk != "Mid"  && bnk != "GB")
      {
        bank.dat[[bnk]]<-restratwp(bank.dat[[bnk]],list(detail.poly.surv))
        paste("Strata reassigned",
          with(subset(bank.dat[[bnk]],year==yr & state=='live'),
               round((1-sum(stratum == new.stratum,na.rm=T)/length(stratum))*100)),'%')
        strat.chk <- subset(bank.dat[[bnk]],year==yr & state=='live'& random==1)
        strata.mis.match[[bnk]] <- na.omit(strat.chk[strat.chk$stratum != strat.chk$new.stratum,
                                               c("year","cruise","bank","tow","stratum","new.stratum")])
        strata.mis.match[[bnk]] <- rbind(strata.mis.match[[bnk]],strat.chk[is.na(strat.chk$new.stratum),
                                                           c("year","cruise","bank","tow","stratum","new.stratum")])
      } # end  if(bnk != "Ger" && bnk != "Mid")

		# MEAT WEIGHT DATA from 2011-current
		# Get the mw data from 2011 to this year
    if(bnk != "GB" && bnk != "GBa" && bnk != "GBb") mw[[bnk]] <- subset(MW.dat.new,bank==bnk)
    # GB spring in 2015 didn't have a true survey so it shouldn't be included.
    if(bnk=="GB")              mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GB","GBa","GBb") & month < 7)
    if(bnk=="GBa")             mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GBa") & month > 7)
    if(bnk=="GBb")             mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GBb") & month > 7)
		
		# For the most recent data
		mw.dm <- subset(mw[[bnk]],year==yr)
		mw.dm$sh<-mw.dm$sh/100
		
		# MODEL - This is the meat weight Shell height realationship.  
		#MEAT WEIGHT SHELL HEIGHT RELATIONSHIP in current year 
		#Source11 source("fn/shwt.lme.r") note thtat the exponent is set as a parameter here b=3
		SpatHtWt.fit[[bnk]] <- shwt.lme(mw.dm,random.effect='tow',b.par=3)
		
		# here we are putting the MW hydration sampling from 2010 and earlier together with the data since 2010 and 
		# then we export it as a csv.
		if(bnk == "Mid") 
		  {
  		  # MEAT WEIGHT DATA - hydration sampling, This will vary for certain banks, 
  		  # Create and export a MW-SH object
  		  mw.dat.all[[bnk]] <- merge(subset(MW.dat,bank==bnk & month %in% 5:6 & year > 1983,
  		                                       c("tow","year","lon","lat","depth","sh","wmw")),
                         subset(mw[[bnk]],select=c("tow","year","lon","lat","depth","sh","wmw")),all=T)
        mw.dat.all[[bnk]]$ID<-paste(mw.dat.all[[bnk]]$year,mw.dat.all[[bnk]]$tow,sep='.')
        write.csv(mw.dat.all[[bnk]],paste(direct,"Data/Survey_data/",yr,"/Spring/",bnk,"/mw_Data.csv",sep=""),row.names=F)
        ## MODEL - This is the model used to esimate condition factor across Middle Bank
        #Source13 source("fn/condFac.r")
        # Due to the sparseness of the data for this bank the most complex model we can fit is a gam_d, 
        # data this is like far more complex still than the really allows for.
        cf.data[[bnk]]<-condFac(mw.dat.all[[bnk]],bank.dat[[bnk]],model.type='gam_d',dirct=direct)
      } # end if(bnk == "Mid")
		
		if(bnk != "Mid")
		  {
		    if(bnk != "GB") mw.tmp <- subset(MW.dat,bank == bnk)
		    if(bnk == "GB") mw.tmp <- subset(MW.dat,bank %in% c("GB","GBa","GBb"))
		    mw.tmp$ID <- paste(mw.tmp$year,mw.tmp$tow,sep='.')
		    # Grab the relavent Meat-Weight Shell height data and make a flat file from it
		    if(bnk %in% c("BBn","Ger","Sab","BBs","GB")) mw.dat.all[[bnk]] <- merge(subset(mw.tmp, month %in% 5:6 & year %in% years, 
		                                c("ID","year","lon","lat","depth","sh","wmw","tow")),
		                            subset(mw[[bnk]],select=c("ID","year","lon","lat","depth","sh","wmw","tow")),all=T)
		    # Grab the relavent Meat-Weight Shell height data for the summer 
		    if(bnk %in% c("GBa","GBb")) mw.dat.all[[bnk]] <- merge(subset(mw.tmp, month > 7 & year %in% years, 
		                                      c("ID","year","lon","lat","depth","sh","wmw","tow")),
		                               subset(mw[[bnk]],select=c("ID","year","lon","lat","depth","sh","wmw","tow")),all=T)
		  } # end if(bnk == "Sab" | bnk == "Ger") 
#		mw.dat.all[[bnk]] <- subset(mw.dat.all[[bnk]], year != 2015)
		## MODEL - This is the model used to esimate condition factor across the bank for all banks but Middle
		
		if(bnk != "Mid") cf.data[[bnk]]<-condFac(na.omit(mw.dat.all[[bnk]]),bank.dat[[bnk]],model.type='gam_f',dirct=direct)

		# Because of the lined survey on German we want to differentiate between the lined and unlined CF data
		if(bnk == "Ger")
		    {
	  	cf.data[[bnk]]$CFyrs$CF2[cf.data[[bnk]]$CFyrs$year > 2007] <- cf.data[[bnk]]$CFyrs$CF[cf.data[[bnk]]$CFyrs$year>2007]
	  	cf.data[[bnk]]$CFyrs$CF[cf.data[[bnk]]$CFyrs$year > 2007] <- NA
		    } # end if(bnk == "Ger")
		# Fill the years without any data with NA's (this helps with plotting data.)
		cf.data[[bnk]]$CFyrs <-merge(cf.data[[bnk]]$CFyrs,data.frame(year=1983:yr),all=T)
		
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
		#Source14 source("fn/surv.by.tow.r") surv.by.tow calculates number or biomass of pre, rec and com size scallops in each tow
		# Note that the surv.by.tow function needs the CS/RS to be 5 higher than they really are, just for binning purposes.
		if(bnk %in% c("Mid","Ger","BBn","GB","GBa","GBb")) 
		 {
		  surv.dat[[bnk]]<- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS)
		  surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS, type='B', mw.par="CF")
		 } # end if(bnk %in% c("Mid","Ger","BBn","GB","GBa","GBb"))
		
		if(bnk == "Sab" || bnk == "BBs" ) 
		  {
		    surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS)
		    surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS, type='B', mw.par="CFh")
		  } # end if(bnk == "Sab" || bnk == "BBs" ) 

		
		
		# On Georges spring we need to tidy up some of the randoms..
		if(bnk == "GB")
		{
		  surv.dat[[bnk]]$random[surv.dat[[bnk]]$year < 2013] <-4
		  surv.dat[[bnk]]$random[surv.dat[[bnk]]$tow %in% c(1:24,301:324)] <-3
		  surv.dat[[bnk]]$random[surv.dat[[bnk]]$tow > 12 & surv.dat[[bnk]]$year == 1988] <- 4
		} # end if(bnk == "GB")
		
		# Subset the data into the clappers (dead) and live scallops.  Use only random survey tows for Clappers...
		# For GB spring the survey of interest are the comparative tows...
		surv.Clap[[bnk]]<-subset(surv.dat[[bnk]],state=='dead')
		
		surv.Live[[bnk]]<-subset(surv.dat[[bnk]],state=='live')
		surv.Rand[[bnk]]<-subset(surv.dat[[bnk]],state=='live' & random==1)		
		surv.Clap.Rand[[bnk]]<-subset(surv.dat[[bnk]],state=='dead'& random==1)
		if(bnk == "GB") surv.Clap.Rand[[bnk]] <- subset(surv.dat[[bnk]],state=='dead'& random==3)
		if(bnk == "GB") surv.Rand[[bnk]]<-subset(surv.dat[[bnk]],state=='live' & random==3)	
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
		#Source15 source("fn/simple.surv.r") prepare survey index data obj
		if(bnk == "Mid") 
		  {
		    survey.obj[[bnk]]<-simple.surv(surv.Live[[bnk]],years=years)
		    survey.obj[[bnk]][[1]]$CF <- sapply(1:length(years),
		                                                function(x){with(subset(surv.Live[[bnk]],year == years[x]),
		                                                                 weighted.mean(CF,com.bm,na.rm=T))})
		    
		    clap.survey.obj[[bnk]]<-simple.surv(surv.Clap.Rand[[bnk]],years=years)
		  } #end 	if(bnk == "Mid")  
		# And here is Georges Bank spring survey results
		if(bnk == "GB")  
		  {
		    survey.obj[[bnk]]<-simple.surv(surv.Live[[bnk]], random %in% c(1,3),years=years)
		    survey.obj[[bnk]][[1]]$CF <- sapply(1:length(years),
		                                                function(x){with(subset(surv.Live[[bnk]],year == years[x]),
		                                                                 weighted.mean(CF,com.bm,na.rm=T))})
		    clap.survey.obj[[bnk]]<-simple.surv(surv.Clap.Rand[[bnk]], years=years, random %in% c(1,3),)
		  } # end 	if(bnk == "GB")  
		# For German bank we have the matched survey design which leads to some unique processing to get survey results.
		if(bnk == "Ger")
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
		  spr.survey.obj <- sprSurv(lined.dat,2008:yr,ger.tows,chng=T)
		  
		  # prepare survey index data obj
		  #Source15 source("fn/simple.surv.r")
		  unlined.dat<-rbind(subset(surv.Live[[bnk]],year<2007),subset(surv.Live[[bnk]],year == 2007 & tow <= 431),
		                     subset(surv.Live[[bnk]],year == 2008 & tow < 451))
		  # Going to use the unlined survey as the survey.obj, the matched/lined survey specialness will get it's own names
		  survey.obj[[bnk]]<-simple.surv(unlined.dat,years=1983:2008)
		  lined.survey.obj<-simple.surv(lined.dat,years=2008:yr)
		  clap.survey.obj[[bnk]]<-simple.surv(surv.Clap.Rand[[bnk]],years=2008:yr)
		  
		  # The total lined survey object, in 2011 it seems we didn't do repeat tows.
		  merged.survey.obj<-merge(subset(spr.survey.obj[[1]],year!=2011),subset(lined.survey.obj[[1]],year==2011),all=T)
		  
		  # matched survey results.
		  matched.tows<-rbind(subset(surv.Live[[bnk]], year == (yr-1) & tow %in% spr.survey.obj[[2]]$tow.y1),
		                      subset(surv.Live[[bnk]], year == yr & tow %in% spr.survey.obj[[2]]$tow.y2))
		  matched.survey.obj<-simple.surv(matched.tows, years=(yr-1):yr)
		  # This is used below to generate summarys of the survey data on the bank for the most recent year.
		  SS.summary[[bnk]] <- merged.survey.obj
		  SS.summary[[bnk]]$bank <- bnk
		  SHF.summary[[bnk]] <- as.data.frame(cbind(lined.survey.obj[[1]]$year,lined.survey.obj[[2]]$n.yst))
		  SHF.summary[[bnk]]$bank <- bnk
		    }# end if(bnk == "Ger")
		  
		# Get the survey estimates for the banks for which we have strata.
		if(bnk != "Ger" && bnk != "Mid" && bnk != "GB") 
		  {
		    survey.obj[[bnk]] <- survey.dat(surv.Rand[[bnk]], RS=RS, CS=CS, 
		                                bk=bnk, areas=strata.areas, mw.par="CF")	
		    clap.survey.obj[[bnk]] <- survey.dat(surv.Clap.Rand[[bnk]],SpatHtWt.fit[[bnk]], RS=RS, CS= CS, 
		                                  bk=bnk, areas=strata.areas, mw.par="CF")		
	
		    survey.obj[[bnk]][[1]]$CF <- na.omit(sapply(1:length(years),
		                                    function(x){with(subset(surv.Rand[[bnk]],year == years[x]),
		                                                     weighted.mean(CF,com.bm,na.rm=T))}))
		    survey.obj[[bnk]][[1]]$clappers<-clap.survey.obj[[bnk]][[1]]$N
		    survey.obj[[bnk]][[1]]$clappersR<-clap.survey.obj[[bnk]][[1]]$NR
		   
		  } # end if(bnk != "Ger" && bnk != "Mid")
		  
		  # Mostly due to GB, but I want to have the CS and RS for each year of the calculations here...
		  survey.obj[[bnk]][[1]]$CS <- CS
		  survey.obj[[bnk]][[1]]$RS <- RS
		  # Now get the rest of the Survey summary and SHF summaries for the banks, later we'll export as csv's.
		  if(bnk != "Ger")
		    {
		    
		      SS.summary[[bnk]] <- survey.obj[[bnk]][[1]]
		      SS.summary[[bnk]]$bank <- bnk
		      # Same for the SHF data.
		      SHF.summary[[bnk]] <- as.data.frame(cbind(survey.obj[[bnk]][[1]]$year,survey.obj[[bnk]][[2]]$n.yst))
		      SHF.summary[[bnk]]$bank <- bnk
		    } # end if(bnk != "Ger")
		    
		  # Give the SS.summary headers nice names and output the results to the appropriate folder
		  names(SS.summary[[bnk]]) <- c("year","n","FR.BM","CV.FR.BM","R.BM","CV.R.BM","Pre.BM","CV.Pre.BM",
		                              "FR_N", "CV.FR.N",  "R.N","CV.R.N","Pre.N", "CV.Pre.N","bank")
		  
		  
		# MEAT COUNT & CONDITION FACTOR requires some processing
		CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank == bnk & year==yr,c('tow','lon','lat')),
		                               SpatHtWt.fit[[bnk]]$fit))
		if(bnk == "GB") CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank %in% c("GB","GBa","GBb") & year==yr & month < 7,
		                                                        c('tow','lon','lat')),SpatHtWt.fit[[bnk]]$fit))
		if(bnk == "GBa") CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank == bnk & year==yr & month > 6,
		                                                         c('tow','lon','lat')),SpatHtWt.fit[[bnk]]$fit))
		if(bnk == "GBb") CF.current[[bnk]]<-na.omit(merge(subset(na.omit(SurvDB$pos),bank == bnk & year==yr & month > 6,
		                                                         c('tow','lon','lat')), SpatHtWt.fit[[bnk]]$fit))
		names(CF.current[[bnk]])[4]<-"CF"
		CF.current[[bnk]]<-merge(CF.current[[bnk]],subset(surv.Live[[bnk]],year==yr,c('year','tow','lon','lat',"com","com.bm")))
		# Meat count per 500g
		CF.current[[bnk]]$meat.count<-0.5/(CF.current[[bnk]]$com.bm/CF.current[[bnk]]$com)
	
		# Output some of the summary data from the survey.
		write.csv(SS.summary[[bnk]],
		          file = paste(direct,"Data/Survey_data/",yr,"/",unique(bank.dat[[bnk]]$survey),"/",bnk,"/Annual_summary",
		                       yr,".csv",sep=""),row.names = F)
		write.csv(SHF.summary[[bnk]],
		          file = paste(direct,"Data/Survey_data/",yr,"/",unique(bank.dat[[bnk]]$survey),"/",bnk,"/Annual_SHF_summary",
		                       yr,".csv",sep=""),row.names = F)
		write.csv(mw.dat.all[[bnk]],paste(direct,"Data/Survey_data/",yr,"/",unique(bank.dat[[bnk]]$survey),"/",bnk,
		                                "/mw_Data.csv",sep=""),row.names=F)
		#Write4 - Output the raw survey data in it's entirety
		write.table(surv.dat[[bnk]],
		            paste(direct,"Data/Survey_data/",yr,"/",unique(bank.dat[[bnk]]$survey),"/",bnk,
		                  "/Survey",min(years),"-",max(years),".csv",sep=""),sep=',',row.names=F)
		
		
		# The seedbox calculations		
		# Bring in the seeboxes for the latest year
		sb <- subset(seedboxes,Bank == bnk & Open >= paste(yr,"-01-01",sep=""))
		# If there were any seeboxes closed in this year get the results from the box(es)
		if(length(sb[,1]) > 0)
        {
          # Here we pull out the data from within the seedboxes, this could be used
          # too look at results from any seedbox of interest as long as we have it's name (but if so use the seebox object
          # as BBboxes is subset to just be currently closed boxes on BBn)
          #Source15. #source("fn/simple.surv.r")
          boxes <- as.PolySet(sb,projection = "LL")
          box.dat <- data.frame(EID=1:nrow(surv.Live[[bnk]]),X=surv.Live[[bnk]]$lon,Y=surv.Live[[bnk]]$lat)
          box.names <- unique(boxes$SCALLOP_Group_ID)
          # In case we have multipe boxes closed...
          for(m in 1:length(box.names))
            {  
              key <-findPolys(box.dat, subset(boxes,SCALLOP_Group_ID == box.names[m]))
              seedbox.obj[[bnk]][[m]] <- simple.surv(surv.Live[[bnk]][1:nrow(surv.Live[[bnk]]) %in% key$EID,],years=2007:yr)
            } # end for(m in 1:length(box.names))
        } #end if(length(boxes[,1]) > 0))

		# If we only have a subset of the banks do this... Note that if we add Banquereau to this we'll need to make this 9!
		if(num.surveys <  8)		  save(list = ls(all.names = TRUE),
		                           file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_results_for_",bnk,".Rdata",sep=""))
#		if(num.surveys <  8)		save(survey.obj[[bnk]],file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_object_",bnk,#
#		                                            ".Rdata",sep=""))		
				
} # end loop
		
##########################################################################################################################
# Now that we've done everything we can save all the results or we could skip ahead and just load them all if already run.
# If we have all the data save it as this, note that if we add Banquereau to this we'll need to make this 9!
if(num.surveys == 8)	save(list = ls(all.names = TRUE), 
                          file = paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))
if(num.surveys == 8)	  save(survey.obj,file=paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_object.Rdata",sep=""))



} # end function

	