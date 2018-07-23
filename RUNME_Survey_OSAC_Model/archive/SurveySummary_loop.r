################################################################################################################
##### This is the script for accessing the survey data and for producing the output data for analysis
#####  This along with Survey Summary figures replaces Survey Summary final
#####  DK December 11 2015.
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
#   1:  import.survey.dat.r
#   2:  springsurveydata.r (2011)
#   3:  summersurveydata.r (2011)
#   4:  springsurveydata.r (2012)
#   5:  summersurveydata.r (2012)
#   6:  get.offshore.survey.r 
#   7:  import.hyd.data.r
#   8:  import.fishery.data.r
#   9:  ScallopMap.r
#   10: getdis.r
#   11: shwt.lme.r
#   12: shwt.plt1.r
#   13: confac.r
#   14: surv.by.tow.r
#   15: simple.surv.r
#   16: stds.plt.r
#   17: survey.ts.r
#   18: shf.plt.r
#   19: contour.gen.r
#   20: restrat.wps.r
#   21: survey.dat.r
#   22: shwt.plt.r
#   23: sprSurv.r
#   24: BBNsurveyObj.r
#   25: clap3.plt.r
#   26: clap.plt.r
#   27: fishery.dat.r
#   28: fishsum.plt.r
#   29: gridPlot.r
#   30: GBcfData.r
#   31: surveyObj.r
##
###############################################################################################################


##############################################################################################################
################################### SECTION 1 SECTION 1 SECTION 1 ############################################
#################################### LOAD PACKAGES, LOAD FLAT FILES (where possible) #########################
################################### PRE-PROCESS DATA (where possible)               #########################
################################### SECTION 1 SECTION 1 SECTION 1 ############################################
##############################################################################################################
# NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:# NOTE:    
# If you have run this once and saved "Survey_preprocessed.R" you can skip SECTION 1 and just load that workspace
# NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:  # NOTE:# NOTE:    

################################### Start Load Packages and Functions #########################################
# Step 1:  DK August 17, 2015.  We could make it so that we load all required packages and functions immediately
#
##########################################################################################################
# First load required packages
require(PBSmapping)
require(RColorBrewer)


############################# GENERAL DATA ########################################################
############################# GENERAL DATA ########################################################
# Enter here standard data which is used throughout this script.
atow<-800*2.4384/10^6 # area of standard tow in km2
# The year of survey data you want to pull. The Sys.time gives the current year, just subtract from that to get the year of interest
yr = as.numeric(format(Sys.time(), "%Y"))  # 
years = 1984:yr # The years of interest for the bank, may be overwritten on specific banks...
# This function is now hopefully completely portable but for the SQL calls which will require you to be
# able to access the SQL database.  By copying the "Assessmnet_fns" and all subfolders to a specified directory
# this entire program should work.
#direct = "Y:/Offshore scallop/Assessment/"
direct = "d:/r/"
#direct = "e:/fn/"
############################# END GENERAL DATA ########################################################




############################# LOAD DATA IF YOU HAVE ALREADY RUN SCRIPT ########################################################
# HEADS UP HERE TIME SAVER!!! # # HEADS UP HERE TIME SAVER!!! # # HEADS UP HERE TIME SAVER!!! #
# If you have already saved the data for a year of interest you can run one of these two lines..
# HEADS UP HERE TIME SAVER!!! # # HEADS UP HERE TIME SAVER!!! # # HEADS UP HERE TIME SAVER!!! #

# If you have saved the "Pre-processed data saved (i.e all of Section 1) you can load all of that data
# by running this line of code, you can then proceed directly to the bank of interest and start running that analysis.
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.R",sep=""))
# You may need to reload your R profile if you use it...
#source("d:/r/.Rprofile")

### NOTE IF YOU HAVE RUN EITHER OF THE ABOVE LOAD CALLS YOU CAN SKIP the REST OF SECTION 1!!!!!!!


############################# LOAD FUNCTIONS ########################################################

# Now load all functions in the program in one location.  All calls to these functions are linked via the commented source #
# so we can easily tie the function call to the script for that function.
# The  functions are in this directory unless explicitly specified
# These 8 functions are pre-processing functions used to bring in and arrange various pieces of data
source(paste(direct,"Assessment_fns/import.survey.data.r",sep="")) #Source1
source(paste(direct,"Assessment_fns/SpringSurveyData_2011.r",sep="")) #Source2 
source(paste(direct,"Assessment_fns/summerSurveyData_2011.r",sep="")) #Source3 
source(paste(direct,"Assessment_fns/SpringSurveyData_2012.r",sep="")) #SOurce4 
source(paste(direct,"Assessment_fns/SummerSurveyData_2012.r",sep="")) #Source5 
source(paste(direct,"Assessment_fns/get.offshore.survey.r",sep="")) #Source6  sources the views Jessica created to do the calculations.
source(paste(direct,"Assessment_fns/import.hyd.data.r",sep="")) #Source7
source(paste(direct,"Assessment_fns/logs_and_fishery_data.r",sep=""))#Source8 

# These are the functions used to within the heart of the code to make stuff happen
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) # Source9 Revised by DK September 2015
source(paste(direct,"Assessment_fns/getdis.r",sep="")) #SOurce10
source(paste(direct,"Assessment_fns/shwt.lme.r",sep="")) #Source11
source(paste(direct,"Assessment_fns/shwt.plt1.r",sep="")) #Source12
source(paste(direct,"Assessment_fns/condFac.r",sep="")) #Source13
source(paste(direct,"Assessment_fns/surv.by.tow.r",sep="")) #Source14 Revised by DK September 2015
source(paste(direct,"Assessment_fns/simple.surv.r",sep="")) #Source15 Revised by DK September 2015
source(paste(direct,"Assessment_fns/stdts.plt.R",sep="")) #Source16
source(paste(direct,"Assessment_fns/survey.ts.r",sep=""),local=T) #Source17 Revised by DK September 2015
source(paste(direct,"Assessment_fns/shf.plt.r",sep="")) #Source18
source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) #Source19 Revised by DK September 2015
source(paste(direct,"Assessment_fns/restratwp.r",sep=""),local=T) #Source20
source(paste(direct,"Assessment_fns/survey.dat.r",sep="")) #Source21 Revised by DK September 2015
source(paste(direct,"Assessment_fns/shwt.plt.r",sep="")) #Source22
source(paste(direct,"Assessment_fns/sprSurv.r",sep="")) #Source23
source(paste(direct,"Assessment_fns/Clap3.plt.R",sep="")) #Source25
source(paste(direct,"Assessment_fns/Clap.plt.R",sep="")) #Source26
source(paste(direct,"Assessment_fns/fishery.dat.r",sep="")) #Source27 Revised by DK September 2015
source(paste(direct,"Assessment_fns/fishsum.plt.r",sep="")) #Source28
source(paste(direct,"Assessment_fns/gridPlot.r",sep="")) #Source29

################################## End Load Functions   #######################################################



################################### START LOAD & PRE-PROCESS DATA ############################################
# Step 2:  Load Data:  Where possible it would be nice to load all data here, likely instances
# where this won't work, but we should clearly identify where we are bringing in data and
# explain why we do it that way (e.g. a SQL call vs. flat file), perhaps a group decision on what our 
# philosophy should be.
##########################################################################################################



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


# GETS OFFSHORE DATA FROM VARIOUS SOURCES AND PRODUCES INDUSTRY REPORT
# Imports and manipulates older data from flat files stored on "Y:/Data",  
#exports working data to data folder of working directory
# survey data

  
		# This creates an object (and also exports the data to a csv file) containing the 
    # May and August survey data from 1984 until 2010.  Given these files exist and are no longer being
    # updated why don't we just import the flat files and what are we exporting new flat files every time?
    # We also need to think about where these are stored
    # I know the data is never changing but don't like removing numbered columns rather than as below removing named columns
    #Source1 source("fn/import.survey.data.r")
    # I believe for any surveys before 1998 we don't have the date information.
    # DK NOTE:  I was worried that we were mixing up fathoms and meters
    # with the next few pieces of data, but as best I can tell the data
    # in the flat files coming from import.survey.data are already in meters, e.g. look at plot(survMay.dat$depth~survMay.dat$year)
    # and plot plot(survAug.dat$depth~survAug.dat$year) in this latter one it does appear we stop sampling deep from 1990 onwards
    # which worries me about potential for biasing results a bit but nothing pathological I don't imagine.
    survMay.dat<-import.survey.data(1984:2010,survey='May',explore=T,export=F,dirc=direct)
    survAug.dat<-import.survey.data(1981:2010,survey='Aug',explore=T,export=F,dirc=direct)
		
    # Here we are subseting these data and getting rid of totwt and baskets bearing and distance coefficient
    survMay.dat<-survMay.dat[which(!names(survMay.dat)%in%c("dis","brg",'totwt','baskets'))]
    survAug.dat<-survAug.dat[which(!names(survAug.dat)%in%c("dis","brg",'totwt','baskets'))]
    # Using month as an indicator of whether data is spring or summer, so these might be off by a month but will do the trick.
    survMay.dat$month <-  5
    survAug.dat$month <-  8
    
    # The next function is simply used to organize the Spring survey data for 2011, the rest is in the database now, yah!
		# Using function from source("d:/Offshore scallop/Assessment/2011/r2/SpringSurveyData.r")
		Springsurv2011<-getSpringSurveyData2011(direct=direct)
		Springsurv2011$MWs$month <- 5
		Springsurv2011$SHF <- Springsurv2011$SHF[which(!names(Springsurv2011$SHF)%in%c("dis","brg"))]
		Springsurv2011$SHF$month <-  as.numeric(format(Springsurv2011$SHF$date, "%m"))
		# From 2013 - Here we are getting the data directly from the SQL server.
		#Source6 source("fn/get.offshore.survey.r") Get the data directly from SQL
	  # DK Note, I still need to get access to OSTOWS table for this to work
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
		all.surv.dat <- rbind(survMay.dat,survAug.dat,Springsurv2011$SHF,SurvDB$SHF)
		all.surv.dat$survey[all.surv.dat$month < 7] <- "spring"
		all.surv.dat$survey[all.surv.dat$month > 6] <- "summer"
		# Make German and BBn all spring too, 2015 did occur in summer, but these are spring surveys so makes the below work easier.
		all.surv.dat$survey[all.surv.dat$bank %in% c("Ger","BBn")] <- "spring"
		
		# This makes sure that ALL the data have the lat/long calculated in the same way
		all.surv.dat$lon<-with(all.surv.dat,apply(cbind(elon,slon),1,mean))
		all.surv.dat$lat<-with(all.surv.dat,apply(cbind(elat,slat),1,mean))
		
		# THe MW data is from 2011 to current
		MW.dat.new <- rbind(Springsurv2011$MWs,SurvDB$MWs)
		
		#Source7 	source("fn/import.hyd.data.r") Hydration sampling data which was stopped in 2010
		# Note that the summer 2010 mw-data is still included in this even though there was no hydration sampling done
		# that year, just easier
		MW.dat<-import.hyd.data(yrs=1982:2010, export=F,dirt=direct)
		
		# For survey TE10 things are a bit messy, so we need to do this bit to get the positions
		# dates, and depth into the data.
		surv.TE10 <- subset(all.surv.dat,cruise=="TE10" & state == "live")
		surv.TE10$date <- as.Date(surv.TE10$date, "%Y-%m-%d")
	  surv.TE10$day <- format(surv.TE10$date, "%d")
	  surv.TE10$month <- format(surv.TE10$date, "%m")

		# Now we can add in the lat/lon depth and day/month information for the tows
		for(x in 1:length(surv.TE10$tow))
		  {
		    MW.dat[MW.dat$cruise=="TE10" & MW.dat$tow == as.character(surv.TE10$tow[x]),c("lon","lat","depth","day","month")] <- 
	      surv.TE10[x,c("lon","lat","depth","day","month")]
		  } # end for(x in 1:length(surv.TE10$tow))
		
		#Source8, source(paste(direct,"Assessment_fns/logs_and_fishery_data.r") 
		# This sources SQL so be sure you have un/pw to access the db.
		logs_and_fish(loc="offshore",year = 1981:yr,un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
		# The returned data matches exactly what the old function returned so now we can merge the functions, if you
		# get any NA's related warnings it may be something is being treated as a Factor in one of the two files.  
		# This should combine without any warnings so check that.
		fish.dat<-merge(new.log.dat,old.log.dat,all=T)
		fish.dat$ID<-1:nrow(fish.dat)
		
		

		###############################################################################################################
		################################## END DATA PRE-PROCESSING ################################## 

		# Optionally output all of the data into an worksapce object you can call back in so that you don't need to 
		# Run all of these steps every time.  Make sure you remove your SQL UN and PWD from the data first...
		rm("un.ID","pwd.ID")
		save.image(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.R",sep=""))

		
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
surveys <- as.character(unique(all.surv.dat$surv.bank))
num.surveys <- length(surveys)
bank.dat <- NULL
strata.mis.match <- NULL
mw <- NULL
SpatHtWt.fit <- NULL
mw.dat.all <- NULL
cf.data <- NULL
surv.dat <- NULL
surv.Clap <- NULL
surv.Live <- NULL
surv.Rand <- NULL
survey.obj <- NULL
clap.survey.obj <- NULL
SS.summary <- NULL
SHF.summary <- NULL
com.contours <- NULL
rec.contours <- NULL
pre.contours <- NULL
CF.current <- NULL
cf.contours <- NULL
mc.contours <- NULL
clap.contours <- NULL
tow.dis <- NULL
seedbox.obj <- NULL


# Now get the survey summary results for all the banks...
for(i in 1:num.surveys)
{
#  So first thing to do is get the data for the bank....
bnk <- as.character(unique(subset(all.surv.dat,surv.bank==surveys[i])$bank))
bank.dat[[bnk]] <- subset(all.surv.dat,surv.bank==surveys[i])
RS <- size.cats$RS[size.cats$Bank == bnk]
CS <- size.cats$CS[size.cats$Bank == bnk]
yr <- max(bank.dat[[bnk]]$year,na.rm=T)
years <- 1984:yr
# Remove years in which we don't have good data for specific banks, 1984 very problematic with clappers/live data.
if(bnk == "Ger" || bnk == "Mid" || bnk == "Sab") years <- 1985:yr
if(bnk == "BBs") years <- c(1985,1986,1988:yr)
if(bnk == "BBn") years <- c(1991:yr)
bank.dat[[bnk]] <- subset(bank.dat[[bnk]] , year %in% years)

#####################################  TOW TRACKS, THIS SECTION MAY NEED ADJUSTED EACH YEAR ##########################
# The tow tracks remain messy, but I don't see a way around this
# Next we will input all of the tow tracks for the banks, this is messy, but can't see a way around it..
# TOW TRACK DATA MIDDLE BANK
# Need to input the number of tows, file location, weighting scheme, seconds between readings (rule) 
# #Source10 source("fn/getdis.r")  
if(bnk == "Mid") tow.dis[[bnk]] <-dist.coef(1:15,path=paste(direct,"data/Tow_tracks/",yr,"/Spring/Middle/",sep=""),w=c(1:10,9:1),
                  rule=8,smooth=T,plt=F,meh=1000)
# TOW TRACK DATA SABLE BANK
# Need to input the number of tows, file location, weighting scheme, seconds between readings
#Source10 source("fn/getdis.r") 
if(bnk == "Sab") tow.dis[[bnk]] <- dist.coef(1:100,path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/Sable/",sep=""),
                                           w=c(1:10,9:1),rule=8,smooth=T,plt=F)

# TOW TRACK DATA GERMAN BANK
# Need to input the number of tows, file location, weighting scheme, seconds between readings
#Source10 source("fn/getdis.r") 
if(bnk == "Ger") tow.dis[[bnk]]<-dist.coef(c(404,405,444,446,447,458,472,c(407:415),c(417:423),c(425:431),
                    c(433:436),c(439:442),c(449:451),c(454:456),c(460:468), c(474:480),1:20),
                  path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/German/",sep=""), w=c(1:10,9:1),rule=8,smooth=T,plt=F)

# I don't have BBs example will need to add perhaps whenever we get back out there.

# Tow tracks for BBn

# TOW TRACK DATA BBn BANK Need to input the number of tows, file location, weighting scheme, seconds between readings
#Source10 source("fn/getdis.r") 

if(bnk == "BBn")
  {
reg<-dist.coef(c(1:100),path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/BBn/",sep=""),
                  w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000)
extra<-dist.coef(c(950:970),path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/BBnextras/",sep=""),
                         w=c(1:10,9:1),rule=8,smooth=T,plt=F)

tow.dis[[bnk]] <- as.data.frame(rbind(reg,extra))
  } # end if(bnk == "BBn")

#Source10 source("fn/getdis.r") Calculate the tow track distances for Georges Spring survey.  
if(bnk == "GB") tow.dis[[bnk]] <- dist.coef(subset(bank.dat[[bnk]],year==yr & state=='live')$tow,
                    path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/GB/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)


#Source10 source("fn/getdis.r") Tow distances in GBa and GBb and with extra tows
if(bnk == "GBa")
  {
    reg<-dist.coef(c(1:200),path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBa/",sep=""),
                  w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000)
    extra<-dist.coef(c(910:915),
                       path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBa/",sep=""),
                       w=c(1:10,9:1),rule=8,smooth=T, plt=F,meh=1000)
    tow.dis[[bnk]] <- as.data.frame(rbind(reg,extra))
    
  } # end if(bnk == "GBa")


if(bnk == "GBb") tow.dis[[bnk]] <- dist.coef(301:330,path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBb/",sep=""),
                  w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000)



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

		# MEAT WEIGHT DATA from 2011-2014
		# Get the mw data from 2011 to this year
    if(bnk != "GB" && bnk != "GBa" && bnk != "GBb") mw[[bnk]] <- subset(MW.dat.new,bank==bnk)
    if(bnk=="GB")              mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GB","GBa","GBb") & month < 7)
    if(bnk=="GBa")             mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GBa")& month > 6)
    if(bnk=="GBb")             mw[[bnk]] <- subset(MW.dat.new,bank %in% c("GBb")& month > 6)
		
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
        cf.data[[bnk]]<-condFac(mw.dat.all[[bnk]],bank.dat[[bnk]],model.type='gam_d')
      } # end if(bnk == "Mid")
		
		if(bnk != "Mid")
		  {
		    mw.tmp <- subset(MW.dat,bank==bnk)
		    mw.tmp$ID <- paste(mw.tmp$year,mw.tmp$tow,sep='.')
		    # Grab the relavent Meat-Weight Shell height data and make a flat file from it
		    mw.dat.all[[bnk]] <- merge(subset(mw.tmp, month %in% 5:6 & year %in% years, 
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
		surv.dat[[bnk]]<-cf.data[[bnk]]$pred.dat
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
		if(bnk %in% c("Mid","Ger","BBn","GB","GBa","GBb")) 
		 {
		  surv.dat[[bnk]]<- surv.by.tow(surv.dat[[bnk]], years, pre.ht=(RS+5), rec.ht=(CS+5))
		  surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=(RS+5), rec.ht=(CS+5), type='B', mw.par="CF")
		 }
		
		if(bnk == "Sab" || bnk == "BBs" ) 
		  {
		    surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=(RS+5), rec.ht=(CS+5))
		    surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=(RS+5), rec.ht=(CS+5), type='B', mw.par="CFh")
		  }
		
		
		# On Georges spring we need to tidy up some of the randoms..
		if(bnk == "GB")
		{
		  surv.dat[[bnk]]$random[surv.dat[[bnk]]$year < 2013] <-4
		  surv.dat[[bnk]]$random[surv.dat[[bnk]]$tow %in% c(1:24,301:324)] <-3
		  surv.dat[[bnk]]$random[surv.dat[[bnk]]$tow > 12 & surv.dat[[bnk]]$year == 1988] <- 4
		} # end if(bnk == "GB")
		
		# Subset the data into the clappers (dead) and live scallops.  Use only random survey tows for Clappers...
		# For GB spring the survey of interest are the comparative tows...
		surv.Clap[[bnk]]<-subset(surv.dat[[bnk]],state=='dead'& random==1)
		if(bnk == "GB") surv.Clap[[bnk]]<-subset(surv.dat[[bnk]],state=='dead'& random==3)
		surv.Live[[bnk]]<-subset(surv.dat[[bnk]],state=='live')
		surv.Rand[[bnk]]<-subset(surv.dat[[bnk]],state=='live' & random==1)		
		if(bnk == "GB") surv.Rand[[bnk]]<-subset(surv.dat[[bnk]],state=='live' & random==3)	
		#########  Now calculate the clappers...
		
		# Clappers the banks for each size class
		
		surv.Clap[[bnk]]$clap.prop<-surv.Clap[[bnk]]$tot/(surv.Rand[[bnk]]$tot+surv.Clap[[bnk]]$tot)*100
		surv.Clap[[bnk]]$clap.prop[is.na(surv.Clap[[bnk]]$clap.prop)]<-0
		surv.Clap[[bnk]]$clap.propCom<-surv.Clap[[bnk]]$com/(surv.Rand[[bnk]]$com+surv.Clap[[bnk]]$com)*100
		surv.Clap[[bnk]]$clap.propCom[is.na(surv.Clap[[bnk]]$clap.propCom)]<-0
		surv.Clap[[bnk]]$clap.propRec<-surv.Clap[[bnk]]$rec/(surv.Rand[[bnk]]$rec+surv.Clap[[bnk]]$rec)*100
		surv.Clap[[bnk]]$clap.propRec[is.na(surv.Clap[[bnk]]$clap.propRec)]<-0
		surv.Clap[[bnk]]$clap.propPre<-surv.Clap[[bnk]]$pre/(surv.Rand[[bnk]]$pre+surv.Clap[[bnk]]$pre)*100
		surv.Clap[[bnk]]$clap.propPre[is.na(surv.Clap[[bnk]]$clap.propPre)]<-0
		surv.Clap[[bnk]]$clap.prop[is.na(surv.Clap[[bnk]]$clap.prop)]<-0
		
		
		  
		# Using the Live scallops only make the Middle Bank survey object
		#Source15 source("fn/simple.surv.r") prepare survey index data obj
		if(bnk == "Mid") 
		  {
		    survey.obj[[bnk]]<-simple.surv(surv.Live[[bnk]],years=years)
		    clap.survey.obj[[bnk]]<-simple.surv(surv.Clap[[bnk]],years=years)
		  } #end 	if(bnk == "Mid")  
		# And here is Georges Bank spring survey results
		if(bnk == "GB")  
		  {
		    survey.obj[[bnk]]<-simple.surv(surv.Live[[bnk]],years=1990:yr, random %in% c(1,3),)
		    clap.survey.obj[[bnk]]<-simple.surv(surv.Clap[[bnk]],years=1990:yr, random %in% c(1,3),)
		  } # end 	if(bnk == "GB")  
		# For German bank we have the matched survey design which leads to some unique processing to get survey results.
		if(bnk == "Ger")
		  {
		  
		  # Survey results
		  # On German we have a repeated measures survey design (we repeat stations each year) 
		  lined.dat<-rbind(subset(surv.Live[[bnk]],year==2008 & stratum==2),subset(surv.Live[[bnk]],year > 2008))
		  surv.Clap[[bnk]]<-rbind(subset(surv.Clap[[bnk]],year==2008 & stratum==2),subset(surv.Clap[[bnk]],year > 2008))
		  
		  ### WHICH TOWS ARE MATCHED WITH WHICH!!!!
		  #new.matched <- subset(surv.Live[[bnk]], year==yr & random==3,c("tow","slon","slat","lat","lon","random","year"))
		  new.ger.tows <- subset(surv.Live[[bnk]], year==yr,c("tow","slon","slat","lat","lon","random","year"))
		  new.ger.tows$EID <- 1:nrow(new.ger.tows)
		  last.ger.tows <- subset(surv.Live[[bnk]], year==yr-1)
		  # Now get all the tows that appear to overlap and they are our matched tows, search on slat and lat.
		  for(k in 1:nrow(new.ger.tows))
		   {
		    if(any(round(last.ger.tows$slat,digits=2) == round(new.ger.tows$slat[k],digits=2) & 
		           round(last.ger.tows$slon,digits=2) == round(new.ger.tows$slon[k],digits=2)))
		      new.ger.tows$EID[k] <- last.ger.tows$tow[round(last.ger.tows$slat,digits=2) == round(new.ger.tows$slat[k],digits=2) & 
		                                                 round(last.ger.tows$slon,digits=2) == round(new.ger.tows$slon[k],digits=2)]
		    if(any(round(last.ger.tows$lat,digits=2) == round(new.ger.tows$lat[k],digits=2) & 
		           round(last.ger.tows$lon,digits=2) == round(new.ger.tows$lon[k],digits=2)))
		      new.ger.tows$EID[k] <- last.ger.tows$tow[round(last.ger.tows$lat,digits=2) == round(new.ger.tows$lat[k],digits=2) & 
		                                                 round(last.ger.tows$lon,digits=2) == round(new.ger.tows$lon[k],digits=2)]
		   } # end for(i in 1:nrow(new.ger.tows))
		  
		  # Now this won't be perfect, should get most but not all of them so check the results over.
		  # Now replace all the ones that were not matched tows with a small number.
		  new.ger.tows$EID[new.ger.tows$random == 1] <- 1:length(new.ger.tows$EID[new.ger.tows$random==1] )
		  new.ger.tows$stratum <- new.ger.tows$random
		  new.ger.tows$stratum[new.ger.tows$stratum == 3] <- 2
		  new.ger.tows <- subset(new.ger.tows,select = c("tow","EID","lon","lat","stratum","year"))
		  
		  # A list of the tows for 2009-2014 in German Bank, used for sprSurv function
		  ger.tow.list <- read.csv(paste(direct,"data/Survey_data/",yr-1,"/Spring/Ger/towlist_2009-",yr-1,".csv",sep=""))
		  
		  # Update the list with the new matched tows
		  ger.tow.list <- rbind(ger.tow.list,new.ger.tows)
		  
		  # Create a new file with the updated towlist for German Bank with the current years data, used for sprSurv function
		  write.csv(ger.tow.list,paste(direct,"data/Survey_data/",yr,"/Spring/Ger/towlist_2009-",yr,".csv",sep=""))
		  
		  # This gets us the overall estimates for the bank, but it doesn't get the shell height frequency data we need
		  # but in 2011 the survey design was not set up for this so we'll need to grab that data from the lined.survey.obj
		  spr.survey.obj<-sprSurv(lined.dat,2008:yr,ger.tow.list,chng=T)
		  
		  # prepare survey index data obj
		  #Source15 source("fn/simple.surv.r")
		  unlined.dat<-rbind(subset(surv.Live[[bnk]],year<2007),subset(surv.Live[[bnk]],year %in% 2007:2008 & stratum==1))
		  # Going to use the unlined survey as the survey.obj, the matched/lined survey specialness will get it's own names
		  survey.obj[[bnk]]<-simple.surv(unlined.dat,years=1983:2008)
		  lined.survey.obj<-simple.surv(lined.dat,years=2008:yr)
		  clap.survey.obj[[bnk]]<-simple.surv(surv.Clap[[bnk]],years=2008:yr)
		  
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
		    survey.obj[[bnk]] <- survey.dat(surv.Rand[[bnk]], RS=(RS+5), CS=(CS+5), 
		                                bk=bnk, areas=strata.areas, mw.par="CF")	
		    clap.survey.obj[[bnk]] <- survey.dat(surv.Clap[[bnk]],SpatHtWt.fit[[bnk]], RS=(RS+5), CS=(CS+5), 
		                                  bk=bnk, areas=strata.areas, mw.par="CF")		
	
		    survey.obj[[bnk]][[1]]$CF <- na.omit(sapply(1:length(years),
		                                    function(x){with(subset(surv.Rand[[bnk]],year == years[x]),
		                                                     weighted.mean(CF,com.bm,na.rm=T))}))
		    survey.obj[[bnk]][[1]]$clappers<-clap.survey.obj[[bnk]][[1]]$N
		    survey.obj[[bnk]][[1]]$clappersR<-clap.survey.obj[[bnk]][[1]]$NR
		   
		  } # end if(bnk != "Ger" && bnk != "Mid")
		  
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
		CF.current[[bnk]]<-merge(CF.current[[bnk]],subset(surv.Live[[bnk]],year==yr,c('year','tow','lon','lat',
		                                                                   paste('h',seq(5,200,5),sep=''))))
		
		#Source14 source("fn/surv.by.tow.r") surv.by.tow calculates number and/or biomass of pre, rec and 
		#com size scallops in each tow + meat count.
		CF.current[[bnk]] <-surv.by.tow(CF.current[[bnk]], yr, pre.ht=(RS+5), rec.ht=(CS+5), type='ALL', mw.par="CF")
		CF.current[[bnk]]<- surv.by.tow(CF.current[[bnk]], yr, pre.ht=(RS+5), rec.ht=(CS+5), type='B', mw.par="CF")

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
		
} # end loop
		
##########################################################################################################################
# Now that we've done everything we can save all the results or we could skip ahead and just load them all if already run.
		
		save.image(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.R",sep=""))



	