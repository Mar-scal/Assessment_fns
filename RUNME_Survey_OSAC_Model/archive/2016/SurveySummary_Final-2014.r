################################################################################################################
##### This is the script for accessing the survey data and producing the summary figures tables, (WHAT ELSE)
####  Commented and checked by DK starting on July 24, 2015.
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
yr = as.numeric(format(Sys.time(), "%Y")) -1 # 
years = 1984:yr # The years of interest for the bank, may be overwritten on specific banks...
# This function is now hopefully completely portable but for the SQL calls which will require you to be
# able to access the SQL database.  By copying the "Assessmnet_fns" and all subfolders to a specified directory
# this entire program should work.
#direct = "Y:/Offshore scallop/Assessment/"
direct = "d:/r/"
#direct = "e:/fn/"
# Set up objects for plot labels.  
N.tow.lab <- expression(bgroup("(",N/tow,")"))
cf.lab <-    expression(paste("CF: ",bgroup("(",g/dm^3   ,")")))
mc.lab <-    expression(paste("MC: ",bgroup("(",N/"500 g",")"))) 

# I don't think I want this for the survey...
#survey.lab <- expression(paste("Strata: ",bgroup("(",frac(N,tow),")"))) 
# Set up default colors for our spatial plot for survey strata, Tow Number, Condition Factor, and Meat Count.
N.col <- "YlGn"
cf.col <- "YlOrBr"
mc.col <- "Spectral"
X.lvl <- "PuBuGn" # For any time we have an usually large event this is the color of the extra levels required.
clap.col <- "YlOrRd"
############################# END GENERAL DATA ########################################################


############################# LOAD FUNCTIONS ########################################################

# Now load all functions in the program in one location.  All calls to these functions are linked via the commented source #
# so we can easily tie the function call to the script for that function.
# The  functions are in this directory unless explicitly specified
# These 8 functions are pre-processing functions used to bring in and arrange various pieces of data
source(paste(direct,"Assessment_fns/import.survey.data.r",sep="")) #Source1
source(paste(direct,"Assessment_fns/SpringSurveyData_2011.r",sep="")) #Source2 
source(paste(direct,"ssessment_fns/summerSurveyData_2011.r",sep="")) #Source3 
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


# Optional loads
# These 3 need to be produced before they really are of any use, so don't load them here unless you know what they are! 
source(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/BBnsurveyObj.R",sep="")) #Source24
source(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/GBcfData.R",sep=""))     #Source30
source(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/surveyObj.R",sep=""))    #Source31 
source(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/surveyObj.R",sep=""))    #Source31 
################################## End Load Functions   #######################################################



################################### START LOAD & PRE-PROCESS DATA ############################################
# Step 2:  Load Data:  Where possible it would be nice to load all data here, likely instances
# where this won't work, but we should clearly identify where we are bringing in data and
# explain why we do it that way (e.g. a SQL call vs. flat file), perhaps a group decision on what our 
# philosophy should be.
##########################################################################################################



################# LOAD GENERAL FLAT FILES ###################################################################
# Here we are grabbing the available temperature data from several temperature database flat files.
# DK August 17 2015 PROBLEM/NOTE:  The data files from which I am pulling below are from 2013, these 
# must have been accidently placed in the 2014 directory at some point.  Will need to re-upload the correct temperature data
# from whatever database we found them in.
#Read1 - Pulling in multiple data tables to make one object
temps<-list()
for(i in 1:length(dir(paste(direct,"data/Temperature_data/",yr,sep=""))))
    {
  temps[[i]]<-read.table(paste(direct,"data/Temperature_data/",yr,"/",
                               dir(paste(direct,"data/Temperature_data/",yr,sep=""))[i],collapse="",sep=""),
                         header=T,stringsAsFactors = F) # DK added August 17, 2015.
    } # end for(i in 1:length(dir(paste(direct,"data/Temperature_data/",yr,sep="")))

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

###############################################################################################################
################################## End LOAD FLAT FILES ################################## 


###############################################################################################################
################################## BEGIN DATA PRE-PROCESSING ################################## 


# GETS OFFSHORE DATA FROM VARIOUS SOURCES AND PRODUCES INDUSTRY REPORT
# Imports and manipulates older data from flat files stored on "Y:/Data",  
#exports working data to data folder of working directory
# survey data

  # Will start by Unwrapping the temperature data (see Read call above) and give the banks the correct names
  temp.dat<-do.call("rbind",temps)
  names(temp.dat)<-c("tow","mean.temp", "sd.temp","bank","cruise")
  temp.dat$bank[temp.dat$bank=="BBN"]<-"BBn"
  temp.dat$bank[temp.dat$bank=="BBS"]<-"BBs"
  temp.dat$bank[temp.dat$bank=="GER"]<-"Ger"
  temp.dat$bank[temp.dat$bank=="MID"]<-"Mid"
  temp.dat$bank[temp.dat$bank=="SAB"]<-"Sab"
  temp.dat$bank[temp.dat$bank=="BANQ"]<-"Ban"
  
  # before we can turn the tow numbers into numbers there are a few we need to clean up, first make data numeric
  # if there are NA's produced here probably need to remove a character string (e.g. gsub("M",""))
  temp.dat$tow<-as.numeric(temp.dat$tow)
  # GBb data needs to have 300 added to tow numbers to align with other data.
  temp.dat$tow[552:581] <- temp.dat$tow[552:581] + 300
  
  # Note if this gives a warning message that NA's are produced check the tow data to ensure there are no "letters" in the string.
  # This may have NA's if there were tows with no data recorded
  temp.dat$mean.temp<-as.numeric(temp.dat$mean.temp)
  temp.dat$sd.temp<-as.numeric(temp.dat$sd.temp)
   
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
		
    # Here we are subseting these data and getting rid of totwt and baskets.
    survMay.dat<-survMay.dat[which(!names(survMay.dat)%in%c('totwt','baskets'))]
    survAug.dat<-survAug.dat[which(!names(survAug.dat)%in%c('totwt','baskets'))]
    
		# The next 4 founctions are simply used to organize the Spring/Summer survey data from 2011 and 2012.
		# Using function from source("d:/Offshore scallop/Assessment/2011/r2/SpringSurveyData.r")
		Springsurv2011<-getSpringSurveyData2011(direct=direct)
		#Source3 source("summerSurveyData.r")
		Summersurv2011<-getSummerSurveyData2011(MWs=T,direct=direct)
		#Source4 source("d:/Offshore scallop/Assessment/2012/r/SpringSurveyData.r")
		Springsurv2012<-getSpringSurveyData2012(direct=direct)
		#Source5 source("d:/Offshore scallop/Assessment/2012/r/summerSurveyData.r")
		Summersurv2012<-getSummerSurveyData2012(MWs=T,direct=direct)
		
		# From 2013 - Here we are getting the data directly from the SQL server.
		#Source6 source("fn/get.offshore.survey.r") Get the data directly from SQL
	  # DK Note, I still need to get access to OSTOWS table for this to work
		SurvDB<-get.offshore.survey(db.con ="ptran", un=scal.un , pw = scal.pw)
		
   	# New object lined up nicely so we can now make the names consistent between the different survey data.
		names(SurvDB$MWs)<-names(Springsurv2012$MWs)
		SurvDB$MWs$ID<-with(SurvDB$MWs,paste(cruise,tow,sep='.'))
	 
		# DK NOTE August 21, 2015: Now what about the shell heights, everything looks o.k. here, except that the old 
		# data (2012 and earlier) Has T/F, while the new data has 1-5 depending on the tow type. 
		names(SurvDB$SHF)<-names(Springsurv2012$SHF)
		SurvDBSHF <- SurvDB$SHF
		
		Springsurv2013SHF<-subset(SurvDBSHF,cruise=="TE15")
		Summersurv2013SHF<-subset(SurvDBSHF,cruise=="TE16")
		Springsurv2014SHF<-subset(SurvDBSHF,cruise=="TE17")
		Summersurv2014SHF<-subset(SurvDBSHF,cruise=="TE18")
		Summersurv2015SHF<-subset(SurvDBSHF,cruise=="LE02")
		
		#Write1 Output the data to flat files, likely to be used in next years iteration of this program 
		#write.csv(Springsurv2014SHF,paste(direct,"/Data/Survey_data/", yr,"/Spring/Springsurv",yr,"SHF.csv",sep=""),row.names=F)
		#Write2
		write.csv(Summersurv2015SHF,paste(direct,"/Data/Survey_data/",yr,"/Summer/Summersurv",yr,"SHF.csv",sep=""),row.names=F)
	
		
		# Now combine all these objects into one object with all the spring data and one object with all the summer data.
		# Note that this coerces the T/F data found in the random column to be 0's and 1's, this may not
		# be what we were expecting to happen here. TRUE, the assessment tows are assigned a 1, False, 
		# the experimental tows are assigned a 0 so all regular tows are labeled at a 1.
		
		springSurv.dat<-rbind(survMay.dat,Springsurv2011$SHF,Springsurv2012$SHF,Springsurv2013SHF,Springsurv2014SHF)
		summerSurv.dat<-rbind(survAug.dat,Summersurv2011$SHF,Summersurv2012$SHF,Summersurv2013SHF,Summersurv2014SHF,Summersurv2015SHF)
		# Make sure the cruise data characters not factors.
		springSurv.dat$cruise<-as.character(springSurv.dat$cruise)
		summerSurv.dat$cruise<-as.character(summerSurv.dat$cruise)
		
		# This makes sure that ALL the data have the lat/long calculated in the same way
		springSurv.dat$lon<-with(springSurv.dat,apply(cbind(elon,slon),1,mean))
		springSurv.dat$lat<-with(springSurv.dat,apply(cbind(elat,slat),1,mean))
		summerSurv.dat$lon<-with(summerSurv.dat,apply(cbind(elon,slon),1,mean))
		summerSurv.dat$lat<-with(summerSurv.dat,apply(cbind(elat,slat),1,mean))

		# Merge the Summer Survey data with the temperature data, do it as a left join, retain all the 
		#summerSurv.dat table, will fill unmatched rows with "NA"'s
		summerSurv.dat<-merge(summerSurv.dat,temp.dat,all.x=T)

		
		#Source7 	source("fn/import.hyd.data.r") Hydration sampling data which was stopped in 2010
		MW.dat<-import.hyd.data(yrs=1982:2010, export=F,direct=direct)
		
		#Source8, source(paste(direct,"logs_and_fishery_data.r") 
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
		rm("un.ID","pwd.ID","scal.un","scal.pw")
		save.image(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.R",sep=""))
		
		# Optionally if you have this image compiled just load it now.
		load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.R",sep=""))

		
		
######################## SPECIAL PLOTS ######################## SPECIAL PLOTS  #################################
######################## SPECIAL PLOTS ######################## SPECIAL PLOTS  #################################
## Slide 2 from Spring survey summary can be made here
windows(11,8.5)
ScallopMap(area="NL",plot.bathy=T,bathy.source = "quick",boundries = "offshore",shore="nwatlHR",
           bound.color = T,plot.boundries = T,label.boundries = F,offshore.names=T,xlab="",ylab="",
           title=paste("Offshore scallop spring survey results (",yr,")",sep=""),cex.mn=2,dec.deg = F,direct=direct)
######################## SPECIAL PLOTS ######################## SPECIAL PLOTS  #################################
######################## SPECIAL PLOTS ######################## SPECIAL PLOTS  #################################

		
		
		
################################################################################################################
################################### END SECTION 1 END SECTION 1#################################################
################################### END LOAD & PRE-PROCESS DATA ###############################################
################################### END SECTION 1 END SECTION 1#################################################
###############################################################################################################
		

		
				
		
		
################################################################################################################
################################ SECTION 2 MIDDLE BANK ########################################################
###  MIDDLE ###  MIDDLE ###  MIDDLE ###  MIDDLE ###  MIDDLE ###  MIDDLE ###  MIDDLE ###  MIDDLE ###  MIDDLE 
############################### SECTION 2 MIDDLE BANK ########################################################
############################################################################################################
		

##################  Bring in and ID the variables needed to create the plots ##################################
		# Here we enter in the basic data for the Middle Bank Survey
		# Shell height for knife edge recruitment based on portsampling data
		# The CS and RS specified here actually 5 higher than the actual shell heights
		CS = 95 # CS = Shell height for knife-edge recriutment   
		RS = 85 # RS = Shell height 1 year previous to CS
    bnk = "Mid" # In a move towards automation replace all "Mid" with bnk, do this for each bank...
    ## Set up plot titles
    survey.title <- substitute(bold(paste("Survey (",bank,"-",year,")",sep="")),
                               list(year=as.character(yr),bank=bnk))
    tow.track.title <- substitute(bold(paste("Tow tracks (",bank,"-",year,")",sep="")),
                                  list(year=as.character(yr),bank=bnk))
    fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
                                  list(a=as.character(CS-5),year=as.character(yr),bank=bnk))
    rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
                            list(a=as.character(CS-6),b=as.character(RS-5),year=as.character(yr),bank=bnk))
    pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
                                list(b=as.character(RS-5),year=as.character(yr),bank=bnk))
    cf.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
                           list(year=as.character(yr),bank=bnk))
    mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
                           list(m=as.character(CS-5),year=as.character(yr),bank=bnk))
    survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
                                    list(year=as.character(yr),bank=bnk))
    survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
                                     list(bank=bnk))
    SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
                             list(bank=bnk))
    MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-",year,")",sep="")),
                             list(year=as.character(yr),bank=bnk))
    CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank,")",sep="")),
                              list(year=as.character(yr),bank=bnk))
    clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-",year,")",sep="")),
                                 list(c=as.character(85),bank=bnk,year=as.character(yr)))
    clap.ts.title <- substitute(bold(paste("Clapper time series (",bank,")",sep="")),
                                list(bank=bnk))
    clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,")",sep="")),
                                    list(bank=bnk))
    
    
		# TOW TRACK DATA MIDDLE BANK
		# Need to input the number of tows, file location, weighting scheme, seconds between readings (rule) 
		# #Source10 source("fn/getdis.r")  
		MidDis<-dist.coef(1:15,path=paste(direct,"data/Tow_tracks/",yr,"/Spring/Middle/",sep=""),w=c(1:10,9:1),
		                  rule=8,smooth=T,plt=F,meh=1000)
		
		# Get the Middle bank survey boundary polygon this replaces #Read3
		Mid.survey.bound.poly<-subset(survey.bound.polys,label=bnk)
		
		# Here we get the spring survey data for Mid
		survMid.dat<-subset(springSurv.dat,bank==bnk)
		# Give each tow a unique identifier.
		survMid.dat$ID<-paste(survMid.dat$year,survMid.dat$tow,sep='.')
	
		# MEAT WEIGHT DATA MIDDLE BANKS from 2011-2014, stitch them together.
		mid14.mw<-subset(SurvDB$MWs,bank==bnk & year==yr)
		mid13.mw<-subset(SurvDB$MWs,bank==bnk & year==2013)
		mid12.mw<-subset(Springsurv2012$MWs,bank==bnk)
		mid11.mw<-subset(Springsurv2011$MWs,bank==bnk)
		mid.mw<-rbind(mid11.mw,mid12.mw,mid13.mw,mid14.mw)

		# Convert Shell height to decimeters for 2014
		mid14.mwdm<-mid14.mw
		mid14.mwdm$sh<-mid14.mw$sh/100
		# MEAT WEIGHT DATA - hydration sampling, it contains data from 1983-2007
		Mid.wgt.dat<-subset(MW.dat,bank==bnk)
		
		# MODEL - This is the meat weight Shell height realationship.  
		#MEAT WEIGHT SHELL HEIGHT RELATIONSHIP 2014 
		#Source11 source("fn/shwt.lme.r") note thtat the exponent is set as a parameter here b=3
		Mid.SpatHtWt.fit<-shwt.lme(mid14.mwdm,random.effect='tow',b.par=3)
		
		# Create and export a MW-SH object
		Midmw.dat<-merge(subset(Mid.wgt.dat,month %in% 5:6 & year > 1983,c("tow","year","lon","lat","depth","sh","wmw")),
		                 subset(mid.mw,select=c("tow","year","lon","lat","depth","sh","wmw")),all=T)
		Midmw.dat$ID<-paste(Midmw.dat$year,Midmw.dat$tow,sep='.')
		#Write3
		write.csv(Midmw.dat,paste(direct,"Data/Survey_data/",yr,"/Spring/Mid/Mid_mw_Data.csv",sep=""),row.names=F)
		
		## MODEL - This is the model used to esimate condition factor across Middle Bank
		#Source13 source("fn/condFac.r")
		# Due to the sparseness of the data for this bank the most complex model we can fit is a gam_d, 
		# data this is like far more complex still than the really allows for.
		MidcfData<-condFac(Midmw.dat,survMid.dat,model.type='gam_d')
		
		# Oubput the predictions for Middle Bank from the model
		survMid.dat<-MidcfData$pred.dat
		# Pull out the ID and condition factor
		tmp.dat<-subset(MidcfData$CF.data,select=c("ID","CF"))
		# Rename CF to CFh
		names(tmp.dat)[2]<-"CFh"
		# merge the two data sets, keeping all x values
		survMid.dat<-merge(survMid.dat,tmp.dat,all.x=T)
		# Replace any NA's in CFh with the original Condition Factor.
		survMid.dat$CFh[is.na(survMid.dat$CFh)]<-survMid.dat$CF[is.na(survMid.dat$CFh)]
		
		# Calculate the biomass of the Pre-recruits, Recruits and the Commerical Scallops in each tow on Middle bank.
		#Source14 source("fn/surv.by.tow.r") surv.by.tow calculates number or biomass of pre, rec and com size scallops in each tow
		survMid.dat<-surv.by.tow(survMid.dat, years, pre.ht=RS, rec.ht=CS, type='ALL', mw.par="CF")
		
		#Write4 - Output these results
		write.table(survMid.dat,paste(direct,"Data/Survey_data/",yr,"/Spring/Mid/Mid_Survey_1983-",yr,".csv",sep=""),
		            sep=',',row.names=F)
		
		# Subset the data into the clappers (dead) and live scallops.
		survMidClap.dat<-subset(survMid.dat,state=='dead')
		survMidLive.dat<-subset(survMid.dat,state=='live')
		
		# Using the Live scallops only make the Middle Bank survey object
		#Source15 source("fn/simple.surv.r") prepare survey index data obj
		survMid.obj<-simple.surv(survMidLive.dat,years=1984:yr)
		
		### GENERATE THE CONTOURS for each of the Middle bank distribtuion maps ###
		
		# COMMERICAL SIZED SCALLOP ON MIDDLE BANK
		#Source19 source("fn/contour.gen.r")
		com.contours<-contour.gen(subset(survMidLive.dat,year==yr,c('tow','lon','lat','com')),ticks=c(1,5,10,50,100),
		                          str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',
		                          title='fully recruited (>= 95 mm shell height)',color.fun=tim.colors,id.par=5,units='#/tow',
		                          plot=F,subscale=0.1,direct =direct)
		
		# PRE RECRUIT AND RECRUIT SIZED SCALLOP ON MIDDLE BANK
		survMidLive.dat$sub<-survMidLive.dat$pre+survMidLive.dat$rec
		#Source19 source("fn/contour.gen.r")
		pre.contours<-contour.gen(subset(survMidLive.dat,year==yr,c('tow','lon','lat','sub')),ticks=c(1,5,10,50,100),
		                          str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',
		                          title='fully recruited (>= 95 mm shell height)',color.fun=tim.colors,id.par=5,units='#/tow',
		                          plot=F,subscale=0.1,direct = direct)
		
		
	
		
		# MEAT COUNT & CONDITION FACTOR OF SCALLOP ON MIDDLE BANK (requires a bit of preprocessing)
		MidCF2014.dat<-na.omit(merge(subset(na.omit(SurvDB$pos),bank == bnk & year==yr,c('tow','lon','lat')),
		                             Mid.SpatHtWt.fit$fit))
		names(MidCF2014.dat)[4]<-"CF"
		MidCF2014.dat<-merge(MidCF2014.dat,subset(survMidLive.dat,year==yr,c('year','tow','lon','lat',
		                                                                       paste('h',seq(5,200,5),sep=''))))
		
		#Source14 source("fn/surv.by.tow.r") surv.by.tow calculates number and/or biomass of pre, rec and 
		#com size scallops in each tow + meat count.
		MidCF2014.dat<-surv.by.tow(MidCF2014.dat, yr, pre.ht=RS, rec.ht=CS, type='ALL', mw.par="CF")
		
		# CONDITION FACTOR and MEAT COUNT OF SCALLOP ON MIDDLE BANK in 2014
		#Source19  source("fn/contour.gen.r") 
		mc.contours<-contour.gen(subset(MidCF2014.dat,select=c('tow','lon','lat','meat.count')),
		                         ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,
		                         units="scallops/500g",interp.method='gstat',key='strata',blank=F,
		                         plot=F,subset.poly=Mid.survey.bound.poly,subset.eff=0,subscale=0.25,direct = direct)
		
		cf.contours<-contour.gen(subset(MidCF2014.dat,select=c('tow','lon','lat','CF')),ticks='define',
		                         nstrata=7,str.min=0,place=2,id.par=3.5,
		                         interp.method='gstat',key='strata',blank=F,plot=F,
		                         subset.poly='square',subset.eff=0,subscale=0.25,direct = direct)
		### End Contour generation ###
		
##################  Create  plots for Middle Bank using the above data. ##################################

		
		
		  # SLide 4, Scallop map of Middle Bank with tow locations.
		windows(11,8.5)
		ScallopMap(bnk,title=survey.title, plot.boundries = T,direct=direct,
		           plot.bathy=T,bathy.source = "quick",xlab="",ylab="",cex.mn=2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,subset(survMidLive.dat,year== yr & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(lat~lon,survMidLive.dat,subset=year==yr & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survMidLive.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')
		
		
		# ABUNDANCE AND  BIOMASS TRENDS PLOTS (slide 5 & 6 repsetively in presentation)
		#Source17 source("fn/survey.ts.r",local=T) lot survey time series
		survey.ts(survMid.obj[[1]],1984:yr,Bank=bnk,pdf=F, RS=RS-5, CS=CS-5,ys=.8,ht=6.5,wd=10,clr='blue',se=F,pch=16,yl2=40,
		          add.title=T,titl = survey.ts.N.title, cx.mn = 3)
		survey.ts(survMid.obj[[1]],1984:yr,Bank=bnk,pdf=F,type='B', RS=RS-5, CS=CS-5,ys=.8,ht=6.5,wd=10,clr='blue',se=F,pch=16,
		          yl2=c(0.11,0.11,1.1),add.title=T,titl = survey.ts.BM.title, cx.mn = 3)
	
		# SHELL HEIGHT FREQUENCY HISTORGRAMS MIDDLE BANK (slide 7 in presentation)
		#Source18 source("fn/shf.plt.r")
		shf.plt(ps.dat,survMid.obj,from='surv',yr=2008:yr,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),rel=F,ymax=10,
		        recline=c(RS-5,CS-5),wd=7,ht=8,add.title = T,titl = SHF.title,cex.mn=3)	
		
		
		# COMMERICAL SIZED SCALLOP DISTRIBUTION (> 90 mm) MIDDLE BANK (slide 8 in presentation)
		lvls.com=c(1,5,seq(10,40,10))
		CL <- contourLines(com.contours$image.dat,levels=lvls.com)
		CP <- convCP(CL)
		comCont.poly <- CP$PolySet
		cont.data <- data.frame(PID=1:length(lvls.com),col=brewer.pal(length(lvls.com),N.col),border=NA,stringsAsFactors = F) 
		str(cont.data)
		windows(11,8.5)
		#Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		ScallopMap(bnk,contour=list(comCont.poly,cont.data), title=fully.rec.title, plot.boundries = T,
		           plot.bathy=T,bathy.source = "quick",xlab="",ylab="",cex.mn=2,dec.deg = F,direct=direct)
		# Add the regular survey tows.
		points(lat~lon,subset(survMidLive.dat,year== yr & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(lat~lon,survMidLive.dat,subset=year==yr & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls.com[-length(lvls.com)],'-',lvls.com[-1],sep=''),
		                      paste(lvls.com[length(lvls.com)],'+',sep='')),
		       fill=cont.data$col,title=N.tow.lab, title.adj = 0.2,inset=0.02,bg='white',box.col='white')
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survMidLive.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')
		
		# PRE RECRUIT AND RECRUIT SCALLOP DISTRIBUTION ON MIDDLE BANK	(slide 9 in presentation)	
		lvls=c(1,5,seq(10,40,10))
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- CP$PolySet
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F) 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,contour=list(preCont.poly,cont.data),  plot.bathy=T,bathy.source = "quick",plot.boundries = T,
		           title=substitute(bold(paste("Smaller scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
		                            list(b=CS-5,year=yr,bank=bnk)), xlab="", ylab="",cex.mn=2,dec.deg = F,direct=direct)
		# Regular survey tows
		points(lat~lon,subset(survMidLive.dat,year== yr & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(lat~lon,survMidLive.dat,subset=year==yr & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=cont.data$col,title=N.tow.lab, title.adj = 0.2,inset=0.02,bg='white',box.col='white')
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survMidLive.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')
		
		
	
		# MEAT WEIGHT SHELL HEIGHT PLOT MIDDLE BANK (slide 10, 2014 presentation)
		#Source12 source("fn/shwt.plt1.r") 
		windows(15,8)
		par(mfrow=c(1,2))
		shwt.plt1(Mid.SpatHtWt.fit,lw=3,cx=1.2,las=1,titl = MWSH.title,cex.mn = 2)
		# CONDITION FACTOR PLOT MIDDLE BANK (also slide 10, 2014 presentation)
		#Source16 source("fn/stdts.plt.R")
		stdts.plt(merge(MidcfData$CFyrs,data.frame(year=years),all=T),y='CF',pch=16,
		          ylab=cf.lab, mean.line=T,graphic='none',xlab='Year',ylim=c(8,18),las=1,labcs=1.2,titl = CF.ts.title,cex=2)		
		
		
		
		# MEAT COUNT DISTRIBUTION ON MIDDLE BANK (slide 11 in presentation)
		lvls=seq(8,12,1)
		div=3
		CL <- contourLines(mc.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <-  convCP(contourLines(com.contours$image.dat,levels=lvls.com))$PolySet
		cont.poly <- joinPolys(CP$PolySet,subset(comCont.poly,PID==1))
		Ncol=length(unique(cont.poly$PID))+div
		cont.data<- data.frame(PID=unique(cont.poly$PID),
		                       border=NA,col=brewer.pal(Ncol,mc.col)[c(Ncol:(div+1))],stringsAsFactors = F)
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,plot.boundries=T,contour=list(cont.poly,cont.data),direct=direct,
		           title=mc.title,plot.bathy=T,bathy.source = "quick",xlab="",ylab="",cex.mn=2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,subset(survMidLive.dat,year== yr & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(lat~lon,survMidLive.dat,subset=year==yr & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		# Add the legends.
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=cont.data$col,title=mc.lab,inset=0.02,bg='white',box.col='white')
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survMidLive.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')

############ PLOT NOT USED IN PRESENTATION
		
		
		# CONDITION FACTOR DISTRIBUTION ON MIDDLE BANK (not presented but typically goes before MEAT COUNT figure.)
		
		lvls=12:20
		CL <- contourLines(cf.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <- convCP(contourLines(com.contours$image.dat,levels=lvls.com))$PolySet
		cont.poly <- joinPolys(CP$PolySet,subset(comCont.poly,PID==1))
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),cf.col),border=NA,stringsAsFactors = F) 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,contour=list(cont.poly,cont.data),title=cf.title,plot.boundries = T,direct = direct,
		           plot.bathy=T,bathy.source = "quick",xlab="",ylab="",cex.mn=2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,subset(survMidLive.dat,year== yr & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(lat~lon,survMidLive.dat,subset=year==yr & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=cont.data$col,title = cf.lab,inset=0.02,bg='white',box.col='white')
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survMidLive.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')
		
		# TOW TRACK PLOT MIDDLE BANK (not used in presentation)
		#Source9
		windows(11,8.5)
		ScallopMap(bnk,title=tow.track.title,plot.boundries = T,direct=direct,dec.deg = F,
		           plot.bathy=T,bathy.source = "quick",xlab="",ylab="",cex.mn=2)
		addLines(MidDis[[2]],col='blue')
		# Add the regular survey tows.
		points(elat~elon,subset(survMidLive.dat,year== yr & random==1),pch=20,bg='black',cex=0.8)
		points(slat~slon,subset(survMidLive.dat,year== yr & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(elat~elon,survMidLive.dat,subset=year==yr & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		points(slat~slon,survMidLive.dat,subset=year==yr & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survMidLive.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')
		
################################################################################################################		
################################ END SECTION 2 MIDDLE BANK ########################################################
################################################################################################################				
		
		
				
			
			
################################################################################################################
################################ SECTION 3 SABLE BANK ########################################################
###  Sable ###  Sable ###  Sable ###  Sable ###  Sable ###  Sable ###  Sable ###  Sable ###  Sable 
################################ SECTION 3 SABLE BANK ########################################################
############################################################################################################
		
		# Shell height for knife edge recruitment based on portsampling data DK note:  These are 5 higher than reality.
		CS = 95 # CS = Shell height for knife-edge recriutment   
		RS = 85 # RS = Shell height 1 year previous to CS
		bnk = "Sab" # use bnk for all bank specifics helps to generalize the code for later
		
		## Set up plot titles
		survey.title <- substitute(bold(paste("Survey (",bank,"-",year,")",sep="")),
		                           list(year=as.character(yr),bank=bnk))
		tow.track.title <- substitute(bold(paste("Tow tracks (",bank,"-",year,")",sep="")),
		                              list(year=as.character(yr),bank=bnk))
		fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
		                              list(a=as.character(CS-5),year=as.character(yr),bank=bnk))
		rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
		                        list(a=as.character(CS-6),b=as.character(RS-5),year=as.character(yr),bank=bnk))
		pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
		                            list(b=as.character(RS-5),year=as.character(yr),bank=bnk))
		cf.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
		                       list(year=as.character(yr),bank=bnk))
		mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
		                       list(m=as.character(CS-5),year=as.character(yr),bank=bnk))
		survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
		                                list(year=as.character(yr),bank=bnk))
		survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
		                                 list(bank=bnk))
		SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
		                         list(bank=bnk))
		MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-",year,")",sep="")),
		                         list(year=as.character(yr),bank=bnk))
		CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank,")",sep="")),
		                          list(year=as.character(yr),bank=bnk))
		clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-",year,")",sep="")),
		                             list(c=as.character(85),bank=bnk,year=as.character(yr)))
		clap.ts.title <- substitute(bold(paste("Clapper time series (",bank,")",sep="")),
		                            list(bank=bnk))
		clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,")",sep="")),
		                                list(bank=bnk))
		
		# Sable Bank Polygons	
		#Read4 Read drooped #Detailed Survey polygons
		Sab.survey.detail.poly<- subset(survey.detail.polys,label==bnk)
		attr(Sab.survey.detail.poly,"projection")<-"LL"
		
		#Read5 read dropped # Survey information.
		Sab.surv.info<-subset(survey.info,label==bnk)
		#Read7 Read 7 no longer needed, data read in above
		Sab.strata.areas <- subset(Sab.surv.info,select =c("PID","towable_area"))
		
		#Read6 read dropped for survey boundary polygon
		Sab.survey.bound.poly<-subset(survey.bound.polys,label==bnk)
		attr(Sab.survey.bound.poly,"projection")<-"LL"
		
		# TOW TRACK DATA SABLE BANK
		# Need to input the number of tows, file location, weighting scheme, seconds between readings
		#Source10 source("fn/getdis.r") 
		SabDis<-dist.coef(1:100,path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/Sable/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)
		
		# Now bring in the survey data
		survSab.dat<-subset(springSurv.dat,bank==bnk)
		
		#Source20 source("fn/restratwp.r",local=T)
		# Reassign the strat based on location and write to the screen what percentage of strata were reassigned.
		survSab.dat<-restratwp(survSab.dat,list(Sab.survey.detail.poly))
		paste("Strata reassigned",
		      with(subset(survSab.dat,year==yr & state=='live'),
		           round((1-sum(stratum==new.stratum,na.rm=T)/length(stratum))*100)),'%')
		
		
		# Meat weight data from Sable Bank for 2011 - 2014, stitch them all together.
		sab11.mw<-subset(Springsurv2011$MWs,bank==bnk)
		sab12.mw<-subset(Springsurv2012$MWs,bank==bnk)
		sab13.mw<-subset(SurvDB$MWs,bank==bnk&year==2013)
		sab14.mw<-subset(SurvDB$MWs,bank==bnk&year==2014)
		sab.mw<-rbind(sab11.mw,sab12.mw,sab13.mw,sab14.mw)
		# Convert shell height in 2014 to decimeters.
		sab14.mwdm<-sab14.mw
		sab14.mwdm$sh<-sab14.mw$sh/100
		
		# MEAT WEIGHT DATA - hydration sampling, it contains data from 1983-2007, tidy up the ID 
		Sab.wgt.dat<-subset(MW.dat,bank==bnk)
		Sab.wgt.dat$ID<-paste(Sab.wgt.dat$cruise,Sab.wgt.dat$tow,sep='.')
		# Grab the relavent Meat-Weight Shell height data and make a flat file from it
		Sabmw.dat<-merge(subset(Sab.wgt.dat,month %in% 5:6,c("ID","year","lon","lat","depth","sh","wmw")),
		                 subset(sab.mw,select=c("ID","year","lon","lat","depth","sh","wmw")),all=T)
		#Write5
		write.csv(Sabmw.dat,paste(direct,"Data/Survey_data/",yr,"/Spring/Sab/SabmwData.csv",sep=""),row.names=F)
		
		# MODEL - This is the meat weight Shell height realationship.  
		#MEAT WEIGHT SHELL HEIGHT RELATIONSHIP 2014 
		#Source11 source("fn/shwt.lme.r") note thtat the exponent is set as a parameter here b=3
		Sab.SpatHtWt.fit<-shwt.lme(sab14.mwdm,random.effect='tow',b.par=3)
		
		## MODEL - This is the model used to esimate condition factor across Sable Bank
		# We have more data than we do for Sable so a more complex model is fit.  
		#Source13 source("fn/condFac.r")
		SabcfData<-condFac(Sabmw.dat,survSab.dat,model.type='gam_f')
		
		# Output the predictions for Sable Bank from the Condition Factor model
		survSab.dat<-SabcfData$pred.dat
		# tidy up the ID and Pull out the ID and condition factor
		survSab.dat$ID<-paste(survSab.dat$year,survSab.dat$tow,sep='.')
		tmp.dat<-subset(SabcfData$CF.data,select=c("ID","CF"))
		# Rename CF to CFh
		names(tmp.dat)[2]<-"CFh"
		# merge the two data sets, keeping all x values
		survSab.dat<-merge(survSab.dat,tmp.dat,all.x=T)
		# Replace any NA's in CFh with the original Condition Factor.
		survSab.dat$CFh[is.na(survSab.dat$CFh)]<-survSab.dat$CF[is.na(survSab.dat$CFh)]
		
		
		# Calculate the biomass of the Pre-recruits, Recruits and the Commerical Scallops in each tow on Sable Bank
		#Source14 source("fn/surv.by.tow.r")
		survSab.dat<-surv.by.tow(survSab.dat, years, pre.ht=RS, rec.ht=CS)
		survSab.dat<-surv.by.tow(survSab.dat, years, pre.ht=RS, rec.ht=CS, type='B', mw.par="CFh")
		
		#Write6 - Out put these results
		write.table(survSab.dat,paste(direct,"Data/Survey_data/",yr,"/Spring/Sab/SabSurvey8314.csv",sep=""),sep=',',row.names=F)
		
		# Subset the survey data into the clappers(dead) and live scallops.
		survSabClap.dat<-subset(survSab.dat,state=='dead')
		survSabLive.dat<-subset(survSab.dat,state=='live')
		
		# The Sable survey data (from PEDstrata)
		#Source21 source("fn/survey.dat.r")# survey data for model input 
		survSab.obj<-survey.dat(survSabLive.dat, years=1987:yr, RS=RS, CS=CS, bk=bnk, areas=Sab.strata.areas, mw.par="CF")		

		### GENERATE THE CONTOURS for each of the Sable bank distribtuion maps ###
		
		#Source19 source("fn/contour.gen.r")
		# Pre-recruits (< 80 mm shell height)
		pre.contours<-contour.gen(subset(survSabLive.dat,year==yr,c('tow','lon','lat','pre')),
		                          ticks='define',nstrata=9,str.min=0,interp.method='gstat',points=T,blank=T,
		                          key='log.cont',title='pre-recruits (< 80 mm shell height)',color.fun=tim.colors,id.par=3.5,
		                          units='#/tow',plot=F,blank.dist=0.08,direct=direct)
		# Recruit (80 - 90 mm shell height)
		rec.contours<-contour.gen(subset(survSabLive.dat,year==yr,c('tow','lon','lat','rec')),
		                          ticks=c(1,5,10,50,100,500,1000,5000),str.min=0,interp.method='gstat',
		                          points=T,blank=T,key='log.cont',title='recruits(80 - 90 mm shell height)',
		                          color.fun=tim.colors,id.par=3.5,units='#/tow',plot=F,blank.dist=0.08,direct=direct)
		# Sable Commercial (>= 90 mm shell height)
		com.contours<-contour.gen(subset(survSab.dat,state=='live'&year==yr,c('tow','lon','lat','com')),
		                          ticks=c(1,5,10,50,100,500,1000,5000),
		                          str.min=0,interp.method='gstat',points=T,blank=T,key='log.cont',
		                          title='fully recruited (>= 90 mm shell height)',
		                          color.fun=tim.colors,id.par=3.5,units='#/tow',plot=F,blank.dist=0.08,direct=direct)
		
		
		# MEAT COUNT & CONDITION FACTOR OF SCALLOP ON Sable BANK (requires a bit of preprocessing)
		SabCF2014.dat<-na.omit(merge(subset(na.omit(SurvDB$pos),bank==bnk&year==yr,c('tow','lon','lat')),Sab.SpatHtWt.fit$fit))
		names(SabCF2014.dat)[4]<-"CF"
		SabCF2014.dat<-merge(SabCF2014.dat,subset(survSabLive.dat,
		                                          year==yr,c('year','tow','lon','lat',paste('h',seq(5,200,5),sep=''))))
		#Source14 source("fn/surv.by.tow.r") surv.by.tow calculates number and/or biomass of pre, rec and 
		#com size scallops in each tow + meat count.
		SabCF2014.dat<-surv.by.tow(SabCF2014.dat, yr, pre.ht=RS, rec.ht=CS)
		SabCF2014.dat<-surv.by.tow(SabCF2014.dat, yr, pre.ht=RS, rec.ht=CS, type='B', mw.par="CF")
		# Meat count per 500g
		SabCF2014.dat$meat.count<-0.5/(SabCF2014.dat$com.bm/SabCF2014.dat$com)
		
		# CONDITION FACTOR and MEAT COUNT OF SCALLOP ON SABLE BANK in 2014
		#Source19  source("fn/contour.gen.r")
		mc.contours<-contour.gen(subset(SabCF2014.dat,select=c('tow','lon','lat','meat.count')),ticks='define',
		                         nstrata=7,str.min=0,place=2,id.par=3.5,units="scallops/500g",interp.method='gstat',
		                         key='strata',blank=F,plot=F,subset.poly='square',
		                         subset.eff=0,subscale=0.25,direct=direct)
		# Condition factor
		cf.contours<-contour.gen(subset(survSab.dat,state=='live'&year==yr,c('tow','lon','lat','CF')),
		                         ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5, interp.method='gstat',
		                         key='strata',blank=F,plot=F,subset.poly='square',subset.eff=0,subscale=0.25,direct=direct)
		
		
		##################  Create  plots for Sable Bank using the above data. ##################################
		
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") 
		# Plot of the tow tracks and strata definitions for Sable Bank.  This exact plot is not used but is similar to Slide 14
		windows(11,8.5)
		ScallopMap(bnk,poly.lst=list(Sab.survey.detail.poly,Sab.surv.info),direct=direct,
		           title=survey.title,plot.boundries = F,
		           plot.bathy=T,bathy.source = "quick",xlab="",ylab="",cex.mn=2,dec.deg = F)
		# Add the regular survey tows.
    points(lat~lon,subset(survSab.dat,year== yr & state=='live' & random==1),pch=20,bg='black',cex=0.8)
    # Add other survey tows
    points(lat~lon,survSab.dat,subset=year==yr & state=='live' & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
    # Add the legends
    legend("topleft",pch=c(20), pt.bg = c("black"), title="Tow type",
       legend = paste('regular (n =',length(unique(subset(survSab.dat,year==yr & random==1)$tow)),")",sep=""),
       inset=0.01,bg='white',box.col='white')
    legend("topright",legend=Sab.surv.info$PName,fill=Sab.surv.info$col,bty='n',cex=1, title = "Strata")
    
    legend("top",legend = round(Sab.surv.info$area_km2),
           fill=c(Sab.surv.info$col),border=c(rep('black',length(Sab.surv.info$PName))),
           pch=c(rep(NA,length(Sab.surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.1,
           pt.bg = c(rep(NA,length(Sab.surv.info$PName))),col='black',bty='n')
    # DK NOTE:  For 2014 only to match previous documentation I have adjusted the number of tows in strata 1 and 2 by 2 and 1
    # Please remove this for any other year!
    legend(-61.55,44.5,legend = as.numeric(with(subset(survSab.dat,year== yr & state=='live' & random==1),
                                                tapply(tow,new.stratum,length))+c(2,-1,0,0,0)),
           fill=c(Sab.surv.info$col),border=c(rep('black',length(Sab.surv.info$PName))),
           pch=c(rep(NA,length(Sab.surv.info$PName))),title = "Number of tows",title.adj=0.1,
           pt.bg = c(rep(NA,length(Sab.surv.info$PName))),col='black',bty='n')
    
    
		# plot survey time series of abundance (slide 15) and biomass (slide 16) 2014 presentation.
		#Source17 source("fn/survey.ts.r",local=T)
		survey.ts(survSab.obj[[1]],1987:yr,Bank=bnk,pdf=F, RS=RS-5, CS=CS-5,
		          areas=Sab.strata.areas$towable_area,ys=.8,clr='blue',se=T,pch=16,yl2=95,ypos=3,
		          add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx=1.5)
		
		survey.ts(survSab.obj[[1]],1987:yr,Bank=bnk,pdf=F,type='B', RS=RS-5, CS=CS-5,
		          areas=Sab.strata.areas$towable_area,ys=.8,clr='blue',se=T,pch=16,yl2=1.3,
		          add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx=1.5)
		
		# Shell height frequency plots (slide 17, 2014 presentation)
		#Source18 source("fn/shf.plt.r")
		shf.plt(ps.dat,survSab.obj,from='surv',yr=2008:2014,col1='grey80',type='sh',col2=1,col3=1,
		        xl=c(0,200),rel=F,ymax=25,recline=c(RS-5,CS-5),wd=7,ht=8,add.title = T,titl = SHF.title,cex.mn=3)	
		
		# PRE-RECRUIT SIZED SCALLOP DISTRIBUTION (< 80 mm) SABLE BANK (slide 18 in presentation)
		lvls=c(1,2,5,10,20,50,100)
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,Sab.survey.bound.poly)
		precont.data<- data.frame(PID=1:7,col=brewer.pal(7,N.col),border=NA,stringsAsFactors = F) 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,contour=list(preCont.poly,precont.data), title=pre.rec.title, plot.boundries = T,direct=direct,
		          plot.bathy = T,bathy.source = "quick",xlab="",ylab="",cex.mn = 2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,subset(survSab.dat,year== yr & state=='live' & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(lat~lon,survSab.dat,subset=year==yr & state=='live' & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survSab.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')		
		legend("bottomright",title=N.tow.lab, title.adj = 0.2,c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                       paste(lvls[length(lvls)],'+',sep='')),fill=precont.data$col,
		       inset=0.04,bg='white',box.col='white')
		
		# RECRUIT SIZED SCALLOP DISTRIBUTION (> 90 mm) SABLE BANK (slide 19 in presentation)
		CL <- contourLines(rec.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		recCont.poly <- joinPolys(CP$PolySet,Sab.survey.bound.poly)
		windows(11,8.5)
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		ScallopMap(bnk,contour=list(recCont.poly,precont.data), title=rec.title, plot.boundries = T,direct=direct,
		           plot.bathy = T,bathy.source = "quick",xlab="",ylab="",cex.mn = 2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,subset(survSab.dat,year== yr & state=='live' & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(lat~lon,survSab.dat,subset=year==yr & state=='live' & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survSab.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')	
		legend("bottomright",title=N.tow.lab, title.adj = 0.2,
		       c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=precont.data$col, inset=0.04,bg='white',box.col='white')
		
		# COMMERCIAL SIZED SCALLOP DISTRIBUTION (> 90 mm) SABLE BANK (slide 20 in presentation)
		lvls.com=c(1,2,5,10,20,50,100)
		CL <- contourLines(com.contours$image.dat,levels=lvls.com)
		CP <- convCP(CL)
		comCont.poly <- joinPolys(CP$PolySet,Sab.survey.bound.poly)
		windows(11,8.5)
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		ScallopMap(bnk,contour=list(comCont.poly,precont.data),title=fully.rec.title, plot.boundries = T,direct=direct,
		           plot.bathy = T,bathy.source = "quick",xlab="",ylab="",cex.mn = 2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,subset(survSab.dat,year== yr & state=='live' & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(lat~lon,survSab.dat,subset=year==yr & state=='live' & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survSab.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')	
		legend("bottomright",title=N.tow.lab, title.adj = 0.2,
		       c(paste(lvls.com[-length(lvls.com)],'-',lvls.com[-1],sep=''),paste(lvls.com[length(lvls.com)],'+',sep='')),
		       fill=precont.data$col,inset=0.04,bg='white',box.col='white')
		
		# MEAT WEIGHT SHELL HEIGHT PLOT SABLE BANK (slide 21, 2014 presentation)
		#Source12 source("fn/shwt.plt1.r")
		windows(15,8)
		par(mfrow=c(1,2))
		shwt.plt1(Sab.SpatHtWt.fit,lw=3,cx=1.2,titl=MWSH.title,cex.mn=2)
		# CONDITION FACTOR PLOT SABLE BANK (also slide 21, 2014 presentation)
		#Source16 source("fn/stdts.plt.R")
		stdts.plt(merge(SabcfData$CFyrs,data.frame(year=years),all=T),y='CF',pch=16,
		          ylab=cf.lab, mean.line=T,graphic='none',xlab='Year',ylim=c(8,18),las=1,titl=CF.ts.title,cex.mn=2)
	
		# Condition Factor (slide 22 in 2014 presentation)
		lvls=8:16
		CL <- contourLines(cf.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		cont.poly <- joinPolys(CP$PolySet,Sab.survey.bound.poly)
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlOrBr"),border=NA,stringsAsFactors = F)
		windows(11,8.5)
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		ScallopMap(bnk,contour=list(cont.poly,cont.data),title=cf.title, plot.boundries = T,direct=direct,
		           plot.bathy = T,bathy.source = "quick",xlab="",ylab="",cex.mn = 2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,subset(survSab.dat,year== yr & state=='live' & random==1),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		points(lat~lon,survSab.dat,subset=year==yr & state=='live' & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survSab.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')	
		legend("bottomright",title=cf.lab,
		       c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=cont.data$col,inset=0.04,bg='white',box.col='white')
		
		# Sable Meat Count (slide 23 in 2014 presentation)
		#Source19 source("fn/contour.gen.r")
		lvls=seq(10,45,5)
		div=3
		CL <- contourLines(mc.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		cont.poly <- joinPolys(CP$PolySet,subset(comCont.poly,PID==1))
		Ncol=length(unique(cont.poly$PID))+div
		cont.data<- data.frame(PID=unique(cont.poly$PID),col=brewer.pal(Ncol,"Spectral")[c(Ncol:(div+2),1)],
		                       border=NA,stringsAsFactors = F) 
				windows(11,8.5)
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		ScallopMap(bnk,contour=list(cont.poly,cont.data),title=mc.title, plot.boundries = T,direct=direct,
		           plot.bathy = T,bathy.source = "quick",xlab="",ylab="",cex.mn = 2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,subset(SabCF2014.dat),pch=20,bg='black',cex=0.8)
		# Add other survey tows
		# Add the legends
		legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
		       legend = paste('regular (n =',length(unique(subset(survSab.dat,year==yr & random==1)$tow)),")",sep=""),
		       inset=0.01,bg='white',box.col='white')	
		legend("bottomright",title=mc.lab,
		       c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=cont.data$col,inset=0.04,bg='white',box.col='white')
		
		
################################################################################################################		
################################ END SECTION 3 SABLE BANK ########################################################
################################################################################################################				
			
			
			
			
			
################################################################################################################
################################ SECTION 4 GERMAN BANK ########################################################
### GERMAN### GERMAN### GERMAN### GERMAN### GERMAN### GERMAN### GERMAN### GERMAN### GERMAN### GERMAN### GERMAN
################################ SECTION 4 GERMAN BANK ########################################################
############################################################################################################


		# Shell height for knife edge recruitment based on portsampling data DK note:  These are 5 higher than reality.
	 	CS=110
		RS=100 # RS = Shell height 1 year previous to CS
	 	bnk = "Ger" # use bnk for all bank specifics helps to generalize the code for later
	 	years=1984:yr
	 	
	 	## Set up plot titles
	 	survey.title <- substitute(bold(paste("Survey (",bank,"-",year,")",sep="")),
	 	                           list(year=as.character(yr),bank=bnk))
	 	tow.track.title <- substitute(bold(paste("Tow tracks (",bank,"-",year,")",sep="")),
	 	                              list(year=as.character(yr),bank=bnk))
	 	fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
	 	                              list(a=as.character(CS-5),year=as.character(yr),bank=bnk))
	 	rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
	 	                        list(a=as.character(CS-6),b=as.character(RS-5),year=as.character(yr),bank=bnk))
	 	pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
	 	                            list(b=as.character(RS-5),year=as.character(yr),bank=bnk))
	 	cf.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
	 	                       list(year=as.character(yr),bank=bnk))
	 	mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
	 	                       list(m=as.character(CS-5),year=as.character(yr),bank=bnk))
	 	survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
	 	                                list(year=as.character(yr),bank=bnk))
	 	survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
	 	                                 list(bank=bnk))
	 	SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
	 	                         list(bank=bnk))
	 	MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-",year,")",sep="")),
	 	                         list(year=as.character(yr),bank=bnk))
	 	CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank,")",sep="")),
	 	                          list(year=as.character(yr),bank=bnk))
	 	clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-",year,")",sep="")),
	 	                             list(c=as.character(85),bank=bnk,year=as.character(yr)))
	 	clap.ts.title <- substitute(bold(paste("Clapper time series (",bank,")",sep="")),
	 	                            list(bank=bnk))
	 	clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,")",sep="")),
	 	                                list(bank=bnk))
	 	
	 	#German Bank Boundary Polygon
	 	GerBndry<-subset(newAreaPolys,label=='SFA26') 

	 	# TOW TRACK DATA GERMAN BANK
	 	# Need to input the number of tows, file location, weighting scheme, seconds between readings
	 	#Source10 source("fn/getdis.r") 
	 	GerDis<-dist.coef(401:480,path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/German/",sep=""),
	 	                  w=c(1:10,9:1),rule=8,smooth=T,plt=F)
	 	
	 	# A list of the tows for 2009-214 in German Bank, used for sprSurv function
	 	ger.tow.list <- read.csv(paste(direct,"data/Survey_data/2014/Spring/Ger/towlist_2009-",yr,".csv",sep=""))

		# Grab the German bank and the lie scallops on German
		survGer.dat<-subset(springSurv.dat,bank=='Ger')
		survGerLive.dat<-subset(survGer.dat,state=='live')
		
		# Meat weight data from German Bank for 2011 - 2014, stitch them all together.
		Ger11.mw<-subset(Springsurv2011$MWs,bank=="Ger")
		Ger12.mw<-subset(Springsurv2012$MWs,bank=="Ger")
		Ger13.mw<-subset(SurvDB$MWs,bank=="Ger"&year==2013&sh<190)
		Ger14.mw<-subset(SurvDB$MWs,bank=="Ger"&year==2014)
		Ger.mw<-rbind(Ger11.mw,Ger12.mw,Ger13.mw,Ger14.mw)
		# Convert shell height in 2014 to decimeters.
		Ger14.mwdm<-Ger14.mw
		Ger14.mwdm$sh<-Ger14.mw$sh/100
		
		# MEAT WEIGHT DATA - hydration sampling, it contains data from 1983-2007, tidy up the ID 
		Ger.wgt.dat<-subset(MW.dat,bank=="Ger")
		Ger.wgt.dat$ID<-paste(Ger.wgt.dat$year,Ger.wgt.dat$tow,sep='.')
		
		# Grab the relavent Meat-Weight Shell height data and make a flat file from it
				Germw.dat<-merge(subset(Ger.wgt.dat,month%in%5:6,c("ID","year","lon","lat","depth","sh","wmw")),
		                 subset(Ger.mw,select=c("ID","year","lon","lat","depth","sh","wmw")),all=T)
		#Write7 this file could be read in to skip all the preprocessing on German
		write.csv(Germw.dat,paste(direct,"Data/Survey_data/",yr,"/Spring/Ger/GermwData.csv",sep=""),row.names=F)
		
		
		# MODEL - This is the meat weight Shell height realationship.  
		#MEAT WEIGHT SHELL HEIGHT RELATIONSHIP 2014 
		#Source11 source("fn/shwt.lme.r") note thtat the exponent is set as a parameter here b=3
		Ger.SpatHtWt.fit<-shwt.lme(Ger14.mwdm,random.effect='tow',b.par=3)
		
		
		## MODEL - This is the model used to esimate condition factor across German Bank
		# Same model as Sable, tho both are more complex than German Bank due to data constraints there
		#Source13 source("fn/condFac.r")
		GercfData<-condFac(Germw.dat,survGer.dat,model.type='gam_f')
		
		# Output the predictions for German Bank from the Condition Factor model
		survGer.dat<-GercfData$pred.dat
		# tidy up the ID and Pull out the ID and condition factor
		survGer.dat$ID<-paste(survGer.dat$year,survGer.dat$tow,sep='.')
		tmp.dat<-subset(GercfData$CF.data,select=c("ID","CF"))
		# Rename CF to CFh
		names(tmp.dat)[2]<-"CFh"
		# merge the two data sets, keeping all x values
		survGer.dat<-merge(survGer.dat,tmp.dat,all.x=T)
		# Replace any NA's in CFh with the original Condition Factor.
		survGer.dat$CFh[is.na(survGer.dat$CFh)]<-survGer.dat$CF[is.na(survGer.dat$CFh)]
		
		# Calculate the biomass of the Pre-recruits, Recruits and the Commerical Scallops in each tow on German Bank
		#Source14 source("fn/surv.by.tow.r")
		survGer.dat<-surv.by.tow(survGer.dat, years, pre.ht=RS, rec.ht=CS)
		survGer.dat<-surv.by.tow(survGer.dat, years, pre.ht=RS, rec.ht=CS, type='B', mw.par="CF")
		#Write6 - Out put these results
		write.table(survGer.dat,paste(direct,"Data/Survey_data/",yr,"/Spring/Ger/GerSurvey8414.csv",sep=""),sep=',',row.names=F)
		
		# Subset the survey data into the clappers(dead) and live scallops.
		survGerClap.dat<-subset(survGer.dat,state=='dead')
		survGerLive.dat<-subset(survGer.dat,state=='live')
		
		
		# MEAT COUNT & CONDITION FACTOR OF SCALLOP ON GERMAN BANK (requires a bit of preprocessing)
		# I think CF2 data is related the lined tows
		GerCF.dat<-merge(GercfData$CFyrs,data.frame(year=1983:yr),all=T)
		GerCF.dat$CF2<-GerCF.dat$CF
		GerCF.dat$CF[GerCF.dat$year>2007]<-NA
		GerCF.dat$CF2[GerCF.dat$year<2008]<-NA
		
		# Survey results
		# On German we have a repeated measures survey design (we repeat stations each year) 
		lined.dat<-rbind(subset(survGerLive.dat,year==2008 & stratum==2),subset(survGerLive.dat,year > 2008))
		clap.dat<-rbind(subset(survGerClap.dat,year==2008 & stratum==2),subset(survGerClap.dat,year > 2008))
		
		#Source23 source("fn/sprSurv.r") Using the towlists from 2009-2014 (see data input section)
		# We can get the survey results for this type of survey design.
		spr.obj<-sprSurv(lined.dat,2008:yr,ger.tow.list,chng=T)
		
		# prepare survey index data obj
		
		#Source15 source("fn/simple.surv.r")
		unlined.dat<-rbind(subset(survGerLive.dat,year<2007),subset(survGerLive.dat,year%in%2007:2008&stratum==1))
		unlined.obj<-simple.surv(unlined.dat,years=1983:2008)
		lined.obj<-simple.surv(lined.dat,years=2008:2014)
		clap.obj<-simple.surv(clap.dat,years=2008:2014)
		
		merged.obj<-merge(subset(spr.obj[[1]],year!=2011),subset(lined.obj[[1]],year==2011),all=T)
		
		# matched tows
		matched.tows<-rbind(subset(survGerLive.dat, year==2013 & tow %in% spr.obj[[2]]$tow.y1),
		                    subset(survGerLive.dat, year==2014 & tow %in% spr.obj[[2]]$tow.y2))
		matched.obj<-simple.surv(matched.tows, years=2013:2014)
		
		### GENERATE THE CONTOURS for each of the Sable bank distribtuion maps ###

		# Pre-recruits (< 95 mm shell height)
		#Source19 source("fn/contour.gen.r")
		pre.contours<-contour.gen(subset(survGerLive.dat,year==2014,c('tow','lon','lat','pre')),ticks='define',
		                          nstrata=9,str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',
		                          title='pre-recruits (< 80 mm shell height)',color.fun=tim.colors,id.par=3.5,
		                          units='#/tow',plot=F,blank.dist=0.08,direct=direct)
		
		# Recruit (95 - 105 mm shell height)
		#Source19 source("fn/contour.gen.r")
		rec.contours<-contour.gen(subset(survGerLive.dat,year==2014,c('tow','lon','lat','rec')),
		                          ticks=c(1,5,10,50,100,500,1000,5000),str.min=0,interp.method='gstat',points=T,
		                          blank=T,res=0.01,key='log.cont',title='recruits(80 - 90 mm shell height)',
		                          color.fun=tim.colors,id.par=3.5,units='#/tow',plot=F,blank.dist=0.08,direct=direct)
		# Commercial (>= 105 mm shell height)
		#Source19 source("fn/contour.gen.r")
		com.contours<-contour.gen(subset(survGerLive.dat,year==2014,c('tow','lon','lat','com')),
		                          ticks=c(1,5,10,50,100,500,1000,5000),str.min=0,interp.method='gstat',
		                          points=T,blank=T,res=0.01,key='log.cont',title='fully recruited (>= 90 mm shell height)',
		                          color.fun=tim.colors,id.par=3.5,units='#/tow',plot=F,blank.dist=0.08,direct=direct)
		
		# MEAT COUNT & CONDITION FACTOR OF SCALLOP ON German BANK (requires a bit of preprocessing)
		GerCF2014.dat<-na.omit(merge(subset(na.omit(SurvDB$pos),bank=="Ger"&year==2014,c('tow','lon','lat')),Ger.SpatHtWt.fit$fit))
		names(GerCF2014.dat)[4]<-"CF"
		GerCF2014.dat<-merge(GerCF2014.dat,subset(survGerLive.dat,
		                                          year==2014,c('year','tow','lon','lat',paste('h',seq(5,200,5),sep=''))))
		#Source14 source("fn/surv.by.tow.r") surv.by.tow calculates number and/or biomass of pre, rec and 
		#com size scallops in each tow + meat count.                                                                       
		GerCF2014.dat<-surv.by.tow(GerCF2014.dat, 2014, pre.ht=RS, rec.ht=CS)
		GerCF2014.dat<-surv.by.tow(GerCF2014.dat, 2014, pre.ht=RS, rec.ht=CS, type='B', mw.par="CF")
		# Meat count per 500g
		GerCF2014.dat$meat.count<-0.5/(GerCF2014.dat$com.bm/GerCF2014.dat$com)
		
		# CONDITION FACTOR and MEAT COUNT OF SCALLOP ON GERMAN BANK in 2014
		#Source19 source("fn/contour.gen.r")
		mc.contours<-contour.gen(na.omit(subset(GerCF2014.dat,select=c('tow','lon','lat','meat.count'))),
		                         ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="scallops/500g",
		                         interp.method='gstat',key='strata',blank=F,plot=F,subset.poly='square',
		                         subset.eff=0,subscale=0.25,direct=direct)
		
		#Condition Factor. #Source19 source("fn/contour.gen.r")
		cf.contours<-contour.gen(subset(GerCF2014.dat,select=c('tow','lon','lat','CF')),ticks='define',
		                         nstrata=7,str.min=0,place=2,id.par=5,units="scallops/500g",
		                         interp.method='gstat',key='strata',blank=F,plot=F,subset.poly='square',
		                         subset.eff=0,subscale=0.25,direct=direct)
		
		
		# GERMAN BANK FIGURES # GERMAN BANK FIGURES # GERMAN BANK FIGURES # GERMAN BANK FIGURES # GERMAN BANK FIGURES
		
		# DK Note the numbers for matched vs. unmatched does not line up with 2014 presentation!!
		# Mactched vs. unmatched tow locations	Slide 25 in Spring Survey Presenation.
		windows(11,8.5)
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		ScallopMap(bnk,title=survey.title,plot.boundries = T,boundries = "offshore",
		           plot.bathy=T,bathy.source = "quick",xlab="",ylab="",cex.mn=2,direct=direct,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survGerLive.dat,subset=year==2014 & random==1,pch=20,bg='black',cex=0.8)
		# Add the Matched tows
		points(lat~lon,survGerLive.dat,subset=year==2014 & random==3,pch=22,bg="yellow",cex=0.8)
		# Add the legend
		legend('topleft',legend=
		        c(paste('regular (n =',length(unique(subset(survGerLive.dat,year==yr & random==1)$tow)),")", sep=""),
		          paste('matched (n =',length(unique(subset(survGerLive.dat,year==yr & random==3)$tow)),")", sep="")),
		        pch=c(20,22), pt.bg = c("black","yellow"),bty='n',cex=1, inset = .02)
		
		#legend(-66.4,43.65,c("unmatched","matched"),pch=1:2,bty='n',title="Survey Stations",inset=0.05,cex=1.2)
		
		# Survey time series of Abundance and Biomass per tow.  Figures on Slides 26 and 27 respectively
		#Source17 source("fn/survey.ts.r",local=T)
		survey.ts(unlined.obj[[1]],1993:yr,Bank='Ger',pdf=F, CS=CS-5, RS=RS-5, ymin=-5,
		          dat2=merged.obj,clr=c('blue','red'),pch=c(16,17),se=T,yl2=400,
		          add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
		legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
		survey.ts(unlined.obj[[1]],1993:yr,Bank='Ger',pdf=F,type='B', CS=CS-5, RS=RS-5, ymin=-5,yl2=6.5,
		          dat2=merged.obj,clr=c('blue','red'),se=T,pch=c(16,17),
		          add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx=1.5)
		legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
		
		
		
		# Shell height frequency plots for various years.
		#Source18 source("fn/shf.plt.r")
		# SHF for Lined tows on German bank from 2008-2014, Slide 28 in 2014 presentation
		shf.plt(ps.dat,lined.obj,from='surv',yr=2008:2014,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),rel=F,
		        ymax=120,rows=7,mean.line=F,recline=c(RS-5,CS-5),adj=0.9,add.title = T,titl = SHF.title,cex.mn=3)	
		# SHF for matched tows on German bank from 2013-2014, Slide 29 in 2014 presentation
		shf.plt(ps.dat,matched.obj,from='surv',yr=2013:2014,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),rel=F,
		        rows=2,recline=c(RS-5,CS-5),ymax=120,mean.line=F,adj=0.9,add.title = T,titl = "SHF - Matched Tows (Ger)",cex.mn=3)	

		# Pre-recuits Distribution plots for all tows on German 2014
		lvls=c(2,5,10,20,50,100,200,500,1000) # Used for all spatial plots of abundance.
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,GerBndry)
		precont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F) 
	
		windows(11,8.5) # Distribution of Pre recruits on German Bank 2014, Slide 34 (note 30-34 are 2010-2013 versions of this)	
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		ScallopMap(bnk,contour=list(preCont.poly,precont.data),boundries="offshore",plot.boundries = T,
		           title=pre.rec.title,plot.bathy = T,bathy.source="quick",direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		#points(lat~lon,survGerLive.dat,subset=year==2014,pch=16,cex=0.5)
		# Add the regular survey tows.
		points(lat~lon,survGerLive.dat,subset=year==2014 & random==1,pch=20,bg='black',cex=0.8)
		# Add the Matched tows
		points(lat~lon,survGerLive.dat,subset=year==2014 & random==3,pch=22,bg="yellow",cex=0.8)
		# Add the legend
		legend('topleft',legend=
		         c(paste('regular (n =',length(unique(subset(survGerLive.dat,year==yr & random==1)$tow)),")", sep=""),
		           paste('matched (n =',length(unique(subset(survGerLive.dat,year==yr & random==3)$tow)),")", sep="")),
		       pch=c(20,22), pt.bg = c("black","yellow"),bty='n',cex=1, inset = .02)
		legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=comCont.data$col,title=N.tow.lab, title.adj = 0.2,bg='white',box.col='white',inset=0.02)
		
		
		# Recruits Distribution plots for all tows on German 2014
		CL <- contourLines(rec.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		recCont.poly <- joinPolys(CP$PolySet,GerBndry)
		recCont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F) 
		
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)  # Distribution of recruits on German Bank 2014, Slide 35
		ScallopMap(bnk,contour=list(recCont.poly,recCont.data),boundries="offshore",plot.boundries = T,
		           title=rec.title,plot.bathy = T,bathy.source="quick",direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		#points(lat~lon,survGerLive.dat,subset=year==2014,pch=16,cex=0.5)
		# Add the regular survey tows.
		points(lat~lon,survGerLive.dat,subset=year==2014 & random==1,pch=20,bg='black',cex=0.8)
		# Add the Matched tows
		points(lat~lon,survGerLive.dat,subset=year==2014 & random==3,pch=22,bg="yellow",cex=0.8)
		# Add the legend
		legend('topleft',legend=
		         c(paste('regular (n =',length(unique(subset(survGerLive.dat,year==yr & random==1)$tow)),")", sep=""),
		           paste('matched (n =',length(unique(subset(survGerLive.dat,year==yr & random==3)$tow)),")", sep="")),
		       pch=c(20,22), pt.bg = c("black","yellow"),bty='n',cex=1, inset = .02)
		legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=comCont.data$col,title=N.tow.lab, title.adj = 0.2,bg='white',box.col='white',inset=0.02)
		
		# Commercial
		CL <- contourLines(com.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <- joinPolys(CP$PolySet,GerBndry)
		comCont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F) 
		
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5) # Distribution of commercial scallops on German Bank 2014, Slide 36
		ScallopMap(bnk,contour=list(comCont.poly,comCont.data),boundries="offshore",plot.boundries = T,
		           title=fully.rec.title,plot.bathy = T,bathy.source="quick",direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		#points(lat~lon,survGerLive.dat,subset=year==2014,pch=16,cex=0.5)
		# Add the regular survey tows.
		points(lat~lon,survGerLive.dat,subset=year==2014 & random==1,pch=20,bg='black',cex=0.8)
		# Add the Matched tows
		points(lat~lon,survGerLive.dat,subset=year==2014 & random==3,pch=22,bg="yellow",cex=0.8)
		# Add the legend
		legend('topleft',legend=
		         c(paste('regular (n =',length(unique(subset(survGerLive.dat,year==yr & random==1)$tow)),")", sep=""),
		           paste('matched (n =',length(unique(subset(survGerLive.dat,year==yr & random==3)$tow)),")", sep="")),
		       pch=c(20,22), pt.bg = c("black","yellow"),bty='n',cex=1, inset = .02)
		legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=comCont.data$col,title=N.tow.lab, title.adj = 0.2,bg='white',box.col='white',inset=0.02)
		
	
		## MEAT WEIGHT SHELL HEIGHT PLOT Ger Bank Slide 37 of 2014 Spring Summary presentation
		#Source12 source("fn/shwt.plt1.r") Slide 37 of 2014 Spring Summary presentation
		windows(15,8)
		par(mfrow=c(1,2))
		shwt.plt1(Ger.SpatHtWt.fit,lw=3,cx=1.2,titl = MWSH.title,cex.mn = 2)
		# Condition factor on German bank CCF2 data is related the lined tows
		#Source16 source("fn/stdts.plt.R") Slide 37 of 2014 Spring Summary presentation
		# DK Note there is something very different for CF in 1984-1986 on my plot compared to 2014 presentation, otherwise ok!
		stdts.plt(GerCF.dat,y=c('CF','CF2'),pch=c(16,16),col=c('black','darkgreen'),ylab=cf.lab,
		          mean.line=T,graphic='none',xlab='Year',ylim=c(8,18.5),las=1,labcs=1.2,titl = CF.ts.title,cex.mn=2)

		
		# CONDITION FACTOR DISTRIBUTION ON GERMAN BANK Slide 38 of 2014 Spring Summary presentation
		lvls=6:14
		CL <- contourLines(cf.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		cont.poly <- joinPolys(CP$PolySet,subset(comCont.poly,PID==1&SID%in%c(1,7)))
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),cf.col),border=NA,stringsAsFactors = F) 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5) # Slide 39 of 2014 Spring Summary presentation
		ScallopMap(bnk,contour=list(cont.poly,cont.data),boundries="offshore",plot.boundries = T,
		           title=cf.title,plot.bathy = T,bathy.source="quick",direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		# Grab the matched/regular tows from the condition factor data.
		matched.tw <- subset(survGerLive.dat,year==2014 & random==3)$tow
		cf.matched <- subset(GerCF2014.dat,tow%in% matched.tw)
		cf.regular <- GerCF2014.dat[-which(GerCF2014.dat$tow %in% matched.tw),]
		
		# Add CF survey tows. 
		points(lat~lon,cf.regular,bg='black',pch=20,cex=0.8)
	  #points(lat~lon,survGerLive.dat,subset=year==2014 & random==1,pch=20,bg='black',cex=0.8)
		# Add the Matched tows
		points(lat~lon,cf.matched,pch=22,bg="yellow",cex=0.8)
		# Add the legend
		legend('topleft',legend=
		         c(paste('regular (n =',length(cf.regular$tow),")", sep=""),
		           paste('matched (n =',length(cf.matched$tow),")", sep="")),
		       pch=c(20,22), pt.bg = c("black","yellow"),bty='n',cex=1, inset = .02)
		legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=cont.data$col,title=cf.lab,bg='white',box.col='white',inset=0.02)
		
		# MEAT COUNT DISTRIBUTION ON GERMAN BANK Slide 39 of 2014 Spring Summary presentation
		lvls=seq(5,40,5)
		div=3
		CL <- contourLines(mc.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		cont.poly <- joinPolys(CP$PolySet,subset(comCont.poly,PID==1&SID%in%c(1,7)))
		Ncol=length(lvls)+div
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,mc.col)[c(Ncol:(div+2),1)],border=NA,stringsAsFactors = F) 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)  
		ScallopMap(bnk,contour=list(cont.poly,cont.data),boundries="offshore",plot.boundries = T,
		           title=mc.title,plot.bathy = T,bathy.source="quick",direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		#points(lat~lon,survGerLive.dat,subset=year==2014,pch=16,cex=0.5)
		
		# Grab the matched/regular tows from the condition factor data.
		matched.tows <- subset(survGerLive.dat,year==2014 & random==3)$tow
		cf.matched <- subset(GerCF2014.dat,tow%in% matched.tows)
		cf.regular <- GerCF2014.dat[-which(GerCF2014.dat$tow %in% matched.tows),]
		
		# Add CF survey tows. 
		points(lat~lon,cf.regular,bg='black',pch=20,cex=0.8)
		#points(lat~lon,survGerLive.dat,subset=year==2014 & random==1,pch=20,bg='black',cex=0.8)
		# Add the Matched tows
		points(lat~lon,cf.matched,pch=22,bg="yellow",cex=0.8)
		# Add the legend
		legend('topleft',legend=
		         c(paste('regular (n =',length(cf.regular$tow),")", sep=""),
		           paste('matched (n =',length(cf.matched$tow),")", sep="")),
		       pch=c(20,22), pt.bg = c("black","yellow"),bty='n',cex=1, inset = .02)
		
		legend("bottomright",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),
		       fill=cont.data$col,title=mc.lab,bg='white',box.col='white',inset=0.02)
		
		
		###########################  not in PRESENTATION BUT USEFUL MAYBE? ########################
		###########################  not in PRESENTATION BUT USEFUL MAYBE? ########################
		###########################  not in PRESENTATION BUT USEFUL MAYBE? ########################
		
		# Useful plots for survey summary that weren't in presentation
		#Source22 source("fn/shwt.plt.r") The individual tows for the SH-MW relationship, not included in presentation
		shwt.plt(Ger.SpatHtWt.fit,pt.col=rgb(0,0,0,0.2))
		
		#Individulal tow tracks, plot not in presentation.
		#Source10 source("fn/getdis.r"). 
		ScallopMap(xlim=c(-66.7,-65.5),ylim=c(42.9,43.7),bathy.source="usgs",bathcol=rgb(0,0,1,0.3),dec.deg = F)
		addLines(GerDis[[2]],col='blue')
		points(slat~slon,survGer.dat,subset=year==2014&state=='live',pch=16,cex=0.5)
		points(elat~elon,survGer.dat,subset=year==2014&state=='live',pch=16,cex=0.5)
		#with(subset(survGer.dat,year==2014&state=='live'),identify(slon,slat))
		#subset(survGer.dat,year==2014&state=='live')[c(42),]

		# SHF plots that may be of interest, including Clappers and unlined tows from 2008 and earlier
		shf.plt(ps.dat,clap.obj,from='surv',yr=2008:2014,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),rel=F,
		        rows=7,mean.line=F,ht=8,recline=c(95,105),add.title = T,titl = SHF.title,cx.mn=2)	
		shf.plt(ps.dat,unlined.obj,from='surv',yr=2002:2008,col1='grey80',type='sh',col2=1,col3=1,
		        xl=c(0,200),rel=F,add.title = T,titl = SHF.title,cx.mn=2)			
		
		
################################################################################################################		
################################ END SECTION 4 GERMAN BANK #####################################################
################################################################################################################				
		
		
		
		
		
################################################################################################################
################################ SECTION 5 BBn BANK ############################################################
### BBn### BBn### BBn### BBn### BBn### BBn### BBn### BBn### BBn### BBn### BBn### BBn### BBn### BBn### BBn  #####
################################ SECTION 5 BBn BANK ############################################################
################################################################################################################
	
		
		# The years for Browns Bank North, note these differ from other banks
		years=1991:yr
		# Shell height for knife edge recruitment based on portsampling data
		# DK note:  Are the CS and RS specified here actually 5 higher than the actual shell heights? if not we got problems...
		CS = 100 # CS = Shell height for knife-edge recriutment   
		RS = 90 # RS = Shell height 1 year previous to CS
		bnk = "BBn" # use bnk for all bank specifics helps to generalize the code for later
		
		
		## Set up plot titles
		survey.title <- substitute(bold(paste("Survey (",bank,"-",year,")",sep="")),
		                           list(year=as.character(yr),bank=bnk))
		tow.track.title <- substitute(bold(paste("Tow tracks (",bank,"-",year,")",sep="")),
		                              list(year=as.character(yr),bank=bnk))
		fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
		                              list(a=as.character(CS-5),year=as.character(yr),bank=bnk))
		rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
		                             list(a=as.character(CS-6),b=as.character(RS-5),year=as.character(yr),bank=bnk))
		pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
		                            list(b=as.character(RS-5),year=as.character(yr),bank=bnk))
		cf.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
		                            list(year=as.character(yr),bank=bnk))
		mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
		                            list(m=as.character(CS-5),year=as.character(yr),bank=bnk))
		survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
		                                list(year=as.character(yr),bank=bnk))
		survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
		                                 list(bank=bnk))
		SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
		                         list(bank=bnk))
		MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-",year,")",sep="")),
		                         list(year=as.character(yr),bank=bnk))
		CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank,")",sep="")),
		                          list(year=as.character(yr),bank=bnk))
	  clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-",year,")",sep="")),
		                             list(c=as.character(85),bank=bnk,year=as.character(yr)))
		clap.ts.title <- substitute(bold(paste("Clapper time series (",bank,")",sep="")),
		                                      list(bank=bnk))
		clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,")",sep="")),
		                                      list(bank=bnk))
		# Bring in the seeboxes that were closed in BBn in 2014
		BBboxes <- as.PolySet(subset(seedboxes,Bank==bnk & Open >= paste(yr,"-01-01",sep="")),projection = "LL")
		
		# Survey Polygons for BBn
		#Read20 read removed... The detailed survey strata polygon.
		BBn.survey.detail.poly<-subset(survey.detail.polys,label==bnk)
		attr(BBn.survey.detail.poly,"projection")<-"LL"
		
		#Read25 read removed... Get all the details of the survey strata
		BBn.surv.info <- subset(survey.info,label== bnk)
		
		#Read27 read removed... Get the towable area for BBn
		BBn.strata.areas <- subset(BBn.surv.info,select =c("PID","towable_area"))
		# Now get the survey boundary polygon
		BBn.survey.bound.poly <- subset(survey.bound.polys,label==bnk)
		
		
		# TOW TRACK DATA BBn BANK Need to input the number of tows, file location, weighting scheme, seconds between readings
		#Source10 source("fn/getdis.r") 
		BBnDis<-dist.coef(c(201:300,950:966),path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/BBn/",sep=""),
		                  w=c(1:10,9:1),rule=8,smooth=T,plt=F)
		
		# Here is the survey data for BBn
		survBBn.dat<-subset(springSurv.dat,bank==bnk)
		
		#Reassign the strat based on location and write to the screen what percentage of strata were reassigned.
		#Source20 #source("fn/restratwp.r",local=T)
		survBBn.dat<-restratwp(survBBn.dat,list(BBn.survey.detail.poly))
		survBBn.dat$tow[survBBn.dat$year %in% 1991:2008]<-survBBn.dat$tow[survBBn.dat$year %in% 1991:2008]+200
		paste("Strata reassigned",
		      with(subset(survBBn.dat,year==yr & state == 'live' & random == 1),
		           round((1-sum(stratum == new.stratum, na.rm = T) / length(stratum))*100)),'%')
		
		# Meat weight data from BBn for 2011 - Current, stitch them all together.
		BBn11.mw<-subset(Springsurv2011$MWs,bank==bnk)
		BBn12.mw<-na.omit(subset(Springsurv2012$MWs,bank==bnk))
		BBn13.mw<-subset(SurvDB$MWs,bank==bnk&year==2013)
		BBn14.mw<-subset(SurvDB$MWs,bank==bnk&year==2014)
		BBn.mw<-rbind(BBn11.mw,BBn12.mw,BBn13.mw,BBn14.mw)
		
		#Convert shell height in current year to decimeters.
		BBn14.mwdm<-BBn14.mw
		BBn14.mwdm$sh<-BBn14.mw$sh/100
		# MEAT WEIGHT DATA - hydration sampling, it contains data from 1983-2007, tidy up the ID 
		BBn.wgt.dat<-subset(MW.dat,bank==bnk)
		BBn.wgt.dat$ID<-paste(BBn.wgt.dat$year,BBn.wgt.dat$tow,sep='.')
		# Grab the relavent Meat-Weight Shell height data and make a flat file from it
		BBnmw.dat<-merge(subset(BBn.wgt.dat,year>1990&month%in%5:6,c("ID","year","lon","lat","depth","sh","wmw")),
		                 subset(BBn.mw,select=c("ID","year","lon","lat","depth","sh","wmw")),all=T)
		#Write9
		write.csv(BBnmw.dat,paste(direct,"Data/Survey_Data/",yr,"/Spring/BBn/BBnmwData.csv",sep=""),row.names=F)
		
		# MODEL - This is the meat weight Shell height realationship.  
		#MEAT WEIGHT SHELL HEIGHT RELATIONSHIP 2014 
		#Source11 source("fn/shwt.lme.r") note thtat the exponent is set as a parameter here b=3
		BBn.SpatHtWt.fit<-shwt.lme(BBn14.mwdm,random.effect='tow',b.par=3)
		
		## MODEL - This is the model used to esimate condition factor across BBn
		# We have more data than we did for Sable so a more complex model is fit.  
		#Source13 source("fn/condFac.r")
		BBncfData<-condFac(na.omit(BBnmw.dat),survBBn.dat,model.type='gam_f')
		
		# Output the predictions for BBn from the Condition Factor model
		survBBn.dat<-BBncfData$pred.dat
		# tidy up the ID and Pull out the ID and condition factor
		survBBn.dat$ID<-paste(survBBn.dat$year,survBBn.dat$tow,sep='.')
		tmp.dat<-subset(BBncfData$CF.data,select=c("ID","CF"))
		# Rename CF to CFh
		names(tmp.dat)[2]<-"CFh"
		# merge the two data sets, keeping all survBBn.dat values
		survBBn.dat<-merge(survBBn.dat,tmp.dat,all.x=T)
		# Replace any NA's in CFh with the original Condition Factor.
		survBBn.dat$CFh[is.na(survBBn.dat$CFh)]<-survBBn.dat$CF[is.na(survBBn.dat$CFh)]
		
		# Calculate the biomass of the Pre-recruits, Recruits and the Commerical Scallops in each tow on BBn
		#Source14 source("fn/surv.by.tow.r")
		survBBn.dat<-surv.by.tow(survBBn.dat, pre.ht=RS, rec.ht=CS)
		survBBn.dat<-surv.by.tow(survBBn.dat,pre.ht=RS, rec.ht=CS, type='B', mw.par="CFh")
		
		#Write10 Out put these results
		write.table(survBBn.dat,paste(direct,"Data/Survey_Data/",yr,"/Spring/BBn/BBnSurvey9114.csv",sep=""),sep=',',row.names=F)
		
		# Subset the survey data into the clappers(dead) and live scallops and 
		# the results from tows classifed a "1" (I believe non-matched)
		survBBnClap.dat<-subset(survBBn.dat,state=='dead')
		survBBnLive.dat<-subset(survBBn.dat,state=='live')
		survBBnRandom.dat<-subset(survBBn.dat,state=='live'& random==1)
		
		# The BBn survey data (from PEDstrata)
		#Source21 source("fn/survey.dat.r")# survey data for model input (Note this is the only place RS=RS and CS=CS)
		BBnsurvey.obj<-survey.dat(survBBnRandom.dat, years=years, RS=RS, CS=CS, bk=bnk, 
		                          areas=BBn.strata.areas, mw.par="CFh")	
		BBnclap.obj<-survey.dat(survBBnClap.dat, years=years, RS=RS, CS=CS, bk=bnk, 
		                        areas=BBn.strata.areas, mw.par="CFh")
		
		# Add the condition factor and number of commercial and recruit size clappers.
		# The Condition is a weighted mean based on the commerical biomass for each year
		BBnsurvey.obj[[1]]$CF <- sapply(1:length(years),
		                                function(x){with(subset(survBBnRandom.dat,year == years[x]),
		                                                 weighted.mean(CFh,com.bm,na.rm=T))})
		BBnsurvey.obj[[1]]$clappers<-BBnclap.obj[[1]]$N
		BBnsurvey.obj[[1]]$clappersR<-BBnclap.obj[[1]]$NR
		
		#Source24 - This is where the BBn survey object is created, this is updated each year with the results of the
		# caclculations perfomed in this setion
		dump(c('BBnsurvey.obj','BBnclap.obj'),'BBnsurveyObj.R')
		# If we want to skip all the processing we could just call in the survey object like so...
		#Source24 source('BBnsurveyObj.R')
		
		
		# Here we pull out the data from within the seedboxes, this could be used
		# too look at results from any seedbox of interest as long as we have it's name (but if so use the seebox object
		# as BBboxes is subset to just be currently closed boxes on BBn)
		#Source15. #source("fn/simple.surv.r")
		box.dat <- data.frame(EID=1:nrow(survBBnLive.dat),X=survBBnLive.dat$lon,Y=survBBnLive.dat$lat)
		key<-findPolys(box.dat, subset(BBboxes,SCALLOP_Group_ID=='201401C2'))
		C2.14.obj<-simple.surv(survBBnLive.dat[1:nrow(survBBnLive.dat) %in% key$EID,],years=2007:yr)
		key<-findPolys(box.dat,subset(BBboxes,SCALLOP_Group_ID=='201401C3'))
		C3.14.obj<-simple.surv(survBBnLive.dat[1:nrow(survBBnLive.dat) %in% key$EID,],years=2007:yr)
		
		
		## CLAPPERS
		# Get the survey data for the Clappers on BBn  
		clap.obj<-survey.dat(survBBnClap.dat, BBn.SpatHtWt.fit, years=years, RS=RS, CS=CS, bk=bnk, 
		                     areas=BBn.strata.areas, mw.par="CFh")		
		# Combine this data with the live data.
		Clap.dat<-merge(data.frame(year=survBBnClap.dat$year,tow=survBBnClap.dat$tow,
		                           lon=survBBnClap.dat$lon,lat=survBBnClap.dat$lat,deadPre=survBBnClap.dat$pre,
		                           deadRec=survBBnClap.dat$rec,deadCom=survBBnClap.dat$com,
		                           dead85=survBBnClap.dat$rec+survBBnClap.dat$com),
		                data.frame(year=survBBnLive.dat$year,tow=survBBnLive.dat$tow,
		                           livePre=survBBnLive.dat$pre,liveRec=survBBnLive.dat$rec,
		                           liveCom=survBBnLive.dat$com,Live85=survBBnLive.dat$rec+survBBnLive.dat$com))
		# Caclculate the percentage of Clappers in population our estimate of M.
		Clap.dat$clap.propPre<-Clap.dat$deadPre / (Clap.dat$livePre+Clap.dat$deadPre)*100
		Clap.dat$clap.propRec<-Clap.dat$deadRec / (Clap.dat$liveRec+Clap.dat$deadRec)*100
		Clap.dat$clap.propCom<-Clap.dat$deadCom / (Clap.dat$liveCom+Clap.dat$deadCom)*100
		Clap.dat$clap.prop<-(Clap.dat$deadCom+Clap.dat$deadRec) / 
		  (Clap.dat$liveCom+Clap.dat$deadCom+Clap.dat$liveRec+Clap.dat$deadRec)*100
		
	
		### GENERATE THE CONTOURS for each of the BBn distribtuion maps ###
		# Pre-Recruit (<95 mm shell height)		# Distribution plots for all tows on BBn 2014
		#Source19 # This gets the contours for pre-recruits in 2012
		pre.contours.2012<-contour.gen(subset(survBBnLive.dat,year==2012,c('tow','lon','lat','pre')),
		                               ticks='define',nstrata=9,str.min=0,interp.method='gstat',
		                               points=T,blank=T,res=0.005,key='log.cont',
		                               color.fun=tim.colors,id.par=3.5,units='#/tow',plot=F,blank.dist=0.08,
		                               subset.poly='square',subset.eff=0,subscale=0.25,direct=direct)
		# And 2014.
		pre.contours<-contour.gen(subset(survBBnLive.dat,year==yr,c('tow','lon','lat','pre')),
		                               ticks='define',nstrata=9,str.min=0,interp.method='gstat',
		                               points=T,blank=T,res=0.005,key='log.cont',
		                               color.fun=tim.colors,id.par=3.5,units='#/tow',plot=F,blank.dist=0.08,
		                               subset.poly='square',subset.eff=0,subscale=0.25,direct=direct)
		# Recruit (95 - 105 mm shell height)
		rec.contours<-contour.gen(subset(survBBnLive.dat,year==yr,c('tow','lon','lat','rec')),
		                          ticks=c(1,5,10,50,100,500,1000,5000),str.min=0,interp.method='gstat',
		                          points=T,blank=T,res=0.005,key='log.cont',color.fun=tim.colors,
		                          id.par=3.5,units='#/tow',plot=F,blank.dist=0.08,subset.poly='square',
		                          subset.eff=0,subscale=0.25,direct=direct)
		# Commercial sized scallop (> 105 mm)
		com.contours<-contour.gen(subset(survBBnLive.dat,year==yr,c('tow','lon','lat','com')),
		                          ticks=c(1,5,10,50,100,500,1000,5000),str.min=0,interp.method='gstat',
		                          points=T,blank=T,res=0.005,key='log.cont',color.fun=tim.colors,
		                          id.par=3.5,units='#/tow',plot=F,blank.dist=0.08,subset.poly='square',
		                          subset.eff=0,subscale=0.25,direct=direct)
		
		# MEAT COUNT & CONDITION FACTOR OF SCALLOP ON BBn (requires a bit of preprocessing)
		BBnCF2014.dat<-na.omit(merge(subset(na.omit(SurvDB$pos),bank==bnk&year==yr,
		                                    c('tow','lon','lat')),BBn.SpatHtWt.fit$fit))
		names(BBnCF2014.dat)[4]<-"CF"
		BBnCF2014.dat<-merge(BBnCF2014.dat,subset(survBBnLive.dat,year==yr,c('year','tow','lon','lat',
		                                                                       paste('h',seq(5,200,5),sep=''))))
		#Source14 source("fn/surv.by.tow.r") surv.by.tow calculates number and/or biomass of pre, rec and 
		#com size scallops in each tow + meat count.
		BBnCF2014.dat<-surv.by.tow(BBnCF2014.dat, yr, pre.ht=RS, rec.ht=CS)
		BBnCF2014.dat<-surv.by.tow(BBnCF2014.dat, yr, pre.ht=RS, rec.ht=CS, type='B', mw.par="CF")
		
		# Meat Count per 500 g
		BBnCF2014.dat$meat.count<-0.5/(BBnCF2014.dat$com.bm/BBnCF2014.dat$com)
		
		# CONDITION FACTOR MEAT COUNT and Clappers BBn in 2014 
		#Source19 source("fn/contour.gen.r")
		mc.contours<-contour.gen(subset(BBnCF2014.dat,select=c('tow','lon','lat','meat.count')),ticks='define',
		                         nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',direct=direct,
		                         key='strata',blank=F,plot=F,subset.poly='square',subset.eff=0,subscale=0.25)
		# Condition Factor
		cf.contours<-contour.gen(subset(BBnCF2014.dat,select=c('tow','lon','lat','CF')),ticks='define',
		                         nstrata=7,str.min=0,place=2,id.par=3.5,interp.method='gstat',direct=direct,
		                         key='strata',blank=F,plot=F,subset.poly='square',subset.eff=0,subscale=0.25)
		# Clapper contour plot
		clap.contours<-contour.gen(na.omit(subset(Clap.dat,year==yr,c('tow','lon','lat','clap.prop'))),
		                           ticks='define',nstrata=9,str.min=0,interp.method='gstat',direct=direct,
		                           points=T,blank=T,res=0.005,key='log.cont',color.fun=tim.colors,id.par=3.5,
		                           units='#/tow',plot=F,blank.dist=0.08,subset.poly='square',subset.eff=0,subscale=0.25)
		
		
### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES
### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES
### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES### BBn FIGURES
		
		
		# Plot of the strata and center point of the tows. (SLIDE 42 of 2014 presentation.)
		# BBn strata is BASED ON HISTORICAL COMMERICAL CATCH RATE NOT SURVEY DATA AS THE 
		# SURVEY INFO ON BBn was too sparse.
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,poly.lst=list(BBn.survey.detail.poly,BBn.surv.info),xlab="",ylab="",title=survey.title,
		           bathy.source="quick",plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survBBnLive.dat,subset=year==yr & random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survBBnLive.dat,subset=year==yr  & random==4,pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topleft",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",
		       legend = c(paste('regular (n =',length(unique(subset(survBBnLive.dat,year==yr & random==1)$tow)),")",sep=""),
		                  paste('exploratory (n =',length(unique(subset(survBBnLive.dat,year==yr & random==4)$tow)),")",sep="")),
		       inset=0.01,bg='white',box.col='white')
		legend("bottomleft",legend=BBn.surv.info$PName,fill=BBn.surv.info$col,bty='n',cex=1, title = "Strata")
		legend("bottom",legend = round(BBn.surv.info$area_km2),
		       fill=c(BBn.surv.info$col),border=c(rep('black',length(BBn.surv.info$PName))),
		       pch=c(rep(NA,length(BBn.surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.1,
		       pt.bg = c(rep(NA,length(BBn.surv.info$PName))),col='black',bty='n')

		legend("bottomright",legend = as.numeric(with(subset(survBBnLive.dat,year== yr),
		                                            tapply(tow,new.stratum,length))),
		       fill=c(BBn.surv.info$col),border=c(rep('black',length(BBn.surv.info$PName))),
		       pch=c(rep(NA,length(BBn.surv.info$PName))),title = "Number of tows",title.adj=0.1,
		       pt.bg = c(rep(NA,length(BBn.surv.info$PName))),col='black',bty='n')
		# Add the survey boxes.
		addPolys(BBboxes,lty=2,lwd=2)
		
	
		# SURVEY TIME SERIES OF ABUNDANCE AND BIOMASS (SLIDES 43 and 44 of 2014 presentation.)
		#Source17 source("fn/survey.ts.r",local=T)
		survey.ts(BBnsurvey.obj[[1]],1991:yr,Bank=bnk,pdf=F, RS=RS-5, CS=CS-5,
		          areas=BBn.strata.areas$towable_area,ys=.99,clr='blue',se=T,pch=16,yl2=c(5000,750,750),
		          add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
		survey.ts(BBnsurvey.obj[[1]],1991:yr,Bank=bnk,pdf=F,type='B', 
		          RS=RS-5, CS=CS-5,areas=BBn.strata.areas$towable_area,ys=.99,clr='blue',se=T,pch=16,yl2=12,
		          add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
		
	
		
		# Shell height frequency plot, SLIDE 45, 2014 presentation
		#Source18 #source("fn/shf.plt.r")
		shf.plt(ps.dat,BBnsurvey.obj,from='surv',yr=2007:yr,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),
		        rel=F,recline=c(85,95),rows=8,mean.line=F,ymax=730,adj=0.9,add.title = T,titl = SHF.title,cex.mn=3)	
		
		
		
		# Pre-recruits for BBn in 2014 (< 95 mm shell height) Slide 48 2014 presentation
		lvls=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		precont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F) 
		# This is a new group of levels to get at the crazy big 2013 pre-recruit outbreak.
		lvls2=c(2000,5000,10000,20000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls2)
		CP <- convCP(CL)
		preCont2.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		preCont2.poly$PID<-preCont2.poly$PID+length(lvls)
		precont2.data<- data.frame(PID=(length(lvls)+1):(length(lvls)+length(lvls2)),
		                           col=rev(brewer.pal(length(lvls2)*2,X.lvl))[(1:length(lvls2)+1)],border=NA)
		lvls<-c(lvls,lvls2)
		precont.data<-rbind(precont.data,precont2.data)
		preCont.poly<-rbind(preCont.poly,preCont2.poly)
		#This is the prerecruit plot in the presentation.  There is also a 2013 version of this in the ppt.
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,contour=list(preCont.poly,precont.data),title=pre.rec.title,
		           bathy.source="quick",plot.bathy=T,plot.boundries=T,boundries="offshore",
		           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survBBnLive.dat,subset=year==yr & random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survBBnLive.dat,subset=year==yr  & random==4,pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topleft",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",
		       legend = c(paste('regular (n =',length(unique(subset(survBBnLive.dat,year==yr & random==1)$tow)),")",sep=""),
		                  paste('exploratory (n =',length(unique(subset(survBBnLive.dat,year==yr & random==4)$tow)),")",sep="")),
		       inset=0.01,bg='white',box.col='white')
		
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(precont.data$col),title=N.tow.lab, title.adj = 0.2,
		       border=c(rep(length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
		addPolys(BBboxes,lty=2,lwd=2)
		
		# Recruit Scallops on BBn in 2014 (Slide 50 in 2014 presentation)
		lvls=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(rec.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		recCont.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		recCont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),
		                          border=NA,stringsAsFactors = F) 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,contour=list(recCont.poly,recCont.data),title=rec.title,
		           bathy.source="quick",plot.bathy=T,plot.boundries=T,boundries="offshore",
		           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survBBnLive.dat,subset=year==yr & random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survBBnLive.dat,subset=year==yr  & random==4,pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topleft",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",
		       legend = c(paste('regular (n =',length(unique(subset(survBBnLive.dat,year==yr & random==1)$tow)),")",sep=""),
		                  paste('exploratory (n =',length(unique(subset(survBBnLive.dat,year==yr & random==4)$tow)),")",sep="")),
		       inset=0.01,bg='white',box.col='white')
		
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(recCont.data$col),title=N.tow.lab, title.adj = 0.2,
		       border=c(rep(length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
				addPolys(BBboxes,lty=2,lwd=2)
		
		# Fully Recruited Scallops on BBn in 2014 (Slide 52 in 2014 presentation)
		
		lvls=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(com.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		Cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),
		                       border=NA,stringsAsFactors = F) 
		#Source9
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,contour=list(comCont.poly,Cont.data),title=fully.rec.title,
		           bathy.source="quick",plot.bathy=T,plot.boundries=T,boundries="offshore",
		           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survBBnLive.dat,subset=year==yr & random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survBBnLive.dat,subset=year==yr  & random==4,pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topleft",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",
		       legend = c(paste('regular (n =',length(unique(subset(survBBnLive.dat,year==yr & random==1)$tow)),")",sep=""),
		                  paste('exploratory (n =',length(unique(subset(survBBnLive.dat,year==yr & random==4)$tow)),")",sep="")),
		       inset=0.01,bg='white',box.col='white')
		
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(Cont.data$col),title=N.tow.lab, title.adj = 0.2,
		       border=c(rep(length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
				addPolys(BBboxes,lty=2,lwd=2)
		
		
############################ SEEDBOXES############################ SEEDBOXES############################ SEEDBOXES##############
############################ SEEDBOXES############################ SEEDBOXES############################ SEEDBOXES##############
				
						# Shell Height Frequency from seedboxes C22014 and C32014. (Slides 53 and 54 from 2014 presenation)
		#Source18 #source("fn/shf.plt.r")
		shf.plt(ps.dat,C2.14.obj,from='surv',yr=2007:yr,col1='grey80',type='sh',col2=1,col3=1,
		        xl=c(0,200),rel=F,recline=c(85,95),rows=8,mean.line=F,ymax=3600,adj=0.9,
		        add.title=T,cex.mn=3,titl= "Shell height frequency (Seedbox C2-2014)")
		shf.plt(ps.dat,C3.14.obj,from='surv',yr=2007:yr,col1='grey80',type='sh',col2=1,col3=1,
		        xl=c(0,200),rel=F,recline=c(85,95),wd=7,ht=8,rows=8,mean.line=F,adj=0.9,ymax=3600,
		        add.title=T,cx.mn=3,titl= "Shell height frequency (Seedbox C3-2014)")
		
		#SeedBoxes This is used as an inset in the powerpoint for C2-2014 (Sidebar on Slide 53, 2014 Presentation.)
		# I think I will make it a stand alone image.
		# To do so nicely we need this to get the regions in the seedboxes.
		BBn.tows.2014 <- subset(survBBnLive.dat,year==2014,select=c('tow','lon','lat'))
		names(BBn.tows.2014)<-c("EID","X","Y")
		C2 <-findPolys(BBn.tows.2014,subset(BBboxes,ID=='C2-012014'))
		C3 <-findPolys(BBn.tows.2014,subset(BBboxes,ID=='C3-012014'))
		
		
		
		# Pre-recruits for BBn in 2014 (< 95 mm shell height) Slide 48 2014 presentation
		lvls=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		precont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F) 
		# This is a new group of levels to get at the crazy big 2013 pre-recruit outbreak.
		lvls2=c(2000,5000,10000,20000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls2)
		CP <- convCP(CL)
		preCont2.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		preCont2.poly$PID<-preCont2.poly$PID+length(lvls)
		precont2.data<- data.frame(PID=(length(lvls)+1):(length(lvls)+length(lvls2)),
		                           col=rev(brewer.pal(length(lvls2)*2,X.lvl))[(1:length(lvls2)+1)],border=NA)
		lvls<-c(lvls,lvls2)
		precont.data<-rbind(precont.data,precont2.data)
		preCont.poly<-rbind(preCont.poly,preCont2.poly)
		
		windows(11,8.5)
		ScallopMap(ylim=c(42.797,42.867),xlim=c(-66.282,-66.12),bathy.source="quick",ylab="",xlab="",cex.mn=2,
		           contour=list(preCont.poly,precont.data),plot.bathy = T,plot.boundries = T,direct=direct,
		           title=substitute(bold(paste("Pre-recruit scallops (",""<b, " mm " ,"Seedbox C2-",year,")",sep="")),
		                            list(b=as.character(RS-5),year=as.character(yr))),dec.deg = F)
				addPolys(BBboxes,lty=2,lwd=2)
		addLines(BBnDis[[2]],col='blue')
		# Add the regular survey tows.
		points(slat~slon,survBBn.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1)
		# Add the exploratory survey tows
		points(slat~slon,survBBn.dat,subset=year==yr&state =='live' & random==4,pch=24,bg="darkorange",cex=1)
		# Add the regular survey tows.
		#points(elat~elon,survBBn.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1)
		# Add the exploratory survey tows
		#points(elat~elon,survBBn.dat,subset=year==yr&state =='live' & random==4,pch=24,bg="darkorange",cex=1)
		par(xpd=T)
		legend(-66.143,42.82,c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(precont.data$col),
		       border=c(rep(length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),box.col = "white",bg="white")
		legend(-66.143,42.8288,N.tow.lab,box.col="white",bg="white")
		legend("topright",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",inset=0.01,
		       legend = c(paste('regular (n =',
		                        length(subset(survBBn.dat,year==yr & state=='live'& random==1 & tow %in% C2$EID)$tow),")",sep=""),
		                  paste('exploratory (n =',
		                        length(subset(survBBn.dat,year==yr & state=='live'& random==4 & tow %in% C2$EID)$tow),")",sep="")),
		       bg='white',box.col='white')
		#SeedBoxes This is used as an inset in the powerpoint for C3-2014 (Sidbar on Slide 54, 2014 Presentation)
		windows(11,8.5)
		ScallopMap(ylim=c(42.78,42.885),xlim=c(-66.05,-65.94),bathy.source="quick",xlab="",ylab="",cex.mn=2,
		           contour=list(preCont.poly,precont.data),plot.bathy = T,plot.boundries = T,direct=direct,
		           title=substitute(bold(paste("Pre-recruit scallops (",""<b, " mm " ,"Seedbox C3-",year,")",sep="")),
		                            list(b=as.character(RS-5),year=as.character(yr))),dec.deg = F)
		addLines(BBnDis[[2]],col='blue')
		# Add the regular survey tows.
		points(slat~slon,survBBn.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1)
		# Add the exploratory survey tows
		points(slat~slon,survBBn.dat,subset=year==yr&state =='live' & random==4,pch=24,bg="darkorange",cex=1)
		# and bounding box and the legend (outside of plot area)
		addPolys(BBboxes,lty=2,lwd=2)
		par(xpd=T)
		legend(-65.92,42.84,c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(precont.data$col),title=N.tow.lab, title.adj = 0.2,
		       border=c(rep(length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
		legend(-65.925,42.865,pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",
		       legend = c(paste('regular (n =',
		                        length(subset(survBBn.dat,year==yr & state=='live'& random==1 & tow %in% C3$EID)$tow),")",sep=""),
		                  paste('exploratory (n =',
		                        length(subset(survBBn.dat,year==yr & state=='live'& random==4 & tow %in% C3$EID)$tow),")",sep="")),
		       bg='white',box.col='white')
		
		
		
		# SURVEY TIME SERIES OF CLAPPERS (SLIDE 55 of 2014 presentation.)
		# DK note this isn't identical to the slide, close but not quite.
		survey.ts(BBnclap.obj[[1]],1991:yr,Bank=bnk,pdf=F, RS=RS-5, CS=CS-5,
		          areas=BBn.strata.areas$area,ys=.99,ht=6.5,wd=10,clr='blue',se=T,pch=20,yl2=65,
		          add.title = T,titl = clap.ts.title,cx.mn=3,axis.cx = 1.5)
		
		
		# Proportion of Clappers in the data, Slide 56 in the powerpoint
		# DK Note: PROBLEM This clapper plot is not producing the same result as Slide 56 in the powerpoint.  
		# Definitely something different in the data than what was used in this plot.  Go back and compare!
		#Source25 source("fn/Clap3.plt.R")
		Clap3.plt(Clap.dat,years=1991:yr,yl=c(0,25),add.title = T,cex.mn = 3,
		          titl = clap.per.ts.title ,CS=CS-5,RS=RS-5,axis.cx = 1.5)
		
		
		
		# CLAPPER SPATIAL DISTRIBUTION BBn 2014 (Slide 57 in 2014 presentation)
		lvls=c(1:5,10,15,20,50)
		CL <- contourLines(clap.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		precont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),clap.col),border=NA,stringsAsFactors = F) 
		
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,contour=list(preCont.poly,precont.data),title=clap.dis.title,
		           bathy.source="quick",plot.bathy=T,plot.boundries=T,boundries="offshore",
		           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survBBnLive.dat,subset=year==yr & random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survBBnLive.dat,subset=year==yr  & random==4,pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topleft",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",
		       legend = c(paste('regular (n =',length(unique(subset(survBBnLive.dat,year==yr & random==1)$tow)),")",sep=""),
		                  paste('exploratory (n =',length(unique(subset(survBBnLive.dat,year==yr & random==4)$tow)),")",sep="")),
		       inset=0.01,bg='white',box.col='white')
		
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(precont.data$col),title="% Dead", title.adj = 0.2,
		       border=c(rep(length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
				addPolys(BBboxes,lty=2,lwd=2)
		
		
		# MEAT WEIGHT SHELL HEIGHT PLOT SABLE BANK (slide 58, 2014 presentation)
		windows(15,8)
		par(mfrow=c(1,2))
		#Source12 source("fn/shwt.plt1.r") 
		shwt.plt1(BBn.SpatHtWt.fit,lw=3,ht=10,wd=12,cx=1.5,titl = MWSH.title,cex.mn = 2,las=1)
		# CONDITION FACTOR TIME SERIES (SLIDE 58, 2014 presentation)
		#Source16 source("fn/stdts.plt.R")
		stdts.plt(merge(BBncfData$CFyrs,data.frame(year=years),all=T),y='CF',pch=16,ylab = cf.lab,
		          mean.line=T,graphic='none',xlab='Year',ylim=c(8,18),las=1,
		          titl = CF.ts.title,cex.mn = 2)
		
		
		
		#CONDITION FACTOR on BBn for 2014. SLide 59 from 2014 presenation 
		lvls=8:16
		CL <- contourLines(cf.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		cont.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),cf.col),border=NA,stringsAsFactors = F) 
		
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,contour=list(cont.poly,cont.data),title=cf.title,
		           bathy.source="quick",plot.bathy=T,plot.boundries=T,boundries="offshore",
		           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		# Grab the exploratory/regular tows from the condition factor data.
		exp.tw <- subset(survBBnLive.dat,year==2014 & random==4)$tow
		cf.exp <- subset(BBnCF2014.dat,tow%in% exp.tw)
		cf.reg <- BBnCF2014.dat[-which(BBnCF2014.dat$tow %in% exp.tw),]
		
		
		# Add the regular survey tows.
		points(lat~lon,cf.reg,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,cf.exp,pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topleft",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",
		       legend = c(paste('regular (n =',length(cf.reg$tow),")",sep=""),
		                  paste('exploratory (n =',length(cf.exp$tow),")",sep="")),
		       inset=0.01,bg='white',box.col='white')
		
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(cont.data$col),title=cf.lab, title.adj = 0.2,
		       border=c(rep(length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
				addPolys(BBboxes,lty=2,lwd=2)
		
		
		# MEAT COUNT on BBn for 2014.  Slide 60 of 2014 presentation
		lvls=seq(5,40,5)
		div=3
		CL <- contourLines(mc.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		cont.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		Ncol=length(unique(cont.poly$PID))+div
		cont.data<- data.frame(PID=unique(cont.poly$PID),
		                       col=brewer.pal(Ncol,mc.col)[c(Ncol:(div+2),1)],border=NA,stringsAsFactors = F)
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk,contour=list(cont.poly,cont.data),title=mc.title,
		           bathy.source="quick",plot.bathy=T,plot.boundries=T,boundries="offshore",
		           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,cf.reg,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,cf.exp,pch=24,bg="darkorange",cex=0.8)
		# Add the legends
		legend("topleft",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",
		       legend = c(paste('regular (n =',length(cf.reg$tow),")",sep=""),
		                  paste('exploratory (n =',length(cf.exp$tow),")",sep="")),
		       inset=0.01,bg='white',box.col='white')
		
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(cont.data$col),title=mc.lab, title.adj = 0.2,
		       border=c(rep(length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
				addPolys(BBboxes,lty=2,lwd=2)
		
################  NOT in presentation but likely of interest  ############################################
################  NOT in presentation but likely of interest  ############################################
		
		# Pre-Recruits for BBn in 2012 (Slide 46 in presentation but won't be in this one...)
		lvls=c(20,50,100,200,500,1000,2000,5000,10000)
		CL <- contourLines(pre.contours.2012$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,BBn.survey.bound.poly)
		precont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),
		                          border=NA,stringsAsFactors = F) 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(12,8)
		ScallopMap(ylim=c(42.5,42.95),xlim=c(-66.5,-65.7),bathy.source="quick",plot.bathy = T,
		           direct=direct,contour=list(preCont.poly,precont.data),
		           title="BBn 2012 pre-recruits (< 85 mm shell height)",cex=1.2,plot.boundries = T,boundries="offshore",dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survBBn.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survBBn.dat,subset=year==yr&state =='live' & random==4,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c('exploratory','survey',paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=precont.data$col,title="#/tow",
		       border=c(NA,NA,rep('black',length(lvls))),pch=c(24,20,rep(NA,length(lvls))),
		       pt.bg = c("darkorange",rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white',cex=1.2)
		addPolys(BBboxes,lty=2,lwd=2)
		
		
		# MEAT WEIGHT SHELL HEIGHT PLOT looking at each random factor (tow) individudally, not presented
		#Source22 source("fn/shwt.plt.r")
		shwt.plt(BBn.SpatHtWt.fit,pt.col=rgb(0,0,0,0.2))
		
		# Shell Height Frequency of Clappers.
		shf.plt(ps.dat,BBnclap.obj,from='surv',yr=2008:yr,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),
		        rel=F,recline=c(85,95),wd=7,ht=8,rows=7,mean.line=F,ymax=14,adj=0.9)	
		
		# Clapper time series on Browns north from PEDstrata. #Source17 source("fn/survey.ts.r",local=T)
		# DK Note:  Need to think about this plot a little bit, this is the output from survey.dat
		# thus these estiamtes are right from PEDstrata, the plot we currently use is plotting raw survey results.
		survey.ts(clap.obj[[1]],1991:yr,Bank=bnk,pdf=F, RS=RS-5, CS=CS-5,areas=BBn.strata.areas$area,ys=.8,
		          ht=7,wd=10,clr='blue',se=T,pch=16, plots=c('rec','com'))
		
		# Percentage of clappers in survey, only 1 panel.  Source26 source("fn/Clap.plt.R")
		Clap.plt(na.omit(Clap.dat),years=1991:yr,yl=c(0,10))
		
		
		######## Fishery Data to make the time series of Catch and CPUE on BBn
		#Source27 source("fn/fishery.dat.r")
		BBn.cpue.ts<-fishery.dat(fish.dat,bk=bnk,yr=1981:yr,method='jackknife',direct=direct) 			
		# Subset the data to be all of Browns Bank from June 1st to present.
		BBnfish.dat<-na.omit(subset(fish.dat,bank==bnk& date > as.Date("2013-06-01"),c('ID','lon','lat','pro.repwt')))
		names(BBnfish.dat)[1:4]<-c("EID","X","Y","Z")
		
		#Source28 source("fn/fishsum.plt.r")
		fishsum.plt(BBn.cpue.ts,years=1981:yr)
		
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		# Spatial map of Catch on BBn in 2014.  Not presented.
		#Source29 source("fn/gridPlot.r")
		lvls=c(10,50,100,500,1000,5000,10000,50000)
		
		windows(12,8)
		BBnpolys <- gridPlot(BBnfish.dat,BBn.survey.bound.poly,lvls,border=NA,FUN=sum,grid.size=1/60)
		ScallopMap(ylim=c(42.5,42.95),xlim=c(-66.5,-65.7),bathy.source='usgs',plot.boundries=T,plot.bathy = T,
		           direct=direct,poly.lst=BBnpolys[1:2],title="",dec.deg = F)
		tlvls<-lvls/1000
		# Add the regular survey tows.
		points(lat~lon,survBBn.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survBBn.dat,subset=year==yr&state =='live' & random==4,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c('exploratory','survey',paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(NA,NA,BBnpolys[[3]]),title='tons landed',
		       border=c(NA,NA,rep('black',length(lvls))),pch=c(24,20,rep(NA,length(lvls))),
		       pt.bg = c("darkorange",rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white',cex=1.1)
		# Add the survey boxes.
		addPolys(subset(BBboxes),lty=2)
		
		################################################################################################################		
		################################ END SECTION 5 BBn #####################################################
		################################################################################################################				
		
		
		
		
		
		################################################################################################################
		################################ SECTION 6 Georges BANK SPRING##################################################
		### Georges BANK SPRING ### Georges BANK SPRING ### Georges BANK SPRING ### Georges BANK SPRING ### ############
		################################ SECTION 5 Georges Bank Spring #################################################
		################################################################################################################
		
		# Shell height for knife edge recruitment based on portsampling data DK note: These are 5 higher than reality.
		CS = 100 # CS = Shell height for knife-edge recriutment   
		RS = 90 # RS = Shell height 1 year previous to CS
		bnk = c("GB","GBa","GBb") # use bnk for all bank specifics helps to generalize the code for later
		# GB spring specific dates.
		years=1984:yr
		
		## Set up plot titles
		survey.title <- substitute(bold(paste("Survey (",bank,"-Spring ",year,")",sep="")),
		                           list(year=as.character(yr),bank=bnk[1]))
		tow.track.title <- substitute(bold(paste("Tow tracks (",bank,"-Spring ",year,")",sep="")),
		                              list(year=as.character(yr),bank=bnk[1]))
		fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-Spring ",year,")",sep="")),
		                              list(a=as.character(CS-5),year=as.character(yr),bank=bnk[1]))
		rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-Spring ",year,")",sep="")),
		                        list(a=as.character(CS-6),b=as.character(RS-5),year=as.character(yr),bank=bnk[1]))
		pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-Spring ",year,")",sep="")),
		                            list(b=as.character(RS-5),year=as.character(yr),bank=bnk[1]))
		cf.title <- substitute(bold(paste("Condition factor (", bank,"-Spring ",year,")",sep="")),
		                       list(year=as.character(yr),bank=bnk[1]))
		mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-Spring ",year,")",sep="")),
		                       list(m=as.character(CS-5),year=as.character(yr),bank=bnk[1]))
		survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank," Spring)",sep="")),
		                                list(year=as.character(yr),bank=bnk[1]))
		survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank," Spring)",sep="")),
		                                 list(bank=bnk[1]))
		SHF.title <-  substitute(bold(paste("Shell height frequency (",bank," Spring)",sep="")),
		                         list(bank=bnk[1]))
		MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-Spring ",year,")",sep="")),
		                         list(year=as.character(yr),bank=bnk[1]))
		CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank," Spring)",sep="")),
		                          list(year=as.character(yr),bank=bnk[1]))
		clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-Spring ",year,")",sep="")),
		                             list(c=as.character(85),bank=bnk[1],year=as.character(yr)))
		clap.ts.title <- substitute(bold(paste("Clapper time series (",bank," Spring)",sep="")),
		                            list(bank=bnk[1]))
		clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank," Spring)",sep="")),
		                                list(bank=bnk[1]))
		## Set up plot titles

		
		# Subset the seed-boxes for GBa
		GBa.boxes <- as.PolySet(subset(seedboxes,Bank==bnk[2] & Open >= paste(yr,"-01-01",sep="")), projection = "LL")
		
		# Get the GB area polygon
		GBarea.poly<-newAreaPolys[newAreaPolys$label=="SFA27",]
		
		
		#survGB.dat<-subset(springSurv.dat,bank=='GB') Survey data for spring on both Georges Banks
		survGBMay.dat<-subset(springSurv.dat,bank %in% bnk & year > 1989)
		
		#Source10 source("fn/getdis.r") Calculate the tow track distances for Georges Spring survey.  
		GBDis<-dist.coef(subset(survGBMay.dat,year==yr & state=='live')$tow,
		                 path=paste(direct,"Data/Tow_tracks/",yr,"/Spring/GB/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)
		
		# Meat weight data for Georges Spring from 2011 - 2014, stitch them all together.
		GB11.mw<-subset(Springsurv2011$MWs,bank==bnk[1])
		GB12.mw<-subset(Springsurv2012$MWs,bank==bnk[1])
		GB13.mw<-subset(SurvDB$MWs,bank %in% bnk[2:3] & cruise=="TE15")
		GB14.mw<-subset(SurvDB$MWs,bank %in% bnk[2:3] & cruise=="TE17")
		GB.mw<-rbind(GB11.mw,GB12.mw,GB13.mw,GB14.mw)
		# Convert shell height in 2014 to decimeters.
		GB14.mwdm<-GB14.mw
		GB14.mwdm$sh<-GB14.mw$sh/100
		
		# MEAT WEIGHT DATA - hydration sampling, it contains data from 1983-2007, tidy up the ID 
		GB.wgt.dat<-subset(MW.dat,bank %in% bnk)
		GB.wgt.dat$ID<-paste(GB.wgt.dat$cruise,GB.wgt.dat$tow,sep='.')
		# Grab the relavent Meat-Weight Shell height data and make a flat file from it
		GBmwMay.dat<-merge(subset(GB.wgt.dat,month%in%5:6&tow%in%301:324,c("ID","year","lon","lat","depth","sh","wmw","tow")),
		                   subset(GB.mw,tow%in%301:324,c("ID","year","lon","lat","depth","sh","wmw","tow")),all=T)
		#Write11 
		write.csv(GBmwMay.dat,paste(direct,"Data/Survey_data/",yr,"/Spring/GB/GBMaymw.csv",sep=""),row.names=F)
		
		# MODEL - This is the meat weight Shell height realationship.  
		#MEAT WEIGHT SHELL HEIGHT RELATIONSHIP 2014 
		#Source11 source("fn/shwt.lme.r")
		GB.spring.SpatHtWt.fit<-shwt.lme(GB14.mwdm,random.effect='tow',b.par=3)
		
		## MODEL - This is the model used to esimate condition factor across GB spring
		# We have more data than we do for Sable so a more complex model is fit.  
		#Source13 source("fn/condFac.r")
		GBcfDataMay<-condFac(GBmwMay.dat,survGBMay.dat,model.type='gam_f')
		# Output the predictions for GB spring from the Condition Factor model and combine with the rest of the data
		survGBMay.dat<-merge(survGBMay.dat,GBcfDataMay$pred.dat,all=T)
		# Rename CF to CFMay
		names(GBcfDataMay$CFyrs)[5]<-'CFMay'
		# merge the two data sets, keeping all values
		CFdata<-merge(GBcfData$CFyrs,GBcfDataMay$CFyrs[,-(2:4)],all=T)
		# Add a year column which offsets the year slightly (for plotting purposes)
		CFdata$y2<-CFdata$year-0.25
		# DK Note:  WTF!! So we are fudging our CF data, some sort of seasonality correction, the 
		# plot is used in the presentation (Slide 67), what the heck we doing?
		# There is something weird here that I need to investigate further
		# The SH-MW plot agrees more with this fudge (that is May should be higher than Aug) but 
		# it may be the gam is doing something weird.  Needs a deeper investigation.
		fudge=1.85
		CFdata$CF<-CFdata$CF-fudge
		CFdata$CFMay<-CFdata$CFMay+fudge
		
		# Calculate the biomass of the Pre-recruits, Recruits and the Commerical Scallops in each tow on Sable Bank
		#Source14 source("fn/surv.by.tow.r")
		survGBMay.dat<-surv.by.tow(survGBMay.dat, years, pre.ht=RS, rec.ht=CS)
		survGBMay.dat<-surv.by.tow(survGBMay.dat, years, pre.ht=RS, rec.ht=CS, type='B', mw.par="CF")
		
		#Write12 Output these results
		write.table(survGBMay.dat,paste(direct,"Data/Survey_data/",yr,"/Spring/GB/GBMaySurvey8414.csv",sep=""),sep=',',row.names=F)
		
		
		# Assign the type of tow for the surveys for several of these we need to 'fix' a bunch of data.
		# DK note:  We are calling all tows before 2013 comparative tows, any 300's are repeats, and anything above 12 in 1988 is 
		# a comparative tow.  Likely this isn't a biggy, but the repeat one we need to be careful with that's for sure!!
		survGBMay.dat$random[survGBMay.dat$year < 2013] <-4
		survGBMay.dat$random[survGBMay.dat$tow %in% c(1:24,301:324)] <-3
		survGBMay.dat$random[survGBMay.dat$tow > 12 & survGBMay.dat$year == 1988] <- 4
		# Subset the survey data into the clappers(dead) and live scallops.
		survGBClap.dat<-subset(survGBMay.dat,state=='dead')
		survGBLive.dat<-subset(survGBMay.dat,state=='live')
		
		
		#Source15 source("fn/simple.surv.r") # Using the Live scallops and regular survey tows make the GB survey object.
		# Use all regular are repeat tows for this
		survGB.obj<-simple.surv(subset(survGBLive.dat,random %in% c(1,3)),years=1990:yr)
		# # Using the Live scallops and all regular, repeat, and exploratory tows here.
		survGB2.obj<-simple.surv(subset(survGBLive.dat,random %in% c(1,3,5)),years=1990:yr)
		
		
		
		
		### GENERATE THE CONTOURS for each of the Georges Bank Spring distribtuion maps ###
		
		# COMMERICAL SIZED SCALLOP ON Georges Bank Spring
		#Source19 source("fn/contour.gen.r")
		com.contours<-contour.gen(subset(survGBLive.dat,year==yr,c('tow','lon','lat','com')),
		                          ticks=c(1,5,10,50,100,500,1000,5000,10000,20000,50000),str.min=0,interp.method='gstat',
		                          points=T,blank=T,res=0.01,key='log.cont',color.fun=tim.colors,id.par=5,units='#/tow',
		                          plot=F,subscale=0.1,blank.dist=0.08,direct = direct)
		rec.contours<-contour.gen(subset(survGBLive.dat,year==yr,c('tow','lon','lat','rec')),
		                          ticks=c(1,5,10,50,100,500,1000,5000,10000,20000,50000),str.min=0,interp.method='gstat',
		                          points=T,blank=T,res=0.01,key='log.cont',color.fun=tim.colors,id.par=5,units='#/tow',
		                          plot=F,subscale=0.1,blank.dist=0.08,direct = direct)
		pre.contours<-contour.gen(subset(survGBLive.dat,year==yr,c('tow','lon','lat','pre')),
		                          ticks=c(1,5,10,50,100,500,1000,5000,10000,20000,50000),str.min=0,interp.method='gstat',
		                          points=T,blank=T,res=0.01,key='log.cont',color.fun=tim.colors,id.par=5,units='#/tow',
		                          plot=F,subset.poly='square',subscale=0.1,blank.dist=0.08,subset.eff=0,direct = direct)
		
		
		
		
		### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### 
		### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### 
		### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### 
		### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### GBa Spring FIGURES### 
		
		# The Survey stations for GBa spring survey, SLide 62 from 2014 presentation
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk[1],xlab="",ylab="",title=survey.title,nafo='all',nafo.bord = T, plot.EEZ = T,
		           bathy.source="quick",plot.bathy = T,plot.boundries = T,boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
		# Add the comparative survey tows.
		points(lat~lon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3,pch=23,bg='BLUE',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5),pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",
		       legend = c(paste('exploratory (n =',length(unique(subset(survGBLive.dat,year==yr & 
		                                                                  random%in% c(2,4,5))$tow)),")",sep=""),
		                  paste('comparative (n =',length(unique(subset(survGBLive.dat,year==yr & random==3)$tow)),")",sep="")),
		       
		       pch=c(24,23), pt.bg = c("darkorange","blue"),inset=0.01,bg='white',box.col='white',cex=1.1)
		# Add the survey boxes.
		addPolys(GBa.boxes,lty=2,lwd=2)
		
		

		#Source17 source("fn/survey.ts.r",local=T) Abundance time series, Slide 63 in presentation
		survey.ts(survGB.obj[[1]],1990:2014,Bank=bnk[1],pdf=F, RS=RS-5, CS=CS-5,ys=.8,
		         clr='blue',se=F,pch=16,yl2=c(4150,550,550),add.title = T,
		          titl=survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
		
		#Source18 source("fn/shf.plt.r") Shell Height Frequency data for GB spring, this is abundance (slide 64 2014 presentation)
		shf.plt(ps.dat,survGB.obj,from='surv',yr=2008:2014,rows=7,col1='grey80',adj=0.9,
		        type='sh',col2=1,col3=1,xl=c(0,150),ymax=1000,rel=F,recline=c(85,95),
		        add.title = T,titl=SHF.title,cex.mn=3)	
		
		
		
		# Pre-recruits (50 - 85 mm shell height) Slide 65 2014 presenation
		lvls=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,GBarea.poly)
		precont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F)
		# Because of the huge numbers we need a second color ramp here!
		lvls2=c(2000,5000,10000,20000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls2)
		CP <- convCP(CL)
		preCont2.poly <- joinPolys(CP$PolySet,GBarea.poly)
		preCont2.poly$PID<-preCont2.poly$PID+length(lvls)
		precont2.data<- data.frame(PID=(length(lvls)+1):(length(lvls)+length(lvls2)),
		                           col=rev(brewer.pal(length(lvls2)*2,X.lvl))[(1:length(lvls2)+1)],border=NA)
		lvls<-c(lvls,lvls2)
		precont.data<-rbind(precont.data,precont2.data)
		preCont.poly<-rbind(preCont.poly,preCont2.poly)
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk[1],contour=list(preCont.poly,precont.data), xlab="",ylab="",title=pre.rec.title,nafo='all',nafo.lab = F,
		           nafo.bord = T, plot.EEZ = T, bathy.source="quick",plot.bathy = T,plot.boundries = T,
		           boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
		
		# Add the comparative survey tows.
		points(slat~slon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3 , pch=23,bg='BLUE',cex=0.8)
		# Add the exploratory survey tows
		points(slat~slon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5), pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(precont.data$col),title=N.tow.lab,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white',title.adj=0.2)
     legend("topright",
       legend = c(paste('exploratory (n =',length(unique(subset(survGBLive.dat,year==yr & 
                                                                  random%in% c(2,4,5))$tow)),")",sep=""),
                  paste('comparative (n =',length(unique(subset(survGBLive.dat,year==yr & random==3)$tow)),")",sep="")),
           pch=c(24,23), pt.bg = c("darkorange","blue"),inset=0.01,box.col="white",bg="white")
      # Add the survey boxes.
      addPolys(subset(GBa.boxes),lty=2)

		
		
		
		
		
		# Meat Weight shell height plot.  Slide 66 in 2014 presentation. 
		#Source12 source("fn/shwt.plt1.r")
    windows(15,8)
		par(mfrow=c(1,2))
    shwt.plt1(GB.spring.SpatHtWt.fit,lw=3,cx=1.5,titl = MWSH.title,cex.mn = 2,las=1)
		#Source16 source("fn/stdts.plt.R") Slide 67 of 2014 presentation.
		stdts.plt(CFdata[-(1:3),],x=c('year','y2'),y=c('CF','CFMay'),pch=16,ylab=cf.lab,
		          mean.line=T,graphic='none',xlab='Year',ylim=c(10,20),titl = CF.ts.title,cex.mn=2,las=1)
		legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02)		
		
		
		# SEEDBOXES:  All of this is required to make something similar to Slide 68 in 2014 presentation, slightly modified to
		# to work with new seedbox data and added other information.
		# Grab the GB tows from 2014 from within the seedboxes.
		GBs.stows.2014 <- subset(survGBLive.dat,year==2014,select=c('tow','slon','slat'))
		names(GBs.stows.2014)<-c("EID","X","Y")
		GBs.etows.2014 <- subset(survGBLive.dat,year==2014,select=c('tow','elon','elat'))
		C1 <-findPolys(GBs.stows.2014,subset(GBa.boxes,ID=='C1-012014'))
		SB <- findPolys(GBs.stows.2014,subset(GBa.boxes,ID=='Seed box (2012 modified)'))
		AB <- findPolys(GBs.stows.2014,subset(GBa.boxes,ID=='A/B line closure'))
		# Set up a plot paramter for the boxes to make labeling and plotting occur right in ScallopMap.
		# There are no tows in 2014 in AB line box thus I clipped the plot to exclude this box.
		box.par<-data.frame(PID = unique(GBa.boxes$PID),label=unique(GBa.boxes$ID),lty=2)
		windows(11,8.5)
		#Source 9 (/ScallopMap.r)
		ScallopMap(ylim=c(41.8,42.17),xlim=c(-67.15,-66.6),poly.lst=list(GBa.boxes,box.par),
		xlab="",ylab="",title="", plot.EEZ = T, bathy.source="quick",plot.bathy = T,plot.boundries = T,
		boundries="offshore",direct=direct,cex.mn=2,dec.deg = F)
		           
		addLabels(box.par,polyProp=data.frame(PID = unique(GBa.boxes$PID),stringsAsFactors = F),
		          placement="CENTROID",cex=1,polys=GBa.boxes,adj=c(0.5,-5))
		addLines(subset(GBDis[[2]],PID %in% C1$EID),col='blue')
		addLines(subset(GBDis[[2]],PID %in% SB$EID),col='blue')
		addLines(subset(GBDis[[2]],PID %in% AB$EID),col='blue')
		addLabels(subset(GBDis[[2]],PID %in% AB$EID))
		# Add the comparative survey tows in Seedbox 2012
		points(slat~slon,survGBLive.dat,subset = year == 2014 & random == 3 &tow %in% SB$EID,
		       pch=23,bg='BLUE',cex=0.4)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBLive.dat,subset = year==2014 &  random %in% c(2,4,5)& tow %in% SB$EID,
		       pch=24,bg="darkorange",cex=0.4)
		# Add the comparative survey tows in C1
		points(slat~slon,survGBLive.dat,subset = year == 2014 &  random == 3 &tow %in% C1$EID,
		       pch=23,bg='BLUE',cex=0.4)
		# Add the exploratory survey tows in C1
		points(slat~slon,survGBLive.dat,subset = year==2014 &  random %in% c(2,4,5)& tow %in% C1$EID,
		       pch=24,bg="darkorange",cex=0.4)
		# Add the comparative survey tows in AB
		points(slat~slon,survGBLive.dat,subset = year == 2014 &  random == 3 &tow %in% AB$EID,
		       pch=23,bg='BLUE',cex=0.4)
		# Add the exploratory survey tows in AB
		points(slat~slon,survGBLive.dat,subset = year==2014 & random %in% c(2,4,5)& tow %in% AB$EID,
		       pch=24,bg="darkorange",cex=0.4)
		
		# Add the legend
		legend("topright",pch=c(24,23),pt.bg = c("darkorange","Blue"), title = "Tow type",
		       legend = c(paste('exploratory (n =',length(unique(subset(survGBLive.dat,year==yr & 
		                                           random %in% c(2,4,5) & (tow %in% AB$EID | tow %in% C1$EID | tow %in% SB$EID)
		                                           )$tow)),")",sep=""),
		                  paste('comparative (n =',length(unique(subset(survGBLive.dat,year==yr & 
		                                           random ==3 & (tow %in% AB$EID | tow %in% C1$EID | tow %in% SB$EID)
		                                           )$tow)),")",sep="")),box.col = "white",inset=0.01)
		C1.labs<-subset(survGBLive.dat,year==2014 & tow %in% C1$EID,c('tow','elon','elat','tow'))
		names(C1.labs)<-c("PID","X","Y","label")
		SB.labs<-subset(survGBLive.dat,year==2014 & tow %in% SB$EID,c('tow','elon','elat','tow'))
		names(SB.labs)<-c("PID","X","Y","label")
		AB.labs<-subset(survGBLive.dat,year==2014 & tow %in% AB$EID,c('tow','elon','elat','tow'))
		names(AB.labs)<-c("PID","X","Y","label")
		addLabels(C1.labs,cex=0.8)
		addLabels(SB.labs,cex=0.8)
		addLabels(AB.labs,cex=0.8)
		
		
		
		## Here we plot the maps for each of the seed banks, first we do Seedbox for the 3 size classes. Slide 69, 2014 Presentation
		#lvls=c(1,100,200,500,1000,2000,5000,10000)
		lvls=c(1,seq(100,700,100),1000)#,2000,5000,10000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID =='Seed box (2012 modified)'))
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA,stringsAsFactors = F) 
		CL <- contourLines(com.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='Seed box (2012 modified)'))
		CL <- contourLines(rec.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		recCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='Seed box (2012 modified)'))
		windows(11,8.5)
		par(mfrow=c(2,2))
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r") The Fully recruited scallops
		ScallopMap(ylim=c(41.93,42.015),xlim=c(-66.835,-66.74),bathy.source="quick",plot.bathy=T,plot.boundries = T,
		           contour=list(comCont.poly,cont.data),direct=direct,xlab = "",ylab = "",title="",dec.deg = F)
		title(paste("Seed box (",bnk[1],"-Spring ",yr,")",sep=""),outer=T,cex.main=3,line=-3)
		addLines(subset(GBDis[[2]],PID%in%SB$EID),col='blue')
		# Add the comparative survey tows.
		points(slat~slon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3 &tow %in% SB$EID,
		       pch=23,bg='BLUE',cex=1)
		# Add the exploratory survey tows
		points(slat~slon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5)& tow %in% SB$EID,
		       pch=24,bg="darkorange",cex=1)
		addPolys(subset(GBa.boxes),lty=2)
		# The recruit scallops.
		ScallopMap(ylim=c(41.93,42.015),xlim=c(-66.835,-66.74),bathy.source="quick",plot.bathy=T,plot.boundries = T,
		           ,contour=list(recCont.poly,cont.data),title="",direct=direct,xlab = "",ylab = "",dec.deg = F)
		addLines(subset(GBDis[[2]],PID%in%SB$EID),col='blue')
		# Add the comparative survey tows.
		points(slat~slon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3 &tow %in% SB$EID,
		       pch=23,bg='BLUE',cex=1)
		# Add the exploratory survey tows
		points(slat~slon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5)& tow %in% SB$EID,
		       pch=24,bg="darkorange",cex=1)
		addPolys(subset(GBa.boxes),lty=2)
		# The pre-recruit scallops.
		ScallopMap(ylim=c(41.93,42.015),xlim=c(-66.835,-66.74),bathy.source="quick",direct=direct,xlab = "",ylab = "",
		           contour=list(preCont.poly,cont.data),title="",plot.bathy=T,plot.boundries = T,dec.deg = F)
		addLines(subset(GBDis[[2]],PID %in% SB$EID),col='blue')
		# Add the comparative survey tows.
		points(slat~slon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3 &tow %in% SB$EID,
		       pch=23,bg='BLUE',cex=1)
		# Add the exploratory survey tows
		points(slat~slon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5)& tow %in% SB$EID,
		       pch=24,bg="darkorange",cex=1)
		addPolys(subset(GBa.boxes),lty=2)
		plot(1,1,type="n",bty="n",ylab="",xlab="",xaxt="n",yaxt="n")
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                  paste(lvls[length(lvls)],'+',sep='')),fill=c(precont.data$col),title=N.tow.lab,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white',cex=1.2)
		
		legend("topright",pch=c(24,23),pt.bg = c("darkorange","Blue"), title = "Tow type",
		       legend = c(paste('exploratory (n =',length(unique(subset(survGBLive.dat,year==yr & 
		                                      random %in% c(2,4,5) &  tow %in% SB$EID)$tow)),")",sep=""),
		       paste('comparative (n =',length(unique(subset(survGBLive.dat,year==yr & 
		                                      random ==3 & tow %in% SB$EID)$tow)),")",sep="")),box.col = "white",inset=0.01,cex=1.2)
		
		
		# C1 Seedbox Abundance spatial plot, slide 71 in 2014 presentation.
		lvls=c(1,100,200,400,600,800,1000,1500,2000)#,5000,10000)
		#lvls=c(1,seq(100,700,100),1000)#,2000,5000,10000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID =='C1-012014'))
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA,stringsAsFactors = F) 
		CL <- contourLines(com.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='C1-012014'))
		CL <- contourLines(rec.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		recCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='C1-012014'))
		# Start plot...
		windows(9,8)
		#windows(10,12)
		par(mfrow=c(2,2))
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r") # The Fully Recruited Scallops
		ScallopMap(ylim=c(42.055,42.13),xlim=c(-67.13,-66.96),bathy.source="quick",plot.bathy=T,plot.boundries = T,
		           contour=list(comCont.poly,cont.data),direct=direct,xlab = "",ylab = "",title="",plot.EEZ = T,dec.deg = F)
		title(paste("C1-012014 (",bnk[1],"-Spring ",yr,")",sep=""),outer=T,cex.main=3,line=-3)
		addLines(subset(GBDis[[2]],PID%in%C1$EID),col='blue')
		# Add the comparative survey tows.
		points(slat~slon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3 &tow %in% C1$EID,
		       pch=23,bg='BLUE',cex=1)
		# Add the exploratory survey tows
		points(slat~slon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5)& tow %in% C1$EID,
		       pch=24,bg="darkorange",cex=1)
		addPolys(subset(GBa.boxes),lty=2)
		# The Recruit Scallops
		ScallopMap(ylim=c(42.055,42.13),xlim=c(-67.13,-66.96),bathy.source="quick",plot.bathy=T,plot.boundries = T,
		           contour=list(recCont.poly,cont.data),direct=direct,xlab = "",ylab = "",title="",plot.EEZ = T,dec.deg = F)
		addLines(subset(GBDis[[2]],PID%in%C1$EID),col='blue')
		# Add the comparative survey tows.
		points(slat~slon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3 &tow %in% C1$EID,
		       pch=23,bg='BLUE',cex=1)
		# Add the exploratory survey tows
		points(slat~slon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5)& tow %in% C1$EID,
		       pch=24,bg="darkorange",cex=1)
		addPolys(subset(GBa.boxes),lty=2)
		# The pre-recruit scallops.
		ScallopMap(ylim=c(42.055,42.13),xlim=c(-67.13,-66.96),bathy.source="quick",plot.bathy=T,plot.boundries = T,
		           contour=list(preCont.poly,cont.data),direct=direct,xlab = "",ylab = "",title="",plot.EEZ = T,dec.deg = F)
		addLines(subset(GBDis[[2]],PID %in% C1$EID),col='blue')
		# Add the comparative survey tows.
		points(slat~slon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3 &tow %in% C1$EID,
		       pch=23,bg='BLUE',cex=1)
		# Add the exploratory survey tows
		points(slat~slon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5)& tow %in% C1$EID,
		       pch=24,bg="darkorange",cex=1)
		addPolys(subset(GBa.boxes),lty=2)
		plot(1,1,type="n",bty="n",ylab="",xlab="",xaxt="n",yaxt="n")
		par(xpd=T)
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                  paste(lvls[length(lvls)],'+',sep='')),fill=c(precont.data$col),title=N.tow.lab,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=-0.01,bg='white',box.col='white',cex=1.2)
		
		legend("topright",pch=c(24,23),pt.bg = c("darkorange","Blue"), title = "Tow type",
		       legend = c(paste('exploratory (n =',length(unique(subset(survGBLive.dat,year==yr & 
		                                                                  random %in% c(2,4,5) &  tow %in% C1$EID)$tow)),")",sep=""),
		                  paste('comparative (n =',length(unique(subset(survGBLive.dat,year==yr & 
		                                                                  random ==3 & tow %in% C1$EID)$tow)),")",sep="")),
		       box.col = "white",inset=0.01,cex=1.2)
		
		

##################  Figures potentially of interest but not included in presentation ##################
##################  Figures potentially of interest but not included in presentation ##################  
##################  Figures potentially of interest but not included in presentation ##################
		
		# Plot of the tow tracks.
		ScallopMap(bnk[1],nafo='all',bathy.source="usgs",bathcol=rgb(0,0,1,0.3),dec.deg = F)
		addLines(GBDis[[2]],col='blue')
		points(slat~slon,survGBMay.dat,subset=year==2014&state=='live',pch=16,cex=0.5)
		points(elat~elon,survGBMay.dat,subset=year==2014&state=='live',pch=16,cex=0.5)
		#with(subset(survGBMay.dat,year==2014&state=='live'),identify(slon,slat))
		#subset(survGBMay.dat,year==2014&state=='live')[c(42),]
		
		
		# Biomass time series for each size class
		survey.ts(survGB.obj[[1]],1990:2014,Bank=bnk[1],pdf=F,type='B', RS=RS-5, CS=CS-5,ys=.8,ht=6.5,wd=10,
		          clr='blue',se=F,pch=16,yl2=12)
		
		#  Shell height frequency plots using the GB2 data, so including random tows 3 and 5.
		shf.plt(ps.dat,survGB2.obj,from='surv',yr=2008:2014,rows=7,col1='grey80',
		        type='sh',col2=1,col3=1,xl=c(0,150),ymax=1000,rel=F,recline=c(85,95),wd=6,ht=8,ylab='')	
		
		
		# Fully Recruited Scallops on GB in Spring survey.  This is not included in presentation in 2014
		lvls=c(1,5,10,50,100,500,1000,5000)
		CL <- contourLines(com.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <- joinPolys(CP$PolySet,GBarea.poly)
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),"YlGn"),border=NA,stringsAsFactors = F) 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(12,8)
		ScallopMap(ylim=c(41.2,42.2),xlim=c(-67.3,-65.6),bathy.source="quick",bathcol=rgb(0,0,1,0.3),plot.bathy = T,plot.boundries = T,
		           contour=list(comCont.poly,cont.data),title="GBa 2014 fully recruited scallops (>= 95 mm shell height)",dec.deg = F)
		# Add the comparative survey tows.
		points(slat~slon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3 , pch=24,bg='BLUE',cex=0.8)
		# Add the exploratory survey tows
		points(slat~slon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5), pch=24,bg="darkorange",cex=0.8)
		addPolys(subset(GBa.boxes),lty=2)
		legend("bottomleft",c('exploratory','comparative',paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(NA,NA,precont.data$col),title='tons landed',
		       border=c(NA,NA,rep('black',length(lvls))),pch=c(24,23,rep(NA,length(lvls))),
		       pt.bg = c("darkorange","blue",rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white',cex=1.1)
		
		# Recruit Scallop spatial distribution, not presented in 2014 Presentation
		CL <- contourLines(rec.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		recCont.poly <- joinPolys(CP$PolySet,GBarea.poly)
		#Source9
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(12,8)
		ScallopMap(ylim=c(41.2,42.2),xlim=c(-67.3,-65.6),bathy.source="quick",bathcol=rgb(0,0,1,0.3),plot.bathy = T,plot.boundries = T,
		           contour=list(recCont.poly,cont.data),title="GBa 2014 recruit scallops (85 - 95 mm shell height)",dec.deg = F)
		# Add the comparative survey tows.
		points(slat~slon,survGBLive.dat,subset = year == 2014 & state=='live' & random == 3 , pch=24,bg='BLUE',cex=0.8)
		# Add the exploratory survey tows
		points(slat~slon,survGBLive.dat,subset = year==2014 & state =='live' & random %in% c(2,4,5), pch=24,bg="darkorange",cex=0.8)
		addPolys(subset(GBa.boxes),lty=2)
		legend("bottomleft",c('exploratory','comparative',paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(NA,NA,precont.data$col),title='tons landed',
		       border=c(NA,NA,rep('black',length(lvls))),pch=c(24,23,rep(NA,length(lvls))),
		       pt.bg = c("darkorange","blue",rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white',cex=1.1)
		
		#Source27 source("fn/fishery.dat.r") # The fishery data, we don't really want this for anything, but could be
		# helpful as an FYI or to look at some data on your own, this is survey summary so really don't want the 
		# commercial data here.  !!!!BE WARNED THIS CAN TAKE UP TO 5 MINUTES!!!
		GBa.cpue.ts<-fishery.dat(fish.dat,bk=bnk[2],yr=1981:yr,method='jackknife',direct=direct)
		#Source28 source("fn/fishsum.plt.r") Summary of the fishery data, Catch/CPUE on GB since 1981.
		# The error bars are so small, holy smokers!
		fishsum.plt(GBa.cpue.ts,years=1981:2014,pch=NA)
		
		# subset the fishery data to 2014 and GBa and rename the data
		GBafish.dat<-na.omit(subset(fish.dat,bank==bnk[2]&year==2014,c('ID','lon','lat','pro.repwt')))
		names(GBafish.dat)[1:4]<-c("EID","X","Y","Z")
		
		# Plot the spatial distribtuion of Catch on GBa.
		lvls=c(10,50,100,500,1000,5000,10000,50000)
		windows(12,8)
		#Source29 source("fn/gridPlot.r") Get the spatial fishery data set up and plot it using scallopMap.
		GBapolys<-gridPlot(GBafish.dat,GBarea.poly,lvls,border=NA,FUN=sum,grid.size=1/60)
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		ScallopMap(ylim=c(41.35,42.2),xlim=c(-67.3,-65.9),bathy.source='quick',bathcol=rgb(0,0,1,0.3),plot.boundries = T,
		           plot.bathy = T,poly.lst=GBapolys[1:2],title="",cex=1,dec.deg = F)
		addPolys(subset(GBa.boxes),lty=2)
		tlvls<-lvls/1000
		legend("bottomleft",c(paste(tlvls[-length(tlvls)],'-',tlvls[-1],sep=''),paste(tlvls[length(tlvls)],'+',sep='')),
		       fill=GBapolys[[3]],title='tons landed',inset=0.02,bg='white',box.col='white')
		
######################################  END SPRING SURVEY!!!!!!    ##########################################				
################################################################################################################		
################################ END SECTION 6 Georges Spring #####################################################
################################################################################################################				
######################################  END SPRING SURVEY!!!!!!    ##########################################				
######################################  END SPRING SURVEY!!!!!!    ##########################################				
######################################  END SPRING SURVEY!!!!!!    ##########################################				
######################################  END SPRING SURVEY!!!!!!    ##########################################				
######################################  END SPRING SURVEY!!!!!!    ##########################################				


		
		
		
		
		
###################################  SUMMER SURVEY!!!!!!  SUMMER SURVEY!!!!!!  SUMMER SURVEY!!!!!!   #################				
###################################  SUMMER SURVEY!!!!!!  SUMMER SURVEY!!!!!!  SUMMER SURVEY!!!!!!   #################				
###################################  SUMMER SURVEY!!!!!!  SUMMER SURVEY!!!!!!  SUMMER SURVEY!!!!!!   #################				
		
		
		######################## SPECIAL PLOTS ######################## SPECIAL PLOTS  #################################
		######################## SPECIAL PLOTS ######################## SPECIAL PLOTS  #################################
		## Intro slide for Summer survey summary can be made here
		windows(11,8.5)
		ScallopMap(area="WOB",plot.bathy=T,bathy.source = "quick",boundries = "offshore", plot.strata = T,strata="offshore",
		           bound.color = T,plot.boundries = T,label.boundries = F,offshore.names=T,xlab="",ylab="", un= un.ID,pw=pwd.ID,
		           title=paste("Offshore scallop summer survey results (",yr,")",sep=""),cex.mn=2,direct=direct,dec.deg = F)
		######################## SPECIAL PLOTS ######################## SPECIAL PLOTS  #################################
		######################## SPECIAL PLOTS ######################## SPECIAL PLOTS  #################################
		
		
		
		################################################################################################################
		################################ SECTION 7 Georges BANK SUMMER##################################################
		### Georges BANK SUMMER ### Georges BANK SUMMER### Georges BANK SUMMER### Georges BANK SUMMER### Georges BANK SUMMER
		################################ SECTION 7 Georges BANK SUMMER #################################################
		################################################################################################################
		
		##################  Bring in and ID the variables needed to create the plots ##################################
		# Shell height for knife edge recruitment based on portsampling data
		# The CS and RS specified here actually 5 higher than the actual shell heights
		# CS = Shell height for knife-edge recriutment:  
    # Correctly specifying the years here really matters since the RS and CS are changing with time, begs for a better method!!
		# 1981-1985 CS = 80, RS = 65
		# From 1986-1995 CS = 90, RS= 80
		# From 1996-current CS= 100, RS = 90
		CS = c(rep(80,5),rep(90,10),rep(100,19))
		# RS = Shell height 1 year previous to CS
		RS = c(rep(65,5),rep(80,10),rep(90,19))
		# RS = Shell height 1 year previous to CS (new LVB parameters)
		# Years
		years=1981:yr
		bnk = c("GB","GBa","GBb") # use bnk for all bank specifics helps to generalize the code for later
		
		
		## Set up plot titles
		survey.title <- substitute(bold(paste("Survey (",bank,"-",year,")",sep="")),
		                           list(year=as.character(yr),bank=bnk[1]))
		tow.track.title <- substitute(bold(paste("Tow tracks (",bank,"-",year,")",sep="")),
		                              list(year=as.character(yr),bank=bnk[1]))
		fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
		                              list(a=as.character(CS[length(CS)]-5),year=as.character(yr),bank=bnk[1]))
		rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
		                    list(a=as.character(CS[length(CS)]-6),b=as.character(RS[length(RS)]-5),year=as.character(yr),bank=bnk[1]))
		pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"",year,")",sep="")),
		                            list(b=as.character(RS[length(RS)]-5),year=as.character(yr),bank=bnk[1]))
		cf.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
		                       list(year=as.character(yr),bank=bnk[1]))
		mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
		                       list(m=as.character(CS[length(CS)]-5),year=as.character(yr),bank=bnk[1]))
		survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
		                                list(year=as.character(yr),bank=bnk[1]))
		survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
		                                 list(bank=bnk[1]))
		SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
		                         list(bank=bnk[1]))
		MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-",year,")",sep="")),
		                         list(year=as.character(yr),bank=bnk[1]))
		CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank,")",sep="")),
		                          list(year=as.character(yr),bank=bnk[1]))
		clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-",year,")",sep="")),
		                             list(c=as.character(85),bank=bnk[1],year=as.character(yr)))
		clap.ts.title <- substitute(bold(paste("Clapper time series (",bank,")",sep="")),
		                            list(bank=bnk[1]))
		clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,")",sep="")),
		                                list(bank=bnk[1]))
		## Set up plot titles
		
		# Subset the seed-boxes for GBa
		GBa.boxes <- as.PolySet(subset(seedboxes,Bank==bnk[2] & Open >= paste(yr,"-01-01",sep="")), projection = "LL")
		
		### Boundary polygons
		
		# GB survey poly for the entire bank
		GB.survey.bound.poly <-  subset(survey.bound.polys,label==bnk[1])
		
		#GBa polys
		#Read38 read removed... Strata polygons for GBa 
		GBa.survey.detail.poly <- subset(survey.detail.polys,label==bnk[2])
		attr(GBa.survey.detail.poly,"projection")<-"LL"
		# Detailed information for the survey strata on GBa
		GBa.surv.info <- subset(survey.info,label==bnk[2])
		#Read43 read removed... Area of each strata for GBa
		GBa.strata.areas <-subset(GBa.surv.info,select =c("PID","towable_area"))
		# Strata Boundaries for GBa
		GBa.survey.bound.poly <- subset(survey.bound.polys,label==bnk[2])
		
		# GBb polys
		#Read39 read removed... # detailed Strata polygons for GBb
		GBb.survey.detail.poly <- subset(survey.detail.polys,label==bnk[3])
		attr(GBb.survey.detail.poly,"projection")<-"LL"
		# Detailed information for the survey strata on GBb
		GBb.surv.info <- subset(survey.info,label==bnk[3])
		#Read44 read removed # Towable Area of each strata in GBb
		GBb.strata.areas <-subset(GBb.surv.info,select =c("PID","towable_area"))
		# Strata Boundaries for GBb
		GBb.strata.bound.poly <-  subset(survey.bound.polys,label==bnk[3])
		
		# Boundary polygons for GBa and GBb banks
		GBaBoundPoly <-  subset(newAreaPolys,label== "SFA27A" )
		GBbBoundPoly <-  subset(newAreaPolys,label== "SFA27B" )
		### End of polygon data
		
		
		#Source10 source("fn/getdis.r") Tow distances in GBa and GBb and with extra tows
		GBaDis<-dist.coef(1:200,path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBa/",sep=""),
		                  w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000)
		GBbDis<-dist.coef(1:30,path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBb/",sep=""),
		                  w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000)
		GBextrasDis<-dist.coef(c(903,904,908:917,920:924,926,928),
		                       path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/extras/",sep=""),
		                       w=c(1:10,9:1),rule=8,smooth=T, plt=F,meh=1000)
		
		#Read45 # The MW data from May on Georges Bank, if available, only really helpful if you are running GB in the
		# summer and don't want to re-run the spring mw data. Won't be available in 2015
		GBMay.mw<-read.csv(paste(direct,"data/Survey_data/",yr,"/Spring/GB/GBMaymw.csv",sep=""),stringsAsFactors = F) 
		
		##  Now bring in the data
		# survey data for GBa and GBb
		survGB.dat<-subset(summerSurv.dat,bank %in% bnk[2:3])
		# Reassign all survey tows > 900 to be experimental tows
		survGB.dat$random[survGB.dat$year==yr&survGB.dat$tow>900]<-2
		#Write13 Output the GB survey data for 2014
		write.csv(subset(survGB.dat,year==yr),paste(direct,"data/Survey_data/",yr,"/Summer/GB/Aug",yr,"SHF.csv",sep=""),row.names=F)
		# MEAT WEIGHT DATA GEORGES BANKS from 2011-2014, stitch them together. Drop the stratum column from 2011-2012.
		GB11.mw<-subset(Summersurv2011$MWs[,-grep("stratum",names(Summersurv2011$MWs))],bank %in% bnk[2:3])
		GB12.mw<-na.omit(subset(Summersurv2012$MWs[,-grep("stratum",names(Summersurv2012$MWs))],bank %in% bnk[2:3]))
		GB13.mw<-subset(SurvDB$MWs,bank %in% bnk[2:3] & cruise=="TE16")
		GB14.mw<-subset(SurvDB$MWs,bank %in% bnk[2:3] & cruise=="TE18")
		GBAug.mw<-rbind(GB11.mw,GB12.mw,GB13.mw,GB14.mw)
		GBAug.mw$month<-8
		# Convert Shell height to decimeters for 2014
		GB14.mwdm<-GB14.mw
		GB14.mwdm$sh<-GB14.mw$sh/100
		# DK Note: We have an NA in the data from 2014, flag that to Alan.
		GB14.mwdm <- GB14.mwdm[-which(is.na(GB14.mwdm$wmw)),]
		# For the Spring data on Georges Bank, code the month as 5 (i.e. May)
		GBMay.mw$month<-5
		# MEAT WEIGHT DATA - hydration sampling, it contains data from 1983-2007
		GB.wgt.dat<-subset(MW.dat,bank %in% bnk)
		GB.wgt.dat$ID<-paste(GB.wgt.dat$year,GB.wgt.dat$tow,sep='.')
		# Combine the Spring Meat Weight data with the summer survey data
		GB.mw<-merge(subset(GBAug.mw,select=c("ID","year","month","lon","lat","depth","sh","wmw","tow")),
		             subset(GBMay.mw,select=c("ID","year","month","lon","lat","depth","sh","wmw","tow")),all=T)
		# And the hydration sampling data with the more recent data.
		GBmw.dat<-merge(subset(GB.wgt.dat,select=c("ID","year","month","lon","lat","depth","sh","wmw","tow")),
		                subset(GB.mw,select=c("ID","year","month","lon","lat","depth","sh","wmw","tow")),all=T)
		
		#Write14 Output the meat weight data
		write.csv(GBmw.dat,paste(direct,"data/Survey_data/",yr,"/Summer/GB/GBmwData.csv",sep=""),row.names=F)
		
		# MODEL - This is the meat weight Shell height realationship.  
		#MEAT WEIGHT SHELL HEIGHT RELATIONSHIP 2014 
		#Source11 source("fn/shwt.lme.r") note thtat the exponent is set as a parameter here b=3
		GB.summer.SpatHtWt.fit<-shwt.lme(GB14.mwdm,random.effect='tow',b.par=3)
		
		## MODEL - This is the model used to esimate condition factor across all of Georges Bank
		#Source13 source("fn/condFac.r") # Note that the condition Factor is calculated for August
		GBcfData <- condFac(na.omit(subset(GBmw.dat,month==8)),survGB.dat,model.type='gam_f')
		# Output the predictions and give a new ID
		survGB.dat<-GBcfData$pred.dat
		survGB.dat$ID<-paste(survGB.dat$year,survGB.dat$tow,sep='.')
		# Pull out the ID and condition factor
		tmp.dat<-subset(GBcfData$CF.data,select=c("ID","CF"))
		# Rename CF to CFh
		names(tmp.dat)[2]<-"CFh"
		# merge the two data sets, keeping all x values
		survGB.dat<-merge(survGB.dat,tmp.dat,all.x=T)
		# Replace any NA's in CFh with the original Condition Factor.
		survGB.dat$CFh[is.na(survGB.dat$CFh)]<-survGB.dat$CF[is.na(survGB.dat$CFh)]
		
		
		#Source13 source("fn/condFac.r") Calculate the condition factor in May
    # DK NOTE:  This May data is very different from what we show on the plots!
		# need to figure this one out, see "fudge" in spring data!
		GBcfDataMay<-condFac(GBmw.dat[GBmw.dat$month %in% c(5,6),],model.type='gam_f')
		# ID the may data
		names(GBcfDataMay$CFyrs)[5]<-'CFMay'
		# Merge the data, don't need the lat/long/depth data from GBcfDataMay
		CFdata<-merge(GBcfData$CFyrs,GBcfDataMay$CFyrs[,-(2:4)],all=T)
		# Some minor tweaking of data 
		CFdata$y2<-CFdata$year-0.25
		CFdata	<- merge(CFdata,data.frame(year=2015),all=T)
		
		#Source30  This is where teh GBcfData.R object is created, each year it will be updated 
		#dump('GBcfData','GBcfData.R')
		
		
		###### GBa Specific Results  ###### GBa Specific Results   ###### GBa Specific Results   ###### GBa Specific Results
		###### GBa Specific Results  ###### GBa Specific Results   ###### GBa Specific Results   ###### GBa Specific Results
		###### GBa Specific Results  ###### GBa Specific Results   ###### GBa Specific Results   ###### GBa Specific Results
		###### GBa Specific Results  ###### GBa Specific Results   ###### GBa Specific Results   ###### GBa Specific Results
		
		# Grab the GBa data
		survGBa.dat<-subset(survGB.dat,bank==bnk[2])
		#Reassign the strata based on location and write to the screen what percentage of strata were reassigned.
		#Source20 source("fn/restratwp.r") 
		survGBa.dat<-restratwp(survGBa.dat,list(GBa.survey.detail.poly))
		paste("Strata reassigned",
		      with(subset(survGBa.dat,year==yr&state=='live'&random==1),
		           round((1-sum(stratum==new.stratum,na.rm=T)/length(stratum))*100)),'%')
		
		
		# Calculate the biomass of the Pre-recruits, Recruits and the Commerical Scallops in each tow on GBa
		#Source14 source("fn/surv.by.tow.r")
		survGBa.dat<-surv.by.tow(survGBa.dat, pre.ht=RS, rec.ht=CS)
		survGBa.dat<-surv.by.tow(survGBa.dat,pre.ht=RS, rec.ht=CS, type='B', mw.par="CFh")
		survGBa.dat$lon<-with(survGBa.dat,apply(cbind(elon,slon),1,mean))
		survGBa.dat$lat<-with(survGBa.dat,apply(cbind(elat,slat),1,mean))
		
		#Write15 Out put these results
		write.table(survGBa.dat,paste(direct,"data/Survey_data/",yr,"/Summer/GBa/GBaSurvey9114.csv",sep=""),sep=',',row.names=F)
		
		# Subset the survey data into the clappers(dead) and live scallops and 
		# the results from tows classifed a "1" (regular survey tows)
		survGBaClap.dat<-subset(survGBa.dat,state=='dead')
		survGBaLive.dat<-subset(survGBa.dat,state=='live')
		survGBaRandom.dat<-subset(survGBa.dat,state=='live'&random==1)
		
		# The GBa survey data (from PEDstrata)
		#Source21 source("fn/survey.dat.r")
		# DK Note:  Notice that is survey.dat we need to specify RS and CS as 5 higher than they really are.  FIX!
		survey.obj<-survey.dat(survGBaRandom.dat, years=years, RS=RS, CS=CS, bk=bnk[2], areas=GBa.strata.areas, mw.par="CFh")	
		clap.obj<-survey.dat(survGBaClap.dat, years=years, RS=RS, CS=CS, bk=bnk[2], areas=GBa.strata.areas, mw.par="CFh")		
		# Add the condition factor and number of commercial and recruit size clappers.
		
		# CF calculated annually weighted by the biomass for each tow
		survey.obj[[1]]$CF<-sapply(1:length(years),
		                           function(x){with(subset(survGBaRandom.dat,year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})
		# Add the time series of clappers, also going to add pre-recruits here.
		survey.obj[[1]]$clappers<-clap.obj[[1]]$N
		survey.obj[[1]]$clappersR<-clap.obj[[1]]$NR
		survey.obj[[1]]$clappersPR<-clap.obj[[1]]$NPR
		#Source31:  This is where the surveyObj.R object is created, this contains all the survey information for GBa in one
		# really really massive r object.
		dump(c('survey.obj','clap.obj'),'surveyObj.R')
		# If we want to skip all the processing we could just call in the survey object like so...
		#Source31 source('surveyObj.R') 
		
		# Proportion of Clappers on GBa for each class and overall
		survGBaClap.dat$clap.prop<-survGBaClap.dat$tot/(survGBaLive.dat$tot+survGBaClap.dat$tot)*100
		survGBaClap.dat$clap.prop[is.na(survGBaClap.dat$clap.prop)]<-0
		survGBaClap.dat$clap.propCom<-survGBaClap.dat$com/(survGBaLive.dat$com+survGBaClap.dat$com)*100
		survGBaClap.dat$clap.propCom[is.na(survGBaClap.dat$clap.propCom)]<-0
		survGBaClap.dat$clap.propRec<-survGBaClap.dat$rec/(survGBaLive.dat$rec+survGBaClap.dat$rec)*100
		survGBaClap.dat$clap.propRec[is.na(survGBaClap.dat$clap.propRec)]<-0
		survGBaClap.dat$clap.propPre<-survGBaClap.dat$pre/(survGBaLive.dat$pre+survGBaClap.dat$pre)*100
		survGBaClap.dat$clap.propPre[is.na(survGBaClap.dat$clap.propPre)]<-0
		
		############## END GBa Specific Results ############## END GBa Specific Results ################ END GBa Specific Results
		############## END GBa Specific Results ############## END GBa Specific Results ################ END GBa Specific Results
		
		
		
		
		############ GBb Specific Results############ GBb Specific Results############ GBb Specific Results############ 
		############ GBb Specific Results############ GBb Specific Results############ GBb Specific Results############ 
		############ GBb Specific Results############ GBb Specific Results############ GBb Specific Results############ 
		############ GBb Specific Results############ GBb Specific Results############ GBb Specific Results############ 
		# Grab the GBb data
		survGBb.dat<-subset(survGB.dat,bank==bnk[3])
		
		#Reassign the strata based on location and write to the screen what percentage of strata were reassigned.
		#Source20 source("fn/restratwp.r") 
		survGBb.dat<-restratwp(survGBb.dat,list(GBb.survey.detail.poly))
		
		# surv.by.tow calculates number or biomass of pre, rec and com size scallops in each tow
		
		# Calculate the biomass of the Pre-recruits, Recruits and the Commerical Scallops in each tow on GBb
		#Source14 source("fn/surv.by.tow.r")
		survGBb.dat<-surv.by.tow(survGBb.dat, pre.ht=RS, rec.ht=CS)
		survGBb.dat<-surv.by.tow(survGBb.dat,pre.ht=RS, rec.ht=CS, type='B', mw.par="CFh")
		survGBb.dat$lon<-with(survGBb.dat,apply(cbind(elon,slon),1,mean))
		survGBb.dat$lat<-with(survGBb.dat,apply(cbind(elat,slat),1,mean))
		#Write16 Output the results
		write.table(survGBb.dat,paste(direct,"data/Survey_data/",yr,"/Summer/GBb/GBbSurvey9114.csv",sep=""),sep=',',row.names=F)
		
		# Subset the survey data into the clappers(dead) and live scallops and 
		# the results from tows classifed a "1" (regular survey tows)
		survGBbClap.dat<-subset(survGBb.dat,state=='dead')
		survGBbLive.dat<-subset(survGBb.dat,state=='live')
		survGBbRandom.dat<-subset(survGBb.dat,state=='live'& random==1)
		# Add the new strata to survGBblive.
		survGBbLive.dat$new.stratum<-survGBbLive.dat$new.stratum.y
		
		# Clappers on GBb, this is simply total clappers not broken into classes.
		survGBbClap.dat$clap.prop<-survGBbClap.dat$tot/(survGBbLive.dat$tot+survGBbClap.dat$tot)*100
		survGBbClap.dat$clap.prop[is.na(survGBbClap.dat$clap.prop)]<-0
		attr(GBb.survey.detail.poly,"projection")<-"LL"

		# Survey data for GBb using PEDstrata
		#Source21 source("fn/survey.dat.r")
		surveyGBb.obj<-survey.dat(survGBbRandom.dat, years=years, RS=RS, CS=CS, bk=bnk[3], areas=GBb.strata.areas, mw.par="CFh")
		# Calculated CF for each year using a weighted mean for each tow
		surveyGBb.obj[[1]]$CF<-sapply(1:length(years),function(x){with(subset(survGBbRandom.dat,year==years[x]),
		                                                               weighted.mean(CF,com.bm,na.rm=T))})
		# Get clappers and add them to the survey object for GBb
		clapGBb.obj<-survey.dat(survGBbClap.dat, years=years, RS=RS, CS=CS, bk=bnk[3], areas=GBb.strata.areas, mw.par="CFh")	
		surveyGBb.obj[[1]]$clappers<-clapGBb.obj[[1]]$N
		surveyGBb.obj[[1]]$clappersR<-clapGBb.obj[[1]]$NR
		surveyGBb.obj[[1]]$clappersPR<-clapGBb.obj[[1]]$NPR
		
		
		############## END GBb Specific Results ############## END GBb Specific Results ################ END GBb Specific Results
		############## END GBb Specific Results ############## END GBb Specific Results ################ END GBb Specific Results
		
		################################################################################################################################
		# SEEDBOXES # SEEDBOXES # SEEDBOXES # SEEDBOXES # SEEDBOXES # SEEDBOXES# SEEDBOXES # SEEDBOXES # SEEDBOXES # SEEDBOXES
		###############################################################################################################################
		
		# to work with new seedbox data and added other information. Grab the GB tows from 2014 from within the seedboxes.
		GB.summer.stows.2014 <- subset(survGBaLive.dat,year==yr,select=c('tow','slon','slat'))
		names(GB.summer.stows.2014)<-c("EID","X","Y")
		GB.summer.etows.2014 <- subset(survGBaLive.dat,year==yr,select=c('tow','elon','elat'))
		names(GB.summer.etows.2014)<-c("EID","X","Y")
		# This will change each year and will effect what is plotted below.
		C1 <- findPolys(GB.summer.stows.2014,subset(GBa.boxes,ID=='C1-012014'))
		SB <- findPolys(GB.summer.stows.2014,subset(GBa.boxes,ID=='Seed box (2012 modified)'))
		AB <- findPolys(GB.summer.stows.2014,subset(GBa.boxes,ID=='A/B line closure'))
		
		
		# SHF	# Survey Data for seedboxes.
		#Read48 - Only needed if the Spring survey data has not been run above.
		survGBMay.dat <- read.csv(paste(direct,"data/Survey_data/",yr,"/Spring/GB/GBMaySurvey8414.csv",sep=""),stringsAsFactors = F)
		survGBMay.dat <- subset(survGBMay.dat,state=='live')
		survGBAug.dat <- subset(survGBaLive.dat,year>2006)
		# Offset the August data by a a quarter of a year.
		survGBAug.dat$year <- survGBAug.dat$year+0.7
		survGBMay.dat$year <- survGBMay.dat$year+0.2
		# Merge the Spring and Summer survey data
		GBa.surv.0714<-merge(survGBAug.dat,subset(survGBMay.dat,year>2007),all=T)
		# Make some new columns
		GBa.surv.0714$EID<-1:nrow(GBa.surv.0714)
		GBa.surv.0714$X<-GBa.surv.0714$slon
		GBa.surv.0714$Y<-GBa.surv.0714$slat
		# This will change each year and will effect what is plotted below.
		C1.surv <- findPolys(GBa.surv.0714,subset(GBa.boxes,ID=='C1-012014'))
		SB.surv <- findPolys(GBa.surv.0714,subset(GBa.boxes,ID=='Seed box (2012 modified)'))
		AB.surv <- findPolys(GBa.surv.0714,subset(GBa.boxes,ID=='A/B line closure'))
		# Survey results from each seedbox for 2011 to 2014
		#Source15 source("fn/simple.surv.r")
		C1line.obj<-simple.surv(subset(GBa.surv.0714,EID%in%C1.surv$EID),years=seq(2012.7,2014.7,0.5))
		SBline.obj<-simple.surv(subset(GBa.surv.0714,EID%in%SB.surv$EID),years=seq(2011.7,2014.7,1))
		ABline.obj<-simple.surv(subset(GBa.surv.0714,EID%in%AB.surv$EID),years=seq(2011.7,2014.7,1))
		
		################################################################################################################################
		# END SEEDBOXES # END SEEDBOXES # END SEEDBOXES # END SEEDBOXES # END SEEDBOXES # END SEEDBOXES # END SEEDBOXES # END SEEDBOXES
		###############################################################################################################################
		
		
		
		
		
		###################################  COMBINED SPATIAL COUNTOUR DATA ################################################
		###################################  COMBINED SPATIAL COUNTOUR DATA ################################################
		###################################  COMBINED SPATIAL COUNTOUR DATA ################################################
		# Pre-recruits (< 85 mm shell height) for GBa and GBb combined
		#Source19 source("fn/contour.gen.r")
		con.dat<-rbind(subset(survGBaLive.dat,year==yr,c('tow','lon','lat','pre')),
		               subset(survGBbLive.dat,year==yr,c('tow','lon','lat','pre')))
		pre.contours<-contour.gen(con.dat,ticks='define',nstrata=14,str.min=0,interp.method='gstat',
		                          points=T,blank=T,res=0.01,key='log.cont',title='pre-recruits (< 85 mm shell height)',
		                          color.fun=tim.colors,id.par=3.5,units='#/tow',plot=F,subscale=0.1,blank.dist=0.08,direct = direct)
		# Recruits (85 - 95 mm shell height)
		con.dat<-rbind(subset(survGBaLive.dat,year==yr,c('tow','lon','lat','rec')),
		               subset(survGBbLive.dat,year==yr,c('tow','lon','lat','rec')))
		rec.contours<-contour.gen(con.dat,ticks='define',nstrata=14,str.min=0,interp.method='gstat',points=T,blank=T,
		                          res=0.01,key='log.cont',title='recruits(85 - 95 mm shell height)',color.fun=tim.colors,id.par=3.5,
		                          units='#/tow',plot=F,subscale=0.1,blank.dist=0.08,direct = direct)
		# Fully Recruited (>= 95 mm shell height)
		con.dat<-rbind(subset(survGBaLive.dat,year==yr,c('tow','lon','lat','com')),
		               subset(survGBbLive.dat,year==yr,c('tow','lon','lat','com')))
		com.contours<-contour.gen(con.dat,ticks='define',nstrata=14,str.min=0,interp.method='gstat',points=T,blank=T,
		                          res=0.01,key='log.cont',title='fully recruited (>= 95 mm shell height)',
		                          color.fun=tim.colors,id.par=5,units='#/tow',plot=F,subscale=0.1,blank.dist=0.08,direct = direct)
		# Meat Count 2014
		survGBaLive.dat$meat.count<-0.5/(survGBaLive.dat$com.bm/survGBaLive.dat$com)
		survGBbLive.dat$meat.count<-0.5/(survGBbLive.dat$com.bm/survGBbLive.dat$com)
		con.dat<-rbind(subset(survGBaLive.dat,year==yr,c('tow','lon','lat','meat.count')),
		               subset(survGBbLive.dat,year==yr,c('tow','lon','lat','meat.count')))
		
		# Meat Count data 2014 
		mc.contours<-contour.gen(na.omit(con.dat),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,
		                         units="scallops/500g",interp.method='gstat',key='strata',blank=F,plot=F,
		                         subset.poly='square',subset.eff=0,subscale=0.2,direct = direct,blank.dist=0.16)
		# Condition Factor 2014 note I do not specify blank distance, allow the blank.blank function to determine optimal given data.
		cf.contours<-contour.gen(subset(GBcfData$CF.data,year==yr,c('ID','lon','lat','CF')),interp.method='gstat',
		                         ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="scallops/500g",direct = direct,
		                         key='strata',blank=T,plot=F,subset.eff=0,subscale=0.25,subset.poly='square')
		# Bottom temp  
		temp.contours<-contour.gen(na.omit(subset(summerSurv.dat,year==yr & state=='live',c('tow','lon','lat','mean.temp'))),
		                           ticks='define',nstrata=7,str.min=-10,place=2,id.par=3.5,interp.method='gstat',res=0.01,
		                           blank=F,plot=F,subset.poly='square',subset.eff=-20,subscale=0.25,direct=direct)
		# Clappers on Georges in 2014.  This is total percentage not broken by size class.
		con.dat<-rbind(subset(survGBaClap.dat,year==yr,c('tow','lon','lat','clap.prop')),
		               subset(survGBbClap.dat,year==yr,c('tow','lon','lat','clap.prop')))
		clap.contours<-contour.gen(con.dat,nstrata=9,str.min=0,smooth=F,interp.method='gstat',points=T,blank=T,
		                           ticks='define',res=0.01,key='log.cont',title='clappers',color=clap.col,id.par=3.5,
		                           units='% dead',plot=F,blank.dist=0.08,place=2,direct=direct)
		
		
		
		
		###################  GB PLOTS!!###################  GB PLOTS!!###################  GB PLOTS!!###################  GB PLOTS!!
		###################  GB PLOTS!!###################  GB PLOTS!!###################  GB PLOTS!!###################  GB PLOTS!!
		###################  GB PLOTS!!###################  GB PLOTS!!###################  GB PLOTS!!###################  GB PLOTS!!
		###################  GB PLOTS!!###################  GB PLOTS!!###################  GB PLOTS!!###################  GB PLOTS!!
	
		
		######################  THESE FIRST FEW PLOTS ARE GEORGES BANK OVERALL, NOT SUBSET INTO "a" OR 'b" ######################
		######################  THESE FIRST FEW PLOTS ARE GEORGES BANK OVERALL, NOT SUBSET INTO "a" OR 'b" ######################
		
		# DK Note:  This should be clarified in the presentation as it stands now these look  like "a" specific plots ###########
		
		############
		#Source12 Meat Height Shell weight plot on Slide 13  source("fn/shwt.plt1.r") 
		windows(15,8)
		par(mfrow=c(1,2))
		shwt.plt1(GB.summer.SpatHtWt.fit,lw=3,ht=10,wd=12,cx=1.5,titl = MWSH.title,cex.mn = 2,las=1)
		############
		#Source16 source("fn/stdts.plt.R") Condition Factor time series slide 14 in 2014 presentation 
		# DK Note:  See above but May CF data is not what we show on the plot, see also GB spring fudge...
		stdts.plt(CFdata[-(1:3),],x=c('year','y2'),y=c('CF','CFMay'),pch=16,ylab=cf.lab,las=1,col=c("blue","red"),
		          mean.line=T,graphic='none',xlab='Year',ylim=c(12,22),titl=CF.ts.title,cex.mn=2)
		legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
		############
		
		############
		# Plot for Bottom Temperature across GB, This is slide 21 from presentation and covers all of the bank.
		lvls=seq(4,14,1)
		CL <- contourLines(temp.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		Tcont.poly <- joinPolys(CP$PolySet,GB.survey.bound.poly)
		Tcont.data<- data.frame(PID=1:length(lvls),col=rev(brewer.pal(length(lvls),"RdYlBu")),border=NA,stringsAsFactors = F) 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[1],plot.boundries=T,bathy.source='quick', plot.bathy=T, direct=direct,xlab="",ylab="",cex.mn = 2,
		           contour=list(Tcont.poly,Tcont.data),title=paste("Temperature Data (",bnk[1],"-",yr,")",sep=""),dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survGBaLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		points(lat~lon,survGBbLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBaLive.dat,subset=year==yr &state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		points(lat~lon,survGBbLive.dat,subset=year==yr & state=='live' & random==2,pch=24,bg='darkorange',cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(Tcont.data$col),
		       title=expression(Temperature~(degree*C)),
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bty="n")
		legend("topright",
		       legend = c(paste('exploratory (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==2)$tow))+
		                        length(unique(subset(survGBbLive.dat,year==yr & random==2)$tow)),
		                        ")",sep=""),
		                  paste('regular (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==1)$tow))+
		                        length(unique(subset(survGBbLive.dat,year==yr & random==1)$tow)),
		                        ")",sep="")),
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
				addPolys(GBa.boxes,lty=2,lwd=2)
		############

		###################  PLOTS using GBa specific data.
		## Set up plot titles
				## Set up plot titles
				survey.title <- substitute(bold(paste("Survey (",bank,"-",year,")",sep="")),
				                           list(year=as.character(yr),bank=bnk[2]))
				tow.track.title <- substitute(bold(paste("Tow tracks (",bank,"-",year,")",sep="")),
				                              list(year=as.character(yr),bank=bnk[2]))
				fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
				                              list(a=as.character(CS[length(CS)]-5),year=as.character(yr),bank=bnk[2]))
				rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
				                   list(a=as.character(CS[length(CS)]-6),b=as.character(RS[length(RS)]-5),year=as.character(yr),bank=bnk[2]))
				pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
				                            list(b=as.character(RS[length(RS)]-5),year=as.character(yr),bank=bnk[2]))
				cf.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
				                       list(year=as.character(yr),bank=bnk[2]))
				mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
				                       list(m=as.character(CS[length(CS)]-5),year=as.character(yr),bank=bnk[2]))
				survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
				                                list(year=as.character(yr),bank=bnk[2]))
				survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
				                                 list(bank=bnk[2]))
				SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
				                         list(bank=bnk[2]))
				MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-",year,")",sep="")),
				                         list(year=as.character(yr),bank=bnk[2]))
				CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank,")",sep="")),
				                          list(year=as.character(yr),bank=bnk[2]))
				clap.dis.title <- substitute(bold(paste("Clappers (% dead ", bank,"-",year,")",sep="")),
				                             list(bank=bnk[2],year=as.character(yr)))
				clap.ts.title <- substitute(bold(paste("Clapper time series (",bank,")",sep="")),
				                            list(bank=bnk[2]))
				clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,")",sep="")),
				                                list(bank=bnk[2]))
				## Set up plot titles
		
		############
		#Survey with tow locations Creates figure for slide 3 of powerpoint.
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(bnk[1],poly.lst=list(GBa.survey.detail.poly,GBa.surv.info),direct = direct,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,
		           nafo.bord = T,nafo="all",nafo.lab = F,title=survey.title,dec.deg=F)
		# Add the regular survey tows.
		points(lat~lon,survGBaLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBaLive.dat,subset=year==yr & state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		
		legend("bottomleft",legend=c(GBa.surv.info$PName),
		       fill=c(GBa.surv.info$col),border=c(rep('black',length(GBa.surv.info$PName))),
		       pch=c(rep(NA,length(GBa.surv.info$PName))),title = "Strata",title.adj=0.01,
		       pt.bg = c(rep(NA,length(GBa.surv.info$PName))),col='black',bty='n',inset=0.01)
		# Add area, convert to km^2 from number of towable units. (NTU/(1000*1000/800/2.4384)
		legend(-67.27,41.82,legend = round(GBa.surv.info$area_km2),
		       fill=c(GBa.surv.info$col),border=c(rep('black',length(GBa.surv.info$PName))),
		       pch=c(rep(NA,length(GBa.surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.9,
		       pt.bg = c(rep(NA,length(GBa.surv.info$PName))),col='black',bty='n')
		legend(-66,41.45,legend = as.numeric(with(subset(survGBaLive.dat,year==yr),tapply(tow,new.stratum,length))),
		       fill=c(GBa.surv.info$col),border=c(rep('black',length(GBa.surv.info$PName))),
		       pch=c(rep(NA,length(GBa.surv.info$PName))),title = "Number of tows",title.adj=0.1,
		       pt.bg = c(rep(NA,length(GBa.surv.info$PName))),col='black',bty="n",bg="white")
		legend("topright",
		       legend = c(paste('exploratory (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==2)$tow)),
		                        ")",sep=""),
		                  paste('regular (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==1)$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
				addPolys(GBa.boxes,lty=2,lwd=2)
		############
		
		
		############
		# plot survey time series #Source17 source("fn/survey.ts.r",local=T)
		# This produces figure on Slide 4
		survey.ts(survey.obj[[1]],1981:yr,Bank=bnk[2],pdf=F, areas=GBa.strata.areas$towable_area,ys=.8,CS=CS[length(years)]-5,
		          RS=RS[length(years)]-5, ht=6.5,wd=10,clr='blue',se=T,pch=16,yl2=c(2000,350,350),
		          add.title = T,titl=survey.ts.N.title,cx.mn = 3,axis.cx=1.5)
		# This produces figure on slide 5
		survey.ts(survey.obj[[1]],1981:yr,Bank=bnk[2],pdf=F,type='B', areas=GBa.strata.areas$towable_area,ys=.8,CS=CS[length(years)]-5,
		          RS=RS[length(years)]-5, ht=6.5,wd=10,clr='blue',se=T,pch=16,yl2=5.2,
		          add.title = T,titl=survey.ts.BM.title,cx.mn = 3,axis.cx=1.5)
		############
		
		
		############
		# Shell height frequency plots #Source18 source("fn/shf.plt.r")
		# This produces histogram on slide 6
		shf.plt(ps.dat,survey.obj,from='surv',yr=2008:yr,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),adj=0.9,
		        rel=F,recline=c(RS[length(years)]-5,CS[length(years)]-5),ymax=310,mean.line=F,titl=SHF.title,cex.mn = 3,
		        add.title = T)	
		############
		
		
		############
		# Plot for Fully Recruited across GB, this includes both 'a' and 'b' but plot will be bank specific.  GBa slide 8 from 2014
		lvls1=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(com.contours$image.dat,levels=lvls1)
		CP <- convCP(CL)
		comCont1.poly <- joinPolys(CP$PolySet,GBa.survey.bound.poly)
		comcont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F) 
		lvls2=c(2000,5000,10000,20000,50000)
		# If counts are high enough to need lvls2 we use it, if not we just use lvls1.
		if(max(com.contours$image.dat$z)>=lvls2[1])
		{
		  CL <- contourLines(com.contours$image.dat,levels=lvls2)
		  CP <- convCP(CL)
		  comCont2.poly <- joinPolys(CP$PolySet,GBaBoundPoly)
		  comCont2.poly$PID<-comCont2.poly$PID+length(lvls1)
		  comCont.poly<-rbind(comCont1.poly,comCont2.poly)
		  lvls<-c(lvls1,lvls2)
		  comcont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
		                             col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA,stringsAsFactors = F) 
		  comcont.data<-rbind(comcont1.data,comcont2.data)
		} # end if(max(com.contours$image.dat$z)>=lvls2[1])
		if(max(com.contours$image.dat$z)<lvls2[1])
		{
		  comCont.poly<-comCont1.poly
		  comcont.data<-comcont1.data
		  lvls <- lvls1
		} # end if(max(com.contours$image.dat$z)<lvls2[1])
		# GBa This is the fully recruited scallops, slide 8 of the powerpoint.
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[1],contour=list(comCont.poly,comcont.data),direct = direct,title=fully.rec.title,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",cex.mn=2,
		           nafo.bord = T,nafo="all",nafo.lab = F,dec.deg=F)
		# Add the regular survey tows.
		points(lat~lon,survGBaLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBaLive.dat,subset=year==yr&state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(comcont.data$col),title=N.tow.lab,
		       border=c(rep(length(lvls))),
		       inset=0.04,bty="n",title.adj = 0.2)
		legend("topright",
		       legend = c(paste('exploratory (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==2)$tow)),
		                        ")",sep=""),
		                  paste('regular (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==1)$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
		# Add seedboxes
				addPolys(GBa.boxes,lty=2,lwd=2)
		############
		
		
		############
		# Plot for Recruits across GB, this includes both 'a' and 'b' but plot will be bank specific.  GBa slide 10 of the powerpoint.
		lvls1=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(rec.contours$image.dat,levels=lvls1)
		CP <- convCP(CL)
		recCont1.poly <- joinPolys(CP$PolySet,GBa.survey.bound.poly)
		reccont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F)
		lvls2=c(2000,5000,10000,20000,50000)
		# If counts are high enough to need lvls2 we use it, if not we just use lvls1.
		if(max(rec.contours$image.dat$z)>=lvls2[1])
		{
		  CL <- contourLines(rec.contours$image.dat,levels=lvls2)
		  CP <- convCP(CL)
		  recCont2.poly <- joinPolys(CP$PolySet,GBa.survey.bound.poly)
		  recCont2.poly$PID<-recCont2.poly$PID+length(lvls1)
		  recCont.poly<-rbind(recCont1.poly,recCont2.poly)
		  lvls<-c(lvls1,lvls2)
		  reccont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
		                             col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA,stringsAsFactors = F)
		  reccont.data<-rbind(reccont1.data,reccont2.data)
		} # end if(max(rec.contours$image.dat$z)>=lvls2[1])
		if(max(rec.contours$image.dat$z)<lvls2[1])
		{
		  recCont.poly<-recCont1.poly
		  reccont.data<-reccont1.data
		  lvls <- lvls1
		} # end if(max(rec.contours$image.dat$z)<lvls2[1])
		# GBa This is the recruit scallops, slide 10 of the powerpoint.
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[1], contour=list(recCont.poly,reccont.data),direct = direct, title=rec.title,cex.mn=2,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survGBaLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBaLive.dat,subset=year==yr&state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(reccont.data$col),title=N.tow.lab,
		       border=c(rep(length(lvls))),
		       inset=0.04,bty="n",title.adj = 0.2)
		legend("topright",
		       legend = c(paste('exploratory (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==2)$tow)),
		                        ")",sep=""),
		                  paste('regular (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==1)$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
		# Add seedboxes
				addPolys(GBa.boxes,lty=2,lwd=2)
		############
		
		
		############
		# Pre-recruits across GB, this includes both 'a' and 'b' but plot this plot is GBa Slide 12 in 2014 presentation
		lvls1=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls1)
		CP <- convCP(CL)
		preCont1.poly <- joinPolys(CP$PolySet,GBa.survey.bound.poly)
		precont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F) 
		lvls2=c(2000,5000,10000,20000,50000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls2)
		CP <- convCP(CL)
		preCont2.poly <- joinPolys(CP$PolySet,GBa.survey.bound.poly)
		preCont2.poly$PID<-preCont2.poly$PID+length(lvls1)
		preCont.poly<-rbind(preCont1.poly,preCont2.poly)
		lvls<-c(lvls1,lvls2)
		precont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
		                           col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA)
		precont.data<-rbind(precont1.data,precont2.data)
		# This produces figure on slide 12 for 2014 pre recruts
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[1],contour=list(preCont.poly,precont.data),direct = direct, title=pre.rec.title,cex.mn=2,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survGBaLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBaLive.dat,subset=year==yr&state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(precont.data$col),title=N.tow.lab,
		       border=c(rep(length(lvls))),
		       inset=0.04,bty="n",title.adj = 0.2)
		legend("topright",
		       legend = c(paste('exploratory (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==2)$tow)),
		                        ")",sep=""),
		                  paste('regular (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==1)$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
		# Add seedboxes
				addPolys(GBa.boxes,lty=2,lwd=2)
		############
		
		
		
		#############
		# Plot for Condition Factor across GB, this includes both 'a' and 'b' but plot will be bank specific slide 16 
		# DK Note:  This is not very similar to the 2014 plot, points are diferent/missing in places...
		# Mostly due to missing points in the seedboxes.
		lvls=12:20
		CL <- contourLines(cf.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		cont.poly <- joinPolys(CP$PolySet,GBa.survey.bound.poly)
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),cf.col),border=NA,stringsAsFactors = F) 
		#Condition factor for Georges A, slide 16 in powerpoint
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[1],contour=list(cont.poly,cont.data),direct = direct, title=cf.title,cex.mn=2,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,dec.deg = F)
		# Add the survey tows in GBa, somewhat messy way to get there...
		# Grab the exploratory/regular tows from the condition factor data.
		tmp <- subset(GBcfData$CF.data,year==2014)
		reg.tw <- subset(survGBaLive.dat,year==2014 & random==1)$tow
		exp.tw <- subset(survGBaLive.dat,year==2014 & random==2)$tow
		cf.reg <- subset(tmp, tow %in% reg.tw)
		cf.exp <- subset(tmp, tow %in% exp.tw)
		# Add the regular survey tows.
		points(lat~lon,cf.reg,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,cf.exp,pch=24,bg="darkorange",cex=0.8)
		
		# Add the legend
		legend("topright",legend = c(paste('exploratory (n =',length(cf.exp$tow),")",sep=""),
		           paste('regular (n =',length(cf.reg$tow),")",sep="")),title="Tow type",
		         pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(cont.data$col),title=cf.lab,
		       border=c(rep(length(lvls))),
		       inset=0.04,bty="n",title.adj = 0.2)
		# Add seedboxes
				addPolys(GBa.boxes,lty=2,lwd=2)
		############
	
		
		
		############
		#Source16 This produces the figure on slide 17 in 2014 presentation
		#Source16 source("fn/stdts.plt.R")
		windows(10,8)
		par(mfrow=c(3,1),omi=c(0.3,0.4,0.3,0.2))
		stdts.plt(subset(survey.obj[[1]],year>1995),y="l.bar",pch=17,lty=1,ylab="Average\n shell\n height\n (mm)",las=1,
		          mean.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
		stdts.plt(subset(survey.obj[[1]],year>1995),y="CF",pch=17,lty=1,ylab=cf.lab,las=1,
		          mean.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
		stdts.plt(subset(survey.obj[[1]],year>1995),y="w.bar",pch=17,lty=1,ylab="Average\n meat\n weight\n(g)",
		          mean.line=T,graphic='none',xlab='',labcx=1.2,las=1,axis.cx=1.2)
		title(paste("Shell height, Condition factor, Meat weight (",bnk[2],"-",yr,")",sep=""), cex.main=3,outer=T)
		############
		
		
		############
		# Meat Count GBa slide 19 2014 presentation
		# Plot for Meat Count across GB, this includes both 'a' and 'b' but plot will be bank specific. 
		lvls=c(seq(10,30,5),33)
		div=2
		CL <- contourLines(mc.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		# Note here I used the "comCont.poly" from the figure for slide 12 to define the spatial boundaries...
		# Done so as plot with blank data added has some bad edge effects. Run slide 12 code to create the comCont.poly object.
		MCcont.poly <- joinPolys(CP$PolySet,GBa.survey.bound.poly)
		Ncol=length(lvls)+div
		MCcont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,mc.col)[c(Ncol:(div+2),1)],border=NA,stringsAsFactors = F)
		# GBa Meat count of scallops > 95 mm in Georges A, slide 19. 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[2],contour=list(MCcont.poly,MCcont.data),direct = direct, title=mc.title,cex.mn=2,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survGBaLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBaLive.dat,subset=year==yr&state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(MCcont.data$col),title=mc.lab,
		       border=c(rep(length(lvls))),
		       inset=0.03,bty="n",title.adj = 0.2)
		legend("topright",
		       legend = c(paste('exploratory (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==2)$tow)),
		                        ")",sep=""),
		                  paste('regular (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==1)$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
		# Add seedboxes
				addPolys(GBa.boxes,lty=2,lwd=2)
		############
		
		
		
		############
		#Source26 Clapper plot for Georges A, slide 22 in 2014 presentation source("fn/Clap.plt.R")
		Clap.plt(subset(survGBaClap.dat,random==T),years=years,yl=c(0,15),
		         add.title = T,titl=clap.per.ts.title,cex.mn=2,lab.cx=1.2,axis.cx=1)
		############
		
		
		############
		#Clappers in Georges A slide 23
		lvls=c(1,2,5,10,15,20,30,40,50)
		CL <- contourLines(clap.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		clapCont.poly <- joinPolys(CP$PolySet,GBa.survey.bound.poly)
		cont.data<- data.frame(PID=unique(clapCont.poly$PID),
		                       col=brewer.pal(length(unique(clapCont.poly$PID)),clap.col),border=NA,stringsAsFactors = F)
		#Clappers in Georges A slide 23
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[2],contour=list(clapCont.poly,cont.data),direct = direct, title=clap.dis.title,cex.mn=2,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survGBaLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBaLive.dat,subset=year==yr&state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(cont.data$col),title="% Dead",
		       border=c(rep(length(lvls))),
		       inset=0.03,bty="n",title.adj = 0.2)
		legend("topright",
		       legend = c(paste('exploratory (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==2)$tow)),
		                        ")",sep=""),
		                  paste('regular (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==1)$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
		# Add seedboxes
				addPolys(GBa.boxes,lty=2,lwd=2)
		############
		
		
		###################### SEEDBOX PLOTS ###################### SEEDBOX PLOTS ###################### SEEDBOX PLOTS
		###################### SEEDBOX PLOTS ###################### SEEDBOX PLOTS ###################### SEEDBOX PLOTS
		
		# Set up a plot paramter for the boxes to make labeling and plotting occur right in ScallopMap.
		box.par<-data.frame(PID = unique(GBa.boxes$PID),label=unique(GBa.boxes$ID),lty=2)
		###########
		# SLide 25, general layout of the seedboxes with tow tracks and tow numbers added.
		windows(11,8.5)
		#Source 9 (/ScallopMap.r)
		ScallopMap(ylim=c(41.8,42.17),xlim=c(-67.15,-66.1),poly.lst=list(GBa.boxes,box.par),direct = direct, 
		           title=paste("Seedboxes (",bnk[2],"-",yr,")",sep=""),cex.mn=2,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,dec.deg = F)
		# To get this lined up nicely I need to do this for each box, this will need altered depending on the # of boxes.
		addLabels(box.par[1,],polyProp=data.frame(PID = unique(GBa.boxes$PID),stringsAsFactors = F),
		          placement="CENTROID",cex=1,polys=GBa.boxes,adj=c(0.5,-4))
		addLabels(box.par[2,],polyProp=data.frame(PID = unique(GBa.boxes$PID),stringsAsFactors = F),
		          placement="CENTROID",cex=1,polys=GBa.boxes,adj=c(0.2,-6.5))
		addLabels(box.par[3,],polyProp=data.frame(PID = unique(GBa.boxes$PID),stringsAsFactors = F),
		          placement="CENTROID",cex=1,polys=GBa.boxes,adj=c(0.5,-3))
		# Add the tow tracks
		addLines(subset(GBextrasDis[[2]],PID %in% C1$EID),col='blue')
		addLines(subset(GBextrasDis[[2]],PID %in% SB$EID),col='blue')
		addLines(subset(GBextrasDis[[2]],PID %in% AB$EID),col='blue')
		addLines(subset(GBaDis[[2]],PID %in% C1$EID),col='blue')
		addLines(subset(GBaDis[[2]],PID %in% SB$EID),col='blue')
		addLines(subset(GBaDis[[2]],PID %in% AB$EID),col='blue')
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% SB$EID,
		       pch=20,bg='black',cex=0.6)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% SB$EID,
		       pch=24,bg="darkorange",cex=0.6)
		# Add the comparative survey tows in C1
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% C1$EID,
		       pch=20,bg='black',cex=0.6)
		# Add the exploratory survey tows in C1
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% C1$EID,
		       pch=24,bg="darkorange",cex=0.6)
		# Add the comparative survey tows in AB
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% AB$EID,
		       pch=20,bg='black',cex=0.6)
		# Add the exploratory survey tows in AB
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% AB$EID,
		       pch=24,bg="darkorange",cex=0.6)
		# Add the legend
		legend("topright",
		   legend = c(paste('exploratory (n =',
		     length(unique(subset(survGBaLive.dat,year==yr & random==2 & (tow %in% AB$EID | tow %in% C1$EID | tow %in% SB$EID))$tow)),
		                      ")",sep=""),
		  paste('regular (n =',
		       length(unique(subset(survGBaLive.dat,year==yr & random==1 & (tow %in% AB$EID | tow %in% C1$EID | tow %in% SB$EID))$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
		# Now add the tow numbers to the plot.
		C1.labs<-subset(survGBaLive.dat,year==yr & tow %in% C1$EID,c('tow','elon','elat','tow'))
		names(C1.labs)<-c("PID","X","Y","label")
		SB.labs<-subset(survGBaLive.dat,year==yr & tow %in% SB$EID,c('tow','elon','elat','tow'))
		names(SB.labs)<-c("PID","X","Y","label")
		AB.labs<-subset(survGBaLive.dat,year==yr & tow %in% AB$EID,c('tow','elon','elat','tow'))
		names(AB.labs)<-c("PID","X","Y","label")
		addLabels(C1.labs,cex=0.8)
		addLabels(SB.labs,cex=0.8)
		addLabels(AB.labs,cex=0.8)
		###########
		

		
		#############  ABLine Plots ################
		
		###########
		#Source18 Shell height frequencies in George A, slide 26 (AB line?) source("fn/shf.plt.r")
		shf.plt(ps.dat,ABline.obj[[2]]$n.yst,from='special',yr=2011:yr,col1='grey80',type='sh',
		        col2=1,col3=1,xl=c(0,150),rel=F,rows=4,mean.line=F,
		        sample.size=ABline.obj[[1]]$n,
		        recline=c(RS[length(RS)]-5,CS[length(CS)]-5),ymax=1100,add.title = T,
		        titl = paste("Shell height frequency (A/B-LINE ",bnk[2],")",sep=""),cex.mn=3)
		###########
		
		# ABline
		lvls=c(1,100,200,500,1000,2000,5000,10000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='A/B line closure'))
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F) 
		CL <- contourLines(com.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='A/B line closure'))
		CL <- contourLines(rec.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		recCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='A/B line closure'))
		#Source9 This is showing the various scallop size classes in the NW closure, slide 32
		#  source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		par(mfrow=c(2,2),mar=c(4,4,4,6))
		# Fully recruited
		ScallopMap(ylim=c(41.85,42.015),xlim=c(-66.35,-66.1),bathy.source="quick", plot.bathy=T,plot.boundries = T,
		           bathcol=rgb(0,0,1,0.3),contour=list(comCont.poly,cont.data),xlab="",ylab="",cex.mn=2,
		           title= substitute(bold(paste("Fully recruited scallops (","">=a," mm)",sep="")),
		                             list(a=as.character(CS[length(years)]-5))),direct=direct,dec.deg = F)
		addLines(subset(GBextrasDis[[2]],PID %in% AB$EID),col='blue',lwd=2)
		addLines(subset(GBaDis[[2]],PID %in% AB$EID),col='blue',lwd=2)
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% AB$EID,
		       pch=20,bg='black',cex=1.3)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% AB$EID,
		       pch=24,bg="darkorange",cex=1.3)
		# Recruits
		ScallopMap(ylim=c(41.85,42.015),xlim=c(-66.35,-66.1),bathy.source="quick", plot.bathy=T,plot.boundries = T,
		           contour=list(recCont.poly,cont.data),cex.mn=2,xlab="",ylab="",
		           title= substitute(bold(paste("Recruit scallops (",b-a," mm)",sep="")),
		                             list(a=as.character(CS[length(years)]-6),b=as.character(RS[length(years)]-5))),
		           direct=direct,dec.deg = F)
		
		addLines(subset(GBextrasDis[[2]],PID %in% AB$EID),col='blue',lwd=2)
		addLines(subset(GBaDis[[2]],PID %in% AB$EID),col='blue',lwd=2)
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% AB$EID,
		       pch=20,bg='black',cex=1.3)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% AB$EID,
		       pch=24,bg="darkorange",cex=1.3)
		# Pre Recruits
		ScallopMap(ylim=c(41.85,42.015),xlim=c(-66.35,-66.1),bathy.source="quick", plot.bathy=T,plot.boundries = T,
		           contour=list(preCont.poly,cont.data),cex.mn=2,xlab="",ylab="",
		           title= substitute(bold(paste("Pre-recruit scallops (",""<b," mm)",sep="")),
		                             list(b=as.character(RS[length(years)]-5))),direct=direct,dec.deg = F)
		addLines(subset(GBextrasDis[[2]],PID %in% AB$EID),col='blue',lwd=2)
		addLines(subset(GBaDis[[2]],PID %in% AB$EID),col='blue',lwd=2)
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% AB$EID,
		       pch=20,bg='black',cex=1.3)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% AB$EID,
		       pch=24,bg="darkorange",cex=1.3)
		
		plot(1:10,type='n',axes=F,xlab='',ylab='',main=paste("A/B-LINE (",bnk[2],"-",yr,")",sep=""),cex.main=2)
		# Add the legend
		legend("left",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                  paste(lvls[length(lvls)],'+',sep='')),fill=c(reccont.data$col),title=N.tow.lab,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bty="n",cex=1.3)
		legend("topright",
		 legend = c(paste('exploratory (n =',
		   length(unique(subset(survGBaLive.dat,year==yr & random==2 & (tow %in% AB$EID ))$tow)),
		         ")",sep=""),
		  paste('regular (n =',
		    length(unique(subset(survGBaLive.dat,year==yr & random==1 & (tow %in% AB$EID ))$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bty="n",cex=1.3)
		
		#############  Seedbox2012 Plots ################
		
		###########
		#Source18 George A, Seedbox2012 shell height mean number, slide 28 source("fn/shf.plt.r")
		shf.plt(ps.dat,SBline.obj[[2]]$n.yst,from='special',yr=2011:yr,col1='grey80',
		        col2=1,col3=1,xl=c(0,150),rel=F,rows=4,sample.size=SBline.obj[[1]]$n,
		        recline=c(RS[length(RS)]-5,CS[length(CS)]-5),ymax=300,add.title = T,
		        titl = paste("Shell height frequency (SEEDBOX-2012 ",bnk[2],")",sep=""),cex.mn=3)
		###########
		
		
		# Seed box 2012 spatial plot, slide 30 from 2014.
		lvls=c(1,seq(100,700,100),1000)#,2000,5000,10000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='Seed box (2012 modified)'))
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F) 
		CL <- contourLines(com.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='Seed box (2012 modified)'))
		CL <- contourLines(rec.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		recCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='Seed box (2012 modified)'))
		#This is showing the various scallop size classes in Seed (box?), slide 30
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		par(mfrow=c(2,2))
		ScallopMap(ylim=c(41.93,42.015),xlim=c(-66.835,-66.74),bathy.source="quick", plot.bathy=T,plot.boundries = T,
		           bathcol=rgb(0,0,1,0.3),contour=list(comCont.poly,cont.data),xlab="",ylab="",cex.mn=2,
		           title= substitute(bold(paste("Fully recruited scallops (","">=a," mm)",sep="")),
		                             list(a=as.character(CS[length(CS)]-5))),
		           direct=direct,dec.deg = F)
		           
		addLines(subset(GBextrasDis[[2]],PID %in% SB$EID),col='blue',lwd=2)
		addLines(subset(GBaDis[[2]],PID %in% SB$EID),col='blue',lwd=2)
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% SB$EID,
		       pch=20,bg='black',cex=1.3)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% SB$EID,
		       pch=24,bg="darkorange",cex=1.3)
		
		
		ScallopMap(ylim=c(41.93,42.015),xlim=c(-66.835,-66.74),plot.bathy=T,plot.boundries = T,
		           contour=list(recCont.poly,cont.data),cex.mn=2,xlab="",ylab="",
		           title= substitute(bold(paste("Recruit scallops (",b-a," mm)",sep="")),
		                             list(a=as.character(CS[length(years)]-6),b=as.character(RS[length(years)]-5)))
		           ,direct=direct,dec.deg = F)
		addLines(subset(GBextrasDis[[2]],PID %in% SB$EID),col='blue',lwd=2)
		addLines(subset(GBaDis[[2]],PID %in% SB$EID),col='blue',lwd=2)
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% SB$EID,
		       pch=20,bg='black',cex=1.3)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% SB$EID,
		       pch=24,bg="darkorange",cex=1.3)
		
		
		ScallopMap(ylim=c(41.93,42.015),xlim=c(-66.835,-66.74),bathy.source="quick", plot.bathy=T,plot.boundries = T,
		           contour=list(preCont.poly,cont.data),cex.mn=2,xlab="",ylab="",
		           title= substitute(bold(paste("Pre-recruit scallops (",""<b," mm)",sep="")),
		                             list(b=as.character(RS[length(years)]-5)))
		           ,direct=direct,dec.deg = F)
		addLines(subset(GBextrasDis[[2]],PID %in% SB$EID),col='blue',lwd=2)
		addLines(subset(GBaDis[[2]],PID %in% SB$EID),col='blue',lwd=2)
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% SB$EID,
		       pch=20,bg='black',cex=1.3)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% SB$EID,
		       pch=24,bg="darkorange",cex=1.3)
		
		plot(1:10,type='n',axes=F,xlab='',ylab='',main=paste("Seedbox-2012 (",bnk[2],"-",yr,")",sep=""),cex.main=2)
		# Add the legend
		legend("left",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                  paste(lvls[length(lvls)],'+',sep='')),fill=c(reccont.data$col),title=N.tow.lab,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bty="n",cex=1.3)
		legend("topright",
		       legend = c(paste('exploratory (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==2 & (tow %in% SB$EID ))$tow)),
		                        ")",sep=""),
		                  paste('regular (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==1 & (tow %in% SB$EID ))$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bty='n',cex=1.3)
		
		
		#############  C1-012014 Plots ################
		
		###########
		# Since we include the Spring tows here I need to give the date of survey here.
		C1.t<-c("Aug 2012","May 2013","Aug 2013","May 2014","Aug 2014")
		#Source18 Georges A, C1-012014, slide 31, very cool! source("fn/shf.plt.r")
		shf.plt(ps.dat,C1line.obj[[2]]$n.yst,from='special',yr=C1.t,col1='grey80',
		        col2=1,col3=1,xl=c(0,150),rel=F,rows=5,ht=12,sample.size=C1line.obj[[1]]$n,
		        recline=c(RS[length(RS)]-5,CS[length(CS)]-5),ymax=600,add.title = T,
		        titl = paste("Shell height frequency (C1-012014 ",bnk[2],")",sep=""),cex.mn=3)
		###########
		
		###########
		# C1-012014 Spatial abundance plots slide 32
		#lvls=c(1,100,200,500,1000,2000,5000,10000)
		lvls=c(1,seq(100,700,100),1000)#,2000,5000,10000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		preCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='C1-012014'))
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),N.col),border=NA,stringsAsFactors = F) 
		CL <- contourLines(com.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		comCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='C1-012014'))
		CL <- contourLines(rec.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		recCont.poly <- joinPolys(CP$PolySet,subset(GBa.boxes,ID=='C1-012014'))
		#Source9 This is showing the various scallop size classes in the NW closure, slide 32
		#  source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		# Retain this window size as the plot goes bad with other sized windows.
		windows(11,8.5)
		par(mfrow=c(2,2),omi=c(0.2,0.2,0.2,0.5))
		# Fully recruited
		ScallopMap(ylim=c(42.05,42.14),xlim=c(-67.14,-66.95),bathy.source="quick", plot.bathy=T,plot.boundries = T,
		           bathcol=rgb(0,0,1,0.3),contour=list(comCont.poly,cont.data),xlab="",ylab="",cex.mn=2,
		           title= substitute(bold(paste("Fully recruited scallops (","">=a," mm)",sep="")),
		                             list(a=as.character(CS[length(years)]-5)))
		           ,direct=direct,dec.deg = F)
		addLines(subset(GBextrasDis[[2]],PID %in% C1$EID),col='blue',lwd=2)
		addLines(subset(GBaDis[[2]],PID %in% C1$EID),col='blue',lwd=2)
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% C1$EID,
		       pch=20,bg='black',cex=1.3)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% C1$EID,
		       pch=24,bg="darkorange",cex=1.3)
		# Recruits
		ScallopMap(ylim=c(42.05,42.14),xlim=c(-67.14,-66.95),plot.bathy=T,plot.boundries = T,
		           contour=list(recCont.poly,cont.data),cex.mn=2,xlab="",ylab="",
		           title= substitute(bold(paste("Recruit scallops (",b-a," mm)",sep="")),
		                             list(a=as.character(CS[length(years)]-6),b=as.character(RS[length(years)]-5)))
		           ,direct=direct,dec.deg = F)
		
		addLines(subset(GBextrasDis[[2]],PID %in% C1$EID),col='blue',lwd=2)
		addLines(subset(GBaDis[[2]],PID %in% C1$EID),col='blue',lwd=2)
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% C1$EID,
		       pch=20,bg='black',cex=1.3)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% C1$EID,
		       pch=24,bg="darkorange",cex=1.3)
		# Pre Recruits
		ScallopMap(ylim=c(42.05,42.14),xlim=c(-67.14,-66.95),bathy.source="quick", plot.bathy=T,plot.boundries = T,
		           contour=list(preCont.poly,cont.data),cex.mn=2,xlab="",ylab="",
		           title= substitute(bold(paste("Pre-recruit scallops (",""<b," mm)",sep="")),
		                             list(b=as.character(RS[length(years)]-5)))
		           ,direct=direct,dec.deg = F)
		addLines(subset(GBextrasDis[[2]],PID %in% C1$EID),col='blue',lwd=2)
		addLines(subset(GBaDis[[2]],PID %in% C1$EID),col='blue',lwd=2)
		# Add the regular survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year == yr & state=='live' & random == 1 &tow %in% C1$EID,
		       pch=20,bg='black',cex=1.3)
		# Add the exploratory survey tows in Seedbox 2012
		points(slat~slon,survGBaLive.dat,subset = year==yr & state =='live' & random %in% c(2,4,5)& tow %in% C1$EID,
		       pch=24,bg="darkorange",cex=1.3)
		
		plot(1:10,type='n',axes=F,xlab='',ylab='',main=paste("C1-012014 (",bnk[2],"-",yr,")",sep=""),cex.main=2)
		
		# Add the legend
		legend("left",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                  paste(lvls[length(lvls)],'+',sep='')),fill=c(reccont.data$col),title=N.tow.lab,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bty="n",cex=1.3)
		legend("topright",
		       legend = c(paste('exploratory (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==2 & (tow %in% SB$EID ))$tow)),
		                        ")",sep=""),
		                  paste('regular (n =',
		                        length(unique(subset(survGBaLive.dat,year==yr & random==1 & (tow %in% SB$EID ))$tow)),
		                        ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bty="n",cex=1.3)		###########
		
		
		
		
		
		
		## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots
		## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots
		## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots
		## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots## GBb plots
		## Set up plot titles
		survey.title <- substitute(bold(paste("Survey (",bank,"-",year,")",sep="")),
		                           list(year=as.character(yr),bank=bnk[3]))
		tow.track.title <- substitute(bold(paste("Tow tracks (",bank,"-",year,")",sep="")),
		                              list(year=as.character(yr),bank=bnk[3]))
		fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
		                              list(a=as.character(CS[length(CS)]-5),year=as.character(yr),bank=bnk[3]))
		rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
		                    list(a=as.character(CS[length(CS)]-6),b=as.character(RS[length(RS)]-5),year=as.character(yr),bank=bnk[3]))
		pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
		                            list(b=as.character(RS[length(RS)]-5),year=as.character(yr),bank=bnk[3]))
		cf.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
		                       list(year=as.character(yr),bank=bnk[3]))
		mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
		                       list(m=as.character(CS[length(CS)]-5),year=as.character(yr),bank=bnk[3]))
		survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
		                                list(year=as.character(yr),bank=bnk[3]))
		survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
		                                 list(bank=bnk[3]))
		SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
		                         list(bank=bnk[3]))
		MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-",year,")",sep="")),
		                         list(year=as.character(yr),bank=bnk[3]))
		CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank,")",sep="")),
		                          list(year=as.character(yr),bank=bnk[3]))
		clap.dis.title <- substitute(bold(paste("Clappers (% dead ", bank,"-",year,")",sep="")),
		                             list(bank=bnk[3],year=as.character(yr)))
		clap.ts.title <- substitute(bold(paste("Clapper time series (",bank,")",sep="")),
		                            list(bank=bnk[3]))
		clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,")",sep="")),
		                                list(bank=bnk[3]))
		## Set up plot titles
		
		
		
		##### DATA
		############
		#GBb Scallops per tow on Georges B, slide 34
		windows(11,8.5)
		ScallopMap(bnk[3],poly.lst=list(GBb.survey.detail.poly,GBb.surv.info),,direct = direct,cex.mn=2,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,title=survey.title,dec.deg = F)
		# Add the regular survey tows.
		#bg.col<-tapply(GBb.surv.info$col,GBb.surv.info$PName,unique)[c(2,3,1,4,5)]
		# Add the regular survey tows.
		points(lat~lon,survGBbLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBbLive.dat,subset=year==yr & state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		legend(-66.7,41.76,,legend=c(GBb.surv.info$PName),
		       fill=c(GBb.surv.info$col),border=c(rep('black',length(GBb.surv.info$PName))),
		       pch=c(rep(NA,length(GBb.surv.info$PName))),title = "Strata",title.adj=0.1,
		       pt.bg = c(rep(NA,length(GBb.surv.info$PName))),col='black',bty='n')
		legend(-66.25,41.76,legend = round(GBb.surv.info$area_km2),
		       fill=c(GBb.surv.info$col),border=c(rep('black',length(GBb.surv.info$PName))),
		       pch=c(rep(NA,length(GBb.surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.1,
		       pt.bg = c(rep(NA,length(GBb.surv.info$PName))),col='black',bty='n')
		legend(-66.1,41.76,legend = as.numeric(with(subset(survGBbLive.dat,year==yr),tapply(tow,stratum,length))),
		       fill=c(GBb.surv.info$col),border=c(rep('black',length(GBb.surv.info$PName))),
		       pch=c(rep(NA,length(GBb.surv.info$PName))),title = "Number of tows",title.adj=0.1,
		       pt.bg = c(rep(NA,length(GBb.surv.info$PName))),col='black',bty='n')
		legend("topright",legend = c(paste('exploratory (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==2)$tow)),
		                                   ")",sep=""),
		                             paste('regular (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==1)$tow)),
		                                   ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
		############
		
		
		################
		# This is GBb abundance time series slide 35 (DK note 2014 slide is actually the 2013 trend..oopsy...)
		# note scale of pre-recruits! #Source17 source("fn/survey.ts.r",local=T)
		survey.ts(surveyGBb.obj[[1]],years,Bank=bnk[3],pdf=F, CS=CS[length(years)]-5,RS=RS[length(years)]-5,
		          areas=GBb.strata.areas$towable_area,ys=.8,clr='blue',se=T,pch=16,yl2=c(2000,500,500),
		          add.title = T,titl=survey.ts.N.title,cx.mn = 3,axis.cx = 1.5)
		################
		
		
		################
		# This is GBb abundance time series slide 36, be cool to see what happens with that crazy precruitment of 2014, 
		# note scale of pre-recruits! #Source17 source("fn/survey.ts.r",local=T)
		survey.ts(surveyGBb.obj[[1]],years,Bank=bnk[3],pdf=F, CS=CS[length(years)]-5,RS=RS[length(years)]-5,
		          areas=GBb.strata.areas$towable_area,ys=.8,clr='blue',se=T,pch=16,yl2=c(16900,1690,1690),
		          add.title = T,titl=survey.ts.N.title,cx.mn = 3,axis.cx=1.5)
		################
		
		################
		# slide 37, this is biomass time series for GBb
		survey.ts(surveyGBb.obj[[1]],1981:yr,Bank=bnk[3],pdf=F,type='B', CS=CS[length(years)]-5,RS=RS[length(years)]-5, 
		          areas=GBb.strata.areas$towable_area,ys=.9,clr='blue',se=T,pch=16,yl2=12.3,
		          add.title = T,titl=survey.ts.BM.title,cx.mn = 3,axis.cx = 1.5)
		################
		
		# Shell height frequency plots on GBb #Source18 source("fn/shf.plt.r")
		################
		# Shell height frequency histograms, slide 38.
		shf.plt(ps.dat,surveyGBb.obj,from='surv',yr=2008:yr,col1='grey80',type='sh',
		        col2=1,col3=1,xl=c(0,200),rel=F,recline=c(RS[length(years)]-5,CS[length(years)]-5),
		        ymax=410,sample.size=F,mean.line=F,adj=0.9,add.title = T,titl=SHF.title,cex.mn = 3)	
		################
		
		################
		# Shell height frequency histograms, same as previous but rescaled to put this new cohort into perspective, slide 39
		shf.plt(ps.dat,surveyGBb.obj,from='surv',yr=2008:yr,col1='grey80',type='sh',
		        col2=1,col3=1,xl=c(0,200),rel=F,recline=c(RS[length(years)]-5,CS[length(years)]-5),
		        ymax=4100,sample.size=F,mean.line=F,adj=0.9,add.title = T,titl=SHF.title,cex.mn = 3)	
		################
	
	
		
		
		############
		# Plot for Fully Recruited across GB, this includes both 'a' and 'b' but plot will be bank specific. GBb slide 40 from 2014
		lvls1=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(com.contours$image.dat,levels=lvls1)
		CP <- convCP(CL)
		comCont1.poly <- joinPolys(CP$PolySet,GBb.strata.bound.poly)
		comcont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F) 
		lvls2=c(2000,5000,10000,20000,50000)
		# If counts are high enough to need lvls2 we use it, if not we just use lvls1.
		if(max(com.contours$image.dat$z)>=lvls2[1])
		{
		  CL <- contourLines(com.contours$image.dat,levels=lvls2)
		  CP <- convCP(CL)
		  comCont2.poly <- joinPolys(CP$PolySet,GBb.strata.bound.poly)
		  comCont2.poly$PID<-comCont2.poly$PID+length(lvls1)
		  comCont.poly<-rbind(comCont1.poly,comCont2.poly)
		  lvls<-c(lvls1,lvls2)
		  comcont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
		                             col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA,stringsAsFactors = F) 
		  comcont.data<-rbind(comcont1.data,comcont2.data)
		} # end if(max(com.contours$image.dat$z)>=lvls2[1])
		if(max(com.contours$image.dat$z)<lvls2[1])
		{
		  comCont.poly<-comCont1.poly
		  comcont.data<-comcont1.data
		  lvls <- lvls1
		} # end if(max(com.contours$image.dat$z)<lvls2[1])
		#Fully recruited for Georges B slide 40, 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[3],contour=list(comCont.poly,comcont.data),direct = direct,title=fully.rec.title,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,cex.mn=2,dec.deg = F)
		          
		# Add the regular survey tows.
		points(lat~lon,survGBbLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBbLive.dat,subset=year==yr&state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(reccont.data$col),title=N.tow.lab,title.adj=0.2,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.03,bty="n")
		legend("topright",legend = c(paste('exploratory (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==2)$tow)),
		                                   ")",sep=""),
		                             paste('regular (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==1)$tow)),
		                                   ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")	
		############
		
		############
		# Plot for Recruits across GB, this includes both 'a' and 'b' but plot will be bank specific. Georges B, slide 41
		lvls1=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(rec.contours$image.dat,levels=lvls1)
		CP <- convCP(CL)
		recCont1.poly <- joinPolys(CP$PolySet,GBb.strata.bound.poly)
		reccont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F)
		lvls2=c(2000,5000,10000,20000,50000)
		# If counts are high enough to need lvls2 we use it, if not we just use lvls1.
		if(max(rec.contours$image.dat$z)>=lvls2[1])
		{
		  CL <- contourLines(rec.contours$image.dat,levels=lvls2)
		  CP <- convCP(CL)
		  recCont2.poly <- joinPolys(CP$PolySet,GBb.strata.bound.poly)
		  recCont2.poly$PID<-recCont2.poly$PID+length(lvls1)
		  recCont.poly<-rbind(recCont1.poly,recCont2.poly)
		  lvls<-c(lvls1,lvls2)
		  reccont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
		                             col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA,stringsAsFactors = F)
		  reccont.data<-rbind(reccont1.data,reccont2.data)
		} # end if(max(rec.contours$image.dat$z)>=lvls2[1])
		if(max(rec.contours$image.dat$z)<lvls2[1])
		{
		  recCont.poly<-recCont1.poly
		  reccont.data<-reccont1.data
		  lvls <- lvls1
		} # end if(max(rec.contours$image.dat$z)<lvls2[1])
		#Source9 This is recruits again, Georges B, slide 41
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[3],contour=list(recCont.poly,reccont.data),direct = direct,title=rec.title,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,cex.mn=2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survGBbLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBbLive.dat,subset=year==yr&state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(reccont.data$col),title=N.tow.lab,title.adj=0.2,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.03,bty="n")
		legend("topright",legend = c(paste('exploratory (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==2)$tow)),
		                                   ")",sep=""),
		                             paste('regular (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==1)$tow)),
		                                   ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")	
		############
		
		############
		# Distribution plots for all tows on GB Aug 2014 Slide 42 in 2014 presentation
		# Pre-recruits across GB, this includes both 'a' and 'b' but plot will produce results for GBb only (based on Polygon)
		lvls1=c(2,5,10,20,50,100,200,500,1000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls1)
		CP <- convCP(CL)
		preCont1.poly <- joinPolys(CP$PolySet,GBb.strata.bound.poly)
		precont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F) 
		lvls2=c(2000,5000,10000,20000,50000)
		CL <- contourLines(pre.contours$image.dat,levels=lvls2)
		CP <- convCP(CL)
		preCont2.poly <- joinPolys(CP$PolySet,GBb.strata.bound.poly)
		preCont2.poly$PID<-preCont2.poly$PID+length(lvls1)
		preCont.poly<-rbind(preCont1.poly,preCont2.poly)
		lvls<-c(lvls1,lvls2)
		precont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
		                           col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA)
		precont.data<-rbind(precont1.data,precont2.data)
		# This is the Georges B prerecruits slide 42
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[3],contour=list(preCont.poly,precont.data),direct = direct,title=pre.rec.title,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,cex.mn=2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survGBbLive.dat,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBbLive.dat,subset=year==yr&state =='live' & random==2,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(precont.data$col),title=N.tow.lab,title.adj=0.2,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.03,bty="n")
		legend("topright",legend = c(paste('exploratory (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==2)$tow)),
		                                   ")",sep=""),
		                             paste('regular (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==1)$tow)),
		                                   ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")	
		############
		
		
		############
		# GBb Condition factor slide 44
		lvls=seq(6,14,by=1)
		CL <- contourLines(cf.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		cont.poly <- joinPolys(CP$PolySet,GBb.strata.bound.poly)
		cont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(length(lvls),cf.col),border=NA,stringsAsFactors = F) 
		#Source9 Condition factor for Georges B, slide 44.  
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[3],contour=list(cont.poly,cont.data),direct = direct,title=cf.title,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,cex.mn=2,dec.deg = F)
		# Add the survey tows in GBb, somewhat messy way to get there...
		temp <- subset(GBcfData$CF.data,year==yr,c('ID','lon','lat','CF'))
		names(temp) <- c("EID","X","Y","CF")
		temp$EID <- 1:nrow(temp)
		CF.B <-findPolys(temp,GBb.strata.bound.poly)
		addPoints(subset(temp,EID %in% CF.B$EID),pch=20,bg='black',cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(cont.data$col),title = cf.lab,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.025,bty="n")
		legend("topright",legend = c(paste('regular (n =',
		                                   length(subset(temp,EID %in% CF.B$EID)$CF),
		                                   ")",sep="")),title="Tow type",
		       pt.bg = c("black"),pch=c(20),bg = "white",inset=0.01,box.col="white")	
		############
		
		
		############
		# GBb Meat Count Slide 46, 2014
		lvls=c(seq(20,50,5))
		div=2
		CL <- contourLines(mc.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		# Note here I used the "comCont.poly" from the figure for slide 40 to define the spatial boundaries...
		# Done so as plot with blank data added has some bad edge effects. Run slide 40 code to create the comCont.poly object.
		MCcont.poly <- joinPolys(CP$PolySet,GBb.strata.bound.poly)
		Ncol=length(lvls)+div
		MCcont.data<- data.frame(PID=1:length(lvls),col=brewer.pal(Ncol,mc.col)[c(Ncol:(div+2),1)],border=NA,stringsAsFactors = F)
		# GBb Meat count but for Georges B, this is Slide 46 of the powerpoint.
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[3],contour=list(MCcont.poly,MCcont.data),direct = direct,title=mc.title,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,cex.mn=2,dec.deg = F)
		#points(lat~lon,na.omit(subset(survGBaLive.dat,year==2014,c('tow','lon','lat','meat.count'))),pch=16,cex=0.5)
		# Add the regular survey tows.
		points(lat~lon,na.omit(subset(survGBbLive.dat,year==yr & random ==1,
		                              c('tow','lon','lat','meat.count'))),pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,na.omit(subset(survGBbLive.dat,year==yr & random ==2,
		                              c('tow','lon','lat','meat.count'))),pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(MCcont.data$col),title=mc.lab,
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bty="n")
		legend("topright",legend = c(paste('exploratory (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==2)$tow)),
		                                   ")",sep=""),
		                             paste('regular (n =',
		                                   length(unique(subset(survGBbLive.dat,year==yr & random==1)$tow)),
		                                   ")",sep="")),title="Tow type",
		       pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")		############
		
		
		
		############
		#Source26 Georges B clappers, slide 48 source("fn/Clap.plt.R")
		Clap.plt(subset(survGBbClap.dat,random==T),years=years,yl=c(0,30),
		         add.title = T,titl=clap.per.ts.title,cex.mn=2,lab.cx=1.2,axis.cx=1)
		############
		
		############
		# Clappers in George B, slide 49 
		
		lvls=c(1,2,5,10,15,20,30,40)
		CL <- contourLines(clap.contours$image.dat,levels=lvls)
		CP <- convCP(CL)
		clapCont.poly <- joinPolys(CP$PolySet,GBb.strata.bound.poly)
		cont.data<- data.frame(PID=unique(clapCont.poly$PID),
		                       col=brewer.pal(length(unique(clapCont.poly$PID)),clap.col),border=NA,stringsAsFactors = F) 
		# Clappers in George B, slide 49 
		# Source9 source(paste(direct,"Maps/ScallopMap_test.r") #source("fn/ScallopMap.r")
		windows(11,8.5)
		ScallopMap(area=bnk[3],contour=list(clapCont.poly,cont.data),direct = direct,title=clap.dis.title,
		           plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
		           nafo.bord = T,nafo="all",nafo.lab = F,cex.mn=2,dec.deg = F)
		# Add the regular survey tows.
		points(lat~lon,survGBbClap.dat,subset=year==yr & random==1,pch=20,bg='black',cex=0.8)
		# Add the exploratory survey tows
		points(lat~lon,survGBbClap.dat,subset=year==yr & random==2,pch=24,bg="darkorange",cex=0.8)
		# Add the legend
		legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
		                      paste(lvls[length(lvls)],'+',sep='')),fill=c(cont.data$col),title = "% dead",
		       border=c(rep('black',length(lvls))),pch=c(rep(NA,length(lvls))),
		       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bty="n")
		legend("topright",legend = c(paste('regular (n =',
		                                   length(subset(survGBbClap.dat,year==yr & random==1)$tow),
		                                   ")",sep="")),title="Tow type",
		       pt.bg = c("black"),pch=c(20),bg = "white",inset=0.01,box.col="white")	
		
		
		#### END GBb #### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb
		#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb
		#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb#### END GBb
		
		
		
		###############  POTENTIALLY INTERESTING PLOTS NOT IN PRESENTATION
		# Figure not in the powerpoint, plotting the tow tracks
		windows(10,8)
		ScallopMap(ylim=c(41.2,42.2),xlim=c(-67.3,-65.6),cex=1.2,bathy.source="usgs",bathcol=rgb(0,0,1,0.3))
		points(slat~slon,survGBa.dat,subset=year==yr&state=='live',pch=21,col='red',
		       bg=GBa.surv.info$col[survGBa.dat$new.stratum[survGBa.dat$year==yr&survGBa.dat$state=='live']],cex=1,dec.deg = F)
		addLines(GBaDis[[2]],col='blue')
		addLines(GBextrasDis[[2]],col='blue')
		
		points(elat~elon,survGBa.dat,subset=year==yr&state=='live',pch=16,cex=0.5)
		# Not sure what this is for not in the powerpoint.
		ScallopMap(ylim=c(41.8,42.1),xlim=c(-67,-66.6),poly.lst=list(GBa.survey.detail.poly,GBa.surv.info),cex=1.2
		           ,bathy.source="usgs",bathcol=rgb(0,0,1,0.3))
		points(slat~slon,survGBa.dat,subset=year==yr&state=='live'&tow%in%c(41,81),pch=21,col='red',
		       bg=GBa.surv.info$col[survGBa.dat$new.stratum[survGBa.dat$year==yr&survGBa.dat$state=='live'&tow%in%c(41,81)]],cex=1)
		addLines(subset(GBaDis[[2]],PID%in%c(41,81)),col='blue')
		
		
		#Source22 Creates xyplot from lattice not in the powerpoint. source("fn/shwt.plt.r"), GB overall
		shwt.plt(GB.summer.SpatHtWt.fit,pt.col=rgb(0,0,0,0.2))
		
		# Shell Height Frequency Plots GBa specific.
		# This produces 2001-2007, not in powerpoint
		shf.plt(ps.dat,survey.obj,from='surv',yr=2001:2007,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),
		        rel=F,recline=c(75,95),wd=7,ht=8,ymax=110,mean.line=F)	
		# This produces the year 2014, not in powerpoint
		shf.plt(ps.dat,survey.obj,from='surv',yr=yr,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),
		        rel=F,recline=c(85,95),wd=7,ht=5,ymax=310,mean.line=F,rows=1)	
		
		
		
		#Source25 Clapper plot for Georges A, size disagregated not in powerpoint source("fn/Clap3.plt.R")
		Clap3.plt(subset(survGBaClap.dat,random==T),years=1981:yr,yl=c(0,15))
		
		
		##### FISHERY:
		
		#Read49
		fish.dat<-read.csv("data/RawFisheryData1981-2014.csv")
		GBaFish14<-subset(fish.dat,bank==bnk[3]&date>as.Date("2013-08-01"),c('lon','lat','pro.repwt'))
		names(GBaFish14)<-c("X","Y","Z")
		GBaFish14$EID<-1:nrow(GBaFish14)
		
		
		SeedF<-findPolys(GBaFish14,subset(boxes,NAME=='seed12revised'))
		GBaFish14seed<-subset(GBaFish14,EID%in%SeedF$EID)
		
		#This plots shell height, condition and meat weight time series, not used in powerpoint.
		#Source16 source("fn/stdts.plt.R")
		windows(8.5,10.5)
		par(mfrow=c(3,1),mar=c(1.5,1,0.5,0.5),omi=c(0.7,0.7,0.4,0.4))
		stdts.plt(subset(surveyGBb.obj[[1]],year>1995),y="l.bar",pch=17,lty=2,ylab="Average shell height (mm)",
		          mean.line=T,graphic='none',xlab='')
		stdts.plt(subset(surveyGBb.obj[[1]],year>1995),y="CF",pch=17,lty=2,ylab=expression("Condition factor  (" * g/dm^3*")"),
		          mean.line=T,graphic='none',xlab='')
		stdts.plt(subset(surveyGBb.obj[[1]],year>1995),y="w.bar",pch=17,lty=2,ylab="Average meat weight (g)",
		          mean.line=T,graphic='none',xlab='Year')
		
		# SHF in GBb but only 2014 rescaled, not used in powerpoint.
		shf.plt(ps.dat,surveyGBb.obj,from='surv',yr=2008:yr,col1='grey80',type='sh',
		        col2=1,col3=1,xl=c(0,200),rel=F,recline=c(RS[length(years)]-5,CS[length(years)]-5),
		        ymax=c(rep(410,6),4100),sample.size=F,mean.line=F,adj=0.9,add.title = T,titl=SHF.title,cex.mn = 3)	
		
		
		
		
	