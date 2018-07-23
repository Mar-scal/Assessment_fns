################################################################################################################
##### This script is used to determine what happened to our condition factor estimates from 2010-2015
################################################################################################################
####
################################################################################################################
#Revisions
# Oct 2016:  This file will need worked on to work properly given changes to the assessment file structure...

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
# First load required packages and data
require(PBSmapping)
require(RColorBrewer)
# Set the working directory
#direct = "Y:/Offshore scallop/Assessment/Assessment_fns/"
direct = "d:/r/"
yr=2015
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.R",sep=""))
direct = "d:/r/" # DK note this is just needed until I fix up survey summary
############################# GENERAL DATA ########################################################
############################# GENERAL DATA ########################################################

############################# END GENERAL DATA ########################################################


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

# Optional loads
# These 3 need to be produced before they really are of any use, so don't load them here unless you know what they are! 

source(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/GBcfData.R",sep=""))     #Source30
#source(paste(direct,"Assessment_fns/Data/Survey_data/",yr,"/Survey_summary_output/surveyObj.R",sep=""))    #Source31 
#source(paste(direct,"Assessment_fns/Data/Survey_data/",yr,"/Survey_summary_output/surveyObj.R",sep=""))    #Source31 
################################## End Load Functions   #######################################################



		################################################################################################################
		################################ SECTION 2 Georges BANK SUMMER##################################################
		### Georges BANK SUMMER ### Georges BANK SUMMER### Georges BANK SUMMER### Georges BANK SUMMER### Georges BANK SUMMER
		################################ SECTION 2 Georges BANK SUMMER #################################################
		################################################################################################################
		
		##################  Bring in and ID the variables needed to create the plots ##################################
		# Shell height for knife edge recruitment based on portsampling data
		# The CS and RS specified here actually 5 higher than the actual shell heights
		# CS = Shell height for knife-edge recriutment:  
    # Correctly specifying the years here really matters since the RS and CS are changing with time, begs for a better method!!
		# 1981-1985 CS = 80, RS = 65
		# From 1986-1995 CS = 90, RS= 80
		# From 1996-current CS= 100, RS = 90
		CS = c(rep(80,5),rep(90,10),rep(100,yr-1995))
		# RS = Shell height 1 year previous to CS
		RS = c(rep(65,5),rep(80,10),rep(90,yr-1995))
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
		GBbDis<-dist.coef(301:330,path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBb/",sep=""),
		                  w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000)
		GBextrasDis<-dist.coef(c(910:915),
		                       path=paste(direct,"Data/Tow_tracks/",yr,"/Summer/GBa/",sep=""),
		                       w=c(1:10,9:1),rule=8,smooth=T, plt=F,meh=1000)
		

		##  Now bring in the data
		# survey data for GBa and GBb
		survGB.dat<-subset(summerSurv.dat,bank %in% bnk[2:3])
		# Reassign all survey tows > 900 to be experimental tows
		survGB.dat$random[survGB.dat$year==yr&survGB.dat$tow>900]<-2
		# MEAT WEIGHT DATA GEORGES BANKS from 2011-2014, stitch them together. Drop the stratum column from 2011-2012.
		GB11.mw<-subset(Summersurv2011$MWs[,-grep("stratum",names(Summersurv2011$MWs))],bank %in% bnk[2:3])
		GB12.mw<-na.omit(subset(Summersurv2012$MWs[,-grep("stratum",names(Summersurv2012$MWs))],bank %in% bnk[2:3]))
		GB13.mw<-subset(SurvDB$MWs,bank %in% bnk[2:3] & cruise=="TE16")
		GB14.mw<-subset(SurvDB$MWs,bank %in% bnk[2:3] & cruise=="TE18")
		GB15.mw<-subset(SurvDB$MWs,bank %in% bnk[2:3] & cruise=="LE02")
		GBAug.mw<-rbind(GB11.mw,GB12.mw,GB13.mw,GB14.mw,GB15.mw)
		GBAug.mw$month<-8
		# Convert Shell height to decimeters for 2015
		GB15.mwdm<-GB15.mw
		GB15.mwdm$sh<-GB15.mw$sh/100
		GB14.mwdm<-GB14.mw
		GB14.mwdm$sh<-GB14.mw$sh/100
		GB13.mwdm<-GB13.mw
		GB13.mwdm$sh<-GB13.mw$sh/100
		GB12.mwdm<-GB12.mw
		GB12.mwdm$sh<-GB12.mw$sh/100
		GB11.mwdm<-GB11.mw
		GB11.mwdm$sh<-GB11.mw$sh/100
		
		# DK Note: We have an NA in the data from 2014, flag that to Alan.
		# Clean up any NA's and note them to Alan
		#GB15.mwdm <- GB15.mwdm[-which(is.na(GB15.mwdm$wmw)),]
		GB14.mwdm <- GB14.mwdm[-which(is.na(GB14.mwdm$wmw)),]
		#GB13.mwdm <- GB13.mwdm[-which(is.na(GB13.mwdm$wmw)),]
		#GB12.mwdm <- GB12.mwdm[-which(is.na(GB12.mwdm$wmw)),]
		GB11.mwdm <- GB11.mwdm[-which(is.na(GB11.mwdm$wmw)),]
		
		# Now load in the May data.
		#survGB.dat<-subset(springSurv.dat,bank=='GB') Survey data for spring on both Georges Banks
		survGBMay.dat<-subset(springSurv.dat,bank %in% bnk & year > 1989)
		
		#Source10 source("fn/getdis.r") Calculate the tow track distances for Georges Spring survey. no 2015 only 2014 
		GBDis.may<-dist.coef(subset(survGBMay.dat,year==2014 & state=='live')$tow,
		                 path=paste(direct,"Data/Tow_tracks/",2014,"/Spring/GB/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)
		
		# Meat weight data for Georges Spring from 2011 - 2014, stitch them all together.
		GB11may.mw<-subset(Springsurv2011$MWs,bank==bnk[1])
		GB12may.mw<-subset(Springsurv2012$MWs,bank==bnk[1])
		GB13may.mw<-subset(SurvDB$MWs,bank %in% bnk[2:3] & cruise=="TE15")
		GB14may.mw<-subset(SurvDB$MWs,bank %in% bnk[2:3] & cruise=="TE17")
		GBMay.mw<-rbind(GB11may.mw,GB12may.mw,GB13may.mw,GB14may.mw)
		# Convert shell height in 2014 to decimeters.
		GB14May.mwdm<-GB14may.mw
		GB14May.mwdm$sh<-GB14may.mw$sh/100
		GB13May.mwdm<-GB13may.mw
		GB13May.mwdm$sh<-GB13may.mw$sh/100
		GB12May.mwdm<-GB12may.mw
		GB12May.mwdm$sh<-GB12may.mw$sh/100
		GB11May.mwdm<-GB11may.mw
		GB11May.mwdm$sh<-GB11may.mw$sh/100
		
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
		#write.csv(GBmw.dat,paste(direct,"data/Survey_data/",yr,"/Summer/GB/GBmwData.csv",sep=""),row.names=F)
		
		# Here is 2015 model for the summer...
		GB.summer.mwsh.15 <- shwt.lme(GB15.mwdm,random.effect='tow',b.par=3)
		GB.summer.mwsh.15$A*1^3
		
		
		# MODEL - This is the meat weight Shell height realationship.  
		#MEAT WEIGHT SHELL HEIGHT RELATIONSHIP 2014 
		#Source11 source("fn/shwt.lme.r") note thtat the exponent is set as a parameter here b=3
		GB.summer.mwsh.14<-shwt.lme(GB14.mwdm,random.effect='tow',b.par=3)
		GB.spring.mwsh.14<-shwt.lme(GB14May.mwdm,random.effect='tow',b.par=3)
		
		windows(15,8)
		par(mfrow=c(1,2))
		shwt.plt1(GB.summer.mwsh.14,lw=3,ht=10,wd=12,cx=1.5,titl = "MWSH Summer 2014",cex.mn = 2,las=1)
		shwt.plt1(GB.spring.mwsh.14,lw=3,ht=10,wd=12,cx=1.5,titl = "MWSH Spring 2014",cex.mn = 2,las=1)
		# At 100 mm what do we expect for spring vs. summer
		GB.summer.mwsh.14$A*1^3
		GB.spring.mwsh.14$A*1^3
		
		
		GB.summer.mwsh.13<-shwt.lme(GB13.mwdm,random.effect='tow',b.par=3)
		GB.spring.mwsh.13<-shwt.lme(GB13May.mwdm,random.effect='tow',b.par=3)
		
		windows(15,8)
		par(mfrow=c(1,2))
		shwt.plt1(GB.summer.mwsh.13,lw=3,ht=10,wd=12,cx=1.5,titl = "MWSH Summer 2013",cex.mn = 2,las=1)
		shwt.plt1(GB.spring.mwsh.13,lw=3,ht=10,wd=12,cx=1.5,titl = "MWSH Spring 2013",cex.mn = 2,las=1)
		# At 100 mm what do we expect for spring vs. summer
		GB.summer.mwsh.13$A*1^3
		GB.spring.mwsh.13$A*1^3
		
		GB.summer.mwsh.12<-shwt.lme(GB12.mwdm,random.effect='tow',b.par=3)
		GB.spring.mwsh.12<-shwt.lme(GB12May.mwdm,random.effect='tow',b.par=3)
		
		windows(15,8)
		par(mfrow=c(1,2))
		shwt.plt1(GB.summer.mwsh.12,lw=3,ht=10,wd=12,cx=1.5,titl = "MWSH Summer 2012",cex.mn = 2,las=1)
		shwt.plt1(GB.spring.mwsh.12,lw=3,ht=10,wd=12,cx=1.5,titl = "MWSH Spring 2012",cex.mn = 2,las=1)
		# At 100 mm what do we expect for spring vs. summer
		GB.summer.mwsh.12$A*1^3
		GB.spring.mwsh.12$A*1^3
		
		# 2011 relationship...
		GB.summer.mwsh.11<-shwt.lme(GB11.mwdm,random.effect='tow',b.par=3)
		GB.spring.mwsh.11<-shwt.lme(GB11May.mwdm,random.effect='tow',b.par=3)
		
		windows(15,8)
		par(mfrow=c(1,2))
		shwt.plt1(GB.summer.mwsh.11,lw=3,ht=10,wd=12,cx=1.5,titl = "MWSH Summer 2011",cex.mn = 2,las=1)
		shwt.plt1(GB.spring.mwsh.11,lw=3,ht=10,wd=12,cx=1.5,titl = "MWSH Spring 2011",cex.mn = 2,las=1)
		# At 100 mm what do we expect for spring vs. summer
		GB.summer.mwsh.11$A*1^3
		GB.spring.mwsh.11$A*1^3
		
		## MODEL - This is the model used to esimate condition factor across all of Georges Bank
		#Source13 source("fn/condFac.r") # Note that the condition Factor is calculated for August
		# DK Note: And this was done incorrectly for GB in survey summary forever which messes up
		# everything that uses GBcfData, which includes...all CF plots, MC plots, 
		CF.sum.15 <- condFac(na.omit(subset(GBmw.dat,month%in%c(8,"08") & year <=2015)),
		                     subset(summerSurv.dat,bank %in% bnk[2:3] & year <=2015),model.type='gam_f')
		CF.sum.14 <- condFac(na.omit(subset(GBmw.dat,month%in%c(8,"08") & year <=2014)),
		                     subset(summerSurv.dat,bank %in% bnk[2:3] & year <=2014),model.type='gam_f')
		CF.sum.13 <- condFac(na.omit(subset(GBmw.dat,month%in%c(8,"08") & year <=2013)),
		                     subset(summerSurv.dat,bank %in% bnk[2:3] & year <=2013),model.type='gam_f')
		CF.sum.12 <- condFac(na.omit(subset(GBmw.dat,month%in%c(8,"08") & year <=2012)),
		                     subset(summerSurv.dat,bank %in% bnk[2:3] & year <=2012),model.type='gam_f')
		CF.sum.11 <- condFac(na.omit(subset(GBmw.dat,month%in%c(8,"08") & year <=2011)),
		                     subset(summerSurv.dat,bank %in% bnk[2:3] & year <=2011),model.type='gam_f')
		
		CF.spr.15 <- condFac(na.omit(subset(GBmw.dat,month%in%c(5,6) & year <=2015)),
		                     subset(springSurv.dat,bank %in% bnk & year > 1989 & year <=2015),model.type='gam_f')
		
		
		names(CF.spr.15$CFyrs)[5]<-'CFMay'
		CF.15 <- merge(CF.sum.15$CFyrs,CF.spr.15$CFyrs[,-(2:4)],all=T)
		CF.15$y2<-CF.15$year-0.25
		CF.15	<- merge(CF.15,data.frame(year=2016),all=T)
		
		CF.spr.14 <- condFac(na.omit(subset(GBmw.dat,month%in%c(5,6) & year <=2014)),
		                     subset(springSurv.dat,bank %in% bnk & year > 1989 & year <=2014),model.type='gam_f')
		names(CF.spr.14$CFyrs)[5]<-'CFMay'
		CF.14 <- merge(CF.sum.14$CFyrs,CF.spr.14$CFyrs[,-(2:4)],all=T)
		CF.14$y2<-CF.14$year-0.25
		CF.14	<- merge(CF.14,data.frame(year=2015),all=T)
		
		CF.spr.13 <- condFac(na.omit(subset(GBmw.dat,month%in%c(5,6) & year <=2013)),
		                     subset(springSurv.dat,bank %in% bnk & year > 1989 & year <=2013),model.type='gam_f')
		names(CF.spr.13$CFyrs)[5]<-'CFMay'
		CF.13 <- merge(CF.sum.13$CFyrs,CF.spr.13$CFyrs[,-(2:4)],all=T)
		CF.13$y2<-CF.13$year-0.25
		CF.13	<- merge(CF.13,data.frame(year=2014),all=T)
		
		
		CF.spr.12 <- condFac(na.omit(subset(GBmw.dat,month%in%c(5,6) & year <=2012)),
		                     subset(springSurv.dat,bank %in% bnk & year > 1989 & year <=2012),model.type='gam_f')
		names(CF.spr.12$CFyrs)[5]<-'CFMay'
		CF.12 <- merge(CF.sum.12$CFyrs,CF.spr.12$CFyrs[,-(2:4)],all=T)
		CF.12$y2<-CF.12$year-0.25
		CF.12	<- merge(CF.12,data.frame(year=2013),all=T)
		
		CF.spr.11 <- condFac(na.omit(subset(GBmw.dat,month%in%c(5,6) & year <=2011)),
		                     subset(springSurv.dat,bank %in% bnk & year > 1989 & year <=2011),model.type='gam_f')
		names(CF.spr.11$CFyrs)[5]<-'CFMay'
		CF.11 <- merge(CF.sum.11$CFyrs,CF.spr.11$CFyrs[,-(2:4)],all=T)
		CF.11$y2<-CF.11$year-0.25
		CF.11	<- merge(CF.11,data.frame(year=2012),all=T)
		
		windows(11,8.5)
		stdts.plt(CF.15[-(1:3),],x=c('year','y2'),y=c('CF','CFMay'),pch=16,ylab="CF 2015",las=1,col=c("blue","red"),
		          mean.line=T,graphic='none',xlab='Year',ylim=c(12,22),titl=CF.ts.title,cex.mn=2)
		legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
		############
		windows(11,8.5)
		stdts.plt(CF.14[-(1:3),],x=c('year','y2'),y=c('CF','CFMay'),pch=16,ylab="CF 2014",las=1,col=c("blue","red"),
		          mean.line=T,graphic='none',xlab='Year',ylim=c(12,22),titl=CF.ts.title,cex.mn=2)
		legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
		############
		windows(11,8.5)
		stdts.plt(CF.13[-(1:3),],x=c('year','y2'),y=c('CF','CFMay'),pch=16,ylab="CF 2013",las=1,col=c("blue","red"),
		          mean.line=T,graphic='none',xlab='Year',ylim=c(12,22),titl=CF.ts.title,cex.mn=2)
		legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
		############
		windows(11,8.5)
		stdts.plt(CF.12[-(1:3),],x=c('year','y2'),y=c('CF','CFMay'),pch=16,ylab="CF 2012",las=1,col=c("blue","red"),
		          mean.line=T,graphic='none',xlab='Year',ylim=c(12,22),titl=CF.ts.title,cex.mn=2)
		legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
		############
		windows(11,8.5)
		stdts.plt(CF.11[-(1:3),],x=c('year','y2'),y=c('CF','CFMay'),pch=16,ylab="CF 2011",las=1,col=c("blue","red"),
		          mean.line=T,graphic='none',xlab='Year',ylim=c(12,22),titl=CF.ts.title,cex.mn=2)
		legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
		############
		
		# Output the predictions and give a new ID
		sum.15<-CF.sum.15$pred.dat
		sum.15$ID<-paste(sum.15$year,sum.15$tow,sep='.')
		tmp.dat<-subset(CF.sum.15$CF.data,select=c("ID","CF"))
		names(tmp.dat)[2]<-"CFh"
		sum.15<-merge(sum.15,tmp.dat,all.x=T)
		sum.15$CFh[is.na(sum.15$CFh)]<-sum.15$CF[is.na(sum.15$CFh)]
		
		sum.14<-CF.sum.14$pred.dat
		sum.14$ID<-paste(sum.14$year,sum.14$tow,sep='.')
		tmp.dat<-subset(CF.sum.14$CF.data,select=c("ID","CF"))
		names(tmp.dat)[2]<-"CFh"
		sum.14<-merge(sum.14,tmp.dat,all.x=T)
		
		sum.13<-CF.sum.13$pred.dat
		sum.13$ID<-paste(sum.12$year,sum.12$tow,sep='.')
		tmp.dat<-subset(CF.sum.13$CF.data,select=c("ID","CF"))
		names(tmp.dat)[2]<-"CFh"
		sum.13<-merge(sum.13,tmp.dat,all.x=T)
		
		
		sum.12<-CF.sum.12$pred.dat
		sum.12$ID<-paste(sum.12$year,sum.12$tow,sep='.')
		tmp.dat<-subset(CF.sum.12$CF.data,select=c("ID","CF"))
		names(tmp.dat)[2]<-"CFh"
		sum.12<-merge(sum.12,tmp.dat,all.x=T)
		
		sum.11<-CF.sum.11$pred.dat
		sum.11$ID<-paste(sum.11$year,sum.11$tow,sep='.')
		tmp.dat<-subset(CF.sum.11$CF.data,select=c("ID","CF"))
		names(tmp.dat)[2]<-"CFh"
		sum.11<-merge(sum.11,tmp.dat,all.x=T)
		
		
		# Output the predictions and give a new ID
		spr.15<- CF.spr.15$pred.dat
		spr.14<-CF.spr.14$pred.dat
		spr.13<-CF.spr.13$pred.dat
		spr.12<-CF.spr.12$pred.dat
		spr.11<-CF.spr.11$pred.dat

		

		
		##### What is the confac model actually giving us...
		
		str(CF.sum.15)
		
		plot(CF~year,CF.sum.15$CFyrs,type='o',pch=16)
		wgt.dat2 <- na.omit(subset(GBmw.dat,month%in%c(8,"08") & year <=2011))
		wgt.dat2 <- na.omit(subset(GBmw.dat,month%in%c(5,6) & year <=2014))
		wgt.dat2 <- na.omit(subset(GBmw.dat,month=c(5,6) & year <=2011))
		
		# Convert shell heights to decimeters
		wgt.dat2$sh<-wgt.dat2$sh/100
		
		# Calculate the meat weight shell height relationship, remember if b.par = 3 this assumes an allometric realtionship.
		# Notice that we use a different random effect here, it is ID not tow, this is done since we may have the same tow # in different years.
		SpatHtWt.fit<-shwt.lme(wgt.dat2,random.effect='ID',b.par=b.par,verbose=F)
		
    SpatHtWt.fit$A
    SpatHtWt.fit$B
    range(SpatHtWt.fit$a)
    SpatHtWt.fit$b
		
    
    CF.data<-merge(wgt.dat2[!duplicated(wgt.dat2$ID),c('ID','lon','lat','year','depth','tow')],SpatHtWt.fit$fit,all=T)
    # Make sure the names are what we want
    names(CF.data)<-c('ID','lon','lat','year','depth','tow','CF')
    
    # Predict condition factor over bank using one of 5 models.
    # This model assumes CF varies only with depth and year, Gaussian and linear relationship, no random effects (year might be best treated as such)
    CF.fit<-gam(CF~s(lon,lat)+s(depth)+as.factor(year),data=CF.data)
    CF.fit<-gam(CF~s(depth)+as.factor(year),data=CF.data)
    CF.fit<-gam(CF ~ as.factor(year),data=CF.data)
    
    plot(CF.fit)
    # Make a new object to build predictions from, basically predictions for mean data for each year.
    CFyrs<-data.frame(year=1985:2014,depth=mean(CF.data$depth),lon=mean(CF.data$lon),lat=mean(CF.data$lat))
    # Now do the prediction
    CFyrs$CF=predict(CF.fit,CFyrs)
    windows(8,8)
    plot(CFyrs$CF~CFyrs$year,type="o",col="blue")
    abline(h = mean(CFyrs$CF),col="blue")
		lines(CFyrs$CF~CFyrs$year,type="o",lty=2,col="grey")
		abline(h = mean(CFyrs$CF),col="grey")
		CFyrs$CF[CFyrs$year==1998]
		mean(CFyrs$CF)
		