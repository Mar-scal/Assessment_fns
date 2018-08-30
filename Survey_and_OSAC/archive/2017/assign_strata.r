####  This is a new function built during June of 2017 and is based on the restratwp function based on location of the tow.
## Update history
## June 2017, Revised to inclue the actual Strata_ID that is used in the survey and cleaned up the function a bit
## to remove some bits that weren't being used.
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
#    
#      
##
###############################################################################################################

#Arguments
#surv.dat:  Data from the survey
#polys:     Bounding polygons for area(s) of interest, called in as a list, 
#State:     Subset data into 'live' or 'dead'.  Default = "live"


# source("Y:/Assessment/2010/r/fn/restratwp.r")

assign.strata<-function(surv.dat,polys,State='live')
{
  # Load package
	require(PBSmapping)  || stop("You need PBSmapping package installed for this to work")
	# Subset the data into the state chosen, i.e. alive or dead
  survlive.dat<-subset(surv.dat,state==State)
	# Make a temporaty dataframe with locations, ID's, years and tow number
  tmp.dat<-data.frame(EID=1:nrow(survlive.dat),X=survlive.dat$slon,Y=survlive.dat$slat,year=survlive.dat$year,tow=survlive.dat$tow)
	# Find the polygons within the subset data
	str.dat<-findPolys(tmp.dat,polys)
	# Remove any EID's that are duplicated.
	str.dat<-str.dat[!duplicated(str.dat$EID),]
	# Assign the Strata ID to the str.dat
	for(i in 1:nrow(str.dat)) str.dat$Strata_ID[i] <- polys$Strata_ID[polys$PID == str.dat$PID[i]][1] 
	#	Merge the data together
	str.dat<-merge(tmp.dat,str.dat,all=T)
	surv.dat<-merge(surv.dat,str.dat[,c('year','tow',"Strata_ID")],by=c('year','tow'))
	# reorder the data by year and tow number
	surv.dat<-surv.dat[order(surv.dat$year,surv.dat$tow),]
	# Remove the old stratum column
	surv.dat <- surv.dat[,-which(names(surv.dat) == "stratum")]
	# Return object
	surv.dat
} #end function

		
		