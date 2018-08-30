####  Commented and checked by DK starting on July 28, 2015. Function is used to reassign the strata
####  based on location of the tow.
## Update history
## March 31 2016 by DK, tidying up structure and comments
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
#poly.data: Not sure.  Default = NULL
#State:     Subset data into 'live' or 'dead'.  Default = "live"


# source("Y:/Assessment/2010/r/fn/restratwp.r")

restratwp<-function(surv.dat,polys,poly.data=NULL,State='live')
{
  # Load package
	require(PBSmapping)  || stop("You need PBSmapping package installed for this to work")
	# Subset the data into the state chosen, i.e. alive or dead
  survlive.dat<-subset(surv.dat,state==State)
	# Make a temporaty dataframe with locations, ID's, years and tow number
  tmp.dat<-data.frame(EID=1:nrow(survlive.dat),X=survlive.dat$slon,Y=survlive.dat$slat,year=survlive.dat$year,tow=survlive.dat$tow)
	str.lst<-list(NULL)
	
	# Run a loop for each list element in polys, often this will just be 1.
	for(i in 1:length(polys))
	  {
	    # Find the polygons within the subset data
		  str.lst[[i]]<-findPolys(tmp.dat,polys[[i]])
	  } #end for(i in 1:length(polys))
	# Unwrap the data, skip any EID's that are duplicated.
	str.dat<-subset(do.call("rbind",str.lst),!duplicated(EID))
	
	# if we have poly data change the name to "new.stratum" and merge the data together.
	if(is.null(poly.data)==F)
	  {
  		names(poly.data)[names(poly.data)=='strata']<-'new.stratum'
  		str.dat<-merge(str.dat,poly.data[c('PID','new.stratum')])
	  } #end if(!is.null(poly.data))
	
	# If no poly.data just make a new column of new straum based on PID's
	if(is.null(poly.data)==T) str.dat$new.stratum<-str.dat$PID
	
  #	Merge the data together
	str.dat<-merge(tmp.dat,str.dat,all=T)
	surv.dat<-merge(surv.dat,str.dat[,c('year','tow','new.stratum')],by=c('year','tow'))
	# reorder the data by year and tow number
	surv.dat<-surv.dat[order(surv.dat$year,surv.dat$tow),]
	
	# Return object
	surv.dat

} #end function

		
		