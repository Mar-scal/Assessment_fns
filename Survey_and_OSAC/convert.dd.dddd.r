#####  DK looking at this file as of July 27, 2015 #########
### CONVERTS lat/lon data to decimal degrees can handle a vector with multiple formats
## Update history
## March 31 2016 by DK, tidying up structure and comments
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##
##  1: get.offshore.survey.r 
##  2: getdis.r
##  3:  A bunch of others I should document!!
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#  
#      
##
###############################################################################################################

###############################################################################################################
# ARGUMENTS
# X:  A vector of lat or long data.
# format:  how to convert the data.  Default = 'dec.deg', also takes 'deg.min'
###############################################################################################################



convert.dd.dddd<-function(x,format='dec.deg')
  {
	
	if(format=='dec.deg')
	  {
		dat<-data.frame(ddmm.mm=x,dd.dddd=NA)
		
			#convert from degrees-minutes-seconds -> degrees
		  # Here we choose only data which are not NA's and data are > 9000, the 9000 tell us that it must be in ddmmss format 
			ddmmss<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>9000]
			# this moves to a ddmm.ss format
			ddmm.ss<-ddmmss/100
			# round down to close integer
			ddmm<-trunc(ddmm.ss)
			# Pull out the seconds and convert them to decimals
			ss<-(ddmm.ss-ddmm)*100
			dd.mm<-ddmm/100
			# round down to close integer 
			dd<-trunc(dd.mm)
			# pull out the minutes and convert them
			mm<-(dd.mm-dd)*100
			# Put it all together, note conversions from mintues and seconds.
			dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>9000]<-dd+mm/60+ss/3600
		
			#degrees-minutes -> degrees
			# If the data are between 90 and 9000 it must be in ddmm.ss format
			dd.mmmm<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>90&abs(dat$ddmm.mm)<9000]/100
			# round down to closest integer 
			dd<-trunc(dd.mmmm)
			mm.mm<-(dd.mmmm-dd)*100
			# Put it all together, note conversion from mintues
			dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>90&abs(dat$ddmm.mm)<9000]<-dd+mm.mm/60
	
			#degrees -> degrees if the data are less than 90, format is dd.mm, no conversion is necessary.
			dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)<90]<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)<90]
		
			# return the number to any function calling this one.
		return(dat$dd.dddd)
	}
	
	if(format=='deg.min')
	  {
		dat<-data.frame(ddmm.mm=NA,dd.dddd=x)
		
			#degrees-minutes-seconds -> degrees-minutes
		  # Here we choose only data which are not NA's and data are > 9000, the 9000 tell us that it must be in ddmmss format 
		  ddmmss<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]
			# This bit pulls out the seconds
		  ddmm.ss<-ddmmss/100
			ddmm<-trunc(ddmm.ss)
			ss<-(ddmm.ss-ddmm)*100
			# Put it all together, note conversion from seconds while minutes remains as is.
			dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]<-ddmm+ss/60
		
			#degrees-minutes -> degrees-minutes
			# If the data are between 90 and 9000 it must be in ddmm.ss format so done do anything
			dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]

			#degrees -> degrees-minutes
			# if the data are less than 90, format is dd.mm need to convert to ddmm.
			dd.dddd<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]
			# This bit pulls out the minutes from the decimal
			dd<-trunc(dd.dddd)
			# Note the converstion to minutes here
			mm.mm<-(dd.dddd-dd)*60
			# Put it all together, note dd needs to be *100 (60 becomes 6000 in this format)
			dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]<-dd*100+mm.mm
	
		return(dat$ddmm.mm)
	}
### The code in this section originated with
### Author: Daniela Cianci (University of Utrecht)  Contact: neil.alexander@zoo.ox.ac.uk
### website: http://www.edenextdata.com   
### October 2014 DK modified to allow for  multiple values, negatives, extract as an expression 
###  and to add degree symbol into the final ouput object   
### The returned object here is rather different than deg.min or dec.deg, the objective of this
### section was (at least originally) to get "nice" coordinates for Mapping functions.
  if(format=='deg.min.sec')
    {
      dat<-x
      # If data is not negative we will skip various sections of this.
      neg <- F
      # If data is negative make it a positive but also make a note 
      # that is was converted so we can convert it back.
      if(any(dat < 0 ))
        {
          dat[dat < 0 ] <- -dat[dat < 0 ]
          neg <- T 
        }  # end if(any(degfloat < 0 ))
      
      deg <- as.integer(dat)
      
      minfloat <- 60*(dat - deg)
      min <- as.integer(minfloat)
      secfloat <- 60*(minfloat - min)
      ### Round seconds to desired accuracy:
      sec <- round(secfloat, digits=2 )
      ### After rounding, the seconds might become 60
      ### The following if-tests are not necessary if no 
      ### rounding is done.
      if (any(sec == 60)) 
        {
          min[sec==60] <- min[sec==60] + 1
          sec[sec==60] <- 0
        }
      if (any(min == 60))
        {
         deg[min==60] <- deg[min==60] + 1
         min[min==60] <- 0
        } # end if (min == 60)
      if(neg == T) deg <- -deg
      ## Now I want to output a couple of different objects from this
      ## One is a simple output of the deg-min-sec, the other is
      ## an expression that includes the degree symbol for Mapping nice coordinates.
      # First the simple return
      dm<-paste(deg,min,sep=":")
      dms<-data.frame(paste(dm,sec,sep=":"))
      colnames(dms) <- "Degree_Minute_Seconds"
      dm <- data.frame(dm)
      colnames(dm) <-"Degree_Minutes"
      # Now return the expression for plotting axes...
      dash <- "-"
      dms.lab <- parse(text = paste(deg, "*degree~",min,":",sec, sep = ""))
      dm.lab <- parse(text = paste(deg, "*degree~",min,sep = ""))
      s.lab <- sec
      dat <- list(Degree_Min=dm,Degree_Min_Sec=dms,DMS_Label=dms.lab,DM_label=dm.lab,S_label=s.lab)
      return(dat)
    } # end if(format=='deg.min.sec')
  
} # end function

