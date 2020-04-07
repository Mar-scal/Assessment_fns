####  Commented and checked by DK starting on July 27, 2015. This function is used to grab the hydration sample data
####  The hydration data has not been collected since 2010, this function has been checked and tested in case we 
#### start collecting this data again.
# Update history
#Commented, checked  and revised by DK March 31, 2016
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
#      1: fn/convert.dd.dddd.r
#      2: fn/get.new.hyd.r
##
###############################################################################################################

# Arguments
# Bank: 	Which banks do we want to obtain samples from.  Defaults to all of the below options:
#	c("GB","GBa","GBb","BBn","BBs","Ger","Sab","Mid","Ban","SP")
# yrs: 	choose years of interest, default is 1981-2009.  Data exists for 1981-2010.
# mths: 	choose months of interest.  Default is 1-12, any combination of numerical months works.
# path: 	location of the files, default is Y:/Data/Hydration/
#	export: Export the results to a flat file.  (T/F) default is T
# dirt:   Directory to grab data from.  Default ="Y:/Offshore scallop/Assessment/Assessment_fns/"

# source("Y:/Assessment/2010/r/fn/import.hyd.data.r")

import.hyd.data <- function(Bank=c("GB","GBa","GBb","BBn","BBs","Ger","Sab","Mid","Ban","SP"),yrs=1981:2009, 
                            mths=1:12,export=T,dirt="Y:/Offshore scallop/Assessment/")
{
	
	require(splancs)  || stop("You need to install the splancs package!")

	#Source1
	#source("Assessment_fns/Survey/convert.dd.dddd.r")
  #Source2
  source(paste(direct_fns,"Survey_and_OSAC/get.new.hyd.r",sep=""))
  source(paste(direct_fns,"Survey_and_OSAC/convert.dd.dddd.r",sep=""))
  
  # After 2008 we use source("Assessment_fns/Survey/get.new.hyd.r") to obtain the "new" hydration data, which... 
  # isn't really new anymore... stopped in 2010
	if(sum(yrs>2008)>0)
	  {
	  	#Source2 source("Assessment_fns/Survey/get.new.hyd.r"), bring in the data from the appropriate year/area/cruise combination.Note 
  	  # this is a pretty clunky wat to do this, but given there are only 2 years of data to grab might as well...
  	  new1.wgt.dat<-get.new.hyd(year=2009, cruise="TE07", bank=c("GBa","GBb"),dirc=dirt)
  		new2.wgt.dat<-get.new.hyd(year=2009, cruise="TE04",dirc=dirt)
  		new3.wgt.dat<-get.new.hyd(year=2010, cruise="TE08", bank=c("BBn","Sab","GB","Ger"),dirc=dirt)
  		new4.wgt.dat <- get.new.hyd(year=2010, cruise="TE10", bank=c("GBa","GBb"),dirc=dirt)
  		# Stitch the data together.
  		new.wgt.dat<-rbind(new1.wgt.dat,new2.wgt.dat,new3.wgt.dat,new4.wgt.dat)
	  }# End if sum(yrs > 2008) > 0
	
  
  # 
	if(sum(yrs<2009)>0)
	  {
	    # subset the years to before 2009
  		old.yrs<- yrs[yrs<2009]
  
  		#Read1 bring in the 1982-1992 data
  		weight1.dat <- read.csv(paste(dirt,"Data/Hydration/Weights_82-92.csv",sep=""),stringsAsFactors = F)
  		
  		#Read2 bring in the 1992-2008 data
  		weight2.dat <- read.csv(paste(dirt,"Data/Hydration/Weights_92-08.csv",sep=""),stringsAsFactors = F)
  	
  		# Merge the data together, keep everything.
  		weight.dat<-data.frame(merge(weight1.dat,weight2.dat,all=T))
  		
  		#Source1 source("Assessment_fns/Survey/convert.dd.dddd.r") Convert the lat/lon to decimal degrees
  		lon<-convert.dd.dddd(weight.dat$Long.DDMM.MM)
  		lat<-convert.dd.dddd(weight.dat$Lat.DDMM.MM)
  		# Grab a subset of the data, not a big fan of grabbing by column numbers, but these files aren't changing so it works
  		old.wgt.dat<-data.frame(weight.dat[,1:6],lon,lat,weight.dat[,9:18])
  	
  		# Rename the new object.
  		names(old.wgt.dat) <- c('bank','cruise','tow','day','month','year','lon','lat','depth',
  		                        'scalnum','sex','mat','sh','wmw','wgw','dmw','dgw','wspw')
  	
  		# Delinate the boundaries of GB, BBs, and BBn.
  		GBarea.xy<-data.frame(lon=c(-67.30935,-66.754167,-66,-65.666667,-65.666667,-66.16895,-67.30935),
  		                      lat=c(42.333333,42.333333,42.333333,42,41.583333,41,42.333333))
  		BBs.bounding.xy <- data.frame(lon = c(-65.62486, -65.975, -65.5,-64.4, -64.5, -65.62486), 
  		                              lat = c(43, 42.308333, 42.2,42.8, 43.2, 43))
  		BBn.bounding.xy <- data.frame(lon=c(-65.62486,-65.975,-66.7,-65.62486),lat=c(43,42.308333,42.9,43))
  	
  		# If any lat/lon data is an NA, make those 0's, needed for the next call
  		old.wgt.dat$lon[is.na(old.wgt.dat$lon)]<-0
  		old.wgt.dat$lat[is.na(old.wgt.dat$lat)]<-0
  		
  		# Using the splancs lovely inout function determine which banks this data is located in.
  		# Notice that for GB we subseted the areas so we can have GBa and GBb in bounding box.
  		old.wgt.dat$bank[with(old.wgt.dat, inout(cbind(lon,lat), GBarea.xy[-(3:4),], bound = T))]<-"Georges a"
  		old.wgt.dat$bank[with(old.wgt.dat, inout(cbind(lon,lat), GBarea.xy[c(2:5,2),], bound = T))]<-"Georges b"
  		old.wgt.dat$bank[with(old.wgt.dat, inout(cbind(lon,lat), BBs.bounding.xy, bound = T))]<-"Browns South"
  		old.wgt.dat$bank[with(old.wgt.dat, inout(cbind(lon,lat), BBn.bounding.xy, bound = T))]<-"Browns North"
  		
  		# Reset the lat/lon 0's back to NA as they should be.
  		old.wgt.dat$lon[old.wgt.dat$lon==0]<-NA
  		old.wgt.dat$lat[old.wgt.dat$lon==0]<-NA
  		
  		# Rename all the banks to the shorthand we use throughout.
  		old.wgt.dat$bank[old.wgt.dat$bank=="Georges"]<-"GB" 
  		old.wgt.dat$bank[old.wgt.dat$bank=="Georges a"]<-"GBa" 
  		old.wgt.dat$bank[old.wgt.dat$bank=="Georges b"]<-"GBb" 
  		old.wgt.dat$bank[old.wgt.dat$bank=="Browns North"]<-"BBn" 
  		old.wgt.dat$bank[old.wgt.dat$bank=="Browns South"]<-"BBs" 
  		old.wgt.dat$bank[old.wgt.dat$bank=="German"]<-"Ger" 
  		old.wgt.dat$bank[old.wgt.dat$bank=="Sable"]<-"Sab" 
  		old.wgt.dat$bank[old.wgt.dat$bank=="Middle"]<-"Mid" 
  		old.wgt.dat$bank[old.wgt.dat$bank=="Banquereau"]<-"Ban" 
  		old.wgt.dat$bank[old.wgt.dat$bank=="St. Pierre"]<-"SP" 
  		
	  } # end if(sum(yrs<2009)>0)
	
  # If we don't have data from 2009 or 2010 the "new.wgt.dat" won't exist.	
  if(max(yrs) < 2009)   wgt.dat <- old.wgt.dat
  if(max(yrs) >= 2009 && min(yrs) < 2009) 	wgt.dat <- merge(old.wgt.dat,new.wgt.dat,all=T)
  if(max(yrs) >= 2009 && min(yrs) >= 2009) 	wgt.dat <- new.wgt.dat
  
  # Combine the old and the new wgt data, incluse everything.
	
	# if BANK or mths are not the defaults this subsets the data into the Bank/month combinations requested.
	wgt.dat<-subset(wgt.dat,bank%in%Bank&year%in%yrs&month%in%mths)
	
	# if we want to make a new table with these data, we export it here.  It appears this was last done in 2011 for BBn from 1983-2010
	# DK Note August 2015:  So there is no reason we couldn't just run this one more time and export everything to a flatfile and 
	# skip all of this unless/until we start sampling again Or until this is all entered into a database.
	if(export) write.table(wgt.dat,file = 
	                         paste(dirt,"Data/Hydration/Weight_Summary/WeightData",Bank,min(yrs),"-",max(yrs),".csv",sep=""),sep=",", 
	                       row.names = F, col.names = T)

	
	wgt.dat
}





