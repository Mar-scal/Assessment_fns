####  Commented and checked by DK starting on July 27, 2015. This function is used to grab the hydration sample data
####  This is the meat weight shell height data from before 2010.  This is being loaded into the SQL database, 
####  currently (May 2016) the SQL database goes back to 2002 so this is just needed for MW-SH data before this.
# Update history
#Commented, checked  and revised by DK May 16, 2016
# Sept 2016 DK revised to eliminate get.new.hyd option in the function as the data from 2009 and 2010 is now found in our SQL database.
# Jan 2021 - direct_fns option changed to go to github by default...
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
##
###############################################################################################################

# Arguments
# Bank: 	    Which banks do we want to obtain samples from.  Defaults to all of the below options:
#	                c("GB","GBa","GBb","BBn","BBs","Ger","Sab","Mid","Ban","SP")
# yrs: 	      choose years of interest, default is 1981-2008  Data exists for 1981-2010.
# mths: 	    choose months of interest.  Default is 1-12, any combination of numerical months works.
# path: 	    location of the files, default is Y:/Data/Hydration/
#	export:     Export the results to a flat file.  (T/F) default is T
# dirt:       Directory to grab data from.  Default ="Y:/Offshore scallop/Assessment/Assessment_fns/"
# direct_fns: Where you are sourcing your functions from.  Default is missing which goes to github.
# source("Y:/Assessment/2010/r/fn/import.hyd.data.r")

import.hyd.data <- function(Bank=c("GB","GBa","GBb","BBn","BBs","Ger","Sab","Mid","Ban","SP"),yrs=1981:2008, 
                            mths=1:12,export=T,dirt="Y:/Offshore scallop/Assessment/",direct_fns)
{
	require(splancs)  || stop("You need to install the splancs package!")

  
  ### DK:  I believe I need this, but maybe not? Now defaults to looking at Github if not specified.
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
} # end for(un in funs)
  } # end  if(missing(direct_fns))

  #source(paste(direct_fns,"Survey_and_OSAC/get.new.hyd.r",sep=""))
  if(!missing(direct_fns)) source(paste(direct_fns,"Survey_and_OSAC/convert.dd.dddd.r",sep=""))
  
  # Get's the data for the years before 2009.
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
  		wgt.dat<-data.frame(weight.dat[,1:6],lon,lat,weight.dat[,9:18])
  	
  		# Rename the new object.
  		names(wgt.dat) <- c('bank','cruise','tow','day','month','year','lon','lat','depth',
  		                        'scalnum','sex','mat','sh','wmw','wgw','dmw','dgw','wspw')
  	
  		# Delinate the boundaries of GB, BBs, and BBn.
  		GBarea.xy<-data.frame(lon=c(-67.30935,-66.754167,-66,-65.666667,-65.666667,-66.16895,-67.30935),
  		                      lat=c(42.333333,42.333333,42.333333,42,41.583333,41,42.333333))
  		BBs.bounding.xy <- data.frame(lon = c(-65.62486, -65.975, -65.5,-64.4, -64.5, -65.62486), 
  		                              lat = c(43, 42.308333, 42.2,42.8, 43.2, 43))
  		BBn.bounding.xy <- data.frame(lon=c(-65.62486,-65.975,-66.7,-65.62486),lat=c(43,42.308333,42.9,43))
  	
  		# If any lat/lon data is an NA, make those 0's, needed for the next call
  		wgt.dat$lon[is.na(wgt.dat$lon)]<-0
  		wgt.dat$lat[is.na(wgt.dat$lat)]<-0
  		
  		# Using the splancs lovely inout function determine which banks this data is located in.
  		# Notice that for GB we subseted the areas so we can have GBa and GBb in bounding box.
  		wgt.dat$bank[with(wgt.dat, inout(cbind(lon,lat), GBarea.xy[-(3:4),], bound = T))]<-"Georges a"
  		wgt.dat$bank[with(wgt.dat, inout(cbind(lon,lat), GBarea.xy[c(2:5,2),], bound = T))]<-"Georges b"
  		wgt.dat$bank[with(wgt.dat, inout(cbind(lon,lat), BBs.bounding.xy, bound = T))]<-"Browns South"
  		wgt.dat$bank[with(wgt.dat, inout(cbind(lon,lat), BBn.bounding.xy, bound = T))]<-"Browns North"
  		
  		# Reset the lat/lon 0's back to NA as they should be.
  		wgt.dat$lon[wgt.dat$lon==0]<-NA
  		wgt.dat$lat[wgt.dat$lon==0]<-NA
  		
  		# Rename all the banks to the shorthand we use throughout.
  		wgt.dat$bank[wgt.dat$bank=="Georges"]<-"GB" 
  		wgt.dat$bank[wgt.dat$bank=="Georges a"]<-"GBa" 
  		wgt.dat$bank[wgt.dat$bank=="Georges b"]<-"GBb" 
  		wgt.dat$bank[wgt.dat$bank=="Browns North"]<-"BBn" 
  		wgt.dat$bank[wgt.dat$bank=="Browns South"]<-"BBs" 
  		wgt.dat$bank[wgt.dat$bank=="German"]<-"Ger" 
  		wgt.dat$bank[wgt.dat$bank=="Sable"]<-"Sab" 
  		wgt.dat$bank[wgt.dat$bank=="Middle"]<-"Mid" 
  		wgt.dat$bank[wgt.dat$bank=="Banquereau"]<-"Ban" 
  		wgt.dat$bank[wgt.dat$bank=="St. Pierre"]<-"SP" 
  		
	  } # end if(sum(yrs<2009)>0)


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





