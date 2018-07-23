### This function gets the detailed sampling data for 2009-2010 for some odd in between data, this will be replaced by SQL database in 2016
# Update history
####  Commented and checked by DK starting on July 27, 2015.
#Commented, checked  and revised by DK March 31, 2016

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "import.hyd.data"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#   1:  "convert.dd.dddd.r" (called in above function...)
#      
##
###############################################################################################################

# Arguments
# bank: 	Which banks do we want to obtain samples from.  Defaults to all of the below options:
#	c("GB","BBn","BBs","Ger","Sab")
# year: 	choose year of interest, default is 2009, only can pick one year at a time and 2009-2010 only options that work
# cruise: ID of the cruise from the year chosen
# dirc:   Directory to grab data from.  Default ="Y:/Offshore scallop/Assessment/Assessment_fns/"

# source("Y:/Assessment/2010/r/fn/get.new.hyd.r")

get.new.hyd<-function(year=2009, cruise="TE04", bank=c("GB","BBn","BBs","Ger","Sab"),
                                                       dirc="Y:/Offshore scallop/Assessment/")
  {

  require(splancs)  || stop("You need to install the splancs package!")
  

  # initialize the variable
	hyd.lst<-list(NULL)
	
	# Run a loop to obtain the data from all banks of interest
	for(i in 1:length(bank))
	  {
	    # The summer survey of 2010 is different so needs to be processed on it's own.
	    if(((bank[i] == "GBa" || bank[i] == "GBb")   && year == 2010)==F)
	      {
    	  # Grab the data and make it a list.  Add the Bank information as a new column
      		hyd.lst[[i]]<-read.csv(paste(dirc,"Data/Hydration/Hyd",year,"/dhyd",cruise,bank[i],year,".csv",sep=""))
      		hyd.lst[[i]]$bank<-bank[i]
    	  } # end if(((bank[i] == "GBa" || bank[i] == "GBa")   && year == 2010)==F)
	  
	 
	  } # end for(i in 1:length(bank))
	
	# Grab the data if the summer of 2010 survey is what you want
	if(bank %in% c("GBb","GBa")   && year == 2010)
  	{
  	  hyd.lst[[1]]<-read.table(paste(dirc,"Data/Hydration/Hyd",year,"/TE10GB10mtwt_a.txt",sep=""),sep="\t",header=T)
  	  hyd.lst[[2]]<-read.table(paste(dirc,"Data/Hydration/Hyd",year,"/TE10GB10mtwt_b.txt",sep=""),sep="\t",header=T)
	  } # end if((bank[i] == "GBa" || bank[i] == "GBa")   && year == 2010)
	
	
	# Unpack the list into a dataframe and give the headers the appropriate names.
	hyd.dat<-do.call("rbind",hyd.lst)
	names(hyd.dat) <- c('year','cruise','tow','lat','lon','depth','scalnum','sh','sex','mat','wmw','wgw','wspw','bank')
	
	# Format the dates and day/month/year each it's own column.
	hyd.dat$day<-as.numeric(substr(hyd.dat$year,7,8))
	hyd.dat$month<-as.numeric(substr(hyd.dat$year,5,6))
	hyd.dat$year<-as.numeric(substr(hyd.dat$year,1,4))
	# For the TE10 cruise we need to add the month differently (This is a hack!!)
	hyd.dat$month[hyd.dat$cruise == "TE10"] <- 8
	

	
	# Do the below for any data except this call as it doesn't have the lat/lon data.
	if((bank[1] == "GBa" && bank[2] == "GBb" && year == 2010)==F)
	  {
	    # These weights are on a differernt scale than older data so correct 
	    hyd.dat[,11:13]<-hyd.dat[,11:13]*0.01
    	# Convert the locations into decimal degrees.
    	hyd.dat$lon<-convert.dd.dddd(hyd.dat$lon)*-1
    	hyd.dat$lat<-convert.dd.dddd(hyd.dat$lat)
    	
    	# Delineated Georges banks 
    	GBarea.xy<-data.frame(lon=c(-67.30935,-66.754167,-66,-65.666667,-65.666667,-66.16895,-67.30935),
    	                      lat=c(42.333333,42.333333,42.333333,42,41.583333,41,42.333333))
    	# Set NA's = 0 so the following function works
    	hyd.dat$lon[is.na(hyd.dat$lon)]<-0
    	hyd.dat$lat[is.na(hyd.dat$lat)]<-0
    	# Using splancs inout function determine whether GB data is GBa or GBb.
    	hyd.dat$bank[with(hyd.dat, inout(cbind(lon,lat), GBarea.xy[-(3:4),], bound = T))]<-"GBa"
    	hyd.dat$bank[with(hyd.dat, inout(cbind(lon,lat), GBarea.xy[c(2:5,2),], bound = T))]<-"GBb"
    	# Reset hydration data to NA.
    	hyd.dat$lon[hyd.dat$lon==0]<-NA
    	hyd.dat$lat[hyd.dat$lon==0]<-NA
	  } # end ((bank[1] == "GBa" || bank[2] == "GBb")   && year == 2010)==F
	
	
	
	
	hyd.dat

}
		
#	
	

