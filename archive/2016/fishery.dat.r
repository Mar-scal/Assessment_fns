####  Commented and checked by DK starting on July 28, 2015.  This function is used to select the CPUE data that 
# we want use tp calculate annual CPUE's and s.e.'s

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#   1:/r/fn/jackknife.r
#      
##
###############################################################################################################

# Arguments
# fishery.data,
#yr: 1955:2008,
#bk: 'GBa',
#NAFO: bk,
#Gin: F,
#export: F,
#method: 'avgdaily' or jackknife
#model.out: F,
#period:  Are we looking at the calendar year or the survey year.  Calendar year = 'calyr' while survey year ='survyr'
#surv: 'Aug' or May




# source("Y:/Assessment/2011/r/fn/fishery.dat.r")

fishery.dat<-function(fishery.data,yr=1955:2008,bk='GBa',NAFO=bk,Gin=F,export=F,method='avgdaily',model.out=F,period='calyr',surv='Aug')
{
		

	require(splancs) || stop("This won't work unless you install splancs... thanks") 

  # Add a column to the fishery.data called year1 which will house the original year.
	fishery.data$year1<-fishery.data$year
	
	# If we are looking at the "survey" year then we need to divide up the year into pre/post survey
	if(period=='survyr')
	  {
	
  		if(surv=='May')
  		  {
  		  	presurv.mon<-c("January","February","March","April","May")
  			  postsurv.mon<-c("June","July","August","September","October","November","December")
  		  } # end if(surv=='May')
  		
  		if(surv=='Aug')
  		  {
  			  presurv.mon<-c("January","February","March","April","May","June","July","August")
  			  postsurv.mon<-c("September","October","November","December")
  		  } # ened if(surv=='Aug')
  		
	    # So this takes any data in the post survey months (which depends on the survey chosen) and adds 1 to the year
  		fishery.data$year[months(as.Date(fishery.data$date))%in%postsurv.mon] <- fishery.data$year[months(as.Date(fishery.data$date)) %in% 
  		                                                                                                                  postsurv.mon]+1
  		# Determine the catch after the survey.  Pick the post survey months, and the last year, on the bank of interest, add up all the catchs (in kg)
  		post.surv.catch<-sum(fishery.data$pro.repwt[months(as.Date(fishery.data$date)) %in% postsurv.mon & 
  		                                              fishery.data$year==max(yr) & fishery.data$bank %in% bk],na.rm=T)
	  } # end if(period=='survyr')
  
	# Now subset the fishery data for the years of interest, if using the survyr this drops all of the final year post survey data from data 
	#(which is o.k. as we are missing the pre survey information.
	fishery.data<-subset(fishery.data,year %in% yr)
	
	# if the NAFO region is not the same as the bank subset the fishery data by NAFO region
	if(NAFO != bk) fishery.data<-subset(fishery.data,nafo %in% NAFO)
	
	# Calculate the annual catch on a given bank.
	catch<-with(subset(fishery.data,bank %in% bk & year %in% yr),tapply(pro.repwt,year,sum,na.rm=T))/1000
	# Calculate the annual effort on a given bank.
	effort<-with(subset(fishery.data,bank %in% bk & year %in% yr),tapply(hm,year,sum,na.rm=T))
	# Put the catch data into a new dataframe
	catch.dat<-data.frame(year=as.numeric(dimnames(catch)[[1]]),catch,effort)
	

	
	# If we want GBa or GBb, well they didn't exist before 1999 so we need to determine these for old data
	if(bk=="GBa" && min(yr) < 1999 || bk=="GBb" && min(yr) < 1999)
	  {
  		# Identify the GB boundary
  	  GBarea.xy<-data.frame(lon=c(-67.30935,-66.754167,-66,-65.666667,-65.666667,-66.16895,-67.30935),lat=c(42.333333,42.333333,42.333333,42,41.583333,41,42.333333))
  		# GBa is this
  	  GBaArea.xy<-GBarea.xy[-(3:4),]
  		# While GBb is this.
  	  GBbArea.xy<-GBarea.xy[c(2:5,2),]
  		
  	  # get the catch for all of Georges Bank, this may/may not be the survey year data
  		GBCatch<-with(subset(fishery.data,bank=="GB" & year<1999),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		# repeat with year1, this is the calander year data for sure
  		GBCatch1<-with(subset(fishery.data,bank=="GB"& year<1999),tapply(pro.repwt,year1,sum,na.rm=T))/1000
  		
  		# Get the effort, this is for the calendar year data
  		GBEffort<-with(subset(fishery.data,bank=="GB"&year<1999),tapply(hm,year,sum,na.rm=T))
  		# This outlines where GBa is
  		fishery.data$bank[fishery.data$lon < 0 & fishery.data$lat > 0 & !is.na(fishery.data$lon) & !is.na(fishery.data$lat) &
  		                    fishery.data$year < 1999][with(subset(fishery.data,lon < 0 & lat > 0 & !is.na(lon) & !is.na(lat) & year < 1999), 
  		                                                 inout(cbind(lon,lat), GBaArea.xy, bound = T))]<-"GBa"
  		# This outlines where GBb is
  		fishery.data$bank[fishery.data$lon < 0 & fishery.data$lat > 0 & !is.na(fishery.data$lon) & !is.na(fishery.data$lat) &
  		                    fishery.data$year < 1999][with(subset(fishery.data,lon < 0 & lat > 0 & !is.na(lon) & !is.na(lat) & year<1999), 
  		                                                 inout(cbind(lon,lat), GBbArea.xy, bound = T))]<-"GBb"
  		# Now we can I.D. the catch in GBa and GBb before 1999
  		GBbCatch<-with(subset(fishery.data,bank =='GBb' & year < 1999),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		GBaCatch<-with(subset(fishery.data,bank =='GBa' & year < 1999),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		
  		# And the effort for GBa and GBb before 1999
  		GBbEffort<-with(subset(fishery.data,bank == 'GBb' & year < 1999),tapply(hm,year,sum,na.rm=T))
  		GBaEffort<-with(subset(fishery.data,bank == 'GBa' & year < 1999),tapply(hm,year,sum,na.rm=T))
  		
  		propCatch.dat<-merge(data.frame(year=as.numeric(dimnames(GBCatch)[[1]]),GB=GBCatch),
  		                     merge(data.frame(year=as.numeric(dimnames(GBaCatch)[[1]]),GBa=GBaCatch),
  		                           data.frame(year=as.numeric(dimnames(GBbCatch)[[1]]),GBb=GBbCatch),all=T),all=T)
  		propCatch.dat[is.na(propCatch.dat)]<-0
  		
  		propEffort.dat<-merge(data.frame(year=as.numeric(dimnames(GBEffort)[[1]]),GB=GBEffort),
  		                      merge(data.frame(year=as.numeric(dimnames(GBaEffort)[[1]]),GBa=GBaEffort),
  		                            data.frame(year=as.numeric(dimnames(GBbEffort)[[1]]),GBb=GBbEffort),all=T),all=T)
  		
  		propEffort.dat[is.na(propEffort.dat)]<-0
  		
  		prop.dat<-data.frame(year=propCatch.dat$year,catch=propCatch.dat$GB*unlist(propCatch.dat[bk]/rowSums(propCatch.dat[,3:4])),
  		                     effort=propEffort.dat$GB*unlist(propEffort.dat[bk]/rowSums(propEffort.dat[,3:4])))
  		
  		catch.dat<-rbind(prop.dat,catch.dat)
		
	  }# end if(bk=="GBa" && min(yr) < 1999 || bk=="GBb" && min(yr) < 1999)
	
	
	# If we want Browns Bank division N/S before 1999, well they didn't exist before 1999 so we need to determine these for old data
	if(bk=="BBn"||bk=="BBs")
	  {
  		BBsArea.xy <- data.frame(lon = c(-65.62486, -65.975, -65.5,-64.4, -64.5, -65.62486), lat = c(43, 42.308333, 42.2,42.8, 43.2, 43))
  		BBnArea.xy <- data.frame(lon=c(-65.62486,-65.975,-66.7,-65.62486),lat=c(43,42.308333,42.9,43))
  		
  		BBCatch<-with(subset(fishery.data,bank=="BB"&year<1999),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		BBEffort<-with(subset(fishery.data,bank=="BB"&year<1999),tapply(hm,year,sum,na.rm=T))
  		
  		fishery.data$bank[fishery.data$lon<0&fishery.data$lat>0&!is.na(fishery.data$lon)&!is.na(fishery.data$lat)&fishery.data$year<1999][with(subset(fishery.data,lon<0&lat>0&!is.na(lon)&!is.na(lat)&year<1999), inout(cbind(lon,lat), BBnArea.xy, bound = T))]<-"BBn"
  		fishery.data$bank[fishery.data$lon<0&fishery.data$lat>0&!is.na(fishery.data$lon)&!is.na(fishery.data$lat)&fishery.data$year<1999][with(subset(fishery.data,lon<0&lat>0&!is.na(lon)&!is.na(lat)&year<1999), inout(cbind(lon,lat), BBsArea.xy, bound = T))]<-"BBs"
  		
  		BBsCatch<-with(subset(fishery.data,bank=='BBs'&year<1999),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		BBnCatch<-with(subset(fishery.data,bank=='BBn'&year<1999),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		
  		BBsEffort<-with(subset(fishery.data,bank=='BBs'&year<1999),tapply(hm,year,sum,na.rm=T))
  		BBnEffort<-with(subset(fishery.data,bank=='BBn'&year<1999),tapply(hm,year,sum,na.rm=T))
  		
  		propCatch.dat<-merge(data.frame(year=as.numeric(dimnames(BBCatch)[[1]]),BB=BBCatch),merge(data.frame(year=as.numeric(dimnames(BBnCatch)[[1]]),BBn=BBnCatch),data.frame(year=as.numeric(dimnames(BBsCatch)[[1]]),BBs=BBsCatch),all=T),all=T)
  		propCatch.dat[is.na(propCatch.dat)]<-0
  		
  		propEffort.dat<-merge(data.frame(year=as.numeric(dimnames(BBEffort)[[1]]),BB=BBEffort),merge(data.frame(year=as.numeric(dimnames(BBnEffort)[[1]]),BBn=BBnEffort),data.frame(year=as.numeric(dimnames(BBsEffort)[[1]]),BBs=BBsEffort),all=T),all=T)
  		propEffort.dat[is.na(propEffort.dat)]<-0
  		
  		prop.dat<-data.frame(year=propCatch.dat$year,catch=propCatch.dat$BB*unlist(propCatch.dat[bk]/rowSums(propCatch.dat[,3:4])),effort=propEffort.dat$BB*unlist(propEffort.dat[bk]/rowSums(propEffort.dat[,3:4])))
  		
  		catch.dat<-rbind(prop.dat,catch.dat)
	  } # end if(bk=="BBn"||bk=="BBs")
		
		
	
	if(method=='avgdaily'){
		cpue<-with(subset(fishery.data,bank%in%bk&year%in%yr&datclass==1),tapply(kg.hm,year,mean,na.rm=T))
		cpue.dat<-data.frame(year=as.numeric(dimnames(cpue)[[1]]),cpue)
	}
	if(method=='jackknife'){
	  
	  #Source1
		source("Y:/Offshore scallop/Assessment/2014/r/fn/jackknife.r",local=T)
		jack.dat<-with(subset(fishery.data,bank%in%bk&year%in%yr&datclass==1),data.frame(year=year,catch=pro.repwt,effort=hm))
		cpue.dat<-jackknife(jack.dat,err='sd')[,-(2:4)]
	}

	total<-merge(data.frame(year=yr),merge(catch.dat,cpue.dat,all=T),all=T)

	if(Gin==T){
	  
	  #Read1
		catch.gin<-read.table("Y:/Data/Fishery/gbcatch.txt",header=T)
		if(period=='survyr'){
			catch.gin$Canada[catch.gin$Year<1999]<-GBCatch/GBCatch1*catch.gin$Canada[catch.gin$Year<1999]
			catch.gin$U.S.A.[catch.gin$Year<1999]<-GBCatch/GBCatch1*catch.gin$U.S.A.[catch.gin$Year<1999]
		}
		if(bk=="GBa"||bk=="GBb")total$catch[total$year<1999]<-catch.gin$Canada[catch.gin$Year<1999]*unlist(propCatch.dat[bk]/rowSums(propCatch.dat[,3:4]))+catch.gin$U.S.A.[catch.gin$Year<1999]
		if(bk=="GB")total$catch[total$year<1999]<-catch.gin$Canada[catch.gin$Year<1999]+catch.gin$U.S.A.[catch.gin$Year<1999]
	}

	#Write1
	if(export==T) write.table(total, file ="data/FisherySummary.txt",sep="\t", row.names = F, col.names = T)
	total$effort[is.na(total$effort)]<-0
	out<-total
	if(model.out)out<-data.frame(year=yr,C=as.vector(total$catch),E=as.vector(total$effort),U=total$cpue,U.cv=sqrt(total$cpue.var)/total$cpue)
	
	if(period=='survyr')
	  {
		  print("Post-survey catch")
		  print(post.surv.catch/1000)
	  } # end if(period=='survyr')
	
	
	return(out)

} # end function fishery.dat

	