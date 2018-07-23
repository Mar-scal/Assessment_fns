####  Commented and checked by DK starting on July 28, 2015.  This function is used to select the CPUE data that 
# we want use tp calculate annual CPUE's and s.e.'s
### Revisions
## 
## May 2017, revised the BBn survyr bit to work if there are no NA's in the data.
# March 2018, revised to GBa section so that the script works when looking at a small portion of the bank (the merge function was broken for GBa when 
# you zoom in and don't have data for both GBa and GBb banks and are looking before 1999...
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

#fishery.data:  Fishery data from logs_and_fishery_data.r. 
#yr:        The years of interest.  Default is to use all years from 1955 to current year.  Note that since this is using output from logs_and_fishery_data.r 
#            ensure that the years in the call to that function include the years used in this function.
#bk:        Which bank to select from using abbreviated bank id.  Default is GBa (Georges Bank)
#           The options are: 
#             "GBa": Georges a 
#             "GBb": Georges b 
#   	        "BBn": Browns north 
#             "BBs": Browns south 
#             "Ger": German 
#       	    "Mid": Middle 
#       	    "Sab": Sable 
#       	    "Ban": Banquereau 
#       	    "SPB": St. Pierre Bank
#nafo.div:   Used if interested in a particular NAFO division within the bank chosen.  This will 
#            always just look at CPUE in NAFO division on the bank chosen. There are currently 42 
#            possible NAFO areas to choose from (any combination of the 42 is acceptable just 
#            ensure they align with the bank):
#Gin:       Use the table gbcatch which includes historic calculations of the actual catch on Georges 
#           Bank. (T/F) default is F.
#export:	  Export the annual CPUE table.  (T/F) default is F
#method:	  How are we calculating the annual CPUE + variance, options are "avgdaily" and
#           "jackknife".  Default option is "avgdaily' which is simply calculating the mean for the year + 
#           ariance using every data point available.  The preferred, yet slower if using large datasets, 
#           option is to use 'jackknife' function based on Smith(1980) to  obtain an unbiased estimate of CPUE and its variance.
#model.out: Specifiy the 'out' object to have a specific format.  (T/F) default is F.
#period:  	Are we looking at the calendar year or the survey year.  Calendar year = 'calyr' while	survey year ='survyr'    
#surv: 	    If period = 'survyr' do you want to split the data using the spring 'May" or summer 
              #"August" survey.
#direct:    Working directory "Y:/Offshore scallop/Assessment/Assessment_fns/"



# source("Y:/Assessment/2011/r/fn/fishery.dat.r")

fishery.dat<-function(fishery.data,yr=1955:as.numeric(format(Sys.Date(),"%Y")),bk='GBa',nafo.div=bk,Gin=F,export=F,
                      method='avgdaily',model.out=F,period='calyr',surv='August',
                      direct = "Y:/Offshore scallop/Assessment/")
{
		
######################################### Section 1: Processing data #####################################################

	require(splancs) || stop("This won't work unless you install splancs... thanks") 
  
  # Only needed if method = "jackknife"
  if(method == "jackknife") source(paste(direct,"Assessment_fns/Fishery/jackknife.r",sep=""),local=T)
  
  # Add a column to the fishery.data called year.act which will house the original year.
	fishery.data$year.act<-fishery.data$year
	# Before adjusting the dates in  the survey option below we may need to calculate the Catch on GB 
	# (this is only useful for data before 1999)
  if(Gin == T) 
    {
      GBCatch.cyear<-with(subset(fishery.data,bank_old== "GB"),tapply(pro.repwt,year,sum,na.rm=T))/1000
      GBCatch.cyear <- data.frame(year=dimnames(GBCatch.cyear)[[1]],GB=GBCatch.cyear,stringsAsFactors = F)
    } # end if(Gin == T) 
	
	# If we are looking at the "survey" year then we need to divide up the year into pre/post survey
	if(period=='survyr')
	  {
	  month <- c("January","February","March","April","May","June","July","August","September","October","November","December")
	  presurv.mon<-month[1:which(month==surv)]
  	postsurv.mon<-month[(which(month==surv)+1):12]
  			  
	    # So this takes any data in the post survey months (which depends on the survey chosen) and adds 1 to the year
  		fishery.data$year[months(as.Date(fishery.data$date)) %in% postsurv.mon] <- 
  		                  fishery.data$year[months(as.Date(fishery.data$date)) %in% postsurv.mon]+1
  		                                                                                                                  
  		# Determine the catch after the survey from the year before the last (the year = max(yr) picks the previous years survey since year was updated by 1 above.
  		# This is simply printed to screen at the end of the run so does not seems especially important.
  		post.surv.catch<-sum(fishery.data$pro.repwt[months(as.Date(fishery.data$date)) %in% postsurv.mon & 
  		                                              fishery.data$year== max(yr) & fishery.data$bank %in% bk],na.rm=T)
  		
  		# Now subset the fishery data for the years of interest, again if using survey year the post survey months are set to the following year, meaning the 
  		# last year in the yr is really looking at the previous calander years data.  Here we can drop the first year data as well since we
  		# only have that for the pre-survey, it's incomplete thus we shouldn't be using it...
  		fishery.data<-subset(fishery.data,year %in% yr[-1])
	  } # end if(period=='survyr')
  
	# if the NAFO region is not the same as the bank subset the fishery data by NAFO region
	if(nafo.div != bk) fishery.data<-subset(fishery.data,nafo %in% nafo.div)
	#########################################  End Section 1: Processing and subdividing data #####################################################
	
	
	######################################### Section 2: Calculating Catch and Effort for specific Banks  #####################################################
	
	
	# Calculate the annual catch on a given bank.
	catch<-with(subset(fishery.data,bank %in% bk & year %in% yr),tapply(pro.repwt,year,sum,na.rm=T))/1000
	# Calculate the annual effort on a given bank, notice this is in HOUR-METERS
	effort<-with(subset(fishery.data,bank %in% bk & year %in% yr),tapply(hm,year,sum,na.rm=T))
	# Put the catch data into a new dataframe
	catch.dat<-data.frame(year = as.numeric(dimnames(catch)[[1]]),
	                      catch = as.numeric(catch),effort = as.numeric(effort),stringsAsFactors = F)
	
	# For GBa and GBb these didn't exist before 1999, so we will also calculate the catch on the whole bank. along with 
	# the GBa and GBb regions estimated from their co-ordinates (this was done in logs_and_fishery_data function)
	# Given the incomplete data and fishing in what are now US waters (before 1977) in the old data the Catch/Effort on GB 
	# will not be the sum of GBa and GBb.
	
	if((bk=="GBa" || bk=="GBb") && min(yr) < 1999)
	  {

  	  # get the catch for all of Georges Bank, this may/may not be the survey year data, note we use the 
	    # bank_old here as this contains the information on catch locations from the time of the log info.
	    # Note this was being done wrong when looking at survey years, it was subsetting on year rather
	    # than year.act, indeed this subset is pointless with the move to bank_old.
	    # If this is survey year, the year represents the pre-survey data from that year combined with post survey data of previous year.
	    # DK Note Sept 2015:  Also we should note the first year data from this method is garbage as it doesn't include the
	    # post survey year data from the year previous when using period=survyr
  		GBCatch<-with(subset(fishery.data,bank_old== "GB"),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		GBCatch <- data.frame(year=dimnames(GBCatch)[[1]],GB=GBCatch,stringsAsFactors = F)
  		
  		# Get the effort, this may/may not be the survey year data. note we use the 
  		# bank_old here as this contains the information on catch locations from the time of the log info.
  		GBEffort<-with(subset(fishery.data,bank_old=="GB"),tapply(hm,year,sum,na.rm=T))
  		GBEffort <- data.frame(year=dimnames(GBEffort)[[1]],GB=GBEffort,stringsAsFactors = F)
  		
  		# For the above when looking at the 1998 post survey data we need to do some funny stick handling as the 1999 pre-survey data will be coded GBa and GBb
  		if(period == "survyr")
  		  {
  		    # in case there are no NA's we need this if statement to make sure we get some data!
  		    if(length(which(is.na(fishery.data$bank))) == 0) fd <- fishery.data
  		    if(length(which(is.na(fishery.data$bank))) > 0)  fd <- fishery.data[-which(is.na(fishery.data$bank)),]
  		    dat.1999 <- fd[fd$year == 1999 & (fd$bank == "GBa" | fd$bank == "GBb"),]
  		    GB.C.1999 <- with(dat.1999,tapply(pro.repwt,year,sum,na.rm=T))/1000
  		    GB.E.1999 <- with(dat.1999,tapply(hm,year,sum,na.rm=T))
  		    GBEffort$GB[GBEffort$year == 1999] <- GB.E.1999
  		    GBCatch$GB[GBCatch$year == 1999] <- GB.C.1999
  		  }# end if(period == "survyr")
  		  
  		# Now we can I.D. the catch in GBa and GBb, the old code really screws up the data in the 1998-1999 transition
  		# when using survyr, I hope we never used it for anything important...
  		GBbCatch<- with(subset(fishery.data,bank =='GBb'),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		GBbCatch <- data.frame(year = dimnames(GBbCatch)[[1]],GBb=GBbCatch,stringsAsFactors = F)
  		GBaCatch<-with(subset(fishery.data,bank =='GBa'),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		GBaCatch <- data.frame(year = dimnames(GBaCatch)[[1]],GBa=GBaCatch,stringsAsFactors = F)
  	
  		
  		# And the effort for GBa and GBb before 1999
  		GBbEffort<-with(subset(fishery.data,bank == 'GBb'),tapply(hm,year,sum,na.rm=T))
  		GBaEffort<-with(subset(fishery.data,bank == 'GBa'),tapply(hm,year,sum,na.rm=T))
  		GBbEffort <- data.frame(year = dimnames(GBbEffort)[[1]],GBb=GBbEffort,stringsAsFactors = F)
  		GBaEffort <- data.frame(year = dimnames(GBaEffort)[[1]],GBa=GBaEffort,stringsAsFactors = F)
  		# Put these together into one object
  		
  		# If we have data for GBa and for GBb then do this...
  		if(nrow(GBaCatch) > 0 && nrow(GBbCatch) > 0)
  		{
  		# Put these together into one object
  		GBsCatch <- merge(GBbCatch,GBaCatch,all=T)
  		GBsEffort <- merge(GBbEffort,GBaEffort,all=T)
  		}
  		
  		# If we don't have GBa data than do this...
  		if(nrow(GBaCatch) == 0)
  		{
  		  # Make the GBs thingy 
  		  GBsCatch <- GBbCatch
  		  GBsEffort <- GBbEffort
  		}
  		# If we don't have GBb data than do this... not that this might look a little backwards!
  		if(nrow(GBbCatch) == 0)
  		{
  		  # Make the GBs thingy 
  		  GBsCatch <- GBaCatch
  		  GBsEffort <- GBaEffort
  		}
  		
  		# Now put all the old GB catch data together and make NA's = 0
  		propCatch.dat<-merge(GBCatch,GBsCatch,all=T)
  		propCatch.dat[is.na(propCatch.dat)]<-0
  		# Now put all the old GB effort data together and make NA's = 0
  		propEffort.dat <- merge(GBEffort,GBsEffort,all=T)
  		propEffort.dat[is.na(propEffort.dat)]<-0
  		# These data need trimmed if looking at survey year.
  		if(period == "survyr") 
  		  {
    		  propCatch.dat <- propCatch.dat[propCatch.dat$year < 2000,]
    		  propEffort.dat <- propEffort.dat[propEffort.dat$year < 2000,]
  		  } # end if(period == "survyr")
  		
  		if(period == "calyr")
    		{
    		  propCatch.dat <- propCatch.dat[propCatch.dat$year < 1999,]
    		  propEffort.dat <- propEffort.dat[propEffort.dat$year < 1999,]
    		} # end if(period == "calyr")
  		
  		# This is looking at the proportion of the known catch on a particular bank (GBa or GBb).  
  		# We then takes the total GB catch and assumes that the proportion caught on the sub-bank accounts for the difference between the whole bank
  		# and the amount caught on the sub-bank.
  		# DK September 2015 Note: If using old data this assumption is sketchy as there are approximately 25,000 older entries (especially pre-1977) that
  		# were actually in what are now USA waters.  Between 1981-1999 there are still 2000 entries that are in US waters.
  		# Post 1977 this may not be a big issue, but certainly before 1977 this isn't right.  Something to think about here as
  		# I think this is a pretty strong assumption and we should ensure it is documented clearly.
  		if( nrow(GBaCatch) > 0 && nrow(GBbCatch) > 0) prop.dat<-data.frame(year=as.numeric(propCatch.dat$year),
  		                                                              catch=as.numeric(propCatch.dat$GB*unlist(propCatch.dat[bk]/rowSums(propCatch.dat[,3:4]))),
  		                                                              effort=as.numeric(propEffort.dat$GB*unlist(propEffort.dat[bk]/rowSums(propEffort.dat[,3:4]))),
  		                                                              stringsAsFactors = F)
  		# In this case all the catch is from one bank so it is easy....
  		if( nrow(GBaCatch) == 0 || nrow(GBbCatch) == 0) prop.dat<-data.frame(year=as.numeric(propCatch.dat$year),
  		                                                               catch=as.numeric(propCatch.dat[,3]),
  		                                                               effort=as.numeric(propEffort.dat[,3]),
  		                                                                stringsAsFactors = F)
  		
  		
  		# Put this proportional data together with the catch data after 1998, before this time the data isn't correct for GBa/GBb
  		if(period == "calyr") catch.dat<-rbind(prop.dat,catch.dat[catch.dat$year>=1999,])
  		if(period == "survyr") catch.dat<-rbind(prop.dat,catch.dat[catch.dat$year>=2000,])
	  }# end if(bk=="GBa" && min(yr) < 1999 || bk=="GBb" && min(yr) < 1999)
	
	
	# If we want Browns Bank division N/S before 1999, well they didn't exist before 1999 so we need to determine these for old data
	if( (bk=="BBn"||bk=="BBs") && min(yr) < 1999)
	  {
  		# get the catch (in mt) and effort on all of Browns Bank. this may/may not be the survey year data, note we use the 
	    # bank_old here as this contains the information on catch locations from the time of the log info.
	    # Note this was being done wrong when looking at survey years, it was subsetting on year rather
	    # than year.act, indeed this subset is pointless with the move to bank_old.
  		BBCatch<-with(subset(fishery.data,bank_old=="BB"), tapply(pro.repwt,year,sum,na.rm=T))/1000
  		BBEffort<-with(subset(fishery.data,bank_old=="BB"), tapply(hm,year,sum,na.rm=T))
  		BBCatch <- data.frame(year=dimnames(BBCatch)[[1]],BB=BBCatch,stringsAsFactors = F)
  		BBEffort <- data.frame(year=dimnames(BBEffort)[[1]],BB=BBEffort,stringsAsFactors = F)
  		
  		if(period == "survyr")
  		{
  		  if(length(which(is.na(fishery.data$bank) ==T)) == 0) fd <- fishery.data
  		  if(length(which(is.na(fishery.data$bank) ==T)) > 0) fd <- fishery.data[-which(is.na(fishery.data$bank)),]
  		  dat.1999 <- fd[fd$year == 1999 & (fd$bank == "BBs" | fd$bank == "BBn"),]
  		  BB.C.1999 <- with(dat.1999,tapply(pro.repwt,year,sum,na.rm=T))/1000
  		  BB.E.1999 <- with(dat.1999,tapply(hm,year,sum,na.rm=T))
  		  BBEffort$BB[BBEffort$year == 1999] <- BB.E.1999
  		  BBEffort$BB[BBEffort$year == 1999] <- BB.C.1999
  		}# end if(period == "survyr")
  		
  		# Now do the same for BBs and BBn
  		BBsCatch<-with(subset(fishery.data,bank=='BBs'),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		BBnCatch<-with(subset(fishery.data,bank=='BBn'),tapply(pro.repwt,year,sum,na.rm=T))/1000
  		BBsEffort<-with(subset(fishery.data,bank=='BBs'),tapply(hm,year,sum,na.rm=T))
  		BBnEffort<-with(subset(fishery.data,bank=='BBn'),tapply(hm,year,sum,na.rm=T))
  		BBsCatch <- data.frame(year=dimnames(BBsCatch)[[1]],BBs=BBsCatch,stringsAsFactors = F)
  		BBnCatch <- data.frame(year=dimnames(BBnCatch)[[1]],BBn=BBnCatch,stringsAsFactors = F)
  		BBsEffort <- data.frame(year=dimnames(BBsEffort)[[1]],BBs=BBsEffort,stringsAsFactors = F)
  		BBnEffort <- data.frame(year=dimnames(BBnEffort)[[1]],BBn=BBnEffort,stringsAsFactors = F)
  		# Put the sub bank catch or effort together  if applicable
  		if(nrow(BBsCatch) >0 && nrow(BBnCatch) >0)
  		{
    		BBtCatch <- merge(BBsCatch,BBnCatch,all=T)
    		BBtEffort <- merge(BBsEffort,BBnEffort,all=T)
    		
    		# Now a final merge and set all NA's =0
    		propCatch.dat <- merge(BBCatch,BBtCatch,all=T)
    		propCatch.dat[is.na(propCatch.dat)]<-0
  
    		# Now a final merge and set all NA's =0
    		propEffort.dat <- merge(BBEffort,BBtEffort,all=T)
    		propEffort.dat[is.na(propEffort.dat)]<-0
    		
    		# These data need trimmed if looking at survey year.
    		if(period == "survyr") 
    		{
    		  propCatch.dat <- propCatch.dat[propCatch.dat$year < 2000,]
    		  propEffort.dat <- propEffort.dat[propEffort.dat$year < 2000,]
    		} # end if(period == "survyr")
    		
    		if(period == "calyr")
    		{
    		  propCatch.dat <- propCatch.dat[propCatch.dat$year < 1999,]
    		  propEffort.dat <- propEffort.dat[propEffort.dat$year < 1999,]
    		} # end if(period == "calyr")
  
    		
    		# This is looking at the proportion of the known catch on a particular bank (BBn or BBs).  
    		# We then takes the total GB catch and assumes that the proportion caught on the sub-bank accounts for the difference between the whole bank
    		# and the amount caught on the sub-bank.
    		# DK September 2015 Note: If using old data this assumption is sketchy (see GB above for problem there.)
    		# Something to think about here as
    		# I think this is a pretty strong assumption and we should ensure it is documented clearly.
    		prop.dat<-data.frame(year=as.numeric(propCatch.dat$year),
    		                     catch=as.numeric(propCatch.dat$BB*unlist(propCatch.dat[bk]/rowSums(propCatch.dat[,3:4]))),
    		                     effort=as.numeric(propEffort.dat$BB*unlist(propEffort.dat[bk]/rowSums(propEffort.dat[,3:4]))))
    		
    		# Now we can combine the catch data from BBn/BBs from before 1999 with the catch data after 1998
    		if(period == "calyr") catch.dat<-rbind(prop.dat,catch.dat[catch.dat$year>=1999,])
    		if(period == "survyr") catch.dat<-rbind(prop.dat,catch.dat[catch.dat$year>=2000,])
  	  } # end if(bk=="BBn"||bk=="BBs")
	  } # end if(nrow(BBsCatch) >0 && nrow(BBnCatch) >0)
		
	######################################### End Section 2: Calculating CPUE for specific Banks  #####################################################
	
	
	######################################### Section 3: Calculating CPUE and post-processing  #####################################################
	
	
	# The method to calculate the CPUE, this method is a simple mean and is rather different than the jackknife estimator.
	if(method=='avgdaily')
	  {
	    # Select the fishery data on selected bank(s), for all years, and with a data class of 1.  Calculate the 
	    # CPUE in kg/(h-m) for the year using the annual mean value. Then turn this into a dataframe.
  		cpue<-with(subset(fishery.data,bank %in% bk & year %in% yr & datclass==1),tapply(kg.hm,year,mean,na.rm=T))
  		cpue.var <- with(subset(fishery.data,bank %in% bk & year %in% yr & datclass==1),tapply(kg.hm,year,var,na.rm=T))
  		n <- with(subset(fishery.data,bank %in% bk & year %in% yr & datclass==1),tapply(kg.hm,year,function(x) length(na.omit(x))))
  		cpue.dat<-data.frame(year=as.numeric(dimnames(cpue)[[1]]),n,cpue,cpue.var)
  	 } # end if(method=='avgdaily')
	
	# This is the Jackknife method from the grand master Smith(1980)
	if(method=='jackknife')
	  {
	  
  	  # # Select the fishery data on selected bank(s), for all years, and with a data class of 1.  Calculate the 
  	  # CPUE in kg/(h-m) for the year using the estimator from Smith(1980). Then turn this into a dataframe.
  		jack.dat<-with(subset(fishery.data,bank %in% bk & year %in% yr & datclass==1),data.frame(year=year,catch=pro.repwt,effort=hm))
  		#Source1 source("Y:/Offshore scallop/Assessment/2014/r/fn/jackknife.r",local=T)
  		# Look at jackkniffe function along with Smith(1980) to understand what 'se' v.s. 'sd' options really mean
  		cpue.dat<-jackknife(jack.dat)[,-(3:4)]
	  } # end if(method=='jackknife')

	# Put the calculated data with the Catch/effort data.
	total<-merge(data.frame(year=min(catch.dat$year,na.rm=T):max(catch.dat$year,na.rm=T)),merge(catch.dat,cpue.dat,all=T),all=T)

	# Here we bring in a flat file with the historic Georges Bank catch from US and Canada.  We replace any data
	# calculated before 1999 with the catch data from these data and prorate the catch on our bank by the proportion caught on that bank
  # for the data we have.
	if(Gin==T)
	  {
  	  #Read1 Bring in the US vs. Canada table.
  		catch.gin<-read.table(paste(direct,"Data/Offshore/Fishery_data/gbcatch.txt",sep=""),header=T)
  	  # Subset the data to the years we have data for, use the GBCatch.cyear object to pull the years
  		c.gin <- catch.gin[catch.gin$Year %in% GBCatch.cyear$year,]
  		
  		if(period=='survyr')
  		  {
    		  # If we are looking at a "survey year" we take the GBCatch in the survey year and divide by the GBCatch in a calendar year
    		  # to get the corrected Catch values.  DK September 2015 Note:  This is another section in which we should be using
    		  # the GB catch in US waters from the old database if we really want to get this right.  Also it may be brain twisting
  		    # but we use GBCatch from, for example, 1987 (which is combo of late 86 early 87) and match it with CBCatch.cyear of 1986.
  		    # and we use that ratio with the Gin catch data from 1986.
  		    # this is not ideal, but no solution here really is, use the Gin option with caution!
  		  
      		c.gin$Canada <- GBCatch$GB / GBCatch.cyear$GB * c.gin$Canada
    			c.gin$U.S.A. <- GBCatch$GB / GBCatch.cyear$GB * c.gin$U.S.A.
    			catch.gin[catch.gin$Year %in% GBCatch.cyear$year,] <- c.gin
    		} # end if(period=='survyr')
  		
  		  # To use the Gin data for GBa and GBb we take the proportion caught on either GBa or GBb and replace it.
    		if(bk=="GBa"|| bk=="GBb")
    		  {
    		    total$catch[total$year %in% catch.gin$Year] <- c.gin$Canada * unlist(propCatch.dat[bk] / rowSums(propCatch.dat[,3:4])) + c.gin$U.S.A.
    		  } # end   if(bk=="GBa"|| bk=="GBb")
    		
  		  # If looking at Georges bank in whole this replaces our calculation with the Gin data. I haven't removed this entirely
  		  # but to use a GB option would require large changes to the if we want that option I want to make sure we don't lose this bit.
    		#if(bk=="GB") total$catch[catch.gin$Year %in% GBCatch.cyear$year]<- c.gin$Canada + c.gin$U.S.A.
  		
    } # end if(Gin==T)

	# Now there is no more processing to occur so we make 'out', which is the object we return
	out<-total 
	# Remove any NA's from the data, we want total to contain NA's for below export 
	out$effort[is.na(out$effort)]<-0
	
	#Note we are writing the total file year not out as we want the NA's to show up in the Summary.
	# Includes several options to ensure we know what we are exporting when we look at it 3 months later.
	# Could add more if we actually use the Gin data for now I assume that is generally set to false if exporting data.
	if(export==T) 
	  {
	    if(period == "survyr")
	      {
	        #Write1 
	        if(nafo.div != bk) write.table(total, file = paste(direct,"Data/Fishery_data/CPUE/Annual_CPUE_tables_",
	                                                           min(total$year,na.rm=T),
	                                                          "-",max(total$year,na.rm=T),"_",bk,"-",nafo.div,"_",period,"-",surv,
	                                                          ".txt",sep=""),
	                                                           sep="\t", row.names = F, col.names = T)
	        #Write2 
	        if(nafo.div == bk) write.table(total, file = paste(direct,"Data/Fishery_data/CPUE/Annual_CPUE_tables_",
	                                                           min(total$year,na.rm=T),
	                                                          "-",max(total$year,na.rm=T),"_",bk,"_",period,"-",surv,".txt",sep=""),
	                                                           sep="\t", row.names = F, col.names = T)
	      }# end  if(period = "survyr")
  	  if(period == "calyr")
  	    {
  	      #Write3
    	    if(nafo.div != bk) write.table(total, file = paste(direct,"Data/Fishery_data/CPUE/Annual_CPUE_tables_",
    	                                                       min(total$year,na.rm=T),
    	                                                       "-",max(total$year,na.rm=T),"_",bk,"_",nafo.div,".txt",sep=""),
    	                                                         sep="\t", row.names = F, col.names = T)
  	      #Write4 
  	      if(nafo.div == bk) write.table(total, file = paste(direct,"Data/Fishery_data/CPUE/Annual_CPUE_tables_",
  	                                                         min(total$year,na.rm=T),
    	                                                       "-",max(total$year,na.rm=T),"_",bk,".txt",sep=""),
    	                                                        sep="\t", row.names = F, col.names = T)
  	    }# end  if(period = "survyr")
	 
	  }
	# if this is = T we tweak the out object to have these headers/data.  Perhaps if sending data to our model or based on model output??
	if(model.out == T) out<-data.frame(year=c(min(total$year,na.rm=T):max(total$year,na.rm=T)),
	                                   C=as.vector(total$catch),E=as.vector(total$effort),
	                                   U=total$cpue,U.cv=sqrt(total$cpue.var)/total$cpue)
	
	# if looking at survey year Print the Post-survey catch from the year before the last year requested.
	if(period=='survyr')
	  {
		  print("Post-survey catch")
		  print(post.surv.catch/1000)
	  } # end if(period=='survyr')
	
	# End the results back to function calling this.
	return(out)

	######################################### End Section 3: Calculating CPUE and post-processing  #####################################################
	
	
	
} # end function fishery.dat

	