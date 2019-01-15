#   This function is used to import the survey data from 1981-2010.  Given
# This data is unchanging and a number of flat files now exist I am unsure of the need for this function moving 
#  forward
# Update history
# Commented, checked  and revised by DK March 31, 2016
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary_data.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files"), 
#  functions 2-5 are located internally (i.e in this file)
# 
#      1: "convert.dd.dddd.r"
#      2: "import.hf.data"
#      3: "parse.dis
#      4: "parse.dtp"
#      5 "parse.shf"
##     
###############################################################################################################


# ARGUMENTS 
# import.survey.data()
# years:    For summer (Aug) survey choose any combination from 1978-2010, for spring (May) 1983-2010 are boundaries.  Note the 
#           for the summer 1978-1980 are not normally looked at, while for spring 1983 is usually not selected, reason unknown but
#           presumably there is something amiss with these data.  Default is 1981-2009
# survey:   Choices here are limited to 'May' for the spring survey and 'Aug' for the summer survey
# type:     Options are 'surv', 'grid', default is 'surv'.
# explore:  Use both the exploratory and assessment tows (T/F) default is True which means use all the data.
# export:   Export the data as a csv? (T/F) default is True
# dirc:    Directory to look for the data in.  Default = "Y:/Offshore scallop/Assessment/"

# source("d:/Assessment/2009/r/fn/import.survey.data.r")

###########################################  SECTION 1 - import.survy.data #########################################################


##### import.survey.data.r: Imports survey data from flat files

import.survey.data<-function(years=1981:2009, survey='Aug', type='surv',explore=T, 
                             export=T,dirc="Y:/Offshore scallop/Assessment/")
  {
	# Load in necessary packages
  require(chron) || stop("Install chron package")
  require(splancs)  || stop("Install splancs package")
  
  # Call in the function to convert lat/long data to decimal degrees, used in sub-functions only.
	source(paste(dirc,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep=""))
	
  
  # Here we bring in all the survey data loop runs through all the years chosen for the survey chosen (May vs. Aug) and the type chosen.
	shf.lst<-list(NULL)
	for(i in 1:length(years)){
		shf.lst[[i]]<-import.hf.data(survey=survey,type=type,year=years[i],dirt=dirc)
		print(unique(shf.lst[[i]]$bank))
		print(years[i])	# just to check on your progress
	}

	# converts to data.frame
	shf.dat<-do.call("rbind",shf.lst)

	# exploratory vs assessment tows: assessment tows are random = T.  This is a series of logical statements
	# to determine which tows are assessment tows.  There are some tricky logical calls, note
	# that the spring defaults to T (assessment) and the code is used to correct, while the summer is set to F and the code is used
	# to correct.
	if(survey=='May'){
	  # Set everything to True, change the exceptions.
		shf.dat$random<-T
		# If on Browns Bank and year is before 2003
		shf.dat$random[(shf.dat$bank=='BBn'|shf.dat$bank=='BBs')&shf.dat$year<2003&is.na(shf.dat$stratum)]<-F
		# Or after 2002 and the stratum is NA than false.  
		shf.dat$random[(shf.dat$bank=='BBn'|shf.dat$bank=='BBs')&shf.dat$year>2002&is.na(shf.dat$stratum)]<-F
		# Also on Browns stratum 4 and 5 are set to F, must be exploritory tows.
		shf.dat$random[(shf.dat$bank=='BBn'|shf.dat$bank=='BBs')&shf.dat$year<2003&shf.dat$stratum>4]<-F
		shf.dat$random[(shf.dat$bank=='BBn'|shf.dat$bank=='BBs')&shf.dat$year>2002&shf.dat$stratum>5]<-F
		# After 2006 stratum 2 is I.D.'ed as exploritory on German bank.
		shf.dat$random[shf.dat$bank=='Ger'&shf.dat$year>2006&shf.dat$stratum==2]<-F
	}
	# So we only do this is type == surv, while for spring (above) we do it regardless.
	if(type=='surv'&&survey=='Aug'){
	  # Set all to False, change the exceptions
		shf.dat$random<-F
		# Strata <  6 before 1998 are assessment (NA's remain F)
		shf.dat$random[shf.dat$year<1998&shf.dat$stratum<6&!is.na(shf.dat$stratum)]<-T
		# From 2004-2008 all strata < 6 are assessment
		shf.dat$random[shf.dat$year>2003&shf.dat$year<2009&shf.dat$stratum<6]<-T
		# After 2008 strata < 8 are assessment (NA's remain F)
		shf.dat$random[shf.dat$year>2008&shf.dat$stratum<8&!is.na(shf.dat$stratum)]<-T
		# Between 1998 and 2003 all tows < 151 are assessment.
		shf.dat$random[shf.dat$year>1997&shf.dat$year<2004&shf.dat$tow<151]<-T
	}
	# If we just want assessment tows do this
	if(explore==F)shf.dat<-subset(shf.dat,random==T)
	
	# Export data to a csv, these are now largely useless, but for completeness let's place these here...
	if(export==T)write.table(shf.dat,file = paste("Survey_data/Old_summer/",survey,type,"SHF.csv",sep=""),sep=",", row.names = F, col.names = T)
	
	# return the opject
	shf.dat
	
}

###########################################  END SECTION 1 - import.survy.data #########################################################


###########################################  SECTION 2 - import.hf.data #########################################################

##### import.hf.data.r: Imports single year of survey data by combining flat files in "d:/Data/Survey"

import.hf.data <- function(survey = 'May', year = 2008,bank,type='surv',dirt=dirc)
{
  require(splancs)  || stop("Install splancs package")
  # Imports Ginette's survey data
	GBsurvey.gin <- read.table(paste(dirt,"/Data/Survey_data/Old_Summer/GBSurvey.txt",sep=""),header=T)
		
	### Run this section of code if we are looking at the may survey.
	if(survey == 'May')
	  {
	  # Note where these files are stored
	path = paste(dirt,"Data/Survey_data/Old_Spring/",sep="")
				
	SHF.lst<-list(NULL)
	
	# if not specified gets data for all banks surveyed that year
	if(missing(bank)) 
	  {
		# The list of our banks
	  banks<-c("Ban","BB","BBn","BBs","GB","Ger","Mid","Sab","BanIce")
		# The data
	  tbb<-read.csv(paste(dirt,"Data/Survey_data/towbybank.csv",sep=""))
	  # Select the bank names that we have data for in a given year.
	  bank<-banks[!is.na(tbb[tbb$Year==year,-c(1,6)])]
	} # end if(missing(bank)) {
	
	# This runs a loop to return the data for the banks that we are interested in (default is all banks)
		for(i in 1:length(bank)){
			# read hf file, this file has  with the data for a specific bank/year combination for shell height frequencies
			hf <- parse.shf(paste(path, year, "/hf", bank[i], year, ".txt", sep = ""),survey=survey, yr = year)
			# read dis file (1994- ) If later than 1994 we need to run the parse.dis function to align the data. Contains data for distance coefficients.
			if(year>1993)dis <- parse.dis(paste(path, year, "/dis", bank[i], year, ".txt", sep = ""),survey=survey, yr = year)
			# read dtp file, and run with parse.dtp function.  returns the bearing information among other things
			dtp <- parse.dtp(paste(path, year, "/dtp", bank[i], year, ".txt", sep = ""),survey=survey, yr = year,dirt = dirc)
			# adjust tow numbers to match between files (e.g. 403 = 003)
			if(year<2009){
				if(hf$tow[1]>199) hf$tow<-hf$tow-floor(hf$tow/100)*100
				if(year>1993&&dis$tow[1]>199) dis$tow<-dis$tow-floor(dis$tow/100)*100
				if(dtp$tow[1]>199) dtp$tow<-dtp$tow-floor(dtp$tow/100)*100
			} #end  if year< 2009
			
			# if one of these banks & the data is post 2005 we combine the data in this fashion
			if((bank[i] == "BB" || bank[i] == "BBn" || bank[i] == "BBs" || bank[i] == "Sab" || bank[i] == "Ger") && year > 2005){
				# Bring in some new data, looks to be tow and strata information DK Note August 18, 2015 this is something to check if strata is correct
			  sr <- read.table(paste(path, year, "/sr", bank[i], year, ".txt", sep = ""), header = T)
			  # Merge hf and dis by tow and keep everything that doesn't match as well
				tmp <- merge(hf,dis, by = "tow",all=T)
				# create the shf object from the hf data, again picking column #'s never makes me comfortable... note the sweep of teh hf data which
				# is the shell height frequencies.
				shf <- cbind(hf[,1:6], sweep(hf[,7:46],1,FUN='*',tmp$dc2))
				# We've decided that dc2 is the distance coefficient although there were 4 to chose from, why?
				shf$dis<-tmp$dc2
				# merge the shf object with the sr object again by tow
				shf <- merge(shf, sr, by = "tow", all.x = T)
				tmp <- merge(shf, dtp, by = "tow", all.x = T)
				# Combine everything into a datafram
				SHF.lst[[i]] <- with(tmp, data.frame(year = rep(year, nrow(tmp)), cruise = as.character(cruise.x), bank=rep(bank[i], 
				                                    nrow(tmp)), date, tow, stratum = stratum.y, slat, slon, elat, elon, depth, state,dis,brg, 
				                                    shf[, 7:46],baskets, totwt,stringsAsFactors = F))
			} # end if bank[i] == ... & year > 2005
			
			# if one of these banks & the data is between 1991 and 2005 
			else if((bank[i] == "BB" || bank[i] == "BBn" || bank[i] == "BBs" || bank[i] == "Sab" || bank[i] == "Ger") && year < 2006 && year > 1990){
				if(year>1993){
					tmp <- merge(hf,dis, by = "tow",all=T)
					
					# create the shf object from the hf data, again picking column #'s never makes me comfortable... note the sweep of teh hf data which
					# is the shell height frequencies.
					shf <- cbind(hf[,1:6], sweep(hf[,7:46],1,FUN='*',tmp$dc2))
					# We've decided that dc2 is the distance coefficient although there were 4 to chose from, why?
					shf$dis<-tmp$dc2
				} # end else if bank[i] == ...
				else if(year<1994){ 
					tmp <- merge(hf,dtp, by = "tow",all=T)
					# create the shf object from the hf data, again picking column #'s never makes me comfortable... note the sweep of teh hf data which
					# is the shell height frequencies.
					shf <- cbind(hf[,1:6], sweep(hf[,7:46],1,FUN='*',tmp$dc)) 
					# For some reason we've decided that dc is the distance coefficient in this case, inconsistent with above tho perhaps a reason?
					shf$dis<-tmp$dc
				}# end else if < 1994 (note nothing bank specific before 1994)
				
			  # mearge the shf and dtp data, left join so keep everything in the shf data.
			  tmp <- merge(shf, dtp, by = "tow", all.x = T)
			  st <- as.numeric(read.table(paste(path, year, "/sr", year, bank[i], ".txt", sep = "")))
				# Make a list with all the Shell height frequency data (among other things) in it.
			  SHF.lst[[i]] <- with(tmp, data.frame(year = rep(year, nrow(tmp)), cruise = as.character(cruise.x), bank=rep(bank[i], 
			                                             nrow(tmp)), date, tow, stratum = st, slat, slon, elat, elon, depth, 
			                                             state,dis,brg, shf[, 7:46],baskets, totwt,stringsAsFactors = F))
			} # end if bank[i]... and year between 1991 and 2005

			# If on these partiulare banks before 1991
			else if(year < 1991 || (bank[i] != "BB" || bank[i] != "BBn" || bank[i] != "BBs" || bank[i] != "Sab" || bank[i] != "Ger")){
#			browser()
				if(year>1993){
					tmp <- merge(hf,dis, by = "tow",all=T)
					shf <- cbind(hf[,1:6], sweep(hf[,7:46],1,FUN='*',tmp$dc2))
					shf$dis<-tmp$dc2
				} # end if(year>1993){
				else if(year<1994){ 
					tmp <- merge(hf,dtp, by = "tow",all=T)
					shf <- cbind(hf[,1:6], sweep(hf[,7:46],1,FUN='*',tmp$dc)) 
					shf$dis<-tmp$dc
				} # end if(year < 1994){
				tmp <- merge(shf, dtp, by = "tow", all.x = T)
				st <- rep(NA, nrow(tmp))
				SHF.lst[[i]] <- with(tmp, data.frame(year = rep(year, nrow(tmp)), cruise = as.character(cruise.x), bank=rep(bank[i], nrow(tmp)), date, 
				                                     tow, stratum = st, slat, slon, elat, elon, depth, state,dis,brg, 
				                                     shf[, 7:46],baskets, totwt,stringsAsFactors = F))
			} # end else if year < 1991 || (bank[i] !=

		} # end for(i in 1:length(bank)){
		
	  # Take the SHF.lst and turn it into a dataframe.
	  SHF<-do.call("rbind",SHF.lst)
		
	  # Add some bounding boxes for Browns North and south
		BBs.bounding.xy <- data.frame(slon = c(-65.62486, -65.975, -65.5,-64.4, -64.5, -65.62486), slat = c(43, 42.308333, 42.2,42.8, 43.2, 43))
		BBn.bounding.xy <- data.frame(slon=c(-65.62486,-65.975,-66.7,-65.62486),slat=c(43,42.308333,42.9,43))
		# Make sure we don't have a factor here.
		SHF$bank<-as.character(SHF$bank)
		# This determines which points are in Browns North and Browns South via splancs "inout' function
		SHF$bank[with(SHF, inout(cbind(slon,slat), BBn.bounding.xy, bound = T))]<-'BBn'
		SHF$bank[with(SHF, inout(cbind(slon,slat), BBs.bounding.xy, bound = T))]<-'BBs'


	} # end if(survey == 'May'){
	
	### AUGUST SURVEY
	if(survey == 'Aug'){
		
		if(type=='surv'){
		  # The path differs here from May survey
				path = paste(dirt,"Data/Survey_data/Old_Summer/",sep="")
			# Set the bank	
			bank <- "GB"
			# Account for 2009 split into GBa and GBb
			if(year>2008) bank <- c("GBa","GBb")
			# Make a list for the loop (note loop index is either 1 or 2 as only 2 banks)
			SHF.lst<-list(NULL)
			for(i in 1:length(bank)){
				# Get the shell height frequency data
			  hf <- parse.shf(paste(path, year, "/survhf", bank[i], year, ".txt", sep = ""),survey=survey, yr = year)
				# Get the bearing (and perhaps other info) data
			  dtp <- parse.dtp(paste(path, year, "/survdtp", bank[i], year, ".txt", sep = ""),survey=survey, yr = year,dirt = dirc)
				# Get the distance coefficients
			  if(year>1993){
					dis <- parse.dis(paste(path, year, "/survdis", bank[i], year, ".txt", sep = ""),survey=survey, yr = year)
					tmp <- merge(hf,dis, by = "tow",all=T)
					shf <- cbind(hf[,1:6], sweep(hf[,7:46],1,FUN='*',tmp$dc2))
					# again we use dc2 here (have 4 options) but below we use dc, why the differnce?
					shf$dis<-tmp$dc2
				} # end if year > 1993
				else if(year<1994){ 
					tmp <- merge(hf,dtp, by = "tow",all=T)
					hf <- merge(hf,dtp[,1:2], all=T)
					if(year<1982){
						scaler<-tmp$sw/tmp$totwt	#subsample
						scaler[is.na(scaler)]<-1
						hf[,7:46]<-sweep(hf[,7:46],1,FUN='/',scaler)
					}# end if year < 1982
					shf <- cbind(hf[,1:6], sweep(hf[,7:46],1,FUN='*',tmp$dc)) 
					# again we use dc here (have 4 options) but above we use dc2 why the differnce?
						shf$dis<-tmp$dc
				} # end if year < 1994
			  
			  # Combine these data into a new object (str)
				str <- data.frame(tow=GBsurvey.gin$tow[GBsurvey.gin$year==year],stratum.y=GBsurvey.gin$stratum[GBsurvey.gin$year==year],stringsAsFactors = F)
				
				# After 2007 this is our str.
				if(year>2007) str <- read.table(paste(path, year,"/survstr",bank[i],year,".txt",sep=""),header=T)
				# merge shf and str, take all shf data
				shf <- merge(shf,str, by = "tow", all.x = T)
				# merge shf and dtp, take all shf data
				tmp <- merge(shf, dtp, by = "tow", all.x = T)
				# Make a SHF list
				SHF.lst[[i]] <- with(tmp, data.frame(year = rep(year, nrow(tmp)), cruise = as.character(cruise.x), bank=rep(bank[i], nrow(tmp)), 
				                                     date, tow, stratum = stratum.y, slat, slon, elat, elon, depth, state,dis,
				                                     brg, shf[, 7:46],baskets, totwt,stringsAsFactors = F))
			} # end for(i in 1:length(bank))
			
			# Turn SHF into a dataframe
			SHF<-do.call("rbind",SHF.lst)
			
			# Get GB bounding area
			GBarea.xy<-data.frame(slon=c(-67.30935,-66.754167,-66,-65.666667,-65.666667,-66.16895,-67.30935),slat=c(42.333333,42.333333,42.333333,42,41.583333,41,42.333333))
			SHF$bank<-as.character(SHF$bank)
			# Correct everything before 2009 to indicate whether it was GBa or GBb
			SHF$bank[SHF$year<2009][with(SHF, inout(cbind(slon,slat), GBarea.xy[c(2:5,2),], bound = T))]<-'GBb'
			SHF$bank[SHF$year<2009][with(SHF, inout(cbind(slon,slat), GBarea.xy[-(3:4),], bound = T))]<-'GBa'
		} # end if type = surv
		
	  # If instead of type = survey we chose type= grid (reasons for doing this are unclear) this section is how one would make the GB SHF object.
	  # There is grid information available for GB.
		if(type=='grid'){
				path = paste(dirt,"Data/Survey_data/Old_Summer/",sep="")
			
			bank <- "GB"
			# So here we chose the grid flat files instead of the regular ones and we process the data similar to how we did above.
			hf <- parse.shf(paste(path, year, "/gridhf", bank, year, ".txt", sep = ""),survey=survey, yr = year)
			dtp <- parse.dtp(paste(path, year, "/griddtp", bank, year, ".txt", sep = ""),survey=survey, yr = year,dirt=dirc)
			if(year>1993){
				dis <- parse.dis(paste(path, year, "/griddis", bank, year, ".txt", sep = ""),survey=survey, yr = year)
				shf <- cbind(hf[,1:6], sweep(hf[,7:46],1,FUN='*',dis$dc2))
			}# end if year > 1993
			else if(year<1994){ shf <- cbind(hf[,1:6], hf[,7:46] * dtp$dc) }
			
			shf$id<-sort(rep(1:(nrow(shf)/2),2))
			dtp$id<-1:nrow(dtp)
			tmp <- merge(shf, dtp, by = "id", all.x = T)
			
			# The SHF datafram
			SHF <- with(tmp, data.frame(year = rep(year, nrow(tmp)), cruise = as.character(cruise.x), bank=rep(bank, nrow(tmp)), date, 
			                            tow=tow.y, slat, slon, elat, elon, depth, state, shf[, 7:46],stringsAsFactors = F))
			# GB area boundaries so we can split the area up into GBa and b.
			GBarea.xy<-data.frame(slon=c(-67.30935,-66.754167,-66,-65.666667,-65.666667,-66.16895,-67.30935),slat=c(42.333333,42.333333,42.333333,42,41.583333,41,42.333333))
			SHF$bank<-as.character(SHF$bank)
			SHF$bank[with(SHF, inout(cbind(slon,slat), GBarea.xy[c(2:5,2),], bound = T))]<-'GBb'
			SHF$bank[with(SHF, inout(cbind(slon,slat), GBarea.xy[-(3:4),], bound = T))]<-'GBa'
			# Something wrong with  tows less than 300, add 307 to them.
			SHF$tow[SHF$tow<300]<-SHF$tow[SHF$tow<300]+307

		} # end if type = grid
	} # end if survey = Aug
	SHF
} # end import.hf.data
###########################################  END SECTION 2 - import.hf.data #########################################################



###########################################  SECTION 3 - The 3 parse functions #########################################################





##### parse.dis.r: reads dis file, this gives us the data we need to calculate a distance coefficient for the model, this is all detailed manipulation to get the dataframes in the correct alignment
#  no detailed comments on this section of code have been performed.

parse.dis <- function(dis, survey, yr){
	
	# So if we are looking at Spring survey run this section of code, very specific processing here.
	if(survey=='May'){
		dis <- read.csv(dis, header = F)
		dis[,1] <- as.character(dis[,1])
		n <- nrow(dis)
		
		# The 1980 data needs massaged as per below
		if(yr == 1980){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(NA, substr(tmp1,2,5), NA, NA, NA)
			}	# end for for(i in 1:n){
			
			# Make it into a datafame with names
			tmp <- data.frame(tow = 1:n, dc1 = as.numeric(tmp[,2]), dc2 = NA, dc3 = NA, dc4 = NA)
			tmp$dc1[tmp$dc1 == 0] <- NA
			tmp$dc1[is.na(tmp$dc1)] <- 1
		} # end if(yr == 1980){
		
		# The 1981 data needs massaged as per below
		else if(yr == 1981){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(substr(tmp1,1,3), substr(tmp1,4,8), NA, NA, NA)
			}	# end for for(i in 1:n){
				
			#Make it into a datafame with names
			tmp <- data.frame(tow = as.numeric(tmp[,1]), dc1 = as.numeric(tmp[,2]), dc2 = NA, dc3 = NA, dc4 = NA)
			tmp$dc1[tmp$dc1 == 0] <- NA
			tmp$dc1[is.na(tmp$dc1)] <- 1
		}# end else if == 1981
			
		else if(yr > 1981 && yr < 1991){
			tmp <- dis
			#Make it into a datafame with names
			tmp <- data.frame(tow = as.numeric(as.character(tmp[,1])), dc1 = as.numeric(as.character(tmp[,2])), dc2 = NA, dc3 = NA, dc4 = NA)
			tmp$dc1[is.na(tmp$dc1)] <- 1
		}	# end else if > 1981 < 1991
			
		else if(yr > 1990 && yr < 1993){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(substr(tmp1,1,3), substr(tmp1,7,11), substr(tmp1,18,22), NA, NA)
			}	# end for for(i in 1:n){
				
			#Make it into a datafame with names
			tmp <- data.frame(tow = as.numeric(tmp[,1]), dc1 = as.numeric(tmp[,2]), dc2 = as.numeric(tmp[,3]), dc3 = NA, dc4 = NA)
			tmp$dc1[is.na(tmp$dc1)] <- 1
			tmp$dc2[is.na(tmp$dc2)] <- 1
		}# end else if > 1990 < 1993
		
		else if(yr == 1993){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(substr(tmp1,1,3), substr(tmp1,7,11), NA, NA, NA)
			}	# end for for(i in 1:n){
			
			#Make it into a datafame with names
			tmp <- data.frame(tow = as.numeric(tmp[,1]), dc1 = as.numeric(tmp[,2]), dc2 = NA, dc3 = NA, dc4 = NA)
			tmp$dc1[is.na(tmp$dc1)] <- 1
		}# end else if == 1993
		
		else if(yr > 1993){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(substr(tmp1,1,3), substr(tmp1,7,11), substr(tmp1, 15, 19), substr(tmp1, 23, 27), substr(tmp1, 31, 35))
			}	# end for for(i in 1:n){
			
			#Make it into a datafame with names	
			tmp <- data.frame(tow = as.numeric(tmp[,1]), dc1 = as.numeric(tmp[,2]), dc2 = as.numeric(tmp[,3]), dc3 = as.numeric(tmp[,4]), dc4 = as.numeric(tmp[,5]))
			tmp$dc1[is.na(tmp$dc1)] <- 1
			tmp$dc2[is.na(tmp$dc2)] <- 1
			tmp$dc3[is.na(tmp$dc3)] <- 1
			tmp$dc4[is.na(tmp$dc4)] <- 1
		} # end else if > 1993
			
	} #end if(survey=='May'){
	
	if(survey=="Aug"){
	
		dis <- read.csv(dis, header = F)
		dis[,1] <- as.character(dis[,1])
		n <- nrow(dis)
		
		if(yr == 1980){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(NA, substr(tmp1,2,5), NA, NA, NA)
			}	# end for for(i in 1:n){
				
			tmp <- data.frame(tow = 1:n, dc1 = as.numeric(tmp[,2]), dc2 = NA, dc3 = NA, dc4 = NA)
			tmp$dc1[tmp$dc1 == 0] <- NA
			tmp$dc1[is.na(tmp$dc1)] <- 1
		} # end  if == 1980
		
		else if(yr == 1981){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(substr(tmp1,1,3), substr(tmp1,4,8), NA, NA, NA)
			}	# end for for(i in 1:n){
				
			tmp <- data.frame(tow = as.numeric(tmp[,1]), dc1 = as.numeric(tmp[,2]), dc2 = NA, dc3 = NA, dc4 = NA)
			tmp$dc1[tmp$dc1 == 0] <- NA
			tmp$dc1[is.na(tmp$dc1)] <- 1
		}# end else if == 1981
			
		else if(yr > 1981 && yr < 1991){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(substr(tmp1,1,3), substr(tmp1,4,8), NA, NA, NA)
			}	# end for for(i in 1:n){
				
			tmp <- data.frame(tow = as.numeric(tmp[,1]), dc1 = as.numeric(tmp[,2]), dc2 = NA, dc3 = NA, dc4 = NA)
			tmp$dc1[is.na(tmp$dc1)] <- 1
		}# end else if > 1981 < 1991
			
		else if(yr > 1990 && yr < 1993){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(substr(tmp1,1,3), substr(tmp1,7,11), substr(tmp1,18,22), NA, NA)
			}	# end for for(i in 1:n){
				
			tmp <- data.frame(tow = as.numeric(tmp[,1]), dc1 = as.numeric(tmp[,2]), dc2 = as.numeric(tmp[,3]), dc3 = NA, dc4 = NA)
			tmp$dc1[is.na(tmp$dc1)] <- 1
			tmp$dc2[is.na(tmp$dc2)] <- 1
		} # end else if > 1990 < 1993
		
		else if(yr == 1993){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(substr(tmp1,1,3), substr(tmp1,7,11), NA, NA, NA)
			}	# end for for(i in 1:n){
				
			tmp <- data.frame(tow = as.numeric(tmp[,1]), dc1 = as.numeric(tmp[,2]), dc2 = NA, dc3 = NA, dc4 = NA)
			tmp$dc1[is.na(tmp$dc1)] <- 1
		}# end else if == 1993
		
		else if(yr > 1993){
			tmp <- matrix(NA, n, 5)
			for(i in 1:n){
				tmp1 <- as.character(dis[i,])
				tmp[i, ] <- c(substr(tmp1,1,3), substr(tmp1,7,11), substr(tmp1, 15, 19), substr(tmp1, 23, 27), substr(tmp1, 31, 35))
			}	# end for for(i in 1:n){
				
			tmp <- data.frame(tow = as.numeric(tmp[,1]), dc1 = as.numeric(tmp[,2]), dc2 = as.numeric(tmp[,3]), 
			                  dc3 = as.numeric(tmp[,4]), dc4 = as.numeric(tmp[,5]))
			tmp$dc1[is.na(tmp$dc1)] <- 1
			tmp$dc2[is.na(tmp$dc2)] <- 1
			tmp$dc3[is.na(tmp$dc3)] <- 1
			tmp$dc4[is.na(tmp$dc4)] <- 1
		}# end else if > 1993
			
	} # End if (survey == "Aug")
  # return the tmp object
	tmp

}# End parse.dis function

####### parse.dtp.r: reads dtp file, this gives us information about the bearing of the tow, this is all detailed manipulation to get the dataframes in the correct alignment
#  no detailed comments on this section of code have been performed.

parse.dtp <- function(dtpfile, yr, survey='May',dirt=dirc){
	
  require(chron) || stop("Install chron package")

  
  # Notice the file is read in as one giant column which is turned into a character and subdivided below, OMG!
	dtp <- read.csv(dtpfile, header = F)
	dtp[,1] <- as.character(dtp[,1])
	n <- nrow(dtp)
	
	if(yr < 1980){
		tmp <- read.table(dtpfile)
		slat <- tmp[,3] + tmp[,4]/60 + tmp[,5]/3600
		slon <- tmp[,6] + tmp[,7]/60 + tmp[,8]/3600
		elat <- tmp[,9] + tmp[,10]/60 + tmp[,11]/3600
		elon <- tmp[,12] + tmp[,13]/60 + tmp[,14]/3600
		
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), slat = slat, slon = slon, elat = elat, elon = elon, 
		                  dc = as.numeric(tmp[,15]), brg = as.numeric(tmp[,16]), swt = as.numeric(tmp[,17]), baskets = as.numeric(tmp[,20]), 
		                  totwt = as.numeric(tmp[,18]), date=NA,stringsAsFactors = F)
	}
	
	if(yr == 1980){
		tmp <- matrix(NA, n, 10)
		for(i in 1:n){
			tmp1 <- as.character(dtp[i,])
			tmp[i, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,8,13), substr(tmp1,14,19), 	substr(tmp1,20,25), 
			              substr(tmp1,26,31), substr(tmp1,32,37), substr(tmp1,39,41), substr(tmp1,54,56), substr(tmp1,58,61))
			}	
	
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), slat = as.numeric(tmp[,3]), 
		                  slon = as.numeric(tmp[,4]), elat = as.numeric(tmp[,5]), elon = as.numeric(tmp[,6]), 
		                  dc = as.numeric(tmp[,7]), brg = as.numeric(tmp[,8]), swt = NA, baskets=NA, totwt = NA, date = NA,stringsAsFactors = F)
	}
	
	
	if(yr == 1981){
		tmp <- matrix(NA, n, 10)
		for(i in 1:n){
			tmp1 <- as.character(dtp[i,])
			tmp[i, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,8,13), substr(tmp1,14,19), 	
			              substr(tmp1,20,25), substr(tmp1,26,31), substr(tmp1,32,37), substr(tmp1,39,41), 
			              substr(tmp1,42,45), substr(tmp1,46,49))
			#, substr(tmp1,58,58)
			}	
		
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), slat = as.numeric(tmp[,3]), 
		                  slon = as.numeric(tmp[,4]), elat = as.numeric(tmp[,5]), elon = as.numeric(tmp[,6]), 
		                  dc = as.numeric(tmp[,7]), brg = as.numeric(tmp[,8]), swt = as.numeric(tmp[,9]), 
		                  baskets=NA, totwt = as.numeric(tmp[,10]), date = NA,stringsAsFactors = F)
	}
	
	
	else if(yr > 1981 && yr < 1994){
		tmp <- matrix(NA, n, 10)
		for(i in 1:n){
			tmp1 <- as.character(dtp[i,])
			tmp[i, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,8,13), substr(tmp1,14,19), 	substr(tmp1,20,25), 
			              substr(tmp1,26,31), substr(tmp1,35,39), substr(tmp1,41,43), substr(tmp1,44,47), substr(tmp1,48,51))
			#, substr(tmp1,60,60)
			}	
			
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), slat = as.numeric(tmp[,3]), 
		                  slon = as.numeric(tmp[,4]), elat = as.numeric(tmp[,5]), elon = as.numeric(tmp[,6]), 
		                  dc = as.numeric(tmp[,7]), brg = as.numeric(tmp[,8]), swt = as.numeric(tmp[,9]), baskets=NA, 
		                  totwt = as.numeric(tmp[,10]), date = NA,stringsAsFactors = F)
	}
	
	else if(yr == 1994){
		tmp <- matrix(NA, n, 10)
		for(i in 1:n){
			tmp1 <- as.character(dtp[i,])
			tmp[i, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,8,13), substr(tmp1,14,19), 	substr(tmp1,20,25), 
			              substr(tmp1,26,31), substr(tmp1,35,39), substr(tmp1,41,43), substr(tmp1,44,47), substr(tmp1,48,51))
			}	
			
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), slat = as.numeric(tmp[,3]), 
		                  slon = as.numeric(tmp[,4]), elat = as.numeric(tmp[,5]), elon = as.numeric(tmp[,6]), 
		                  dc = as.numeric(tmp[,7]), brg = as.numeric(tmp[,8]), swt = as.numeric(tmp[,9]), baskets=NA, 
		                  totwt = as.numeric(tmp[,10]), date = NA,stringsAsFactors = F)
	}
	
	else if(yr == 1995 || yr == 1996){
		tmp <- matrix(NA, n, 10)
		for(i in 1:n){
			tmp1 <- as.character(dtp[i,])
			tmp[i, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,8,13), substr(tmp1,14,19), 	substr(tmp1,20,25), 
			              substr(tmp1,26,31), substr(tmp1,34,37), substr(tmp1,39,41), substr(tmp1,43,47), substr(tmp1,48,52))
			}	
			
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), slat = as.numeric(tmp[,3]), 
		                  slon = as.numeric(tmp[,4]), elat = as.numeric(tmp[,5]), elon = as.numeric(tmp[,6]), 
		                  spd = as.numeric(tmp[,7]), brg = as.numeric(tmp[,8]), swt = as.numeric(tmp[,9]), baskets=NA, 
		                  totwt = as.numeric(tmp[,10]), date = NA,stringsAsFactors = F)
	}
	
	else if(yr == 1997&survey=='May'){
		tmp <- matrix(NA, n, 10)
		for(i in 1:n){
			tmp1 <- as.character(dtp[i,])
			tmp[i, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,8,13), substr(tmp1,14,19), 	
			              substr(tmp1,20,25), substr(tmp1,26,31), substr(tmp1,34,37), substr(tmp1,39,41), 
			              substr(tmp1,43,47), substr(tmp1,48,52))
			}	
			
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), slat = as.numeric(tmp[,3]), 
		                  slon = as.numeric(tmp[,4]), elat = as.numeric(tmp[,5]), elon = as.numeric(tmp[,6]), 
		                  spd = as.numeric(tmp[,7]), brg = as.numeric(tmp[,8]), swt = as.numeric(tmp[,9]), baskets=NA, 
		                  totwt = as.numeric(tmp[,10]), date = NA,stringsAsFactors = F)
	}
	
	else if(yr == 1997&survey=='Aug'){
		tmp <- matrix(NA, n, 10)
		for(i in 1:n){
			tmp1 <- as.character(dtp[i,])
			tmp[i, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,8,14), substr(tmp1,15,21), 	
			              substr(tmp1,22,28), substr(tmp1,29,35), substr(tmp1,38,41), substr(tmp1,43,45), 
			              substr(tmp1,48,51), substr(tmp1,53,57))
			}	
			
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), slat = as.numeric(tmp[,3]), 
		                  slon = as.numeric(tmp[,4]), elat = as.numeric(tmp[,5]), elon = as.numeric(tmp[,6]), 
		                  spd = as.numeric(tmp[,7]), brg = as.numeric(tmp[,8]), baskets = as.numeric(tmp[,9]), 
		                  totwt = as.numeric(tmp[,10]), date = NA,stringsAsFactors = F)
	} 
	else if(yr > 1997){
		tmp <- matrix(NA, n, 11)
		for(i in 1:n){
			tmp1 <- as.character(dtp[i,])
			tmp[i, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,9,15), substr(tmp1,17,23), 	
			              substr(tmp1,25,31), substr(tmp1,33,39), substr(tmp1,42,45), substr(tmp1,47,51), 
			              substr(tmp1,53,57), substr(tmp1,59,63),substr(tmp1,72,79))
			}	
	#browser()	
		substr(tmp[,11],1,1)[substr(tmp[,11],1,1)==" "]<-0	
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), slat = as.numeric(tmp[,3]), 
		                  slon = as.numeric(tmp[,4]), elat = as.numeric(tmp[,5]), elon = as.numeric(tmp[,6]), 
		                  spd = as.numeric(tmp[,7]), brg = as.numeric(tmp[,8]), baskets = as.numeric(tmp[,9]), 
		                  totwt = as.numeric(tmp[,10]), date = as.character(as.Date(tmp[,11],"%e %m %Y")),stringsAsFactors = F)
	} 
		

	tmp$slat <- convert.dd.dddd(tmp$slat)
	tmp$slon <- convert.dd.dddd(tmp$slon) * -1
	tmp$elat <- convert.dd.dddd(tmp$elat)
	tmp$elon <- convert.dd.dddd(tmp$elon) * -1
#browser()		
#	stime <- with(tmp, ifelse(nchar(stime) == 5, paste("0", stime, sep=""), stime))
#	stime <- ifelse(nchar(stime) == 4, paste("00", stime, sep=""), stime)	
#	stime <- ifelse(nchar(stime) == 3, paste("000", stime, sep=""), stime)	
#	etime <- with(tmp, ifelse(nchar(etime) == 5, paste("0", etime, sep=""), etime))
#	etime <- ifelse(nchar(etime) == 4, paste("00", etime, sep=""), etime)	
#	etime <- ifelse(nchar(etime) == 3, paste("000", etime, sep=""), etime)	
#	stime <- as.numeric(chron(times = stime, format = "hms", out.format = "h:m:s")) * 1440
#	etime <- as.numeric(chron(times = etime, format = "hms", out.format = "h:m:s")) * 1440
	
#	sdate <- with(tmp, ifelse(nchar(sdate) == 7, paste("0", sdate, sep = ""), sdate))
#	sdate <- paste(substr(sdate,1,2), "/", substr(sdate,3,4), "/", substr(sdate,5,8), sep="")
#	sdate <- dates(as.character(sdate), format = 'd/m/y', out.format = 'm/d/y')
	
#	edate <- with(tmp, ifelse(nchar(edate) == 7, paste("0", edate, sep = ""), edate))
#	edate <- paste(substr(edate,1,2), "/", substr(edate,3,4), "/", substr(edate,5,8), sep="")
#	edate <- dates(as.character(edate), format = 'd/m/y', out.format = 'm/d/y')
	
#	tmp$stime <- stime
#	tmp$etime <- etime
#	tmp$sdate <- sdate
#	tmp$edate <- edate
		
	
	tmp$totwt[tmp$totwt == 9999] <- NA
	if('swt'%in%names(tmp))tmp$swt[tmp$swt == 9999] <- NA
	
	tmp
}

####### parse.shf.r: reads hf file, shell hieght frequency data, this is all detailed manipulation to get the dataframes in the correct alignment
#  no detailed comments on this section of code have been performed.

parse.shf <- function(hf, survey='May', yr){
	
  require(chron) || stop("Install chron package")
	
	hf <- read.csv(hf, header = F)
	hf[,1] <- as.character(hf[,1])
	n <- nrow(hf)
	
	if(yr < 1990){
		tmp <- matrix(NA, n, 44)
		st <- c()
		j <- 1
		for(i in seq(1, n, by = 2)){
			tmp1 <- as.character(hf[i,])
			tmp2 <- as.character(hf[i+1,])
			if(substr(tmp1, 80, 80) == 0) st[j] <- "live"
			else if(substr(tmp1, 80, 80) == 2){ st[j] <- "dead" }
			tmp[j, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,11,16), substr(tmp1,17,19), 
			              substr(tmp1,20,22), substr(tmp1,23,25), substr(tmp1,26,28), substr(tmp1,29,31), 
			              substr(tmp1,32,34), substr(tmp1,35,37), substr(tmp1,38,40), substr(tmp1, 41,43), 
			              substr(tmp1,44,46), substr(tmp1,47,49), substr(tmp1,50,52), substr(tmp1,53,55), 
			              substr(tmp1,56,58), substr(tmp1,59,61), substr(tmp1,62,64), substr(tmp1,65,67), 
			              substr(tmp1,68,70), substr(tmp1,71,73), substr(tmp1,74,76), substr(tmp1,77,79), 
			              substr(tmp2,20,22), substr(tmp2,23,25), substr(tmp2,26,28), substr(tmp2,29,31), 
			              substr(tmp2,32,34), substr(tmp2,35,37), substr(tmp2,38,40), substr(tmp2, 41,43), 
			              substr(tmp2,44,46), substr(tmp2,47,49), substr(tmp2,50,52), substr(tmp2,53,55), 
			              substr(tmp2,56,58), substr(tmp2,59,61), substr(tmp2,62,64), substr(tmp2,65,67), 
			              substr(tmp2,68,70), substr(tmp2,71,73), substr(tmp2,74,76), substr(tmp2,77,79))
			j <- j + 1
		}
		
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), stratum = rep(NA, length(tmp[,2])), 
		                  tms = as.numeric(tmp[,3]), depth = as.numeric(tmp[,4]), state = st, h5 = as.numeric(tmp[,5]), 
		                  h10 = as.numeric(tmp[,6]), h15 = as.numeric(tmp[,7]), h20 = as.numeric(tmp[,8]), h25 = as.numeric(tmp[,9]), 
		                  h30 = as.numeric(tmp[,10]), h35 = as.numeric(tmp[,11]), h40 = as.numeric(tmp[,12]), h45 = as.numeric(tmp[,13]), 
		                  h50 = as.numeric(tmp[,14]), h55 = as.numeric(tmp[,15]), h60 = as.numeric(tmp[,16]), h65 = as.numeric(tmp[,17]), 
		                  h70 = as.numeric(tmp[,18]), h75 = as.numeric(tmp[,19]), h80 = as.numeric(tmp[,20]), h85 = as.numeric(tmp[,21]), 
		                  h90 = as.numeric(tmp[,22]),h95 = as.numeric(tmp[,23]), h100 = as.numeric(tmp[,24]), h105 = as.numeric(tmp[,25]), 
		                  h110 = as.numeric(tmp[,26]), h115 = as.numeric(tmp[,27]), h120 = as.numeric(tmp[,28]), h125 = as.numeric(tmp[,29]), 
		                  h130 = as.numeric(tmp[,30]), h135 = as.numeric(tmp[,31]), h140 = as.numeric(tmp[,32]), h145 = as.numeric(tmp[,33]), 
		                  h150 = as.numeric(tmp[,34]), h155 = as.numeric(tmp[,35]), h160 = as.numeric(tmp[,36]), h165 = as.numeric(tmp[,37]), 
		                  h170 = as.numeric(tmp[,38]), h175 = as.numeric(tmp[,39]), h180 = as.numeric(tmp[,40]), h185 = as.numeric(tmp[,41]), 
		                  h190 = as.numeric(tmp[,42]), h195 = as.numeric(tmp[,43]), h200 = as.numeric(tmp[,44]),stringsAsFactors = F)
		
		tmp[,7:46][is.na(tmp[,7:46])] <- 0
		tmp <- tmp[!is.na(tmp[,1]),]
	}
	
	else if(yr == 1990 && survey=='May'){
		tmp <- matrix(NA, n, 44)
		st <- c()
		j <- 1
		for(i in seq(1, n, by = 2)){
			tmp1 <- as.character(hf[i,])
			tmp2 <- as.character(hf[i+1,])
			if(substr(tmp1, 80, 80) == 0) st[j] <- "live"
			else if(substr(tmp1, 80, 80) == 2){ st[j] <- "dead" }
			tmp[j, ] <- c(substr(tmp1,1,4), substr(tmp1,5,7), substr(tmp1,11,16), substr(tmp1,17,19), substr(tmp1,20,22), 
			              substr(tmp1,23,25), substr(tmp1,26,28), substr(tmp1,29,31), substr(tmp1,32,34), substr(tmp1,35,37), 
			              substr(tmp1,38,40), substr(tmp1, 41,43), substr(tmp1,44,46), substr(tmp1,47,49), substr(tmp1,50,52), 
			              substr(tmp1,53,55), substr(tmp1,56,58), substr(tmp1,59,61), substr(tmp1,62,64), substr(tmp1,65,67), 
			              substr(tmp1,68,70), substr(tmp1,71,73), substr(tmp1,74,76), substr(tmp1,77,79), substr(tmp2,20,22), 
			              substr(tmp2,23,25), substr(tmp2,26,28), substr(tmp2,29,31), substr(tmp2,32,34), substr(tmp2,35,37), 
			              substr(tmp2,38,40), substr(tmp2, 41,43), substr(tmp2,44,46), substr(tmp2,47,49), substr(tmp2,50,52), 
			              substr(tmp2,53,55), substr(tmp2,56,58), substr(tmp2,59,61), substr(tmp2,62,64), substr(tmp2,65,67), 
			              substr(tmp2,68,70), substr(tmp2,71,73), substr(tmp2,74,76), substr(tmp2,77,79))
			j <- j + 1
		}
		
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), stratum = rep(NA, length(tmp[,2])), 
		                  tms = as.numeric(tmp[,3]), depth = as.numeric(tmp[,4]), state = st, h5 = as.numeric(tmp[,5]), 
		                  h10 = as.numeric(tmp[,6]), h15 = as.numeric(tmp[,7]), h20 = as.numeric(tmp[,8]), h25 = as.numeric(tmp[,9]), 
		                  h30 = as.numeric(tmp[,10]), h35 = as.numeric(tmp[,11]), h40 = as.numeric(tmp[,12]), h45 = as.numeric(tmp[,13]), 
		                  h50 = as.numeric(tmp[,14]), h55 = as.numeric(tmp[,15]), h60 = as.numeric(tmp[,16]), h65 = as.numeric(tmp[,17]), 
		                  h70 = as.numeric(tmp[,18]), h75 = as.numeric(tmp[,19]), h80 = as.numeric(tmp[,20]), h85 = as.numeric(tmp[,21]), 
		                  h90 = as.numeric(tmp[,22]),h95 = as.numeric(tmp[,23]), h100 = as.numeric(tmp[,24]), h105 = as.numeric(tmp[,25]), 
		                  h110 = as.numeric(tmp[,26]), h115 = as.numeric(tmp[,27]), h120 = as.numeric(tmp[,28]), h125 = as.numeric(tmp[,29]), 
		                  h130 = as.numeric(tmp[,30]), h135 = as.numeric(tmp[,31]), h140 = as.numeric(tmp[,32]), h145 = as.numeric(tmp[,33]), 
		                  h150 = as.numeric(tmp[,34]), h155 = as.numeric(tmp[,35]), h160 = as.numeric(tmp[,36]), h165 = as.numeric(tmp[,37]), 
		                  h170 = as.numeric(tmp[,38]), h175 = as.numeric(tmp[,39]), h180 = as.numeric(tmp[,40]), h185 = as.numeric(tmp[,41]), 
		                  h190 = as.numeric(tmp[,42]), h195 = as.numeric(tmp[,43]), h200 = as.numeric(tmp[,44]),stringsAsFactors = F)
		
		tmp[,7:46][is.na(tmp[,7:46])] <- 0
		tmp <- tmp[!is.na(tmp[,1]),]
	}
	
	else if(yr == 1990 && survey=='Aug'){
		hf <- hf[,-28]
		tmp <- matrix(NA, n, 46)
		st <- c()
		j <- 1
		for(i in seq(1, n, by = 2)){
			tmp1 <- as.character(hf[i,])
			tmp2 <- as.character(hf[i+1,])
			if(hf[i,length(hf[i,])] == 0) st[j] <- "live"
			else if(hf[i,length(hf[i,])] == 2){ st[j] <- "dead" }
			tmp[j, ] <- c(tmp1[-length(tmp1)], tmp2[7:(length(tmp2)-1)])
			j <- j + 1
		}
	
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), stratum = as.numeric(tmp[,3]), 
		                  tms = as.factor(paste(tmp[,4],tmp[,5], sep="")), depth = as.numeric(tmp[,6]), state = st, 
		                  h5 = as.numeric(tmp[,7]), h10 = as.numeric(tmp[,8]), h15 = as.numeric(tmp[,9]), h20 = as.numeric(tmp[,10]), 
		                  h25 = as.numeric(tmp[,11]), h30 = as.numeric(tmp[,12]), h35 = as.numeric(tmp[,13]), h40 = as.numeric(tmp[,14]), 
		                  h45 = as.numeric(tmp[,15]), h50 = as.numeric(tmp[,16]), h55 = as.numeric(tmp[,17]), h60 = as.numeric(tmp[,18]), 
		                  h65 = as.numeric(tmp[,19]), h70 = as.numeric(tmp[,20]), h75 = as.numeric(tmp[,21]), h80 = as.numeric(tmp[,22]), 
		                  h85 = as.numeric(tmp[,23]), h90 = as.numeric(tmp[,24]),h95 = as.numeric(tmp[,25]), h100 = as.numeric(tmp[,26]), 
		                  h105 = as.numeric(tmp[,27]), h110 = as.numeric(tmp[,28]), h115 = as.numeric(tmp[,29]), h120 = as.numeric(tmp[,30]), 
		                  h125 = as.numeric(tmp[,31]), h130 = as.numeric(tmp[,32]), h135 = as.numeric(tmp[,33]), h140 = as.numeric(tmp[,34]), 
		                  h145 = as.numeric(tmp[,35]), h150 = as.numeric(tmp[,36]), h155 = as.numeric(tmp[,37]), h160 = as.numeric(tmp[,38]), 
		                  h165 = as.numeric(tmp[,39]), h170 = as.numeric(tmp[,40]), h175 = as.numeric(tmp[,41]), h180 = as.numeric(tmp[,42]), 
		                  h185 = as.numeric(tmp[,43]), h190 = as.numeric(tmp[,44]), h195 = as.numeric(tmp[,45]), h200 = as.numeric(tmp[,46]),
		                  stringsAsFactors = F)	
		
		#tmp[,6:46][is.na(tmp[,6:46])] <- 0
		tmp <- tmp[!is.na(tmp[,1]),]
	}
	
	else if(yr > 1990){
		tmp <- matrix(NA, n, 46)
		st <- c()
		j <- 1
		for(i in seq(1, n, by = 2)){
			tmp1 <- as.character(hf[i,])
			tmp2 <- as.character(hf[i+1,])
			if(hf[i,length(hf[i,])] == 0) st[j] <- "live"
			else if(hf[i,length(hf[i,])] == 2){ st[j] <- "dead" }	
			tmp[j, ] <- c(tmp1[-length(tmp1)], tmp2[7:(length(tmp2)-1)])
			j <- j + 1
		}
	
		tmp <- data.frame(cruise = as.factor(tmp[,1]), tow = as.numeric(tmp[,2]), stratum = as.numeric(tmp[,3]), 
		                  tms = as.factor(paste(tmp[,4],tmp[,5], sep="")), depth = as.numeric(tmp[,6]), state = st, h5 = as.numeric(tmp[,7]), 
		                  h10 = as.numeric(tmp[,8]), h15 = as.numeric(tmp[,9]), h20 = as.numeric(tmp[,10]), h25 = as.numeric(tmp[,11]), 
		                  h30 = as.numeric(tmp[,12]), h35 = as.numeric(tmp[,13]), h40 = as.numeric(tmp[,14]), h45 = as.numeric(tmp[,15]), 
		                  h50 = as.numeric(tmp[,16]), h55 = as.numeric(tmp[,17]), h60 = as.numeric(tmp[,18]), h65 = as.numeric(tmp[,19]), 
		                  h70 = as.numeric(tmp[,20]), h75 = as.numeric(tmp[,21]), h80 = as.numeric(tmp[,22]), h85 = as.numeric(tmp[,23]), 
		                  h90 = as.numeric(tmp[,24]),h95 = as.numeric(tmp[,25]), h100 = as.numeric(tmp[,26]), h105 = as.numeric(tmp[,27]),
		                  h110 = as.numeric(tmp[,28]), h115 = as.numeric(tmp[,29]), h120 = as.numeric(tmp[,30]), h125 = as.numeric(tmp[,31]), 
		                  h130 = as.numeric(tmp[,32]), h135 = as.numeric(tmp[,33]), h140 = as.numeric(tmp[,34]), h145 = as.numeric(tmp[,35]), 
		                  h150 = as.numeric(tmp[,36]), h155 = as.numeric(tmp[,37]), h160 = as.numeric(tmp[,38]), h165 = as.numeric(tmp[,39]), 
		                  h170 = as.numeric(tmp[,40]), h175 = as.numeric(tmp[,41]), h180 = as.numeric(tmp[,42]), h185 = as.numeric(tmp[,43]), 
		                  h190 = as.numeric(tmp[,44]), h195 = as.numeric(tmp[,45]), h200 = as.numeric(tmp[,46]),stringsAsFactors = F)	
		
		tmp <- tmp[!is.na(tmp[,1]),]
	}
		
	tmp
}
