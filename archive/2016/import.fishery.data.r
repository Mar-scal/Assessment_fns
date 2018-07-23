
####  Commented and checked by DK starting on July 27, 2015.

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
#      Internal 1:  get.old.logs
#      Internal 2: get.new.logs
##
###############################################################################################################

#Arguments
# years,
# export=F
# get.data='dump'




# source("fn/import.fishery.data.r")

import.fishery.data <- function(years,export=F,get.data='dump'){


	require(splancs)  || stop("Error you need to install splancs... thanks!")
  
	if(missing(years)) years<- c(1955:1959,1961:2008)
	
	if(sum(years>2008)>0)
	  {
		  new.yrs<- years[years>2008]
		
		  # This loop grabs the log data for all years after 2008 using the internal function get.new.logs.
		  for (i in 1:length(new.yrs))
		    {
    			newlog.dat<-get.new.logs(new.yrs[i])
    			print(new.yrs[i])
    			if(i==1)logdata<-newlog.dat
    			if(i>1) logdata<-merge(logdata,newlog.dat,all=T)
    		} # end for (i in 1:length(new.yrs))
		
		names(logdata)<-gsub("_",".",names(logdata))
		logdata$date<- as.Date(logdata$DATE.FISHED, "%d-%b-%y")
		logdata$year<-as.numeric(format(logdata$date, "%Y"))
		logdata$bank<-NA
		logdata$bank[logdata$FISHING.AREA=="27A"]<-"GBa"
		logdata$bank[logdata$FISHING.AREA=="27B"]<-"GBb"
		logdata$bank[logdata$FISHING.AREA=="25A"]<-"Sab"
		logdata$bank[logdata$FISHING.AREA=="25A"& logdata$NAFO.UNIT.AREA=="4WE"]<-"Mid"
		logdata$bank[logdata$FISHING.AREA=="26C"]<-"Ger"
		logdata$bank[logdata$FISHING.AREA=="26A"]<-"BBn"
		logdata$bank[logdata$FISHING.AREA=="26B"]<-"BBs"
		logdata$bank[logdata$FISHING.AREA=="25B"]<-"Ban"
		logdata$trip.id<-with(logdata,paste(year,VR.NUMBER,TRIP.ID,sep='.'))
		
		# calculate effort and CPUE
		logdata$h <- logdata$NO.TOWS.PER.WATCH * logdata$AVG.TOW.TIME / 60
		logdata$m <- logdata$NO.RAKES.FISHED * logdata$GEAR.SIZE.FEET * 0.3048
		logdata$hm <- logdata$m * logdata$h
		logdata$kg.h <- logdata$PRORATED.RPTD.WEIGHT.KGS / logdata$h
		logdata$kg.hm <- logdata$PRORATED.RPTD.WEIGHT.KGS / logdata$hm
				
		assign("new.log.dat", logdata, pos = 1)
	}
		
	
	
	
	if(sum(years<2009)>0){
		old.yrs<- years[years<2009]
		
		# import and manipulate data
		for (i in 1:length(old.yrs))
		  {
  			oldlog.dat<-get.old.logs(old.yrs[i])
  			print(old.yrs[i])
  			if(i==1)log.ts.dat<-oldlog.dat
  			if(i>1) log.ts.dat<-merge(log.ts.dat,oldlog.dat,all=T)
		  } # end for (i in 1:length(old.yrs))
	
		log.ts.dat$trip.id<-with(log.ts.dat,paste(year,vesid,tripnum,sep='.'))
		log.ts.dat$month <- as.character(months(as.Date(log.ts.dat$date)))
		
		# calculate prorated daily catch (in kg) for both fleets
		log.ts.dat$pro.repwt<-NA
		log.ts.dat$trip.id<-with(log.ts.dat,paste(year,vesid,tripnum,sep='.'))
		totalbags<-with(log.ts.dat,tapply(numbags,trip.id,sum))
		bag.dat<-data.frame(totalbags,trip.id=names(totalbags))
		log.ts.dat<-merge(log.ts.dat, bag.dat)
		log.ts.dat$pro.repwt<-round(with(log.ts.dat,numbags/totalbags*landing/ 2.2046))
		log.ts.dat$pro.repwt[log.ts.dat$fleet=="FT"]<-round(log.ts.dat$day.land[log.ts.dat$fleet=="FT"] / 2.2046)
		log.ts.dat$pro.repwt[log.ts.dat$year==2008&log.ts.dat$fleet=="WF"]<-round(log.ts.dat$day.land[log.ts.dat$year==2008&log.ts.dat$fleet=="WF"] / 2.2046)
		
		
		# calculate effort and CPUE
		log.ts.dat$h <- with(log.ts.dat, numtow * avgtime / 60)
		log.ts.dat$m <- with(log.ts.dat, gear.ft * 0.3048)
		log.ts.dat$hm <- with(log.ts.dat, m * numtow * avgtime / 60)
		log.ts.dat$hm[log.ts.dat$hm==0]<-NA
		log.ts.dat$crhm <- with(log.ts.dat, numcrew * m * numtow * avgtime / 60)
		log.ts.dat$kg.h <- log.ts.dat$pro.repwt / log.ts.dat$h
		log.ts.dat$kg.hm <- log.ts.dat$pro.repwt / log.ts.dat$hm
		log.ts.dat$kg.crhm <- log.ts.dat$pro.repwt / log.ts.dat$crhm
	
		
		#Write1
		if(export)write.table(log.ts.dat, file = paste("data/RawFisheryData",min(old.yrs),"-",max(old.yrs),".txt",sep=""),sep="\t", row.names = F, col.names = T)
	
		assign("old.log.dat", log.ts.dat, pos = 1)
	}

}


get.old.logs <- function(year=2008,path="Y:/Data/Fishery/Logdata/",export=T, Win = T){
	

	
	if(year<1999){
		log.file<-paste(path,"log",year,".txt",sep="")
		logWF<-parse.log(log.file,fleet="WF",year=year)
			
		# assign bank by nafo
		logWF$bank[logWF$nafo=='5ZE'|logWF$nafo=='5ZC']<-'GB'
		logWF$bank[logWF$nafo=='4XP'|logWF$nafo=='4XO']<-'BB'
		logWF$bank[logWF$nafo=='4WJ'|logWF$nafo=='4WF'|logWF$nafo=='4WL']<-'Sab'
		logWF$bank[logWF$nafo=='4XQ']<-'Ger'
		logWF$bank[logWF$nafo=='4WE']<-'Mid'
		logWF$bank[logWF$nafo=='4VC']<-'Ban'
		logWF$bank[logWF$nafo=='3PS']<-'SPB'

		logWF$fleet<-"WF"
		log1<-logWF
	}
	
	if(year>1998){
		
		# reg = all banks except "GBb" & "BBs"
		log.file1<-paste(path,"log",year,"reg.txt",sep="")
		logWF1<-parse.log(log.file1,fleet="WF",year=year)
		if(year>2007)logWF1<-parse.log(log.file1,fleet="FT",year=year)
		
		# spec = "GBb" & "BBs"
		log.file2<-paste(path,"log",year,"spec.txt",sep="")
		logWF2<-parse.log(log.file2,fleet="WF",year=year)
		if(year>2007)logWF2<-parse.log(log.file2,fleet="FT",year=year)
		
		# assign bank by nafo
		logWF1$bank[logWF1$nafo=='5ZE'|logWF1$nafo=='5ZC']<-'GBa'
		logWF1$bank[logWF1$nafo=='4XP'|logWF1$nafo=='4XO']<-'BBn'
		logWF2$bank[logWF2$nafo=='5ZE'|logWF2$nafo=='5ZC']<-'GBb'
		logWF2$bank[logWF2$nafo=='4XP'|logWF2$nafo=='4XO']<-'BBs'
		
		logWF<-merge(logWF1,logWF2,all=T)
		
		logWF$bank[logWF$nafo=='4WJ'|logWF$nafo=='4WF']<-'Sab'
		logWF$bank[logWF$nafo=='4XQ']<-'Ger'
		logWF$bank[logWF$nafo=='4WE']<-'Mid'
		logWF$bank[logWF$nafo=='4VC']<-'Ban'
		logWF$bank[logWF$nafo=='3PS']<-'Sp'
		
		logWF$fleet<-"WF"
		log1<-logWF
		
		# Freezer trawlers
		if(year>2001){
			
			# reg = all banks except "GBb" & "BBs"
			log.file1<-paste(path,"logFT",year,"reg.txt",sep="")
			logFT<-parse.log(log.file1,fleet="FT",year=year)
			
			# assign bank by nafo
			logFT$bank[logFT$nafo=='5ZE'|logFT$nafo=='5ZC']<-'GBa'
			logFT$bank[logFT$nafo=='4XP'|logFT$nafo=='4XO']<-'BBn'
			
			if(year>2004){
				# spec = "GBb" & "BBs"
				log.file2<-paste(path,"logFT",year,"spec.txt",sep="")
				logFT2<-parse.log(log.file2,fleet="FT",year=year)
				
				# assign bank by nafo
				logFT2$bank[logFT2$nafo=='5ZE'|logFT2$nafo=='5ZC']<-'GBb'
				logFT2$bank[logFT2$nafo=='4XP'|logFT2$nafo=='4XO']<-'BBs'
				
				logFT<-merge(logFT,logFT2,all=T)
			}
			
			logFT$bank[logFT$nafo=='4WJ'|logFT$nafo=='4FT']<-'Sab'
			logFT$bank[logFT$nafo=='4XQ']<-'Ger'
			logFT$bank[logFT$nafo=='4WE']<-'Mid'
			logFT$bank[logFT$nafo=='4VC']<-'Ban'
			logFT$bank[logFT$nafo=='3PS']<-'Sp'
			
			logFT$fleet<-"FT"
			log1<-merge(logWF,logFT,all=T)
		}
		
	}
		
	logf<-log1[order(log1$vesid,log1$tripnum),]
	if(year>2007)logf$numbags<-NA
	
	if(export==T){
	  
	  #Write2
		write.table(logf, file = paste("Y:/Data/Fishery/logexport/log",year,".csv",sep=""),sep=",", row.names = F, col.names = T)
	}
logf
}

parse.log <- function(log, fleet = "WF", year){
require(chron)
source("Y:/Offshore scallop/Assessment/2014/r/fn/convert.dd.dddd.r")

  #Read1
	log <- read.csv(log,header=F)
	
	n <- nrow(log)
	
	tmp <- matrix(NA, n, 18)
	
	if(year>1997){
		for(i in 1:n){
			tmp1 <- as.character(log[i,])
			
			if(fleet == "WF"){
				tmp[i, ] <- c(paste(substr(tmp1,1,2), "/", substr(tmp1,3,4), "/", substr(tmp1,5,8), sep=""), substr(tmp1,9,9), substr(tmp1,10,11), substr(tmp1,12,15), substr(tmp1,16,17), substr(tmp1,18,22), substr(tmp1,23,29), substr(tmp1,30,36), substr(tmp1,40,41), substr(tmp1,42,42), substr(tmp1,43,43), substr(tmp1,44,46), substr(tmp1,47,47), substr(tmp1,54,55), substr(tmp1,56,58), substr(tmp1,61,62), substr(tmp1,63,64), substr(tmp1,69,74))
				}
			else if(fleet == "FT"){
					tmp[i, ] <- c(paste(substr(tmp1,1,2), "/", substr(tmp1,3,4), "/", substr(tmp1,5,8), sep=""), substr(tmp1,9,9), substr(tmp1,10,11), substr(tmp1,12,15), substr(tmp1,16,17), substr(tmp1,18,22), substr(tmp1,23,29), substr(tmp1,30,36), substr(tmp1,40,41), substr(tmp1,42,42), substr(tmp1,43,43), substr(tmp1,44,46), substr(tmp1,47,47), substr(tmp1,54,55), substr(tmp1,56,60), substr(tmp1,63,64), substr(tmp1,65,66), substr(tmp1,71,76))
			}
		}
		tmp <- data.frame(year=year,date = as.Date(as.character(tmp[,1]), format='%d/%m/%Y'), day.fishing = as.numeric(tmp[,2]), tripnum = as.numeric(tmp[,3]), vesid = as.factor(tmp[,4]), gear.ft = as.numeric(tmp[,5]), land.port = as.factor(tmp[,6]), lat = as.numeric(tmp[,7]), lon = as.numeric(tmp[,8]), depth = as.numeric(tmp[,9]), bottom = as.factor(tmp[,10]), day.fished = as.numeric(tmp[,11]), nafo = as.factor(tmp[,12]), datclass = as.factor(tmp[,13]), numcrew = as.numeric(tmp[,14]), numbags = as.numeric(tmp[,15]), numtow = as.numeric(tmp[,16]), avgtime = as.numeric(tmp[,17]), landing = as.numeric(tmp[,18]))
		
		if(fleet == "FT") names(tmp)[16] <- "day.land"
		
		tmp$lat <- convert.dd.dddd(tmp$lat)
		
		tmp$lon <- -1 * convert.dd.dddd(tmp$lon)
	}
	
	if(year<1998){
		for(i in 1:n){
			tmp1 <- as.character(log[i,])
			
			tmp[i, ] <- c(paste(substr(tmp1,1,2), "/", substr(tmp1,3,4), "/19", substr(tmp1,5,6), sep=""), substr(tmp1,7,7), substr(tmp1,8,9), substr(tmp1,10,13), substr(tmp1,14,15), substr(tmp1,16,20), substr(tmp1,21,26), substr(tmp1,27,32), substr(tmp1,36,37), substr(tmp1,38,38), substr(tmp1,39,39), substr(tmp1,40,42), substr(tmp1,43,43), substr(tmp1,50,51), substr(tmp1,52,54), substr(tmp1,57,58), substr(tmp1,59,60), substr(tmp1,66,70))
		}
		
		tmp <- data.frame(year=year,date = as.Date(as.character(tmp[,1]), format='%d/%m/%Y'), day.fishing = as.numeric(tmp[,2]), tripnum = as.numeric(tmp[,3]), vesid = as.factor(tmp[,4]), gear.ft = as.numeric(tmp[,5]), land.port = as.factor(tmp[,6]), lat = as.numeric(tmp[,7]), lon = as.numeric(tmp[,8]), depth = as.numeric(tmp[,9]), bottom = as.factor(tmp[,10]), day.fished = as.numeric(tmp[,11]), nafo = as.factor(tmp[,12]), datclass = as.factor(tmp[,13]), numcrew = as.numeric(tmp[,14]), numbags = as.numeric(tmp[,15]), numtow = as.numeric(tmp[,16]), avgtime = as.numeric(tmp[,17]), landing = as.numeric(tmp[,18]))
		
		if(fleet == "FT") names(tmp)[15] <- "day.land"
		
		tmp$lat <- convert.dd.dddd(tmp$lat)
		
		tmp$lon <- -1 * convert.dd.dddd(tmp$lon)
	}
	
	
	
	
	
	tmp
}



get.new.logs <- function(year=2008,export=F, Win = T, get.data='dump'){

# Function for connecting to MARFIS commercial landings db
# Extract subset of info from Offshore Scallop Views created by Jerry Black
#		OFFSHORE_SCALLOP_LOG_2008
#		OFFSHORE_SCALLOP_SLIP_2008
#		
#		Created by Ian Jonsen 14Mar2008
#
	


	if(get.data=='marfis'){
		require(RODBC) || stop("Package RODBC cannot be found")

		if(Win) chan <- odbcConnect("mflib", "jonseni", "dbmt587d", case="oracle")
		else if(Win == F){ 
			chan <- odbcConnect("jonseni2", "jonseni", "dbmt587d", case="oracle")
		}
		if(year<2010) qu.slip <- paste("select * from marfis.P_OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-0",year-2000,"'",sep="")
		if(year<2010) qu.log <- paste("select * from marfis.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-0",year-2000,"'",sep="")
		if(year>=2010) qu.slip <- paste("select * from marfis.P_OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-",year-2000,"'",sep="")
		if(year>=2010) qu.log <- paste("select * from marfis.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-",year-2000,"'",sep="")
		slip.dat <- sqlQuery(chan, qu.slip)
		log.dat <- sqlQuery(chan, qu.log)
		odbcCloseAll()

	}
	if(get.data=='dump'){
	  
	  #Read2
		log.dat<-read.csv(paste("Y:\\Data\\Fishery\\log",year,".csv",sep=""))
		
		#Read3
		slip.dat<-read.csv(paste("Y:\\Data\\Fishery\\slip",year,".csv",sep=""))
	}

	boats.ft <- c(105736,105912,105457,106604,106605,102056)
	boats.wet <- c(1555,4062,100199,101965,4031,1516,1518,4051,1548,5409)
	log.dat$FLEET <- as.factor(ifelse(log.dat$VR_NUMBER %in% boats.ft,"FT","WF"))
	log.dat$GEAR_SIZE_FEET<-slip.dat[match(log.dat$MON_DOC_ID, slip.dat$MON_DOC_ID),]$GEAR_SIZE_FEET
	landing<-with(slip.dat,tapply(SLIP_WEIGHT_LBS,MON_DOC_ID,sum))
#	browser()
	date.land<-as.Date(with(slip.dat,tapply(LANDING_DATE_TIME,MON_DOC_ID,unique)),"%d-%b-%y")
	log.dat<-merge(log.dat,data.frame(MON_DOC_ID=as.numeric(names(landing)),landing,date.land),all=T)

		
	if(export==T){
	  
	  #Write3
		write.table(log.dat, file = paste("Y:\\Fishery data\\Data\\log",year,".csv",sep=""),sep=",", row.names = F, col.names = T)
		
	  #Write4
	  write.table(slip.dat, file = paste("Y:\\Fishery data\\Data\\slip",year,".csv",sep=""),sep=",", row.names = F, col.names = T)
	}
	
	log.dat
	


}

	