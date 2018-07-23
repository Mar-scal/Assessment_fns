################################################################################################################
##### This is the script for accessing the 2011 summer survey data for each offshore area and combining it
##### into one dataframe for use elsewhere
################################################################################################################
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1:  SurveySUmmary.r
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    1: getdis.r   
#    2: convert.dd.dddd.r
##
###############################################################################################################

#Agruments:
# MWs:  Do we want to pull out the meat weight information (T/F) Default is T.
# direct:    Directory to look for the data in.  Default = "Y:/Offshore scallop/Assessment/Assessment_fns/"

# source("C:/Assessment/2011/r2/SummerSurveyDAta.r")

# DK Aug 13, 2015:  To be clear this is pointing to Y:\Offshore scallop\Assessment\2011\r2\

getSummerSurveyData2011<-function(MWs=T,direct = "Y:/Offshore scallop/Assessment/")
  {

	################# Summer Survey #################################
	
  require(PBSmapping)  || stop("Install PBSmapping Package") # Needed for getdis.r
  
  #DK Note August 19 2015 Load the necessary functions, an idea for later might be to have all sources point to 1 directory, fun.dir.  
  #fun.dir <- "d:/Offshore scallop/Assessment/2014/r"
  #Sounce1  
  #Sounce1  
  source(paste(direct,"Assessment_fns/getdis.r",sep="")) 
  #Source2
  source(paste(direct,"Assessment_fns/convert.dd.dddd.r",sep="")) 
  
  # DK Note August 19, 2015: Let's set local directory here this will make adjusting these csv calls later much easier if we change our folder structure
  loc <- paste(direct,"Data/Survey_data/2011/Summer/",sep="")
   
   #Read1 Following are just bringing in necessary survey data, all shell height related.
   tmp.1<-read.csv(paste(loc,"TE12GBahf60.csv",sep=""))
	 #Read2
   tmp.2<-read.csv(paste(loc,"TE12GBahf120.csv",sep=""))
	 #Read3
   tmp.3<-read.csv(paste(loc,"TE12GBahf180.csv",sep=""))
	 #Read4
   tmp.4<-read.csv(paste(loc,"TE12GBahf208.csv",sep=""))
	 #Read5
   tmp.5<-read.csv(paste(loc,"TE12GBbhf.csv",sep=""))
	 #Read6
   tmp.6<-read.csv(paste(loc,"TE12BBnhf.csv",sep=""))
	 
   # Combine them into one object
	 tmp.t1<-na.omit(data.frame(t(tmp.1[,-1])[,c(1,4:24)]))
	 tmp.t2<-na.omit(data.frame(t(tmp.2[,-1])[,c(1,4:24)]))
	 tmp.t3<-na.omit(data.frame(t(tmp.3[,-1])[,c(1,4:24)]))
	 tmp.t4<-na.omit(data.frame(t(tmp.4[,-1])[,c(1,4:24)]))
	 tmp.t5<-na.omit(data.frame(t(tmp.5[,-1])[,c(1,4:24)]))
	 tmp.t6<-na.omit(data.frame(t(tmp.6[,-1])[,c(1,4:24)]))
	 tmp.t<-rbind(tmp.t1,tmp.t2,tmp.t3,tmp.t4,tmp.t5,tmp.t6)
	 
	 # So this splits up tmp.t by column "X22" which is a code line, code's 0 and 2 are placed together, and codes 1 and 3 are places together
	 # we also remove the code column in the subset. 
	 # These codes distinquish whether the scallop was live or dead and if the shell height was > or < 100(ish)
	 tmp.dat<-cbind(subset(tmp.t,X22%in%c(0,2),1:21),subset(tmp.t,X22%in%c(1,3),2:21))
	 names(tmp.dat)<-c('tow',paste('h',seq(5,200,5),sep=''))
	 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
	 tmp.dat$year<-2011
	 
	#Read7 Brings in the tow track start and end locations.
	TE12pos<-read.csv(paste(loc,"TE12positions.csv",sep=""))
	TE12pos$year<-2011
	#Source2 source("fn/convert.dd.dddd.r") Converts the lat/lon to decimal degrees.
	TE12pos$lon<-with(TE12pos,apply(cbind(convert.dd.dddd(elon),convert.dd.dddd(slon)),1,mean))*-1
	TE12pos$lat<-with(TE12pos,apply(cbind(convert.dd.dddd(elat),convert.dd.dddd(slat)),1,mean))
	# Convert from fathoms to meters.
	TE12pos$depth<-TE12pos$depth.f*1.8288
	
	
	## Get Distance coefficients: 
	#Read8 Note that this opens over 200 hundred flat files with log track information in them
	## used to calculate distance covered by each tow.
	#Source1 source("fn/getdis.r")
	GBaDis<-dist.coef(1:208,path=paste(loc,"logfiles/GBa/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000)
	GBbDis<-dist.coef(601:630,path=paste(loc,"logfiles/GBb/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)
	BBnDis<-dist.coef(2201:2210,path=paste(loc,"logfiles/BBn/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=10000)
	
	# The dist.coef returns a list of 2 objects, first is a simple dataframe, second is output from PBSmapping, this step selects the
	# simple dataframe information (tow, distance, Date and bearing)
	GBa.dis<-with(GBaDis[[1]],data.frame(bank='GBa',tow=tow,dis=dis,date=date,brg=brg))
	GBb.dis<-with(GBbDis[[1]],data.frame(bank='GBb',tow=tow,dis=dis,date=date,brg=brg))
	BBn.dis<-with(BBnDis[[1]],data.frame(bank='BBn',tow=tow,dis=dis,date=date,brg=brg))

	dis<-rbind(GBa.dis,GBb.dis,BBn.dis)
	
	
	### put it all together
	#browser()
	TE12<-merge(merge(TE12pos,tmp.dat,all=T),dis,all=T)
	
	#Source2  source("fn/convert.dd.dddd.r") Convert lat/lon to decimal degrees.
	TE12$slat<-convert.dd.dddd(TE12$slat)
	TE12$slon<-convert.dd.dddd(TE12$slon)*-1
	TE12$elat<-convert.dd.dddd(TE12$elat)
	TE12$elon<-convert.dd.dddd(TE12$elon)*-1
	TE12$cruise<-"TE12"
	TE12 <- cbind(TE12[,c('year','cruise','bank','date','tow','stratum','slat','slon','elat','elon','depth','state','dis','brg')], sweep(TE12[,paste('h',seq(5,200,5),sep='')],1,FUN='*',TE12$dis))
	
	TE12$random<-!is.na(TE12$stratum)
	
	####### Meat weights, this is optional in this function (in the spring version we always grab the meat weights)
	if(MWs){
	  #Read9
		mw<-read.csv(paste(loc,"TE12mtwt_no check.csv",sep=""))
		# combine mw with the TE12pos object, only keep the rows for which we have information on both.
		MWs<-merge(TE12pos,mw,all=F)
		
		#Source2  source("fn/convert.dd.dddd.r") Convert lat/lon to decimal degrees.
		MWs$lon<-with(MWs,apply(cbind(convert.dd.dddd(elon),convert.dd.dddd(slon)),1,mean))*-1
		MWs$lat<-with(MWs,apply(cbind(convert.dd.dddd(elat),convert.dd.dddd(slat)),1,mean))
		MWs$ID<-paste(MWs$cruise,MWs$tow,sep='.')
	}
	# Combine the TE12pos and the tow track distance data, keep everything, aka an outer join
	TE12pos<-merge(TE12pos,dis,all=T)

	list(SHF=TE12,MWs=MWs,pos=TE12pos)
}

 




