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
# direct:  Directory to call functions and data from.  Default = "Y:/Offshore scallop/Assessment/"

# source("C:/Assessment/2012/r/SummerSurveyDAta.r")

# DK Aug 13, 2015:  To be clear this is pointing to Y:\Offshore scallop\Assessment\2012\r\
# Note DK August 19, 2015:  I did not add a lot of detailed comments to this file, if interested in that look at the 2011 version of this
# They are essentially identical, just calling different flat files.

getSummerSurveyData2012<-function(MWs=T,direct = "Y:/Offshore scallop/Assessment/")
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
  loc <- paste(direct,"Data/Survey_data/2012/Summer/",sep="")
  
  #Read1 
	 tmp.1<-read.csv(paste(loc,"TE14GBahf60.csv",sep=""))
	 #Read2
	 tmp.2<-read.csv(paste(loc,"TE14GBahf120.csv",sep=""))
	 #Read3
	 tmp.3<-read.csv(paste(loc,"TE14GBahf180.csv",sep=""))
	 #Read4
	 tmp.4<-read.csv(paste(loc,"TE14GBahf227.csv",sep=""))
	 #Read5
	 tmp.5<-read.csv(paste(loc,"TE14GBbhf.csv",sep=""))
	 
	 tmp.t1<-na.omit(data.frame(t(tmp.1[,-1])[,c(1,4:24)]))
	 tmp.t2<-na.omit(data.frame(t(tmp.2[,-1])[,c(1,4:24)]))
	 tmp.t3<-na.omit(data.frame(t(tmp.3[,-1])[,c(1,4:24)]))
	 tmp.t4<-na.omit(data.frame(t(tmp.4[,-1])[,c(1,4:24)]))
	 tmp.t5<-na.omit(data.frame(t(tmp.5[,-1])[,c(1,4:24)]))
	 tmp.t<-rbind(tmp.t1,tmp.t2,tmp.t3,tmp.t4,tmp.t5)
	 
	 tmp.dat<-cbind(subset(tmp.t,X22%in%c(0,2),1:21),subset(tmp.t,X22%in%c(1,3),2:21))
	 names(tmp.dat)<-c('tow',paste('h',seq(5,200,5),sep=''))
	 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
	 tmp.dat$year<-2012
	
	 #Read6
	TE14pos<-read.csv(paste(loc,"TE14positions.csv",sep=""))
	TE14pos$year<-2012
	#Source2  source("fn/convert.dd.dddd.r") Convert lat/lon to decimal degrees.
	TE14pos$lon<-with(TE14pos,apply(cbind(convert.dd.dddd(elon),convert.dd.dddd(slon)),1,mean))*-1
	TE14pos$lat<-with(TE14pos,apply(cbind(convert.dd.dddd(elat),convert.dd.dddd(slat)),1,mean))
	TE14pos$depth<-TE14pos$depth.f*1.8288
	
	
	 ## Get Distance coefficients
	#setwd("C:/Assessment/2012/r") # DK August 19, 2015 Removed this comment.
	#Source1 source("fn/getdis.r")
	#Read7
	GBaDis<-dist.coef(1:227,path=paste(loc,"logfiles/GBa/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)
	GBbDis<-dist.coef(601:630,path=paste(loc,"logfiles/GBb/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)

	GBa.dis<-with(GBaDis[[1]],data.frame(bank='GBa',tow=tow,dis=dis,date=date,brg=brg))
	GBb.dis<-with(GBbDis[[1]],data.frame(bank='GBb',tow=tow,dis=dis,date=date,brg=brg))
	
	dis<-rbind(GBa.dis,GBb.dis)
	
	
	### put it all together
	#browser()
	TE14<-merge(merge(TE14pos,tmp.dat,all=T),dis,all=T)
	
	#Source2  source("fn/convert.dd.dddd.r") Convert lat/lon to decimal degrees.
	TE14$slat<-convert.dd.dddd(TE14$slat)
	TE14$slon<-convert.dd.dddd(TE14$slon)*-1
	TE14$elat<-convert.dd.dddd(TE14$elat)
	TE14$elon<-convert.dd.dddd(TE14$elon)*-1
	TE14$cruise<-"TE14"
	TE14 <- cbind(TE14[,c('year','cruise','bank','date','tow','stratum','slat','slon','elat','elon','depth','state','dis','brg')], sweep(TE14[,paste('h',seq(5,200,5),sep='')],1,FUN='*',TE14$dis))
	
	TE14$random<-!is.na(TE14$stratum)
	
	####### Meat weights
	if(MWs){
	  #Read8
		mw<-read.csv(paste(loc,"TE14mtwt_no check.csv",sep=""))
		
		MWs<-merge(TE14pos,mw,all=F)
		
		#Source2  source("fn/convert.dd.dddd.r") Convert lat/lon to decimal degrees.
		MWs$lon<-with(MWs,apply(cbind(convert.dd.dddd(elon),convert.dd.dddd(slon)),1,mean))*-1
		MWs$lat<-with(MWs,apply(cbind(convert.dd.dddd(elat),convert.dd.dddd(slat)),1,mean))
		MWs$ID<-paste(MWs$cruise,MWs$tow,sep='.')
	}
	TE14pos<-merge(TE14pos,dis,all=T)
	
	list(SHF=TE14,MWs=MWs,pos=TE14pos,tracks=list(GBa=GBaDis[[2]],GBb=GBbDis[[2]]))
}

 




