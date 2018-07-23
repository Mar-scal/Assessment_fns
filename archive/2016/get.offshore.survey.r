################################################################################################################
##### GETS OFFSHORE DATA FROM ORACLE DATABASE
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
#      1: fn/convert.dd.dddd.r
##
###############################################################################################################

# ARGUMENTS
# Template:  (T/F) Default is T
# code.tables:   (T/F) Default is F


# where did the .ar come from in the below...
# source("fn/get.offshore.survey.ar.r")

### GETS OFFSHORE DATA FROM ORACLE DATABASE
### currently standardized live shell height frequency

# DK August 20, 2015, function call altered so DB credentials are entered directly into function call.
get.offshore.survey <- function(Template=F,code.tables=F){
	require(RODBC) || stop("Package RODBC cannot be found")
	
	chan <- odbcConnect("ptran64", "keithd", "Narrow24",believeNRows=FALSE)
#	chan <- odbcConnect("bank.canso3", "scaloff", "fgb256k")

  
	#####################################################################################################################
  # Jessica has new views for these calls, ,all this prorating is not necessary anymore as she's taken care of it in SQL
  # Key is to import those tables and send it out of this file looking identical!  
	######################################################################################################################
  
  qu.tow <- "select * from SCALOFF.OSTOWS"
	qu.surv <- "select * from SCALOFF.OSSURVEYS"
	qu.cruise <- "select * from SCALOFF.OSCRUISES"
	qu.fs <- "select * from SCALOFF.OSHFREQSAMPLES"
	qu.hf <- "select * from SCALOFF.OSHEIGHTFREQ"
	qu.samp <- "select * from SCALOFF.OSSAMPLES"
	qu.incr <- "select * from SCALOFF.OSINCREMENTS"
	
	if(code.tables==T){
		qu.code <- "select * from ALL_TABLES"
		tables <- sqlQuery(chan, qu.code)
		codetables<-c(tables$TABLE_NAME[tables$OWNER=="SCALOFF"][grep("CODES",tables$TABLE_NAME[tables$OWNER=="SCALOFF"])],tables$TABLE_NAME[tables$OWNER=="SCALOFF"][grep("TYPES",tables$TABLE_NAME[tables$OWNER=="SCALOFF"])])
		codes<-list()
		for(i in 1:length(codetables)){
			codes[[i]]<-sqlQuery(chan, paste("select * from",codetables[i]))
		
			#Write1
				write.csv(codes[[i]],paste(codetables[i],".csv",sep=''),row.names=F)
		}
	}

	# Grab the SQL data from the respective database tables
	tows <- sqlQuery(chan, qu.tow)
	surv <- sqlQuery(chan, qu.surv)
	cruise <- sqlQuery(chan, qu.cruise)
	hf <- sqlQuery(chan, qu.hf)
	fs <- sqlQuery(chan, qu.fs)
	samp <- sqlQuery(chan, qu.samp)
	incr <- sqlQuery(chan, qu.incr)
	odbcCloseAll()
	
	# Combine surv and tows tables.
	tows<-merge(surv[,c(1,2,6)],tows)
	
	
	if(Template==T){
		shf<-merge(fs[,c(1:2,6)],hf[,-1])
		Lbask<-subset(shf,LIVECODE=="L"&CONTAINER_TYPE_ID==1,-c(1,3:4))
		Dbask<-subset(shf,LIVECODE=="D"&CONTAINER_TYPE_ID==1,-c(1,3:4))
		Lbuck<-subset(shf,LIVECODE=="L"&CONTAINER_TYPE_ID==2,-c(1,3:4))
		Dbuck<-subset(shf,LIVECODE=="D"&CONTAINER_TYPE_ID==2,-c(1,3:4))
		names(Lbask)[3]<-"LIVE_QTY_BASKET"
		names(Dbask)[3]<-"DEAD_QTY_BASKET"
		names(Lbuck)[3]<-"LIVE_QTY_BUCKET"
		names(Dbuck)[3]<-"DEAD_QTY_BUCKET"
		loaderSHF<-merge(merge(merge(Lbask,Dbask,all=T),Lbuck,all=T),Dbuck,all=T)
		loaderSHF<-merge(tows[c("CRUISE","SURVEY_NAME","MGT_AREA_CD","TOW_DATE","TOW_SEQ","TOW_NO")],loaderSHF,all=T)
	
		#Write2
			write.csv(loaderSHF,"LoaderTemplateHF.csv",row.names=F)
	}
		
	# proration of sampling (buckets and baskets)
	hfLive<-rbind(subset(hf,LIVECODE=="L"),data.frame(HEIGHT_FREQ_SEQ=NA, HFREQ_SAMPLE_SEQ=NA, LIVECODE="L", BIN_ID=seq(0,195,5), NUMBER_IN_BIN=0))
	names(hfLive)[5]<-'h'
	hfLive<-subset(reshape(hfLive[order(hfLive$BIN_ID),],v.names='h',timevar='BIN_ID',idvar='HFREQ_SAMPLE_SEQ',direction='wide'),!is.na(HEIGHT_FREQ_SEQ))
	hfLive[is.na(hfLive)]<-0
	fs$scaler<-fs$SAMPLED/fs$TOTAL
	shfLive<-merge(hfLive,fs,all=T)
	shfbin<-as.numeric(substr(names(shfLive),3,nchar(names(shfLive))))
	shfLive[,which(shfbin==0):which(shfbin==195)]<-sweep(shfLive[,which(shfbin==0):which(shfbin==195)],1,FUN='/',shfLive$scaler)
	shfLive<-aggregate(shfLive[,which(shfbin==0):which(shfbin==195)],by=list(shfLive$TOW_SEQ),FUN=sum,na.rm=T)
	shfLive[is.na(shfLive)]<-0
	shfLive$state='live'
	names(shfLive)[1]<-'TOW_SEQ'
	
	shfLive.dat<-merge(tows,shfLive)
	
	hfDead<-rbind(subset(hf,LIVECODE=="D"),data.frame(HEIGHT_FREQ_SEQ=NA, HFREQ_SAMPLE_SEQ=NA, LIVECODE="D", BIN_ID=seq(0,195,5), NUMBER_IN_BIN=0))
	names(hfDead)[5]<-'h'
	hfDead<-subset(reshape(hfDead[order(hfDead$BIN_ID),],v.names='h',timevar='BIN_ID',idvar='HFREQ_SAMPLE_SEQ',direction='wide'),!is.na(HEIGHT_FREQ_SEQ))
	hfDead[is.na(hfDead)]<-0
	fs$scaler<-fs$SAMPLED/fs$TOTAL
	shfDead<-merge(hfDead,fs,all=T)
	shfbin<-as.numeric(substr(names(shfDead),3,nchar(names(shfDead))))
	shfDead[,which(shfbin==0):which(shfbin==195)]<-sweep(shfDead[,which(shfbin==0):which(shfbin==195)],1,FUN='/',shfDead$scaler)
	shfDead<-aggregate(shfDead[,which(shfbin==0):which(shfbin==195)],by=list(shfDead$TOW_SEQ),FUN=sum,na.rm=T)
	shfDead[is.na(shfDead)]<-0
	shfDead$state='dead'
	names(shfDead)[1]<-'TOW_SEQ'
	
	shfDead.dat<-merge(tows,shfDead)
	
	surv.dat<-merge(shfLive.dat,shfDead.dat,all=T)
	survbin<-as.numeric(substr(names(surv.dat),3,nchar(names(surv.dat))))
	
	
	# Industry report
	ind.rep<-with(shfLive.dat,data.frame(YEAR=format(TOW_DATE,"%Y"),BANK=MGT_AREA_CD,TOW_NO,START_LAT,START_LON,END_LAT,END_LON,DEPTH_F,preL=rowSums(shfLive.dat[,which(survbin==0):which(survbin==65)]),recL=rowSums(shfLive.dat[,which(survbin==70):which(survbin==95)]),comL=rowSums(shfLive.dat[,which(survbin==100):which(survbin==195)]),
  	preC=rowSums(shfDead.dat[,which(survbin==0):which(survbin==65)]),recC=rowSums(shfDead.dat[,which(survbin==70):which(survbin==95)]),comC=rowSums(shfDead.dat[,which(survbin==100):which(survbin==195)]),COMMENTS))
	
	#Write3
	write.csv(ind.rep,"IndustryReport2.csv",row.names=F)
	
	# Standardization
	surv.dat[,which(survbin==0):which(survbin==195)]<-sweep(surv.dat[,which(survbin==0):which(survbin==195)],1,FUN='*',surv.dat$DIS_COEF)
	
	#Source1
	source("fn/convert.dd.dddd.r")
	surv.dat$slat<-convert.dd.dddd(surv.dat$START_LAT)
	surv.dat$slon<-convert.dd.dddd(surv.dat$START_LON)
	surv.dat$elat<-convert.dd.dddd(surv.dat$END_LAT)
	surv.dat$elon<-convert.dd.dddd(surv.dat$END_LON)
	# Takes the mid-point of the tow as strata
	surv.dat$lon<-with(surv.dat,apply(cbind(elon,slon),1,mean))
	surv.dat$lat<-with(surv.dat,apply(cbind(elat,slat),1,mean))
	

	surv.dat$year<-as.numeric(format(surv.dat$TOW_DATE,'%Y'))
	surv.dat$depth<-surv.dat$DEPTH_F*1.8288
	# This is the OSSAMPLES_VW view.
	MWs<-merge(subset(surv.dat,state=='live',c('TOW_SEQ','MGT_AREA_CD','TOW_NO','START_LAT','START_LON','END_LAT','END_LON','DEPTH_F','year','lon','lat','depth','CRUISE')),subset(samp,!is.na(WET_MEAT_WGT),c('TOW_SEQ','SCALLOP_NUM','WET_MEAT_WGT','SHELL_HEIGHT')))
	# Need this too, all in OLIVERES_VW
	pos=subset(surv.dat,state=='live',c('MGT_AREA_CD','TOW_NO','START_LAT','START_LON','END_LAT','END_LON','DEPTH_F','year','lon','lat','depth'))
	list(SHF=surv.dat,MWs=MWs,pos=pos)
}
