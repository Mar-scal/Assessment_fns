################################################################################################################
##### This is the script for accessing the 2012 spring survey data for each offshore area and combining it
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
# direct:    Directory to look for the data in.  Default = "Y:/Offshore scallop/Assessment/Assessment_fns/"

# DK Aug 13, 2015:  To be clear this is pointing to Y:\Offshore scallop\Assessment\2012\r\

# Note DK August 19, 2015:  I did not add a lot of detailed comments to this file, if interested in that look at the 2011 version of this
# They are essentially identical, just calling different flat files.

getSpringSurveyData2012<-function(direct = "Y:/Offshore scallop/Assessment/")
  {

  #Sounce1  
  source(paste(direct,"Assessment_fns/getdis.r",sep="")) 
  #Source2
  source(paste(direct,"Assessment_fns/convert.dd.dddd.r",sep="")) 
  
# DK Note August 19, 2015: Let's set local directory here this will make adjusting these csv calls later much easier if we change our folder structure
loc <- paste(direct,"Data/Survey_data/2012/Spring/",sep="")
  
################# Banquereau #################################


 #Read1
 tmp.s<-read.csv(paste(loc,"TE13BanSeahf.csv",sep=""))
 #Read2
 tmp.i<-read.csv(paste(loc,"TE13BanIcehf.csv",sep=""))
 
 tmp.t<-data.frame(t(tmp.s[-(2:3),-1])[,1:22])
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:21),subset(tmp.t,X24%in%c(1,3),2:21))
 names(tmp.dat)<-c('tow',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2012
 
 TE13Banhf<-tmp.dat
 TE13Banhf$bank<-"Ban"

 tmp.t<-data.frame(t(tmp.i[-(2:3),-1])[,1:22])
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:21),subset(tmp.t,X24%in%c(1,3),2:21))
 names(tmp.dat)<-c('tow',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2012
 
 TE13BanIcehf<-tmp.dat
 TE13BanIcehf$bank<-"BanIce"
 
################# Middle #################################


#Read3
 tmp<-read.csv(paste(loc,"TE13Midhf.csv",sep=""))
 
 tmp.t<-na.omit(data.frame(t(tmp[-(2:3),-1])))
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:21),subset(tmp.t,X24%in%c(1,3),2:21))
 names(tmp.dat)<-c('tow',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2012
 
 TE13Midhf<-tmp.dat

 TE13Midhf$bank<-"Mid"

################# Sable #################################
 #Read4
 tmp.1<-read.csv(paste(loc,"TE13Sabhf60.csv",sep=""))
 #Read5
 tmp.2<-read.csv(paste(loc,"TE13Sabhf100.csv",sep=""))
 
 tmp.t1<-na.omit(data.frame(t(tmp.1[-(2:3),-1])[,1:22]))
 tmp.t2<-na.omit(data.frame(t(tmp.2[-(2:3),-1])[,1:22]))
 tmp.t<-rbind(tmp.t1,tmp.t2)
 
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:21),subset(tmp.t,X24%in%c(1,3),2:21))
 names(tmp.dat)<-c('tow',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2012
 
 TE13Sabhf<-tmp.dat

 TE13Sabhf$bank<-"Sab"

################# Browns North #################################
 #Read6
 tmp.1<-read.csv(paste(loc,"TE13BBnhf60.csv",sep=""))
 #Read7
 tmp.2<-read.csv(paste(loc,"TE13BBnhf100.csv",sep=""))
 
 tmp.t1<-na.omit(data.frame(t(tmp.1[-(2:3),-1])[,1:22]))
 tmp.t2<-na.omit(data.frame(t(tmp.2[-(2:3),-1])[,1:22]))
 tmp.t<-rbind(tmp.t1,tmp.t2)
 
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:21),subset(tmp.t,X24%in%c(1,3),2:21))
 names(tmp.dat)<-c('tow',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2012
 
 TE13BBnhf<-tmp.dat

 TE13BBnhf$bank<-"BBn"


################# German #################################
 #Read8
 tmp.1<-read.csv(paste(loc,"TE13Gerhf60.csv",sep=""))
 #Read9
 tmp.2<-read.csv(paste(loc,"TE13Gerhf80.csv",sep=""))
 
 tmp.t1<-na.omit(data.frame(t(tmp.1[-(2:3),-1])[,1:22]))
 tmp.t2<-na.omit(data.frame(t(tmp.2[-(2:3),-1])[,1:22]))
 tmp.t<-rbind(tmp.t1,tmp.t2)
 
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:21),subset(tmp.t,X24%in%c(1,3),2:21))
 names(tmp.dat)<-c('tow',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2012
 
 TE13Gerhf<-tmp.dat

 TE13Gerhf$bank<-"Ger"



################# Georges #################################
 #Read10
 tmp<-read.csv(paste(loc,"TE13GBhf.csv",sep=""))
 
 tmp.t<-na.omit(data.frame(t(tmp[-(2:3),-1])[,1:22]))
 
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:21),subset(tmp.t,X24%in%c(1,3),2:21))
 names(tmp.dat)<-c('tow',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2012
 
 TE13GBhf<-tmp.dat

 TE13GBhf$bank<-"GB"


TE13shf<-rbind(TE13Banhf,TE13BanIcehf,TE13Midhf,TE13Sabhf,TE13BBnhf,TE13Gerhf,TE13GBhf)

## Positions
#Read11
TE13pos<-read.csv(paste(loc,"TE13positions.csv",sep=""))
TE13Ice<-subset(TE13pos,bank=="Ban")
TE13Ice$bank<-"BanIce"
TE13pos<-rbind(TE13pos,TE13Ice)
TE13pos$year<-2012
TE13pos$lon<-with(TE13pos,apply(cbind(convert.dd.dddd(elon),convert.dd.dddd(slon)),1,mean))*-1
TE13pos$lat<-with(TE13pos,apply(cbind(convert.dd.dddd(elat),convert.dd.dddd(slat)),1,mean))




## Get Distance coefficients: 
#Read16 Note that this opens hundreds of flat files with log track information in them
## used to calculate distance covered by each tow.
#Source1 source("fn/getdis.r")    
ger.dis<-data.frame(bank='Ger',dist.coef(401:480,path=paste(loc,"logfiles/German/",sep=""),
                                         w=c(1:10,9:1),rule=8,smooth=T,plt=F)[[1]])
sab.dis<-data.frame(bank='Sab',dist.coef(1:100,path=paste(loc,"logfiles/Sable/",sep=""),
                                         w=c(1:10,9:1),rule=8,smooth=T,plt=F)[[1]])
mid.dis<-data.frame(bank='Mid',dist.coef(1:15,path=paste(loc,"logfiles/Middle/M",sep=""),
                                         w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=100)[[1]])
bbn.dis<-data.frame(bank='BBn',dist.coef(201:302,path=paste(loc,"logfiles/BBN/",sep=""),
                                         w=c(1:10,9:1),rule=8,smooth=T,plt=F)[[1]])
ban.dis<-data.frame(bank='Ban',dist.coef(901:936,path=paste(loc,"logfiles/Ban/",sep=""),
                                         w=c(1:10,9:1),rule=8,smooth=T,plt=F)[[1]])
banice.dis<-data.frame(bank='BanIce',dist.coef(901:936,path=paste(loc,"logfiles/Ban/",sep=""),
                                               w=c(1:10,9:1),rule=8,smooth=T,plt=F)[[1]])
gb.dis <-data.frame(bank='GB',dist.coef(301:340,path=paste(loc,"logfiles/Georges/",sep="")
                                        ,w=c(1:10,9:1),rule=8,smooth=T,plt=F)[[1]])

# Exclude the lat/long data as this messes up a merge which occurs below.
ger.dis<-with(ger.dis,data.frame(bank='Ger',tow=tow,dis=dis,date=date,brg=brg))
sab.dis<-with(sab.dis,data.frame(bank='Sab',tow=tow,dis=dis,date=date,brg=brg))
mid.dis<-with(mid.dis,data.frame(bank='Mid',tow=tow,dis=dis,date=date,brg=brg))
bbn.dis<-with(bbn.dis,data.frame(bank='BBn',tow=tow,dis=dis,date=date,brg=brg))
ban.dis<-with(ban.dis,data.frame(bank='Ban',tow=tow,dis=dis,date=date,brg=brg))
gb.dis<-with(gb.dis,data.frame(bank='GB',tow=tow,dis=dis,date=date,brg=brg))
banice.dis<-with(banice.dis,data.frame(bank='BanIce',tow=tow,dis=dis,date=date,brg=brg))



dis<-rbind(ger.dis,sab.dis,mid.dis,bbn.dis,ban.dis,banice.dis,gb.dis)



## Get Strata
#Read12
bbnTows<-read.csv(paste(loc,"BBNTows2012.csv",sep=""))
#Read13
gerTows<-read.table(paste(loc,"GerTows2012.txt",sep=""),header=T,sep='\t')
#Read14
sabTows<-read.table(paste(loc,"SabTows2012.txt",sep=""),header=T,sep='\t')

tows<-rbind(data.frame(bank="BBn",bbnTows[,c(1,5)]),data.frame(bank="Ger",gerTows[,c(1,5)]),data.frame(bank="Sab",sabTows[,c(1,5)]))
names(tows)[2:3]<-c('tow','stratum')

### put it all together
#browser()
TE13pos$depth<-TE13pos$depth.f*1.8288
TE13<-merge(merge(merge(TE13pos,TE13shf,all=T),dis,all=T),tows,all=T)


TE13$slat<-convert.dd.dddd(TE13$slat)
TE13$slon<-convert.dd.dddd(TE13$slon)*-1
TE13$elat<-convert.dd.dddd(TE13$elat)
TE13$elon<-convert.dd.dddd(TE13$elon)*-1
TE13$cruise<-"TE13"
#browser()
TE13 <- cbind(TE13[,c('year','cruise','bank','date','tow','stratum','slat','slon','elat','elon','depth','state','dis','brg')], sweep(TE13[,paste('h',seq(5,200,5),sep='')],1,FUN='*',TE13$dis))

TE13$random<-T

####### Meat weights
#Read15
mw<-read.csv(paste(loc,"TE13SS12mtwt.csv",sep=""))
#mw2<-read.table("Y:/Alan/TE13/DATA/TE13SS12mtwt_complete.chk.txt",header=T,sep='\t')
MWs<-merge(TE13pos,mw,all=F)

MWs$lon<-with(MWs,apply(cbind(convert.dd.dddd(elon),convert.dd.dddd(slon)),1,mean))*-1
MWs$lat<-with(MWs,apply(cbind(convert.dd.dddd(elat),convert.dd.dddd(slat)),1,mean))
MWs$ID<-paste(MWs$cruise,MWs$tow,sep='.')

list(SHF=TE13,MWs=MWs,pos=TE13pos)
}
