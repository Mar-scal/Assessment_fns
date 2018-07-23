################################################################################################################
##### This is the script for accessing the 2011 spring survey data for each offshore area and combining it
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
#No arguments used in this function.

# source("Y:/Assessment/2011/r2/SpringSurveyDAta.r")

getSpringSurveyData2011<-function(){

  
  require(PBSmapping)  || stop("Install PBSmapping Package") # Needed for getdis.r

#DK Note August 19 2015 Load the necessary functions, an idea for later might be to have all sources point to 1 directory, fun.dir.  
  #fun.dir <- "d:/Offshore scallop/Assessment/2014/r"
  #Sounce1  
  source("fn/getdis.r")    
  #Source2
  source("fn/convert.dd.dddd.r")
  
  # DK Note August 19, 2015: Let's set local directory here this will make adjusting these csv calls later much easier if we change our folder structure
  loc <- "D:/Offshore scallop/Assessment/2011/r2/data/SpringSurvey/"
  
################# Middle #################################

  
  #Read1 Survey data for Middle bank in 2011
 tmp<-read.csv(paste(loc,"TE11Midhfchecked.csv",sep=""))
 
  # Remove NA's, transpose the matrix, drop the first row and chose only columns 1:24, this give us just shell height frequencies.
 tmp.t<-na.omit(data.frame(t(tmp[,-1])[,1:24]))
 # So this splits up tmp.t by column "X24" which is a code line, code's 0 and 2 are placed together, and codes 1 and 3 are places together
 # we also remove the code column in the subset. 
 # These codes distinquish whether the scallop was live or dead and if the shell height was > or < 100(ish)
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:23),subset(tmp.t,X24%in%c(1,3),4:23))
 names(tmp.dat)<-c('tow','TMS','depth',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2011
 
 TE11Midhf<-tmp.dat

 TE11Midhf$bank<-"Mid"

################# Sable #################################
 #Read2 Next 3 files are Survey data for Sable bank in 2011
 tmp.1<-read.csv(paste(loc,"TE11Sabhf60checked.csv",sep=""))
 #Read3
 tmp.2<-read.csv(paste(loc,"TE11Sabhf120checked.csv",sep=""))
 #Read4
 tmp.3<-read.csv(paste(loc,"TE11Sabhf135checked.csv",sep=""))
 
#browser()
 
 # Combine these data into tmp.t
 tmp.t1<-na.omit(data.frame(t(tmp.1[,-1])[,1:24]))
 tmp.t2<-na.omit(data.frame(t(tmp.2[,-1])[,1:24]))
 tmp.t3<-na.omit(data.frame(t(tmp.3[,-1])[,1:24]))
 tmp.t<-rbind(tmp.t1,tmp.t2,tmp.t3)
 
 
 # So this splits up tmp.t by column "X24" which is a code line, code's 0 and 2 are placed together, and codes 1 and 3 are places together
 # we also remove the code column in the subset. 
 # These codes distinquish whether the scallop was live or dead and if the shell height was > or < 100(ish)
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:23),subset(tmp.t,X24%in%c(1,3),4:23))
 names(tmp.dat)<-c('tow','TMS','depth',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2011
 
 TE11Sabhf<-tmp.dat

 TE11Sabhf$bank<-"Sab"

################# Browns North #################################
 #Read5 Next 2 files bring in Browns north survey data from 2011
 tmp.1<-read.csv(paste(loc,"TE11BBnhf58checked.csv",sep=""))
 #Read6
 tmp.2<-read.csv(paste(loc,"TE11BBnhf5970checked.csv",sep=""))
 
 # Combine these data into tmp.t
 tmp.t1<-na.omit(data.frame(t(tmp.1[,-1])[,1:24]))
 tmp.t2<-na.omit(data.frame(t(tmp.2[,-1])[,1:24]))
 tmp.t<-rbind(tmp.t1,tmp.t2)
 
 
 # So this splits up tmp.t by column "X24" which is a code line, code's 0 and 2 are placed together, and codes 1 and 3 are places together
 # we also remove the code column in the subset. 
 # These codes distinquish whether the scallop was live or dead and if the shell height was > or < 100(ish)
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:23),subset(tmp.t,X24%in%c(1,3),4:23))
 names(tmp.dat)<-c('tow','TMS','depth',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2011
 
 TE11BBnhf<-tmp.dat

 TE11BBnhf$bank<-"BBn"

################# Browns south #################################
 #Read7 This is the Browns south survey data from 2011
 tmp<-read.csv(paste(loc,"TE11BBshf25checked.csv",sep=""))
 
 tmp.t<-na.omit(data.frame(t(tmp[,-1])[,1:24]))
 # So this splits up tmp.t by column "X24" which is a code line, code's 0 and 2 are placed together, and codes 1 and 3 are places together
 # we also remove the code column in the subset. 
 # These codes distinquish whether the scallop was live or dead and if the shell height was > or < 100(ish)
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:23),subset(tmp.t,X24%in%c(1,3),4:23))
 names(tmp.dat)<-c('tow','TMS','depth',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2011
 
 TE11BBshf<-tmp.dat

 TE11BBshf$bank<-"BBs"
################# German #################################
 #Read8 The next 2 contain the German bank survey data for 2011
 tmp.1<-read.csv(paste(loc,"TE11Gerhf60checked.csv",sep=""))
 #Read9
 tmp.2<-read.csv(paste(loc,"TE11Gerhf90checked.csv",sep=""))
 
 # combine these and make a tmp.t object
 tmp.t1<-na.omit(data.frame(t(tmp.1[,-1])[,1:24]))
 tmp.t2<-na.omit(data.frame(t(tmp.2[,-1])[,1:24]))
 tmp.t<-rbind(tmp.t1,tmp.t2)
 
 # So this splits up tmp.t by column "X24" which is a code line, code's 0 and 2 are placed together, and codes 1 and 3 are places together
 # we also remove the code column in the subset. 
 # These codes distinquish whether the scallop was live or dead and if the shell height was > or < 100(ish)
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:23),subset(tmp.t,X24%in%c(1,3),4:23))
 names(tmp.dat)<-c('tow','TMS','depth',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2011
 
 TE11Gerhf<-tmp.dat

 TE11Gerhf$bank<-"Ger"



################# Georges #################################
 #Read10 The next two bring in the Georges bank survey data from 2011
 tmp.1<-read.csv(paste(loc,"TE11GBhf.peanutchecked.csv",sep=""))
 #Read11
 tmp.2<-read.csv(paste(loc,"TE11GBhf46.no.peanutchecked.csv",sep=""))
 
 #Combine these and make a tmp.t object
 tmp.t1<-na.omit(data.frame(t(tmp.1[,-1])[,1:24]))
 tmp.t2<-na.omit(data.frame(t(tmp.2[,-1])[,1:24]))
 tmp.t<-rbind(tmp.t1,tmp.t2)
 
 # So this splits up tmp.t by column "X24" which is a code line, code's 0 and 2 are placed together, and codes 1 and 3 are places together
 # we also remove the code column in the subset. 
 # These codes distinquish whether the scallop was live or dead and if the shell height was > or < 100(ish)
 tmp.dat<-cbind(subset(tmp.t,X24%in%c(0,2),1:23),subset(tmp.t,X24%in%c(1,3),4:23))
 names(tmp.dat)<-c('tow','TMS','depth',paste('h',seq(5,200,5),sep=''))
 tmp.dat$state<-rep(c('live','dead'),nrow(tmp.t)/4)
 tmp.dat$year<-2011
 
 TE11GBhf<-tmp.dat

 TE11GBhf$bank<-"GB"

# Combine each of the banks shell height frequency data.
TE11shf<-rbind(TE11Midhf,TE11Sabhf,TE11BBnhf,TE11BBshf,TE11Gerhf,TE11GBhf)

## Positions
#Read12 These are the positions and the depth for every tow.  Includes start and end long/lat so that the distance covered by the tow can be calculated.
TE11pos<-read.csv(paste(loc,"TE11positions.csv",sep=""))

TE11pos$year<-2011
# Convert the position data into decimal degrees.
#Source2 source("fn/convert.dd.dddd.r") 
TE11pos$lon<-with(TE11pos,apply(cbind(convert.dd.dddd(elon),convert.dd.dddd(slon)),1,mean))*-1
TE11pos$lat<-with(TE11pos,apply(cbind(convert.dd.dddd(elat),convert.dd.dddd(slat)),1,mean))



## Get Distance coefficients: 
#Read18 Note that this opens hundreds of flat files with log track information in them
## used to calculate distance covered by each tow.
#Source1 source("fn/getdis.r")    
gerDis<-dist.coef(402:480,path=paste(loc,"logfiles/German/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)
sabDis<-dist.coef(c(1:100,102:135),path=paste(loc,"logfiles/Sable/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=1000)
midDis<-dist.coef(1:15,path=paste(loc,"logfiles/Middle/M",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=100)
bbnDis<-dist.coef(201:270,path=paste(loc,"logfiles/BBN/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)
bbsDis<-dist.coef(501:525,path=paste(loc,"logfiles/BBS/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)
gbDis <-dist.coef(c(301:324,802:823),path=paste(loc,"logfiles/Georges/",sep=""),w=c(1:10,9:1),rule=8,smooth=T,plt=F)

# The dist.coef returns a list of 2 objects, first is a simple dataframe, second is output from PBSmapping, this step selects the
# simple dataframe information (tow, distance, Date and bearing)
ger.dis<-data.frame(bank='Ger',gerDis[[1]])
sab.dis<-data.frame(bank='Sab',sabDis[[1]])
mid.dis<-data.frame(bank='Mid',midDis[[1]])
bbn.dis<-data.frame(bank='BBn',bbnDis[[1]])
bbs.dis<-data.frame(bank='BBs',bbsDis[[1]])
gb.dis <-data.frame(bank='GB',gbDis[[1]])

# Put these into one dataframe
dis<-rbind(ger.dis,sab.dis,mid.dis,bbn.dis,bbs.dis,gb.dis)


## Get Strata
#Read13 # The next 4 files get the strata information for Browns, German, and Sable banks in 2011, note no GB strata information from spring survey.
bbnTows<-read.csv(paste(loc,"BBNTows2011_ar.csv",sep=""))
#Read14
bbsTows<-read.csv(paste(loc,"BBSTows2011_ar.csv",sep=""))
#Read15
gerTows<-read.csv(paste(loc,"GerTows2011_ar.csv",sep=""))
#Read16
sabTows<-read.csv(paste(loc,"SabTows2011_ar.csv",sep=""))

# Combine this information into tows
tows<-rbind(data.frame(bank="BBn",bbnTows[,c(1,7)]),data.frame(bank="BBs",bbsTows[,c(1,7)]),data.frame(bank="Ger",gerTows[,c(1,7)]),data.frame(bank="Sab",sabTows[,c(1,7)]))
names(tows)[2:3]<-c('tow','stratum')

### put it all together
#browser()
TE11<-merge(na.omit(merge(merge(TE11pos,TE11shf,all=T),dis,all=T)),tows,all=T)
TE11pos$depth<-TE11pos$depth.f*1.8288

#Source2 source("fn/convert.dd.dddd.r") Convert the lat/long into decimal degrees.
TE11$slat<-convert.dd.dddd(TE11$slat)
TE11$slon<-convert.dd.dddd(TE11$slon)*-1 # flip the sign to a negative for longitude
TE11$elat<-convert.dd.dddd(TE11$elat)
TE11$elon<-convert.dd.dddd(TE11$elon)*-1 # flip the sign to a negative for longitude
TE11$cruise<-"TE11"
TE11 <- cbind(TE11[,c('year','cruise','bank','date','tow','stratum','slat','slon','elat','elon','depth','state','dis','brg')], sweep(TE11[,paste('h',seq(5,200,5),sep='')],1,FUN='*',TE11$dis))

TE11$random<-T

####### Meat weights
#Read17 Bring in the meat weight data from the spring survey.
mw<-read.csv(paste(loc,"TE11SS11mtwt_no.check.csv",sep=""))

# Add meat weights to TE11pos, note this only keeps rows that match from both objects.
MWs<-merge(TE11pos,mw,all=F)

#  convert lat/long data into decimal degrees.
#Source2 source("fn/convert.dd.dddd.r") 
MWs$lon<-with(MWs,apply(cbind(convert.dd.dddd(elon),convert.dd.dddd(slon)),1,mean))*-1
MWs$lat<-with(MWs,apply(cbind(convert.dd.dddd(elat),convert.dd.dddd(slat)),1,mean))
MWs$ID<-paste(MWs$cruise,MWs$tow,sep='.')

list(SHF=TE11,MWs=MWs,pos=TE11pos)
}
