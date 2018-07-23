##############################################  This script is used to pull out fishery data of interest from survey...


yr = as.numeric(format(Sys.time(), "%Y"))  # 
years = 1984:yr # The years of interest for the bank, may be overwritten on specific banks...
# This function is now hopefully completely portable but for the SQL calls which will require you to be
# able to access the SQL database.  By copying the "Assessmnet_fns" and all subfolders to a specified directory
# this entire program should work.
#direct = "Y:/Offshore scallop/Assessment/Assessment_fns/"
#direct = "d:/r/"
#direct = "e:/fn/"
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.R",sep=""))


# Middle Bank
MidFish<-subset(fish.dat,bank=="Mid" & year %in% 2010:yr,c('lon','lat','pro.repwt','date',"bank","year","hm","datclass"))
names(MidFish)<-c("X","Y","pro.repwt","date","bank","year","hm","datclass")
MidFish$EID<-1:nrow(MidFish)
Mid.Fish <-fishery.dat(MidFish,bk='Mid',yr=2010:yr,method='jackknife') 
Mid.Fish$ID <- "Mid"

# Sable Bank
SabFish<-subset(fish.dat,bank=="Sab" & year %in% 2010:yr,c('lon','lat','pro.repwt','date',"bank","year","hm","datclass"))
names(SabFish)<-c("X","Y","pro.repwt","date","bank","year","hm","datclass")
SabFish$EID<-1:nrow(SabFish)
Sab.Fish <-fishery.dat(SabFish,bk='Sab',yr=2010:yr,method='jackknife') 
Sab.Fish$ID <- "Sab"



# German Bank
GerFish<-subset(fish.dat,bank=="Ger" & year %in% 2010:yr,c('lon','lat','pro.repwt','date',"bank","year","hm","datclass"))
names(GerFish)<-c("X","Y","pro.repwt","date","bank","year","hm","datclass")
GerFish$EID<-1:nrow(GerFish)
Ger.Fish <-fishery.dat(GerFish,bk='Ger',yr=2010:yr,method='jackknife') 
Ger.Fish$ID <- "Ger"

# Browns North
BBnFish<-subset(fish.dat,bank=="BBn" & year %in% 2010:yr,c('lon','lat','pro.repwt','date',"bank","year","hm","datclass"))
names(BBnFish)<-c("X","Y","pro.repwt","date","bank","year","hm","datclass")
BBnFish$EID<-1:nrow(BBnFish)


SeedC2 <-findPolys(BBnFish,subset(seedboxes,ID=='C2-012014'))
BBnC2<-subset(BBnFish,EID%in%SeedC2$EID)
BBn.C2<-fishery.dat(BBnC2,bk='BBn',yr=2010:yr,method='jackknife') 
BBn.C2$ID <- "C2-box"

SeedC3 <-findPolys(BBnFish,subset(seedboxes,ID=='C3-012014'))
BBnC3<-subset(BBnFish,EID%in%SeedC3$EID)
BBn.C3<-fishery.dat(BBnC3,bk='BBn',yr=2010:yr,method='jackknife') 
BBn.C3$ID <- "C3-box"

BBn.Fish <-fishery.dat(BBnFish,bk='BBn',yr=2010:yr,method='jackknife') 
BBn.Fish$ID <- "BBn"

max(fish.dat$date,na.rm=T)

# Georges A

GBaFish<-subset(fish.dat,bank=="GBa" & year %in% 2010:yr,c('lon','lat','pro.repwt','date',"bank","year","hm","datclass"))
names(GBaFish)<-c("X","Y","pro.repwt","date","bank","year","hm","datclass")
GBaFish$EID<-1:nrow(GBaFish)

GBa.Fish <- fishery.dat(GBaFish,bk='GBa',yr=2010:yr,method='jackknife') 
GBa.Fish$ID <- "GBa"

SeedC1 <-findPolys(GBaFish,subset(seedboxes,ID=='C1-012014'))
GBaC1<-subset(GBaFish,EID%in%SeedC1$EID)
GBa.C1<-fishery.dat(GBaC1,bk='GBa',yr=2010:yr,method='jackknife') 
GBa.C1$ID <- "C1-box"
SeedAB <-findPolys(GBaFish,subset(seedboxes,ID=='A/B line closure'))
GBaAB<-subset(GBaFish,EID%in%SeedAB$EID)
GBa.AB<-fishery.dat(GBaAB,bk='GBa',yr=2010:yr,method='jackknife') 
GBa.AB$ID <- "AB-box"
SeedSB <-findPolys(GBaFish,subset(seedboxes,ID=='Seed box (2012 modified)'))
GBaSB<-subset(GBaFish,EID%in%SeedSB$EID)
GBa.SB<-fishery.dat(GBaSB,bk='GBa',yr=2010:yr,method='jackknife') 
GBa.SB$ID <- "SB-box"


# Catch/effort/CPUE summary since 2015.

total.data <- rbind(Mid.Fish,Sab.Fish,Ger.Fish,BBn.Fish,BBn.C2,BBn.C3,GBa.Fish,GBa.C1,GBa.AB,GBa.SB)

write.csv(total.data,file= paste(direct,yr,"/Presentations/OSAC/Fishing_history.csv",sep = "")
