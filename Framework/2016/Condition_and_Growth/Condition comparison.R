direct = "d:/r/"
#direct = "f:/r/"
yr = 2015
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_results_for_GBa.Rdata",sep=""))  


library(lme4)
library(mgcv)
source(paste(direct,"Assessment_fns/shwt.lme.r",sep="")) #Source11

pred.dat= bank.dat[["GBa"]]
model.type='gam_d'
b.par=3


wgt.dat <- na.omit(mw.dat.all[["GBa"]])
wgt.dat$sh<-wgt.dat$sh/100
# Here is the model we use for GBa
SpatHtWt.fit1<-shwt.lme(wgt.dat,random.effect='ID',b.par=b.par,verbose=F)

# Merge the raw data with the model fit, just keep the first sample for each tow in which we have multiple samples.
CF.data1<-merge(wgt.dat[!duplicated(wgt.dat$ID),c('ID','lon','lat','year','depth','tow')],SpatHtWt.fit1$fit,all=T)
CF.data.sub <-merge(wgt.sub[!duplicated(wgt.sub$ID),c('ID','lon','lat','year','depth','tow')],SpatHtWt.fit.sub$fit,all=T)
# Make sure the names are what we want
names(CF.data1)<-c('ID','lon','lat','year','depth','tow','CF')
names(CF.data.sub)<-c('ID','lon','lat','year','depth','tow','CF')
CF.data1.2015 <- CF.data1[grep("LE02",CF.data1$ID),]

wgt.dat2015 <- na.omit(subset(mw.dat.all[["GBa"]],year==2015))
wgt.dat2015$sh<-wgt.dat2015$sh/100

SpatHtWt.fit<-shwt.lme(wgt.dat2015,random.effect='ID',b.par=b.par,verbose=F)
# Merge the raw data with the model fit, just keep the first sample for each tow in which we have multiple samples.
CF.data2015<-merge(wgt.dat2015[!duplicated(wgt.dat2015$ID),c('ID','lon','lat','year','depth','tow')],SpatHtWt.fit$fit,all=T)
# Make sure the names are what we want
names(CF.data2015)<-c('ID','lon','lat','year','depth','tow','CF')
#CF.data2015

# The CF from this is essentially idenetical  
tail(CF.data1.2015)
tail(CF.data2015)


####  For the glmer we need to do some stick handling...
wgt.glmer <- na.omit(subset(mw.dat.all[["GBa"]],year==years[i] & sh > 40))
#tail(wgt.glmer)
#create dataset for model
wgt.glmer$depth <- -wgt.glmer$depth
range(wgt.glmer$depth)
wgt.glmer$log.sh <- log(wgt.glmer$sh)

wgt.glmer$log.sh.ctr <- wgt.glmer$log.sh-mean(wgt.glmer$log.sh)
wgt.glmer$log.depth <- log(abs(wgt.glmer$depth)) #take abs to keep value positive
#exp(mean(wgt.glmer$log.depth))
wgt.glmer$log.depth.ctr <- wgt.glmer$log.depth-mean(wgt.glmer$log.depth)
#summary(wgt.glmer)

### The models.....
# Full fit
CF.fit1<-gam(CF~s(depth)+s(year),data=CF.data1)
# One year of data fit
CF.fit2015 <- gam(CF~s(depth),data=CF.data2015)

#run model; update model name to correspond to year
CF.fit.lme<-glmer(wmw~log.sh.ctr+log.depth.ctr+(log.sh.ctr|tow),data=wgt.glmer,
                    family=Gamma(link=log), na.action = na.omit)

summary(CF.fit.lme)



##########  Predictions...###########
info.surv <- subset(surv.info, label== "GBa")
strata.dets <- data.frame(Strata=info.surv$PID, NH=info.surv$towable_area[order(info.surv$PID)])

#Construct data.frame similar to BFlivenfreq for weight per tow
years <- unique(mw.dat.all[["GBa"]]$year)
Rec <- NULL
FR <- NULL
PR <- NULL
#CF.fit.lme <- NULL
library(PEDstrata)
i=20
for(i in 1:length(years))
  {
    ####  For the glmer we need to do some stick handling...
    wgt.glmer <- na.omit(subset(mw.dat.all[["GBa"]],year==years[i] & sh > 40))
    #tail(wgt.glmer)
    #create dataset for model
    wgt.glmer$depth <- -wgt.glmer$depth
    range(wgt.glmer$depth)
    wgt.glmer$log.sh <- log(wgt.glmer$sh)
    
    wgt.glmer$log.sh.ctr <- wgt.glmer$log.sh-mean(wgt.glmer$log.sh)
    wgt.glmer$log.depth <- log(abs(wgt.glmer$depth)) #take abs to keep value positive
    #exp(mean(wgt.glmer$log.depth))
    wgt.glmer$log.depth.ctr <- wgt.glmer$log.depth-mean(wgt.glmer$log.depth)
    
    # and now the model fit..
    CF.fit.lme<-glmer(wmw~log.sh.ctr+log.depth.ctr+(log.sh.ctr|tow),data=wgt.glmer,
                      family=Gamma(link=log), na.action = na.omit)
    
    bank.dat.GBa <-subset (surv.Live[["GBa"]], year==years[i])
    bank.dat.GBa.temp <- bank.dat.GBa
    
    #Use random effects for tows in detail sample and fixed effects otherwise
    #create matrix of depths by tow to use in predict function
    Log.ctr.depth <- log(abs(bank.dat.GBa$depth)) - mean(wgt.glmer$log.depth) #depth by tow
    Log.sh.ctr <- log(seq(2.5, 197.5, by = 5)) - mean(wgt.glmer$log.sh) #each shell height bin to predict on
    
    temp<-matrix(NA,dim(bank.dat.GBa)[1],40)
    random.pred<-(1:dim(bank.dat.GBa)[1])[is.element(bank.dat.GBa$tow,unique(wgt.glmer$tow))]
    
    fixed.pred<-(1:dim(bank.dat.GBa)[1])[!is.element(bank.dat.GBa$tow,unique(wgt.glmer$tow))]
    
    for(j in random.pred) temp[j,]<-as.vector(predict(CF.fit.lme,newdata=data.frame(log.sh.ctr=Log.sh.ctr,
                                          log.depth.ctr=rep(Log.ctr.depth[j] ,40), tow=bank.dat.GBa$tow[j]),re.form=NULL,type="response"))
    
    for(j in fixed.pred) temp[j,]<-as.vector(predict(CF.fit.lme,newdata=data.frame(log.sh.ctr=Log.sh.ctr,
                                          log.depth.ctr=rep(Log.ctr.depth[j] ,40)),re.form=~0,type="response"))
    
    
    bank.dat.GBa[,grep("^h",names(bank.dat.GBa))]<- temp * bank.dat.GBa.temp[,grep("^h",names(bank.dat.GBa))]/1000
    bank.dat.GBa <- bank.dat.GBa[,-c(61:72)]
    
    # in these years the size bins were 85 and 95 mm for RS and CS respectively
    # 1981-1985 CS = 75, RS = 60
    # From 1986-1995 CS = 85, RS= 75
    # From 1996-current CS= 95, RS = 85
    if(years[i] >= 1981 && years[i] < 1986)
    {
      Rec[[as.character(years[i])]]$year <- years[i]
      Rec[[as.character(years[i])]]$B.per.tow <-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                  catch=apply(bank.dat.GBa[,26:28],1,sum),Subset=bank.dat.GBa$random==1))$yst
      Rec[[as.character(years[i])]]$Bmass<-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                             catch=apply(bank.dat.GBa[,26:28],1,sum),  Subset=bank.dat.GBa$random==1))$Yst
      
      
      
      FR[[as.character(years[i])]]$year <- years[i]
      FR[[as.character(years[i])]]$B.per.tow <-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                 catch=apply(bank.dat.GBa[,29:53],1,sum),Subset=bank.dat.GBa$random==1))$yst
      FR[[as.character(years[i])]]$Bmass <-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                             catch=apply(bank.dat.GBa[,29:53],1,sum),Subset=bank.dat.GBa$random==1))$Yst
      
      PR[[as.character(years[i])]]$year <- years[i]
      PR[[as.character(years[i])]]$B.per.tow<-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                catch=apply(bank.dat.GBa[,14:25],1,sum),Subset=bank.dat.GBa$random==1))$yst
      PR[[as.character(years[i])]]$Bmass<-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                            catch=apply(bank.dat.GBa[,14:25],1,sum), Subset=bank.dat.GBa$random==1))$Yst
    } # end if(years[i] >= 1981 && years[i] < 1986)
    
    if(years[i] >= 1986 && years[i] < 1996)
    {
      Rec[[as.character(years[i])]]$year <- years[i]
      Rec[[as.character(years[i])]]$B.per.tow <-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                  catch=apply(bank.dat.GBa[,29:30],1,sum),Subset=bank.dat.GBa$random==1))$yst
      Rec[[as.character(years[i])]]$Bmass<-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                             catch=apply(bank.dat.GBa[,29:30],1,sum),  Subset=bank.dat.GBa$random==1))$Yst
      
      
      
      FR[[as.character(years[i])]]$year <- years[i]
      FR[[as.character(years[i])]]$B.per.tow <-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                 catch=apply(bank.dat.GBa[,31:53],1,sum),Subset=bank.dat.GBa$random==1))$yst
      FR[[as.character(years[i])]]$Bmass <-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                             catch=apply(bank.dat.GBa[,31:53],1,sum),Subset=bank.dat.GBa$random==1))$Yst
      
      PR[[as.character(years[i])]]$year <- years[i]
      PR[[as.character(years[i])]]$B.per.tow<-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                catch=apply(bank.dat.GBa[,14:28],1,sum),Subset=bank.dat.GBa$random==1))$yst
      PR[[as.character(years[i])]]$Bmass<-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                            catch=apply(bank.dat.GBa[,14:28],1,sum), Subset=bank.dat.GBa$random==1))$Yst
     
    } # end if(years[i] >= 1986 && years[i] < 1996)
    
    
    
    # in these years the size bins were 85 and 95 mm for RS and CS respectively
    if(years[i] >= 1996)
      {
          Rec[[as.character(years[i])]]$year <- years[i]
          Rec[[as.character(years[i])]]$B.per.tow <-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                 catch=apply(bank.dat.GBa[,31:32],1,sum),Subset=bank.dat.GBa$random==1))$yst
          Rec[[as.character(years[i])]]$Bmass<-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                 catch=apply(bank.dat.GBa[,31:32],1,sum),  Subset=bank.dat.GBa$random==1))$Yst
                                            
      
      
          FR[[as.character(years[i])]]$year <- years[i]
          FR[[as.character(years[i])]]$B.per.tow <-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                 catch=apply(bank.dat.GBa[,33:53],1,sum),Subset=bank.dat.GBa$random==1))$yst
          FR[[as.character(years[i])]]$Bmass <-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                 catch=apply(bank.dat.GBa[,33:53],1,sum),Subset=bank.dat.GBa$random==1))$Yst
       
          PR[[as.character(years[i])]]$year <- years[i]
          PR[[as.character(years[i])]]$B.per.tow<-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                catch=apply(bank.dat.GBa[,14:30],1,sum),Subset=bank.dat.GBa$random==1))$yst
          PR[[as.character(years[i])]]$Bmass<-summary(PEDstrata(bank.dat.GBa,strata.dets,"new.stratum",
                                                                catch=apply(bank.dat.GBa[,14:30],1,sum), Subset=bank.dat.GBa$random==1))$Yst
      } # end if(years[i] >= 1996)

} # end for i in 1:length(years)
library(plyr)

# This splits the list into a dataframe...
FR.dat <- ldply(FR,data.frame)
PR.dat <- ldply(PR,data.frame)
Rec.dat <- ldply(Rec,data.frame)
FR.dat <- FR.dat[order(FR.dat$year),]
PR.dat <- PR.dat[order(PR.dat$year),]
Rec.dat <- Rec.dat[order(Rec.dat$year),]
FR.dat$Bmass <- FR.dat$Bmass/1000
PR.dat$Bmass <- PR.dat$Bmass/1000
Rec.dat$Bmass <- Rec.dat$Bmass/1000
FR.dat$Bmass_gam <- survey.obj[["GBa"]][[1]][,]$I
Rec.dat$Bmass_gam <- survey.obj[["GBa"]][[1]][,]$IR
PR.dat$Bmass_gam <- survey.obj[["GBa"]][[1]][,]$IPR

# How different are the results??
FR.dat$diff <- (FR.dat$Bmass - FR.dat$Bmass_gam)
PR.dat$diff <- (PR.dat$Bmass -  PR.dat$Bmass_gam)
Rec.dat$diff <- (Rec.dat$Bmass -  Rec.dat$Bmass_gam)
FR.dat$pdiff <- (FR.dat$Bmass - FR.dat$Bmass_gam)/ FR.dat$Bmass_gam
PR.dat$pdiff <- (PR.dat$Bmass -  PR.dat$Bmass_gam)/ PR.dat$Bmass_gam
Rec.dat$pdiff <- (Rec.dat$Bmass -  Rec.dat$Bmass_gam)/ Rec.dat$Bmass_gam

#FR.dat <- FR.dat[FR.dat$year>=1996,]
#PR.dat <- PR.dat[PR.dat$year>=1996,]
#Rec.dat <- Rec.dat[Rec.dat$year>=1996,]
dim(FR.dat)

length(which(abs(FR.dat$pdiff) > 0.05))
length(which(abs(FR.dat$pdiff) < 0.05))
length(which(abs(Rec.dat$pdiff) < 0.05))

median(abs(FR.dat$pdiff))
median(abs(Rec.dat$pdiff))
median(abs(FR.dat$diff))
median(abs(Rec.dat$diff))
median(abs(PR.dat$pdiff))
range(FR.dat$pdiff)
range(FR.dat$diff)
range(Rec.dat$pdiff)
range(Rec.dat$diff)

sum(FR.dat$Bmass) - sum(FR.dat$Bmass_gam)

windows(11,8.5)
plot(FR.dat$Bmass~FR.dat$year,type="o",pch=15,ylab="Biomass (tonnes)",xlab="year",main="Fully recruited Biomass")
lines(FR.dat$Bmass_gam~FR.dat$year,type="o",lty=2,col="blue",pch=16)
legend("topright",c("Inshore method","Offshore method"),col=c("black","blue"),lty=c(1,2),pt.bg=c("black","blue"),pch=c(15,16),bty="n")

windows(11,8.5)
plot(Rec.dat$Bmass~Rec.dat$year,type="o",pch=15,ylab="Biomass (tonnes)",xlab="year",main="Recruit Biomass")
lines(Rec.dat$Bmass_gam~Rec.dat$year,type="o",lty=2,col="blue",pch=16)
legend("topright",c("Inshore method","Offshore method"),col=c("black","blue"),lty=c(1,2),pt.bg=c("black","blue"),pch=c(15,16),bty="n")

### Now moving on the calculation of SHF and l.bar

years <- 1983:2015
num.yrs <- length(years)

GBa.SHF <- NULL
for(i in 1:num.yrs)
  {
    bank.dat <-subset (surv.Live[["GBa"]], year==years[i])
    for(j in 1:40)
      {
      GBa.SHF[[i]]<-summary(PEDstrata(bank.dat,strata.dets,"new.stratum",catch=bank.dat[,13+j], Subset=bank.dat$random==1))$yst
      }
  }



