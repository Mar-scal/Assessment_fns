##################################################   Seedboxes   #########################################################		
##################################################   Seedboxes #########################################################		
##################################################   Seedboxes#########################################################		

# Here I am looking at how removing the seedboxes would effect our view of the bank

yr = as.numeric(format(Sys.time(), "%Y"))  # 
direct = "d:/r/"
library(RColorBrewer)
library(PBSmapping)

# Load the survey data.
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))
source(paste(direct,"Assessment_fns/Survey_and_OSAC/survey.dat.r",sep="")) #Source21 Revised by DK September 2015
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) 
# You may need to reload your R profile if you use it...
#source("d:/r/.Rprofile")
bnk <- c("GBa","BBn")
cf.lab <-    expression(paste("CF:",bgroup("(",frac(g,dm^3)   ,")")))
mc.lab <-    expression(paste("MC:",bgroup("(",frac(N,"500 g"),")"))) 

i=1
# Remove the seedbox tows
# Here is GBa summer without the seedbox data from 2007-2015
surv.Rand[["GBa"]]$EID<-1:nrow(surv.Rand[["GBa"]])
surv.Rand[["GBa"]]$X<-surv.Rand[["GBa"]]$slon
surv.Rand[["GBa"]]$Y<-surv.Rand[["GBa"]]$slat
surv.Live[["GBa"]]$EID<-1:nrow(surv.Live[["GBa"]])
surv.Live[["GBa"]]$X<-surv.Live[["GBa"]]$slon
surv.Live[["GBa"]]$Y<-surv.Live[["GBa"]]$slat
# Now lets pull out all 3 boxes rather than just the current one and look at impact...
C1.rm <- findPolys(surv.Rand[["GBa"]],subset(seedboxes,ID %in% c('C1-012014',"Seed box (2012 modified)","A/B line closure")))
C1.rm.live <- findPolys(surv.Live[["GBa"]],subset(seedboxes,ID %in% c('C1-012014',"Seed box (2012 modified)","A/B line closure")))

GBa.no.box <- surv.Rand[["GBa"]][-which(surv.Rand[["GBa"]]$EID %in% C1.rm$EID),]
GBa.box <- surv.Rand[["GBa"]][which(surv.Rand[["GBa"]]$EID %in% C1.rm$EID),]
GBa.no.box.live <- surv.Live[["GBa"]][-which(surv.Live[["GBa"]]$EID %in% C1.rm.live$EID),]
GBa.box.live <- surv.Live[["GBa"]][which(surv.Live[["GBa"]]$EID %in% C1.rm.live$EID),]

# This removes the seedboxes from our estimates
t1 <- joinPolys(subset(survey.detail.polys,label=="GBa"),subset(seedboxes,ID == c('C1-012014')),"DIFF")
t2 <- joinPolys(t1,subset(seedboxes,ID == c('Seed box (2012 modified)')),"DIFF")
survey.no.box <- joinPolys(t2,subset(seedboxes,ID == c('A/B line closure')),"DIFF")
# This removes everything else from our estimates
C1 <- joinPolys(subset(survey.detail.polys,label=="GBa"),subset(seedboxes,ID %in% c('C1-012014')),"INT")
seed <- joinPolys(subset(survey.detail.polys,label=="GBa"),subset(seedboxes,ID == c('Seed box (2012 modified)')),"INT")
AB <- joinPolys(subset(survey.detail.polys,label=="GBa"),subset(seedboxes,ID == c('A/B line closure')),"INT")




# Get the area estimates for GBa with and without the seedboxes. The cacluations from
# PBS mapping don't quite jive with our strata area numbers, but for the purposes of this I think this should be o.k.
GBa.area.nobox <-calcArea(survey.no.box,rollup = 1)
GBa.area.nobox$area <- GBa.area.nobox$area*10000 # I'm not sure why I need the 10,000 so that worries me something is screwy!
GBa.area.nobox$towable_area <- GBa.area.nobox$area*1000*1000/800/(8/3.2808)

GB.area.total <- calcArea(subset(survey.detail.polys,label=="GBa"),rollup = 1)
GB.area.total$area <- GB.area.total$area*10000  # I'm not sure why I need the 10,000 so that worries me something is screwy!
GB.area.total$towable_area <- GB.area.total$area*1000*1000/800/(8/3.2808)

GBa.area.box <- GBa.area.nobox
GBa.area.box$area <- GB.area.total$area - GBa.area.nobox$area
GBa.area.box$towable_area <- GBa.area.box$area*1000*1000/800/(8/3.2808)

# Make sure these are right
windows(8,8)
ScallopMap("GBa",poly.lst = list(survey.no.box,subset(surv.info,label=="GBa")))
ScallopMap("GBa",poly.lst = list(t2,subset(surv.info,label=="GBa")))

# These shoud already be alinged if looking at all the data from 1981 to current.
#CS = c(rep(80,5),rep(90,10),rep(100,yr-1995))
# RS = Shell height 1 year previous to CS
#RS = c(rep(65,5),rep(80,10),rep(90,yr-1995))
# RS = Shell height 1 year previous to CS (new LVB parameters)
# Years
#years=1981:yr
survey.no.box <- survey.dat(GBa.no.box, years=years, RS=RS, CS=CS, bk=bnk[2], areas=GBa.area.nobox[,c(1,3)], mw.par="CFh")
survey.box<-survey.dat(GBa.box,years=years,RS=RS, CS=CS,areas=GBa.area.box[,c(1,3)], mw.par="CFh")	
survey.obj<- survey.dat(surv.Rand[["GBa"]], years=years, RS=RS, CS=CS, bk=bnk[2], areas=GB.area.total[,c(1,3)], mw.par="CFh")	
survey.no.box[[1]]$CF<-sapply(1:length(years),
                           function(x){with(subset(GBa.no.box,year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})
survey.box[[1]]$CF<-sapply(1:length(years),
                              function(x){with(subset(GBa.box,year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})
survey.obj[[1]]$CF<-sapply(1:length(years),
                           function(x){with(subset(surv.Rand[["GBa"]],year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})

# Now do the same with all of the data (i.e. include exploratory tows)
survey.no.box.all <- survey.dat(GBa.no.box.live, years=years, RS=RS, CS=CS, bk=bnk[2], areas=GBa.area.nobox[,c(1,3)], mw.par="CFh")
survey.box.all<-survey.dat(GBa.box.live,years=years,RS=RS, CS=CS,areas=GBa.area.box[,c(1,3)], mw.par="CFh")	
survey.obj.all<- survey.dat(surv.Live[["GBa"]], years=years, RS=RS, CS=CS, bk=bnk[2], areas=GB.area.total[,c(1,3)], mw.par="CFh")	
survey.no.box.all[[1]]$CF<-sapply(1:length(years),
                              function(x){with(subset(GBa.no.box.live,year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})
survey.box.all[[1]]$CF<-sapply(1:length(years),
                           function(x){with(subset(GBa.box.live,year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})
survey.obj.all[[1]]$CF<-sapply(1:length(years),
                           function(x){with(subset(surv.Live[["GBa"]],year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})



# This is actually really interesting, here are the trends in the last 5 years, inside, outside, and within the seedboxes (aggregated)
nobox.5 <- tail(survey.no.box[[1]])
survey.5 <-  tail(survey.obj[[1]])
box.5 <- tail(survey.box[[1]])

# Fully recruited
windows(8,8)
plot(nobox.5$N ~ nobox.5$year,type='o',lwd=2,col="red",ylim=c(0,400),xlab="",
     ylab="Number (millions)",pch=21,bg= "red",main = "Fully Recruited Abundance GBa")
points(survey.5$N ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$N ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Fully recruited per tow
windows(8,8)
plot(nobox.5$N/sum(GBa.area.nobox)*1e6 ~ nobox.5$year,type='o',lwd=2,col="red",
     ylim=c(0,800),xlab="",ylab="Number (per tow)",pch=21,bg= "red",main = "Fully Recruited Abundance GBa")
points(survey.5$N/sum(GB.area.total)*1e6  ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$N/sum(GBa.area.box)*1e6  ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Biomass
windows(8,8)
plot(nobox.5$I ~ nobox.5$year,type='o',lwd=2,col="red",ylim=c(0,8000),xlab="",
     ylab="Biomass (tonnes)",pch=21,bg= "red",main = "Fully Recruited Biomass GBa")
points(survey.5$I ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$I ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Biomass per tow
windows(8,8)
plot(nobox.5$I/sum(GBa.area.nobox)*1e3 ~ nobox.5$year,type='o',lwd=2,col="red",
     ylim=c(0,20),xlab="",ylab="Biomass per tow",pch=21,bg= "red",main = "Fully Recruited Biomass GBa")
points(survey.5$I/sum(GB.area.total)*1e3 ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$I/sum(GBa.area.box)*1e3 ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")



# Recruits
windows(8,8)
plot(nobox.5$NR ~ nobox.5$year,type='o',lwd=2,col="red",ylim=c(0,400),xlab="",
     ylab="Number (millions)",pch=21,bg= "red",main = "Recruit Abundance GBa")
points(survey.5$NR ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$NR ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Recruits per tow
windows(8,8)
plot(nobox.5$NR/sum(GBa.area.nobox)*1e6 ~ nobox.5$year,type='o',lwd=2,col="red",
     ylim=c(0,400),xlab="",ylab="Number (per tow)",pch=21,bg= "red",main = "Recruit Abundance GBa")
points(survey.5$NR/sum(GB.area.total)*1e6  ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$NR/sum(GBa.area.box)*1e6  ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Biomass
windows(8,8)
plot(nobox.5$IR ~ nobox.5$year,type='o',lwd=2,col="red",ylim=c(0,4000),xlab="",
     ylab="Biomass (tonnes)",pch=21,bg= "red",main = "Recruit Biomass GBa")
points(survey.5$IR ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$IR ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Biomass per tow
windows(8,8)
plot(nobox.5$IR/sum(GBa.area.nobox)*1e3 ~ nobox.5$year,type='o',lwd=2,col="red",
     ylim=c(0,5),xlab="",ylab="Biomass per tow",pch=21,bg= "red",main = "Recruit Biomass GBa")
points(survey.5$IR/sum(GB.area.total)*1e3 ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$IR/sum(GBa.area.box)*1e3 ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")



# Finally the Pre-recruts
# 
windows(8,8)
plot(nobox.5$NPR ~ nobox.5$year,type='o',lwd=2,col="red",ylim=c(0,4000),xlab="",
     ylab="Number (millions)",pch=21,bg= "red",main = "Pre-recruit Abundance GBa")
points(survey.5$NPR ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$NPR ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Pre-recruits per tow
windows(8,8)
plot(nobox.5$NPR/sum(GBa.area.nobox)*1e6 ~ nobox.5$year,type='o',lwd=2,col="red",
     ylim=c(0,4000),xlab="",ylab="Number (per tow)",pch=21,bg= "red",main = "Pre-recruit Abundance GBa")
points(survey.5$NPR/sum(GB.area.total)*1e6  ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$NPR/sum(GBa.area.box)*1e6  ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Biomass
windows(8,8)
plot(nobox.5$IPR ~ nobox.5$year,type='o',lwd=2,col="red",ylim=c(0,10000),xlab="",
     ylab="Biomass (tonnes)",pch=21,bg= "red",main = "Pre-recruit Biomass GBa")
points(survey.5$IPR ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$IPR ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Biomass per tow
windows(8,8)
plot(nobox.5$IPR/sum(GBa.area.nobox)*1e3 ~ nobox.5$year,type='o',lwd=2,col="red",
     ylim=c(0,10),xlab="",ylab="Biomass per tow",pch=21,bg= "red",main = "Pre-recruit Biomass GBa")
points(survey.5$IPR/sum(GB.area.total)*1e3 ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$IPR/sum(GBa.area.box)*1e3 ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")






## More typical plots....

windows(8,8)
par(mfrow=c(1,1))
survey.ts(survey.no.box[[1]],1981:yr,Bank=bnk[2],pdf=F, areas=GBa.sa.no.box$towable_area,ys=.8,CS=CS[length(years)]-5,
          RS=RS[length(years)]-5, ht=6.5,wd=10,clr='blue',se=T,pch=16,yl2=c(2000,350,350),
          add.title = T,cx.mn = 3,axis.cx=1.5)

survey.ts(survey.box[[1]],1981:yr,Bank=bnk[2],pdf=F, areas=GBa.sa.box$towable_area,ys=.8,CS=CS[length(years)]-5,
          RS=RS[length(years)]-5, ht=6.5,wd=10,clr='blue',se=T,pch=16,yl2=c(5000,1000,1500),
          add.title = T,cx.mn = 3,axis.cx=1.5)

# Biomass
survey.ts(survey.no.box[[1]],1981:yr,Bank=bnk[2],pdf=F,type='B', areas=GBa.sa.no.box$towable_area,ys=.8,CS=CS[length(years)]-5,
          RS=RS[length(years)]-5, ht=6.5,wd=10,clr='blue',se=T,pch=16,yl2=5.2,
          add.title = T,titl=survey.ts.BM.title,cx.mn = 3,axis.cx=1.5)

survey.ts(survey.box[[1]],1981:yr,Bank=bnk[2],pdf=F, type="B",areas=GBa.sa.box$towable_area,ys=.8,CS=CS[length(years)]-5,
          RS=RS[length(years)]-5, ht=6.5,wd=10,clr='blue',se=T,pch=16,yl2=c(15,12,25),
          add.title = T,titl=survey.ts.BM.title,cx.mn = 3,axis.cx=1.5)


# Shell height frequency
shf.plt(ps.dat,survey.no.box,from='surv',yr=2008:yr,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),adj=0.9,
        rel=F,recline=c(RS[length(years)]-5,CS[length(years)]-5),ymax=310,mean.line=F,titl=SHF.title,cex.mn = 3,
        add.title = T,rows=8)	

# Shell height frequency
shf.plt(ps.dat,survey.box,from='surv',yr=2008:yr,col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),adj=0.9,
        rel=F,recline=c(RS[length(years)]-5,CS[length(years)]-5),ymax=310,mean.line=F,titl=SHF.title,cex.mn = 3,
        add.title = T,rows=8)	

# Meat weight CF time series

par(mfrow=c(3,1),omi=c(0.3,0.6,0.3,0.2))
stdts.plt(subset(survey.no.box[[1]],year>1995),y="l.bar",pch=17,lty=1,ylab="Average\n shell\n height\n (mm)",las=1,
          mean.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
stdts.plt(subset(survey.no.box[[1]],year>1995),y="CF",pch=17,lty=1,las=1, ylab=cf.lab,
          mean.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
stdts.plt(subset(survey.no.box[[1]],year>1995),y="w.bar",pch=17,lty=1,ylab="Average\n meat\n weight\n(g)",
          mean.line=T,graphic='none',xlab='',labcx=1.2,las=1,axis.cx=1.2)
title(paste("Shell height, Condition factor, Meat weight (",bnk[2],"-",yr,")",sep=""), cex.main=3,outer=T)


par(mfrow=c(3,1),omi=c(0.3,0.6,0.3,0.2))
stdts.plt(subset(survey.box[[1]],year>1995),y="l.bar",pch=17,lty=1,ylab="Average\n shell\n height\n (mm)",las=1,
          mean.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
stdts.plt(subset(survey.box[[1]],year>1995),y="CF",pch=17,lty=1,las=1, ylab=cf.lab,
          mean.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
stdts.plt(subset(survey.box[[1]],year>1995),y="w.bar",pch=17,lty=1,ylab="Average\n meat\n weight\n(g)",
          mean.line=T,graphic='none',xlab='',labcx=1.2,las=1,axis.cx=1.2)
title(paste("Shell height, Condition factor, Meat weight (",bnk[2],"-",yr,")",sep=""), cex.main=3,outer=T)






##########################################################################################
## Now lets do the same thing on BBn
##########################################################################################

### BBn boxes....
surv.Rand[["BBn"]]$EID<-1:nrow(surv.Rand[["BBn"]])
surv.Rand[["BBn"]]$X<-surv.Rand[["BBn"]]$slon
surv.Rand[["BBn"]]$Y<-surv.Rand[["BBn"]]$slat
surv.Live[["BBn"]]$EID<-1:nrow(surv.Live[["BBn"]])
surv.Live[["BBn"]]$X<-surv.Live[["BBn"]]$slon
surv.Live[["BBn"]]$Y<-surv.Live[["BBn"]]$slat

## Now lets pull out both BBN boxes rather than just the  one and look at impact...
C2_c3.rm <- findPolys(surv.Rand[["BBn"]],subset(seedboxes,ID %in% c('C2-012014',"C3-012014")))
C2_c3.rm.live <- findPolys(surv.Live[["BBn"]],subset(seedboxes,ID %in% c('C2-012014',"C3-012014")))

BBn.no.box <- surv.Rand[["BBn"]][-which(surv.Rand[["BBn"]]$EID %in% C2_c3.rm$EID),] # Tows
BBn.box <- surv.Rand[["BBn"]][which(surv.Rand[["BBn"]]$EID %in% C2_c3.rm$EID),]
BBn.no.box.live <- surv.Live[["BBn"]][-which(surv.Live[["BBn"]]$EID %in% C2_c3.rm.live$EID),]
BBn.box.live <- surv.Live[["BBn"]][which(surv.Live[["BBn"]]$EID %in% C2_c3.rm.live$EID),]

# This removes the seedboxes from our estimates
t2 <- joinPolys(subset(survey.detail.polys,label=="BBn"),subset(seedboxes,ID == c('C2-012014')),"DIFF")
BBn.survey.no.box <- joinPolys(t2,subset(seedboxes,ID == c('C3-012014')),"DIFF")

# This removes everything else from our estimates
#C2 <- joinPolys(subset(survey.detail.polys,label=="BBn"),subset(seedboxes,ID %in% c('C2-012014')),"INT")
#C3 <- joinPolys(subset(survey.detail.polys,label=="BBn"),subset(seedboxes,ID == c('C3-012014')),"INT")


# Get the area estimates for BBn with and without the seedboxes. The cacluations from
# PBS mapping don't quite jive with our strata area numbers, but for the purposes of this I think this should be o.k.
BBn.area.nobox <-calcArea(BBn.survey.no.box,rollup = 1)
BBn.area.nobox$area <- BBn.area.nobox$area*10000 # I'm not sure why I need the 10,000 so that worries me something is screwy!
BBn.area.nobox$towable_area <- BBn.area.nobox$area*1000*1000/800/(8/3.2808)

BBn.area.total <- calcArea(subset(survey.detail.polys,label=="BBn"),rollup = 1)
BBn.area.total$area <- BBn.area.total$area*10000  # I'm not sure why I need the 10,000 so that worries me something is screwy!
BBn.area.total$towable_area <- BBn.area.total$area*1000*1000/800/(8/3.2808)

BBn.area.box <- BBn.area.nobox
BBn.area.box$area <- BBn.area.total$area - BBn.area.nobox$area
BBn.area.box$towable_area <- BBn.area.box$area*1000*1000/800/(8/3.2808)
BBn.surv.info <- subset(survey.info, label= "BBn")

# Make sure these are right
windows(8,8)
ScallopMap("BBn",poly.lst = list(BBn.survey.no.box,subset(survey.info,label=="BBn")))
#ScallopMap("BBn",poly.lst = list(t2,subset(survey.info,label=="BBn")))

# These shoud already be alinged if looking at all the data from 1981 to current.
# Years
survey.no.box <- survey.dat(BBn.no.box, years=1992:2016, RS=85, CS=95, bk=bnk[2], areas=BBn.area.nobox[,c(1,3)], mw.par="CFh")
survey.box<-survey.dat(BBn.box,years=1992:2016,RS=85, CS=95,areas=BBn.area.box[,c(1,3)], mw.par="CFh")	
survey.obj<- survey.dat(surv.Rand[["BBn"]], years=1992:2016, RS=RS, CS=CS, bk=bnk[2], areas=BBn.area.total[,c(1,3)], mw.par="CFh")	
survey.no.box[[1]]$CF<-sapply(1:length(years),
                              function(x){with(subset(BBn.no.box,year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})
survey.box[[1]]$CF<-sapply(1:length(years),
                           function(x){with(subset(BBn.box,year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})
survey.obj[[1]]$CF<-sapply(1:length(years),
                           function(x){with(subset(surv.Rand[["BBn"]],year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})

# Now do the same with all of the data (i.e. include exploratory tows)
survey.no.box.all <- survey.dat(BBn.no.box.live, years=1992:2016, RS=RS, CS=CS, bk=bnk[2], areas=BBn.area.nobox[,c(1,3)], mw.par="CFh")
survey.box.all<-survey.dat(BBn.box.live,years=1992:2016,RS=RS, CS=CS,areas=BBn.area.box[,c(1,3)], mw.par="CFh")	
survey.obj.all<- survey.dat(surv.Live[["BBn"]], years=1992:2016, RS=RS, CS=CS, bk=bnk[2], areas=BBn.area.total[,c(1,3)], mw.par="CFh")	
survey.no.box.all[[1]]$CF<-sapply(1:length(years),
                                  function(x){with(subset(BBn.no.box.live,year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})
survey.box.all[[1]]$CF<-sapply(1:length(years),
                               function(x){with(subset(BBn.box.live,year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})
survey.obj.all[[1]]$CF<-sapply(1:length(years),
                               function(x){with(subset(surv.Live[["BBn"]],year==years[x]),weighted.mean(CF,com.bm,na.rm=T))})



# This is actually really interesting, here are the trends in the last 5 years, inside, outside, and within the seedboxes (aggregated)
nobox.5 <- tail(survey.no.box[[1]],n=25)
survey.5 <-  tail(survey.obj[[1]],n=25)
box.5 <- tail(survey.box[[1]],n=25)

# Fully recruited
windows(8,8)
plot(nobox.5$N ~ nobox.5$year,type='o',lwd=2,col="red",ylim=c(0,1000),xlab="",
     ylab="Number (millions)",pch=21,bg= "red",main = "Fully Recruited Abundance BBn")
points(survey.5$N ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$N ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")

# Fully recruited biomass
windows(8,8)
plot(nobox.5$I ~ nobox.5$year,type='o',lwd=2,col="red",xlab="",ylim=c(50,8000),
     ylab="Biomass (tonnes)",pch=21,bg= "red",main = "Survey Biomass BBn",log='y')
points(survey.5$I ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$I ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")


nobox.5 <- tail(survey.no.box.all[[1]],n=25)
survey.5 <-  tail(survey.obj.all[[1]],n=25)
box.5 <- tail(survey.box.all[[1]],n=25)

# Fully recruited biomass log scale
windows(8,8)
plot(nobox.5$I ~ nobox.5$year,type='o',lwd=2,col="red",xlab="",ylim=c(50,8000),
     ylab="Biomass (tonnes)",pch=21,bg= "red",main = "Survey Biomass BBn",log='y')
points(survey.5$I ~ survey.5$year,type='o',lwd=2,xlab="",ylab="",pch=22,bg="blue",col="blue")
points(box.5$I ~ box.5$year,type='o',lwd=2,col="orange",xlab="",ylab="",pch=23,bg="orange")
legend("topright",c("No seedboxes","Complete bank", "Seedboxes"),lwd=2,
       pt.bg= c("red","blue","orange"),col=c("red","blue","orange"),pch=c(21,22,23),bty="n")


