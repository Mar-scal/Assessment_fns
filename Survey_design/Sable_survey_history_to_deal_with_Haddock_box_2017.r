## This script was tossed together to take a look at the historical location of tows on Sable Bank.

direct = "d:/r/"
yr = 2016
source(paste(direct_fns,"Survey_and_OSAC/OSAC_fishery_figures.r",sep="")) #Source1
source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct_fns,"Fishery/fishery.dat.r",sep=""))
source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))
library(PBSmapping)

# These are the coordinates for the much anticipated "Star box"
star.box <- as.PolySet(data.frame(PID = c(1,1,1,1),POS=c(1,2,3,4),X = c(-61.3,-61.5,-61.5,-61.3),Y=c(43.83333,43.83333,43.5,43.5)))


## Sable bank VMS data
load(file=paste(direct,"SPERA/CWP/Output/Paper_figures_data_VMS.RData",sep=""))

pdf(paste(direct,"2017/Survey_Design/Sab/Sable_VMS.pdf",sep=""),width = 11, height = 8.5,bg = "transparent")
ScallopMap("Sab")
addPolys(star.box,border="blue",lwd=2,lty=2)
points(VMS.lb$X,VMS.lb$Y,pch=19,cex=0.1)
dev.off()


load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
# Get the  bank survey boundary polygon
bound.poly.surv <- subset(survey.bound.polys,label=="Sab") 
# These need a different boundary polygon
attr(bound.poly.surv,"projection")<-"LL"

#Detailed survey polygones
detail.poly.surv <- subset(survey.detail.polys,label=="Sab")
attr(detail.poly.surv,"projection")<-"LL"
# Get the strata areas.
strata.areas <- subset(survey.info,label=="Sab",select =c("PID","towable_area"))
#Get all the details of the survey strata
surv.info <- subset(survey.info,label== "Sab")
pdf(paste(direct,"2017/Survey_Design/Sab/History_of_sable_tows.pdf",sep=""),onefile=T,width = 11, height = 8.5,bg = "transparent")
  #windows(11,8.5)
  #par(mfrow=c(1,1))
  ScallopMap("Sab",poly.lst=list(detail.poly.surv,surv.info),direct = direct, direct_fns = direct_fns,cex.mn=2, boundries="offshore",
               plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
               nafo.bord = F,nafo.lab = F,title="Historic Survey Tows Sable",dec.deg = F)
  # This gets all the points ever...
  addPolys(star.box,border="blue",lwd=2,lty=2)
  points(lat~lon,surv.Live[["Sab"]],subset= state=='live',pch=20,bg='black',cex=0.4)
  
  
  years <- unique(surv.Live[["Sab"]]$year)
  
  
  par(mfrow=c(2,2))
  for(j in 1:length(years))
  {
    ScallopMap("Sab",poly.lst=list(detail.poly.surv,surv.info),direct = direct, direct_fns = direct_fns,cex.mn=2, boundries="offshore",
               plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
               nafo.bord = F,nafo.lab = F,title=paste("Survey Tows Sable",years[j]),dec.deg = F)
   # This gets all the points ever...
    addPolys(star.box,border="blue",lwd=2,lty=2)
    points(lat~lon,surv.Live[["Sab"]],subset= year == years[j] & state=='live',pch=20,bg='black',cex=0.4)
   
  }
  dev.off()
  

  
  # Get the offshore logs...
  
  yr <- 2016
  years = 1980:yr # The years of interest make temporary as this could be overwritten when I bring in the survey results...
  direct = "D:/r/"
  un=un.ID # Your SQL username
  pw=pwd.ID # Your SQL password.
  db.con="ptran"
  # Load functions and external datafiles we might need
  
  # So we update the fishery data with the lastest for this analysis in case the above is out of date.
  # This queries the offshore so gets the most up to date fishery information which we should be sure we are using!
  logs_and_fish(loc="offshore",year = years,un=un,pw=pwd,db.con=db.con,direct.off=direct)
  fish.dat<-merge(new.log.dat,old.log.dat,all=T)
  fish.dat$ID<-1:nrow(fish.dat)
  
  pdf(paste(direct,"2017/Survey_Design/Sab/Sable_log_records.pdf",sep=""),onefile=T,width = 11, height = 8.5,bg = "transparent")
  ScallopMap("Sab",poly.lst=list(detail.poly.surv,surv.info),direct = direct, direct_fns = direct_fns,cex.mn=2, boundries="offshore",
             plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
             nafo.bord = F,nafo.lab = F,title="Historic Sable Logbook Records",dec.deg = F)
  # This gets all the points ever...
  addPolys(star.box,border="blue",lwd=2,lty=2)
  points(fish.dat$lon,fish.dat$lat,pch=19,cex=0.2)
  
  years <- sort(unique(fish.dat$year))
  
  
  par(mfrow=c(2,2))
  for(j in 1:length(years))
  {
    ScallopMap("Sab",poly.lst=list(detail.poly.surv,surv.info),direct = direct, direct_fns = direct_fns,cex.mn=2, boundries="offshore",
               plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
               nafo.bord = F,nafo.lab = F,title=paste("Annual Logs Sable",years[j]),dec.deg = F)
    # This gets all the points ever...
    addPolys(star.box,border="blue",lwd=2,lty=2)
    points(lat~lon,fish.dat,subset= year == years[j] ,pch=20,bg='black',cex=0.4)
  }
  dev.off()
 
  