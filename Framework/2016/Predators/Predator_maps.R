################################################################################################################
##### This script is used to map out the distribution of predators on GB, this data is from
##### Clearwaters video survey work in 2015, provided by Jim Mosher, please retain results in house!
################################################################################################################
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  None, this is a top level file whose output is either csv files or figures.
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files"), 
# see "Source_relation_SurveySummary.docx" for complete details
#   9:  ScallopMap.r

##
###############################################################################################################
direct <- "d:/r/"
yr <- 2016
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) # Source9 Revised by DK September 2015
source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) 
library(viridis)
library(PBSmapping)

# Bing in the predator data
preds <- read.csv(paste(direct,"Data/Model/2016/Framework/Predators/GB_2015_pred_video_counts.csv",sep=""))
# Bring in the survey boundary data
survey.bound.polys<-read.csv(paste(direct,"data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
                                                   header=T,stringsAsFactors = F)


# Now process the data, first get the right survey boundaries...
# Get the  bank survey boundary polygon
bound.poly.surv <- subset(survey.bound.polys,label == "GB")
# These need a different boundary polygon
attr(bound.poly.surv,"projection")<-"LL"


# Now get the predators into decent shape.
# Subset it to the station data, this is fine enough for visualization purposes
preds <- subset(preds, select = c("Area","Station","Latitude","Longitude","SeaStars","Crabs","HermitCrabs"))
attr(preds,"projection")<-"LL"
preds$Unique_ID <- paste(preds$Area,preds$Station,sep="_")


# Now count the total numbers observed at each station and take the "mean" location for a station and stictch the data back together.
stn.preds <- aggregate(cbind(SeaStars,Crabs,HermitCrabs) ~ Unique_ID,FUN = sum,data=preds)
# Crabs/Hermitcrabs are simply presence/absence, any number > 1 should just indicate that they found it at multiple stations.
stn.preds$Crabs[stn.preds$Crabs > 1] <- 1
stn.preds$HermitCrabs[stn.preds$HermitCrabs > 1] <- 1
stn.loc <- aggregate(cbind(Latitude,Longitude) ~ Unique_ID,FUN = mean,data=preds)
stn.dat <- merge(stn.preds,stn.loc,by="Unique_ID")

#names(stn.dat) <- c("EID","SeaStars","Crabs","HermitCrabs","Y","X")
attr(stn.dat,"projection")<-"LL"




star.con<-contour.gen(subset(stn.dat, select = c('Unique_ID','Longitude','Latitude','SeaStars')),
                                      ticks=c(0,1,2,5,10,20,30,40),
                                      str.min=0,interp.method='gstat',points=T,blank=T,res=0.005,key='log.cont',blank.dist=0.05,
                                      color.fun=tim.colors,id.par=5,units='#/tow',
                                      plot=F,subscale=0.1,direct =direct)


lvls=c(0,1,2,5,10,20,30,40)
CL <- contourLines(star.con$image.dat,levels=lvls)
CP <- convCP(CL)
Cont.poly <- joinPolys(CP$PolySet,bound.poly.surv)
cont.data<- data.frame(PID=1:length(lvls),col=rev(magma(length(lvls),alpha=1,begin=0,end=1)),border=NA,stringsAsFactors = F) 
#cont1.data<- data

#windows(11,11)
png(paste(direct,yr,"/Updates/Framework/Figures/Predators/Sea_stars.png",sep=""),units="in",width = 11, 
    height = 8.5,res=800,bg = "transparent")
ScallopMap("GBa",contour=list(Cont.poly,cont.data),title="Sea stars (2015 video survey)",
           plot.bathy=T,plot.boundries=T,boundries="offshore",bathy.source = "usgs",
           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
                      paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,
       title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')

points(stn.dat$Longitude,stn.dat$Latitude,cex=0.1)
dev.off()



crab.con<-contour.gen(subset(stn.dat, select = c('Unique_ID','Longitude','Latitude','Crabs')),
                      ticks=c(0,0.5,1,2),
                      str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',blank.dist=0.05,
                      color.fun=tim.colors,id.par=5,units='#/tow',
                      plot=F,subscale=0.1,direct =direct)

lvls=c(0,0.5,1,2)
CL <- contourLines(crab.con$image.dat,levels=lvls)
CP <- convCP(CL)
Cont.poly <- joinPolys(CP$PolySet,bound.poly.surv)
cont.data<- data.frame(PID=1:length(lvls),col=rev(magma(length(lvls),alpha=1,begin=0,end=1)),border=NA,stringsAsFactors = F) 
#cont1.data<- data

#windows(11,11)
png(paste(direct,yr,"/Updates/Framework/Figures/Predators/crabs.png",sep=""),units="in",width = 11, 
    height = 8.5,res=800,bg = "transparent")
ScallopMap("GBa",contour=list(Cont.poly,cont.data),title="Crabs (2015 video survey)",
           plot.bathy=T,plot.boundries=T,boundries="offshore",
           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
legend("bottomleft","Crabs present",fill=cont.data$col[1],
       title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')

points(stn.dat$Longitude,stn.dat$Latitude,cex=0.1)
dev.off()


hermits.con<-contour.gen(subset(stn.dat, select = c('Unique_ID','Longitude','Latitude','HermitCrabs')),
                      ticks=c(0,0.5,1,2),
                      str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',blank.dist=0.05,
                      color.fun=tim.colors,id.par=5,units='#/tow',
                      plot=F,subscale=0.1,direct =direct)

lvls=c(0,0.5,1,2)
CL <- contourLines(hermits.con$image.dat,levels=lvls)
CP <- convCP(CL)
Cont.poly <- joinPolys(CP$PolySet,bound.poly.surv)
cont.data<- data.frame(PID=1:length(lvls),col=rev(magma(length(lvls),alpha=1,begin=0,end=1)),border=NA,stringsAsFactors = F) 
#cont1.data<- data

#windows(11,11)
png(paste(direct,yr,"/Updates/Framework/Figures/Predators/hermit_crabs.png",sep=""),units="in",width = 11, 
    height = 8.5,res=800,bg = "transparent")
ScallopMap("GBa",contour=list(Cont.poly,cont.data),title="Hermit Crabs (2015 video survey)",
           plot.bathy=T,plot.boundries=T,boundries="offshore",
           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
legend("bottomleft","Hermit crabs present",fill=cont.data$col[1],
       title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
points(stn.dat$Longitude,stn.dat$Latitude,cex=0.1)
dev.off()