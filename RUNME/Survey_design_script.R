# This is where you can run your different options for the survey design, a basic call is below, look in survey design folder for function details.
## Created by DK Aguust 2016
## Update history

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  
##
###############################################################################################################

###############################################################################################################
## This script needs these functions to work (a.k.a. "support files")
# 1: source(paste(direct,"Assessment_fns/Survey_design/Survey_design.r",sep=""))
# 
###############################################################################################################
yr <- 2022
#direct = "d:/r/"
direct <- "Y:/Offshore/Assessment/"
# set direct_fns if you don't want to use the marscal github version
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"
source(paste(direct_fns,"Survey_design/Survey_design_sf.r",sep=""))

# USE SF IN 2022!!!

# Run the survey design, pick your year, bank(s) and other options to create the survey design for a given year.
Survey.design(yr = yr,banks = "Ger",direct = direct,export=T,relief.plots = F,fig="screen",seed=62294, repo=direct_fns,
              point.style = "points", x.adj=0.025, y.adj=0.005, ger.new = 80,add.extras=F, ger.rep=20)
Survey.design(yr = yr,banks = "BBn",direct = direct,export=F,relief.plots = F,fig="screen",seed=(yr-2000), repo=direct_fns,
              point.style = "points", x.adj=0.025, y.adj=0.005, ger.new = 80,add.extras=F, ger.rep=20, cables=T)
Survey.design(yr = yr,banks = "Mid",direct = direct,export=T,relief.plots = F,fig="screen",seed=(yr-20), repo=direct_fns,
              point.style = "points", x.adj=0.04, y.adj=0.005, ger.new = 80,add.extras=F, ger.rep=20)
Survey.design(yr = yr,banks = "GB",direct = direct,export=T,relief.plots = F,fig="screen",seed=20, repo=direct_fns,
              point.style = "points", x.adj=0.04, y.adj=0.005, ger.new = 80,add.extras=F, ger.rep=20)
Survey.design(yr = yr,banks = "Sab",direct = direct, export=T,relief.plots = F,fig="screen",seed=(yr-2000), repo=direct_fns,
              point.style = "points", x.adj=0.03, y.adj=0.001, ger.new = 80,add.extras=F, ger.rep=20)
Survey.design(yr = yr,banks = "BBs",direct = direct,export=T,relief.plots = F,fig="screen",seed=(yr-2000), repo=direct_fns,
              point.style = "points", x.adj=0.04, y.adj=0.001, ger.new = 80,add.extras=F, ger.rep=20)

# use fig="leaflet" if you want an interactive figure. No strata yet though
seed1 <- floor(runif(n = 1, min = 10000, max=99999)) 
# 2022 seed1 = 14188
for(i in seed1:(seed1+4)){
  # Survey.design(yr = yr,banks = "Ger",direct = direct,export=T,relief.plots = F,fig="png",seed=i, repo=direct_fns,
  #                point.style = "both", x.adj=0.015, y.adj=0.001, ger.new = 80,add.extras=F, ger.rep=20)
  Survey.design(yr = yr,banks = "BBn",direct = direct,export=F,relief.plots = F,fig="png",seed=i, repo=direct_fns,
                point.style = "both", x.adj=0.01, y.adj=0.001, ger.new = 80,add.extras=F, ger.rep=20, cables=T)
  # # Survey.design(yr = yr,banks = "BBs",direct = direct,export=T,relief.plots = F,fig="png",seed=i, repo=direct_fns,
  # #               point.style = "points", x.adj=0.04, y.adj=0.001, ger.new = 80,add.extras=F, ger.rep=20)
  # Survey.design(yr = yr,banks = "Mid",direct = direct,export=T,relief.plots = F,fig="png",seed=i, repo=direct_fns,
  #               point.style = "both", x.adj=0.01, y.adj=0.001, ger.new = 80,add.extras=F, ger.rep=20)
  # Survey.design(yr = yr,banks = "GB",direct = direct,export=T,relief.plots = F,fig="png",seed=i, repo=direct_fns,
  #               point.style = "both", x.adj=0.02, y.adj=0.001, ger.new = 80,add.extras=F, ger.rep=20)
  # Survey.design(yr = yr,banks = "Sab",direct = direct, export=T,relief.plots = F,fig="png",seed=i, repo=direct_fns,
  #               point.style = "both", x.adj=0.015, y.adj=0.001, ger.new = 80,add.extras=T, ger.rep=20)
}
# for(i in c(19174:1984)){
i <- 19173
Survey.design(yr = yr,banks = "GBb",direct = direct, export=T,relief.plots = F,fig="png",seed=i,
              point.style = "points", x.adj=0.04, y.adj=0.001, ger.new = 80,add.extras=F, ger.rep=20)
#}

# get bearing from 2012 Ban survey
source(paste(direct,"Assessment_fns/Survey_and_OSAC/get.offshore.survey.brg.r",sep=""))
DBpull<-get.offshore.survey(db.con = db.con, un=un.ID , pw = pwd.ID,direct=direct)
  

# Add in proposed extra stations, if these are surveyed they will need to go into the file called "Extra_stations.csv" located in the Data/Survey_data folder...
# These are proposed extras on GBA in 2017.

#windows(11,8.5)
Survey.design(yr = yr,banks = "GBa",direct = direct,export=F,fig="pdf",seed=(yr-2000),text.points = F,zoom=T)
#addPoints(pts,pch=19,cex=0.5)


# Bring in the proposed GBa stations...
#dat <- read.csv(paste(direct,"Data/Survey_data/2017/Summer/GBa/Survey_design_Tow_locations.csv",sep=""))
#require(raster)
#r = raster(dat)
##plot(r)
#writeRaster(r,"r.tiff","GTiff")
# Bring in flat files to make GBA look pretty
#surv.polyset <- read.csv(paste(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv",sep=""),stringsAsFactors = F) #Read1
##surv.polydata <- read.csv(paste(direct,"Data/Survey_data/survey_information.csv",sep=""),stringsAsFactors = F)#Read2
#seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes.csv",sep=""),
#stringsAsFactors = F,header=T) # Read3
#sb <- subset(seedboxes,Bank == "GBa" & Open >= paste(yr,"-01-01",sep=""))
# Get the correct survey polygons
#surv.poly <- subset(surv.polyset,label=="GBa")
#attr(surv.poly,"projection")<-"LL"
#polydata <- subset(surv.polydata,label=="GBa")

# Once the tows are ordered (Done in coordination with Ginette after the figures are laid out) we need to make plots with the correct numbering on them.
# This should get rolled into the Survey.design function once we have the bugs sorted out.

direct <- "d:/r/" # If running from network change to "Y:/Offshore Scallop/Assessment/"
surv.polyset <- read.csv(paste(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv",sep=""),stringsAsFactors = F) #Read1
surv.polydata <- read.csv(paste(direct,"Data/Survey_data/survey_information.csv",sep=""),stringsAsFactors = F)#Read2
#seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes.csv",sep=""),
#stringsAsFactors = F,header=T) # Read3
#sb <- subset(seedboxes,Bank == "GBa" & Open >= paste(yr,"-01-01",sep=""))
# Get the correct survey polygons
ger.tow.list <- read.csv(paste(direct,"Data/Survey_data/2017/Spring/Ger/GerTowlist_2017.csv",sep="")) 

png(paste(direct,"2017/Survey_design/Ger/Ger_Final_survey_stations.png",sep=''),width = 8.5, height = 8.5,bg = "transparent",units="in",res=960)

Ger.polyset <- subset(read.csv(paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),stringsAsFactors = F),label=="Ger")
Ger.polyset$PID <- 1 # Force the PID to be 1, since it is a boundary there is only 1 unique PID...
attr(Ger.polyset,"projection")<-"LL"
ScallopMap("Ger",plot.bathy = T,plot.boundries = T,boundries = "offshore")
addPolys(Ger.polyset,border=NA,col=rgb(0,0,0,0.2))
text(ger.tow.list$X[ger.tow.list$Tow_type == "Random"],ger.tow.list$Y[ger.tow.list$Tow_type == "Random"],
     labels=ger.tow.list$Station[ger.tow.list$Tow_type == "Random"],cex=0.7)
text(ger.tow.list$X[ger.tow.list$Tow_type == "Repeated"],ger.tow.list$Y[ger.tow.list$Tow_type == "Repeated"],
     labels=ger.tow.list$Station[ger.tow.list$Tow_type == "Repeated"],cex=0.7,col= "blue")
title(paste("German Final Tow Locations",2017,sep=" "))
legend("bottomleft",c("New tows (black)","Repeated tows (blue)"),text.col=c("black","blue"),bty="n")
dev.off()


sab.tow.list <- read.csv(paste(direct,"Data/Survey_data/2017/Spring/Sab/SabTowlist_2017.csv",sep="")) 
surv.poly <- subset(surv.polyset,label=="Sab")
attr(surv.poly,"projection")<-"LL"
surv.info <- subset(surv.polydata,label=="Sab")
png(paste(direct,"2017/Survey_design/Sab/Sab_Final_survey_stations.png",sep=''),width = 8.5, height = 8.5,bg = "transparent",units="in",res=960)
ScallopMap("Sab",plot.bathy=T,plot.boundries =T,poly.lst=list(surv.poly,surv.info))                         
text(sab.tow.list$X,sab.tow.list$Y,labels=sab.tow.list$Station,cex=0.7)
title(paste("Sable Final Tow Locations",2017,sep=" "))
dev.off()

bbn.tow.list <- read.csv(paste(direct,"Data/Survey_data/2017/Spring/BBn/BBnTowlist_2017.csv",sep="")) 
surv.poly <- subset(surv.polyset,label=="BBn")
attr(surv.poly,"projection")<-"LL"
surv.info <- subset(surv.polydata,label=="BBn")
png(paste(direct,"2017/Survey_design/BBn/BBn_Final_survey_stations.png",sep=''),width = 8.5, height = 8.5,bg = "transparent",units="in",res=960)
ScallopMap("BBn",plot.bathy=T,plot.boundries =T,poly.lst=list(surv.poly,surv.info))                         
text(bbn.tow.list$X,bbn.tow.list$Y,labels=bbn.tow.list$Station,cex=0.7)
title(paste("BBn Final Tow Locations",2017,sep=" "))
dev.off()
