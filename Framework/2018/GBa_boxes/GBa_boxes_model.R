##################################################   Seedboxes   #########################################################		
##################################################   Seedboxes #########################################################		
##################################################   Seedboxes#########################################################		

# Now we can look at the boxes on GBa and pick a couple common ones and look at their history.  I also include 2 of the boxes which were opened in 2018 (I exclude the
# experimenatal boxes because it is too small)

yr = as.numeric(format(Sys.time(), "%Y")) -1 # 
direct = "d:/r/"
library(RColorBrewer)
library(PBSmapping)
library(R2jags)
library(VGAMdata)
library(ggplot2)
# Load the survey data.
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.RData",sep=""))



direct = "d:/r/"
# Get the correct Shell heights for GBa.
RS <- size.cats$RS[size.cats$Bank == "GBa"]
CS <- size.cats$CS[size.cats$Bank == "GBa"]

# Load functions and external datafiles we might need

# Load in the functions needed for this function to run.
source(paste(direct,"Assessment_fns/Model/projections.r",sep=""))
source(paste(direct,"Assessment_fns/Model/decision.r",sep=""))
source(paste(direct,"Assessment_fns/Model/post.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/exploit.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Model/fit.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/prediction_evaluation_function.r",sep="")) #load in the new prediction evaluation function
source(paste(direct,"Assessment_fns/Model/prediction_evaluation_figure.r",sep="")) #load in the new prediction evaluation figure function
source(paste(direct,"Assessment_fns/Model/biomass.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/OSAC_fishery_figures.r",sep="")) #Source1
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Fishery/fishery.dat.r",sep=""))
source(paste(direct,"Assessment_fns/Survey_and_OSAC/survey.dat.r",sep="")) #Source21 Revised by DK September 2015
source(paste(direct,"Assessment_fns/Survey_and_OSAC/simple.surv.r",sep="")) 
source(paste(direct,"Assessment_fns/Model/Update_function_JAGS.r",sep=""))

#Read1 Bring in the VonB model parameters
cat("We read in the von B growth parameters from the file .../Data/Ageing/Von_B_growth_parameters.csv")
vonB <- read.csv(paste(direct,"Data/Ageing/Von_B_growth_parameters.csv",sep=""))

# Get the fishery data for everywhere...
logs_and_fish(loc="offshore",year = 1981:yr,un=un,pw=pwd,db.con=db.con,direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

# Get rid of the "Boot" seedbox from the list as we don't have the coordinates for it and it's causing problems...
seedboxes <- seedboxes[seedboxes$ID != "Boot",]

# Now plot the fishing by year for GBa, overlay the closures we have information for, any evidence for closures at annual scale before 2008?
pdf(paste(direct,"2018/Framework/GBa_box_model/Annual_fishery_locations.pdf",sep=''),width = 8.5, height = 8.5,bg = "transparent",onefile=T)
for(i in 1981:2017)
{
ScallopMap("GBa",direct=direct)
addPolys(seedboxes[seedboxes$Bank == "GBa",],lwd=0.5,lty=2)
points(fish.dat$lon[fish.dat$year == i],fish.dat$lat[fish.dat$year == i],pch=19,cex=0.2)
title(i)
}
dev.off()

# Make the names for surveys a little cleaner...
GBa.live <- surv.Live[["GBa"]]
GBa.clap <- surv.Clap[["GBa"]]

### GBa boxes....
# Make an EID and X & Y so that PBSmapping can pull out the tows in the right spots, the first will pull based on tow start location

GBa.live$EID<-1:nrow(GBa.live)
GBa.live$X<-GBa.live$slon
GBa.live$Y<-GBa.live$slat
GBa.live$Strata_ID <- 1
# Need the clappers too...
GBa.clap$EID<-1:nrow(GBa.clap)
GBa.clap$X<-GBa.clap$slon
GBa.clap$Y<-GBa.clap$slat
GBa.clap$Strata_ID <- 1

# And subset the fish.dat so it's just GBa.
GBa.fish.dat <- fish.dat[fish.dat$bank == "GBa" & fish.dat$year >= 1984 & !is.na(fish.dat$bank),]
GBa.fish.dat$EID<-1:nrow(GBa.fish.dat)
GBa.fish.dat$X<-GBa.fish.dat$lon
GBa.fish.dat$Y<-GBa.fish.dat$lat
GBa.fish.dat <- GBa.fish.dat[!is.na(GBa.fish.dat$EID),]

# Now get the tows that are inside the boxes based on the start location
## So lets run with using the two new boxes and pull out survey and fishery data from these.  I want to model each box seperately.
seeds.tows<- findPolys(GBa.live,subset(seedboxes,ID %in% "Seed box (2012 modified)"))
seeds.clap <- findPolys(GBa.clap,subset(seedboxes,ID %in% "Seed box (2012 modified)"))
peanuts.tows<- findPolys(GBa.live,subset(seedboxes,ID %in% "Peanut (2010)"))
peanuts.clap <- findPolys(GBa.clap,subset(seedboxes,ID %in% "Peanut (2010)"))
# Now get C4 and C5 in here
C4s.tows<- findPolys(GBa.live,subset(seedboxes,ID %in% "C4-122016"))
C4s.clap <- findPolys(GBa.clap,subset(seedboxes,ID %in% "C4-122016"))
C5s.tows<- findPolys(GBa.live,subset(seedboxes,ID %in% "C5-122016"))
C5s.clap <- findPolys(GBa.clap,subset(seedboxes,ID %in% "C5-122016"))

# Make a dataframe with EID's, PID's
seed.tows <- data.frame(EID = unique(c(unique(seeds.tows$EID),unique(seeds.tows$EID))),PID = unique(subset(seedboxes,ID %in% "Seed box (2012 modified)")$PID), Bdry=0)
peanut.tows <- data.frame(EID = unique(c(unique(peanuts.tows$EID),unique(peanuts.tows$EID))),PID = unique(subset(seedboxes,ID %in% "Peanut (2010)")$PID), Bdry=0)
seed.clap <- data.frame(EID = unique(c(unique(seeds.clap$EID),unique(seeds.clap$EID))),PID = unique(subset(seedboxes,ID %in% "Seed box (2012 modified)")$PID), Bdry=0)
peanut.clap <- data.frame(EID = unique(c(unique(peanuts.clap$EID),unique(peanuts.clap$EID))),PID = unique(subset(seedboxes,ID %in% "Peanut (2010)")$PID), Bdry=0)

C4.tows <- data.frame(EID = unique(c(unique(C4s.tows$EID),unique(C4s.tows$EID))),PID = unique(subset(seedboxes,ID %in% "C4-122016")$PID), Bdry=0)
C5.tows <- data.frame(EID = unique(c(unique(C5s.tows$EID),unique(C5s.tows$EID))),PID = unique(subset(seedboxes,ID %in% "C5-122016")$PID), Bdry=0)
C4.clap <- data.frame(EID = unique(c(unique(C4s.clap$EID),unique(C4s.clap$EID))),PID = unique(subset(seedboxes,ID %in% "C4-122016")$PID), Bdry=0)
C5.clap <- data.frame(EID = unique(c(unique(C5s.clap$EID),unique(C5s.clap$EID))),PID = unique(subset(seedboxes,ID %in% "C5-122016")$PID), Bdry=0)

# Subset the survey data for each seedbox
seed.survey <- GBa.live[GBa.live$EID %in% seed.tows$EID,] # Tows
peanut.survey <- GBa.live[GBa.live$EID %in% peanut.tows$EID,] # Tows
seed.clappers <- GBa.clap[GBa.clap$EID %in% seed.tows$EID,] # Tows
peanut.clappers <- GBa.clap[GBa.clap$EID %in% peanut.tows$EID,] # Tows

C4.survey <- GBa.live[GBa.live$EID %in% C4.tows$EID,] # Tows
C5.survey <- GBa.live[GBa.live$EID %in% C5.tows$EID,] # Tows
C4.clappers <- GBa.clap[GBa.clap$EID %in% C4.tows$EID,] # Tows
C5.clappers <- GBa.clap[GBa.clap$EID %in% C5.tows$EID,] # Tows


# Now find the fishery data located within each seedbox...
seed.fished <- findPolys(GBa.fish.dat,subset(seedboxes,ID %in% "Seed box (2012 modified)"))
peanut.fished <- findPolys(GBa.fish.dat,subset(seedboxes,ID %in% "Peanut (2010)"))
seed.fishery <- GBa.fish.dat[GBa.fish.dat$EID %in% seed.fished$EID,]
peanut.fishery <- GBa.fish.dat[GBa.fish.dat$EID %in% peanut.fished$EID,]
C4.fished <- findPolys(GBa.fish.dat,subset(seedboxes,ID %in% "C4-122016"))
C5.fished <- findPolys(GBa.fish.dat,subset(seedboxes,ID %in% "C5-122016"))
C4.fishery <- GBa.fish.dat[GBa.fish.dat$EID %in% C4.fished$EID,]
C5.fishery <- GBa.fish.dat[GBa.fish.dat$EID %in% C5.fished$EID,]

# DK note: This needs checked, there are a bunch of 0 hour effort records in here that I need to sort out what they are...
seed.fished <- findPolys(GBa.fish.dat,subset(seedboxes,ID %in% "Seed box (2012 modified)"))
peanut.fished <- findPolys(GBa.fish.dat,subset(seedboxes,ID %in% "Peanut (2010)"))
C4.fishery <- GBa.fish.dat[GBa.fish.dat$EID %in% C4.fished$EID,]
C5.fishery <- GBa.fish.dat[GBa.fish.dat$EID %in% C5.fished$EID,]


# Get the area of the seedboxes and the towable area
GBa.sb <- as.PolySet(seedboxes[seedboxes$Bank =="GBa",], projection="LL")
seed.area <- calcArea(GBa.sb[GBa.sb$ID == "Seed box (2012 modified)",],rollup=1)
seed.area$PID <- 1
seed.area$towable_area <- seed.area$area*1000*1000/800/(8/3.2808)
peanut.area <- calcArea(GBa.sb[GBa.sb$ID == "Peanut (2010)",],rollup=1)
peanut.area$towable_area <- peanut.area$area*1000*1000/800/(8/3.2808)
peanut.area$PID <- 1
C4.area <- calcArea(GBa.sb[GBa.sb$ID == "C4-122016",],rollup=1)
C4.area$PID <- 1
C4.area$towable_area <- C4.area$area*1000*1000/800/(8/3.2808)
C5.area <- calcArea(GBa.sb[GBa.sb$ID == "C5-122016",],rollup=1)
C5.area$towable_area <- C5.area$area*1000*1000/800/(8/3.2808)
C5.area$PID <- 1


# Now make the survey objects...
seed.obj <- survey.dat(seed.survey, RS=RS, CS=CS, bk="GBa", areas=seed.area[,c(1,3)], mw.par="CF",err="ran")	
seed.obj[[1]]$CF <- na.omit(sapply(1:length(unique(seed.survey$year)), function(x){with(subset(seed.survey,year == unique(seed.survey$year)[x]), weighted.mean(CF,com.bm,na.rm=T))}))
seed.clap.obj <- survey.dat(seed.clappers, RS=RS, CS=CS, bk="GBa", areas=seed.area[,c(1,3)], mw.par="CF",err="ran")	
seed.obj[[1]]$clappers<-seed.clap.obj[[1]]$N
seed.obj[[1]]$clappersR<-seed.clap.obj[[1]]$NR
seed.obj[[1]]$CS <- CS
seed.obj[[1]]$RS <- RS

peanut.obj <- survey.dat(peanut.survey, RS=RS, CS=CS, bk="GBa", areas=peanut.area[,c(1,3)], mw.par="CF",err="ran")	
peanut.obj[[1]]$CF <- na.omit(sapply(1:length(unique(peanut.survey$year)), function(x){with(subset(peanut.survey,year == unique(peanut.survey$year)[x]), weighted.mean(CF,com.bm,na.rm=T))}))
peanut.clap.obj <- survey.dat(peanut.clappers, RS=RS, CS=CS, bk="GBa", areas=peanut.area[,c(1,3)], mw.par="CF",err="ran")	
peanut.obj[[1]]$clappers<-peanut.clap.obj[[1]]$N
peanut.obj[[1]]$clappersR<-peanut.clap.obj[[1]]$NR
peanut.obj[[1]]$CS <- CS
peanut.obj[[1]]$RS <- RS

C4.obj <- survey.dat(C4.survey, RS=RS, CS=CS, bk="GBa", areas=C4.area[,c(1,3)], mw.par="CF",err="ran")	
C4.obj[[1]]$CF <- na.omit(sapply(1:length(unique(C4.survey$year)), function(x){with(subset(C4.survey,year == unique(C4.survey$year)[x]), weighted.mean(CF,com.bm,na.rm=T))}))
C4.clap.obj <- survey.dat(C4.clappers, RS=RS, CS=CS, bk="GBa", areas=C4.area[,c(1,3)], mw.par="CF",err="ran")	
C4.obj[[1]]$clappers<-C4.clap.obj[[1]]$N
C4.obj[[1]]$clappersR<-C4.clap.obj[[1]]$NR
C4.obj[[1]]$CS <- CS
C4.obj[[1]]$RS <- RS

# Thebox has had too many years with only 1 tow in it to be very useful, I won't bother modelling it, Peanut gives a good overview of how this region is doing anyways...
C5.obj <- survey.dat(C5.survey, RS=RS, CS=CS, bk="GBa", areas=C5.area[,c(1,3)], mw.par="CF",err="ran")	
C5.obj[[1]]$CF <- na.omit(sapply(1:length(unique(C5.survey$year)), function(x){with(subset(C5.survey,year == unique(C5.survey$year)[x]), weighted.mean(CF,com.bm,na.rm=T))}))
C5.clap.obj <- survey.dat(C5.clappers, RS=RS, CS=CS, bk="GBa", areas=C5.area[,c(1,3)], mw.par="CF",err="ran")	
C5.obj[[1]]$clappers<-C5.clap.obj[[1]]$N
C5.obj[[1]]$clappersR<-C5.clap.obj[[1]]$NR
C5.obj[[1]]$CS <- CS
C5.obj[[1]]$RS <- RS




# 
# pdf(paste(direct,"2018/Framework/GBa_box_model/Annual_survey_locations.pdf",sep=''),width = 8.5, height = 8.5,bg = "transparent",onefile=T)
# for(i in 1981:2016)
# {
#   ScallopMap("GBa",direct=direct)
#   addPolys(seedboxes[seedboxes$Bank == "GBa",],lwd=0.5,lty=2)
#   if(nrow(seed.survey[seed.survey$year ==i,]) > 0) addPoints(seed.survey[seed.survey$year ==i,],pch=19,cex=0.5)
#   if(nrow(peanut.survey[peanut.survey$year ==i,]) > 0) addPoints(peanut.survey[peanut.survey$year ==i,],pch=19,cex=0.5)
#   title(i)
# }
# dev.off()
# 
# pdf(paste(direct,"2018/Framework/GBa_box_model/All_Annual_survey_locations.pdf",sep=''),width = 8.5, height = 8.5,bg = "transparent",onefile=T)
# for(i in 1981:2016)
# {
#   ScallopMap("GBa",direct=direct)
#   addPolys(seedboxes[seedboxes$Bank == "GBa",],lwd=0.5,lty=2)
#   if(nrow(GBa.live[GBa.live$year ==i,]) > 0) addPoints(GBa.live[GBa.live$year ==i,],pch=19,cex=0.5)
#   
#   title(i)
# }
# dev.off()

###################  IMPORTANT SECTION HERE!!!!###################  IMPORTANT SECTION HERE!!!!
# DK Note I need to tidy up some of these data, this is a proposed way, I welcome other ideas...
# I need to deal with years in which there is only 1 tow in the box, there is no CV in these years.
# What I am doing is taking the 0's or NA's and filling in a value for them
# Also need to get a number on the clappers for years we didn't see any in the survey inside a box

# Make the 0's the median
seed.obj[[1]]$I.cv[seed.obj[[1]]$I.cv == 0] <- median(seed.obj[[1]]$I.cv[seed.obj[[1]]$I.cv != 0],na.rm=T)
seed.obj[[1]]$IR.cv[seed.obj[[1]]$IR.cv == 0 | is.nan(seed.obj[[1]]$IR.cv)] <- median(seed.obj[[1]]$IR.cv[seed.obj[[1]]$IR.cv != 0],na.rm=T)
seed.obj[[1]]$IPR.cv[seed.obj[[1]]$IPR.cv == 0] <- median(seed.obj[[1]]$IPR.cv[seed.obj[[1]]$IPR.cv != 0],na.rm=T)
seed.obj[[1]]$N.cv[seed.obj[[1]]$N.cv == 0 ] <- median(seed.obj[[1]]$N.cv[seed.obj[[1]]$N.cv != 0],na.rm=T)
seed.obj[[1]]$NR.cv[seed.obj[[1]]$NR.cv == 0| is.nan(seed.obj[[1]]$NR.cv)] <- median(seed.obj[[1]]$NR.cv[seed.obj[[1]]$NR.cv != 0],na.rm=T)
seed.obj[[1]]$NPR.cv[seed.obj[[1]]$NPR.cv == 0] <- median(seed.obj[[1]]$NPR.cv[seed.obj[[1]]$NPR.cv != 0],na.rm=T)
seed.obj[[1]]$clappersR[seed.obj[[1]]$clappersR == 0] <- median(seed.obj[[1]]$clappersR[seed.obj[[1]]$clappersR != 0],na.rm=T)
seed.obj[[1]]$clappers[seed.obj[[1]]$clappers == 0] <- median(seed.obj[[1]]$clappers[seed.obj[[1]]$clappers != 0],na.rm=T)
# This is for the l.k and w.k (no recruits...)
seed.obj[[1]]$l.k[is.nan(seed.obj[[1]]$l.k)] <- median(seed.obj[[1]]$l.k,na.rm=T)
seed.obj[[1]]$w.k[is.nan(seed.obj[[1]]$w.k)] <- median(seed.obj[[1]]$w.k,na.rm=T)
# In case we didn't see any recruits in a year
seed.obj[[1]]$IR[seed.obj[[1]]$IR == 0] <- median(seed.obj[[1]]$IR[seed.obj[[1]]$IR > 0],na.rm=T)

# Similar for the peanut data....
peanut.obj[[1]]$I.cv[peanut.obj[[1]]$I.cv == 0] <- median(peanut.obj[[1]]$I.cv[peanut.obj[[1]]$I.cv != 0],na.rm=T)
peanut.obj[[1]]$IR.cv[peanut.obj[[1]]$IR.cv == 0 | is.nan(peanut.obj[[1]]$IR.cv)] <- median(peanut.obj[[1]]$IR.cv[peanut.obj[[1]]$IR.cv != 0],na.rm=T)
peanut.obj[[1]]$NR.cv[peanut.obj[[1]]$NR.cv == 0| is.nan(peanut.obj[[1]]$NR.cv)] <- median(peanut.obj[[1]]$NR.cv[peanut.obj[[1]]$NR.cv != 0],na.rm=T)

# This is for the l.k and w.k (no recruits...)
peanut.obj[[1]]$l.k[is.nan(peanut.obj[[1]]$l.k)] <- median(peanut.obj[[1]]$l.k,na.rm=T)
peanut.obj[[1]]$w.k[is.nan(peanut.obj[[1]]$w.k)] <- median(peanut.obj[[1]]$w.k,na.rm=T)
peanut.obj[[1]]$IR[peanut.obj[[1]]$IR == 0] <- median(peanut.obj[[1]]$IR[peanut.obj[[1]]$IR > 0],na.rm=T)
peanut.obj[[1]]$NR[peanut.obj[[1]]$NR == 0] <- min(peanut.obj[[1]]$NR[peanut.obj[[1]]$NR != 0],na.rm=T)
peanut.obj[[1]]$clappersR[peanut.obj[[1]]$clappersR == 0] <- median(peanut.obj[[1]]$clappersR[peanut.obj[[1]]$clappersR > 0],na.rm=T)
peanut.obj[[1]]$clappers[peanut.obj[[1]]$clappers == 0] <- median(peanut.obj[[1]]$clappers[peanut.obj[[1]]$clappers > 0],na.rm=T)

# Now for the C4 box...
C4.obj[[1]]$I.cv[C4.obj[[1]]$I.cv == 0] <- median(C4.obj[[1]]$I.cv[C4.obj[[1]]$I.cv != 0],na.rm=T)
C4.obj[[1]]$IR.cv[C4.obj[[1]]$IR.cv == 0 | is.nan(C4.obj[[1]]$IR.cv)] <- median(C4.obj[[1]]$IR.cv[C4.obj[[1]]$IR.cv != 0],na.rm=T)
C4.obj[[1]]$NR.cv[C4.obj[[1]]$NR.cv == 0| is.nan(C4.obj[[1]]$NR.cv)] <- median(C4.obj[[1]]$NR.cv[C4.obj[[1]]$NR.cv != 0],na.rm=T)

# This is for the l.k and w.k (no recruits...)
C4.obj[[1]]$l.k[is.nan(C4.obj[[1]]$l.k)] <- median(C4.obj[[1]]$l.k,na.rm=T)
C4.obj[[1]]$w.k[is.nan(C4.obj[[1]]$w.k)] <- median(C4.obj[[1]]$w.k,na.rm=T)
C4.obj[[1]]$IR[C4.obj[[1]]$IR == 0] <- median(C4.obj[[1]]$IR[C4.obj[[1]]$IR > 0],na.rm=T)
C4.obj[[1]]$NR[C4.obj[[1]]$NR == 0] <- min(C4.obj[[1]]$NR[C4.obj[[1]]$NR != 0],na.rm=T)
C4.obj[[1]]$clappersR[C4.obj[[1]]$clappersR == 0] <- median(C4.obj[[1]]$clappersR[C4.obj[[1]]$clappersR > 0],na.rm=T)
C4.obj[[1]]$clappers[C4.obj[[1]]$clappers == 0] <- median(C4.obj[[1]]$clappers[C4.obj[[1]]$clappers > 0],na.rm=T)

# Now for the C5 box...
C5.obj[[1]]$I.cv[C5.obj[[1]]$I.cv == 0] <- median(C5.obj[[1]]$I.cv[C5.obj[[1]]$I.cv != 0],na.rm=T)
C5.obj[[1]]$IR.cv[C5.obj[[1]]$IR.cv == 0 | is.nan(C5.obj[[1]]$IR.cv)] <- median(C5.obj[[1]]$IR.cv[C5.obj[[1]]$IR.cv != 0],na.rm=T)
C5.obj[[1]]$NR.cv[C5.obj[[1]]$NR.cv == 0| is.nan(C5.obj[[1]]$NR.cv)] <- median(C5.obj[[1]]$NR.cv[C5.obj[[1]]$NR.cv != 0],na.rm=T)

# This is for the l.k and w.k (no recruits...)
C5.obj[[1]]$l.k[is.nan(C5.obj[[1]]$l.k)] <- median(C5.obj[[1]]$l.k,na.rm=T)
C5.obj[[1]]$w.k[is.nan(C5.obj[[1]]$w.k)] <- median(C5.obj[[1]]$w.k,na.rm=T)
C5.obj[[1]]$IR[C5.obj[[1]]$IR == 0] <- median(C5.obj[[1]]$IR[C5.obj[[1]]$IR > 0],na.rm=T)
C5.obj[[1]]$NR[C5.obj[[1]]$NR == 0] <- min(C5.obj[[1]]$NR[C5.obj[[1]]$NR != 0],na.rm=T)
C5.obj[[1]]$clappersR[C5.obj[[1]]$clappersR == 0] <- median(C5.obj[[1]]$clappersR[C5.obj[[1]]$clappersR > 0],na.rm=T)
C5.obj[[1]]$clappers[C5.obj[[1]]$clappers == 0] <- median(C5.obj[[1]]$clappers[C5.obj[[1]]$clappers > 0],na.rm=T)




###########################  Model RUNS###########################  Model RUNS###########################  Model RUNS#############################################
###########################  Model RUNS###########################  Model RUNS###########################  Model RUNS#############################################

##########################  THE seed MODEL #######################################  THE seed MODEL #######################################  THE seed MODEL #############
##########################  THE seed MODEL #######################################  THE seed MODEL #######################################  THE seed MODEL #############
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
# 
# Run this for one or both banks
# Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
years <- min(seed.obj[[1]]$year):max(seed.obj[[1]]$year)
# Bring in the vonB parameters..
vonB.par <-vonB[vonB$Bank =="GBa",]
# Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
cpue.dat <- fishery.dat(seed.fishery,bk="GBa",yr=c(1984:2017),method='jackknife',surv='August',
                        direct=direct,period = "survyr") 	
with(subset(seed.fishery,bank %in% "GBa" & year %in% 1985),tapply(pro.repwt,year,sum,na.rm=T))/1000
# Now on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
#for all other years we need to do this for Browns Bank North
# It really makes very little difference which way this is done as the catch in June-August
# has averaged around just 40 tonnes since about 1996.
if(any(is.na(cpue.dat))) cpue.dat[is.na(cpue.dat)] <- 0 # Set any NA's to 0. 
# Combine the survey and Fishery data here.
mod.dat.seed <- merge(seed.obj[[1]],cpue.dat,by ="year")
# DK Note For years in which we have essentially no fishing effort on the bank we need to blow up the variance of those estimates.
# If < 20 tonnes assume low effort and blow up the CV to 1.
#mod.dat.seed$cpue.se[mod.dat.seed$catch < 20] <- mod.dat.seed$cpue[mod.dat.seed$catch < 20]
# Get the CV for the CPUE...
mod.dat.seed$U.cv <- mod.dat.seed$cpue.se/mod.dat.seed$cpue
mod.dat.seed$U.cv[is.nan(mod.dat.seed$U.cv)] <- 1 # Doesn't really matter since CPUE =0

# now get the catch data from end of survey until end of the year for the projection
proj.sub <- subset(seed.fishery,year %in% years & months(as.Date(seed.fishery$date)) %in% c("September","October","November","December"))
# Again on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
# for all other years we need to do this for Browns Bank North
# Note that June-August seems to be a pretty minimal fishery on GBa
# Now calculate the fishery statistics for the projection period
proj.dat <- fishery.dat(proj.sub,bk="GBa",yr=(min(years)-1):max(years),method='jackknife',
                        direct=direct,period = "calyr") 	
# So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
# and the biomass from each tow to come up with an overall bank average condition factor.
# This is weight in this year, which becomes t-1 
waa.tm1 <- mod.dat.seed$CF*(mod.dat.seed$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat.seed$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat.seed$CF[-1],mod.dat.seed$CF[nrow(mod.dat.seed)])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
waa.t2 <- mod.dat.seed$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat.seed$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat.seed$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat.seed$CF*(mod.dat.seed$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat.seed$l.k
waa.t <- c(mod.dat.seed$CF[-1],mod.dat.seed$CF[nrow(mod.dat.seed)])*(laa.t/100)^3
waa.t2 <- mod.dat.seed$CF*(laa.t/100)^3
mod.dat.seed$gR <- waa.t/waa.tm1
mod.dat.seed$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
mod.dat.seed$U.cv[is.nan(mod.dat.seed$U.cv)] <- median(mod.dat.seed$U.cv,na.rm=T)
mod.dat.seed$cpue[mod.dat.seed$cpue == 0] <- median(mod.dat.seed$cpue,na.rm=T)

# DK Note, more refining, blow up the survey variance in years we don't have sufficient tows...
#mod.dat.seed$I.cv[mod.dat.seed$n.x < 5] <- 1
#mod.dat.seed$IR.cv[mod.dat.seed$n.x < 5] <- 1
#mod.dat.seed$U.cv[mod.dat.seed$n.x >= 5] <- 1 # Downweight the CPUE data when we have good survey data...
# Now for the model...

# Set the working directory for figures and tables to be output
plotsGo <- paste(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/seed/",sep="")
#Read2 Get the managment data, note this file needs updated annually! 
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Grab the data, start model at 1986
DD.dat <- subset(mod.dat.seed,year %in% 1986:2017)
names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                    "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS","RS","C","E","n.trips","U",
                    "U.var","U.se","LCI","UCI","U.cv", "g","g2","gR","gR2") 
# Organize the data and set up the model priors/initialization data, then run the model.
yrs<-min(DD.dat$year):max(DD.dat$year)
NY<- length(yrs)
DD.lst<-as.list(subset(DD.dat,year %in% yrs,c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers",
                                              "clappersR","g2","gR2")))
# DK NOTE: Downweight the CV for the CPUE data. This is done to be consistent with CV used
# Previously in the model assessments. This has been flagged as an action item to investigate 
# and resolve in the next framework.
ifelse(names(DD.lst)[9] == "U.se", names(DD.lst)[9] <- "U.cv", DD.lst$U.cv <- DD.lst$U.cv*50)
# Also, if doing this we need to change the original data to represent what the model is seeing..
# So if we used the SE let's replace the U.cv data with the U.se data, if we are doing the
# 50x to match what we've done before than we need to change those data as well.
#ifelse(names(DD.lst)[9] == "U.se", DD.dat$U.cv <- DD.dat$U.se, DD.dat$U.cv <- DD.dat$U.cv*50)

# Add a couple items to the DD.lst list...
DD.lst$NY<- length(DD.lst$C)
DD.lst$year<-min(DD.dat$year):max(DD.dat$year)
# Set up Priors.  This first bit is getting our variance correct for the CV's for Biomass, Recruit biomass, and catch rates.
# This is then added to our list of priors to get them correct.
# Biomass CV
uI=log(DD.lst$I.cv^2+1) # See Smith and Hubley 2014 for details, this is variance of the log of a CV
# DK Note:  Smith/Hubley suggest this should be 3, so why we setting it to 2???
Ip.a=2+(uI/uI)^2 # This is the alpha prior term for the prior (an inverse-gamma, i.e. gamma using 1/var); a rather funky way of setting alpha =2
Ip.b=1/(uI*((uI/uI)^2+1)) # This is the beta term for the prior, again a strangly complex way of saying 1/(2*(uI))
# Recruit biomass CV, see above comments for details.
uIR=log(DD.lst$IR.cv^2+1)
IRp.a=2+(uIR/uIR)^2
IRp.b=1/(uIR*((uIR/uIR)^2+1))
# Catch Rate CV, see above comments for details.
uU=log(DD.lst$U.cv^2+1)
Up.a=2+(uU/uU)^2
Up.b=1/(uU*((uU/uU)^2+1))

DDpriors=list(
  logK=			    list(a=7,		  b=7,		d="dnorm",	l=1		),		# scaler to total biomass, a= mean  b = sd, this gives a huge range of starting values
  r=				    list(a=0, 		b=1,		d="dlnorm",	l=NY	),		# scaled recruit biomass, a= meanlog  b = sdlog
  m=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality fully recruited a= meanlog  b = sdlog
  mR=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality  recruits a= meanlog  b = sdlog
  S=				    list(a=8, 		b=11,		d="dbeta",  l=1		),		# clapper dissolution rate a= shape1, b=shape2, 8 & 11 gives ~ normal mean of .45ish
  q=				    list(a=20, 		b=40,		d="dbeta",	l=1		),		# survey catchability fully recruited a= shape1, b=shape2
  qU=				    list(a=0,		  b=1,	  d="dunif",	l=1		),		# fishery catchability CPUE a= min, b = max
  sigma=			  list(a=0, 		b=5,		d="dunif",	l=1		),		# process error (SD) a = min, b = max
  ikappa.tau2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error FR clappers  a = shape, b = scale (1/rate)
  ikappa.rho2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error recruit clappers a = shape, b = scale (1/rate)
  I.precision=	list(a=Ip.a,	b=Ip.b,	d="dgamma",	l=NY	),		# measurement error variance survey FR a = shape, b = scale (1/rate)
  IR.precision=	list(a=IRp.a,	b=IRp.b,d="dgamma",	l=NY	),		# measurement error variance survey recruits a = shape, b = scale (1/rate)
  U.precision=	list(a=Up.a,	b=Up.b,	d="dgamma",	l=NY	)		  # measurement error variance CPUE  a = shape, b = scale
)

#Prepare priors for JAGS
for(h in 1:length(DDpriors))
{
  # Get the variances for log-normal and normal converted to precisions, note that in BUGS language the precision is
  # the inverse of the squared standard deviation (which is what you specify in R).  The standard deviation is what
  # was specified in the Prior list (as it is more intuitive)
  if(DDpriors[[h]]$d%in%c("dlnorm","dnorm")) DDpriors[[h]]$b <- 1/DDpriors[[h]]$b^2
  # For a Gamma to convert to precision the precision term is  the inverse of the 'Scale" term in a typical 
  # gamma distribution parameterization, aka this is now knonwn as the rate.
  # Happily this is the same as the parameterization in R dgamma(x,shape,rate) so our b parameter is correct for posterior plots.
  if(DDpriors[[h]]$d=="dgamma")DDpriors[[h]]$b<-1/DDpriors[[h]]$b
} # end for(h in 1:length(DDpriors))
# Made a data.frame of the priors, unwrap the list and combine by row.
prior.dat<- data.frame(par=names(DDpriors),do.call("rbind",lapply(DDpriors,rbind)))
prior.lst<-list()
# Now turn this into a list
for(k in seq(1,nrow(prior.dat)*2,2))
{
  prior.lst[[k]]<-prior.dat$a[[ceiling(k/2)]]
  prior.lst[[k+1]]<-prior.dat$b[[ceiling(k/2)]]
} # end for(k in seq(1,nrow(prior.dat)*2,2))
# And give the list names
names(prior.lst)<-paste(rep(prior.dat$par,2)[order(rep(1:nrow(prior.dat),2))],rep(c('a','b'),nrow(prior.dat)),sep='.')

# Now if they haven't already been selected grab the parameters you want for the model.
parameters <- c(names(DDpriors),'K','P','B','R','mu','Imed','Ipred','Irep', 'IRmed','IRpred','IRrep',
                "Cmed","Crep","CRmed","CRrep",'sIresid','sIRresid','sPresid','Iresid',
                'IRresid','Presid',"Cresid","CRresid","sCresid","sCRresid")
# Run the model and see how long it takes.
# n = 400,000 and burn = 100,000, thin = 20 with 2 chains do not decrease these as retaining this much
# data is needed to stabilize the projections, it does lengthen the run time to 10-20 minutes in serial
# Running in parallel stick with that burn in but we can get away with n=200,000, burn = 100,000, thin = 20, and 6 chains
# they are longer chains than really are needed for the model to converge, but this is really being done just for the projections.
# Run the model now.
model.jags = "Assessment_fns/Model/DDwSE3_jags.bug"

start<-Sys.time()
out <- jags.parallel(data =  c(prior.lst,DD.lst), inits = NULL,parameters.to.save = parameters,  
                     model.file = paste(direct,model.jags,sep=""),n.chains = 10, n.iter = 200000, n.burnin = 125000, 
                     n.thin = 10)
print(Sys.time()-start)

# Rename the output so I retain the results 
DD.out <- list(data=c(prior.lst,DD.lst,yrs), sims.list=out$BUGSoutput$sims.list,median=out$BUGSoutput$median,
               mean=out$BUGSoutput$mean,summary=out$BUGSoutput$summary,priors = prior.lst,parameters=parameters)

# I will also retain the MCMC object produced in case I want it for something.
mod.out <- out

#source("fn/projections.r")
# The catch since the survey for the most recent year is
#proj.catch <- proj.dat$catch[proj.dat$year == max(DD.dat$year)] 
proj.catch <- 0 # just set it to 0 for the moment...
# Get the low and upper boundaries for the decision table (this might be a silly way to do this...)
D_low   <- 0
D_high <-  250
# The increment size for the decision table.  500 for GBa and 50 for GBa
step <- 25
# Get the projection scenarios of interest
proj <- seq(D_low,D_high,step) + proj.catch
# The interim TAC is
TACi <- 0

# Note that these plots don't account for growth next year, so are somewhat misleading compared to the decision table
# which does account for growth!!  This should be not too painful to fix I think?

# Now do the projections
DD.out<- projections(DD.out,C.p=proj) # C.p = potential catches in decision table

### Generate Decision Table ###
### Note that from the 2015 SSR we have these definitely set at...
#Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
#The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
D.tab<-decision(DD.out,"GBa", mu=0.15,post.survey.C=proj.catch)
write.csv(D.tab,paste0(plotsGo,"Decision.csv",sep=""),row.names=F) #Write2



## Some summary stats...
# Some model outputs needed for the Update.  First the mortality
mort <- 1- exp(-DD.out$mean$m[length(DD.out$mean$m)])
# This lines up the column headers with the projected catch...
TACI<- which(DD.out$data$C.p==(TACi+proj.catch))
# This get us the predicted biomass for next year based on the projected catch
BM.proj.1yr <- DD.out$median$B.p[TACI]

# Here we can grab the Fully recruited and recruit biomass for the last 2 years and the median of the time series.
FR.bm <- DD.out$median$B[(length(DD.out$mean$B)-1):length(DD.out$median$B)]
# We exclude the current year from the median estimate
FR.ltm <- median(DD.out$median$B[-length(DD.out$median$B)])
# Recruit biomass
rec.bm <- DD.out$median$R[(length(DD.out$median$R)-1):length(DD.out$median$R)]
# We exclude the current year from the median estimate
rec.ltm <- median(DD.out$median$R[-length(DD.out$median$R)])

# Get the percent biomass change from the projection. 0 means unchanged, + means % increase, - means % decline
percent.B.change <- (BM.proj.1yr / DD.out$median$B[length(DD.out$median$B)]) -1



####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS 
##### Now we can run some model diagnostics.
# Some quick diagnoistics, the maximum should be < 1.05
rhat <- summary(DD.out$summary[,8])

# Effective number of observations.  
#Not sure what our minimum should be here, but using the Rhat + looking at the chains should indicate where there are problems...
neff <- range(DD.out$summary[,9])


## Make the figures
# posterior densities for model parameters
post.plt(DD.out,DDpriors,years=yrs, graphic="pdf",multi=T,path=plotsGo)
#dev.off()
##exploitaiton time series
exploit.plt(DD.out, years=yrs, plt=c('f','m','mR'),graphic="pdf",path=plotsGo)
#dev.off()
# model biomass fit to survey
fit.plt(DD.out, years = yrs, CI=T,graphic="pdf",path=plotsGo,CV=T)
# diagnostic plot
diag.plt(DD.out, years = yrs,graphic="pdf",path=plotsGo)

# and our big old biomass plot
biomass.plt(DD.out,years=yrs, graphic="pdf",TAC=TACi+proj.catch,path=plotsGo,refs=NULL,pred=1,avg.line=median,Bymax=5000)

# Some plots....
plot(DD.out$mean$B~DD.out$mean$m,type="n")
text(DD.out$mean$B~DD.out$mean$m,labels=1995:2016,cex=0.65)

# Do a quick CCF sniff to see if anything interesting, nothing really, time series likely too short to be honest.
ccf(DD.out$mean$B,DD.out$mean$m)
ccf(DD.out$mean$R,DD.out$mean$m)
ccf(DD.out$mean$B,DD.out$mean$R) 
ccf(mod.dat.seed$I,mod.dat.seed$IR) 
plot(DD.out$mean$B[1:22]~DD.out$mean$m[2:23])
plot(DD.out$mean$B[1:22]~DD.out$mean$mR[2:23])
plot(DD.dat$NPR[1:22] ~DD.out$mean$mR[2:23])
plot(DD.dat$NPR[1:22] ~DD.out$mean$m[2:23])
plot(DD.dat$NPR ~DD.out$mean$m)
plot(DD.out$mean$B~DD.out$mean$mR)
# There isn't any relationship between B and n thankfully.
plot(DD.out$mean$B ~ DD.dat$n)
plot(DD.dat$I ~ DD.dat$n)
# Nothing interesting with exploitation rate and natural mortality.
plot(DD.out$mean$mu~DD.out$mean$m)

save.image(paste(direct,"Data/Framework/2018/GBa_box_model/seed_testing.RData",sep=""))
#load(paste(direct,"Data/Framework/2018/GBa_box_model/seed_testing.RData",sep=""))

####################### End the seed Model####################### End the seed Model####################### End the seed Model####################### End the seed Model
####################### End the seed Model####################### End the seed Model####################### End the seed Model####################### End the seed Model


##########################  THE peanut MODEL #######################################  THE peanut MODEL #######################################  THE peanut MODEL #############
##########################  THE peanut MODEL #######################################  THE peanut MODEL #######################################  THE peanut MODEL #############
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
# 
# Run this for one or both banks
# Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
years <- min(peanut.obj[[1]]$year):max(peanut.obj[[1]]$year)
# Bring in the vonB parameters..
vonB.par <-vonB[vonB$Bank =="GBa",]
# Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
cpue.dat <- fishery.dat(peanut.fishery,bk="GBa",yr=c(1984:2017),method='jackknife',surv='August',
                        direct=direct,period = "survyr") 	

if(any(is.na(cpue.dat))) cpue.dat[is.na(cpue.dat)] <- 0 # Set any NA's to 0. 
#cpue.dat <- rbind(cpue.dat,c(2016,rep(0,8)))

# Combine the survey and Fishery data here.
mod.dat.peanut <- merge(peanut.obj[[1]],cpue.dat,by ="year")
# DK Note For years in which we have essentially no fishing effort on the bank we need to blow up the variance of those estimates.
# If < 20 tonnes assume low effort and blow up the CV to 1.
mod.dat.peanut$cpue.se[mod.dat.peanut$catch < 20] <- mod.dat.peanut$cpue[mod.dat.peanut$catch < 20]
# Get the CV for the CPUE...
mod.dat.peanut$U.cv <- mod.dat.peanut$cpue.se/mod.dat.peanut$cpue
mod.dat.peanut$U.cv[is.nan(mod.dat.peanut$U.cv)] <- 1 # Doesn't really matter since CPUE =0

# now get the catch data from end of survey until end of the year for the projection
proj.sub <- subset(peanut.fishery,year %in% years & months(as.Date(peanut.fishery$date)) %in% c("September","October","November","December"))
# Again on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
# for all other years we need to do this for Browns Bank North
# Note that June-August seems to be a pretty minimal fishery on GBa
# Now calculate the fishery statistics for the projection period
proj.dat <- fishery.dat(proj.sub,bk="GBa",yr=(min(years)-1):max(years),method='jackknife',
                        direct=direct,period = "calyr") 	
# So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
# and the biomass from each tow to come up with an overall bank average condition factor.
# This is weight in this year, which becomes t-1 
waa.tm1 <- mod.dat.peanut$CF*(mod.dat.peanut$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat.peanut$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat.peanut$CF[-1],mod.dat.peanut$CF[nrow(mod.dat.peanut)])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
waa.t2 <- mod.dat.peanut$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat.peanut$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat.peanut$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat.peanut$CF*(mod.dat.peanut$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat.peanut$l.k
waa.t <- c(mod.dat.peanut$CF[-1],mod.dat.peanut$CF[nrow(mod.dat.peanut)])*(laa.t/100)^3
waa.t2 <- mod.dat.peanut$CF*(laa.t/100)^3
mod.dat.peanut$gR <- waa.t/waa.tm1
mod.dat.peanut$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
mod.dat.peanut$U.cv[is.nan(mod.dat.peanut$U.cv)] <- median(mod.dat.peanut$U.cv,na.rm=T)
mod.dat.peanut$cpue[mod.dat.peanut$cpue == 0] <- median(mod.dat.peanut$cpue,na.rm=T)

# DK Note, more refining, blow up the survey variance in years we don't have sufficient tows...
#mod.dat.peanut$I.cv[mod.dat.peanut$n.x < 5] <- 1
##mod.dat.peanut$IR.cv[mod.dat.peanut$n.x < 5] <- 1
#mod.dat.peanut$U.cv[mod.dat.peanut$n.x >= 5] <- 1 # Downweight the CPUE data when we have good survey data...
# Now for the model...

# Set the working directory for figures and tables to be output
plotsGo <- paste(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/peanut/",sep="")
#Read2 Get the managment data, note this file needs updated annually! 
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Grab the data, start model at either 1986 (note that GBa data starts in 1991 so anything earlier will default to 1991)
DD.dat <- subset(mod.dat.peanut,year %in% 1986:2017)
names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                    "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS",  "RS","C","E","n.trips","U",
                    "U.var","U.se","LCI","UCI","U.cv", "g","g2","gR","gR2") 
# Organize the data and set up the model priors/initialization data, then run the model.
yrs<-min(DD.dat$year):max(DD.dat$year)
NY<- length(yrs)
DD.lst<-as.list(subset(DD.dat,year %in% yrs,c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers",
                                              "clappersR")))
# DK NOTE: Downweight the CV for the CPUE data. This is done to be consistent with CV used
# Previously in the model assessments. This has been flagged as an action item to investigate 
# and resolve in the next framework.
cat(paste("*NOTE # 2* See code for message about the CPUE CV being downweighted artificially, this needs revised in next Framework as
                it ain't legit.\n",sep = " "))
ifelse(names(DD.lst)[9] == "U.se", names(DD.lst)[9] <- "U.cv", DD.lst$U.cv <- DD.lst$U.cv*50)
# Also, if doing this we need to change the original data to represent what the model is seeing..
# So if we used the SE let's replace the U.cv data with the U.se data, if we are doing the
# 50x to match what we've done before than we need to change those data as well.
#ifelse(names(DD.lst)[9] == "U.se", DD.dat$U.cv <- DD.dat$U.se, DD.dat$U.cv <- DD.dat$U.cv*50)

# Add a couple items to the DD.lst list...
DD.lst$NY<- length(DD.lst$C)
DD.lst$year<-min(DD.dat$year):max(DD.dat$year)
# Set up Priors.  This first bit is getting our variance correct for the CV's for Biomass, Recruit biomass, and catch rates.
# This is then added to our list of priors to get them correct.
# Biomass CV
uI=log(DD.lst$I.cv^2+1) # See Smith and Hubley 2014 for details, this is variance of the log of a CV
# DK Note:  Smith/Hubley suggest this should be 3, so why we setting it to 2???
Ip.a=2+(uI/uI)^2 # This is the alpha prior term for the prior (an inverse-gamma, i.e. gamma using 1/var); a rather funky way of setting alpha =2
Ip.b=1/(uI*((uI/uI)^2+1)) # This is the beta term for the prior, again a strangly complex way of saying 1/(2*(uI))
# Recruit biomass CV, see above comments for details.
uIR=log(DD.lst$IR.cv^2+1)
IRp.a=2+(uIR/uIR)^2
IRp.b=1/(uIR*((uIR/uIR)^2+1))
# Catch Rate CV, see above comments for details.
uU=log(DD.lst$U.cv^2+1)
Up.a=2+(uU/uU)^2
Up.b=1/(uU*((uU/uU)^2+1))

DDpriors=list(
  logK=			    list(a=7,		  b=7,		d="dnorm",	l=1		),		# scaler to total biomass, a= mean  b = sd, this gives a huge range of starting values
  r=				    list(a=0, 		b=1,		d="dlnorm",	l=NY	),		# scaled recruit biomass, a= meanlog  b = sdlog
  m=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality fully recruited a= meanlog  b = sdlog
  mR=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality  recruits a= meanlog  b = sdlog
  S=				    list(a=8, 		b=11,		d="dbeta",  l=1		),		# clapper dissolution rate a= shape1, b=shape2, 8 & 11 gives ~ normal mean of .45ish
  q=				    list(a=20, 		b=40,		d="dbeta",	l=1		),		# survey catchability fully recruited a= shape1, b=shape2
  qU=				    list(a=0,		  b=1,	  d="dunif",	l=1		),		# fishery catchability CPUE a= min, b = max
  sigma=			  list(a=0, 		b=5,		d="dunif",	l=1		),		# process error (SD) a = min, b = max
  ikappa.tau2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error FR clappers  a = shape, b = scale (1/rate)
  ikappa.rho2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error recruit clappers a = shape, b = scale (1/rate)
  I.precision=	list(a=Ip.a,	b=Ip.b,	d="dgamma",	l=NY	),		# measurement error variance survey FR a = shape, b = scale (1/rate)
  IR.precision=	list(a=IRp.a,	b=IRp.b,d="dgamma",	l=NY	),		# measurement error variance survey recruits a = shape, b = scale (1/rate)
  U.precision=	list(a=Up.a,	b=Up.b,	d="dgamma",	l=NY	)		  # measurement error variance CPUE  a = shape, b = scale
)

#Prepare priors for JAGS
for(h in 1:length(DDpriors))
{
  # Get the variances for log-normal and normal converted to precisions, note that in BUGS language the precision is
  # the inverse of the squared standard deviation (which is what you specify in R).  The standard deviation is what
  # was specified in the Prior list (as it is more intuitive)
  if(DDpriors[[h]]$d%in%c("dlnorm","dnorm")) DDpriors[[h]]$b <- 1/DDpriors[[h]]$b^2
  # For a Gamma to convert to precision the precision term is  the inverse of the 'Scale" term in a typical 
  # gamma distribution parameterization, aka this is now knonwn as the rate.
  # Happily this is the same as the parameterization in R dgamma(x,shape,rate) so our b parameter is correct for posterior plots.
  if(DDpriors[[h]]$d=="dgamma")DDpriors[[h]]$b<-1/DDpriors[[h]]$b
} # end for(h in 1:length(DDpriors))
# Made a data.frame of the priors, unwrap the list and combine by row.
prior.dat<- data.frame(par=names(DDpriors),do.call("rbind",lapply(DDpriors,rbind)))
prior.lst<-list()
# Now turn this into a list
for(k in seq(1,nrow(prior.dat)*2,2))
{
  prior.lst[[k]]<-prior.dat$a[[ceiling(k/2)]]
  prior.lst[[k+1]]<-prior.dat$b[[ceiling(k/2)]]
} # end for(k in seq(1,nrow(prior.dat)*2,2))
# And give the list names
names(prior.lst)<-paste(rep(prior.dat$par,2)[order(rep(1:nrow(prior.dat),2))],rep(c('a','b'),nrow(prior.dat)),sep='.')

# Now if they haven't already been selected grab the parameters you want for the model.
parameters <- c(names(DDpriors),'K','P','B','R','mu','Imed','Ipred','Irep', 'IRmed','IRpred','IRrep',
                "Cmed","Crep","CRmed","CRrep",'sIresid','sIRresid','sPresid','Iresid',
                'IRresid','Presid',"Cresid","CRresid","sCresid","sCRresid")
# Run the model and see how long it takes.
# n = 400,000 and burn = 100,000, thin = 20 with 2 chains do not decrease these as retaining this much
# data is needed to stabilize the projections, it does lengthen the run time to 10-20 minutes in serial
# Running in parallel stick with that burn in but we can get away with n=200,000, burn = 100,000, thin = 20, and 6 chains
# they are longer chains than really are needed for the model to converge, but this is really being done just for the projections.
# Run the model now.
model.jags = "Assessment_fns/Model/DDwSE3_jags.bug"


start<-Sys.time()
out <- jags.parallel(data =  c(prior.lst,DD.lst), inits = NULL,parameters.to.save = parameters,  
                     model.file = paste(direct,model.jags,sep=""),n.chains = 10, n.iter = 200000, n.burnin = 125000, 
                     n.thin = 10)
print(Sys.time()-start)


# Rename the output so I retain the results 
DD.out <- list(data=c(prior.lst,DD.lst,yrs), sims.list=out$BUGSoutput$sims.list,median=out$BUGSoutput$median,
               mean=out$BUGSoutput$mean,summary=out$BUGSoutput$summary,priors = prior.lst,parameters=parameters)

# I will also retain the MCMC object produced in case I want it for something.
mod.out <- out

#source("fn/projections.r")
# The catch since the survey for the most recent year is
#proj.catch <- proj.dat$catch[proj.dat$year == max(DD.dat$year)] 
proj.catch <- 0 # just set it to 0 for the moment...
# Get the low and upper boundaries for the decision table (this might be a silly way to do this...)
D_low   <- 0
D_high <-  500
# The increment size for the decision table.  500 for GBa and 50 for GBa
step <- 50
# Get the projection scenarios of interest
proj <- seq(D_low,D_high,step) + proj.catch
# The interim TAC is
TACi <- 0

# Note that these plots don't account for growth next year, so are somewhat misleading compared to the decision table
# which does account for growth!!  This should be not too painful to fix I think?

# Now do the projections
DD.out<- projections(DD.out,C.p=proj) # C.p = potential catches in decision table

### Generate Decision Table ###
### Note that from the 2015 SSR we have these definitely set at...
#Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
#The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
D.tab<-decision(DD.out,"GBa", mu=0.15,post.survey.C=proj.catch)
write.csv(D.tab,paste0(plotsGo,"Decision.csv",sep=""),row.names=F) #Write2



## Some summary stats...
# Some model outputs needed for the Update.  First the mortality
mort <- 1- exp(-DD.out$mean$m[length(DD.out$mean$m)])
# This lines up the column headers with the projected catch...
TACI<- which(DD.out$data$C.p==(TACi+proj.catch))
# This get us the predicted biomass for next year based on the projected catch
BM.proj.1yr <- DD.out$median$B.p[TACI]

# Here we can grab the Fully recruited and recruit biomass for the last 2 years and the median of the time series.
FR.bm <- DD.out$median$B[(length(DD.out$mean$B)-1):length(DD.out$median$B)]
# We exclude the current year from the median estimate
FR.ltm <- median(DD.out$median$B[-length(DD.out$median$B)])
# Recruit biomass
rec.bm <- DD.out$median$R[(length(DD.out$median$R)-1):length(DD.out$median$R)]
# We exclude the current year from the median estimate
rec.ltm <- median(DD.out$median$R[-length(DD.out$median$R)])

# Get the percent biomass change from the projection. 0 means unchanged, + means % increase, - means % decline
percent.B.change <- (BM.proj.1yr / DD.out$median$B[length(DD.out$median$B)]) -1



####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS 
##### Now we can run some model diagnostics.
# Some quick diagnoistics, the maximum should be < 1.05
rhat <- summary(DD.out$summary[,8])

# Effective number of observations.  
#Not sure what our minimum should be here, but using the Rhat + looking at the chains should indicate where there are problems...
neff <- range(DD.out$summary[,9])


## Make the figures
# posterior densities for model parameters
post.plt(DD.out,DDpriors,years=yrs, graphic="pdf",multi=T,path=plotsGo)
#dev.off()
##exploitaiton time series
exploit.plt(DD.out, years=yrs, plt=c('f','m','mR'),graphic="pdf",path=plotsGo)
#dev.off()
# model biomass fit to survey
fit.plt(DD.out, years = yrs, CI=T,graphic="pdf",path=plotsGo,CV=T)
# diagnostic plot
diag.plt(DD.out, years = yrs,graphic="pdf",path=plotsGo)

# and our big old biomass plot
biomass.plt(DD.out,years=yrs, graphic="pdf",TAC=TACi+proj.catch,path=plotsGo,refs=NULL,pred=1,avg.line=NULL,Bymax=5000)

# Hmm, actually there is something here, whenever B was above 1500 m was above 0.22.  Recruit mortality
# also happened to be high in these same periods.
plot(DD.out$mean$B~DD.out$mean$m,type="n")
text(DD.out$mean$B~DD.out$mean$m,labels=1995:2016,cex=0.65)

# Do a quick CCF sniff to see if anything interesting, nothing really, time series likely too short to be honest.
ccf(DD.out$mean$B,DD.out$mean$m)
ccf(DD.out$mean$R,DD.out$mean$m)
ccf(DD.out$mean$B,DD.out$mean$R) # Can see a two year lag b/t recruits and Biomass peaking here which is interesting...
ccf(mod.dat.peanut$I,mod.dat.peanut$IR) # Using just the survey data the 1 year lag is clearer and both are fairly significant.  Interesting the model weakens this...

plot(DD.out$mean$B[2:23]~DD.out$mean$R[1:22]) # 1 year lag, Some weirdness in there for sure, problem is high biomasses with low recruits which is likely
# years in which the recruitment 2 years previous was very strong, need something more co-horty to get at this I think.
plot(DD.out$mean$B[3:23]~DD.out$mean$R[1:21]) # 2 year lag, can see this is really driven by 2 points, odd this isn't more obvious
plot(mod.dat.peanut$I[2:23]~mod.dat.peanut$IR[1:22]) # That 1 year lag is also driven by 1 point so less than stellar.
plot(mod.dat.peanut$I[3:23]~mod.dat.peanut$IR[1:21]) # That 2 year lag is also driven by 1 point so less than stellar.
plot(DD.out$mean$B[1:22]~DD.out$mean$mR[2:23])
# No evidence of a relationship between our mortalities and our pre-recruit abundances.
plot(DD.dat$NPR[1:22] ~DD.out$mean$mR[2:23])
plot(DD.dat$NPR[1:22] ~DD.out$mean$m[2:23])
plot(DD.dat$NPR ~DD.out$mean$m)
plot(DD.out$mean$B~DD.out$mean$mR)
# There isn't any relationship between B and n thankfully.
plot(DD.out$mean$B ~ DD.dat$n)
plot(DD.dat$I ~ DD.dat$n)
# Nothing interesting with exploitation rate and natural mortality.
plot(DD.out$mean$mu~DD.out$mean$m)


save.image(paste(direct,"Data/Framework/2018/GBa_box_model/peanut_testing.RData",sep=""))
#load(paste(direct,"Data/Framework/2018/GBa_box_model/peanut_testing.RData",sep=""))

####################### End the peanut Model####################### End the peanut Model####################### End the peanut Model####################### End the peanut Model
####################### End the peanut Model####################### End the peanut Model####################### End the peanut Model####################### End the peanut Model


##########################  THE C4 MODEL #######################################  THE C4 MODEL #######################################  THE C4 MODEL #############
##########################  THE C4 MODEL #######################################  THE C4 MODEL #######################################  THE C4 MODEL #############
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
# 
# Run this for one or both banks
# Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
C4.obj[[1]]$year <- as.numeric(levels(C4.obj[[1]]$year))
years <- min(C4.obj[[1]]$year):max(C4.obj[[1]]$year)
# Bring in the vonB parameters..
vonB.par <-vonB[vonB$Bank =="GBa",]
# Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
cpue.dat <- fishery.dat(C4.fishery,bk="GBa",yr=c(1984:2017),method='jackknife',surv='August',
                        direct=direct,period = "survyr") 	
with(subset(C4.fishery,bank %in% "GBa" & year %in% 1985),tapply(pro.repwt,year,sum,na.rm=T))/1000
# Now on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
#for all other years we need to do this for Browns Bank North
# It really makes very little difference which way this is done as the catch in June-August
# has averaged around just 40 tonnes since about 1996.
if(any(is.na(cpue.dat))) cpue.dat[is.na(cpue.dat)] <- 0 # Set any NA's to 0. 
# Combine the survey and Fishery data here.
mod.dat.C4 <- merge(C4.obj[[1]],cpue.dat,by ="year")
# DK Note For years in which we have essentially no fishing effort on the bank we need to blow up the variance of those estimates.
# If < 20 tonnes assume low effort and blow up the CV to 1.
#mod.dat.C4$cpue.se[mod.dat.C4$catch < 20] <- mod.dat.C4$cpue[mod.dat.C4$catch < 20]
# Get the CV for the CPUE...
mod.dat.C4$U.cv <- mod.dat.C4$cpue.se/mod.dat.C4$cpue
mod.dat.C4$U.cv[is.nan(mod.dat.C4$U.cv)] <- 1 # Doesn't really matter since CPUE =0

# now get the catch data from end of survey until end of the year for the projection
proj.sub <- subset(C4.fishery,year %in% years & months(as.Date(C4.fishery$date)) %in% c("September","October","November","December"))
# Again on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
# for all other years we need to do this for Browns Bank North
# Note that June-August seems to be a pretty minimal fishery on GBa
# Now calculate the fishery statistics for the projection period
proj.dat <- fishery.dat(proj.sub,bk="GBa",yr=(min(years)-1):max(years),method='jackknife',
                        direct=direct,period = "calyr") 	
# So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
# and the biomass from each tow to come up with an overall bank average condition factor.
# This is weight in this year, which becomes t-1 
waa.tm1 <- mod.dat.C4$CF*(mod.dat.C4$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat.C4$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat.C4$CF[-1],mod.dat.C4$CF[nrow(mod.dat.C4)])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
waa.t2 <- mod.dat.C4$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat.C4$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat.C4$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat.C4$CF*(mod.dat.C4$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat.C4$l.k
waa.t <- c(mod.dat.C4$CF[-1],mod.dat.C4$CF[nrow(mod.dat.C4)])*(laa.t/100)^3
waa.t2 <- mod.dat.C4$CF*(laa.t/100)^3
mod.dat.C4$gR <- waa.t/waa.tm1
mod.dat.C4$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
mod.dat.C4$U.cv[is.nan(mod.dat.C4$U.cv)] <- median(mod.dat.C4$U.cv,na.rm=T)
mod.dat.C4$U.cv[mod.dat.C4$U.cv==0] <- median(mod.dat.C4$U.cv,na.rm=T)
mod.dat.C4$cpue[mod.dat.C4$cpue == 0] <- median(mod.dat.C4$cpue,na.rm=T)

# DK Note, more refining, blow up the survey variance in years we don't have sufficient tows...
#mod.dat.C4$I.cv[mod.dat.C4$n.x < 5] <- 1
#mod.dat.C4$IR.cv[mod.dat.C4$n.x < 5] <- 1
#mod.dat.C4$U.cv[mod.dat.C4$n.x >= 5] <- 1 # Downweight the CPUE data when we have good survey data...
# Now for the model...

# Set the working directory for figures and tables to be output
plotsGo <- paste(direct,"2018/Framework/GBa_box_model/Figures_and_Tables/C4/",sep="")
#Read2 Get the managment data, note this file needs updated annually! 
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Lets just run from 1998 to present as we are missing a survey in 1997
DD.dat <- subset(mod.dat.C4,year %in% 1998:2017)
names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                    "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS","RS","C","E","n.trips","U",
                    "U.var","U.se","LCI","UCI","U.cv", "g","g2","gR","gR2") 
# Organize the data and set up the model priors/initialization data, then run the model.
yrs<-min(DD.dat$year):max(DD.dat$year)
NY<- length(yrs)
DD.lst<-as.list(subset(DD.dat,year %in% yrs,c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers",
                                              "clappersR","g2","gR2")))
# DK NOTE: Downweight the CV for the CPUE data. This is done to be consistent with CV used
# Previously in the model assessments. This has been flagged as an action item to investigate 
# and resolve in the next framework.
ifelse(names(DD.lst)[9] == "U.se", names(DD.lst)[9] <- "U.cv", DD.lst$U.cv <- DD.lst$U.cv*50)
# Also, if doing this we need to change the original data to represent what the model is seeing..
# So if we used the SE let's replace the U.cv data with the U.se data, if we are doing the
# 50x to match what we've done before than we need to change those data as well.
#ifelse(names(DD.lst)[9] == "U.se", DD.dat$U.cv <- DD.dat$U.se, DD.dat$U.cv <- DD.dat$U.cv*50)

# Add a couple items to the DD.lst list...
DD.lst$NY<- length(DD.lst$C)
DD.lst$year<-min(DD.dat$year):max(DD.dat$year)
# Set up Priors.  This first bit is getting our variance correct for the CV's for Biomass, Recruit biomass, and catch rates.
# This is then added to our list of priors to get them correct.
# Biomass CV
uI=log(DD.lst$I.cv^2+1) # See Smith and Hubley 2014 for details, this is variance of the log of a CV
# DK Note:  Smith/Hubley suggest this should be 3, so why we setting it to 2???
Ip.a=2+(uI/uI)^2 # This is the alpha prior term for the prior (an inverse-gamma, i.e. gamma using 1/var); a rather funky way of setting alpha =2
Ip.b=1/(uI*((uI/uI)^2+1)) # This is the beta term for the prior, again a strangly complex way of saying 1/(2*(uI))
# Recruit biomass CV, see above comments for details.
uIR=log(DD.lst$IR.cv^2+1)
IRp.a=2+(uIR/uIR)^2
IRp.b=1/(uIR*((uIR/uIR)^2+1))
# Catch Rate CV, see above comments for details.
uU=log(DD.lst$U.cv^2+1)
Up.a=2+(uU/uU)^2
Up.b=1/(uU*((uU/uU)^2+1))

DDpriors=list(
  logK=			    list(a=7,		  b=7,		d="dnorm",	l=1		),		# scaler to total biomass, a= mean  b = sd, this gives a huge range of starting values
  r=				    list(a=0, 		b=1,		d="dlnorm",	l=NY	),		# scaled recruit biomass, a= meanlog  b = sdlog
  m=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality fully recruited a= meanlog  b = sdlog
  mR=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality  recruits a= meanlog  b = sdlog
  S=				    list(a=8, 		b=11,		d="dbeta",  l=1		),		# clapper dissolution rate a= shape1, b=shape2, 8 & 11 gives ~ normal mean of .45ish
  q=				    list(a=20, 		b=40,		d="dbeta",	l=1		),		# survey catchability fully recruited a= shape1, b=shape2
  qU=				    list(a=0,		  b=1,	  d="dunif",	l=1		),		# fishery catchability CPUE a= min, b = max
  sigma=			  list(a=0, 		b=5,		d="dunif",	l=1		),		# process error (SD) a = min, b = max
  ikappa.tau2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error FR clappers  a = shape, b = scale (1/rate)
  ikappa.rho2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error recruit clappers a = shape, b = scale (1/rate)
  I.precision=	list(a=Ip.a,	b=Ip.b,	d="dgamma",	l=NY	),		# measurement error variance survey FR a = shape, b = scale (1/rate)
  IR.precision=	list(a=IRp.a,	b=IRp.b,d="dgamma",	l=NY	),		# measurement error variance survey recruits a = shape, b = scale (1/rate)
  U.precision=	list(a=Up.a,	b=Up.b,	d="dgamma",	l=NY	)		  # measurement error variance CPUE  a = shape, b = scale
)

#Prepare priors for JAGS
for(h in 1:length(DDpriors))
{
  # Get the variances for log-normal and normal converted to precisions, note that in BUGS language the precision is
  # the inverse of the squared standard deviation (which is what you specify in R).  The standard deviation is what
  # was specified in the Prior list (as it is more intuitive)
  if(DDpriors[[h]]$d%in%c("dlnorm","dnorm")) DDpriors[[h]]$b <- 1/DDpriors[[h]]$b^2
  # For a Gamma to convert to precision the precision term is  the inverse of the 'Scale" term in a typical 
  # gamma distribution parameterization, aka this is now knonwn as the rate.
  # Happily this is the same as the parameterization in R dgamma(x,shape,rate) so our b parameter is correct for posterior plots.
  if(DDpriors[[h]]$d=="dgamma")DDpriors[[h]]$b<-1/DDpriors[[h]]$b
} # end for(h in 1:length(DDpriors))
# Made a data.frame of the priors, unwrap the list and combine by row.
prior.dat<- data.frame(par=names(DDpriors),do.call("rbind",lapply(DDpriors,rbind)))
prior.lst<-list()
# Now turn this into a list
for(k in seq(1,nrow(prior.dat)*2,2))
{
  prior.lst[[k]]<-prior.dat$a[[ceiling(k/2)]]
  prior.lst[[k+1]]<-prior.dat$b[[ceiling(k/2)]]
} # end for(k in seq(1,nrow(prior.dat)*2,2))
# And give the list names
names(prior.lst)<-paste(rep(prior.dat$par,2)[order(rep(1:nrow(prior.dat),2))],rep(c('a','b'),nrow(prior.dat)),sep='.')

# Now if they haven't already been selected grab the parameters you want for the model.
parameters <- c(names(DDpriors),'K','P','B','R','mu','Imed','Ipred','Irep', 'IRmed','IRpred','IRrep',
                "Cmed","Crep","CRmed","CRrep",'sIresid','sIRresid','sPresid','Iresid',
                'IRresid','Presid',"Cresid","CRresid","sCresid","sCRresid")
# Run the model and see how long it takes.
# n = 400,000 and burn = 100,000, thin = 20 with 2 chains do not decrease these as retaining this much
# data is needed to stabilize the projections, it does lengthen the run time to 10-20 minutes in serial
# Running in parallel stick with that burn in but we can get away with n=200,000, burn = 100,000, thin = 20, and 6 chains
# they are longer chains than really are needed for the model to converge, but this is really being done just for the projections.
# Run the model now.
model.jags = "Assessment_fns/Model/DDwSE3_jags.bug"

start<-Sys.time()
out <- jags.parallel(data =  c(prior.lst,DD.lst), inits = NULL,parameters.to.save = parameters,  
                     model.file = paste(direct,model.jags,sep=""),n.chains = 10, n.iter = 200000, n.burnin = 125000, 
                     n.thin = 10)
print(Sys.time()-start)

# Rename the output so I retain the results 
DD.out <- list(data=c(prior.lst,DD.lst,yrs), sims.list=out$BUGSoutput$sims.list,median=out$BUGSoutput$median,
               mean=out$BUGSoutput$mean,summary=out$BUGSoutput$summary,priors = prior.lst,parameters=parameters)

# I will also retain the MCMC object produced in case I want it for something.
mod.out <- out

#source("fn/projections.r")
# The catch since the survey for the most recent year is
#proj.catch <- proj.dat$catch[proj.dat$year == max(DD.dat$year)] 
proj.catch <- 0 # just set it to 0 for the moment...
# Get the low and upper boundaries for the decision table (this might be a silly way to do this...)
D_low   <- 0
D_high <-  250
# The increment size for the decision table.  500 for GBa and 50 for GBa
step <- 25
# Get the projection scenarios of interest
proj <- seq(D_low,D_high,step) + proj.catch
# The interim TAC is
TACi <- 0

# Note that these plots don't account for growth next year, so are somewhat misleading compared to the decision table
# which does account for growth!!  This should be not too painful to fix I think?

# Now do the projections
DD.out<- projections(DD.out,C.p=proj) # C.p = potential catches in decision table

### Generate Decision Table ###
### Note that from the 2015 SSR we have these definitely set at...
#Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
#The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
D.tab<-decision(DD.out,"GBa", mu=0.15,post.survey.C=proj.catch)
write.csv(D.tab,paste0(plotsGo,"Decision.csv",sep=""),row.names=F) #Write2



## Some summary stats...
# Some model outputs needed for the Update.  First the mortality
mort <- 1- exp(-DD.out$mean$m[length(DD.out$mean$m)])
# This lines up the column headers with the projected catch...
TACI<- which(DD.out$data$C.p==(TACi+proj.catch))
# This get us the predicted biomass for next year based on the projected catch
BM.proj.1yr <- DD.out$median$B.p[TACI]

# Here we can grab the Fully recruited and recruit biomass for the last 2 years and the median of the time series.
FR.bm <- DD.out$median$B[(length(DD.out$mean$B)-1):length(DD.out$median$B)]
# We exclude the current year from the median estimate
FR.ltm <- median(DD.out$median$B[-length(DD.out$median$B)])
# Recruit biomass
rec.bm <- DD.out$median$R[(length(DD.out$median$R)-1):length(DD.out$median$R)]
# We exclude the current year from the median estimate
rec.ltm <- median(DD.out$median$R[-length(DD.out$median$R)])

# Get the percent biomass change from the projection. 0 means unchanged, + means % increase, - means % decline
percent.B.change <- (BM.proj.1yr / DD.out$median$B[length(DD.out$median$B)]) -1



####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS 
##### Now we can run some model diagnostics.
# Some quick diagnoistics, the maximum should be < 1.05
rhat <- summary(DD.out$summary[,8])

# Effective number of observations.  
#Not sure what our minimum should be here, but using the Rhat + looking at the chains should indicate where there are problems...
neff <- range(DD.out$summary[,9])


## Make the figures
# posterior densities for model parameters
post.plt(DD.out,DDpriors,years=yrs, graphic="pdf",multi=T,path=plotsGo)
#dev.off()
##exploitaiton time series
exploit.plt(DD.out, years=yrs, plt=c('f','m','mR'),graphic="pdf",path=plotsGo)
#dev.off()
# model biomass fit to survey
fit.plt(DD.out, years = yrs, CI=T,graphic="pdf",path=plotsGo,CV=T)
# diagnostic plot
diag.plt(DD.out, years = yrs,graphic="pdf",path=plotsGo)

# and our big old biomass plot
biomass.plt(DD.out,years=yrs, graphic="pdf",TAC=TACi+proj.catch,path=plotsGo,refs=NULL,pred=1,avg.line=median,Bymax=1200)

# Some plots....
plot(DD.out$mean$B~DD.out$mean$m,type="n")
text(DD.out$mean$B~DD.out$mean$m,labels=1995:2016,cex=0.65)

# Do a quick CCF sniff to see if anything interesting, nothing really, time series likely too short to be honest.
ccf(DD.out$mean$B,DD.out$mean$m)
ccf(DD.out$mean$R,DD.out$mean$m)
ccf(DD.out$mean$B,DD.out$mean$R) 
ccf(mod.dat.C4$I,mod.dat.C4$IR) 
plot(DD.out$mean$B[1:22]~DD.out$mean$m[2:23])
plot(DD.out$mean$B[1:22]~DD.out$mean$mR[2:23])
plot(DD.dat$NPR[1:22] ~DD.out$mean$mR[2:23])
plot(DD.dat$NPR[1:22] ~DD.out$mean$m[2:23])
plot(DD.dat$NPR ~DD.out$mean$m)
plot(DD.out$mean$B~DD.out$mean$mR)
# There isn't any relationship between B and n thankfully.
plot(DD.out$mean$B ~ DD.dat$n)
plot(DD.dat$I ~ DD.dat$n)
# Nothing interesting with exploitation rate and natural mortality.
plot(DD.out$mean$mu~DD.out$mean$m)

save.image(paste(direct,"Data/Framework/2018/GBa_box_model/C4_testing.RData",sep=""))
#load(paste(direct,"Data/Framework/2018/GBa_box_model/C4_testing.RData",sep=""))

####################### End the C4 Model####################### End the C4 Model####################### End the C4 Model####################### End the C4 Model
####################### End the C4 Model####################### End the C4 Model####################### End the C4 Model####################### End the C4 Model


