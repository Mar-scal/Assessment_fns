##################################################   Seedboxes   #########################################################		
##################################################   Seedboxes #########################################################		
##################################################   Seedboxes#########################################################		

# The BBn boxes sadly don't have a lot of overlap, a couple of spots but not nearly as nice as I would have hoped.  Think I need to talk with 
# Ginette more about the history of the boxes on BBn.

yr = as.numeric(format(Sys.time(), "%Y"))  # 
direct = "d:/r/"
library(RColorBrewer)
library(PBSmapping)
library(R2jags)
library(VGAMdata)
library(ggplot2)
# Load the survey data.
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.RData",sep=""))



direct = "d:/r/"
# Get the correct Shell heights for BBn.
RS <- size.cats$RS[size.cats$Bank == "BBn"]
CS <- size.cats$CS[size.cats$Bank == "BBn"]

# Load functions and external datafiles we might need

# Load in the functions needed for this function to run.
source(paste(direct,"Assessment_fns/Model/projections.r",sep=""))
source(paste(direct,"Assessment_fns/Model/decision.r",sep=""))
source(paste(direct,"Assessment_fns/Model/post.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/exploit.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Model/fit.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/peR_jags.r",sep="")) #peR is function call
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



# Now plot the fishing by year for BBn, overlay the closures we have information for, any evidence for closures at annual scale before 2008?
pdf(paste(direct,"2017/Framework/BBn_box_model/Annual_fishery_locations.pdf",sep=''),width = 8.5, height = 8.5,bg = "transparent",onefile=T)
for(i in 1981:2017)
{
ScallopMap("BBn",direct=direct)
addPolys(seedboxes[seedboxes$Bank == "BBn",],lwd=0.5,lty=2)
points(fish.dat$lon[fish.dat$year == i],fish.dat$lat[fish.dat$year == i],pch=19,cex=0.2)
title(i)
}
dev.off()
# It looks like 1997 they weren't in East of Happy Valley, but in 1998 they came back (especially the smaller EHV box to the south)
# In 1998 they were hardly in Happy Valley at all, but were around the edges quite significatly, so again possibly a closure there.
# In 1999 they weren't in Happy Valley even for a second, also not in the small EHV box, they were a bit into the larger EHV box 
# In 2000 they were in HV, though mostly focused on the SW corner of the box, not in the small EHV box at all
# In 2001 they were in HV even more heavily, essentially still not in the SW part of EHV
# In 2002 again in HV and not in EHV (either the new box or the smaller SW corner box).
# In 2003 they hit the EHV small box but still hardly anthing in the larger area.  Not so busy over in Happy Valley, but still there.
# In 2004 they hit HV hard, they are on the SE edge of small EHV box, but hardly in the larger box at all.
# In 2005 they are completely out of HV, going hard into EHV.
# In 2006 they remain out of original HV box, some effort in the "new HV" box though. No effort in EHV-SW, a bit of effort in the larger new-EHV box tho
# In 2007 they are in part of HV, most the overlapping area between the "new HV" and the "old HV".  Pretty heavy into the new EHV area but not so much EHV-SW
# In 2008 they aren't in old-HV at all, some effort in the new-HV area, not a lot tho.  They are not in HV-SW at all, moderate in new-EHV
# In 2009 there was nothing on any of the bank
# In 2010 there was effort mostly in the new-HV area, nothign in any part of EHV
# In 2011 they hit everywhere hard, except the new part of EHV
# In 2012 they hit everywhere a bit
# In 2013 they did a little bit everywhere
# In 2014 the latest version of the boxes show up, nothing in those boxes and not much in the area in general
# In 2015/2016 effort was all around the boxes, especially inbetween the two closure areas.

# Hmm... For HV, when any part of the original HV was shut the "new-HV" area didn't see much fishing at all, given there is a bit of an overlap perhaps 
# focusing on the new-HV area might be best, I like the old HV area more but the 2015-2016 exploitation in there when new-HV was shut to me
# is a problem I can't see an easy way around.
# For EHV I think we could go with the current box as well, certainly periods when there was no fishing then heavy fishing inside the box
# Let's put the catch/effort into numbers.


### BBn boxes....
# Make an EID and X & Y so that PBSmapping can pull out the tows in the right spots, the first will pull based on tow start location

surv.Live[["BBn"]]$EID<-1:nrow(surv.Live[["BBn"]])
surv.Live[["BBn"]]$X<-surv.Live[["BBn"]]$slon
surv.Live[["BBn"]]$Y<-surv.Live[["BBn"]]$slat
surv.Live[["BBn"]]$Strata_ID <- 1
# Need the clappers too...
surv.Clap[["BBn"]]$EID<-1:nrow(surv.Clap[["BBn"]])
surv.Clap[["BBn"]]$X<-surv.Clap[["BBn"]]$slon
surv.Clap[["BBn"]]$Y<-surv.Clap[["BBn"]]$slat
surv.Clap[["BBn"]]$Strata_ID <- 1

# Now get the tows that are inside the boxes based on the start location
## So lets run with using the two new boxes and pull out survey and fishery data from these.  I want to model each box seperately.
C2s.tows<- findPolys(surv.Live[["BBn"]],subset(seedboxes,ID %in% "C2-012014"))
C2s.clap <- findPolys(surv.Clap[["BBn"]],subset(seedboxes,ID %in% "C2-012014"))
C3s.tows<- findPolys(surv.Live[["BBn"]],subset(seedboxes,ID %in% "C3-012014"))
C3s.clap <- findPolys(surv.Clap[["BBn"]],subset(seedboxes,ID %in% "C3-012014"))

# This will pull on end location, this will get all tows that were at any point inside the box...
surv.Live[["BBn"]]$EID<-1:nrow(surv.Live[["BBn"]])
surv.Live[["BBn"]]$X<-surv.Live[["BBn"]]$elon
surv.Live[["BBn"]]$Y<-surv.Live[["BBn"]]$elat
surv.Live[["BBn"]]$Strata_ID <- 1
# Need the clappers too...
surv.Clap[["BBn"]]$EID<-1:nrow(surv.Clap[["BBn"]])
surv.Clap[["BBn"]]$X<-surv.Clap[["BBn"]]$elon
surv.Clap[["BBn"]]$Y<-surv.Clap[["BBn"]]$elat
surv.Clap[["BBn"]]$Strata_ID <- 1

# Now get the tows that are inside the boxes based on the start location
## So lets run with using the two new boxes and pull out survey and fishery data from these.  I want to model each box seperately.
C2e.tows<- findPolys(surv.Live[["BBn"]],subset(seedboxes,ID %in% "C2-012014"))
C2e.clap <- findPolys(surv.Clap[["BBn"]],subset(seedboxes,ID %in% "C2-012014"))
C3e.tows<- findPolys(surv.Live[["BBn"]],subset(seedboxes,ID %in% "C3-012014"))
C3e.clap <- findPolys(surv.Clap[["BBn"]],subset(seedboxes,ID %in% "C3-012014"))


C2.tows <- data.frame(EID = unique(c(unique(C2s.tows$EID),unique(C2e.tows$EID))),PID = 20, Bdry=0)
C3.tows <- data.frame(EID = unique(c(unique(C3s.tows$EID),unique(C3e.tows$EID))),PID = 20, Bdry=0)
C2.clap <- data.frame(EID = unique(c(unique(C2s.clap$EID),unique(C2e.clap$EID))),PID = 20, Bdry=0)
C3.clap <- data.frame(EID = unique(c(unique(C3s.clap$EID),unique(C3e.clap$EID))),PID = 20, Bdry=0)


# About 700 tows inside these boxes, and 1750 outside
C2.survey <- surv.Live[["BBn"]][surv.Live[["BBn"]]$EID %in% C2.tows$EID,] # Tows
C3.survey <- surv.Live[["BBn"]][surv.Live[["BBn"]]$EID %in% C3.tows$EID,] # Tows
C2.clappers <- surv.Clap[["BBn"]][surv.Clap[["BBn"]]$EID %in% C2.tows$EID,] # Tows
C3.clappers <- surv.Clap[["BBn"]][surv.Clap[["BBn"]]$EID %in% C3.tows$EID,] # Tows


# And subset the fish.dat so it's just BBN.
bbn.fish.dat <- fish.dat[fish.dat$bank == "BBn" & fish.dat$year >= 1991 & !is.na(fish.dat$bank),]
bbn.fish.dat$EID<-1:nrow(bbn.fish.dat)
bbn.fish.dat$X<-bbn.fish.dat$lon
bbn.fish.dat$Y<-bbn.fish.dat$lat
bbn.fish.dat <- bbn.fish.dat[!is.na(bbn.fish.dat$EID)]

C2.fished <- findPolys(bbn.fish.dat,subset(seedboxes,ID %in% "C2-012014"))
C3.fished <- findPolys(bbn.fish.dat,subset(seedboxes,ID %in% "C3-012014"))
C2.fishery <- bbn.fish.dat[bbn.fish.dat$EID %in% C2.fished$EID,]
C3.fishery <- bbn.fish.dat[bbn.fish.dat$EID %in% C3.fished$EID,]
# DK note: This needs checked, there are a bunch of 0 hour effort records in here that I need to sort out what they are...
C2.fishery<- C2.fishery[C2.fishery$h >0,]
C3.fishery<- C3.fishery[C3.fishery$h >0,]
# Get the area of the seedboxes and the towable area
bbn.sb <- as.PolySet(seedboxes[seedboxes$Bank =="BBn",], projection="LL")
C2.area <- calcArea(bbn.sb[bbn.sb$ID == "C2-012014",],rollup=1)
C2.area$PID <- 1
C2.area$towable_area <- C2.area$area*1000*1000/800/(8/3.2808)
C3.area <- calcArea(bbn.sb[bbn.sb$ID == "C3-012014",],rollup=1)
C3.area$towable_area <- C3.area$area*1000*1000/800/(8/3.2808)
C3.area$PID <- 1

# No tows in 1993
C2.obj <- survey.dat(C2.survey, RS=RS, CS=CS, bk="BBn", areas=C2.area[,c(1,3)], mw.par="CF",err="ran")	
C2.obj[[1]]$CF <- na.omit(sapply(1:length(unique(C2.survey$year)), function(x){with(subset(C2.survey,year == unique(C2.survey$year)[x]), weighted.mean(CF,com.bm,na.rm=T))}))
C2.clap.obj <- survey.dat(C2.clappers, RS=RS, CS=CS, bk="BBn", areas=C2.area[,c(1,3)], mw.par="CF",err="ran")	
C2.obj[[1]]$clappers<-C2.clap.obj[[1]]$N
C2.obj[[1]]$clappersR<-C2.clap.obj[[1]]$NR
C2.obj[[1]]$CS <- CS
C2.obj[[1]]$RS <- RS

C3.obj <- survey.dat(C3.survey, RS=RS, CS=CS, bk="BBn", areas=C3.area[,c(1,3)], mw.par="CF",err="ran")	
C3.obj[[1]]$CF <- na.omit(sapply(1:length(unique(C2.survey$year)), function(x){with(subset(C3.survey,year == unique(C3.survey$year)[x]), weighted.mean(CF,com.bm,na.rm=T))}))
C3.clap.obj <- survey.dat(C3.clappers, RS=RS, CS=CS, bk="BBn", areas=C3.area[,c(1,3)], mw.par="CF",err="ran")	
C3.obj[[1]]$clappers<-C3.clap.obj[[1]]$N
C3.obj[[1]]$clappersR<-C3.clap.obj[[1]]$NR
C3.obj[[1]]$CS <- CS
C3.obj[[1]]$RS <- RS

# Now for a C2 object in which 100 mm is our fully recruited size, this should line up with the Stokesbury work in C2...
C2.large <- survey.dat(C2.survey, RS=95, CS=110, bk="BBn", areas=C2.area[,c(1,3)], mw.par="CF",err="ran")	
C2.large[[1]]$CF <- na.omit(sapply(1:length(unique(C2.survey$year)), function(x){with(subset(C2.survey,year == unique(C2.survey$year)[x]), weighted.mean(CF,com.bm,na.rm=T))}))
C2.clap.large <- survey.dat(C2.clappers, RS=95, CS=110, bk="BBn", areas=C2.area[,c(1,3)], mw.par="CF",err="ran")	
C2.large[[1]]$clappers<-C2.clap.large[[1]]$N
C2.large[[1]]$clappersR<-C2.clap.large[[1]]$NR
C2.large[[1]]$CS <- 110
C2.large[[1]]$RS <- 95

# Now plot the survey stations by year.
pdf(paste(direct,"2017/Framework/BBn_box_model/Annual_survey_locations.pdf",sep=''),width = 8.5, height = 8.5,bg = "transparent",onefile=T)
for(i in 1991:2016)
{
  ScallopMap("BBn",direct=direct)
  addPolys(seedboxes[seedboxes$Bank == "BBn",],lwd=0.5,lty=2)
  if(nrow(C2.survey[C2.survey$year ==i,]) > 0) addPoints(C2.survey[C2.survey$year ==i,],pch=19,cex=0.5)
  if(nrow(C3.survey[C3.survey$year ==i,]) > 0) addPoints(C3.survey[C3.survey$year ==i,],pch=19,cex=0.5)
  title(i)
}
dev.off()

pdf(paste(direct,"2017/Framework/BBn_box_model/All_Annual_survey_locations.pdf",sep=''),width = 8.5, height = 8.5,bg = "transparent",onefile=T)
for(i in 1991:2016)
{
  ScallopMap("BBn",direct=direct)
  addPolys(seedboxes[seedboxes$Bank == "BBn",],lwd=0.5,lty=2)
  if(nrow(surv.Live[["BBn"]][surv.Live[["BBn"]]$year ==i,]) > 0) addPoints(surv.Live[["BBn"]][surv.Live[["BBn"]]$year ==i,],pch=19,cex=0.5)
  
  title(i)
}
dev.off()

###################  IMPORTANT SECTION HERE!!!!###################  IMPORTANT SECTION HERE!!!!
# DK Note I need to tidy up some of these data, this is a proposed way, I welcome other ideas...
# I need to deal with years in which there is only 1 tow in the box, there is no CV in these years.
# What I am doing is taking the 0's or NA's and filling in a value for them

# Make the 0's the median
C2.obj[[1]]$I.cv[C2.obj[[1]]$I.cv == 0] <- median(C2.obj[[1]]$I.cv[C2.obj[[1]]$I.cv != 0],na.rm=T)
C2.obj[[1]]$IR.cv[C2.obj[[1]]$IR.cv == 0 | is.nan(C2.obj[[1]]$IR.cv)] <- median(C2.obj[[1]]$IR.cv[C2.obj[[1]]$IR.cv != 0],na.rm=T)
C2.obj[[1]]$IPR.cv[C2.obj[[1]]$IPR.cv == 0] <- median(C2.obj[[1]]$IPR.cv[C2.obj[[1]]$IPR.cv != 0],na.rm=T)
C2.obj[[1]]$N.cv[C2.obj[[1]]$N.cv == 0 ] <- median(C2.obj[[1]]$N.cv[C2.obj[[1]]$N.cv != 0],na.rm=T)
C2.obj[[1]]$NR.cv[C2.obj[[1]]$NR.cv == 0| is.nan(C2.obj[[1]]$NR.cv)] <- median(C2.obj[[1]]$NR.cv[C2.obj[[1]]$NR.cv != 0],na.rm=T)
C2.obj[[1]]$NPR.cv[C2.obj[[1]]$NPR.cv == 0] <- median(C2.obj[[1]]$NPR.cv[C2.obj[[1]]$NPR.cv != 0],na.rm=T)

# This is for the l.k and w.k (no recruits...)
C2.obj[[1]]$l.k[is.nan(C2.obj[[1]]$l.k)] <- median(C2.obj[[1]]$l.k,na.rm=T)
C2.obj[[1]]$w.k[is.nan(C2.obj[[1]]$w.k)] <- median(C2.obj[[1]]$w.k,na.rm=T)
# We didn't see any recruits in 1999, only 3 tows, clearly there were lots of recruits out there given
# what we see in the years around this, need to figure out what to put here as a number, leave it as 0 even though that's not real...
C2.obj[[1]]$IR[C2.obj[[1]]$IR == 0] <- median(C2.obj[[1]]$IR[C2.obj[[1]]$IR > 0],na.rm=T)

# Similar for the C3 data....
C3.obj[[1]]$I.cv[C3.obj[[1]]$I.cv == 0] <- median(C3.obj[[1]]$I.cv[C3.obj[[1]]$I.cv != 0],na.rm=T)
C3.obj[[1]]$IR.cv[C3.obj[[1]]$IR.cv == 0 | is.nan(C3.obj[[1]]$IR.cv)] <- median(C3.obj[[1]]$IR.cv[C3.obj[[1]]$IR.cv != 0],na.rm=T)
C3.obj[[1]]$NR.cv[C3.obj[[1]]$NR.cv == 0| is.nan(C3.obj[[1]]$NR.cv)] <- median(C3.obj[[1]]$NR.cv[C3.obj[[1]]$NR.cv != 0],na.rm=T)

# This is for the l.k and w.k (no recruits...)
C3.obj[[1]]$l.k[is.nan(C3.obj[[1]]$l.k)] <- median(C3.obj[[1]]$l.k,na.rm=T)
C3.obj[[1]]$w.k[is.nan(C3.obj[[1]]$w.k)] <- median(C3.obj[[1]]$w.k,na.rm=T)

C3.obj[[1]]$IR[C3.obj[[1]]$IR == 0] <- median(C3.obj[[1]]$IR[C3.obj[[1]]$IR > 0],na.rm=T)
C3.obj[[1]]$NR[C3.obj[[1]]$NR == 0] <- min(C3.obj[[1]]$NR[C3.obj[[1]]$NR != 0],na.rm=T)

C3.obj[[1]]$clappersR[C3.obj[[1]]$clappersR == 0] <- min(C3.obj[[1]]$clappersR[C3.obj[[1]]$clappersR > 0],na.rm=T)

# And for the C2.large object
# Make the 0's the median
C2.large[[1]]$I.cv[C2.large[[1]]$I.cv == 0] <- median(C2.large[[1]]$I.cv[C2.large[[1]]$I.cv != 0],na.rm=T)
C2.large[[1]]$IR.cv[C2.large[[1]]$IR.cv == 0 | is.nan(C2.large[[1]]$IR.cv)] <- median(C2.large[[1]]$IR.cv[C2.large[[1]]$IR.cv != 0],na.rm=T)
C2.large[[1]]$IPR.cv[C2.large[[1]]$IPR.cv == 0] <- median(C2.large[[1]]$IPR.cv[C2.large[[1]]$IPR.cv != 0],na.rm=T)
C2.large[[1]]$N.cv[C2.large[[1]]$N.cv == 0 ] <- median(C2.large[[1]]$N.cv[C2.large[[1]]$N.cv != 0],na.rm=T)
C2.large[[1]]$NR.cv[C2.large[[1]]$NR.cv == 0| is.nan(C2.large[[1]]$NR.cv)] <- median(C2.large[[1]]$NR.cv[C2.large[[1]]$NR.cv != 0],na.rm=T)
C2.large[[1]]$NPR.cv[C2.large[[1]]$NPR.cv == 0] <- median(C2.large[[1]]$NPR.cv[C2.large[[1]]$NPR.cv != 0],na.rm=T)

# This is for the l.k and w.k (no recruits...)
C2.large[[1]]$l.k[is.nan(C2.large[[1]]$l.k)] <- median(C2.large[[1]]$l.k,na.rm=T)
C2.large[[1]]$w.k[is.nan(C2.large[[1]]$w.k)] <- median(C2.large[[1]]$w.k,na.rm=T)
# We didn't see any recruits in 1999, only 3 tows, clearly there were lots of recruits out there given
# what we see in the years around this, need to figure out what to put here as a number, leave it as 0 even though that's not real...
C2.large[[1]]$IR[C2.large[[1]]$IR == 0] <- median(C2.large[[1]]$IR[C2.large[[1]]$IR > 0],na.rm=T)




###########################  Model RUNS###########################  Model RUNS###########################  Model RUNS#############################################
###########################  Model RUNS###########################  Model RUNS###########################  Model RUNS#############################################

##########################  THE C2 MODEL #######################################  THE C2 MODEL #######################################  THE C2 MODEL #############
##########################  THE C2 MODEL #######################################  THE C2 MODEL #######################################  THE C2 MODEL #############
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
# 
# Run this for one or both banks
# Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
years <- min(C2.obj[[1]]$year):max(C2.obj[[1]]$year)
# Bring in the vonB parameters..
vonB.par <-vonB[vonB$Bank =="BBn",]
# Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
cpue.dat <- fishery.dat(C2.fishery,bk="BBn",yr=c(1991:2017),method='jackknife',surv='May',
                        direct=direct,period = "survyr") 	

# Now on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
#for all other years we need to do this for Browns Bank North
# It really makes very little difference which way this is done as the catch in June-August
# has averaged around just 40 tonnes since about 1996.
if(yr != 2015) cpue.dat <- fishery.dat(C2.fishery,bk="BBn",yr=c(1991:2017),surv='May',
                                       method='jackknife',direct=direct,period = "survyr") 	
if(any(is.na(cpue.dat))) cpue.dat[is.na(cpue.dat)] <- 0 # Set any NA's to 0. 
# Combine the survey and Fishery data here.
mod.dat.C2 <- merge(C2.obj[[1]],cpue.dat,by ="year")
# DK Note For years in which we have essentially no fishing effort on the bank we need to blow up the variance of those estimates.
# If < 20 tonnes assume low effort and blow up the CV to 1.
mod.dat.C2$cpue.se[mod.dat.C2$catch < 20] <- mod.dat.C2$cpue[mod.dat.C2$catch < 20]
# Get the CV for the CPUE...
mod.dat.C2$U.cv <- mod.dat.C2$cpue.se/mod.dat.C2$cpue
mod.dat.C2$U.cv[is.nan(mod.dat.C2$U.cv)] <- 1 # Doesn't really matter since CPUE =0

# now get the catch data from end of survey until end of the year for the projection
proj.sub <- subset(C2.fishery,year %in% years & months(as.Date(C2.fishery$date)) %in% c("September","October","November","December"))
# Again on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
# for all other years we need to do this for Browns Bank North
# Note that June-August seems to be a pretty minimal fishery on BBn
if(yr != 2015 ) proj.sub <- subset(C2.fishery,year %in% years & months(as.Date(C2.fishery$date)) 
                                   %in% c("June","July","August","September","October","November","December"))
# Now calculate the fishery statistics for the projection period
proj.dat <- fishery.dat(proj.sub,bk="BBn",yr=(min(years)-1):max(years),method='jackknife',
                        direct=direct,period = "calyr") 	
# So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
# and the biomass from each tow to come up with an overall bank average condition factor.
# This is weight in this year, which becomes t-1 
waa.tm1 <- mod.dat.C2$CF*(mod.dat.C2$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat.C2$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat.C2$CF[-1],mod.dat.C2$CF[nrow(mod.dat.C2)])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
waa.t2 <- mod.dat.C2$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat.C2$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat.C2$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat.C2$CF*(mod.dat.C2$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat.C2$l.k
waa.t <- c(mod.dat.C2$CF[-1],mod.dat.C2$CF[nrow(mod.dat.C2)])*(laa.t/100)^3
waa.t2 <- mod.dat.C2$CF*(laa.t/100)^3
mod.dat.C2$gR <- waa.t/waa.tm1
mod.dat.C2$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
mod.dat.C2$U.cv[is.nan(mod.dat.C2$U.cv)] <- median(mod.dat.C2$U.cv,na.rm=T)
mod.dat.C2$cpue[mod.dat.C2$cpue == 0] <- median(mod.dat.C2$cpue,na.rm=T)

# DK Note, more refining, blow up the survey variance in years we don't have sufficient tows...
mod.dat.C2$I.cv[mod.dat.C2$n.x < 5] <- 1
mod.dat.C2$IR.cv[mod.dat.C2$n.x < 5] <- 1
mod.dat.C2$U.cv[mod.dat.C2$n.x >= 5] <- 1 # Downweight the CPUE data when we have good survey data...
# Now for the model...

# Set the working directory for figures and tables to be output
plotsGo <- paste(direct,"2017/Framework/BBn_box_model/Figures_and_Tables/C2/",sep="")
#Read2 Get the managment data, note this file needs updated annually! 
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Grab the data, start model at either 1986 (note that BBn data starts in 1991 so anything earlier will default to 1991)
DD.dat <- subset(mod.dat.C2,year %in% 1995:2017)
names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                    "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS","RS","C","E","n.trips","U",
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
#ifelse(names(DD.lst)[9] == "U.se", names(DD.lst)[9] <- "U.cv", DD.lst$U.cv <- DD.lst$U.cv*50)
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
                     model.file = paste(direct,model.jags,sep=""),n.chains = 8, n.iter = 100000, n.burnin = 40000, 
                     n.thin = 25)
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
# The increment size for the decision table.  500 for GBa and 50 for BBn
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
D.tab<-decision(DD.out,"BBn", mu=0.15,post.survey.C=proj.catch)
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

# Hmm, actually there is something here, whenever B was above 1500 m was above 0.22.  Recruit mortality
# also happened to be high in these same periods.
plot(DD.out$mean$B~DD.out$mean$m,type="n")
text(DD.out$mean$B~DD.out$mean$m,labels=1995:2016,cex=0.65)

# Do a quick CCF sniff to see if anything interesting, nothing really, time series likely too short to be honest.
ccf(DD.out$mean$B,DD.out$mean$m)
ccf(DD.out$mean$R,DD.out$mean$m)
ccf(DD.out$mean$B,DD.out$mean$R) # Even the 1 year lag  b/t recruits and fully recruited isn't poping out very strong, interesting 6 year cycle signal.
ccf(mod.dat.C2$I,mod.dat.C2$IR) # more or less the same here, again suggestion to not use the pre 2000 data is sensisble.
plot(DD.out$mean$B[1:22]~DD.out$mean$m[2:23])
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

save.image(paste(direct,"Data/Framework/2017/BBn_box_model/C2_testing.RData",sep=""))
#load(paste(direct,"Data/Framework/2017/BBn_box_model/C2_testing.RData",sep=""))

####################### End the C2 Model####################### End the C2 Model####################### End the C2 Model####################### End the C2 Model
####################### End the C2 Model####################### End the C2 Model####################### End the C2 Model####################### End the C2 Model


##########################  THE C3 MODEL #######################################  THE C3 MODEL #######################################  THE C3 MODEL #############
##########################  THE C3 MODEL #######################################  THE C3 MODEL #######################################  THE C3 MODEL #############
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
# 
# Run this for one or both banks
# Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
years <- min(C3.obj[[1]]$year):max(C3.obj[[1]]$year)
# Bring in the vonB parameters..
vonB.par <-vonB[vonB$Bank =="BBn",]
# Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
cpue.dat <- fishery.dat(C3.fishery,bk="BBn",yr=c(1991:2017),method='jackknife',surv='May',
                        direct=direct,period = "survyr") 	

# Now on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
#for all other years we need to do this for Browns Bank North
# It really makes very little difference which way this is done as the catch in June-August
# has averaged around just 40 tonnes since about 1996.
if(yr != 2015) cpue.dat <- fishery.dat(C3.fishery,bk="BBn",yr=c(1991:2017),surv='May',
                                       method='jackknife',direct=direct,period = "survyr") 	
if(any(is.na(cpue.dat))) cpue.dat[is.na(cpue.dat)] <- 0 # Set any NA's to 0. 
#cpue.dat <- rbind(cpue.dat,c(2016,rep(0,8)))

# Combine the survey and Fishery data here.
mod.dat.C3 <- merge(C3.obj[[1]],cpue.dat,by ="year")
# DK Note For years in which we have essentially no fishing effort on the bank we need to blow up the variance of those estimates.
# If < 20 tonnes assume low effort and blow up the CV to 1.
mod.dat.C3$cpue.se[mod.dat.C3$catch < 20] <- mod.dat.C3$cpue[mod.dat.C3$catch < 20]
# Get the CV for the CPUE...
mod.dat.C3$U.cv <- mod.dat.C3$cpue.se/mod.dat.C3$cpue
mod.dat.C3$U.cv[is.nan(mod.dat.C3$U.cv)] <- 1 # Doesn't really matter since CPUE =0

# now get the catch data from end of survey until end of the year for the projection
proj.sub <- subset(C3.fishery,year %in% years & months(as.Date(C3.fishery$date)) %in% c("September","October","November","December"))
# Again on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
# for all other years we need to do this for Browns Bank North
# Note that June-August seems to be a pretty minimal fishery on BBn
if(yr != 2015 ) proj.sub <- subset(C3.fishery,year %in% years & months(as.Date(C3.fishery$date)) 
                                   %in% c("June","July","August","September","October","November","December"))
# Now calculate the fishery statistics for the projection period
proj.dat <- fishery.dat(proj.sub,bk="BBn",yr=(min(years)-1):max(years),method='jackknife',
                        direct=direct,period = "calyr") 	
# So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
# and the biomass from each tow to come up with an overall bank average condition factor.
# This is weight in this year, which becomes t-1 
waa.tm1 <- mod.dat.C3$CF*(mod.dat.C3$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat.C3$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat.C3$CF[-1],mod.dat.C3$CF[nrow(mod.dat.C3)])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
waa.t2 <- mod.dat.C3$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat.C3$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat.C3$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat.C3$CF*(mod.dat.C3$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat.C3$l.k
waa.t <- c(mod.dat.C3$CF[-1],mod.dat.C3$CF[nrow(mod.dat.C3)])*(laa.t/100)^3
waa.t2 <- mod.dat.C3$CF*(laa.t/100)^3
mod.dat.C3$gR <- waa.t/waa.tm1
mod.dat.C3$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
mod.dat.C3$U.cv[is.nan(mod.dat.C3$U.cv)] <- median(mod.dat.C3$U.cv,na.rm=T)
mod.dat.C3$cpue[mod.dat.C3$cpue == 0] <- median(mod.dat.C3$cpue,na.rm=T)

# DK Note, more refining, blow up the survey variance in years we don't have sufficient tows...
mod.dat.C3$I.cv[mod.dat.C3$n.x < 5] <- 1
mod.dat.C3$IR.cv[mod.dat.C3$n.x < 5] <- 1
mod.dat.C3$U.cv[mod.dat.C3$n.x >= 5] <- 1 # Downweight the CPUE data when we have good survey data...
# Now for the model...

# Set the working directory for figures and tables to be output
plotsGo <- paste(direct,"2017/Framework/BBn_box_model/Figures_and_Tables/C3/",sep="")
#Read2 Get the managment data, note this file needs updated annually! 
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Grab the data, start model at either 1986 (note that BBn data starts in 1991 so anything earlier will default to 1991)
DD.dat <- subset(mod.dat.C3,year %in% 1994:2017)
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
#ifelse(names(DD.lst)[9] == "U.se", names(DD.lst)[9] <- "U.cv", DD.lst$U.cv <- DD.lst$U.cv*50)
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
                     model.file = paste(direct,model.jags,sep=""),n.chains = 8, n.iter = 100000, n.burnin = 40000, 
                     n.thin = 25)
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
# The increment size for the decision table.  500 for GBa and 50 for BBn
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
D.tab<-decision(DD.out,"BBn", mu=0.15,post.survey.C=proj.catch)
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

# Hmm, actually there is something here, whenever B was above 1500 m was above 0.22.  Recruit mortality
# also happened to be high in these same periods.
plot(DD.out$mean$B~DD.out$mean$m,type="n")
text(DD.out$mean$B~DD.out$mean$m,labels=1995:2016,cex=0.65)

# Do a quick CCF sniff to see if anything interesting, nothing really, time series likely too short to be honest.
ccf(DD.out$mean$B,DD.out$mean$m)
ccf(DD.out$mean$R,DD.out$mean$m)
ccf(DD.out$mean$B,DD.out$mean$R) # Can see a two year lag b/t recruits and Biomass peaking here which is interesting...
ccf(mod.dat.C3$I,mod.dat.C3$IR) # Using just the survey data the 1 year lag is clearer and both are fairly significant.  Interesting the model weakens this...

plot(DD.out$mean$B[2:23]~DD.out$mean$R[1:22]) # 1 year lag, Some weirdness in there for sure, problem is high biomasses with low recruits which is likely
# years in which the recruitment 2 years previous was very strong, need something more co-horty to get at this I think.
plot(DD.out$mean$B[3:23]~DD.out$mean$R[1:21]) # 2 year lag, can see this is really driven by 2 points, odd this isn't more obvious
plot(mod.dat.C3$I[2:23]~mod.dat.C3$IR[1:22]) # That 1 year lag is also driven by 1 point so less than stellar.
plot(mod.dat.C3$I[3:23]~mod.dat.C3$IR[1:21]) # That 2 year lag is also driven by 1 point so less than stellar.
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


save.image(paste(direct,"Data/Framework/2017/BBn_box_model/C3_testing.RData",sep=""))
#load(paste(direct,"Data/Framework/2017/BBn_box_model/C3_testing.RData",sep=""))

####################### End the C3 Model####################### End the C3 Model####################### End the C3 Model####################### End the C3 Model
####################### End the C3 Model####################### End the C3 Model####################### End the C3 Model####################### End the C3 Model


##########################  THE C2 large MODEL #######################################  THE C2 larege MODEL #######################################  
##########################  THE C2 large MODEL #######################################  THE C2 larege MODEL #######################################  
## The biggest problem I have with this model is that we assume all the catch is larger than 110 which is a pretty bad assumption!!
# 
# Run this for one or both banks
# Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
years <- min(C2.large[[1]]$year):max(C2.large[[1]]$year)
# Bring in the vonB parameters..
vonB.par <-vonB[vonB$Bank =="BBn",]
# Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
cpue.dat <- fishery.dat(C2.fishery,bk="BBn",yr=c(1991:2017),method='jackknife',surv='May',
                        direct=direct,period = "survyr") 	

# Now on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
#for all other years we need to do this for Browns Bank North
# It really makes very little difference which way this is done as the catch in June-August
# has averaged around just 40 tonnes since about 1996.
if(yr != 2015) cpue.dat <- fishery.dat(C2.fishery,bk="BBn",yr=c(1991:2017),surv='May',
                                       method='jackknife',direct=direct,period = "survyr") 	
if(any(is.na(cpue.dat))) cpue.dat[is.na(cpue.dat)] <- 0 # Set any NA's to 0. 
# Combine the survey and Fishery data here.
mod.dat.C2.large <- merge(C2.large[[1]],cpue.dat,by ="year")
# DK Note For years in which we have essentially no fishing effort on the bank we need to blow up the variance of those estimates.
# If < 20 tonnes assume low effort and blow up the CV to 1.
mod.dat.C2.large$cpue.se[mod.dat.C2.large$catch < 20] <- mod.dat.C2.large$cpue[mod.dat.C2.large$catch < 20]
# Get the CV for the CPUE...
mod.dat.C2.large$U.cv <- mod.dat.C2.large$cpue.se/mod.dat.C2.large$cpue
mod.dat.C2.large$U.cv[is.nan(mod.dat.C2.large$U.cv)] <- 1 # Doesn't really matter since CPUE =0

# now get the catch data from end of survey until end of the year for the projection
proj.sub <- subset(C2.fishery,year %in% years & months(as.Date(C2.fishery$date)) %in% c("September","October","November","December"))
# Again on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
# for all other years we need to do this for Browns Bank North
# Note that June-August seems to be a pretty minimal fishery on BBn
if(yr != 2015 ) proj.sub <- subset(C2.fishery,year %in% years & months(as.Date(C2.fishery$date)) 
                                   %in% c("June","July","August","September","October","November","December"))
# Now calculate the fishery statistics for the projection period
proj.dat <- fishery.dat(proj.sub,bk="BBn",yr=(min(years)-1):max(years),method='jackknife',
                        direct=direct,period = "calyr") 	
# So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
# and the biomass from each tow to come up with an overall bank average condition factor.
# This is weight in this year, which becomes t-1 
waa.tm1 <- mod.dat.C2.large$CF*(mod.dat.C2.large$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat.C2.large$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat.C2.large$CF[-1],mod.dat.C2.large$CF[nrow(mod.dat.C2.large)])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
waa.t2 <- mod.dat.C2.large$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat.C2.large$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat.C2.large$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat.C2.large$CF*(mod.dat.C2.large$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat.C2.large$l.k
waa.t <- c(mod.dat.C2.large$CF[-1],mod.dat.C2.large$CF[nrow(mod.dat.C2.large)])*(laa.t/100)^3
waa.t2 <- mod.dat.C2.large$CF*(laa.t/100)^3
mod.dat.C2.large$gR <- waa.t/waa.tm1
mod.dat.C2.large$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
mod.dat.C2.large$U.cv[is.nan(mod.dat.C2.large$U.cv)] <- median(mod.dat.C2.large$U.cv,na.rm=T)
mod.dat.C2.large$cpue[mod.dat.C2.large$cpue == 0] <- median(mod.dat.C2.large$cpue,na.rm=T)

# DK Note, more refining, blow up the survey variance in years we don't have sufficient tows...
mod.dat.C2.large$I.cv[mod.dat.C2.large$n.x < 5] <- 1
mod.dat.C2.large$IR.cv[mod.dat.C2.large$n.x < 5] <- 1
mod.dat.C2.large$U.cv[mod.dat.C2.large$n.x >= 5] <- 1 # Downweight the CPUE data when we have good survey data...
# Now for the model...

# Set the working directory for figures and tables to be output
plotsGo <- paste(direct,"2017/Framework/BBn_box_model/Figures_and_Tables/C2_large/",sep="")
#Read2 Get the managment data, note this file needs updated annually! 
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Grab the data, start model at either 1986 (note that BBn data starts in 1991 so anything earlier will default to 1991)
DD.dat <- subset(mod.dat.C2.large,year %in% 1995:2017)
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
#ifelse(names(DD.lst)[9] == "U.se", names(DD.lst)[9] <- "U.cv", DD.lst$U.cv <- DD.lst$U.cv*50)
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
                     model.file = paste(direct,model.jags,sep=""),n.chains = 8, n.iter = 100000, n.burnin = 40000, 
                     n.thin = 25)
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
# The increment size for the decision table.  500 for GBa and 50 for BBn
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
D.tab<-decision(DD.out,"BBn", mu=0.15,post.survey.C=proj.catch)
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

# Hmm, actually there is something here, whenever B was above 1500 m was above 0.22.  Recruit mortality
# also happened to be high in these same periods.
plot(DD.out$mean$B~DD.out$mean$m,type="n")
text(DD.out$mean$B~DD.out$mean$m,labels=1995:2016,cex=0.65)

# Do a quick CCF sniff to see if anything interesting, nothing really, time series likely too short to be honest.
ccf(DD.out$mean$B,DD.out$mean$m)
ccf(DD.out$mean$R,DD.out$mean$m)
ccf(DD.out$mean$B,DD.out$mean$R) # Even the 1 year lag  b/t recruits and fully recruited isn't poping out very strong, interesting 6 year cycle signal.
ccf(mod.dat.C2.large$I,mod.dat.C2.large$IR) # more or less the same here, again suggestion to not use the pre 2000 data is sensisble.
plot(DD.out$mean$B[1:22]~DD.out$mean$m[2:23])
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

save.image(paste(direct,"Data/Framework/2017/BBn_box_model/C2_large_testing.RData",sep=""))
#load(paste(direct,"Data/Framework/2017/BBn_box_model/C2_testing.RData",sep=""))

####################### End the C2 large Model####################### End the C2 large Model####################### End the C2 large Model######################
####################### End the C2 large Model####################### End the C2 large Model####################### End the C2 large Model######################


##################################  This section is runnig a DeLury Model for the years we have enough data ####################################
##################################  This section is runnig a DeLury Model for the years we have enough data ####################################
##################################  This section is runnig a DeLury Model for the years we have enough data ####################################
## A very quick and dirty DeLury depelation estimate, all you need is the catch/effort within year to do this, problem is
## it assumes a closed population which seems less than ideal unless a short fishery, which is certainly was in 2017!!
## The Leslie model used here is the easiest to explain and to show, basically it is a regression of cpue v.s. cumulative catch
## as cumulative catch increases we expect CPUE to trend towards 0, so the exploitable biomass is simply where the regression line
# crosses the y axis (i.e. the cumulative catch when CPUE = 0 is the exploitable biomass at the start).  The Leslie model could use any
# index of abundance for the y, the parameterization used in the canned package is simply CPUE ~ cumulative catch, which is fine, I compare
# that with a Catch ~ Cumulative catch model as well, but the CPUE is better since it is standardized

# Delury Model... much less straightforward, but hinges on some assumptions that enable us to assume Effort is a good proxy for catch.
# The Delury model (see DeLury 1947 especially App 1) is slightly different, it is a regression of log(CPUE) against the cumulative effort.  
# Really it is a special case of the Leslie Model where Effort can be used as a proxy for catch assuming certain assumptions hold
# Here we are using an exponential model which starts with saying the that rate of change in abundance during fishing is essentially 
# just a function of the effort (and catchability, which is the abundance per unit effort) so in set time window delta(N) = total effort * catchability
# assuming the fishing happens quickly so popualation 
# is effectively closed (ignoring natural mortality, movement, and growth) this might not be a terrible set of assumptions for a short duration high 
# intensity scallop fishery like what happens in a seedbox.
# If we look in a particular window of time the Number at the end is a function of the Number at the start * cumulative effort.
# But of course we likely won't have the number at the end.  But the rate of change in abundance per unit of effort (CPUE) can replace the abundance
# and leads to the the equation CPUE = k*(N0)*exp(-k*E(t)), where E(t) is the cumulative catch at time (t), k is catchability coefficient, and N0
# is the starting population size (which is what we are interested in).  We linearize this by taking the logs resulting in the DeLury equation
# log(CPUE) = log(k*N0) -k*E(t)
# the Effort term (-k*)

# First the C2 fishery
C2.fishery <- bbn.fish.dat[bbn.fish.dat$EID %in% C2.fished$EID,]
tot.catch <- aggregate(pro.repwt/1000~year,C2.fishery,FUN=sum)

# To be more consistent let's segregate the fisheries
C2.fishery <- C2.fishery[C2.fishery$fleet == "FT",]
# Calculate the annual catch on a given bank.
C2.ann.catch <- aggregate(pro.repwt/1000~year,C2.fishery,FUN=sum)

catch<- aggregate(pro.repwt~date,C2.fishery,FUN=sum)
catch.by.vessel <- aggregate(pro.repwt/1000~ves + date.land,C2.fishery[C2.fishery$year == 2017,],FUN=sum)
cpue.by.vessel <- aggregate(kg.hm~ves+date.land,C2.fishery[C2.fishery$year == 2017,],FUN=median)
# Calculate the annual effort on a given bank, notice this is in HOUR-METERS
effort <- aggregate(hm~date,C2.fishery,FUN=sum) # Wheter we us Hour or hour-meters does matter in the result, not huge difference, but a difference.


C2.deLury.dat <- merge(catch,effort,by="date")
C2.deLury.dat$year <- as.numeric(format(C2.deLury.dat$date, "%Y"))
C2.deLury.dat$cpue <- catch$pro.repwt/effort$h
names(C2.deLury.dat) <- c("date","catch","effort","year","cpue")

# Plot the cpue over time...
windows(11,11)
p <- ggplot(C2.deLury.dat, aes(date,cpue)) +
  facet_wrap(~year,nrow=4,scales = "free_x") +  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  geom_point(color="blue") # Now add the points
p
# From this the years in which the Depletion model could work are...1995,1997,1998(maybe),2000,2001,2002,2003,2004,maybe 2007,
# 2010, 2011, and 2017
#plot(test$catch~test$effort)
Dep.years <- c(1995,1997,1998,2000,2001,2002,2003,2004,2007,2010,2011,2017)
Dep.years <- 2017
mod.DL <- NULL
mod.DLr <- NULL
mod.Les <- NULL
mod.output <- NULL
mod.fit <- NULL
mod.bm <- data.frame(Biomass.DL= rep(NA,length(Dep.years)),
                     Biomass.DLr= rep(NA,length(Dep.years)),
                     Biomass.Les= rep(NA,length(Dep.years)),
                     Biomass.Les.catch = rep(NA,length(Dep.years)),
                     years = Dep.years)
count <- 0
for(i in Dep.years)
{
  count <- count + 1
  # For a subset of years (mostly the early ones) I only take the data up to the start of June, I think I did this for years when
  # the fishery mostly occurs in June but then there are a couple trips late in the season, apples to apples comparison should happen 
  # for a shorter duration fishery.
  # Note that the biomass estimates really would correspond to the previous year's survey (with some growth) results.
  if(i %in% c(1995,1997,1998,2000,2001,2002,2004)) dat <- C2.deLury.dat[C2.deLury.dat$year == i & C2.deLury.dat$date < paste(i,"-06-01",sep=""),]
  if(i %in% c(2003,2007,2010,2011,2017)) dat <- C2.deLury.dat[C2.deLury.dat$year == i ,]
  mod.DL[[as.character(i)]] <- DeLury(dat$catch, dat$effort,type="DeLury",ricker=F)
  mod.DLr[[as.character(i)]] <- DeLury(dat$catch, dat$effort,type="DeLury",ricker=T)
  mod.Les[[as.character(i)]] <- DeLury(dat$catch, dat$effort,type="Leslie",ricker=F)
  # An alternative Leslie model using catch as the y variable, curious how different this is to CPUE
  les.alt <-  lm(mod.Les[[as.character(i)]]$catch ~ mod.Les[[as.character(i)]]$K)
  
  mod.bm$Biomass.DL[count] <- mod.DL[[as.character(i)]]$N0/1e6 # Convert to Kilo-tonnes   
  mod.bm$Biomass.DLr[count] <- mod.DLr[[as.character(i)]]$N0/1e6 # Convert to Kilo-tonnes   
  mod.bm$Biomass.Les[count] <- mod.Les[[as.character(i)]]$N0/1e6 # Convert to Kilo-tonnes  
  mod.bm$Biomass.Les.catch[count] <- les.alt$coefficients[1] / -(les.alt$coefficients[2]) /1e6 # Get initial biomass and convert to Kilo-tonnes  
  mod.output[[as.character(i)]] <- data.frame(cpue = mod.DL[[as.character(i)]]$CPUE,effort = mod.DL[[as.character(i)]]$E,
                                              catch = mod.Les[[as.character(i)]]$catch,cum_catch = mod.Les[[as.character(i)]]$K,year = i)
  mod.fit[[as.character(i)]] <- with(mod.DL[[as.character(i)]], lmfit)
  
} # end  for(i in Dep.years)

# For the Deluary model the initial Number/biomass is exp(intercept)/-(slope)
exp(mod.DL[["2017"]]$lmfit$coefficients[1])/-(mod.DL[["2017"]]$lmfit$coefficients[2])

p <- ggplot(mod.output[["2017"]], aes(cum_catch,cpue)) +
  geom_point(color="blue") +                   # Now add the points
  geom_smooth(method = "lm") # This is the regression for each year, same thing as is done in the DeLury model.
windows(11,11); p

mod.output <- do.call("rbind",mod.output)

# It is worth noting that because these data are for the most part from the start of the year (exceptions being 2003 and 2007,2010,2011 which are both post survey)
# That the years really correspond to the year before in our models.
# For example in 2004 we used catch from Jan-May 2004, because this is getting the biomass estimate at the start of this time series this is the
# Biomass in January of 2004 which should be more similar to the 2003 survey results b/c far less fishing has occured (in general) in the fall
# Then occurs between Jan-May on BBn (typically but not always)
mod.bm # This is the model biomass estimate in Kilo-tonnes for the 3 different methods.
windows(11,11)
with(mod.DL[[2017]] , plot(log(CPUE) ~ E, las = 1, pch = 19, main = "DeLury method",
                           xlab = "E(t)", ylab = "1 + log(C(t))", col = "blue"))
#mylmfit <- with(table1, lmfit)
lines(mod.fit[[5]]$x[, 2],  predict.lm(mod.fit[[5]]), col = "red", lty = "dashed")


# Now plot this by year...

p <- ggplot(mod.output, aes(effort,log(cpue))) +
  facet_wrap(~year,nrow=4)+  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  geom_point(color="blue") +                   # Now add the points
  geom_smooth(method = "lm") # This is the regression for each year, same thing as is done in the DeLury model.
windows(11,11); p

# Just the 2017 data...
p <- ggplot(mod.output[mod.output$year==2017,], aes(effort,log(cpue))) +
  geom_point(color="blue") +                   # Now add the points
  geom_smooth(method = "lm") # This is the regression for each year, same thing as is done in the DeLury model.
windows(11,11); p

# How does the DeLury q compare to the model fishery q?  Can see the Delury method gives a much smaller q.  I don't
# know if these are really directly comparable mind you..
DD.out$median$qU
mod.fit # The Et(i.e. the slope) is equivalent to the catchability.


###################
### Now do this for the C3 fishery
##################
C3.fishery <- bbn.fish.dat[bbn.fish.dat$EID %in% C3.fished$EID,]
tot.catch <- aggregate(pro.repwt/1000~year,C3.fishery,FUN=sum)
C3.fishery

# Just look one fleet at a time, too garbled to compare using both
C3.fishery <- C3.fishery[C3.fishery$fleet == "FT",]

# Can see teh GS Mersey is only vessel out before about April 15th, this skews the CPUE downwards for the first bit of the time series in 2017
C3.fishery[C3.fishery$year == 2017,c("date","ves")]
# Calculate the daily catch on a given bank.
catch<- aggregate(pro.repwt~date,C3.fishery,FUN=sum)
C3.ann.catch <- aggregate(pro.repwt/1000~year,C3.fishery,FUN=sum)
catch.by.vessel <- aggregate(pro.repwt/1000~ves + date.land,C3.fishery[C3.fishery$year == 2017,],FUN=sum)
cpue.by.vessel <- aggregate(kg.hm~ves+date.land,C3.fishery[C3.fishery$year == 2017,],FUN=median)
# Calculate the daily effort on a given bank, notice this is in HOUR-METERS
effort <- aggregate(hm~date,C3.fishery,FUN=sum) # Wheter we us Hour or hour-meters does matter in the result, not huge difference, but a difference.

C3.deLury.dat <- merge(catch,effort,by="date")
C3.deLury.dat$year <- as.numeric(format(C3.deLury.dat$date, "%Y"))
C3.deLury.dat$cpue <- catch$pro.repwt/effort$hm
names(C3.deLury.dat) <- c("date","catch","effort","year","cpue")
# Let's look at one year...
#test <- C3.deLury.dat[C3.deLury.dat$year == 2004 & C3.deLury.dat$date < "2004-06-01",]

windows(11,11)
p <- ggplot(C3.deLury.dat, aes(date,cpue)) +
  facet_wrap(~year,nrow=4,scales = "free_x") +  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  geom_point(color="blue") # Now add the points
p
# From this the years in which the Depletion model could work are...1994,1995,1996,2003,2005,2007,2008,2011,2012,2013,2017
#plot(test$catch~test$effort)
Dep.years <- c(1994,1995,1996,2003,2005,2007,2008,2011,2013,2017)
Dep.years <- 2017
mod.DL <- NULL
mod.DLr <- NULL
mod.Les <- NULL
mod.output <- NULL
mod.fit <- NULL
mod.bm <- data.frame(Biomass.DL= rep(NA,length(Dep.years)),
                     Biomass.DLr= rep(NA,length(Dep.years)),
                     Biomass.Les= rep(NA,length(Dep.years)),
                     Biomass.Les.catch = rep(NA,length(Dep.years)),
                     years = Dep.years)
count <- 0
for(i in Dep.years)
{
  count <- count + 1
  if(i %in% c(1995,2011,2013)) dat <- C3.deLury.dat[C3.deLury.dat$year == i & C3.deLury.dat$date < paste(i,"-06-01",sep=""),]
  if(i %in% c(1994,1996,2003,2005,2007,2008,2017)) dat <- C3.deLury.dat[C3.deLury.dat$year == i ,]
  # In 2017 the catch rates are low for the first bit of the fishery, this appears due to just the GS Mersey and approaching the middle of April
  # a couple of Comeau boats being out there, so dump the fishery before April 15th and it at least gives a number....
  mod.DL[[as.character(i)]] <- DeLury(dat$catch, dat$effort,type="DeLury",ricker=F)
  mod.DLr[[as.character(i)]] <- DeLury(dat$catch, dat$effort,type="DeLury",ricker=T)
  mod.Les[[as.character(i)]] <- DeLury(dat$catch, dat$effort,type="Leslie",ricker=F)
  # An alternative Leslie model using catch as the y variable, curious how different this is to CPUE
  les.alt <-  lm(mod.Les[[as.character(i)]]$catch ~ mod.Les[[as.character(i)]]$K)
  
  mod.bm$Biomass.DL[count] <- mod.DL[[as.character(i)]]$N0/1e6 # Convert to Kilo-tonnes   
  mod.bm$Biomass.DLr[count] <- mod.DLr[[as.character(i)]]$N0/1e6 # Convert to Kilo-tonnes   
  mod.bm$Biomass.Les[count] <- mod.Les[[as.character(i)]]$N0/1e6 # Convert to Kilo-tonnes  
  mod.bm$Biomass.Les.catch[count] <- les.alt$coefficients[1] / -(les.alt$coefficients[2]) /1e6 # Get initial biomass and convert to Kilo-tonnes  
  mod.output[[as.character(i)]] <- data.frame(cpue = mod.DL[[as.character(i)]]$CPUE,effort = mod.DL[[as.character(i)]]$E,
                                              catch = mod.Les[[as.character(i)]]$catch,cum_catch = mod.Les[[as.character(i)]]$K,year = i)
  mod.fit[[as.character(i)]] <- with(mod.DL[[as.character(i)]], lmfit)
  
} # end  for(i in Dep.years)

# For the Deluary model the initial Number/biomass is exp(intercept)/-(slope)
exp(mod.DL[["2017"]]$lmfit$coefficients[1])/-(mod.DL[["2017"]]$lmfit$coefficients[2])

p <- ggplot(mod.output[["2017"]], aes(cum_catch,cpue)) +
  geom_point(color="blue") +                   # Now add the points
  geom_smooth(method = "lm") # This is the regression for each year, same thing as is done in the DeLury model.
windows(11,11); p

mod.output <- do.call("rbind",mod.output)

# It is worth noting that because these data are for the most part from the start of the year (exceptions being 2003 and 2007,2010,2011 which are both post survey)
# That the years really correspond to the year before in our models.
# For example in 2004 we used catch from Jan-May 2004, because this is getting the biomass estimate at the start of this time series this is the
# Biomass in January of 2004 which should be more similar to the 2003 survey results b/c far less fishing has occured (in general) in the fall
# Then occurs between Jan-May on BBn (typically but not always)
mod.bm # This is the model biomass estimate in Kilo-tonnes for the 3 different methods.
windows(11,11)
with(mod.DL[[2017]] , plot(log(CPUE) ~ E, las = 1, pch = 19, main = "DeLury method",
                           xlab = "E(t)", ylab = "1 + log(C(t))", col = "blue"))
#mylmfit <- with(table1, lmfit)
lines(mod.fit[[count]]$x[, 2],  predict.lm(mod.fit[[count]]), col = "red", lty = "dashed")


# Now plot this by year...

p <- ggplot(mod.output, aes(effort,log(cpue))) +
  facet_wrap(~year,nrow=4)+  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  geom_point(color="blue") +                   # Now add the points
  geom_smooth(method = "lm") # This is the regression for each year, same thing as is done in the DeLury model.
windows(11,11); p

# Just the 2017 data...
p <- ggplot(mod.output[mod.output$year==2017,], aes(effort,log(cpue))) +
  geom_point(color="blue") +                   # Now add the points
  geom_smooth(method = "lm") # This is the regression for each year, same thing as is done in the DeLury model.
windows(11,11); p

# How does the DeLury q compare to the model fishery q?  Can see the Delury method gives a much smaller q.  I don't
# know if these are really directly comparable mind you..
DD.out$median$qU
mod.fit # The Et(i.e. the slope) is equivalent to the catchability.




##################################################  Now I want to do some sort of Yield per Recruit modelling  ################################################
##################################################  Now I want to do some sort of Yield per Recruit modelling  ################################################
##################################################  Now I want to do some sort of Yield per Recruit modelling  ################################################

# I should pretty easily be able to get a yield under different fishing and mortality scenarios.  Basically it is just running projections from the model
# with different parameter for growth and mortality, start with 1,000,000 85mm scallop, grow them under a suite of scenarios and we 
# have our yield per recruit, think in general something like this would be handy for us to have 

