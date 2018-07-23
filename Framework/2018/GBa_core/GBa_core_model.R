##################################################   Look at difference between GBa core fishery and non-core regions.   #########################################################		
##################################################    Look at difference between GBa core fishery and non-core regions. #########################################################		
##################################################    Look at difference between GBa core fishery and non-core regions.#########################################################		

# Now we can look at the core of GBa vs. some less important parts of the bank...

yr = as.numeric(format(Sys.time(), "%Y")) -1 # 
direct = "d:/r/"
library(RColorBrewer)
library(PBSmapping)
library(R2jags)
library(VGAMdata)
library(ggplot2)
library(tidyverse)
library(reshape2)
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
# I should be adding this to our in house function list, this allows us to add transparency to points we plot easily...
addalpha <- function(colors, alpha=1.0) 
{
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

#Read1 Bring in the VonB model parameters
cat("We read in the von B growth parameters from the file .../Data/Ageing/Von_B_growth_parameters.csv")
vonB <- read.csv(paste(direct,"Data/Ageing/Von_B_growth_parameters.csv",sep=""))

# Get the fishery data for everywhere...
logs_and_fish(loc="offshore",year = 1984:yr,un=un,pw=pwd,db.con=db.con,direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

# Get rid of the "Boot" seedbox from the list as we don't have the coordinates for it and it's causing problems...
seedboxes <- seedboxes[seedboxes$ID != "Boot",]

# GBa divided into quadrents...
GBa.quads <- read.csv(paste(direct,"Data/Framework/2018/GBa_quadrents.csv",sep=""),stringsAsFactors = F)


# Make the names for surveys a little cleaner...
GBa.live <- surv.Rand[["GBa"]]
GBa.clap <- surv.Clap.Rand[["GBa"]]

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

# This will pull on end location, this will get all tows that were at any point inside the box...
GBa.live$EID<-1:nrow(GBa.live)
GBa.live$X<-GBa.live$elon
GBa.live$Y<-GBa.live$elat
GBa.live$Strata_ID <- 1
# Need the clappers too...
GBa.clap$EID<-1:nrow(GBa.clap)
GBa.clap$X<-GBa.clap$elon
GBa.clap$Y<-GBa.clap$elat
GBa.clap$Strata_ID <- 1


# Now put the GBa, what is defined as core depends on what GBa.core is, so look in GBa.quads to see what I've chosen here...
GBa.core <- GBa.quads[GBa.quads$ID == "Large_core",]
GBa.core <- as.PolySet(GBa.core,projection="LL")
GBa.core.poly <- Polygons(list( Polygon(cbind(GBa.core$X,GBa.core$Y))),"GBa.core")
GBa.core.poly.sp <-SpatialPolygons(list(GBa.core.poly))
GBa.core.X.range <- range(GBa.core.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,1])
GBa.core.Y.range <- range(GBa.core.poly.sp@polygons[[1]]@Polygons[[1]]@coords[,2])
# Now plot the survey stations by year.
coordinates(GBa.live) <- ~slon+slat
coordinates(GBa.fish.dat) <- ~lon+lat
# Get the points within the core region
GBa.core.pts <- over(GBa.live,GBa.core.poly.sp)
GBa.core.fish.pts <- over(GBa.fish.dat,GBa.core.poly.sp)
GBa.core.surv <- GBa.live[which(GBa.core.pts==1),]
GBa.core.clap <- GBa.clap[which(GBa.core.pts==1),]
GBa.core.fish <- GBa.fish.dat[which(GBa.core.fish.pts==1),]
GBa.non.core.surv <- GBa.live[which(is.na(GBa.core.pts)),]
GBa.non.core.clap <- GBa.clap[which(is.na(GBa.core.pts)),]
GBa.non.core.fish <- GBa.fish.dat[which(is.na(GBa.core.fish.pts)),]
# Now we can make the survey object...


# Now to get the area I need to clip the "core area" to the survey strata, this might not be pretty.... look at it tomorrow....
GBa.bound <- survey.bound.polys[survey.bound.polys$label == "GBa",]
GBa.detail <- as.PolySet(survey.detail.polys[survey.detail.polys$label == "GBa",],projection="LL")
GBa.core.bound <- joinPolys(GBa.bound,GBa.core,"INT")
GBa.core.detail <- joinPolys(GBa.detail,GBa.core,"INT")
GBa.non.core.detail <- joinPolys(GBa.detail,GBa.core,"DIFF")
core.area <- calcArea(GBa.core.detail,rollup=1)
core.area$PID <- 1:nrow(core.area)
core.area$towable_area <- core.area$area*1000*1000/800/(8/3.2808)
non.core.area <- calcArea(GBa.non.core.detail,rollup=1)
non.core.area$PID <- 1:nrow(non.core.area)
non.core.area$towable_area <- non.core.area$area*1000*1000/800/(8/3.2808)



core.obj <- survey.dat(GBa.core.surv@data, RS=RS, CS=CS, bk="GBa", areas=core.area[,c(1,3)], mw.par="CF",err="str")	
core.obj[[1]]$CF <- na.omit(sapply(1:length(unique(GBa.core.surv@data$year)), function(x){with(subset(GBa.core.surv@data,year == unique(GBa.core.surv@data$year)[x]), weighted.mean(CF,com.bm,na.rm=T))}))
core.clap.obj <- survey.dat(GBa.core.clap, RS=RS, CS=CS, bk="GBa", areas=core.area[,c(1,3)], mw.par="CF",err="ran")	
core.obj[[1]]$clappers<-core.clap.obj[[1]]$N
core.obj[[1]]$clappersR<-core.clap.obj[[1]]$NR
core.obj[[1]]$CS <- CS
core.obj[[1]]$RS <- RS
core.obj[[1]]$year <- as.numeric(core.obj[[1]]$year)

non.core.obj <- survey.dat(GBa.non.core.surv@data, RS=RS, CS=CS, bk="GBa", areas=non.core.area[,c(1,3)], mw.par="CF",err="str")	
non.core.obj[[1]]$CF <- na.omit(sapply(1:length(unique(GBa.non.core.surv@data$year)), function(x){with(subset(GBa.non.core.surv@data,year == unique(GBa.non.core.surv@data$year)[x]), weighted.mean(CF,com.bm,na.rm=T))}))
non.core.clap.obj <- survey.dat(GBa.non.core.clap, RS=RS, CS=CS, bk="GBa", areas=non.core.area[,c(1,3)], mw.par="CF",err="ran")	
non.core.obj[[1]]$clappers<-non.core.clap.obj[[1]]$N
non.core.obj[[1]]$clappersR<-non.core.clap.obj[[1]]$NR
non.core.obj[[1]]$CS <- CS
non.core.obj[[1]]$RS <- RS
non.core.obj[[1]]$year <- as.numeric(non.core.obj[[1]]$year)



# It wouldn't hurt to have a plot of this "core" area...
windows(11,11)
ScallopMap("GBa",plot.boundries = T,plot.bathy = T)
points(GBa.fish.dat,cex=0.1,col = addalpha("blue",0.1))
addPolys(seedboxes[seedboxes$ID == "C4-122016",],border="red",lty=2,lwd=3)
addPolys(seedboxes[seedboxes$ID == "C5-122016",],border="chartreuse3",lty=1,lwd=3)
addPolys(seedboxes[seedboxes$ID == "Peanut (2010)",],border="red",lty=2,lwd=3)
addPolys(GBa.core,lwd=2)
addPolys(survey.bound.polys[survey.bound.polys$label == "GBa",])

##########################  THE Core MODEL data input #######################################  THE GBa core MODEL #######################################  THE GBa core MODEL #############
##########################  THE GBa core MODEL #######################################  THE GBa core MODEL #######################################  THE GBa core MODEL #############
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
# 
# Run this for one or both banks
# Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
years <- min(core.obj[[1]]$year):max(core.obj[[1]]$year)
# Bring in the vonB parameters..
vonB.par <-vonB[vonB$Bank =="GBa",]
# Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
cpue.dat <- fishery.dat(GBa.core.fish@data,bk="GBa",yr=c(1984:2017),method='jackknife',surv='August',
                        direct=direct,period = "survyr") 	
# Now on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
#for all other years we need to do this for Browns Bank North
# It really makes very little difference which way this is done as the catch in June-August
# has averaged around just 40 tonnes since about 1996.
if(any(is.na(cpue.dat))) cpue.dat[is.na(cpue.dat)] <- 0 # Set any NA's to 0. 
# Combine the survey and Fishery data here.
mod.dat.core <- merge(core.obj[[1]],cpue.dat,by ="year")
#mod.dat.core$year <- as.numeric(levels(mod.dat.core$year))[mod.dat.core$year]
# DK Note For years in which we have essentially no fishing effort on the bank we need to blow up the variance of those estimates.
# If < 20 tonnes assume low effort and blow up the CV to 1.
#mod.dat.core$cpue.se[mod.dat.core$catch < 20] <- mod.dat.core$cpue[mod.dat.core$catch < 20]
# Get the CV for the CPUE...
mod.dat.core$U.cv <- mod.dat.core$cpue.se/mod.dat.core$cpue
mod.dat.core$U.cv[is.nan(mod.dat.core$U.cv)] <- 1 # Doesn't really matter since CPUE =0

# now get the catch data from end of survey until end of the year for the projection
proj.sub <- subset(GBa.core.fish@data,year %in% years & months(as.Date(GBa.core.fish@data$date)) %in% c("September","October","November","December"))
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
waa.tm1 <- mod.dat.core$CF*(mod.dat.core$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat.core$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat.core$CF[-1],mod.dat.core$CF[nrow(mod.dat.core)])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
waa.t2 <- mod.dat.core$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat.core$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat.core$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat.core$CF*(mod.dat.core$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat.core$l.k
waa.t <- c(mod.dat.core$CF[-1],mod.dat.core$CF[nrow(mod.dat.core)])*(laa.t/100)^3
waa.t2 <- mod.dat.core$CF*(laa.t/100)^3
mod.dat.core$gR <- waa.t/waa.tm1
mod.dat.core$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
mod.dat.core$U.cv[is.nan(mod.dat.core$U.cv)] <- median(mod.dat.core$U.cv,na.rm=T)

# fix up any 0 data that can't be 0...
mod.dat.core$U.cv[mod.dat.core$U.cv ==0] <- median(mod.dat.core$U.cv,na.rm=T)
mod.dat.core$cpue[mod.dat.core$cpue == 0] <- median(mod.dat.core$cpue,na.rm=T)
mod.dat.core$clappers[mod.dat.core$clappers == 0] <- median(mod.dat.core$ clappers,na.rm=T)
mod.dat.core$clappersR[mod.dat.core$clappersR == 0] <- median(mod.dat.core$clappersR,na.rm=T)





# DK Note, more refining, blow up the survey variance in years we don't have sufficient tows...
#mod.dat.core$I.cv[mod.dat.core$n.x < 5] <- 1
#mod.dat.core$IR.cv[mod.dat.core$n.x < 5] <- 1
#mod.dat.core$U.cv[mod.dat.core$n.x >= 5] <- 1 # Downweight the CPUE data when we have good survey data...
# Now for the model...

# Set the working directory for figures and tables to be output
plotsGo <- paste(direct,"2018/Framework/GBa_core/Figures_and_Tables/core/",sep="")
#Read2 Get the managment data, note this file needs updated annually! 
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Grab the data, start model at 1986
DD.dat <- subset(mod.dat.core,year %in% 1986:2017)

# End the data inputs for the "core" model.

################################################################################################################################################################################
###################### Now the data inputs if you want to run the Non.core model ########################################################################################
###################### Now the data inputs if you want to run the Non.core model...########################################################################################..
###################### Now the data inputs if you want to run the Non.core model....########################################################################################.
################################################################################################################################################################################

#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
# 
# Run this for one or both banks
# Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
years <- min(non.core.obj[[1]]$year):max(non.core.obj[[1]]$year)
# Bring in the vonB parameters..
vonB.par <-vonB[vonB$Bank =="GBa",]
# Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
cpue.dat <- fishery.dat(GBa.non.core.fish@data,bk="GBa",yr=c(1984:2017),method='jackknife',surv='August',
                        direct=direct,period = "survyr") 	
# Now on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
#for all other years we need to do this for Browns Bank North
# It really makes very little difference which way this is done as the catch in June-August
# has averaged around just 40 tonnes since about 1996.
if(any(is.na(cpue.dat))) cpue.dat[is.na(cpue.dat)] <- 0 # Set any NA's to 0. 
# Combine the survey and Fishery data here.
mod.dat.non.core <- merge(non.core.obj[[1]],cpue.dat,by ="year")
#mod.dat.core$year <- as.numeric(levels(mod.dat.core$year))[mod.dat.core$year]
# DK Note For years in which we have essentially no fishing effort on the bank we need to blow up the variance of those estimates.
# If < 20 tonnes assume low effort and blow up the CV to 1.
#mod.dat.core$cpue.se[mod.dat.core$catch < 20] <- mod.dat.core$cpue[mod.dat.core$catch < 20]
# Get the CV for the CPUE...
mod.dat.non.core$U.cv <- mod.dat.non.core$cpue.se/mod.dat.non.core$cpue
mod.dat.non.core$U.cv[is.nan(mod.dat.non.core$U.cv)] <- 1 # Doesn't really matter since CPUE =0

# now get the catch data from end of survey until end of the year for the projection
proj.sub <- subset(GBa.non.core.fish@data,year %in% years & months(as.Date(GBa.non.core.fish@data$date)) %in% c("September","October","November","December"))
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
waa.tm1 <- mod.dat.non.core$CF*(mod.dat.non.core$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat.non.core$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat.non.core$CF[-1],mod.dat.non.core$CF[nrow(mod.dat.non.core)])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
waa.t2 <- mod.dat.non.core$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat.non.core$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat.non.core$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat.non.core$CF*(mod.dat.non.core$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat.non.core$l.k
waa.t <- c(mod.dat.non.core$CF[-1],mod.dat.non.core$CF[nrow(mod.dat.non.core)])*(laa.t/100)^3
waa.t2 <- mod.dat.non.core$CF*(laa.t/100)^3
mod.dat.non.core$gR <- waa.t/waa.tm1
mod.dat.non.core$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
mod.dat.non.core$U.cv[is.nan(mod.dat.non.core$U.cv)] <- median(mod.dat.non.core$U.cv,na.rm=T)

# fix up any 0 data that can't be 0...
mod.dat.non.core$U.cv[mod.dat.non.core$U.cv ==0] <- median(mod.dat.non.core$U.cv,na.rm=T)
mod.dat.non.core$cpue[mod.dat.non.core$cpue == 0] <- median(mod.dat.non.core$cpue,na.rm=T)
mod.dat.non.core$clappers[mod.dat.non.core$clappers == 0] <- median(mod.dat.non.core$ clappers,na.rm=T)
mod.dat.non.core$clappersR[mod.dat.non.core$clappersR == 0] <- median(mod.dat.non.core$clappersR,na.rm=T)





# DK Note, more refining, blow up the survey variance in years we don't have sufficient tows...
#mod.dat.non.core$I.cv[mod.dat.non.core$n.x < 5] <- 1
#mod.dat.non.core$IR.cv[mod.dat.non.core$n.x < 5] <- 1
#mod.dat.non.core$U.cv[mod.dat.non.core$n.x >= 5] <- 1 # Downweight the CPUE data when we have good survey data...
# Now for the model...

# Set the working directory for figures and tables to be output
plotsGo <- paste(direct,"2018/Framework/GBa_core/Figures_and_Tables/non_core/",sep="")
#Read2 Get the managment data, note this file needs updated annually! 
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Grab the data, start model at 1986
DD.dat <- subset(mod.dat.non.core,year %in% 1986:2017)

###################  End the non core model data input ############################################################################################




###################### Run the core or non-core model ###################### Run the core or non-core model ###################### Run the core or non-core model 
###################### Run the core or non-core model ###################### Run the core or non-core model ###################### Run the core or non-core model 
###################### Run the core or non-core model ###################### Run the core or non-core model ###################### Run the core or non-core model 

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
D_high <-  6000
# The increment size for the decision table.  500 for GBa and 50 for GBa
step <- 100
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
exploit.plt(DD.out, years=yrs, plt=c('f',"m","mR"),graphic="pdf",path=plotsGo)

#dev.off()
# model biomass fit to survey
fit.plt(DD.out, years = yrs, CI=T,graphic="pdf",path=plotsGo,CV=T)
# diagnostic plot
diag.plt(DD.out, years = yrs,graphic="pdf",path=plotsGo)

# and our big old biomass plot
biomass.plt(DD.out,years=yrs, graphic="pdf",TAC=TACi+proj.catch,path=plotsGo,refs=NULL,pred=1)

# Some plots....
plot(DD.out$mean$B~DD.out$mean$m,type="n")
text(DD.out$mean$B~DD.out$mean$m,labels=1995:2016,cex=0.65)

# Do a quick CCF sniff to see if anything interesting, nothing really, time series likely too short to be honest.
ccf(DD.out$mean$B,DD.out$mean$m)
ccf(DD.out$mean$R,DD.out$mean$m)
ccf(DD.out$mean$B,DD.out$mean$R) 
#ccf(mod.dat.core$I,mod.dat.core$IR) 
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

save.image(paste(direct,"Data/Framework/2018/Gba_core/non_core/non_core_results.RData",sep=""))
#save.image(paste(direct,"Data/Framework/2018/Gba_core/core_results.RData",sep=""))
#load(paste(direct,"Data/Framework/2018/GBa_core/core_results.RData",sep=""))
#load(paste(direct,"Data/Framework/2018/GBa_core/non_core/non_core_results.RData",sep=""))

############# Do the prediction evalulations if I so desire...

pe.years <- max(yrs):2004
#Prediction Evaluation using the current year CF
res <- pred.eval(DD.lst, DD.out$priors, pe.years= pe.years, growth="modelled",model = model.jags,  bank="GBa",parameters = DD.out$parameters,
          niter = 200000,nburn = 125000, nthin = 10,nchains=10,direct=direct,save.res = paste0(direct,"Data/Framework/2018/Gba_core/non_core/"))
out <- res$PE.modelled
# I can load in the results from here rather than running the prediction evaluation too many times...
#load(paste(direct,"Data/Framework/2018/Gba_core/Projection_evaluation_modelled_growth.RData",sep=""))

# Now we make the figures and save them...
pe.fig(input =out, years=2005:2017,growth="modelled",graphic = "screen",direct= direct,bank = "GBa",plot="box",path=plotsGo)


# Comparing the core, non-core, and southern model exploitation rates....
load(paste(direct,"Data/Framework/2018/GBa_core/non_core/non_core_results.RData",sep=""))
nc.out <- DD.out
non.core.exploit <- as.data.frame(DD.out$sims.list$mu)
non.core.pe <- as.data.frame(DD.out$sims.list$Presid)
non.core.m <- as.data.frame(DD.out$sims.list$m)
names(non.core.exploit) <- yrs
names(non.core.pe) <- yrs
names(non.core.m) <- yrs
head(non.core.exploit)
non.core.exploit <- melt(non.core.exploit,value.name = "exploit",variable.name = "year")
non.core.pe <- melt(non.core.pe,value.name = "process_e",variable.name = "year")
non.core.m <- melt(non.core.m,value.name = "mort",variable.name = "year")

non.core.dat <- data.frame(year = non.core.exploit$year,exploit = non.core.exploit$exploit,process_e = non.core.pe$process_e,mort = non.core.m$mort,region = "Non Core")



# Now grab the core results
load(paste(direct,"Data/Framework/2018/GBa_core/core_results.RData",sep=""))
core.out <- DD.out
core.exploit <- as.data.frame(DD.out$sims.list$mu)
core.pe <- as.data.frame(DD.out$sims.list$Presid)
core.m <- as.data.frame(DD.out$sims.list$m)
names(core.exploit) <- yrs
names(core.pe) <- yrs
names(core.m) <- yrs
head(core.exploit)
core.exploit <- melt(core.exploit,value.name = "exploit",variable.name = "year")
core.pe <- melt(core.pe,value.name = "process_e",variable.name = "year")
core.m <- melt(core.m,value.name = "mort",variable.name = "year")

core.dat <- data.frame(year = core.exploit$year,exploit = core.exploit$exploit,process_e = core.pe$process_e,mort = core.m$mort,region = "Core")


# Finally get the Southern region exploitation
# load(paste(direct,"Data/Framework/2018/GBa_core/South_results.RData",sep=""))
# south.exploit <- as.data.frame(DD.out$sims.list$mu)
# names(south.exploit) <- yrs
# head(south.exploit)
# south.exploit <- melt(south.exploit,value.name = "exploit",variable.name = "year")
# south.exploit$region <- "South"

dat.by.region <- rbind(core.dat,non.core.dat)#,south.exploit)
dat.by.region$mort <- 1-exp(-dat.by.region$mort)
dat.by.region$year <- as.numeric(levels(dat.by.region$year)[dat.by.region$year])
median.by.region <- aggregate(cbind(exploit,mort,process_e)~year + region,dat.by.region,median)

median.by.region$period <- "Pre 2004"
median.by.region$period[median.by.region$year %in% 2004:2089] <- "2004-2008"
median.by.region$period[median.by.region$year >= 2009] <- "2009-2017"
median.by.region$yr = substr(median.by.region$year,3,4)

windows(11,11)
ggplot(median.by.region, aes(year,exploit,colour=region)) + geom_line(size=1) + geom_point() + scale_colour_manual(values = c("blue","grey")) +
   theme_bw(base_size = 24) + theme(panel.grid=element_blank()) +xlab("") + ylab("Expolitation rate")+ scale_x_continuous(breaks = seq(1985,2015,by=5))
ggsave(paste0(direct,"2018/Framework/GBa_core/Figures_and_Tables/Comparing_exploitation_core_non_core.png"),width=11,height=8,units="in")
# Compare initial and final exploitation rates...
initial.exp.by.region <- aggregate(exploit~region,median.exploit.by.region[median.exploit.by.region$year < 1995,],mean)
recent.exp.by.region <- aggregate(exploit~region,median.exploit.by.region[median.exploit.by.region$year >1999,],mean)
(recent.exp.by.region$exploit -initial.exp.by.region$exploit)/initial.exp.by.region$exploit


# Here I plot the Process error against the fully recruited natural mortality term.

windows(11,11)
ggplot(median.by.region, aes(mort,process_e,label = yr,colour=period)) + geom_text(show.legend = FALSE,size=4) + facet_wrap(~region)+
  theme_bw(base_size = 24) + theme(panel.grid=element_blank()) +xlab("natural mortality") + ylab("Process error")+
geom_hline(data=all.dat, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Process error") + xlab("FR Natural Mortality")+ #theme(legend.position="none") + 
  scale_color_manual(values = c("black","blue","grey"))#+
ggsave(paste0(direct,"2018/Framework/GBa_core/Figures_and_Tables/PE_vs_nat_mort_core_non_core.png"),width=11,height=8,units="in")

## Here I compare the non core with the core data, looking at biomass trends in the time series...
load(paste(direct,"Data/Framework/2018/GBa_core/non_core/non_core_results.RData",sep=""))
nc.out <- DD.out
load(paste(direct,"Data/Framework/2018/GBa_core/core_results.RData",sep=""))




# The proportion of Biomass in the Non-Core region...
nc.out$median$B/(nc.out$median$B+core.out$median$B)
mean(nc.out$median$B/(nc.out$median$B+DD.out$median$B))
# Last 10 year average
mean((nc.out$median$B/(nc.out$median$B+DD.out$median$B))[15:31])
mean((nc.out$median$B/(nc.out$median$B+DD.out$median$B))[1:10])