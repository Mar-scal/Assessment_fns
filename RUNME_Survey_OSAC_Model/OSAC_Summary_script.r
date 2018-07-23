#######################################################################################################################################  
############## The script will be used to create summary information for the OSAC presentation                       ######################## 
#######################################################################################################################################  
## Created by DK April 2016
## Update history
## May 16, 2016:  Revised to work with new OSAC_summary function, I tested the function for 1 afternoon... it needs more testing!

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  
##
###############################################################################################################

###############################################################################################################
## This script needs these functions to work (a.k.a. "support files")
# 1: source(paste(direct,"Assessment_fns/Survey/OSAC_summary.r",sep=""))
# 
###############################################################################################################

# Load your directory and the survey year
direct <- "d:/r/"
# direct <- "Y:/Offshore scallop/Assessment/"
yr <- 2017

# Load the function...
source(paste(direct,"Assessment_fns/Survey_and_OSAC/OSAC_summary.r",sep = ""))
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep = ""))

# This will take more than 10 minutes when using GBa as it takes a while to do the jackknife for the CPUE on GBa.
# Note that this will lead to an error if there was no fishing on a bank in a given year so make sure you remove any
# banks that don't have fishery data for the current year!
OSAC_summary(direct = direct,un=un.ID,pw=pwd.ID,db.con="ptran64",yr=2017,
             bank = c("BBs","Ger","GBa","GBb","BBn","Mid","Sab","Ban","SPB"),
             save.fig = T,save.res=T,export=T)


OSAC_summary(direct = direct,un=un.ID,pw=pwd.ID,db.con="ptran64",yr=2014,
             bank = c("GBb"),save.fig = F,save.res=F,export=F)

# Get rid of some clutter...
rm("fleet_data","new.log.dat","old.log.dat","slip.dat")
# And the data is all summarized in the OSAC_res object
OSAC_res

#load(paste(direct,"Data/Fishery_data/Summary/2017/OSAC_summary.RData",sep = ""))
sum.stat

length(which(OSAC_res$fish.cells$catch >=1))

tail(survey.obj$GBb$model.dat)

# Get the number of cells > some value for each bank...
length(which(fish.cells$catch[fish.cells$bank== "Sab"] >=1))
length(which(fish.cells$catch[fish.cells$bank== "Ger"] >=1))
length(which(fish.cells$catch[fish.cells$bank== "BBs"] >=1))
length(which(fish.cells$catch[fish.cells$bank== "SPB"] >=1))

length(which(fish.cells$catch[fish.cells$bank== "BBn"] >=10))
length(which(fish.cells$catch[fish.cells$bank== "GBa"] >=10))

# Here's a rough estimate of the area fished for each bank, based on 1 minute by 1 minute cells having an area of 2.6 km^2 throughout the region
# Which is not perfect, but is pretty close (assumes 1 minute of longitude = 1.4 km, and 1 minute of latitude = 1.85 km) for our region.
# Only works for the banks we have some survey strata...
area <- 1.4 * 1.85
surv.info <- read.csv(paste(direct,"Data/Survey_data/survey_information.csv",sep=""))

bnk.areas <- aggregate(area_km2 ~ label, surv.info,FUN=sum)

100 * length(fish.cells$catch[fish.cells$bank== "Sab"]) * area / bnk.areas$area_km2[bnk.areas$label == "Sab"]
100 * length(fish.cells$catch[fish.cells$bank== "GBa"]) * area / bnk.areas$area_km2[bnk.areas$label == "GBa"]
100 * length(fish.cells$catch[fish.cells$bank== "BBs"]) * area / bnk.areas$area_km2[bnk.areas$label == "BBs"]
100 * length(fish.cells$catch[fish.cells$bank== "BBn"]) * area / bnk.areas$area_km2[bnk.areas$label == "BBn"]
100 * length(fish.cells$catch[fish.cells$bank== "GBb"]) * area / bnk.areas$area_km2[bnk.areas$label == "GBb"]

100 * 18 * area / bnk.areas$area_km2[bnk.areas$label == "BBn"]

sum.stat
fish.res

head(new.log.dat)

# Here I can get a quick summary of the landings by area using the logs_and_fish function...
logs_and_fish(loc = "offshore",year=2014:2017,export=F,get.marfis = F,ex.marfis = F,
                          un=un.ID,pw=pwd.ID,db.con="ptran64",direct.off=direct)
fish.dat <- new.log.dat
# Or do this if you have older data in here...
#fish.dat<-merge(new.log.dat,old.log.dat,all=T)

aggregate(pro.repwt ~ year + sfa,fish.dat,sum)