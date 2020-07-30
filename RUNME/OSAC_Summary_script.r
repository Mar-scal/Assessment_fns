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
yr <- 2016

# Load the function...
source(paste(direct,"Assessment_fns/Survey_and_OSAC/OSAC_summary.r",sep = ""))

# This will take more than 10 minutes when using GBa as it takes a while to do the jackknife for the CPUE on GBa.
# Note that this will lead to an error if there was no fishing on a bank in a given year so make sure you remove any
# banks that don't have fishery data for the current year!
OSAC_summary(direct = direct,yr=2018,
             bank = "all",
             save.fig = T,save.res=T,export=T)

OSAC_summary(direct = direct,yr=2018,
             bank = "BBn",
             save.fig = F,save.res=F,export=F)

# Get rid of some clutter...
rm("fleet_data","new.log.dat","old.log.dat","slip.dat")
# And the data is all summarized in the OSAC_res object
OSAC_res

#load(paste(direct,"Data/Fishery_data/Summary/2016/OSAC_summary.RData",sep = ""))

length(which(OSAC_res$fish.cells$catch >=1))

