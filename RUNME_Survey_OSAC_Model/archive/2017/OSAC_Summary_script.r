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
# direct <- "Y:/Offshore scallop/Assessment"
yr <- 2015

# Load the function...
source(paste(direct,"Assessment_fns/Survey_and_OSAC/OSAC_summary.r",sep = ""))

# This will take more than 10 minutes when using GBa as it takes a while to do the jackknife for the CPUE on GBa.
OSAC_summary(direct = direct,un=un.ID,pw=pwd.ID,db.con="ptran64",yr=2012,save.res = F,bank="Mid")
# Get rid of some clutter...
rm("fleet_data","new.log.dat","old.log.dat","slip.dat")
# And the data is all summarized in the OSAC_res object
OSAC_res

