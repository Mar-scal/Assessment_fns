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

direct = "d:/r/"
direct = "Y:/Offshore scallop/Assessment/"
source(paste(direct,"Assessment_fns/Survey_design/Survey_design.r",sep=""))

# Run the survey design, pick your year, bank(s) and other options to create the survey design for a given year.
Survey.design(yr = 2017,banks = "Ger",direct = direct,export=T,relief.plots = T,fig="png",seed=(yr-2000))
