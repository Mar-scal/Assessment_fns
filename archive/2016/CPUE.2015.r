# DK August 13, 2015 Commented and checked by DK.  This is a holding function for  to export appropriate Obs/Catch data.
# Note that the WF (wet fish) and FT (freezer trawlers) are calculated seperately.

####
################################################################################################################

#####################################  File Summary ########################################################
####  
##  Note that this isn't actually a function it is just a file from which to print the apropriate 
##  Catch.Effort.Tables and Obs.Trip.Effort results.
##  
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#  1:  get.marfis.logs.2015.r (not actually used in this file, needed in Catch.Offort.Tables.2015)
#  2:  ObsTripEffort.r
#  3:  Catch.Effort.Tables.2015.r
##
###############################################################################################################




### CHECK DATE FORMAT IN csv file!!!!!

source("Y:\\Fishery data\\r\\fn\\get.marfis.logs.2015.r")
source("Y:\\Fishery data\\r\\fn\\ObsTripEffort.R")
#source("Y:\\Fishery data\\r\\fn\\CatchRate.r")
source("Y:\\Fishery data\\r\\fn\\Catch.Effort.Tables.2015.r")

# These calls are for both types of boats in each NAFO region of Georges bank.  Files are exported as per Catch.Effort.Tables.[year]
# Can we automate this so there is only one version (not a new one every year/month...)
Catch.Effort.Tables (bank = "Georges", fleet = "FT", nafo.div=c("5ZEJ"),export= T)
Catch.Effort.Tables (bank = "Georges", fleet = "FT", nafo.div=c("5ZEM"),export= T)
Catch.Effort.Tables (bank = "Georges", fleet = "WF", nafo.div=c("5ZEJ"),export= T)
Catch.Effort.Tables (bank = "Georges", fleet = "WF", nafo.div=c("5ZEM"),export= T)


#January
# no fishing on GB
#February
ObsTripEffort (v=102056, land.date='2015-02-13')
#March
ObsTripEffort (v=105736, land.date='2015-03-10')
ObsTripEffort (v=106605, land.date='2015-04-08')
#April
ObsTripEffort (v=101965, land.date='2015-04-22')
ObsTripEffort (v=106604, land.date='2015-05-05')
#May
ObsTripEffort (v=105736, land.date='2015-06-03')
ObsTripEffort (v=1518, land.date='2015-05-27')

#June

#July

#August

#September

#October

#November  

#December


Catch.Effort.Tables( bank = "Georges a", fleet = "FT")
Catch.Effort.Tables( bank = "Georges a", fleet = "WF")

Catch.Effort.Tables( bank = "Georges b", fleet = "FT")
Catch.Effort.Tables( bank = "Georges b", fleet = "WF")

Catch.Effort.Tables( bank = "German", fleet = "FT")
Catch.Effort.Tables( bank = "German", fleet = "WF")

Catch.Effort.Tables( bank = "Browns North", fleet = "FT")
Catch.Effort.Tables( bank = "Browns North", fleet = "WF")

Catch.Effort.Tables( bank = "Browns South", fleet = "FT")
Catch.Effort.Tables( bank = "Browns South", fleet = "WF")

Catch.Effort.Tables( bank = "Sable", nafo.div=c("4WH", "4WJ", "4WF", "4WL"), fleet = "FT")
Catch.Effort.Tables( bank = "Sable", nafo.div=c("4WH", "4WJ", "4WF", "4WL"), fleet = "WF")


Catch.Effort.Tables( bank = "Middle", nafo.div=c("4WE"), fleet = "FT")
Catch.Effort.Tables( bank = "Middle", nafo.div=c("4WE"), fleet = "WF")

Catch.Effort.Tables( bank = "Banquereau", fleet = "FT")
Catch.Effort.Tables( bank = "Banquereau", fleet = "WF")





