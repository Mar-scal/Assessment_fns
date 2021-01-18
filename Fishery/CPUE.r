# DK August 13, 2015 Commented and checked by DK.  This is just the script where the 
# CPUE by month in a given area or by observer trip is calculated, now using the function 
# Updates
# May 16, 2016:  Added note regarding accessing database using 64 bit version of R.

####
################################################################################################################

#####################################  File Summary ########################################################
####  
##  Note that this isn't actually a function it is just a file from which to print the apropriate 
##  CPUE.mon and Obs.Trip.Effort results.
##  
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#  1:  logs_and_fishery_data_DK (not actually used in this file, needed in Catch.Offort.Tables.2015)
#  2:  CPUE_monthly_or_observer
##
###############################################################################################################
direct = "d:/R/" 

# This will grab the latest version of this function from github...
funs <- c("https://raw.githubusercontent.com/Dave-Keith/Assessment_fns/master/Fishery/CPUE_monthly_or_observer.R")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} # end for(fun in funs) 


#source(paste(direct_fns,("Fishery/FishMonth.r"),sep=""))

# Sample CPUE.mon calls...
# Note if running 64 bit you'll need a new db.con name (DK don't forget yours is called ptran64)
CPUE.mon(boxes="GB",year=1987:2015,bank="GB",fleet="FT",export.logs=F,export.tables=T,
         un=un.ID,pw=pwd.ID,db.con="ptran64",direct=direct, direct_fns=direct_fns)

CPUE.mon(bank = "GBa", fleet = "FT",un=un.ID,pw=pwd.ID,db.con="ptran",boxes="ALL",year=2015,months =c(1:12),direct=direct, direct_fns=direct_fns)
CPUE.seedboxes.monthly

# These will grab the 2015 data for these banks...
CPUE.mon( bank = "GBa", fleet = "FT",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon( bank = "GBa", fleet = "WF",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon( bank = "GBa", fleet = "ALL",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)

CPUE.mon( bank = "GBb", fleet = "FT",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon( bank = "GBb", fleet = "WF",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)

CPUE.mon( bank = "Ger", fleet = "FT",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns,year=2015,export.logs=T,export.tables = T)
CPUE.mon( bank = "Ger", fleet = "WF",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)

CPUE.mon( bank = "BBn", fleet = "FT",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon( bank = "BBn", fleet = "WF",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon( bank = "BBn", fleet = "ALL",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)

CPUE.mon( bank = "BBs", fleet = "FT",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon( bank = "BBs", fleet = "WF",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)

CPUE.mon( bank = "Sab", nafo.div=c("4WH", "4WJ", "4WF", "4WL"), fleet = "FT",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon( bank = "Sab", nafo.div=c("4WH", "4WJ", "4WF", "4WL"), fleet = "WF",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)


CPUE.mon( bank = "Mid", nafo.div=c("4WE"), fleet = "FT",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon( bank = "Mid", nafo.div=c("4WE"), fleet = "WF",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)

CPUE.mon( bank = "Ban", fleet = "FT",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon( bank = "Ban", fleet = "WF",un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)

# To get observer data CPUE from 2012-2015

CPUE.mon(CPUE="obs",year=2012:2015,un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
obs.data

# For a specifice trip is it (ensure the year includes the trip of interest.)
CPUE.mon(CPUE="obs",year=2012:2015,obs.vnum = 1518,obs.land.date = '2014-04-11',un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
CPUE.mon(CPUE="obs",year=2012:2015,obs.vnum = 1518,obs.land.date = '2014-04-11',un=un.ID,pw=pwd.ID,db.con="ptran",direct=direct, direct_fns=direct_fns)
obs.data

