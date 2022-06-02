#####This pulls and formats offshore scallop tow location data for the ShinySpatialApp #####
###Amy Glass May 2022####
## This saves the .RDATA file into the Y:/Offshore/Assessment folder within the function.  This will need to be
## updated when Quentin Stoyel assigns a location for data storage for the app
###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#  
#  convert.dd.dddd    
#
###############################################################################################################

###############################################################################################################
# ARGUMENTS
# X:  
# format: 

# source function ("Y:/Offshore/Assessment/Assessment_fns/Other_functions/RR_scaloff_pull.r")

###############################################################################################################

###############################################################################################################
# EXAMPLE:
#
# RR_scaloff_pull(un.ID="keyserf", pwd.ID="******", db.con = "ptran")
# load(savelocation)
# head(data) # 20383 obs, 10 variables.
#
###############################################################################################################

RR_scaloff_pull<-function(un.ID, pwd.ID, db.con) 
  
{
  require(ROracle)
  con= dbConnect(dbDriver("Oracle"), un.ID, pwd.ID, db.con) 
  #using SCALOFF.OSTOWS table because it only contains location data (no catch info)
  dat=dbGetQuery(con, ("Select TOW_NO, MGT_AREA_CD, TOW_DATE, START_LAT, START_LON 
                     from SCALOFF.OSTOWS"))
  library(dplyr)
  library(lubridate)
  
  #convert lats and longs to decimal degrees
  source("Y:/Offshore/Assessment/Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r")
  dat$sLat<-convert.dd.dddd(dat$START_LAT,format='dec.deg')
  dat$sLon<-convert.dd.dddd(dat$START_LON,format='dec.deg')
  
  #format column names
  names(dat)[2]<-"bank"
  #format date and separate y m d
  dat$TOW_DATE = as.factor(as.Date(dat$TOW_DATE, format = "%Y-%m-%d"))
  data<-dat %>% 
    mutate(TOW_DATE = ymd(TOW_DATE)) %>% 
    mutate_at(vars(TOW_DATE), list(year=year, month=month, day=day)) 
  
  #the output (OSdat.RData) is next called into the offshore_scallop_preprocessing.R script
  savelocation <- paste0("Y:/Offshore/Assessment/Data/Survey_data/", year(Sys.Date()), "/OSdat.RData")
  save(data, file = savelocation)
  print(paste0("Data is saved here: ", savelocation))
  
}
