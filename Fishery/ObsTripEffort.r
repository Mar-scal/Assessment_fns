# DK August 27, 2015 Commented and checked by DK.  This function calculates the effort of specific vessels
# and trip data, this currently works only with offshore data, if we updated the inshore fleet vessel data
# we could also do the inshore. This was originally created to calculate fishing effort (h/trip) on observed trips
# in Georges by-catch estimation, it could easily be expanded to be a far more useful function...
#		

####
################################################################################################################

#####################################  File Summary ########################################################
####  
##  This function is used in these files (a.k.a. 'dependent files') 
##  1:  CPUE_2015
##  
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#  1:  logs_and_fishery_data
##
###############################################################################################################

# Arguments

# ves.ID:     This accepts either the vessel name (IN ALL CAPS) or the vessel number
# land.date:  The date of landing, please enter as YYYY-MM-DD (e.g. 2016-02-25)
# un:         your SQL username.  default = un.ID (if set in your r.Profile this will run automatically)
# pw:         Your SQL password.  default = pwd.ID  (if set in your r.Profile this will run automatically)
# db.con:     Database to connect to.  Default is  "ptran"   
#direct:      Directory to find the functions.  Default is "Y:/Offshore scallop/Assessment/Assessment_fns/")


#source("Y:\\Fishery data\\r\\fn\\ObsTripEffort.R")

ObsTripEffort <- function(ves.ID, land.date,un=un.ID,pw=pw.ID,db.con="ptran")
{
   require(RODBC) || stop("This won't work unless you install the RODBC package!")

  #
  
  # get log data. 
  source(paste(direct,"logs_and_fishery_data.r",sep=""))
  
  # Get the offshore log information for the current year. 
  logs_and_fish(loc="offshore",un=un,pw=pw,db.con=db.con)

  # logs and fish now returns a vessel ID for the offshore (for data from 2008 - present)
  if(length(which(fleet_data$ID == ves.ID)) > 0) 
    {
      ves <- as.character(fleet_data$Vessel[fleet_data$ID == ves.ID])
      v <- ves.ID
    } # end if(length(which(fleet_data$ID == ves.ID)) > 0)
  if(length(which(fleet_data$Vessel == ves.ID)) > 0) 
    {
      ves <- ves.ID
      v <- as.numeric(fleet_data$ID[fleet_data$Vessel == ves.ID])
    } # if(length(which(fleet_data$Vessel == ves.ID)) > 0)
  
  # If you've entered an invalid name/number send back an error. 
  if(length(which(fleet_data$ID == ves.ID)) == 0 && 
     length(which(fleet_data$Vessel == ves.ID)) == 0) stop("Vessel ID not valid, please check your ves.ID")
  

slip <- subset(slip.dat, date.land == as.Date(land.date) & vrnum == v)

# Send back an error as we have variable gear in this slip!
if(length(unique(slip$gear.ft))>1) stop("More than one unique gear in slip")

# Now grab the appropriate log data 
log <- subset(new.log.dat, mdid == unique(slip$mdid))


# Now we can calculate the how fished on the banks for a given vessel.
hours <- NULL
hours[1] <- sum(with(subset(log, nafo == '5ZEJ'), numtow * avgtime) / 60)
hours[2] <- sum(with(subset(log, nafo == '5ZEM'), numtow * avgtime) / 60)
hours[3] <- sum(hours)
names(hours) <- c("5Zej", "5Zem", "total")

hourmetres <- c()
hourmetres[1] <- sum(with(subset(log, nafo == '5ZEJ'), numtow * avgtime / 60 * numrake * gear.ft * 0.3048))
hourmetres[2] <- sum(with(subset(log, nafo == '5ZEM'), numtow * avgtime / 60 * numrake * gear.ft * 0.3048))
hourmetres[3] <- sum(hourmetres)
names(hourmetres) <- c("5Zej", "5Zem", "total")
#browser()

effort<-list(round(hours),round(hourmetres))
names(effort) <- c("Effort in hours / trip", "Effort in hourmetres / trip")

# Return the effort to use however needed.
assign("effort",effort,pos=1)
}

