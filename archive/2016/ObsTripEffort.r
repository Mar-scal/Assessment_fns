#source("Y:\\Fishery data\\r\\fn\\ObsTripEffort.R")

ObsTripEffort <- function(ves1, land.date, v, Win = T,data=F){

# Function for calculating fishing effort (h/trip) on observed trips
#		for use in Georges by-catch estimation
#		
#		Created by Ian Jonsen 14Mar2008
#

# get data from MARFIS 
source("Y:\\Fishery data\\r\\fn\\get.logs.r")

# This indicates we have at least 2 versions of get.logs.r.
if(data)get.log(Win = Win)

# look-up table for vessel names
  # This would be far better if it was just selecting a row from an object with these in them.
  ves1 <- "ATLANTIC GUARDIAN"
  vessels <- read.table("D:/Offshore/fn/Offshore_Fleet.csv",sep=",",header=T)
  v <- vessels$ID[vessels$Vessel == ves1]    
if(missing(v)){
if(ves1 == 'ATLANTIC GUARDIAN') v <- '105912'
else if(ves1 == 'ATLANTIC DESTINY'){ v <- '105736'}
else if(ves1 == 'ATLANTIC LEADER'){ v <- '105457'}
else if(ves1 == 'ATLANTIC PRESERVER'){ v <- '106604'}
else if(ves1 == 'ATLANTIC PROTECTOR'){ v <- '106605'}
else if(ves1 == 'CAPE LAHAVE'){ v <- '102056'}
else if(ves1 == 'FREEDOM 99'){ v <- '101965'}
else if(ves1 == 'LADY COMEAU'){ v <- '1516'}
else if(ves1 == 'LADY DENISE'){ v <- '1518'}
else if(ves1 == 'LADY YVETTE'){ v <- '1548'}
else if(ves1 == 'BICKERTON PRIDE'){ v <- '1555'}
else if(ves1 == 'CACHALOT'){ v <- '4062'}
else if(ves1 == 'CHOCKLE CAP'){ v <- '100199'}
else if(ves1 == 'E.E. PIERCE'){ v <- 4055}
#else if(ves1 == 'FORTUNE LADY'){ v <- 'FORTUNE LADY'}
else if(ves1 == 'G S MERSEY'){ v <- '4031'}
else if(ves1 == 'LADY LISA'){ v <- '4051'}
#else if(ves1 == 'OCEAN LADY'){ v <- 'OCEAN LADY'}
else if(ves1 == 'TENACITY'){ v <- '5409'}
#else if(ves1 == 'A.F. PIERCE'){ v <- 'A.F. PIERCE'}
}

slip <- subset(slip.dat, land == as.Date(land.date) & vrnum == v)
gear<-unique(slip$gear)
if(length(gear)>1)stop("More than one unique gear in slip")
log <- subset(log.dat, mdid == unique(slip$mdid))


# calc effort in hours fished
hours <- c()
hours[1] <- sum(with(subset(log, nafo == '5ZEJ'), numtow * avgtime) / 60)
hours[2] <- sum(with(subset(log, nafo == '5ZEM'), numtow * avgtime) / 60)
hours[3] <- sum(hours)
names(hours) <- c("5Zej", "5Zem", "total")

hourmetres <- c()
hourmetres[1] <- sum(with(subset(log, nafo == '5ZEJ'), numtow * avgtime / 60 * numrake * gear * 0.3048))
hourmetres[2] <- sum(with(subset(log, nafo == '5ZEM'), numtow * avgtime / 60 * numrake * gear * 0.3048))
hourmetres[3] <- sum(hourmetres)
names(hourmetres) <- c("5Zej", "5Zem", "total")
#browser()

effort<-list(round(hours),round(hourmetres))
names(effort) <- c("Effort in hours / trip", "Effort in hourmetres / trip")
effort
}

