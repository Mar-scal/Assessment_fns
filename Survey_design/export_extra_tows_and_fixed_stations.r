# A quick script used to get all of the fixed and "extra" survey stations for the banks used over the years.
# I don't expect we actually ever need to run this script unless something changes with the fixed stations as I'm hoping
# the extra stations will be manually updated into "Extra_stations.csv" going forward
# This is assuming we start using the script "Survey_design.r", this file create\d Extra_stations.csv file and fixed_station_banks_towlst.csv

#######  Revision History ##########
# March 9, 2017 - Commented out the "fixed stations" for Mid and GB as the file with the exact station locations has been located
#                 These data are now found in the file 'fixed_station_banks_towlst.csv.


# Set the working directory 
direct <- "d:/r/"
#direct <- "Y:/Offshore scallop/Assessment/"

yr=2017
# I only need to load in two different files because I ran this in August so the Spring results were loaded, but the
# summer results weren't loaded yet
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  


## First get the the fixed station banks, Middle and Georges Bank spring
#Mid.spring <- subset(surv.Live[["Mid"]],random %in% 1 & year==yr,select = c("tow","slon","slat","bank","year","cruise","survey"))
#GB.spring <- subset(surv.Live[["GB"]],random %in% 3 & year==yr,select = c("tow","slon","slat","bank","year","cruise","survey"))
#fixed.stations <- rbind(Mid.spring,GB.spring)
#colnames(fixed.stations) <- c("EID","X","Y","Bank","Year","Cruise","Survey")
#write.csv(fixed.stations,paste(direct,"Data/Survey_data/Survey_design/fixed_station_banks_towlst.csv",sep=""),row.names=F)


# Now we can get the extra tows for banks that we've had extra tows on.
# Get the extra tows for GB spring and BBn
#Sab.star.box <- subset(surv.Live[["Sab"]],year == 2017 & tow > 900,select = c("tow","lon","lat","bank","year","cruise","survey"))
GB.extra <- subset(surv.Live[["GB"]],random %in% c(0,2,4,5),select = c("tow","lon","lat","bank","year","cruise","survey"))
BBn.spring <- subset(surv.Live[["BBn"]],random %in% c(0,2,4,5),select = c("tow","lon","lat","bank","year","cruise","survey"))
Sab.spring <- subset(surv.Live[["Sab"]],random %in% c(0,2,4,5),select = c("tow","lon","lat","bank","year","cruise","survey"))
# Now load this to get the GBa and GBb tows, again only necessary for the two different loads because of the timing
# of when I ran this.
load(paste(direct,"Data/Survey_data/",2016,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
GBa <-  subset(surv.Live[["GBa"]],random %in% c(0,2,4,5),select = c("tow","lon","lat","bank","year","cruise","survey"))
GBb <-  subset(surv.Live[["GBb"]],random %in% c(0,2,4,5),select = c("tow","lon","lat","bank","year","cruise","survey"))
# Combine them all
extra.stations <- rbind(GB.extra,BBn.spring,Sab.spring,GBa,GBb)
colnames(extra.stations) <- c("EID","X","Y","Bank","Year","Cruise","Survey")
# Output to a csv, if you run this line make sure you know what you are doing so you don't overwrite something!!
#write.csv(extra.stations,paste(direct,"Data/Survey_data/Extra_stations.csv",sep=""),row.names=F)

extra.stations[extra.stations$Year == 2017,]



