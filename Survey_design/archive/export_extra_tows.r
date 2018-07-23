# A quick script used to get all of the "extra" survey stations for the banks used over the years.
# Assuming we start using the Extra_stations.csv file and updating it 
direct <- "d:/r/"
yr=2016
# I only need to load in two different files because I ran this in August so the Spring results were loaded, but the
# summer results weren't loaded yet
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
# Get the extra tows for GB spring and BBn
GB.spring <- subset(surv.Live[["GB"]],random %in% c(0,2,4,5),select = c("tow","lon","lat","bank","year","cruise","survey"))
BBn.spring <- subset(surv.Live[["BBn"]],random %in% c(0,2,4,5),select = c("tow","lon","lat","bank","year","cruise","survey"))

# Now load this to get the GBa and GBb tows, again only necessary for the two different loads because of the timing
# of when I ran this.
load(paste(direct,"Data/Survey_data/",2015,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
GBa <-  subset(surv.Live[["GBa"]],random %in% c(0,2,4,5),select = c("tow","lon","lat","bank","year","cruise","survey"))
GBb <-  subset(surv.Live[["GBb"]],random %in% c(0,2,4,5),select = c("tow","lon","lat","bank","year","cruise","survey"))
# Combine them all
extra.stations <- rbind(GB.spring,BBn.spring,GBa,GBb)
colnames(extra.stations) <- c("EID","X","Y","Bank","Year","Cruise","Survey")
# Output to a csv, if you run this line make sure you know what you are doing so you don't overwrite something!!
#write.csv(extra.stations,paste(direct,"Data/Survey_data/Extra_stations.csv",sep=""),row.names=F)
