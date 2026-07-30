library(lubridate)
library(tidyverse)

source("C:/Users/keyserf/Documents/Github/Assessment_fns/Survey_and_OSAC/olex_import.R")
source("C:/Users/keyserf/Documents/Github/Assessment_fns/Survey_and_OSAC/olex_check_strata.R")
#rbind track files (.log) in a directory and add tow


file.locs <- readxl::read_xlsx("Y:/Offshore/Assessment/Data/2026/Temperature/LE23/logbook_locations__and_offsets_2018_2026.xlsx",sheet=1)

file.locs <- file.locs[file.locs$Year==2026,]

# Now we do the analysis, going one row at a time.
tow.data <- NULL

# There is an issue with 41 that I can't figure out... the GB spring survey in 2023 
for(i in 1:nrow(file.locs))
{
  print(paste("This is loop",i, 'of', nrow(file.locs)))
  dat <- file.locs[i,]
  # .gz files are the Olex files, so if we aren't using the olex files then we get the file pull.
  is.it.olex <- grepl(".gz",dat$tow_files)
  
  # UTM 32619 for GBa, GBb, BBn, Ger
  # UTM 32620 for BBs, Sab, Mid
  # UTM 32621 for Ban
  # If it is olex, then we have to go in and pull the data using the Olex Import function, this can take a minute (literally)
  if(is.it.olex == T) 
  {
    if(dat$Bank %in% c("GBa", "GBb", "BBn", "Ger", "GB")) {
      olex.tst <- olex_import(filename=dat$tow_files, 
                              UTM = 32619, earliest="2026-05-01", latest="2026-09-15", type="sf",
                              tow_number_key = dat$tow_order)
    }
    if(!dat$Bank %in% c("GBa", "GBb", "BBn", "Ger", "GB")) {
      olex.tst <- olex_import(filename=dat$tow_files, 
                              UTM = 32620, earliest="2026-05-01", latest="2026-09-01", type="sf",
                              tow_number_key = dat$tow_order)
    }
       
    #hmm <- olex.tst[olex.tst$official_tow_number %in% 301:324,]
    # Drop columns I don't want
    # Subset to the bank I'm doing this for...
    if(dat$Bank != "GB") olex.tmp <- olex.tst[olex.tst$Bank == dat$Bank,]
    if(dat$Bank == "GB") olex.tmp <- olex.tst[olex.tst$Bank %in% c("GBa","GBb"),]
    # Get rid of NAs in tow number if they exist
    olex.tmp <- olex.tmp[!is.na(olex.tmp$official_tow_number),]
    # Now just keep what I need.
    drops <- which(names(data.frame(olex.tmp)) %in% c("tow","geometry","shp","Bank"))
    tow.data[[i]] <- data.frame(olex.tmp)[,-drops]
    names(tow.data[[i]]) <- c("start_atz","end_atz","tow")
    
    tow.data[[i]]$start_atz <-  with_tz(tow.data[[i]]$start_atz, "Canada/Atlantic")
    tow.data[[i]]$end_atz <-  with_tz(tow.data[[i]]$end_atz, "Canada/Atlantic")
    tow.data[[i]]$type <- "Olex"
    tow.data[[i]]$bank <- dat$Bank
    tow.data[[i]]$cruise <- dat$Cruise
    tow.data[[i]]$year <- dat$Year
  }
}
# 
# # For some reason, the Middle bank tows 5-15 in 2022 are off by exactly one day. Why, nobody knows, but this fixed it.  
# # These 11 tows should happen on May 18th, if they don't this could be the culprit!
# tow.data[[32]]$start_atz[5:15] <- tow.data[[32]]$start_atz[5:15] + lubridate::days(1)
# tow.data[[32]]$end_atz[5:15] <- tow.data[[32]]$end_atz[5:15] + lubridate::days(1)

# Save this because it takes forever to run!
#saveRDS(tow.data,file="Y:/Offshore/Assessment/2026/Supporting_tasks/temperature/Tow_locations_and_time.Rds")
tow.data <- readRDS(file="Y:/Offshore/Assessment/Data/2026/Temperature/LE23/Tow_locations_and_time.Rds")

#Checking if the tow numbers are in chronological order

# Next lets pull in all the temperature data we have, I'll take that data and make it a long
yrs <- c(2026)
temperature.path <- "Y:/Offshore/Assessment/Data/Survey_data/"
temp.dat <- NULL
for(y in yrs)
{
  temp.temp.files <- list.files(path= paste0(temperature.path,y,"/Temperature/"),pattern = ".csv",full=T)
  n.files <- length(temp.temp.files)
  # Read in the temperature data
  temp.temp.dat <- NULL
  for (f in 1:n.files)  
  {
    print(paste("Reading file ",f, " of ",n.files))
    if(y < 2022) temp.temp.dat[[f]] <- as.data.frame(read.table(temp.temp.files[f],sep=",", as.is=T, header=F, skip=7))
    if(y >= 2022) temp.temp.dat[[f]] <- as.data.frame(read.table(temp.temp.files[f],sep=",", as.is=T, header=F, skip=8))
  }
  temp.dat[[as.character(y)]] <- do.call("rbind",temp.temp.dat)
}

temperature.data <- do.call("rbind",temp.dat)
names(temperature.data) <- c("date","time","t_deg_c")
temperature.data$time <- ymd_hms(paste(temperature.data$date,temperature.data$time),tz = "Canada/Atlantic")
# This also takes a minute remotely, so save this crap
#saveRDS(temperature.data,file="Y:/Offshore/Assessment/2026/Supporting_tasks/temperature/all_temp_data.Rds")

temperature.data <- readRDS(file="Y:/Offshore/Assessment/Data/2026/Temperature/LE23/all_temp_data.Rds")
#write.csv(temperature.data,"D:/testing_folder/Model_testing/Data/temperature_data.csv") # There is too much data for excel!


# Now I think we loop through the tow data and then link up the temperature data to each of these objects
n.locs <- length(tow.data)
sample.time <- lubridate::minutes(4) # Take the mean of the final 4 minutes of the tow. The timestep used in the past has varied, so it's not perfect here.
temp.out <- NULL
for(i in 1:n.locs)
{
  t.tow.data <- tow.data[[i]]
  n.tows <- nrow(t.tow.data)
  offset <- lubridate::seconds(file.locs$time_offset_secs[i]) 
  temp.temp.out <- NULL
  for(n in 1:n.tows)
  {
    t.tow <- t.tow.data[n,]
    # Now find temperature data within the time range of the tow +- 10 minutes
    #s.time <- t.tow$start_atz - lubridate::minutes(7) - lubridate::hours(1)
    e.time <- t.tow$end_atz + offset #- lubridate::hours(1)
    start.sample <- e.time - sample.time
    t.temp <- temperature.data |> collapse::fsubset(time < e.time & time >=start.sample)
    
    if(nrow(t.temp) > 0) t.tow$temperature <- mean(t.temp$t_deg_c,na.rm=T)
    if(nrow(t.temp) == 0) t.tow$temperature <- NA
    
    temp.temp.out[[n]] <- t.tow
    
  } # end for(n in 1:n.tows)

  temp.out[[i]] <- do.call('rbind',temp.temp.out)
  
} # end for(i in 1:n.locs)

temp.res <- do.call('rbind',temp.out)

# Remove tows... 
# In Sable 2021, there were a number of tows 1-29, that didn't have temperature readings, I believe the logger was turned on for tow 30, but
# was not on the gear yet, thus we remove that tow.
#rm1 <- which(temp.res$temperature == temp.res$temperature[temp.res$bank == "Sab" & temp.res$tow == '029' & temp.res$year == 2021])
# Same thing for BBn in 2021, tow 240 wasn't on the gear yet so should be scrubbed
#rm2 <- which(temp.res$temperature == temp.res$temperature[temp.res$bank == "BBn" & temp.res$tow == '240' & temp.res$year == 2021])
# There is an odd temperature on GBa in 2022 for tow 006... There is something wrong with the time stamp on this tow, 
# This corrects that time stamp  (from Trish) and enters the correct temperature (mean of the last 4 minutes of the tow)
#temp.res$temperature[temp.res$bank == "GBa" & temp.res$year == 2022 & temp.res$tow == c('006')] <- 9.36875
#temp.res$start_atz[temp.res$bank == "GBa" & temp.res$year == 2022 & temp.res$tow == c('006')] <- as.POSIXct("2022-08-05 00:17:54", tz = "Canada/Atlantic")
#temp.res$end_atz[temp.res$bank == "GBa" & temp.res$year == 2022 & temp.res$tow == c('006')] <- as.POSIXct("2022-08-05 00:27:54", tz = "Canada/Atlantic")
# And remove the two bad tows...

#temp.res <- temp.res[c(-rm1,-rm2),]

ggplot(temp.res) + geom_text(aes(x=year,y=temperature,label=tow),size=4) + facet_wrap(~bank)

#showtext_auto(enable = FALSE)

png("Y:/Offshore/Assessment/Data/2026/Temperature/LE23/temperature_plot.png", width=15, height=5,units="in", res=400)
print(ggplot() + geom_point(data=temp.res, aes(ymd_hms(start_atz), temperature)) +
  geom_line(data=temp.res, aes(ymd_hms(start_atz), temperature, group=1)) +
  facet_wrap(~bank, scale="free"))
dev.off()




#write.csv(temp.res,"Y:/Offshore/Assessment/2026/Supporting_tasks/temperature/cleaned_temperature_data_2026.csv")
temp.res <- read.csv("Y:/Offshore/Assessment/Data/2026/Temperature/LE23/cleaned_temperature_data_2026.csv")


# Load in the entire dataset which I cleaned up externally.  The 2015-2017 data was pulled from the Database and cleaned seperately as well.
write.csv(x = temp.res,"Y:/Offshore/Assessment/Data/2026/Temperature/LE23/cleaned_temperature_data_2026.csv")

ggplot(temp.res) + geom_text(aes(x=year,y=temperature,label=tow),size=4) + facet_wrap(~bank)

