################################  This function is used to calculate the bottom temperature for every tow in the inshore survey.  It needs to line up 
################################  The survey data and the temperature recorder data (which is just a stand alone Hobo type temperature logger).
################################  This function allows you to use the time that a tow starts, ends, or both to calculate the bottom temperature
################################  You can pick you many temperature readings you want to combine to calculate your minimum as well.
################################# You should also check for comments in the corresponding survey book in case there are issues with the tow start times, 
################################  re-do's etc. 

# Created by FK August 2017, Func'ed up by DK, August 2017

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
##
###############################################################################################################

###############################################################################################################
# Arguments
# 1:  direct:       The working directory to pull data from and save figures to.  Default = "Y:/INSHORE SCALLOP/Survey/2017/" 
# 2:  cruise:       The survey cruise you want to pull the temperature data from.  Default = "BI2017" (the data this was tested on)
# 3:  survey_time:  Do you want to use the "start" of the tow, the "end" of the tow, or "both" pieces of information to build
#                   a survey tow window.
# 4:  num.temps:    How many temperature values from within a particular tow do you want to use to find the Bottom Temperature.
#                   Default = 8 which takes the lowest 8 values (4 minutes.  The values are sorted from smallest to largest and a mean is chosen, 
#                   thus the value chosen is the mean of the "X" lowest tow temperatures.
# 5:  tow_duration: If you use only the "start" or "end" options in the "survey_time" arguement then you need to determine
#                   how long the tow lasts.  Generally this is approximately 10 minutes.  When just using start tow times set to 20 to start
# 6:  fig:          Do you want to save the figure.  Default = "pdf" which produces a plot of 1 page per survey day.
#                   any other choice will plot to the screen (with each day of survey data opening a new plot window)
# 7:  export:       Save the results as a csv.  T/F, default = F
###############################################################################################################

# Run these two lines to run the function
#source("Y:/Inshore/Survey/2017/temperature/Extract_survey_temperatures_function.r")
#survey.bottom.temps(survey_time = "start",export=T)

survey.bottom.temps <- function(direct = "Y:/Inshore/Survey/2017/", cruise = "BI2017", towfile=NULL,
                                survey_time= "start",num.temps = 8,tow_duration = 20,fig="pdf",export=F)
{
  #####################################################################
  
  require(dplyr)  || stop("You need the dplyr package installed or this ain't gonna work")
  require(scales) || stop("You need the scales package installed or this ain't gonna work")
  require(plyr) || stop("You need the plyr package installed or this ain't gonna work")
  require(lubridate) || stop("You need the lubridate package installed or this ain't gonna work")
  require(ggforce) || stop("You need the ggforce package installed or this ain't gonna work")
  require(ggplot2) || stop("You need the ggplot2 package installed or this ain't gonna work")
  require(tidyr) || stop("You need the tidyr package installed or this ain't gonna work")
  require(stringr) || stop("You need the stringr package installed or this ain't gonna work")
  require(viridis)|| stop("You need the viridis package installed or this ain't gonna work")

  ## read in files
  towdata <- read.csv(towfile)
  towdata <- towdata %>% 
    dplyr::group_by(bank, official_tow_number) %>%
    dplyr::summarize(start=min(ymd_hms(Date_time)), 
                     end=max(ymd_hms(Date_time))) %>%
    mutate(Tow=official_tow_number)
  
  # Now pull in the minilog files which contain the temperature by time files.
  #read in minilog files
  temps<- list.files(path = paste0(direct, cruise, "/"), pattern=".csv", full=TRUE)
  temps <- temps[grep(x=temps, pattern = "Minilog")]
  print("These are the temperature files you have pulled in")
  print(temps)
  
  # Combine all the temperature files into one dataframe and get the dates all sorted out.
  tempstows <- do.call(rbind, lapply(temps, function(x) {read.csv(x, header=T, skip=7, encoding="latin1")}))
  names(tempstows) <- c("Datelocal","Time","Temperature")
  
  ############################ Data Analysis ############################ Data Analysis
  #### Now start correcting the times, first we'll do the temperature data...
  
  # tell R that the tempstows data is ADT
  tempstows$Datetimelocal <- ymd_hms(paste0(tempstows$Datelocal, " ", tempstows$Time), tz = "Canada/Atlantic")
  # round to the minute
  tempstows$Datetimelocal_minute <- floor_date(tempstows$Datetimelocal, unit="minute")
  # tell R that the tempstows data is ADT
  tempstows$Datelocal <- ymd(tempstows$Datelocal,tz= "Canada/Atlantic") 
  # sort temps chronologically
  tempstows <- tempstows %>% arrange(Datetimelocal)

  # And now the data from the tow files.
  ## format dates and time, needs to be POSIXct format to work in ggplot...
  # If using the survey start time, or both the start and end time do this...
  if(survey_time %in% c("both","start"))
  {
    # This will add the leading 0's to the time field necessary to make the format match the time format
    # towdata$Time_start.UTC <- str_pad(towdata$Time_start.UTC,4,pad="0")
    # towdata$Tow_date <- str_pad(towdata$Date_time,8,pad="0")
    # Now stitch the time and data together to get a Date-Time column
    # Tell R that the tow data is in UTC
    towdata$Datetime_UTC_start <- towdata$start
    #as.POSIXct(strptime(paste0(towdata$Tow_date,towdata$Time_start.UTC),format = "%d%m%Y%H%M",tz = "UTC"))
    # Convert the UTC times to Canada/Atlantic (ADT)
    towdata$Datetime_local_start <- with_tz(towdata$Datetime_UTC_start, tzone="Canada/Atlantic") # set it finally to halifax
    towdata$Date_local_start <- floor_date(towdata$Datetime_local_start, unit="day")
    # User defined tow end time.
    if(survey_time == "start") towdata$Datetime_local_end <- towdata$Datetime_local_start + lubridate::minutes(tow_duration)
    # sort tows chronologically, could be done either by start or end time...
    towdata <- towdata %>% arrange(Datetime_local_start)
  } # end if(survey_time %in% c("both","start"))
  
  # If using the survey end time, or both the start and end time do this...
  if(survey_time %in% c("both","end"))
  {
    # This will add the leading 0's to the time field necessary to make the format match the time format
    # towdata$Time_start.UTC <- str_pad(towdata$Time_start.UTC,4,pad="0")
    # towdata$Tow_date <- str_pad(towdata$Date_time,8,pad="0")
    # Now stitch the time and data together to get a Date-Time column
    # Tell R that the tow data is in UTC
    towdata$Datetime_UTC_end <- towdata$end
    #as.POSIXct(strptime(paste0(towdata$Tow_date,towdata$Time_end.UTC),format = "%d%m%Y%H%M",tz = "UTC"))
    # Convert the UTC times to Canada/Atlantic (ADT)
    towdata$Datetime_local_end <- with_tz(towdata$Datetime_UTC_end, tzone="Canada/Atlantic") # set it finally to halifax
    towdata$Date_local_end <- floor_date(towdata$Datetime_local_end, unit="day")
    # User defined tow end time.
    if(survey_time == "end") towdata$Datetime_local_end <- towdata$Datetime_local_end - lubridate::minutes(tow_duration)
    # sort tows chronologically, could be done either by start or end time...
    towdata <- towdata %>% arrange(Datetime_local_end)
  } # if(survey_time %in% c("both","end"))

   tempstows$Tow <- NA
  # Here I want to add in the tow number for a given time window
  for(i in unique(towdata$Tow))
  {
    time.range <- data.frame(start = towdata$Datetime_local_start[towdata$Tow == i],end = towdata$Datetime_local_end[towdata$Tow == i])
    # So now we can ID the times in which we think we were towing.
    tempstows$Tow[tempstows$Datetimelocal_minute >= time.range$start & tempstows$Datetimelocal_minute <= time.range$end] <- i        
  } # end for(i in unique(towdata$Tow))
  # add a unique identifier column called "log" which corresponds to logger output

  # Use this browser to handle weird tows manually:
  #browser()
  
  tempstows$log <- 1:nrow(tempstows)
  # clean up any na's
  tows.only <- tempstows[!is.na(tempstows$Tow),]
  # determine temperatures using the lowest 8 temperatures recorded during the tow window (equivalent to 4 minutes of measurement), where the tow window is within 20 minutes of the tow starting time
  tows.only$Bottom_temp <- NA
  tows.only$Bottom_time <- as.POSIXct(tows.only$Datetimelocal[1]) # need to seed this with the correct timezone or it behaves weirdly...
  
  for(i in unique(tows.only$Tow)) 
  {  
    # This picks the 8 lowest temperatures from inside the plot window chosen for a given tow
    the.chosen <- which(tows.only$Temperature[tows.only$Tow == i] %in% sort(tows.only$Temperature[tows.only$Tow == i])[1:num.temps])
    # Get the temperature
    tows.only$Bottom_temp[tows.only$Tow == i] <- mean(tows.only$Temperature[tows.only$Tow == i][the.chosen])
    # Get the "mean" time that these reading occured.
    tows.only$Bottom_time[tows.only$Tow == i] <- mean(tows.only$Datetimelocal[tows.only$Tow == i][the.chosen])
  } # end for(i in unique(tows.only$Tow)) 
  
  # Just the temperature and tow data...
  tempsfinal <- aggregate(Temperature~Tow,tows.only,FUN=min)
  tempsfinal$Temperature <- round(tempsfinal$Temperature, 1)
  
  
  #########################  The figure....
  ## check out some snapshots of recorded tow time vs. temperature profile

  if(fig == "pdf") pdf(paste(direct, cruise, "/", cruise, "-Tow_",survey_time,"_time_vs_temp_profile.pdf",sep=""),onefile=T,width=11,height=6)
  
  dates <- unique(tows.only$Datelocal) # These are the dates for which there were tow
  plot.list <- NULL
  for (i in 1:length(dates))
  {
    # There might be a nice way to facet wrap all this, but for the moment this works at least...
    temps.tmp <- tempstows[tempstows$Datelocal == dates[i] & tempstows$Temperature < 18,]
    todays.tows <- unique(temps.tmp$Tow[!is.na(temps.tmp$Tow)])
    xy.int <- data.frame(Tow = todays.tows,start = as.POSIXct(towdata$Datetime_local_start[towdata$Tow %in% todays.tows],tz = "Canada/Atlantic"),
                         end = as.POSIXct(towdata$Datetime_local_end[towdata$Tow %in% todays.tows],tz = "Canada/Atlantic"),
                         min.T = min(temps.tmp$Temperature, na.rm = T),max.T = max(temps.tmp$Temperature, na.rm = T))
    # Limit the plot window to be 30 minutes before first tow and 30 minutes after the last tow.
    x.lim <- as.POSIXct(tows.only$Datetimelocal[1]) # need to seed this with the correct timezone details or tries to convert to AST for some reason...
    x.lim[1]  <- min(xy.int$end)-lubridate::minutes(30) ; x.lim[2] <- max(xy.int$end)+lubridate::minutes(30)
    ## Make the plot
    p <- ggplot() + ggtitle(dates[i]) + xlab("") + ylim(xy.int$min.T[1],xy.int$max.T[1]) +
      geom_point(data=temps.tmp, size=0.5, colour = "blue",aes(Datetimelocal, Temperature)) +
      geom_rect(data = xy.int,aes(xmin=start,xmax=end,ymin=min.T,ymax=max.T),fill=plasma(1,0.2,0.1),color="transparent") +
      scale_x_datetime(breaks = date_breaks("60 min"), labels=date_format(format = "%H:%M",tz = "Canada/Atlantic"),limits = x.lim) +
      geom_point(data=tows.only, aes(Bottom_time, Bottom_temp), fill=viridis(1,1,alpha=0.10), size=3, shape=21) +
      geom_text(data=xy.int, aes(end-lubridate::minutes(5), max.T, label=Tow), size=3) +
      theme_bw()
    plot.list[[i]] <- p
    if(fig != "pdf") windows(12,6); print(plot.list[[i]])
  } # end for (i in 1:length(dates))
  if(fig == "pdf") plot.list; dev.off()
  
  ### the output df is: tempsfinal
  if(export == T) write.csv(tempsfinal, paste0(direct, cruise, "/", cruise, "_towtemperatures.csv"))
} # end function