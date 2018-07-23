

### A partial call to grab last 2 years of MARFISSCI data...
# This is just needed to make the next if statement work for the case of year == "ALL" can't do math on a character after all.
ifelse(year[1] == "ALL", yr <- current.year, yr <- max(year))


if(year[1] == "ALL" || (current.year - yr) <=1 ) # This will only look in MARFISSCI if year includes the current year and/or the year before
{
  
  # This sets the length of the loop, usually it will be 2, but if only looking for the current year it will just be one.
  # the if statement to enter this loop should make this if statement robust even though it might seem too flexible.
  if(year[1] =="ALL") len <- 2
  
  # This is far too complicated to do something easy, which is determine if len should be 1 or 2!
  if(year[1] != "ALL") len <- any(year == current.year) +  any(year == (current.year-1))
  
  # initialize our lists
  log.lst <- NULL
  slip.lst <- NULL
  
  # Run the loop, the maximum for "len" is 2, so there should never be more than 2 years of data coming from this loop.
  for(i in 1:len)
  {
    
    # The query to make to the SQL database
    # The "like %-" ignores first two digits and selects anything ending in the year of interest
    
    if(i == 1 && yr == current.year) # This will always grab the data from the most recent year of interest.
    {
      # Open the channel
      chan <- odbcConnect(db.con,uid=un,pwd=pw)
      # The query to grab log data
      qu.slip <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-",yr-2000,"'",sep="")
      qu.log <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-",yr-2000,"'",sep="")
      # Run the query and add data to the log.lst object            
      log.lst[[i]] <- sqlQuery(chan, qu.log)
      slip.lst[[i]] <- sqlQuery(chan, qu.slip)
      odbcCloseAll()  # close the database connection.
      
    } # END if i =1
    # This will always grab the data for the year before the current year if you are not looking for this years data
    if(i==1 && yr == current.year-1) 
    {
      # Open the channel
      chan <- odbcConnect(db.con,uid=un,pwd=pw)
      # The query to grab log data
      qu.slip <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-",yr-2000,"'",sep="")
      qu.log <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-",yr-2000,"'",sep="")
      # Run the query and add data to the log.lst object            
      log.lst[[i]] <- sqlQuery(chan, qu.log)
      slip.lst[[i]] <- sqlQuery(chan, qu.slip)
      odbcCloseAll()  # close the database connection.
    } # END if i =2
    
    # This will always grab the data for the year before the current year if you ARE looking for both years data
    if(i == 2)  
    {
      # Open the channel
      chan <- odbcConnect(db.con,uid=un,pwd=pw)
      # The query to grab log data
      qu.slip <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-",yr-1-2000,"'",sep="")
      qu.log <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-",yr-1-2000,"'",sep="")
      # Run the query and add data to the log.lst object            
      log.lst[[i]] <- sqlQuery(chan, qu.log)
      slip.lst[[i]] <- sqlQuery(chan, qu.slip)
      odbcCloseAll()  # close the database connection.
    } # END if i =2
    
    
  } # End the for loop.
  # Unpack the lists into a couple of data frames and select the columns of interest
  log.SQL1 <-  do.call("rbind",log.lst)
  slip.SQL1 <- do.call("rbind",slip.lst)
  log.SQL <- with(log.SQL1, data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME,vrnum = VR_NUMBER, tripnum = TRIP_ID, 
                                       fished = as.Date(DATE_FISHED,format="%d-%b-%y"), nafo = NAFO_UNIT_AREA, sfa = FISHING_AREA, 
                                       lon = LONGITUDE_DEG, lat = LATITUDE_DEG, depth.f = DEPTH_FM, bottom = BOTTOM_TYPE, watch = WATCH, 
                                       numrake = NO_RAKES_FISHED, numtow = NO_TOWS_PER_WATCH, avgtime = AVG_TOW_TIME, 
                                       pro.repwt = PRORATED_RPTD_WEIGHT_KGS, roeon = ROE_ON, numbags = NO_OF_BAGS)) 
  slip.SQL <- with(slip.SQL1, data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME, vrnum = VR_NUMBER, tripnum = TRIP_NUMBER, 
                                         sail = as.Date(DATE_SAILED,format="%d-%b-%y"), 
                                         land = as.Date(LANDING_DATE_TIME,format="%d-%b-%y"), gear.ft = GEAR_SIZE_FEET, 
                                         numshuck = NUM_OF_CREW_SHUCKING, numcrew = NUM_OF_CREW, weight = SLIP_WEIGHT_LBS, 
                                         grade = FISH_GRADE))
  
  
  # Now I want the option to export the data from last year into a flat file
  if(export ==T)
  {
    # If true we have data for last year, I want to make that a special flat file we will use next year, this will be
    # Added to the "Raw" folder...
    if(year[1] == "ALL" || any(year == current.year-1))
    {
      # Grab last years slip and log data and export them
      slip_last <- slip.SQL[grep((current.year-1), slip.SQL$sail),]
      log_last <- log.SQL[grep((current.year-1), log.SQL$fished),]
      
      write.table(log_last, file = paste("D:/Data/Approved/Offshore/Logs/Raw_logs/",(current.year-1),
                                         "log.csv",sep=""),sep=",",row.names=F,col.names=T)
      write.table(slip_last, file = paste("D:/Data/Approved/Offshore/Slips/Raw_slips/",(current.year-1),
                                          "slip.csv",sep=""),sep=",", row.names=F,col.names=T)
    } # end if(any(year == current.year-1))
  } #end if(export ==T)
} # end  if(year == "ALL" || (current.year - yr) <=1 ) 


#################  End Section 2 import the offshore data for this year and last year from SQL database  #############################




