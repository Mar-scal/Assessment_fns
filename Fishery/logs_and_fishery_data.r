# DK Created August 24th, 2015.  This one file obtains the log/slip information from both/either SQL or flat files 
# available on our shared drive. This now works for both inshore and offshore and if we ever get the offshore
# into our own Schema this can be easily altered to select the data from the new SQL table.
# Revision History
## May 16, 2016 by DK, now can use 64 bit version of R when querying database.
## September 2016, DK, updating file locations
## January 2019:  DK, updated to work with either Roracle or RODBC
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1:  CPUE_monthly_or_observer
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
# 
##
###############################################################################################################

#		Created by Ian Jonsen 14Mar2008
#   Updated by Dave Keith August 2015 and beyond.

##### ARGUMENTS ####################################################
#loc:	        The log data to recover.  Default is 'both', Options are "inshore", "offshore", and  "both"
#year:        The year(s) from which to extract the data.  Default is the current year according to your 
#             computer.  The "ALL" option will select all available data. Note that "ALL" for inshore is 
#             2002 - 2015, while for offshore logs it is 1955-2015 (excluding 1960), for the offshore slips it is 2009-2015
#export:      Export the non-MARFIS data to flat files.  (T/F), default is F
#get.marfis:  Grab the offshore data from the marfis database? Note this will also export these data to special flat files if export =T. 
#             (T/F) default is F.
#ex.marfis:   Export the marfis data to special flat files?  (T/F) default is F.
#un:          your SQL username.  default = un.ID (if set in your r.Profile this will run automatically)
#pw:          Your SQL password.  default = pwd.ID  (if set in your r.Profile this will run automatically)
#db.con:      Database to connect to.  Default is  "ptran"   
#direct       The directory to put the logs for the offshore.  Default = direct which needs to be given a name somewhere
#direct.in    The directory to put the logs for the inshore.  Default = NULL which will put it here paste(direct,"Data/Inshore/Logs/Processed/",sep="")
# db.lib      The R library you are using to access the database.  default = "ROracle", RODBC also works
##### End ARGUMENTS ####################################################


logs_and_fish <- function(loc = "both",year=as.numeric(format(Sys.Date(),"%Y")),export=F,get.local=T,get.marfis = F,ex.marfis = F,
                          direct.in = NULL, un=un.ID,pw=pwd.ID,db.con="ptran",db.lib = "ROracle", direct, direct_fns)
{
  # Set up the directories
  direct.off <- direct
  if(is.null(direct.in)) direct.in <- paste(direct,"Data/Inshore/Logs/Processed/",sep="")
  if(get.marfis == T)
  {
    # If you didn't specify direct_funs go grab our master version
    if(missing(direct_fns))
    {
      funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Other_functions/ScallopQuery.R")
      # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
      for(fun in funs) 
      {
        download.file(fun,destfile = basename(fun))
        source(paste0(getwd(),"/",basename(fun)))
        file.remove(paste0(getwd(),"/",basename(fun)))
      } # end for(fun in funs)
    } else {source(paste0(direct_fns, "Other_functions/ScallopQuery.R"))} # end if(missing(direct_funs))
  } # end (if(get.marfis == T))                 

  require(splancs) || stop("Package splancs cannot be found")
  require(lubridate) || stop("Package lubridate cannot be found")
  
  # First get the current year from your computer, need for some of the if/table writing options.
  current.year <- as.numeric(format(Sys.Date(),"%Y"))

  # Need this  for some of our logical calls later...
  ifelse(year[1] == "ALL", log.year <- c(1955:1959,1961:2008,2009:current.year), log.year <- year)
#############################  Section 1 import the inshore data, optionally export a csv of  the log data.  #############################
  
  # So this is the call to obtain the inshore log data, this is pretty straightforward...
  if(loc == "inshore" || loc == "both" )  
    {
    # set up so we have a numeric "year" to work with when year == "ALL"
    ifelse(year[1] == "ALL", yr <- c(1955:1959,1961:2008,2009:(current.year-2)), yr <- year)
    
      # This gets the data for any year(s) requested from the SQL database, this allows for multi-year requests too.
      # There is currently no data for inshore before 2002 so no point running anything if that's the case!
      if(year[1] !='ALL' && max(yr) >=2002 )
      {
        # Initialize the variable
        log.lst <- NULL
        # Run through the loop for the year(s) requested
        for(i in 1:length(year))
        {
          # Pick the years < 2010, note this adds a 0 to the front of the query, 
          #if we get logs back before 2002 we'll need a new if statement here
          if(year[i] <2010 && year[i] > 2000) 
            {
            # Open the channel to the database
            #chan <- odbcConnect(package="db.lib", un=un.ID, pw=pwd.ID, db.con=db.con, db.con,uid=un,pwd=pw,believeNRows=FALSE)
            # The query to grab log data
            qu.log <- paste("select * from SCALLOP.SCALLOP_LOG_MARFIS where to_char(DATE_FISHED,'yy')='","0",year[i]-2000,"'",sep="")
            # Run the query and add data to the log.lst object
            log.lst[[i]] <- ScallopQuery(package=db.lib, un=un, pw=pw, db.con=db.con, SQLtext= qu.log)
            #log.lst[[i]] <- sqlQuery()
            # close the odbc connection
            #odbcCloseAll()
            } # end if year < 2010
          
          if(year[i] >=2010) 
            {
            # Open the channel to the database
            #chan <- odbcConnect(db.con,uid=un,pwd=pw,believeNRows=FALSE)
            # The query to grab log data
            qu.log <- paste("select * from SCALLOP.SCALLOP_LOG_MARFIS where to_char(DATE_FISHED,'yy')='",year[i]-2000,"'",sep="")
            # Run the query and add data to the log.lst object            
            log.lst[[i]] <- ScallopQuery(package=db.lib, un=un, pw=pw, db.con=db.con, SQLtext= qu.log)
            # close the odbc connection
            #odbcCloseAll()
            } # end if year >= 2010
          
        } # end for i in 1:length(year))
      # unpack the data from the list and make it a dataframe.
      log.dat.in <- do.call("rbind",log.lst)  
      } # end if(year != "ALL")  
      
      # If we just pick all years it is simple!
      if(year[1]=='ALL')
      {
        # Open the channel to the database
        #chan <- odbcConnect(db.con,uid=un,pwd=pw,believeNRows=FALSE)
        qu.log <- "select * from SCALLOP.SCALLOP_LOG_MARFIS"
        log.dat.in <- ScallopQuery(package=db.lib, un=un, pw=pw, db.con=db.con, SQLtext= qu.log)
        # close the odbc connection
        odbcCloseAll()
      } # end if year == ALL
      
    # Let's make the variable names closer matches to offshore, add
    # a couple columns and return log.dat.in to any function calling this function.
      if(max(yr) >=2002) 
      {
        log.dat.in <- with(log.dat.in, data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME,vrnum = VR_NUMBER, tripnum = TRIP_ID, 
                                             fished = as.Date(DATE_FISHED,format="%d-%b-%y"), nafo = NAFO_UNIT_AREA, 
                                             sfa.marfis = MARFIS_DET_AREA, 
                                             lon = LONGITUDE, lat = LATITUDE, depth.f = DEPTH_F, bottom = BOTTOM_TYPE,  
                                             numtow = NUM_OF_TOWS, cpue.kgh = CPUE_KG, pro.repwt = DAY_CATCH_KG,
                                             slip.wt.lb = SUM_SLIP_WEIGHT_LBS)) 
        # Now I want to add some columns to new.log.dat
        # First make a "date" column
        log.dat.in$date<- log.dat.in$fished
        # Next a column for the year
        log.dat.in$year<-as.numeric(format(log.dat.in$date, "%Y"))
         # A second "trip.id" which is a combination of several other pieces of information
        log.dat.in$trip.id<-with(log.dat.in,paste(year,vrnum,tripnum,sep='.'))
        # Also lets make sure we have a depth in meters here
        log.dat.in$depth.m <- log.dat.in$depth.f * 1.8828
        
        
          assign("log.dat.in", log.dat.in, pos = 1)
      } # end if(max(yr) >=2002) 
    
      # This exports the data to one flat file if requested, there are a crap load of scenarios we should cover with this 
      #filename so we know what we exported!
      if(export==T && max(yr) >=2002)
      {
        # Put this data in a new directory, if just picking one year from the past this is the file name to give it.
        if(year[1] != "ALL" && year != current.year && length(year) ==1) 
          {
          #Write11
          write.table(log.dat.in, file = paste(direct.in,"log_inshore_",year,".csv",sep=""),sep=",", 
                      row.names = F, col.names = T)
          } # end if just picking one year which isn't the current year...
        
        # if grabing multiple years but not the current year
        if(year[1] != "ALL" && max(year) != current.year && length(year) >1) 
          {
          #Write12
          write.table(log.dat.in, file = paste(direct.in,"log_inshore_",max(min(yr),2002),"-",
                      max(year),".csv",sep=""),sep=",", row.names = F, col.names = T)
          } # end if picking multiple years but not the current year.
        
        # If just pulling the logs from this year we should clarify the data the call was made so we know how up to date the log 
        # information is therein We use the latest date data are found in the database from the SQL call to clarify what are in the data.
        if(year[1] != "ALL" && max(year) == current.year && length(year) ==1) 
          {
          #Write13
          write.table(log.dat.in, file = paste(direct.in,"log_inshore_",year,"_up_to_",
                      max(format(log.dat.in$DATE_FISHED, "%Y-%m-%d"),na.rm=T),".csv",sep=""),sep=","
                      , row.names = F, col.names = T)
          } # end if picking just the current year data.
        
        # Now if grabbing the current year + other years (but not all the data) the filename should be...
        if(year[1] != "ALL" && max(year) == current.year && length(year) >1) 
        {
          #Write14
          write.table(log.dat.in, file = paste(direct.in,"log_inshore_",max(min(yr),2002),"-",max(year),"_up_to_",
                      max(format(log.dat.in$DATE_FISHED, "%Y-%m-%d"),na.rm=T),".csv",sep=""),sep=",", row.names = F, col.names = T)
        } # end if picking just the current year data.
        
        # If we are picking all of the data we should be a little more specific about what "all" actually is, 
        # as per above it is the most recent log data in the database.
        #Write15
        if(year[1] == "ALL") write.table(log.dat.in, file = paste(direct.in,"log_inshore_ALLDATA_up_to_",
                                         max(format(log.dat.in$DATE_FISHED, "%Y-%m-%d"),na.rm=T),".csv",sep=""),sep=",", 
                                         row.names = F, col.names = T)
      } # end if Export = T
    
    

    } # End if(loc == "inshore" || loc == "all")  
  
  #############################  END Section 1 import the inshore data, optionally export a csv of  the log data.  ################
 
  
#############################  Section 2 import the offshore data for this year and last year from SQL database  ############
  
  
# First import the most recent data (data for the current year and the year before), the remainder if interested will be pulled
# from our locally sourced flat files containing the log/slip information.  I think this would be tidier if we just had all the
# data stored in 1 flat file rather than have each year individually.
  
 
  # Start the offshore call.
  if(loc == "offshore" || loc == "both")
    {
  
    #####  The rest of the data has to be read from flat files.  We have these scattered all over the place.  Again I'm suggesting 
    # That these be consolidated into one location instead of at least 3
    # Again need to tweak this as we don't want to grab this year or last year
    # Kinda ugly but these are the years we have data for
    ifelse(year[1] == "ALL", yr <- c(1955:1959,1961:2008,2009:(current.year)), yr <- year)
  
    ## There was lots of code to modify the old data (as per the above with the new, but it has all been calculated and won't be changing
    ## until/unless we get it uploaded into SQL (code all preserved in the archived version of import.fishery.data if needed).
    # I will do a silly import and then assign those results to an object that anything using import.fishery.data can use.
    # These flat files appear from my spot checks to be identical to the long function used to generate them.
    # If only looking at recent data the object called "old.log.dat" will not exist.
    # So the to do list here is to grab the flat files that exist, not need to do anything fancy!
    # For the stuff before 2008 the files are simply these, grab them and bring them into import.fishery.data_DK
    # These are already processed and ready to go so no need to do anything pre-2009 here.
  
    if(get.local==T || is.null(get.local)){
        if(max(yr) > 2008) 
        {
          new.yr <- yr[yr > 2008]
          log.lst <- NULL
          slip.lst <- NULL

          for(i in 1:length(new.yr))
          {
            #Read1 # These files have been cleaned up a little to remove all # from the data, clearly these haven't been used in R programs
            # before or this is some new R problem with having "#" in the csv file.
            
            # If we have the final log data then we pull that down
            if(file.exists(paste(direct.off,"Data/Fishery_data/Logs/QAQC_logs/",new.yr[i],"log.csv",sep=""))==T)
            {
              log.lst[[i]] <-read.table(paste(direct.off,"Data/Fishery_data/Logs/QAQC_logs/",new.yr[i],"log.csv",sep=""),sep=",",header=T,
                                        stringsAsFactors = F,quote='')
            } # end if(file.exists(paste(direct.off,
            # If we are still using the preliminary logs then we use this
            if(file.exists(paste(direct.off,"Data/Fishery_data/Logs/QAQC_logs/",new.yr[i],"log.csv",sep=""))==F)
            {
              log.lst[[i]] <-read.table(paste(direct.off,"Data/Fishery_data/Logs/Preliminary/",new.yr[i],"log.csv",sep=""),
                                        sep=",",header=T,stringsAsFactors = F,quote='') # use fill=T to diagnose/troubleshoot in a browser() if something goes wrong here
            }# end if(file.exists(paste(direct.off,
            
            # If we have the final slip data then we pull that down
            if(file.exists(paste(direct.off,"Data/Fishery_data/Slips/QAQC_slips/",new.yr[i],"slip.csv",sep=""))==T)
            {
              #Read2 Had to get rid of the "'"  that was in several names before this would work...
              slip.lst[[i]] <-read.table(paste(direct.off,"Data/Fishery_data/Slips/QAQC_slips/",new.yr[i],"slip.csv",sep=""),sep=",",header=T,
                                         stringsAsFactors = F,quote='')
            } # end if(file.exists(paste(direct.off,
            
            # If we are still using the preliminary slips then we use this
            if(file.exists(paste(direct.off,"Data/Fishery_data/Slips/QAQC_slips/",new.yr[i],"slip.csv",sep=""))==F)
            {
                #Read2 Had to get rid of the "'"  that was in several names before this would work...
              slip.lst[[i]] <-read.table(paste(direct.off,"Data/Fishery_data/Slips/Preliminary/",new.yr[i],"slip.csv",sep=""),
                                        sep=",",header=T,stringsAsFactors = F,quote='')
            }# end if(file.exists(paste(direct.off,
            
            # to deal with the addition of the TOTAL_MEAT_KGS column to handle roe-on scallop:
            if("TOTAL_MEAT_KGS" %in% names(log.lst[[i]])) {
              # replace the pro.repwt column with the total meat kgs
              log.lst[[i]]$PRORATED_RPTD_WEIGHT_KGS <- log.lst[[i]]$TOTAL_MEAT_KGS
              
              # remove the total_meat_kgs column so that our dimensions match between years
              log.lst[[i]] <- log.lst[[i]][, !names(log.lst[[i]]) %in% "TOTAL_MEAT_KGS"]
            }
            
          } # end for 1:length(new.yr)
          
          # check for extra columns
          for(i in 1:length(log.lst)){
            if(any(names(log.lst[[i]]) == "X")) {
              if(all(grep(x = names(log.lst[[i]]), pattern="X") > 35) & # if the offending columns are towards the end of the df
                 all(nchar(names(log.lst[[i]])[grep(x = names(log.lst[[i]]), pattern="X")]) <4) &# if the names are shorter than 4 characters (e.g. X.1)
                 all(is.na(log.lst[[i]][grep(x = names(log.lst[[i]]), pattern="X")]))) # and if the columns only contain NAs
              {
                message(paste0("Extra empty columns ", paste(names(log.lst[[i]])[grep(x = names(log.lst[[i]]), pattern="X")], sep=",", collapse = ", "), " were removed from log.lst[[", i, "]]"))
                log.lst[[i]] <- dplyr::select(log.lst[[i]], -names(log.lst[[i]])[grep(x = names(log.lst[[i]]), pattern="X")])
              }
            }
          }
          for(i in 1:length(slip.lst)){
            if(any(names(slip.lst[[i]]) == "X")) {
              if(all(grep(x = names(slip.lst[[i]]), pattern="X") > 35) & # if the offending columns are towards the end of the df
                 all(nchar(names(slip.lst[[i]])[grep(x = names(slip.lst[[i]]), pattern="X")]) <4) &# if the names are shorter than 4 characters (e.g. X.1)
                 all(is.na(slip.lst[[i]][grep(x = names(slip.lst[[i]]), pattern="X")]))) # and if the columns only contain NAs
              {
                message(paste0("Extra empty columns ", paste(names(slip.lst[[i]])[grep(x = names(slip.lst), pattern="X")], sep=",", collapse = ", "), " were removed from slip.lst[[", i, "]]"))
                slip.lst[[i]] <- dplyr::select(slip.lst[[i]], -names(slip.lst[[i]])[grep(x = names(slip.lst[[i]]), pattern="X")])
              }
            }
          }
          
          log1 <- do.call("rbind",log.lst)
          slip1 <- do.call("rbind",slip.lst)

          # This removes columns/variables we do not need.
          slip <- with(slip1, data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME, vrnum = VR_NUMBER, tripnum = TRIP_ID, 
                                         sail = as.Date(DATE_SAILED,format="%d-%b-%y"), 
                                         land = as.Date(LANDING_DATE_TIME,format="%d-%b-%y"), 
                                         gear.ft = GEAR_SIZE_FEET, numshuck = NUM_OF_CREW_SHUCKING, numcrew = NUM_OF_CREW, 
                                         weight = SLIP_WEIGHT_LBS, grade = FISH_GRADE,stringsAsFactors = F))
          
          # This removes columns/variables we do not need.
          log <- with(log1, data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME,vrnum = VR_NUMBER, tripnum = TRIP_ID, 
                                       fished = as.Date(DATE_FISHED,format="%d-%b-%y"), nafo = NAFO_UNIT_AREA, 
                                       sfa = FISHING_AREA, lon = LONGITUDE_DEG, lat = LATITUDE_DEG, depth.f = DEPTH_FM, 
                                       bottom = BOTTOM_TYPE, watch = WATCH, numrake = NO_RAKES_FISHED, 
                                       numtow = NO_TOWS_PER_WATCH, avgtime = AVG_TOW_TIME, 
                                       pro.repwt = PRORATED_RPTD_WEIGHT_KGS, roeon = ROE_ON, numbags = NO_OF_BAGS,stringsAsFactors = F)) 
      } # End if max(yr > 2008)
    
    

    # Grab the data from 2008 onwards, these are flat files which are SQL data fixed.
    if(max(log.year) > 2008 )
      {
        new.log.dat <- log
        slip.dat <- slip
        
        # Make sure all the longitude data are negative as they should be, there at one point were a couple entered incorrectly.
        new.log.dat$lon[new.log.dat$lon > 0 & !is.na(new.log.dat$lon)] <- new.log.dat$lon[new.log.dat$lon > 0 & !is.na(new.log.dat$lon)]*-1
        
        # Now to do fishery calculations we need information on the type of boat we are dealing with.
        # These are the offshore vessel numbers, ft = freezer trawler, wet is a wet boat (no freezer)
        # This file location will need adjusted.
        #Read3
        boat.types <- read.table(paste(direct.off,"Data/Offshore_fleet.csv",sep=""),sep=",",header=T)
        # select the freezer trawlers
        boats.ft <- subset(boat.types,Type== "FT")$ID
        # select the wet fish boats
        boats.wet <- subset(boat.types,Type== "WF")$ID
        
        # ID the boats as Wet fish or freezers, if you are getting an NA here the "Offshore_fleet.csv" needs to be updated with the ID of 
        # the new boats in the fleet.
        new.log.dat$fleet[new.log.dat$vrnum %in% boats.ft] <- "FT"
        new.log.dat$fleet[new.log.dat$vrnum %in% boats.wet] <- "WF"    
        
        # Now I want to transfer the landing weight, number of crew shucking, and date information from slips into the logs
        new.log.dat$gear.ft<-slip.dat[match(new.log.dat$mdid, slip.dat$mdid),]$gear.ft
        new.log.dat$numshuck<-slip.dat[match(new.log.dat$mdid, slip.dat$mdid),]$numshuck
        
        # Combine the data, I'd usually use a tapply here, but with the dates this is occasionally giving a very
        # annoying error, thus I'm moving to the less efficient for loop
        # Initialize variables and reset the dates to characters
        doc.id <- unique(slip.dat$mdid)
        num.ids <- length(doc.id)
        slip.dat$sail <- as.character(slip.dat$sail)
        slip.dat$land <- as.character(slip.dat$land)
        date.sail <- NULL
        date.land <- NULL
        landing <- NULL
        slips <- NULL
        hdr <- NULL
        # Run the for loop to extract the unique date/ID combinations to match the landing totals above
        for(i in 1:num.ids)
        {
          date.sail[i] <- slip.dat$sail[slip.dat$mdid == doc.id[i]][1]
          date.land[i] <- slip.dat$land[slip.dat$mdid == doc.id[i]][1]
          landing[i] <- sum(slip.dat$weight[slip.dat$mdid == doc.id[i]],na.rm=T)
          hdr[i] <- slip.dat$mdid[slip.dat$mdid == doc.id[i]][1]
        }
         
        # convert it back to a date.
        date.sail <- as.Date(date.sail, "%Y-%m-%d")
        date.land <- as.Date(date.land,"%Y-%m-%d")
     
        # Add in the landing and date landed information into the log dat.
        new.log.dat<-merge(new.log.dat,data.frame(mdid=hdr,landing,date.sail, date.land),all=T)
        #new.log.dat$date.land <- as.Date(new.log.dat$date.land,"%Y-%m-%d")
        
        # Now I want to add some columns to new.log.dat
        # First make a "date" column
        new.log.dat$date<- new.log.dat$fished
        # Next a column for the year
        new.log.dat$year<-as.numeric(format(new.log.dat$date, "%Y"))
        # Now an ID for the banks
        new.log.dat$bank<-NA
        new.log.dat$bank[new.log.dat$sfa=="27A"]<-"GBa"
        new.log.dat$bank[new.log.dat$sfa=="27B"]<-"GBb"
        new.log.dat$bank[new.log.dat$sfa=="25A"]<-"Sab"
        new.log.dat$bank[new.log.dat$sfa=="25A"& new.log.dat$nafo=="4WE"]<-"Mid"
        new.log.dat$bank[new.log.dat$sfa=="26C"]<-"Ger"
        new.log.dat$bank[new.log.dat$sfa=="26A"]<-"BBn"
        new.log.dat$bank[new.log.dat$sfa=="26B"]<-"BBs"
        new.log.dat$bank[new.log.dat$sfa=="25B"]<-"Ban"
        new.log.dat$bank[new.log.dat$sfa=="3PS"]<-"SPB"
        new.log.dat$bank[new.log.dat$sfa%in% c(10,11,12)]<-"SPB"
        
        # A second "trip.id" which is a combination of several other pieces of information
        new.log.dat$trip.id<-with(new.log.dat,paste(year,vrnum,tripnum,sep='.'))
        # Make sure numrake is numeric, there may be some weird data in there from SQL database
        if(length(new.log.dat$numrake == "UNK") > 0) new.log.dat$numrake[new.log.dat$numrake == "UNK"] <- NA
        new.log.dat$numrake <- as.numeric(new.log.dat$numrake)
        # Also lets make sure we have a depth in meters here
        new.log.dat$depth.m <- new.log.dat$depth.f * 1.8828
        # Finally add a date class, this is for compatibility with old.log.dat
        new.log.dat$datclass<-1
        
        # Now calculate the fishery summary data and add this to the file...
        # Only the "new.log.data" needs this information calculated (it is already in old.log.dat and the inshore too)  
        # Calculate how effort in hours
        new.log.dat$h <- new.log.dat$numtow * new.log.dat$avgtime / 60
        # Calculate the area swept in meters
        new.log.dat$m <- new.log.dat$numrake * new.log.dat$gear.ft * 0.3048
        # Now Effort in hour-meters
        new.log.dat$hm <- new.log.dat$m * new.log.dat$h
        # CPUE in kg/hr
        new.log.dat$kg.h <- new.log.dat$pro.repwt / new.log.dat$h
        # CPUE in kg/(hr-m)
        new.log.dat$kg.hm <- new.log.dat$pro.repwt / new.log.dat$hm
        # And output the data to whomever wants		
        assign("new.log.dat", new.log.dat, pos = 1)
        assign("slip.dat", slip.dat, pos = 1)
        assign("fleet_data", boat.types, pos = 1)
        
        # Now if we want to export all of these data (from 2009-present) we can do it here.
        if(export == T)
        {
          # this last.year is used to determine if the data includes information from the current year, because
          # this is incomplete I want to I.D. when the latest data is in here.
          last.year <- ifelse(year[1]=="ALL",current.year,max(year))
          if(last.year != current.year)
          {
            #Write16 These are somewhat more complex than necessary, the ifelse really isn't needed...
            write.table(new.log.dat, file = paste(direct.off,"Data/Fishery_data/Logs/Compiled/",
                                                  ifelse(year[1]=="ALL",2009,max(min(year),2009)),"-",
                                                  ifelse(year[1]=="ALL",current.year,max(year)),"log.csv",sep=""),
                                                  sep=",", row.names=F,col.names=T)
            #Write17
            write.table(slip.dat, file = paste(direct.off,"Data/Fishery_data/Slips/Compiled/",
                                               ifelse(year[1]=="ALL",2009,max(min(year),2009)),"-",
                                               ifelse(year[1]=="ALL",current.year,max(year)),"slip.csv",sep=""),
                                               sep=",", row.names=F,col.names=T)
          } # if(last.year != current.year)
          # If we have the data from the current year in here we want to do this...
          if(last.year == current.year)
          {
            #Write18
            write.table(new.log.dat, file = paste(direct.off,"Data/Fishery_data/Logs/Compiled/",
                                                  ifelse(year[1]=="ALL",2009,max(min(year),2009)),"-",
                                                  ifelse(year[1]=="ALL",current.year,max(year)),"_up_to_",
                                                  max(format(ymd(new.log.dat$fished), "%Y-%m-%d"),na.rm=T),
                                                  "log.csv",sep=""), # end of the big ugle paste!
                                                  sep=",", row.names=F,col.names=T)
            
            
            #Write19
            write.table(slip.dat, file = paste(direct.off,"Data/Fishery_data/Slips/Compiled/",
                                               ifelse(year[1]=="ALL",2009,max(min(year),2009)),"-",
                                               ifelse(year[1]=="ALL",current.year,max(year)),"_up_to_",
                                               max(format(ymd(slip.dat$sail), "%Y-%m-%d"),na.rm=T),
                                               "slip.csv",sep=""), # end of the big ugle paste!
                                               sep=",", row.names=F,col.names=T)
          } # if(last.year == current.year)
          
          } # end if(export ==T)
        
        
      } #end if(max(log.year) > 2008)
      
    
    
    
    #################### Now bring in and combine the log data from before 2009. 
    
    if(min(log.year) <= 2008 ) 
      {

        # These two files include fishery data, they were previously processed by archived version of import.fishery.data.r 
        # No need to keep doing that.
        #Read4
        really.old.log.dat <- read.table(paste(direct.off,"Data/Fishery_data/Logs/1955_2008/LogData1955-1980.txt",sep=""),sep="\t",
                                         header=T,stringsAsFactors = F)
        #Read5
        less.old.log.dat <- read.table(paste(direct.off,"Data/Fishery_data/Logs/1955_2008/LogData1981-2008.txt",sep=""),sep="\t",
                                       header=T,stringsAsFactors = F)
        old.log.dat <- merge(really.old.log.dat,less.old.log.dat,all=T)
        # Make St. Pierre Bank "SPB" throughout docs.
        old.log.dat$bank[old.log.dat$bank =="Sp"] <- "SPB"
        # For for the data from before 1999 Browns and Georges Banks are not delineated, but by stealing this code from
        # fishery.dat we can do this right here with the raw data and have it prepared for anyone who wants to use the logs.
        # Set up a variable name so we keep the old bank info. And remove the info from bank (we will replace in last step...)
        old.log.dat$bank_old <- old.log.dat$bank
        old.log.dat$bank <- NA
        banks.xy <- read.table(paste(direct.off,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""),header=T,sep=",")
        # This is so splancs knows what columns to select.
        colnames(banks.xy) <- c("PID","POS","x","y","label")
        # GBa is this and GBb
        GBaArea.xy <- banks.xy[banks.xy$label == "SFA27A",]
        GBbArea.xy<- banks.xy[banks.xy$label == "SFA27B",]
        BBnArea.xy <- banks.xy[banks.xy$label == "SFA26N",]
        BBsArea.xy<- banks.xy[banks.xy$label == "SFA26S",]
        GerArea.xy <- banks.xy[banks.xy$label == "SFA26",]
        # Notice some of the data comes from the US side...
        GB.USA <- rbind(GBaArea.xy[5:7,3:4],BBnArea.xy[6:8,3:4],c(-70,43),c(-70,40.04755),c(-65.87748,40.04755))
        
        # This outlines where GBa is
        old.log.dat$bank[old.log.dat$lon < 0 & old.log.dat$lat > 0 & !is.na(old.log.dat$lon) & !is.na(old.log.dat$lat) &
                           old.log.dat$year < 1999][with(subset(old.log.dat,lon < 0 & lat > 0 & !is.na(lon) & !is.na(lat) & year < 1999), 
                                                           inout(cbind(lon,lat), GBaArea.xy, bound = T))]<-"GBa"
        # This outlines where GBb is
        old.log.dat$bank[old.log.dat$lon < 0 & old.log.dat$lat > 0 & !is.na(old.log.dat$lon) & !is.na(old.log.dat$lat) &
                           old.log.dat$year < 1999][with(subset(old.log.dat,lon < 0 & lat > 0 & !is.na(lon) & !is.na(lat) & year<1999), 
                                                           inout(cbind(lon,lat), GBbArea.xy, bound = T))]<-"GBb"
        
        # This is the old catch that occured on the US waters back in the day.
        old.log.dat$bank[old.log.dat$lon < 0 & old.log.dat$lat > 0 & !is.na(old.log.dat$lon) & !is.na(old.log.dat$lat) &
                           old.log.dat$year < 1999][with(subset(old.log.dat,lon < 0 & lat > 0 & !is.na(lon) & !is.na(lat) & year<1999), 
                                                         inout(cbind(lon,lat), GB.USA, bound = T))]<-"GBUSA"
        
        # This outlines where BBn is
        old.log.dat$bank[old.log.dat$lon < 0 & old.log.dat$lat > 0 & !is.na(old.log.dat$lon) & !is.na(old.log.dat$lat) &
                           old.log.dat$year < 1999][with(subset(old.log.dat,lon < 0 & lat > 0 & !is.na(lon) & !is.na(lat) & year < 1999), 
                                                         inout(cbind(lon,lat), BBnArea.xy, bound = T))]<-"BBn"
        # This outlines where BBs is
        old.log.dat$bank[old.log.dat$lon < 0 & old.log.dat$lat > 0 & !is.na(old.log.dat$lon) & !is.na(old.log.dat$lat) &
                           old.log.dat$year < 1999][with(subset(old.log.dat,lon < 0 & lat > 0 & !is.na(lon) & !is.na(lat) & year<1999), 
                                                         inout(cbind(lon,lat), BBsArea.xy, bound = T))]<-"BBs"
        # This outlines where German Bank is
        old.log.dat$bank[old.log.dat$lon < 0 & old.log.dat$lat > 0 & !is.na(old.log.dat$lon) & !is.na(old.log.dat$lat) &
                           old.log.dat$year < 1999][with(subset(old.log.dat,lon < 0 & lat > 0 & !is.na(lon) & !is.na(lat) & year<1999), 
                                                         inout(cbind(lon,lat), GerArea.xy, bound = T))]<-"Ger"
        
        # For the remainder of the data I will refill using the original bank information.
        old.log.dat$bank[is.na(old.log.dat$bank)] <- old.log.dat$bank_old[is.na(old.log.dat$bank)]
        
        # subset the data for the years of interest.
        start <- max(min(old.log.dat$year),min(yr))
        finish <- min(max(old.log.dat$year),max(yr)) 
        range <- seq(start,finish,by=1)
        old.log.dat <- old.log.dat[old.log.dat$year %in% range,]
        
        # And then output the data
        assign("old.log.dat", old.log.dat, pos = 1)  	   
   
        # Now if there is data from the pre-2009 logs and we want to export it lets do it here.
        if(export == T)
          {
            #Write20
            write.table(old.log.dat, file = paste(direct.off,"Data/Fishery_data/Logs/Compiled/",ifelse(year[1]=="ALL",1955,max(min(year),1955)),
                                               "-",ifelse(year[1]=="ALL",2008,min(max(year),2008)),"log.csv",sep=""),
                                               sep=",",row.names=F,col.names=T)
          } # end if(export ==T)
      } # end if(min(log.year) <= 2008 ) 
    }
     
    #############################  End Section 2 import the the offshore data from local flat files  #############################
    
  

  #############################   Section 3 import the the offshore data from Marfis  #############################

  # This data HAS NOT BEEN CORRECTED so if you are using this data you better know why/what you are doing! 
  # The offshore data from Section 2 (above) is the corrected and checked data and should be used in all analyses.  
  # Note that the marfis data exists only for 2009 onwards.  
  
  if(get.marfis==T && max(log.year) >= 2008)
  {
    
    # If the data is from the last 2 years we grab the data from the Production version of MARFIS.
    # If the data is older than this it is stored in the archive version of marfis.
    # This is just needed to make the next if statement work for the case of year == "ALL" can't do math on a character after all.
    ifelse(year[1] == "ALL", yr <- 2008:current.year, yr <- year[year >= 2008])
    len <- length(yr)
    # initialize our lists
    log.lst <- NULL
    slip.lst <- NULL
      
      # Run the loop across all years of data
      for(i in 1:len)
        {
          # The query to make to the SQL database
          # The "like %-" ignores first two digits and selects anything ending in the year of interest
          # This will grab the data for the most recent 2 years.
          if(yr[i] == current.year || yr[i] == current.year-1) # This will always grab the data from the most recent year of interest.
            {
              # Open the channel
              #chan <- odbcConnect(db.con,uid=un,pwd=pw,believeNRows=FALSE)
              # The query to grab log data
              qu.slip <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-",yr[i]-2000,"'",sep="")
              qu.log <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-",yr[i]-2000,"'",sep="")
              
              # Run the query and add data to the log.lst object
              log.lst[[i]] <- ScallopQuery(package=db.lib, un=un, pw=pw, db.con=db.con, SQLtext= qu.log)
              slip.lst[[i]] <- ScallopQuery(package=db.lib, un=un, pw=pw, db.con=db.con, SQLtext= qu.slip)
              
              #odbcCloseAll()  # close the database connection.
              
              # Send each year to a flat file in the Raw_MARFIS folder
              # if we are extracting data from the current year include the date so we know how current the data are.
              if(ex.marfis ==T)
                {
                # If data is from last year then simple files names are good enough
                if(yr[i] != current.year)
                  {
                  #Write1
                  write.table(log.lst[[i]], file = paste(direct.off,"Data/Fishery_data/Logs/MARFIS/MARFIS_log",yr[i],
                                                     ".csv",sep=""),sep=",",row.names=F,col.names=T)
                  #Write2
                  write.table(slip.lst[[i]], file = paste(direct.off,"Data/Fishery_data/Slips/MARFIS/MARIFS_slip",yr[i],
                                              ".csv",sep=""),sep=",", row.names=F,col.names=T)
                  } # END if(yr[i] != current.year)       
                
                # if data are from this year more specific information on how current the data are should be included.
                if(yr[i] == current.year)
                  {
                    #Write3
                    write.table(log.lst[[i]], file = paste(direct.off,"Data/Fishery_data/Logs/MARFIS//MARFIS_log_up_to",
                                                           max(log.lst[[i]]$DATE_FISHED,na.rm=T), ".csv",sep=""),
                                                           sep=",",row.names=F,col.names=T)
                    #Write4
                    write.table(slip.lst[[i]], file = paste(direct.off,"Data/Fishery_data/Slips/MARFIS/MARIFS_slip_up_to",
                                                            max(slip.lst[[i]]$DATE_SAILED,na.rm=T),".csv",sep=""),
                                                            sep=",", row.names=F,col.names=T)
                  } # END if(yr[i] != current.year)   
                } # end if(ex.marfis==T)
              
            } #end if(yr[i] == current.year || yr[i] == current.year-1)
            
          # This grabs the archived MARFIS table data from before this year.  These data are more dubious in nature than
          # even the above P_OFFSHORE tables, I hope you know what you are doing!!
          if(yr[i] < current.year-1) 
            {
              # Open the channel
              #chan <- odbcConnect(db.con,uid=un,pwd=pw,believeNRows=FALSE)
              # The query to grab log data, the qurey differs for before/after 2010
              if(yr[i] > 2009)
                {
                qu.slip <- paste("select * from marfissci.OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-",yr[i]-2000,"'",sep="")
                qu.log <- paste("select * from marfissci.OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-",yr[i]-2000,"'",sep="")
                } # end if(yr[i] > 2009)
              if(yr[i] <= 2009)
              {
                qu.slip <- paste("select * from marfissci.OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-0",yr[i]-2000,"'",sep="")
                qu.log <- paste("select * from marfissci.OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-0",yr[i]-2000,"'",sep="")
              } # end if(yr[i] > 2009)
              
              
            # Run the query and add data to the log.lst object            
            log.lst[[i]] <- ScallopQuery(package=db.lib, un=un, pw=pw, db.con=db.con, SQLtext= qu.log)
            slip.lst[[i]] <- ScallopQuery(package=db.lib, un=un, pw=pw, db.con=db.con, SQLtext= qu.slip)
            #odbcCloseAll()  # close the database connection.
            
            # Send each year to a flat file in the MARFIS folder
              if(ex.marfis ==T)
                {
                  #Write5
                  write.table(log.lst[[i]], file = paste(direct.off,"Data/Fishery_data/Logs/MARFIS/MARFIS_log",yr[i],
                                                         ".csv",sep=""),sep=",",row.names=F,col.names=T)
                  #Write6
                  write.table(slip.lst[[i]], file = paste(direct.off,"Data/Fishery_data/Slips/MARFIS/MARIFS_slip",yr[i],
                                                          ".csv",sep=""),sep=",", row.names=F,col.names=T)
                } # end if(ex.marfis==T)
              } # END if(yr[i] < current.year-1) 
        } # End the for loop.
      
      # Now between the two tables things are not quite the same, here's a useful subset of data we can pull out that
      # is in both tables.  Same data as in the flat file logs/slips above wherever possible.
    
      # Run the loop across all years of data
      for(k in 1:len)
        {
          log.lst[[k]] <- with(log.lst[[k]], data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME,vrnum = VR_NUMBER, tripnum = TRIP_ID, 
                                                       fished = as.Date(DATE_FISHED,format="%d-%b-%y"), nafo = NAFO_UNIT_AREA, 
                                                       sfa = FISHING_AREA, lon = LONGITUDE_DEG, lat = LATITUDE_DEG,
                                                       lon_ent = ENT_LONGITUDE,lat_ent = ENT_LATITUDE,
                                                       depth.f = DEPTH_FM, bottom = BOTTOM_TYPE, watch = WATCH, 
                                                       numrake = NO_RAKES_FISHED, numtow = NO_TOWS_PER_WATCH,
                                                       avgtime = AVG_TOW_TIME, pro.repwt = PRORATED_RPTD_WEIGHT_KGS,
                                                       weight = WEIGHT,roeon = ROE_ON, numbags = NO_OF_BAGS,comments = COMMENTS)) 
  
          slip.lst[[k]] <- with(slip.lst[[k]], data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME, vrnum = VR_NUMBER, tripnum = TRIP_ID, 
                                                          sfa = SCALLOP_FISHING_AREA,nafo = NAFO_UNIT_AREA,
                                                          sail = as.Date(DATE_SAILED,format="%d-%b-%y"), 
                                                          land = as.Date(LANDING_DATE_TIME,format="%d-%b-%y"), 
                                                          gear.ft = GEAR_SIZE_FEET, numshuck = NUM_OF_CREW_SHUCKING, numcrew = NUM_OF_CREW, 
                                                          weight = SLIP_WEIGHT_LBS, grade = FISH_GRADE))
        }# end  for(k in 1:len)
    
    # Now we can unpack the lists into a couple of data frames and select the columns of interest
    log.SQL <-  do.call("rbind",log.lst)
    slip.SQL <- do.call("rbind",slip.lst)

    # now we can also add a bank field to the log data...
    
    log.SQL$bank<-NA
    log.SQL$bank[log.SQL$sfa=="27A"]<-"GBa"
    log.SQL$bank[log.SQL$sfa=="27B"]<-"GBb"
    log.SQL$bank[log.SQL$sfa=="25A"]<-"Sab"
    log.SQL$bank[log.SQL$sfa=="25A"& log.SQL$nafo=="4WE"]<-"Mid"
    log.SQL$bank[log.SQL$sfa=="26C"]<-"Ger"
    log.SQL$bank[log.SQL$sfa=="26A"]<-"BBn"
    log.SQL$bank[log.SQL$sfa=="26B"]<-"BBs"
    log.SQL$bank[log.SQL$sfa=="25B"]<-"Ban"
    log.SQL$bank[log.SQL$sfa=="3PS"]<-"SPB"
    log.SQL$bank[log.SQL$sfa%in% c(10,11,12)]<-"SPB"
    
    # And same for the slips...
    slip.SQL$bank<-NA
    slip.SQL$bank[slip.SQL$sfa=="27A"]<-"GBa"
    slip.SQL$bank[slip.SQL$sfa=="27B"]<-"GBb"
    slip.SQL$bank[slip.SQL$sfa=="25A"]<-"Sab"
    slip.SQL$bank[slip.SQL$sfa=="25A"& slip.SQL$nafo=="4WE"]<-"Mid"
    slip.SQL$bank[slip.SQL$sfa=="26C"]<-"Ger"
    slip.SQL$bank[slip.SQL$sfa=="26A"]<-"BBn"
    slip.SQL$bank[slip.SQL$sfa=="26B"]<-"BBs"
    slip.SQL$bank[slip.SQL$sfa=="25B"]<-"Ban"
    slip.SQL$bank[slip.SQL$sfa=="3PS"]<-"SPB"
    slip.SQL$bank[slip.SQL$sfa%in% c(10,11,12)]<-"SPB"
    
    
    if(max(yr) != current.year & ex.marfis ==T)
    {
      #Write7
      write.table(log.SQL, file = paste(direct.off,"Data/Fishery_data/Logs/MARFIS/MARFIS_log",min(yr),"-",max(yr),
                                        ".csv",sep=""),sep=",",row.names=F,col.names=T)
      #Write8
      write.table(slip.SQL, file = paste(direct.off,"Data/Fishery_data/Slips/MARFIS/MARIFS_slip",min(yr),"-",max(yr),
                                         ".csv",sep=""),sep=",", row.names=F,col.names=T)
    } # END if(yr[i] != current.year)       
    
    # if data are from this year more specific information on how current the data are should be included.
    if(max(yr) == current.year & ex.marfis ==T)
    {
      #Write9
      write.table(log.SQL, file = paste(direct.off,"Data/Fishery_data/Logs/MARFIS/MARFIS_log_",min(yr),"-",
                                        max(log.SQL$fished,na.rm=T), ".csv",sep=""),
                  sep=",",row.names=F,col.names=T)
      #Write10
      write.table(slip.SQL, file = paste(direct.off,"Data/Fishery_data/Slips/MARFIS/MARIFS_slip_",min(yr),"-",
                                         max(log.SQL$fished,na.rm=T),".csv",sep=""),
                  sep=",", row.names=F,col.names=T)
    } # END if(yr[i] != current.year)   
    
    # Return the marifs slip and log data for use elsewhere.
    assign('marfis.log',log.SQL,pos=1)
    assign('marfis.slip',slip.SQL,pos=1)
    
    
    # Make sure all the longitude data are negative as they should be, there at one point were a couple entered incorrectly.
    log.SQL$lon[log.SQL$lon > 0 & !is.na(log.SQL$lon)] <- log.SQL$lon[log.SQL$lon > 0 & !is.na(log.SQL$lon)]*-1
    
    # Now to do fishery calculations we need information on the type of boat we are dealing with.
    # These are the offshore vessel numbers, ft = freezer trawler, wet is a wet boat (no freezer)
    # This file location will need adjusted.
    #Read3
    boat.types <- read.table(paste(direct.off,"Data/Offshore_fleet.csv",sep=""),sep=",",header=T)
    # select the freezer trawlers
    boats.ft <- subset(boat.types,Type== "FT")$ID
    # select the wet fish boats
    boats.wet <- subset(boat.types,Type== "WF")$ID
    
    # ID the boats as Wet fish or freezers, if you are getting an NA here the "Offshore_fleet.csv" needs to be updated with the ID of 
    # the new boats in the fleet.
    log.SQL$fleet[log.SQL$vrnum %in% boats.ft] <- "FT"
    log.SQL$fleet[log.SQL$vrnum %in% boats.wet] <- "WF"    
    
    # Now I want to transfer the landing weight, number of crew shucking, and date information from slips into the logs
    log.SQL$gear.ft<-slip.SQL[match(log.SQL$mdid, slip.SQL$mdid),]$gear.ft
    log.SQL$numshuck<-slip.SQL[match(log.SQL$mdid, slip.SQL$mdid),]$numshuck
    
    # Combine the data, I'd usually use a tapply here, but with the dates this is occasionally giving a very
    # annoying error, thus I'm moving to the less efficient for loop
    # Initialize variables and reset the dates to characters
    doc.id <- unique(slip.SQL$mdid)
    num.ids <- length(doc.id)
    slip.SQL$sail <- as.character(slip.SQL$sail)
    slip.SQL$land <- as.character(slip.SQL$land)
    date.sail <- NULL
    date.land <- NULL
    landing <- NULL
    slips <- NULL
    hdr <- NULL
    # Run the for loop to extract the unique date/ID combinations to match the landing totals above
    for(i in 1:num.ids)
    {
      date.sail[i] <- slip.SQL$sail[slip.SQL$mdid == doc.id[i]][1]
      date.land[i] <- slip.SQL$land[slip.SQL$mdid == doc.id[i]][1]
      landing[i] <- sum(slip.SQL$weight[slip.SQL$mdid == doc.id[i]],na.rm=T)
      hdr[i] <- slip.SQL$mdid[slip.SQL$mdid == doc.id[i]][1]
    }
    
    # convert it back to a date.
    date.sail <- as.Date(date.sail, "%Y-%m-%d")
    date.land <- as.Date(date.land,"%Y-%m-%d")
    
    # Add in the landing and date landed information into the log dat.
    log.SQL<-merge(log.SQL,data.frame(mdid=hdr,landing,date.sail, date.land),all=T)
    #new.log.dat$date.land <- as.Date(new.log.dat$date.land,"%Y-%m-%d")
    
    # fix formatting 
    if(!is.numeric(log.SQL$depth.f) & is.character(log.SQL$depth.f)) log.SQL$depth.f <- as.numeric(log.SQL$depth.f)
    if(!is.numeric(log.SQL$numtow) & is.character(log.SQL$numtow)) log.SQL$numtow <- as.numeric(log.SQL$numtow)
    if(!is.numeric(log.SQL$avgtime) & is.character(log.SQL$avgtime)) log.SQL$avgtime <- as.numeric(log.SQL$avgtime)
    if(!is.numeric(log.SQL$gear.ft) & is.character(log.SQL$gear.ft)) log.SQL$gear.ft <- as.numeric(log.SQL$gear.ft)
    if(!is.numeric(log.SQL$numrake) & is.character(log.SQL$numrake)) log.SQL$numrake <- as.numeric(log.SQL$numrake)
    
    # Now I want to add some columns to new.log.dat
    # First make a "date" column
    log.SQL$date<- log.SQL$fished
    # Next a column for the year
    log.SQL$year<-as.numeric(format(log.SQL$date, "%Y"))
    
    # A second "trip.id" which is a combination of several other pieces of information
    log.SQL$trip.id<-with(log.SQL,paste(year,vrnum,tripnum,sep='.'))
    # Make sure numrake is numeric, there may be some weird data in there from SQL database
    if(length(log.SQL$numrake == "UNK") > 0) log.SQL$numrake[log.SQL$numrake == "UNK"] <- NA
    
    # Also lets make sure we have a depth in meters here
    log.SQL$depth.m <- log.SQL$depth.f * 1.8828
    # Finally add a date class, this is for compatibility with old.log.dat
    log.SQL$datclass<-1
    
    # Now calculate the fishery summary data and add this to the file...
    # Only the "new.log.data" needs this information calculated (it is already in old.log.dat and the inshore too)  
    # Calculate how effort in hours
    log.SQL$h <- log.SQL$numtow * log.SQL$avgtime / 60
    # Calculate the area swept in meters
    log.SQL$m <- log.SQL$numrake * log.SQL$gear.ft * 0.3048
    # Now Effort in hour-meters
    log.SQL$hm <- log.SQL$m * log.SQL$h
    # CPUE in kg/hr
    log.SQL$kg.h <- log.SQL$pro.repwt / log.SQL$h
    # CPUE in kg/(hr-m)
    log.SQL$kg.hm <- log.SQL$pro.repwt / log.SQL$hm
   
    assign('marfis.log.dat',log.SQL,pos=1)
      
  } # end get.marfis==T    
    #################  End Section 3 import the offshore data  from SQL database  #############################
  } # end if(loc == "offshore" || loc == "both")        
} # End function logs_and_fishery_data.r



