# DK August 27, 2015 Commented and checked by DK.  # Function for calculating fishing effort on user specified bank by month
# and also for extracting observer data.
# Revision history
# DK September 2016, cleaned up file names/locations and tidied up layout of function in general.
# DK/FK August 2017, fixed the observer calculations to allow for split trips (it was calculating all values for just 1 bank previously)
# Catch and Effort should match OSAC values, but CPUE will be different since "total CPUE" here is a MEAN not a calculated value.
#####################################  File Summary ########################################################
####  
##  This function is used in these files (a.k.a. 'dependent files') 
##  1:  Catch.Effort.Tables.r
##  
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#  1:  logs_and_fishery_data
##
###############################################################################################################



#source("Y:\\Fishery data\\r\\fn\\Catch.Effort.Tables.R")

#Catch.Effort.Tables

# Agruments: 
# CPUE:     CPUE to calculate, CPUE by month/bank/nafo, observer specific trip CPUE or both.  Default is "month".  Options are "month", "obs", and "both"
#           Arguments needed for observer data start with "obs."
# bank:     Which bank to select from using abbrevated bank id.  Default is GBa (Georges Bank A). 
#           The options are "GBa" = Georges a, "GBb" = Georges b, "BBn" = Browns north, "BBs" = Browns south, "GBBB" = all of Georges and Browns banks.
#           "GB" = all of Georges Bank, "BB" = all of Browns Bank, Ger" = German, 
#           "Mid" = Middle, "Sab" = Sable, "Ban" = Banquereau, and "SPB" = St. Pierre Bank
# year:     the year of interest.  We are limited to years from which we have slip data, so this is limited to 2009:current_year (maybe 2008 is possible)
# fleet:    ALL = total fleet, FT = freezer trawlers, WF = wet fishery		
# boxes:    Summarize the data for the seed boxes?  Default is NULL, other  options currently are "GB", "BB", "ALL"
# print:    Print the results to the screen.  (T/F), default = F
# output:   Return the results as r objects.  (T/F), default = T
# nafo.div: The nafo division.  This needs to align with the Bank choice as well, what is produced is the amount landed on bank X nafo division y
#           if nafo division crosses multple banks this will only pull out part of NAFO division data.  Default is NULL.  This 
#           option won't work great for the data before 2008 without careful attention as these data have some differernt division names.
# export.logs:    Do you want to export the log and fishery data.  This does not include these monthly tables. (T/F) default is F  
#                 See logs_and_fishery_data.r for details
# export.tables:  Do you want to export the tables produced in this query.  (T/F), default is F
# months:         Select the months of interest.  Numerice, default is all months, c(1:12)
# obs.vnum:       The vessel number for the observer trips, multiple trips are allowed BUT MUST BE PAIRED WITH APPROPRIATE obs.land.date argument.  
#                 If CPUE = "obs" or "both" and this is = NULL this will read in a flat file with observer trip information in it. Default is NULL
# obs.land.date:  The landing date of the observer trip(s), multiple trips are allowed BUT THIS MUST BE PAIRED WITH APPROPRIATE obs.vnum 
#                 If CPUE = "obs" or "both" and this is = NULL this will read in a flat file with observer trip information in it. Default is NULL
# obs.export:     Export the observer CPUE data?  (T/F), default =F
#un:              your SQL username.  default = un.ID (if set in your r.Profile this will run automatically)
#pw:              Your SQL password.  default = pwd.ID  (if set in your r.Profile this will run automatically)
#db.con:          Database to connect to.  Default is  "ptran"   
#direct:          Directory to find the functions.  Default is "Y:/Offshore scallop/Assessment/")


CPUE.mon <- function(CPUE = "month", bank = NULL, year = as.numeric(format(Sys.Date(),"%Y")), fleet = "ALL", boxes=NULL,
                     print=F,output=T, export.tables = "F", export.logs="F",months = c(1:12),nafo.div =NULL,obs.vnum = NULL,
                     obs.land.date = NULL, obs.export=F,un=un.ID,pw=pwd.ID,db.con="ptran",get.marfis=F,
                     direct=direct, direct_bycatch=direct_bycatch)
{

########################################################## Section 1 Monthly CPUE calculations ##########################################	
  
  require(splancs)  || stop("You need the package splancs... thanks")
  require(RODBC) || stop("Package RODBC cannot be found")
  require(plyr)
  require(lubridate)
  source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
  
  if(any(months > 12)) stop("You have specified a month > 12, please fix 'months' in function call")
  #Source1 source("d:/R/fn/logs_and_fishery_data_DK.r") get data from logs and fish function
  if(get.marfis == F) logs_and_fish(loc="offshore",year=year,export=export.logs, get.marfis = F, direct=direct)
  if(get.marfis == T) logs_and_fish(loc="offshore",year=year,export=export.logs, get.marfis = T, un=un, pw=pw, db.con=db.con, direct=direct)
  # For these monthly calculations we need to know the month fishing occured
  new.log.dat$month <- as.numeric(format(new.log.dat$fished,"%m"))
  
  # This large section is run if CPUE = "month" or "both", this calculates the monthly CPUE for different Banks.
  if(CPUE == "month" || CPUE == "both")
  {
    # Here we initialize some variables that we need for later, kinda ugly eh...
    variables <- as.character(c("year","month","bank","nafo","fleet","days", "h", "hm","crhm", "lbs",  "kg","mt","kg.h", "kg.hm","kg.crhm"))
    
    month.lab <- as.character(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Total"))
    if(length(months) < 12) month.lab <- c(month.lab[months],"Total")
    old.dates <- c("January","February","March","April","May","June","July","August","September","October","November","December")
    
    # Here are some labels for the created data frames
    # For days, h, hm, crhm, lbs, kg and mt we want the sums  
    sum.names <- c("days","h", "hm", "crhm", "lbs", "kg","mt")
    # For the CPUE metrics we want averages
    avg.names <- c("kg.h","kg.hm","kg.crhm")
    
    # Make a dataframe for all of our data...
    dat <-data.frame(setNames(replicate(length(variables),numeric(length(months)+1), simplify = F), 
                              variables),row.names = month.lab)
    old.dat <-data.frame(setNames(replicate(length(variables),numeric(length(months)+1), simplify = F), 
                                  variables),row.names = month.lab)
    
    
    # Select the boats that below to the appropriate part of the fleet.
    if(fleet == "FT") boats <- fleet_data$ID[fleet_data$Type =="FT"]
    if(fleet == "WF") boats <- fleet_data$ID[fleet_data$Type =="WF"]
    if(fleet == "ALL") boats <- fleet_data$ID
    
    # Create new objects for the below calculations based upon the bank chosen...
    # Need to treat GB and BB differently as they are a combo of two banks...
    if(!is.null(bank) && !bank %in% c("GB", "GBBB", "BB"))
    {
      nafo1 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank==bank]))
      bank1 <- as.character(unique(new.log.dat$sfa[new.log.dat$bank==bank]))
    } # end if(bank != "GB")
    
    # For Browns Bank we need to be more specific.
    if(!is.null(bank) && bank == "BB")
    {
      # This works for GB as GBa overlaps the nafo boundaries...
      nafo.t1 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank=="BBn"]))
      nafo.t2 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank=="BBs"]))
      nafo1 <- c(nafo.t1,nafo.t2)
      bank1 <- c("26A","26B")
    } # end if(bank == "GB")
    
    # For all of Georges Bank we need to be more specific.
    if(!is.null(bank) && bank == "GB")
    {
      # This works for GB as GBa overlaps the nafo boundaries...
      nafo1 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank=="GBa"]))
      bank1 <- c("27A","27B")
    } # end if(bank == "GB")
    # For all of Georges Bank we need to be more specific.
    if(!is.null(bank) && bank == "GBBB")
    {
      # This works for GB as GBa overlaps the nafo boundaries...
      nafo.t1 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank=="GBa"]))
      nafo.t2 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank=="BBn"]))
      nafo.t3 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank=="BBs"]))
      nafo1 <- c(nafo.t1,nafo.t2,nafo.t3)
      bank1 <- c("26A","26B","27A","27B")
    } # end if(bank == "GB")
    
    # If we want to select a particular nafo division do so here.  This needs to align with the Bank choice as well, what
    # is produced is the amount landed on bank X nafo division y
    if(is.null(nafo.div) == F) nafo1 <- nafo.div
    
    # initialize a couple of the lists we need
    box.list <- NULL # Formerly Table 2
    month.list <- NULL
    box.list.all <- NULL
    old.list <- NULL
    
    # Loop across all years of interest.	
    for(y in 1:length(year))	
    {
      # For earlier years we don't have the data in the same format as before it, we could do a similar calculation using these data though I'd be 
      # less certain about it's quality.  This will not pick up all of the sub bank data since the banks were not divided into sub-banks at various
      # points of time in the past.  These should be used carefully!
      if(year[y] <=2008) 
      {
        # Create new objects for the below calculations based upon the bank chosen...
        # Need to treat GB and BB differently as it is a combo of two banks...
        if(bank != "GB" && bank != "GBBB" && bank != "BB")
        {
          old.nafo <- as.character(unique(old.log.dat$nafo[old.log.dat$bank==bank]))
          old.nafo <- old.nafo[!is.na(old.nafo)]
          old.bank <- as.character(unique(old.log.dat$bank[old.log.dat$bank==bank]))
          old.bank <- old.bank[!is.na(old.bank)]
        } # end if(bank != "GB")
        # For all of Georges Bank we need to be more specific.
        if(bank == "BB")
        {
          # This should get all the BB data from the old logs
          old.nafo.t1 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="BBs"]))
          old.nafo.t2 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="BBn"]))
          old.nafo.t3 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="BB"]))
          old.nafo <- unique(c(old.nafo.t1,old.nafo.t2,old.nafo.t3))
          old.nafo <- old.nafo[!is.na(old.nafo)]
          old.bank <- c("BBn","BBs","BB")
        } # end if(bank == "GB")
        # For all of Georges Bank we need to be more specific.
        if(bank == "GB")
        {
          # This should get all the GB data from the old logs
          old.nafo.t1 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="GBa"]))
          old.nafo.t2 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="GBb"]))
          old.nafo.t3 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="GB"]))
          old.nafo <- unique(c(old.nafo.t1,old.nafo.t2,old.nafo.t3))
          old.nafo <- old.nafo[!is.na(old.nafo)]
          old.bank <- c("Gba","GBb","GB")
        } # end if(bank == "GB")
        # For all of Georges Bank we need to be more specific.
        if(bank == "GBBB")
        {
          # This should get all the GB and BB data fromt the old log data
          old.nafo.t1 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="GBa"]))
          old.nafo.t2 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="BBn"]))
          old.nafo.t3 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="BBs"]))
          old.nafo.t4 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="BB"]))
          old.nafo.t5 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="GB"]))
          old.nafo.t6 <- as.character(unique(old.log.dat$nafo[old.log.dat$bank=="GBb"]))
          old.nafo <- unique(c(old.nafo.t1,old.nafo.t2,old.nafo.t3,old.nafo.t4,old.nafo.t5,old.nafo.t6))
          old.nafo <- old.nafo[!is.na(old.nafo)]
          old.bank <- c("GB","BB","GBa","GBb","BBn","BBS","BB")
        } # end if(bank == "GB")
        c = 0 # initialize a counter, see below
        # Run through each month of the year in specified region
        for(m in months)
        {
          c <- c + 1 # I need a counter for cases when m is not 1:12...      
          # This will pick all the old log data in from a given month/year combination in a nafo/bank combination (see if statements above)
          # The if statement determine whether we need to specify the fleet or not.
          if(fleet != "ALL")
          {
            temp <- old.log.dat[old.log.dat$month == old.dates[m] & old.log.dat$year == year[y] & old.log.dat$nafo %in% old.nafo & 
                                  old.log.dat$bank %in% old.bank & old.log.dat$fleet == fleet ,]
          } # end if(fleet != "ALL")
          # If we want both fleets than don't need old.log.dat$fleet == fleet
          if(fleet == "ALL")
          {
            temp <- old.log.dat[old.log.dat$month == old.dates[m] & old.log.dat$year == year[y] & old.log.dat$nafo %in% old.nafo & 
                                  old.log.dat$bank %in% old.bank ,]
          } # end if(fleet == "ALL")
          # Get the number of days out fishing for the month
          old.dat$days[c] <- sum(temp$day.fishing,na.rm=T)
          # Add up all the effort data for the month (all calculated for each cell already)
          old.dat$h[c] <- sum(temp$h,na.rm=T)
          old.dat$hm[c] <- sum(temp$hm, na.rm = T)
          old.dat$crhm[c] <- sum(temp$crhm, na.rm = T)
          
          # Catch in lbs then convert to kg and metric tonnes.
          old.dat$lbs[c] <- sum(temp$pro.repwt,na.rm=T) * 2.2046
          old.dat$kg[c] <- sum(temp$pro.repwt,na.rm=T)
          old.dat$mt[c] <- round(old.dat$kg[c] * 0.001)
          
          # Now calculate the various CPUE metrics, kg/hr, kg/(hr-m), kg/(hr-m-crew), I think I could take the averages from the cells but this 
          # seems a more correct way to do this...
          old.dat$kg.h[c] <- round(old.dat$kg[c] / old.dat$h[c], 2)
          old.dat$kg.hm[c] <- round(old.dat$kg[c] / old.dat$hm[c], 2)
          old.dat$kg.crhm[c] <- round(old.dat$kg[c] / old.dat$crhm[c], 2)	
          
          # Toss in some other useful id type data.
          old.dat$month[c] <- old.dates[m]
          old.dat$bank[c] <-  ifelse(length(unique(temp$bank)) > 0,paste(unique(temp$bank),collapse="-"),NA)
          old.dat$nafo[c] <- ifelse(length(unique(temp$nafo)) > 0,paste(unique(temp$nafo),collapse="-"),NA)
          old.dat$year[c] <- year[y]
          # Make the rownames an informative combination of month-year
          rownames(old.dat)[c] <- paste(old.dat$month[c],year[y],sep="-")
        } # end for(m in months)
        # Now in the final row we place the totals or averages for the year.
        old.dat[length(months)+1,names(old.dat) %in% sum.names]  <- colSums(old.dat[1:length(months),names(old.dat) %in% sum.names],na.rm=T)
        old.dat[length(months)+1,names(old.dat) %in% avg.names]  <- round(colMeans(old.dat[1:length(months),names(old.dat) %in% avg.names],na.rm=T),2)
        # get rownames for the totals too. 
        rownames(old.dat)[c+1] <- paste("Total",old.dat$year[1],sep="-")
        old.dat$bank[c+1] <- paste(unique(old.bank),collapse="-")
        old.dat$nafo[c+1] <- paste(unique(old.nafo),collapse="-")
        old.dat$month[c+1] <- "Total"
        old.dat$year[c+1] <- year[y]
        old.dat$fleet <- rep(fleet,length(old.dat[,1]))
        # Toss the old data into a list
        old.list[[y]] <- old.dat
        
      } # end if(year[y] <=2008) 
      
      # If the date is 2009 or later we use the slip data to make these monthly calculations
      if(year[y] > 2008)  
      {	
        c = 0
        # Run through each month of the year in specified region
        for(m in months)
        {
          c <- c + 1 # I need a counter for cases when m is not 1:12...
          # So this selects the nafo region we chose, within the correct fishing year, between the dates for the month of interest
          # for each boat of interest in the fleet.
          if(!is.null(bank)) temp  <- new.log.dat[new.log.dat$nafo %in% nafo1 & new.log.dat$sfa %in% bank1 &
                                                    new.log.dat$month == m & new.log.dat$vrnum %in% boats &
                                                    new.log.dat$year == year[y] ,]
          if(is.null(bank)) temp  <- new.log.dat[new.log.dat$nafo %in% nafo1 & 
                                                   new.log.dat$month == m & new.log.dat$vrnum %in% boats &
                                                   new.log.dat$year == year[y] ,]
          
          # Then we calculate the days each boat is out and then add these all up and you have the number of days the fleet was out                    
          days.by.boat <- tapply(temp$fished,temp$ves,function(x) length(unique(x)))
          dat$days[c] <- sum(days.by.boat,na.rm=T)
          
          # Here we calculate effort in hours, then hour-meters, then crew-hour-meters
          dat$h[c] <- sum(with(temp, numtow * avgtime / 60), na.rm = T)
          dat$hm[c] <- sum(with(temp, slip.dat[match(mdid, slip.dat$mdid),]$gear.ft 
                                      * 0.3048 * numrake * numtow * avgtime / 60), na.rm = T)
          dat$crhm[c] <- sum(with(temp, with(slip.dat[match(mdid, slip.dat$mdid),], 
                                                   gear.ft * 0.3048 * numshuck) * numrake * numtow * avgtime / 60), na.rm = T)
          
          # Catch in lbs then convert to kg and metric tonnes
          dat$lbs[c] <- with(temp,sum(pro.repwt * 2.2046, na.rm = T))
          dat$kg[c] <- with(temp,sum(pro.repwt * 2.2046, na.rm = T)) / 2.2046
          dat$mt[c] <- with(temp,sum(pro.repwt * 2.2046, na.rm = T)) / 2.2046 * 0.001
          
          # Now calculate the various CPUE metrics, kg/hr, kg/(hr-m), kg/(hr-m-crew)
          dat$kg.h[c] <- dat$kg[c] / sum(with(temp, numtow * avgtime / 60), na.rm = T)
          dat$kg.hm[c] <- dat$kg[c] / dat$hm[c]
          dat$kg.crhm[c] <- dat$kg[c] / dat$crhm[c]
          ## 2019-10-23: All rounding is going to occur AFTER the calculations are performed. This will result in some mismatches in the
          ## output table, but they are simply due to rounding. You should note this in public-facing documents to avoid confusion.
          ## "Small discrepancies are due to rounding"
          
          # Toss in some other useful id type data. Note that the m subscript on the old.dates is correct...
          dat$month[c] <- old.dates[m]
          dat$bank[c] <-  ifelse(length(unique(temp$bank)) > 0,paste(unique(temp$bank),collapse="-",sep=""),NA)
          dat$nafo[c] <- ifelse(length(unique(temp$nafo)) > 0,paste(unique(temp$nafo),collapse="-",sep=""),NA)
          dat$year[c] <- year[y]
          # Make the rownames an informative combination of month-year
          rownames(dat)[c] <- paste(dat$month[c],year[y],sep="-")
          
        } # end for(m in months)
        
        # Now in the final row we place the totals or averages for the year.
        ##### NOTE!!! THIS CALCULATES AVERAGE CPUE! NOT "TRUE" CPUE BASED ON THE TOTALS
        dat[length(months)+1,names(dat)%in%sum.names]  <- colSums(dat[1:length(months),names(dat)%in%sum.names],na.rm=T)
        dat[length(months)+1,names(dat)%in%avg.names]  <- round(colMeans(dat[1:length(months),names(dat)%in%avg.names],na.rm=T),2)
        # get rownames for the totals too. fill in NA's if no fishing occured in that location that month.
        rownames(dat)[c+1] <- paste("Total",year[y],sep="-")
        if(!is.null(bank)) dat$bank[c+1] <- paste(unique(bank1),collapse="-")
        if(is.null(bank)) dat$bank[c+1] <- NA
        dat$nafo[c+1] <- paste(unique(nafo1),collapse="-")
        dat$month[c+1] <- "Total"
        dat$year[c+1] <- year[y]
        dat$fleet <- rep(fleet,length(dat[,1]))
        
        # Now round all the values in the table
        dat[, c("h", "hm", "crhm", "lbs", "kg", "mt")] <- apply(dat[, c("h", "hm", "crhm", "lbs", "kg", "mt")], 2, round)
        dat[, c("kg.h", "kg.hm", "kg.crhm")] <- apply(dat[, c("kg.h", "kg.hm", "kg.crhm")], 2, function(x) round(x, 2))
        
        # And put the data into a list
        month.list[[y]] <- dat
        
        # Now if we want to calculate this within the seed boxes...
        if(is.null(boxes) == F)
        {
          #Read1 First we bring in the seedbox data 
          sd.bx <- read.csv(paste(direct,"data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),header=T)
          # Make a data.frame for the box data
          dat.box <-data.frame(setNames(replicate(length(variables),numeric(length(months)+1), simplify = F), 
                                        variables))
          
          #First lets bring in the seedbox data of interest.
          if(boxes == "GB") seedboxes <- sd.bx[(sd.bx$Bank=="GBa" | sd.bx$Bank=="GBb" |sd.bx$Bank=="GB") &  sd.bx$Active =="Yes",]  
          if(boxes == "BB" ) seedboxes <- sd.bx[(sd.bx$Bank=="BBn" | sd.bx$Bank=="BBs" | sd.bx$Bank=="BB") &  sd.bx$Active =="Yes",]  
          if(boxes == "ALL" ) seedboxes <- sd.bx[sd.bx$Active =="Yes",]  
          
          # We will need to initialize a couple lists and determine how many boxes we are looking at
          # How many active seedboxes do we have?
          box.id <- unique(seedboxes$PID)
          num.boxes <- length(box.id)
          
          for(i in 1:num.boxes)
          {
            # seedboxes
            box <- seedboxes[seedboxes$PID==box.id[i],]
            names(box) <- c("PID","POS","x","y","ID","Bank","Closed","Open","Active","SCALLOP_Group_ID","SCALLOP_Group_ID_additional_names","comment")
            # This selects the data that is within our seedbox, we need to toss NA's for inout as that makes it die but I still love splancs inout function
            new.log.dat.box <- new.log.dat[with(new.log.dat[!is.na(new.log.dat$lon) & new.log.dat$year==year[y], ], 
                                                inout(cbind(lon,lat), box , bound = T)), ]
            c = 0 # reset the counter (used for cases when m isn't 1:12)
            # Run through each month of the year in specified region
            for(m in months)
            {
              c <- c + 1 # I need a counter for cases when m is not 1:12...
              if(!is.null(bank)) temp  <- new.log.dat.box[new.log.dat.box$nafo %in% nafo1 & new.log.dat.box$sfa %in% bank1 &
                                                            new.log.dat.box$month == m &
                                                            new.log.dat.box$vrnum %in% boats & new.log.dat.box$year == year[y],]
              
              if(is.null(bank)) temp  <- new.log.dat.box[new.log.dat.box$nafo %in% nafo1 &
                                                           new.log.dat.box$month == m &
                                                           new.log.dat.box$vrnum %in% boats & new.log.dat.box$year == year[y],]
              
              # Then we calculate the days each boat is out and then add these all up and you have the number of days the fleet was out                    
              days.by.boat <- tapply(temp$fished,temp$ves,function(x) length(unique(x)))
              dat.box$days[c] <- sum(days.by.boat,na.rm=T)
              
              # Here we calculate effort in hours, then hour-meters, then crew-hour-meters
              dat.box$h[c] <- round(sum(with(temp, numtow * avgtime / 60), na.rm = T))
              dat.box$hm[c] <- round(sum(with(temp, slip.dat[match(mdid, slip.dat$mdid),]$gear.ft 
                                              * 0.3048 * numrake * numtow * avgtime / 60), na.rm = T))
              dat.box$crhm[c] <- round(sum(with(temp, with(slip.dat[match(mdid, slip.dat$mdid),], 
                                                           gear.ft * 0.3048 * numshuck) * numrake * numtow * avgtime / 60), na.rm = T))
              
              # Catch in lbs then convert to kg and metric tonnes
              dat.box$lbs[c] <- round(with(temp,sum(pro.repwt * 2.2046, na.rm = T)))
              dat.box$kg[c] <- round(dat.box$lbs[c] / 2.2046)
              dat.box$mt[c] <- round(dat.box$kg[c] * 0.001)
              
              # Now calculate the various CPUE metrics, kg/hr, kg/(hr-m), kg/(hr-m-crew)
              dat.box$kg.h[c] <- round(dat.box$kg[c] / dat.box$h[c], 2)
              dat.box$kg.hm[c] <- round(dat.box$kg[c] / dat.box$hm[c], 2)
              dat.box$kg.crhm[c] <- round(dat.box$kg[c] / dat.box$crhm[c], 2)	
              
              # Toss in some other useful id type data.
              dat.box$month[c] <- old.dates[m]
              dat.box$bank[c] <-  ifelse(length(unique(temp$bank)) > 0,paste(unique(temp$bank),collapse="-"),NA)
              dat.box$nafo[c] <- ifelse(length(unique(temp$nafo)) > 0,paste(unique(temp$nafo),collapse="-"),NA)
              dat.box$year[c] <- year[y]
              dat.box$ID[c] <- as.character(box$ID)[1]
              # Add in information about the box here.
              rownames(dat.box)[c] <- paste(dat.box$month[c],year[y],dat.box$ID[c],sep="-")
            }	# end for(m in months)
            
            # Take the data and make annual totals, either by adding up the columns or taking the column average.
            dat.box[length(months)+1,names(dat.box)%in%sum.names] <- colSums(dat.box[1:length(months),names(dat.box)%in%sum.names],na.rm=T)
            dat.box[length(months)+1,names(dat.box)%in%avg.names] <- round(colMeans(dat.box[1:length(months),names(dat.box)%in%avg.names],na.rm=T),2)
            
            # get rownames for the totals too and tidy the columns in which there were no calculcations.
            rownames(dat.box)[c+1] <- paste("Total",year[y],dat.box$ID[1],sep="-")
            dat.box$ID[c+1] <-   as.character(box$ID)[1]
            if(!is.null(bank)) dat.box$bank[c+1] <- paste(unique(bank1),collapse="-")
            if(is.null(bank)) dat.box$bank[c+1] <- NA
            dat.box$nafo[c+1] <- paste(unique(nafo1),collapse="-")
            dat.box$month[c+1] <- "Total"
            dat.box$year[c+1] <- year[y]
            dat.box$fleet <- rep(fleet,length(dat.box[,1]))
            # Enter the data into list.
            box.list[[i]] <- dat.box
          } # end for(i in 1:num.boxes)
          # Unwrap the box.list by month into a dataframe
          box.temp <- do.call("rbind",box.list)
          # Then put it back into a list by year.
          box.list.all[[y]] <- box.temp
        } # end if(is.null(boxes) == F)
      }# end if year > 2008
    }# end for(y in 1:length(year))	
    
    
    # Let's unravel the box.list into a dataframe, if it exists that is.
    if(is.null(boxes) == F & max(year) > 2008) box.dat <- do.call("rbind",box.list.all)
    
    # same for the new and old data, a few if's needed here to take care of 3 scenarios
    if(max(year) <= 2008) dat <- do.call("rbind",old.list)
    if(min(year) > 2008) dat <- do.call("rbind",month.list)
    if(min(year) <=2008 & max(year) > 2008) 
    {
      dat.old <- do.call("rbind",old.list)
      dat.new <- do.call("rbind",month.list)
      dat <- rbind(dat.old,dat.new)
    } # end if we have old and new data..
    ### Print the results to screen? 
    if(print==T)
    {
      # First the bank chosen and the non-seedbox data
      if(!is.null(bank)) print(bank)
      if(!is.null(nafo1)) print(nafo1)
      print(dat)
      # if we have seedboxes print all of them as well.
      if(is.null(boxes) == F & max(year) > 2008)
      {
        for(i in 1:num.boxes)
        {
          print(box.list[[i]]$ID[1])
          print(box.list[[i]])
        } # end for(i in 1:num.boxes)
      } # end if(boxes==T)
    } # end if(print==T)
    
    # If we want to export these tables do so here
    if(export.tables==T)
    {
      # If we are just interested in the bank data and not nafo
      if(is.null(nafo.div) ==T)
      {
        #Write1
        if(length(year) == 1) write.table(dat,paste(direct,"data/fishery_data/CPUE/",max(year,na.rm=T),"/CPUE_tables_",
                                                    year,"_",paste(bank1,collapse="-"),"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
        #Write2                                        
        if(length(year) > 1) write.table(dat,paste(direct,"data/fishery_data/CPUE/",max(year,na.rm=T),"/CPUE_tables_",min(year,na.rm=T),"-",
                                                   max(year,na.rm=T),"_",bank,"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
        
        
        # if we requested seedboxes                                                               
        if(is.null(boxes) == F && max(year) > 2008) 
        {
          #Write3
          if(length(year) == 1) write.table(box.dat,paste(direct,"data/fishery_data/CPUE/",max(year,na.rm=T),"/CPUE_seedbox_tables_",
                                                          year,"_",paste(bank1,collapse="-"),"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
          #Write4
          if(length(year) > 1) write.table(box.dat,paste(direct,"data/fishery_data/CPUE/",max(year,na.rm=T),"/CPUE_seedbox_tables_",
                                                         min(year,na.rm=T),"-",max(year,na.rm=T),"_",bank,"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
        } # end  if(is.null(boxes) == F) 
      }# end  if(is.null(nafo.div)) 
      
      # Now if we have the potential for multiple nafo divisions this gets a little tricky
      if(is.null(nafo.div) ==F )
      {
        #Write5
        if(length(year) == 1) write.table(dat,paste(direct,"data/fishery_data/CPUE/",max(year,na.rm=T),"/CPUE_tables_",year,"_",paste(nafo1,collapse="-"),
                                                    paste(bank1,collapse="-"),"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
        #Write6
        if(length(year) > 1) write.table(dat,paste(direct,"data/fishery_data/CPUE/",max(year,na.rm=T),"/CPUE_tables_",min(year,na.rm=T),"-",
                                                   max(year,na.rm=T),"_",paste(nafo1,collapse="-"),paste(bank1,collapse="-"),
                                                   "_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
        
        # if we requested seedboxes                                                               
        if(is.null(boxes) == F && max(year) > 2008) 
        {
          #Write7
          if(length(year) == 1) write.table(box.dat,paste(direct,"data/fishery_data/CPUE/",max(year,na.rm=T),"/CPUE_seedbox_tables_",year,"_",
                                                          paste(nafo1,collapse="-"),paste(bank1,collapse="-"),"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
          #Write8
          if(length(year) > 1) write.table(box.dat,paste(direct,"data/fishery_data/CPUE/",max(year,na.rm=T),"/CPUE_seedbox_tables_",
                                                         min(year,na.rm=T),"-",max(year,na.rm=T),paste(nafo1,collapse="-"),
                                                         paste(bank1,collapse="-"),"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
        } # end  if(is.null(boxes) == F && max(year) > 2008) 
      }# end  if(is.null(nafo.div)) 
    }   # end if(export.tables==T)
    
    # finally do we want to return the results for use elsewhere
    if(output==T)
    {
      
      assign("CPUE.monthly", dat, pos=1)
      if(is.null(boxes) == F & max(year) > 2008) assign("CPUE.seedboxes.monthly",box.dat, pos = 1)
    } # end if(output==T)
  } #end if(CPUE == "month" || CPUE == "both")  	
  
  
########################################################## End Section 1 Monthly CPUE calculations ##########################################	
		
	
########################################################## Section 2 Observer trip CPUE calculations ##########################################	
	
# Now do the observer data caluclations, very straightforward if only one combination entered. I have 
# started a flat file with observer trip combinations in it from previous years.  We can source this file directly
# or enter a specific combination of data.
if(CPUE == "obs" || CPUE == "both")
{	
  # if we want the observer data but don't specify  the vessle AND the landing date then pull this from 
  # our local flat file.
  if(is.null(obs.vnum) ==T || is.null(obs.land.date) ==T)
  {
    #Read2 Grab our observer trip data
    obs.metadata <- list.files(paste0(direct_bycatch, "data"))
    obs.metadata <- obs.metadata[which(grepl(x=obs.metadata, pattern="Observed scallop trip metadata_2008-2018_tidy_"))]
    metadataruns <- gsub(x=obs.metadata, pattern="Observed scallop trip metadata_2008-2018_tidy_", "")
    metadataruns <- max(ymd(gsub(x=metadataruns, pattern=".csv", ""))) #### IT USES THE MOST RECENT METADATA RUN
    
    obs.trips <- read.table(paste0(direct_bycatch,"data/Observed scallop trip metadata_2008-2018_tidy_", metadataruns, ".csv"), sep = ",", header=T,stringsAsFactors = F)
    # What years are we looking at here
    obs.trips$year <- year(ymd(obs.trips$LANDING_DATE))
    obs.trips <- obs.trips[obs.trips$year %in% year,]
    obs.trips <- obs.trips[,-which(names(obs.trips) == "X")]
    # How many trips are there, if this is 0 then return an error.
    num.obs.trips <- length(obs.trips$CFV)
    if(num.obs.trips == 0) stop("Dang it!  There is no Observer data in that(those) year(s)")
    
    # match Jtrips to tripIDs
    tripnums <- NULL
    for (i in 1:length(unique(obs.trips$TRIP))){
      print(i)
      obs <- obs.trips[obs.trips$TRIP==unique(obs.trips$TRIP)[i],]
      
      vrnum <- new.log.dat[new.log.dat$vrnum == obs$CFV,]
      aftersail <- vrnum[vrnum$fished > obs$BOARD_DATE,]
      beforeland <- aftersail[aftersail$fished < obs$LANDING_DATE,] 
      tripnum <- unique(beforeland$tripnum)
      tripnums <- rbind(tripnums, data.frame(tripnum, trip=unique(obs.trips$TRIP)[i]))
    }
    
    ### warning messages are ok as long as:
    length(unique(tripnums$tripnum)) # is similar to:
    length(unique(tripnums$trip)) # Q3: off by one, which is ok because one of the observer trips has an A and B.
    table(tripnums$tripnum)
    table(tripnums$trip)
    
    ## join observer trip IDs to efforts
    new.log.dat <- join(new.log.dat, tripnums, type="left")
    new.log.dat.obs <- new.log.dat[!is.na(new.log.dat$trip),]
    
    obs.data <- NULL
    temp <- NULL
    count <- 0
    # Now I want to run a loop through these data and do the observer trip calculations
    for(i in 1:num.obs.trips)
    {
      temp <-  new.log.dat.obs[new.log.dat.obs$date.land == obs.trips$LANDING_DATE[i] & new.log.dat.obs$vrnum == obs.trips$CFV[i]  ,]
      
      temp <- ddply(.data=temp, .(year, date.land, mdid, ves, vrnum, nafo, sfa, bank, fleet),
                    summarize,
                    kg.trip = sum(pro.repwt, na.rm=T),
                    h.trip = sum(h, na.rm=T),
                    hm.trip = sum(hm, na.rm=T),
                    kgh.trip =  sum(pro.repwt,na.rm=T)/sum(h,na.rm=T),
                    kghm.trip = sum(pro.repwt,na.rm=T)/sum(hm, na.rm=T))
      
      # If the trip was just on 1 bank do this.
      if(length(unique(temp$bank)) ==1)
      {
        count <- count + 1
      temp$kg.trip <- sum(temp$pro.repwt,na.rm=T)
      temp$h.trip <- sum(temp$h,na.rm=T)
      temp$hm.trip <- sum(temp$hm,na.rm=T)
      temp$kgh.trip <- temp$kg.trip[1]/temp$h.trip[1]
      temp$kghm.trip <- temp$kg.trip[1]/temp$hm.trip[1]
      
      obs.data[[count]] <- with(temp, data.frame(year=year, date.land=as.Date(date.land,"%Y-%m-%d"),mdid=mdid,ves=ves,vrnum=vrnum,
                                             nafo=nafo,sfa=sfa,bank=bank,fleet=fleet,kg.trip = kg.trip,h.trip=h.trip,
                                             hm.trip=hm.trip,kgh.trip=kgh.trip,kghm.trip =kghm.trip,stringsAsFactors=F))[1,]
      }
      # To handle split trips we need to do this.
      if(length(unique(temp$bank)) >1)
      {
        # Loop through each bank fished for the split trip.
        for(j in 1:length(unique(temp$bank)))
        {
          count <- count + 1
          bnk <- unique(temp$bank)[j]
          temp2 <-  new.log.dat[new.log.dat$date.land == obs.trips$land.date[i] & new.log.dat$vrnum == obs.trips$V_Num[i] &
                                 new.log.dat$bank == bnk,]
          temp2$kg.trip <- sum(temp2$pro.repwt,na.rm=T)
          temp2$h.trip <- sum(temp2$h,na.rm=T)
          temp2$hm.trip <- sum(temp2$hm,na.rm=T)
          temp2$kgh.trip <- temp2$kg.trip[1]/temp2$h.trip[1]
          temp2$kghm.trip <- temp2$kg.trip[1]/temp2$hm.trip[1]
          if(max(temp$fished) == max(temp2$fished)) date.landed <-  temp$date.land[1]
          if(max(temp$fished) > max(temp2$fished)) date.landed <-   max(temp2$fished)
          obs.data[[count]] <- with(temp2, data.frame(year=year, date.land=date.landed,mdid=mdid,ves=ves,vrnum=vrnum,
                                                 nafo=nafo,sfa=sfa,bank=bank,fleet=fleet,kg.trip = kg.trip,h.trip=h.trip,
                                                 hm.trip=hm.trip,kgh.trip=kgh.trip,kghm.trip =kghm.trip,stringsAsFactors=F))[1,]
        }# end for(j in 1:length(unique(temp$bank)))
      }

                                                  
            
    } # END for(i in 1:num.obs.trips)
    obs.dat <- do.call("rbind",obs.data)

  } # end if(is.null(obs.vnum) ==T || is.null(obs.land.date) ==T)
    
  if(is.null(obs.vnum) == F && is.null(obs.land.date) ==F)
  {
    # Don't forget we want to subset this by nafo area too... something like... & new.log.dat$nafo =="5ZEJ"
    temp <- new.log.dat[new.log.dat$date.land == obs.land.date & new.log.dat$vrnum == obs.vnum  ,]
    if(length(temp[,1]) == 0) stop("Uh oh!  There is no observer data for that Landing Date - Vessel Number combination")
    temp <- ddply(.data=temp, .(year, date.land, mdid, ves, vrnum, nafo, sfa, bank, fleet),
          summarize,
          kg.trip = sum(pro.repwt, na.rm=T),
          h.trip = sum(h, na.rm=T),
          hm.trip = sum(hm, na.rm=T),
          kgh.trip =  sum(pro.repwt,na.rm=T)/sum(h,na.rm=T),
          kghm.trip = sum(pro.repwt,na.rm=T)/sum(hm, na.rm=T))
    
    obs.dat <- with(temp, data.frame(year=year, date.land=as.Date(date.land,"%Y-%m-%d"),mdid=mdid,ves=ves,vrnum=vrnum,
                                           nafo=nafo,sfa=sfa,bank=bank,fleet=fleet,kg.trip = kg.trip,h.trip=h.trip,
                                           hm.trip=hm.trip,kgh.trip=kgh.trip,kghm.trip =kghm.trip,stringsAsFactors=F))
  } #end if(is.null(obs.vnum) ==F && is.null(obs.land.date) ==F)
  
  # Export the results to a table
  if(obs.export == T)
  {
    #Write9
    if(length(obs.dat$year) == 1) write.table(obs.dat,paste(direct,"data/fishery_data/CPUE/Observer/Obs_effort_",
                                                            obs.dat$date.land,"_",obs.dat$vrnum,".csv",sep=""),sep=",",row.names=F)
    #Write10                                                
    if(length(obs.dat$year) > 1) write.table(obs.dat,paste(direct,"data/fishery_data/CPUE/Observer/Obs_effort_from_",
                                                     min(obs.dat$date.land,na.rm=T),"_to_",max(obs.dat$date.land,na.rm=T),
                                                     ".csv",sep=""),sep=",",row.names=F)
  } # end  if(obs.export == T)
  # Return the observer data for use elsewhere as necessary if output = T
  if(print==T) print(obs.dat)
  if(output==T) assign("obs.data",obs.dat,pos=1)
} # End if(CPUE == "obs" || CPUE == "both")
	
} # End fishmonth function
