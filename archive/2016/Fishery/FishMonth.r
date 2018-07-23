# DK August 27, 2015 Commented and checked by DK.  # Function for calculating fishing effort on user specified bank by month
# and also for extracting observer data.

#####################################  File Summary ########################################################
####  
##  This function is used in these files (a.k.a. 'dependent files') 
##  1:  Catch.Effort.Tables.r
##  
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#  1:  logs_and_fishery_data(not actually used in this file, needed in Catch.Effort.Tables.2015)
##
###############################################################################################################



###############################################################################################################
# Agruments: 
# CPUE:     CPUE to calculate, CPUE by month/bank/nafo, Observer specific trip CPUE or both.  Default is "Month".  Options are "Month", "Obs", and "Both"
#           Arguments needed for Observer data start with "Obs."
# bank:     Which bank to select from using abbrevated bank id.  Default is GBa (Georges Bank A). 
#           The options are "GBa" = Georges a, "GBb" = Georges b, "BBn" = Browns north, "BBs" = Browns south, "GBBB" = all of Georges and Browns banks.
#           "GB" = all of Georges Bank, "BB" = all of Browns Bank, Ger" = German, "Mid" = Middle, "Sab" = Sable, "Ban" = Banquereau, and "SPB" = St. Pierre Bank
# year:     tthe year of interest.  Note that before 2009 we aren't using the "slip" data but old log data, the quality of which is an unknown.
# fleet:    ALL = total fleet, FT = freezer trawlers, WF = wet fishery		
# boxes:    Summarize the data for the seed boxes?  Default is NULL, other  options currently are "GB", "BB", "ALL"
#           
# print:    Print the results to screen?  T/F Default = T.
# output:   Return the results for use elsewhere, this returns a list to your default r workspace.  T/F Default = T
# nafo.div: The nafo division.  This needs to align with the Bank choice as well, what is produced is the amount landed on bank X nafo division y
#           if nafo division crosses multple banks this will only pull out part of NAFO division data.  Default is NULL.  This 
#           option won't work great for the data before 2008 without careful attention as these data have some differernt division names.
# export.logs:    Do you want to export the log and fishery data.  This does not include these monthly tables. (T/F) default is F  
#                 See logs_and_fishery.r for details
# export.tables:  Do you want to export the tables produced in this query.  (T/F), default is F
# months:         Select the months of interest.  Numerice, default is all months, c(1:12)
# Obs.vnum:       The vessel number for the observer trips, multiple trips are allowed BUT MUST BE PAIRED WITH APPROPRIATE Obs.land.date argument.  
#                 If CPUE = "Obs" or "Both" and this is = NULL this will read in a flat file with observer trip information in it. Default is NULL
# Obs.land.date:  The landing date of the Observer trip(s), multiple trips are allowed BUT THIS MUST BE PAIRED WITH APPROPRIATE Obs.vnum 
#                 If CPUE = "Obs" or "Both" and this is = NULL this will read in a flat file with observer trip information in it. Default is NULL
# Obs.export:     Export the observer CPUE data?  (T/F), default =F
#direct:          Directory to find the functions.  Default is "Y:/Offshore scallop/Assessment/")
#un:              your SQL username.  default = un.ID (if set in your r.Profile this will run automatically)
#pw:              Your SQL password.  default = pwd.ID  (if set in your r.Profile this will run automatically)
#db.con:          Database to connect to.  Default is  "ptran"   
###############################################################################################################


FishMonth <- function(CPUE = "Month", bank = "GBBB", year = as.numeric(format(Sys.Date(),"%Y")), fleet = "ALL", 
                      boxes=NULL,print=F,output=T, export.tables = "F", export.logs="F",months = c(1:12),nafo.div =NULL,
                      Obs.vnum = NULL, Obs.land.date = NULL, Obs.export=F,un=un.ID,pw=pwd.ID,db.con="ptran",
                      direct="Y:/Offshore scallop/Assessment/")
{

	require(splancs)  || stop("You need the package splancs... thanks")
  require(RODBC) || stop("Package RODBC cannot be found")
  source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
  
	#Source1 source("d:/R/fn/logs_and_fishery_data_DK.r") get data from logs and fish function
	logs_and_fish(loc="offshore",year=year,export=export.logs,un=un,pw=pwd,db.con=db.con)

	
	########################################################## Section 1 Monthly CPUE calculations ##########################################	
	
	# This large section is run if CPUE = "Month" or "Both", this calculates the monthly CPUE for different Banks.
  if(CPUE == "Month" || CPUE == "Both")
    {
    	# Here we initialize some variables that we need for later, kinda ugly eh...
    	variables <- as.character(c("year","month","bank","nafo","fleet","days", "h", "hm","crhm", "lbs",  "kg","mt","kg.h", "kg.hm","kg.crhm"))
    	month.lab <- as.character(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Total"))
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
    	# Need to treat GB differently as it is a combo of two banks...
    	if(bank != "GB" && bank != "GBBB" && bank != "BB")
    	  {
      	  nafo1 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank==bank]))
      	  bank1 <- as.character(unique(new.log.dat$sfa[new.log.dat$bank==bank]))
    	  } # end if(bank != "GB")
      	
    	# For all of Georges Bank we need to be more specific.
    	if(bank == "BB")
      	{
      	  # This works for GB as GBa overlaps the nafo boundaries...
      	  nafo.t1 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank=="BBn"]))
      	  nafo.t2 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank=="BBs"]))
      	  nafo1 <- c(nafo.t1,nafo.t2)
      	  bank1 <- c("26A","26B")
      	} # end if(bank == "GB")
      	
    	# For all of Georges Bank we need to be more specific.
    	if(bank == "GB")
    	  {
      	  # This works for GB as GBa overlaps the nafo boundaries...
          nafo1 <- as.character(unique(new.log.dat$nafo[new.log.dat$bank=="GBa"]))
          bank1 <- c("27A","27B")
    	  } # end if(bank == "GB")
    	# For all of Georges Bank we need to be more specific.
    	if(bank == "GBBB")
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
           
           for(m in months)
             {
              
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
               old.dat$days[m] <- sum(temp$day.fishing,na.rm=T)
               # Add up all the effort data for the month (all calculated for each cell already)
               old.dat$h[m] <- sum(temp$h,na.rm=T)
               old.dat$hm[m] <- sum(temp$hm, na.rm = T)
               old.dat$crhm[m] <- sum(temp$crhm, na.rm = T)
               
               # Catch in lbs then convert to kg and metric tonnes.
               old.dat$lbs[m] <- sum(temp$pro.repwt,na.rm=T) * 2.2046
               old.dat$kg[m] <- sum(temp$pro.repwt,na.rm=T)
               old.dat$mt[m] <- round(old.dat$kg[m] * 0.001)
               
               # Now calculate the various CPUE metrics, kg/hr, kg/(hr-m), kg/(hr-m-crew), I think I could take the averages from the cells but this 
               # seems a more correct way to do this...
               old.dat$kg.h[m] <- round(old.dat$kg[m] / old.dat$h[m], 2)
               old.dat$kg.hm[m] <- round(old.dat$kg[m] / old.dat$hm[m], 2)
               old.dat$kg.crhm[m] <- round(old.dat$kg[m] / old.dat$crhm[m], 2)	
               
               # Toss in some other useful id type data.
               old.dat$month[m] <- old.dates[m]
               old.dat$bank[m] <-  ifelse(length(unique(temp$bank)) > 0,paste(unique(temp$bank),collapse="-"),NA)
               old.dat$nafo[m] <- ifelse(length(unique(temp$nafo)) > 0,paste(unique(temp$nafo),collapse="-"),NA)
               old.dat$year[m] <- year[y]
               # Make the rownames an informative combination of month-year
               rownames(old.dat)[m] <- paste(old.dat$month[m],old.dat$year[m],sep="-")
               
             } # end for(m in months)
             
             # Now in the final row we place the totals or averages for the year.
             old.dat[length(months)+1,names(old.dat)%in%sum.names]  <- colSums(old.dat[1:length(months),names(old.dat)%in%sum.names],na.rm=T)
             dat[length(months)+1,names(old.dat)%in%avg.names]  <- round(colMeans(old.dat[1:length(months),names(old.dat)%in%avg.names],na.rm=T),2)
             # get rownames for the totals too. 
             rownames(old.dat)[m+1] <- paste("Total",old.dat$year[m],sep="-")
             old.dat$bank[m+1] <- paste(unique(old.bank),collapse="-")
             old.dat$nafo[m+1] <- paste(unique(old.nafo),collapse="-")
             old.dat$month[m+1] <- "Total"
             old.dat$year[m+1] <- year[y]
             old.dat$fleet <- rep(fleet,length(old.dat[,1]))
             
             # Toss the old data into a list
             old.list[[y]] <- old.dat
                    
        } # end if(year[y] <=2008) 
        
      # If the date is 2009 or later we use the slip data to make these monthly calculations
      if(year[y] > 2008)  
        {	
        
          # Make an object that is the first day of each month of the the current year + first day of next year.
          mdate <- seq(as.Date(paste(year[y],"-01-01",sep="")), length.out = 13, by = "1 month") # 
        	# Run through each month of the year in specified region
        	for(m in months)
      	    {
          	  # So this selects the nafo region we chose, within the correct fishing year, between the dates for the month of interest
          	  # for each boat of interest in the fleet.
              temp  <- new.log.dat[new.log.dat$nafo %in% nafo1 & new.log.dat$sfa %in% bank1 &
                                     new.log.dat$fished >= mdate[m] & new.log.dat$fished < mdate[m+1] & 
                                     new.log.dat$vrnum %in% boats & new.log.dat$year == year[y] ,]
              
              # Then we calculate the days each boat is out and then add these all up and you have the number of days the fleet was out                    
          		days.by.boat <- tapply(temp$fished,temp$ves,function(x) length(unique(x)))
          	  dat$days[m] <- sum(days.by.boat,na.rm=T)
        		
        		  # Here we calculate effort in hours, then hour-meters, then crew-hour-meters
        		  dat$h[m] <- round(sum(with(temp, numtow * avgtime / 60), na.rm = T))
          		dat$hm[m] <- round(sum(with(temp, slip.dat[match(mdid, slip.dat$mdid),]$gear.ft 
          		                            * 0.3048 * numrake * numtow * avgtime / 60), na.rm = T))
          		dat$crhm[m] <- round(sum(with(temp, with(slip.dat[match(mdid, slip.dat$mdid),], 
          		                              gear.ft * 0.3048 * numshuck) * numrake * numtow * avgtime / 60), na.rm = T))
          	
          		# Catch in lbs then convert to kg and metric tonnes
          		dat$lbs[m] <- round(with(temp,sum(pro.repwt * 2.2046, na.rm = T)))
          		dat$kg[m] <- round(dat$lbs[m] / 2.2046)
          		dat$mt[m] <- round(dat$kg[m] * 0.001)
        
          		# Now calculate the various CPUE metrics, kg/hr, kg/(hr-m), kg/(hr-m-crew)
          		dat$kg.h[m] <- round(dat$kg[m] / dat$h[m], 2)
          		dat$kg.hm[m] <- round(dat$kg[m] / dat$hm[m], 2)
          		dat$kg.crhm[m] <- round(dat$kg[m] / dat$crhm[m], 2)	
        		
          		# Toss in some other useful id type data.
          		dat$month[m] <- old.dates[m]
          		dat$bank[m] <-  ifelse(length(unique(temp$bank)) > 0,paste(unique(temp$bank),collapse="-",sep=""),NA)
          		dat$nafo[m] <- ifelse(length(unique(temp$nafo)) > 0,paste(unique(temp$nafo),collapse="-",sep=""),NA)
          		dat$year[m] <- year[y]
          		# Make the rownames an informative combination of month-year
          		rownames(dat)[m] <- paste(dat$month[m],dat$year[m],sep="-")
          		
          		
        	  } # end for(m in months)
          
        	# Now in the final row we place the totals or averages for the year.
        	dat[length(months)+1,names(dat)%in%sum.names]  <- colSums(dat[1:length(months),names(dat)%in%sum.names],na.rm=T)
        	dat[length(months)+1,names(dat)%in%avg.names]  <- round(colMeans(dat[1:length(months),names(dat)%in%avg.names],na.rm=T),2)
      
        	# get rownames for the totals too. fill in NA's if no fishing occured in that location that month.
        	rownames(dat)[m+1] <- paste("Total",dat$year[m],sep="-")
        	dat$bank[m+1] <- paste(unique(bank1),collapse="-")
        	dat$nafo[m+1] <- paste(unique(nafo1),collapse="-")
        	dat$month[m+1] <- "Total"
        	dat$year[m+1] <- year[y]
        	dat$fleet <- rep(fleet,length(dat[,1]))
        	
        	# And put the data into a list
        	month.list[[y]] <- dat
      	
        	# Now if we want to calculate this within the seed boxes...
        	if(is.null(boxes) == F)
        	  {
        	  #Read1 First we bring in the seedbox data 
        	  sd.bx <- read.table(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes.csv",sep=''),header=T,sep=",") 
        	  # Make a data.frame for the box data
        	  dat.box <-data.frame(setNames(replicate(length(variables),numeric(length(months)+1), simplify = F), 
        	                            variables))
        	  
        	  #First lets bring in the seedbox data of interest.
        	  if(boxes == "GB") seedboxes <- sd.bx[(sd.bx$Bank=="GBa" | sd.bx$Bank=="GBb" |sd.bx$Bank=="GBa") &  sd.bx$Active =="Y",]  
        	  if(boxes == "BB" ) seedboxes <- sd.bx[(sd.bx$Bank=="BBn" | sd.bx$Bank=="BBs" | sd.bx$Bank=="BB") &  sd.bx$Active =="Y",]  
        	  if(boxes == "ALL" ) seedboxes <- sd.bx[sd.bx$Active =="Y",]  
        	    
        	  # We will need to initialize a couple lists and determine how many boxes we are looking at.
        	  
        	  # How many active seedboxes do we have?
        	  box.id <- unique(seedboxes$PID)
        	  num.boxes <- length(box.id)
            
        		for(i in 1:num.boxes)
        		  {
          		  # seedboxes
          		  box <- seedboxes[seedboxes$PID==box.id[i],]
          		  
          		  # This selects the data that is within our seedbox, we need to toss NA's for inout as that makes it die but I still love splancs inout function
          		  new.log.dat.box <- new.log.dat[with(new.log.dat[!is.na(new.log.dat$lon) & new.log.dat$year==year[y], ], inout(cbind(lon,lat), box , bound = T)), ]
          		  
        			  for(m in months)
        			    {
        				
          			    temp  <- new.log.dat.box[new.log.dat.box$nafo %in% nafo1 & new.log.dat.box$sfa %in% bank1 &
          			                           new.log.dat.box$fished >= mdate[m] & new.log.dat.box$fished < mdate[m+1] &
          			                           new.log.dat.box$vrnum %in% boats & new.log.dat.box$year == year[y],]
          			    
          			    # Then we calculate the days each boat is out and then add these all up and you have the number of days the fleet was out                    
          			    days.by.boat <- tapply(temp$fished,temp$ves,function(x) length(unique(x)))
          			    dat.box$days[m] <- sum(days.by.boat,na.rm=T)
          			    
          			    # Here we calculate effort in hours, then hour-meters, then crew-hour-meters
          			    dat.box$h[m] <- round(sum(with(temp, numtow * avgtime / 60), na.rm = T))
          			    dat.box$hm[m] <- round(sum(with(temp, slip.dat[match(mdid, slip.dat$mdid),]$gear.ft 
          			                                * 0.3048 * numrake * numtow * avgtime / 60), na.rm = T))
          			    dat.box$crhm[m] <- round(sum(with(temp, with(slip.dat[match(mdid, slip.dat$mdid),], 
          			                                             gear.ft * 0.3048 * numshuck) * numrake * numtow * avgtime / 60), na.rm = T))
          			    
          			    # Catch in lbs then convert to kg and metric tonnes
          			    dat.box$lbs[m] <- round(with(temp,sum(pro.repwt * 2.2046, na.rm = T)))
          			    dat.box$kg[m] <- round(dat.box$lbs[m] / 2.2046)
          			    dat.box$mt[m] <- round(dat.box$kg[m] * 0.001)
          			    
          			    # Now calculate the various CPUE metrics, kg/hr, kg/(hr-m), kg/(hr-m-crew)
          			    dat.box$kg.h[m] <- round(dat.box$kg[m] / dat.box$h[m], 2)
          			    dat.box$kg.hm[m] <- round(dat.box$kg[m] / dat.box$hm[m], 2)
          			    dat.box$kg.crhm[m] <- round(dat.box$kg[m] / dat.box$crhm[m], 2)	
        			  
          			 
          			    # Toss in some other useful id type data.
          			    dat.box$month[m] <- old.dates[m]
          			    dat.box$bank[m] <-  ifelse(length(unique(temp$bank)) > 0,paste(unique(temp$bank),collapse="-"),NA)
          			    dat.box$nafo[m] <- ifelse(length(unique(temp$nafo)) > 0,paste(unique(temp$nafo),collapse="-"),NA)
          			    dat.box$year[m] <- year[y]
          			    dat.box$ID[m] <- as.character(box$ID)[1]
          			    # Add in information about the box here.
          			    rownames(dat.box)[m] <- paste(dat.box$month[m],dat.box$year[m],dat.box$ID[m],sep="-")
          			 }	# end for(m in months)
       
          		      # Take the data and make annual totals, either by adding up the columns or taking the column average.
              		  dat.box[length(months)+1,names(dat.box)%in%sum.names]  <- colSums(dat.box[1:length(months),names(dat.box)%in%sum.names],na.rm=T)
              		  dat.box[length(months)+1,names(dat.box)%in%avg.names]  <- round(colMeans(dat.box[1:length(months),names(dat.box)%in%avg.names],na.rm=T),2)
         
              		  # get rownames for the totals too and tidy the columns in which there were no calculcations.
              		  rownames(dat.box)[m+1] <- paste("Total",dat.box$year[y],dat.box$ID[i],sep="-")
              		  dat.box$ID[m+1] <-   as.character(box$ID)[1]
              		  dat.box$bank[m+1] <- paste(unique(bank1),collapse="-")
              		  dat.box$nafo[m+1] <- paste(unique(nafo1),collapse="-")
              		  dat.box$month[m+1] <- "Total"
              		  dat.box$year[m+1] <- year[y]
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
      		print(bank)
      		print(dat.box)
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
      	    if(length(year) == 1) write.table(dat,paste(direct,"Data/Fishery_data/CPUE/CPUE_tables_",year,"_",paste(bank1,collapse="-"),
      	                                                "_fleet-",fleet,".csv",sep=""),sep=",",row.names=F) 
            #write2      	                                                
      	    if(length(year) > 1) write.table(dat,paste(direct,"Data/Fishery_data/CPUE/CPUE_tables_",min(year,na.rm=T),"-",
      	                                             max(year,na.rm=T),"_",bank,"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
      	      
      	      
      	      # if we requested seedboxes                                                               
      	        if(is.null(boxes) == F && max(year) > 2008) 
      	          {
      	            #write3
      	            if(length(year) == 1) write.table(box.dat,paste(direct,"Data/Fishery_data/CPUE/CPUE_seedbox_tables_",
      	                                                        year,"_",paste(bank1,collapse="-"),"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
      	            #write4
      	            if(length(year) > 1) write.table(box.dat,paste(direct,"Data/Fishery_data/CPUE/CPUE_seedbox_tables_",
      	                                         min(year,na.rm=T),"-",max(year,na.rm=T),"_",bank,"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
      	          } # end  if(is.null(boxes) == F) 
      	      }# end  if(is.null(nafo.div)) 
    	    
      	    # Now if we have the potential for multiple nafo divisions this gets a little tricky
    	      if(is.null(nafo.div) ==F )
      	      {
    	          #Write5
    	          if(length(year) == 1) write.table(dat,paste(direct,"Data/Fishery_data/CPUE/CPUE_tables_",year,"_",paste(nafo1,collapse="-"),
    	                                                      paste(bank1,collapse="-"),"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
    	          #Write6
        	      if(length(year) > 1) write.table(dat,paste(direct,"Data/Fishery_data/CPUE/CPUE_tables_",min(year,na.rm=T),"-",
        	                                                 max(year,na.rm=T),"_",paste(nafo1,collapse="-"),paste(bank1,collapse="-"),
        	                                                 "_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
        	                                       
        	     # if we requested seedboxes                                                               
        	     if(is.null(boxes) == F && max(year) > 2008) 
        	       {
        	         #write7
        	         if(length(year) == 1) write.table(box.dat,paste(direct,"Data/Fishery_data/CPUE/CPUE_seedbox_tables_",year,"_",paste(nafo1,collapse="-"),
        	                                                     paste(bank1,collapse="-"),"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
        	        #write8 
        	        if(length(year) > 1) write.table(box.dat,paste(direct,"Data/Fishery_data/CPUE/CPUE_seedbox_tables_",
        	                                          min(year,na.rm=T),"-",max(year,na.rm=T),paste(nafo1,collapse="-"),
        	                                          paste(bank1,collapse="-"),"_fleet-",fleet,".csv",sep=""),sep=",",row.names=F)
      	          } # end  if(is.null(boxes) == F) 
      	       }# end  if(is.null(nafo.div)) 
    	     }   # end if(export.tables==T)
    	      
    	# finally do we want to return the results for use elsewhere
    	if(output==T)
    	  {
    		
    		assign("CPUE.monthly", dat, pos=1)
    		if(is.null(boxes) == F & max(year) > 2008) assign("CPUE.seedboxes.monthly",box.dat, pos = 1)
    		} # end if(output==T)
  } #end if(CPUE == "Month" || CPUE == "Both")  	
  
	
########################################################## End Section 1 Monthly CPUE calculations ##########################################	
		
	
	########################################################## Section 2 Observer trip CPUE calculations ##########################################	
	
# Now do the observer data caluclations, very straightforward if only one combination entered. I have 
# started a flat file with observer trip combinations in it from previous years.  We can source this file directly
# or enter a specific combination of data.
  if(CPUE == "Obs" || CPUE == "Both")
    {	
    # if we want the Observer data but don't specify  the vessle AND the landing date then pull this from 
    # our local flat file.
    if(is.null(Obs.vnum) ==T || is.null(Obs.land.date) ==T)
      {
        # Grab our Observer trip data
        obs.trips <- read.table(paste(direct,"Data/Observer_trips.csv",sep=""),sep=",",header=T,stringsAsFactors = F)
        # What years are we looking at here
        obs.trips <- obs.trips[obs.trips$year %in% year,]
        # How many trips are there, if this is 0 then return an error.
        num.obs.trips <- length(obs.trips$V_Num)
        if(num.obs.trips == 0) stop("Dang it!  There is no Observer data in that(those) year(s)")
        
        #obs.names <- c("year","date.land","mdid","ves","vrnum","nafo","sfa","bank","pro.repwt","fleet")
        obs.data <- NULL
        temp <- NULL
        # Now I want to run a loop through these data and do the Observer trip calculations
        for(i in 1:num.obs.trips)
          {
            temp <-  new.log.dat[new.log.dat$date.land == obs.trips$land.date[i] & new.log.dat$vrnum == obs.trips$V_Num[i]  ,]
            temp$kg.trip <- sum(temp$pro.repwt,na.rm=T)
            temp$h.trip <- sum(temp$h,na.rm=T)
            temp$hm.trip <- sum(temp$hm,na.rm=T)
            temp$kgh.trip <- temp$kg.trip[1]/temp$h.trip[1]
            temp$kghm.trip <- temp$kg.trip[1]/temp$hm.trip[1]
            
            obs.data[[i]] <- with(temp, data.frame(year=year, date.land=as.Date(date.land,"%Y-%m-%d"),mdid=mdid,ves=ves,vrnum=vrnum,
                                                  nafo=nafo,sfa=sfa,bank=bank,fleet=fleet,kg.trip = kg.trip,h.trip=h.trip,
                                                  hm.trip=hm.trip,kgh.trip=kgh.trip,kghm.trip =kghm.trip,stringsAsFactors=F))[1,]
                                                  
            
          } # END for(i in 1:num.obs.trips)
        obs.dat <- do.call("rbind",obs.data)
        
       } # end if(is.null(Obs.vnum) ==T || is.null(Obs.land.date) ==T)
    
    
    if(is.null(Obs.vnum) == F && is.null(Obs.land.date) ==F)
      {
        # Don't forget we want to subset this by nafo area too... something like... & new.log.dat$nafo =="5ZEJ"
        temp <- new.log.dat[new.log.dat$date.land == Obs.land.date & new.log.dat$vrnum == Obs.vnum  ,]
        if(length(temp[,1]) == 0) stop("Uh oh!  There is no observer data for that Landing Date - Vessel Number combination")
        temp$kg.trip <- sum(temp$pro.repwt,na.rm=T)
        temp$h.trip <- sum(temp$h,na.rm=T)
        temp$hm.trip <- sum(temp$hm,na.rm=T)
        temp$kgh.trip <- temp$kg.trip[1]/temp$h.trip[1]
        temp$kghm.trip <- temp$kg.trip[1]/temp$hm.trip[1]
        
        obs.dat <- with(temp, data.frame(year=year, date.land=as.Date(date.land,"%Y-%m-%d"),mdid=mdid,ves=ves,vrnum=vrnum,
                                               nafo=nafo,sfa=sfa,bank=bank,fleet=fleet,kg.trip = kg.trip,h.trip=h.trip,
                                               hm.trip=hm.trip,kgh.trip=kgh.trip,kghm.trip =kghm.trip,stringsAsFactors=F))[1,]
      } #end if(is.null(Obs.vnum) ==F && is.null(Obs.land.date) ==F)
  
    # Export the results to a table
    if(Obs.export == T)
      {
      #Write9
      if(length(obs.dat$year) == 1) write.table(obs.dat,paste(direct,"Data/Fishery_data/CPUE/Observer/Obs_effort_",
                                                              obs.dat$date.land,"_",obs.dat$vrnum,".csv",sep=""),sep=",",row.names=F)
      #Write10                                                
      if(length(obs.dat$year) > 1) write.table(obs.dat,paste(direct,"Data/Fishery_data/CPUE/Observer/Obs_effort_from_",
                                                     min(obs.dat$date.land,na.rm=T),"_to_",max(obs.dat$date.land,na.rm=T),
                                                     ".csv",sep=""),sep=",",row.names=F)
      } # end  if(Obs.export == T)
  # Return the Observer data for use elsewhere as necessary.
	assign("obs.data",obs.dat,pos=1)
  } # End if(CPUE == "Obs" || CPUE == "Both")
	
} # End fishMonth function
