
# DK August 12, 2015 Commented and checked by DK.  This function is used to get the log and slip data from 
# the Marfis database.

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "CPUE.2015"
##  2:  FishMonth.2015.r
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
# 
##
###############################################################################################################



#	source("Y:\\Fishery data\\r\\fn\\get.marfis.logs.r")

### Gets Log and Slip data from Marfis database
# Function for connecting to MARFIS commercial landings db
# Extract subset of info from Offshore Scallop Views created by Jerry Black
#	OFFSHORE_SCALLOP_LOG_2008
#	OFFSHORE_SCALLOP_SLIP_2008
#		
#		Created by Ian Jonsen 14Mar2008
#   Updated by Dave Keith August 12 2015

##### ARGUMENTS ####################################################
# year:     The year from which to extract the data.  Default is the current Year according to your computer
# export:   Export the data to flat files
# get.data: Default is "dump" which extracts data from flat files already created.  If reading from the database choose "marfis"
#	un:  your SQL username.  default = "un.ID" (if set in your r.Profile this will run automatically)
#	pw:  Your SQL password.  default is "pwd.ID"  (if set in your r.Profile this will run automatically)
# db.con:  Database to connect to.  Default is  "ptran"   
##### End ARGUMENTS ####################################################
# Old Argument "Win":      This has been removed, it's utility is no longer required, I think it was used if running from a non-windows op-system


# DK August 12, 2015 added in arguments for using your own username/pw/connection for this.
get.marfis.logs <- function(year=as.numeric(format(Sys.Date(),"%Y")),export=F, Win = T, get.data='marfis',un=un.ID,pw=pwd.ID,db.con="ptran"){



# Open the marfis connection if get.data = "marfis"
	if(get.data=='marfis'){
		require(RODBC) || stop("Package RODBC cannot be found")

		# This "if(Win)" used to be in here, perhaps meant for determining in a windows machine vs Linux?  No notes so no idea
	  # Basically the odbc call changed based on that, but given it is user specific it is unnecesary.
		chan <- odbcConnect(db.con, uid=un, pwd= pw) # DK August 12, 2015, note also that this (case="oracle") should not work
		#else if(Win == F){ 
		#	chan <- odbcConnect("jonseni2", "jonseni", "dbmt587d", case="oracle")
		
		# Marfis database now marfissci, DK August 12, 2015
		# The "like %-" ignores first two digits and selects anything ending in the year of interest
		qu.slip <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-",year-2000,"'",sep="")
		qu.log <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-",year-2000,"'",sep="")
		# Now the actual database query
		slip1 <- sqlQuery(chan, qu.slip)
		log1 <- sqlQuery(chan, qu.log)
		odbcCloseAll() # close the database
	 

	} # end if(get.data=='marfis'){ 
	
  # If the data already exists in a flat file, get.data="dump" then grab the data from here.  Why is this directory "AMY"?
  # DK August 12, 2015 adjusted the read so that we can pick up any years data we want to look at
  if(get.data=='dump'){
	  #Read1
    log1 <-read.table(paste("D:/Offshore scallop/Amy/",year,"fisherydata/",year,"log.csv",sep=""),sep=",",header=T)
		#Read2
		slip1 <-read.table(paste("D:/Offshore scallop/Amy/",year,"fisherydata/",year,"slip.csv",sep=""),sep=",",header=T)
	} # end if(get.data=='dump'){

  # These are the offshore vessel numbers, ft = freezer trawler, wet is a wet boat (no freezer)
  # This file location will need adjusted.
  boat.types <- read.table("D:/Offshore/logs/Offshore_fleet.csv",sep=",",header=T)
  # select the freezer trawlers
  boats.ft <- subset(boat.types,Type== "FT")$ID
	# select the wet fish boats
  boats.wet <- subset(boat.types,Type== "WF")$ID
	
	# This removes columns/variables we do not need.
	slip <- with(slip1, data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME, vrnum = VR_NUMBER, tripnum = TRIP_NUMBER, sail = as.Date(DATE_SAILED,format="%d-%b-%y"), land = as.Date(LANDING_DATE_TIME,format="%d-%b-%y"), gear = GEAR_SIZE_FEET, numshuck = NUM_OF_CREW_SHUCKING, numcrew = NUM_OF_CREW, weight = SLIP_WEIGHT_LBS, grade = FISH_GRADE))
	
	# extra slip doc fields may be useful later
	# docnum = DOCUMENT_NO, confnum = CONF_NUMBER, tripid = TRIP_ID, hailin = HAIL_IN_CALL_ID, nafo = NAFO_UNIT_AREA, sfa = SCALLOP_FISHING_AREA, port = COMMUNITY_NAME, captain = CAPTAIN_NAME, company = EA_COMPANY, mate = MATES_NAME, buyer.ag = BUYER_AGENT_NAME, buyer = BUYER_NAME, lic.id = LICENCE_ID, g.slip.entry.date = GREATEST_SLIP_ENTRY_DATE, g.slip.change.date = GREATEST_SLIP_CHANGE_DATE
	
	# This removes columns/variables we do not need.
	log <- with(log1, data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME,vrnum = VR_NUMBER, tripid = TRIP_ID, fished = as.Date(DATE_FISHED,format="%d-%b-%y"), nafo = NAFO_UNIT_AREA, sfa = FISHING_AREA, lon = LONGITUDE_DEG, lat = LATITUDE_DEG, depth = DEPTH_FM, bottom = BOTTOM_TYPE, watch = WATCH, numrake = NO_RAKES_FISHED, numtow = NO_TOWS_PER_WATCH, avgtime = AVG_TOW_TIME, pro.repwt = PRORATED_RPTD_WEIGHT_KGS, roeon = ROE_ON, numbags = NO_OF_BAGS)) 
	# ID the boats as Wet or freezers
	log$fleet <- as.factor(ifelse(log$vrnum %in% boats.ft,"FT","WF"))

	# extra log doc fields, may useful later
	# docnum = DOCUMENT_NO, confnum = CONF_NUMBER,licid = LICENCE_ID, hailin = HAIL_IN_CALL_ID, sfa = FISHING_AREA, elat = ENT_LATITUDE, elon = ENT_LONGITUDE, dmp = DMP_COMPANY_NAME, weight = WEIGHT, depth = DEPTH_FM, bottom = BOTTOM_TYPE, numbags = NO_OF_BAGS, comments = COMMENTS, rndwt = RW_KGS, catch.use = CATCH_USAGE, pro.rndwt = PRORATED_RND_WEIGHT_KGS, units = UNIT_OF_MEASURE, post.quota.flag = POST_TO_QUOTA_FLAG, g.slip.entry.date = GREATEST_SLIP_ENTRY_DATE, g.slip.change.date = GREATEST_SLIP_CHANGE_DATE, landform = LANDED_FORM
	
	
	# This returns the log and slip information as log.dat and slip.dat to wherever we need it
	assign("log.dat", log, pos = 1)
	assign("slip.dat", slip, pos = 1)
	
	# This exports the data to flat files to avoid the SQL call.
	if(export==T){
	  #Write1
		write.table(log.dat, file = paste("D:/Fishery data/Data/log",year,".csv",sep=""),sep=",", row.names = F, col.names = T)
		#Write2 is this correct, shouldn't this be "slip" not log???  DK changed August 12, 2015
	  write.table(slip.dat, file = paste("D:/Fishery data/Data/slip",year,".csv",sep=""),sep=",", row.names = F, col.names = T)
	} # end if(export==T){
	
  
} # End get.marfis.logs.2015.r






