#	source("Y:\\Fishery data\\r\\fn\\get.logs.r")


get.log <- function(year=2010,export=T, Win = T){

# Function for connecting to MARFIS commercial landings db
# Extract subset of info from Offshore Scallop Views created by Jerry Black
#		OFFSHORE_SCALLOP_LOG_2008
#		OFFSHORE_SCALLOP_SLIP_2008
#		
#		Created by Ian Jonsen 14Mar2008
#
	

	if(year>2007){

		require(RODBC) || stop("Package RODBC cannot be found")
	
		if(Win) chan <- odbcConnect("mflib", "jonseni", "dbmt587d", case="oracle")
		else if(Win == F){ 
			chan <- odbcConnect("jonseni2", "jonseni", "dbmt587d", case="oracle")
			}
		qu.slip <- paste("select * from marfis.P_OFFSHORE_SCALLOP_SLIP_2008 where DATE_SAILED like '%-08'")
		qu.log <- paste("select * from marfis.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-08'")
		slip1 <- sqlQuery(chan, qu.slip)
		log1 <- sqlQuery(chan, qu.log)
		boats.ft <- as.character(c("ATLANTIC DESTINY","ATLANTIC GUARDIAN","ATLANTIC LEADER","ATLANTIC PRESERVER","ATLANTIC PROTECTOR","CAPE LAHAVE","FORTUNE LADY"))
		boats.wet <- as.character(c("FREEDOM 99","LADY DENISE II","LADY kg.hmEAU II","LADY YVETTE II","CACHALOT I","BICKERTON PRIDE","G S MERSEY" ))
		
		slip <- with(slip1, data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME, tripnum = TRIP_NUMBER, sail = as.Date(DATE_SAILED), land = as.Date(LANDING_DATE_TIME), gear = GEAR_SIZE_FEET, numshuck = NUM_OF_CREW_SHUCKING, numcrew = NUM_OF_CREW, weight = SLIP_WEIGHT_LBS, grade = FISH_GRADE))
		
		# extra slip doc fields may be useful later
		# docnum = DOCUMENT_NO, confnum = CONF_NUMBER, tripid = TRIP_ID, vrnum = VR_NUMBER, hailin = HAIL_IN_CALL_ID, nafo = NAFO_UNIT_AREA, sfa = SCALLOP_FISHING_AREA, port = COMMUNITY_NAME, captain = CAPTAIN_NAME, company = EA_COMPANY, mate = MATES_NAME, buyer.ag = BUYER_AGENT_NAME, buyer = BUYER_NAME, lic.id = LICENCE_ID, g.slip.entry.date = GREATEST_SLIP_ENTRY_DATE, g.slip.change.date = GREATEST_SLIP_CHANGE_DATE
		
		log <- with(log1, data.frame(mdid = MON_DOC_ID, ves = VESSEL_NAME, tripid = TRIP_ID, fished = as.Date(DATE_FISHED), nafo = NAFO_UNIT_AREA, sfa = SCALLOP_FISHING_AREA, lon = LONGITUDE_DEG, lat = LATITUDE_DEG, depth = DEPTH_FM, bottom = BOTTOM_TYPE, watch = WATCH, numrake = NO_RAKES_FISHED, numtow = NO_TOWS_PER_WATCH, avgtime = AVG_TOW_TIME, pro.repwt = PRORATED_RPTD_WEIGHT_KGS, roeon = ROE_ON, numbags = NO_OF_BAGS)) 
		log$fleet <- as.factor(ifelse(log$ves %in% boats.ft,"FT","WF"))

		# extra log doc fields, may useful later
		# docnum = DOCUMENT_NO, confnum = CONF_NUMBER,vrnum = VR_NUMBER,licid = LICENCE_ID, hailin = HAIL_IN_CALL_ID, sfa = FISHING_AREA, elat = ENT_LATITUDE, elon = ENT_LONGITUDE, dmp = DMP_COMPANY_NAME, weight = WEIGHT, depth = DEPTH_FM, bottom = BOTTOM_TYPE, numbags = NO_OF_BAGS, comments = COMMENTS, rndwt = RW_KGS, catch.use = CATCH_USAGE, pro.rndwt = PRORATED_RND_WEIGHT_KGS, units = UNIT_OF_MEASURE, post.quota.flag = POST_TO_QUOTA_FLAG, g.slip.entry.date = GREATEST_SLIP_ENTRY_DATE, g.slip.change.date = GREATEST_SLIP_CHANGE_DATE, landform = LANDED_FORM
		
		assign("log.dat", log, pos = 1)
		assign("slip.dat", slip, pos = 1)
		if(export==T){
			write.table(log.dat, file = paste("Y:\\Fishery data\\Data\\log",year,".csv",sep=""),sep=",", row.names = F, col.names = T)
			write.table(slip.dat, file = paste("Y:\\Fishery data\\Data\\slip",year,".csv",sep=""),sep=",", row.names = F, col.names = T)
		}
		
		odbcCloseAll()
	}
	
	if(year<2008){
		source("T:\\Fishery data\\fn\\parse.log.r")
		log.file<-paste("T:\\Fishery data\\Data\\Logdata55-07\\log",year,".txt",sep="")
		log.fileFT<-paste("T:\\Fishery data\\Data\\Logdata55-07\\logFT",year,".txt",sep="")
		logWF<-parse.log(log.file,fleet="WF")
		logFT<-parse.log(log.fileFT,fleet="FT")

		logWF$fleet<-"WF"
		logFT$fleet<-"FT"
		
		log<-merge(logWF,logFT,all=T)
#		log$fleet<-as.factor(log$fleet)
		
		assign("log.dat", log, pos = 1)
		if(export==T){
			write.table(log.dat, file = paste("Y:\\Fishery data\\Data\\log",year,".csv",sep=""),sep=",", row.names = F, col.names = T)
		}
	}

}






