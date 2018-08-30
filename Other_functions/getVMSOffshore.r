###....................................................###
###  RETREIVE VMS BY YEAR AS FUNCTION OF LIST OF VRNs  ###
###  J. Sameoto										   ###
###  March 7, 2014									   ###		  
###....................................................###

	library(RODBC)
	UN <- "hubleyb"
	PW <- "p35mghk"
	RODBCconn <- odbcConnect("bank", uid=UN , pwd=PW)

#..................#
# Define Variables #
#..................#
	startdate <- 19990101   # Start DateTime 
	stopdate <- 19991231    # Stop DateTime 	

# Define List of VRNs from Monitoring Documents #
	vrn.list <- c(105736,105912,105457,106604,106605,1555,4062,102056,100199,4055,152320,101965,4031,1516,1518,4051,1548,5409,105457,2320,4007,4051,4055,4146,4157,4211,4233,4264,4289,4293,4304,4305,4321,4356,4378,4394,4432,4498,4536,4596,4599,4600,4642,4677,4729,4762,5000,5001,5002,5003,5004,5005,5457,5736,5912,6604,6605)
	vrn.list <- paste(vrn.list,collapse="','")
	
#............#	 
# Select VMS #	
#............#	
	begTime <- Sys.time()
	quer2 <- paste(
		"SELECT rownum vesid, p.lon, p.lat, NVL(v.vessel_name,p.vrn) vessel_name, p.vrn, 'V_'||p.vrn vr_number, ",
			"TO_CHAR(p.pdate,'YYYY') YEAR, TO_CHAR(p.pdate,'YYYY-MM-DD') vmsdate, p.pdate vmstime, ",
			"p.hailout, to_number(TO_CHAR(to_number(TO_CHAR(pdate,'J')) + to_number(TO_CHAR(pdate,'HH24'))/24.,'99999999.999999')) julian_date, ",
			"p.speed_knots, 1 obs,",
			"v.vessel_name || ' '|| p.vrn || ' (LOA ' || v.loa || ') '|| TO_CHAR(p.pdate,'YYYY/MM/DD HH24:MI')|| ' ' || speed_knots ves_id, ",
			"'http://foip.mar.dfo-mpo.ca/pls/foip/foip$lic_vessels.p_vrn:1in_vrn=' || p.vrn licence_href ",
		"FROM mfd_obfmi.vms_pos p,                         ",
		"	  mfd_obfmi.marfis_vessels_syn v               ",
		"WHERE                                             ",
		"	  p.vrn = v.vr_number(+)                       ",
		"AND  p.vrn IN ('",vrn.list,"')                    ",
		"AND p.pdate >= to_date(", startdate, ",'YYYYMMDD')",
		"AND p.pdate <= to_date(", stopdate, ",'YYYYMMDD') ",
		"AND p.lon BETWEEN -85 AND 180                     ", 
		"AND p.lat < 90                                    ", 
		sep=""
	  )   
	dat <- sqlQuery(RODBCconn, quer2, believeNRows=FALSE)  
	runTime <- Sys.time()-begTime

	names(dat) <- tolower(names(dat))
	dat$vmstime <- as.POSIXct(dat$vmstime)
	dat$vmsdate <- as.POSIXct(dat$vmsdate)
	dat <- dat[order(dat$vrn, dat$vmstime), ]  # Order dataframe by vrn and DateTime 

	paste("Number of VMS records: ",dim(dat)[1],sep="")
	
#........................................#
# Extract unique rows/ Remove duplicates # 
#........................................#
	tot.dat <- dim(dat)[1]
	dat <- dat[!duplicated(dat),] #removes any rows that are fully duplicated
	dups <- dat[duplicated(dat[,c(5,9)]),]
	dups <- dups[order(dups[5],dups[9]),]
	dat <- dat[!duplicated(dat[,c(5,9)]),] # removes any rows where vrn(col.5) and vmstime(col.6) are duplicated
	dups.rm <- tot.dat - dim(dat)[1]
	paste("Number of row removed:", dups.rm, sep=" ")

write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2012.csv",row.names=F)
write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2011.csv",row.names=F)

write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2010.csv",row.names=F)

write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2009.csv",row.names=F)
head(dat)

write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2008.csv",row.names=F)
write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2007.csv",row.names=F)
write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2006.csv",row.names=F)
write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2005.csv",row.names=F)
write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2004.csv",row.names=F)
write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2003.csv",row.names=F)
write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2002.csv",row.names=F)
write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2001.csv",row.names=F)
write.csv(dat,"C:/Assessment/2014/r/data/VMS/OffshoreVMS2000.csv",row.names=F)

