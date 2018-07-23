# DK July 31, 2015:  ####  Commented and checked by DK 
# Function for connecting to ___________________ and
# Extracting subset of bathymetric data from _______________ 
# SQL created by Jerry Black
#	Revision history
# May 16, 2016, revised to enable RODBC to work with R-64 bit. (DK)
#
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "ScallopMap.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#      
#      
##
###############################################################################################################
#############  Arguments ####################


#	area: Choose area corresponding to a list, generally this is done in a call to a parent function.  If  'custom' where xlim & ylim are user specified in ScallopMap
# db.con:  SQL database connection name, user specific, default is ptran
# un:  Username for your SQL call, please set this up in your R-profile and do not enter it directly into the function
# pwd:  Password for your SQL call, please set this up in your R-profile and do not enter it directly into the function 
# xl: Range of longitude data to obtain 
#	yl: Range of latitude data to obtain
# db.con:   The database to connect to.  Default ="ptran",
# un:       Your username to connect to SQL database.  Default = un.ID
# pw:       Your password to connect to SQL database.  Default = pwd.ID
# source("Y:/Assessment/2009/r/fn/get.bathy.r")


############################  Section 1 - Function ##########################################################

get.bathy <- function(area='custom',db.con="ptran",un = un.ID, pw = pwd.ID,xl,yl){

  require(RODBC) || stop("Package RODBC cannot be found")

	if(area=='custom'){
		
		
		# This gets the Latitude into proper format for the call to the SQL database.  
	  #format Lat = dd-mm-ss.sssN  i.e. 42-30-00.000N
		
	  # Initialize/create objects for the for loop
	  Lat<-c()
		Long<-c()
		
		#  Preprocessing to get Latitude components into proper dd, mm, and ss.sss format rather than decimal degrees. 
		dd<-trunc(yl)
		mm<-trunc((yl-dd)*60)
		ss.sss<-round(((yl-dd)*60-mm)*60)

		# Now create the Latitude in the proper format.  Loop runs from 1:2 because we have min/max latitude data.
		# The if statements are ensuring the the minutes and seconds have a 0 in front of them.
		
		for(i in 1:2){
			if(dd[i]>=10&&mm[i]>=10&&ss.sss[i]>=10)Lat[i]<-paste(dd[i],'-',mm[i],'-',ss.sss[i],'N',sep='')
			if(dd[i]>=10&&mm[i]>=10&&ss.sss[i]<10)Lat[i]<-paste(dd[i],'-',mm[i],'-0',ss.sss[i],'N',sep='')
			if(dd[i]>=10&&mm[i]<10&&ss.sss[i]<10)Lat[i]<-paste(dd[i],'-0',mm[i],'-0',ss.sss[i],'N',sep='')
			if(dd[i]>=10&&mm[i]<10&&ss.sss[i]>=10)Lat[i]<-paste(dd[i],'-0',mm[i],'-',ss.sss[i],'N',sep='')
		} # end for(i in 1:2){
		
		# This gets the Longitude into proper format for the call to the SQL database.  
		# format Long = dd-mm-ss.sssW  i.e. 65-00-00.000W
		
		#  Preprocessing to get Longitude components into proper dd, mm, and ss.sss format rather than decimal degrees. 
		# Note we flip the sign in each step here as SQL has longitudes in as positive values.
		dd<-trunc(-xl)
		mm<-trunc((-xl-dd)*60)
		ss.sss<-as.character(round(((-xl-dd)*60-mm)*60))
		
		
		# Now create the Longitude in the proper format.  Loop runs from 1:2 because we have min/max Longitude data.
		# The if statements are ensuring the the minutes and seconds have a 0 in front of them.
		for(i in 1:2){
			if(dd[i]>=10&&mm[i]>=10&&ss.sss[i]>=10)Long[i]<-paste(dd[i],'-',mm[i],'-',ss.sss[i],'W',sep='')
			if(dd[i]>=10&&mm[i]>=10&&ss.sss[i]<10)Long[i]<-paste(dd[i],'-',mm[i],'-0',ss.sss[i],'W',sep='')
			if(dd[i]>=10&&mm[i]<10&&ss.sss[i]<10)Long[i]<-paste(dd[i],'-0',mm[i],'-0',ss.sss[i],'W',sep='')
			if(dd[i]>=10&&mm[i]<10&&ss.sss[i]>=10)Long[i]<-paste(dd[i],'-0',mm[i],'-',ss.sss[i],'W',sep='')
		} # end for(i in 1:2){
	} # End if(area=='custom'){

	
	
  # This is no good, need this tidied up.  I can't open this channel as I connect to ptran not mflib, is this user specific?
  # chan <- odbcConnect("mflib", "hubleyb", "p35mghk")

	chan <- odbcConnect(db.con, uid=un, pwd = pw,believeNRows=FALSE)
  
	# Select the correct query based on area
	if(area=='custom')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '",Lat[1],"' and '",Lat[2],"' and y between '",Long[2],"' and '",Long[1],"';",sep='')
	if(area=='BBn')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '42-20-00.000N' and '43-00-00.000N' and y between '65-40-00.000W' and '66-30-00.000W';")
	if(area=='GB')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '41-00-00.000N' and '42-30-00.000N' and y between '65-00-00.000W' and '68-00-00.000W';")
	if(area=='Ger')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '42-50-00.000N' and '43-50-00.000N' and y between '65-00-00.000W' and '67-00-00.000W';")
	if(area=='Sab')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '42-30-00.000N' and '44-30-00.000N' and y between '58-30-00.000W' and '62-30-00.000W';")
	if(area=='WSS')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '41-30-00.000N' and '44-00-00.000N' and y between '64-00-00.000W' and '67-00-00.000W';")
	if(area=='ESS')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '43-00-00.000N' and '45-20-00.000N' and y between '57-00-00.000W' and '62-30-00.000W';")
	if(area=='SS')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '41-00-00.000N' and '46-00-00.000N' and y between '57-00-00.000W' and '68-00-00.000W';")
	# Note there is no "ALL" option in ScallopMap, perhaps used in a different function or might be obsolete.
	if(area=='ALL')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '20-00-00.000N' and '60-00-00.000N' and y between '30-00-00.000W' and '80-00-00.000W';")
	
	# Send the query, currently the table/view I've requested either doesn't exist or I just don't have access to it (July 31, 2015)
	bathy1 <- sqlQuery(chan, qu.bathy)                       
	# Take query result and modify it.
	bathy <- with(bathy1, data.frame(lon = LON, lat = LAT, depth = Z)) 
	# close the odbc connection
	odbcCloseAll()
	# Return the bathy object
	assign("bathy",bathy,pos=1)	
	#assign("bathy.dat", , pos = 1)
	
} # End function





