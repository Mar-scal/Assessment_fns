# source("Y:/Assessment/2009/r/fn/get.bathy.r")


get.bathy <- function(area='custom',xl,yl){

# Function for connecting to ___________________ and
# Extracting subset of bathymetric data from _______________ 
# SQL created by Jerry Black
#		
#

	if(area=='custom'){
		
		
		# format Lat = dd-mm-ss.sssN  i.e. 42-30-00.000N
		Lat<-c()
		Long<-c()
		dd<-trunc(yl)
		mm<-trunc((yl-dd)*60)
		ss.sss<-round(((yl-dd)*60-mm)*60)

		for(i in 1:2){
			if(dd[i]>=10&&mm[i]>=10&&ss.sss[i]>=10)Lat[i]<-paste(dd[i],'-',mm[i],'-',ss.sss[i],'N',sep='')
			if(dd[i]>=10&&mm[i]>=10&&ss.sss[i]<10)Lat[i]<-paste(dd[i],'-',mm[i],'-0',ss.sss[i],'N',sep='')
			if(dd[i]>=10&&mm[i]<10&&ss.sss[i]<10)Lat[i]<-paste(dd[i],'-0',mm[i],'-0',ss.sss[i],'N',sep='')
			if(dd[i]>=10&&mm[i]<10&&ss.sss[i]>=10)Lat[i]<-paste(dd[i],'-0',mm[i],'-',ss.sss[i],'N',sep='')
		}
		
	
		# format Long = dd-mm-ss.sssW  i.e. 65-00-00.000W
		dd<-trunc(-xl)
		mm<-trunc((-xl-dd)*60)
		ss.sss<-as.character(round(((-xl-dd)*60-mm)*60))
		
		for(i in 1:2){
			if(dd[i]>=10&&mm[i]>=10&&ss.sss[i]>=10)Long[i]<-paste(dd[i],'-',mm[i],'-',ss.sss[i],'W',sep='')
			if(dd[i]>=10&&mm[i]>=10&&ss.sss[i]<10)Long[i]<-paste(dd[i],'-',mm[i],'-0',ss.sss[i],'W',sep='')
			if(dd[i]>=10&&mm[i]<10&&ss.sss[i]<10)Long[i]<-paste(dd[i],'-0',mm[i],'-0',ss.sss[i],'W',sep='')
			if(dd[i]>=10&&mm[i]<10&&ss.sss[i]>=10)Long[i]<-paste(dd[i],'-0',mm[i],'-',ss.sss[i],'W',sep='')
		}
	}

	require(RODBC) || stop("Package RODBC cannot be found")
	
  # This is no good, need this tidied up way to user specific!!  This should be working but isn't!
	chan <- odbcConnect("ptran", uid=un_DK, pwd =pw_DK)

	if(area=='custom')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '",Lat[1],"' and '",Lat[2],"' and y between '",Long[2],"' and '",Long[1],"';",sep='')
	if(area=='BBn')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '42-20-00.000N' and '43-00-00.000N' and y between '65-40-00.000W' and '66-30-00.000W';")
	if(area=='GB')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '41-00-00.000N' and '42-30-00.000N' and y between '65-00-00.000W' and '68-00-00.000W';")
	if(area=='Ger')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '42-50-00.000N' and '43-50-00.000N' and y between '65-00-00.000W' and '67-00-00.000W';")
	if(area=='Sab')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '42-30-00.000N' and '44-30-00.000N' and y between '58-30-00.000W' and '62-30-00.000W';")
	if(area=='WSS')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '41-30-00.000N' and '44-00-00.000N' and y between '64-00-00.000W' and '67-00-00.000W';")
	if(area=='ESS')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '43-00-00.000N' and '45-20-00.000N' and y between '57-00-00.000W' and '62-30-00.000W';")
	if(area=='SS')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '41-00-00.000N' and '46-00-00.000N' and y between '57-00-00.000W' and '68-00-00.000W';")
	if(area=='ALL')	qu.bathy <- paste("SELECT x,(to_number(substr(x,1,2)) + to_number(substr(x,4,2))/60. + to_number(substr(x,7,6))/3600.) * decode(substr(x,13,1),'S',-1,1) lat,    y,   (to_number(substr(y,1,2)) + to_number(substr(y,4,2))/60. + to_number(substr(y,7,6))/3600.) * decode(substr(y,13,1),'W',-1,1) lon,    z FROM mflib.chsdem_ind WHERE x between '20-00-00.000N' and '60-00-00.000N' and y between '30-00-00.000W' and '80-00-00.000W';")
	bathy1 <- sqlQuery(chan, qu.bathy)                       
	bathy <- with(bathy1, data.frame(lon = LON, lat = LAT, depth = Z)) 
	odbcCloseAll()
	bathy	
	#assign("bathy.dat", , pos = 1)
	
}





