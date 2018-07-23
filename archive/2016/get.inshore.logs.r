# DK August 13, 2015 Commented and checked by DK.  This function is used to get the INSHORE log and slip data from 
# the SCALLOP.SCALLOP_LOG_MARFIS table there are two versions of get.logs.r each with a different function name, but same filename. Yikes! 

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  ??
##  
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
# 
##
###############################################################################################################

##### ARGUMENTS ####################################################
# year:     The year from which to extract the data, "all" extracts the entire table
#	un:  your SQL username.  default = "un.ID" (if set in your r.Profile this will run automatically)
#	pw:  Your SQL password.  default is "pwd.ID"  (if set in your r.Profile this will run automatically)
# db.con:  Database to connect to.  Default is  "ptran"   
##### End ARGUMENTS ####################################################


# DK August 13, 2015 revised to make user enter their SQL information in the call. SImply SQL call to database + return data to parent function.
get.inshore.logs <- function(year='ALL',un= un.ID,pw= pwd.ID,db.con="ptran")
  {
	require(RODBC) || stop("Package RODBC cannot be found")
	chan <- odbcConnect(db.con,uid=un,pwd=pw)
	if(year!='ALL' && year > 2009)qu.log <- paste("select * from SCALLOP.SCALLOP_LOG_MARFIS where to_char(DATE_FISHED,'yy')='",year-2000,"'",sep="")
	if(year!='ALL'&& year <= 2009)qu.log <- paste("select * from SCALLOP.SCALLOP_LOG_MARFIS where to_char(DATE_FISHED,'yy')='","0",year-2000,"'",sep="")
	if(year=='ALL')qu.log <- "select * from SCALLOP.SCALLOP_LOG_MARFIS"
	log.dat <- sqlQuery(chan, qu.log)
	odbcCloseAll()
	log.dat
} # End Function

