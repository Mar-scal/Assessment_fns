# source("fn/get.inshore.survey.r")

### GETS INSHORE DATA FROM ORACLE DATABASE
### currently standardized live shell height frequency

get.inshore.survey <- function(){
	require(RODBC) || stop("Package RODBC cannot be found")
	chan <- odbcConnect("bank", "hubleyb", "p35mghk")
	qu.log <- "select * from SCALLSUR.SCLIVERES left join SCALLSUR.SCTOWS on SCALLSUR.SCLIVERES.CRUISE = SCALLSUR.SCTOWS.CRUISE and SCALLSUR.SCLIVERES.TOW_NO = SCALLSUR.SCTOWS.TOW_NO"
	log.dat <- sqlQuery(chan, qu.log)
	odbcCloseAll()
	log.dat
}

get.clappers <- function(){
	require(RODBC) || stop("Package RODBC cannot be found")
	chan <- odbcConnect("bank", "hubleyb", "p35mghk")
	qu.log <- "select * from SCALLSUR.SCDEADRES left join SCALLSUR.SCTOWS on SCALLSUR.SCDEADRES.CRUISE = SCALLSUR.SCTOWS.CRUISE and SCALLSUR.SCDEADRES.TOW_NO = SCALLSUR.SCTOWS.TOW_NO"
	log.dat <- sqlQuery(chan, qu.log)
	odbcCloseAll()
	log.dat
}

get.survey.defs <- function(){
	require(RODBC) || stop("Package RODBC cannot be found")
	chan <- odbcConnect("bank", "hubleyb", "p35mghk")
	qu.log <- "select * from SCALLSUR.SCSTRATAINFO"
	log.dat <- sqlQuery(chan, qu.log)
	odbcCloseAll()
	log.dat
}

get.repeated.tows <- function(){
	require(RODBC) || stop("Package RODBC cannot be found")
	chan <- odbcConnect("bank", "hubleyb", "p35mghk")
	qu.log <- "select * from SCALLSUR.SCREPEATEDTOWS"
	log.dat <- sqlQuery(chan, qu.log)
	odbcCloseAll()
	log.dat
}

get.growth.data <- function(){
	require(RODBC) || stop("Package RODBC cannot be found")
	chan <- odbcConnect("bank", "hubleyb", "p35mghk")
	qu.log <- "select * from SCALLSUR.SCWGTHGT"
	log.dat <- sqlQuery(chan, qu.log)
	odbcCloseAll()
	log.dat
}

#	require(RODBC) || stop("Package RODBC cannot be found")
#	chan <- odbcConnect("bank", "hubleyb", "p35mghk")
#	qu.log <- "select * from SCALLSUR.SCTOWTYPECODES"
#	log.dat <- sqlQuery(chan, qu.log)
#	odbcCloseAll()
#	log.dat
