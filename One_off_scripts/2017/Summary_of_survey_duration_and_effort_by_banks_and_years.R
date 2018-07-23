## A quick script to look at the survey catches over time for each bank, + 

yr <- 2017
direct <- "d:/r/"
require(ROracle)
source(paste0(direct,".Rprofile"))
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.Rdata",sep="")) 
# I also want to grab all the data from teh OSCRUISES Table which provides the start and end dates of the survey.

# Change this to a Roracle based query!!

chan <- odbcConnect(db.con, uid = un.ID,pwd = pwd.ID,believeNRows=FALSE)

#####################################################################################################################
# Jessica has new views for these calls, ,all this prorating is not necessary anymore as she's taken care of it in SQL
# Key is to import those tables and send it out of this file looking identical!  
######################################################################################################################

# DK Oct 29, 2015, don't need tow data either, we don't ever use it....
qu.cruise <- "select * from SCALOFF.OSCRUISES"
#qu.tow <- "select * from SCALOFF.OSTOWS"
# Grab the SQL data from the respective database tables
#strata <- sqlQuery(chan, qu.strata)
cruises <- sqlQuery(chan, qu.cruise)
#tow <- sqlQuery(chan, qu.tow)
odbcCloseAll()


# Here's a summary of all the tow types on GBa and BBn ever...

GBa.tows <- aggregate(tot~random+year,surv.Live$GBa,FUN=length)
GBa.extras <- GBa.tows[GBa.tows$random !=1,]
GBa.reg <- GBa.tows[GBa.tows$random ==1,]
BBn.tows <- aggregate(tot~random+year,surv.Live$BBn,FUN=length)
BBn.extras <- BBn.tows[BBn.tows$random !=1,]
BBn.reg <- BBn.tows[BBn.tows$random ==1,]

windows(11,11)
par(mfrow=c(2,2),mar=c(2,6,2,0))
plot(GBa.extras$tot ~ GBa.extras$year,type="p",pch=19,ylim=c(0,50),ylab="Non-regular tows",xlab="",main="GBa")
plot(BBn.extras$tot ~ BBn.extras$year,type="p",pch=19,ylim=c(0,50),ylab="Non-regular tows",xlab="",main="BBn")
plot(GBa.reg$tot ~ GBa.reg$year,type="p",pch=19,ylim=c(0,200),ylab="Regular tows",xlab="",main="GBa")
plot(BBn.reg$tot ~ BBn.reg$year,type="p",pch=19,ylim=c(0,200),ylab="Regular tows",xlab="",main="BBn")
