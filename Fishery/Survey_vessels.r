# Just a little script to get the survey vessel dates
# Open the channel to the database
chan <- odbcConnect(db.con,uid=un.ID,pwd=pwd.ID,believeNRows=FALSE)
# The query to grab log data
qu.log <- "select * from SCALOFF.OSCRUISES"
# Run the query and add data to the log.lst object
res <- sqlQuery(chan, qu.log)
# close the odbc connection
odbcCloseAll()
#} # end if year < 2010

write.csv(res,file="D:/R/SPERA/Data/Scallop_fishery/Survey_vessels_and_dates-from_db.csv")
