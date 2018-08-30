#compare sql output from OSLIVERES and OSDEADRES to R generated numbers
#ok
setwd("Y:/Offshore scallop/SurveyDatabase/ViewTesting/March052015")
direct <- "D:/R/fn/"
# Bring in the data from the databaes
require(RODBC) || stop("Package RODBC cannot be found")

chan <- odbcConnect("ptran", uid = un.ID,pwd = pwd.ID)


#qu.strata <- "select * from SCALOFF.OSSTRATA"
qu.live <- "select * from SCALOFF.OSLIVERES_VW"
qu.dead <- "select * from SCALOFF.OSDEADRES_VW"
qu.mw <- "select * from SCALOFF.OSSAMPLES_VW"
# Grab the SQL data from the respective database tables
#strata <- sqlQuery(chan, qu.strata)
sqldata.dead <- sqlQuery(chan, qu.live)
sqldata.live <- sqlQuery(chan, qu.live)
sqldata.mw <- sqlQuery(chan, qu.mw)
odbcCloseAll()


#SET DATA TO WORK ON BELOW:
#Only work on one at a time for all cruises live or dead, comment out the one your are not using
#sqldata <- read.csv("OSLIVERES_data_Mar302015.csv") # live animal standardized data 
#sqldata <- read.csv("OSDEADRES_data_Mar302015.csv") # dead animal standardized data 

r.dat <- read.csv(paste(direct,"Data/SQL_testing/SurvDat_fromR_Oct82015.csv",sep=""))

r.dat <- r.dat[r.dat$state=="live",] #subset if live or dead 
#r.dat <- r.dat[r.dat$state=="dead",] #subset if live or dead 

unique(r.dat$state)

#SELECT JUST ONE SURVEY to work on at a time, keep track
r.dat <- r.dat[r.dat$SURVEY_NAME=="GB2015",] #BE SURE YOU WORK ON SAME SURVEY FOR r.dat and sqldata
unique(r.dat$SURVEY_NAME)
sqldata <- subset(sqldata.live,SURVEY_NAME == "GB2015")
#sqldata <- subset(sqldata.dead,SURVEY_NAME == "GB2015")

unique(sqldata$SURVEY_NAME)

#RUN CODE, selects and organizes rows to compare for each data set
r.dat <- r.dat[,c(4,5,6,27:66)]  
r.dat <- r.dat[order(r.dat$CRUISE,r.dat$SURVEY_NAME,r.dat$TOW_NO),]


sqldata <- sqldata[,c(2,3,5,16:55)] 

sqldata <- sqldata[order(sqldata$CRUISE,sqldata$SURVEY_NAME,sqldata$TOW_NO),]

#Gives you a preview of what the data looks like, check that the Survey_Name, Cruise, Tow_No, and one or two of the HF numbers match
head(r.dat) 
head(sqldata) 
dim(r.dat)

tail(r.dat) 
tail(sqldata) 

# Lets add a match for each column that isn't data...
#Subtracts one matrix from the other to check for errors
test <- r.dat[,4:43] - sqldata[,4:43]
#Gives you a summary of the errors so you can see if there are any issues, check for differences of more than 8 decimal places (e-8) 
summary(abs(unique(as.vector(unlist(test)))))
#Gives sorted values to take a look at
sort(abs(unique(as.vector(unlist(test)))))
# Gives out a Histogram of the differences
#Will need to change the title for each one: live or dead & cruise name
hist(unique(as.vector(unlist(test))),main="OSLIVERES_VW: R vs SQL SAB2012", xlab="Abs Difference (R - SQL)")


## Now do the same kind of thing for the meat weight data..

r.mw <- read.csv(paste(direct,"Data/SQL_testing/ViewsMeatWt_fromR_Oct82015.csv",sep=""))
r.mw <- subset(r.mw,CRUISE == "LE02")
sqldata <- subset(sqldata.mw,CRUISE == "LE02")

dim(sqldata)
dim(r.mw)

head(r.mw)
r.mw <- r.mw[,c(3,14,15,4:10,16,17)]  
r.mw <- r.mw[order(r.mw$CRUISE,r.mw$TOW_NO,r.mw$SCALLOP_NUM),]
unique(r.mw$TOW_NO)
names(r.mw)
names(sqldata)

sqldata <- sqldata[,c(6,2,14,5,10:13,9,1,22,15)] 

sqldata <- sqldata[order(sqldata$CRUISE,sqldata$TOW_NO,sqldata$SCALLOP_NUM),]
 
names(r.mw)
names(sqldata)

#Gives you a preview of what the data looks like, check that the Survey_Name, Cruise, Tow_No, and one or two of the HF numbers match
head(r.mw) 
head(sqldata) 
dim(r.mw)

tail(r.mw) 
tail(sqldata) 

# Lets add a match for each column that isn't data...
#Subtracts one matrix from the other to check for errors
test <- r.mw[,3:12] - sqldata[,3:12]
#Gives you a summary of the errors so you can see if there are any issues, check for differences of more than 8 decimal places (e-8) 
summary(abs(unique(as.vector(unlist(test)))))
#Gives sorted values to take a look at
sort(abs(unique(as.vector(unlist(test)))))
# Gives out a Histogram of the differences
#Will need to change the title for each one: live or dead & cruise name
hist(unique(as.vector(unlist(test))),main="OSLIVERES_VW: R vs SQL SAB2012", xlab="Abs Difference (R - SQL)")





#DON'T NEED TO RUN Unless you see differences of more than 8 decimal places (e-8)
# When you find a difference, make a note and let Jess know
unique(test$h.0)
unique(test$h.5)
unique(test$h.10)
unique(test$h.15)
unique(test$h.20)
unique(test$h.25)
unique(test$h.30)
unique(test$h.35)
unique(test$h.40)
unique(test$h.45)
unique(test$h.50)
unique(test$h.55)
unique(test$h.60)
unique(test$h.65)
unique(test$h.70)
unique(test$h.75)
unique(test$h.80)
unique(test$h.85)
unique(test$h.90)
unique(test$h.95)
unique(test$h.100)
unique(test$h.105)
unique(test$h.110)
unique(test$h.115)
unique(test$h.120)
unique(test$h.125)
unique(test$h.130)
unique(test$h.135)
unique(test$h.140)
unique(test$h.145)
unique(test$h.150)
unique(test$h.155)
unique(test$h.160)
unique(test$h.165)
unique(test$h.170)
unique(test$h.175)
unique(test$h.180)
unique(test$h.185)
unique(test$h.190)
unique(test$h.195)
