# Log error checking script, you run this script to check whatever logs you want from marfis

direct <- "D:/r/"
source(paste(direct,"Assessment_fns/testing/log_error_checking.r",sep="")) #logs_and_fish is function call
log_checks(direct =direct, yrs = 2018 , 
                       un=un.ID,pw=pwd.ID,db.con="ptran",db.lib = "ROracle", export = "fish.dat",
                       bank = NULL ,trips = NULL, dates = NULL, vrnum = NULL,tow.time.check = c(3,80),trip.tol = 1,spatial = T)

# Check in on a vessel
vessel.check <- log_checks(direct =direct, yrs = 2018 , un=un.ID,pw=pwd.ID,db.con="ptran",db.lib = "ROracle",
                     bank = NULL ,trips = NULL, dates = NULL, vrnum = 106605 ,tt.check = c(3,80),trip.tol = "round",plot.trips = F)
vessel.check # Look at everything that was flagged
vessel.check$log.checks # Looks at the log which are missing lat/lon/numrakes/sfa/watch/numtow/avg tow time/weight/fish date/nafo
vessel.check$missing.dat # Looks at the log which are missing bank/date fished/vrnum/tripnum
vessel.check$num.rake.wrong # Looks at the logs in which the number of rakes doesn't match normal vessel number of rakes
vessel.check$gear.size.wrong # Looks at the logs in which the gear size doesn't match normal vessel gear size
vessel.check$weight.log.wrong # The logs that don't make slip sum weights
vessel.check$weight.slip.wrong # The slips sums that don't make logs weights
vessel.check$tow.time.outliers # The logs in which the average tow time is outside range in tt.check
vessel.check$roe.on # The logs in which roe was recorded as a "Y".
# Run a bank
bank.check <- log_checks(direct =direct, yrs = 2018 , un=un.ID,pw=pwd.ID,db.con="ptran",db.lib = "ROracle",
                           bank = "Sab" ,trips = NULL, dates = NULL, vrnum = NULL ,tt.check = c(3,80),trip.tol = "round",plot.trips = F)

# Run a trip
trip.check <- log_checks(direct =direct, yrs = 2018 , un=un.ID,pw=pwd.ID,db.con="ptran",db.lib = "ROracle",
                           bank = NULL ,trips = 467902 , dates = NULL, vrnum = NULL ,tt.check = c(3,80),trip.tol = "round",plot.trips = F)

# Run a date range
date.check <- log_checks(direct =direct, yrs = 2018 , un=un.ID,pw=pwd.ID,db.con="ptran",db.lib = "ROracle",
                           bank = NULL ,trips = NULL , dates = c("2018-08-10","2018-08-20"), vrnum = NULL ,tt.check = c(3,80),trip.tol = "round",plot.trips = F)


# Only check for trips in which the slip and logs differ by 1000 lbs
weight.check <- log_checks(direct =direct, yrs = 2018 , un=un.ID,pw=pwd.ID,db.con="ptran",db.lib = "ROracle",
                         bank = NULL ,trips = NULL , dates = NULL, vrnum = NULL ,tt.check = c(3,80),trip.tol = 1000,plot.trips = F)
# Here you are really just interested in these two pieces of information
weight.check$weight.log.wrong
weight.check$weight.slip.wrong

# Check for trips in which the average tow time was < 10 and > 50
tow.time.check <- log_checks(direct =direct, yrs = 2018 , un=un.ID,pw=pwd.ID,db.con="ptran",db.lib = "ROracle",
                         bank = NULL ,trips = NULL , dates = NULL, vrnum = NULL ,tt.check = c(10,50),trip.tol = 1000,plot.trips = F)
# With this the output of interest is just
tow.time.check$tow.time.outliers # The logs in which the average tow time is outside range in tt.check


source(paste(direct,"Assessment_fns/Maps/pectinid_projector.r",sep="")) # The new scallopMap

pecjector("GBa",add_EEZ = T,add_sfas = "all",add_land = T,add_bathy =10,add_strata = "all")

source(paste(direct,"Assessment_fns/Maps/Convert_PBSmapping_into_GIS_shapefiles.R",sep="")) 

pbs.2.gis(dat = "D:/R/Data/Maps/approved/Survey/survey_boundary_polygons.csv", save.loc = "D:/R/Data/Maps/approved/GIS_layers/survey_boundaries",
          type="polygon")
