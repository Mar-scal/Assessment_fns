### Catch Effort Tables - generalized for all years.
### Please use this script to run the catch effort tables as of 2019-10-23. 
### Note! CPUE will not match OSAC values from OSAC_res$fish.res because the OSAC CPUEs are calculated using a jackknife estimation approach. Catch and effort values should be very close to OSAC. If effort differs by >1, you should investigate.
### Other options/arguments for these functions are provided at the end of this script.

### read in some packages
require(dplyr)
require(openxlsx)

### Set your directory, year, and source the CPUE_monthly_or_observer function
### for the data and outputs:
direct <- "Y:/Offshore/Assessment/"

### for the code:
### If you want to use the MASTER version:
# direct_fns <- "Y:/Offshore/Assessment/Assessment_fns/"
### If you want to use a development version:
# direct_fns <- "Y:/Github/Offshore/Assessment_fns/FK/"
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"

year <- c(2022)

source(paste0(direct_fns, "Fishery/CPUE_monthly_or_observer.R"))


### For catch effort tables by bank and fleet, run the following:
banks <- c("GBa", "GBb", "Sab", "Ger", "Mid", "BBn", "BBs", "Ban", 
  "SPB") # pick your banks
fleets <- c("FT", "WF") # pick your fleets
marfis = T # T for pull from marfis (uncorrected), F for pull from log and slip csvs (corrected)
# un.ID <- ""
# pwd.ID <-""

combos <- expand.grid(banks=banks, fleets=fleets)
combos <- arrange(combos, banks)
combos$n <- 1:nrow(combos)

bank_fleet_table_all <- NULL
for(i in 1:nrow(combos)){
  CPUE.mon(CPUE="month", year=year, bank=combos$banks[i], fleet=combos$fleets[i], print=F, output=T, 
           direct=direct, direct_fns=direct_fns, get.marfis = marfis)#, un=un.ID, pw = pwd.ID) # enter un or pw if get.marfis=F
  bank_fleet_table <- cbind(data.frame(bank=combos$banks[i]), CPUE.monthly[, -which(names(CPUE.monthly) %in% "bank")])
  bank_fleet_table_all <- rbind(bank_fleet_table_all, bank_fleet_table)
}

row.names(bank_fleet_table_all) <- NULL


### bank_fleet_table_all contains all your data. Now put it into an XLSX workbook:
tabs <- c("GBa", "GBb", "Ger", "BBn", 
          "BBs", "Sab", "Mid", "Ban")

CETwb <- createWorkbook(title="Catch effort tables")

for(i in 1:length(tabs)) {
  # add a worksheet for the bank
  addWorksheet(wb=CETwb, sheetName=tabs[i])

  # create tables for each fleet on the bank
  subtable_FT <- bank_fleet_table_all[bank_fleet_table_all$bank==tabs[i] & bank_fleet_table_all$fleet=="FT",]
  subtable_WF <- bank_fleet_table_all[bank_fleet_table_all$bank==tabs[i] & bank_fleet_table_all$fleet=="WF",]
  writeData(wb=CETwb, sheet = i, x = subtable_FT, borders = "surrounding")
  writeData(wb=CETwb, sheet = i, x = subtable_WF, borders = "surrounding", startRow = 16)
  writeData(wb=CETwb, sheet = i, x = 'Note: kg.h, kg.hm, and kg.crhm values in the "Total" row are means (not calculated based on total kg and total h/hm/crhm)', startRow = 31, startCol = "M")
}

### Open your workbook to check it over. 
openXL(CETwb)

### If it looks good, run the following to save it to a standardized location
saveWorkbook(CETwb, file=paste0(direct, "Data/Fishery_data/CPUE/", year, "/catch_effort_tables_", Sys.Date(), ".xlsx"))
### If Error because File already exists, and you're sure you want to overwrite, run this:
# saveWorkbook(CETwb, file=paste0(direct, "Data/Fishery_data/CPUE/", year, "/catch_effort_tables_", Sys.Date(), ".xlsx"), overwrite = T)

# make some plots?
require(ggplot2)
ggplot() + geom_point(data=bank_fleet_table_all[!bank_fleet_table_all$month=="Total",], aes(month, hm, colour=fleet)) + 
  geom_line(data=bank_fleet_table_all[!bank_fleet_table_all$month=="Total",], aes(month, hm, colour=fleet, group=fleet)) + 
  facet_grid(year~bank) +
  theme_bw() +
  ggtitle(paste0("Run on ", Sys.Date()))

###########################################################################################################################################


### For catch effort tables for observer trips, run the following with your desired parameters. This should be run in conjunction with other checks and verified against OTIS reports!!
# for a specific trip:
CPUE.mon(CPUE="obs", year=2020, obs.vnum = 105912, obs.land.date = "2020-02-17",
         print=T, output=T, nafo.div = c("5ZEJ", "5ZEM"), 
         direct=direct_off, 
         direct_fns=direct_off_fns)

# for multiple trips
CPUE.mon(CPUE = "obs", year=2020, 
         print=T, output=T, get.marfis=F, 
         direct_fns=direct_fns, 
         direct_bycatch="C:/Users/keyserf/Documents/Version_control_pandemic/Bycatch/", 
         direct = direct)


# For survey year catch
direct <- "Y:/Offshore/Assessment/"

### for the code:
### If you want to use the MASTER version:
direct_fns <- "Y:/Offshore/Assessment/Assessment_fns/"
### If you want to use a development version:
#direct_fns <- "Y:/Github/Offshore/Assessment_fns/FK/"

year <- c(2020)
  
source(paste0(direct_fns, "Fishery/logs_and_fishery_data.R"))
source(paste0(direct_fns, "Fishery/fishery.dat.R"))

logs_and_fish(loc = "GBa", year = 2019:2020, get.marfis = F, export = F, direct=direct, direct_fns=direct_fns)
cpue.dat <- fishery.dat(new.log.dat,bk="GBa",yr=2019:2020,method='jackknife',
                                   period = "survyr", direct=direct, direct_fns=direct_fns) 
logs_and_fish(loc = "BBn", year = 2019:2020, get.marfis = F, export = F, direct=direct, direct_fns=direct_fns)
cpue.dat <- fishery.dat(new.log.dat,bk="BBn",yr=2019:2020,method='jackknife',
                        period = "survyr", direct=direct, direct_fns=direct_fns) 


###########################################################################################################################################


# Here are all the options you can try:
# CPUE:     CPUE to calculate, CPUE by month/bank/nafo, observer specific trip CPUE or both.  Default is "month".  Options are "month", "obs", and "both"
#           Arguments needed for observer data start with "obs."
# bank:     Which bank to select from using abbrevated bank id.  Default is GBa (Georges Bank A). 
#           The options are "GBa" = Georges a, "GBb" = Georges b, "BBn" = Browns north, "BBs" = Browns south, "GBBB" = all of Georges and Browns banks.
#           "GB" = all of Georges Bank, "BB" = all of Browns Bank, Ger" = German, 
#           "Mid" = Middle, "Sab" = Sable, "Ban" = Banquereau, and "SPB" = St. Pierre Bank
# year:     the year of interest.  We are limited to years from which we have slip data, so this is limited to 2009:current_year (maybe 2008 is possible)
# fleet:    ALL = total fleet, FT = freezer trawlers, WF = wet fishery		
# boxes:    Summarize the data for the seed boxes?  Default is NULL, other  options currently are "GB", "BB", "ALL"
# print:    Print the results to the screen.  (T/F), default = F
# output:   Return the results as r objects.  (T/F), default = T
# nafo.div: The nafo division.  This needs to align with the Bank choice as well, what is produced is the amount landed on bank X nafo division y
#           if nafo division crosses multple banks this will only pull out part of NAFO division data.  Default is NULL.  This 
#           option won't work great for the data before 2008 without careful attention as these data have some differernt division names.
# export.logs:    Do you want to export the log and fishery data.  This does not include these monthly tables. (T/F) default is F  
#                 See logs_and_fishery_data.r for details
# export.tables:  Do you want to export the tables produced in this query.  (T/F), default is F
# months:         Select the months of interest.  Numerice, default is all months, c(1:12)
# obs.vnum:       The vessel number for the observer trips, multiple trips are allowed BUT MUST BE PAIRED WITH APPROPRIATE obs.land.date argument.  
#                 If CPUE = "obs" or "both" and this is = NULL this will read in a flat file with observer trip information in it. Default is NULL
# obs.land.date:  The landing date of the observer trip(s), multiple trips are allowed BUT THIS MUST BE PAIRED WITH APPROPRIATE obs.vnum 
#                 If CPUE = "obs" or "both" and this is = NULL this will read in a flat file with observer trip information in it. Default is NULL
# obs.export:     Export the observer CPUE data?  (T/F), default =F
#un:              your SQL username.  default = un.ID (if set in your r.Profile this will run automatically)
#pw:              Your SQL password.  default = pwd.ID  (if set in your r.Profile this will run automatically)
#db.con:          Database to connect to.  Default is  "ptran"   
#direct:          Directory to find the functions.  Default is "Y:/Offshore scallop/Assessment/")