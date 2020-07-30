## RUNME log and slip data compiling

# set your directory
direct <- "Y:/Offshore/Assessment/"

source(paste0(direct, "Assessment_fns/Fishery/logs_and_fishery_data.r"))

# to export csv (adjust years to whatever you want):
logs_and_fish(loc="offshore", year=2009:2019, get.marfis = F, export = T, direct = direct, direct_fns="Y:/Offshore/Assessment/Assessment_fns/")

# CSV will be stored here: 
paste0(direct,"Data/Fishery_data/Logs/Compiled/")