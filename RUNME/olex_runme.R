###### RUNME to Import Olex tow data and calculate distance coefficients for entry into SCALOFF #####
### Freya Keyser and Tricia Pearo Drew May 2022 ####
###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#  
#  olex_import.R
#  olex_check_strata.R
#
###############################################################################################################

## To source functions from local directory:
# direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"
# source(paste0(direct_fns, "Survey_and_OSAC/olex_import.R"))
# source(paste0(direct_fns, "Survey_and_OSAC/olex_check_strata.R"))

# OR source from github:
funcs <- c("https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Survey_and_OSAC/olex_import.R",
           "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Survey_and_OSAC/olex_check_strata.R")
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# UTM 32619 for GBa, GBb, BBn, Ger
# UTM 32620 for BBs, Sab, Mid
# UTM 32621 for Ban

### OPTIONAL: Extract the start and endpoints in sf format. Warning: May contain tows from wrong years!
olex_se <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE17/MidSabtracksLE17.gz", 
                       type="startend", UTM=32620,
                       tow_number_key = "Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE17/LE17trackorder.xlsx")

### OPTIONAL: Extract the tow tracks in sf format
olex_sf <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE17/MidSabtracksLE17.gz", 
                       UTM = 32620, earliest="2023-05-01", latest="2023-07-01", type="sf",
                       tow_number_key = "Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE17/LE17trackorder.xlsx")

### OPTIONAL: Extract the tow tracks for sharing in txt file or CSV file
olex_tracks <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE17/MidSabtracks.gz", 
                           UTM = 32620, earliest="2023-05-01", latest="2023-07-01", type="tracks",
                           tow_number_key = "Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE17/LE17trackorder.xlsx")

# for industryreport
for(i in unique(olex_tracks$Bank)){
  write <- filter(olex_tracks, Bank==i)
  year <- unique(lubridate::year(write$Date_time))
  write$Year <- year
  write <- dplyr::select(write, -Date_time)
  if(i == "SFA25A") write$Bank <- "SabMid"
  if(i == "SFA25B") write$Bank <- "Ban"
  if(i == "SFA26A") write$Bank <- "BBn"
  if(i == "SFA26B") write$Bank <- "BBs"
  if(i == "SFA26C") write$Bank <- "Ger"
  if(i == "SFA27A") write$Bank <- "GBa"
  if(i == "SFA27B") write$Bank <- "GBb"
  write.csv(write, paste0("Y:/Offshore/Assessment/Data/Survey_data/2023/Industry Reports/", unique(write$Bank), "_olex_tracks_", year, ".csv"))
}

##### Import olex data from gz or txt file, and calculate distance coefficient and bearing/
##### w setting was determined based on testing results in Supporting_task_code/2022/olex_vs_ov_2022.Rmd
olex_load <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE18/LE18GBatracks_final.gz", 
                         UTM=32620, type="load", correction_factor = 1.04, earliest="2023-08-01", latest="2023-09-01")

# to create an editable CSV file
olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE18/LE18GBatracks_final.gz", 
            UTM=32620, type="csv", correction_factor = 1.04, earliest="2023-08-01", latest="2023-09-01")
# to load using an edited CSV file
olex_load <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE18/LE18GBatracks_final.gz", 
                         UTM=32620, type="load", correction_factor = 1.04, earliest="2023-08-01", latest="2023-09-01",
                         edited_csv="path/to/csv_to_edit.csv")

#write.csv(x=olex_load, file="Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE17/Olex_distance_coefficients_MidSab.csv")

##### Optional: 
### Check to see if they stayed in the right strata.
output <- olex_check_strata(towplan = "C:/Users/keyserf/Documents/temp_data/sab_plan23.csv", # from final station list
                            towfile="Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE17/MidSabtracks_May21.gz",
                            bank="Sab", interactive=F, UTM=32620, earliest="2023-05-01", latest="2023-07-01")

# to check tow lengths
olex_sf$length <- st_length(olex_sf)

olex_sf <- left_join(olex_sf, olex_load)

olex_sf[olex_sf$bank=="SFA25A" & as.numeric(olex_sf$dis_coef) > 0.9 & as.numeric(olex_sf$dis_coef) <10, c("tow", "bank", "length", "dis_coef")]
olex_sf[as.numeric(olex_sf$length)<800,]
