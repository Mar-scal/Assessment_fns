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
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"
source(paste0(direct_fns, "Survey_and_OSAC/olex_import.R"))
source(paste0(direct_fns, "Survey_and_OSAC/olex_check_strata.R"))

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


##### Import olex data from gz or txt file, and calculate distance coefficient and bearing
olex <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/MidSabLE15tracks.txt", ntows=121, type="load")

##### Optional: 
### Check to see if they stayed in the right strata.
output <- olex_check_strata(towplan = "C:/Users/keyserf/Desktop/sab_plan.csv", # from final station list
                            towfile="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/MidSabLE15tracks.txt",
                            bank="Sab", interactive=F)

### Extract the start and endpoints in sf format
olex <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/MidSabLE15tracks.txt", ntows=121, type="startend")

### Extract the tow tracks in sf format
olex <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/MidSabLE15tracks.txt", ntows=121, type="track")






