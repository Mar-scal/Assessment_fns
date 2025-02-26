## RUNME log and slip data compiling

# set your directory
direct <- "Y:/Offshore/Assessment/"

# read the function from github
funs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Fishery/logs_and_fishery_data.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}

# to export csv (adjust years to whatever you want):
logs_and_fish(loc="offshore", year=2009:2019, get.marfis = F, export = T, direct = direct)

# CSV will be stored here: 
paste0(direct,"Data/Fishery_data/Logs/Compiled/")