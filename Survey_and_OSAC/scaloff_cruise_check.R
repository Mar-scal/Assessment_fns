### scaloff_cruise_check 

### Script to run pre-loading checks on xlsx templates prior to loading to scaloff database
### This is run to check data across surveys/banks within a single CRUISE.

scaloff_cruise_check(tow=TRUE, year, direct="Y:/Offshore scallop/Assessment/",
              type="xlsx", 
              cruise, season, nickname=NULL) {
  
  ### packages
  require(readxl) || stop("Make sure you have readxl package installed to run this")
  require(plyr) || stop("Make sure you have plyr package installed to run this")
  require(geosphere) || stop("Make sure you have geosphere package installed to run this")
  require(rgeos) || stop("Make sure you have rgeos package installed to run this")
  require(ggplot2) || stop("Make sure you have rgeos package installed to run this")
  require(reshape2) || stop("Make sure you have reshape2 package installed to run this")
  
  ### other functions
  source(paste0(direct, "Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r"))
  
  if(season=="spring") {
    banks <- c("Sab", "Mid", "Ban", "Ger", "BBn", "BBs", "GB")
  }
  
  if(season=="summer") {
    banks <- c("GBa", "GBb")
  }
  
  if(season=="both") {
    banks <- c("Sab", "Mid", "Ban", "Ger", "BBn", "BBs", "GB", "GBa", "GBb")
  }
  # 
  # ### load the data
  # ## from the xlsx template:
  # if(type=="xlsx"){
  #   if(!is.null(nickname)) {
  #     if(tow==TRUE) tows <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.tow.template", "_", nickname, ".xlsx"))
  #     if(hf==TRUE) hfs <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.hf.template", "_", nickname, ".xlsx"))
  #     if(mwsh==TRUE) mwshs <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.meat.shell.template", "_", nickname, ".xlsx"))
  #   }
  #   
  #   if(is.null(nickname)) {
  #     if(tow==TRUE) tows <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.tow.template.xlsx"))
  #     if(hf==TRUE) hf <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.hf.template.xlsx"))
  #     if(mwsh==TRUE) mwsh <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.meat.shell.template.xlsx"))
  #   }
  # }
  
  ## from a csv:
  tows<-list()
  hfs<-list()
  mwshs<-list()
  for(i in 1:length(unique(banks))){
    if(type=="csv"){
      files <- list.files(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/"))
      towfile <- files[grep(files, pattern="tows")][grep(files[grep(files, pattern="tows")], pattern=".csv")]
      mwshfile <- files[grep(files, pattern="mwsh")][grep(files[grep(files, pattern="mwsh")], pattern=".csv")]
      hffile <- files[grep(files, pattern="hf")][grep(files[grep(files, pattern="hf")], pattern=".csv")]
      if(length(towfile) > 1 | length(mwshfile) > 1 | length(hffile) > 1) {
        message(paste0("\nThere are multiple files of the same type (tow/mwsh/hf). Check the following files in the folder: \n", 
                       direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i]))
        if(length(towfile) > 1) print(towfile)
        if(length(mwshfile) > 1) print(mwshfile)
        if(length(hffile) > 1) print(hffile)
      }
      
      bank <- banks[i]
      
      if(tow==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", towfile))){
        tows[[bank]] <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", towfile))
      }
      if(hf==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", hffile))) {
        hfs[[bank]] <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", hffile))
        }
      if(mwsh==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", mwshfile))) {
        mwshs[[bank]] <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", mwshfile))
      }
    }
  }
  

  ### keep at this. it's reading in files from the different banks nicely now! maybe get it to print which banks it found, and which files it's reading for each bank?
  
  # then make it check tow numbers between banks
  
}
