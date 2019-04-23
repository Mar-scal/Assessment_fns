### scaloff_cruise_check 

### Script to run pre-loading checks on xlsx templates prior to loading to scaloff database
### This is run to check data across surveys/banks within a single CRUISE.

scaloff_cruise_check(tow=TRUE, year, direct="Y:/Offshore scallop/Assessment/",
              type="csv", 
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
        tows[[bank]][[1]] <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", towfile))
        tows[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", towfile)
      }
      if(hf==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", hffile))) {
        hfs[[bank]][[1]] <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", hffile))
        hfs[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", hffile)
        }
      if(mwsh==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", mwshfile))) {
        mwshs[[bank]][[1]] <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", mwshfile))
        mwshs[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", banks[i], "/", mwshfile)
      }
    }
  }
  
  message("Check to make sure the right files were read into R.")
  
  message("Loaded the following tow files:")
  print(sapply(tows, "[[", 2))
  
  message("Loaded the following HF files:")
  print(sapply(hfs, "[[", 2))
  
  message("Loaded the following MWSH files:")
  print(sapply(mwshs, "[[", 2))

  # check for number of tows by survey name, and area by survey name
  for(i in 1:length(banks)){
    message("Survey names by area (number of tows):")
    print(table(tows[[i]][[1]]$SURVEY_NAME, tows[[i]][[1]]$MGT_AREA_CD))
    
    message("Survey names by tow type ID (number of tows):")
    print(table(tows[[i]][[1]]$SURVEY_NAME, tows[[i]][[1]]$TOW_TYPE_ID))
  }
  
  # then make it check tow numbers between banks
  # are there any duplicate tow numbers within the same survey name (on different banks)
  tow_survey <- NULL
  for(i in 1:length(names(tows))){
    tow_surveys <- data.frame(CRUISE=tows[[i]][[1]]$CRUISE,
               SURVEY_NAME=tows[[i]][[1]]$SURVEY_NAME, 
               MGT_AREA_CD=tows[[i]][[1]]$MGT_AREA_CD,
               TOW_NO=tows[[i]][[1]]$TOW_NO,
               TOW_DATE=tows[[i]][[1]]$TOW_DATE,
               TOW_TYPE_ID=tows[[i]][[1]]$TOW_TYPE_ID)
    tow_survey <- rbind(tow_survey, tow_surveys)
  }
  
  if(any(table(tow_survey$TOW_NO, tow_survey$SURVEY_NAME) > 1)) {
    message("Tow numbers duplicated within survey!!! Very Important Error!! Review and edit the following tow numbers:")
    tow_survey_check <- data.frame(table(tow_survey$TOW_NO, tow_survey$SURVEY_NAME))
    names(tow_survey_check) <- c("TOW_NO", "SURVEY_NAME", "Freq")
    tow_survey_check$conflicting_banks <- paste(unique(tow_survey$MGT_AREA_CD[tow_survey$SURVEY_NAME %in% 
                                                                                      tow_survey_check$SURVEY_NAME && 
                                                                                      tow_survey$TOW_NO %in% 
                                                                                      tow_survey_check$TOW_NO]), collapse="/")
    print(tow_survey_check[tow_survey_check$Freq > 1,])
  }
}
