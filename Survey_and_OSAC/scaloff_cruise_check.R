### scaloff_cruise_check 
### DK changed behaviour of direct_fns in Jan 2021
### Script to run pre-loading checks on xlsx templates prior to loading to scaloff database
### This is run to check data across surveys/banks within a single CRUISE.

scaloff_cruise_check <- function(tow=TRUE, hf=TRUE, mwsh=TRUE, 
                                 year, direct=direct, direct_fns,
                                 type="xlsx", cruise, season, nickname=NULL) {
  
  ### packages
  require(readxl) || stop("Make sure you have readxl package installed to run this")
  require(plyr) || stop("Make sure you have plyr package installed to run this")
  require(geosphere) || stop("Make sure you have geosphere package installed to run this")
  #require(rgeos) || stop("Make sure you have rgeos package installed to run this")
  require(ggplot2) || stop("Make sure you have rgeos package installed to run this")
  require(reshape2) || stop("Make sure you have reshape2 package installed to run this")
  
  ### other functions
  ### DK:  I believe I need this, but maybe not? Now defaults to looking at Github if not specified.
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
} # end for(un in funs)
  } # end  if(missing(direct_fns))
  
  
  if(!missing(direct_fns)) source(paste(direct_fns,"Survey_and_OSAC/convert.dd.dddd.r",sep=""))
  
  if(season=="spring") {
    banks <- c("Sab", "Mid", "Ban", "Ger", "BBn", "BBs", "GB","GBMon")
  }
  
  if(season=="summer") {
    banks <- c("GBa", "GBb")
  }
  
  if(season=="both") {
    banks <- c("Sab", "Mid", "Ban", "Ger", "BBn", "BBs", "GB", "GBa", "GBb")
  }
  
  tows<-list()
  hfs<-list()
  mwshs<-list()
  
  for(j in 1:length(cruise)) {
    ## from an xlsx:
    if(type=="xlsx"){
      message("STOP! Please re-consider using CSVs instead of XLSX files. You should have created CSVs because of date formatting issues between users with XLSX files.")
      for(i in 1:length(unique(banks))){
        files <- list.files(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/"))
        towfile <- files[grep(files, pattern=".tow")][grep(files[grep(files, pattern=".tow")], pattern=".xlsx")]
        mwshfile <- files[grep(files, pattern=".meat")][grep(files[grep(files, pattern=".meat")], pattern=".xlsx")]
        hffile <- files[grep(files, pattern=".hf")][grep(files[grep(files, pattern=".hf")], pattern=".xlsx")]
        if(length(towfile) > 1 | length(mwshfile) > 1 | length(hffile) > 1) {
          message(paste0("\nThere are multiple files of the same type (tow/mwsh/hf). Check the following files in the folder: \n", 
                         direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i]))
          if(length(towfile) > 1) print(towfile)
          if(length(mwshfile) > 1) print(mwshfile)
          if(length(hffile) > 1) print(hffile)
        }
        
        bank <- banks[i]
        
        if(tow==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", towfile))){
          tows[[bank]][[1]] <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", towfile))
          tows[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", towfile)
        }
        if(hf==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", hffile))) {
          hfs[[bank]][[1]] <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", hffile))
          hfs[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", hffile)
        }
        if(mwsh==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", mwshfile))) {
          mwshs[[bank]][[1]] <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", mwshfile))
          mwshs[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", mwshfile)
        }
      }
    }
    
    ## from a csv:
    if(type=="csv"){
      for(i in 1:length(unique(banks))){
        files <- list.files(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/"))
        towfile <- files[grep(files, pattern=".tow")][grep(files[grep(files, pattern=".tow")], pattern=".csv")]
        mwshfile <- files[grep(files, pattern=".meat")][grep(files[grep(files, pattern=".meat")], pattern=".csv")]
        hffile <- files[grep(files, pattern=".hf")][grep(files[grep(files, pattern=".hf")], pattern=".csv")]
        # bring in the extras or icelandic if they are in separate CSVs
        extrahffile <- hffile[grep(hffile, pattern="extra")]
        extratowfile <- towfile[grep(towfile, pattern="extra")]
        icehffile <- hffile[grep(hffile, pattern="ice")]
        icetowfile <- towfile[grep(towfile, pattern="ice")]
        icemwshfile <- mwshfile[grep(mwshfile, pattern="ice")]
        if(length(extratowfile)>0) towfile <- towfile[towfile != extratowfile]
        if(length(extrahffile)>0) hffile <- hffile[hffile != extrahffile]
        if(length(icetowfile)>0) towfile <- towfile[towfile != icetowfile]
        if(length(icehffile)>0) hffile <- hffile[hffile != icehffile]
        if(length(icemwshfile)>0) mwshfile <- mwshfile[mwshfile != icemwshfile]
        
        if(length(towfile) > 1 | length(mwshfile) > 1 | length(hffile) > 1) {
          message(paste0("\nThere are multiple files of the same type (tow/mwsh/hf). Check the following files in the folder: \n", 
                         direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i]))
          if(length(towfile) > 1) print(towfile)
          if(length(mwshfile) > 1) print(mwshfile)
          if(length(hffile) > 1) print(hffile)
        }
        
        bank <- banks[i]
        
        if(tow==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", towfile))){
          tows[[bank]][[1]] <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", towfile))
          # append the extras if they exist:
          if(length(extratowfile)>0) tows[[bank]][[1]] <- rbind(tows[[bank]][[1]], data.frame(read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", extratowfile))))
          tows[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", towfile)
          if(length(extratowfile)>0) tows[[bank]][[2]] <- c(tows[[bank]][[2]],  paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", extratowfile))
          # append the ice if they exist:
          if(bank == "Ban"){
            if(length(icetowfile)>0) tows[[bank]][[1]] <- rbind(tows[[bank]][[1]], data.frame(read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", icetowfile))))
            tows[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", towfile)
            if(length(icetowfile)>0) tows[[bank]][[2]] <- c(tows[[bank]][[2]],  paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", icetowfile))
          }
        }
        if(hf==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", hffile))) {
          hfs[[bank]][[1]] <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", hffile))
          # append the extras if they exist:
          if(length(extrahffile)>0) hfs[[bank]][[1]] <- rbind(hfs[[bank]][[1]], data.frame(read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", extrahffile))))
          hfs[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", hffile)
          if(length(extratowfile)>0) hfs[[bank]][[2]] <- c(hfs[[bank]][[2]],  paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", extrahffile))
          # append the ice if they exist:
          if(bank == "Ban"){
            if(length(icehffile)>0) hfs[[bank]][[1]] <- rbind(hfs[[bank]][[1]], data.frame(read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", icehffile))))
            hfs[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", hffile)
            if(length(icehffile)>0) hfs[[bank]][[2]] <- c(hfs[[bank]][[2]],  paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", icehffile))
          }
        }
        if(mwsh==TRUE & file.exists(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", mwshfile))) {
          mwshs[[bank]][[1]] <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", mwshfile))
          mwshs[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", mwshfile)
          # append the ice if they exist:
          if(bank == "Ban"){
            if(length(icemwshfile)>0) mwshs[[bank]][[1]] <- rbind(mwshs[[bank]][[1]], data.frame(read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", icemwshfile))))
            mwshs[[bank]][[2]] <- paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", mwshfile)
            if(length(icemwshfile)>0) mwshs[[bank]][[2]] <- c(mwshs[[bank]][[2]],  paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise[j], "/", banks[i], "/", icemwshfile))
          }
        }
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
  for(i in 1:length(names(tows))){
    
    message(paste0("Survey names by area (number of tows) in ", names(tows[i]), " tow file:"))
    if(names(tows)[i] == "Ban") {
      print("Ban Sea scallop:")
      print(table(tows[[i]][[1]]$SURVEY_NAME[tows[[i]][[1]]$SPECIES_ID=="1 - Sea scallop"], tows[[i]][[1]]$MGT_AREA_CD[tows[[i]][[1]]$SPECIES_ID=="1 - Sea scallop"]))
      print("Ban Icelandic:")
      print(table(tows[[i]][[1]]$SURVEY_NAME[tows[[i]][[1]]$SPECIES_ID=="2 - Iceland scallop"], tows[[i]][[1]]$MGT_AREA_CD[tows[[i]][[1]]$SPECIES_ID=="2 - Iceland scallop"]))
    }
    if(!names(tows)[i] == "Ban") print(table(tows[[i]][[1]]$SURVEY_NAME, tows[[i]][[1]]$MGT_AREA_CD))
    
    message(paste0("Survey names by tow type ID (number of tows) in ", names(tows[i]), " tow file:"))
    if(names(tows)[i] == "Ban") {
      print("Ban Sea scallop:")
      print(table(tows[[i]][[1]]$SURVEY_NAME[tows[[i]][[1]]$SPECIES_ID=="1 - Sea scallop"], tows[[i]][[1]]$TOW_TYPE_ID[tows[[i]][[1]]$SPECIES_ID=="1 - Sea scallop"]))
      print("Ban Icelandic:")
      print(table(tows[[i]][[1]]$SURVEY_NAME[tows[[i]][[1]]$SPECIES_ID=="2 - Iceland scallop"], tows[[i]][[1]]$TOW_TYPE_ID[tows[[i]][[1]]$SPECIES_ID=="2 - Iceland scallop"]))
    }
    if(!names(tows)[i] == "Ban") print(table(tows[[i]][[1]]$SURVEY_NAME, tows[[i]][[1]]$TOW_TYPE_ID))
  }
  
  # then make it check tow numbers between banks. Summarize banquereau data to do this. 
  # are there any duplicate tow numbers within the same survey name (on different banks)
  tow_survey <- NULL
  for(i in 1:length(names(tows))){
    tow_surveys <- unique(data.frame(CRUISE=tows[[i]][[1]]$CRUISE,
                                     SURVEY_NAME=tows[[i]][[1]]$SURVEY_NAME, 
                                     MGT_AREA_CD=tows[[i]][[1]]$MGT_AREA_CD,
                                     TOW_NO=tows[[i]][[1]]$TOW_NO,
                                     TOW_DATE=tows[[i]][[1]]$TOW_DATE,
                                     TOW_TYPE_ID=tows[[i]][[1]]$TOW_TYPE_ID,
                                     SPECIES_ID=tows[[i]][[1]]$SPECIES_ID))
    tow_survey <- rbind(tow_survey, tow_surveys)
  }
  
  tow_survey <- tow_survey[tow_survey$SPECIES_ID=="1 - Sea scallop",]
  
  tow_survey_tab <- data.frame(table(tow_survey$TOW_NO, tow_survey$SURVEY_NAME))
  
  if(any(tow_survey_tab$Freq > 1)) {
    message("Tow numbers duplicated within survey!!! Very Important Error!!\nIf only one bank is listed in the banks column, that means there are duplicate tows labelled with the same MGT_AREA_CD.\nIf there are multiple banks in the banks column, there are duplicate tows within the same SURVEY_NAME,\nbut with different MGT_AREA_CD.\nReview and edit the following tow numbers:")
    tow_survey_check <- data.frame(table(tow_survey$TOW_NO, tow_survey$SURVEY_NAME))
    names(tow_survey_check) <- c("TOW_NO", "SURVEY_NAME", "Freq")
    tow_survey_check$TOW_NO <- as.numeric(as.character(tow_survey_check$TOW_NO))
    conflicting_banks <- join(tow_survey[,c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_NO")], tow_survey_check, type="left", by=c("SURVEY_NAME", "TOW_NO"))
    conflicting_banks <- ddply(.data=conflicting_banks,
                                      .(CRUISE, SURVEY_NAME, TOW_NO, Freq),
                                      summarize,
                                      banks = paste(unique(MGT_AREA_CD), collapse="/"))
    print(conflicting_banks[conflicting_banks$Freq > 1,])
  }

if(length(unique(data.frame(table(tow_survey$TOW_NO, tow_survey$SURVEY_NAME))$Freq)) == 1 | (length(unique(data.frame(table(tow_survey$TOW_NO, tow_survey$SURVEY_NAME))$Freq)) == 2 & 
   max(data.frame(table(tow_survey$TOW_NO, tow_survey$SURVEY_NAME))$Freq) == 1)) {
  message("Successfully passed duplicate tow check without any issues. Yay!")
}
}
