### scaloff_check
### Script to run pre-loading checks on xlsx templates prior to loading to scaloff database

scaloff_check(tow=TRUE, hf=TRUE, mwsh=TRUE, year, direct="Y:/Offshore scallop/Assessment/", nickname=NULL) {
  
  
  ### packages
  require(readxl) || stop("Make sure you have readxl package installed to run this")
  require(plyr) || stop("Make sure you have plyr package installed to run this")
  
  ### load the data
  if(!is.null(nickname)) {
    if(tow==TRUE) tows <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.tow.template", "_", nickname, ".xlsx"))
    if(hf==TRUE) hf <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.hf.template", "_", nickname, ".xlsx"))
    if(mwsh==TRUE) mwsh <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.meat.shell.template", "_", nickname, ".xlsx"))
  }
  
  if(is.null(nickname)) {
    if(tow==TRUE) tows <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.tow.template.xlsx"))
    if(hf==TRUE) hf <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.hf.template.xlsx"))
    if(mwsh==TRUE) mwsh <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.meat.shell.template.xlsx"))
  }
  
  
  ### check tow data
  if(tow==TRUE) {
    
    # If there are non-numeric values in TOW_NO column, print the offending tows
    if(any(!tows$TOW_NO %in% grep(x=tows$TOW_NO, pattern = "([[:digit:]])"))) {
      message("There are non-numeric values in TOW_NO column. Check the following:")
      print(data.frame(tows[!tows$TOW_NO %in% grep(x=tows$TOW_NO, pattern = "([[:digit:]])"),]))
      tows$TOW_NO <- as.numeric(as.character(tows$TOW_NO))
    }
    
    # tow_no must be unique within the survey
    tow_no_unique <- ddply(.data=tows, .(SURVEY_NAME),
          summarize,
          numberoftows = length(TOW_NO),
          numberofuniquetows = length(unique(TOW_NO)),
          mintownum = min(TOW_NO, na.rm=T),
          maxtownum = max(TOW_NO, na.rm=T))
    
    # If the number of tows in the survey doesn't match the number of unique tow_no's, identify the duplicate or missing tow_no 
    if(tow_no_unique$numberoftows>tow_no_unique$numberofuniquetows) {
      message("There are more tows than there are unique tow_no's. One or more of the following tows may be mislabelled:")
      print(data.frame(tows[tows$TOW_NO == names(which(table(tows$TOW_NO)>1)),]))
    }
    
    # Are there gaps in tow numbers? If so, print the tows before and after the gap. 
    if(any(!seq(tow_no_unique$mintownum, tow_no_unique$maxtownum, 1) %in% tows$TOW_NO)){
      message("There is a gap in tow numbering. This might be fine if you have extra tows with different numbering schemes.The tows below are the ones before and after the gap. Check them.")
      print(data.frame(tows[!tows$TOW_NO %in% grep(x=tows$TOW_NO, pattern = "([[:digit:]])"),]))
    }
  }
  
  
  
  
  
  
}