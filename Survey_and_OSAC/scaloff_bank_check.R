### scaloff_bank_check
# DK came and change direct_fns behaviour in Jan 2021
### Script to run pre-loading checks on csv files generated from xlsx templates prior to loading to scaloff database
### This is run to check data WITHIN a single bank.

scaloff_bank_check <- function(tow=TRUE, hf=TRUE, mwsh=TRUE, year, direct=direct, direct_fns,
                               type="csv", 
                               cruise, bank, survey_name, nickname=NULL,
                               spatialplot=TRUE,
                               assign.strata=TRUE,
                               olex.csv = NULL,
                               un=NULL,
                               pwd.ID=NULL) {
  icetow <- NULL
  icehfs <- NULL
  icemwshs <- NULL
  ### packages
  require(readxl) || stop("Make sure you have readxl package installed to run this")
  require(plyr) || stop("Make sure you have plyr package installed to run this")
  #require(geosphere) || stop("Make sure you have geosphere package installed to run this")
  #require(rgeos) || stop("Make sure you have rgeos package installed to run this")
  require(ggplot2) || stop("Make sure you have rgeos package installed to run this")
  require(reshape2) || stop("Make sure you have reshape2 package installed to run this")
  require(lubridate) || stop("Make sure you have lubridate package installed to run this")
  #require(sp) || stop("Make sure you have sp package installed to run this")
  require(sf) || stop("Make sure you have sf package installed to run this")
  require(dplyr) || stop("Make sure you have dplyr package installed to run this")
  
  ### other functions
  ### DK:  I believe I need this, but maybe not? Now defaults to looking at Github if not specified.
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Other_functions/ScallopQuery.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/github_spatial_import.R")
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
  
  if(!missing(direct_fns)) {
    source(paste(direct_fns,"Survey_and_OSAC/convert.dd.dddd.r",sep=""))
    source(paste(direct_fns,"Other_functions/ScallopQuery.r",sep=""))
    source(paste(direct_fns,"Maps/Github_spatial_import.R",sep=""))
  }
    
  
  ### load the data
  ## from the xlsx template:
  if(type=="xlsx"){
    message("Are you sure you want to use XLSX files? You really should convert the XLSX files to CSVs and then run these checks. The XLSX files have date formatting problems...")
    if(!is.null(nickname)) {
      if(tow==TRUE) tows <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/OS.scallop.tow", nickname, ".xlsx"),  .name_repair="universal")
      if(hf==TRUE) hfs <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/OS.scallop.hf", nickname, ".xlsx"), .name_repair="universal")
      if(mwsh==TRUE) mwshs <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/OS.scallop.meat.shell", nickname, ".xlsx"), .name_repair="universal")
      if(bank=="Ban" && tow==TRUE) {
        icetow <- tows[tows$SPECIES_ID=="2 - Iceland scallop",]
        tows <- tows[tows$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icetow)[1] == dim(tows)[1] | !dim(icetow)[2] == dim(tows)[2]) message("icemtows dimensions different from tows. Ok?")
      }
      if(bank=="Ban" && hf==TRUE) {
        icehfs <- hfs[hfs$SPECIES_ID=="2 - Iceland scallop",]
        hfs <- hfs[hfs$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icehfs)[1] == dim(hfs)[1] | !dim(icehfs)[2] == dim(hfs)[2]) message("icehfs dimensions different from hfs. Ok?")
      }
      if(bank=="Ban" && mwsh==TRUE) {
        icemwshs <- mwshs[mwshs$SPECIES_ID=="2 - Iceland scallop",]
        mwshs <- mwshs[mwshs$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icemwshs)[1] == dim(mwshs)[1] | !dim(icemwshs)[2] == dim(mwshs)[2]) message("icemwshs dimensions different from mwshs. Ok?")
      }
    }
      
    if(is.null(nickname)) {
      stop("fix the paths first")
      if(tow==TRUE) tows <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.tows.template.xlsx"), .name_repair="universal")
      if(hf==TRUE) hf <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.hf.template.xlsx"), .name_repair="universal")
      if(mwsh==TRUE) mwsh <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.meat.shell.template.xlsx"), .name_repair="universal")
      if(bank=="Ban" && tow==TRUE) {
        icetow <- tows[tows$SPECIES_ID=="2 - Iceland scallop",]
        tows <- tows[tows$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icetow)[1] == dim(tows)[1] | !dim(icetow)[2] == dim(tows)[2]) message("icemtows dimensions different from tows. Ok?")
      }
      if(bank=="Ban" && hf==TRUE) {
        icehfs <- hfs[hfs$SPECIES_ID=="2 - Iceland scallop",]
        hfs <- hfs[hfs$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icehfs)[1] == dim(hfs)[1] | !dim(icehfs)[2] == dim(hfs)[2]) message("icehfs dimensions different from hfs. Ok?")
      }
      if(bank=="Ban" && mwsh==TRUE) {
        icemwshs <- mwshs[mwshs$SPECIES_ID=="2 - Iceland scallop",]
        mwshs <- mwshs[mwshs$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icemwshs)[1] == dim(mwshs)[1] | !dim(icemwshs)[2] == dim(mwshs)[2]) message("icemwshs dimensions different from mwshs. Ok?")
      }
    }
  }
  
  ## from a csv:
  if(type=="csv"){
    if(!is.null(nickname)) {
      if(tow==TRUE) tows <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/OS.scallop.tow", nickname, ".csv"))
      if(hf==TRUE) hfs <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/OS.scallop.hf", nickname, ".csv"))
      if(mwsh==TRUE) mwshs <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/OS.scallop.meat.shell", nickname, ".csv"))
      if(bank=="Ban" && tow==TRUE) {
        icetow <- tows[tows$SPECIES_ID=="2 - Iceland scallop",]
        tows <- tows[tows$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icetow)[1] == dim(tows)[1] | !dim(icetow)[2] == dim(tows)[2]) message("icemtows dimensions different from tows. Ok?")
      }
      if(bank=="Ban" && hf==TRUE) {
        icehfs <- hfs[hfs$SPECIES_ID=="2 - Iceland scallop",]
        hfs <- hfs[hfs$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icehfs)[1] == dim(hfs)[1] | !dim(icehfs)[2] == dim(hfs)[2]) message("icehfs dimensions different from hfs. Ok?")
      }
      if(bank=="Ban" && mwsh==TRUE) {
        icemwshs <- mwshs[mwshs$SPECIES_ID=="2 - Iceland scallop",]
        mwshs <- mwshs[mwshs$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icemwshs)[1] == dim(mwshs)[1] | !dim(icemwshs)[2] == dim(mwshs)[2]) message("icemwshs dimensions different from mwshs. Ok?")
      }
      
    }
    if(is.null(nickname)) {
      if(tow==TRUE) tows <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/", survey_name, "tows.csv"))
      if(hf==TRUE) hfs <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/", survey_name, "hf.csv"))
      if(mwsh==TRUE) mwshs <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/", survey_name, "mwsh.csv"))
      if(bank=="Ban" && tow==TRUE) {
        icetow <- tows[tows$SPECIES_ID=="2 - Iceland scallop",]
        tows <- tows[tows$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icetow)[1] == dim(tows)[1] | !dim(icetow)[2] == dim(tows)[2]) message("icemtows dimensions different from tows. Ok?")
      }
      if(bank=="Ban" && hf==TRUE) {
        icehfs <- hfs[hfs$SPECIES_ID=="2 - Iceland scallop",]
        hfs <- hfs[hfs$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icehfs)[1] == dim(hfs)[1] | !dim(icehfs)[2] == dim(hfs)[2]) message("icehfs dimensions different from hfs. Ok?")
      }
      if(bank=="Ban" && mwsh==TRUE) {
        icemwshs <- mwshs[mwshs$SPECIES_ID=="2 - Iceland scallop",]
        mwshs <- mwshs[mwshs$SPECIES_ID=="1 - Sea scallop",]
        if(!dim(icemwshs)[1] == dim(mwshs)[1] | !dim(icemwshs)[2] == dim(mwshs)[2]) message("icemwshs dimensions different from mwshs. Ok?")
      }
    }
  }

  
  ############################################
  
  ### check tow data
  if(tow==TRUE) {
    
    towchecks <- function(tows, icetow=NULL){
      
      message("Are there empty rows at the bottom of the Tow dataframe? If so, go back to your spreadsheet and delete empty rows.")
      print(tail(tows))

      # if there are multiple species in the file, print error message.
      if(length(unique(tows$SPECIES_ID))>1) {
        stop("\nThere are multiple species in the tow file.")
      }
      
      # If there are non-numeric values (incl. blanks or NA's) in TOW_NO column, print the offending tows
      if(any(is.na(as.numeric(as.character(tows$TOW_NO))))) {
        message("\nThere are non-numeric values in TOW_NO column. Check the following:")
        print(data.frame(tows[which(is.na(as.numeric(as.character(tows$TOW_NO)))),]))
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
        message("\nThere are more tows than there are unique tow_no's. One or more of the following tows may be mislabelled:")
        print(data.frame(tows[tows$TOW_NO == names(which(table(tows$TOW_NO)>1)),]))
        
      }
      
      if(any(is.na(tows$DIS_COEF_ID) | is.null(tows$DIS_COEF_ID))) {
        message("Data missing from DIS_COEF_ID column")
      }
      
      # Are there gaps in tow numbers? If so, print the tows before and after the gap. 
      if(any(!seq(tow_no_unique$mintownum, tow_no_unique$maxtownum, 1) %in% tows$TOW_NO)){
        message("\nThere are gaps in tow numbering. This might be fine if you have extra tows with different numbering schemes.The tows below are the ones before and after the gap(s). Check them.")
        missing <- seq(tow_no_unique$mintownum, tow_no_unique$maxtownum, 1)[which(!seq(tow_no_unique$mintownum, tow_no_unique$maxtownum, 1) %in% tows$TOW_NO)]
        show <- sort(c(missing-1, missing+1))
        print(data.frame(tows[tows$TOW_NO %in% show,]))
        
      }
      
      # Check mgt_area_cd. Must correspond to area_cd of the associated Survey. AREA_CD isn't in tow template, so let's just make sure MGT_AREA_CD is one of the following:
      if(any(!unique(tows$MGT_AREA_CD) %in% c("GBa", "GBb", "Ger", "BBn", "BBs", "Mid", "Sab", "SPB", "Ban"))){
        message("\nThere are values in MGT_AREA_CD that are not contained in the validation table. The data cannot be loaded to the DB like this.
Check the MGT_AREA_CD values for the following tows:")
        print(data.frame(tows[!tows$MGT_AREA_CD %in% c("GBa", "GBb", "Ger", "BBn", "BBs", "Mid", "Sab", "SPB", "Ban"),]))
        
      }
      
      # Make sure that MGT_AREA_CD matches survey name
      if(any(unique(tows$MGT_AREA_CD) %in% c("Sab", "Mid", "Ger", "Ban"))) {
        mgt_matches_survey <- grep(x=tolower(tows$SURVEY_NAME), pattern=unique(tolower(tows$MGT_AREA_CD)))
        if(length(mgt_matches_survey) != length(tows$SURVEY_NAME)) {
          message("There are MGT_AREA_CDs in the tow file that do not match the SURVEY_NAME:")
          print(data.frame(tows[!tows$TOW_NO %in% tows$TOW_NO[mgt_matches_survey],]))
          
        }
      }
      
      if(any(unique(tows$MGT_AREA_CD) %in% c("BBn", "BBs"))) {
        mgt_matches_survey <- grep(x=tows$SURVEY_NAME, pattern="BB")
        if(length(mgt_matches_survey) != length(tows$SURVEY_NAME)) {
          message("There are MGT_AREA_CDs in the tow file that do not match the SURVEY_NAME:")
          print(data.frame(tows[!tows$TOW_NO %in% tows$TOW_NO[mgt_matches_survey],]))
          
        }
      }
      
      if(any(unique(tows$MGT_AREA_CD) %in% c("GBa", "GBb"))) {
        mgt_matches_survey <- grep(x=tows$SURVEY_NAME, pattern=paste0("GB", year))
        if(length(mgt_matches_survey) != length(tows$SURVEY_NAME)) {
          message("There are MGT_AREA_CDs in the tow file that do not match the SURVEY_NAME:")
          print(data.frame(tows[!tows$TOW_NO %in% tows$TOW_NO[mgt_matches_survey],]))
        }
        
        if(dim(tows[grep(x=tows$SURVEY_NAME, pattern=paste0("GB", year, ".1")),])[1] > 0){
          if(any(!tows[tows$TOW_NO %in% c(304, 305, 306, 312),]$MGT_AREA_CD == "GBb")) {
            message("Possibly the wrong MGT_AREA_CD for these tows (should be GBb?):")
            print(tows[tows$TOW_NO %in% c(304, 305, 306, 312),])
          }
        }
      }
      
      # check depth. Must be between 20 and 200.
      if(any(tows$DEPTH_F < 20 | tows$DEPTH_F > 200)){
        message("\nThere are values in DEPTH_F that are less than 20 or greater than 200. Check DEPTH_F for the following tows:")
        print(data.frame(tows[tows$DEPTH_F < 20 | tows$DEPTH_F > 200,]))
        
      }
      
      # check tow date. It can be NULL, but the DB will produce a warning. Let's flag any nulls or iffy values here first. 
      if(any(is.null(tows$TOW_DATE) | is.na(tows$TOW_DATE))) {
        stop("\nThe following tows are missing a TOW_DATE. The DB will accept this, so make sure it's what you want!")
        print(data.frame(tows[is.null(tows$TOW_DATE) | is.na(tows$TOW_DATE),]))
        
      }
      
      # these tow dates might not be formatted correctly:
      if(type=="xlsx"){
        if(any(is.na(ymd(tows$TOW_DATE, quiet=T)) & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE))) {
          message("\nThe following tows have dates that may not be formatted correctly. They should look like dd/mm/yyyy.")
          print(data.frame(tows[which(is.na(ymd(tows$TOW_DATE, quiet = T)) & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE)),]))
        }
        
        if(any(as.character(tows$TOW_DATE) & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE))) {
          message("\nThe following tows have dates that may not be formatted correctly. They should look like dd/mm/yyyy.")
          print(data.frame(tows[which(is.na(ymd(tows$TOW_DATE, quiet = T)) & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE)),]))
        }
        
        # these tow dates might not be formatted correctly:
        if(any(!month(ymd(tows$TOW_DATE, quiet=T)) %in% c(5, 6, 7, 8) & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE))) {
          message("\nThe following tows have dates that may not be formatted correctly. They should look like dd/mm/yyyy. Currently, it appears that the month is not typical (e.g. it should be spring or summer)")
          print(data.frame(tows[which(!month(ymd(tows$TOW_DATE, quiet=T)) %in% c(5, 6, 7, 8) & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE)),]))
        }
      }
      if(type=="csv"){
        if(any(is.na(dmy(tows$TOW_DATE, quiet=T)))) {# & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE))) {
          message("\nThe following tows have dates that may not be formatted correctly. They should look like dd/mm/yyyy.")
          print(data.frame(tows[which(is.na(dmy(tows$TOW_DATE, quiet = T)) & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE)),]))
        }
        
        if(any(grep(x=as.character(tows$TOW_DATE), pattern = "-", fixed=TRUE))) { # & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE)) {
          message("\nThe following tows have dates contain dashes (-) instead of slashes (/). They should look like dd/mm/yyyy.")
          print(data.frame(tows[grep(x=as.character(tows$TOW_DATE), pattern = "-", fixed=TRUE),]))
        }
        
        # these tow dates might not be formatted correctly:
        if(any(!month(dmy(tows$TOW_DATE, quiet=T)) %in% c(5, 6, 7, 8))) {#} & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE))) {
          message("\nThe following tows have dates that may not be formatted correctly. They should look like dd/mm/yyyy. Currently, it appears that the month is not typical (e.g. it should be spring or summer)")
          print(data.frame(tows[which(!month(dmy(tows$TOW_DATE, quiet=T)) %in% c(5, 6, 7, 8)),]))# & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE)),]))
        }
      }
        
      # The subsampling amount is less than or equal to total amount:
      if(any((tows$Basket.Wgt.Sampled..kg.> tows$Total.Basket.Wgt..kg. & !is.na(tows$Total.Basket.Wgt..kg.)) |
             (tows$No..Buckets.Sampled > tows$Total.Buckets & !is.na(tows$Total.Buckets)))) {
        message("\nThe following tows have more basket KG or buckets sampled than the totals")
        print(data.frame(tows[(tows$Basket.Wgt.Sampled..kg.> tows$Total.Basket.Wgt..kg.) |
                                (tows$No..Buckets.Sampled > tows$Total.Buckets),]))
      }
      
      # check tow numbering series
      if(bank=="Ban" && any(tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO < 900 | tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO > 1000)) message("Unexpected tow numbering series. Should be in the 900's\n")
      if(bank=="BBn" && any(tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO < 200 | tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO > 301)) message("Unexpected tow numbering series. Should be 201-300\n")
      if(bank=="GB" && any(tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO < 300 | tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO > 325)) message("Unexpected tow numbering series. Should be 301-325\n")
      if(bank=="Ger" && any(tows[tows$TOW_TYPE_ID %in% c("1 - Regular survey tow","3 - Repeat tow from previous year"),]$TOW_NO < 400 | tows[tows$TOW_TYPE_ID %in% c("1 - Regular survey tow","3 - Repeat tow from previous year"),]$TOW_NO > 481)) message("Unexpected tow numbering series. Should be 401-480\n")
      if(bank=="Mid" && any(tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO > 16)) message("Unexpected tow numbering series. Should be 1-15\n")
      if(bank=="Sab" && any(tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO > 101)) message("Unexpected tow numbering series. Should be 1-100\n")
      if(bank=="GBa" && any(tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO > 201)) message("Unexpected tow numbering series. Should be 1-200\n")
      if(bank=="GBb" && any(tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO < 600 | tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO > 631)) message("Unexpected tow numbering series. Should be 601-630\n")
      
      #check olex tow numbering
      if(!is.null(olex.csv)){
        olex <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise , "/", olex.csv))
        olex <- olex[olex$Bank==bank,]
        olex$TOW_NO <- olex$official_tow_number
        
        numcheck <- left_join(tows, olex)
        if(any(abs(numcheck$START_LON - numcheck$start_lon) > 0.01) | any(abs(numcheck$START_LAT - numcheck$start_lat) > 0.01)){
          check <- numcheck[which(abs(numcheck$START_LON - numcheck$start_lon) > 0.01 |
                                    abs(numcheck$START_LAT - numcheck$start_lat) > 0.01), 
                            c("TOW_NO", "START_LON", "START_LAT", "start_lon", "start_lat")]
          message("Possible issues in tow numbering with olex file. Compare coordinates in olex csv, to loader csv for the following tows:")
          print(check)
        }
      }
      # check format of coordinates
      if(!is.numeric(tows$START_LAT) | !is.numeric(tows$START_LON) | !is.numeric(tows$END_LAT) | !is.numeric(tows$END_LON)) {
        message("\nThe following tows have non-numeric values in the coordinates columns:")
        print(data.frame(rbind(
          tows[!tows$START_LON %in% tows$START_LON[grep(x=tows$START_LON, pattern = "([[:digit:]])")],],
          tows[!tows$START_LAT %in% tows$START_LAT[grep(x=tows$START_LAT, pattern = "([[:digit:]])")],],
          tows[!tows$END_LON %in% tows$END_LON[grep(x=tows$END_LON, pattern = "([[:digit:]])")],],
          tows[!tows$END_LAT %in% tows$END_LAT[grep(x=tows$END_LAT, pattern = "([[:digit:]])")],]
        )))
        tows[,c("START_LAT","START_LON", "END_LAT", "END_LON")] <- apply(tows[,c("START_LAT","START_LON", "END_LAT", "END_LON")], 2, function(x) as.numeric(as.character(x)))
        
      }
      
      # check values in coordinates
      if(is.numeric(tows$START_LAT) & is.numeric(tows$START_LON) & is.numeric(tows$END_LAT) & is.numeric(tows$END_LON) &
         any(tows[,c("START_LAT","END_LAT")] > 4730 | tows[,c("START_LAT", "END_LAT")] < 4100, na.rm=T)){
        message("\nThere are values in the latitude columns that are outside the bounds. Check the following:")
        print(data.frame(tows[tows$START_LAT > 4730 | tows$END_LAT > 4730 | tows$START_LAT < 4100 | tows$END_LAT < 4100,]))
        
      }
      if(is.numeric(tows$START_LAT) & is.numeric(tows$START_LON) & is.numeric(tows$END_LAT) & is.numeric(tows$END_LON) &
         any(tows[,c("START_LON","END_LON")] > -5500 | tows[,c("START_LON", "END_LON")] < -6710, na.rm=T)){
        message("\nThere are values in the longitude columns that are outside the bounds. Check the following:")
        print(data.frame(tows[tows$START_LON > -5500 | tows$END_LON > -5500 | tows$START_LON < -6710 | tows$END_LON < -6710,]))
        
      }
      
      # spatial check coordinates relative to mgt_area_cd shapefile. Make plots. This is adapted from check.tows.spatial.R which is used for Inshore Survey. 
      
      # converting all lats and longs to decimal degrees
      tows_con <- data.frame(apply(tows[,c("START_LAT","START_LON", "END_LAT", "END_LON")], 2, function(x) convert.dd.dddd(format="dec.deg", x)))
      names(tows_con) <- paste0(names(tows_con), "_DD")
      tows_con <- cbind(tows[, c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NO","START_LAT","START_LON", "END_LAT", "END_LON")],
                        tows_con)
      
      # grab the authoritative management area polygons file. 
      area <- github_spatial_import(subfolder = "survey_boundaries",zipname = "survey_boundaries.zip", quiet=T)
      area$ID <- gsub(x = area$ID, pattern = ".shp", replacement="")
      if(bank=="GB") area <- area[!area$ID %in% c("GBa", "GBb"),]
      if(!bank=="GB") area <- area[!area$ID %in% c("GB"),]
      
      area$AREA_ID <- as.numeric(as.factor(area$ID))
      
      # # this creates bank labels
      area_lab <- area %>% 
        dplyr::group_by(ID) %>%
        dplyr::summarize() %>%
        st_cast("MULTIPOLYGON") %>%
        st_centroid()
      
      if(spatialplot==TRUE){
        # based on start location first, then by end location. compare them to determine if they cross an area line.
        
        starts <- st_as_sf(tows_con, coords=c("START_LON_DD", "START_LAT_DD"), crs=4326, remove = F)
        starts <- st_join(starts, area)
        starts$start_ID <- starts$AREA_ID
        starts <- dplyr::select(starts, -AREA_ID)
        st_geometry(starts) <- NULL
        
        ends <- st_as_sf(tows_con, coords=c("END_LON_DD", "END_LAT_DD"), crs=4326, remove = F)
        ends <- st_join(ends, area)
        ends$end_ID <- ends$AREA_ID
        ends <- dplyr::select(ends, -AREA_ID)
        st_geometry(ends) <- NULL
        
        area.test <- dplyr::full_join(starts, ends) 
    
        area.test$flag <- NULL
        ## these ones cross a management area line or are outside the management area boundary
        if(any(!area.test$start_ID == area.test$end_ID | is.na(area.test$start_ID) | is.na(area.test$end_ID))) {
          message("\nThe following tows are outside the management area boundary (or cross the line):")
          print(area.test[!area.test$start_ID == area.test$end_ID | is.na(area.test$start_ID) | is.na(area.test$end_ID),])
          area.test$flag[!area.test$start_ID == area.test$end_ID | is.na(area.test$start_ID) | is.na(area.test$end_ID)] <- "flag"
        }
        
        area.test$flag[area.test$start_ID == area.test$end_ID & !is.na(area.test$start_ID) & !is.na(area.test$end_ID)] <- "ok"
        
        starts$type <- "start"
        starts$AREA_ID <- starts$start_ID
        starts <- dplyr::select(starts, -start_ID) %>% st_as_sf(coords=c("START_LON_DD", "START_LAT_DD"), crs=4326, remove = F)
        
        ends$type <- "end"
        ends$AREA_ID <- ends$end_ID
        ends <- dplyr::select(ends, -end_ID) %>% st_as_sf(coords=c("END_LON_DD", "END_LAT_DD"), crs=4326, remove = F)
        
        tows2 <- rbind(starts, ends) %>%
          group_by(across(c(-type, -geometry, -AREA_ID, -ID))) %>%
          summarize() %>%
          st_cast("LINESTRING")
        tows2 <- left_join(tows2, area.test)

        # calculating the distance of each tow
        tows2$dist.calc <- st_length(tows2)
        
        # flag the tow if the distance is greater than 2 km
        if(any(as.numeric(tows2$dist.calc) > 2000 | as.numeric(tows2$dist.calc) < 500)) {
          message("\nThere are some tows longer than 2km or shorter than 500m. Check their coordinates:")
          print(data.frame(tows2[!is.na(tows2$dist.calc) & (as.numeric(tows2$dist.calc) > 2000 | as.numeric(tows2$dist.calc) < 500),]))
          
        }
        
        plot.list <- NULL
        ## plot tows to PDF
        for(i in unique(area$ID)){
          p <- ggplot() + geom_sf(data=area[area$ID==i,], fill=NA, colour="black",na.rm=T) +
            geom_sf_text(data=area_lab[area_lab$ID==i,], aes(label=ID), size=4, colour="blue",na.rm=T) +
            theme_bw() + theme(panel.grid=element_blank()) +
            geom_sf(data=tows2[tows2$MGT_AREA_CD==i,], aes(colour=flag), lwd=1,na.rm=T) +
            #scale_colour_manual(values=c("black", "white")) +
            geom_sf_text(data=tows2[tows2$MGT_AREA_CD==i,], aes(label=TOW_NO, colour=flag), lwd=1,na.rm=T)
          plot.list[[i]] <- p
        }
        
        if(!is.null(nickname)) {
          pdf(paste0(direct, "/Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/spatial_checks", nickname, ".pdf"),onefile=T,width=22,height=12)
          print(plot.list)
          dev.off()
        }
        if(is.null(nickname)) {
          pdf(paste0(direct, "/Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/spatial_checks.pdf"),onefile=T,width=22,height=12)
          print(plot.list)
          dev.off()
        }
      
        files <- list.files(paste0(direct, "/Data/Survey_data/", year, "/Database loading/", cruise, "/"))
        files <- files[grep(x = files, pattern=bank)]
        files <- files[grep(x = files, pattern="Olex_distance_coefficients")]
        
        if(length(files)>0) {
          if(length(files)>1) stop("I don't know which olex file to use. Use browser() to do this manually, then turn off spatialplots when you want to skip this step.")
          olex <- read.csv(paste0(direct, "/Data/Survey_data/", year, "/Database loading/", cruise, "/", files))
          olex$startlon <- convert.dd.dddd(olex$start_lon)
          olex$startlat <- convert.dd.dddd(olex$start_lat)
          olex$endlon <- convert.dd.dddd(olex$end_lon)
          olex$endlat <- convert.dd.dddd(olex$end_lat)
          
          png(paste0(direct, "/Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/olex_compare_spatial.png"), width=11, height=8, units="in", res=600)
          print(ggplot() + 
            # geom_segment(data=area.test.both, aes(x=START_LON_DD, xend=END_LON_DD, y=START_LAT_DD, yend=END_LAT_DD, linetype="tow file"), lwd=3, na.rm = T) +
            geom_segment(data=olex[olex$Bank==bank,], aes(x=startlon, xend=endlon, y=startlat, yend=endlat, linetype="olex"), na.rm = T) +
            geom_text(data=area.test, aes(x=START_LON_DD, y=START_LAT_DD, colour="tow file (start point)", label=TOW_NO), na.rm = T, size=1) +
            geom_text(data=olex[olex$Bank==bank,], aes(x=endlon, y=endlat, colour="olex (end point)", label=tow), na.rm = T, size=1) +
            scale_colour_manual(values=c("red", "blue")) + 
            coord_sf())
          dev.off()
          
          olex_sf <- st_as_sf(olex, coords=c("startlon", "startlat"), crs=4326)
          
          UTM <- ifelse(bank %in% c("GBa", "GBb", "Ger", "BBn"), 32619, 32620)
          
          tows_sf <- tows
          tows_sf$START_LON_DD <- convert.dd.dddd(tows_sf$START_LON)
          tows_sf$START_LAT_DD <- convert.dd.dddd(tows_sf$START_LAT)
          tows_sf <- st_as_sf(tows_sf, coords=c("START_LON_DD", "START_LAT_DD"), crs=4326)
          tows_sf_buffer <- st_transform(st_buffer(st_transform(tows_sf, UTM), dist = 500), 4326)
          
          compare <- st_intersection(tows_sf_buffer, olex_sf)
          
          if(nrow(compare) == nrow(tows_sf)) message("number of matched tows = number of tows expected - Good!")
          if(!nrow(compare) == nrow(tows_sf)) message("number of matched tows = number of tows expected - Bad! Probably just means buffer is too big though. Use browser() to adjust it.")
          if(all(compare$bearing - compare$BEARING < 1)) message("bearings match between olex and tow file")  
          if(any(compare$bearing - compare$BEARING > 1)) {
            message("bearings mismatch between olex and tow file. review the following tows:")  
            print(compare[which(compare$bearing-compare$BEARING >1),])
          }
          if(all(compare$dis_coef - compare$DIS_COEF < 0.001)) message("distance coefficients match between olex and tow file")  
          if(any(compare$dis_coef - compare$DIS_COEF > 0.001)) {
            message("distance coefficient mismatch between olex and tow file. review the following tows:")  
            print(compare[which(compare$dis_coef-compare$DIS_COEF >0.001),])
          }
        }
        
        if(length(files)==0) message("Could not find corresponding Olex file, make sure bank name is in filename.")        
        
        if(!is.null(icetow)) {
          if(unique(icetow$SPECIES_ID) == "2 - Iceland scallop"){
            test <- dim(icetow)[1] == dim(join(tows[, c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NO", "TOW_TYPE_ID")], icetow[, c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NO")], type="full"))[1]
            if(test == FALSE) message("There is a difference in the tow metadata between sea scallop and icelandic tows")
          }
        }
      
        #assign to strata
        if(assign.strata==TRUE & bank %in% c("Sab", "BBn", "BBs", "GBa", "GBb")){
          shp <- github_spatial_import(subfolder = "offshore_survey_strata", zipname = "offshore_survey_strata.zip",quiet = T, specific_shp = paste0(bank,".shp"))
          shp <- st_make_valid(shp)
          if(bank %in% c("Sab","BBs")) CRS <- 32620
          if(!bank %in% c("Sab","BBs")) CRS <- 32619
          tows_sf <- st_intersection(st_transform(tows_sf, CRS), st_transform(shp, CRS)) %>% 
            dplyr::select(-PID, -col, -border, -PName, -towbl_r, -are_km2, -label, -startyr)
          
          #check allocation
          assigned <- as.data.frame(table(tows_sf$Strt_ID[tows_sf$TOW_TYPE_ID=="1 - Regular survey tow"]))
          names(assigned)[1] <- "Strt_ID"
          assigned$Strt_ID <- as.numeric(as.character(assigned$Strt_ID))
          survey.info <- read.csv(paste0(direct, "Data/Survey_data/survey_information.csv"))
          survey.info <- survey.info[survey.info$label==bank,]
          survey.info <- survey.info[survey.info$startyear==unique(survey.info$startyear)[which.min(year - unique(survey.info$startyear))],]
          if(bank=="GBa") survey.info$allocation <- c(51,37,32,26,35,11,8) 
          if(!bank=="GBa") survey.info$allocation <- survey.info$area_km2/sum(survey.info$area_km2) * nrow(tows[tows$TOW_TYPE_ID=="1 - Regular survey tow",])
          names(survey.info)[which(names(survey.info)=="Strata_ID")] <- "Strt_ID"
          assigned <- left_join(assigned, survey.info) 
          
          if(all(abs(assigned$Freq - round(assigned$allocation,0))==0)) message("Assigned strata matches expected allocation") 
          if(any(!abs(assigned$Freq - round(assigned$allocation,0))==0)) message("Assigned strata does not match expected allocation")
          if((nrow(tows_sf) == nrow(tows) & !any(is.na(tows_sf$Strt_ID))) | (any(!is.na(tows_sf$Strt_ID)) & any(unique(tows$TOW_TYPE_ID) %in% c("2 - Exploratory tow", "5 - Exploratory repeat tow")))){
            message("Assigned strata successfully. See tow_file_strata.csv")
            write.csv(x = dplyr::arrange(tows_sf[tows_sf$TOW_TYPE_ID=="1 - Regular survey tow",], TOW_NO), paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/tow_file_strata.csv"))
          }
          if(!nrow(tows_sf) == nrow(tows) | any(is.na(tows_sf$Strt_ID))){
            message("Strata assignment failed for some tows. Check this to make sure it's ok (e.g. Sable extras).")
            }
        }
        
      }
    
      if(bank %in% c("Sab", "BBn", "BBs") & all(is.na(tows$STRATA_ID)) & assign.strata==F) message("You need to assign the strata for this bank. Set spatialplot=T and assign.strata=T and re-run, then transfer Strt_ID column.")
      
      if(any(complete.cases(dplyr::select(tows, -STRATA_ID, -BOTTOM_TEMP, -BOTTOM_TYPE_ID, -PIC_NUM, -COMMENTS)) == FALSE)) {
        message("missing data in the following Tow file rows:")
        print(tows[which(complete.cases(dplyr::select(tows, -STRATA_ID, -BOTTOM_TEMP, -BOTTOM_TYPE_ID, -PIC_NUM, -COMMENTS))==FALSE),])
      }
    }
    
    # run checks only on tow data if it's all sea scallops, otherwise, compare sea scallop tow metadata to icelandic scallop metadata
    if(is.null(icetow)) towchecks(tows=tows)
    if(!is.null(icetow)) towchecks(tows=tows, icetow=icetow)
    
  }

  

  ####################### Height frequency checks
  
  if(hf==TRUE){
    
    hfchecks <- function(hfs, tows){
      
      message("Are there empty rows at the bottom of the HF dataframe? If so, go back to your spreadsheet and delete empty rows.")
      print(tail(hfs))
      
      
      # missing tow number
      if(any(is.na(hfs$TOW_NUM))) {
        message("The following rows have no tow number:")
        print(hfs[which(is.na(hfs$TOW_NUM)),])
      }
      
      # check tow date. It can be NULL, but the DB will produce a warning. Let's flag any nulls or iffy values here first. 
      if(any(is.null(hfs$TOW_DATE) | is.na(hfs$TOW_DATE))) {
        message("\nThe following hfs are missing a TOW_DATE. The DB might accept this, so make sure it's what you want!")
        print(data.frame(hfs[is.null(hfs$TOW_DATE) | is.na(hfs$TOW_DATE),]))
        
      }
      
      if(type=="csv"){
        # these tow dates might not be formatted correctly:
        if(any(is.na(dmy(hfs$TOW_DATE, quiet=T)) & !is.na(hfs$TOW_DATE) & !is.null(hfs$TOW_DATE))) {
          message("\nThe following hfs have dates that may not be formatted correctly. They should look like dd/mm/yyyy.")
          print(data.frame(hfs[which(is.na(dmy(hfs$TOW_DATE, quiet=T)) && !is.na(hfs$TOW_DATE) && !is.null(hfs$TOW_DATE)),]))
          
        }
        
        if(any(grep(x=as.character(hfs$TOW_DATE), pattern = "-", fixed=TRUE) & !is.na(hfs$TOW_DATE) & !is.null(hfs$TOW_DATE))) {
          message("\nThe following tows have dates contain dashes (-) instead of slashes (/). They should look like dd/mm/yyyy.")
          print(data.frame(hfs[which(grep(x=as.character(hfs$TOW_DATE), pattern = "-", fixed=TRUE) & !is.na(hfs$TOW_DATE) & !is.null(hfs$TOW_DATE)),]))
        }
        
        # these tow dates might not be formatted correctly:
        if(any(!month(dmy(hfs$TOW_DATE, quiet=T)) %in% c(5, 6, 7, 8) & !is.na(hfs$TOW_DATE) & !is.null(hfs$TOW_DATE))) {
          message("\nThe following hfs have dates that may not be formatted correctly. They should look like dd/mm/yyyy. Currently, it appears that the month is not typical (e.g. it should be spring or summer)")
          print(data.frame(hfs[which(!month(dmy(hfs$TOW_DATE, quiet=T)) %in% c(5, 6, 7, 8) & !is.na(hfs$TOW_DATE) & !is.null(hfs$TOW_DATE)),]))
        }
      }
      
      ## compare to tow file. Make sure the number of baskets and buckets matches between files.
      names(tows)[grep(x=names(tows), "TOW_NO")] <- "TOW_NUM"
      comparehf <- join(hfs, tows, type="left")
    
      ## make sure all tow metadata matches between files
      if(any(is.na(comparehf$TOW_TYPE_ID))) {
        message("Something in the tow metadata doesn't match between the tow and hf files. Check date formats, survey name, cruise name, etc.")
        mismatch <- unique(comparehf[is.na(comparehf$TOW_TYPE_ID),]$TOW_NUM)
        # try the tow dates
        if(any(!unique(hfs[hfs$TOW_NUM %in% mismatch,]$TOW_DATE) == tows[tows$TOW_NUM %in% mismatch,]$TOW_DATE)){
          message("Tow dates or other metadata does not match between tow file and hf file. See records below for a hint.")
          hfs$file <- "HF File"
          tows$file <- "Tow File"
          print(arrange(join(unique(hfs[hfs$TOW_NUM %in% mismatch, c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NUM", "SPECIES_ID", "file")]), tows[tows$TOW_NUM %in% mismatch, c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NUM", "SPECIES_ID", "file")], type="full"), TOW_NUM))
        }
      }
      
      ## Make sure the number of baskets corresponds between files.
      if(any((comparehf$LIVE_QTY_BASKET > 0 | comparehf$DEAD_QTY_BASKET > 0) & (!is.na(comparehf$LIVE_QTY_BASKET) | !is.na(comparehf$DEAD_QTY_BASKET)) & 
             (comparehf$Total.Baskets==0 | is.na(comparehf$Total.Baskets)) & !is.na(comparehf$TOW_TYPE_ID))) {
        message("Too many baskets in HF file compared to Total Baskets in Tow file")
        print(comparehf[which((comparehf$LIVE_QTY_BASKET > 0 | comparehf$DEAD_QTY_BASKET > 0) & (!is.na(comparehf$LIVE_QTY_BASKET) | !is.na(comparehf$DEAD_QTY_BASKET)) & 
                                (comparehf$Total.Baskets==0 | is.na(comparehf$Total.Baskets) & !is.na(comparehf$TOW_TYPE_ID))),])
      }
      
      ## Make sure the number of buckets corresponds between files.
      if(any((comparehf$LIVE_QTY_BUCKET > 0 | comparehf$DEAD_QTY_BUCKET > 0) & 
             (!is.na(comparehf$LIVE_QTY_BUCKET) | !is.na(comparehf$DEAD_QTY_BUCKET)) & 
             (comparehf$Total.Buckets==0 | is.na(comparehf$Total.Buckets)) & 
             !is.na(comparehf$TOW_TYPE_ID))) {
        message("Too many buckets in HF file compared to Total Buckets in Tow file")
        print(comparehf[which((comparehf$LIVE_QTY_BUCKET > 0 | comparehf$DEAD_QTY_BUCKET > 0) & 
                                (!is.na(comparehf$LIVE_QTY_BUCKET) | !is.na(comparehf$DEAD_QTY_BUCKET)) & 
                                (comparehf$Total.Buckets==0 | is.na(comparehf$Total.Buckets))),])
      }
      
      ## Make sure the number of baskets corresponds between files.
      if(any((comparehf$LIVE_QTY_BASKET > 0 | comparehf$DEAD_QTY_BASKET > 0) & 
             (!is.na(comparehf$LIVE_QTY_BASKET) | !is.na(comparehf$DEAD_QTY_BASKET)) & 
             (comparehf$Total.Baskets==0 | is.na(comparehf$Total.Baskets)) & 
             !is.na(comparehf$TOW_TYPE_ID))) {
        message("Too many baskets in HF file compared to Total Baskets in Tow file")
        print(comparehf[which((comparehf$LIVE_QTY_BASKET > 0 | comparehf$DEAD_QTY_BASKET > 0) & 
                                (!is.na(comparehf$LIVE_QTY_BASKET) | !is.na(comparehf$DEAD_QTY_BASKET)) & 
                                (comparehf$Total.Baskets==0 | is.na(comparehf$Total.Baskets) & !is.na(comparehf$TOW_TYPE_ID))),])
      }
      
      ## Make sure the number of buckets corresponds between files.
      
      comparehf2 <- comparehf %>%
        group_by(TOW_NUM, Total.Buckets, Total.Baskets) %>%
        summarize(LIVE_QTY_BASKET = sum(LIVE_QTY_BASKET),
                  DEAD_QTY_BASKET = sum(DEAD_QTY_BASKET),
                  LIVE_QTY_BUCKET = sum(LIVE_QTY_BUCKET),
                  DEAD_QTY_BUCKET = sum(DEAD_QTY_BUCKET))
      if(any(comparehf2$Total.Buckets>0 & comparehf2$LIVE_QTY_BUCKET==0 & comparehf2$DEAD_QTY_BUCKET ==0)) {
        message("Total buckets is greater than 0, but there is nothing in HF file. Review these tows")
        print(comparehf2[comparehf2$Total.Buckets>0 & comparehf2$LIVE_QTY_BUCKET==0 & comparehf2$DEAD_QTY_BUCKET==0,])
      }
      
      if(any(comparehf2$Total.Baskets>0 & comparehf2$LIVE_QTY_BASKET==0 & comparehf2$DEAD_QTY_BASKET ==0)) {
        message("Total baskets is greater than 0, but there is nothing in HF file. Review these tows")
        print(comparehf2[comparehf2$Total.Baskets>0 & comparehf2$LIVE_QTY_BASKET==0 & comparehf2$DEAD_QTY_BASKET==0,])
      }
      
      # make sure all bins are present
      bins <- as.data.frame(table(hfs$TOW_NUM, hfs$BIN_ID))
      if(dim(bins[bins$Freq<1 | bins$Freq>1,])[1] > 0) {
        message("missing or extra bin(s) in HF file.")
        message("Bins:")
        print(as.character(bins[bins$Freq<1 | bins$Freq>1,]$Var2))
        message("From following Tow(s):")
        print(unique(as.character(bins[bins$Freq<1 | bins$Freq>1,]$Var1)))
      }
      
      longhfs <- reshape2::melt(hfs,
                      measure.vars = c("LIVE_QTY_BASKET", 
                                       "LIVE_QTY_BUCKET",
                                       "DEAD_QTY_BASKET",
                                       "DEAD_QTY_BUCKET"),
                      id.vars = c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NUM", 
                                  "SPECIES_ID", "BIN_ID"))
      longhfs <- longhfs[!is.na(longhfs$CRUISE),]
      
      # for the plot, re-organize the order of the panels.
      longhfs$variable <- factor(longhfs$variable, levels=c("LIVE_QTY_BUCKET",
                                                            "LIVE_QTY_BASKET",
                                                            "DEAD_QTY_BUCKET",
                                                            "DEAD_QTY_BASKET"))
      
      # check for NAs
      if(any(is.na(longhfs$value))){
        message('the following contain NAs')
        print(longhfs[is.na(longhfs$value),])
      }
      
      # are any records <50mm in baskets instead of buckets?
      if(nrow(longhfs[longhfs$variable %in% c("LIVE_QTY_BASKET", "DEAD_QTY_BASKET") & longhfs$BIN_ID<50 & longhfs$value>0,])>0) {
        message("The following HF records should be in BUCKETS not BASKETS. Check them out.")
        print(longhfs[longhfs$variable %in% c("LIVE_QTY_BASKET", "DEAD_QTY_BASKET") & longhfs$BIN_ID<50 & longhfs$value>0,])
      }
      
      # are any records >50mm in buckets instead of baskets?
      if(nrow(longhfs[longhfs$variable %in% c("LIVE_QTY_BUCKET", "DEAD_QTY_BUCKET") & longhfs$BIN_ID>50 & longhfs$value>0,])>0) {
        message("The following HF records should be in BASKETS not BUCKETS. Check them out.")
        print(longhfs[longhfs$variable %in% c("LIVE_QTY_BUCKET", "DEAD_QTY_BUCKET") & longhfs$BIN_ID>50 & longhfs$value>0,])
      }
      
      # plot the raw HF distributions by tow (one tow per pdf page)
      ## plot tows to PDF
      print("plotting HF distributions:")
      plotnum <- seq(1,length(unique(longhfs$TOW_NUM)), 4)
      plot.list <- NULL
      for(i in 1:length(plotnum)){
        p <- ggplot() + geom_col(data=longhfs[longhfs$TOW_NUM %in% c(unique(longhfs$TOW_NUM)[plotnum[i]], 
                                                                     unique(longhfs$TOW_NUM)[plotnum[i]+1],
                                                                     unique(longhfs$TOW_NUM)[plotnum[i]+2],
                                                                     unique(longhfs$TOW_NUM)[plotnum[i]+3]),], 
                                 aes(BIN_ID, value), na.rm = T) + 
          facet_grid(TOW_NUM~variable, scales="free_y")+
          theme_bw() + theme(panel.grid=element_blank()) +
          ggtitle("Number per bin, by tow (4 tows per page)")
        plot.list[[i]] <- p
      }
      
      if(!is.null(nickname)) {
        if(!unique(hfs$SPECIES_ID)=="2 - Iceland scallop") pdf(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/HF_distribution_checks", nickname, ".pdf"),onefile=T,width=22,height=12)
        if(unique(hfs$SPECIES_ID)=="2 - Iceland scallop") pdf(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/HF_distribution_checks_ice", nickname, ".pdf"),onefile=T,width=22,height=12)
        print(plot.list)
        dev.off()
      }
      if(is.null(nickname)) {
        pdf(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/HF_distribution_checks.pdf"),onefile=T,width=22,height=12)
        print(plot.list)
        dev.off()
      }
      
      
      if(any(complete.cases(hfs) == FALSE)) {
        message("missing data in the following HF file rows:")
        print(hfs[which(complete.cases(hfs)==FALSE),])
      }
    }
    
    hfchecks(hfs, tows)
  
    if(!is.null(icehfs)) hfchecks(icehfs, tows=icetow)
    
    
  }
  
  ####################### Detailed sampling checks
  if(mwsh==TRUE){
    
    mwshchecks<- function(mwshs){
      
      message("Are there empty rows at the bottom of the MWSH dataframe? If so, go back to your spreadsheet and delete empty rows.")
      print(tail(mwshs))
      
      # missing tow number
      if(any(is.na(mwshs$TOW_NUM))) {
        message("The following rows have no tow number:")
        print(hfs[which(is.na(mwshs$TOW_NUM)),])
      }
      
      # duplicate tow number
      dups <- as.data.frame(table(mwshs$TOW_NUM, mwshs$SCALLOP_NUM))
      names(dups) <- c("TOW_NUM", "SCALLOP_NUM", "Freq")
      if(any(dups$Freq>1)) {
        message("Duplicate tow numbers in MWSH file. Review records below:")
        print(dups[dups$Freq>1,])
      }
      
      # missing sampler ID
      if(any(is.na(mwshs$SAMPLER_ID) | is.null(mwshs$SAMPLER_ID))) {
        message("Data missing from SAMPLER_ID column")
      }
      
      if(any(complete.cases(mwshs) == FALSE)) {
        message("missing data in the following MWSH file rows:")
        print(mwshs[which(complete.cases(mwshs)==FALSE),])
      }
      
      if(!is.null(un) & !is.null(pwd.ID)){
        samplers <- ScallopQuery("ROracle", un, pwd.ID, db.con="ptran", 
                                 SQLtext = "SELECT * FROM SCALOFF.OSSAMPLERCODES")
        
        if(any(!unique(mwshs$SAMPLER_ID) %in% paste0(samplers$SAMPLER_ID, " - ", samplers$SAMPLER_NAME))) {
          message("SAMPLER_ID not found in SCALOFF database. CSV contains following IDs: ")
          message(c(unique(mwshs$SAMPLER_ID)))
        }
      }
      
      if(is.null(un) | is.null(pwd.ID)) message("Must supply un and pwd.ID to check SAMPLER_ID column against SCALOFF table")
      
      print("plotting MWSH relationships:")
      
      plot.list <- NULL
      for(i in 1:length(unique(mwshs$TOW_NUM))){
        p <- ggplot() + 
          geom_smooth(data=mwshs[mwshs$TOW_NUM %in% unique(mwshs$TOW_NUM)[i],],
                      aes(SHELL_HEIGHT, WET_MEAT_WGT), na.rm = T, method="glm", method.args = list(family=gaussian(link="log")))+
          geom_text(data=mwshs[mwshs$TOW_NUM %in% unique(mwshs$TOW_NUM)[i],],
                    aes(SHELL_HEIGHT, WET_MEAT_WGT, label=SCALLOP_NUM), na.rm = T) +
          theme_bw() + theme(panel.grid=element_blank()) +
          ggtitle(paste0("Tow ", unique(mwshs$TOW_NUM)[i], "- MWSH relationship by tow (numbers are SCALLOP_NUM)")) 
        plot.list[[i]] <- p
      }
      
      if(!is.null(nickname)) {
        if(!unique(mwshs$SPECIES_ID) == "2 - Iceland scallop") pdf(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/MWSH_checks", nickname, ".pdf"),onefile=T,width=15,height=12)
        if(unique(mwshs$SPECIES_ID) == "2 - Iceland scallop") pdf(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/MWSH_checks_ice", nickname, ".pdf"),onefile=T,width=15,height=12)
        print(plot.list)
        dev.off()
      }
      if(is.null(nickname)) {
        pdf(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/MWSH_checks.pdf"),onefile=T,width=15,height=12)
        print(plot.list)
        dev.off()
      }
      
      # check tow date. It can be NULL, but the DB will produce a warning. Let's flag any nulls or iffy values here first. 
      if(any(is.null(mwshs$TOW_DATE) | is.na(mwshs$TOW_DATE))) {
        message("\nThe following mwshs are missing a TOW_DATE. The DB might accept this, so make sure it's what you want!")
        print(data.frame(mwshs[is.null(mwshs$TOW_DATE) | is.na(mwshs$TOW_DATE),]))
        
      }
      
      if(type=="csv"){
        # these tow dates might not be formatted correctly:
        if(any(is.na(dmy(mwshs$TOW_DATE, quiet=T)) & !is.na(mwshs$TOW_DATE) & !is.null(mwshs$TOW_DATE))) {
          message("\nThe following mwshs have dates that may not be formatted correctly. They should look like dd/mm/yyyy.")
          print(data.frame(mwshs[which(is.na(dmy(mwshs$TOW_DATE, quiet = T)) & !is.na(mwshs$TOW_DATE) & !is.null(mwshs$TOW_DATE)),]))
          
        }
        
        # these tow dates might not be formatted correctly:
        if(any(!month(dmy(mwshs$TOW_DATE, quiet=T)) %in% c(5, 6, 7, 8) & !is.na(mwshs$TOW_DATE) & !is.null(mwshs$TOW_DATE))) {
          message("\nThe following mwshs have dates that may not be formatted correctly. They should look like dd/mm/yyyy. Currently, it appears that the month is not typical (e.g. it should be spring or summer)")
          print(data.frame(mwshs[which(!month(dmy(mwshs$TOW_DATE, quiet=T)) %in% c(5, 6, 7, 8) & !is.na(mwshs$TOW_DATE) & !is.null(mwshs$TOW_DATE)),]))
        }
      }
    }
    mwshchecks(mwshs)
    
    if(!is.null(icemwshs)) mwshchecks(icemwshs)
  }
  
}
