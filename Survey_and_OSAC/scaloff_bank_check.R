### scaloff_bank_check

### Script to run pre-loading checks on csv files generated from xlsx templates prior to loading to scaloff database
### This is run to check data WITHIN a single bank.

scaloff_bank_check <- function(tow=TRUE, hf=TRUE, mwsh=TRUE, year, direct=direct, direct_fns=direct_fns,
                               type="csv", 
                               cruise, bank, survey_name, nickname=NULL,
                               spatialplot=TRUE) {
  icetow <- NULL
  icehfs <- NULL
  icemwshs <- NULL
  ### packages
  require(readxl) || stop("Make sure you have readxl package installed to run this")
  require(plyr) || stop("Make sure you have plyr package installed to run this")
  require(geosphere) || stop("Make sure you have geosphere package installed to run this")
  require(rgeos) || stop("Make sure you have rgeos package installed to run this")
  require(ggplot2) || stop("Make sure you have rgeos package installed to run this")
  require(reshape2) || stop("Make sure you have reshape2 package installed to run this")
  require(lubridate) || stop("Make sure you have lubridate package installed to run this")
  require(sp) || stop("Make sure you have sp package installed to run this")
  
  ### other functions
  source(paste0(direct_fns, "Survey_and_OSAC/convert.dd.dddd.r"))

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
      if(unique(tows$MGT_AREA_CD) %in% c("Sab", "Mid", "Ger", "Ban")) {
        mgt_matches_survey <- grep(x=tolower(tows$SURVEY_NAME), pattern=unique(tolower(tows$MGT_AREA_CD)))
        if(length(mgt_matches_survey) != length(tows$SURVEY_NAME)) {
          message("There are MGT_AREA_CDs in the tow file that do not match the SURVEY_NAME:")
          print(data.frame(tows[!tows$TOW_NO %in% tows$TOW_NO[mgt_matches_survey],]))
          
        }
      }
      
      if(unique(tows$MGT_AREA_CD) %in% c("BBn", "BBs")) {
        mgt_matches_survey <- grep(x=tows$SURVEY_NAME, pattern="BB")
        if(length(mgt_matches_survey) != length(tows$SURVEY_NAME)) {
          message("There are MGT_AREA_CDs in the tow file that do not match the SURVEY_NAME:")
          print(data.frame(tows[!tows$TOW_NO %in% tows$TOW_NO[mgt_matches_survey],]))
          
        }
      }
      
      if(unique(tows$MGT_AREA_CD) %in% c("GBa", "GBb")) {
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
      if(bank=="GBb" && any(tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO < 300 | tows[tows$TOW_TYPE_ID == "1 - Regular survey tow",]$TOW_NO > 331)) message("Unexpected tow numbering series. Should be 301-330\n")
      
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
      
      # calculating the distance of each tow
      tows_con$dist.calc <- distGeo(matrix(c(tows_con$START_LON_DD, tows_con$START_LAT_DD), ncol=2), matrix(c(tows_con$END_LON_DD, tows_con$END_LAT_DD), ncol=2))
      
      # flag the tow if the distance is greater than 2 km
      if(any(tows_con$dist.calc > 2000 | tows_con$dist.calc < 500)) {
        message("\nThere are some tows longer than 2km or shorter than 500m. Check their coordinates:")
        print(data.frame(tows_con[!is.na(tows_con$dist.calc) & (tows_con$dist.calc > 2000 | tows_con$dist.calc < 500),]))
        
      }
      
      # grab the authoritative management area polygons file. 
      area <- read.csv(paste0(direct, "Data/Maps/approved/Survey/survey_boundary_polygons.csv"))
      area <- area[!(area$startyear==1900 & area$label=="Sab"),] 
      area <- area[!(area$startyear==1900 & area$label=="Ban"),]
      
      
      if(bank=="GB") area <- area[!area$label %in% c("GBa", "GBb"),]
      if(!bank=="GB") area <- area[!area$label %in% c("GB"),]
      
      area$AREA_ID <- as.numeric(as.factor(area$label))
      
      # this creates strata labels
      area_lab <- ddply(.data=area, .(AREA_ID, label),
                        summarize,
                        LONGITUDE = mean(X), 
                        LATITUDE = mean(Y))
      
      if(spatialplot==TRUE){
        # based on start location first, then by end location. compare them to determine if they cross an area line.
        area.test <- NULL
        
        for(i in unique(area$AREA_ID)){
          
          points <- SpatialPoints(matrix(c(tows_con$START_LON_DD, tows_con$START_LAT_DD), ncol=2), proj4string=CRS("+proj=longlat +datum=WGS84"))
          
          coord_list <- split(area[area$AREA_ID %in% i, c("X", "Y", "AREA_ID")], area[area$AREA_ID %in% i,]$SID)
          coord_list <- lapply(coord_list, function(x) { x["AREA_ID"] <- NULL; x })
          ps <- sapply(coord_list, Polygon)
          p1 <- Polygons(ps, ID = 1) 
          
          # assuming offshore.csv polys are NAD83 but we really have no idea because RM didn't keep track...
          my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) 
          
          # create SpatialPolygons object
          my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +datum=WGS84") ) 
          
          test <- data.frame(TOW_NO = tows_con$TOW_NO, gContains(my_spatial_polys, points, byid=TRUE), AREA_ID=i)
          area.true <- subset(test, X1=="TRUE", select=c("TOW_NO", "AREA_ID"))
          area.test <- rbind(area.test, area.true)
        }
        area.test <- join(tows_con, area.test, type="full", by="TOW_NO")
        area.test <- join(area.test, unique(area[,c("label", "AREA_ID")]), type="left", by="AREA_ID")
        
        # by end location
        area.test.end <- NULL
        for(i in unique(area$AREA_ID)){
          
          points <- SpatialPoints(matrix(c(tows_con$END_LON_DD, tows_con$END_LAT_DD), ncol=2), proj4string=CRS("+proj=longlat +datum=WGS84"))
          
          coord_list <- split(area[area$AREA_ID %in% i, c("X", "Y", "AREA_ID")], area[area$AREA_ID %in% i,]$SID)
          coord_list <- lapply(coord_list, function(x) { x["AREA_ID"] <- NULL; x })
          ps <- sapply(coord_list, Polygon)
          p1 <- Polygons(ps, ID = 1) 
          
          # assuming offshore.csv polys are NAD83 but we really have no idea because RM didn't keep track...
          my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) 
          
          # create SpatialPolygons object
          my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +datum=WGS84") ) 
          
          test <- data.frame(TOW_NO = tows_con$TOW_NO, gContains(my_spatial_polys, points, byid=TRUE), AREA_ID=i)
          area.true <- subset(test, X1=="TRUE", select=c("TOW_NO", "AREA_ID"))
          area.test.end <- rbind(area.test.end, area.true)
        }
        area.test.end <- join(tows_con, area.test.end, type="full", by="TOW_NO")
        area.test.end <- join(area.test.end, unique(area[,c("label", "AREA_ID")]), type="left", by="AREA_ID")
        
        colnames(area.test)[which(names(area.test) == "label")] <- "bank.start"
        colnames(area.test.end)[which(names(area.test.end) == "label")] <- "bank.end"
        
        area.test.both <- join(area.test[,-which(names(area.test) == "AREA_ID")], area.test.end[,-which(names(area.test.end) == "AREA_ID")], type="left", 
                               by=c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NO", "START_LAT", "START_LON", "END_LAT", "END_LON", "dist.calc"))
        area.test.both$bank.start <- as.character(area.test.both$bank.start)
        area.test.both$bank.end <- as.character(area.test.both$bank.end)
        area.test.both$bank.start[is.na(area.test.both$bank.start)] <- "FALSE"
        area.test.both$bank.end[is.na(area.test.both$bank.end)] <- "FALSE"
        
        ## these ones cross a management area line or are outside the management area boundary
        if(any(!area.test.both$bank.end == area.test.both$bank.start | area.test.both$bank.end == "FALSE" | area.test.both$bank.start == "FALSE")) {
          message("\nThe following tows are outside the management area boundary (or cross the line):")
          print(area.test.both[!area.test.both$bank.end == area.test.both$bank.start | area.test.both$bank.end == "FALSE" | area.test.both$bank.start == "FALSE",])
          
        }
        
        area.test.both$flag[!area.test.both$bank.end == area.test.both$bank.start | area.test.both$bank.end == "FALSE" | area.test.both$bank.start == "FALSE"] <- "flag"
        area.test.both$flag[!(!area.test.both$bank.end == area.test.both$bank.start | area.test.both$bank.end == "FALSE" | area.test.both$bank.start == "FALSE")] <- "ok"
        colnames(area.test.both) <- make.unique(names(area.test.both))
        
        plot.list <- NULL
        ## plot tows to PDF
        for(i in unique(area$AREA_ID)){
          p <- ggplot() + geom_polygon(data=area[area$AREA_ID==i,], aes(X, Y, group=SID), fill=NA, colour="black", na.rm = T) +
            geom_text(data=area_lab[area_lab$AREA_ID==i,], aes(LONGITUDE, LATITUDE, label=label), size=4, colour="blue", na.rm = T) +
            coord_map() + 
            theme_bw() + theme(panel.grid=element_blank()) +
            geom_segment(data=area.test.both, aes(x=START_LON_DD, xend=END_LON_DD, y=START_LAT_DD, yend=END_LAT_DD, colour=flag), lwd=1, na.rm = T) +
            #scale_colour_manual(values=c("black", "white")) +
            geom_text(data=tows_con, aes(START_LON_DD, START_LAT_DD, label=TOW_NO), size=3, na.rm = T) +
            xlim(min(area[area$AREA_ID %in% i,]$X), max(area[area$AREA_ID %in% i,]$X)) +
            ylim(min(area[area$AREA_ID %in% i,]$Y)-0.05, max(area[area$AREA_ID %in% i,]$Y)+0.05)
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
      }
      
      if(!is.null(icetow)) {
        if(unique(icetow$SPECIES_ID) == "2 - Iceland scallop"){
        test <- dim(icetow)[1] == dim(join(tows[, c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NO", "TOW_TYPE_ID")], icetow[, c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NO")], type="full"))[1]
        if(test == FALSE) message("There is a difference in the tow metadata between sea scallop and icelandic tows")
        }
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
      if(any((comparehf$LIVE_QTY_BUCKET > 0 | comparehf$DEAD_QTY_BUCKET > 0) & (!is.na(comparehf$LIVE_QTY_BUCKET) | !is.na(comparehf$DEAD_QTY_BUCKET)) & (comparehf$Total.Buckets==0 | is.na(comparehf$Total.Buckets)) & !is.na(comparehf$TOW_TYPE_ID))) {
        message("Too many buckets in HF file compared to Total Buckets in Tow file")
        print(comparehf[which((comparehf$LIVE_QTY_BUCKET > 0 | comparehf$DEAD_QTY_BUCKET > 0) & (!is.na(comparehf$LIVE_QTY_BUCKET) | !is.na(comparehf$DEAD_QTY_BUCKET)) & (comparehf$Total.Buckets==0 | is.na(comparehf$Total.Buckets))),])
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
      
      longhfs <- melt(hfs,
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
