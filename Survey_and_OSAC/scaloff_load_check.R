### scaloff_check
### Script to run pre-loading checks on xlsx templates prior to loading to scaloff database

scaloff_check(tow=TRUE, hf=TRUE, mwsh=TRUE, year, direct="Y:/Offshore scallop/Assessment/",
              type="xlsx", 
              cruise, bank, survey_name, nickname=NULL) {
  
  ### packages
  require(readxl) || stop("Make sure you have readxl package installed to run this")
  require(plyr) || stop("Make sure you have plyr package installed to run this")
  require(geosphere) || stop("Make sure you have geosphere package installed to run this")
  
  ### other functions
  source(paste0(direct, "Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r"))
  
  ### load the data
  ## from the xlsx template:
  if(type=="xlsx"){
    if(!is.null(nickname)) {
      if(tow==TRUE) tows <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.tow.template", "_", nickname, ".xlsx"))
      if(hf==TRUE) hfs <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.hf.template", "_", nickname, ".xlsx"))
      if(mwsh==TRUE) mwshs <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.meat.shell.template", "_", nickname, ".xlsx"))
    }
    
    if(is.null(nickname)) {
      if(tow==TRUE) tows <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.tow.template.xlsx"))
      if(hf==TRUE) hf <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.hf.template.xlsx"))
      if(mwsh==TRUE) mwsh <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/OS.scallop.meat.shell.template.xlsx"))
    }
  }
  
  ## from a csv:
  if(type=="csv"){
    if(!is.null(nickname)) {
      if(tow==TRUE) tows <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/", survey_name, "tows_", nickname, ".csv"))
      if(hf==TRUE) hfs <- read(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/", survey_name, "hf_", nickname, ".csv"))
      if(mwsh==TRUE) mwshs <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/", survey_name, "mwsh_", nickname, ".csv"))
    }
    if(is.null(nickname)) {
      if(tow==TRUE) tows <- read.csv(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/", survey_name, "tows.csv"))
      if(hf==TRUE) hfs <- read(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/", survey_name, "hf.csv"))
      if(mwsh==TRUE) mwshs <- read_excel(paste0(direct, "Data/Survey_data/", year, "/Database loading/", cruise, "/", bank, "/", survey_name, "mwsh.csv"))
    }
  }
  
  ############################################
  
  ### check tow data
  if(tow==TRUE) {
    
    # If there are non-numeric values in TOW_NO column, print the offending tows
    if(any(!tows$TOW_NO %in% grep(x=tows$TOW_NO, pattern = "([[:digit:]])"))) {
      message("\nThere are non-numeric values in TOW_NO column. Check the following:")
      print(data.frame(tows[!tows$TOW_NO %in% tows$TOW_NO[grep(x=tows$TOW_NO, pattern = "([[:digit:]])")],]))
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
    if(any(!tows$MGT_AREA_CD %in% c("GBa", "GBb", "Ger", "BBn", "BBs", "Mid", "Sab", "SPB", "Ban"))){
      message("\nThere are values in MGT_AREA_CD that are not contained in the validation table. The data cannot be loaded to the DB like this.
Check the MGT_AREA_CD values for the following tows:")
      print(data.frame(tows[!tows$MGT_AREA_CD %in% c("GBa", "GBb", "Ger", "BBn", "BBs", "Mid", "Sab", "SPB", "Ban"),]))
    }
    
    # check depth. Must be between 20 and 200.
    if(any(tows$DEPTH_F < 20 | tows$DEPTH_F > 200)){
      message("\nThere are values in DEPTH_F that are less than 20 or greater than 200. Check DEPTH_F for the following tows:")
      print(data.frame(tows[tows$DEPTH_F < 20 | tows$DEPTH_F > 200,]))
    }
    
    # check tow date. It can be NULL, but the DB will produce a warning. Let's flag any nulls or iffy values here first. 
    if(any(is.null(tows$TOW_DATE) | is.na(tows$TOW_DATE))) {
      message("\nThe following tows are missing a TOW_DATE. The DB will accept this, so make sure it's what you want!")
      print(data.frame(tows[is.null(tows$TOW_DATE) | is.na(tows$TOW_DATE),]))
    }
    
    # these tow dates might not be formatted correctly:
    if(any(is.na(dmy(tows$TOW_DATE, quiet=T)) & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE))) {
      message("\nThe following tows have dates that may not be formatted correctly. They should look like dd/mm/yyyy.")
      print(data.frame(tows[which(is.na(dmy(tows$TOW_DATE, quiet = T)) & !is.na(tows$TOW_DATE) & !is.null(tows$TOW_DATE)),]))
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
      any(tows[,c("START_LON","END_LON")] > -5500 | tows[,c("START_LON", "END_LON")] < -6700, na.rm=T)){
      message("\nThere are values in the longitude columns that are outside the bounds. Check the following:")
      print(data.frame(tows[tows$START_LON > -5500 | tows$END_LON > -5500 | tows$START_LON < -6700 | tows$END_LON < -6700,]))
    }
    
    # spatial check coordinates relative to mgt_area_cd shapefile. Make plots. This is adapted from check.tows.spatial.R which is used for Inshore Survey. 

    # converting all lats and longs to decimal degrees
    tows_con <- cbind(tows[, c("CRUISE", "SURVEY_NAME", "MGT_AREA_CD", "TOW_DATE", "TOW_NO")],
      apply(tows[,c("START_LAT","START_LON", "END_LAT", "END_LON")], 2, function(x) convert.dd.dddd(format="dec.deg", x)))

    # calculating the distance of each tow
    tows_con$dist.calc <- distGeo(matrix(c(tows_con$START_LON, tows_con$START_LAT), ncol=2), matrix(c(tows_con$END_LON, tows_con$END_LAT), ncol=2))
    
    # flag the tow if the distance is greater than 2 km
    if(any(tows_con$dist.calc > 2000 | tows_con$dist.calc < 500)) {
      message("\nThere are some tows longer than 2km or shorter than 500m. Check their coordinates:")
      print(data.frame(tows_con[!is.na(tows_con$dist.calc) & (tows_con$dist.calc > 2000 | tows_con$dist.calc < 500),]))
    }
    
    # grab the authoritative management area polygons file. 
    area <- read.csv(paste0(direct, "Data/Maps/approved/Survey/survey_boundary_polygons.csv"))
    area <- area[!(area$startyear==1900 & area$label=="Sab"),] 
    
    area$AREA_ID <- as.numeric(as.factor(area$label))
    
    # this creates strata labels
    area_lab <- ddply(.data=area, .(AREA_ID, label),
                      summarize,
                      LONGITUDE = mean(X), 
                      LATITUDE = mean(Y))
    
    # based on start location first, then by end location. compare them to determine if they cross an area line.
    area.test <- NULL
    for(i in unique(area$AREA_ID)){
      
      points <- SpatialPoints(matrix(c(tows_con$START_LON, tows_con$START_LAT), ncol=2), proj4string=CRS("+proj=longlat +datum=WGS84"))
      
      coord_list <- split(area[area$AREA_ID %in% i, c("X", "Y", "AREA_ID")], area[area$AREA_ID %in% i,]$SID)
      coord_list <- lapply(coord_list, function(x) { x["AREA_ID"] <- NULL; x })
      ps <- sapply(coord_list, Polygon)
      p1 <- Polygons(ps, ID = 1) 
      
      # assuming offshore.csv polys are NAD83 but we really have no idea because RM didn't keep track...
      my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +towgs84=0,0,0,0 +no_defs")) 
      
      # create SpatialPolygons object
      my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +datum=WGS84") ) 
      
      test <- data.frame(TOW_NO = tows_con$TOW_NO, gContains(my_spatial_polys, points, byid=TRUE), AREA_ID=i)
      area.true <- subset(test, X1=="TRUE", select=c("TOW_NO", "AREA_ID"))
      area.test <- rbind(area.test, area.true)
    }
    area.test <- join(tows_con, area.test, type="full")
    area.test <- join(area.test, unique(area[,c("label", "AREA_ID")]), type="left")
    
    # by end location
    area.test.end <- NULL
    for(i in unique(area$AREA_ID)){
      
      points <- SpatialPoints(matrix(c(tows_con$END_LON, tows_con$END_LAT), ncol=2), proj4string=CRS("+proj=longlat +datum=WGS84"))
      
      coord_list <- split(area[area$AREA_ID %in% i, c("X", "Y", "AREA_ID")], area[area$AREA_ID %in% i,]$SID)
      coord_list <- lapply(coord_list, function(x) { x["AREA_ID"] <- NULL; x })
      ps <- sapply(coord_list, Polygon)
      p1 <- Polygons(ps, ID = 1) 
      
      # assuming offshore.csv polys are NAD83 but we really have no idea because RM didn't keep track...
      my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +towgs84=0,0,0,0 +no_defs")) 
      
      # create SpatialPolygons object
      my_spatial_polys <- SpatialPolygons(list(p1), proj4string = CRS("+proj=longlat +datum=WGS84") ) 
      
      test <- data.frame(TOW_NO = tows_con$TOW_NO, gContains(my_spatial_polys, points, byid=TRUE), AREA_ID=i)
      area.true <- subset(test, X1=="TRUE", select=c("TOW_NO", "AREA_ID"))
      area.test.end <- rbind(area.test.end, area.true)
    }
    area.test.end <- join(tows_con, area.test.end, type="full")
    area.test.end <- join(area.test.end, unique(area[,c("label", "AREA_ID")]), type="left")
  
    colnames(area.test)[which(names(area.test) == "label")] <- "bank.start"
    colnames(area.test.end)[which(names(area.test.end) == "label")] <- "bank.end"
    
    area.test.both <- join(area.test[,-which(names(area.test) == "AREA_ID")], area.test.end[,-which(names(area.test.end) == "AREA_ID")], type="left")
    area.test.both$bank.start[is.na(area.test.both$bank.start)] <- "FALSE"
    area.test.both$bank.end[is.na(area.test.both$bank.end)] <- "FALSE"
    
    ## these ones cross a management area line or are outside the management area boundary
    if(any(!area.test.both$bank.end == area.test.both$bank.start | area.test.both$bank.end == FALSE | area.test.both$bank.start == FALSE)) {
      message("\nThe following tows are outside the management area boundary (or cross the line):")
      print(area.test.both[!area.test.both$bank.end == area.test.both$bank.start | area.test.both$bank.end == FALSE | area.test.both$bank.start == FALSE,])
    }
    
    area.test.both$flag[!area.test.both$bank.end == area.test.both$bank.start | area.test.both$bank.end == FALSE | area.test.both$bank.start == FALSE] <- "flag"
    area.test.both$flag[!(!area.test.both$bank.end == area.test.both$bank.start | area.test.both$bank.end == FALSE | area.test.both$bank.start == FALSE)] <- "ok"
    
    plot.list <- NULL
    ## plot tows to PDF
    for(i in unique(area$AREA_ID)){
      p <- ggplot() + geom_polygon(data=area[area$AREA_ID==i,], aes(X, Y, group=SID), fill=NA, colour="black") +
        geom_text(data=area_lab[area_lab$AREA_ID==i,], aes(LONGITUDE, LATITUDE, label=label), size=4, colour="blue") +
        coord_map() + 
        theme_bw() + theme(panel.grid=element_blank()) +
        geom_segment(data=area.test.both, aes(x=START_LON, xend=END_LON, y=START_LAT, yend=END_LAT, colour=flag), lwd=1) +
        #scale_colour_manual(values=c("black", "white")) +
        geom_text(data=tows_con, aes(START_LON, START_LAT, label=TOW_NO), size=3) +
        xlim(min(area[area$AREA_ID %in% i,]$X), max(area[area$AREA_ID %in% i,]$X)) +
        ylim(min(area[area$AREA_ID %in% i,]$Y)-0.05, max(area[area$AREA_ID %in% i,]$Y)+0.05)
      plot.list[[i]] <- p
    }
    
    if(!is.null(nickname)) {
      pdf(paste0(direct, "/Data/Survey_data/", year, "/Database loading/spatial_checks_", nickname, ".pdf"),onefile=T,width=22,height=12)
      print(plot.list)
      dev.off()
    }
    if(!is.null(nickname)) {
      pdf(paste0(direct, "/Data/Survey_data/", year, "/Database loading/spatial_checks.pdf"),onefile=T,width=22,height=12)
      print(plot.list)
      dev.off()
    }
  }

  
  
####################### Height frequency checks
  
  if(hf==TRUE){
  
    # plot the raw HF distributions by tow (one tow per pdf page)
    ggplot() + geom_histogram(data=hfs, aes(BIN_ID, LIVE_QTY_BASKET), stat="identity") + 
      facet_wrap(~TOW_NUM, scales="free") + theme_bw() + theme(panel.grid=element_blank())
    # ^^ still to do
    
    # plot max tow size per bin, 
    
    
      
    }

  
}
