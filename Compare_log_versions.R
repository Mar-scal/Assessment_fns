# This function can be used to compare two log CSV files, so that you can see what corrections have been made in the latest version. 
# Useful for checking Bycatch spreadsheet and catch effort tables, but needs more testing!
# FK March 1, 2019
######################################
## Arguments:
# old_log: path to the old version of the log.csv that you want to compare to
# new_log: path to the new version of the log.csv that you want to compare to
# check_all: compares the values in the following columns - "VR_NUMBER", "TRIP_ID", "NAFO_UNIT_AREA", "FISHING_AREA", "DATE_FISHED", "WATCH", "NO_RAKES_FISHED", "NO_TOWS_PER_WATCH", "AVG_TOW_TIME" 
# check_kg_hm: compares prorepwt and db_new$NO_RAKES_FISHED*db_new$NO_TOWS_PER_WATCH*(db_new$AVG_TOW_TIME/60) (NOT ACTUALLY EFFORT!) 
### NOTE: DOES NOT HANDLE SLIPS YET!

compare_logs <- function(old_log = PATH, new_log = PATH, check_all=T, check_kg_hm=T, by="month"){
  require(compareDF)
  
  db_old <- read.csv(old_log)
  db_old$row <- 1:dim(db_old)[1]
  
  db_new <- read.csv(new_log)
  db_new$row <- 1:dim(db_new)[1]
  
  if(any(grepl(names(db_old), pattern="X")==TRUE)) db_old <- db_old[, -which(grepl(names(db_old), pattern="X"))]
  if(any(grepl(names(db_new), pattern="X")==TRUE)) db_new <- db_new[, -which(grepl(names(db_new), pattern="X"))]
  
  comparisondf <- NULL
  j<-NULL
  for(i in seq(1,dim(db_new)[1],100)){
    if(!i==seq(1,dim(db_new)[1],100)[length(seq(1,dim(db_new)[1],100))]) j<-i+100
    if(i==seq(1,dim(db_new)[1],100)[length(seq(1,dim(db_new)[1],100))]) j<-dim(db_new)[1]
    
    if(check_all==T) test <- compare_df(db_new[i:j,c("row", "VR_NUMBER", "TRIP_ID", "NAFO_UNIT_AREA", "FISHING_AREA", "DATE_FISHED", "WATCH", "NO_RAKES_FISHED", "NO_TOWS_PER_WATCH", "AVG_TOW_TIME")], 
                                        db_old[i:j,c("row", "VR_NUMBER", "TRIP_ID", "NAFO_UNIT_AREA", "FISHING_AREA", "DATE_FISHED", "WATCH", "NO_RAKES_FISHED", "NO_TOWS_PER_WATCH", "AVG_TOW_TIME")], 
                                        "row", tolerance = 0, stop_on_error = F) 
    if(!check_all==T) test <- compare_df(db_new[i:j,c("row", check_all)], 
                                         db_old[i:j,c("row", check_all)], 
                                         "row", tolerance = 0, stop_on_error = F) 
    print(test$html_output)
    comparisondf <- rbind(comparisondf, test$comparison_df)
  }
  
  if(!is.null(comparisondf)) {
    comparisondf <- dplyr::select(comparisondf, -"chng_type")
    for(i in seq(1, dim(comparisondf)[1], 2)){
      print(paste0("TRIP_ID=", comparisondf[i,]$TRIP_ID, " Row number=", comparisondf[i,]$row))
      prints <- paste0(names(comparisondf)[which(!comparisondf[i,] == comparisondf[i+1,])], " changed from ", 
                   comparisondf[c(i),which(!comparisondf[i,] == comparisondf[i+1,])], " to ",
                   comparisondf[c(i+1),which(!comparisondf[i,] == comparisondf[i+1,])])
      print(prints[!which(grepl(prints, pattern="row"))])
    }
  }
  
  ## trip totals
  if(check_kg_hm==T) {
    db_old$month <- month(dmy(db_old$DATE_FISHED))
    db_new$month <- month(dmy(db_new$DATE_FISHED))
    
    db_old$hmproxy <- db_old$NO_RAKES_FISHED*db_old$NO_TOWS_PER_WATCH*(db_old$AVG_TOW_TIME/60)
    db_new$hmproxy <- db_new$NO_RAKES_FISHED*db_new$NO_TOWS_PER_WATCH*(db_new$AVG_TOW_TIME/60)
    
    if(by=="month") db_old_prorepwt <- ddply(.data=db_old, .(NAFO_UNIT_AREA, month),
                                             summarise,
                                             totalrndwt = sum(PRORATED_RND_WEIGHT_KGS, na.rm=T),
                                             hmproxy = sum(hmproxy, na.rm=T))
    if(by=="month") db_new_prorepwt <- ddply(.data=db_new, .(NAFO_UNIT_AREA, month),
                                             summarise,
                                             totalrndwt = sum(PRORATED_RND_WEIGHT_KGS, na.rm=T),
                                             hmproxy = sum(hmproxy, na.rm=T))
    
    if(by=="trip") db_old_prorepwt <- ddply(.data=db_old, .(TRIP_ID, NAFO_UNIT_AREA),
                                            summarise,
                                            totalrndwt = sum(PRORATED_RND_WEIGHT_KGS, na.rm=T),
                                            hmproxy = sum(hmproxy, na.rm=T))
    if(by=="trip") db_new_prorepwt <- ddply(.data=db_new, .(TRIP_ID, NAFO_UNIT_AREA),
                                            summarise,
                                            totalrndwt = sum(PRORATED_RND_WEIGHT_KGS, na.rm=T),
                                            hmproxy = sum(hmproxy, na.rm=T))
    
    db_old_prorepwt$row <- 1:nrow(db_old_prorepwt)
    db_new_prorepwt$row <- 1:nrow(db_new_prorepwt)
    comparisondf2 <- NULL
    j<-NULL
    for(i in seq(1,dim(db_old_prorepwt)[1],100)){
      if(!i==seq(1,dim(db_old_prorepwt)[1],100)[length(seq(1,dim(db_old_prorepwt)[1],100))]) j<-i+100
      if(i==seq(1,dim(db_old_prorepwt)[1],100)[length(seq(1,dim(db_old_prorepwt)[1],100))]) j<-dim(db_old_prorepwt)[1]
      
      test <- compare_df(db_new_prorepwt[i:j,], 
                         db_old_prorepwt[i:j,], 
                         "row", tolerance = 0, stop_on_error = F) 
      print(test$html_output)
      comparisondf2 <- rbind(comparisondf2, test$comparison_df)
    }
    
    comparisondf2 <- dplyr::select(comparisondf2, -"chng_type")
    for(i in seq(1, dim(comparisondf2)[1], 2)){
      if(by=="month") print(paste("month", comparisondf2[i,]$month, "NAFO", comparisondf2[i,]$NAFO_UNIT_AREA, sep=" "))
      if(by=="trip") print(paste("TRIP_ID", comparisondf2[i,]$TRIP_ID, "NAFO", comparisondf2[i,]$NAFO_UNIT_AREA, sep=" "))
      print(paste0("Changed variable: ", names(comparisondf2)[which(!comparisondf2[i,] == comparisondf2[i+1,])]))
      print("Before/After:")
      print(comparisondf2[c(i, i+1),which(!comparisondf2[i,] == comparisondf2[i+1,])])
      print("Difference(s): ")
      print(comparisondf2[i+1,which(!comparisondf2[i,] == comparisondf2[i+1,])] - comparisondf2[i,which(!comparisondf2[i,] == comparisondf2[i+1,])])
    }
  }
}

compare_logs(old_log, new_log, check_all="VR_NUMBER")
