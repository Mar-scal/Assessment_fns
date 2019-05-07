lines_to_poly <- function(direct=direct, folder=folder, file=file,
                          saveas=NULL) {
  shp <- readOGR(dsn = paste0(direct, folder, file), layer = paste0(gsub(x=file, pattern=".shp", replacement="", fixed=T)))
  if(class(shp) == "SpatialLinesDataFrame"){
    shp_df <- as.data.frame(shp)
    shp <- as(shp, "SpatialLines")
    shp <- maptools::SpatialLines2PolySet(shp)
    shp <- maptools::PolySet2SpatialPolygons(shp)
    
    shp <- SpatialPolygonsDataFrame(shp, shp_df, match.ID = F)
    
    shp <- createSPComment(shp)
    
    for(i in 1:length(shp$ID)) {
      windows()
      print(plot(shp[i,], col="blue"))
    }

    if(!is.null(saveas)) writeOGR(obj = shp, dsn = paste0(direct, file, saveas), layer = paste0(gsub(x=saveas, pattern=".shp", replacement="", fixed=T)),
                                  driver="ESRI Shapefile")
    
  }
}

