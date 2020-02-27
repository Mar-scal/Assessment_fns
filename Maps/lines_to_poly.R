lines_to_poly <- function(direct, direct_fns, folder=folder, file=file, splitlayers=T,
                          saveas=NULL) {
  require(rgdal)
  require(sp)
  require(rgeos)
  
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
    
    if(!is.null(saveas) & splitlayers==F) writeOGR(obj = shp, dsn = paste0(direct, file, saveas), layer = paste0(gsub(x=saveas, pattern=".shp", replacement="", fixed=T)),
                                                   driver="ESRI Shapefile")
    
    if(!is.null(saveas) & splitlayers==T) {
      for(i in 1:length(shp$ID)) {
        saveas_ID <- gsub(x = saveas, pattern = ".shp", replacement = paste0("_", i, ".shp"))
        shp_sub <- shp[i,]
        writeOGR(obj = shp_sub, dsn = paste0(direct, folder, saveas_ID), layer = paste0(gsub(x=saveas_ID, pattern=".shp", replacement="", fixed=T)),
                 driver="ESRI Shapefile")
      }
      
    }
    return(shp)
  }
}