## SHP to GEOTIFF FOR SURVEY STRATA
## Adjust resolution as needed by changing ncols and nrows within the function

shp_to_tiff() <- function(direct=direct, folder=folder, file=file, bank=bank, res="low", 
                          saveas=NULL){
  require(rgdal)
  require(rgeos)
  require(raster)
  
  shp <- readOGR(dsn = paste0(direct, folder, file), layer = "Sab")
  
  ## the shp is in WGS84:
  # shp@proj4string
  
  ## get the extent
  shp_f <- fortify(shp)
  extentshp <- c(min(shp_f$long), max(shp_f$long), min(shp_f$lat), max(shp_f$lat))
  
  if(class(shp) == "SpatialLinesDataFrame") {
    source(paste0(direct, "/Assessment_fns/Maps/lines_to_poly.R"))
    shp <- lines_to_poly(direct=direct, folder=folder, file=file, splitlayers=F, saveas=NULL)
  }
  
  ## convert SpatialPolygonsDataFrame to SpatialPolygons
  shp <- as(shp, "SpatialPolygons")
  
  ## create a raster frame
  if(res="low") r <- raster(ncols=100, nrows=100)
  if(res="high") r <- raster(ncols=10000, nrows=10000)
  extent(r) <- extentshp
  rastershp <- rasterize(shp, r, fun="first")

  rat <- ratify(rastershp)
  rat <- levels(rat)[[1]]
  levels(rastershp) <- rat
  rat.df <- data.frame(value=rat$ID,color=brewer_pal()(length(rat$ID)),attribute=rat$ID)
  
  if(!is.null(saveas))  writeRaster(rastershp, paste0(direct, folder, saveas), format = "GTiff", overwrite=T)
  
  writeGDAL(rastershp, paste0(direct, folder, saveas), type="Byte", 
            colorTable=list(rat.df$color)[[1]], 
            catNames=list(rat.df$attribute)[[1]], mvFlag=11L)
  ### very stuck here!! Hmm...
  
  
  rat.df$color<- as.character(rat.df$color)
  rat.df$attribute<- as.character(rat.df$attribute)
  outRst <- writeGDAL(r, outRstName, type="Byte", 
                      colorTable=list(rat.df$color), 
                      catNames=list(rat.df$attribute), mvFlag=11L)
  
  
}
