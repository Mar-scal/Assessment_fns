## SHP to GEOTIFF FOR SURVEY STRATA
## Adjust resolution as needed by changing ncols and nrows within the function

shp_to_tiff() <- function(direct=direct, folder=folder, file=file, bank=bank, res="low", 
                          saveas=NULL){
  require(rgdal)
  require(rgeos)
  require(raster)
  
  shp <- readOGR(dsn = paste0(direct, folder, file), layer = "Sab_verylow")
  
  ## the shp is in WGS84:
  # shp@proj4string
  
  ## get the extent
  shp_f <- fortify(shp)
  extentshp <- c(min(shp_f$long), max(shp_f$long), min(shp_f$lat), max(shp_f$lat))
  
  ## convert SpatialPolygonsDataFrame to SpatialPolygons
  shp <- as(shp, "SpatialPolygons")
  
  ## create a raster frame
  if(res="low") r <- raster(ncols=100, nrows=100)
  if(res="high") r <- raster(ncols=10000, nrows=10000)
  extent(r) <- extentshp
  rastershp <- rasterize(shp, r, fun="first")
 
  if(!is.null(saveas)) writeRaster(rastershp, paste0(direct, folder, saveas), options=c('TFW=YES'))
}