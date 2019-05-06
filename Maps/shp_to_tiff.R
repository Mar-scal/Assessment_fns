## SHP to GEOTIFF FOR SURVEY STRATA
### Incomplete. Nothing is written to file at this point. (May 6, 2019)

shp_to_tiff() <- function(direct=direct, folder=folder, file=file, bank=bank, res="low"){
  require(rgdal)
  require(rgeos)
  require(raster)
  
  shp <- readOGR(dsn = paste0(direct, folder, file), layer = "Sab")
  plot(shp)
  
  ## the shp is in WGS84:
  # shp@proj4string
  
  ## get the extent
  shp_f <- fortify(shp)
  extentshp <- c(min(shp_f$long), max(shp_f$long), min(shp_f$lat), max(shp_f$lat))
  
  ## convert SpatialLinesDataFrame to SpatialLines
  shp <- as(shp, "SpatialLines")
  
  ## create a raster frame
  if(res="low") r <- raster(ncols=100, nrows=100)
  if(res="high") r <- raster(ncols=1000, nrows=1000)
  extent(r) <- extentshp
  rastershp <- rasterize(shp, r, fun="first")
  
  #plot(rastershp)
  
}