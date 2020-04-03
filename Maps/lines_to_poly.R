# A function to turn MultiLinestring object into a Multi Polygon object in SF.  Added an option to convert what is returned as an sf object instead of an sp object

# Arguements:
#1:   direct        The directory in which your shapefile resides, usually something like "Y:/Offshore/Assessment/"...
#2:   folder        The folder inside that directorty you are looking for, e.g. "Data/Maps/approved/GIS_layers/offshore/"
#3:   file          The shapefile you want to convert from a spatial line to a spatail polygon, eg "BBs.shp"
#4:   splitlayers   Save the layers as seperate shapefiles (I think), only done for saving Shapefile into a directory (i.e. saveas = "a_file_name_of_some_sort")
#5:   saveas        Do you want to save the shapefile to a directory, note it saves to the same folder as it comes from so could overwrite data. This should have a ".shp" extenstion
#6:   make.sf       Do you want to make the objected returned in your R workspace an sf object.

lines_to_poly <- function(direct=direct, folder=folder, file=file, splitlayers=T,
                          saveas=NULL,make.sf=F) {
  require(rgdal)
  require(sp)
  require(rgeos)
  require(sf)
  
  #shp <- readOGR(dsn = paste0(direct, folder, file), layer = paste0(gsub(x=file, pattern=".shp", replacement="", fixed=T)))
  shp <- st_read(paste0(direct, folder, file))
  if(class(st_geometry(shp))[1] == "sfc_MULTILINESTRING") shp <- st_cast(shp, "MULTIPOLYGON")

    if(!is.null(saveas) & splitlayers==F) st_write(obj = shp, dsn = paste0(direct, folder , saveas), layer = paste0(gsub(x=saveas, pattern=".shp", replacement="", fixed=T)),
                                                   driver="ESRI Shapefile")
    
    if(!is.null(saveas) & splitlayers==T) {
      for(i in 1:length(shp$ID)) {
        saveas_ID <- gsub(x = saveas, pattern = ".shp", replacement = paste0("_", i, ".shp"))
        shp_sub <- shp[i,]
        st_write(obj = shp_sub, dsn = paste0(direct, folder,saveas_ID), layer = paste0(gsub(x=saveas_ID, pattern=".shp", replacement="", fixed=T)),
                 driver="ESRI Shapefile")
      }
    }
    # If you want to make the shapefile into an SF object that's Easy P. Easy
    if(make.sf == F) shp <- as_Spatial(shp)
    return(shp)
}