# This is a function to convert  an ESRI raster geodatabase
# to an R raster.
# Note that you need a 32 bit version of R for this to work...
# because our version of Arc is 32 bit.  64 bit R would work
# once we have ArcPro on network.
# Also note that you need access to an Arc license for this
# as well, so need to be on DFO network.

# Arguements
#1: gd.input:     The folder in which the ESRI geodatabase resides, 
#                   see for example "D:\NAS\Projects\GB_time_area_closure_SPERA\Data\CoML_layers.gdb\sst_avg"
#                   Note that the sst_avg is not a folder inside this, it is an object in the 
#                   Geodatabase, it isn't obvious that it is in there without knowing what is
#                   in the geodatabase.
#2: raster.output: Where do you want to save your raster.  This should be something like "D:/location/gd.name.RData"

gd.rasters <- function(gd.input, raster.output)
{
library(arcgisbinding)
library(sf)
library(raster)
library(stars)
arc.check_product() 
raster.layer <- as.raster(arc.raster(arc.open(gd.input)))

save.image(raster.output)
}