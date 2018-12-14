# Here is a simple little function to load in multiple shapefile layers into 1 object in R.  Found on stack exchange and modified for SABHU by DK
# If looking for background go here  https://gis.stackexchange.com/questions/194945/read-multiple-layers-of-kml-file-using-r


# Arguements:
#1:   loc    The folder in which all the different shapefile layers are found.



all.layers <- function(loc)
{
  require(rgdal)  || stop("You need to have the package 'rgdal' or this won't work pal")
  # This gets the list of all the layers
  lyr <- ogrListLayers(loc)
  # Creates an object.
  my.shp <- list()
  # Run this for each layer
  for (i in 1:length(lyr)) 
  {
    my.shp[i] <- readOGR(loc,lyr[i])
  } # end for (i in 1:length(lyr)) 
  # Give each layer a proper name and return the results for later.
  names(my.shp) <- lyr
  return(my.shp)
} # end all.layers <- function(loc)