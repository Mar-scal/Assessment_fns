# Here is a simple little function to load in multiple shapefile layers into 1 object in R.  Found on stack exchange and modified for SABHU by DK
# If looking for background go here  https://gis.stackexchange.com/questions/194945/read-multiple-layers-of-kml-file-using-r
# Note that the object returned is now an SF object (FK sometime in 2020)

# Arguements:
#1:   loc         The folder in which all the different shapefile layers are found.
#2:   make.sf     Do you want the object returned to be an SF or an sp object.  Defaults to = F to maintain backwards compatability with older code.
#3:   make.polys  Do you want the object to be returned as a polygon.  This is handy if you have a shapefile that is lines but you want polygons returned
all.layers <- function(loc,make.sf = F,make.polys=F)
{
  require(sf)  || stop("You need to have the package 'sf' or this won't work pal")
  require(rgdal)  || stop("You need to have the package 'sf' or this won't work pal")
  # This gets the list of all the layers
  lyr <- ogrListLayers(loc)
  
  # Creates an object.
  my.shp <- NULL
  # Run this for each layer
  for (i in 1:length(lyr)) 
  {
    my.shp[[i]] <- st_read(paste0(loc,"/",lyr[i], ".shp"))
    # If you want to return the layer as a polygon, should be a multilinesegment or similar.
    if(make.polys == T) 
    {
      #my.shp[[i]] <- concaveman(my.shp[[i]])
      my.shp[[i]] <- st_cast(my.shp[[i]],"MULTIPOLYGON")
    }
    # Need to put a name on it now
    if(nrow(my.shp[[i]]) == 1) my.shp[[i]]$ID <- lyr[i]
    if(nrow(my.shp[[i]])  >1) my.shp[[i]]$ID <- paste0(lyr[i],"_",my.shp[[i]]$ID)
    
  } # end for (i in 1:length(lyr)) 
  
  num.cols <- unique(purrr::map_chr(my.shp, function(x) ncol(x)))
  if(length(num.cols) > 1){
    for (i in 1:length(lyr)) 
    {
      if(i==1) full.shp <- my.shp[[i]]
      if(i>1) full.shp <- st_union(full.shp, st_transform(my.shp[[i]], st_crs(full.shp)))
    }
  }
  if(length(num.cols) == 1){
    full.shp <- NULL
    for (i in 1:length(lyr)) 
    {
      if(i == 1) crs.1 <- st_crs(my.shp[[i]])
      full.shp <- rbind(full.shp, st_transform(my.shp[[i]], st_crs(crs.1)))
    }
  }
  # Convert to an SP object as required.
  if(make.sf ==F) full.shp <- as_Spatial(full.shp)
  # Give each layer a proper name and return the results for later.
  #names(my.shp) <- full.shp.sf
  return(full.shp)
} # end all.layers <- function(loc)