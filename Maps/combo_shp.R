# Here is a simple little function to load in multiple shapefile layers into 1 object in R.  Found on stack exchange and modified for SABHU by DK
# If looking for background go here  https://gis.stackexchange.com/questions/194945/read-multiple-layers-of-kml-file-using-r
# Note that the object returned is now an SF object (FK sometime in 2020)

# Arguements:
#1:   loc         The folder in which all the different shapefile layers are found.
#2:   make.sf     Do you want the object returned to be an SF or an sp object.  Defaults to = F to maintain backwards compatability with older code.
#3:   make.polys  Do you want the object to be returned as a polygon.  This is handy if you have a shapefile that is lines but you want polygons returned
combo.shp <- function(loc,make.sf = F,make.polys=F,make.lines =F, quiet=F)
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
    my.shp[[i]] <- st_read(paste0(loc,"/",lyr[i], ".shp"), quiet=quiet)
    # If your shapefile doesn't have a CRS set it to 4326 and print a warning
    if(is.na(st_crs(my.shp[[i]]))) 
    {
      my.shp[[i]] <- my.shp[[i]] %>% st_set_crs(4326) 
      print("Heads up that your shapefile did not have a CRS set, it was set to EPSG 4326")
    } 
    # If you want to return the layer as a polygon, should be a multilinesegment or similar.
    if(make.polys == T) 
    {
      #my.shp[[i]] <- concaveman(my.shp[[i]])
      my.shp[[i]] <- st_cast(my.shp[[i]],"MULTIPOLYGON")
    }
    
    if(make.lines == T) 
    {
      #my.shp[[i]] <- concaveman(my.shp[[i]])
      my.shp[[i]] <- st_cast(my.shp[[i]],"MULTILINESTRING")
    }
    
    # Need to put a name on it now
    if(nrow(my.shp[[i]]) == 1) my.shp[[i]]$ID <- lyr[i]
    if(nrow(my.shp[[i]])  >1) my.shp[[i]]$ID <- paste0(lyr[i],"_",my.shp[[i]]$ID)
    
  } # end for (i in 1:length(lyr)) 
  
  # So this checks if the number of columns in the data is all the same, if so
  # they are easy to merge...
  num.cols <- unique(purrr::map_chr(my.shp, function(x) ncol(x)))
  # The number of colums is not the same we need to clean up the data so we can merge them into one object
  # We should also spit a warning here...
  if(length(num.cols) > 1){
    cat("Warning, you are combining shapefiles with different numbers of columns, this method retains only the first column which we rename 'ID'
       so you might want to try a different tool to combine your data as this method will lead to loss of some data!!!")
    for (i in 1:length(lyr)) 
    {
      # Just keep the first column
      my.shp[[i]] <- my.shp[[i]][,1]
      # And rename that column to ID
      names(my.shp[[i]]) <- c("ID","geometry")
      if(i==1) full.shp <- my.shp[[i]]
      if(i>1) full.shp <- rbind(full.shp,st_transform(my.shp[[i]],st_crs(full.shp)))
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