# Function to convert a dataframe of points to a polygon
points_to_polygon <- function(points_df=data.frame(X=c(-63.635693, -63.657874, -63.666479, -63.634416,-63.619178, -63.616740),
                                                   Y=c(44.668185, 44.682985, 44.716164, 44.705941, 44.694069, 44.677514)),
                              crs=4326){
  require(sf)
  points_sf <- st_as_sf(points_df, coords=c(X="X", Y="Y"), crs=crs)
  poly_sf <- st_combine(points_sf) %>% st_cast("POLYGON")
  return(poly_sf)
}


#### EXAMPLE:
# create a dataframe of your coordinates,in the order that you want them to be connected.
# test <- data.frame(X=c(-63.635693, -63.657874, -63.666479, -63.634416,-63.619178, -63.616740),
#                    Y=c(44.668185, 44.682985, 44.716164, 44.705941, 44.694069, 44.677514))
# apply the function to the dataframe. set the coordinate system as needed. 
# out <- points_to_polygon(points_df = test,
#                   crs=4326)
# plot(out)
