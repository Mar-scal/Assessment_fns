# create a grid template for rasterizing other data
# gridsize: length of each side of the grid cell in metres
# polygon: the name of the polygon you want to grid

create_grid <- function(gridsize, polygon) {
  require(sf)
  require(raster)
  require(stars)
  nx <- round((st_bbox(polygon)$xmax[[1]] - st_bbox(polygon)$xmin[[1]])/gridsize, 0)
  ny <- round((st_bbox(polygon)$ymax[[1]] -  st_bbox(polygon)$ymin[[1]])/gridsize, 0)
  # grid cells are 5 km2
  
  # adjust these numbers to get exactly 1km2
  xmin <- st_bbox(polygon)$xmin[[1]]
  xmax <- xmin + ((nx+1)*gridsize)
  ymin <- st_bbox(polygon)$ymin[[1]]
  ymax <- ymin + ((ny+1)*gridsize)
  box <- st_as_sf(x = expand.grid(x=c(xmin,xmax), y=c(ymin, ymax))[c(1,2,4,3),],coords=c(X="x", Y="y"), crs=st_crs(polygon)) %>% 
    st_combine() %>% 
    st_cast("POLYGON")
  r <- st_rasterize(sf = st_sf(box), template = st_as_stars(box, nx = (nx+1), ny = (ny+1)))
  r <- st_as_sf(r)
  r$cell <- 1:nrow(r)
  return(r)
}


# example:
# GBraster <- create_grid(15000, GB)
# fish_grid <- st_intersection(fish_sf[fish_sf$year %in% years,], GBraster) %>%
#   dplyr::group_by(cell) %>%
#   dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
#                    hm = sum(hm, na.rm=T),
#                    nvessels = length(unique(vrnum)),
#                    ncompanies = length(unique(Company)))
# 
# st_geometry(fish_grid) <- NULL
# 
# fish_grid <- dplyr::left_join(GBraster, fish_grid)