# to import layers from github for adding to figures manually

github_spatial_import <- function(subfolder, zipname, direct_fns=direct_fns) {
  source(paste0(direct_fns, "/Maps/combine_shapefile_layers.R"))
  temp <- tempfile()
  download.file(paste0("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/", subfolder, "/", zipname), temp)
  # Download this to the temp directory you created above
  temp2 <- tempfile()
  # Unzip it
  unzip(zipfile=temp, exdir=temp2)
  imported_sf_obj <- all.layers(temp2,make.sf=T,make.polys=F)
  return(imported_sf_obj)
}

