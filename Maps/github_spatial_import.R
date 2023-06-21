# to import layers from github for adding to figures manually

github_spatial_import <- function(subfolder, zipname, specific_shp=NULL, direct_fns, quiet=F) {
  
  # Load our file
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/combo_shp.R")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
    for(fun in funs) 
    {
      download.file(fun,destfile = basename(fun))
      source(paste0(getwd(),"/",basename(fun)))
      file.remove(paste0(getwd(),"/",basename(fun)))
    }
  } else {source(paste0(direct_fns, "/Maps/combo_shp.R"))}
  
  
  temp <- tempfile()
  download.file(paste0("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/", subfolder, "/", zipname), temp)
  # Download this to the temp directory you created above
  temp2 <- tempfile()
  # Unzip it
  unzip(zipfile=temp, exdir=temp2)
  if(is.null(specific_shp)){
    imported_sf_obj <- combo.shp(temp2,make.sf=T,make.polys=F, quiet=quiet)
    return(imported_sf_obj)
  }
  
  if(!is.null(specific_shp)){
    return(st_read(paste0(temp2, "\\", specific_shp)))
  }
}

