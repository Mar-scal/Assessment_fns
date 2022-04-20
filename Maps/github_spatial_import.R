# to import layers from github for adding to figures manually

github_spatial_import <- function(subfolder, zipname, direct_fns, quiet=F) {
  
  # Load our file
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/convert_inla_mesh_to_sf.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/centre_of_gravity.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/combo_shp.R")
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
  imported_sf_obj <- combo.shp(temp2,make.sf=T,make.polys=F, quiet=quiet)
  return(imported_sf_obj)
}

