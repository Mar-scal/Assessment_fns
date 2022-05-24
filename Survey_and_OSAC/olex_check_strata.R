

olex_check_strata <- function(filename, bank){
  #Import olex data:
  # converting offshore survey strata into Olex format
  # Figure out where your tempfiles are stored
  temp <- tempfile()
  
  # Download this to the temp directory you created above
  download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore_survey_strata/offshore_survey_strata.zip", temp)
  # Figure out what this file was saved as
  temp2 <- tempfile()
  # Unzip it
  unzip(zipfile=temp, exdir=temp2)
  funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/convert_coords.R",
            "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/combo_shp.R",
            "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",
            "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
            "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Survey_and_OSAC/olex_import.R")
  # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
  for(fun in funs) {
    download.file(fun,destfile = basename(fun))
    source(paste0(getwd(),"/",basename(fun)))
    file.remove(paste0(getwd(),"/",basename(fun)))
  } # end for(un in funs)
  
  # This pulls in all the layers from the above location
  offshore.strata <- combo.shp(temp2,make.sf=T,make.polys=F) %>% 
    st_transform(4326)
  
  ##### Now get the tow tracks 
  track <- olex_import(filename) %>%
    st_transform(4326)
  
  inter <- st_intersection(track, dplyr::select(offshore.strata[offshore.strata$label==bank,], Strt_ID, col, PName))
  
  
  baseplot <- pecjector(area=bank, add_layer = list(survey=c("offshore", "detailed")))
  
  print(baseplot + 
          geom_sf(data=inter) +
          theme_bw() +
          coord_sf(expand=F))
  
  return(inter)
}


output <- olex_check_strata(filename="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/MidSabLE15tracks.txt", 
                  bank="Sab")
output[output$ID==38,]


