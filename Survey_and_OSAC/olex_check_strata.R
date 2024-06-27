# For comparing Olex tows to planned tow coordinates and strata, with interactive plotly option
# FK 2022

# example
# output <- olex_check_strata(towplan = "C:/Users/keyserf/Desktop/sab_plan.csv",
#                             towfile="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/MidSabLE15tracks.txt",
#                             bank="Sab", interactive=T)
# if interactive=T, HTML widget "olex_check_strata.html" is saved in your working directory (getwd()) 


olex_check_strata <- function(towplan, towfile, bank, interactive=F, UTM=NULL, earliest=NULL, latest=NULL){
  require(sf)
  message("This doesn't quite work! Use the tow_file_strata.csv and compare manually.")
  sf_use_s2(FALSE)
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
            "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/pectinid_projector_sf.R",
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
  track <- olex_import(towfile, type="sf", UTM=UTM, earliest = earliest, latest = latest) %>%
    st_transform(4326)
  
  sf_use_s2(FALSE)
  
  inter <- st_intersection(track, dplyr::select(offshore.strata[offshore.strata$label==bank,], Strt_ID, col, PName))
  
  
  #### get the station list (tow plan)
  planned <- read.csv(towplan)
  planned <- planned %>%
    st_as_sf(coords=c("Long.dd", "Lat.dd"), crs=4326)
  names(planned)[1] <- "plan_ID"
  
  planned_buff <- st_transform(planned, UTM) %>%
    st_buffer(800) %>%
    st_transform(4326)
  
  plancheck <- st_intersection(inter, dplyr::select(planned_buff, plan_ID, Strata_ID, STRATA))
  
  if(any(nchar(plancheck$Strt_ID) == 3) & any(nchar(plancheck$Strata_ID)<3)) plancheck$Strt_ID <- str_sub(plancheck$Strt_ID, start=3, end=3)
  browser()
  if(any(!plancheck$Strt_ID == plancheck$Strata_ID)) {
    message("The following planned tows ended up in a different strata:")
    print(plancheck[which(!plancheck$Strt_ID == plancheck$Strata_ID),])
  }
  if(any(!inter$ID %in% plancheck$ID)) {
    message("The following completed tows did not intersect with a planned tow (with 800m buffer):")
    print(inter[which(!inter$ID %in% plancheck$ID),])
  }
  
  #baseplot <- pecjector(area=bank, add_layer = list(survey=c("offshore", "detailed")))
  ggplot() + 
    geom_sf(data=offshore.strata[offshore.strata$label==bank,], aes(fill=Strt_ID), alpha=0.5)+
    scale_fill_viridis_d()+
    geom_sf(data=inter, colour="blue") +
    geom_sf(data=planned, colour="red")+
    theme_bw()
  
  sf_use_s2(FALSE)
  
  if(interactive==T) {
    require(plotly)
    htmlwidgets::saveWidget(
      ggplotly(ggplot() + 
                 geom_sf(data=offshore.strata[offshore.strata$label==bank,], aes(fill=Strt_ID), alpha=0.5)+
                 scale_fill_viridis_d()+
                 geom_sf(data=inter, colour="blue")+
                 geom_sf(data=planned, colour="red")+
                 theme_bw() +
                 coord_sf(expand=F)),
      paste0(getwd(), "/olex_check_strata.html"))
  }
  
  return(plancheck)
}


