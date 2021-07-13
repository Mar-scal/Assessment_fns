
# csv: csv file in PBSMapping format already projected in WGS84. 
# destination: complete filename and path to save the .txt file.

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
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} # end for(un in funs)
# This pulls in all the layers from the above location
offshore.strata <- combo.shp(temp2,make.sf=T,make.polys=F)

plot(offshore.strata)

require(RColorBrewer)
require(raster)
require(scales)
require(purrr)
require(stars)

for(i in unique(offshore.strata$label)) {
  
  test <- st_as_sf(offshore.strata[offshore.strata$label==i,])
  test$raster_id <- as.numeric(test$Strt_ID)*100
  test <- st_rasterize(test, st_as_stars(st_bbox(test), nx = 1000, ny = 1000, values = NA_real_))
  plot(test)
  
  rasterdf <- as.data.frame(test) %>%
    dplyr::select(y, x, raster_id) %>%
    filter(!is.na(raster_id))
  
  names(rasterdf) <- c("Y", "X", "ID")
  print(paste("printing", i))
  write.table(rasterdf, paste0(direct,"Data/Maps/approved/Survey/olex_strata_", i, ".txt"),
              append = FALSE, sep = '\t', dec = ".",
              row.names = F, col.names = TRUE)
}


olex_gba <- read.table(paste0(direct,"Data/Maps/approved/Survey/olex_strata_GBa.txt"), header=T)

olex_gba <- st_as_sf(olex_gba, coords=c("X", "Y"))
plot(olex_gba)

olex_gba %>%
st_crop(st_bbox(c(xmin=-66.95, xmax=-66.945, ymin=42.17, ymax=42.18)))

#compare to JS example
olex_29 <- read.table("C:/Users/keyserf/Downloads/SDM29ForOLEX.txt", header=T)

olex_29 <- st_as_sf(olex_29, coords=c("X", "Y"))
plot(olex_29)

olex_29 %>%
  st_crop(st_bbox(c(xmin=-66.34, xmax=-66.33, ymin=43.6, ymax=43.62))) %>%
  plot()