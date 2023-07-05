# converting offshore survey strata into Olex format

direct <- "Y:/Offshore/Assessment/"
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
require(stars) #new version of raster that uses sf


# boundary
boundary <- offshore.strata[offshore.strata$label=="BBn",]
boundary <- st_union(boundary) %>% st_sf()
boundary$raster_id <- 1
test <- boundary

for(i in unique(offshore.strata$label)) {
  
  # subsetting for just the bank in this round of the loop
  test <- st_as_sf(offshore.strata[offshore.strata$label==i,])
  
  # assigning a unique ID to each row
  test$raster_id <- 1:length(unique(test$Strt_ID))*100
  
  # take the bounding box of the strata (aka the survey domain) and convert it to a raster (grid) of 1000x1000 (adjustable).
  grid <- st_as_stars(st_bbox(test), nx = 1000, ny = 1000)#, values = NA_real_)
  
  # apply the raster grid to the original strata polygons
  test <- st_rasterize(sf=test, template = grid)
  
  plot(test)
  
  # turn the strata raster into a dataframe
  rasterdf <- as.data.frame(test) %>%
    # remove unnecessary columns
    dplyr::select(y, x, raster_id) %>%
    # remove NAs
    dplyr::filter(!is.na(raster_id))
  
  ggplot() + geom_raster(data=rasterdf, aes(x, y, fill=raster_id))
  
  # rename columns
  names(rasterdf) <- c("Y", "X", "ID")
  print(paste("printing", i))
  
  # round to ensure consistency
  rasterdf$Y <- round(rasterdf$Y, 5)
  rasterdf$X <- round(rasterdf$X, 5)
  
  # save the as a txt file
  write.table(rasterdf, paste0(direct,"Data/Maps/approved/Survey/olex_strata_", i, ".txt"),
              append = FALSE, sep = '\t', dec = ".",
              row.names = F, col.names = TRUE)
  
  write.table(rasterdf, paste0(direct,"Data/Maps/approved/Survey/olex_strata_bbn_Jun29_noRound.txt"),
              append = FALSE, sep = '\t', dec = ".",
              row.names = F, col.names = TRUE)
}


olex_gbb <- read.table(paste0(direct,"Data/Maps/approved/Survey/olex_strata_bbn_Jun28_rounded_keepBbox.txt"), header=T)

olex_gbb <- st_as_sf(olex_gbb, coords=c("X", "Y"))
plot(olex_gbb)

olex_gba <- read.table(paste0(direct,"Data/Maps/approved/Survey/olex_strata_GBa.txt"), header=T)
ggplot() + geom_raster(data=olex_gba, aes(X, Y, fill=ID))

olex_gba <- st_as_sf(olex_gba, coords=c("X", "Y"))
plot(olex_gba)

olex_bbn <- read.table(paste0(direct,"Data/Maps/approved/Survey/olex_strata_BBn.txt"), header=T)

olex_bbn <- st_as_sf(olex_bbn, coords=c("X", "Y"))
plot(olex_bbn)

olex_sab <- read.table(paste0(direct,"Data/Maps/approved/Survey/olex_strata_Sab.txt"), header=T)

olex_sab <- st_as_sf(olex_sab, coords=c("X", "Y"))
plot(olex_sab)

olex_gba %>%
st_crop(st_bbox(c(xmin=-66.95, xmax=-66.945, ymin=42.17, ymax=42.18)))

#compare to JS example
olex_29 <- read.table("C:/Users/keyserf/Downloads/SDM29ForOLEX.txt", header=T)

olex_29 <- st_as_sf(olex_29, coords=c("X", "Y"))
plot(olex_29)

olex_29 %>%
  st_crop(st_bbox(c(xmin=-66.34, xmax=-66.33, ymin=43.6, ymax=43.62))) %>%
  plot()





GBa_new <- read.table(paste0(direct,"Data/Maps/approved/Survey/olex_strata_GBa.txt"), header=T)
GBa_old <- read.table(paste0(direct,"Data/Maps/approved/Survey/olex_strata_oldGBa.txt"), header=T)

head(GBa_new)
head(GBa_old)

GBa_new$Yround <- round(GBa_new$Y, 5)
GBa_old$Yround <- round(GBa_old$Y, 5)


