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
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/github_spatial_import.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/combo_shp.R")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
} # end for(un in funs)
# This pulls in all the layers from the above location
# offshore.strata <- combo.shp(temp2,make.sf=T,make.polys=F)
# 
# plot(offshore.strata)

require(RColorBrewer)
require(raster)
require(scales)
require(purrr)
require(stars) #new version of raster that uses sf
require(fasterize)
require(ggplot2)
require(sf)

for(i in c("BBn", "BBs", "GBa", "GBb", "Sab")) {
  
  if(i == "GBa") sf_use_s2(TRUE)
  if(!i == "GBa") sf_use_s2(FALSE)
  
  test <- github_spatial_import("offshore_survey_strata", "offshore_survey_strata.zip", specific_shp=paste0(i, ".shp"), quiet = T)
  # test <- combo.shp(test,make.sf=T,make.polys=F)
  
  # subsetting for just the bank in this round of the loop
  # test <- st_as_sf(offshore.strata[offshore.strata$label==i,])
  
  # assigning a unique ID to each row
  test$raster_id <- 1:length(unique(test$Strt_ID))*100
  
  if(i %in% c("BBn", "GBa", "GBb")) UTM <- 32619 
  if(i %in% c("BBs", "Sab")) UTM <- 32620
  
  test <- st_transform(test, UTM)
  
  # width <- (st_bbox(test)$xmax[[1]] - st_bbox(test)$xmin[[1]])/100
  # height <- (st_bbox(test)$ymax[[1]] - st_bbox(test)$ymin[[1]])/100

  # raster package. Make a grid, 20x20m
  r <- raster(test, res=20)
  
  if(i == "GBa") sf_use_s2(FALSE)
  # make the boundary buffer
  outer <- st_boundary(st_union(test))
  
  outer <- st_sf(raster_id = (length(unique(test$Strt_ID))+1)*100, 
                 st_sfc(outer))
  
  if(i=="GBa") {
    remove <- st_crop(outer, xmin=-66.4, xmax=-66.2, ymin=41.8, ymax=41.9)
    outer <- st_difference(outer, remove)
  }
  
  # apply the raster grid to the original strata polygons
  #test <- st_rasterize(sf=test, template = grid)
  test <- fasterize(test, r, field="raster_id", fun="min")
  
  #outer <- st_rasterize(sf=outer, template=grid)
  # have to use raster package for this (instead of fasterize) since it's a line and not a polygon. 
  outer <- rasterize(outer, r, field="raster_id", fun="min")
  
  # convert to XYZ df
  test_df <- as.data.frame(test, xy=T)
  test_df <- test_df[!is.na(test_df$layer),]
  outer_df <- as.data.frame(outer, xy=T)
  outer_df <- outer_df[!is.na(outer_df$layer),]
  
  rasterdf <- rbind(test_df, outer_df)
  #rasterdf <- test_df
  
  # convert it back to WGS84
  rasterdf <- st_as_sf(rasterdf, coords=c("x", "y"), crs=UTM)
  # this step is slow:
  rasterdf <- st_transform(rasterdf, 4326)
  
  # then back to a dataframe
  coords <- st_coordinates(rasterdf)
  st_geometry(rasterdf) <- NULL
  rasterdf <- cbind(coords, rasterdf)
  
  # rename columns
  names(rasterdf) <- c("X", "Y", "ID")
  rasterdf <- dplyr::select(rasterdf, Y, X, ID)
  print(paste("printing", i))
  
  # round to ensure consistency
  rasterdf$Y <- round(rasterdf$Y, 7)
  rasterdf$X <- round(rasterdf$X, 7)
  
  # save the as a txt file
  write.table(rasterdf, paste0("C:/Users/keyserf/Documents/GitHub/GIS_layers/offshore_survey_strata/olex/olex_strata_", i, "_2023_w_border.txt"),
              append = FALSE, sep = '\t', dec = ".",
              row.names = F, col.names = TRUE)
}


require(ggplot2)
ggplot() + geom_point(data=rasterdf[rasterdf$Y<42.59,], aes(X,Y,colour=ID)) + coord_map() 

tows <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2023/Database loading/LE17/GBMon/OS.scallop.tow.GBMonLE17.csv")
tows$START_LAT <- convert.dd.dddd(tows$START_LAT)
tows$START_LON <- convert.dd.dddd(tows$START_LON)
tows$END_LAT <- convert.dd.dddd(tows$END_LAT)
tows$END_LON <- convert.dd.dddd(tows$END_LON)
starts <- st_as_sf(tows, coords=c(X="START_LON", Y="START_LAT"), crs=4326)
starts$type <- "start"
starts <- dplyr::select(starts, TOW_NO, type, geometry)
ends <- st_as_sf(tows, coords=c(X="END_LON", Y="END_LAT"), crs=4326)
ends$type <- "end"
ends <- dplyr::select(ends, TOW_NO, type, geometry)
tows <- rbind(starts, ends) %>%
  dplyr::group_by(TOW_NO) %>%
  dplyr::summarize(do_union=F) %>%
  st_cast("LINESTRING")

ggplot() + geom_point(data=rasterdf, aes(X,Y,colour=ID)) + 
  geom_sf(data=tows, colour="red") + 
  xlim(-66.2, -66) +
  ylim(42.55, 42.65)

sfa29w <- read.table("Y:/Offshore/Survey/Olex/SDM29ForOLEX.txt", header=T)
head(sfa29w)

ggplot() + geom_point(data=sfa29w, aes(X,Y,colour=ID), size=0.001) + coord_map() 

olex_gbb <- read.table("C:/Users/keyserf/Documents/GitHub/GIS_layers/offshore_survey_strata/olex/olex_strata_GBb_2023.txt", header=T)
olex_gbb <- st_as_sf(olex_gbb, coords=c("X", "Y"))
plot(olex_gbb)

olex_gba <- read.table("C:/Users/keyserf/Documents/GitHub/GIS_layers/offshore_survey_strata/olex/olex_strata_GBa_2023.txt", header=T)
olex_gba <- st_as_sf(olex_gba, coords=c("X", "Y"))
plot(olex_gba)

olex_bbn <- read.table("C:/Users/keyserf/Documents/GitHub/GIS_layers/offshore_survey_strata/olex/olex_strata_BBn_2023.txt", header=T)
olex_bbn <- st_as_sf(olex_bbn, coords=c("X", "Y"))
plot(olex_bbn)

olex_bbs <- read.table("C:/Users/keyserf/Documents/GitHub/GIS_layers/offshore_survey_strata/olex/olex_strata_BBs_2023.txt", header=T)
olex_bbs <- st_as_sf(olex_bbs, coords=c("X", "Y"))
plot(olex_bbn)

olex_sab <- read.table("C:/Users/keyserf/Documents/GitHub/GIS_layers/offshore_survey_strata/olex/olex_strata_Sab_2023.txt", header=T)
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