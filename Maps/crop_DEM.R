# Extracting area from Scotian Shelf DEM

require(sf)
require(raster)
require(stars)
require(tidyverse)
require(ggplot2)
require(mapview)

# read in DEM
bathy <- raster("Z:/Inshore/StandardDepth/ScotianShelfDEM_Olex/mdem_olex/w001001.adf")
# do not try to do much with this, because it is SUCH a large file. Memory issues and time-wasting will result

# read in a file that will help you define the extent that you want to crop the DEM to
stns <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2023/Spring/Ger/54880/Preliminary_Survey_design_Tow_locations_Ger.csv")

# convert your reference object to the CRS of the DEM
stns <- st_as_sf(stns, coords=c("X", "Y"), crs=4326) %>%
  st_transform(crs(bathy))

# create an extent object
e <- extent(stns)*1.2 # multiply by some 1.X to make the extent larger than the extent of your reference object
e@xmax <- e@xmax + 10000 # For some areas, you might like to expand further in one particular direction (e.g. further east for German)

# crop the DEM to the extent object
bathyc <- crop(bathy, e) 

# convert cropped DEM to stars object for plotting
bathystars <- st_as_stars(bathyc)

# plot it up! Ooooh, ahhhhh! Very nice. 
ggplot() + geom_stars(data=bathystars) +
  geom_sf(data=stns)

# Write the cropped DEM to a tif file for future use in R or ArcPro
write_stars(obj = bathystars, "C:/Users/keyserf/Documents/mdem_olex_german.tif")

# Read the cropped DEM in alter
DEM <- raster("C:/Users/keyserf/Documents/mdem_olex_german.tif")
plot(DEM)


# interactive plot!
DEM <- st_as_stars(DEM)
mapview(list(DEM, stns))

fish <- read.csv("Y:/Offshore/Assessment/Data/Fishery_data/Logs/Preliminary/2022log.csv")
fish <- fish[fish$FISHING_AREA=="26C",]
fish <- st_as_sf(fish, coords=c(X="LONGITUDE_DEG", Y="LATITUDE_DEG"), crs=4326) %>%
  st_transform(crs(DEM))

mapview(DEM) +
  mapview(fish, col.regions="red") +
  mapview(stns, col.regions="blue")

