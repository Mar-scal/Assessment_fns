# Offshore Scallop Survey Data - converting Olex to .csv and useable coordinates
# Code by TPD April 2022, inspired by https://github.com/Mar-scal/Inshore/blob/main/Survey/OLEX-latlong_conversion.R

#example:
# olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/MidSabLE15tracks.txt")


olex_import <- function(filename){
  #Import olex data:
  library(data.table)
  library(splitstackshape)
  library(tidyverse)
  library(sf)
  library(rmapshaper)
  library(mapview)
  library(ggplot2)
  library(dplyr)
  
  #Norwegian translation according to Google:
  #Grønnramme - basic framework
  #Navn - name
  #Rute uten navn- Route without name
  #Garnstart - start
  #Garnstopp - stop
  #Brunsirkel - brown circle (points along trackline?)
  funcs <- "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r"
  dir <- getwd()
  for(fun in funcs) 
  {
    temp <- dir
    download.file(fun,destfile = basename(fun))
    source(paste0(dir,"/",basename(fun)))
    file.remove(paste0(dir,"/",basename(fun)))
  }
  
  zz <- read.csv(filename)
  
  str(zz)
  zz$Ferdig.forenklet <- as.character(zz$Ferdig.forenklet)
  
  #Split characters separated by spaces into columns.
  zz <- cSplit(zz, "Ferdig.forenklet", sep = " ", type.convert = FALSE) 
  
  zz <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstart", "Garnstopp")) %>% 
    select(Ferdig.forenklet_1, Ferdig.forenklet_2, Ferdig.forenklet_4) %>% 
    mutate(Latitude = as.numeric(Ferdig.forenklet_1)/60) %>% 
    mutate(Longitude = as.numeric(Ferdig.forenklet_2)/60)
  
  #Occasionally a "Grønnramme" occurs where a "Garnstopp" should be or between tow tracks. 
  #RUN but Check if number of "Garnstart" == "Garnstopp"!! 
  zz <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstart", "Garnstopp", "Grønnramme")) %>% 
    dplyr::select(Ferdig.forenklet_1, Ferdig.forenklet_2, Ferdig.forenklet_4) %>% 
    mutate(Latitude = as.numeric(Ferdig.forenklet_1)/60) %>% 
    mutate(Longitude = as.numeric(Ferdig.forenklet_2)/60)
  
  # this must return TRUE!
  length(zz[zz$Ferdig.forenklet_4 == "Garnstart",]$Latitude) == length(zz[zz$Ferdig.forenklet_4 == "Garnstopp",]$Latitude)
  
  View(zz)
  
  #Select the row where the track data starts (i.e. the first "Garnstart"). Check for "Grønnramme".
  zz <- zz[min(which(zz$Ferdig.forenklet_4 == "Garnstart")):nrow(zz),] #[Row# where "Garnstart" first occurs: to end of data]  #Most likely its however many stations there are, but could be more if observations were added.
  
  #Convert decimal degrees to decimal minutes seconds.
  zz$Latitude.deg <- convert.dd.dddd(zz$Latitude, format = 'deg.min')
  zz$Longitude.deg <- convert.dd.dddd(zz$Longitude, format = 'deg.min')*-1
  
  zz.start <- zz %>% filter(Ferdig.forenklet_4 == "Garnstart") %>% 
    dplyr::rename(Start_lat = Latitude.deg) %>% 
    dplyr::rename(Start_long = Longitude.deg) %>% 
    dplyr::select(Start_lat, Start_long, Start_lat_dec = Latitude, Start_long_dec = Longitude)
  
  #If required to have 3 decimal places - values copied down in fieldbook are not rounded so run to get 3 decimal places not rounded:
  zz.start$Start_lat <- trunc(zz.start$Start_lat*10^3)/10^3
  zz.start$Start_long <- trunc(zz.start$Start_long*10^3)/10^3
  
  zz.end <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstopp")) %>% #"Grønnramme"
    dplyr::rename(End_lat = Latitude.deg) %>% 
    dplyr::rename(End_long = Longitude.deg) %>% 
    dplyr::select(End_lat, End_long, End_lat_dec = Latitude, End_long_dec = Longitude)
  
  zz.end$End_lat <- trunc(zz.end$End_lat*10^3)/10^3
  zz.end$End_long <- trunc(zz.end$End_long*10^3)/10^3
  
  coords <- cbind(zz.start, zz.end) %>% 
    mutate(ID = seq(1,nrow(zz.start),1))  #NOTE - ID IS NOT TOW NUMBER (although it could lineup). it is only used to compare records when matching strata #s and SPAs.
  
  # Match Strata ID and SPA # to lat and long data (use start lat long)
  coords.sf <- st_as_sf(coords, coords = c("Start_long_dec", "Start_lat_dec"), crs = 4326, remove=F) %>%
    mutate(X=Start_long_dec, Y=Start_lat_dec)
  ggplot() + geom_sf(data=coords.sf) 
  
  coords.sf.end <- st_as_sf(coords, coords = c("End_long_dec", "End_lat_dec"), crs = 4326, remove=F) %>%
    mutate(X=End_long_dec, Y=End_lat_dec)
  ggplot() + geom_sf(data=coords.sf.end) 
  
  coords.track <- rbind(coords.sf, coords.sf.end) %>%
    group_by(ID) %>%
    summarize() %>%
    st_cast("LINESTRING")
  
  print(ggplot() + geom_sf(data=coords.track, lwd=1) + coord_sf() + theme_bw())

  return(coords.track)  
  
}