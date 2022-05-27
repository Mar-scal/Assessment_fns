# Offshore Scallop Survey Data - converting Olex to .csv and useable coordinates
# Code by TPD April 2022, inspired by https://github.com/Mar-scal/Inshore/blob/main/Survey/OLEX-latlong_conversion.R

###################
### ARGUMENTS
# filename: gz or txt file with olex data
# ntows: the number of tows you are expecting to extract from file (just as a check). Can be left NULL
# type: "load" gives you the information needed to load into SCALOFF, track" gives the full tow track in sf format, "startend" gives only the start and end points in sf format 

#example:
# olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/MidSabLE15tracks.txt", ntows=115)
# will also work for .gz files!



olex_import <- function(filename, ntows=NULL, type){
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
  
  #str(zz)
  zz$Ferdig.forenklet <- as.character(zz$Ferdig.forenklet)
  
  #Split characters separated by spaces into columns.
  zz <- cSplit(zz, "Ferdig.forenklet", sep = " ", type.convert = FALSE) 
  
  
  # startend <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstart", "Garnstopp")) %>% 
  #   dplyr::select(Ferdig.forenklet_1, Ferdig.forenklet_2, Ferdig.forenklet_4) %>% 
  #   mutate(Latitude = as.numeric(Ferdig.forenklet_1)/60) %>% 
  #   mutate(Longitude = as.numeric(Ferdig.forenklet_2)/60)
  # 
  #Occasionally a "Grønnramme" occurs where a "Garnstopp" should be or between tow tracks. 
  #RUN but Check if number of "Garnstart" == "Garnstopp"!! 
  startend <- zz %>% filter(Ferdig.forenklet_4 %in% c("Garnstart", "Garnstopp", "Grønnramme")) %>% 
    dplyr::select(Ferdig.forenklet_1, Ferdig.forenklet_2, Ferdig.forenklet_4) %>% 
    mutate(Latitude = as.numeric(Ferdig.forenklet_1)/60) %>% 
    mutate(Longitude = as.numeric(Ferdig.forenklet_2)/60)
  
  # this must return TRUE!
  if(!length(startend[startend$Ferdig.forenklet_4 == "Garnstart",]$Latitude) == length(startend[startend$Ferdig.forenklet_4 == "Garnstopp",]$Latitude)) stop("Error in olex file")
  #View(zz)
  if(any(startend$Ferdig.forenklet_4=="Grønnramme")) stop(paste0("There is a Grønnramme in your tow file. It needs to be manually removed."))
  
  #Select the row where the track data starts (i.e. the first "Garnstart"). Check for "Grønnramme".
  startend <- startend[min(which(startend$Ferdig.forenklet_4 == "Garnstart")):nrow(startend),] #[Row# where "Garnstart" first occurs: to end of data]  #Most likely its however many stations there are, but could be more if observations were added.
  
  #Convert decimal degrees to decimal minutes seconds.
  startend$Latitude.deg <- convert.dd.dddd(startend$Latitude, format = 'deg.min')
  startend$Longitude.deg <- convert.dd.dddd(startend$Longitude, format = 'deg.min')*-1
  
  start <- startend %>% filter(Ferdig.forenklet_4 == "Garnstart") %>% 
    dplyr::rename(Start_lat = Latitude.deg) %>% 
    dplyr::rename(Start_long = Longitude.deg) %>% 
    dplyr::select(Start_lat, Start_long, Start_lat_dec = Latitude, Start_long_dec = Longitude)
  
  #If required to have 3 decimal places - values copied down in fieldbook are not rounded so run to get 3 decimal places not rounded:
  start$Start_lat <- trunc(start$Start_lat*10^3)/10^3
  start$Start_long <- trunc(start$Start_long*10^3)/10^3
  
  end <- startend %>% filter(Ferdig.forenklet_4 %in% c("Garnstopp")) %>% #"Grønnramme"
    dplyr::rename(End_lat = Latitude.deg) %>% 
    dplyr::rename(End_long = Longitude.deg) %>% 
    dplyr::select(End_lat, End_long, End_lat_dec = Latitude, End_long_dec = Longitude)
  
  end$End_lat <- trunc(end$End_lat*10^3)/10^3
  end$End_long <- trunc(end$End_long*10^3)/10^3
  
  if(!is.null(ntows)){
    if(!length(start$Start_lat)==ntows) message(paste0("Number of tows tracked (", length(start$Start_lat), ") does not equal expected number of tows, beware!"))
  }
  
  coords <- cbind(start, end) %>% 
    mutate(ID = seq(1,nrow(start),1))  #NOTE - ID IS NOT TOW NUMBER (although it could lineup). it is only used to compare records when matching strata #s and SPAs.
  
  # Match Strata ID and SPA # to lat and long data (use start lat long)
  coords.sf <- st_as_sf(coords, coords = c("Start_long_dec", "Start_lat_dec"), crs = 4326, remove=F) %>%
    mutate(X=Start_long_dec, Y=Start_lat_dec)
  
  coords.sf.end <- st_as_sf(coords, coords = c("End_long_dec", "End_lat_dec"), crs = 4326, remove=F) %>%
    mutate(X=End_long_dec, Y=End_lat_dec)
  
  coords.track <- rbind(coords.sf, coords.sf.end) %>%
    st_transform(32620) %>%
    group_by(ID) %>%
    summarize() %>%
    st_cast("LINESTRING") %>%
    st_transform(4326)
  
  if(type=="startend") {
    print(ggplot() + geom_sf(data=coords.track, lwd=1) + coord_sf() + theme_bw())
    return(coords.track)
  }
  
  
  
  track <- data.frame(start=which(zz$Ferdig.forenklet_4=="Garnstart"), end=which(zz$Ferdig.forenklet_4=="Garnstopp"))
  if(any(!track$start<track$end)) stop("check tow file, seems like there is a Garnstopp before a Garnstart")
  track$tow <- 1:nrow(track)
  
  trackpts <- NULL
  for(i in 1:length(track$tow)){
    trackpts1 <- zz[track$start[i]:track$end[i],] %>%
      as.data.frame() %>%
      dplyr::select(Ferdig.forenklet_1, Ferdig.forenklet_2, Ferdig.forenklet_4) %>% 
      mutate(Latitude = as.numeric(Ferdig.forenklet_1)/60) %>% 
      mutate(Longitude = as.numeric(Ferdig.forenklet_2)/60) %>%
      mutate(tow=i)
    trackpts <- rbind(trackpts, trackpts1)
  }
  
  trackpts <- trackpts %>%
    st_as_sf(coords=c("Longitude", "Latitude"), crs=4326) %>%
    st_transform(32620) %>%
    group_by(tow) %>%
    summarize() %>%
    st_cast("LINESTRING") %>%
    st_transform(4326)
  
  if(!is.null(ntows)){
    if(!length(trackpts$tow)==ntows) message(paste0("Number of tows tracked (", length(zz.start$Start_lat), ") does not equal expected number of tows, beware!"))
  }
  
  print(ggplot() + geom_sf(data=trackpts, lwd=1) + coord_sf() + theme_bw())
  
  trackpts$length <- trackpts %>% 
    st_transform(32620) %>%
    group_by(tow) %>% st_length()
  
  trackpts$dis_coef <- 800/trackpts$length
  
  if(type=="track"){
    return(trackpts)  
  }
  
  if(type=="load"){
    trackpts$bearing <- NA
    trackpts$start_lon <- NA
    trackpts$start_lat <- NA
    trackpts$end_lon <- NA
    trackpts$end_lat <- NA
    for(i in 1:nrow(trackpts)){
      trackpts$bearing[i] <- geosphere::bearing(p1 = c(coords$Start_long_dec[i], coords$Start_lat_dec[i]), 
                                                p2 = c(coords$End_long_dec[i], coords$End_lat_dec[i]))
      trackpts$start_lon[i] <- -coords$Start_long[i]
      trackpts$start_lat[i] <- coords$Start_lat[i]
      trackpts$end_lon[i] <- -coords$End_long[i]
      trackpts$end_lat[i] <- coords$End_lat[i]
    }
    st_geometry(trackpts) <- NULL
    trackpts <- dplyr::select(trackpts, tow, start_lat, start_lon, end_lat, end_lon, dis_coef, bearing)
    trackpts$bearing <- ifelse(trackpts$bearing < 0, trackpts$bearing+360, trackpts$bearing)
    
    return(trackpts)
  }
  
}







