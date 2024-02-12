
# for reviewing olex track numbering
# change the file name as needed
track <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2023/Industry Reports/BBn_olex_tracks_2023_tpd.csv")

require(sf)
require(ggplot2)
require(dplyr)

head(track)

table(track$Tow, track$Bank)

track <- st_as_sf(track, coords=c("Longitude", "Latitude"), crs=4326)
track <- track %>%
  group_by(Tow, Bank) %>%
  summarise(do_union=F) %>%
  st_cast("LINESTRING")

# change the plot title as needed
ggplot() + geom_sf(data=track) + 
  geom_sf_text(data=track, aes(label=Tow)) +
  ggtitle("BBn")


# for GBMon, do it for the two separate GB files
track <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2023/Industry Reports/GBa_olex_tracks_2023_tpd.csv")
track2 <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2023/Industry Reports/GBb_olex_tracks_2023_tpd.csv")

head(track)

track <- st_as_sf(track, coords=c("Longitude", "Latitude"), crs=4326)
track <- track %>%
  group_by(Tow) %>%
  summarise(do_union=F) %>%
  st_cast("LINESTRING")

head(track)

track2 <- st_as_sf(track2, coords=c("Longitude", "Latitude"), crs=4326)
track2 <- track2 %>%
  group_by(Tow) %>%
  summarise(do_union=F) %>%
  st_cast("LINESTRING")


ggplot() + geom_sf(data=track) + 
  geom_sf_text(data=track, aes(label=Tow)) +
  geom_sf(data=track2) + 
  geom_sf_text(data=track2, aes(label=Tow)) +
  ggtitle("GBMon")
