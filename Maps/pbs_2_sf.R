# simple PBS mapping to sf MULTIPOLYGON (e.g. for converting offshore strata)
pbs_2_sf <- function(pbs, lon, lat) {
  require(tidyverse) || stop("install tidyverse")
  require(sf) || stop("install sf package")
  #require(nngeo) || stop("install nngeo package")

  sf_use_s2(FALSE)
  pids <- NULL
  for(i in 1:length(unique(pbs$PID))){
    print(i)
    pid <- pbs %>%
      filter(PID == unique(pbs$PID)[i])
    sids <- NULL  
    for(j in 1:length(unique(pid$SID))){
      print(j)
      sid <- pid %>%
        filter(SID==unique(pid$SID)[j])
      
      if(sid$POS[1]<sid$POS[2]) sid$hole <- "N"
      if(sid$POS[1]>sid$POS[2]) sid$hole <- "Y"
      
      sid <- sid %>%
        st_as_sf(coords=c(lon, lat), crs=4326) %>%
        dplyr::group_by(PID, SID, hole) %>%
        dplyr::summarize(do_union=F) %>%
        st_cast("POLYGON")
      
      if(j==1) {sids <- rbind(sids, sid)}
      if(j>1) {
        if(unique(sid$hole)=="Y"){
          sids <- st_difference(sids, sid) %>%
            dplyr::select(PID, SID, hole)
        }
        if(unique(sid$hole)=="N"){
          sid <- dplyr::select(sid, PID, SID, hole)
          sids <- rbind(sids, sid)
        }
      }
    }
    pid <- st_combine(sids) %>% st_sf()
    pids <- rbind(pids, pid)
  }
  
  # out <- pbs %>% sf::st_as_sf(coords=c(x=lon, y=lat), crs=4326) %>%
  #   group_by(PID, SID) %>%
  #   summarize(do_union=F) %>% 
  #   st_cast("POLYGON") %>%
  #   ungroup() %>%
  #   group_by(PID) %>%
  #   summarize(do_union=F) %>%
  #   st_cast("MULTIPOLYGON") #%>%
 #   st_simplify()  # removes holes for GBb!
  
  # pbs <- pbs[, -which(names(pbs) %in% c(lon, lat))]
  # pbs <- dplyr::select(pbs, -SID)
  # if(any(names(pbs)=="POS")) pbs <- dplyr::select(pbs, -POS)
  # pbs <- unique(pbs)
  # out <- left_join(out, pbs)
  
  return(pids)
}

# gba <- pbs_2_sf(area[area$label=="GBa",], lon="X", lat="Y")
# gbb <- pbs_2_sf(area[area$label=="GBb",], lon="X", lat="Y")
# out <- pbs_2_sf(survey.detail.polys[survey.detail.polys$label=="GBb",], lon="X", lat="Y")
# out <- pbs_2_sf(survey.detail.polys[survey.detail.polys$label=="BBn",], lon="X", lat="Y")
# 
# 
# st_area(out)
# out$PID <- 1:nrow(out)
# ggplot() + geom_sf(data=out, aes(fill=PID)) + facet_wrap(~PID)
# 
# ggplot() + geom_sf(data=offshore[offshore$label=="GBa",], aes(fill=PID)) + facet_wrap(~PID)
# st_area(offshore[offshore$label=="BBn",])

