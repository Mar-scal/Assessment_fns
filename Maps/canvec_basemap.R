install.packages("rcanvec")

require(rcanvec)
require(prettymapr)

maritimes <- makebbox(46, -64, 44.5, -66)

canvec.download(nts(bbox=maritimes))
canvec.export(nts(bbox=maritimes),layers = "waterbody", tofolder=getwd())
canvec.qplot(bbox=maritimes, layers=c("waterbody"), stoponlargerequest = FALSE)

getwd()

require(sf)
dat <- read_sf(paste0(getwd(), "/waterbody.shp"))

ggplot() + geom_sf(data=dat, colour=NA, fill="grey") + coord_sf() + theme_bw()
