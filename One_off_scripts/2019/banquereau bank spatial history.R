BanIceSurvey2012
# borrow df's from survey summary data...

BanIceSurvey2012_melt <- melt(BanIceSurvey2012, id.vars=c("year", "lon", "lat", "tow", "bank", "state"), measure.vars = c("pre","rec", "com", "tot","pre.bm", "rec.bm","com.bm", "tot.bm"))

png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ban/IceBM.png"), res=250, unit="in", width=9, height=7)
ggplot() + geom_point(data=BanIceSurvey2012_melt[BanIceSurvey2012_melt$state=="live" & 
                                                   BanIceSurvey2012_melt$value > 0 &
                                                   grepl(x=BanIceSurvey2012_melt$variable, pattern=".bm"),], 
                      aes(lon, lat, colour=value)) +
  geom_polygon(data=survey.bound.polys[survey.bound.polys$label=="Ban",],
               aes(X,Y), fill=NA, colour="black") +
  facet_grid(year~variable) +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  coord_map() 
dev.off()

png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ban/IceN.png"), res=250, unit="in", width=9, height=7)
ggplot() + geom_point(data=BanIceSurvey2012_melt[BanIceSurvey2012_melt$state=="live" & 
                                                   BanIceSurvey2012_melt$value > 0 &
                                                   !grepl(x=BanIceSurvey2012_melt$variable, pattern=".bm"),], 
                      aes(lon, lat, colour=value)) +
  geom_polygon(data=survey.bound.polys[survey.bound.polys$label=="Ban",],
               aes(X,Y), fill=NA, colour="black") +
  facet_grid(year~variable) +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  coord_map() 
dev.off()

##### fishery data

source(paste0(direct, "Assessment_fns/Fishery/logs_and_fishery_data.r"))

require(ggplot2)
require(lubridate)
require(plyr)
logs_and_fish(loc="offshore",year = 1981:2018,un=un.ID,pw=pwd.ID,db.con=db.con, get.marfis = T)
ban.fish.dat.new <- new.log.dat[new.log.dat$bank=="Ban" & !is.na(new.log.dat$bank=="Ban"),]
ban.fish.dat.old <- old.log.dat[old.log.dat$bank=="Ban" & !is.na(old.log.dat$bank=="Ban"),]
ban.fish.dat <- plyr::join(ban.fish.dat.new, ban.fish.dat.old, type="full")
# require(plyr)
# test<- ddply(.data=ban.fish.dat.new, .(year),
#       summarize,
#       max_x = max(lon),
#       min_x = min(lon),
#       max_y = max(lat),
#       min_y = min(lat))
ban.fish.dat$ID<-1:nrow(ban.fish.dat)

ban.fish.dat$date <- ymd(ban.fish.dat$date)

# ban.fish.dat <- ban.fish.dat[!is.na(ban.fish.dat$year),]

# Get the survey boundary polygon for the bank 
bnk.survey.bound.poly <- subset(survey.bound.polys,label=="Ban")

# Set the levels, might need to think a bit about these!
lvls= c(10,50,100,500,1000,5000,10000,50000)

#format fishery data
ban.fish.dat.plot <- subset(ban.fish.dat, select = c('ID','lon','lat','pro.repwt', 'year'))
names(ban.fish.dat.plot)[1:5]<-c("EID","X","Y","Z", "year")

ban.fish.dat.plot <- ban.fish.dat.plot[!is.na(ban.fish.dat.plot$Z),]
ban.fish.dat.plot <- ban.fish.dat.plot[!ban.fish.dat.plot$X==0 & !ban.fish.dat.plot$Y==0,]
ban.fish.dat.plot <- subset(ban.fish.dat.plot, EID %in% findPolys(ban.fish.dat.plot,bnk.survey.bound.poly)$EID)

years <- sort(unique(ban.fish.dat.plot$year))

#Get the total removals from each 1 minute cell within the bank for the levels (10 kg to 50 tonnes!)
source(paste0(direct, "Assessment_fns/Survey_and_OSAC/gridPlot.r"))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) #Source3 
pdf(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ban/Spatial_history.pdf"), onefile=T)
for (i in 1:length(years)) {
  
  titl <- paste("Sea Scallop Catch (","Ban","-",years[i],")",sep="")
  
  bnk.polys <- gridPlot(ban.fish.dat.plot[ban.fish.dat.plot$year==years[i], c("EID", "X", "Y", "Z")], bnk.survey.bound.poly, lvls, FUN=sum, grid.size=1/60)  
  
  ScallopMap("Ban",poly.lst=bnk.polys[1:2],poly.border=bnk.polys[[2]]$border,xlab="",ylab="",
             title=titl, bathy.source="quick",
             plot.bathy = T,plot.boundries = T,boundries="offshore",cex.mn=2,dec.deg = F)

  
  tlvls<-lvls/1000
  legend("bottomleft",c(paste(tlvls[-length(tlvls)],'-',tlvls[-1],sep=''),paste(tlvls[length(tlvls)],'+',sep='')),
         fill=bnk.polys[[3]],title='Catch (t)',inset=0.02,bg='white',box.col='white')
}
dev.off()


## make a few ppt slides for recent years:
#Get the total removals from each 1 minute cell within the bank for the levels (10 kg to 50 tonnes!)
source(paste0(direct, "Assessment_fns/Survey_and_OSAC/gridPlot.r"))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) #Source3 
rec.years <- 2012:2018
#plot the proposed survey stations too
banstations <- read.csv(paste0(direct, "/Data/Survey_data/fixed_station_banks_towlst.csv"))
banstations2 <- read.csv(paste0(direct, "/Data/Survey_data/extra_stations.csv"))
banstations <- banstations[banstations$Bank=="Ban",]
banstations2 <- banstations2[banstations2$bank=="Ban",]

rec.years<-unique(ban.fish.dat.plot[ban.fish.dat.plot$year %in% rec.years,]$year)

for (i in 1:length(rec.years)) {
  png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ban/Spatial_history_", rec.years[i], ".png"), units = "in", width=11, height=8.5, res=920)
  
  titl <- paste("Sea Scallop Catch (","Ban","-",rec.years[i],")",sep="")
  
  bnk.polys <- gridPlot(ban.fish.dat.plot[ban.fish.dat.plot$year==rec.years[i], c("EID", "X", "Y", "Z")], bnk.survey.bound.poly, lvls, FUN=sum, grid.size=1/60)  
  
  ScallopMap("Ban",poly.lst=bnk.polys[1:2],poly.border=bnk.polys[[2]]$border,xlab="",ylab="",
             title=titl, bathy.source="quick",
             plot.bathy = T,plot.boundries = T,boundries="offshore",cex.mn=2,dec.deg = F)
  points(banstations$X, banstations$Y)
  points(banstations2$lon, banstations2$lat, pch=24, bg="darkorange")
  
  tlvls<-lvls/1000
  legend("bottomleft",c(paste(tlvls[-length(tlvls)],'-',tlvls[-1],sep=''),paste(tlvls[length(tlvls)],'+',sep='')),
         fill=bnk.polys[[3]],title='Catch (t)',inset=0.02,bg='white',box.col='white')
  dev.off()
}


#### calculate cumulative catch
cum.ban.fish.dat.plot <- ban.fish.dat.plot[ban.fish.dat.plot$year %in% 2012:2018,]
require(plyr)
cum.ban.fish.dat.plot <- ddply(.data=cum.ban.fish.dat.plot, .(as.character(X), as.character(Y)),
                               summarize,
                               Z=sum(Z))

cum.ban.fish.dat.plot$EID <- 1:nrow(cum.ban.fish.dat.plot)

names(cum.ban.fish.dat.plot) <- c("X", "Y", "Z", "EID")

cum.ban.fish.dat.plot$X <- as.numeric(cum.ban.fish.dat.plot$X)
cum.ban.fish.dat.plot$Y <- as.numeric(cum.ban.fish.dat.plot$Y)

cum.ban.fish.dat.plot <- cum.ban.fish.dat.plot[!is.na(cum.ban.fish.dat.plot$X) | !is.na(cum.ban.fish.dat.plot$Y),]
cum.ban.fish.dat.plot <- cum.ban.fish.dat.plot[cum.ban.fish.dat.plot$Z>0,]
cum.ban.fish.dat.plot <- dplyr::select(cum.ban.fish.dat.plot, EID, X, Y, Z)
source(paste0(direct, "Assessment_fns/Survey_and_OSAC/gridPlot.r"))
bnk.polys.cum <- gridPlot(cum.ban.fish.dat.plot, bnk.survey.bound.poly, lvls, FUN=sum, grid.size=1/60)  

png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ban/Spatial_history_cumulative_2012-2018.png"), units = "in", width=11, height=8.5, res=920)
titl <- paste("Ban - Cumulative Sea Scallop Catch (2012-2018)")
ScallopMap("Ban",poly.lst=bnk.polys.cum[1:2],poly.border=bnk.polys[[2]]$border,xlab="",ylab="",
           title=titl, bathy.source="quick",
           plot.bathy = T,plot.boundries = T,boundries="offshore",cex.mn=2,dec.deg = F)
points(banstations$X, banstations$Y)
points(banstations2$lon, banstations2$lat, pch=24, bg="darkorange")
# points(-59.4925, 44.45083)
tlvls<-lvls/1000
legend("bottomleft",c(paste(tlvls[-length(tlvls)],'-',tlvls[-1],sep=''),paste(tlvls[length(tlvls)],'+',sep='')),
       fill=bnk.polys[[3]],title='Catch (t)',inset=0.02,bg='white',box.col='white')
dev.off()



#### INLA map of Icelandic/Sea scallop ratio
direct <- "C:/Documents/Offshore scallop/Assessment"
load(paste0(direct, "/Data/Survey_data/2012/Survey_summary_output/testing_results_Banspring3.Rdata"))
load(paste0(direct, "/Data/Survey_data/2012/Survey_summary_output/Ban_figures_res_250-250.Rdata"))

BanPR <- mod.res[["PR-spatial"]]
BanRec <- mod.res[["Rec-spatial"]]
BanFR <- mod.res[["FR-spatial"]]

load(paste0(direct, "/Data/Survey_data/2012/Survey_summary_output/BanIce_figures_res_250-250.Rdata"))
BanIcePR <- mod.res[["PR-spatial"]]
BanIceRec <- mod.res[["Rec-spatial"]]
BanIceFR <- mod.res[["FR-spatial"]]

bound.poly.surv.sp <- PolySet2SpatialPolygons(PS=as.PolySet(bnk.survey.bound.poly, projection="LL"))

source(paste0(direct, "/Assessment_fns/Maps/pectinid_projector.R"))

#ratio="IceAllSeaAll"
#ratio="SeaAllIceAll"
#ratio="SeaFRIceFR"
ratio="IceFRSeaFR"
spat.sub="West"
#spat.sub=NULL

if(ratio=="IceAllSeaAll") {
  BanModMatrix <- (BanIceFR+BanIcePR+BanIceRec)/(BanFR+BanPR+BanRec)
  if(!is.null(spat.sub)) lvls <- c(0,1, 2, 5,10, 25, 50, 75)
  else lvls <- c(0,5,50, 150, 300, 500, 650)
}
if(ratio=="SeaAllIceAll"){
  BanModMatrix <- (BanFR+BanPR+BanRec)/(BanIceFR+BanIcePR+BanIceRec)
  if(!is.null(spat.sub)) lvls <- c(0,0.1, 0.5, 1, 1.5, 2, 5,10, 25)
  else lvls <- c(0,1,2,3,5,7,10,15)
}
if(ratio=="SeaFRIceFR") {
  BanModMatrix <- BanFR/BanIceFR
  if(!is.null(spat.sub)) lvls <- c(0, 1, 5,10,50, 100, 500, 1500)
  else lvls <- c(0,1,10,100,1000,2000)
}
if(ratio=="IceFRSeaFR") {
  BanModMatrix <- BanIceFR/BanFR
  if(!is.null(spat.sub)) lvls <- c(0, 0.5, 1, 2, 5,10, 50)
  else lvls <- c(0,1,10,100,1000,2000)
}

if(!is.null(spat.sub)){
  sub <- proj$lattice$loc[,1] < -59
  BanModMatrix[!sub] <- NA
  xlim <- c(-60.2, -59)
  ylim <- c(44.2,44.8)
  area <- "custom"
} else {
  xlim <- NULL
  ylim <- NULL
  area <- "Ban"
}
BanModMatrix[!pred.in] <- NA
cols <- brewer.pal(length(lvls)-1,"YlOrRd")
leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
              paste(lvls[length(lvls)-1],'+',sep=''))

#windows(8.5, 8.5)
png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ratio_spatial_", ratio, spat.sub, ".png"), height=8.5, width=8.5, units="in", res=200)
ScallopMap(area,title="",bathy.source="quick",isobath = c(seq(50,200,by=50)),
           plot.bathy = T,plot.boundries=T,boundries="offshore",
           direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F, xlim=xlim, ylim=ylim)
image(list(x = proj$x, y=proj$y, z = BanModMatrix), axes=F,add=T,breaks = lvls,col=cols)
title(ratio)
points(banstations$X, banstations$Y)
points(banstations2$lon, banstations2$lat, pch=24, bg="darkorange")
plot(bound.poly.surv.sp,add=T,lwd=2)
legend("bottomleft",leg.lvls,fill=cols,
       title=ratio, title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg=NA,box.col=NA)
dev.off()





vals <- BanModMatrix
vals[is.na(vals)] <- 0
mean(vals)

