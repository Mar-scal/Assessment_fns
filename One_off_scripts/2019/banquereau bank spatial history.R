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

somedat <- read.csv(paste0(direct, "Data/Fishery_data/Logs/Compiled/logs_1981-2017.csv"))
ggplot() + geom_point(data=somedat, aes(lon, lat, colour=kg))
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
  
  titl <- paste("Scallop Fishery Catch (","Ban","-",years[i],")",sep="")
  
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

#plot the proposed survey stations too
banstations <- read.csv(paste0(direct, "/Data/Survey_data/fixed_station_banks_towlst.csv"))
banstations2 <- read.csv(paste0(direct, "/Data/Survey_data/extra_stations.csv"))
banstations <- banstations[banstations$Bank=="Ban",]
banstations2 <- banstations2[banstations2$bank=="Ban",]

rec.years <- 2012:2018

rec.years<-unique(ban.fish.dat.plot[ban.fish.dat.plot$year %in% rec.years,]$year)

for (i in 1:length(rec.years)) {
  png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ban/Spatial_history_", rec.years[i], ".png"), units = "in", width=11, height=8.5, res=920)
  
  titl <- paste("Scallop Fishery Catch (","Ban","-",rec.years[i],")",sep="")
  
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
rec.years <- 1999:2011
cum.ban.fish.dat.plot <- ban.fish.dat.plot[ban.fish.dat.plot$year %in% rec.years,]
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

png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ban/Spatial_history_cumulative_", min(rec.years),"-", max(rec.years),".png"), units = "in", width=11, height=8.5, res=920)
titl <- paste0("Ban - Cumulative Catch (", min(rec.years), "-", max(rec.years), ")")
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

require(PBSmapping)
require(maptools)
bound.poly.surv.sp <- PolySet2SpatialPolygons(PS=as.PolySet(bnk.survey.bound.poly, projection="LL"))

source(paste0(direct, "/Assessment_fns/Maps/pectinid_projector.R"))
require(RColorBrewer)

#ratio="IceAllSeaAll"
#ratio="SeaAllIceAll"
#ratio="SeaFRIceFR"
ratio="IceFRSeaFR"
ratio="IceFRTotal"
#spat.sub="West"
spat.sub=NULL

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
  ratio.title <- "Icelandic FR abundance : Sea Scallop FR abundance"
  leg.title <- "Icelandic/Sea"
  if(!is.null(spat.sub)) lvls <- c(0, 0.5, 1, 2, 5,10, 50)
  else lvls <- c(0,1,10,50,100,250,500,750)
}
if(ratio=="IceFRTotal") {
  BanModMatrix <- BanIceFR/(BanIceFR+BanFR)
  ratio.title <- "Icelandic FR abundance : Overall FR scallop abundance"
  leg.title <- "Icelandic/\n(Icelandic+Sea)"
  if(!is.null(spat.sub)) lvls <- pretty(BanModMatrix)
  else lvls <-  pretty(BanModMatrix)
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
cols <- brewer.pal(length(lvls)-1,"YlGnBu")
leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
              paste(lvls[length(lvls)-1],'+',sep=''))

vals <- BanModMatrix
vals[is.na(vals)] <- 0
mean(vals)

#windows(8.5, 8.5)
png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ratio_spatial_", ratio, spat.sub, ".png"), height=8.5, width=11, units="in", res=420, bg="transparent")
  ScallopMap(area,title="",bathy.source="quick",isobath = c(seq(50,200,by=50)),
             plot.bathy = T,plot.boundries=T,boundries="offshore",
             direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F, xlim=xlim, ylim=ylim)
  image(list(x = proj$x, y=proj$y, z = BanModMatrix), axes=F,add=T,breaks = lvls,col=cols)
  contour(x = proj$x, y=proj$y, z = BanModMatrix, axes=F,add=T,levels = lvls,col="grey",drawlabels=F,lwd=1)
  title(ratio.title)
  points(banstations$X, banstations$Y)
  points(banstations2$lon, banstations2$lat, pch=24, bg="darkorange")
  plot(bound.poly.surv.sp,add=T,lwd=2)
  legend("bottomleft",leg.lvls,fill=cols,
         title=leg.title, title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
         pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg=NA,box.col=NA)
  legend("bottomright", title="Mean ratio:", legend=round(mean(vals), 2), inset=0.01, bg=NA, box.col=NA)
dev.off()


#### pulling ISDB data
#### two trips visited banquereau - J12-0326 and J16-0528
bydirect <- "C:/Documents/Bycatch/"
source(paste0(bydirect, "Bycatch_fns/getdiscards.r"))
require(plyr)
sets <- read.csv(paste0(bydirect, "data/Observed scallop trip metadata_2008-2018_tidysets_2019-02-04.csv"))
sets <- subset(sets, TRIP %in% c("J12-0326", "J16-0528") & area == "Ban" & !is.na(area))

discards2012 <- getdiscards_long_detail(trip="J12-0326", un = un.ID, pw = pwd.ID, db.con = "ptran", direct.off = direct, package="ROracle") # must use getdiscards_long in order to deal with split trips properly
discards2016 <- getdiscards_long_detail(trip="J16-0528", un = un.ID, pw = pwd.ID, db.con = "ptran", direct.off = direct, package="ROracle") # must use getdiscards_long in order to deal with split trips properly

discards_ban <- rbind(discards2012, discards2016)

discards_ban <- join(sets, discards_ban, type="left")

discards_ban <- discards_ban[discards_ban$COMMON %in% c("ICELAND SCALLOP", "SEA SCALLOP", "SCALLOPS"),]

discards_ban <- ddply(.data=discards_ban, .(LATITUDE, LONGITUDE, area, COMMON, SOURCE, TRIP),
                      summarize,
                      total_discard=sum(`SUM(EST_DISCARD_WT)`, na.rm=T),
                      total_kept=sum(`SUM(EST_KEPT_WT)`, na.rm=T))

discards_ban$total_caught <- discards_ban$total_discard + discards_ban$total_kept

ggplot() + geom_point(data=discards_ban, aes(-LONGITUDE, LATITUDE, colour=total_caught)) +
  facet_grid(TRIP~COMMON) + 
  coord_map()

discards_ban_wide <- reshape2::dcast(data=discards_ban, TRIP+LATITUDE+LONGITUDE+area ~ COMMON, value.var="total_caught")
discards_ban_wide$`ICELAND SCALLOP`[is.na(discards_ban_wide$`ICELAND SCALLOP`)] <- 0
discards_ban_wide$`SEA SCALLOP`[is.na(discards_ban_wide$`SEA SCALLOP`)] <- 0
discards_ban_wide$`SCALLOPS`[is.na(discards_ban_wide$`SCALLOPS`)] <- 0
discards_ban_wide$prop_iceland <- discards_ban_wide$`ICELAND SCALLOP` / (discards_ban_wide$`ICELAND SCALLOP` + discards_ban_wide$SCALLOPS + discards_ban_wide$`SEA SCALLOP`)

# discards_ban_wide$iceland_1_0[discards_ban_wide$prop_iceland<1] <- "sea > ice"
# discards_ban_wide$iceland_1_0[discards_ban_wide$prop_iceland>1] <- "ice > sea"
# discards_ban_wide$iceland_1_0[discards_ban_wide$prop_iceland==1] <- "ice = sea"

levs <- pretty(discards_ban_wide$prop_iceland[!is.na(discards_ban_wide$prop_iceland)], n=5)
levs <- sort(c(levs, 0.5))
lev.labs <- c(paste0(levs[1:length(levs)-1], " - ", levs[2:length(levs)]), paste0(levs[length(levs)], " +"))

discards_ban_wide$LONGITUDE <- -discards_ban_wide$LONGITUDE
discards_ban_wide <- arrange(discards_ban_wide, LONGITUDE, LATITUDE)
discards_ban_wide$prop_iceland <- cut(discards_ban_wide$prop_iceland, breaks = levs)

source(paste0(direct, "Assessment_fns/Maps/pectinid_projector.R"))
pecjector(area=data.frame(x=c(-60.25,-59.25), y=c(44.4,44.65),proj_sys = as.character("+init=epsg:4326")), repo="local", direct=direct, plot_package = "ggplot2", add_bathy=T, add_land = T)
#display.brewer.all(type="all", n=NULL)
require(viridis)
png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Banquereau_Bycatch.png"), height=8.5, width=8.5, units="in", res=420, bg="transparent")
pect_ggplot + 
  geom_path(data=bound.poly.surv, aes(X, Y, group=PID))+
  geom_point(data=discards_ban_wide[!is.na(discards_ban_wide$prop_iceland),], aes(LONGITUDE, LATITUDE, fill=prop_iceland), colour="black",shape=21, size=2) +
  facet_wrap(~TRIP, nrow=2) + 
  #scale_x_continuous(expand=c(0,0)) +
  #scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette = 'YlGnBu', name="Proportion of Icelandic\nin observed scallop catch\n(kept and discarded)", labels=lev.labs, drop=F)
 # scale_colour_brewer(palette="Dark2", name="Observed catch\n(kept and discarded)", labels=c("Icelandic > Sea scallop", "Sea scallop > Icelandic"))
dev.off()



######## BANQUEREAU INLA
Ban_raw <- surv.Live[["Ban"]][surv.Live[["Ban"]]$year == 2012,c("tow", "lon", "lat","com")]
BanIce_raw <- surv.Live[["BanIce"]][surv.Live[["BanIce"]]$year == 2012,c("tow", "lon", "lat","com")]
Ban_raw$sp <- "sea"
BanIce_raw$sp <- "ice"
require(plyr)
Ban_both <- join(Ban_raw, BanIce_raw, type="full")
require(reshape2)
Ban_both$lon <- round(Ban_both$lon, 6)
Ban_both$lat <- round(Ban_both$lat, 6)

Ban_both <- dcast(data=Ban_both, formula = tow + lon + lat ~ sp, value.var=c("com"))
Ban_both$propIcetotal <- Ban_both$ice/(Ban_both$ice+Ban_both$sea)

loc <- cbind(Ban_both$lon, Ban_both$lat) ### TESTING

# Convert the sp boundary object to a mesh boundary for INLA.
require(INLA)
bound <- inla.sp2segment(bound.poly.surv.sp)
xyl <- rbind(x=range(bound$loc[,1]), y=range(bound$loc[,2])) # get the xy ranges of our survey extent.

# build the mesh
print(paste0("mesh start - ", Sys.time()))
mesh <- inla.mesh.2d(loc, max.edge=c(1, 5)*0.03, offset=0.1,
                     cutoff = 0.006) 
print(paste0("mesh done - ", Sys.time()))

# to add the spatial component:
# Now make the A matrix
A <- inla.spde.make.A(mesh, loc)

spde <- inla.spde2.pcmatern(mesh, 
                            prior.sigma=c(1,0.5), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                            prior.range=c(0.1,0.5)) # The Median range and the probability that the range is less than this..

spat.index <- inla.spde.make.index(name="s", n.spde=spde$n.spde)

# must transform propIcetotal to get rid of 0's and 1's
Ban_both$propIcetotal_trans <-(Ban_both$propIcetotal * (length(Ban_both$propIcetotal)-1) + 0.5)/length(Ban_both$propIcetotal)

# This is the stack for estimation from the INLA model
print("stack")
stk <- inla.stack(tag="est", data=list(propIcetotal=Ban_both$propIcetotal_trans, link=1L),
                  effects=list(intercept = rep(1, nrow(Ban_both)),
                               s = spat.index),
                  A = list(1, A))
print(stk)

# Add an index to the data
# The spatial model, simple model with a intercept (overall bank average) with the spde spatial component
# basically the random deviations for each piece of the mesh.
intercept=1

# This is the INLA model itself
mod <- inla(propIcetotal ~ 0 + intercept + f(s, model=spde), family="beta", 
            data = inla.stack.data(stk),
            control.fam = list(control.link=list(model="logit")),
            control.predictor=list(A=inla.stack.A(stk)))

# Now that this is done we need to make a prediction grid for projection onto our mesh,
print("projecting")
proj <- inla.mesh.projector(mesh=mesh, 
                            xlim=xyl[1, ], ylim=xyl[2,],
                            dims = c(500,500)
) # 500 x 500 gives very fine results but is slow.        
# Then make a matrix of the correct dimension
require(boot)
mod.res.proj <- inla.mesh.project(proj, inv.logit(mod$summary.random$s$mean + mod$summary.fixed$mean))

pred.in <- splancs::inout(proj$lattice$loc,bound$loc)
#mod.res.proj[!pred.in] <- NA # TURN THIS OFF TO MAKE EAST PLOT

## plot it
source(paste0(direct, "Assessment_fns/Maps/pectinid_projector.R"))

# from INLAutils ggplot_projection_shapefile (which is deprecated)
#undebug(ggplot_projection_shapefile)
require(INLAutils)
vals <- mod.res.proj
vals <- raster::raster(t(vals)[nrow(vals):1,])
raster::extent(vals) <- c(range(proj$x), 
                            range(proj$y))
#valsagg <- aggregate(vals, fact=3)
raster.df <- as(vals, "SpatialPixelsDataFrame")
raster.df <- as.data.frame(raster.df)
raster.df <- tidyr::gather(raster.df, key = "raster_name", 
                           value = "z", -x, -y)
names(raster.df) <- c("long", "lat", "raster_name", 
                      "value")
levs <- sort(c(pretty(raster.df$value, n=5), 0.5))
tile <- raster.df
tile$value <- cut(x = tile$value, breaks=levs, labels=levs[1:length(levs)-1])
tile$value <- tile$value

lev.labs <- c(paste0(levs[1:length(levs)-1], " - ", levs[2:length(levs)]), paste0(levs[length(levs)], " +"))

names(banstations2) <- c("EID", "X", "Y", "Bank", "Year", "Cruise", "Survey")
banstations2$EID <- as.character(banstations2$EID)
banstations$type <- "Exploratory repeat"
banstations2$type <- "Extra"
require(RColorBrewer)
cols <- c(brewer.pal(name="YlGnBu", n=length(levs)), "transparent", "darkorange") 
names(cols) <- c(levs, "Exploratory repeat", "Extra")
banstations_all <- join(banstations, banstations2, type="full")

### points only! No INLA

## warning - this takes a long time to plot unless coord_quickmap is used
# pecjector(area="Ban", repo="local", direct=direct, plot_package = "ggplot2", add_nafo=F, add_bathy=T, add_land = T)
# png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ratio_INLA_raster.png"), height=8.5, width=8.5, units="in", res=420, bg="transparent")
# pect_ggplot + 
#   geom_tile(data = raster.df[!is.na(raster.df$value),c("long", "lat", "value")], 
#             aes(long, lat, colour = value))+
#   coord_quickmap()+ # needed to make geom_tile plot fast. Should run it without this line first to make sure that projection looks ok.
#   geom_path(data=bound.poly.surv, aes(X, Y, group=PID))+
#   scale_colour_gradient(high=brewer.pal(n=3, 'YlGnBu')[1], low=brewer.pal(n=3, 'YlGnBu')[3], name="Proportion of Icelandic\nin overall scallop catch",
#                       trans="reverse", expand=c(0,0))+
#   annotate(geom="text",-Inf, Inf, hjust=-0.05, vjust=1.1,label=paste0("Bank mean=", round(mean(raster.df$value),2))) +
#   geom_point(data=banstations_all, aes(X, Y, shape=type, fill=type)) +
#   scale_shape_manual(name="Tow type", values=c(21, 24)) +
#   scale_fill_manual(name="Tow type", values=cols)
# dev.off()
# 
# # alternatively:
# pecjector(area="Ban", repo="local", direct=direct, plot_package = "ggplot2", add_nafo=F, add_bathy=T, add_land = T)
# png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ratio_INLA_contour.png"), height=8.5, width=8.5, units="in", res=420, bg="transparent")
# pect_ggplot + 
#   geom_tile(data = tile[!is.na(tile$value),c("long", "lat", "value")], 
#             aes(long, lat, fill = as.factor(value)))+
#   geom_contour(data=raster.df[!is.na(raster.df$value),c("long", "lat", "value")], aes(long, lat, z=value), breaks=levs, colour="black") +
#   coord_quickmap()+ # needed to make geom_tile plot fast. Should run it without this line first to make sure that projection looks ok.
#   geom_path(data=bound.poly.surv, aes(X, Y, group=PID))+
#   scale_fill_brewer(palette = 'YlGnBu', name="Proportion of Icelandic\nin overall scallop catch", labels=lev.labs, drop=F)+
#   annotate(geom="text",-Inf, Inf, hjust=-0.05, vjust=2,label=paste0("Bank mean=", round(mean(raster.df$value),2)))+
#   geom_point(data=banstations_all[banstations_all$type=="Exploratory repeat",], aes(X, Y), shape=21, size=2) +
#   geom_point(data=banstations_all[banstations_all$type=="Extra",], aes(X, Y), shape=24, fill="darkorange", size=2) +
#   geom_segment(data=surv.Live[["Ban"]][surv.Live[["Ban"]]$year==2012,], aes(x=slon, xend=elon, y=slat, yend=elat))
# ### cannot for the life of me figure out how to create two separate fill legends :(
# dev.off()
# 
# pecjector(area=data.frame(x=c(-60.084, -59.4), y=c(44.3, 44.8), proj_sys = as.character("+init=epsg:4326")), repo="local", direct=direct, plot_package = "ggplot2", add_nafo=F, add_bathy=T, add_land = T)
# raster.df2 <- raster.df[raster.df$long > -60.084 & raster.df$long < -59.4 & raster.df$lat > 44.3 & raster.df$lat < 44.8,]
# png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ratio_INLA_contour_zoomEast.png"), height=8.5, width=8.5, units="in", res=420, bg="transparent")
# pect_ggplot + 
#   geom_tile(data = tile[!is.na(tile$value),c("long", "lat", "value")], 
#             aes(long, lat, fill = as.factor(value)))+
#   geom_contour(data=raster.df2[!is.na(raster.df2$value),c("long", "lat", "value")], aes(long, lat, z=value), breaks=levs, colour="black") +
#   coord_quickmap()+ # needed to make geom_tile plot fast. Should run it without this line first to make sure that projection looks ok.
#   geom_path(data=bound.poly.surv, aes(X, Y, group=PID))+
#   scale_fill_brewer(palette = 'YlGnBu', name="Proportion of Icelandic\nin overall scallop catch", labels=lev.labs, drop=F)+
#   annotate(geom="text",-Inf, Inf, hjust=-0.05, vjust=2,label=paste0("Area mean=", round(mean(raster.df2$value),2)))+
#   geom_point(data=banstations_all[banstations_all$type=="Exploratory repeat",], aes(X, Y), shape=21, size=2) +
#   geom_point(data=banstations_all[banstations_all$type=="Extra",], aes(X, Y), shape=24, fill="darkorange", size=2) +
#   geom_segment(data=surv.Live[["Ban"]][surv.Live[["Ban"]]$year==2012,], aes(x=slon, xend=elon, y=slat, yend=elat))
# ### cannot for the life of me figure out how to create two separate fill legends :(
# dev.off()

levs <- sort(c(pretty(Ban_both$propIcetotal, n=6), 0.5))
Ban_both$propIcetotal_levs <- cut(x=Ban_both$propIcetotal, breaks=levs, include.lowest=T, right=F)
Ban_both$propIcetotal_levs <- as.character(Ban_both$propIcetotal_levs)
Ban_both$propIcetotal_levs[Ban_both$propIcetotal==0] <- "0"
Ban_both$propIcetotal_levs[Ban_both$propIcetotal==1] <- "1"
Ban_both$propIcetotal_levs <- as.factor(Ban_both$propIcetotal_levs)
Ban_both$propIcetotal_levs <- factor(Ban_both$propIcetotal_levs, levels=c("0", "[0,0.2)", "[0.2,0.4)", "[0.4,0.5)", 
                                                                          "[0.5,0.6)", "[0.6,0.8)","[0.8,1]", "1"))

lev.labs <- c(0, paste0(levs[1:length(levs)-1], " - ", levs[2:length(levs)]), 1)


### bathymetry
isobath <- c(seq(50,200,by=50))
all<- NULL
for(i in 1:length(isobath))
{
  
  #Read4 Bring in the files
  d.ll<-read.table(paste(direct,"Data/Maps/approved/Bathymetry/quick/offshore/d",isobath[i],".ll",sep=''),header=T)
  # remove any "NA" values
  d.ll<-na.omit(d.ll)
  # Make sure data is a projection and it is Latitude/longitude
  attr(d.ll,"projection") <- "LL"
  # Add the lines to the plot
  # addLines(d.ll,col=b.col[i])
  d.ll$FID <- paste0(d.ll$SID, ".", i)
  all <- rbind(all,d.ll)
  
} # end for(i in 1:length(isobath))  

pecjector(area="Ban", repo="local", direct=direct, plot_package = "ggplot2", add_nafo=F, add_land = T)
png(paste0(direct, "2012/Presentations/Survey_summary/test_figures/Ratio_points.png"), height=8.5, width=8.5, units="in", res=420, bg="transparent")

pect_ggplot +
  geom_path(data=all, aes(X, Y, group=FID), colour="lightblue", alpha=0.5) +
  geom_point(data=Ban_both[!is.na(Ban_both$propIcetotal_levs),], aes(lon, lat, fill=propIcetotal_levs), colour="black",shape=21, size=3) +
  #geom_point(data=Ban_both[Ban_both$propIcetotal==0,], aes(lon, lat, fill=propIcetotal_levs), colour="black",shape=21, size=2) +
  #geom_point(data=Ban_both[Ban_both$propIcetotal==1,], aes(lon, lat, fill=propIcetotal_levs), colour="black",shape=21, size=2) +
  scale_fill_brewer(palette = 'YlGnBu', name="Proportion of\nIcelandic in\noverall scallop\nabundance", labels=lev.labs, drop=F)+
  annotate(geom="text", x=-60.2, y=44.72, hjust=0, vjust=0,
           label=paste0("Bank mean = ", round(mean(Ban_both$propIcetotal, na.rm=T),2)))#+
  # geom_point(data=banstations_all[banstations_all$type=="Exploratory repeat",], aes(X, Y), shape=21, size=2) +
  # geom_point(data=banstations_all[banstations_all$type=="Extra",], aes(X, Y), shape=24, fill="darkorange", size=2) +
  #geom_segment(data=surv.Live[["Ban"]][surv.Live[["Ban"]]$year==2012,], aes(x=slon, xend=elon, y=slat, yend=elat))
dev.off()

pect_ggplot + 
  geom_path(data=bound.poly.surv, aes(X, Y, group=PID))+
  geom_point(data=discards_ban_wide[!is.na(discards_ban_wide$prop_iceland),], aes(LONGITUDE, LATITUDE, fill=prop_iceland), colour="black",shape=21, size=2) +
  facet_wrap(~TRIP, nrow=2) + 
  #scale_x_continuous(expand=c(0,0)) +
  #scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette = 'YlGnBu', name="Proportion of Icelandic\nin observed scallop catch\n(kept and discarded)", labels=lev.labs, drop=F)
# scale_colour_brewer(palette="Dark2", name="Observed catch\n(kept and discarded)", labels=c("Icelandic > Sea scallop", "Sea scallop > Icelandic"))
dev.off()

