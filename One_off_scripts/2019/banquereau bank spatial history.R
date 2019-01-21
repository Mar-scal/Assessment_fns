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

logs_and_fish(loc="offshore",year = 1981:2018,un=un.ID,pw=pwd.ID,db.con=db.con,direct.off=direct, get.marfis = T)
ban.fish.dat.new <- new.log.dat[new.log.dat$bank=="Ban" & !is.na(new.log.dat$bank=="Ban"),]
ban.fish.dat.old <- old.log.dat[old.log.dat$bank=="Ban" & !is.na(old.log.dat$bank=="Ban"),]
ban.fish.dat <- join(ban.fish.dat.new, ban.fish.dat.old, type="full")

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





ggplot() + geom_point(data=ban.fish.dat[!is.na(ban.fish.dat$lon) & !is.na(ban.fish.dat$lat) & !is.na(ban.fish.dat$hm) & ban.fish.dat$year>2000,], 
                      aes(lon, lat, colour=hm)) +
  geom_polygon(data=survey.bound.polys[survey.bound.polys$label=="Ban",],
               aes(X,Y), fill=NA, colour="black") +
  facet_wrap(~year) +
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  coord_map() 


ggplot() + geom_point(data=ban.fish.dat[!is.na(ban.fish.dat$lon) & !is.na(ban.fish.dat$lat) & !is.na(ban.fish.dat$kg.hm) & ban.fish.dat$year>2000,], 
                      aes(lon, lat, colour=kg.hm)) +
  geom_polygon(data=survey.bound.polys[survey.bound.polys$label=="Ban",],
               aes(X,Y), fill=NA, colour="black") +
  facet_wrap(~year) +
  theme_bw() +
  theme(panel.grid = element_blank()) + 
  coord_map() 

