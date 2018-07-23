#####################################  This sript is used to get a history of the tows in the experimental box on GBa ##############################
#####################################  and also to compare the actual tows in the area with historically interesting tows in the past ##############
###  Things...
###  The experiment plans to remove around 450 kg of scallop.  Since they've closed the area they've removed over 8 tonnes
###  and since 2014 when the large pulse of pre-recruits was first observed the box has been fished regularly (20+ tonnes removed)
###  The thinning is far to thin!!!
###  Abudances are still high, but DD effects tend to happen to smaller scallop, we are now seeing large pre-recruits and in far
###  few numbers than we saw in previous years, if DD effects are taking place we might have missed the bulk of DD already.
###  Kevin's camera photograph 2.8 m^2, assuming each station is 4 quadrats, that is 180 m^2 in total (survey tow covers 1950 m^2)
###  Are the camera drop stations to be right on the tow or offset, if offset how does this measure DD.  More stations along the survey
###  transects seems reasonable, but logistically not sure that's possible a towed gear sure seems more reasonable.

###  Things I need to do... Is this area even high density, any papers out there suggesting when DD kicks in in scallop
###  Get the Biomass/m^2 for this area, compare to Biomass/m^2 for other closures, is this still a high biomass location
###  How much has been removed in terms of Catch/m^2, how does this compare to other closed areas?
###  Once wew have that info I think we're in pretty good shape for a group discussion on this...

yr <- 2017
direct <- "d:/r/"


# This will get us the results for GB survey history
load(paste(direct,"Data/Survey_data/",2016,"/Survey_summary_output/testing_results.Rdata",sep=""))  

surv.dat.all <- rbind( surv.Live$GBa[,c(1:2,4:11,13:52,56:57,60:84,3)],
                       surv.Live$GBb[,c(1:2,4:11,13:52,56:57,60:84,3)])

yr <- 2017
direct <- "d:/r/"
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
source(paste(direct,"Assessment_fns/Survey_and_OSAC/gridPlot.r",sep="")) #Source1
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Fishery/fishery.dat.r",sep=""))
source(paste(direct,"Assessment_fns/maps/ScallopMap.r",sep=""))
library(PBSmapping)
library(maptools)
# I want to get GBa and GB into one object, GB has an odd structure, but this is a clunky way to get it all sorted...

surv.dat.all <- rbind(surv.Live[["GB"]][,c(1:5,8:12,14:53,57:58,60:84,6)],
                      surv.Live[["BBn"]][,c(1:2,4:11,13:52,56:57,60:84,3)],
                      surv.dat.all)

# Get the fishery data
logs_and_fish(loc="offshore",year = 1984:2017,un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat <- fish.dat[!is.na(fish.dat$bank),]
fish.dat <- fish.dat[!is.na(fish.dat$lon),]
fish.dat$ID<-1:nrow(fish.dat)

#GBa.fish.dat <- fish.dat[which(fish.dat$bank == "GBa" & !is.na(fish.dat$bank)),]
#BBn.fish.dat <- fish.dat[which(fish.dat$bank == "BBn" & !is.na(fish.dat$bank)),]
#cpue.dat <- fishery.dat(GBa.fish.dat,bk="GBa",yr=1983:2017,surv='August',method='jackknife',direct=direct,period = "survyr") 	
# Get the coordinates for the data
coordinates(GBa.fish.dat) <- ~lon+lat
coordinates(BBn.fish.dat) <- ~lon+lat

# original presentations (maybe OSAC, but this isn't OSAC)....
#all.closures <- 
seed.boxes <- unique(seedboxes$ID)
seed.boxes
area <- NULL
surv.seed <- NULL
fish.seed <- NULL
seed.obj <- NULL
fish.obj <- NULL
tow.area.m <- atow*1e6 # This is the tow area for a 800 meter tow using 2.4384 m wide gear.
for(i in 1:length(seed.boxes)) 
{
  sb <- seedboxes[seedboxes$ID == seed.boxes[i],]
  # I don't have coordinates for the first box (boot), and I don't care about the Starbox out on Sable..
  if(sb$Bank[1] != "Sab" & length(sb$X[is.na(sb$X)]) == 0)
  {
  
  bound.poly.surv <- subset(survey.bound.polys,label==sb$Bank[1]) 
  attr(bound.poly.surv,"projection")<-"LL"
  box <- as.PolySet(sb,projection = "LL")
  area[[seed.boxes[i]]] <- calcArea(box)
  box.surv.dat <- data.frame(EID=1:nrow(surv.dat.all),X=surv.dat.all$lon,Y=surv.dat.all$lat)
  box.fish.dat <- data.frame(EID=1:nrow(fish.dat),X=fish.dat$lon,Y=fish.dat$lat)
  # Get the data for the box of interest
  surv.key <-findPolys(box.surv.dat, box) 
  fish.key <-findPolys(box.fish.dat, box)  
  
  surv.seed[[seed.boxes[i]]] <- surv.dat.all[1:nrow(box.surv.dat) %in% surv.key$EID,]
  fish.seed[[seed.boxes[i]]] <- fish.dat[1:nrow(box.fish.dat)     %in% fish.key$EID,]
  # This gets us all the survey information from the area
  seed.obj[[seed.boxes[i]]] <- simple.surv(surv.seed[[seed.boxes[i]]],min(surv.seed[[seed.boxes[i]]]$year):max(surv.seed[[seed.boxes[i]]]$year),
                                           user.bins = c(50,70,85,95,120))
  # For some reason having years with NA's breaks the script, no clue why...
  seed.obj[[seed.boxes[i]]]$model.dat <- seed.obj[[seed.boxes[i]]]$model.dat[!is.na(seed.obj[[seed.boxes[i]]]$model.dat$I),] 
    # This will grab the columns with biomass data.
  bm.cols <- c(which(names(seed.obj[[seed.boxes[i]]]$model.dat) %in% c("year", "I","IR","IPR")),
               grep("bm",names(seed.obj[[seed.boxes[i]]]$model.dat))[1:length(bin+1)])
  # And this will grab the columns with abundance data.
  N.cols <-  c(which(names(seed.obj[[seed.boxes[i]]]$model.dat) %in% c("year","N","NR","NPR")),
               grep("mean",names(seed.obj[[seed.boxes[i]]]$model.dat))[1:length(bin+1)])
  # Now get the total abundance/biomass in the box and the abundance/biomass density in the box.
  seed.obj[[seed.boxes[i]]]$BM.total <- (seed.obj[[seed.boxes[i]]]$model.dat[,bm.cols]/atow)*area[[seed.boxes[i]]]$area/1e6
  seed.obj[[seed.boxes[i]]]$BM.total$year <- seed.obj[[seed.boxes[i]]]$model.dat$year
  seed.obj[[seed.boxes[i]]]$N.total <- (seed.obj[[seed.boxes[i]]]$model.dat[,N.cols]/atow)*area[[seed.boxes[i]]]$area/1e6
  seed.obj[[seed.boxes[i]]]$N.total$year <- seed.obj[[seed.boxes[i]]]$model.dat$year
  # Densities now, this is grams/m^2
  seed.obj[[seed.boxes[i]]]$BM.density <- seed.obj[[seed.boxes[i]]]$model.dat[,bm.cols]/tow.area.m
  seed.obj[[seed.boxes[i]]]$BM.density$year <- seed.obj[[seed.boxes[i]]]$model.dat$year
  # Densities now, this is number/m^2
  seed.obj[[seed.boxes[i]]]$N.density <- seed.obj[[seed.boxes[i]]]$model.dat[,N.cols]/tow.area.m
  seed.obj[[seed.boxes[i]]]$N.density$year <- seed.obj[[seed.boxes[i]]]$model.dat$year
  
  # Now get some more nuianced fishery info...
  fish.obj[[seed.boxes[i]]]$total.catch.by.fleet <- aggregate(pro.repwt~year+fleet,fish.seed[[seed.boxes[i]]],FUN=sum)
  # Catch is in tonnes here.
  fish.obj[[seed.boxes[i]]]$total.catch <- aggregate(pro.repwt/1000~year,fish.seed[[seed.boxes[i]]],FUN=sum)
  # Tonnes per km^2, were the catches really "good" in this area.
  fish.obj[[seed.boxes[i]]]$total.catch$catch.density <-  fish.obj[[seed.boxes[i]]]$total.catch$pro.repwt/area[[seed.boxes[i]]]$area
  fish.obj[[seed.boxes[i]]]$total.effort.by.fleet <- aggregate(hm~year+fleet,fish.seed[[seed.boxes[i]]],FUN=sum)
  fish.obj[[seed.boxes[i]]]$total.effort <- aggregate(hm~year,fish.seed[[seed.boxes[i]]],FUN=sum)
  # Effort in hm/km^2, did they work this area hard
  fish.obj[[seed.boxes[i]]]$total.effort$effort.density <-  fish.obj[[seed.boxes[i]]]$total.effort$hm/area[[seed.boxes[i]]]$area
  # Next grab the CPUE's, doing this quick and dirty...
  fish.obj[[seed.boxes[i]]]$CPUE <-  aggregate(kg.hm~year,fish.seed[[seed.boxes[i]]],FUN=mean)
  } # end the if statement
}# end the i loop

# Now make some plots of all this information.
used.boxes <- names(fish.obj)
windows(11,11)
par(mfrow=c(4,6),mar=c(3,3,2,1))
for(i in 1:length(used.boxes)) plot(fish.obj[[used.boxes[i]]]$CPUE$kg.hm~fish.obj[[used.boxes[i]]]$CPUE$year,main = used.boxes[i],xlab="",ylab="",pch=19)
for(i in 1:length(used.boxes)) plot(fish.obj[[used.boxes[i]]]$total.catch$pro.repwt ~  fish.obj[[used.boxes[i]]]$total.catch$year,
                                    main = used.boxes[i],xlab="",ylab="",pch=19,las=1)
# Catch in tonnes/km^2
for(i in 1:length(used.boxes)) plot(fish.obj[[used.boxes[i]]]$total.catch$catch.density ~  fish.obj[[used.boxes[i]]]$total.catch$year,
                                    main = used.boxes[i],xlab="",ylab="",pch=19,las=1)
# Abundance in N/m^2, first all pre-recruits
for(i in 1:length(used.boxes)) plot(seed.obj[[used.boxes[i]]]$N.density$NPR ~  seed.obj[[used.boxes[i]]]$N.density$year,
                                    main = used.boxes[i],xlab="",ylab="",pch=19,las=1)
# Probably more interesting is the 70-85mm densities, these were at unprecented levels in recent years, but
# now are 1/3 the density we saw even in 2015.  Still relatively "very high" compared to typical densities for sure.
for(i in 1:length(used.boxes)) plot(seed.obj[[used.boxes[i]]]$N.density$`mean.70-85` ~  seed.obj[[used.boxes[i]]]$N.density$year,
                                    main = used.boxes[i],xlab="",ylab="",pch=19,las=1)
# But maybe they are just recruits now... Recruit
for(i in 1:length(used.boxes)) plot(seed.obj[[used.boxes[i]]]$N.density$NR ~  seed.obj[[used.boxes[i]]]$N.density$year,
                                    main = used.boxes[i],xlab="",ylab="",pch=19,las=1)
# Fully recruited look interesting at all?
for(i in 1:length(used.boxes)) plot(seed.obj[[used.boxes[i]]]$N.density$N ~  seed.obj[[used.boxes[i]]]$N.density$year,
                                    main = used.boxes[i],xlab="",ylab="",pch=19,las=1)
for(i in 1:length(used.boxes)) plot(seed.obj[[used.boxes[i]]]$BM.density$I ~  seed.obj[[used.boxes[i]]]$N.density$year,
                                    main = used.boxes[i],xlab="",ylab="",pch=19,las=1)


                                    
                                    
# There are 35 tows with more than 22337 pre-recruits out of 8500 tows ever.
big.pre <- all.dat[all.dat$pre > 10000,]
rank(big.pre$pre)[which(big.pre$pre > 22337 & big.pre$pre < 22338)] # Note the order is backwards with rank, smallest - largest, so really it's 79-45 = 36th
# Of these 79 large pre-recruit tows 30 of them occured in 2014!
length(big.pre$year[big.pre$year == 2014])

plot(all.dat$pre ~ all.dat$year,pch=19,cex=0.3)
plot(surv.seed$pre~surv.seed$year)



## Were the fishing trips with watches in C6 in 2017 actually in C6 fishing, lets check using the VMS.

VMS1 <- read.csv(paste(direct,"Data/VMS/vms_106604_20170107_20170109.csv",sep=""))
VMS1 <- VMS1[,2:4]
names(VMS1) <- c("PID","X","Y")
#VMS1$PID <- rep(1,nrow(VMS1))
VMS1 <- as.PolyData(VMS1,projection = "LL")
VMS2 <- read.csv(paste(direct,"Data/VMS/vms_106605_20170208_20170210.csv",sep=""))
VMS2 <- VMS2[,2:4]
names(VMS2) <- c("PID","X","Y")
VMS3 <- read.csv(paste(direct,"Data/VMS/vms_106605_20170303_20170312.csv",sep=""))
VMS3 <- VMS3[,2:4]
names(VMS3) <- c("PID","X","Y")
VMS1 <- as.PolyData(VMS1,projection = "LL")
VMS2 <- as.PolyData(VMS2,projection = "LL")
VMS3 <- as.PolyData(VMS3,projection = "LL")

# These are the log book locations.
C6.loc.fished.2017 <- fish.seed[["C6-122016"]][fish.seed[["C6-122016"]]$year == 2017,c("year","lon","lat")]
C6.loc.fished.2017 <- cbind(1:nrow(C6.loc.fished.2017),C6.loc.fished.2017[,2:3])
names(C6.loc.fished.2017) <-  c("PID","X","Y")
C6.loc.fished.2017 <- as.PolyData(C6.loc.fished.2017,projection="LL")

windows(11,7)
ScallopMap(xlim=c(-66.5,-66.2),ylim=c(42,42.1))
addPolys(seedboxes[seedboxes$Common_name == "C6",])
addPoints(VMS1,pch=19)
addPoints(VMS2,pch=21,bg="blue")
addPoints(VMS3,pch=21,bg="orange")
addPoints(C6.loc.fished.2017[1,],pch=1)
addPoints(C6.loc.fished.2017[2,],pch=2)
addPoints(C6.loc.fished.2017[3:6,],pch=4)
# In it's entirety we see the whole of this box has been fished to some measure since we first saw the large pre-recruit abundances
# Interestingly the only apparent gap here is very much near where we saw our highest abundances in the survey this year...
ScallopMap(xlim=c(-66.5,-66.2),ylim=c(42,42.1))
plot(C6.poly.sp,add=T)
plot(bound.poly.surv.sp,add=T)

