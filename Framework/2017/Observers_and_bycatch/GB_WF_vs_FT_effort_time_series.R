#################  This little script is used to take a look at the effort time series on Georges Bank #####################################
# Created by DK on June 26th, 2017

##################
yr = as.numeric(format(Sys.time(), "%Y")) # 
direct = "d:/r/"

# Bring in functions
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))

# Get the data from 1981 to 2016
years <- 1981:2016
logs_and_fish(loc="offshore",year = years,un=un.ID,pw=pwd.ID,db.con="PTRAN64",direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

# Now subset the fishery data to GB
bank.fish <- subset(fish.dat, bank %in% c("GBa","GBb"))
# For data before 2009 the NAFO regions are delinated differently, rename them using the division line b/t the two areas.
# in the data from 2009 to current anything >= 41.83333 is 5ZEJ 
# so I stick with that (41° 50' is dividing line, just concerned about rounding/points on the line)
bank.fish$nafo[bank.fish$lat < 41.83333 & bank.fish$year < 2009] <- "5ZEM"
bank.fish$nafo[bank.fish$lat >= 41.83333 & bank.fish$year < 2009] <- "5ZEJ"
bank.fish$month <- as.numeric(format(bank.fish$date, "%m"))
  
effort <- aggregate(hm ~ nafo+fleet+year,bank.fish,FUN="sum",drop=F)
catch <- aggregate(pro.repwt ~ year,bank.fish,FUN="sum",drop=F)
effort <- aggregate(hm ~ year,bank.fish,FUN="sum",drop=F)
# Catch and effort highly correlated since 2007.
ccf(catch$pro.repwt[catch$year > 2006],effort$hm[effort$year > 2006],plot=F)
#plot(catch$pro.repwt[catch$year > 2006],effort$hm[effort$year > 2006],plot=F)
# Using this as a check against our discard xls file (HM_discards ma std16 revisedJun142017.xls)
annual.effort.by.area <- aggregate(hm ~ nafo+year,bank.fish,FUN= function(x) sum(x),drop=F)

# OK, this is ridiculous generalization of code, but this gets us our percentages by year/fleet/area...
tmp.3 <- NULL
for(i in 1:length(years))
{
  tmp <- bank.fish[bank.fish$year == years[i],]
  eff <- aggregate(hm~nafo,tmp,FUN="sum")
  eff.fleet <- aggregate(hm~nafo+fleet,tmp,FUN="sum")
  tmp.1 <- NULL
  tmp.2 <- NULL
  for(j in 1:nrow(eff)) 
  {
    for(k in 1:length(unique(eff.fleet$fleet)))
    {
      per.eff.bank <-  eff.fleet$hm[eff.fleet$nafo == eff$nafo[j] & eff.fleet$fleet ==  unique(eff.fleet$fleet)[k]] /eff[j,]$hm
      tmp.1[[k]] <- c(years[i],eff$nafo[j],unique(eff.fleet$fleet)[k],per.eff.bank)
    } # end  for(k in 1:length(unique(eff.fleet$fleet)))
    tmp.2[[j]] <- do.call("rbind",tmp.1)  
  } # end for(j in 1:nrow(eff)) 
tmp.3[[i]] <- do.call("rbind",tmp.2)  
} # end for(i in 1:length(years))
per.eff.bank <- as.data.frame(do.call("rbind",tmp.3),stringsAsFactors = F)

names(per.eff.bank) <- c("year","nafo","fleet","per.hm")

per.eff.bank$per.hm <- as.numeric(per.eff.bank$per.hm)
per.eff.bank$per.hm[per.eff.bank$per.hm > 1] <- 0 # occaionally when missing data the above code breaks, but in all cases it should be a 0.
per.eff.bank$year <- as.numeric(per.eff.bank$year)

annual.effort.hours.by.area <- aggregate(h ~ nafo+year,bank.fish,FUN="sum",drop=F)
monthly.effort.by.area <- aggregate(hm ~ year+month+nafo+fleet,bank.fish,FUN="sum")
effort.by.area.monthly.2004 <- monthly.effort.by.area[monthly.effort.by.area$year==2004,]

write.csv(monthly.effort.by.area,paste(direct,"data/Framework/2017/Observers_and_bycatch/monthly.effort.by.area.csv",sep=""),row.names = F)

#######################  ADD THE PERCENTAGES!!!
# Some colors I can see...
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73")

# Now make the plots and save them...
png(paste(direct,"2017/Framework/Observers_and_bycatch/Fleet_effort_EZej_vsEZem.png",sep=""),units="in",width = 11, height=8.5,res=420)
p <- ggplot(effort, aes(x=year, y=hm, colour=fleet))  + scale_colour_manual(values=cbbPalette)  
p <- p + facet_grid(. ~ nafo)
p <- p + geom_line(size=1.25) #adds the points. 
p
dev.off()

png(paste(direct,"2017/Framework/Observers_and_bycatch/Fleet_effort_EZej_vsEZem_since_2002.png",sep=""),units="in",width = 11, height=8.5,res=420)
p <- ggplot(effort[effort$year >= 2002,], aes(x=year, y=hm, colour=fleet))  + scale_colour_manual(values=cbbPalette)  
p
p <- p + facet_grid(. ~ nafo)
p
p <- p + geom_line(size=1.25) #adds the points. 
p
dev.off()

png(paste(direct,"2017/Framework/Observers_and_bycatch/Percent_Fleet_effort_EZej_vsEZem_since_2002.png",sep=""),units="in",width = 11, height=8.5,res=420)
p <- ggplot(per.eff.bank[per.eff.bank$year >= 2002,], aes(x=year, y=per.hm, colour=fleet))  + scale_colour_manual(values=cbbPalette)  
p
p <- p + facet_grid(. ~ nafo)
p
p <- p + geom_line(size=1.25) #adds the points. 
p
dev.off()


# What does GBb percentages look like...
GBb.fish <- subset(fish.dat, bank %in% c("GBb"))
GBb.effort <- aggregate(hm ~ fleet+year,GBb.fish,FUN="sum")

tmp.3 <- NULL
for(i in 1:length(years))
{
  tmp <- bank.fish[GBb.fish$year == years[i],]
  if(nrow(tmp) > 0)
  {
  eff.fleet <- aggregate(hm~fleet,tmp,FUN="sum")
  eff <- sum(tmp$hm,na.rm=T)
  tmp.1 <- NULL
    for(k in 1:length(unique(eff.fleet$fleet)))
    {
      per.eff.bank <-  eff.fleet$hm[ eff.fleet$fleet ==  unique(eff.fleet$fleet)[k]] /eff
      tmp.1[[k]] <- c(years[i],unique(eff.fleet$fleet)[k],per.eff.bank)
    } # end  for(k in 1:length(unique(eff.fleet$fleet)))
  tmp.3[[i]] <- do.call("rbind",tmp.1)  
  } # end if(nrow(tmp) > 0)
} # end for(i in 1:length(years))
per.eff.GBb <- as.data.frame(do.call("rbind",tmp.3),stringsAsFactors = F)

names(per.eff.GBb) <- c("year","fleet","per.hm")

per.eff.GBb$per.hm <- as.numeric(per.eff.GBb$per.hm)
per.eff.GBb$per.hm[per.eff.GBb$per.hm > 1] <- 0 # occaionally when missing data the above code breaks, but in all cases it should be a 0.
per.eff.GBb$year <- as.numeric(per.eff.GBb$year)

png(paste(direct,"2017/Framework/Observers_and_bycatch/GBb_Percent_Fleet_effort_since_2002.png",sep=""),units="in",width = 11, height=8.5,res=420)
p <- ggplot(per.eff.GBb[per.eff.GBb$year >= 2002,], aes(x=year, y=per.hm, colour=fleet))  + scale_colour_manual(values=cbbPalette)  
p
#p <- p + facet_grid(. ~ nafo)
#p
p <- p + geom_line(size=1.25) #adds the points. 
p
dev.off()

###  For 2004 this is the effort in hm for the 5 obseved trips
trip1 <- fish.dat[fish.dat$vesid == 5912 & fish.dat$date > "2004-08-23" & fish.dat$date < "2004-09-10",] # 
trip2 <- fish.dat[fish.dat$vesid == 4432 & fish.dat$date > "2004-09-22" & fish.dat$date < "2004-10-01",] #  4432 is GS Mersey's pre 2008 ID code
trip3 <- fish.dat[fish.dat$vesid == 4211  & fish.dat$date > "2004-10-15" & fish.dat$date < "2004-10-21",] # Lady Comeaus's pre 2008 ID code
trip4 <- fish.dat[fish.dat$vesid == 5736 & fish.dat$date > "2004-11-08" & fish.dat$date < "2004-12-01",] # 
trip5 <- fish.dat[fish.dat$vesid == 4536 & fish.dat$date > "2004-12-07" & fish.dat$date < "2004-12-19",] # IOcean Lady's pre 2008 ID code

# Effort in hm
sum(trip1$hm)
sum(trip2$hm)
sum(trip3$hm)
sum(trip4$hm)
sum(trip5$hm)

# Effort in hours
sum(trip1$h)
sum(trip2$h)
sum(trip3$h)
sum(trip4$h)
sum(trip5$h)
