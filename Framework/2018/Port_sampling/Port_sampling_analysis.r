# This script performs a number of different analyses on the port sampling data...

# Frist grab the directory to work from 
direct <- "d:/r/"
# Grab the years of port sampling data.  Note that Ginette says the Port Sampling data goes all the way back to 1996
# I haven't a clue where that data is, but would certainly be interesting to compare the last 20 years of data!!
years <- 2006:2017
options(stringsAsFactors = F) # Don't make anything a Factor as it screws up random crap.

# Load in librarys we may need...
library(ggplot2)
library(xlsx)
library(reshape2)
library(dplyr)
library(plyr)
library(lubridate)
library(PBSmapping)
library(ggfortify)
library(mgcv)
library(maps)
library(maptools)
library(rgeos)
library(splancs)
library(INLA)
library(viridis)
library(fields)
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))


###############  Port sampling analysis
load(paste0(direct,"Data/Framework/2018/Port_sampling/Port_sampling_processed_data.rData"))

# I'll need the survey data for later + need the seebox info...

load(paste0(direct,"Data/Survey_data/2017/Survey_summary_output/Survey_all_results.RData"))

# For the models to come let's center on the day we believe the ASM's arrive on the scence (May 24, 2010 according to Ginette)
# The day here respresents 100 days.
port.sampling$mod.day <- as.numeric(port.sampling$ps.date - as.Date("2010-05-24"))/100
# We also need to add in the "bit" to the meat weights used in the port sampling, Ginette provided this as 1.0989011, this is a coefficient and 
# it is multiplied by the meat weight...
port.sampling$meat_weight <- port.sampling$meat_weight * 1.0989011
port.sampling$mc <- 500 / port.sampling$meat_weight
# And let's just look at GBa
gba.dat <- port.sampling[port.sampling$bank == "GBa",]
gba.boxes <- seedboxes[seedboxes$ID != "Boot" & seedboxes$Bank == "GBa",]


# Note there is a point in the US, so let's remove that one  and any data without lat/lon information...
gba.dat <- gba.dat[-which(gba.dat$lon < -66.75 & gba.dat$lat < 41.8),]
gba.dat <- gba.dat[-which(gba.dat$lon == 0 | gba.dat$lat ==0),]
gba.dat$fleet_fac <- factor.2.number(factor(gba.dat$fleet,labels=c(1,2,3)))


# Now whenever a seedbox opens we run the risk of this opening influencing the actual bank trends.  From the figures it seems the seedbox effect
# generally runs for about 1.5 - 2 months after the seedbox opens, so let's use the seedbox opening date on GBa and remove all the data in the
# 2 months following the opening of the seedbox.  
seedboxes <- seedboxes[!is.na(seedboxes$Open),]
seed.dates <- data.frame(open_date = unique(seedboxes$Open[seedboxes$Bank == "GBa" & seedboxes$Open > ymd('2005-01-01') & seedboxes$Open < ymd('2050-01-01')]))
# Now the same models but removing the month of a seedbox opening and the following 2 months of data...
seed.dates$plus2 <- seed.dates$open_date  %m+% months(2)

# Add an year month combo column to port.sampling...
gba.dat$year_month <- ymd(paste(year(gba.dat$ps.fished),month(gba.dat$ps.fished),15,sep="-"))
# And a centered year and a centered month...
gba.dat$year.cen <- gba.dat$year - 2010 # Make 2010 year 0, this is when the ASM's come onboard....
gba.dat$month.cen <- gba.dat$month - 6 # Make June the intercept, this is the month of best condition...


# Now we can make a gba object that has the 2 months following a closure removed....
tmp <- NULL
for(i in 1:nrow(seed.dates)) tmp[[i]] <- seq(seed.dates$open_date[i],seed.dates$plus2[i],by=1)
rm.dates <- do.call('c',tmp)
# And remove all the data within 2 months of the closure...
gba.sub.dat <- gba.dat[-which(gba.dat$date %in% rm.dates),]

# 
# 
# # Plot a time series of the port sampling data by fleet
# windows(11,11)
# ggplot(port.sampling,aes(fished,meat_weight,colour=fleet)) + geom_point(size=0.2) + geom_smooth()
# 
# # Again by fleet, but lump the years together
# windows(11,11)
# ggplot(port.sampling,aes(year,meat_weight,colour=fleet)) + geom_point(size=0.02) + geom_smooth() + facet_wrap(~ bank)


mc.by.year.fleet <- aggregate(mc~year+fleet,gba.dat,median)
mc.by.year.fleet

# Now with the 2 months after a seedbox opens removed...
mc.by.month.fleet.sb<- aggregate(mc~month+fleet,gba.sub.dat,median)
mc.by.month.fleet.sb

# # Is there a clear seasonal trend in here
# windows(11,11)
# ggplot(gba.dat,aes(mc,colour=fleet)) + geom_histogram() + facet_wrap(~month,scales = "free") +
#   theme_bw() + theme(panel.grid=element_blank()) + scale_color_manual(values=c(alpha("blue",0.5),alpha("black",0.5),alpha("darkgrey",0.5)))
# 
# ## Here are some better figures...
# windows(11,11)
# ggplot(gba.dat,aes(ps.fished,mc,colour=fleet)) + geom_point(size=0.02) + geom_smooth() +
#   theme_bw() + theme(panel.grid=element_blank()) + scale_color_manual(values=c(alpha("blue",0.5),alpha("black",0.5),alpha("darkgrey",0.5))) 
# # Removing the seedbox influenced data...
# windows(11,11)
# ggplot(gba.sub.dat,aes(ps.fished,mc,colour=fleet)) + geom_point(size=0.02) + geom_smooth() +
#   theme_bw(base_size = 18) + theme(panel.grid=element_blank()) + scale_color_manual(values=c(alpha("blue",0.5),alpha("black",0.5),alpha("darkgrey",0.5))) 
# 


# Now I want to zoom in on the month trends during the periods of rapid change (i.e. spring and late summer), how variable is the increase in mw (i.e. condition)
# Note that to keep the smooth using all the data you need to specific coordinate cartesian option for the ylim...
windows(11,11)
ggplot(gba.dat[gba.dat$month %in% c(3:6),],aes(date,meat_weight,colour=fleet)) + geom_point(size=0.02) + facet_wrap(~year,scales = "free") + geom_smooth()+
  theme_bw() + theme(panel.grid=element_blank()) + scale_color_manual(values=c(alpha("blue",0.5),alpha("black",0.5),alpha("darkgrey",0.5)))+
  coord_cartesian(ylim=c(15,30))
# Now late summer...
windows(11,11)
ggplot(gba.dat[gba.dat$month %in% c(7:9),],aes(date,meat_weight,colour=fleet)) + geom_point(size=0.02) + facet_wrap(~year,scales = "free") + geom_smooth()+
  theme_bw() + theme(panel.grid=element_blank()) + scale_color_manual(values=c(alpha("blue",0.5),alpha("black",0.5),alpha("darkgrey",0.5)))+
  coord_cartesian(ylim=c(15,30))

# Now the same with those 2 months post seedbox removed....
windows(11,11)
ggplot(gba.sub.dat[gba.sub.dat$month %in% c(1:6),],aes(date,meat_weight,colour=fleet)) + geom_point(size=0.02) + facet_wrap(~year,scales = "free") + geom_smooth()+
  theme_bw() + theme(panel.grid=element_blank()) + scale_color_manual(values=c(alpha("blue",0.5),alpha("black",0.5),alpha("darkgrey",0.5)))+
  coord_cartesian(ylim=c(15,30))
# Now late summer...
windows(11,11)
ggplot(gba.sub.dat[gba.sub.dat$month %in% c(7:9),],aes(date,meat_weight,colour=fleet)) + geom_point(size=0.02) + facet_wrap(~year,scales = "free") + geom_smooth()+
  theme_bw() + theme(panel.grid=element_blank()) + scale_color_manual(values=c(alpha("blue",0.5),alpha("black",0.5),alpha("darkgrey",0.5)))+
  coord_cartesian(ylim=c(15,30))


# First the simple model, is there a linear trend in meat counts over time...
mod.1 <- lm(mc~mod.day,gba.dat)
# quick diagnostics check, can see that overall there is a trend toward smaller scallop, think this would 
# be slightly more straightforward to interpret if the date wasn't a date but was numeric...
summary(mod.1) # SO on May 24 2010 this says the mc was 28.82 and that is has declined by 0.046 every 100 days since...

# The right skew of the data is somewhat evident in the Normal Q-Q plot, given these are strictly positive 
# it might make sense to go glm with a gamma, poisson, or quasipoisson.
windows(11,11)
par(mfrow=c(2,2))
autoplot(mod.1)

# Here's the lm fit to the data, a small deline over time...
windows(11,11)
ggplot(gba.dat,aes(ps.fished,mc)) + geom_point(size=0.02)+geom_smooth(method="lm")

# Next we make a slighly more sensible model, still linear but looking for a different in trends between fleets...

mod.2 <- lm(mc~mod.day*fleet,gba.dat)
mod.2.me <- lm(mc~mod.day*fleet-mod.day,gba.dat) # This gives the difference between fleets on 2010-05-24 and regression coefficients by fleet since...
summary(mod.2) # so this says the the ASM mean on May 24 2010 was 31.3 and that is has declined by 0.085 every 100 days since
# Whereas the FT fleet mean was 2.86 lower, and the Wf fleet was 4.1 lower.  The decline for the FT was not significantly different than the ASM's
# but the WF fleet decline was significantly different from the ASM (and likely the FT's), the slope being essentially twice that of the ASM fleet
summary(mod.2.me) # Says same as above but gives actual regression coefficients for the slopes rather than the differences.

# Here's the lm fit to the data,
windows(11,11)
ggplot(gba.dat,aes(mod.day,mc,colour=fleet)) + geom_point(size=0.02)+geom_smooth(method="lm")

# Now we can move to a gam type of model to allow for non-linearity in the patterns....
mod.3 <- gamm(mc ~ s(mod.day,by=as.factor(fleet)),data=gba.dat)
summary(mod.3$lme)
summary(mod.3$gam)

# Now we can move to a gam type of model to allow for non-linearity in the patterns....
mod.3.sb <- gamm(mc ~ s(mod.day,by=as.factor(fleet)),data=gba.sub.dat)
summary(mod.3.sb$lme)
summary(mod.3.sb$gam)


# Let's take a closer look at these GAM results....
anova(mod.3$gam)# Careful with these p values as the smoother comes into play on these...
summary(mod.3$lme) #


#Model validation
windows(11,11)
par(mfrow=c(3,1))
plot(mod.3$gam,ylim=c(-10,10))
windows(11,11)
par(mfrow=c(3,1))
plot(mod.3$lme) 

#par(mfrow = c(2,2), mar = c(5,5,2,2))    
E1 <- resid(mod.3$lme, type ="n")
F1 <- fitted(mod.3$lme)

windows(11,11)
par(mfrow = c(2,2))
plot(x = F1, y = E1, xlab = "Fitted values",      ylab ="Residuals",cex=0.01)
abline(h=0, lty=2)

plot(x = gba.dat$mod.day, y = E1,  xlab = "time",  ylab = "Residuals",cex=0.01)
abline(h = 0, lty = 2)

boxplot(E1 ~ fleet, data = gba.dat)
abline(h = 0, lty = 2)

# Now we can get some predictions from the model, this is much tidier than dealing with the gigantic dataset!!
# First lets get good days to predict on, we'll go for the first of every month...
range(port.sampling$date)
pred.dates <- ymd(paste0(sort(rep(2006:2017,12)),"-",rep(1:12,12),"-",rep(0,144),rep(1,144)))
mod.pred.day <- as.numeric(pred.dates - as.Date("2010-05-24"))/100
pred.dat <- ddply(gba.dat, 
                .(fleet), 
                summarize,
                mod.day = seq(min(mod.pred.day), 
                              max(mod.pred.day),
                              length = length(mod.pred.day)),
                date = pred.dates)

pred.dat.sb <- pred.dat

mod.pred <- predict(mod.3$gam, newdata = pred.dat, se = TRUE)
mod.pred.sb <- predict(mod.3.sb$gam, newdata = pred.dat, se = TRUE)
# Glue them all together
pred.dat$mc <- mod.pred$fit
pred.dat$ub <- mod.pred$fit + 1.96 * mod.pred$se.fit
pred.dat$lb <- mod.pred$fit - 1.96 * mod.pred$se.fit
# Also stick the acutal meat weights on here...
pred.dat$mw <- 500/pred.dat$mc
pred.dat$mw.ub <- 500/pred.dat$ub
pred.dat$mw.lb <- 500/pred.dat$lb

# Same for the data excluding the seedboxes...
pred.dat.sb$mc <- mod.pred.sb$fit
pred.dat.sb$ub <- mod.pred.sb$fit + 1.96 * mod.pred.sb$se.fit
pred.dat.sb$lb <- mod.pred.sb$fit - 1.96 * mod.pred.sb$se.fit
# Also stick the acutal meat weights on here...
pred.dat.sb$mw <- 500/pred.dat.sb$mc
pred.dat.sb$mw.ub <- 500/pred.dat.sb$ub
pred.dat.sb$mw.lb <- 500/pred.dat.sb$lb


# Remove all ASM predictions before June 1st 2010 since there were no active vessels before this date (well May 24th, but I'm rounding...)
pred.dat <- pred.dat[-which(pred.dat$fleet == "ASM" & pred.dat$date < "2010-06-01"),]
pred.dat.sb <- pred.dat.sb[-which(pred.dat.sb$fleet == "ASM" & pred.dat.sb$date < "2010-06-01"),]
head(pred.dat)

# The overall picture of the port sampling
windows(11,11)
ggplot(gba.dat, aes(date,mc))  + theme(text = element_text(size=16)) + theme_bw() + geom_smooth()+
  geom_point(shape = 16, size = 0.01,alpha=0.05)+ 
   coord_cartesian(ylim=c(5,40)) + ylab("Meat count") + scale_x_date(date_breaks="1 year") + xlab("") +
  scale_color_manual(values = c("blue")) + scale_fill_manual(values = alpha("darkgrey",0.5))
ggsave(paste0(direct,"2018/Framework/Port_sampling/overall_port_sampling.png"),width=11,height=8.5)


windows(11,11)
ggplot(gba.sub.dat, aes(date,mc))  + theme(text = element_text(size=18)) + theme_bw() + geom_smooth()+
  geom_point(shape = 16, size = 0.01,alpha=0.05)+ 
  coord_cartesian(ylim=c(5,50)) + ylab("Meat count") + scale_x_date(date_breaks="1 year") + xlab("") +
  scale_color_manual(values = c("blue")) + scale_fill_manual(values = alpha("darkgrey",0.5))
ggsave(paste0(direct,"2018/Framework/Port_sampling/overall_port_sampling_excluding_seedboxes.png"),width=11,height=8.5)


windows(11,11)
ggplot(pred.dat, aes(date,mc,colour=fleet))  + theme(text = element_text(size=16)) + theme_bw() + 
           geom_point(data = gba.dat, aes(y = mc, x = date,colour = fleet),shape = 16, size = 0.01,alpha=0.05)+ 
           geom_line()  + ylim(5,60) + ylab("Meat count") + scale_x_date(date_breaks="1 year") + xlab("") +
           geom_ribbon(data = pred.dat, aes(x = date, ymax = ub, ymin = lb,  fill = fleet,color=fleet), alpha = 0.5)+
           scale_color_manual(values = c("blue","black","darkgrey")) + scale_fill_manual(values = c(alpha("blue",0.5),alpha("black",0.5),alpha("darkgrey",0.5)))
ggsave(paste0(direct,"2018/Framework/Port_sampling/Fleet_port_sampling.png"),width=11,height=8.5)


windows(11,11)
ggplot(pred.dat.sb, aes(date,mc,colour=fleet))  + theme(text = element_text(size=16)) + theme_bw() + 
  geom_point(data = gba.sub.dat, aes(y = mc, x = date,colour = fleet),shape = 16, size = 0.01,alpha=0.05)+ 
  geom_line()  + ylim(5,60) + ylab("Meat count") + scale_x_date(date_breaks="1 year") + xlab("") +
  geom_ribbon(data = pred.dat.sb, aes(x = date, ymax = ub, ymin = lb,  fill = fleet,color=fleet), alpha = 0.5)+
  scale_color_manual(values = c("blue","black","darkgrey")) + scale_fill_manual(values = c(alpha("blue",0.5),alpha("black",0.5),alpha("darkgrey",0.5)))
ggsave(paste0(direct,"2018/Framework/Port_sampling/Fleet_port_sampling_removed_seedboxes.png"),width=11,height=8.5)


# Now let's look at the Port sampling for August and May, when we have our survey on GBa.  Using this information along with the
# MW-SH relationship for each year we can determine what size scallop the fleet is targeting, especially groovy if our May and August
# estimates line up!
# Here's my logic on this analysis...
# 1:  There is a meat-weight shell height relationship in May and August, it may differ between these two periods, whether or not the difference is
#     "signifcant" we can figure out I think.
# 2:  This meat-weight shell height relationship holds for the whole bank during this time, so using the May/August MW-SH relationship and the port sampling
#     meat weights we can determine what size (i.e. shell hieghts) that the fleets are targeting during these 2 months.
# 3:  IF the shell heights targeted by each fleet in May and August don't differ significantly WE CAN MAKE THE ASSUMPTION that the fleet, on average, is likely 
#     targeting the same size scallop all year
# 4:  With this assumption in place (and given the data it does appear this is a reasonable assumption) we can make MW-SH models 
#     at a finer temporal scale (e.g. calclate the average MW by fleet for each month) and estimate condition at these finer temporal scales. 
#     now we have an estimate of how condition varies over the year and if condition varies between fleets 
#     (though I'm not sure if the fleet question makes sense, but I think it does...).  If no variation between fleets then we can combine data and look at 
#     the seasonal condition trends.  
# 5:  


# Now what we want is to fit the MW-SH model to these data for each year back to 2006 and for both May and August survey...
# For the most recent data
years <- 2006:2017
aug.mws <- NULL
may.mws <- NULL
a.may <- data.frame(a = rep(NA,length(years)), year = years)
a.aug <- data.frame(a = rep(NA,length(years)), year = years)

# Now fit a linear model on the wmw v.s. (shell height)^3 to find the intercept (essentially condition), note this is forced through 0.
# Now we can subset the predicted data from before and figure out what the targeted shell height is...
for(i in 1:length(years))
{
  aug.mws[[as.character(years[i])]] <-  na.omit(mw[["GBa"]][mw[["GBa"]]$year == years[i],]) # get GBa and chuck the na's
  may.mws[[as.character(years[i])]] <-  na.omit(mw[["GB"]][mw[["GB"]]$year == years[i],]) # get GB and chuck the na's
  aug.mws[[as.character(years[i])]]$sh_3 <- (aug.mws[[as.character(years[i])]]$sh/100)^3 # cube the SH's
  may.mws[[as.character(years[i])]]$sh_3 <- (may.mws[[as.character(years[i])]]$sh/100)^3 # cube the SH's
  aug.mws[[as.character(years[i])]]$sh <- (aug.mws[[as.character(years[i])]]$sh/100) # sh in decimeters
  may.mws[[as.character(years[i])]]$sh <- (may.mws[[as.character(years[i])]]$sh/100) # sh in decimeters
  # Run the model and save the results
  a.aug$a[i] <-  lme(fixed = wmw ~ sh_3 -1, data = aug.mws[[as.character(years[i])]], random = ~ sh_3 -1 | tow, method="REML")$coefficients$fixed
  a.may$a[i] <-  lme(fixed = wmw ~ sh_3 -1, data = may.mws[[as.character(years[i])]], random = ~ sh_3 -1 | tow, method="REML")$coefficients$fixed
  # What is the shell height being targeted in may
  pred.dat.sb$sh[month(pred.dat.sb$date) == 5 & year(pred.dat.sb$date) == years[i]] <- 100*(pred.dat.sb$mw[month(pred.dat.sb$date) == 5 & year(pred.dat.sb$date) == years[i]] /
                                                                                    a.aug$a[a.aug$year == years[i]])^(1/3)
  pred.dat.sb$sh.lb[month(pred.dat.sb$date) == 5 & year(pred.dat.sb$date) == years[i]] <- 100*(pred.dat.sb$mw.lb[month(pred.dat.sb$date) == 5 & year(pred.dat.sb$date) == years[i]] /
                                                                                    a.aug$a[a.aug$year == years[i]])^(1/3)
  pred.dat.sb$sh.ub[month(pred.dat.sb$date) == 5 & year(pred.dat.sb$date) == years[i]] <- 100*(pred.dat.sb$mw.ub[month(pred.dat.sb$date) == 5 & year(pred.dat.sb$date) == years[i]] /
                                                                                       a.aug$a[a.aug$year == years[i]])^(1/3)
  # What is the shell height being targeted in August
  pred.dat.sb$sh[month(pred.dat.sb$date) == 8 & year(pred.dat.sb$date) == years[i]] <- 100*(pred.dat.sb$mw[month(pred.dat.sb$date) == 8 & year(pred.dat.sb$date) == years[i]] /
                                                                                    a.aug$a[a.aug$year == years[i]])^(1/3)
  pred.dat.sb$sh.lb[month(pred.dat.sb$date) == 8 & year(pred.dat.sb$date) == years[i]] <- 100*(pred.dat.sb$mw.lb[month(pred.dat.sb$date) == 8 & year(pred.dat.sb$date) == years[i]] /
                                                                                    a.aug$a[a.aug$year == years[i]])^(1/3)
  pred.dat.sb$sh.ub[month(pred.dat.sb$date) == 8 & year(pred.dat.sb$date) == years[i]] <- 100*(pred.dat.sb$mw.ub[month(pred.dat.sb$date) == 8 & year(pred.dat.sb$date) == years[i]] /
                                                                                    a.aug$a[a.aug$year == years[i]])^(1/3)
  
} # end for(i in 1:length(years))

# Now plot up the results...
lab <- as_labeller(c('5'= "May",'8' ="August"))
ggplot(pred.dat.sb[month(pred.dat.sb$date) %in% c(5,8),],aes(date,sh,colour=fleet)) + geom_point() + geom_line(size=1.5) + 
             facet_wrap(~month(date),labeller = lab) + scale_color_manual(values =c("blue","black","darkgrey"))+
             theme(text = element_text(size=16)) + theme_bw() + ylim(95,115) + ylab("Shell height") + scale_x_date(date_breaks="2 years") + xlab("")  #+
             #geom_ribbon(aes(x = date, ymax = sh.ub, ymin = sh.lb, fill = fleet, color = fleet), alpha = 0.5)
ggsave(paste0(direct,"2018/Framework/Port_sampling/sh_targeted.png"),width=11,height=8.5)

# I can also now estimate, in August, what percentage of the scallop that are being caught are smaller than 95 mm in size based on the MW-SH info and the MW's in August...
# Subset to the august data...
aug.dat <- gba.sub.dat[gba.sub.dat$month == 8,]
# Now we can see what proportion of the fleet is in each size category based on the MW-SH relationship.  Obviously this would be nicer if done spatially
# But this is a start...
for(i in 1:nrow(a.aug))
{
  aug.dat$sh[aug.dat$year == a.aug$year[i]] <- (aug.dat$meat_weight[aug.dat$year == a.aug$year[i]] /a.aug$a[a.aug$year == a.aug$year[i]]) ^(1/3) * 100
}
head(aug.dat)

# Now we can look at what proportion of these meats appear to be from scallop that are < 95 mm in size.
windows(10,10)
ggplot(aug.dat,aes(sh,fill=fleet)) + geom_histogram(binwidth=2) + facet_wrap(~year) + geom_vline(aes(xintercept=95)) + 
  theme(text = element_text(size=16)) + theme_bw() +xlab("Shell height (mm)") +
    scale_fill_manual(values =c("blue","black","darkgrey")) #+ scale_color_manual(values =c("black","black","black"))

aug.below.95.prop <- aggregate(sh ~ year + fleet, aug.dat, function(x) length(which(x < 95))/length(x))
windows(10,10)
ggplot(aug.below.95.prop, aes(year,sh,color=fleet)) + geom_line(size=1) + geom_point(size=1) + theme(text = element_text(size=16)) + theme_bw() +xlab("") + ylab("Proportion of scallop < 95 mm")+
  scale_color_manual(values =c("blue","black","darkgrey")) + scale_x_discrete(limits=c(2006:2017)) + ylim(0,0.2)
# Now based on this I think we can assume that within a fishing season the fleet tends to target roughly the same sized scallop, if so
# we can actual get a feel for how the MW-SH relationship varies over the course of a season by looking at changes in the MW's over the year.
# So for each year let's get the SH targeted as the average of the May and August surveys...
SH.target <- aggregate(sh ~ year(date) + fleet,pred.dat.sb,mean)
names(SH.target) <- c("year","fleet","sh")

mw.by.month <- aggregate(meat_weight ~ year_month + year+fleet,gba.dat,mean)
mw.by.month.sb <- aggregate(meat_weight ~ year_month + year+fleet,gba.sub.dat,mean)
names(mw.by.month) <- c("year_month","year","fleet","meat_weight")
names(mw.by.month.sb) <- c("year_month","year","fleet","meat_weight")
mw.by.month$month <- month(mw.by.month$year_month)
mw.by.month.sb$month <- month(mw.by.month.sb$year_month)
# Now we put the shell height data together with the meat weights and calculate the meat count as well...
ps.mw.sh <- merge(mw.by.month,SH.target,by =c("year","fleet"))
ps.mw.sh.sb <- merge(mw.by.month.sb,SH.target,by =c("year","fleet"))
ps.mw.sh$mc <- 500/ps.mw.sh$meat_weight
ps.mw.sh.sb$mc <- 500/ps.mw.sh.sb$meat_weight

lab <- (c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
windows(11,11)
ggplot(mw.by.month,aes(as.factor(month),meat_weight,fill=fleet)) + geom_boxplot() + theme(text = element_text(size=16)) + theme_bw() +xlab("") + ylab("Condition") + 
  scale_x_discrete(labels=c(lab))


# Doing this without a boxplot and showing the actual data in the backgroup, this is nice ggplot!!
windows(11,11)
ggplot(mw.by.month, aes(as.factor(month),meat_weight,colour=fleet)) +
  geom_point(alpha=0.2, position= position_dodge(width=0.4),size=0.5) +
  stat_summary(fun.data=mean_cl_normal, geom="errorbar",  fun.args = list(conf.int = 0.5), # The confidence interval for the error bars, nice!
               width=0.03,  alpha=0.7,mapping= aes(colour = fleet),position = position_dodge(width=0.4)) +
  stat_summary(fun.y=mean, geom="point",  size=3,mapping= aes(group = fleet),position = position_dodge(width=0.4))  + 
  theme(text = element_text(size=16)) + theme_bw() +xlab("") + ylab("Meat Weight (g)") + 
  scale_x_discrete(labels=c(lab)) + scale_color_manual(values =c("blue","black","darkgrey"))

# Same figure but excluding data within 2 months of a seedbox opening as this often artificially increases the bankwide mw
windows(11,11)
ggplot(mw.by.month.sb, aes(as.factor(month),meat_weight,colour=fleet)) +
  geom_point(alpha=0.2, position= position_dodge(width=0.4),size=0.5) +
  stat_summary(fun.data=mean_cl_normal, geom="errorbar",  fun.args = list(conf.int = 0.5), # The confidence interval for the error bars, nice!
               width=0.03,  alpha=0.7,mapping= aes(colour = fleet),position = position_dodge(width=0.4)) +
  stat_summary(fun.y=mean, geom="point",  size=3,mapping= aes(group = fleet),position = position_dodge(width=0.4))  + 
  theme(text = element_text(size=16)) + theme_bw() +xlab("") + ylab("Meat Weight (g)") + 
  scale_x_discrete(labels=c(lab)) + scale_color_manual(values =c("blue","black","darkgrey"))


# Now is this actually a good model....
# First a simple model looking for a month effect...
mw.by.month.simp <- lm(meat_weight ~ as.factor(month) ,gba.dat)
summary(mw.by.month.simp)
# Now I haven't actually looked, but how different are the fleets, ignoring year and month effects...
mw.by.fleet <- lm(meat_weight ~ fleet ,gba.dat)
summary(mw.by.fleet)
# Now is the month by fleet interaction officially interesting...
mw.by.month.fleet <- lm(meat_weight ~ as.factor(month)*fleet ,gba.dat)
summary(mw.by.month.fleet)

AIC(mw.by.month.simp,mw.by.fleet,mw.by.month.fleet) # And we see including the month-fleet interaction is very useful, despite the large number of parameters we have..


# Here we use that gba dat subset to toss out the dates within 2 months of a closure happening...
mw.by.month.simp.sb <- lm(meat_weight ~ as.factor(month) ,gba.sub.dat) 

summary(mw.by.month.simp.sb)
# Now I haven't actually looked, but how different are the fleets, ignoring year and month effects...
mw.by.fleet.sb <- lm(meat_weight ~ fleet ,gba.sub.dat)
summary(mw.by.fleet.sb)
# Now is the month by fleet interaction officially interesting...
mw.by.month.fleet.sb <- lm(meat_weight ~ as.factor(month)*fleet ,gba.sub.dat)
summary(mw.by.month.fleet.sb)
# Even with 2014 removed there is still a nice fleet by month interaction...
AIC(mw.by.month.simp.sb,mw.by.fleet.sb,mw.by.month.fleet.sb)



# So now we can pull out estimates...
mw.mod.res <- data.frame(month = as.factor(sort(rep((1:12),3))),fleet = rep(c("ASM","FT","WF"),12))
mw.mod.res$fit <- predict(mw.by.month.fleet,newdata = mw.mod.res,se.fit = T)$fit
mw.mod.res$se <- predict(mw.by.month.fleet,newdata = mw.mod.res,se.fit = T)$se.fit

# So now we can pull out estimates from the model excluding 2014 as well...
mw.mod.res.sb <- data.frame(month = as.factor(sort(rep((1:12),3))),fleet = rep(c("ASM","FT","WF"),12))
mw.mod.res.sb$fit <- predict(mw.by.month.fleet.sb,newdata = mw.mod.res.sb,se.fit = T)$fit
mw.mod.res.sb$se <- predict(mw.by.month.fleet.sb,newdata = mw.mod.res.sb,se.fit = T)$se.fit


# Plot the mw model 
lab <- (c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
windows(11,11)
ggplot(mw.mod.res, aes(month,fit,color = fleet)) + geom_point(position=position_dodge(width=0.4),size=3) + 
  geom_errorbar(aes(ymin=fit-2*se, ymax=fit+2*se,group = fleet,),width=0,position=position_dodge(width=0.4)) + 
  theme(text = element_text(size=16)) + theme_bw() +xlab("") + ylab("Meat weigth (g)") + 
  scale_x_discrete(labels=c(lab))+ scale_color_manual(values =c("blue","black","darkgrey"))

# And plot the mw model without 2014 summer in there...
lab <- (c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
windows(11,11)
ggplot(mw.mod.res.sb, aes(month,fit,color = fleet)) + geom_point(position=position_dodge(width=0.4),size=3) + 
  geom_errorbar(aes(ymin=fit-2*se, ymax=fit+2*se,group = fleet,),width=0,position=position_dodge(width=0.4)) + 
  theme(text = element_text(size=16)) + theme_bw() +xlab("") + ylab("Meat weigth (g)") + 
  scale_x_discrete(labels=c(lab))+ scale_color_manual(values =c("blue","black","darkgrey"))


# Condition here is kinda interesting as it standardizes the meat weight between the fleets, i.e. what is the
# meat weight the different fleets are picking up for a 100 mm scallop.  In theroy I wouldn't expect this to be any different...
# What the questions around condition are:
# 1:  How does condition vary throughout the year, when does condition appear to peak.
# 2:  For a 100 mm scallop, is there any difference between the fleets in terms of the size of the meat they capture.
# I'm not sure if my logic holds up here, but I think it does, hinges on the assumption that the targeted SH doesn't vary
# significantly throughout the year
ps.mw.sh$cond <- ps.mw.sh$meat_weight/(ps.mw.sh$sh/100)^3
ps.mw.sh$month <- as.factor(month(ps.mw.sh$year_month))
ps.mw.sh.sb$cond <- ps.mw.sh.sb$meat_weight/(ps.mw.sh.sb$sh/100)^3
ps.mw.sh.sb$month <- as.factor(month(ps.mw.sh.sb$year_month))


windows(11,11)
ggplot(ps.mw.sh,aes(year_month,cond,colour=fleet)) + geom_point() + geom_line() + facet_wrap(~year,scales="free")

windows(11,11)
ggplot(ps.mw.sh,aes(month,cond,group=month,colour=fleet)) + geom_boxplot() +  facet_wrap(~fleet)

windows(11,11)
ggplot(ps.mw.sh,aes(month,meat_weight,group=month,colour=fleet)) + geom_boxplot()  + facet_wrap(~fleet)

windows(11,11)
ggplot(ps.mw.sh,aes(month,mc,group=month,colour=fleet)) + geom_boxplot()  + facet_wrap(~fleet)

# So we can make some models to see if month/fleet effects...
# A condition model first, does condition differ between months, yep, looks like it
# Condition on average peaks in June is about 3 higher than in January, May, July and August all better than January (August only marginally so)
mod.cond.1 <- lm(cond~month,ps.mw.sh)
summary(mod.cond.1) # yep


mod.cond.2 <- lm(cond~fleet,ps.mw.sh)
summary(mod.cond.2) # yep

# What about if we add in fleet, doesn't look like any differences, we can't see any difference in "condition" between the fleets, this
# makes loads of sense, basically there is a size selectivity but not a quality selectivity.  I still don't know that we could see a condition difference
# between fleets given our data, I suspect we can't
mod.cond.3 <- lm(cond~month*fleet,ps.mw.sh)
summary(mod.cond.3)

# And confirmed that adding in fleet isn't helpful here, basically we can see a seasonal trend but there is no evidence for any differences by fleet.
AIC(mod.cond.1,mod.cond.2,mod.cond.3)

# Now the same model with the 2014 June and July removed....
mod.cond.1.sb <- lm(cond~month, ps.mw.sh.sb)
mod.cond.2.sb <- lm(cond~fleet, ps.mw.sh.sb)
mod.cond.3.sb <- lm(cond~month*fleet, ps.mw.sh.sb)

# And confirmed that adding in fleet isn't helpful here, basically we can see a seasonal trend but there is no evidence for any differences by fleet.
AIC(mod.cond.1.sb,mod.cond.2.sb,mod.cond.3.sb)

# Now we can make our models....
mod.cond.res <- data.frame(month = as.factor(1:12))
mod.cond.res$fit <- predict(mod.cond.1,newdata = mod.cond.res,se.fit = T)$fit
mod.cond.res$se <- predict(mod.cond.1,newdata = mod.cond.res,se.fit = T)$se.fit

mod.cond.res.sb <- data.frame(month = as.factor(1:12))
mod.cond.res.sb$fit <- predict(mod.cond.1.sb,newdata = mod.cond.res.sb,se.fit = T)$fit
mod.cond.res.sb$se <- predict(mod.cond.1.sb,newdata = mod.cond.res.sb,se.fit = T)$se.fit

# Set the theme, should really do this up top...
theme_set(theme_bw(base_size = 28))

lab <- (c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
windows(11,11)
ggplot(mod.cond.res, aes(month,fit)) + geom_point() + geom_errorbar(aes(ymin=fit-se, ymax=fit+se),width=0.1) + 
          ylim(14,20) + theme(text = element_text(size=16)) + theme_bw() +xlab("") + ylab("Condition") + 
          scale_x_discrete(labels=c(lab))+ scale_color_manual(values =c("blue","black","darkgrey"))

# Now without the 2 months post seedbox results....
lab <- (c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
windows(11,11)
ggplot(mod.cond.res.sb, aes(month,fit)) + geom_point() + geom_errorbar(aes(ymin=fit-se, ymax=fit+se),width=0.1) + 
  ylim(14,20) + xlab("") + ylab("Condition (g)") + 
  scale_x_discrete(labels=c(lab))+ scale_color_manual(values =c("blue","black","darkgrey"))

# We can pull out a time series of condition as well, this is them
cond.ts <- aggregate(cond~month+year,ps.mw.sh.sb,mean)
cond.ts$date <- ymd(paste(cond.ts$year,cond.ts$month,1,sep="-"))
windows(11,11)
# Here's the condition time series
ggplot(cond.ts, aes(date,cond)) + geom_point() + geom_line() + scale_x_date(date_breaks = "6 months")
#write.csv(cond.ts,paste0(direct,"Data/Framework/2018/Port_sampling/condition_time_series_fishery.csv"))

save.image(paste0(direct,"Data/Framework/2018/Port_sampling/Port_sampling_non_spatial_analysis_results.rData"))



###############Section Spatial Analysis###############Section Spatial Analysis###############Section Spatial Analysis###############Section Spatial Analysis###############Section Spatial Analysis
###############Section Spatial Analysis###############Section Spatial Analysis###############Section Spatial Analysis###############Section Spatial Analysis###############Section Spatial Analysis
###################  Now lets start looking at the port sampling spatially #####################
# We can skip everything above and just load in the results up to here...
load(paste0(direct,"Data/Framework/2018/Port_sampling/Port_sampling_non_spatial_analysis_results.rData"))

# Pick what you want to do here, sub-sample or just use all the data, the reason to sub-sample is the crazy amount of data we have, I simply can't get the model to run because
# it is using too much memory...
#gba.test <- sample_n(gba.dat,10000)
#gba.test <- gba.dat
# We can fold the data up to a daily average for each vessel, should reduce the data by about 90%, lose
tmp <- aggregate(cbind(meat_weight,mc) ~ boat + date,gba.dat,mean)
tmp2 <- aggregate(cbind(lon,lat,fleet,fleet_fac,year,month,day,year.cen,month.cen) ~ boat + date,gba.dat,unique)
gba.test <- cbind(tmp,tmp2[,c('lon','lat','fleet','fleet_fac',"year","month","day","year.cen","month.cen")])
#gba.test <- gba.test[gba.test$year > 2014,] # Here's a model of just the last 3 years of data...
gba.test$lon <- as.numeric(gba.test$lon)
gba.test$lat <- as.numeric(gba.test$lat)
# Optionally just look at the period before 2009 when the ASD's come onboard, if doing this run the next 2 lines as we need to convert the fleet factor to just 1's and 2's 
#gba.test <- gba.test[gba.test$year < 2010,]
#gba.test$fleet_fac <- as.numeric(gba.test$fleet_fac)-1
# Look at the last 3 years of data
#gba.test <- gba.test[gba.test$year >2 014,]

# Or look at the most recent 5 years which could be viewed as the period in which the ASD fleet has become the majority of the fishery...
#gba.test <- gba.test[gba.test$year >2012,]
# Run this if we have all 3 fleets included, if we are missing 1 fleet (i.e. before 2010), then we need to drop one of these...
gba.test$fleet_fac <- as.numeric(gba.test$fleet_fac)


#loc.ps <- cbind(gba.dat$lon,gba.dat$lat)
loc.ps <- cbind(gba.test$lon,gba.test$lat)

windows(11,11)
ScallopMap("GB",plot.boundries = T,plot.bathy = T,boundries = "offshore")
points(loc.ps,pch=19,cex=0.01)

### define a non-convex hull boundary
bound <- inla.nonconvex.hull(loc.ps, 0.1, 0.1)
prj4s <- CRS("+proj=longlat +datum=WGS84")
bound.sp <- SpatialPolygons(list(Polygons(list(Polygon(bound$loc, FALSE)), '0')), proj4=prj4s)

#######  Now we can make a mesh...

# Build the mesh, for our purposes I'm hopeful this should do the trick, the offset makes the area a bit larger so the main predictions 
# should cover our entire fishery area.  Just a mattter of playing around with the options to get a good looking mesh, 
mesh.ps <- inla.mesh.2d(loc.ps, 
                           max.edge=0.05, cutoff=0.01,
                           boundary=inla.sp2segment(bound.sp))
mesh.ps$n

# Let's take a look at what we've created so far, 
windows(11,11)
par(mfrow=c(1,1), mar=c(0,0,0,0), mgp=2:0)
plot(bound.sp, lwd=3, border=0, asp=1)
plot(mesh.ps, add=TRUE)
points(loc.ps, col="yellow", cex=0.1,pch=21)


############### Now we can make some spatial models, hopefully these don't blow up R....

A.ps <- inla.spde.make.A(mesh.ps,loc.ps)
A.ps.fleet <- inla.spde.make.A(mesh.ps,loc.ps,group = gba.test$fleet_fac)
# If we wanted to put in year here, it would be something like this I believe.
#A.ps.year <- inla.spde.make.A(mesh.ps,loc.ps,group=gba.dat$year)

# We can just make the one spde object for all of these as well.
spde.ps <- inla.spde2.pcmatern(mesh.ps,    
                                  prior.sigma=c(1,0.5), # The probabiliy that the marginal standard deviation (first number) is larger than second number, this is sigma
                                  prior.range=c(1,0.5)) # The Meidan range and the probability that the range is less than this, this is rho

#Now because I h ave a group with the A.ps.fleet piece I need to account for this with an index, "s" is the name of this spatial field, used later in the model...
# Note that the fleet index is 3 times longer than the second dimension of the A matrix without the group.
fleet.index <- inla.spde.make.index('s', n.spde=spde.ps$n.spde,
                            n.group=length(unique(gba.test$fleet_fac)))

## For the moment we can role with simply gaussian models as the data is fairly nicely distributed...
family1 = "gaussian"
family2 = "lognormal" # We need to limit the eximation to be positive, if we use the gaussian we get some 0's around the LCI for some reason...
# Note that using the lognormal family takes waaaay longer than the gaussian model!

# As soon as you make a spatial model make your own intercept.
a0 <- 1 # intercept
# Mostly just using stock priors, again fine for our purposes for the moment.  I'm not  using this
pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
# The spatial model, simple model with a intercept (overall bank average) with the spde spatial component
# basically the random deviations for each piece of the mesh.
formula1 <- y ~ 0 + a0 + f(s, model=spde.ps)
formula2 <- y ~ 0 + a0 + fleet + f(s, model=spde.ps)
formula3 <- y ~ 0 + a0 + f(s, model=spde.ps,group =s.group)
formula4 <- y ~ 0 + fleet +  f(s, model=spde.ps,group =s.group) # Does this model make sense, it gets a mean field value and then the deviations from that field, I think!
#formula5 <- y ~ 0 + fleet + year.cen + f(s, model=spde.ps,group =s.group) # Does this model make sense, it gets a mean field value and then the deviations from that field, I think!
# This is the main stack for estimation used in the model
stk.e <- inla.stack(tag="est",data=list(y = gba.test$mc, link=2L),
                       effects=list(data.frame(a0=rep(1, nrow(gba.test))), s = 1:spde.ps$n.spde),
                       A = list(1, A.ps))

stk.fleet.e <- inla.stack(tag="est.fleet",data=list(y = gba.test$mc, link=2L),
                    effects=list(data.frame(a0=rep(1, nrow(gba.test)),fleet = gba.test$fleet), 
                                 s = 1:spde.ps$n.spde),
                    A = list(1, A.ps))
# I think this stack is a model with a unique field for each fleet...
stk.fleet.field <- inla.stack(tag="est.fleet.field",data=list(y = gba.test$mc, link=2L),
                          effects=list(data.frame(a0=rep(1, nrow(gba.test))),
                                       fleet.index), #s = 1:spde.ps$n.spde),
                          A = list(1, A.ps.fleet))
# This model is a unique field for each fleet, but allows for the fleet fixed effect as well...
stk.ff2 <- inla.stack(tag="est.ff2",data=list(y = gba.test$mc, link=2L),
                              effects=list(data.frame(a0=rep(1, nrow(gba.test)),fleet = gba.test$fleet),
                                           fleet.index), #s = 1:spde.ps$n.spde),
                              A = list(1, A.ps.fleet))

# The meat count model....
mod.mc <- inla(formula1, family=family1, data = inla.stack.data(stk.e),
                  control.predictor=list(A=inla.stack.A(stk.e),link=1L, compute=T),
                  control.compute=list(cpo=TRUE))

# This model doesn't allow the shape of the random field to vary by fleet, it just scales the field by the value of fleet I believe...
mod.mc.fleet <- inla(formula2, family=family1, data = inla.stack.data(stk.fleet.e),
               control.predictor=list(A=inla.stack.A(stk.fleet.e),link=1L, compute=T),
               control.compute=list(cpo=TRUE))

# This model allows the shape of the field to vary between fleets..
mod.mc.fleet.field <- inla(formula3, family=family1, data = inla.stack.data(stk.fleet.field),
                     control.predictor=list(A=inla.stack.A(stk.fleet.field),link=1L, compute=T),
                     control.compute=list(cpo=TRUE))

# This model allows the shape of the field to vary between fleets and gives back a fixed estimate for each piece of the fleet.
mod.mc.ff2 <- inla(formula4, family=family1, data = inla.stack.data(stk.ff2),
                           control.predictor=list(A=inla.stack.A(stk.ff2),link=1L, compute=T),
                           control.compute=list(cpo=TRUE))

# What are the fixed effects estimates from the models?
summary(mod.mc)
summary(mod.mc.fleet)
summary(mod.mc.fleet.field)
summary(mod.mc.ff2)
#The value of the Kullback-Leibler divergence (KLD) describes the difference between the standard Gaussian and the Simplified Laplace Approximation (SLA) 
#for each posterior. Small values (as in the current situation) indicate that the posterior distribution
#is well approximated by a Gaussian distribution and thus there is no need to perform the more computationally intense 'full' Laplace approximation.

# Get the results of the random field, convert into proportion with inverse logit transform...
mod.mc.res <- mod.mc$summary.random$s$mean + mod.mc$summary.fixed$mean

mod.mc.fleet.res <- NULL
mod.mc.ff2.res <- NULL
fleets <- sort(unique(gba.test$fleet))
for(i in 1:length(fleets))
{
  mod.mc.fleet.res[[fleets[i]]] <- data.frame(mean.field = mod.mc.fleet$summary.random$s$mean + 
                                                                 mod.mc.fleet$summary.fixed$mean[1] + mod.mc.fleet$summary.fixed$mean[1+i],fleet = fleets[i])
  if(i ==1 ) mod.mc.ff2.res[[fleets[i]]] <- data.frame(mean.field = mod.mc.ff2$summary.random$s$mean[1:ncol(A.ps)] + 
                                              mod.mc.ff2$summary.fixed$mean[i],fleet = fleets[i])
  if(i >1 ) mod.mc.ff2.res[[fleets[i]]] <- data.frame(mean.field = mod.mc.ff2$summary.random$s$mean[((i-1)*ncol(A.ps)+1):(i*(ncol(A.ps)))] + 
                                                         mod.mc.ff2$summary.fixed$mean[i],fleet = fleets[i])
}
mod.mc.fleet.res <- do.call("rbind",mod.mc.fleet.res)
mod.mc.ff2.res <- do.call("rbind",mod.mc.ff2.res)

# This should get me the mean field for each fleet if I've done this correctly....
proj = inla.mesh.projector(mesh.ps, xlim = range(gba.test$lon) ,ylim = range(gba.test$lat), dims=c(500,500))
# SO this is the random field for each fleet...
s.mean <- lapply(1:length(fleets), function(j) {
    x <- inla.mesh.project(
    proj, field=mod.mc.fleet.field$summary.random$s$mean[
      (j-1)*spde.ps$n.spde + 1:spde.ps$n.spde])
  x
})

# SO this is the random field for each fleet...
s.ff2.mean <- lapply(1:length(fleets), function(j) {
  x <- inla.mesh.project(
    proj, field=mod.mc.ff2$summary.random$s$mean[
      (j-1)*spde.ps$n.spde + 1:spde.ps$n.spde])
  x
})

# The spatial meat cound is this random field + the intercept (a0)
field.mean  <- lapply (1:length(fleets), function(j) {s.mean[[j]] + mod.mc.fleet.field$summary.fixed$mean})
ff2.mean  <- lapply (1:length(fleets), function(j) {s.ff2.mean[[j]] + mod.mc.ff2$summary.fixed$mean[j]})

range(mod.mc.ff2$summary.random$s$mean,na.rm=T)
range(mod.mc.fleet.field$summary.random$s$mean,na.rm=T)
range(field.mean,na.rm=T)
range(ff2.mean,na.rm=T)


# Make some figures...
mins <- floor(min(range(ff2.mean,na.rm=T)))
maxs <- ceiling(max(range(ff2.mean,na.rm=T)))
col.rmp <- rev(magma(maxs-mins+1))

windows(11,11)
image.plot(list(x = proj$x, y=proj$y, z = ff2.mean[[1]]),breaks = seq(mins,maxs+1,by=1),col=col.rmp)
points(gba.test$lon[gba.test$fleet == fleets[1]],gba.test$lat[gba.test$fleet == fleets[1]],pch=19,cex=0.05,col=alpha("white",alpha=0.1))

windows(12,8)
image.plot(list(x = proj$x, y=proj$y, z = ff2.mean[[2]]),breaks = seq(mins,maxs+1,by=1),col=col.rmp)
points(gba.test$lon[gba.test$fleet == fleets[2]],gba.test$lat[gba.test$fleet == fleets[2]],pch=19,cex=0.05,col=alpha("white",alpha=0.1))

windows(12,8)
image.plot(list(x = proj$x, y=proj$y, z = ff2.mean[[3]]),breaks = seq(mins,maxs+1,by=1),col=col.rmp)
points(gba.test$lon[gba.test$fleet == fleets[3]],gba.test$lat[gba.test$fleet == fleets[3]],pch=19,cex=0.05,col=alpha("white",alpha=0.1))


# Let's zoom in on a regularly fished area and look at the differences....
windows(12,8)
image.plot(list(x = proj$x, y=proj$y, z = ff2.mean[[1]]),breaks = seq(mins,maxs+1,by=1),col=col.rmp,xlim=c(-66.9,-66.7),ylim=c(42,42.1))
points(gba.test$lon[gba.test$fleet == fleets[1]],gba.test$lat[gba.test$fleet == fleets[1]],pch=19,cex=0.05,col=alpha("white",alpha=0.1))

windows(12,8)
image.plot(list(x = proj$x, y=proj$y, z = ff2.mean[[2]]),breaks =seq(mins,maxs+1,by=1),col=col.rmp,xlim=c(-66.9,-66.7),ylim=c(42,42.1))
points(gba.test$lon[gba.test$fleet == fleets[2]],gba.test$lat[gba.test$fleet == fleets[2]],pch=19,cex=0.05,col=alpha("white",alpha=0.1))

windows(12,8)
image.plot(list(x = proj$x, y=proj$y, z = ff2.mean[[3]]),breaks = seq(mins,maxs+1,by=1),col=col.rmp,xlim=c(-66.9,-66.7),ylim=c(42,42.1))
points(gba.test$lon[gba.test$fleet == fleets[3]],gba.test$lat[gba.test$fleet == fleets[3]],pch=19,cex=0.05,col=alpha("white",alpha=0.1))




### function to extract the -sum(log(CPO)).  CPO, computed predictive order (or something)
# a cross validation method similar to leave one out, better than DIC or other measures, probability of predicting you
# data point.  A CPO of 0 means you completely miss your estiamte.  If INLA fails to compute the CPO
# That is a good warning that you don't have a good fit (I believe), underlying assumptoins aren't met.
# Lower is better for the below calculations.
#Conditional Predictive Ordinate (CPO) is the density of the observed value of yi within the out-of-sample (y???i) posterior predictive distribution. 
#A small CPO value associated with an observation suggests that this observation is unlikely (or surprising) in light of the model, priors and 
#other data in the model. In addition, the sum of the CPO values (or alternatively, the negative of the mean natural logarithm of the CPO values) 
# is a measure of fit.

fcpo <- function(m, id)
  -sum(log(m$cpo$cpo[id]), na.rm=TRUE) # Good to log this because of distributional reasons.


e.id <- inla.stack.index(stk.fleet.e, 'est.fleet')$data
f.id <- inla.stack.index(stk.e, 'est')$data
ff.id <- inla.stack.index(stk.fleet.field, 'est.fleet.field')$data
ff2.id <- inla.stack.index(stk.ff2, 'est.ff2')$data
# Comparing different models is a good way to see which model is better.
# Lower is better, thus the first model here is better than the second, and it seems as we get more complex the models improve.
c(fcpo(mod.mc.fleet, e.id),fcpo(mod.mc,f.id),fcpo(mod.mc.fleet.field,ff.id),fcpo(mod.mc.ff2,ff2.id)) # The model with the fleet as a factor is slightly worse than the fleet as a field mod.
# When running the data from before 2010 (i.e. without the ASM's, we see that the best models are the last two, they are essentially identical, Same for the data for 2013 or 2015 and beyond)
# Here is what cpo looks like, just for shits and giggles...
# Postive values would suggest the first model is better than the second model.
hist(log(mod.mc.ff2$cpo$cpo[ff2.id]) - log(mod.mc$cpo$cpo[f.id]),breaks=2000,xlim=c(-0.2,0.2))


#Probability Integral Transforms (PIT) provides a version of CPO that is calibrated to the level of the Gaussian field so that it is clearer 
#whether or not any of the values are 'small' (all values must be between 0 and 1). A histogram of PIT values that does not look approximately uniform 
#(flat) indicates a lack of model fit.
plot(mod.mc.ff2,F,F,F,F,F,F,plot.cpo=T)


# We can find all our priors in here and these can be compared to any posteriors if so desired...
mod.mc.fleet$all.hyper$predictor # The priors for the linear predictors
mod.mc.fleet$all.hyper$family # The priors for the distribution
mod.mc.fleet$all.hyper$fixed # The priors for the fixed terms are found within here
mod.mc.fleet$all.hyper$random # The priors for the random terms are found within here, this is a huge list for a spatial model!

# Here are more INLA outputs we might be interested in.  First the objects related to parameter estimation
mod.mc.fleet$names.fixed # The names of the fixed parameters
mod.mc.fleet$summary.fixed # summaries (mean, standard deviation and quantiles) of fixed posteriors (on link scale)
mod.mc.fleet$marginals.fixed	#list of posterior marginal densities of the fixed effects
mod.mc.fleet$size.random # number of levels of each of the structured random covariates
mod.mc.fleet$summary.random #  	summaries (mean, standard deviation and quantiles) of the structured random effects (smoothers etc)
mod.mc.fleet$marginals.random	# list of posterior marginal densities of the random effects, this will be a big list for a spatial model!
mod.mc.fleet$summary.hyperpar	#summaries (mean, standard deviation and quantiles) of the model hyper-parameters (precision, overdispersion, correlation etc)
mod.mc.fleet$marginals.hyperpar	#list of posterior marginal densities of the model hyper-parameters

# Next are objects related to prediction and contrasts...
mod.mc.fleet$model.matrix	#model matrix of the fixed effects - a row for each row of the input data
mod.mc.fleet$summary.linear.predictor	#summaries (mean, standard deviation and quantiles) of the linear predictor - a prediction (on the link scale) associated with each row of the input data
mod.mc.fleet$marginals.linear.predictor	#list of posterior marginal densities of the linear predictor
mod.mc.fleet$summary.fitted.values	#summaries (mean, standard deviation and quantiles) of the fitted values - a prediction (on the response scale) associated with each row of the input data
mod.mc.fleet$marginals.fitted.values	#list of posterior marginal densities of the fitted values
mod.mc.fleet$summary.lincomb	#summaries (mean, standard deviation and quantiles) of the defined linear combinations (on the link scale)
mod.mc.fleet$marginals.lincomb	#list of posterior marginal densities of the defined linear combinations
mod.mc.fleet$summary.lincomb.derived	#summaries (mean, standard deviation and quantiles) of the defined linear combinations (on the response scale)
mod.mc.fleet$marginals.lincomb.derived	#list of posterior marginal densities of the defined linear combinations (response scale)

# Finally these objects are related to model performance

mod.mc.fleet$mlik	#log marginal likelihood of the model (when mlik=TRUE)
mod.mc.fleet$dic	#deviance information criterion of the model (when dic=TRUE)
mod.mc.fleet$waic	#Watanabe-Akaike (Widely Applicable) information criterion of the model (when dic=TRUE)
mod.mc.fleet$neffp	#expected number of parameters (and standard deviation) and number of replicates per parameter in the model
mod.mc.fleet$cpo	#conditional predictive ordinate (cpo), probability integral transform (pit) and vector of assumption failures (failures) of the model (when cpo=TRUE)


# Plot the model results, I'll need to think about some of these!!
plot(mod.mc.fleet, plot.fixed.effects=T, plot.lincomb=FALSE, plot.random.effects=FALSE,
     plot.hyperparameters=FALSE, plot.predictor=FALSE, plot.q=FALSE, plot.cpo=F,
     single=FALSE)
plot(mod.mc)


#save.image(paste0(direct,"Data/Framework/2018/Port_sampling/spatial_results.RData"))
#These are the port sampling results from the 3 years before ASD vessels arrive on teh scence
#save.image(paste0(direct,"Data/Framework/2018/Port_sampling/spatial_results_2006-2009.RData"))
#save.image(paste0(direct,"Data/Framework/2018/Port_sampling/spatial_results_2013-2017.RData"))
#save.image(paste0(direct,"Data/Framework/2018/Port_sampling/spatial_results_2015-2017.RData"))
load(paste0(direct,"Data/Framework/2018/Port_sampling/spatial_results.RData"))


###################  Section 2 Visulation of models ###################  Section 2 Visulation of models ###################  Section 2 Visulation of models
######## Visualize it ########################### Visualize it ########################### Visualize it ###################

addalpha <- function(colors, alpha=1.0) 
{
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# A way to plot our data spatially. This can be used to plot everywhere or used to plot a specific location
# This only works 
Scallop.plot.field = function(field, mesh, xlim=c(-65.88, -60.13), ylim=c(45.8, 49.1), zlim = c(0,1), dims = c(50, 50),
                              clip= NULL,lvls = seq(0,1,by=0.01),add_boxes = NULL,colors = c("blue","white","yellow","darkred"),alpha = 0.8) 
{ 
  stopifnot(length(field) == mesh$n)
  proj = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
  field.proj = inla.mesh.project(proj, field)
  if(!is.null(clip)) 
  {
    pred.in <- inout(proj$lattice$loc,clip) 
    field.proj[!pred.in] <- NA
  } # end if(!is.null(clip)) 
  #windows(11,11)
  par(mar=c(4,4,1,1))
  #cols <- terrain.colors(length(lvls)-1)
  ScallopMap(xlim=xlim,ylim=ylim,plot.boundries = T)
  image(list(x = proj$x, y=proj$y, z = field.proj), xlim = xlim, ylim = ylim, zlim=zlim, axes=F,las=1,add=T, breaks=lvls, 
        axis.args=list(at=lvls,labels=round(exp(lvls))),
        col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=alpha))
  if(!is.null(add_boxes)) addPolys(add_boxes,angle = 45,density=8,col="black",lwd=.5)
  # addPolys()
} # end function

# If you don't have access to ScallopMap use this function, plots are the same, just slightly more limited on what you can do with the image plot...
Generic.plot.field = function(field, mesh, xlim=c(-65.88, -60.13), ylim=c(45.8, 49.1), zlim = c(0,1), dims = c(50, 50), trans= "none",
                              clip= NULL,lvls = seq(0,1,by=0.01),add_boxes = NULL,colors = c("blue","white","yellow","darkred"),alpha = 0.8) 
{ 
  stopifnot(length(field) == mesh$n)
  proj = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
  field.proj = inla.mesh.project(proj, field)
  #field.proj[is.na(field.proj)] <- 0
  if(!is.null(clip)) 
  {
    pred.in <- inout(proj$lattice$loc,clip) 
    field.proj[!pred.in] <- NA
  } # end if(!is.null(clip)) 
  #windows(11,11)
  if(trans== "exp") arg.list <- list(at=lvls,labels=round(exp(lvls)))
  if(trans == "none") arg.list <- list(at=lvls,labels=lvls)
  par(mar=c(4,4,1,1))
  image.plot(list(x = proj$x, y=proj$y, z = field.proj), xlim = xlim, ylim = ylim, zlim=zlim, 
             axes=F,las=1,add=F, breaks=lvls, axis.args= arg.list,
             col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=alpha))
  if(!is.null(add_boxes)) addPolys(add_boxes,angle = 45,density=8,col="black",lwd=.5)
  lines(x=c(-67.4675,-65.6992),y=c(42.51889,40.45139),lwd=3,col=magma(1,alpha=0.3,begin=0)) # EEZ b/t Can and US
  # addPolys()
} # end function



# This plots the simple model...
windows(12,8)
Generic.plot.field(mod.mc.res, mesh.ps,xlim = range(gba.dat$lon),ylim = range(gba.dat$lat),dims=c(500,500),
                   lvls=c(seq(mins,maxs+1,by=1)),colors = col.rmp)
# Add the survey locations
points(gba.test$lon[gba.test$fleet == "FT"],gba.test$lat[gba.test$fleet == "FT"],pch=19,cex=0.05,col=alpha("white",alpha=0.1))
points(gba.test$lon[gba.test$fleet == "WF"],gba.test$lat[gba.test$fleet == "WF"],pch=19,cex=0.05,col=alpha("black",alpha=0.1))
points(gba.test$lon[gba.test$fleet == "ASM"],gba.test$lat[gba.test$fleet == "ASM"],pch=19,cex=0.05,col=alpha("orange",alpha=0.1))

# Plot the meat count map by fleet...
# Make some figures...
mins <- floor(min(range(ff2.mean,na.rm=T)))
maxs <- ceiling(max(range(ff2.mean,na.rm=T)))
col.rmp <- rev(magma(maxs-mins+1))

for(i in 1:length(fleets))
{
windows(8,8)
# This little bit is to constrain the colour ramp to relevant values for each fleet...
loc.min <- floor(min(mod.mc.ff2.res$mean.field[mod.mc.ff2.res$fleet == fleets[i]]))
#loc.min = 10
pos.1 <- loc.min-mins+1
loc.max <- floor(max(mod.mc.ff2.res$mean.field[mod.mc.ff2.res$fleet == fleets[i]]))
#loc.max=55
pos.2 <- maxs-loc.max
cols <- col.rmp[(pos.1+1):(length(col.rmp)-pos.2)]
lvls <- c(seq(loc.min,(loc.max+1),by=1))
leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
              paste(lvls[length(lvls)-1],'+',sep=''))
leg.col <- addalpha(colorRampPalette(cols,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=0.8)
# If we want to add in any seedboxes that might have been opened during this period
add.boxes <- NULL
# add.boxes <- gba.boxes[gba.boxes$Open > "2006-01-01" & gba.boxes$Open < "2010-01-01",]
# Make the plot
Scallop.plot.field(mod.mc.ff2.res$mean.field[mod.mc.ff2.res$fleet == fleets[i]], mesh.ps,
                   xlim = range(gba.test$lon),ylim = range(gba.test$lat),dims=c(500,500),
                   lvls=lvls,colors =cols,add_boxes = add.boxes)
points(gba.test$lon[gba.test$fleet == fleets[i]],gba.test$lat[gba.test$fleet == fleets[i]],pch=19,cex=0.05,col=alpha("white",alpha=0.1))
legend("bottomleft",leg.lvls,fill=leg.col,border="black",pch=c(rep(NA,length(lvls))),title.adj = 0.2,
       pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n")
}


# Zooming in on the northern core portion of the bank...
for(i in 1:length(fleets))
{
  windows(12,8)
  # This little bit is to constrain the colour ramp to relevant values for each fleet...
  loc.min <- floor(min(mod.mc.ff2.res$mean.field[mod.mc.ff2.res$fleet == fleets[i]]))
  #loc.min = 10
  pos.1 <- loc.min-mins+1
  loc.max <- floor(max(mod.mc.ff2.res$mean.field[mod.mc.ff2.res$fleet == fleets[i]]))
  #loc.max=55
  pos.2 <- maxs-loc.max
  cols <- col.rmp[(pos.1+1):(length(col.rmp)-pos.2)]
  lvls <- c(seq(loc.min,(loc.max+1),by=1))
  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                paste(lvls[length(lvls)-1],'+',sep=''))
  leg.col <- addalpha(colorRampPalette(cols,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=0.8)
  # If we want to add in any seedboxes that might have been opened during this period
  add.boxes <- NULL
  # add.boxes <- gba.boxes[gba.boxes$Open > "2006-01-01" & gba.boxes$Open < "2010-01-01",]
  # Make the plot
  Scallop.plot.field(mod.mc.ff2.res$mean.field[mod.mc.ff2.res$fleet == fleets[i]], mesh.ps,
                     xlim = c(-67.2,-66.2),ylim = c(41.85,42.2),dims=c(500,500),
                     lvls=lvls,colors =cols,add_boxes = add.boxes)
  points(gba.test$lon[gba.test$fleet == fleets[i]],gba.test$lat[gba.test$fleet == fleets[i]],pch=19,cex=0.05,col=alpha("white",alpha=0.1))
  legend("bottomleft",leg.lvls,fill=leg.col,border="black",pch=c(rep(NA,length(lvls))),title.adj = 0.2,
         pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n")
}



# Here I just want to make a simple figure showing where each fleet tends to be fishing on the bank...
gba.sub <- gba.test[gba.test$year >= 2016,]
windows(11,11)
png(paste0(direct,"2018/Framework/Port_sampling/Fleet_locations_whole_bank.png"),width=11,height=8.5,units="in",res=480)
ScallopMap("GBa",plot.boundries = T)
points(gba.sub$lon[gba.sub$fleet == "WF"],gba.sub$lat[gba.sub$fleet == "WF"],pch="W",cex=0.5,col=alpha("grey",alpha=1),bg=alpha("grey",alpha=1))
points(gba.sub$lon[gba.sub$fleet == "FT"],gba.sub$lat[gba.sub$fleet == "FT"],pch="F",cex=0.5,col=alpha("red",alpha=0.5),bg=alpha("red",alpha=0.5))
points(gba.sub$lon[gba.sub$fleet == "ASM"],gba.sub$lat[gba.sub$fleet == "ASM"],pch="A",cex=0.5,col=alpha("blue",alpha=0.5),bg=alpha("blue",alpha=0.5))
dev.off()

windows(15,6)
png(paste0(direct,"2018/Framework/Port_sampling/Fleet_locations_whole_bank_post_2015_3_panel.png"),width=15,height=6,units="in",res=480)
par(mfrow=c(1,3))
ScallopMap("GBa",plot.boundries = T,title = "WF 2016-2017",cex.mn = 2)
points(gba.sub$lon[gba.sub$fleet == "WF"],gba.sub$lat[gba.sub$fleet == "WF"],pch="W",cex=0.5,col=alpha("grey",alpha=1),bg=alpha("grey",alpha=1))
addPolys(sb)
ScallopMap("GBa",plot.boundries = T,title = "FT 2016-2017",cex.mn = 2)
points(gba.sub$lon[gba.sub$fleet == "FT"],gba.sub$lat[gba.sub$fleet == "FT"],pch="F",cex=0.5,col=alpha("red",alpha=0.5),bg=alpha("red",alpha=0.5))
addPolys(sb)
ScallopMap("GBa",plot.boundries = T,,title = "ASM 2016-2017",cex.mn = 2)
points(gba.sub$lon[gba.sub$fleet == "ASM"],gba.sub$lat[gba.sub$fleet == "ASM"],pch="A",cex=0.5,col=alpha("blue",alpha=0.5),bg=alpha("blue",alpha=0.5))
addPolys(sb)
dev.off()

windows(11,8)
png(paste0(direct,"2018/Framework/Port_sampling/Fleet_locations_north.png"),width=12,height=6,units="in",res=480)
ScallopMap(xlim = c(-67.2,-66.2),ylim=c(41.8,42.2),plot.boundries = T)
points(gba.sub$lon[gba.sub$fleet == "WF"],gba.sub$lat[gba.sub$fleet == "WF"],pch="W",cex=0.5,col=alpha("grey",alpha=1),bg=alpha("grey",alpha=1))
points(gba.sub$lon[gba.sub$fleet == "FT"],gba.sub$lat[gba.sub$fleet == "FT"],pch="F",cex=0.5,col=alpha("red",alpha=0.5),bg=alpha("red",alpha=0.5))
points(gba.sub$lon[gba.sub$fleet == "ASM"],gba.sub$lat[gba.sub$fleet == "ASM"],pch="A",cex=0.5,col=alpha("blue",alpha=0.5),bg=alpha("blue",alpha=0.5))
dev.off()

windows(11,8)
png(paste0(direct,"2018/Framework/Port_sampling/WF_locations_north.png"),width=12,height=6,units="in",res=480)
ScallopMap(xlim = c(-67.2,-66.2),ylim=c(41.8,42.2),plot.boundries = T)#,title = "WF 2016-2017",cex.mn=2)
points(gba.sub$lon[gba.sub$fleet == "WF"],gba.sub$lat[gba.sub$fleet == "WF"],pch="W",cex=0.5,col=alpha("grey",alpha=1),bg=alpha("grey",alpha=1))
addPolys(sb)
dev.off()
png(paste0(direct,"2018/Framework/Port_sampling/FT_locations_north.png"),width=12,height=6,units="in",res=480)
ScallopMap(xlim = c(-67.2,-66.2),ylim=c(41.8,42.2),plot.boundries = T)#,title = "FT 2016-2017",cex.mn=2)
points(gba.sub$lon[gba.sub$fleet == "FT"],gba.sub$lat[gba.sub$fleet == "FT"],pch="F",cex=0.5,col=alpha("red",alpha=0.5),bg=alpha("red",alpha=0.5))
addPolys(sb)
dev.off()
png(paste0(direct,"2018/Framework/Port_sampling/ASM_locations_north.png"),width=12,height=6,units="in",res=480)
ScallopMap(xlim = c(-67.2,-66.2),ylim=c(41.8,42.2),plot.boundries = T)#,title = "ASM 2016-2017",cex.mn=2)
points(gba.sub$lon[gba.sub$fleet == "ASM"],gba.sub$lat[gba.sub$fleet == "ASM"],pch="A",cex=0.5,col=alpha("blue",alpha=0.5),bg=alpha("blue",alpha=0.5))
addPolys(sb)
dev.off()

windows(8,11)
png(paste0(direct,"2018/Framework/Port_sampling/Fleet_locations_northeast.png"),width=11,height=11,units="in",res=480)
ScallopMap(xlim = c(-66.6,-66.25),ylim=c(41.85,42.1),plot.boundries = T)
points(gba.sub$lon[gba.sub$fleet == "WF"],gba.sub$lat[gba.sub$fleet == "WF"],pch="W",cex=1,col=alpha("grey",alpha=1),bg=alpha("grey",alpha=1))
points(gba.sub$lon[gba.sub$fleet == "FT"],gba.sub$lat[gba.sub$fleet == "FT"],pch="F",cex=1,col=alpha("red",alpha=0.5),bg=alpha("red",alpha=0.5))
points(gba.sub$lon[gba.sub$fleet == "ASM"],gba.sub$lat[gba.sub$fleet == "ASM"],pch="A",cex=1,col=alpha("blue",alpha=0.5),bg=alpha("blue",alpha=0.5))
addPolys(sb)
dev.off()

