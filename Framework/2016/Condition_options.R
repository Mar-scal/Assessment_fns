#################################################################### DK January 2016
## This file is used to investigate how we should be approaching the calculation of condition and subsequent biomass on the bank
##  DK January 2016.
## 
# Revision History
# May 2 2016:  Completed (at least for the moment) this analysis for 2015.  The upshot, statisically the best model I was able to fit
#              was an annual model using a GAMM, a smooth on the centered shell height, and spatially accounting for location
#              using a GAMM smoother, tow was a random intercept term, and the model was a Gamma with log link.
#              Using this model all the residual trends looked very good and it seems the model had
#              nice properites.  The old model predictions were very similar to this model (at least for 2015), but statistically 
#              there were a number of issues with the residuals from the model (mostly due to using log-log relationships).
#              I was unable to fit larger models due to memory limitations (16GB ceiling was hit), possible running in LINUX
#              may be able to avoid this problem, but I am actually more interested in looking into TMB or INLA methodologies
#              to really get at the proper method we can use for all of scallop.

# Clear out everything and set up a couple dealios
rm(list=ls())
direct = "d:/r/"
# For linux
#direct = "/media/sf_data/r/"
yr = as.numeric(format(Sys.time(), "%Y")) -1

# re-load rprofile if you have one...
source(".Rprofile")
#Also load in a couple of R files that will help with plotting, these are from the TESA GAMM course by Zuur that occured in Jan 2016
# See DK to get these if you need them.  I placed the scripts in the same folder as my .Rprofile so they would be easily accessable.
source(paste(direct,"DIYBiplot.R",sep=""))
source(paste(direct,"HighstatLibV9.R",sep=""))
library(lattice)
library(gamm4)
library(akima)
library(mapplots)
library(viridis)

# First lets load the data, as per usual I will focus for the moment on Georges A as we have the best data from here...
# Going to source the survey results and do a blend of reviewing those results and doing our own analysis...

load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))
# Here's all the data I need
mw <- subset(mw.dat.all$GBa,year%in% 1982:2015)

#Now this has some super huge objects in it that we don't need and removing them
# Might just help free up enough space for the models to converge...
# THIS FUNCTION IS A CUSTOM FUNCTION I FOUND ONLINE, IF TRYING TO RUN THIS GO SEE DK!!
# This selects teh 30 largest objects in the data
lsos <- function(..., n=40) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
drp <- rownames(lsos())[rownames(lsos()) != "mw"]
rm(list = drp)



mw <- na.omit(mw)
unique(dat$year)
unique(mw$year)

# Make a couple new variables...
mw$sh.log <- log(mw$sh)
mw$wmw.log <- log(mw$wmw)
# There is one depth that is a typo, this will get fixed in new db but as of April 2016 this remains out-standing so I'll fix here..
mw$depth[mw$depth ==38] <- 68
# Center (but don't scale) the depth, centering better than standarized if we need to predict
mw$depth.cen <- scale(mw$depth,center=T,scale=F)
# Now let's center the shell height, easier for the glm models and beyond to converge.  Do not standardize though as this will 
# make it difficult to predict for other shell height
mw$sh.cen <- scale(mw$sh,center=T,scale=F)
range(mw$sh.cen)
# I believe Alan will take care of this when he gets there

#############  Section 1 Data Exploration#############  Section 1 Data Exploration#############  Section 1 Data Exploration
#############  Section 1 Data Exploration#############  Section 1 Data Exploration#############  Section 1 Data Exploration
#############  Section 1F Data Exploration#############  Section 1 Data Exploration#############  Section 1 Data Exploration

# So in here we have our meat weight and shell heights along with year and position, for the moment this is all we need to start
head(mw)

# Are there NA's?
#which(is.na(mw$wmw)) # Yep, let's toss those
#mw <- mw[-which(is.na(mw$wmw)),]

# Lets look at our sampling effort over time
samp.yr <- aggregate(wmw~year,mw,FUN=length)
#Good numbers over time, tho note the big increase in sampling in 2010 when we swtiched to every 2nd tow

range(aggregate(wmw~tow,mw,FUN=length)$wmw) # So there are at least 29 samples for a given tow ID, but up to 5193, tow alone won't
# really be a viable random term as it is repeated
samp.yr.tow <- aggregate(wmw~year/tow,mw,FUN=length) # This is more the model we'll be needing if we want to include everything in one!
# what about location?
samp.loc.tow <- aggregate(wmw~round(mw$lon,digits=1) + round(mw$lat,digits=1),mw,FUN=length)
range(round(mw$lon,digits=1))
# How does that look, this will essentially plot the location of every tow ever.  

# First we need to set up the plotting area, I hate lattice so I'm doing this my way...
# Set the number of rows
nr <- ceiling(sqrt(length(unique(mw$year))))
# Set the number of columns, the funky little command is used to add one to the nc if nr is a perfect square
# As we are printing nchains + 1
ifelse(sqrt(length(unique(mw$year))) %% 1==0,  nc <- ceiling(sqrt(length(unique(mw$year))))+1, nc <- ceiling(sqrt(length(unique(mw$year)))))
windows(15,15)
ly <- layout(matrix(c(1:(nr*nc)), nr, nc, byrow = T))
layout.show(ly)
par(mar=c(0,0,2,0))
for(i in 1:length(unique(mw$year))) 
{
  with(subset(mw,year==unique(mw$year)[i]),plot(lon,lat,pch=16,cex=0.01,xaxt="n",yaxt="n",main= unique(year),bty="U"))
} # end for(i in 1:length(unique(mw$year))) 

# Coverage looks decent, tho some interesting holes in the data and some interesting clusters, better than I had hoped to be honest!
# If we are systematically avoiding spots because there is nothing there or the because the bottom will destroy the gear we need to
# think about the effect on our survey estimates, really these areas should be removed from the survey extent should they not?

# Now the histograms of the shell heights, because the bin size is 10's for these the y-axis * 10 gives us
# the proportion within each bin.  The last time we saw a big spike in any one bin was really 2007 (about 30% were 100-110)
# Otherwise is it oddly smooth proportion of the samples in each size bin.
windows(20,15)
ly <- layout(matrix(c(1:(nr*nc)), nr, nc, byrow = T))
par(mar=c(2,1,1,2))
xmax <- ceiling(max(mw$sh)/10)*10) # This rounds the xmax to the tens place (e.g. 172 becomes 180).
for(i in 1:length(unique(mw$year))) 
{
  if(is.element(i,seq(nr,nr*nc,by=nr))==F)   
  {
    with(subset(mw,year==sort(unique(mw$year))[i]),hist(sh,xlim=c(50,xmax),cex=0.5,ylim=c(0,0.05),main="",yaxt="n",xaxt="n",freq=F))
    with(subset(mw,year==sort(unique(mw$year))[i]),text(0.9*xmax,0.8*0.05,unique(year),cex=1.5))
    axis(2,labels=F)
    axis(1,cex.axis=0.8)
  } # end if(i %in% seq(nr,nr*nc,by=nr))   
  
  if(i %in% seq(nr,nr*nc,by=nr))   
  {
    with(subset(mw,year==sort(unique(mw$year))[i]),hist(sh,xlim=c(50,xmax),cex=0.5,ylim=c(0,0.05),main="",yaxt="n",xaxt="n",freq=F))
    with(subset(mw,year==sort(unique(mw$year))[i]),text(0.9*xmax,0.8*0.05,unique(year),cex=1.5))
    axis(4)
    axis(1,cex.axis=0.8)
  } # end if(i %in% seq(nr,nr*nc,by=nr))   
  if(i == length(unique(mw$year))) axis(4)
} # end for(i in 1:length(unique(mw$year))) 

# Now a dotplot of the data looking for outliers and such...
windows(11,8.5)
Vars <- c("wmw","sh","lat","lon","depth")
Mydotplot(mw[,Vars])
# The things I notice here are mostly related to shell height
# 1 there appears to be a linear trend in the shell height in the first half of the time series, this goes away and 
# nothing really stands out since then, so right off the bat a non-linear trend with time in our shell heights.
# 2: You can kinda see the same thing in the wet meat weights as well, but the highs obscure the picture
# 3: There are a few low outliers in the shell heights, they'll have to go. (someone has removed them them)
# 4:  Given wmw is our response variable, clearly skewed high so Gaussian is not gonna work here (Gamma maybe, depending on model...)
# 5:  The lat/lon data is looking a.o.k., nothing weird in there!

# Now let's look for co-linearity in our data...
windows(11,8.5)
Vars <- c("wmw","sh","lat","lon","year","depth")

pairs(mw[,Vars],lower.panel=panel.cor)
# Depth is highly correlated with lat and lon, simpiler model would just have depth in it IMHO!
# Happily nothing else here, basically no trend with wmw or sh with lat or lon. Again maybe something with year with wmw and 
# sh but very messy Clearly wmw~sh is non-linear....
 


# let's look at the wmw and sh against year with year as a factor...
windows(11,8.5)
with(mw,boxplot(wmw~year))
with(mw,boxplot(sh~year))
# Hmm interesting, again see a difference starting in 1996 with both the mean and the variability of the data
# More noticable with the meat weight than with the shell height, in both this may actually go up to 2002 rather than 1996.

aggregate(wmw~year,mw,FUN=mean) # How does the mean change each year
plot(aggregate(wmw~year,mw,FUN=mean),main="Meat Weight mean") # Here again it's after 1996 that biggest jump happens.\
# but also notice that the jump continues to aroud 2002 or so then levels off and declines more recently.
# Notice how the mean before 1995 was never above 18 and generally in the low teens, after this the mean was rarely below 20. 
aggregate(sh~year,mw,FUN=mean) # Shell height similar story.
plot(aggregate(sh~year,mw,FUN=mean),main="Shell height mean") # Here it could be argued the change happens in early 1990's.

aggregate(wmw~year,mw,FUN=sd) # How does the variance change each year
plot(aggregate(wmw~year,mw,FUN=sd),main="Meat Weight Variance") # Here it sure looks like 1996 is the issue, but also around 2005
# Again the variance was lower when the mean was lower, 
aggregate(sh~year,mw,FUN=sd) # Here the trend doesn't seem so large until we hit 2005 or so.
plot(aggregate(sh~year,mw,FUN=sd),main="Shell Height Variance")

####################  END SECTION 1 ####################  END SECTION 1 .####################  END SECTION 1 ##########################
####################  END SECTION 1 ####################  END SECTION 1 .####################  END SECTION 1 ##########################




####################  SECTION 2 Build the models...####################  SECTION 2 Build the models...####################  SECTION 2 Build the models

# Well let's start at the start, we are looking at relationship between mw and shell height, so let's just barf it all in a model
mod.2 <- lm(wmw~sh,data=mw) # note their used to be a mod.1 when we had outliers in the data, they've been taken care of so mod.1 is gone..
# What's it look like
summary(mod.2) # shocking to see there is a relationship, and it is strong

# How are the residuals
windows(11,8.5)
par(mfrow=c(2,2))
plot(mod.2) 

# Note that we have negative fitted values, there once were outliers here but moving to the DB from flat files these
# we found and cleaned up... praise be to Alan!
# Also appears to be a variance issue in there and clearly the data are not normally distributed.
# Right now we got a hot mess Basically everything is wrong at the moment!  

E2 <- resid(mod.2, type = "pearson")
Dispersion <- sum(E2^2) / mod.2$df.res
Dispersion

# Let's move to a glm, Gamma with a log link should be a reasonable starting point
library(lme4)
library(nlme)
library(mgcv)


mod.3 <- glm(wmw ~ sh.cen,data=mw,family = Gamma(link="log"))
summary(mod.3)

# How does the model look?  
windows(11,8.5)
par(mfrow=c(2,2))
plot(mod.3)
# It sure looks a lot more reasonable now, probably nothing pathological in here, some non-linearity
# in the residuals I don't love (we under predict at high/low shell heights but it's not embarrasingly bad.)
# Variance seems not terrible either.


# Let's check out some residuals and see what we get...
E3 <- resid(mod.3, type = "pearson")
Dispersion <- sum(E3^2) / mod.3$df.res
Dispersion # Well it ain't over dispersed any more!

# Grab the model fitted values and compare to residuals

F3 <- fitted(mod.3)
# FML!!! that's a hot mess isn't it, clearly the residuals are mostly negative at higher wmw's 
# above about 60 grams there are like 5 positive residuals, variance is way higher at low values, blah to gamma...
windows(11,8.5)
par(mfrow=c(1,1))
plot(x = F3, 
     y = E3,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h=0,col="blue",lty=2)


# What is the residual trend?
EF3.gam <- gam(E3~te(F3))
summary(EF3.gam) # 10% of deviance explained by this, ugh...
windows(11,8.5)
plot(EF3.gam) #Clearly underestimating at both low and high shell heights.

#Next how do the residuals look versus shell height,while this plot was above can see the non-linearity 
# Note the the high residuals are mostly for slightly below average shell heights, so the model isn't allowing these to get large enough
# missing a covariate still?
windows(11,8.5)
plot(E3 ~ mw$sh.cen)
abline(h=0,col="blue",lty=2)

# Now plot the residuals and fitted values against covariates both in the model and not in the model.
# first up depth, something curious happening here for sure
windows(11,8.5)
par(mfrow=c(2,1))
plot(mw$depth,E3,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$depth,F3,pch=16) # Notice how all the largest fitted values are at depths in the 65-85 meter depths.
abline(h = 0, col="blue",lty = 2) 

# Let's dive a bit deeper (pun intentend..) and see how serious that trend is for the residuals
dep <- mw$depth
Res.1 <- gam(E3~s(dep))
summary(Res.1) # Explaining almost 17% of the variability, we have ourselves an important covariate!!
# Can see the big drop in the residuals with depth, clearly something interesting happening
# Need to be careful with low and high values, notice the error bars starting to 
# grow above 55 meters and below 100 meters in depth 
windows(11,8.5)
plot(Res.1)

# What about lattitude
windows(11,8.5)
par(mfrow=c(1,2))
plot(mw$lat,E3,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lat,F3,pch=16) # Notice that the large positive residuals (larger scallop than we'd expect) are mostly in the north
# and also that the variance north of 41.8 is much large (though at the most northerly bit it drops off again.)
# Really the large residuals happen between about 41.8 and 42.1 
# The largest meat weight scallops aren't necessarily in these same places, they seem to cluster more in the center of the bank 
# as opposed to be in the norrh or south
lat.res3 <- gam(E3~te(mw$lat))
summary(lat.res3) # There is a bit of something in here, deviance explained around 7%, not huge but there
plot(lat.res3) # As per the above you can see some weirdness up north, a very strong gradient in northern half
# goes from being awesome to shit, the going to shit bit happens super abruptly too.  Depth should help here for sure.


# What about longitude 
windows(11,8.5)
par(mfrow=c(1,2))
plot(mw$lon,E3,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lon,F3,pch=16) # Can see that the residuals really fall off the map on the eastern portion of the bank, for most of the western 
# portion of the bank the residuals are pretty consistent, note that west of about 66.5 it is all the northern region.
# Looks like it is east of this (as the south contributes more area) where we have that decline
# But in terms of large meat weight scallop all the big ones are found between 66.5 and 66.2
# when we include that with the lat data we see that the sweet spot for large scallop is quite clearly the northern part of the bank
lon.res3 <- gam(E3~te(mw$lon))
summary(lon.res3) # There is more than a bit of something here, can see the deviance explained with lon is 15%
plot(lon.res3) # Clear pattern as we move to the east, again residuals all postive up north then fall off steadily as we move to the east.

# Let's look at this in a bit more spatial depth... I wanted to do kriging but we have mutliple measurements at one point
# so that blows up, same problem for contour plots or anything, best solution I can think of is a real 3D scatterplot with
# all the points shown...
library(rgl)
windows(11,8.5)
plot3d(mw$lon,mw$lat,F3) # You can see the largest ones are central to the bank, both north and out, you can see the size fall off
# as we move towards the perimeter of the bank.
plot3d(mw$lon,mw$lat,E3) # Here we se the eastern edge of the bank has a lot of negative residuals, so the scallop are smaller
# Than we expect out here.  Also can see that the residuals become more negative as we move towards the eastern part of the bank
# Again it looks like the western/northern portion of the bank where there is "no" southern portion is the region where
# our residuals are all positive (i.e. our observations are bigger than expected)

# Certainly lots happening spatially which could turn out to be the way to go here, but we need to think carefully about that...
# Let's see how much accounting for the spatial variability matters to the residuals.
lat <- mw$lat
lon <- mw$lon
Res.3 <- gam(E3~te(lon,lat))
summary(Res.3) # Explaining almost 27% of the variability, that's also almost twice as much as just depth alone.
# Clearly the edges of the bank are an issue, espeically in the north.  A large swath of the bank looks very homogeneous wrt the residuals.
# Can see that the redisuals are increasing in the west tho our data is very limited down here (only a few tows).
windows(11,8.5)
plot(Res.3)
# From this we see that depth and space are both issues, but remember they are correlated with each other to a large
# extent, and space would have to be added as a GAM as well which complicates life.
# Here's another way to look at this, can see there is an issue at the edges for sure.
windows(8.5,11)
im <- with(mw,interp(lon,lat,E3,duplicate="mean"))
with(im,image(x,y,z))


### MODEL 4  ############
# Let's start by adding depth to the model. avoids needing a GAM
mod.4 <- glm(wmw ~ sh.cen+ mw$depth.cen,data=mw,family = Gamma(link="log"))
summary(mod.4)

# Compare mod.3 and mod.4
AIC(mod.3,mod.4) # clearly depth matters!!

# How does the model look?
par(mfrow=c(2,2))
plot(mod.4) # Still some weirdness in here...

# Let's check out some residuals and see what we get...
E4 <- resid(mod.4, type = "pearson")
Dispersion <- sum(E4^2) / mod.4$df.res
Dispersion # Well it ain't over dispersed any more!

# Grab the model fitted values and compare to residuals

F4 <- fitted(mod.4)
# FML!!! this remains a hot mess
par(mfrow=c(1,1))
plot(F4,E4,xlab = "Fitted values",ylab = "Pearson residuals")
# What do fitted values look like compared to what we are trying to predict...
# Well it's not a disaster, but certainly more variance as values increase...
plot(F4~mw$wmw,xlab="Fitted values",ylab="Observed Data")
abline(a=1,b=1,col="blue",lty=2)

#Next how do the residuals look versus shell height, not pretty unless you like kidney beans.
windows(11,8.5)
plot(E4 ~ mw$sh)
abline(h=0,col="blue",lty=2)

# Residuals versus depth?
windows(11,8.5)
plot(E4 ~ mw$depth) # Not bad, still can see those high values tend to be at moderate depths
abline(h=0,col="blue",lty=2)

# Residuals versus year?
windows(11,8.5)
boxplot(E4 ~ mw$year) # Looks like residuals in more recent years(1998) tend to be positive, before this they tend to be negative.
abline(h=0,lwd=2,col="blue",lty=2)
# What are fitted values versus year looking like?
windows(11,8.5)
boxplot(F4 ~ mw$year) # Looks like we are missing a trend here, and likely a non-linear one at that...


### Did this help spatially??

# What about lattitude
windows(11,8.5)
par(mfrow=c(1,2))
plot(E4~mw$lat,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lat,F4,pch=16) # If you compare with model 3 which has no depth this is much better, the residuals
lat.res4 <- gam(E4~te(mw$lat))
summary(lat.res4) # there is a little something still in here, but deviance is only 3%, less than half of what we saw before
plot(lat.res4) # The south-north issue has been almost entirely taken care of here except north of 42 degrees there is something more.

# What about longitude 
windows(11,8.5)
par(mfrow=c(1,2))
plot(mw$lon,E4,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lon,F4,pch=16) # Can't see much of a residual trend anymore, while the size falls off at the east the model 
# seems to have done an o.k. job handling this.
lon.res4 <- gam(E4~te(mw$lon))
summary(lon.res4) # The deviance explained dropped from 15% to under 3%, impressive job by depth!
plot(lon.res3) # The remaining pattern is very intereting, residuals a bit too high centrally and both east/west are lower than
# we would expect by accounting for depth.  This is a much smaller spatial effect than just depth alone perhaps
# we could get lucky and this could be some sort of year effect?

# Let's look at this in a bit more spatial depth... I wanted to do kriging but we have mutliple measurements at one point
# so that blows up, same problem for contour plots or anything, best solution I can think of is a real 3D scatterplot with
# all the points shown...
library(rgl)
plot3d(mw$lon,mw$lat,F4) # You can see the largest ones are central to the bank, both north and south, you can see the size fall off
# as we move towards the perimeter of the bank.
plot3d(mw$lon,mw$lat,E4) # Again the depth has taken care of a lot of our issues, still some dodginess around the edges but
# everything is a lot better than it was, especially again the north south issue is gone, and the east-west is really just at the edges.

# Let's see if there is much left to be gained by thinking about these residuals
lat <- mw$lat
lon <- mw$lon
Res.4 <- gam(E4~te(lon,lat))
summary(Res.4) # Still about 10% of variability left (down from 27%), so depth did pretty good.  
windows(11,8.5)
plot(Res.4)
# The residual trends left all are  mostly up at the AB-line and the northern edge of the bank.  Also seems to be
# something in the south-west but I don't think there are very many points down there so I think that's something rather ignorable.
windows(8.5,11)
im <- with(mw,interp(lon,lat,E4,duplicate="mean"))
with(im,image(x,y,z))



# Let's include year in the model, also, let's start year at 1 rather than 19XX, should be easier for model...
mw$year.rel <- mw$year -min(mw$year)
# Note that I'm treating year as a factor rather than a linear term (it'll become a random term later treated as a factor..)
mod.5 <- glm(wmw ~ sh.cen + depth.cen + as.factor(year.rel),data=mw,family = Gamma(link="log"))
summary(mod.5)

AIC(mod.4,mod.5) # Pretty solid improvment in our model.
# How does the model look?
par(mfrow=c(2,2))
plot(mod.5) # Still much of the same weirdness in the residuals unforntunatly

# Let's check out some residuals and see what we get...
E5 <- resid(mod.5, type = "pearson")
Dispersion <- sum(E5^2) / mod.5$df.res
Dispersion # Again over-dispersion not a concern with gamma I don't believe.


F5 <- fitted(mod.5)
# FML!!! that's a hot mess!!
windows(11,8.5)
par(mfrow=c(1,1))
plot(F5,E5,xlab = "Fitted values",ylab = "Pearson residuals")
# What do fitted values look like compared to what we are trying to predict...
# Well it's not a disaster, but certainly more variance as values increase...
windows(11,8.5)
plot(F5~mw$wmw,xlab="Fitted values",ylab="Observed Data")
abline(a=0,b=1,col="blue",lty=2)

#Next how do the residuals look versus shell height, not pretty...
boxplot(E5 ~ mw$sh.cen) # This really shows the underlying non-linearity in these residuals!!
# Residuals versus depth?
plot(E5 ~ mw$depth.cen) # Not bad , more variability at average depths but could be worse for sure.
# Residuals versus year?
boxplot(E5 ~ mw$year)
abline(h=0,col="blue",lty=2) # As expected no residual trends here since we treated year as a factor.

# Let's see if year helped with the spatial issues at all....
# What about lattitude
windows(11,8.5)
par(mfrow=c(1,2))
plot(E5~mw$lat,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lat,F5,pch=16) # If you compare with model 3 which has no depth this is much better, the residuals
lat.res5 <- gam(E5~te(mw$lat))
summary(lat.res5) # Almost identical, about 3% deviance
windows(11,8.5)
plot(lat.res5) # The south-north issue has been almost entirely taken care of here except north of 42 degrees there is something more.

# What about longitude 
windows(11,8.5)
par(mfrow=c(1,2))
plot(mw$lon,E5,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lon,F5,pch=16) # Can't see much of a residual trend anymore, while the size falls off at the east the model 
# seems to have done an o.k. job handling this.
lon.res5 <- gam(E5~te(mw$lon))
summary(lon.res5) # Again year didn't do anything here, patterns remain...
windows(11,8.5)
plot(lon.res5) # Same as last model.

# Let's look at this in a bit more spatial depth... I wanted to do kriging but we have mutliple measurements at one point
# so that blows up, same problem for contour plots or anything, best solution I can think of is a real 3D scatterplot with
# all the points shown...
library(rgl)
plot3d(mw$lon,mw$lat,F5) # You can see the largest ones are central to the bank, both north and south, you can see the size fall off
# as we move towards the perimeter of the bank.
plot3d(mw$lon,mw$lat,E5) # Again the depth has taken care of a lot of our issues, still some dodginess around the edges but
# everything is a lot better than it was, especially again the north south issue is gone, and the east-west is really just at the edges.

# Let's see if there is much left to be gained by thinking about these residuals
lat <- mw$lat
lon <- mw$lon
Res.5 <- gam(E5~te(lon,lat))
summary(Res.5) # Deviance actually increased a little up to 12%
windows(11,8.5)
plot(Res.5)
# Patterns again pretty much idential to model 4.
windows(8.5,11)
im <- with(mw,interp(lon,lat,E4,duplicate="mean"))
with(im,image(x,y,z))



#######################  SUMMARY SO FAR     #######################  SUMMARY SO FAR      #######################  SUMMARY SO FAR
# In the end this non-linear trend is still there.  Zuur does not like the idea of log-transformation
# as it "changes" the nature of the data but I think it's time to log transform our shell height
# as we are having serious problems with small and large shells with the current model, years/depth/spatial effects isn't gonna solve it.
# So the next step is modeling a power law relationships

# So at this point I think we have 3 potential models of interest, all of course will need to be mixed models in the end...
# 1:  a Gamma GLM with SH log transformed 
# 2:  a Gamma GAM with SH log transformed and year and depth as smooth terms
# 3:  a Gamma GAM with SH as a smooth term

# Model 1F is the Gamma GLM with SH log transformed
mod.1F <- glm(wmw ~ sh.log + depth.cen + as.factor(year),data=mw,family = Gamma(link="log"))
summary(mod.1F)

AIC(mod.1F,mod.5) # That just blew the doors of model 5.
# Some data checks...
windows(11,8.5)
par(mfrow=c(2,2))
plot(mod.1F) # Wow are those residuals loooking better!!

# Let's check out some residuals and see what we get...
E1F <- resid(mod.1F, type = "pearson")
Dispersion <- sum(E1F^2) / mod.1F$df.res
Dispersion # Again over-dispersion not a concern with gamma I don't believe.


F1F <- fitted(mod.1F)
windows(11,8.5)
plot(F1F,E1F,xlab = "Fitted values",ylab = "Pearson residuals") # Not in love yet, but again a big improvment
abline(h=0,lty=2,col="blue")
# What is the residual trend.
EF1F.gam <- gam(E1F~s(F1F))
summary(EF1F.gam) # Less than 1% of deviance explained by this model, far better than ever before 
windows(11,8.5)
plot(EF1F.gam)# trend remains, we don't do great at high/low values, but it's a lot better!
# What do fitted values look like compared to what we are trying to predict...
# Again better than anything that's come before it!
windows(11,8.5)
plot(F1F~mw$wmw,xlab="Fitted values",ylab="Observed Data")
abline(a=0,b=1,lty=2,col="blue")


#Next how do the residuals look versus shell height
windows(11,8.5)
boxplot(E1F ~ mw$sh.log) # Huge improvment here, still some issues in the tails but thi this is vastly improved.
abline(h=0,col="blue",lty=2)
# Residuals versus depth?
windows(11,8.5)
plot(E1F ~ mw$depth.cen) # Not bad
dep.cen <- mw$depth.cen
mod.dep <- gam(E1F~s(dep.cen))
summary(mod.dep) # Actually nothing really showing up here, a linear smooth for depth might be just fine, tho I will explore that a bit
# Residuals versus year? Still not brilliant, but constraints of being a linear term likely the issue here.
windows(11,8.5)
boxplot(E1F ~ mw$year)
abline(h=0,col="blue",lty=2)
# Is there ACF in these residuals? Yep, even using it as a factor it 
#looks like we still have an AR1 process here, residuals from last year is correlated with next year
# This might be tackled by simply making the year a smooth 
windows(11,8.5)
pacf(aggregate(E1F,list(mw$year),mean)$x)


# Let's see if the log shell height helped with anything
# What about lattitude
windows(11,8.5)
par(mfrow=c(1,2))
plot(E1F~mw$lat,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lat,F1F,pch=16) # If you compare with model 3 which has no depth this is much better, the residuals
lat.resF1 <- gam(E1F~te(mw$lat))
summary(lat.resF1) # Actually we've gotten a little more deviance here up to 4% using log transformed...
windows(11,8.5)
plot(lat.resF1) # The south-north issue has been almost entirely taken care of here except north of 42 degrees there is something more.

# What about longitude 
windows(11,8.5)
par(mfrow=c(1,2))
plot(mw$lon,E1F,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lon,F1F,pch=16) # No change from previous model
lon.resF1 <- gam(E1F~te(mw$lon))
summary(lon.resF1) # Again year didn't do anything here, patterns remain...
windows(11,8.5)
plot(lon.resF1) # Same as last model.

# Let's look at this in a bit more spatial depth... I wanted to do kriging but we have mutliple measurements at one point
# so that blows up, same problem for contour plots or anything, best solution I can think of is a real 3D scatterplot with
# all the points shown...
library(rgl)
plot3d(mw$lon,mw$lat,F1F) # You can see the largest ones are central to the bank, both north and south, you can see the size fall off
# as we move towards the perimeter of the bank.
plot3d(mw$lon,mw$lat,E1F) # Again the depth has taken care of a lot of our issues, still some dodginess around the edges but
# everything is a lot better than it was, especially again the north south issue is gone, and the east-west is really just at the edges.

# Let's see if there is much left to be gained by thinking about these residuals
lat <- mw$lat
lon <- mw$lon
Res.1F <- gam(E1F~te(lon,lat))
summary(Res.1F) # Deviance actually increased a little up to 12%
windows(11,8.5)
plot(Res.1F)
# Patterns again pretty much idential to model 4.
windows(8.5,11)
im <- with(mw,interp(lon,lat,E1F,duplicate="mean"))
with(im,image(x,y,z))



# O.K. so let's move to a gam and make depth a smooth term to see if that can help out along the edges.
mod.2F <- gam(wmw ~ sh.log + s(depth.cen) + as.factor(year),data=mw,family = Gamma(link="log"))
summary(mod.2F)

# The smooth actually looks really solid, this could be helpful given the non-linearity shallow and deep.
window(11,8.5) 
plot(mod.2F) 

# Using the depth as a smooth yields a nice improvment again..
AIC(mod.1F,mod.2F) 


# Check the residuals...
E2F <- resid(mod.2F, type = "pearson")
F2F <- fitted(mod.2F)
windows(11,8.5)
par(mfrow=c(1,1))
plot(F2F,E2F,xlab = "Fitted values",ylab = "Pearson residuals") # Not in love yet, but not sure we'll ever get any better.
# What do fitted values look like compared to what we are trying to predict...
# Again better than anything that's come before it!
windows(11,8.5)
plot(F2F~mw$wmw,xlab="Fitted values",ylab="Observed Data") # Again I don't mind this.


#Next how do the residuals look versus shell height, not pretty...
boxplot(E2F ~ mw$sh.log) # Similar to E1F was hoping it might clean up the tails a bit but not really.
abline(h=0,col="blue",lty=2)

# Residuals versus depth?
windows(11,8.5)
plot(E2F ~ mw$depth.cen) # Not bad, still that group of samples at really deep depths bugs me
abline(h=0,col="blue",lty=2)
dep.cen <- mw$depth.cen
mod.dep <- gam(E2F~s(dep.cen))
summary(mod.dep) # Actually nothing really showing up here, a linear smooth for depth might be just fine, tho I will explore that a bit
# Residuals versus year? Still not brilliant, but constraints of being a linear term likely the issue here.
windows(11,8.5)
boxplot(E2F ~ mw$year)
# Is there ACF in these residuals? Looks like most of this is taken care of by a smooth on depth... weird!!
windows(11,8.5)
acf(aggregate(E2F,list(mw$year),mean)$x)


############################  a COUPLe WEIRD MODELS ############################  a COUPLe WEIRD MODELS 
# If instead of doing a nice Gamma GAM, what if we did 
#1 an old school log-log tranformation and Gaussian...
#2 a gaussian with log link...
mw$wmw.log <- log(mw$wmw)# This is for model 3F
mod.3F <- gam(wmw.log ~ sh.log + depth.cen + as.factor(year),data=mw,family = gaussian(link="identity"))
summary(mod.3F)
mod.3Fa <- gam(wmw ~ sh.log + depth.cen + as.factor(year),data=mw,family = gaussian(link="log"))
summary(mod.3Fa)
# Here we see the Gamma link gives us a much nicer AIC value, so something not happy with gaussian ...
AIC(mod.2F,mod.3Fa) # we can't compare mod.3F since the response variable has been log-transformed it's very different...

# Check the residuals for the log-transformed data...
E3F <- resid(mod.3F,type="pearson")

F3F <- fitted(mod.3F)
windows(11,8.5)
par(mfrow=c(1,2))
plot(F3F,E3F,xlab = "Fitted values",ylab = "Pearson residuals") # These look better for a second, but if you put the x axis
# on the same scale as the previous model you see they are actually quite similar...
plot(exp(F3F),E3F,xlab = "Fitted values",ylab = "Pearson residuals") 
# So no advantage here, let's levae it...



# Second last non-mixed model I'll look at is making the sh a smooth rather than assuming this power function...

mod.4F <- gam(wmw ~ s(sh.cen) + s(depth.cen) + as.factor(year),data=mw,family = Gamma(link="log"))
summary(mod.4F)

AIC(mod.2F,mod.4F) # Well we do get a much lower AIC... interesting....

# What do the smooths look like...
par(mfrow=c(2,1))
plot(mod.4F)

# Now for the residuals...
E4F <- resid(mod.4F, type = "pearson")
F4F <- fitted(mod.4F)
par(mfrow=c(1,1))
windows(11,8.5)
plot(F4F,E4F,xlab = "Fitted values",ylab = "Pearson residuals") # This is better, not crazy better, but certianly an improvment
# What do fitted values look like compared to what we are trying to predict...
# Not terrible either
windows(11,8.5)
plot(F4F~mw$wmw,xlab="Fitted values",ylab="Observed Data") 


#Next how do the residuals look versus shell height
windows(11,8.5)
boxplot(E4F ~ mw$sh.cen) 
abline(h=0,col="blue",lty=2)
#Well this does get to most of this (as is should being a smooth term now) We are clearly under-estimating the weights 
# for the larger sizes with the log-log models...
# Residuals versus depth?
plot(E4F ~ mw$depth.cen) # Looks grand of course 
dep.cen <- mw$depth.cen
mod.dep <- gam(E4F~s(dep.cen))
summary(mod.dep) # Actually nothing really showing up here, a linear smooth for depth might be just fine, tho I will explore that a bit
# Residuals versus year? Still not brilliant, but constraints of being a linear term likely the issue here.
boxplot(E4F ~ mw$year)
# Is there ACF in these residuals?  Not a bit any more.
acf(aggregate(E4F,list(mw$year),mean)$x)

## Hmm... tough at this point, the best model maybe the smooth on the shell height
## But the most "biologically" model would be either the log-log or the Gamma-sh.log models.
## Let's run these 3 models but now accounting for tow within year as a random effect as we should have all along
## and see how that changes my world view...

# So One last model before getting into the mixed effects, let's see what happens if we add in a spatial term to the model
# instead of having the depth term.
# This model says we have a mw-sh relationship that has a particular shape, this shape shifts up or down
# depending on the location and year (note these are all additive effects), so I am assuming that
# the overall mw-shell height relationship is the relationship, it just wiggles around by location and year.
# 
mod.5F <- gam(wmw ~ s(sh.cen) + te(lon,lat) + as.factor(year),data=mw,family = Gamma(link="log"))
summary(mod.5F)          


AIC(mod.4F,mod.5F) # Adding the spatial component adds 15 DF but absolutely destroys the depth model in terms of AIC.


# What do the smooths look like, can see it is the edges that are interesting, this is what depth was getting at
# But I think much of the improvment comes in the A/B line and northern edge of the bank.
windows(11,8.5)
par(mfrow=c(2,1))
plot(mod.5F)

# Now for the residuals...
E5F <- resid(mod.5F, type = "pearson")
F5F <- fitted(mod.5F)
par(mfrow=c(1,1))
windows(11,8.5)
plot(F5F,E5F,xlab = "Fitted values",ylab = "Pearson residuals") # Slightly better again I think, couple larger residuals but 
# I think we do a nicer job with those high values.
# What do fitted values look like compared to what we are trying to predict...
# I think a little better yet again.
windows(11,8.5)
plot(F5F~mw$wmw,xlab="Fitted values",ylab="Observed Data") 
abline(a=0,b=1,col="blue",lty=2)


#Next how do the residuals look versus shell height
windows(11,8.5)
boxplot(E5F ~ mw$sh.cen) 
abline(h=0,col="blue",lty=2)
#Well this does get to most of this (as is should being a smooth term now) We are clearly under-estimating the weights 
# for the larger sizes with the log-log models...
# Residuals versus depth?
windows(11,8.5)
plot(E5F ~ mw$depth.cen) # Don't look to have lost anything happily.

# Residuals versus year? Still not brilliant, but constraints of being a linear term likely the issue here.
windows(11,8.5)
boxplot(E5F ~ mw$year)
# Is there ACF in these residuals?  Not a bit any more.
windows(11,8.5)
acf(aggregate(E5F,list(mw$year),mean)$x)

# Anything weird with these spatial smooths
# What about lattitude
windows(11,8.5)
par(mfrow=c(1,2))
plot(E5F~mw$lat,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lat,F5F,pch=16) # If you compare with model 3 which has no depth this is much better, the residuals
lat.res5F <- gam(E5F~te(mw$lat))
summary(lat.res5F) # Smooth has done it's job, nothing left!

# What about longitude 
windows(11,8.5)
par(mfrow=c(1,2))
plot(mw$lon,E5F,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(mw$lon,F5F,pch=16) # Nothing weird here.
lon.res5F <- gam(E5F~te(mw$lon))
summary(lon.res5F) # Again smooth has done it's job


# Let's look at this in a bit more spatial depth... I wanted to do kriging but we have mutliple measurements at one point
# so that blows up, same problem for contour plots or anything, best solution I can think of is a real 3D scatterplot with
# all the points shown...
library(rgl)
plot3d(mw$lon,mw$lat,F5F) # You can see this really frees up the data to do what it should both at the edges and in the middle.
plot3d(mw$lon,mw$lat,E5F) # Can see the outlier residual values are spread all over the place which is fine by me.

# There should be nothing here.
lat <- mw$lat
lon <- mw$lon
Res.5F <- gam(E5F~te(lon,lat))
summary(Res.5F) # As it should nothing left over
plot(Res.5)
# Looking pretty sharp now, some local clusters where residuals are interesting but the AB line and northern issue is resolved.
windows(8.5,11)
im <- with(mw,interp(lon,lat,E5F,duplicate="mean"))
with(im,image(x,y,z))

######  Again, what is this model, it is the Meat weight vs shell height relationship on George's bank, the shape
###### of this relationship is being held constant in all years and locations.


mod.5F <- gam(wmw ~ s(sh.cen) + te(lon,lat) + as.factor(year),data=mw,family = Gamma(link="log"))
# We could allow the relationship to vary by year in a fixed effect way, we are essentially setting up
# and interaction between the wmw~sh relationship by year. tensor products better for interactions
# Doesn't matter if specified as.factor(year) or not, the interaction says it doesn't care.
mod.5FI <- gam(wmw ~ te(sh.cen,year)  + te(lon,lat) ,data=mw,family = Gamma(link="log"))
# Adding in the interaction between the relationship and year is not helpful, note that 
# decline in the DF is due to the year being treated as a random term (as it is part of a smooth)
# Zuur course notes have the adjusted AIC we probably should be using for this kinda stuff.
AIC(mod.5F,mod.5FI)



#######################################  RANDOM EFFECTS MODELS ######################################################################
#######################################  RANDOM EFFECTS MODELS ######################################################################

############ THESE MODELS DO NOT CONVERGE############ THESE MODELS DO NOT CONVERGE############ THESE MODELS DO NOT CONVERGE
# There are several ways of specifying the Mixed effects terms I'm interested in exploring but using
# these methods we can't get there as these models do not converge.  These type of large models with nested random effects
# will have to wait for me to turn this into a Bayesian context or for me to get into INLA/TMB.
# First keep our model the same but add in the ID which makes every tow a random term.
mod.1R <- gamm4(wmw ~ s(sh.cen) + t2(lon,lat) + as.factor(year), random = ~ (1|ID),data=mw,family = Gamma(link="log"))
# Keep the year as a fixed term and add tow nested within year, this may be almost identical model.
mod.2R <- gamm4(wmw ~ s(sh.cen) + t2(lon,lat) + as.factor(year), random = ~ (1  |year/tow),data=mw,family = Gamma(link="log"))
# Pull out year from the main part of model and put it in as just a random term. Because I removed a fixed effect term
# I can't compare these models directly
mod.1Ry <- gamm4(wmw ~ s(sh.cen) + t2(lon,lat) , random = ~ (1  |year/tow),data=mw,family = Gamma(link="log"))
# I think this should be approximately the same model.
mod.2Ry <- gamm4(wmw ~ s(sh.cen) + t2(lon,lat) , random = ~ (1  |ID),data=mw,family = Gamma(link="log"))
# Even this model with only year as a random term dies, it doesn't converge and uses all the memory (16 GB) I got...
str <- Sys.time()
tst4 <- gamm4(wmw ~ s(sh.cen) + t2(lon,lat)  , random = ~ (1|year),data=mw,family = Gamma(link="log")) # hoping this is the model...
Sys.time() - str
############ THESE MODELS DO NOT CONVERGE############ THESE MODELS DO NOT CONVERGE############ THESE MODELS DO NOT CONVERGE



# What if I just look at the last 5 years of data, any chance this will work?
mw.2010plus <- subset(mw,year %in% seq(2010,2015,1))

# I'm still not sure if this will ever converge, if it does it takes hours.
tst4 <- gamm4(wmw ~ s(sh.cen) + t2(lon,lat)  , random = ~ (1|year),data=mw.2010plus,family = Gamma(link="log")) # hoping this is the model...

############ THESE MODELS CONVERGE############ THESE MODELS CONVERGE############ THESE MODELS CONVERGE############ THESE MODELS CONVERGE
## These models will all converge in a reasonable amout of time but each has limitations as discusses
# This model works, but the spatial information is all tied up in the tow random effect terms
# If you look into this model below you'll notice that that the spatial patterns also persist in the residuals 
# The bigger issue here is there is absolutely no predictive ability in this model to account for spatial variability
# it is swallowed up in the tow random effect, but tow 1 in 1984 is different from tow 1 in 2012 so it is essentially
# useless other than providing appropritate error bars around the mw-sh relationship.
rand.no.space <- gamm4(wmw ~ s(sh.cen)  , random = ~ (1|year/tow),data=mw,family = Gamma(link="log"))
# This model is an improvement over the above and is likely the best statistical solution to the impossible to 
# converge spatial models that we can get from a GAM.  The downside of course is we miss some of the spatial issues.
rand.depth <- gamm4(wmw ~ s(sh.cen) + s(depth.cen) , random = ~ (1|year/tow),data=mw,family = Gamma(link="log"))
# This model also works but without a depth/tow/spatial component it again isn't a good option
rand.year <- gamm4(wmw ~ s(sh.cen)  , random = ~ (1|year),data=mw,family = Gamma(link="log"))


############## THOUGHTS AT THIS POINT ############## THOUGHTS AT THIS POINT ############## THOUGHTS AT THIS POINT 
#     Essentially having tow as a random term sucks up all the spatial related variability, including
#     that along with a spatial term seems to be overspecifying the model and it has no hope of converging as the 
#     random tow term and the smooth for space seem to fight over much of the same variability.
#     I can think of two options for a full GAM model:
# 1:  Just use a random tow term, this is safe statistically as we are accounting for the samples coming from one tow
#     but we'll never get a spatial fit on top of that so there is really 0 predictive ability of a model such as that
#     Since tow number is really meaningless.
# 2:  Instead of a random tow term we drop that and use a smoother on the location.  The big advantage here is that
#     we should be able to predict on location in a model such as this.  What I'm not sure about is whether our
#     errors are correct, the smooth on location is a random term so there must be some accounting for samples coming
#     from the same location repeatedly but we don't account for any tow effects.
# 3: Another attractive option is to simply stick with a model that is based on each years data
#    so just look at 2015 data when estimating the data.  This allows for a full incorporation of the model terms without
#    running into the computational tradeoffs we see trying to fit a full random effects model.  It also get's rid
#    of old data which may no longer be informative and is collected at different spatial resolutions and with different techniques.
#    What we lose by doing this is the time series of CF values (we'll need to run the model for each year going back in time to get these)





###############################  So lets compare two annual models... ###############################  So lets compare two annual models... 

mw.2015 <- subset(mw,year==2015)

# If bored look at the tow random terms vs depth and you'll see that there is a relationship between them
# in this model since we didn't have a depth/spatial term in the model.
#m2015.t <- gamm4(wmw ~ s(sh.cen)  , random = ~ (1|tow),data=mw.2015,family = Gamma(link="log"))

# Using depth here converges relatively quickly with no issues
m2015 <- gamm4(wmw ~ s(sh.cen) + s(depth.cen) , random = ~ (1|tow),data=mw.2015,family = Gamma(link="log"))
# This still takes a while to run compared to the depth model but it converged inside of 15 minutes.
str <- Sys.time()
m2015s <- gamm4(wmw ~ s(sh.cen) + t2(lon,lat)  , random = ~ (1|tow),data=mw.2015,family = Gamma(link="log"))
Sys.time() - str

summary(m2015s$mer)
summary(m2015s$gam)
m2015s$gam$edf
str(summary(m2015s$mer))
# Save everything!!
#save.image(paste(direct,"Data/Model/2016/Framework/Condition/Model_results.Rdata",sep=""))
#load(paste(direct,"Data/Model/2016/Framework/Condition/Model_results.Rdata",sep=""))


# Comparing the models we see that the spatial model is favoured, but take a look at the bottom of this code
# and you see that for a GAMM that the AIC may not really be accurate given that the splines are random terms.
AIC(m2015$mer,m2015s$mer)

# If we want to be more conservative we can make up our own AIC correction which accounts for the complexity of the GAM's
# Look at the bottom of the code to see how the df's are calculated with a GAM and why we're doing this trick.
# Let's calculate an AIC ourselves which accounts more fully for the smoother.
# df = intercept + covariates  smoother + sigma_eps + sigma_bear
# The first part gets us the logliklihood from the mer part of the model.  
# The second part gets us the corrected degrees of freedom for this model.  The two 1's are due to sigma and sigma for the tow random effect.
# df = intercept + covariates  smoother + sigma_eps + sigma_bear
AIC.m2015s <- -2 * as.numeric(logLik(m2015s$mer)) + 2 * (sum(m2015s$gam$edf) + 1 + 1)
AIC.m2015  <- -2 * as.numeric(logLik(m2015$mer)) + 2 * (sum(m2015$gam$edf) + 1 + 1)

# So this tells us that the Spatial model is preferred
AICs <- data.frame(My_AIC = c(AIC.m2015s,AIC.m2015), edf = c((sum(m2015s$gam$edf) + 1 + 1),(sum(m2015$gam$edf) + 1 + 1)), 
                   row.names = c("Spatial model", "depth_model")) 
AICs

# Look at model results for the depth model
windows(11,8.5)
par(mfrow=c(2,1))
plot(m2015$gam, scale = FALSE, cex.lab = 1.5)  

# Grab the residuals and fitted values
E2015 <- resid(m2015$mer, type = "pearson")
F2015 <- fitted(m2015$mer)

# Now compare residuals to fitted values
windows(11,8.5)
plot(F2015,E2015,xlab = "Fitted values",ylab = "Pearson residuals") # this is fairly solid
abline(h=0,col="blue",lty=2)
windows(11,8.5)
plot(F2015~mw.2015$wmw,xlab="Fitted values",ylab="Observed Data") # Actually a bit of overestimatation for the largest meat weights now.
abline(a=0,b=1,col="blue",lty=2)

# Residuals vs shell height
windows(11,8.5)
plot(E2015~ mw.2015$sh.cen,pch=16) # Residuals against the shell height, they seem fine.
# Residuals vs depth
windows(11,8.5)
plot(E2015~ mw.2015$depth,pch=16) # Residuals vs depth, they seem good as well, I'm thinking the random effects have swallowed this all up!
# Residuals vs tow
windows(11,8.5)
boxplot(E2015~ as.numeric(mw.2015$tow),pch=16)
abline(h = 0, col="blue",lty = 2) 


# What about lattitude
# There should be nothing here.
lat <- mw.2015$lat
lon <- mw.2015$lon
windows(11,8.5)
par(mfrow=c(1,2))
plot(E2015~lat,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(lat,F2015,pch=16) # These are looking good, again the random effects have swallowed up all this residual variation
lat.res2015 <- gam(E2015~te(lat))
summary(lat.res2015) # random effects have swallowed up all the spatial effects!
windows(11,8.5)
plot(lat.res2015)

# What about longitude 
windows(11,8.5)
par(mfrow=c(1,2))
plot(lon,E2015,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(lon,F2015,pch=16) # Nothing weird here.
lon.restst <- gam(E2015~t2(lon))
summary(lon.restst) # random effects again have swallowed up all these spatial effects.
windows(11,8.5)
plot(lon.restst)

# Let's look at this in a bit more spatial depth... I wanted to do kriging but we have mutliple measurements at one point
# so that blows up, same problem for contour plots or anything, best solution I can think of is a real 3D scatterplot with
# all the points shown...
library(rgl)
plot3d(mw$lon,mw$lat,F2015) # You can see this really frees up the data to do what it should both at the edges and in the middle.
plot3d(mw$lon,mw$lat,E2015) # Can see the outlier residual values are spread all over the place which is fine by me.


Res.tst <- gam(E2015~t2(lon,lat))
summary(Res.tst) # Less than 0.5% left here
windows(8.5,11)
plot(Res.tst)
windows(11,8.5)
im <- with(mw.2015,interp(lon,lat,E2015,duplicate="mean"))
with(im,image(x,y,z)) # The edge effects may be popping back up here but residuals are tiny remember...
############################  Now look at the spatial model


# Look at model results for the spatial model
windows(11,8.5)
par(mfrow=c(2,1))
plot(m2015s$gam, scale = FALSE, cex.lab = 1.5)  

# Grab the residuals and fitted values
E2015s <- resid(m2015s$mer, type = "pearson")
F2015s <- fitted(m2015s$mer)

# Now compare residuals to fitted values
windows(11,8.5)
plot(F2015s,E2015s,xlab = "Fitted values",ylab = "Pearson residuals") # this is fairly solid
abline(h=0,col="blue",lty=2)
windows(11,8.5)
plot(F2015s~mw.2015$wmw,xlab="Fitted values",ylab="Observed Data") # Actually a bit of overestimatation for the largest meat weights now.
abline(a=0,b=1,col="blue",lty=2)

# Residuals vs shell height
windows(11,8.5)
plot(E2015s~ mw.2015$sh,pch=16) # Residuals against the shell height, they seem fine.
abline(h = 0, col="blue",lty = 2) 
sh <- mw.2015$sh
gam.mw <- gam(E2015s ~ s(sh))
summary(gam.mw) # You can't get much less deviance explained then this, so we've cleanup this up very nicely...
windows(8.5,11)
plot(gam.mw)

# Residuals vs depth
windows(11,8.5)
plot(E2015s~ mw.2015$depth,pch=16) # Residuals vs depth, they seem good as well, I'm thinking the random effects have swallowed this all up!
abline(h = 0, col="blue",lty = 2) 

# Residuals vs tow
rand.terms <- aggregate(E2015s~mw.2015$tow,FUN = mean)
tow.depths <- aggregate(mw.2015$depth~mw.2015$tow,FUN=unique)
lons <-  aggregate(mw.2015$lon~mw.2015$tow,FUN=unique)
lats <-  aggregate(mw.2015$lat~mw.2015$tow,FUN=unique)
loc <- merge(lons,lats)
rand.terms <- merge(rand.terms,tow.depths)
rand.terms <- merge(rand.terms,loc)
names(rand.terms) <- c("tow","Rand","depth","x","y")
rand.terms <- rand.terms[order(as.numeric(rand.terms$tow)),]
windows(11,8.5)
boxplot(E2015s~ as.numeric(mw.2015$tow),pch=16)
abline(h = 0, col="blue",lty = 2) 
points(rand.terms$Rand~rand.terms$tow,pch=16,col="blue")
windows(11,8.5)
acf(rand.terms$Rand,plot=T) # there is a little correlation between the random terms, not sure it's anything or not
# No relationship between the mean of the random effects and depth, the spatial component took care of that for me 
# if you don't believe me pull out the spatial (or depth) component of the model and compare those residuals!!
plot(rand.terms$Rand~rand.terms$depth)
summary(lm(rand.terms$Rand~rand.terms$depth))
abline(lm(rand.terms$Rand~rand.terms$depth),col="blue",lty=2)



# What about lattitude
# There should be nothing here.
lat <- mw.2015$lat
lon <- mw.2015$lon
windows(11,8.5)
par(mfrow=c(1,2))
plot(E2015s~lat,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(lat,F2015s,pch=16) # These are looking good, again the random effects have swallowed up all this residual variation
lat.res2015s <- gam(E2015s~te(lat))
summary(lat.res2015s) # random effects have swallowed up all the spatial effects!
windows(11,8.5)
plot(lat.res2015s)

# What about longitude 
windows(11,8.5)
par(mfrow=c(1,2))
plot(lon,E2015s,pch=16)
abline(h = 0, col="blue",lty = 2) 
plot(lon,F2015s,pch=16) # Nothing weird here.
lon.res2015s <- gam(E2015s~t2(lon))
summary(lon.res2015s) # random effects again have swallowed up all these spatial effects.
windows(11,8.5)
plot(lon.res2015s)

# Let's look at this in a bit more spatial depth... I wanted to do kriging but we have mutliple measurements at one point
# so that blows up, same problem for contour plots or anything, best solution I can think of is a real 3D scatterplot with
# all the points shown...
library(rgl)
plot3d(mw$lon,mw$lat,F2015s) # You can see this really frees up the data to do what it should both at the edges and in the middle.
plot3d(mw$lon,mw$lat,E2015s) # Can see the outlier residual values are spread all over the place which is fine by me.


Res.tst <- gam(E2015s~t2(lon,lat))
summary(Res.tst) # Less than 2% left here
windows(8.5,11)
plot(Res.tst)
# How very interesting these are, residuals are very small but you can see a curiously smooth east-west and north-south pattern.
windows(8.5,11)
im <- with(mw.2015,interp(lon,lat,E2015s,duplicate="mean"))
with(im,image(x,y,z),color.palette=topo.colors) # The edge effects may be popping back up here but residuals are tiny remember...
text(lon,lat,mw.2015$tow)



# Let's now compare with current offshore model and see if there is anything worrying in what we are doing...
# The current offshore method....
source(paste(direct,"Assessment_fns/survey_and_OSAC/shwt.lme.r",sep=""))
mod.fit<-shwt.lme(na.omit(mw),random.effect='ID',b.par=3,verbose=F)
# Convert the shell height to it's cube.
# Convert shell heights to decimeters
mw$sh<-mw$sh/100
mw$sh.con<-mw$sh^3
mw <- na.omit(mw)
# Now fit a linear model on the wmw v.s. shell height, this changes the slope of this relationship, but
# given that we have linearized the realtionship (using the sh^3) so this is basically
# Getting us the meat weight at a shell height of 100 mm (our CF)
wt.lme <- lme(fixed = wmw ~ sh.con -1, data = mw, random = ~ sh.con -1 | ID, method="REML")
# What does mod.fit look like?
summary(wt.lme)

Efit <- resid(wt.lme,type="pearson")
# Check some covariates...

FFit <- fitted(wt.lme)
fit_n_resid <- data.frame(Resid = Efit, Fit = FFit)
windows(8.5,11)
par(mfrow=c(1,1))
# There are some really high peason residual values, shouldn't be larger than about +-2...
plot(FFit,Efit,xlab = "Fitted values",ylab = "Pearson residuals")  # An Egg shape, curious.look at it with a gam the high end is terrible..
FE <- gam(Efit ~ s(FFit))
summary(FE) # Doesn't explain a ton of variance but P value is suggesting a pattern
plot(FE) # This shows what happens, it falls off a cliff about around a meat weight of 40
plot(FE,xlim=c(0,40)) # Even at the smaller sizes this is a fair bit of variability
FE.small <- gam(Resid[Resid < 40]~Fit[Resid<40], data=fit_n_resid)
summary(FE.small) # again significant model but not explaining a lot of the deviance.
#Next how do the residuals look versus shell height, not pretty...
windows(8.5,11)
boxplot(Efit ~ mw$sh.con) # Terribly underestimating the fit for all large individuals
windows(8.5,11)
boxplot(Efit ~ mw$sh) # For all shell heights larger than about 130 we are severly underestimating their values. 
windows(8.5,11)
plot(FFit~mw$wmw,xlab="Fitted values",ylab="Observed Data") # This isn't too shabby.
abline(a=0,b=1,col="blue",lty=2)

# Residuals versus depth?
# There are some pretty huge residuals in here too, these are pearson residuals which shouldn't really get much above 2...
plot(Efit ~ mw$depth.cen) 
dep.cen <- mw$depth.cen
mod.dep <- gam(Efit~s(dep.cen))
summary(mod.dep) # Actually nothing really showing up here, a linear smooth for depth might be just fine, tho I will explore that a bit
plot(mod.dep)
# Residuals versus year? OK other than huge pearson resids!!
boxplot(Efit ~ mw$year)
# In summary this is a pretty bad model with pretty bad properties and we really shouldn't be using it!!
# Is there a lat/lon trend left in here, I'd assume so but nothing frightening here to me...
library(RColorBrewer)
im <- with(mw,interp(lon,lat,Efit,duplicate="median"))
# You can see that the edges tend to be negative, the south is generally biased positive
with(im,filled.contour(x,y,z,nlevels=11,col = brewer.pal(11,"RdYlBu")))
lon <- mw$lon
lat <- mw$lat
mod.loc <- gam(Efit~te(lon,lat))
summary(mod.loc) # Again, we don't see a big residual pattern here, which seems surprising to me, is the random term sucking it up?
plot(mod.loc)

unlist(tow.depths$`mw$depth`)
# Residuals vs tow
rand.terms <- aggregate(Efit~mw$ID,FUN = mean)# There are a number of these that have multiple depths/lon/lat associated with 1 ID (Garbage!!)
tow.depths <- aggregate(mw$depth~mw$ID,FUN= mean)# First 1984 data is garbage
 
lons <-  as.data.frame(aggregate(mw$lon~mw$ID,FUN=mean))# First 1984 data is garbage
lats <-  as.data.frame(aggregate(mw$lat~mw$ID,FUN=mean))# First 1984 data is garbage
loc <- merge(lons,lats) # First 1984 data is garbage
rand.terms <- merge(rand.terms,tow.depths) # First 1984 data is garbage
rand.terms <- merge(rand.terms,loc)
names(rand.terms) <- c("ID","Rand","depth","x","y")
windows(11,8.5)
boxplot(Efit~ mw$ID,pch=16)
abline(h = 0, col="blue",lty = 2) 
points(rand.terms$Rand~rand.terms$ID,pch=16,col="blue")
windows(11,8.5)
acf(rand.terms$Rand,plot=T) # Now here we see a very strong residual pattern, As per below this isn't a year effect
# I think it is a spatial correlation
# actually in this model?  
# Can see a weird pattern with depth in the residuals...
plot(rand.terms$Rand~rand.terms$depth)
res <- (gam(Rand~s(depth),data = rand.terms))
plot(res,new=F,ylim=c(-0.5,0.5))
points(rand.terms$Rand~rand.terms$depth)
# What's up with year I wonder, not much really, a bit surprising from the ACF, so what's going on with the ACF??
boxplot(Efit~mw$year)
aggregate((Efit~mw$year),FUN=mean)

# Looking at the spatial correlation

# Get our data into a spatial class, note that we have a few duplicates in the data
# so to get this to work I just jittered the latitudes ever so slightly (if any change it is at the 5th decimal place!)
# as the kriging methods all require unique points.
Emod <- data.frame(Rand = Efit,x=mw$lon,y=mw$lat)
Emod$x <- jitter(Emod$x)

rand.terms$x <- jitter(rand.terms$x)
rand.terms <-  SpatialPointsDataFrame(cbind(rand.terms$x,rand.terms$y) ,rand.terms,proj4string = CRS("+proj=longlat +ellps=WGS84"))
Emod <-  SpatialPointsDataFrame(cbind(Emod$x,Emod$y) ,Emod,proj4string = CRS("+proj=longlat +ellps=WGS84"))
# Here is the variogram for the basic data, how does variability change with distance
# Data binned into a size of 1 which I believe is 1 km.

cvgm <- variogram(Rand ~ 1, data = rand.terms, width =1.5)
plot(cvgm,type="o")
# The estimated model results, Nugget is variability @ 0 distance (measurement error or due to sparse sampling is the theory)
#), Exp psill is the variance plateau (distance that points are kinda indepenent of each other), 
# range is the distance at which we hit the sill, seems low for this model!
efitted <- fit.variogram(cvgm,vgm(psill = 1, model = "Exp", range = 5, nugget = 0.005))
efitted

# This creates the object containing the data we need for kriging.
OK_fit <- gstat(id = "OK_fit", formula = Rand ~ 1, data = rand.terms, model = efitted)
# Cross validation of the kriding (n-fold method, or leave one out.) to get prediction error.
pe <- gstat.cv(OK_fit, debug.level = 0, random = FALSE)$residual
round(sqrt(mean(pe^2)), 4)

# Now that's all grand but I need something to predict on, let's predict on the GBa spatial grid.
bound.poly.surv <- as.PolySet(bound.poly.surv)
tst <- PolySet2SpatialLines(bound.poly.surv)
# Set up the bounding box for GBa
tst2 <- bbox(tst)
cs <- rep(-round((tst2[1] - tst2[3])/120,2),2) # 0.01 minute cells I believe this is roughly
cc <- tst2[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(tst2))/cs)  # number of cells per direction
# Now we have our grid
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
# Turn it into a correctly projected data set.
sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(tst)))
summary(sp_grd)
# Predict 
z <- predict(OK_fit, newdata = sp_grd, debug.level = 0)
sp_grd$OK_pred <- z$OK_fit.pred
sp_grd$OK_se <- sqrt(z$OK_fit.var)

# Set up the color palettes
bluepal <- colorRampPalette(c("azure1", "steelblue4"))
brks <- seq(-0.15,0.3,by=0.05)
cols <- bluepal(length(brks) - 1)
#sepal <- colorRampPalette(c("peachpuff1", "tomato3"))
#brks.se <- c(0, 240, 250, 260, 270, 280, 290, 300, 350, 400, 1000)
#cols.se <- sepal(length(brks.se) - 1)
scols <- c("green", "red")


# Again make the image and add the error terms to look for areas of poor fit.
windows(11,8.5)
image(sp_grd, "OK_pred", col = cols,breaks=brks)
symbols(coordinates(rand.terms), circles =  (abs(pe))/50, fg = "black", bg = scols[(pe < 0) + 1], inches = FALSE, add = TRUE)
plot(tst,add=T)

# We can also look at the model random terms...
rands <- data.frame(ID = as.data.frame(aggregate(mw$lon~mw$ID,FUN=mean))[,1],
                    coef = wt.lme$coefficients$random$ID[1:1109], x = as.data.frame(aggregate(mw$lon~mw$ID,FUN=mean))[,2],
                    y=as.data.frame(aggregate(mw$lat~mw$ID,FUN=mean))[,2],
                    depth = as.data.frame(aggregate(mw$depth~mw$ID,FUN=mean))[,2])
windows(11,8.5)
im <- with(rands,interp(x,y,coef,duplicate="median"))
# Here you can see all the variability in the meat weight terms, so much of the 
# spatial condition variability is found in here, but as per the above we are missing
# a north-south pattern and are ignoring any year effects as well...
with(im,image(x,y,z,nlevels=11,col = brewer.pal(11,"RdYlBu")))
plot(tst,add=T)

# Save all these results so we don't have to re-run this again!
#save.image(paste(direct,"Data/Model/2016/Framework/Condition/Model_results.Rdata",sep=""))
############################################## end SECTION 2 END SECTION 2 MODEL COMPARISONS





############################################## SECTION X PREDICTION FROM MODEL######################### SECTION X PREDICTION FROM MODEL

# From the GAMM models I believe the best way forward is using the current year spatial GAMM with shell height and spatial coordinates
# as smooths and tow as a random effect.  There are no worrying residual trends and the model fit seems excellent.
# I've saved all the above main models so we can move forward with this...
# Note that it is huge (500 MB) so will take a while to load!
load(paste(direct,"Data/Model/2016/Framework/Condition/Model_results.Rdata",sep=""))

summary(m2015s)
#diagnostics

# Make an object with the data, residuals, and fitted results
latt <- data.frame(mw.2015, res=residuals(m2015s$mer,"pearson"),fit=fitted(m2015s$mer)) #update model name

# A couple of images to look at the fit for each tow.
windows(11,8.5)
xyplot(res~fit|as.factor(tow),data=latt, xlab="fitted", ylab="residuals",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(h=0)
       })
windows(11,8.5)
xyplot(wmw~fit|as.factor(tow),data=latt, xlab="fitted", ylab="Wet Meat weight")


#Get the survey data for 2015 and non-industry tows.
Live.freq<-subset(surv.Live$GBa, year==2015 & random==1)
# Note that we need to "center" this data using the mean shell height in the mw object as that is the centering we used in the model.
df <- subset(Live.freq, select = c("ID","tow","lat","lon","l.bar"))
df$sh.cen <- df$l.bar - mean(mw.2015$sh)
# Now predict the meat weight
df$wmw <- predict(m2015s$gam,newdata=df,re.form = NULL, type="response")
# Now get the condition on the bank of a 100 mm sized scallop.
df.con <- df
df.con$l.bar <- 100
df.con$sh.cen <- df.con$l.bar - mean(mw.2015$sh)
# Now predict on the CF of a 100 mm sized scallop across the bank.
df$CF <- predict(m2015s$gam,newdata=df.con,re.form = NULL, type="response")


# So how do these predictions differ from what we had using the old methods???

# For the meat weight we see that the results look very similar, there is a bit less variability in the 
# meat weight calculated using this new method.
data.frame(MW_new = mean(df$wmw,na.rm=T),MW_old = mean(Live.freq$w.bar,na.rm=T))
data.frame(MW_new = median(df$wmw,na.rm=T),MW_old = median(Live.freq$w.bar,na.rm=T))
data.frame(MW_new = sd(df$wmw,na.rm=T),MW_old = sd(Live.freq$w.bar,na.rm=T))

# So are there any interesting visual differences between the wet meat weights?
# Looks like this new method will slightly increase low wmw's and decrease
# large wmw's
windows(11,8.5)
plot(df$wmw~Live.freq$w.bar,pch=16)
abline(a=0,b=1,lty=2,col="blue")

# What do they look like spatially, there are some differences but by and large very similar.
windows(11,8.5)
dft <- na.omit(df)
im <- with(dft,interp(lon,lat,wmw,duplicate="median"))
with(im,filled.contour(x,y,z,nlevels=11,col = magma(11)))

windows(11,8.5)
lft <- na.omit(Live.freq)
im <- with(lft,interp(lon,lat,w.bar,duplicate="median"))
with(im,filled.contour(x,y,z,nlevels=11,col = magma(11)))

# What do the differences look like??  Positives mean the new method is larger, negatives mean the old method is larger
# The new method suggests that the wet meat weight along the above and in the south are larger than
# we would have previously predicted.  Also we see a decline in the weat meat weight prediction in the 
# regions with more biomass, this could have un-expected consequences on our biomass predictions!!
windows(11,8.5)
df$diffs <- df$wmw-Live.freq$w.bar
dfd <- na.omit(df)
im <- with(dfd,interp(lon,lat,diffs,duplicate="median"))
with(im,filled.contour(x,y,z,nlevels=11,col = magma(11)))

# Let's look at a weighted mean, that doesn't change things very much here.
data.frame(MW_new = weighted.mean(df$wmw,Live.freq$com,na.rm=T),MW_old = weighted.mean(Live.freq$w.bar,Live.freq$com,na.rm=T))

# If we compare our actual biomass predictions we see very little difference
df$bm <- df$wmw*Live.freq$com
# the change is < 1% between the methods, this might get tweaked a little using the proper stratification...
cbind(BM_new = sum(df$bm,na.rm=T)/1000,BM_old = sum(Live.freq$com.bm))
windows(8.5,11)
plot(df$bm/1000 ~ Live.freq$com.bm)
abline(a=0,b=1,col="blue",lty=2)


# We see that the new prediction method results in a condition factor 0.5 higher than the old method.
# That's a realitvely large increase (3% is a fair bit if it influenced our models, but it doesn't so shouldn't really be a biggy)
# Of course right now I couldn't tell you if that increase holds year over year or not.
# The variability for the CF is less than what we saw using the other method.
data.frame(CF_new = mean(df$CF),CF_old = mean(Live.freq$CF))
data.frame(CF_new = median(df$CF),CF_old = median(Live.freq$CF))
data.frame(CF_new = sd(df$CF),CF_old = sd(Live.freq$CF))

# So are there any interesting spatial differences in this condtion calucation
windows(11,8.5)
plot(df$CF~Live.freq$CF,pch=16)
abline(a=0,b=1,lty=2,col="blue")

windows(11,8.5)
im <- with(df,interp(lon,lat,CF,duplicate="median"))
with(im,filled.contour(x,y,z,levels=seq(10,21,by=1),col = magma(12)))

windows(11,8.5)
im <- with(Live.freq,interp(lon,lat,CF,duplicate="median"))
with(im,filled.contour(x,y,z,levels=seq(10,21,by=1),col = magma(12)))

# Very similar to what we saw before, we are predicting a higher condition out in the east where there are fewer scallop
# and a lower condition in the parts of the banks where we see more individuals.
windows(11,8.5)
df$cf.diffs <-  df$CF-Live.freq$CF
dfd <- na.omit(df)
im <- with(dfd,interp(lon,lat,cf.diffs,duplicate="median"))
with(im,filled.contour(x,y,z,,levels=seq(-5,5,by=1),col = magma(12)))

# Again Let's look at a weighted mean, that doesn't change things very much here.  It actually pulls down the new
# CF and things become closer together, really not a lot of difference between the methods in the end!!
# I do like this method for calculating the CF though, we aren't predicting on a location but are using
# the survey data we have to get the CF.
data.frame(CF_new = weighted.mean(df$CF,Live.freq$com,na.rm=T),CF_old = weighted.mean(Live.freq$CF,Live.freq$com,na.rm=T))











#############################################  AIC Aside #############################################  AIC Aside #####################
#############################################  AIC Aside #############################################  AIC Aside #####################
#############################################  AIC Aside #############################################  AIC Aside #####################

########################################################
# What is the story of the AIC? How does gamm4 calculate
# the AIC? The definition of the AIC is:
#  AIC = -2 * LogLik + 2 * number of parameters
#
# We have a function for the AIC:
AIC(m2015s$mer)

# The LogLik is given by:
logLik(m2015s$mer)
# to confirm that df = 11...
df <- (as.numeric(AIC(m2015s$mer)) + 2* as.numeric(logLik(m2015s$mer)))/2
# Why 11 df?
# Answer: 1 intercept
#         1 sigma for the residuals 
#         1 sigma for the random effect of tow
#         1 sigma for the shell height smoother
#         3 sigmas for the lat-lon smoother
#         1 for the linear part of the fixed effect of the shell height smoother
#         3 for the linear parts of the fixed effects of the lat-lon smoother

# But if we look at the edf from the gam for the smoothers we see that the degrees of freedom 
# for the smoothers alone is over 12.
sum(m2015s$gam$edf)

# Why does do the smoothers not pay this bigger penalty?
# Only the linear part of the smoother is considered a fixed effect.
# So if we have a smoother that is written as a mixed model,
# then AIC considers the second part of the smoother.
# So super complex smoothers don't get fully penalized since it is a random effect
# as a random effect term...which only counts as 1 df.
# It's a random effect problem as we don't know exactly how to count DF of the random part
# of a mixed model.  There are difficult ways to calculate the degrees of freedom
# you could take these 12 df and add to the random sigmas (2, one for tows and one for residuals)
# account for the shape of the smoother, but you are making up your own AIC here so 
# need to be careful, (But this does seem to be sensible and conservative)
# Also note that edf does increase with the number of knots, but as the number of knots
# gets really high the shape of the curve doesn't change much so the edf won't change
# that much (it will increase a bit,
# but it isn't a linear thing unless the shape of the curve changes alot.)
# This is also a problem in GLMM models as the random effects only get penzlized with 1 df
# but may deserve much more of a penalty!!
-2 * logLik(M1$mer) + 10
AIC(M1$mer)

#Indeed...that is what AIC is doing.

# Justification : The model can be written as
# d13C = X * beta + Z * b + epsilon
# First column of X: only ones
# Second column of X: linear part of the smoother
# Z * b: smoother written as a random effects model.
# b ~ N(0, sigma1^2)
# epsilon  ~ N(0m sigma2^2)
# Again NOTE THIS, be careful with the AIC's here...
# So you have to take the AIC with a pinch of salt!

# The interpretation that you see online: Repeat the experiment
# and you get a different smoother (as it comes from a mixed model). 
# But in reality you will get a similar smoother!

