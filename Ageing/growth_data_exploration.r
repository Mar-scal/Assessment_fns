####### Exploring the ageing and growth data available for the inshore fishery ####################		
####### Exploring the ageing and growth data available for the inshore fishery  ####################		
####### Exploring the ageing and growth data available for the inshore fishery  ####################		

# Here, I'm looking at our inshore growth and ageing data.  I will start with the 2016 pilot project that is looking at
# moving away from an ageing program and moving towards measuring growth within a year, 
# justification is that we really don't care about the age of the scallop, we actually only care about the growth rate
# of a scallop of a certain size.  To give it an age is a somewhat dubious task...

########################################
rm(list=ls(all=T))
yr = as.numeric(format(Sys.time(), "%Y"))  #get current year

direct = "d:/r/"
#direct <- "Y:/Offshore scallop/Assessment/"

# Bring in the librarys and functions needed for this script.
library(RColorBrewer)
library(PBSmapping)
library(viridis)
library(plyr)
library(geosphere)
library(animation)
library(lattice)
library(mgcv)
source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))#source the ScallopMap script (function containing maps and parameters)

# Bring in the 2016 data, this is our test data from are 29, this is the first data that was analyzed so much to be learned from this!

dat <- read.csv(paste(direct,"Data/Ageing/incremental_growth_inshore_2016_29_3_6.csv",sep=""),check.names=F) # This data comes from the xlsx (DRAFTageingSFA292016.xlsx)
# Take a look at the data
head(dat)
names <- c("LY",as.character(na.omit(as.numeric(names(dat))))) #
#
# I need to get a unique tow ID, so combine Tow number and area
dat$Tow_ID <- paste(dat$Cruise,dat$Tow_No,sep="_")
# Anything interesting with the current year
boxplot(dat$LY~dat$Tow_ID) # Most of our scallop are currently in the 100-150 size range, looks like the GM samples have smaller scallop.
hist(dat$LY) # Perhaps a truncated normal distribution, we seem to be missing scallop over 150 mm, could also be a uniform, given our
# sampling techniques this would make some sense, we pick a few from every size bin available to get a decent represenation of what is out
# there, but this of course means that we are under-sampling the common sizes and over sampling the rare sizes, really this will
# end up being kind of a truncated uniform, common sizes will have roughly the same sample size, the rare sizes will show up much less.
plot(dat$LY~dat$"2015") ; abline(a=0,b=1,lty=2)
plot(dat$"2015"~dat$"2014") ; abline(a=0,b=1,lty=2)# So from last years data I can be quite confident in what the size next year will be
# no surprise really, on this scale the chipping of the shells in the final measurements is pretty minor, I think that will 
# change when I look at the difference between LY and 2015.  
plot(dat$LY~dat$"2010") # The size of the scallop in 2010 is pretty informative about the current size as well
plot(dat$LY~dat$"2008")# But this weakens as we go back in time, I think this is due to the scallop growth slowing at large sizes
# when we go back too far the scallop remaining are old and large and don't grow much
plot(dat$LY~dat$"2006") # Can see the relationship is almost entirely lost when you go back around decade, these scallop there is
# a decline in growth rate for sure at around 120 mm, and at around 135 mm growth is very slow.  120 is first brake perhaps
# where growth rates really start to get more variable, some grow very slowly above this, others continue to grow more rapidly.
# Likely a function of enviro and genetics.

# Now what about growth, how to represent this, there are a few ways we can look at this I'm sure.
#1:  We can look at it annually, for example take the 2014 size and see how much bigger they were in 2015
#2:  We can look at everything, find the growth in all years and compare these in one big picture
#3:  Other ideas!?!?

# For any idea we need to get the growth in a given year.  For area 29 the survey happens in September so we can assume most of the 
# growth happened in the most recent year, e.g. if I take the difference between 2015 and 2014 I will assume that the growth occured in 2015.

# Let's get the growth data
# First identify the "LY" row, this is the first row with difference data we want so is where we will start the calculations...
start.row <- which(names(dat) =="LY")
# Find the last row with years in it, hopefully we don't name any other columns with somethign that could be numeric or this breaks
end.row <- which(names(dat) == min(as.numeric(names(dat)),na.rm=T))
index <- 0 # and index for putting the data into new columns
# Run the loop stop at the second last year as we can't calculate growth for the last year of data.
for(i in start.row:(end.row-1))
{
  index <- 1+index # update the index
  nam <- paste("G",names(dat[i]),sep="") # get a good column name for the growth data
  dat[,(end.row+index)] <- dat[,i] - dat[,(i+1)] # calcuate the difference
  names(dat)[end.row+index] <- nam # and rename the new data column.
} # end for(i in start.row:(end.row-1))



# DK Note:  I wonder if thinkinga about growth in terms of annual growth rings is actually more sensible?  We will be missing volume growth
# for the thickening of the shell, but wonder if something... very roughly, assuming a Scallop shell is a semi-circle and only including
# growth of the top shell...
# Size in 2014 = 85 mm
# Size in 2015 = 100 mm
# Size in 2016 = 105 mm
# Area growth in 2015 is (pie*100^2 - pie* 85^2) /2 = 
g.area.2015 <- 0.5* (pi*10^2 - pi* 8.5^2)
g.area.2016 <- 0.5* (pi*10.5^2 - pi* 10^2)
#index <- 0
# Now run the same loop but do this rough area calculation...
for(i in start.row:(end.row-1))
{
  index <- 1+index # update the index
  nam <- paste("Area",names(dat[i]),sep="") # get a good column name for the growth data
  dat[,(end.row+index)] <- 0.5* (pi*(dat[,i]/10)^2 - pi*(dat[,(i+1)]/10)^2) # calcuate the difference
  names(dat)[end.row+index] <- nam # and rename the new data column.
} # end for(i in start.row:(end.row-1))


# 

plot(dat$Area2015 ~ dat$"2014")
shp <- gam(dat$Area2015 ~ s(dat$Area2014))

#plot(tst$Area2015 ~ tst$G2015)

# Now I'm going to convert the dat into a long form so that we have "factors" and data is nice to analyze.
growth.names <- names(dat)[grep("G",names(dat))]
area.names <- names(dat)[grep("Area",names(dat))]
dat.long <- reshape(dat,direction="long",varying=list(names),times=names)
dat.long <- dat.long[,-which(names(dat.long) %in% growth.names)]
dat.g.long <- reshape(dat,direction="long",varying=list(growth.names),times=growth.names)

# I need to add in the final year of "data" which we can't have for the growth data, i.e add in a bunch of NA's for growth in the first year
# we have information for.
dat.long$GLY <- c(dat.g.long$GLY,rep(NA,nrow(dat)))
dat.long <- dat.long[,-which(names(dat.long)=="id")]
names(dat.long) <- c(names(dat.long[1:(which(names(dat.long) =="time")-1)]),"year","size","growth")
# Now we can track growth...
# These are the same so I'm pretty confident I didn't screw anything up reshapeing the data happily...
#plot(dat.long$GLY[dat.long$time=="2002"]~dat.long$LY[dat.long$time==2001])
#plot(dat$"G2002"~dat$"2001")
# Let's only look at shells above 40 mm 
dat.long <- dat.long[dat.long$size >=40,]

# Remove the LY data as we know it has chips and doesn't represent a full year of growth.
# And let's look by area, can see that 29A,3,and 6 are very similar, if anything 6 seems to have more low growth individuals that 29
windows(11,8.5)
plot(growth~size,dat.long[dat.long$SPA=="29A" & dat.long$year != "LY",],pch=19,col="blue",cex=0.5,ylim=c(0,35),xlim=c(40,170))
points(growth~size,dat.long[dat.long$SPA=="3" & dat.long$year != "LY",],pch=21,bg="red",cex=0.5)
points(growth~size,dat.long[dat.long$SPA=="6" & dat.long$year != "LY",],pch=22,bg="orange",cex=0.5)

#points(growth~size,dat.long[dat.long$year != "LY",],pch=19,cex=0.5)


#####################  AREA ################AREA ############################  AREA #######################
#####################  AREA ################AREA ############################  AREA #######################
# From here out we'll use the dat.long data
areas <- sort(unique(dat.long$SPA))

dat.long$std_size <- scale(dat.long$size,center=T,scale=F)
size_range <- floor(min(dat.long$std_size,na.rm=T)-10):ceiling(max(dat.long$std_size,na.rm=T))+10
pred.dat <- data.frame(std_size = rep(size_range,length(areas)),
                       SPA=sort(rep(areas,length(size_range)))) # Some data to predict on for the gam...
windows(11,11)
for(i in 1:length(areas))
{
  tmp <- dat.long[dat.long$SPA == areas[i],]
  if(i == 1) 
  {
    plot(growth~std_size,tmp,pch = 19,cex=0.5,ylim=range(dat.long$growth,na.rm=T),xlim=range(dat.long$std_size,na.rm=T),xaxt="n")
    if(i == 1) axis(1,at =seq(0,170,by=10)-mean(dat.long$size,na.rm=T),labels=seq(0,170,10))
    legend("topright",legend = areas,col= plasma(length(areas)),pch=19,lty=1:length(areas), bty="n")
    mod.res <- gam(growth~t2(std_size,by=SPA),data=dat.long,family = "poisson")
    preds <- predict(mod.res,pred.dat,type="response",se=T)
  } # end if(i == 1) 
  if(i > 1) points(growth~std_size,tmp,pch=19,col=plasma(length(areas))[i],cex=0.5)
  lines(preds$fit[pred.dat$SPA==areas[i]]~pred.dat$std_size[pred.dat$SPA==areas[i]],col=plasma(length(areas))[i],lty=i,lwd=2)
  lines((preds$fit[pred.dat$SPA==areas[i]]+ preds$se[pred.dat$SPA==areas[i]])~pred.dat$std_size[pred.dat$SPA==areas[i]],
        col=plasma(length(areas))[i],lty=i,lwd=0.5)
  lines((preds$fit[pred.dat$SPA==areas[i]]- preds$se[pred.dat$SPA==areas[i]])~pred.dat$std_size[pred.dat$SPA==areas[i]],
        col=plasma(length(areas))[i],lty=i,lwd=0.5)  
}# end for(i in 1:length(areas))

# Check out the model residuals
windows(11,11)
plot(residuals(mod.res)~fitted.values(mod.res))
windows(11,11)
plot(na.omit(dat.long$growth)~fitted.values(mod.res),xlab="fitted","ylab"="Observed")
abline(a=0,b=1,col="blue",lty=2,lwd=2)
#### How bad is the over-dispersion, it's not terrible at all really, 
E.mod.1 <- resid(mod.res)
Dispersion <- sum(E.mod.1^2) / mod.res$df.res
Dispersion

## That's not a half bad model to start with



#####################  YEAR ################YEAR ############################  YEAR #######################
#####################  YEAR ################YEAR ############################  YEAR #######################
## What if instead we look by year??
# Now subset the data so the model only runs for years in which we have at least min.dat points.
min.dat <- 50
useable.data <- aggregate(std_size~year,dat.long,FUN=function(x) which(length(x) >=min.dat))
useable.data <- na.omit(useable.data$year[useable.data$V1==1])
dat.mod <- dat.long[dat.long$year %in% useable.data,]
# Get the number of years we have data
years <- sort(unique(dat.mod$year))
# Some data to predict on for the gam...
pred.dat <- data.frame(std_size = rep(size_range,length(years)),year=sort(rep(years,length(size_range)))) 
# Make year a factor, needed for the by = in the GAM
dat.mod$year <- as.factor(dat.mod$year)


# Now the model, GAM on size with year as a factor.
mod.res <- gam(growth~t2(std_size,by=year),data=dat.mod,family = "poisson")
# Grab the model predictions.
preds <- predict(mod.res,pred.dat,type="response",se=T)

# Run the loop to generate image.  
windows(11,11)
index <- 0
for(i in 1:length(years))
{
  tmp <- dat.long[dat.long$year == years[i],]
    index = index+1
    if(index ==1) 
    {
      plot(growth~std_size,tmp,pch = 19,cex=0.5,ylim=range(dat.long$growth,na.rm=T),xlim=range(dat.long$std_size,na.rm=T),xaxt="n")
      #rug(dat.mod$std_size,ticksize=0.01)
      if(index ==1) axis(1,at =seq(0,170,by=10)-mean(dat.long$size,na.rm=T),labels=seq(0,170,10))
      legend("topright",legend = years,col= plasma(length(years)),pch=19,lty=1:length(years), bty="n")
    } # end if(i == 1) 
    if(index > 1) points(growth~std_size,tmp,pch=19,col=plasma(length(years))[i],cex=0.5)
    lines(preds$fit[pred.dat$year==years[i]]~pred.dat$std_size[pred.dat$year==years[i]],col=plasma(length(years))[i],lty=i,lwd=2)
    lines((preds$fit[pred.dat$year==years[i]]+ preds$se[pred.dat$year==years[i]])~pred.dat$std_size[pred.dat$year==years[i]],
          col=plasma(length(years))[i],lty=i,lwd=0.5)
    lines((preds$fit[pred.dat$year==years[i]]- preds$se[pred.dat$year==years[i]])~pred.dat$std_size[pred.dat$year==years[i]],
          col=plasma(length(years))[i],lty=i,lwd=0.5)  
}# end for(i in 1:length(years))



#####################  YEAR BY AREA ################YEAR BY AREA ############################  YEAR BY AREA #######################
#####################  YEAR BY AREA ################YEAR BY AREA ############################  YEAR BY AREA #######################

# Do we have enough data to look at a year by area effect?
dat.long$year_area <- paste(dat.long$year,dat.long$SPA,sep="_")
# Now subset the data so the model only runs for area x year in which we have at least min.dat points.
min.dat <- 20
useable.data <- aggregate(std_size~year_area,dat.long,FUN=function(x) which(length(x) >= min.dat))
useable.data <- na.omit(useable.data$year_area[useable.data$V1==1])
useable.data <- useable.data[-grep("LY",useable.data)] # Remove the LY as that data is already shown to be unhelpful given they are all chipped...
# Subset the data into those with more than min.dat area
dat.mod <- dat.long[dat.long$year_area %in% useable.data,]
year.area <- sort(unique(dat.mod$year_area)) # All the year-area combos possible.

# Make year_area a factor, needed for the by = in the GAM
dat.mod$year_area <- as.factor(dat.mod$year_area)
# The data to predict on
pred.dat <- data.frame(std_size = rep(size_range,length(year.area)),year_area=sort(rep(year.area,length(size_range)))) 


# Now the model, GAM on size for each year in each area, anything of interest
mod.res <- gam(growth~t2(std_size,by=year_area),data=dat.mod,family = "poisson")
preds <- predict(mod.res,pred.dat,type="response",se=T)

# Run the loop to generate image and model results.  What I'm finding more and more surprising is the similarity of the results.
# even when just looking at a area by year subset the models are for the most part very similar, a few weird things
# happening below 70 mm, but I'm guessing those are being driven by very few points.  Between 70 and 160 the models
# are really converging nicely on a relationship.  Looks like no matter what year or area I'd look at I'd roughly get the same story


windows(11,11)
index <- 0
for(i in 1:length(year.area))
{
  tmp <- dat.long[dat.long$year_area == year.area[i],] 
    index = index+1
    if(index ==1) 
    {
      plot(growth~std_size,tmp,pch = 19,cex=0.5,ylim=range(dat.long$growth,na.rm=T),xlim=range(dat.long$std_size,na.rm=T),xaxt="n")
      #rug(dat.mod$std_size,ticksize=0.01)
      if(index ==1) axis(1,at =seq(0,170,by=10)-mean(dat.long$size,na.rm=T),labels=seq(0,170,10))
      legend("topright",legend = year.area,col= plasma(length(year.area)),pch=19,lty=1:length(year.area), bty="n",ncol=3,cex=0.6)

    } # end if(i == 1) 
    if(index > 1) points(growth~std_size,tmp,pch=19,col=plasma(length(year.area))[i],cex=0.5)
    lines(preds$fit[pred.dat$year_area==year.area[i]]~pred.dat$std_size[pred.dat$year_area==year.area[i]],
          col=plasma(length(year.area))[i],lty=i,lwd=2)
    #lines((preds$fit[pred.dat$year_area==year.area[i]]+ preds$se[pred.dat$year_area==year.area[i]])~
    #        pred.dat$std_size[pred.dat$year_area==year.area[i]], col=plasma(length(year.area))[i],lty=i,lwd=0.5)
    #lines((preds$fit[pred.dat$year_area==year.area[i]]- preds$se[pred.dat$year_area==year.area[i]])~
    #        pred.dat$std_size[pred.dat$year_area==year.area[i]], col=plasma(length(year.area))[i],lty=i,lwd=0.5)  
}# end for(i in 1:length(year.area))


#####################################  Now the real model, we've played around but we need to include the TOW random effect bit #######################
#####################################  That should help with the overdispersion in the model ##########################################################

# Random effects mod, using gamm4, lmer structure which I'm used to and should do the trick for this...
# as I need to be able to do a generalized model.  Note that I'm only varying the intercept of the gam by individual
# assuming the overall shape would be unchanged...
library(gamm4) 


# And we also need to account for multiple readings from one individual.
# First we need to make an "individual" ID.
dat.long$ID <- paste(dat.long$SPA,dat.long$Tow_No,dat.long$Scallop_No,sep="_")
head(dat.long)


# Get the data subset into useable bits...
# Note that I also am removing the "LY" data as shown above the chips make this data mostly unuseable for this analysis...
areas <- sort(unique(dat.long$SPA))
dat.long$std_size <- scale(dat.long$size,center=T,scale=F)
size_range <- floor(min(dat.long$std_size,na.rm=T)-10):ceiling(max(dat.long$std_size,na.rm=T))+10
pred.dat <- data.frame(std_size = rep(size_range,length(areas)),
                       SPA=sort(rep(areas,length(size_range)))) # Some data to predict on for the gam...


min.dat <- 50
useable.data <- aggregate(std_size~year,dat.long,FUN=function(x) which(length(x) >=min.dat))
useable.data <- as.character(na.omit(useable.data$year[useable.data$V1==1]))
# Remove the Last year from the data
useable.data <- useable.data[useable.data != "LY"]
dat.mod <- dat.long[dat.long$year %in% useable.data,]

# Note that I needed to change the smooth here as te is not avialbe in gamm4.
mod.res.rand <- gamm4(growth~t2(std_size,by=SPA),
                      random =~ (1 | ID),
                      data=dat.mod,family = "poisson")
preds <- predict(mod.res.rand$gam,pred.dat,type="response",se=T)



windows(11,11)
for(i in 1:length(areas))
{
  tmp <- dat.mod[dat.mod$SPA == areas[i],]
  if(i == 1) 
  {
    plot(growth~std_size,tmp,pch = 19,cex=0.5,ylim=range(dat.long$growth,na.rm=T),xlim=range(dat.long$std_size,na.rm=T),xaxt="n")
    if(i == 1) axis(1,at =seq(0,170,by=10)-mean(dat.long$size,na.rm=T),labels=seq(0,170,10))
    legend("topright",legend = areas,col= plasma(length(areas)),pch=19,lty=1:length(areas), bty="n")
    
  } # end if(i == 1) 
  if(i > 1) points(growth~std_size,tmp,pch=19,col=plasma(length(areas))[i],cex=0.5)
  lines(preds$fit[pred.dat$SPA==areas[i]]~pred.dat$std_size[pred.dat$SPA==areas[i]],col=plasma(length(areas))[i],lty=i,lwd=2)
  lines((preds$fit[pred.dat$SPA==areas[i]]+ preds$se[pred.dat$SPA==areas[i]])~pred.dat$std_size[pred.dat$SPA==areas[i]],
        col=plasma(length(areas))[i],lty=i,lwd=0.5)
  lines((preds$fit[pred.dat$SPA==areas[i]]- preds$se[pred.dat$SPA==areas[i]])~pred.dat$std_size[pred.dat$SPA==areas[i]],
        col=plasma(length(areas))[i],lty=i,lwd=0.5)  
}# end for(i in 1:length(areas))

# Result looks very similar to the non-random effect model, let's dig a little deeper...

# Model validation
plot(mod.res.rand$mer)  #Looks fine.

# I'm not sure if we should use the gam or the mer object for the dispersion calculation
# They fortunately give very similar results, Dispersion parameter between 0.9 and 1.07
# Both of which are nicely improved over the 1.4 in the initial model.
# I"m going with the mer calclation rather the gam mostly b/c that seems to be how Zurr did it
# thankfully both are giving essentially the same answer, though maybe there is some underdispersion
# in the data using the mer method
# I BELIEVE THAT USING THE MER RESIDUALS AND FITTED VALUES IS CORRECT WAY TO GO HERE.
E.gam <- resid(mod.res.rand$gam)
Dispersion.gam <- sum(E.gam^2) / mod.res.rand$gam$df.res
Dispersion.gam

E.mer <- resid(mod.res.rand$mer)
Dispersion.mer <- sum(E.mer^2) / df.residual(mod.res.rand$mer)
Dispersion.mer

# The error with respect to the ID's, nothing odd here I don't think...
windows(11,11)
boxplot(E.gam ~ dat.mod$ID[!is.na(dat.mod$growth)],
        varwidth = TRUE)
abline(h = 0, lty = 2)
windows(11,11)
boxplot(E.mer ~ dat.mod$ID[!is.na(dat.mod$growth)],
        varwidth = TRUE)
abline(h = 0, lty = 2)
# ok I think


# Grab the fitted values
F.mer <- fitted(mod.res.rand$mer)
F.gam <- fitted(mod.res.rand$gam)

# Let's look at fitted gam vs fitted mer results, should be pretty much the same!
# I'm thinking doing this is silly exercise, but I'm not entirely sure
# They aren't identical which seems a little weird!
windows(11,11)
plot(F.mer ~ F.gam)
abline(a = 0, b=1, lty = 2,col="blue",lwd=2)

# versus residuals and fitted vs observed...
windows(11,11)
plot(E.mer ~ F.mer)
abline(h = 0, lty = 2,col="blue",lwd=2)

windows(11,11)
plot(E.gam ~ F.gam)
abline(h = 0, lty = 2,col="blue",lwd=2)

# From this I think the model will underestimate rapid growth, around about 20 it seems to be under estimating growth
# On the low end I think the model sligthly overestimates grwoth, probably only by a couple mm.
# Between about 5 mm and 20 mm growth things look fairly nice to me.
windows(11,11)
plot(na.omit(dat.mod$growth)~ F.mer)
abline(a = 0, b=1, lty = 2,col="blue",lwd=2)

windows(11,11)
plot(na.omit(dat.mod$growth)~ F.gam)
abline(a = 0, b=1, lty = 2,col="blue",lwd=2)

# Can see that the median fitted value is about 0.2 mm larger than the median observed value
median(na.omit(dat.mod$growth)- F.mer)
# But generally the model seems o.k. I think...
windows(11,11)
boxplot(na.omit(dat.mod$growth)- F.mer)


# Now we need to take this shell growth and turn it into a change in biomass somehow...
# It's not a meat weight-shell height I'm interested in it is the change in meat weights with shell heights
# it's like the 1st derivate of the MW-SH curve or sumthin' like that...




