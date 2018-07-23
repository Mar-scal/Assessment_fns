###........................................###
#### Testing a new growth model for GBa based on using incremental growth (for the moment based on SPA 6 as I'm guess that's the best data)
###  I have from the BoF for the moment.  I will use the GAM MW-SH for the GBa data and combine these pieces of information 
###  To get growth estimates to feed the GBa model and take a look to see how these impact our model.
###........................................###

#setwd

#setwd("Y:/INSHORE SCALLOP/SFA29/2016/Growth")
direct <- "d:/r/"
dir.tmp <- direct
yr <- 2016
#####################################################################  Section 1 Load the data##########################################################  
#####################################################################  Section 1 Load the data##########################################################  
#required packages
library (lme4)
library(RODBC)
library(lattice)
library(gamm4)
library(rgl)
library(akima)
library(RColorBrewer)
library(PBSmapping)
library(viridis)
library(plyr)
library(geosphere)
library(mgcv)
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))#source the ScallopMap script (function containing maps and parameters)

#############
# read in the survey data for Georges (and others), the object we want out of this is mw.dat.all[["GBa"]]
############

load(file = paste(direct,"Data/Framework/2017/Growth/Survey_all_results.Rdata",sep=""))
direct <- dir.tmp
# Just get the GBa MW data.
mw.dat <- mw.dat.all[["GBa"]]
mw.dat$area <- "GBa"
head(mw.dat)

#################  Now for the growth data#################  Now for the growth data#################  Now for the growth data
###  Now load in the growth data....

#################  Now for the growth data#################  Now for the growth data#################  Now for the growth data
###  Now load in the growth data....
dat <- read.csv(paste(direct,"Data/Ageing/BoF_complete_ageing_2016.csv",sep=""),check.names=F) # This data comes from the xlsx (DRAFTageingSFA292016.xlsx)
# Take a look at the data
head(dat)
names <- c("LY",as.character(na.omit(as.numeric(names(dat))))) #
#
# I need to get a unique tow ID, so combine Tow number and area
dat$Tow_ID <- paste(dat$Cruise,dat$Tow_No,sep="_")
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
# for the thickening of the shell, but wonder if something... very roughly, assuming a Scallop shell is a semi-circel...
#index <- 0
# Now run the same loop but do this rough area calculation...
for(i in start.row:(end.row-1))
{
  index <- 1+index # update the index
  nam <- paste("Area",names(dat[i]),sep="") # get a good column name for the growth data
  dat[,(end.row+index)] <- 0.5* (pi*(dat[,i]/10)^2 - pi*(dat[,(i+1)]/10)^2) # calcuate the difference
  names(dat)[end.row+index] <- nam # and rename the new data column.
} # end for(i in start.row:(end.row-1))

# Now I'm going to convert the dat into a long form so that we have "factors" and data is nice to analyze.
growth.names <- names(dat)[grep("G",names(dat))]
area.names <- names(dat)[grep("Area",names(dat))]
dat.long <- reshape(dat,direction="long",varying=list(names),times=names)

dat.long <- dat.long[,-which(names(dat.long) %in% c(area.names,growth.names))]
#
dat.g.long <- reshape(dat,direction="long",varying=list(growth.names),times=growth.names)
dat.a.long <- reshape(dat,direction="long",varying=list(area.names),times=area.names)
# I need to add in the final year of "data" which we can't have for the growth data, i.e add in a bunch of NA's for growth in the first year
# we have information for.
dat.long$GLY <- c(dat.g.long$GLY,rep(NA,nrow(dat)))
dat.long$AreaLY <- c(dat.a.long$AreaLY,rep(NA,nrow(dat)))
dat.long <- dat.long[,-which(names(dat.long)=="id")]
names(dat.long) <- c(names(dat.long[1:(which(names(dat.long) =="time")-1)]),"year","size","growth","area")
# Finally, and new, the real "size" is the size that the scallop was before the growth was added, 
dat.long$size_start <- dat.long$size-dat.long$growth


##########################################  Section 2 Comparing MW - SH Models looking at most recent year... ####################################### 
########################################## Section 2 Comparing MW - SH Models looking at most recent year...######################################### 
#### 2016
#Subset for year
dat.mwsh<-subset(mw.dat, year==2016)

#create dataset for model...
test.data<-subset (dat.mwsh, sh>=40)
test.data$sh.log <-log(test.data$sh)
test.data$sh.log.cen <-scale(test.data$sh.log,scale=F,center=T)
test.data$sh.cen <- scale(test.data$sh,scale=F,center=T)
#test.data$depth<-test.data$depth 
test.data$depth.cen <- scale(test.data$depth,scale=F,center=T)
#test.data$DEPTH.CTR<-test.data$DEPTH-mean(test.data$DEPTH)
#test.data$wmw <- test.data$wmw
summary(test.data)

#run model; update model name to correspond to year

current.mod<-glmer(wmw~sh.log+depth.cen+(1+sh.log|tow),data=test.data,
                       family=Gamma(link=log), na.action = na.omit)
# Let's try a depth smooth model rather than the glmer, does this work as nicely for the 29 data as it does for the Georges bank data?
# It works well, but as the AIC shows below in 29 the GLMER model is actually a little better, all the 
# residuals looks solid for both models, though the GLMER actually seems to fit a bit better (one tiny residaul pattern I don't love, but
# to be honest it is likely still fine, whereas the GAM is underfitting the largest MW's)
mod.2016.gam   <- gamm4(wmw ~ s(sh.log) + s(depth.cen) , random = ~ (1  |tow),data=test.data,family = Gamma(link="log"))
# And let's try and remove depth from the equation to see if it matters given we have the tow random term...
mod.2016.gam.no.depth <- gamm4(wmw ~ s(sh.log)  , random = ~ (1  |tow),data=test.data,family = Gamma(link="log"))

# What we see on GBa is that the model with depth in there is a little bit better than the model without...
AIC(current.mod,mod.2016.gam$mer,mod.2016.gam.no.depth$mer)



# Look at model results for the big gams, here we see that the no depth models look good, 
# paritally why I went forward with the individual gams without the depth in them for area 29.
# wmw-sh data.
windows(11,8.5)
par(mfrow=c(2,2))
plot(mod.2016.gam.no.depth$gam, scale = F, cex.lab = 1.5)  
plot(mod.2016.gam$gam, scale = F, cex.lab = 1.5)  

# Grab the residuals and fitted values
E2016.gam <- resid(mod.2016.gam$mer, type = "pearson")
E2016.glmer <- resid(current.mod)
F2016.gam <- fitted(mod.2016.gam$mer)
F2016.glmer <- fitted(current.mod)
# Now compare residuals to fitted values
windows(11,8.5)
par(mfrow=c(2,1))
plot(F2016.gam,E2016.gam,xlab = "Fitted values",ylab = "Pearson residuals - GAM") # this is fairly solid
abline(h=0,col="blue",lty=2)
plot(F2016.glmer,E2016.glmer,xlab = "Fitted values",ylab = "Pearson residuals -GLMER") # this is fairly solid
abline(h=0,col="blue",lty=2)

windows(11,8.5)
par(mfrow=c(2,1))
plot(F2016.gam~test.data$wmw,xlab="Fitted values",ylab="Observed Data - GAM") # Actually a bit of overestimatation for the largest meat weights now.
abline(a=0,b=1,col="blue",lty=2)
plot(F2016.glmer~test.data$wmw,xlab="Fitted values",ylab="Observed Data - GLMER") # This does a little better for high values.
abline(a=0,b=1,col="blue",lty=2)

# Residuals vs shell height
windows(11,8.5)
par(mfrow=c(2,1))
plot(E2016.gam~ test.data$sh.cen,pch=16,main="GAM") # Residuals against the shell height, they seem fine.
abline(h=0,col="blue",lty=2)
plot(E2016.glmer~ test.data$sh.cen,pch=16,main="GLMER") # Residuals against the shell height, they aren't terrible but
# there is a hint of a pattern for larger shells
abline(h=0,col="blue",lty=2)

# Residuals vs depth
windows(11,8.5)
par(mfrow=c(2,1))
plot(E2016.gam~ test.data$depth,pch=16,main="GAM") # Residuals vs depth, they seem good as well, I'm thinking the random effects have swallowed this all up!
abline(h=0,col="blue",lty=2)
plot(E2016.glmer~ test.data$depth,pch=16,main="GLMER") # Residuals vs depth, they seem good as well, I'm thinking the random effects have swallowed this all up!
abline(h=0,col="blue",lty=2)
# Residuals vs tow
windows(11,8.5)
par(mfrow=c(2,1))
boxplot(E2016.gam~ as.numeric(test.data$tow),pch=16,main="GAM")
abline(h = 0, col="blue",lty = 2) 
boxplot(E2016.glmer~ as.numeric(test.data$tow),pch=16,main="GLMER")
abline(h = 0, col="blue",lty = 2) 



# What about lattitude
# There should be nothing here.
lat <- test.data$lat
lon <- test.data$lon
windows(11,8.5)
par(mfrow=c(2,2))
plot(E2016.gam~lat,pch=16,main="GAM")
abline(h = 0, col="blue",lty = 2) 
plot(lat,F2016.gam,pch=16,main="GAM") # These are looking good, again the random effects have swallowed up all this residual variation
plot(E2016.glmer~lat,pch=16,main="GLMER")
abline(h = 0, col="blue",lty = 2) 
plot(lat,F2016.glmer,pch=16,main="GLMER") # These are looking good, again the random effects have swallowed up all this residual variation

lat.res.gam <- gam(E2016.gam~te(lat))
summary(lat.res.gam) # random effects have swallowed up all the spatial effects!
windows(11,8.5)
par(mfrow=c(1,2))
plot(lat.res.gam)

# What about longitude 
windows(11,8.5)
par(mfrow=c(2,2))
plot(lon,E2016.gam,pch=16,main="GAM")
abline(h = 0, col="blue",lty = 2) 
plot(lon,F2016.gam,pch=16,main="GAM") # Nothing weird here.
lon.res.gam <- gam(E2016.gam~t2(lon))
summary(lon.res.gam) # random effects again have swallowed up all these spatial effects.
windows(11,8.5)
plot(lon.res.gam)


# Let's look at this in a bit more spatial depth... I wanted to do kriging but we have mutliple measurements at one point
# so that blows up, same problem for contour plots or anything, best solution I can think of is a real 3D scatterplot with
# all the points shown...

plot3d(mw$lon,mw$lat,F2015) # You can see this really frees up the data to do what it should both at the edges and in the middle.
plot3d(mw$lon,mw$lat,E2015) # Can see the outlier residual values are spread all over the place which is fine by me.


Res.lat.lon.gam <- gam(E2016.glmer~t2(lon,lat))
summary(Res.lat.lon.gam) # Less than 1% left here
Res.lat.lon.glmer <- gam(E2016.glmer~t2(lon,lat))
summary(Res.lat.lon.glmer) # Less than 1% left here
# These models are really nearly identical...
windows(8.5,11)
par(mfrow=c(1,2))
plot(Res.lat.lon.gam)
plot(Res.lat.lon.glmer)
windows(11,8.5)
par(mfrow=c(1,2))
im.gam <- with(test.data,interp(lon,lat,E2016.gam,duplicate="mean"))
with(im.gam,image(x,y,z)) # The edge effects may be popping back up here but residuals are tiny remember...
im.glmer <- with(test.data,interp(lon,lat,E2016.glmer,duplicate="mean"))
with(im.glmer,image(x,y,z)) # The edge effects may be popping back up here but residuals are tiny remember...


## So the upshot from this is that either the GAM or the GLMER is going to provide solid predictions for us.  I lean slightly to the GAM
## because I think the assumptions underpinning it are a little more realisitic, I think the GLMER is a bit of an overfit.
## I can also easily get the uncertainty in the estimate from the GAM model...

##################################################  Section 3 Growth modeling ##################################################  Section 3
##################################################  Section 3 Growth modeling ##################################################  Section 3
##################################################  Section 3 Growth modeling ##################################################  Section 3
#### So now I"m moving to the growth modeling, for the moment I just want to use SPA6

# Remove everything but area 29...
dat.tmp <- dat.long[grep("29D",dat.long$SPA),]

# Let's only look at shells above 40 mm 
dat.tmp <- dat.tmp[dat.tmp$size >=40,]
# And we also need to account for multiple readings from one individual.
# First we need to make an "individual" ID.
dat.tmp$ID <- paste(dat.tmp$SPA,dat.tmp$Tow_No,dat.tmp$Scallop_No,sep="_")
head(dat.tmp)
# Get the data subset into useable bits...
# Note that I also am removing the "LY" data as shown above the chips make this data mostly unuseable for this analysis...
areas <- sort(unique(dat.tmp$SPA))
dat.tmp$std_size <- scale(dat.tmp$size,center=T,scale=F)
#dat.tmp$log_size <- log(dat.tmp$size)
size_range <- floor(min(dat.tmp$std_size,na.rm=T)-10):ceiling(max(dat.tmp$std_size,na.rm=T))+10
pred.dat <- data.frame(std_size = rep(size_range,length(areas)),
                       SPA=sort(rep(areas,length(size_range)))) # Some data to predict on for the gam...
#pred.dat$size <- round(exp(pred.dat$log_size)) # Make sure it's the exact number...
min.dat <- 1
useable.data <- aggregate(std_size~year,dat.tmp,FUN=function(x) which(length(x) >=min.dat))
useable.data <- as.character(na.omit(useable.data$year[useable.data$V1==1]))
# Remove the Last year from the data as the chipped shells make that data on it's own very poor.
useable.data <- useable.data[useable.data != "LY"]
dat.mod <- dat.tmp[dat.tmp$year %in% useable.data,]

# The model... this is basically an over-dispersed poisson model (random term enables the over-dispersion)
mod.res.rand <- gamm4(growth~ t2(std_size),
                      random =~ (1 | ID),
                      data=dat.mod,family = "poisson")

##################################################  Section 4 Final Models and Prediction ##################################################  Section 4
##################################################  Section 4 Final Models and Prediction ##################################################  Section 4
##################################################  Section 4 Final Models and Prediction ##################################################  Section 4
##################################################  Section 4 Final Models and Prediction ##################################################  Section 4

################# Step 1  ################# Step 1  ################# Step 1  ################# Step 1  ################# Step 1  
#We get our meat weight shell height model and predictions for each year....
# Now the model predictions, first set up the loop and data
years <- sort(as.character(unique(mw.dat$year)))
num.years <- length(years)


# Open a plotting device.
pdf(file=paste(direct,"2017/Framework/Growth/Figures/MW_SH_GBa.pdf",sep=""),onefile=T)
# Loop for every year.
mod.mwsh <- NULL
pred.mwsh <- NULL
tmp1 <- NULL
min.dat <- 20 # We need at least 20 samples for this to be worthwhile for an area.
for(j in 1:num.years)
{
  # Get the data and set up some nice variable names
  test.data <- mw.dat[mw.dat$year == years[j],]
  test.data <- test.data[test.data$sh >=40,]
  test.data$sh.log <-log(test.data$sh)
  test.data$sh.log.cen <-test.data$sh.log-mean(test.data$sh.log)
  test.data$sh.cen <- scale(test.data$sh,scale=F,center=T)
  test.data$depth.cen <- scale(test.data$depth,scale=F,center=T)
  areas.mwsh <- unique(test.data$area)


# Here's what the by area model looks like...
#windows(11,11)

  for(i in 1:length(areas.mwsh))
  {
    # get a variable
    tmp <- test.data[test.data$area == areas.mwsh[i],]

    if(nrow(tmp) >= min.dat) # Only make the plots/models if we have at least "min.dat" points, see just above the loop...
    {
      # Run the model for each area if there is enough data!
      mod.mwsh[[areas.mwsh[i]]] <- gamm4(wmw ~ t2(sh.log)  , random = ~ (1  |tow),data=tmp,family = Gamma(link="log"))
      # Get the predictions from the model.
      pred.mwsh[[areas.mwsh[i]]] <- data.frame(sh.log = log(seq(40,170,by=0.1)))
      pred.mwsh[[areas.mwsh[i]]]$sh <- round(exp(pred.mwsh[[areas.mwsh[i]]]$sh.log),digits=4)
      pred.mwsh[[areas.mwsh[i]]]$wmw <- predict(mod.mwsh[[areas.mwsh[i]]]$gam,
                                                              pred.mwsh[[areas.mwsh[i]]],type="response",re.form=NA)
      pred.mwsh[[areas.mwsh[i]]]$se <- predict(mod.mwsh[[areas.mwsh[i]]]$gam,
                                                             pred.mwsh[[areas.mwsh[i]]],type="response",re.form=NA,se=T)$se.fit
      pred.mwsh[[areas.mwsh[i]]]$SPA <- areas.mwsh[[i]] # Get the name of the area
      pred.mwsh[[areas.mwsh[i]]]$year <- years[j] # Get the year
      pred.mwsh[[areas.mwsh[i]]]$LCI.wmw <- pred.mwsh[[areas.mwsh[i]]]$wmw - 2* pred.mwsh[[areas.mwsh[i]]]$se
      pred.mwsh[[areas.mwsh[i]]]$UCI.wmw <- pred.mwsh[[areas.mwsh[i]]]$wmw + 2* pred.mwsh[[areas.mwsh[i]]]$se
      # Now make the plot
      if(i == 1) 
      {
        plot(wmw~sh,tmp,pch = 19,cex=0.5,ylim=range(mw.dat$wmw,na.rm=T),xlim=range(mw.dat$sh,na.rm=T),xaxt="n",main = years[j])
        if(i == 1) axis(1,at =(seq(40,170,by=10)),labels=seq(40,170,10))
        legend("topright",legend = areas.mwsh,col= plasma(length(areas.mwsh)),pch=19,lty=1:length(areas.mwsh), bty="n")
        
      } # end if(i == 1) 
      if(i > 1) points(wmw~sh,tmp,pch=19,col=plasma(length(areas.mwsh))[i],cex=0.5)
      
      lines(wmw~sh, pred.mwsh[[areas.mwsh[[i]]]],col=plasma(length(areas.mwsh))[i],lty=i,lwd=2)
      lines((wmw + 2*se)~sh,  pred.mwsh[[areas.mwsh[[i]]]],col=plasma(length(areas))[i],lty=i,lwd=0.5)
      lines((wmw- 2*se)~sh,  pred.mwsh[[areas.mwsh[[i]]]], col=plasma(length(areas.mwsh))[i],lty=i,lwd=0.5)  
    } # end if(nrow(tmp) >= min.dat)  
  }# end for(i in 1:length(areas))
  tmp1[[years[j]]] <- do.call("rbind",pred.mwsh) # A temporary variable with the results for each year stored in a list.
} # end for(j in 1:num.years)
dev.off()
mwsh.predictions <- do.call("rbind",tmp1)


##############################  Step 2 the growth model.  This is simply modelling growth overall, we could  ##############################  
##############################  estimate growth by year if feeling funky...                                  ##############################  



mod.growth <- NULL
pred.growth <- NULL
# Here's what that model looks like
windows(11,11)
for(i in 1:length(areas))
{
  tmp <- dat.tmp[dat.tmp$SPA == areas[i],]
  tmp$std_size <- scale(tmp$size,center=T,scale=F)
  #dat.tmp$log_size <- log(dat.tmp$size)
  size_range <- seq((min(tmp$std_size,na.rm=T)-10),(max(tmp$std_size,na.rm=T)+10),by=0.1)
  min.dat <- 1 # If I want to remove some data, useful if looking for a curve by year, but I want all these data I think...
  useable.data <- aggregate(std_size~year,tmp,FUN=function(x) which(length(x) >= min.dat))
  useable.data <- as.character(na.omit(useable.data$year[useable.data$V1==1]))
  # Remove the Last year from the data as the chipped shells make that data on it's own very poor.
  useable.data <- useable.data[useable.data != "LY"]
  mod.tmp <- tmp[tmp$year %in% useable.data,]
  
  # Run the model for each area
  mod.growth[[areas[[i]]]] <- gamm4(growth~ t2(std_size), random =~ (1 | ID),data=mod.tmp,family = "poisson")
  # Get the predictions from the model.
  pred.growth[[areas[[i]]]] <- data.frame(std_size = size_range) # Some data to predict on for the gam...
  pred.growth[[areas[[i]]]]$size <- pred.growth[[areas[[i]]]]$std_size + attr(tmp$std_size,"scaled:center")
  pred.growth[[areas[[i]]]]$growth <- predict(mod.growth[[areas[[i]]]]$gam,pred.growth[[areas[[i]]]],type="response",re.form=NA)
  pred.growth[[areas[[i]]]]$se <- predict(mod.growth[[areas[[i]]]]$gam,pred.growth[[areas[[i]]]],type="response",re.form=NA,se=T)$se.fit
  pred.growth[[areas[[i]]]]$SPA <- areas.mwsh[[i]] # Get the name of the area
  pred.growth[[areas[i]]]$size.ny <- pred.growth[[areas[i]]]$size + pred.growth[[areas[i]]]$growth # mean size next year.
  pred.growth[[areas[i]]]$LCI.ny <- pred.growth[[areas[i]]]$size + pred.growth[[areas[i]]]$growth - 2*pred.growth[[areas[i]]]$se 
  pred.growth[[areas[i]]]$UCI.ny <- pred.growth[[areas[i]]]$size + pred.growth[[areas[i]]]$growth + 2*pred.growth[[areas[i]]]$se 
  # Now make the plot
  if(i == 1) 
  {
    plot(growth~size,tmp,pch = 19,cex=0.5,ylim=range(dat.tmp$growth,na.rm=T),xlim=range(dat.tmp$size,na.rm=T),xaxt="n")
    if(i == 1) axis(1,at =(seq(40,170,by=10)),labels=seq(40,170,10))
    legend("topright",legend = areas,col= plasma(length(areas)),pch=19,lty=1:length(areas), bty="n")
    
  } # end if(i == 1) 
  if(i > 1) points(growth~size,tmp,pch=19,col=plasma(length(areas))[i],cex=0.5)
  lines(growth~size,pred.growth[[areas[[i]]]],col=plasma(length(areas))[i],lty=i,lwd=2)
  lines((growth+ 2*se)~size,pred.growth[[areas[[i]]]],col=plasma(length(areas))[i],lty=i,lwd=0.5)
  lines((growth- 2*se)~size,pred.growth[[areas[[i]]]],col=plasma(length(areas))[i],lty=i,lwd=0.5)  
}# end for(i in 1:length(areas))
growth.predictions <- do.call('rbind',pred.growth)
growth.predictions$size <- round(growth.predictions$size,digits=1) # Need to round it make sure we have clean numbers...

############## Step 3, we now have everythign we need to predict growth for all years...
# So to predict how much a shell will grow next year, we have several options here and I'm not sure what the best is...
# What I've done is use the current years survey meat weight-shell height relationship, project the shell growth to next year and 
# get the expected meat weight of that size scallop.
# It's important to note the bit towards the bottom where we see that there is no relationship between the current meat weight

# Make an object with our predictions in it...
mw.pred <- data.frame(com = rep(NA,num.years),rec = rep(NA,num.years),
                      com.ny = rep(NA,num.years),rec.ny = rep(NA,num.years),
                      year = years)
for(i in 1:num.years)
{
  sizes.com <- round(survey.obj$GBa$model.dat$l.bar[i],digits=1)
  sizes.rec <- round(survey.obj$GBa$model.dat$l.k[i],digits=1)
  sizes.ny.com <- round(growth.predictions[growth.predictions$size == sizes.com,]$size.ny,digits=1)
  sizes.ny.rec <- round(growth.predictions[growth.predictions$size == sizes.rec,]$size.ny,digits=1)

  mw.pred$com.ny[i] <- mwsh.predictions$wmw[mwsh.predictions$year==years[i] & mwsh.predictions$sh == sizes.ny.com]
  mw.pred$rec.ny[i] <- mwsh.predictions$wmw[mwsh.predictions$year==years[i] & mwsh.predictions$sh == sizes.ny.rec]
  mw.pred$com.ny.LCI[i] <- mwsh.predictions$LCI.wmw[mwsh.predictions$year==years[i] & mwsh.predictions$sh ==sizes.ny.com]
  mw.pred$rec.ny.LCI[i] <- mwsh.predictions$LCI.wmw[mwsh.predictions$year==years[i] & mwsh.predictions$sh ==sizes.ny.rec]
  mw.pred$com.ny.UCI[i] <- mwsh.predictions$UCI.wmw[mwsh.predictions$year==years[i] & mwsh.predictions$sh ==sizes.ny.com]
  mw.pred$rec.ny.UCI[i] <- mwsh.predictions$UCI.wmw[mwsh.predictions$year==years[i] & mwsh.predictions$sh ==sizes.ny.rec]
  
  mw.pred$com[i] <- mwsh.predictions$wmw[mwsh.predictions$year==years[i] & mwsh.predictions$sh ==sizes.com]
  mw.pred$rec[i] <- mwsh.predictions$wmw[mwsh.predictions$year==years[i] & mwsh.predictions$sh ==sizes.rec]
} # end for(i in 1:num.years)

mw.pred$g.com <- mw.pred$com.ny/mw.pred$com
mw.pred$g.com.LCI <- mw.pred$com.ny.LCI/mw.pred$com
mw.pred$g.com.se <- (mw.pred$g.com - mw.pred$g.com.LCI)/2
mw.pred$g.com.UCI <- mw.pred$com.ny.UCI/mw.pred$com
mw.pred$g.rec <- mw.pred$rec.ny/mw.pred$rec
mw.pred$g.rec.LCI <- mw.pred$rec.ny.LCI/mw.pred$rec
mw.pred$g.rec.UCI <- mw.pred$rec.ny.UCI/mw.pred$rec
mw.pred$g.rec.se <- (mw.pred$g.rec - mw.pred$g.rec.LCI)/2

load(paste(direct,"/Data/Model/2017/GBa/Results/Model_testing_results.RData",sep=""))

g.com.cur.method <- mod.dat$GBa$g[mod.dat$GBa$year %in% years]
g.rec.cur.method <- mod.dat$GBa$gR[mod.dat$GBa$year %in% years]
windows(11,11)
plot(mw.pred$g.com~g.com.cur.method,pch=16,xlim=c(0.5,2),ylim=c(0.5,2.2),type="n",xlab="Von B growth",ylab="Increment growth",main=areas)
text(mw.pred$g.com~g.com.cur.method,labels = mw.pred$year,col = "black",cex=0.5)
text(mw.pred$g.rec~g.rec.cur.method,labels = mw.pred$year,col = "orange",cex=0.5)
segments(g.rec.cur.method,mw.pred$g.rec.LCI,g.rec.cur.method,mw.pred$g.rec.UCI,col="orange")
segments(g.com.cur.method,mw.pred$g.com.LCI,g.com.cur.method,mw.pred$g.com.UCI,col="black")
abline(a=0,b=1,col="blue")
legend("topleft",c("Recruit Growth","Commerical Growth"),col=c("orange","black"),lty=1,lwd=2,bty="n",text.col = c("orange","black"))

# Save the MW prediction object to be loaded into the new model...
save(mw.pred,file=paste(direct,"Data/Framework/2017/Growth/GBa_growth_prediction.Rdata",sep=""))

# Now if we want to look at growth variability, we can look over the history of the time series and see
# what the variability in growth has been at a given size (i.e. if the size this year is 102 mm what would growth at this size been
# expected to be over the history of the time series?)
tmp <- mwsh.predictions[mwsh.predictions$sh == round(growth.predictions[growth.predictions$size == sizes.rec,]$size.ny,digits=1),]
# What is "condition" from this in each year compared to the funky model we used in the past...
plot(mwsh.predictions[mwsh.predictions$sh == round(growth.predictions[growth.predictions$size == 100,]$size,digits=1),]$wmw~
       years,type='b',ylab="")
# Really pretty much the same!
lines(survey.obj$GBa$model.dat$CF~years,col="blue",type="b")
# The current model tends to give higher condition than this new model, but it's generally not a huge difference (< 1 g/100mm)
cond.diff <- mwsh.predictions[mwsh.predictions$sh == round(growth.predictions[growth.predictions$size == 100,]$size,digits=1),]$wmw -
                            survey.obj$GBa$model.dat$CF
mean(cond.diff) # So the difference in MW on average b/t methods is about 1%, but it can be as high as 8-9%
cbind(round(cond.diff,digits=2),years)
# So would the most sensible prediction simply be the average wmw in the past for a given shell height and account for the error around that
# to get at the uncertainty??  If we had some sort of covariate (plankton size) then we could account for that in our wmw estimates
# for the scallop... hmmm... that might just be the way forward, though I think that covariate should go into our wmw-sh model or our growth model
# or both, that way we get a nice answer right from those models, but of course for that we need a big model don't we... Hmm do we model a model
# or try to get at it within one model... let's go for a walk...
plot(mw.pred$g.com[years >= 2000]~survey.obj$GBa$model.dat$l.bar[years >= 2000],type="p")#)
acf(mw.pred$g.com[years >= 2000],plot=T) # No correlation, hmm...

# This is the biggest one year difference in growth for a given size of scallop, t
pred.min <- min(diff(mwsh.predictions$wmw[mwsh.predictions$sh == sizes.ny.com] / mwsh.predictions$wmw[mwsh.predictions$sh ==sizes.com],lag=1))
pred.max <- min(diff(mwsh.predictions$wmw[mwsh.predictions$sh == sizes.ny.com] / mwsh.predictions$wmw[mwsh.predictions$sh ==sizes.com],lag=1))
#The smaller the scallop the more variable
# this is, but using the sizes tat have commonly been observed (on average) in GBa this value is very similar to the LCI and UCI
# from the current years prediction.  Thus I think we can use the current year estimated growth and use the LCI and UCI as predictions
# for high and low growth scenarios.  I think this might not be the case in every area and to get good prediction scenarios for
# the other banks we may need to be thoughtful here and not go with a one size fits all scenario.

## Here's my plan... I think...
# 1:  We model the growth as per the above.
# 2:  We put the growth into the model... there is no perfect way, but the above looks like a nice scenario to me, in this scenario
#     we use the current year MW-SH relationship to model growth, hopefully I can add in the uncertainty to the model, having that
#     might help account for difference with the MW-SH bit, we shall see.
# 3:  For the prediction step I've flip flopped around, but for GBa I think the best way forward is to use the current years predicted growth 
#     for next year as our "expected" case, then make a decision table for the LCI and UCI scenarios as well.


