###........................................###
#### Testing a new growth model using Area 29 as a test case.  This model incorporates GAM's for both the Meat Weight Shell height relationship
#### and the GAM using the new interval growth methods.  This model should be able to get us growth estimates with at least some measure of the
#### uncertainty in those estimates.
###
###........................................###

#setwd

#setwd("Y:/INSHORE SCALLOP/SFA29/2016/Growth")
direct <- "d:/r/"

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
# read in shell height and meat weight data from database
############


RODBCconn <-odbcConnect("ptran", uid=un.ID, pwd=pwd.ID,believeNRows=FALSE)
cruise.list <- c('SFA292001', 'SFA292002','SFA292003','SFA292004','SFA292005','SFA292006',
			'SFA292007','SFA292008', 'SFA292009' ,'SFA292010','SFA292011','SFA292012','SFA292013','SFA292014', 'SFA292015', 'SFA292016') # Must be UPDATED for Current Year! # 
cruise.list <- paste(cruise.list,collapse="','")

#detailed meat weight/shell height sampling data
quer1 <- paste(
		"SELECT *                                     ",
		"FROM scallsur.scwgthgt                      ",                                                                                                                                   
		"WHERE strata_id IN (41, 42, 43, 44, 45)      ",								  
		"AND (cruise in ('",cruise.list,"'))          ",
		sep=""
	    )
SFA29detail.dat <- sqlQuery(RODBCconn, quer1, believeNRows=FALSE)  

#add YEAR column to data
SFA29detail.dat$YEAR <- as.numeric(substr(SFA29detail.dat$CRUISE,6,9))

#############
# read in depth information and add it to the meat weight/shell height dataframe
############
#there is no strata_id in Olex file to select on, need a unique identifier for tows to link to depth ($ID)

SFA29detail.dat$ID<-paste(SFA29detail.dat$CRUISE,SFA29detail.dat$TOW_NO,sep='.')
uniqueID<-unique(SFA29detail.dat$ID)

OlexTows_all<-read.csv("Y:/INSHORE SCALLOP/BoF/StandardDepth/TowData/towsdd_StdDepth.csv")
names(OlexTows_all)[9]<-"OLEXDEPTH_M"   #rename "RASTERVALU" column
OlexTows_all$OLEXDEPTH_M[OlexTows_all$OLEXDEPTH_M==-9999]<-NA
OlexTows_all$ID<-paste(OlexTows_all$CRUISE,OlexTows_all$TOW_NO,sep='.')
OlexTows_bof<-subset (OlexTows_all, ID%in%uniqueID)

SFA29detail<-merge(SFA29detail.dat,subset(OlexTows_bof,select=c("ID","OLEXDEPTH_M")), all.x=T)
SFA29detail$ADJ_DEPTH<-SFA29detail$OLEXDEPTH_M
SFA29detail$ADJ_DEPTH[is.na(SFA29detail$OLEXDEPTH_M)]<--1*SFA29detail$DEPTH[is.na(SFA29detail$OLEXDEPTH_M)] #*-1 because DEPTH is positive


#############
# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
############
summary(SFA29detail)
SFA29detail[is.na(SFA29detail$WET_MEAT_WGT),]
SFA29detail <- SFA29detail[!is.na(SFA29detail$WET_MEAT_WGT),] 

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
SFA29detail2016<-subset(SFA29detail, YEAR==2016)

#create dataset for model...
test.data<-subset (SFA29detail2016, HEIGHT>40)
test.data$sh.log <-log(test.data$HEIGHT)
test.data$sh.log.cen <-test.data$sh.log-mean(test.data$sh.log)
test.data$sh.cen <- scale(test.data$HEIGHT,scale=F,center=T)
test.data$depth<-test.data$ADJ_DEPTH 
test.data$depth.cen <- scale(test.data$ADJ_DEPTH,scale=F,center=T)
#test.data$DEPTH.CTR<-test.data$DEPTH-mean(test.data$DEPTH)
test.data$wmw <- test.data$WET_MEAT_WGT
summary(test.data)

#run model; update model name to correspond to year

current.mod<-glmer(wmw~sh.log+depth.cen+(1+sh.log|TOW_NO),data=test.data,
                       family=Gamma(link=log), na.action = na.omit)
# Let's try a depth smooth model rather than the glmer, does this work as nicely for the 29 data as it does for the Georges bank data?
# It works well, but as the AIC shows below in 29 the GLMER model is actually a little better, all the 
# residuals looks solid for both models, though the GLMER actually seems to fit a bit better (one tiny residaul pattern I don't love, but
# to be honest it is likely still fine, whereas the GAM is underfitting the largest MW's)
mod.2016.gam   <- gamm4(wmw ~ s(sh.log) + s(depth.cen) , random = ~ (1  |TOW_NO),data=test.data,family = Gamma(link="log"))

# If we want to be more conservative we can make up our own AIC correction which accounts for the complexity of the GAM's
# Look at the bottom of the code to see how the df's are calculated with a GAM and why we're doing this trick.
# Let's calculate an AIC ourselves which accounts more fully for the smoother.
# df = intercept + covariates  smoother + sigma_eps + sigma_bear
# The first part gets us the logliklihood from the mer part of the model.  
# The second part gets us the corrected degrees of freedom for this model.  The two 1's are due to sigma and sigma for the tow random effect.
# df = intercept + covariates  smoother + sigma_eps + sigma_bear
AIC.m2016.gam <- -2 * as.numeric(logLik(mod.2016.gam$mer)) + 2 * (sum(mod.2016.gam$gam$edf) + 1 + 1)
#AIC.m2015  <- -2 * as.numeric(logLik(m2015$mer)) + 2 * (sum(m2015$gam$edf) + 1 + 1)
#AICs <- data.frame(My_AIC = c(AIC.m2015s,AIC.m2015), edf = c((sum(m2015s$gam$edf) + 1 + 1),(sum(m2015$gam$edf) + 1 + 1)), 
#                   row.names = c("Spatial model", "depth_model")) 
#AICs


# We wouldn't want all areas to have a different MW-SH relationship by STRATA, but it does makes sense in SPA 29 since
# strata is really sub-areas.
# What I don't like about this model is we are fitting one depth relationship for all 5 areas, but I don't know
# that this is what we really want here, it's rather funky to think about, but as I mention below it's also
# not giving very sensible fits for some areas.  Also if we are using this for predictions for growth at what
# depth should we be predicting, if the mean of the area that will change with each survey depending on the depth 
# that we sample on.  
mod.2016.gam.by.area <- gamm4(wmw ~ s(sh.log,by=STRATA) + s(depth.cen) , random = ~ (1  |TOW_NO),data=test.data,family = Gamma(link="log"))
# I've tried all these models but they mostly suck...
# This one I kinda liked as it allowed the curve and the depth to vary by area, but that didn't turn out great.  If I try
# this model in area 29 with the tow in there it won't converge b/c tow and depth (at least I think) are fighting over the 
# same variability and there isn't enough information to sort out what is what (need more tows at each depth in an area to work.)
#mod.2016.gam.by.area.no.tow <- gam(wmw ~ s(sh.log,by=STRATA) + s(depth.cen,by=STRATA) ,data=test.data,family = Gamma(link="log"))
#mod.2016.glmer <-glmer(wmw~sh.cen+depth.cen + (sh.cen|TOW_NO),data=test.data,family=Gamma(link=log), na.action = na.omit) # This model sucks...
#mod.2016.gam.by.tow   <- gam(wmw ~ s(sh.cen,by=TOW_NO) + s(depth.cen) ,data=test.data,family = Gamma(link="log"))
# Now according to AIC the no depth model is slighlty worse (about 9), but the depth model fits are very strange for area D
# I think if we had the depth vary by area I'd be more comfortable with it, but the model won't converge with that
# as the depth and 
#AIC(current.mod,mod.2016.gam$mer,mod.2016.gam.by.area$mer,mod.2016.gam.by.area.no.depth$mer)

# For SPA29W the biggest improvement comes when we allow the relationship to vary by area, which makes some sense
# So our best model is the gamm with allowances for variation between the areas, which makes sence in SPA29!
# By not allowing the shape of the wmw-sh relationship to vary by tow I am constraining the gam and I think
# this is why we aren't getting as good an AIC as with the glmer.  

# Note also that the glmer does cheat a little here
# as all of these random terms aren't causing a big hit to the df (controversial if they should or not)
# if you made "current.mod" a intercept only and didn't allow the slope (i.e. shape) to vary by tow then it is no longer
# the preferred model.  Question is whether allowing that much freedom seems reasonable biologically.  i.e.
# should we allowed the exponent of the MW-SH relationship to vary by tow or assume the same exponent across tows 
# and only allow the intercept to vary (as the GAM does)? statistically in area 29W the results suggest that yes we should allow that exponent to vary...
# but by tow isn't probably the way to go, it is by area...
#AIC(mod.2016.gam$mer,mod.2016.glmer,current.mod,mod.2016.gam.by.tow)

# It's a little be more straightforward to just have the curve vary by area and allow the Tow_no variability to
# suck up the depth.  The residuals and fitted values from this model are essentially the same as the above model, 
# and may actually look ever so slightly better, but still it seems that the model does a terrible job of modelling area 29D
mod.2016.gam.by.area.no.depth <- gamm4(wmw ~ s(sh.log,by=STRATA)  , random = ~ (1  |TOW_NO),data=test.data,family = Gamma(link="log"))

# I think we have a case of the information from the other areas pulling the Area 29D results towards the 
# overall mean, despite what the data says, for prediction that's less than idea...
# This suggests that this is exactly what is happening, everything is getting pulled towards a mean.
# This isn't what we want for this since I'm not interested in having areas with good MW-SH relationships pulled
# down towards areas with poor relationships.  What we will do moving forwards is model each "area" seperately and go with that...
data.29D <- test.data[test.data$STRATA == "SFA29D",]

mod.2016.gam.29D <- gamm4(wmw ~ s(sh.log) + s(depth.cen) , random = ~ (1  |TOW_NO),data=data.29D,family = Gamma(link="log"))
# In fact depth doesn't help us out at all in any of the area 29 models, likely not the contrast we'd want
# in any one area, so we will go ahead with a model without depth for each area.
mod.2016.gam.29D.no.depth <- gamm4(wmw ~ s(sh.log)  , random = ~ (1  |TOW_NO),data=data.29D,family = Gamma(link="log"))
# Trying to just use depth as a surrogate for TOw doesn't work at all..
mod.2016.gam.29D.no.rand <- gam(wmw ~ s(sh.log) + s(depth.cen) ,data=data.29D,family = Gamma(link="log"))
AIC(mod.2016.gam.29D$mer,mod.2016.gam.29D.no.depth$mer,mod.2016.gam.29D.no.rand)
# So the model we'll use for the final analysis in area 29 is a gam with the log-shell height smoothed and without a depth term.
# I've run the no-depth model compared to the depth model for all areas of 29 and lthe non-depth model is best in all cases in 2016.

# Look at model results for the big gams, here we see that the no depth models look good, 
# paritally why I went forward with the individual gams without the depth in them for area 29.
# wmw-sh data.
windows(11,8.5)
par(mfrow=c(2,3))
plot(mod.2016.gam.by.area.no.depth$gam, scale = F, cex.lab = 1.5)  

# Grab the residuals and fitted values
E2016.gam <- resid(mod.2016.gam.by.area.no.depth$mer, type = "pearson")
E2016.glmer <- resid(current.mod)
F2016.gam <- fitted(mod.2016.gam.by.area.no.depth$mer)
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
boxplot(E2016.gam~ as.numeric(test.data$TOW_NO),pch=16,main="GAM")
abline(h = 0, col="blue",lty = 2) 
boxplot(E2016.glmer~ as.numeric(test.data$TOW_NO),pch=16,main="GLMER")
abline(h = 0, col="blue",lty = 2) 

# Residuals vs AREA
windows(11,8.5)
par(mfrow=c(2,1))
boxplot(E2016.gam~ as.numeric(test.data$STRATA),pch=16,main="GAM")
abline(h = 0, col="blue",lty = 2) 
boxplot(E2016.glmer~ as.numeric(test.data$STRATA),pch=16,main="GLMER")
abline(h = 0, col="blue",lty = 2) 

# Fitted vs. observerd by Area...
windows(11,8.5)
par(mfrow=c(2,3))
for(i in 1:length(areas.mwsh))
{
plot(F2016.gam[test.data$STRATA==areas.mwsh[i]],test.data$wmw[test.data$STRATA==areas.mwsh[i]],xlab = "Fitted values",
     ylab = paste("Pearson residuals - (",areas.mwsh[i],")")) 
abline(a=0,b=1,col="blue",lty=2)
} # end for(i in 1:length(areas.mwsh))
# What about lattitude
# There should be nothing here.
lat <- test.data$START_LAT
lon <- test.data$START_LONG
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
#### So now I"m moving to the growth modeling, for this I only want Area 29 as that's the area I'm comparing with the MW-SH
#### Need to get more sophisticated later on...

# Remove everything but area 29...
dat.long <- dat.long[grep("29",dat.long$SPA),]

# Let's only look at shells above 40 mm 
dat.long <- dat.long[dat.long$size >=40,]
# And we also need to account for multiple readings from one individual.
# First we need to make an "individual" ID.
dat.long$ID <- paste(dat.long$SPA,dat.long$Tow_No,dat.long$Scallop_No,sep="_")
head(dat.long)
# Get the data subset into useable bits...
# Note that I also am removing the "LY" data as shown above the chips make this data mostly unuseable for this analysis...
areas <- sort(unique(dat.long$SPA))
dat.long$std_size <- scale(dat.long$size_start,center=T,scale=F)
#dat.long$log_size <- log(dat.long$size)
size_range <- floor(min(dat.long$std_size,na.rm=T)-10):ceiling(max(dat.long$std_size,na.rm=T))+10
pred.dat <- data.frame(std_size = rep(size_range,length(areas)),
                       SPA=sort(rep(areas,length(size_range)))) # Some data to predict on for the gam...
#pred.dat$size <- round(exp(pred.dat$log_size)) # Make sure it's the exact number...
min.dat <- 50
useable.data <- aggregate(std_size~year,dat.long,FUN=function(x) which(length(x) >=min.dat))
useable.data <- as.character(na.omit(useable.data$year[useable.data$V1==1]))
# Remove the Last year from the data as the chipped shells make that data on it's own very poor.
useable.data <- useable.data[useable.data != "LY"]
dat.mod <- dat.long[dat.long$year %in% useable.data,]

# The model... this is basically an over-dispersed poisson model (random term enables the over-dispersion)
mod.res.rand <- gamm4(growth~ t2(std_size,by=SPA),
                      random =~ (1 | ID),
                      data=dat.mod,family = "poisson")
mod.res.rand.one.curve <- gamm4(growth~ t2(std_size),
                      random =~ (1 | ID),
                      data=dat.mod,family = "poisson")

AIC(mod.res.rand$mer,mod.res.rand.one.curve$mer) # The group of models is better than just one model, so 
# that suggests that there are differences in the shape of these curves between areas
# as per the wm-sh models, I will move to fit one model per area so we have a model for the 5 areas of 29.



##################################################  Section 4 Final Models and Prediction ##################################################  Section 4
##################################################  Section 4 Final Models and Prediction ##################################################  Section 4
##################################################  Section 4 Final Models and Prediction ##################################################  Section 4
##################################################  Section 4 Final Models and Prediction ##################################################  Section 4

################# Step 1  ################# Step 1  ################# Step 1  ################# Step 1  ################# Step 1  
#We get our meat weight shell height model and predictions for each year....
# Now the model predictions, first set up the loop and data
years <- as.character(unique(SFA29detail$YEAR))
num.years <- length(years)


# Open a plotting device.
pdf(file=paste(direct,"2017/Framework/Growth/Figures/MW_SH_Area29.pdf",sep=""),onefile=T)
# Loop for every year.
mod.mwsh.29 <- NULL
pred.mwsh <- NULL
tmp1 <- NULL
min.dat <- 20 # We need at least 20 samples for this to be worthwhile for an area.
for(j in 1:num.years)
{
  # Get the data and set up some nice variable names
  test.data <- SFA29detail[SFA29detail$YEAR == years[j],]
  test.data <- test.data[test.data$HEIGHT >=40,]
  test.data$sh.log <-log(test.data$HEIGHT)
  test.data$sh.log.cen <-test.data$sh.log-mean(test.data$sh.log)
  test.data$sh.cen <- scale(test.data$HEIGHT,scale=F,center=T)
  test.data$depth<-test.data$ADJ_DEPTH 
  test.data$depth.cen <- scale(test.data$ADJ_DEPTH,scale=F,center=T)
  #test.data$DEPTH.CTR<-test.data$DEPTH-mean(test.data$DEPTH)
  test.data$wmw <- test.data$WET_MEAT_WGT
  areas.mwsh <- sort(as.character(unique(test.data$STRATA)))


# Here's what the by area model looks like...
#windows(11,11)

  for(i in 1:length(areas.mwsh))
  {
    # get a variable
    tmp <- test.data[test.data$STRATA == areas.mwsh[i],]

    if(nrow(tmp) >= min.dat) # Only make the plots/models if we have at least "min.dat" points, see just above the loop...
    {
      # Run the model for each area if there is enough data!
      mod.mwsh.29[[areas.mwsh[i]]] <- gamm4(wmw ~ s(sh.log)  , random = ~ (1  |TOW_NO),data=tmp,family = Gamma(link="log"))
      # Get the predictions from the model.
      pred.mwsh[[areas.mwsh[i]]] <- data.frame(sh.log = log(seq(40,160,by=0.1)))
      pred.mwsh[[areas.mwsh[i]]]$sh <- round(exp(pred.mwsh[[areas.mwsh[i]]]$sh.log),digits=4)
      pred.mwsh[[areas.mwsh[i]]]$wmw <- predict(mod.mwsh.29[[areas.mwsh[i]]]$gam,
                                                              pred.mwsh[[areas.mwsh[i]]],type="response",re.form=NA)
      pred.mwsh[[areas.mwsh[i]]]$se <- predict(mod.mwsh.29[[areas.mwsh[i]]]$gam,
                                                             pred.mwsh[[areas.mwsh[i]]],type="response",re.form=NA,se=T)$se.fit
      pred.mwsh[[areas.mwsh[i]]]$SPA <- areas.mwsh[[i]] # Get the name of the area
      pred.mwsh[[areas.mwsh[i]]]$year <- years[j] # Get the year
      pred.mwsh[[areas.mwsh[i]]]$LCI.wmw <- pred.mwsh[[areas.mwsh[i]]]$wmw - 2* pred.mwsh[[areas.mwsh[i]]]$se
      pred.mwsh[[areas.mwsh[i]]]$UCI.wmw <- pred.mwsh[[areas.mwsh[i]]]$wmw + 2* pred.mwsh[[areas.mwsh[i]]]$se
      # Now make the plot
      if(i == 1) 
      {
        plot(wmw~HEIGHT,tmp,pch = 19,cex=0.5,ylim=range(SFA29detail$WET_MEAT_WGT,na.rm=T),xlim=range(SFA29detail$HEIGHT,na.rm=T),xaxt="n")
        if(i == 1) axis(1,at =(seq(40,170,by=10)),labels=seq(40,170,10))
        legend("topright",legend = areas.mwsh,col= plasma(length(areas.mwsh)),pch=19,lty=1:length(areas.mwsh), bty="n")
        
      } # end if(i == 1) 
      if(i > 1) points(wmw~HEIGHT,tmp,pch=19,col=plasma(length(areas.mwsh))[i],cex=0.5)
      
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
  tmp <- dat.long[dat.long$SPA == areas[i],]
  tmp$std_size <- scale(tmp$size,center=T,scale=F)
  #dat.long$log_size <- log(dat.long$size)
  size_range <- (min(tmp$std_size,na.rm=T)-10):(max(tmp$std_size,na.rm=T)+10)
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
    plot(growth~size,tmp,pch = 19,cex=0.5,ylim=range(dat.long$growth,na.rm=T),xlim=range(dat.long$size,na.rm=T),xaxt="n")
    if(i == 1) axis(1,at =(seq(40,170,by=10)),labels=seq(40,170,10))
    legend("topright",legend = areas,col= plasma(length(areas)),pch=19,lty=1:length(areas), bty="n")
    
  } # end if(i == 1) 
  if(i > 1) points(growth~size,tmp,pch=19,col=plasma(length(areas))[i],cex=0.5)
  lines(growth~size,pred.growth[[areas[[i]]]],col=plasma(length(areas))[i],lty=i,lwd=2)
  lines((growth+ 2*se)~size,pred.growth[[areas[[i]]]],col=plasma(length(areas))[i],lty=i,lwd=0.5)
  lines((growth- 2*se)~size,pred.growth[[areas[[i]]]],col=plasma(length(areas))[i],lty=i,lwd=0.5)  
}# end for(i in 1:length(areas))
growth.predictions <- do.call('rbind',pred.growth)


############## Step 3, we now have everythign we need to predict growth for all years...
# So to predict how much a shell will grow next year, we have several options here and I'm not sure what the best is...

size.2016 <- 100 # Well we do need to sort out what the average size of a scallop was in year X for recruits and fully-recruited/commerical.
size.2015 <- 100
# Predicted increase in biomass for the prediction year, assuming the current MW-SH model holds for next year
biomass.growth.2017 <- mwsh.predictions$wmw[mwsh.predictions$sh == round(growth.predictions[growth.predictions$size == size.2016,]$size.ny) & 
                                 mwsh.predictions$year == 2016]/ mwsh.predictions$wmw[mwsh.predictions$sh == size.2016 & mwsh.predictions$year == 2016]
# LCI for predicted change in biomass,assuming the current MW-SH model holds for next year
mwsh.predictions$wmw[mwsh.predictions$sh == round(growth.predictions[growth.predictions$size == size.2016,]$LCI.ny) & mwsh.predictions$year == 2016]/
  mwsh.predictions$wmw[mwsh.predictions$sh == size.2016 & mwsh.predictions$year == 2016]
# UCI for increase in biomass, this isn't quite right, I need to have last years MW-SH curve to get 
# our estimate of average wet meat weight in the previous year.
mwsh.predictions$wmw[mwsh.predictions$sh == round(growth.predictions[growth.predictions$size == size.2016,]$UCI.ny)& mwsh.predictions$year == 2016]/
  mwsh.predictions$wmw[mwsh.predictions$sh == size.2016 & mwsh.predictions$year == 2016]


# Alternatively we could base the growth on the mw-sh relationship from last year
# If I wanted to predict growth from last year how do we do it
# I think we need to assume that the average scallop from last year will grow from current size to next year size based on the wmw from last year, so...
biomass.growth.2016 <- mwsh.predictions$wmw[mwsh.predictions$sh == round(growth.predictions[growth.predictions$size == size.2015,]$size.ny) & 
                                mwsh.predictions$year == 2015]/mwsh.predictions$wmw[mwsh.predictions$sh == size.2015 & mwsh.predictions$year == 2015]
biomass.growth.2016.alt <- mwsh.predictions$wmw[mwsh.predictions$sh == round(growth.predictions[growth.predictions$size == size.2015,]$size.ny) & 
                                  mwsh.predictions$year == 2016]/mwsh.predictions$wmw[mwsh.predictions$sh == size.2015 & mwsh.predictions$year == 2015]

mwsh.predictions$wmw[mwsh.predictions$sh == size.2015 & mwsh.predictions$year == 2015]

# What if we just wanted to look at the suite of MW's for a given growth
tmp <- mwsh.predictions[mwsh.predictions$sh == round(growth.predictions[growth.predictions$size == size.2015,]$size.ny) & mwsh.predictions$SPA == "SFA29D" ,]
# So would the most sensible prediction simply be the average wmw in the past for a given shell height and account for the error around that
# to get at the uncertainty??  If we had some sort of covariate (plankton size) then we could account for that in our wmw estimates
# for the scallop... hmmm... that might just be the way forward, though I think that covariate should go into our wmw-sh model or our growth model
# or both, that way we get a nice answer right from those models, but of course for that we need a big model don't we... Hmm do we model a model
# or try to get at it within one model... let's go for a walk...
plot(tmp$#)
acf(tmp$wmw,plot=F) # No correlation, hmm...

## Here's my plan... think...
# 1:  We model the growth just as we do above, it's all good for this part, it's the interaction into the model I'm struggling with.
# 2:  We put the growth into the model... I'm so torn on how to do this it hurts at the moment, there's no great way to incorporate
#     all of this nice information into this given our current constraints, but I need to pick something!!
# 3:  For the prediction step it's easy, we simply make 3 prediction scenarios, the average scenarios, the low scenario and the high scenario
#     whether we show these I don't know, but if "condition" is low in a given year we have that scenario at our disposal, if it sounds like it is high
#     we have a different scenario.  
