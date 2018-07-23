# This script compares various models and makes nice plots of the BoF increment data that was measured in 2016.  This is the main
# testing script for looking at different models.  No need to re-run this unless adding/changing a model or we have new data, the 
# BoF_2016_initial_increment_modeling.RData has the key results saved

direct <- "d:/r/"
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
library(ggplot2)
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))#source the ScallopMap script (function containing maps and parameters)
source(paste(direct,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep=""))#source the ScallopMap script (function containing maps and parameters)

######### First get the survey information, what I am doing here is linking up the depth (so I can use it as a covariate in the model)


RODBCconn <-odbcConnect("ptran", uid=un.ID, pwd=pwd.ID,believeNRows=FALSE)
cruise.list <- c('GM2016','BI2016','BF2016','SFA292016') # Must be UPDATED for Current Year! # 
cruise.list <- paste(cruise.list,collapse="','")

#detailed meat weight/shell height sampling data
quer1 <- paste(
  "SELECT *                                     ",
  "FROM scallsur.scliveres                      ",                                                                                                                                   
  "WHERE (cruise in ('",cruise.list,"'))          ",
  sep=""
)
BoF.dat <- sqlQuery(RODBCconn, quer1, believeNRows=FALSE)  

# Get a unique ID and then remove everything but that ID and the depth which we want for merging into the growth data later...
BoF.dat$ID <- paste(BoF.dat$CRUISE,BoF.dat$TOW_NO, sep="_")
BoF.dat <- BoF.dat[,names(BoF.dat) %in% c("ID","DEPTH","START_LAT","START_LONG")]
BoF.dat$slat <- convert.dd.dddd(BoF.dat$START_LAT)
BoF.dat$slon <- convert.dd.dddd(BoF.dat$START_LONG)
BoF.dat <- BoF.dat[,-which(names(BoF.dat) %in% c("START_LAT","START_LONG"))]
#################  Now for the growth data#################  Now for the growth data#################  Now for the growth data
###  Now load in the growth data....
dat <- read.csv(paste(direct,"Data/Ageing/BoF_complete_ageing_2016.csv",sep=""),check.names=F,stringsAsFactors = F) # This data comes from the xlsx (DRAFTageingSFA292016.xlsx)
# Take a look at the data
head(dat)
names <- c("LY",as.character(na.omit(as.numeric(names(dat))))) #

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

# I need to get a unique tow ID, so combine Tow number and area, this allows for merging with database and a random effect to get added.
dat$ID <- paste(dat$Cruise,dat$Tow_No,sep="_")
# Now Merge the BoF.dat and dat so that the correct depth gets placed into a new column...
dat <- merge(dat,BoF.dat,by.x="ID")


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

# Make sure the data has no negatives!
dat.long[which(dat.long$growth <0),]

dat.tmp <- dat.long

# Let's only look at shells above 40 mm 
dat.tmp <- dat.tmp[dat.tmp$size >=40,]
# And we also need to account for multiple readings from one individual.
# First we need to make an "individual" ID.
dat.tmp$ID <- paste(dat.tmp$SPA,dat.tmp$Tow_No,dat.tmp$Scallop_No,sep="_")
head(dat.tmp)
# Get the data subset into useable bits...
# Note that I also am removing the "LY" data as shown above the chips make this data mostly unuseable for this analysis...
dat.tmp$std_size <- scale(dat.tmp$size_start,center=T,scale=F)
#dat.tmp$log_size <- log(dat.tmp$size)
size_range <- floor(min(dat.tmp$std_size,na.rm=T)-10):ceiling(max(dat.tmp$std_size,na.rm=T))+10
sizes <- size_range + attr(dat.tmp$std_size,"scaled:center")
min.dat <- 1
useable.data <- aggregate(std_size~year,dat.tmp,FUN=function(x) which(length(x) >=min.dat))
useable.data <- as.character(na.omit(useable.data$year[useable.data$V1==1]))
# Remove the Last year from the data as the chipped shells make that data on it's own very poor.
useable.data <- useable.data[useable.data != "LY"]
dat.mod <- dat.tmp[dat.tmp$year %in% useable.data,]
# Also remove any NA's in the growth side
dat.mod <- dat.mod[!is.na(dat.mod$growth),]
# Make area 29 one area rather than 5 subareas.
dat.mod$SPA[grep("29",dat.mod$SPA)] <- "29"
dat.mod$SPA[grep("4",dat.mod$SPA)] <- "4-5"
dat.mod$SPA[grep("5",dat.mod$SPA)] <- "4-5"
areas <- sort(unique(dat.mod$SPA)) #
dat.mod$SPA <- as.factor(dat.mod$SPA) # Need to make this a factor for the model to work...

# Now get the data to predict on
pred.dat <- data.frame(std_size = rep(size_range,length(areas)),size_start = rep(sizes,length(areas)),
                       SPA=sort(rep(areas,length(size_range)))) # Some data to predict on for the gam...

# The model... this is basically an over-dispersed poisson model (random term enables the over-dispersion)
# This random term is Scallop_No nested within Tow.  Basically I'm giving each scallop it's own "curve" (really I'm just allowing the intercept to vary)
# and these random effects are nested within the Tow, so again I allow the curve intercept to vary by tow.  If I'm understanding what I'm doing basically
# each scallop will be allowed to deviate from a tow average, and each tow is able to deviate from the area average.  The deviations
# for the Tow and Scallop are 0 mean and assumed to be normally distributed around 0.
mod.res.rand <- gamm4(growth~ t2(std_size,by=SPA),
                      random = ~ (1 | ID/Scallop_No),
                      data=dat.mod,family = "poisson")

mod.res.rand.one.curve <- gamm4(growth~ t2(std_size),
                                random = ~ (1 | ID/Scallop_No),
                                data=dat.mod,family = "poisson")

# Weird models.... Fit a curve for each year, probably should drop a large number of the years and just run it from 2010 or something like that.
dat.mod$year <- as.numeric(dat.mod$year)
dat.yr <- dat.mod[dat.mod$year > 2009,]
dat.yr$year <- dat.yr$year # Make this scale more nicely, 2012 is year 0
dat.yr$year <- as.factor(dat.yr$year)
years <- unique(dat.yr$year)
pred.year <- data.frame(std_size = rep(size_range,length(years)),size_start = rep(sizes,length(years)),
                        year=sort(rep(years,length(size_range)))) # Some data to predict on for the annual gam...


# This model won't converge unless we remove some of the years with little to know data, here's a model for just 2012 onwards as I suggest just above.
# Of course we can't compare with with other models since we have a different subset of data...
mod.res.rand.year <- gamm4(growth~ t2(std_size,by=year),
                           random = ~ (1 | ID/Scallop_No),
                           data=dat.yr,family = "poisson")
# Run the area model to compare with the annual model to do the comparison I need to use the same data tho...
mod.res.rand.year.subset <- gamm4(growth~ t2(std_size,by=SPA),
                           random = ~ (1 | ID/Scallop_No),
                           data=dat.yr,family = "poisson")

# Removed everything below 40 mm for the analysis.
mod.res.rand.40 <- gamm4(growth~ t2(std_size,by=SPA),
                         random = ~ (1 | ID/Scallop_No),
                         data=dat.mod[dat.mod$size_start >=40,],family = "poisson")

# A depth model, one curve, AIC shows it is not helpful at all, I believe this would only impact the intercept of
# the GAM model, it's not a particularly exciting model...
mod.res.rand.depth <- gamm4(growth~ t2(std_size) + DEPTH,
                           random = ~ (1 | ID/Scallop_No),
                           data=dat.mod,family = "poisson")


AIC(mod.res.rand$mer,mod.res.rand.one.curve$mer,mod.res.rand.depth$mer) # The model by area is best here...
AIC(mod.res.rand.year$mer,mod.res.rand.year.subset$mer) # The model using area is better according to AIC and fairly convincinly
# Now that doesn't mean looking at the annual patterns isn't something of interest.

# Get the predictions from the better model.
pred.dat$predicted <- predict(mod.res.rand$gam,pred.dat,type="response")
pred.dat$se <-  predict(mod.res.rand$gam,pred.dat,type="response",se=T)$se.fit
# Throw down some confidence intervals...
pred.dat$LCI <-  pred.dat$predicted - 2*pred.dat$se 
pred.dat$UCI <- pred.dat$predicted + 2*pred.dat$se

save.image(paste(direct,"Data/Framework/2017/Growth/BoF_2016_initial_increment_modeling.RData",sep=""))
load(paste(direct,"Data/Framework/2017/Growth/BoF_2016_initial_increment_modeling.RData",sep=""))
# Some figures
windows(11,8.5)
#par(mfrow=c(4,3))
plot(mod.res.rand$gam, scale = T, cex.lab=0.75,residuals=T,rug=T,pages=1,shade=T,ylim=c(-2,1))  

# Grab the residuals and fitted values
E2016.gam <- resid(mod.res.rand$mer, type = "pearson")
F2016.gam <- fitted(mod.res.rand$mer)

windows(11,8.5)
par(mfrow=c(2,3))
for(i in 1:length(areas))
{
  plot(F2016.gam[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],xlab = "Fitted values",
       ylab = paste("Actual Values - (",areas[i],")"),ylim=c(0,35),xlim=c(0,30)) 
  abline(a=0,b=1,col="blue",lty=2)
} # end for(i in 1:length(areas.mwsh))

# Here's a one panel plot showing the growth of scallop and how that varies by area, it's a ggplot!
# First I make a colour blind friendly palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add

# To use for line and point colors, add
scale_colour_manual(values=cbPalette)
windows(11,8.5)
p <- ggplot(pred.dat, aes(x=size_start, y=predicted, colour=SPA))  + scale_colour_manual(values=cbbPalette)  +
  scale_y_continuous(limits = c(0,36))
p
p <- p + geom_point(data=dat.mod, aes(x=size_start,y=growth,colour=SPA))
p
p <- p + geom_ribbon(aes(ymin=pred.dat$LCI, ymax=pred.dat$UCI), linetype=2, alpha=0.2)
#p <- p + geom_point(data=dat.mod, aes(x=size_start,y=growth,colour=SPA)) + scale_colour_manual(values=cbbPalette)#adds the points. 
p
p <- p + geom_line(size=1.25) #adds the points. 
p

windows(11,8.5)
par(mfrow=c(2,3),mar=c(2,2,1,0))
for(i in 1:length(areas))
{
  if(i %in% c(1))
  {
  plot(pred.dat$size[pred.dat$SPA==areas[i]],pred.dat$predicted[pred.dat$SPA==areas[i]],
       ylab = paste("Actual Values - (",areas[i],")"),type="l",ylim=c(0,35),col="blue",lwd=2,xaxt="n")
  points(dat.mod$size_start[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],pch=19,cex=0.2)
  axis(1,labels=NA)
  legend("topright",paste("Area", areas[i]),bty="n")
  } # end if(i %in% c(1))
  if(i %in% c(2,3))
  {
    plot(pred.dat$size[pred.dat$SPA==areas[i]],pred.dat$predicted[pred.dat$SPA==areas[i]],
         ylab = paste("Actual Values - (",areas[i],")"),type="l",ylim=c(0,35),col="blue",lwd=2,xaxt="n",yaxt="n")
    points(dat.mod$size_start[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],pch=19,cex=0.2)
    axis(2,labels=NA)
    axis(1,labels=NA)
    legend("topright",paste("Area", areas[i]),bty="n")
  } # end if(i %in% c(2,3))
  if(i %in% c(4))
  {
    plot(pred.dat$size[pred.dat$SPA==areas[i]],pred.dat$predicted[pred.dat$SPA==areas[i]],
         type="l",ylim=c(0,35),col="blue",lwd=2)
    points(dat.mod$size_start[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],pch=19,cex=0.2)
    legend("topright",paste("Area", areas[i]),bty="n")
  } # end if(i %in% c(4))
  if(i %in% c(5,6))
  {
    plot(pred.dat$size[pred.dat$SPA==areas[i]],pred.dat$predicted[pred.dat$SPA==areas[i]],
         type="l",ylim=c(0,35),col="blue",lwd=2,yaxt="n")
    points(dat.mod$size_start[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],pch=19,cex=0.2)
    axis(2,labels=NA)
    legend("topright",paste("Area", areas[i]),bty="n")
  } # end if(i %in% c(5,6))
} # end for(i in 1:length(areas.mwsh))



windows(11,8.5)
plot(F2016.gam,E2016.gam,xlab = "Fitted values",ylab = "Pearson residuals - GAM") # this is fairly solid
abline(h=0,col="blue",lty=2)

windows(11,8.5)
plot(F2016.gam~na.omit(dat.mod$growth),ylab="Fitted values",xlab="Observed Data - GAM") # Actually a bit of overestimatation for the largest meat weights now.
abline(a=0,b=1,col="blue",lty=2)

windows(11,11)
ScallopMap("inshore",plot.bathy = T,bathy.source = "usgs")
points(dat$slon,dat$slat,cex=1.2,pch=19,col="blue")



######################################################################################################################################################
##### Here is the plot for the model with an annual fit, from 2010-2015.
######################################################################################################################################################

# Get the predictions from the better model.
pred.year$predicted <- predict(mod.res.rand.year$gam,pred.year,type="response")
pred.year$se <-  predict(mod.res.rand.year$gam,pred.year,type="response",se=T)$se.fit
# Throw down some confidence intervals...
pred.year$LCI <-  pred.year$predicted - 2*pred.year$se 
pred.year$UCI <- pred.year$predicted + 2*pred.year$se


windows(11,8.5)
#par(mfrow=c(4,3))
plot(mod.res.rand.year$gam, scale = T, cex.lab=0.75,residuals=T,rug=T,pages=1,shade=T,ylim=c(-2,1))  

# Grab the residuals and fitted values
E2016.gam <- resid(mod.res.rand.year$mer, type = "pearson")
F2016.gam <- fitted(mod.res.rand.year$mer)

windows(11,8.5)
par(mfrow=c(2,3))
for(i in 1:length(years))
{
  plot(F2016.gam[dat.yr$year==years[i]],dat.yr$growth[dat.yr$year==years[i]],xlab = "Fitted values",
       ylab = paste("Actual Values - (",areas[i],")"),ylim=c(0,35),xlim=c(0,30)) 
  abline(a=0,b=1,col="blue",lty=2)
} # end for(i in 1:length(areas.mwsh))

# Here's a one panel plot showing the growth of scallop and how that varies by area, it's a ggplot!
# First I make a colour blind friendly palette
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# To use for fills, add

# To use for line and point colors, add
scale_colour_manual(values=cbPalette)
windows(11,8.5)
p <- ggplot(pred.year, aes(x=size_start, y=predicted, colour=year))  + scale_colour_manual(values=cbbPalette)  +
  scale_y_continuous(limits = c(0,36))
p
p <- p + geom_point(data=dat.yr, aes(x=size_start,y=growth,colour=year))
p
p <- p + geom_ribbon(aes(ymin=pred.year$LCI, ymax=pred.year$UCI), linetype=2, alpha=0.2)
#p <- p + geom_point(data=dat.mod, aes(x=size_start,y=growth,colour=SPA)) + scale_colour_manual(values=cbbPalette)#adds the points. 
p
p <- p + geom_line(size=1.25) #adds the points. 
p

windows(11,8.5)
par(mfrow=c(2,3),mar=c(2,2,1,0))
for(i in 1:length(years))
{
  if(i %in% c(1))
  {
    plot(pred.year$size[pred.year$year==years[i]],pred.year$predicted[pred.year$year==years[i]],
         ylab = paste("Actual Values - (",years[i],")"),type="l",ylim=c(0,35),col="blue",lwd=2,xaxt="n")
    points(dat.mod$size_start[dat.mod$year==years[i]],dat.mod$growth[dat.mod$year==years[i]],pch=19,cex=0.2)
    axis(1,labels=NA)
    legend("topright",paste("Area", years[i]),bty="n")
  } # end if(i %in% c(1))
  if(i %in% c(2,3))
  {
    plot(pred.year$size[pred.year$year==years[i]],pred.year$predicted[pred.year$year==years[i]],
         ylab = paste("Actual Values - (",years[i],")"),type="l",ylim=c(0,35),col="blue",lwd=2,xaxt="n",yaxt="n")
    points(dat.mod$size_start[dat.mod$year==years[i]],dat.mod$growth[dat.mod$year==years[i]],pch=19,cex=0.2)
    axis(2,labels=NA)
    axis(1,labels=NA)
    legend("topright",paste("Area", years[i]),bty="n")
  } # end if(i %in% c(2,3))
  if(i %in% c(4))
  {
    plot(pred.year$size[pred.year$year==years[i]],pred.year$predicted[pred.year$year==years[i]],
         type="l",ylim=c(0,35),col="blue",lwd=2)
    points(dat.mod$size_start[dat.mod$year==years[i]],dat.mod$growth[dat.mod$year==years[i]],pch=19,cex=0.2)
    legend("topright",paste("Area", years[i]),bty="n")
  } # end if(i %in% c(4))
  if(i %in% c(5,6))
  {
    plot(pred.year$size[pred.year$year==years[i]],pred.year$predicted[pred.year$year==years[i]],
         type="l",ylim=c(0,35),col="blue",lwd=2,yaxt="n")
    points(dat.mod$size_start[dat.mod$year==years[i]],dat.mod$growth[dat.mod$year==years[i]],pch=19,cex=0.2)
    axis(2,labels=NA)
    legend("topright",paste("Area", years[i]),bty="n")
  } # end if(i %in% c(5,6))
} # end for(i in 1:length(years))

