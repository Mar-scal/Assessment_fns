

direct <- "e:/r/"
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


plot(dat.long$growth ~ dat.long$size_start)

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
areas <- sort(unique(dat.tmp$SPA))
dat.tmp$std_size <- scale(dat.tmp$size_start,center=T,scale=F)
#dat.tmp$log_size <- log(dat.tmp$size)
size_range <- floor(min(dat.tmp$std_size,na.rm=T)-10):ceiling(max(dat.tmp$std_size,na.rm=T))+10
sizes <- size_range + attr(dat.tmp$std_size,"scaled:center")
pred.dat <- data.frame(std_size = rep(size_range,length(areas)),size = rep(sizes,length(areas)),
                       SPA=sort(rep(areas,length(size_range)))) # Some data to predict on for the gam...
#pred.dat$size <- round(exp(pred.dat$log_size)) # Make sure it's the exact number...
min.dat <- 1
useable.data <- aggregate(std_size~year,dat.tmp,FUN=function(x) which(length(x) >=min.dat))
useable.data <- as.character(na.omit(useable.data$year[useable.data$V1==1]))
# Remove the Last year from the data as the chipped shells make that data on it's own very poor.
useable.data <- useable.data[useable.data != "LY"]
dat.mod <- dat.tmp[dat.tmp$year %in% useable.data,]
# Also remove any NA's in the growth side
dat.mod <- dat.mod[!is.na(dat.mod$growth),]

# The model... this is basically an over-dispersed poisson model (random term enables the over-dispersion)
mod.res.rand <- gamm4(growth~ t2(std_size,by=SPA),
                      random =~ (1 | ID),
                      data=dat.mod,family = "poisson")
mod.res.rand.one.curve <- gamm4(growth~ t2(std_size),
                                random =~ (1 | ID),
                                data=dat.mod,family = "poisson")

AIC(mod.res.rand$mer,mod.res.rand.one.curve$mer) # The group of models is better than just one model, so 

# Get the predictions from the better model.
pred.dat$predicted <- predict(mod.res.rand$gam,pred.dat,type="response")
# Some figures
windows(11,8.5)
#par(mfrow=c(4,3))
plot(mod.res.rand$gam, scale = T, cex.lab=0.75,residuals=T,rug=T,pages=1,shade=T,ylim=c(-2,1))  

# Grab the residuals and fitted values
E2016.gam <- resid(mod.res.rand$mer, type = "pearson")
F2016.gam <- fitted(mod.res.rand$mer)

areas <- na.omit(as.character(unique(dat.tmp$SPA)))
windows(11,8.5)
par(mfrow=c(4,3))
for(i in 1:length(areas))
{
  plot(F2016.gam[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],xlab = "Fitted values",
       ylab = paste("Actual Values - (",areas[i],")")) 
  abline(a=0,b=1,col="blue",lty=2)
} # end for(i in 1:length(areas.mwsh))

# Here's a one panel plot showing the growth of scallop and how that varies by area
windows(11,8.5)
par(mfrow=c(1,1),mar=c(2,3,1,0))
cols <- c("black","blue","orange","grey","green","black","blue","orange","grey","green","black")
l.type <- c(rep(1,5),rep(2,5),3) 
p.type <- c(rep(21,5),rep(22,5),23) 
plot(dat.mod$size_start,dat.mod$growth, ylab = "Growth",type="n",ylim=c(0,35),col="lightgrey",pch=16,cex=0.5,xaxt="n'")
axis(1,at=seq(0,200,by=50))
axis(1,at=seq(0,200,by=10),labels=NA,tcl=-0.2)

for(i in 1:length(areas))
{
    points(dat.mod$size_start[dat.mod$SPA == areas[i]],dat.mod$growth[dat.mod$SPA == areas[i]], cex=0.5,pch=p.type[i],bg=cols[i],col=cols[i])
    lines(pred.dat$size[pred.dat$SPA==areas[i]],pred.dat$predicted[pred.dat$SPA==areas[i]],lwd=2,col=cols[i],lty=l.type[i])
}
legend("topright",as.character(areas),col=cols,lty=l.type,lwd=1,ncol=3,bty="n",pch = p.type,pt.bg = cols)

windows(11,8.5)
par(mfrow=c(4,3),mar=c(2,2,1,0))
for(i in 1:length(areas))
{
  if(i %in% c(1,4,7))
  {
  plot(pred.dat$size[pred.dat$SPA==areas[i]],pred.dat$predicted[pred.dat$SPA==areas[i]],
       ylab = paste("Actual Values - (",areas[i],")"),type="l",ylim=c(0,35),col="blue",lwd=2,xaxt="n")
  points(dat.mod$size_start[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],pch=19,cex=0.2)
  }
  if(i %in% c(10))
  {
    plot(pred.dat$size[pred.dat$SPA==areas[i]],pred.dat$predicted[pred.dat$SPA==areas[i]],
         type="l",ylim=c(0,35),col="blue",lwd=2)
    points(dat.mod$size_start[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],pch=19,cex=0.2)
  }
  if(i %in% c(9,11,12))
  {
    plot(pred.dat$size[pred.dat$SPA==areas[i]],pred.dat$predicted[pred.dat$SPA==areas[i]],
         type="l",ylim=c(0,35),col="blue",lwd=2,yaxt="n")
    points(dat.mod$size_start[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],pch=19,cex=0.2)
  }
  if(i %in% c(2,3,5,6,8))
  {
    plot(pred.dat$size[pred.dat$SPA==areas[i]],pred.dat$predicted[pred.dat$SPA==areas[i]],xlab = "Fitted values",
         type="l",ylim=c(0,35),col="blue",lwd=2,yaxt="n",xaxt="n")
    points(dat.mod$size_start[dat.mod$SPA==areas[i]],dat.mod$growth[dat.mod$SPA==areas[i]],pch=19,cex=0.2)
  }
 # abline(a=0,b=1,col="blue",lty=2)
} # end for(i in 1:length(areas.mwsh))



windows(11,8.5)
plot(F2016.gam,E2016.gam,xlab = "Fitted values",ylab = "Pearson residuals - GAM") # this is fairly solid
abline(h=0,col="blue",lty=2)

windows(11,8.5)
plot(F2016.gam~na.omit(dat.mod$growth),xlab="Fitted values",ylab="Observed Data - GAM") # Actually a bit of overestimatation for the largest meat weights now.
abline(a=0,b=1,col="blue",lty=2)
