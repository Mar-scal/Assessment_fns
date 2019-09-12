##################################################   Survey Tow-by-tow #########################################################		
##################################################   Survey Tow-by-tow #########################################################		
##################################################   Survey Tow-by-tow #########################################################		
# Here I am looking at a series of diagnostics to get a deep understanding of what is happening with the survey data.
# The first part (and perhaps in the end the only stuff in here) is looking tow by tow to get a feel for what 
# happened in each tow for each bank on the survey. 

# Update History
# May 16, 2016:  Set up the ScallopMap function to select database you want to use by setting it at the start of the script.
# enables calling the database using 64 bit R.
# June-July 2017:  Added code so that the directory would be created automatically and various other changes so code works with updated results and new bins...
# July 2018:  Minor updates to account for the possibility of having multiple strata for a bank, updated to use the most recent strata...
yr = as.numeric(format(Sys.time(), "%Y"))  # 
direct = "C:/Documents/Offshore scallop/Assessment/"
database = "ptran" # Set this to your database, needed for a few called to get the bathymetry from ScallopMap. You'll also
# need to set your username/password to access the database, see the ScallopMap function calls.
library(viridis) # for colors...
require(RColorBrewer)

# Load the survey data.  If you've compiled all the surveys use this...
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/testing_results_SCALOFF_LE09.Rdata",sep=""))
# Alternatively you might need to load one of these instead.
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/testing_results.Rdata",sep=""))
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_summer_results.Rdata",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))

# You may need to reload your R profile if you use it...
#source("d:/r/.Rprofile")
# bnk <- c("GBa","GBb")# Once we have spring 2016 survey completed we should be able to add "Sab","BBs","Mid".
 bnk <- c("BBn")#,"Ger","Sab","Mid","GB", "Ban", "BanIce") #"BBs", 
#          "GBa", "GBb")
# bnk <- c("GBa", "GBb", "GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core")
# bnk <- "GB"
# bnk <- "Ger"
cf.lab <-    expression(paste("CF:",bgroup("(",frac(g,dm^3)   ,")")))
mc.lab <-    expression(paste("MC:",bgroup("(",frac(N,"500 g"),")"))) 

cf.data2 <- cf.data

for(i in 1:length(bnk))
{
  print(bnk[i])
  # Set up the directory to save the figures.
  # Get the plot directory
  plot.dir = paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures",bnk[i],"/",sep="")
  # If the above directory does not exist then we need to create it.
  if(dir.exists(plot.dir)==F)
  {
    # This enables us to create the base specified directory on up...
    if(dir.exists(direct) ==F) dir.create(direct)
    if(dir.exists(paste(direct,yr,sep="")) ==F) dir.create(paste(direct,yr,sep=""))
    if(dir.exists(paste(direct,yr,"/Presentations",sep="")) ==F) dir.create(paste(direct,yr,"/Presentations",sep=""))
    if(dir.exists(paste(direct,yr,"/Presentations/Survey_summary",sep="")) ==F) dir.create(paste(direct,yr,"/Presentations/Survey_summary",sep=""))
    if(dir.exists(paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",sep="")) ==F) dir.create(paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",sep=""))
    dir.create(paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],sep=""))
  } # end if(dir.exists(plot.dir)==F)
  
# Grab the survey data if it exists
survey.poly <- subset(survey.detail.polys, label == bnk[i])
survey.inf <- subset(survey.info, label == bnk[i])
# In case we have restratified we'll need to grab the most recet restrat data.
if(yr>2017) max.year <- max(survey.poly$startyear)
if(yr>2017) bank.survey.poly <- survey.poly[survey.poly$startyear == max.year,]
if(yr>2017) bank.survey.info <- survey.inf[survey.inf$startyear == max.year,]
if(yr==2017) bank.survey.poly <- survey.poly
if(yr==2017) bank.survey.info <- survey.inf
if(yr<2017) bank.survey.poly <- NA
if(yr<2017) bank.survey.info <- NA
# If there isn't any data make it an NA
if(nrow(survey.poly) == 0) bank.survey.poly <- NA
if(nrow(survey.inf) == 0) bank.survey.info <- NA

if(bnk[i] %in% c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core")) {
  bound.poly.surv <- as.PolySet(bound.surv.poly[[bnk[i]]],projection ="LL")
}

# Here I am exploring the data to see if there is anything odd happening!
# This has the nice summary of all the tow data in it, subset into 2015 and let's look at what is happening in each tow.
bank.live<- subset(surv.Live[[bnk[i]]],year==yr)
bank.live <- bank.live[order(bank.live$tow),]
# To minimize code changes I will make a new variable called Strata_ID for any bank which has been restratified (so far just Sable) given we'll only be
# running this script for the most recent year this works fine.
if(any(names(bank.live) == "Strata_ID_new")) bank.live$Strata_ID <- bank.live$Strata_ID_new

bank.clap <- subset(surv.Clap[[bnk[i]]],year==yr)
bank.clap <- bank.clap[order(bank.clap$tow),]
# To minimize code changes I will make a new variable called Strata_ID for any bank which has been restratified (so far just Sable) given we'll only be
# running this script for the most recent year this works fine.
if(any(names(bank.clap) == "Strata_ID_new")) bank.clap$Strata_ID <- bank.clap$Strata_ID_new

print("1")

# Get the columns with the shell height frequencies.
cols <- which(names(bank.clap)=="h5"):which(names(bank.clap)=="h200")
# And the MW data for each shelf.
bank.mw <-mw.dat.all[[bnk[i]]][mw.dat.all[[bnk[i]]]$year==yr,]
if(bnk[i] == "BanIce") bank.mw <- MW.dat.new[MW.dat.new$bank=="BanIce" & MW.dat.new$year==yr,]
bank.mw$CF_samp <- bank.mw$wmw/(bank.mw$sh/100)^3

# Check out the Clappers by "real" CF from the bank.mw...
# This gets the mean CF for each tow and adds that to the data 
tmp<- tapply(bank.mw$CF_samp, bank.mw$tow,mean)

cf.data <- as.data.frame(1:length(tmp))
cf.data$tow <- as.numeric(names(tmp))
cf.data$cf <- as.numeric(tmp)
cf.data <- cf.data[,-1]
cf.dat <- merge(bank.clap,cf.data,by.x="tow")
cf.dat <- subset(cf.dat,select = c("clap.prop","clap.propCom","clap.propRec","clap.propPre","tow","cf"))

###boxes bring it over if it isnt already written

##  Now start making the figures.
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/SHF.pdf",sep=""),onefile=T,width=11,height=8.5)
par(mfrow=c(1,2))
for(j in seq(1,length(bank.live[,1])))
{
  # Get both plots on the same scale...
  ymax <- max(max(bank.clap[j,cols],na.rm=T),
              max(bank.live[j,cols],na.rm=T))
  plot(t(bank.live[j,cols]),type="h",xaxt="n",xlab="Shell Height",ylab="Number",ylim=c(0,ymax),bty="L")
  axis(1,at=1:40,labels=F,tcl=0.2)
  axis(1,at=1:40,labels=F,tcl=-0.2)
  axis(1,at = seq(3,40,by=5),labels = names(bank.live[j,seq(16,55,by=5)]))
  title(paste("Standardized Number alive per tow (tow ID-",bank.live$tow[j],")",sep=""),cex=0.6)

  plot(t(bank.clap[j,cols]),type="h",xaxt="n",xlab="Shell Height",ylab="Number",ylim=c(0,ymax),bty="L")
  axis(1,at=1:40,labels=F,tcl=0.2)
  axis(1,at=1:40,labels=F,tcl=-0.2)
  axis(1,at = seq(3,40,by=5),labels = names(bank.clap[j,seq(16,55,by=5)]))
  title(paste("Standardized Number of Clappers per tow (tow ID-",bank.clap$tow[j],")",sep=""),cex=0.6)
} # end for(i in seq(1,length(bank.live[,1])))
dev.off()
print("2")

# Look at Condition factor from each tow, any stand out higher or low?
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/CF_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
#windows(11,8.5)
plot(1:length(bank.live$CF),bank.live$CF,xaxt="n",xlab="tow ID", ylab="CF",bty="L",pch=20,type="n")
text(1:length(bank.live$CF),bank.live$CF,bank.live$tow,cex=0.5)
axis(1,at=seq(5,length(bank.live$CF),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$CF),by=5)]))
title("Condition Factor by tow")
abline(h=mean(bank.live$CF,na.rm=T),col="blue",lty=2,lwd=1.5)

dev.off()


# Look at Condition factor from each tow but label as depth, any stand out higher or low?
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/CF_by_tow and_depth.png",sep=""),width=11,height=8.5, units="in", res=400)
#windows(11,8.5)
plot(1:length(bank.live$CF),bank.live$CF,xaxt="n",xlab="tow ID", ylab="CF",bty="L",pch=20,type="n")
text(1:length(bank.live$CF),bank.live$CF,round(bank.live$depth),cex=0.5)
axis(1,at=seq(5,length(bank.live$CF),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$CF),by=5)]))
title("Condition Factor by tow and depth")
abline(h=mean(bank.live$CF,na.rm=T),col="blue",lty=2,lwd=1.5)

dev.off()

# Let's look at some clapper trends if we have the data...
if(is.null(bank.clap$clap.prop)==F)
{
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_tow and_depth.png",sep=""),
    width=11,height=8.5, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,4,1,1))
plot(1:length(bank.clap$clap.prop),bank.clap$clap.prop,xaxt="n",xlab="tow ID",
     ylab="% dead",bty="L",pch=20,type="n",ylim=c(0,100))
text(1:length(bank.clap$clap.prop),bank.clap$clap.prop,round(bank.clap$depth),cex=0.8)
text(10,80,"Overall")
title("Clappers by tow and depth")
axis(1,at=seq(5,length(bank.clap$clap.prop),by=5),
     labels = c(bank.clap$tow[seq(5,length(bank.clap$clap.prop),by=5)]))
plot(1:length(bank.clap$clap.propCom),bank.clap$clap.propCom,xaxt="n",xlab="tow ID",
     ylab="% dead",bty="L",pch=20,type="n",ylim=c(0,100))
text(1:length(bank.clap$clap.propCom),bank.clap$clap.propCom,round(bank.clap$depth),cex=0.8)
text(10,80,"Fully recruited")
axis(1,at=seq(5,length(bank.clap$clap.prop),by=5),
     labels = c(bank.clap$tow[seq(5,length(bank.clap$clap.prop),by=5)]))
plot(1:length(bank.clap$clap.propRec),bank.clap$clap.propRec,xaxt="n",xlab="tow ID",
     ylab="% dead",bty="L",pch=20,type="n",ylim=c(0,100))
text(1:length(bank.clap$clap.propRec),bank.clap$clap.propRec,round(bank.clap$depth),cex=0.8)
text(10,80,"Recruits")
axis(1,at=seq(5,length(bank.clap$clap.prop),by=5),
     labels = c(bank.clap$tow[seq(5,length(bank.clap$clap.prop),by=5)]))
plot(1:length(bank.clap$clap.propPre),bank.clap$clap.propPre,xaxt="n",xlab="tow ID",
     ylab="% dead",bty="L",pch=20,type="n",ylim=c(0,100))
text(1:length(bank.clap$clap.propPre),bank.clap$clap.propPre,round(bank.clap$depth),cex=0.8)
text(10,80,"Pre-recruits")
axis(1,at=seq(5,length(bank.clap$clap.prop),by=5),
     labels = c(bank.clap$tow[seq(5,length(bank.clap$clap.prop),by=5)]))
dev.off()

print("3")
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,4,1,1))
plot(1:length(bank.clap$clap.prop),bank.clap$clap.prop,xaxt="n",xlab="tow ID",
     ylab="% dead",bty="L",pch=20,type="n",ylim=c(0,100))
text(1:length(bank.clap$clap.prop),bank.clap$clap.prop,round(bank.clap$tow),cex=0.8)
text(10,80,"Overall")
title("Clappers by tow")
axis(1,at=seq(5,length(bank.clap$clap.prop),by=5),
     labels = c(bank.clap$tow[seq(5,length(bank.clap$clap.prop),by=5)]))
plot(1:length(bank.clap$clap.propCom),bank.clap$clap.propCom,xaxt="n",xlab="tow ID",
     ylab="% dead",bty="L",pch=20,type="n",ylim=c(0,100))
text(1:length(bank.clap$clap.propCom),bank.clap$clap.propCom,round(bank.clap$tow),cex=0.8)
text(10,80,"Fully recruited")
axis(1,at=seq(5,length(bank.clap$clap.prop),by=5),
     labels = c(bank.clap$tow[seq(5,length(bank.clap$clap.prop),by=5)]))
plot(1:length(bank.clap$clap.propRec),bank.clap$clap.propRec,xaxt="n",xlab="tow ID",
     ylab="% dead",bty="L",pch=20,type="n",ylim=c(0,100))
text(1:length(bank.clap$clap.propRec),bank.clap$clap.propRec,round(bank.clap$tow),cex=0.8)
text(10,80,"Recruits")
axis(1,at=seq(5,length(bank.clap$clap.prop),by=5),
     labels = c(bank.clap$tow[seq(5,length(bank.clap$clap.prop),by=5)]))
plot(1:length(bank.clap$clap.propPre),bank.clap$clap.propPre,xaxt="n",xlab="tow ID",
     ylab="% dead",bty="L",pch=20,type="n",ylim=c(0,100))
text(1:length(bank.clap$clap.propPre),bank.clap$clap.propPre,round(bank.clap$tow),cex=0.8)
text(10,80,"Pre-recruits")
axis(1,at=seq(5,length(bank.clap$clap.prop),by=5),
     labels = c(bank.clap$tow[seq(5,length(bank.clap$clap.prop),by=5)]))
dev.off()


############
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_depth.png",sep=""),width=8.5,height=11, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,6,1,1))
plot(bank.clap$depth,bank.clap$clap.prop,
     xaxt="n",xlab="depth", ylab="% dead",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.prop,na.rm=T),max(bank.clap$clap.prop,na.rm=T)),yaxt="n")
text(bank.clap$depth,bank.clap$clap.prop,
     round(bank.clap$tow),cex=0.8)
# Because this is log scale this is complicated by 0's!

text(min(bank.clap$depth)*1.2,max(bank.clap$clap.prop,na.rm=T),"Total")
title("Clappers by depth")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.prop,na.rm=T),lwd=2,lty=2,col="blue")

# Now fully recruited
plot(bank.clap$depth,bank.clap$clap.propCom,
     xaxt="n",xlab="depth", ylab="% dead",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propCom,na.rm=T),max(bank.clap$clap.propCom,na.rm=T)),yaxt="n")
text(bank.clap$depth,bank.clap$clap.propCom,
     round(bank.clap$tow),cex=0.8)
text(min(bank.clap$depth)*1.2,max(bank.clap$clap.propCom,na.rm=T),"Fully Recruited")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propCom,na.rm=T),lwd=2,lty=2,col="blue")

# Now the recruits
plot(bank.clap$depth,bank.clap$clap.propRec,
     xaxt="n",xlab="depth", ylab="% dead",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propRec,na.rm=T),max(bank.clap$clap.propRec,na.rm=T)),yaxt="n")
text(bank.clap$depth,bank.clap$clap.propRec,
     round(bank.clap$tow),cex=0.8)
text(min(bank.clap$depth)*1.2,max(bank.clap$clap.propRec,na.rm=T),"Recruits")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propRec,na.rm=T),lwd=2,lty=2,col="blue")

print("4")
# Now the Pre-recruits
plot(bank.clap$depth,bank.clap$clap.propPre,
     xaxt="n",xlab="depth", ylab="% dead",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propPre,na.rm=T),max(bank.clap$clap.propPre,na.rm=T)),yaxt="n")
text(bank.clap$depth,bank.clap$clap.propPre,
     round(bank.clap$tow),cex=0.8)
text(min(bank.clap$depth)*1.2,max(bank.clap$clap.propPre,na.rm=T),"Pre-recruits")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propPre,na.rm=T),lwd=2,lty=2,col="blue")

dev.off()
############


############
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_total_abundance.png",sep=""),
    width=8.5,height=11, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,6,1,1))
plot(bank.live$tot,bank.clap$clap.prop,
     xaxt="n",xlab="Abundance", ylab="% dead",log="x",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.prop,na.rm=T),max(bank.clap$clap.prop,na.rm=T)),yaxt="n")
text(bank.live$tot,bank.clap$clap.prop,
     round(bank.clap$tow),cex=0.8)

text(max(bank.live$tot)*0.8,max(bank.clap$clap.prop,na.rm=T),"Total")
title("Clappers by Total Abundance")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.prop,na.rm=T),lwd=2,lty=2,col="blue")

# Now fully recruited
plot(bank.live$tot,bank.clap$clap.propCom,
     xaxt="n",xlab="Abundance", ylab="% dead",log="x",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propCom,na.rm=T),max(bank.clap$clap.propCom,na.rm=T)),yaxt="n")
text(bank.live$tot,bank.clap$clap.propCom,
     round(bank.clap$tow),cex=0.8)
text(max(bank.live$tot)*0.8,max(bank.clap$clap.propCom,na.rm=T),"Fully Recruited")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propCom,na.rm=T),lwd=2,lty=2,col="blue")

# Now the recruits
plot(bank.live$tot,bank.clap$clap.propRec,
     xaxt="n",xlab="Abundance", ylab="% dead",log="x",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propRec,na.rm=T),max(bank.clap$clap.propRec,na.rm=T)),yaxt="n")
text(bank.live$tot,bank.clap$clap.propRec,
     round(bank.clap$tow),cex=0.8)
text(max(bank.live$tot)*0.8,max(bank.clap$clap.propRec,na.rm=T),"Recruits")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propRec,na.rm=T),lwd=2,lty=2,col="blue")

# Now the Pre-recruits
plot(bank.live$tot,bank.clap$clap.propPre,
     xaxt="n",xlab="Abundance", ylab="% dead",log="x",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propPre,na.rm=T),max(bank.clap$clap.propPre,na.rm=T)),yaxt="n")
text(bank.live$tot,bank.clap$clap.propPre,
     round(bank.clap$tow),cex=0.8)
text(max(bank.live$tot)*0.8,max(bank.clap$clap.propPre,na.rm=T),"Pre-recruits")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propPre,na.rm=T),lwd=2,lty=2,col="blue")

dev.off()
############



# This one is a little funny, just be sure the data frames are aligned, which they should be
#############
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_fully_recruited_abundance.png",sep=""),
    width=8.5,height=11, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,6,1,1))

plot(bank.live$com[bank.live$com>0],bank.clap$clap.prop[bank.live$com>0],
     xaxt="n",xlab="depth", ylab="Total",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.prop,na.rm=T),
                                    max(bank.clap$clap.prop,na.rm=T)),
     xlim=c(0.1*min(bank.live$com[bank.live$com>0],na.rm=T),
            5*max(bank.clap$com,na.rm=T)),
     log="x",yaxt="n")
text(bank.live$com[bank.live$com>0],bank.clap$clap.prop[bank.live$com>0],
     round(bank.live$tow[bank.live$com>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$com[bank.live$com==0]) > 0)
{
  text((0*bank.live$com[bank.live$com==0]+0.1*min(bank.live$com[bank.live$com>0],na.rm=T)),
       bank.clap$clap.prop[bank.live$com==0],
       round(bank.clap$tow[bank.live$com==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(0.2*min(bank.live$com[bank.live$com>0]),max(bank.clap$clap.prop,na.rm=T),"Total")
title("Clappers by Fully recruited abundance")
axis(2)
axis(1,at=axTicks(1,nintLog=10),labels=c(0,axTicks(1,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.prop,na.rm=T),lwd=2,lty=2,col="blue")

print("5")

# Now fully recruited
plot(bank.live$com[bank.live$com>0],bank.clap$clap.propCom[bank.live$com>0],
     xaxt="n",xlab="Fully Recruited abunance", ylab="% dead",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propCom,na.rm=T),
                                    max(bank.clap$clap.propCom,na.rm=T)),
     xlim=c(0.1*min(bank.live$com[bank.live$com>0],na.rm=T),
            5*max(bank.clap$com,na.rm=T)),
     log="x",yaxt="n")
text(bank.live$com[bank.live$com>0],bank.clap$clap.propCom[bank.live$com>0],
     round(bank.live$tow[bank.live$com>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$com[bank.live$com==0]) > 0)
{
  text((0*bank.live$com[bank.live$com==0]+0.1*min(bank.live$com[bank.live$com>0],na.rm=T)),
       bank.clap$clap.propCom[bank.live$com==0],
       round(bank.clap$tow[bank.live$com==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(0.2*min(bank.live$com[bank.live$com>0]),max(bank.clap$clap.propCom,na.rm=T),"Fully recruited")
axis(2)
axis(1,at=axTicks(1,nintLog=10),labels=c(0,axTicks(1,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propCom,na.rm=T),lwd=2,lty=2,col="blue")

# Now the recruits
plot(bank.live$com[bank.live$com>0],bank.clap$clap.propRec[bank.live$com>0],
     xaxt="n",xlab="depth", ylab="Total",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propRec,na.rm=T),
                                    max(bank.clap$clap.propRec,na.rm=T)),
     xlim=c(0.1*min(bank.live$com[bank.live$com>0],na.rm=T),
            5*max(bank.clap$com,na.rm=T)),
     log="x",yaxt="n")
text(bank.live$com[bank.live$com>0],bank.clap$clap.propRec[bank.live$com>0],
     round(bank.live$tow[bank.live$com>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$com[bank.live$com==0]) > 0)
{
  text((0*bank.live$com[bank.live$com==0]+0.1*min(bank.live$com[bank.live$com>0],na.rm=T)),
       bank.clap$clap.propRec[bank.live$com==0],
       round(bank.clap$tow[bank.live$com==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(0.2*min(bank.live$com[bank.live$com>0]),max(bank.clap$clap.propRec,na.rm=T),"Recruits")
axis(2)
axis(1,at=axTicks(1,nintLog=10),labels=c(0,axTicks(1,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propRec,na.rm=T),lwd=2,lty=2,col="blue")

# Now the Pre-recruits
plot(bank.live$com[bank.live$com>0],bank.clap$clap.propPre[bank.live$com>0],
     xaxt="n",xlab="depth", ylab="Total",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propPre,na.rm=T),
                                    max(bank.clap$clap.propPre,na.rm=T)),
     xlim=c(0.1*min(bank.live$com[bank.live$com>0],na.rm=T),
            5*max(bank.clap$com,na.rm=T)),
     log="x",yaxt="n")
text(bank.live$com[bank.live$com>0],bank.clap$clap.propPre[bank.live$com>0],
     round(bank.live$tow[bank.live$com>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$com[bank.live$com==0]) > 0)
{
  text((0*bank.live$com[bank.live$com==0]+0.1*min(bank.live$com[bank.live$com>0],na.rm=T)),
       bank.clap$clap.propPre[bank.live$com==0],
       round(bank.clap$tow[bank.live$com==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(0.2*min(bank.live$com[bank.live$com>0]),max(bank.clap$clap.propPre,na.rm=T),"Pre-recruits")
axis(2)
axis(1,at=axTicks(1,nintLog=10),labels=c(0,axTicks(1,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propPre,na.rm=T),lwd=2,lty=2,col="blue")
dev.off()



# This one is a little funny, just be sure the data frames are aligned, which they should be
#############
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_pre_recruit_abundance.png",sep=""),
    width=8.5,height=11, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,6,1,1))

plot(bank.live$pre[bank.live$pre>0],bank.clap$clap.prop[bank.live$pre>0],
     xaxt="n",xlab="depth", ylab="Total",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.prop,na.rm=T),
                                    max(bank.clap$clap.prop,na.rm=T)),
     xlim=c(0.1*min(bank.live$pre[bank.live$pre>0],na.rm=T),
            5*max(bank.clap$pre,na.rm=T)),
     log="x",yaxt="n")
text(bank.live$pre[bank.live$pre>0],bank.clap$clap.prop[bank.live$pre>0],
     round(bank.live$tow[bank.live$pre>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$pre[bank.live$pre==0]) > 0)
{
  text((0*bank.live$pre[bank.live$pre==0]+0.1*min(bank.live$pre[bank.live$pre>0],na.rm=T)),
       bank.clap$clap.prop[bank.live$pre==0],
       round(bank.clap$tow[bank.live$pre==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(0.2*min(bank.live$pre[bank.live$pre>0]),max(bank.clap$clap.prop,na.rm=T),"Total")
title("Clappers by Pre-recruit abundance")
axis(2)
axis(1,at=axTicks(1,nintLog=10),labels=c(0,axTicks(1,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.prop,na.rm=T),lwd=2,lty=2,col="blue")

print("6")

# Now fully recruited
plot(bank.live$pre[bank.live$pre>0],bank.clap$clap.propCom[bank.live$pre>0],
     xaxt="n",xlab="Fully Recruited abunance", ylab="% dead",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propCom,na.rm=T),
                                    max(bank.clap$clap.propCom,na.rm=T)),
     xlim=c(0.1*min(bank.live$pre[bank.live$pre>0],na.rm=T),
            5*max(bank.clap$pre,na.rm=T)),
     log="x",yaxt="n")
text(bank.live$pre[bank.live$pre>0],bank.clap$clap.propCom[bank.live$pre>0],
     round(bank.live$tow[bank.live$pre>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$pre[bank.live$pre==0]) > 0)
{
  text((0*bank.live$pre[bank.live$pre==0]+0.1*min(bank.live$pre[bank.live$pre>0],na.rm=T)),
       bank.clap$clap.propCom[bank.live$pre==0],
       round(bank.clap$tow[bank.live$pre==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(0.2*min(bank.live$pre[bank.live$pre>0]),max(bank.clap$clap.propCom,na.rm=T),"Fully recruited")
axis(2)
axis(1,at=axTicks(1,nintLog=10),labels=c(0,axTicks(1,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propCom,na.rm=T),lwd=2,lty=2,col="blue")

# Now the recruits
plot(bank.live$pre[bank.live$pre>0],bank.clap$clap.propRec[bank.live$pre>0],
     xaxt="n",xlab="depth", ylab="Total",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propRec,na.rm=T),
                                    max(bank.clap$clap.propRec,na.rm=T)),
     xlim=c(0.1*min(bank.live$pre[bank.live$pre>0],na.rm=T),
            5*max(bank.clap$pre,na.rm=T)),
     log="x",yaxt="n")
text(bank.live$pre[bank.live$pre>0],bank.clap$clap.propRec[bank.live$pre>0],
     round(bank.live$tow[bank.live$pre>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$pre[bank.live$pre==0]) > 0)
{
  text((0*bank.live$pre[bank.live$pre==0]+0.1*min(bank.live$pre[bank.live$pre>0],na.rm=T)),
       bank.clap$clap.propRec[bank.live$pre==0],
       round(bank.clap$tow[bank.live$pre==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(0.2*min(bank.live$pre[bank.live$pre>0]),max(bank.clap$clap.propRec,na.rm=T),"Recruits")
axis(2)
axis(1,at=axTicks(1,nintLog=10),labels=c(0,axTicks(1,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propRec,na.rm=T),lwd=2,lty=2,col="blue")

# Now the Pre-recruits
plot(bank.live$pre[bank.live$pre>0],bank.clap$clap.propPre[bank.live$pre>0],
     xaxt="n",xlab="depth", ylab="Total",
     bty="L",pch=20,type="n",ylim=c(min(bank.clap$clap.propPre,na.rm=T),
                                    max(bank.clap$clap.propPre,na.rm=T)),
     xlim=c(0.1*min(bank.live$pre[bank.live$pre>0],na.rm=T),
            5*max(bank.clap$pre,na.rm=T)),
     log="x",yaxt="n")
text(bank.live$pre[bank.live$pre>0],bank.clap$clap.propPre[bank.live$pre>0],
     round(bank.live$tow[bank.live$pre>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$pre[bank.live$pre==0]) > 0)
{
  text((0*bank.live$pre[bank.live$pre==0]+0.1*min(bank.live$pre[bank.live$pre>0],na.rm=T)),
       bank.clap$clap.propPre[bank.live$pre==0],
       round(bank.clap$tow[bank.live$pre==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(0.2*min(bank.live$pre[bank.live$pre>0]),max(bank.clap$clap.propPre,na.rm=T),"Pre-recruits")
axis(2)
axis(1,at=axTicks(1,nintLog=10),labels=c(0,axTicks(1,nintLog=10)[-1]),las=1)
abline(h=mean(bank.clap$clap.propPre,na.rm=T),lwd=2,lty=2,col="blue")

dev.off()
###############

} # end if(is.null(bank.clap$clap.prop)==F)

### Now I'd also like to look at total number of clappers as well (biomass being less interesting)...
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clapper_numbers_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,4,1,1))
plot(1:length(bank.clap$tot),bank.clap$tot,xaxt="n",xlab="tow ID", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.clap$tot,na.rm=T)))
text(1:length(bank.clap$tot),bank.clap$tot,round(bank.clap$tow),cex=0.8)
text(10,max(bank.clap$tot,na.rm=T),"Overall")
title("Clappers by tow")
axis(1,at=seq(5,length(bank.clap$tot),by=5),labels = c(bank.clap$tow[seq(5,length(bank.clap$tow),by=5)]))

plot(1:length(bank.clap$com),bank.clap$com,xaxt="n",xlab="tow ID", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.clap$com,na.rm=T)))
text(1:length(bank.clap$com),bank.clap$com,round(bank.clap$tow),cex=0.8)
text(10,max(bank.clap$com,na.rm=T),"Fully recruited")
axis(1,at=seq(5,length(bank.clap$com),by=5),labels = c(bank.clap$tow[seq(5,length(bank.clap$tow),by=5)]))

plot(1:length(bank.clap$rec),bank.clap$rec,xaxt="n",xlab="tow ID", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.clap$rec,na.rm=T)))
text(1:length(bank.clap$rec),bank.clap$rec,round(bank.clap$tow),cex=0.8)
text(10,max(bank.clap$rec,na.rm=T),"Recruits")
axis(1,at=seq(5,length(bank.clap$rec),by=5),labels = c(bank.clap$tow[seq(5,length(bank.clap$tow),by=5)]))

plot(1:length(bank.clap$pre),bank.clap$pre,xaxt="n",xlab="tow ID", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.clap$pre,na.rm=T)))
text(1:length(bank.clap$pre),bank.clap$pre,round(bank.clap$tow),cex=0.8)
text(10,max(bank.clap$pre,na.rm=T),"Pre-recruits")
axis(1,at=seq(5,length(bank.clap$pre),by=5),labels = c(bank.clap$tow[seq(5,length(bank.clap$tow),by=5)]))
dev.off()

print("7")

# Now lets look at biomass and abundance trends by tow
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Numbers_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,4,1,1))
plot(1:length(bank.live$tot),bank.live$tot,xaxt="n",xlab="tow ID", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$tot,na.rm=T)))
text(1:length(bank.live$tot),bank.live$tot,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$tot,na.rm=T),"Overall")
title("Numbers by tow")
axis(1,at=seq(5,length(bank.live$tot),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))

plot(1:length(bank.live$com),bank.live$com,xaxt="n",xlab="tow ID", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$com,na.rm=T)))
text(1:length(bank.live$com),bank.live$com,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$com,na.rm=T),"Fully recruited")
axis(1,at=seq(5,length(bank.live$com),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))

plot(1:length(bank.live$rec),bank.live$rec,xaxt="n",xlab="tow ID", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$rec,na.rm=T)))
text(1:length(bank.live$rec),bank.live$rec,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$rec,na.rm=T),"Recruits")
axis(1,at=seq(5,length(bank.live$rec),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))

plot(1:length(bank.live$pre),bank.live$pre,xaxt="n",xlab="tow ID", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$pre,na.rm=T)))
text(1:length(bank.live$pre),bank.live$pre,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$pre,na.rm=T),"Pre-recruits")
axis(1,at=seq(5,length(bank.live$pre),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))
dev.off()

# Now for the biomass
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Biomass_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,4,1,1))
plot(1:length(bank.live$tot.bm),bank.live$tot.bm,xaxt="n",xlab="tow ID", ylab="kg/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$tot.bm,na.rm=T)))
text(1:length(bank.live$tot.bm),bank.live$tot.bm,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$tot.bm,na.rm=T),"Overall")
title("Biomass by tow")
axis(1,at=seq(5,length(bank.live$tot),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))

plot(1:length(bank.live$com.bm),bank.live$com.bm,xaxt="n",xlab="tow ID", ylab="kg/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$com.bm,na.rm=T)))
text(1:length(bank.live$com.bm),bank.live$com.bm,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$com.bm,na.rm=T),"Fully recruited")
axis(1,at=seq(5,length(bank.live$com),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))

plot(1:length(bank.live$rec.bm),bank.live$rec.bm,xaxt="n",xlab="tow ID", ylab="kg/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$rec.bm,na.rm=T)))
text(1:length(bank.live$rec.bm),bank.live$rec.bm,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$rec.bm,na.rm=T),"Recruits")
axis(1,at=seq(5,length(bank.live$rec),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))

plot(1:length(bank.live$pre.bm),bank.live$pre.bm,xaxt="n",xlab="tow ID", ylab="kg/tow",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$pre.bm,na.rm=T)))
text(1:length(bank.live$pre.bm),bank.live$pre.bm,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$pre.bm,na.rm=T),"Pre-recruits")
axis(1,at=seq(5,length(bank.live$pre),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))
dev.off()

# Look at the average shell height and meat weight of fully recruited individuals from each tow.


png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Mean_indy_sh_and_mw.png",sep=""),width=11,height=8.5, units="in", res=400)
#windows(11,8.5)
par(mfrow=c(2,1),mar=c(2,4,1,1))
plot(1:length(bank.live$l.bar),bank.live$l.bar,xaxt="n",xlab="tow ID", ylab="Mean SH (mm)",
     bty="L",pch=20,type="n",ylim=c(min(bank.live$l.bar,na.rm=T),max(bank.live$l.bar,na.rm=T)))
text(1:length(bank.live$tow),bank.live$l.bar,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$l.bar,na.rm=T),"Shell Height (mm)")
title("Mean Shell Height and Meat Weight by tow (Fully Recruited)")
axis(1,at=seq(5,length(bank.live$l.bar),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))
abline(h=mean(bank.live$l.bar,na.rm=T),lwd=2,lty=2,col="blue")

plot(1:length(bank.live$w.bar),bank.live$w.bar,xaxt="n",xlab="tow ID", ylab="Meat weight (g)",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$w.bar,na.rm=T)))
text(1:length(bank.live$w.bar),bank.live$w.bar,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$w.bar,na.rm=T),"Meat Weight (g)")
axis(1,at=seq(5,length(bank.live$w.bar),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$tow),by=5)]))
abline(h=mean(bank.live$w.bar,na.rm=T),lwd=2,lty=2,col="blue")
dev.off()

# Lets plot CF, Meat Weigth and Shell height by depth.

png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/MW_and_SH_and_CF_by_depth.png",sep=""),
    width=11,height=8.5, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(3,1),mar=c(2,4,1,1))
plot(bank.live$depth,bank.live$l.bar,xaxt="n",xlab="tow ID", ylab="Mean SH (mm)",
     bty="L",pch=20,type="n",ylim=c(min(bank.live$l.bar,na.rm=T),max(bank.live$l.bar,na.rm=T)))
text(bank.live$depth,bank.live$l.bar,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$l.bar,na.rm=T),"Shell Height (mm)")
title("Mean Shell Height, Meat Weight, and CF by depth")
axis(1)
abline(h=mean(bank.live$l.bar,na.rm=T),lwd=2,lty=2,col="blue")

plot(bank.live$depth,bank.live$w.bar,xaxt="n",xlab="tow ID", ylab="Meat weight (g)",
     bty="L",pch=20,type="n",ylim=c(0,max(bank.live$w.bar,na.rm=T)))
text(bank.live$depth,bank.live$w.bar,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$w.bar,na.rm=T),"Meat Weight (g)")
axis(1)
abline(h=mean(bank.live$w.bar,na.rm=T),lwd=2,lty=2,col="blue")

plot(bank.live$depth,bank.live$CF,xaxt="n",xlab="tow ID", ylab="CF",
     bty="L",pch=20,type="n",ylim=c(min(bank.live$CF,na.rm=T),max(bank.live$CF,na.rm=T)))
text(bank.live$depth,bank.live$CF,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$w.bar,na.rm=T),"Meat Weight (g)")
axis(1)
abline(h=mean(bank.live$CF,na.rm=T),lwd=2,lty=2,col="blue")

dev.off()
##############

print("8")

### spatial numbers by tow
baths <- rev(viridis(length(seq(40,140,by=10)),option="plasma"))
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/PRspatial_numbers_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F & !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Pre-recruit abundances",dec.deg=F)
} # end if(is.null(bank.survey.info) ==F)

if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T) & !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Pre-recruit abundances",dec.deg=F)
} # end if(is.null(bank.survey.info) ==T)

if(bnk[i] %in% c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core")){
  x.bound <- range(bound.poly.surv$X)
  y.bound <- range(bound.poly.surv$Y)
  ScallopMap(xlim=x.bound,ylim=y.bound,poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
             plot.boundries = T,bathy.source="quick", xlab="",ylab="",
             nafo.bord = F,nafo.lab = F,title="Pre-recruit abundances",dec.deg = F)
}
  
with(bank.live[bank.live$year==yr & bank.live$pre==max(bank.live$pre[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
with(bank.live[bank.live$year==yr,],text(lon,lat,round(pre, 1),cex=0.5))

if(dim(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",])[1]>0) addPolys(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",],lty=2,lwd=2)
if(bnk[i]=="GB") addPolys(boxes[boxes$Bank %in% c("GBa", "GBb") & boxes$Active=="Yes",],lty=2,lwd=2)

if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F & !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  legend("bottomleft",legend=c(bank.survey.info$PName),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Strata",title.adj=0.01,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n',inset=0.01)
  # Add area, convert to km^2 from number of towable units. (NTU/(1000*1000/800/2.4384)
  legend("topright",legend = round(bank.survey.info$area_km2),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.9,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n')
  
  legend("bottomright",legend = as.numeric(with(bank.live,tapply(tow,Strata_ID,length))),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.null(bank.survey.info) ==F)

dev.off()


png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Recspatial_numbers_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Recruit abundances",dec.deg=F)
} # end if(is.null(bank.survey.info) ==F)

if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T)& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Recruit abundances",dec.deg=F)
} # end if(is.null(bank.survey.info) ==T)

if(bnk[i] %in% c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core")){
  x.bound <- range(bound.poly.surv$X)
  y.bound <- range(bound.poly.surv$Y)
  ScallopMap(xlim=x.bound,ylim=y.bound,poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
             plot.boundries = T,bathy.source="quick", xlab="",ylab="",
             nafo.bord = F,nafo.lab = F,title="Recruit abundances",dec.deg = F)
}

with(bank.live[bank.live$year==yr & bank.live$rec==max(bank.live$rec[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
with(bank.live[bank.live$year==yr,],text(lon,lat, round(rec, 1),cex=0.5))
if(dim(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",])[1]>0) addPolys(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",],lty=2,lwd=2)
if(bnk[i]=="GB") addPolys(boxes[boxes$Bank %in% c("GBa", "GBb") & boxes$Active=="Yes",],lty=2,lwd=2)

if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  legend("bottomleft",legend=c(bank.survey.info$PName),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Strata",title.adj=0.01,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n',inset=0.01)
  # Add area, convert to km^2 from number of towable units. (NTU/(1000*1000/800/2.4384)
  legend("topright",legend = round(bank.survey.info$area_km2),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.9,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n')
  
  legend("bottomright",legend = as.numeric(with(bank.live,tapply(tow,Strata_ID,length))),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.null(bank.survey.info) ==F)
dev.off()


png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/FRspatial_numbers_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Fully-recruited abundances",dec.deg=F)
} # end if(is.null(bank.survey.info) ==F)

if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T)& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Fully-recruited abundances",dec.deg=F)
} # end if(is.null(bank.survey.info) ==T)

if(bnk[i] %in% c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core")){
  x.bound <- range(bound.poly.surv$X)
  y.bound <- range(bound.poly.surv$Y)
  ScallopMap(xlim=x.bound,ylim=y.bound,poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
             plot.boundries = T,bathy.source="quick", xlab="",ylab="",
             nafo.bord = F,nafo.lab = F,title="Fully-recruited abundances",dec.deg = F)
}

with(bank.live[bank.live$year==yr & bank.live$com==max(bank.live$com[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
with(bank.live[bank.live$year==yr,],text(lon,lat,round(com, 1),cex=0.5))
if(dim(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",])[1]>0) addPolys(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",],lty=2,lwd=2)
if(bnk[i]=="GB") addPolys(boxes[boxes$Bank %in% c("GBa", "GBb") & boxes$Active=="Yes",],lty=2,lwd=2)

if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  legend("bottomleft",legend=c(bank.survey.info$PName),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Strata",title.adj=0.01,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n',inset=0.01)
  # Add area, convert to km^2 from number of towable units. (NTU/(1000*1000/800/2.4384)
  legend("topright",legend = round(bank.survey.info$area_km2),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.9,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n')
  
  legend("bottomright",legend = as.numeric(with(bank.live,tapply(tow,Strata_ID,length))),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.null(bank.survey.info) ==F)
dev.off()

print("9")
### spatial biomass by tow

png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/PRspatial_biomass_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Pre-recruit biomass",dec.deg=F)
} # end if(is.null(bank.survey.info) ==F)

if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T)& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Pre-recruit biomass",dec.deg=F)
} # end if(is.null(bank.survey.info) ==T)

if(bnk[i] %in% c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core")){
  x.bound <- range(bound.poly.surv$X)
  y.bound <- range(bound.poly.surv$Y)
  ScallopMap(xlim=x.bound,ylim=y.bound,poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
             plot.boundries = T,bathy.source="quick", xlab="",ylab="",
             nafo.bord = F,nafo.lab = F,title="Pre-recruit biomass",dec.deg = F)
}

with(bank.live[bank.live$year==yr & bank.live$pre.bm==max(bank.live$pre.bm[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
with(bank.live[bank.live$year==yr,],text(lon,lat,round(pre.bm, 1),cex=0.5))
if(dim(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",])[1]>0) addPolys(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",],lty=2,lwd=2)
if(bnk[i]=="GB") addPolys(boxes[boxes$Bank %in% c("GBa", "GBb") & boxes$Active=="Yes",],lty=2,lwd=2)


if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  legend("bottomleft",legend=c(bank.survey.info$PName),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Strata",title.adj=0.01,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n',inset=0.01)
  # Add area, convert to km^2 from number of towable units. (NTU/(1000*1000/800/2.4384)
  legend("topright",legend = round(bank.survey.info$area_km2),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.9,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n')
  
  legend("bottomright",legend = as.numeric(with(bank.live,tapply(tow,Strata_ID,length))),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.null(bank.survey.info) ==F)
dev.off()


png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Recspatial_biomass_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Recruit biomass",dec.deg=F)
} # end if(is.null(bank.survey.info) ==F)

if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T)& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Recruit biomass",dec.deg=F)
} # end if(is.null(bank.survey.info) ==T)

if(bnk[i] %in% c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core")){
  x.bound <- range(bound.poly.surv$X)
  y.bound <- range(bound.poly.surv$Y)
  ScallopMap(xlim=x.bound,ylim=y.bound,poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
             plot.boundries = T,bathy.source="quick", xlab="",ylab="",
             nafo.bord = F,nafo.lab = F,title="Recruit biomass",dec.deg = F)
}

with(bank.live[bank.live$year==yr & bank.live$rec.bm==max(bank.live$rec.bm[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
with(bank.live[bank.live$year==yr,],text(lon,lat, round(rec.bm, 1),cex=0.5))
if(dim(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",])[1]>0) addPolys(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",],lty=2,lwd=2)
if(bnk[i]=="GB") addPolys(boxes[boxes$Bank %in% c("GBa", "GBb") & boxes$Active=="Yes",],lty=2,lwd=2)

if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  legend("bottomleft",legend=c(bank.survey.info$PName),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Strata",title.adj=0.01,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n',inset=0.01)
  # Add area, convert to km^2 from number of towable units. (NTU/(1000*1000/800/2.4384)
  legend("topright",legend = round(bank.survey.info$area_km2),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.9,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n')
  
  legend("bottomright",legend = as.numeric(with(bank.live,tapply(tow,Strata_ID,length))),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.null(bank.survey.info) ==F)
dev.off()


png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/FRspatial_biomass_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Fully-recruited biomass",dec.deg=F)
} # end if(is.null(bank.survey.info) ==F)

if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T)& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Fully-recruited biomass",dec.deg=F)
} # end if(is.null(bank.survey.info) ==T)

if(bnk[i] %in% c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core")){
  x.bound <- range(bound.poly.surv$X)
  y.bound <- range(bound.poly.surv$Y)
  ScallopMap(xlim=x.bound,ylim=y.bound,poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
             plot.boundries = T,bathy.source="quick", xlab="",ylab="",
             nafo.bord = F,nafo.lab = F,title="Fully-recruited biomass",dec.deg = F)
}

with(bank.live[bank.live$year==yr & bank.live$com.bm==max(bank.live$com.bm[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
with(bank.live[bank.live$year==yr,],text(lon,lat,round(com.bm, 1),cex=0.5))
if(dim(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",])[1]>0) addPolys(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",],lty=2,lwd=2)
if(bnk[i]=="GB") addPolys(boxes[boxes$Bank %in% c("GBa", "GBb") & boxes$Active=="Yes",],lty=2,lwd=2)

if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F& !bnk[i] %in% 
   c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core"))
{
  legend("bottomleft",legend=c(bank.survey.info$PName),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Strata",title.adj=0.01,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n',inset=0.01)
  # Add area, convert to km^2 from number of towable units. (NTU/(1000*1000/800/2.4384)
  legend("topright",legend = round(bank.survey.info$area_km2),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.9,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n')
  
  legend("bottomright",legend = as.numeric(with(bank.live,tapply(tow,Strata_ID,length))),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.null(bank.survey.info) ==F)
dev.off()

print("10")
### spatials for seedboxes
if(dim(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",])[1]>0 &!bnk[i] %in% c("GBa-North", "GBa-South", "GBa-Central", "GBa-West", "GBa-East", "GBa-Core", "GBa-Large_core")) {
  plots <- c("PRspatial", "Recspatial", "FRspatial")
  for(j in 1:length(plots)){
    png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/", plots[j], "_box_numbers_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
    if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F)
    {
      ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
                 plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
                 nafo.bord = T, nafo="all", nafo.lab = F, title=paste0(plots[j], " box abundances"), dec.deg=F, area="custom", 
                 ylim = c(min(boxes$Y[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) -0.05, max(boxes$Y[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) + 0.05),
                 xlim = c(min(boxes$X[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) -0.05, max(boxes$X[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) + 0.05))
    } # end if(is.null(bank.survey.info) ==F)
    
    if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T))
    {
      ScallopMap(bnk[i], direct = direct,
                 ylab="", xlab="", un=un.ID, pw=pwd.ID, db.con=database,
                 plot.bathy=T, plot.boundries = T,bathy.source="quick", cex.mn=2,
                 bathcol = baths,isobath = c(seq(50,150,by=50)),
                 nafo.bord = T, nafo="all", nafo.lab = F, title=paste0(plots[j], " box abundances"), dec.deg=F,  area="custom", 
                 ylim = c(min(boxes$Y[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) -0.05, max(boxes$Y[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) + 0.05),
                 xlim = c(min(boxes$X[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) -0.05, max(boxes$X[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) + 0.05))
    } # end if(is.null(bank.survey.info) ==T)
    
    if(plots[j]== "PRspatial"){
      with(bank.live[bank.live$year==yr & bank.live$pre==max(bank.live$pre[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
      with(bank.live[bank.live$year==yr,],text(lon,lat,round(pre, 1),cex=0.5))
    }
    
    if(plots[j]== "Recspatial"){
      with(bank.live[bank.live$year==yr & bank.live$rec==max(bank.live$rec[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
      with(bank.live[bank.live$year==yr,],text(lon,lat,round(rec, 1),cex=0.5))
    }
    
    if(plots[j]== "FRspatial"){
      with(bank.live[bank.live$year==yr & bank.live$com==max(bank.live$com[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
      with(bank.live[bank.live$year==yr,],text(lon,lat,round(com, 1),cex=0.5))
    }
    
    addPolys(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",],lty=2,lwd=2)
    
    dev.off()
  }
  
  for(j in 1:length(plots)){
    png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/", plots[j], "_box_biomass_by_tow.png",sep=""),width=11,height=8.5, units="in", res=400)
    if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F)
    {
      ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
                 plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
                 nafo.bord = T,nafo="all",nafo.lab = F,title=paste0(plots[j], " box biomass"),dec.deg=F,  area="custom", 
                 ylim = c(min(boxes$Y[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) -0.05, max(boxes$Y[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) + 0.05),
                 xlim = c(min(boxes$X[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) -0.05, max(boxes$X[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) + 0.05))
    } # end if(is.null(bank.survey.info) ==F)
    
    if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T))
    {
      ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
                 plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
                 nafo.bord = T,nafo="all",nafo.lab = F,title=paste0(plots[j], " box biomass"),dec.deg=F, area="custom", 
                 ylim = c(min(boxes$Y[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) -0.05, max(boxes$Y[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) + 0.05),
                 xlim = c(min(boxes$X[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) -0.05, max(boxes$X[boxes$Bank==bnk[i] & boxes$Active=="Yes"]) + 0.05))
    } # end if(is.null(bank.survey.info) ==T)
    
    if(plots[j]== "PRspatial"){
      with(bank.live[bank.live$year==yr & bank.live$pre.bm==max(bank.live$pre.bm[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
      with(bank.live[bank.live$year==yr,],text(lon,lat,round(pre.bm, 1),cex=0.5))
    }
    
    if(plots[j]== "Recspatial"){
      with(bank.live[bank.live$year==yr & bank.live$rec.bm==max(bank.live$rec.bm[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
      with(bank.live[bank.live$year==yr,],text(lon,lat,round(rec.bm, 1),cex=0.5))
    }
    
    if(plots[j]== "FRspatial"){
      with(bank.live[bank.live$year==yr & bank.live$com.bm==max(bank.live$com.bm[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
      with(bank.live[bank.live$year==yr,],text(lon,lat,round(com.bm, 1),cex=0.5))
    }
    
    addPolys(boxes[boxes$Bank==bnk[i] & boxes$Active=="Yes",],lty=2,lwd=2)
    
    dev.off()
  }
}
############## End of seedbox spatial figures
print("11")

############## Note that On this figure some true zero's will get a very small value so they can be plotted.
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Abundance_by_depth.png",sep=""),width=8.5,height=11, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,6,1,1))
plot(bank.live$depth[bank.live$tot>0],bank.live$tot[bank.live$tot>0],
     xaxt="n",xlab="depth", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0.05*min(bank.live$tot[bank.live$tot>0],na.rm=T),
                                    5*max(bank.live$tot[bank.live$tot>0],na.rm=T)),
     xlim=c(min(bank.live$depth,na.rm=T),max(bank.live$depth,na.rm=T)),log="y",yaxt="n")
text(bank.live$depth[bank.live$tot>0],bank.live$tot[bank.live$tot>0],
     round(bank.live$tow[bank.live$tot>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!  On this figure some true zero's will get a very small value so they can be plotted.
if(length(bank.live$depth[bank.live$tot==0]) > 0)
  {
text(bank.live$depth[bank.live$tot==0],
  jitter((0*bank.live$tot[bank.live$tot==0]+0.1*min(bank.live$tot[bank.live$com>0],na.rm=T)),factor=25),
     round(bank.live$tow[bank.live$tot==0]),cex=0.8)
  } # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(min(bank.live$depth)*1.05,max(bank.live$tot,na.rm=T),"Total")
title("Abundance by depth")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.live$tot,na.rm=T),lwd=2,lty=2,col="blue")

# Now fully recruited
plot(bank.live$depth[bank.live$com>0],bank.live$com[bank.live$com>0],
     xaxt="n",xlab="depth", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0.05*min(bank.live$com[bank.live$com>0],na.rm=T),
                                    5*max(bank.live$com,na.rm=T)),
     xlim=c(min(bank.live$depth,na.rm=T),max(bank.live$depth,na.rm=T)),log="y",yaxt="n")
text(bank.live$depth[bank.live$com>0],bank.live$com[bank.live$com>0],
     round(bank.live$tow[bank.live$com>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$depth[bank.live$com==0]) > 0)
{
  text(bank.live$depth[bank.live$com==0],
    jitter((0*bank.live$com[bank.live$com==0]+0.1*min(bank.live$com[bank.live$com>0],na.rm=T)),factor=25),
    round(bank.live$tow[bank.live$com==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(min(bank.live$depth)*1.05,max(bank.live$com,na.rm=T),"Fully Recruited")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.live$com,na.rm=T),lwd=2,lty=2,col="blue")

# Now the recruits
plot(bank.live$depth[bank.live$rec>0],bank.live$rec[bank.live$rec>0],
     xaxt="n",xlab="depth", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0.05*min(bank.live$rec[bank.live$rec>0],na.rm=T),
                                    5*max(bank.live$rec,na.rm=T)),
     xlim=c(min(bank.live$depth,na.rm=T),max(bank.live$depth,na.rm=T)),log="y",yaxt="n")
text(bank.live$depth[bank.live$rec>0],bank.live$rec[bank.live$rec>0],
     round(bank.live$tow[bank.live$rec>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$depth[bank.live$rec==0]) > 0)
{
  text(bank.live$depth[bank.live$rec==0],
    jitter((0*bank.live$rec[bank.live$rec==0]+0.1*min(bank.live$rec[bank.live$rec>0],na.rm=T)),factor=25),
    round(bank.live$tow[bank.live$rec==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(min(bank.live$depth)*1.05,max(bank.live$rec,na.rm=T),"Recruits")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.live$rec,na.rm=T),lwd=2,lty=2,col="blue")


# Now the Pre-recruits
plot(bank.live$depth[bank.live$pre>0],bank.live$pre[bank.live$pre>0],
     xaxt="n",xlab="depth", ylab="N/tow",
     bty="L",pch=20,type="n",ylim=c(0.05*min(bank.live$pre[bank.live$pre>0],na.rm=T),
                                    5*max(bank.live$pre,na.rm=T)),
     xlim=c(min(bank.live$depth,na.rm=T),max(bank.live$depth,na.rm=T)),log="y",yaxt="n")
text(bank.live$depth[bank.live$pre>0],bank.live$pre[bank.live$pre>0],
     round(bank.live$tow[bank.live$pre>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$depth[bank.live$pre==0]) > 0)
{
  text(bank.live$depth[bank.live$pre==0],
    jitter((0*bank.live$pre[bank.live$pre==0]+0.1*min(bank.live$pre[bank.live$pre>0],na.rm=T)),factor=25),
    round(bank.live$tow[bank.live$pre==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(min(bank.live$depth)*1.05,max(bank.live$pre,na.rm=T),"Pre-recruits")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.live$pre,na.rm=T),lwd=2,lty=2,col="blue")

dev.off()
############







############### Note that On this figure some true zero's will get a very small value so they can be plotted.
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Biomass_by_depth.png",sep=""),width=8.5,height=11, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(2,6,1,1))
plot(bank.live$depth[bank.live$tot.bm>0],bank.live$tot.bm[bank.live$tot.bm>0],
     xaxt="n",xlab="depth", ylab="kg/tow",
     bty="L",pch=20,type="n",ylim=c(0.05*min(bank.live$tot.bm[bank.live$tot.bm>0],na.rm=T),
                                    5*max(bank.live$tot.bm[bank.live$tot.bm>0],na.rm=T)),
     xlim=c(min(bank.live$depth,na.rm=T),max(bank.live$depth,na.rm=T)),log="y",yaxt="n")

text(bank.live$depth[bank.live$tot.bm>0],bank.live$tot.bm[bank.live$tot.bm>0],
     round(bank.live$tow[bank.live$tot.bm>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$depth[bank.live$tot.bm==0]) > 0)
{
  text(bank.live$depth[bank.live$tot.bm==0],
       jitter((0*bank.live$tot.bm[bank.live$tot.bm==0]+
                 0.1*min(bank.live$tot.bm[bank.live$com>0],na.rm=T)),factor=25),
       round(bank.live$tow[bank.live$tot.bm==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot.bm==0]) > 0)
text(min(bank.live$depth)*1.05,max(bank.live$tot.bm,na.rm=T),"total")
title("Biomass by depth")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.live$tot.bm,na.rm=T),lwd=2,lty=2,col="blue")

# Now fully recruited
plot(bank.live$depth[bank.live$com.bm>0],bank.live$com.bm[bank.live$com.bm>0],
     xaxt="n",xlab="depth", ylab="kg/tow",
     bty="L",pch=20,type="n",ylim=c(0.05*min(bank.live$com.bm[bank.live$com.bm>0],na.rm=T),
                                    5*max(bank.live$com.bm,na.rm=T)),
     xlim=c(min(bank.live$depth,na.rm=T),max(bank.live$depth,na.rm=T)),log="y",yaxt="n")

text(bank.live$depth[bank.live$com.bm>0],bank.live$com.bm[bank.live$com.bm>0],
     round(bank.live$tow[bank.live$com.bm>0]),cex=0.8)
# Because this is log scale this is com.bmplicated by 0's!
if(length(bank.live$depth[bank.live$com.bm==0]) > 0)
{
  text(bank.live$depth[bank.live$com.bm==0],
       jitter((0*bank.live$com.bm[bank.live$com.bm==0]+
                 0.1*min(bank.live$com.bm[bank.live$com.bm>0],na.rm=T)),factor=25),
       round(bank.live$tow[bank.live$com.bm==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(min(bank.live$depth)*1.05,max(bank.live$com.bm,na.rm=T),"Fully Recruited")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.live$com.bm,na.rm=T),lwd=2,lty=2,col="blue")

# Now the recruits
plot(bank.live$depth[bank.live$rec.bm>0],bank.live$rec.bm[bank.live$rec.bm>0],
     xaxt="n",xlab="depth", ylab="kg/tow",
     bty="L",pch=20,type="n",ylim=c(0.05*min(bank.live$rec.bm[bank.live$rec.bm>0],na.rm=T),
                                    5*max(bank.live$rec.bm,na.rm=T)),
     xlim=c(min(bank.live$depth,na.rm=T),max(bank.live$depth,na.rm=T)),log="y",yaxt="n")

text(bank.live$depth[bank.live$rec.bm>0],bank.live$rec.bm[bank.live$rec.bm>0],
     round(bank.live$tow[bank.live$rec.bm>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$depth[bank.live$rec.bm==0]) > 0)
{
  text(bank.live$depth[bank.live$rec.bm==0],
       jitter((0*bank.live$rec.bm[bank.live$rec.bm==0]+
                 0.1*min(bank.live$rec.bm[bank.live$rec.bm>0],na.rm=T)),factor=25),
       round(bank.live$tow[bank.live$rec.bm==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(min(bank.live$depth)*1.05,max(bank.live$rec.bm,na.rm=T),"Recruits")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.live$rec.bm,na.rm=T),lwd=2,lty=2,col="blue")


# Now the Pre-recruits
plot(bank.live$depth[bank.live$pre.bm>0],bank.live$pre.bm[bank.live$pre.bm>0],
     xaxt="n",xlab="depth", ylab="kg/tow",
     bty="L",pch=20,type="n",ylim=c(0.05*min(bank.live$pre.bm[bank.live$pre.bm>0],na.rm=T),
                                    5*max(bank.live$pre.bm,na.rm=T)),
     xlim=c(min(bank.live$depth,na.rm=T),max(bank.live$depth,na.rm=T)),log="y",yaxt="n")

text(bank.live$depth[bank.live$pre.bm>0],bank.live$pre.bm[bank.live$pre.bm>0],
     round(bank.live$tow[bank.live$pre.bm>0]),cex=0.8)
# Because this is log scale this is complicated by 0's!
if(length(bank.live$depth[bank.live$pre.bm==0]) > 0)
{
  text(bank.live$depth[bank.live$pre.bm==0],
       jitter((0*bank.live$pre.bm[bank.live$pre.bm==0]+
                 0.1*min(bank.live$pre.bm[bank.live$pre.bm>0],na.rm=T)),factor=25),
       round(bank.live$tow[bank.live$pre.bm==0]),cex=0.8)
} # end if(length(bank.live$depth[bank.live$tot==0]) > 0)
text(min(bank.live$depth)*1.05,max(bank.live$pre.bm,na.rm=T),"Pre-recruits")
axis(1)
axis(2,at=axTicks(2,nintLog=10),labels=c(0,axTicks(2,nintLog=10)[-1]),las=1)
abline(h=mean(bank.live$pre.bm,na.rm=T),lwd=2,lty=2,col="blue")

dev.off()
################

# Set up a color palette even I can see for isobaths, the begin-end sets the hue range should be blue
baths <- rev(viridis(length(seq(40,140,by=10)),option="plasma"))
#plot(1:10,col=baths[9],type="o")
# Nice plot of the GBa tow locations with accompanying survey data.
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/tow_locations.png",sep=""),width=11,height=8.5, units="in", res=400)
#windows(11,8.5)
if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F)
{
ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
           plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
           nafo.bord = T,nafo="all",nafo.lab = F,title="Tow locations",dec.deg=F)
} # end if(is.null(bank.survey.info) ==F)

if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T))
{
ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
           plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
           nafo.bord = T,nafo="all",nafo.lab = F,title="Tow locations",dec.deg=F)
} # end if(is.null(bank.survey.info) ==T)

with(bank.live,text(lon,lat,tow,cex=0.5))
#addPolys(GBa.boxes,lty=2,lwd=2)
#addPolys(BBboxes,lty=2,lwd=2)

if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F)
{
legend("bottomleft",legend=c(bank.survey.info$PName),
       fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
       pch=c(rep(NA,length(bank.survey.info$PName))),title = "Strata",title.adj=0.01,
       pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n',inset=0.01)
# Add area, convert to km^2 from number of towable units. (NTU/(1000*1000/800/2.4384)
legend("topright",legend = round(bank.survey.info$area_km2),
       fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
       pch=c(rep(NA,length(bank.survey.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.9,
       pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n')

legend("bottomright",legend = as.numeric(with(bank.live,tapply(tow,Strata_ID,length))),
       fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
       pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
       pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.null(bank.survey.info) ==F)
dev.off()
############

############
# Here's the Raw CF data by tow.
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/CF_samples_by_tow_or_depth.png",sep=""),
    width=15,height=8.5, units="in", res=400)
#windows(15,8.5)
par(mfrow=c(2,1),mar=c(2,7,2,1))
boxplot(bank.mw$CF_samp ~ bank.mw$tow,xlab="tow",yaxt="n",xaxt="n")
axis(2,las=1)
mtext(side =2,cf.lab,3,las=1)
title("Sample CF for each tow and depth")
axis(1,at=c(1,seq(5,length(unique(bank.mw$tow)),by=5)),
     labels = c(sort(unique(bank.mw$tow)))[c(1,seq(5,length(unique(bank.mw$tow)),by=5))])
text(1:length(unique(bank.mw$tow)),rep(min(bank.mw$CF_samp),length(unique(bank.mw$tow))),sort(unique(bank.mw$tow)),cex=0.5)
text(1:length(unique(bank.mw$tow)),rep(max(bank.mw$CF_samp),length(unique(bank.mw$tow))),
     round(tapply(bank.mw$depth,bank.mw$tow,mean)),cex=0.5)

boxplot(bank.mw$CF_samp ~ bank.mw$depth,xlab="tow",yaxt="n",xaxt="n")
axis(2,las=1)
axis(1,at = 1:length(unique(bank.mw$depth)),labels = round(sort(unique(bank.mw$depth))))
mtext(side =2,cf.lab,3,las=1)
dev.off()
############

############
# Here's the MW data by tow and depth
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/MW_by_tow_and_depth.png",sep=""),
    width=15,height=8.5, units="in", res=400)
#windows(15,8.5)
par(mfrow=c(2,1),mar=c(2,7,2,1))
boxplot(bank.mw$wmw ~ bank.mw$tow,xlab="tow",yaxt="n",xaxt="n")
axis(2,las=1)
mtext(side =2,"MW (g)",3,las=1)
title("Wet Meat Weight for each tow and depth")
axis(1,at=c(1,seq(5,length(unique(bank.mw$tow)),by=5)),
            labels = c(sort(unique(bank.mw$tow)))[c(1,seq(5,length(unique(bank.mw$tow)),by=5))])
text(1:length(unique(bank.mw$tow)),rep(min(bank.mw$wmw),length(unique(bank.mw$tow))),sort(unique(bank.mw$tow)),cex=0.5)
text(1:length(unique(bank.mw$tow)),rep(max(bank.mw$wmw),length(unique(bank.mw$tow))),
        round(tapply(bank.mw$depth,bank.mw$tow,mean)),cex=0.5)

boxplot(bank.mw$wmw ~ bank.mw$depth,xlab="tow",yaxt="n",xaxt="n")
axis(2,las=1)
axis(1,at = 1:length(unique(bank.mw$depth)),labels = round(sort(unique(bank.mw$depth))))
 mtext(side =2,"MW (g)",3,las=1)
 dev.off()
############

############
# Here's the MW data by tow and depth
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/SH_by_tow_and_depth.png",sep=""),
   width=15,height=8.5, units="in", res=400)
#windows(15,8.5)
par(mfrow=c(2,1),mar=c(2,7,2,1))
boxplot(bank.mw$sh ~ bank.mw$tow,xlab="tow",yaxt="n",xaxt="n")
axis(2,las=1)
mtext(side =2,"SH (mm)",3,las=1)
title("Shell height for each tow and depth")
axis(1,at=c(1,seq(5,length(unique(bank.mw$tow)),by=5)),
     labels = c(sort(unique(bank.mw$tow)))[c(1,seq(5,length(unique(bank.mw$tow)),by=5))])
text(1:length(unique(bank.mw$tow)),rep(min(bank.mw$sh),length(unique(bank.mw$tow))),sort(unique(bank.mw$tow)),cex=0.5)
text(1:length(unique(bank.mw$tow)),rep(max(bank.mw$sh),length(unique(bank.mw$tow))),
           round(tapply(bank.mw$depth,bank.mw$tow,mean)),cex=0.5)

boxplot(bank.mw$sh ~ bank.mw$depth,xlab="tow",yaxt="n",xaxt="n")
axis(2,las=1)
axis(1,at = 1:length(unique(bank.mw$depth)),labels = round(sort(unique(bank.mw$depth))))
mtext(side =2,"SH (mm)",3,las=1)
dev.off()
      ############


#### Plot the locations of the CF prediction
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/CF_prediction_location.png",sep=""),
    width=11,height=8.5, units="in", res=400)
if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F)
{
  ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title=paste0("CF prediction location"),dec.deg=F)
} # end if(is.null(bank.survey.info) ==F)

if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T))
{
  ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title=paste0("CF prediction location"),dec.deg=F)
} # end if(is.null(bank.survey.info) ==T)

points(x=cf.data2[[bnk[i]]]$CFyrs$lon[cf.data2[[bnk[i]]]$CFyrs$year==yr], y=cf.data2[[bnk[i]]]$CFyrs$lat[cf.data2[[bnk[i]]]$CFyrs$year==yr], cex=3)

dev.off()

# Now for the clappers by CF, any indication low CF is associated with High clapper numbers.
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_vs_CF.png",sep=""),width=8.5,height=11, units="in", res=400)
#windows(8.5,11)
par(mfrow=c(4,1),mar=c(4,7,1,1))
plot(cf.dat$clap.prop~cf.dat$cf,pch=20,xlab="",ylab = "% dead",bty="L")
text(min(cf.dat$cf,na.rm=T)*1.05,max(cf.dat$clap.prop,na.rm=T),"Total")
title("Clappers_vs_CF")
plot(cf.dat$clap.propCom~cf.dat$cf,pch=20,xlab="",ylab = "% dead",bty="L")
text(min(cf.dat$cf,na.rm=T)*1.05,max(cf.dat$clap.propCom,na.rm=T),"Fully Recruited")
plot(cf.dat$clap.propRec~cf.dat$cf,pch=20,xlab="",ylab = "% dead",bty="L")
text(min(cf.dat$cf,na.rm=T)*1.05,max(cf.dat$clap.propRec,na.rm=T),"Recruits")
plot(cf.dat$clap.propPre~cf.dat$cf,pch=20,xlab=cf.lab,ylab = "% dead",bty="L")
text(min(cf.dat$cf,na.rm=T)*1.05,max(cf.dat$clap.propPre,na.rm=T),"Pre-recruits")
dev.off()


##### Condition factor, Meat Weight, and Shell Height of tows in various depth class bins...
mins <- round(min(bank.live$depth)-5,digits = -1)
maxs <- round(max(bank.live$depth)+5,digits = -1)
bounds <- seq(mins,maxs,by=10)

CF.depth.table <- data.frame(matrix(NA,nrow=(length(bounds)-1),ncol = 6))
MW.depth.table <- data.frame(matrix(NA,nrow=(length(bounds)-1),ncol = 6))
SH.depth.table <- data.frame(matrix(NA,nrow=(length(bounds)-1),ncol = 6))
MC.depth.table <- data.frame(matrix(NA,nrow=(length(bounds)-1),ncol = 6))

colnames(CF.depth.table) <- c("Depth","Mean","Median","Min","Max","Number of Tows")
colnames(MW.depth.table) <- c("Depth","Mean","Median","Min","Max","Number of Tows")
colnames(SH.depth.table) <- c("Depth","Mean","Median","Min","Max","Number of Tows")
colnames(MC.depth.table) <- c("Depth","Mean","Median","Min","Max","Number of Tows")



for(j in 1:length(bounds)-1)
{
  CF.depth.table[j,"Depth"] <- paste(bounds[j],"-",bounds[j+1],sep="")
  CF.depth.table[j,"Mean"] <- mean(bank.live$CF[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  CF.depth.table[j,"Median"] <- median(bank.live$CF[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  CF.depth.table[j,"Min"] <- min(bank.live$CF[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  CF.depth.table[j,"Max"] <- max(bank.live$CF[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  CF.depth.table[j,"Number of Tows"] <- length(bank.live$CF[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ])
  # Now Grab Meat Weight
  MW.depth.table[j,"Depth"] <- paste(bounds[j],"-",bounds[j+1],sep="")
  MW.depth.table[j,"Mean"] <- mean(bank.live$w.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  MW.depth.table[j,"Median"] <- median(bank.live$w.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  MW.depth.table[j,"Min"] <- min(bank.live$w.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  MW.depth.table[j,"Max"] <- max(bank.live$w.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  MW.depth.table[j,"Number of Tows"] <- length(bank.live$w.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ])
  # Now Grab Shell Height
  SH.depth.table[j,"Depth"] <- paste(bounds[j],"-",bounds[j+1],sep="")
  SH.depth.table[j,"Mean"] <- mean(bank.live$l.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  SH.depth.table[j,"Median"] <- median(bank.live$l.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  SH.depth.table[j,"Min"] <- min(bank.live$l.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  SH.depth.table[j,"Max"] <- max(bank.live$l.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ],na.rm=T)
  SH.depth.table[j,"Number of Tows"] <- length(bank.live$l.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ])
  # The Meat count, most of which can be calculated outside the loop
  MC.depth.table[j,"Depth"] <- paste(bounds[j],"-",bounds[j+1],sep="")
  MC.depth.table[j,"Number of Tows"] <- length(bank.live$w.bar[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ])
}
# Notice that I switch the min/max around as the Max MW would be the lowest meat count (which is a good thing!)
MC.depth.table[,c("Mean","Median","Min","Max")] <- 500/MW.depth.table[,c("Mean","Median","Max","Min")]
rownames(CF.depth.table) <- paste("CF:",CF.depth.table$Depth)
rownames(MW.depth.table) <- paste("MW:",MW.depth.table$Depth)
rownames(SH.depth.table) <- paste("SH:",SH.depth.table$Depth)
rownames(MC.depth.table) <- paste("MC:",MC.depth.table$Depth)

Depth.table <- rbind(CF.depth.table,MW.depth.table,MC.depth.table,SH.depth.table)
write.csv(Depth.table,paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Depth_table.csv",sep=""))


### Now grab the abundance data averages for the bank in N/tow or kg/tow
if(!is.null(bins)) bin.names <- paste("mean",sapply(strsplit(survey.obj[[bnk[i]]]$bin.names,"bin",fixed=T),function(x) (x[2])),sep="")
clap.year <- subset(clap.survey.obj[[bnk[i]]][[1]], year == yr)
this.year <- subset(survey.obj[[bnk[i]]][[1]], year == yr)
if(bnk[i] == "Ger") this.year <-  subset(spr.survey.obj$out.obj[[1]], year == yr)
nrows <- 6
if(!is.null(bins)) nrows <- nrows + length(bin.names)
row.names=c("N","NR","NPR","I","IR","IPR")
if(!is.null(bins)) row.names <- c(row.names,bin.names)
df <- data.frame(Live = rep(NA,nrows),Clap = rep(NA,nrows),row.names=row.names)

if(bnk[i] %in%  c("Sab","BBs","GBa","BBn","GBb"))
{
  df[,1] <- as.numeric(this.year[,row.names]/ sum(bank.survey.info$towable_area)*10^6)
  # Get clappers
  df[1,2]  <- clap.year$N / sum(bank.survey.info$towable_area)*10^6
  df[2,2]<- clap.year$NR / sum(bank.survey.info$towable_area)*10^6
  df[3,2]  <- clap.year$NPR / sum(bank.survey.info$towable_area)*10^6

} # end if(bnk[i] %in%  c("Sab","BBs","GBa","BBn","GBb"))

if(bnk[i] %in% c("Ger","Mid","GB"))
{
  # everythign alive
  df[1:length(row.names),1] <- as.numeric(this.year[,row.names])
  # Get clappers

  df[1,2]  <- clap.year$N
  df[2,2] <- clap.year$NR
  df[3,2]  <- clap.year$NPR
} # end if(bnk[i] %in% c("Ger","Mid","GB"))


write.csv(df,paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/N_BM_and_Clap.csv",sep=""))


} # end for i loop.

########################################################  END THE CORE FIGURES###################################  END THE CORE FIGURES






##############################  THIS IS THE END OF THE PLOTS/TABLES THE REST IS JUST SOME EXPLORATORY CRAP ###########

# Just checking out some numbers here...
mean(bank.mw$CF_samp)
bank.live[bank.live$tow == 322,]
bank.live[bank.live$tow == 94,]
bank.live[bank.live$tow == 148,]
mean(bank.live$pre)
median(bank.live$pre)


bank.live[bank.live$depth >= bounds[j] & bank.live$depth < bounds[j+1] ,]

(bank.mw$depth[order(bank.mw$tow)])
### Interesting this is...
which(bank.live$w.bar == "NaN")
# so for tows 145 146 147 148 149 150 151 164 we got no fully recruited scallops!
bank.live[bank.live$w.bar == "NaN",]

# What is the average number of individuals at different depths, where are we seeing our scallops.
(bank.live)

subset(live.dat[[1]],year == 2015,select=(c("tow","depth")))

names(bank.mw)


# Find a particular tows mw/sh data...

bank.mw[bank.mw$tow == 330,]

## If curious about the depth of certain tows...

bank.live$depth[bank.live$tow==28]


# Does any of the data look wrong here?
ranges <- apply(bank.live,2,range,na.rm=T)



############################ End of the best stuff... ############################ End of the best stuff...


















##################  Some other plots I made that might be useful...########  These are for Browns bank i=2 at time of writing...

# 2014 SHF
bank.live<- subset(live.dat[[2]],year==2014)
bank.clap<- subset(clap.dat[[2]],year==2014)
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/SHF_2014.pdf",sep=""),onefile=T,width=11,height=8.5)
par(mfrow=c(1,2))
for(j in seq(1,length(bank.live[,1])))
{
  # Get both plots on the same scale...
  ymax <- max(max(bank.clap[j,cols],na.rm=T),max(bank.live[j,cols],na.rm=T))
  plot(t(bank.live[j,cols]),type="h",xaxt="n",xlab="Shell Height",ylab="Number",ylim=c(0,ymax),bty="L")
  axis(1,at=1:40,labels=F,tcl=0.2)
  axis(1,at=1:40,labels=F,tcl=-0.2)
  axis(1,at = seq(1,40,by=5),labels = names(bank.live[j,seq(16,55,by=5)]))
  title(paste("Standardized Number alive per tow (tow ID-",bank.live$tow[j],")",sep=""),cex=0.6)
  
  plot(t(bank.clap[j,cols]),type="h",xaxt="n",xlab="Shell Height",ylab="Number",ylim=c(0,ymax),bty="L")
  axis(1,at=1:40,labels=F,tcl=0.2)
  axis(1,at=1:40,labels=F,tcl=-0.2)
  axis(1,at = seq(1,40,by=5),labels = names(bank.clap[j,seq(16,55,by=5)]))
  title(paste("Standardized Number of Clappers per tow (tow ID-",bank.clap$tow[j],")",sep=""),cex=0.6)
} # end for(i in seq(1,length(bank.live[,1])))
dev.off()


# 2014 tow locations


# Set up a color palette I can see for isobaths... 
baths <- brewer.pal(length(seq(40,140,by=10)),"Blues")
# Nice plot of the GBa tow locations with accompanying survey data.
png(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/tow_locations_2014.png",sep=""),width=11,height=8.5, units="in", res=400)
#windows(11,8.5)
if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F)
{
  ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , 
             nafo.bord = T,nafo="all",nafo.lab = F,title="Tow locations",dec.deg=F)
} # end if(is.null(bank.survey.info) ==F)

if((is.null(bank.survey.info) ==T | is.na(bank.survey.info) ==T))
{
  ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Tow locations",dec.deg=F)
} # end if(is.null(bank.survey.info) ==T)

with(bank.live,text(lon,lat,tow,cex=0.5))
#addPolys(GBa.boxes,lty=2,lwd=2)
# addPolys(BBboxes,lty=2,lwd=2)

if(is.null(bank.survey.info) ==F & is.na(bank.survey.info) ==F)
{
  legend("bottomleft",legend=c(bank.survey.info$PName),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Strata",title.adj=0.01,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n',inset=0.01)
  # Add area, convert to km^2 from number of towable units. (NTU/(1000*1000/800/2.4384)
  legend(-67.27,41.82,legend = round(bank.survey.info$area_km2),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.9,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty='n')
  legend(-66,41.45,legend = as.numeric(with(bank.live,tapply(tow,Strata_ID,length))),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.null(bank.survey.info) ==F)
dev.off()
############



###################################################################################################################################################
############ Checking for bias caused by German rake loss in 2019 survey. Tows 401-412 were with German Rake, tows 412+ were Georges rake
###################################################################################################################################################
lined.dat$rake[lined.dat$year == 2018] <- "German rake"
lined.dat$rake[lined.dat$year == 2019 & lined.dat$tow %in% 401:411] <- "German rake"
lined.dat$rake[lined.dat$year == 2019 & !lined.dat$tow %in% 401:411] <- "Georges rake"

ggplot() + geom_text(data=lined.dat[lined.dat$year %in% 2018:2019,],
                      aes(lon, lat, colour=rake, label=tow)) + facet_wrap(~year) + theme_bw() + theme(panel.grid=element_blank())

lastyear <- unique(lined.dat[lined.dat$year %in% 2018, c("tow", "lon", "lat")])
names(lastyear) <- c("oldtow", "lon", "lat")
thisyear <- unique(lined.dat[lined.dat$year %in% 2019, c("tow", "lon", "lat")])

# compare outer SW corner tows from 2018 and 2019
# compare inner SW corner tows from 2018 and 2019

# start by making a bounding box for the SW corner
bbox <- c(-66.75, -66.125,  43.0, 43.2)

outerpoly <- cbind(c(-66.625, -66.125, -66.125, -66.625), c(43.25, 43.1, 43.0, 43.0))
innerpoly <- cbind(c(-66.625, -66.125, -66.125, -66.625), c(43.25, 43.25, 43.1, 43.25))
names(innerpoly) <- c("X", "Y")
names(outerpoly) <- c("X", "Y")
data <- data.frame(id=1)

innerpoly_sp <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(innerpoly)), 1)), proj4string=CRS("+init=epsg:4326")), data)
outerpoly_sp <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(outerpoly)), 1)), proj4string=CRS("+init=epsg:4326")), data)

points <- lined.dat[lined.dat$year %in% 2018:2019, c("year", "tow", "lon", "lat")]
points$ID <- paste0(points$year, ".", points$tow)
points_sp <- SpatialPointsDataFrame(coords = cbind(points$lon, points$lat), data = points,
                               proj4string = CRS("+init=epsg:4326"))
require(rgeos)
int_inner <- intersect(points_sp, innerpoly_sp)
int_outer <- intersect(points_sp, outerpoly_sp)
int_inner <- data.frame(int_inner)
int_outer <- data.frame(int_outer)

points$int[points$ID %in% int_inner$ID] <- "inner"
points$int[points$ID %in% int_outer$ID] <- "outer"
points$int[is.na(points$int)] <- "other"

points <- join(lined.dat[lined.dat$year %in% 2018:2019,], points, type="full")
points$int <- factor(points$int, levels=c("inner", "outer", "other"))

map <- ggplot() + 
  #geom_text(data=points,aes(lon, lat, colour=int, label=tow)) + facet_wrap(~year) + theme_bw() + theme(panel.grid=element_blank()) +
  geom_point(data=points,aes(lon, lat, colour=rake)) + facet_wrap(~year) + theme_bw() + theme(panel.grid=element_blank()) +
  geom_polygon(data=as.data.frame(innerpoly), aes(V1, V2), fill=NA, colour="black") +
  scale_colour_manual(values=c("orange", "blue"), name="Rake type") +
  #scale_shape_manual(values=c(15,16, 4)) +
  geom_polygon(data=as.data.frame(outerpoly), aes(V1, V2), fill="grey", colour="black", alpha=0.2) +
  annotate(geom="text", x=-66.125, y=43.25, label="Inner", hjust=1.05, vjust=1.5) +
  annotate(geom="text", x=-66.625, y=43, label="Outer", hjust=-0.05, vjust=-0.5) +
  theme(legend.position="bottom") +
  ggtitle("German Bank survey stations") +
  ylab("Latitude") +
  xlab("Longitude")


sw_melt <- melt(points[!is.na(points$int),], measure.vars = paste0("h", seq(5,200,5)))
sw_melt$variable <- gsub(x=sw_melt$variable, pattern="h", "")
sw_melt$variable <- as.numeric(as.character(sw_melt$variable))

# all tows in SW corner
all <- ggplot() + geom_bar(data=sw_melt[!sw_melt$int == "other",], aes(x=variable, y=value), stat = "identity") + 
  facet_wrap(~year, nrow=2) +
  theme_bw() + theme(panel.grid=element_blank()) +
  geom_vline(data=sw_melt, aes(xintercept=95), linetype="dashed")+
  geom_vline(data=sw_melt, aes(xintercept=105), linetype="dashed") +
  scale_x_continuous(breaks=seq(0, 200, 10)) +
  ggtitle("All tows in SW corner of German") +
  ylab("Number of scallops") +
  xlab("Size bin (5mm)")

# all tows in inner SW corner
inner <- ggplot() + geom_bar(data=sw_melt[sw_melt$int=="inner",], aes(x=variable, y=value, fill=rake), stat = "identity") + 
  facet_wrap(~year, nrow=2) +
  theme_bw() + theme(panel.grid=element_blank()) +
  geom_vline(data=sw_melt[sw_melt$int=="inner",], aes(xintercept=95), linetype="dashed")+
  geom_vline(data=sw_melt[sw_melt$int=="inner",], aes(xintercept=105), linetype="dashed") +
  scale_x_continuous(breaks=seq(0, 200, 10)) +
  scale_fill_manual(values=c("orange", "blue"), name="Rake type") +
  ggtitle("All tows in inner SW corner of German") +
  theme(legend.position="bottom")+
  ylab("Number of scallops") +
  xlab("Size bin (5mm)")

# all tows in outer SW corner
outer <- ggplot() + geom_bar(data=sw_melt[sw_melt$int=="outer",], aes(x=variable, y=value, fill=rake), stat = "identity") + 
  facet_wrap(~year, nrow=2) +
  theme_bw() + theme(panel.grid=element_blank()) +
  geom_vline(data=sw_melt[sw_melt$int=="outer",], aes(xintercept=95), linetype="dashed")+
  geom_vline(data=sw_melt[sw_melt$int=="outer",], aes(xintercept=105), linetype="dashed") +
  scale_fill_manual(values=c("blue"), name="Rake type") +
  scale_x_continuous(breaks=seq(0, 200, 10)) +
  ggtitle("All tows in outer SW corner of German") +
  theme(legend.position="bottom") +
  ylab("Number of scallops") +
  xlab("Size bin (5mm)")


library(ggplot2)
library(gridExtra)
library(grid)

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}
png(paste0(direct, "/2019/Presentations/Survey_summary/test_figures/Ger/Rake_assessment.png"), height=8, width=14, units="in", res=400)
grid_arrange_shared_legend(map, all, inner, outer, ncol=2, nrow=2, position="right")
dev.off()


##### checking out the banquereau icelandic 2012 data
rowSums(bank.dat$BanIce[bank.dat$BanIce$year==2012, names(bank.dat$BanIce) %in% paste0("h", 175:200)])
bank.dat$BanIce[108,]
surv.Clap$BanIce[surv.Clap$BanIce$year==2012 & surv.Clap$BanIce$tow==bank.dat$BanIce[108,]$tow,]
## something is definitely fishy with the 2012 banquereau data because the mtwt data for tow 936 doesn't have any samples >94mm, 
## but here we say that that tow had scallops in bins 180-200. Where is the raw data?

## found multiple versions of "raw" data
require(readxl)
v1 <- read_xls(path = "Y:/Alan/TE13/DATA/Prelim/HFs/TE13Banqhf36.xls", skip = 27)
v1 <- v1[complete.cases(v1),]
names(v1) <- c("towno.", c(paste0("X", rep(901:936, each=4), ".", 0:3)))
v1 <- as.data.frame(v1[-1,])

v2 <- read.csv("Y:/Offshore scallop/Assessment/Data/Survey_data/2012/Spring/TE13BanIcehf.csv", skip = 1)
names(v2) <- c("towno.", c(paste0("X", rep(901:936, each=4), ".", 0:3)))
v2 <- v2[complete.cases(v2),]

require(compareDF)
compared <- compare_df(v1, v2, "towno.", limit_html = 1)
compared$change_summary
compared$comparison_df$`towno.`
names(which(apply(compared$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
# so we have a difference in tow 936 for columns >100/live and <100/dead in bins 75-95. 

require(reshape2)
convert_col_dat_to_bank_dat <- function(v=v1){
  v1melt <- melt(v[!v$`towno.` %in% "code",], id.vars = "towno.")
  v1melt$towno. <- seq(0,95, 5)
  
  live.small <- v1melt[grep(x=as.character(v1melt$variable), pattern=".0", fixed = T),]
  live.large <- v1melt[grep(x=as.character(v1melt$variable), pattern=".1", fixed = T),]
  dead.small <- v1melt[grep(x=as.character(v1melt$variable), pattern=".2", fixed = T),]
  dead.large <- v1melt[grep(x=as.character(v1melt$variable), pattern=".3", fixed = T),]
  
  live.small$state <- "live"
  live.large$state <- "live"
  dead.small$state <- "dead"
  dead.large$state <- "dead"
  
  live.small$variable <- gsub(x=live.small$variable, "X", "", fixed=T)
  live.small$variable <- gsub(x=live.small$variable, ".0", "", fixed=T)
  live.large$variable <- gsub(x=live.large$variable, "X", "", fixed=T)
  live.large$variable <- gsub(x=live.large$variable, ".1", "", fixed=T)
  dead.small$variable <- gsub(x=dead.small$variable, "X", "", fixed=T)
  dead.small$variable <- gsub(x=dead.small$variable, ".2", "", fixed=T)
  dead.large$variable <- gsub(x=dead.large$variable, "X", "", fixed=T)
  dead.large$variable <- gsub(x=dead.large$variable, ".3", "", fixed=T)
  
  live.large$towno. <- live.large$towno. + 100
  dead.large$towno. <- dead.large$towno. + 100
  
  all <- rbind(live.small, live.large, dead.small, dead.large)
  names(all) <- c("bin", "TOW_NO", "count", "state")
  all <- arrange(all, TOW_NO, bin)
  
  # there was no sub-sampling of icelandic scallops in 2012, so we just need the discoef column. we can get this from the DB for Sea scallops.
  chan <-dbConnect(dbDriver("Oracle"),username="keyserf", password="Decade06","ptran")
  db <- "SCALOFF" ### CHANGE HUMF TO SCALOFF!!!
  qu.tows <- paste0("select * from ", db, ".OSTOWS WHERE SURVEY_SEQ in (133)")
  qu.tows <- dbGetQuery(chan, qu.tows)
  dbDisconnect(chan)
  
  bank.dat.manual <- join(all, qu.tows[,c("TOW_NO", "DIS_COEF")], type="full")
  bank.dat.manual$std_count <- bank.dat.manual$count * bank.dat.manual$DIS_COEF
  bank.dat.manual$ID <- paste0(bank.dat.manual$bin, ".", bank.dat.manual$TOW_NO, ".", bank.dat.manual$state)
  
  return(bank.dat.manual)
}

bank.dat.manual.v1 <- convert_col_dat_to_bank_dat(v=v1)
bank.dat.manual.v2 <- convert_col_dat_to_bank_dat(v=v2)

compare_df(bank.dat.manual.v1, bank.dat.manual.v2, "ID")
# all other calculations match except for tow 936. 



##### comparing german SPR to simple means
spr <- spr.survey.obj$out.obj[[1]]
simple <- lined.survey.obj$model.dat

ger <- data.frame(year=spr$year, spr = spr$NPR, simple=simple$NPR)
require(reshape2)
ger <- melt(ger, id.vars="year")

ggplot() + geom_line(data=ger, aes(year, value, colour=variable)) +
  geom_point(data=ger, aes(year, value, colour=variable, shape=variable)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  ylab("NPR") +
  scale_x_continuous(breaks=seq(2008, 2019, 1)) +
  scale_colour_discrete(name="Estimate type")+
  scale_shape_discrete(name="Estimate type") +
  ggtitle("German pre-recruit estimates 2008-2019")

load(paste0(direct, "Data/Survey_data/2019/Survey_summary_output/BBn_figures_res_250-250.RData"))
#mod.res,proj,mesh,pred.in,

class(mod.res$`PR-spatial`)
max(mod.res$`PR-spatial`, na.rm=T)
which(mod.res$`PR-spatial`==max(mod.res$`PR-spatial`, na.rm=T))
mod.res$`PR-spatial`[21000:22000]
png(paste0(direct, "2019/Presentations/Survey_summary/Exploratory_figures/Ger/PR_spatial_with_tows.png"),width=11,height=8.5, units="in", res=400)
ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
           plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
           nafo.bord = T,nafo="all",nafo.lab = F,title="Pre-recruit abundances",dec.deg=F)
image(list(x = proj$x, y=proj$y, z = mod.res$`PR-spatial`), axes=F,add=T,breaks = c(0,1,5,10,50,100,500,1000,2000,5000, 10000),
      col= c(rev(plasma(10,alpha=0.7,begin=0.6,end=1))))
with(bank.live[bank.live$year==yr & bank.live$pre==max(bank.live$pre[bank.live$year==yr]),],points(lon,lat,cex=1,lwd=2,col="black"))
with(bank.live[bank.live$year==yr,],text(lon,lat,round(pre, 1),cex=0.5))
dev.off()


source("C:/Documents/Offshore scallop/Assessment/Assessment_fns/RUNME_Survey_OSAC_Model/Survey_Summary_Word.R")
Survey_Summary_Word(year=2019, reportseason="spring", data="C:/Documents/Offshore scallop/Assessment/Data/Survey_data/2019/Survey_summary_output/testing_results_SCALOFF_LE09.Rdata")
# objects: "bankcheck" df, ntows" df and "highlights" df

