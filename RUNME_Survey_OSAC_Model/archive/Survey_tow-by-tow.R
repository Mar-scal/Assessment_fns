##################################################   Survey Tow-by-tow #########################################################		
##################################################   Survey Tow-by-tow #########################################################		
##################################################   Survey Tow-by-tow #########################################################		
# Here I am looking at a series of diagnostics to get a deep understanding of what is happening with the survey data.
# The first part (and perhaps in the end the only stuff in here) is looking tow by tow to get a feel for what 
# happened in each tow for each bank on the survey. 

# Update History
# May 16, 2016:  Set up the ScallopMap function to select database you want to use by setting it at the start of the script.
# enables calling the database using 64 bit R.

yr = as.numeric(format(Sys.time(), "%Y"))  # 
direct = "d:/r/"
database = "ptran64" # Set this to your database, needed for a few called to get the bathymetry from ScallopMap. You'll also
# need to set your username/password to access the database, see the ScallopMap function calls.
library(viridis) # for colors...

# Load the survey data.  If you've compiled all the surveys use this...
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))
# Alternatively you might need to load one of these instead.
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_summer_results.Rdata",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))

# You may need to reload your R profile if you use it...
#source("d:/r/.Rprofile")
bnk <- c("GBa","GBb")# Once we have spring 2016 survey completed we should be able to add "Sab","BBs","Mid".
#bnk <- "BBn"
cf.lab <-    expression(paste("CF:",bgroup("(",frac(g,dm^3)   ,")")))
mc.lab <-    expression(paste("MC:",bgroup("(",frac(N,"500 g"),")"))) 

for(i in 1:length(bnk))
{

# Grab the survey data if it exists
survey.poly <- subset(survey.detail.polys, label == bnk[i])
survey.inf <- subset(survey.info, label == bnk[i])
bank.survey.poly <- survey.poly
bank.survey.info <- survey.inf
# If there isn't any data make it an NA
if(nrow(survey.poly) == 0) bank.survey.poly <- NA
if(nrow(survey.inf) == 0) bank.survey.info <- NA


# Here I am exploring the data to see if there is anything odd happening!
# This has the nice summary of all the tow data in it, subset into 2015 and let's look at what is happening in each tow.
bank.live<- subset(surv.Live[[bnk[i]]],year==yr)
bank.live <- bank.live[order(bank.live$tow),]

bank.clap <- subset(surv.Clap[[bnk[i]]],year==yr)
bank.clap <- bank.clap[order(bank.clap$tow),]
# Get the columns with the shell height frequencies.
cols <- which(names(bank.clap)=="h5"):which(names(bank.clap)=="h200")
# And the MW data for each shelf.
bank.mw <- subset(mw.dat.all[[bnk[i]]],year==yr)
bank.mw$CF_samp <- bank.mw$wmw/(bank.mw$sh/100)^3

# Check out the Clappers by "real" CF from the bank.mw...
# This gets the mean CF for each tow and adds that to the data 
tmp<- tapply(bank.mw$CF_samp,bank.mw$tow,mean)
cf.data <- as.data.frame(1:length(tmp))
cf.data$tow <- as.numeric(names(tmp))
cf.data$cf <- as.numeric(tmp)
cf.data <- cf.data[,-1]
cf.dat <- merge(bank.clap,cf.data,by.x="tow")
cf.dat <- subset(cf.dat,select = c("clap.prop","clap.propCom","clap.propRec","clap.propPre","tow","cf"))


###  Now start making the figures.
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


# Look at Condition factor from each tow, any stand out higher or low?
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/CF_by_tow.pdf",sep=""),width=11,height=8.5)
#windows(11,8.5)
plot(1:length(bank.live$CF),bank.live$CF,xaxt="n",xlab="tow ID", ylab="CF",bty="L",pch=20,type="n")
text(1:length(bank.live$CF),bank.live$CF,bank.live$tow,cex=0.5)
axis(1,at=seq(5,length(bank.live$CF),by=5),labels = c(bank.live$tow[seq(5,length(bank.live$CF),by=5)]))
title("Condition Factor by tow")
abline(h=mean(bank.live$CF,na.rm=T),col="blue",lty=2,lwd=1.5)

dev.off()


# Look at Condition factor from each tow but label as depth, any stand out higher or low?
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/CF_by_tow and_depth.pdf",sep=""),width=11,height=8.5)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_tow and_depth.pdf",sep=""),
    width=11,height=8.5)
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


pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_tow.pdf",sep=""),width=11,height=8.5)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_depth.pdf",sep=""),width=8.5,height=11)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_total_abundance.pdf",sep=""),
    width=8.5,height=11)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_fully_recruited_abundance.pdf",sep=""),
    width=8.5,height=11)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_by_pre_recruit_abundance.pdf",sep=""),
    width=8.5,height=11)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clapper_numbers_by_tow.pdf",sep=""),width=11,height=8.5)
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


# Now lets look at biomass and abundance trends by tow
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Numbers_by_tow.pdf",sep=""),width=11,height=8.5)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Biomass_by_tow.pdf",sep=""),width=11,height=8.5)
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


pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Mean_indy_sh_and_mw.pdf",sep=""),width=11,height=8.5)
#windows(11,8.5)
par(mfrow=c(2,1),mar=c(2,4,1,1))
plot(1:length(bank.live$l.bar),bank.live$l.bar,xaxt="n",xlab="tow ID", ylab="Mean SH (mm)",
     bty="L",pch=20,type="n",ylim=c(min(bank.live$l.bar,na.rm=T),max(bank.live$l.bar,na.rm=T)))
text(1:length(bank.live$tow),bank.live$l.bar,round(bank.live$tow),cex=0.8)
text(10,max(bank.live$l.bar,na.rm=T),"Shell Height (mm)")
title("Mean Shell Height and Meat Weigth by tow (Fully Recruited)")
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

pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/MW_and_SH_and_CF_by_depth.pdf",sep=""),
    width=11,height=8.5)
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



############## Note that On this figure some true zero's will get a very small value so they can be plotted.
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Abundance_by_depth.pdf",sep=""),width=8.5,height=11)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Biomass_by_depth.pdf",sep=""),width=8.5,height=11)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/tow_locations.pdf",sep=""),width=11,height=8.5)
#windows(11,8.5)
if(is.na(bank.survey.info) ==F)
{
ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
           plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
           nafo.bord = T,nafo="all",nafo.lab = F,title="Tow locations",dec.deg=F)
} # end if(is.na(bank.survey.info) ==F)

if(is.na(bank.survey.info) ==T)
{
ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
           plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
           nafo.bord = T,nafo="all",nafo.lab = F,title="Tow locations",dec.deg=F)
} # end if(is.na(bank.survey.info) ==T)

with(bank.live,text(lon,lat,tow,cex=0.5))
#addPolys(GBa.boxes,lty=2,lwd=2)
#addPolys(BBboxes,lty=2,lwd=2)

if(is.na(bank.survey.info) ==F)
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
legend(-66,41.45,legend = as.numeric(with(bank.live,tapply(tow,new.stratum,length))),
       fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
       pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
       pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.na(bank.survey.info) ==F)
dev.off()
############

############
# Here's the Raw CF data by tow.
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/CF_samples_by_tow_or_depth.pdf",sep=""),
    width=15,height=8.5)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/MW_by_tow_and_depth.pdf",sep=""),
    width=15,height=8.5)
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/SH_by_tow_and_depth.pdf",sep=""),
   width=15,height=8.5)
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
      
 

# Now for the clappers by CF, any indication low CF is associated with High clapper numbers.
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/Clappers_vs_CF.pdf",sep=""),width=8.5,height=11)
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
clap.year <- subset(clap.survey.obj[[bnk[i]]][[1]], year == yr)
this.year <- subset(survey.obj[[bnk[i]]][[1]], year == yr)
df <- data.frame(BM = rep(NA,3),N = rep(NA,3),Clap = rep(NA,3),row.names=c("FR","Rec","PR"))

if(bnk[i] == "Ger") this.year <-  subset(spr.survey.obj[[1]], year == yr)
if(bnk[i] %in%  c("Sab","BBs","GBa","BBn","GBb"))
{
  # Get biomasses
  df[1,1] <-  this.year$I / sum(bank.survey.info$towable_area)*10^6
  df[2,1] <- this.year$IR / sum(bank.survey.info$towable_area)*10^6
  df[3,1] <-  this.year$IPR / sum(bank.survey.info$towable_area)*10^6
  # Get abundances
  df[1,2] <-  this.year$N / sum(bank.survey.info$towable_area)*10^6
  df[2,2]<- this.year$NR / sum(bank.survey.info$towable_area)*10^6
  df[3,2] <-  this.year$NPR / sum(bank.survey.info$towable_area)*10^6
  
  # Get clappers

  df[1,3]  <- clap.year$N / sum(bank.survey.info$towable_area)*10^6
  df[2,3]<- clap.year$NR / sum(bank.survey.info$towable_area)*10^6
  df[3,3]  <- clap.year$NPR / sum(bank.survey.info$towable_area)*10^6

} # end if(bnk[i] %in%  c("Sab","BBs","GBa","BBn","GBb"))

if(bnk[i] %in% c("Ger","Mid","GB"))
{
  # Get biomasses
  df[1,1] <-  this.year$I 
  df[2,1] <- this.year$IR 
  df[3,1] <-  this.year$IPR 
  # Get abundances
  df[1,2] <-  this.year$N 
  df[2,2] <- this.year$NR 
  df[3,2] <-  this.year$NPR 
  # Get clappers
  df[1,3]  <- clap.year$N 
  df[2,3] <- clap.year$NR 
  df[3,3]  <- clap.year$NPR 
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
pdf(file=paste(direct,yr,"/Presentations/Survey_summary/Exploratory_figures/",bnk[i],"/tow_locations_2014.pdf",sep=""),width=11,height=8.5)
#windows(11,8.5)
if(is.na(bank.survey.info) ==F)
{
  ScallopMap(bnk[i],poly.lst=list(bank.survey.poly,bank.survey.info),direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , 
             nafo.bord = T,nafo="all",nafo.lab = F,title="Tow locations",dec.deg=F)
} # end if(is.na(bank.survey.info) ==F)

if(is.na(bank.survey.info) ==T)
{
  ScallopMap(bnk[i],direct = direct,ylab="",xlab="",un=un.ID,pw=pwd.ID,db.con=database,
             plot.bathy=T,plot.boundries = T,bathy.source="quick", cex.mn=2,bathcol = baths , isobath = c(seq(50,150,by=50)),
             nafo.bord = T,nafo="all",nafo.lab = F,title="Tow locations",dec.deg=F)
} # end if(is.na(bank.survey.info) ==T)

with(bank.live,text(lon,lat,tow,cex=0.5))
#addPolys(GBa.boxes,lty=2,lwd=2)
#addPolys(BBboxes,lty=2,lwd=2)

if(is.na(bank.survey.info) ==F)
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
  legend(-66,41.45,legend = as.numeric(with(bank.live,tapply(tow,new.stratum,length))),
         fill=c(bank.survey.info$col),border=c(rep('black',length(bank.survey.info$PName))),
         pch=c(rep(NA,length(bank.survey.info$PName))),title = "Number of tows",title.adj=0.1,
         pt.bg = c(rep(NA,length(bank.survey.info$PName))),col='black',bty="n",bg="white")
} # end if(is.na(bank.survey.info) ==F)
dev.off()
############
