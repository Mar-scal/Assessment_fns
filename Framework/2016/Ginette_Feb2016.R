#################################################################### DK Feb 2016
## This file is a little script I threw together with some special plots I made for a meeting with Ginette in Feb of 2016


# Clear out everything
rm(list=ls())
direct = "d:/r/"
yr = as.numeric(format(Sys.time(), "%Y")) -1

# re-load rprofile if you have one...
source(".Rprofile")

library(lattice)
library(gamm4)
library(akima)


# First lets load the data
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))

# I'm really only interested in GBa for the moment...
windows(8.5,11)
par(mfrow=c(3,1))
plot(survey.obj$GBa$model.dat$N/max(survey.obj$GBa$model.dat$N)~survey.obj$GBa$model.dat$year,type="o",
     pch=16,ylim=c(0,1),ylab="Abundance (% of max)",xlab="")
lines(survey.obj$GBa$model.dat$clappers/max(survey.obj$GBa$model.dat$clappers)~survey.obj$GBa$model.dat$year,
      type="o",pch=22,col="blue",bg="blue",lty=2)
legend("topright",c("Live","Dead"),col=c("black","blue"),pt.bg=c("black","blue"),pch=c(16,22),bty="n",lty=c(1,2))
# Wow, that's interesting, the clapper time series lags the Abundance time series by a year...
ccf(survey.obj$GBa$model.dat$N,survey.obj$GBa$model.dat$clappers,main="Correlation between Abundance and Clappers")

plot(survey.obj$GBa$model.dat$clappers[2:30]/(survey.obj$GBa$model.dat$N[2:30]+survey.obj$GBa$model.dat$clappers[2:30]) ~ 
       as.numeric(survey.obj$GBa$model.dat$N[1:29]/100),type="p",pch=16,ylim=c(0,0.2),
     ylab="Clappers (% at time t+1)",xlab="Abundance of Live at time t")
abline(lm(survey.obj$GBa$model.dat$clappers[2:30]/survey.obj$GBa$model.dat$N[2:30]~as.numeric(survey.obj$GBa$model.dat$N[1:29]/100)))

# Might be something there, maybe...
summary(lm(survey.obj$GBa$model.dat$clappers[2:30]/(survey.obj$GBa$model.dat$N[2:30]+survey.obj$GBa$model.dat$clappers[2:30]) ~ 
             as.numeric(survey.obj$GBa$model.dat$N[1:29]/100)))

# This looks like a MA(1) (moving average order 1) process, with the one year lag representing about 40-50% of information in current year.
# x(t) = w(t) + 0.45w(t-1)
acf(survey.obj$GBa$model.dat$N,main="ACF abundance")
acf(survey.obj$GBa$model.dat$clappers,main="ACF Clappers")
pacf(survey.obj$GBa$model.dat$N,main="ACF abundance")
pacf(survey.obj$GBa$model.dat$clappers,main="ACF Clappers")

# What about Browns North
windows(8.5,11)
par(mfrow=c(3,1))
plot(survey.obj$BBn$model.dat$N/max(survey.obj$BBn$model.dat$N)~survey.obj$BBn$model.dat$year,type="o",
     pch=16,ylim=c(0,1),ylab="Abundance (% of max)",xlab="")
lines(survey.obj$BBn$model.dat$clappers/max(survey.obj$BBn$model.dat$clappers)~survey.obj$BBn$model.dat$year,
      type="o",pch=22,col="blue",bg="blue",lty=2)
legend("topright",c("Live","Dead"),col=c("black","blue"),pt.bg=c("black","blue"),pch=c(16,22),bty="n",lty=c(1,2))
# Not so clear cut on BBn, lag 1 remains important, but biggest correlation is lead 1, dead numbers leading abundance #'s
ccf(survey.obj$BBn$model.dat$N,survey.obj$BBn$model.dat$clappers,main="Correlation between Abundance and Clappers")
# Any lag 1 trend, nope nothing here.
plot(survey.obj$BBn$model.dat$clappers[2:25]/(survey.obj$BBn$model.dat$N[2:25]+survey.obj$BBn$model.dat$clappers[2:25]) ~ 
       as.numeric(survey.obj$BBn$model.dat$N[1:24]/100),type="p",pch=16,ylim=c(0,0.2),
     ylab="Clappers (% at time t+1)",xlab="Abundance of Live at time t")
abline(lm(survey.obj$BBn$model.dat$clappers[2:25]/survey.obj$GBa$model.dat$N[2:25]~as.numeric(survey.obj$GBa$model.dat$N[1:24]/100)))


