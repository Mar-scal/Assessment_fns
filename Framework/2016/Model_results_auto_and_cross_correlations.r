#### This used to be in the update document.  These are just some model results DK was messing around with in Feb of 2016
#### DK may never use this again, but some potentially interesting things going on in here...
direct = "d:/r/"
yr = 2015

load(file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))
# Just curious, is there a correlation between the process residuals on BBn and GBa.
# There is a pretty strong lag -2 signal, interesting... Looks like BBn process
# residuals are doing the same as the GBa just a couple years later...
# This is the plot of the correlation between process residuals on Georges and Browns bank

ccf(DD.out[["GBa"]]$mean$sPresid[7:30],DD.out[["BBn"]]$mean$sPresid)
plot(DD.out[["GBa"]]$mean$sPresid[7:30],type="l")
lines(DD.out[["BBn"]]$mean$sPresid,type="l",col="blue",lwd=1.5)

# Also process error seems to have autocorrelation
# Nothing on GBa in terms of acf
acf(DD.out[["GBa"]]$mean$sPresid)
# pacf has a MA flavour to it but nothing clear-cut
pacf(DD.out[["GBa"]]$mean$sPresid)

# Browns resdiuals look like a solid AR1 process, pacf suggests there could be some MA too, so perhaps a combo of both.
acf(DD.out[["BBn"]]$mean$sPresid)
pacf(DD.out[["BBn"]]$mean$sPresid)


# On this line of thought, is there any autocorrelation in the exploitation/natural mortality terms?
# There might be a hint of an MA1 process here where the most recent value is useful, see example of a MA1
acf(DD.out[["GBa"]]$mean$m)
pacf(DD.out[["GBa"]]$mean$m)
acf(DD.out[["GBa"]]$mean$mR)
pacf(DD.out[["GBa"]]$mean$mR)
# The exploitation sure looks a lot like an AR1 with the decay to 0 in correlation, but it could also be the trend...
acf(DD.out[["GBa"]]$mean$mu)
pacf(DD.out[["GBa"]]$mean$mu)
# Indeed the AR1(ish)ness seems to disappear when we look at the data detreneded, though again potentially an MA1 process on detrended
# Not significant but both acf and pacf have MA1 qualities..
acf(resid(lm(DD.out[["GBa"]]$mean$mu~c(1:30))))
pacf(resid(lm(DD.out[["GBa"]]$mean$mu~c(1:30))))

## On Browns North what do we see... Nothing!
acf(DD.out[["BBn"]]$mean$m)
pacf(DD.out[["BBn"]]$mean$m)
# Maybe a weak (0.3ish) AR1 process here, maybe
acf(DD.out[["BBn"]]$mean$mR)
pacf(DD.out[["BBn"]]$mean$mR)
# Nothing witht he exploitation rate here either...
acf(DD.out[["BBn"]]$mean$mu)
pacf(DD.out[["BBn"]]$mean$mu)


# An example of ACF and pacf for a MA1 with correlation around 0.3 which isn't dis-similar to what we are seeing with our data on Georges A...
ma1acf = ARMAacf(ma = c(.3),lag.max = 14, pacf=F)
plot(ma1acf,type="h", main = "Theoretical ACF of MA(1) with theta = 0.3")
ma1pacf = ARMAacf(ma = c(.3),lag.max = 14, pacf=TRUE)
plot(ma1pacf,type="h", main = "Theoretical PACF of MA(1) with theta = 0.3")
