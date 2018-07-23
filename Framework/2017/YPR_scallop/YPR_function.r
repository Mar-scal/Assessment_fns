#############################  Here I am making a YPR function for Scallop, I"m not sure yet what this will look like but
############################   hoping i can make a general function that can provide YPR using existing scallop data
############################   Typical YPR models are age based, I am making this a stage (size) based YPR model using our growth information
############################   Not really sure if anyone has done this before, only a slight tweak on the age based YPR ideas...

direct <- "d:/r/"
year <- 2017
###  Load in any necessary packages
library(fishmethods)
library(reshape2)


# So I need a growth curve, which we have from the increment data, using that we can project ahead how much each size class will grow
# into next year.  Let's start this with some scallop data to get moving, I think the SHF data + growth curve information should work together...

load(paste(direct,"data/Framework/2017/Growth/BoF_Annual_curves_vs_one_curve_model_results.RData",sep=""))

# Since I'm doing the YPR for offshore (just for the moment), but only have good inshore growth increment data I'm just going to 
# Get predicted data from the "one curve" model which treats all years/areas in this data to be the same.  This will be simple to mange
# once we have area specific growth information.  

#I am also going to do the predictions in 5 mm size bins starting with the 35 mm size bin so our pred.dat just needs 37.5, 42.5, etc.
# Now inside the model we use "standardized" sizes rather thant he actual sizes, so we need a column of the standarized sizes and the actual
#sizes
size.2.pred <- seq(35,200,.1) # This gets us both the mid-point of each size bin plus the 0.5 mm increment, we may need both later on..
stnd.size.2.pred <- size.2.pred - attr(dat.tmp$std_size,"scaled:center")
pred.dat <- data.frame(size = size.2.pred,std_size = stnd.size.2.pred)
pred.dat$predicted <- predict(mod.res.rand.one.curve$gam,pred.dat,type="response")
pred.dat$se <-  predict(mod.res.rand.one.curve$gam,pred.dat,type="response",se=T)$se.fit
# Throw down some confidence intervals...
pred.dat$LCI <-  pred.dat$predicted - 2*pred.dat$se 
pred.dat$UCI <- pred.dat$predicted + 2*pred.dat$se

# So now we have the annual expected growth for the scallop.
load(paste(direct,"data/Survey_data/2017/Survey_summary_output/Survey_spring_results.RData",sep=""))
bnk <- "BBn"
# Using the MW-SH relationship for the bank we can get the weight of individuals for each of the sizes we have outlined.
pred.dat$weight <- SpatHtWt.fit[[bnk]]$A*(pred.dat$size/100)^SpatHtWt.fit[[bnk]]$B

# Now pull in the abundances for the SH's
head(SHF.summary[[bnk]])
names(SHF.summary[[bnk]]) <- c("year",paste("h",seq(2.5,197.5,by=5),sep=""),"bank")

# So let's look at the picture from 2015, I trim everything here so that we only have the N for the size classes from 
SHF.2015 <- SHF.summary[[bnk]][SHF.summary[[bnk]]$year == 2015,c(-1,-42)]
SHF.2015 <- SHF.2015[1,-c(1:7)]
#row.names(SHF.2015) <- "N"

# Now get the current biomass for each size category.
SHF.2015[2,] <- SHF.2015[1,] * pred.dat$weight[pred.dat$size %in% seq(2.5,197.5,by=5)]
#names(SHF.2015) <- c("N","BM")
# Transpose the data so it is easier to work with...
SHF.2015 <- as.data.frame(t(SHF.2015))
names(SHF.2015) <- c("N","BM")
# Now we can pop in some fake numbers and try to watch this cohort move through time.
plot(SHF.2015$N,type="o")
plot(SHF.2015$BM,type="o")

# Now we start with these recruits and our growth information we can "make" up some YPR scenarios
years <- 20 # The number of years of growth for the cohort until I stop tracking it, 20 seems pretty reasonable (170 mm shells)
res <- data.frame(size = rep(NA,years), growth = rep(NA,years))
# This is the Number of recruits per tow, below I get number of recruits in millions so careful not to confuse the two (I did check and these
# are equivalent happily (just need to * this by strata area (in towable units) and divide that by 1e6)
N.rec.per.tow <- sum(SHF.2015$N[rownames(SHF.2015) %in% c("h67.5","h72.5")])
for(i in 1:years) 
{
  if(i == 1)
  {
    
    # First let's get an average size for our recruits.  I can't use N.rec here b/c it is in 
    res$size[i] <- signif(67.5*SHF.2015$N[rownames(SHF.2015) %in% c("h67.5")]/N.rec.per.tow + 
                            72.5*SHF.2015$N[rownames(SHF.2015) %in% c("h72.5")]/N.rec.per.tow,digits=3)
    # Now we can see how much these recruits will grow this year.
    res$growth[i] <- pred.dat$predicted[pred.dat$size == res$size[i]]
  }
  #if( i== 2) res$size[i] <- signif(res$size[i-1] + res$growth[i-1],digits=3)
  if(i >= 2)
  {
  res$size[i] <- signif(res$size[i-1] + res$growth[i-1],digits=3)
  res$growth[i] <- pred.dat$predicted[pred.dat$size == as.character(res$size[i])]
  }
} # for(i in 1:years) 


# So now we have the size that the scallop will be each year, we can get the weight of the individuals using the MW/SH relationship as we did above..
res$mw <- SpatHtWt.fit[[bnk]]$A*(res$size/100)^SpatHtWt.fit[[bnk]]$B
res$mc <- 500/res$mw # This gets us the meat count
#load(paste(direct,"data/Model/2017/BBn/Results/Survey_spring_results.RData",sep=""))
# So lets roll with 2015 recruits and look what long term YPR is for this group given the above.
# Get the numbers and biomasses into millions/tonnage on the bank...
# Note taht the model doesn't output the numbers (biomasses yes), so I'm just going to use the survey results
# and divide those by the survey catchability, this gets us within 10% on the biomass so a very good estimate of the recruits on the bank...
q <- 0.351418 # catchability from 2017 model on BBn.
N.rec <- sum(SHF.2015$N[rownames(SHF.2015) %in% c("h67.5","h72.5")])/q
#N.rec <- survey.obj$BBn$model.dat$NR[survey.obj$BBn$model.dat$year == 2015]/q
# Total biomass a little off what our survey says b/c
tot.bm <- survey.obj$BBn$model.dat$IR[survey.obj$BBn$model.dat$year == 2015]/q # This is relatively close to our N * mw calculcation
# slightly off as this uses condition but below I just use the MW...

# Now we can start playing with M and F.mort using some classic fisheries biology... 
Ms <- seq(0.05,0.25,by=0.05)
# To convert from proprotional mortality to instantaneous m <- -log(1-M)
delay.FM <- 0:7
FM <- seq(0.05,1,by=0.05)
# Let's add in some economics, based on some seemingly reasonable data from 2015 US we can get a price
mc.categories <- c(80,60,50,40,30,20,10) # These need to be in order from largest meat counts (so smallest size) to smallest meat counts
mc.names <- NA
for(i in 1:(length(mc.categories)+1))
{
if(i == 1) mc.names[i] <- paste(mc.categories[i],"_minus",sep="") 
if(i > 1 && i <= length(mc.categories)) mc.names[i] <- paste(mc.categories[i],"-",mc.categories[i-1],sep="")  
if(i > length(mc.categories)) mc.names[i] <- paste(mc.categories[i-1],"_plus",sep="") 
}# end for(i in 1:length(mc.categories))
# This is the price you get for 1 tonne of scallop, I'm assuming $40/kg for 30-40 counts, maybe a bit high, but not
# unreasonable, in the end its the relative difference that matters...
price.index <- c(rep(40000,(length(mc.categories)+1)))
# This is roughly based on the figure I found online, somewhere there must be better data, curious if anyone would be willing to share...
price.index <- c(10000,11000,12000,15000,20000,22500,25000,30000)
# Here's one where each count category gets a 10% premium, again $20000 for 30-40 counts
price.index <- c(20000*0.9^4,20000*0.9^3,20000*0.9^2,20000*0.9,20000,20000*1.1,20000*1.1^2,20000*1.1^3)
econ <- data.frame(mc.names = mc.names,index = price.index)
# Now tie the price index to the meat counts.
for(i in 1:nrow(econ))
{
  if(i == 1) res$value.per.tonne[res$mc >= mc.categories[i]] <- econ$index[i]
  if(i > 1 && i < nrow(econ)) res$value.per.tonne[res$mc >= mc.categories[i] & res$mc < mc.categories[i-1]] <- econ$index[i]
  if(i == nrow(econ)) res$value.per.tonne[res$mc < mc.categories[i-1]] <- econ$index[i]
} # end for(i in 1:nrow(econ))


ypr <- NULL # should be a list with the length of length(Ms) * length(delay.FM) * length(FM)
results <- NULL # A list with the detailed results for each run.
count <- 0 # start the counter at 0.
# Now loop through all of these iterations and see how ypr changes....
for(m in 1:length(Ms))
{  
  for(d in 1:length(delay.FM))
  {
    for(f in 1:length(FM))
    {
count <- count + 1 # A counter for the ypr list

# I played around with "delaying" the fishery until the end of the first year, but it doesn't really do much of anything, basically it is the same
# as just moving the first size bin one step (i.e. if you don't harvest the recruits until the end of the year after they have grown
#then for the purposes of these simple little models we might as well just start with the next size bin up and model from there).
# What this is currently is simply the yield per recruit assuming that mortality starts at a certain size bin which I think is cleaner then
# messing around with mortatlities.
res$F.mort <- c(rep(0,(delay.FM[d])),rep(FM[f],(years-delay.FM[d]))) # Instantaneous Fishery mortality (1-exp(-F.mort) gets proportional 
# rate just in case you want to look at that...)
res$M <- Ms[m] # Instantaneous natural mortality
res$Z <- res$F.mort + res$M  # Total instantaneous rate of mortality
t.step <- 1 # The time step, generally this will just be 1 year...


# Exponential decay
for(i in 1:years)
{
  if(i == 1)  res$N[i] <- N.rec*exp(-res$Z[i]*t.step)
  if(i > 1)  
  {
    res$N[i] <- res$N[i-1]*exp(-res$Z[i]*t.step)
  } # enc if(i > 1)  
  res$catch[i] <- (res$F.mort[i]/res$Z[i]) * res$N[i]* (1-exp(-res$Z[i])) # Barnov's catch equation..
} # end for(i in 1:years)
# Get the biomass now
res$BM <- res$mw*res$N
res$catch.BM <- res$mw*res$catch
res$size_at_first_catch <- ifelse(any(res$F.mort == 0), res$size[which(res$F.mort > 0)[1]],res$size[1])
res$NM <- Ms[m]
res$FM <- FM[f]
res$delay.FM <- delay.FM[d]
res$landed.value <- res$catch.BM * res$value.per.tonne / 1e6 # This gets the landed value in millions of $$
# And now we can get the YPR, basically in this model we catch X tonnes of scallop for every 1 million recruits.
# Or X grams of scallop for every single recruit, so on average we get 7 grams of scallop for every recruit with F = 0.2, and M =0.2
ypr[[count]] <- data.frame(ypr = sum(res$catch.BM,na.rm=T)/N.rec,bpr = sum(res$BM,na.rm=T)/N.rec,
                           mc = 500*sum(res$catch,na.rm=T)/sum(res$catch.BM,na.rm=T),
                           landed.value = sum(res$landed.value),
                           vpr = sum(res$landed.value) / N.rec, # This is how much money (in dollars) that is earned per recruit we see.
                           N.harvest = sum(res$catch),
                           prop.harvest = sum(res$catch)/N.rec,
                           M = Ms[m], FM = FM[f],delay.FM = delay.FM[d],
                           size = res$size,FR_size = res$size_at_first_catch)
## Here I save each individual run
results[[count]] <- res
}}} # close the 3 loops.

ypr.res <- do.call("rbind",ypr)
complete.res <- do.call("rbind",results)

ypr.res[ypr.res$ypr == max(ypr.res$ypr),] # So the best strategy overall is to wait 7 years and harvest these at 0.19 if M is low (0.05)
ypr.res[ypr.res$vpr == max(ypr.res$vpr),] # also economically the best strategy overall is to wait 7 years and harvest these at 0.2 if M is low (0.05)
aggregate(ypr~delay.FM+FM,ypr.res,FUN = "max")

quick.summary <- aggregate(cbind(landed.value,vpr,ypr,N.harvest,prop.harvest)~delay.FM+FM+M,ypr.res,FUN="mean")

quick.summary[quick.summary$M == 0.2,]
# What we are really really interested in is harvesting at a size that optimally trades off %biomass increase with mortality.
for(i in 2:years) res$prop.BM.growth[i-1] <-  res$mw[i]/res$mw[i-1]

# First set up the plot template for the plots where we are looking at, here we want a line plot a grid using M and FR_size
m_fr_plot <- ggplot(ypr.res,aes(FM,ypr)) + 
              geom_line(,color="blue")+ facet_grid(M~ FR_size) + 
              geom_vline(xintercept = 0.2,linetype = 2,colour = "orange") # All plots have a vertical line at whatever F you want to pick here.
             
  # This looks at YPR vs FM for year FM, it's all set up above so just need to add the abline...
  # First set up the plot template
  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  ypr_fm <- m_fr_plot  + geom_abline(slope = 0,intercept = 10,linetype = 2) 
  windows(11,11); ypr_fm
  
  # This looks at Meat count vs FM for year FM
  mc_fm <-m_fr_plot + aes(FM,mc) + geom_abline(slope = 0,intercept = 40,linetype = 2) 
  windows(11,11); mc_fm  
  
  # This looks at VPR vs FM for year FM
  vpr_fm <- m_fr_plot + aes(FM,vpr) + geom_abline(slope = 0,intercept = 0.2,linetype = 2)
  windows(11,11); vpr_fm

  # This looks at BPR vs FM for year FM
  bpr_fm <- m_fr_plot + aes(FM,bpr) 
  windows(11,11); bpr_fm

  # This looks at the total landed value (in millions) vs FM for year FM for a cohort (in this case the cohort had 256,000,000 scallop)
  landval_fm <- m_fr_plot + aes(FM,landed.value) + geom_abline(slope = 0,intercept = 45,linetype = 2) +
  windows(11,11); landval_fm
  
  # This looks at proportion of the initial population that is harvested vs FM for year FM
  pharv_fm <- m_fr_plot + aes(FM,prop.harvest)  
  windows(11,11); pharv_fm
  
  # This looks at total number of scallops harvested 
  nharv_fm <- m_fr_plot + aes(FM,N.harvest)
  windows(11,11); nharv_fm

  
#############################  FISHERY DATA, LOOKING FOR A "HANDLING TIME" metric ############################################
# Now we also could get the fishery data and see if we can have a go at finding a handling time for the
# WF and FT vessels.  This won't be perfect but lets see if anything pops out.
  
# Get the fishery data from everywhere
logs_and_fish(loc="offshore",year = 1981:yr,un=un,pw=pwd,db.con=db.con,direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

aggregate(cbind(numtow,avgtime,h,hm,pro.repwt)~fleet+year,fish.dat,FUN=mean)
# Now I want to look only in high abundance areas so that I'm comparing apples to apples for catch rates
# Perhaps the easiest place to start is 2017 on BBn since 92% of the catch came from inside the boxes...
dat.2017 <- fish.dat[fish.dat$year == 2017,]  
aggregate(pro.repwt ~ fleet + bank ,dat.2017, FUN = mean)
aggregate(kg.hm ~ fleet + bank ,dat.2017, FUN = mean)
aggregate(hm ~ fleet + bank ,dat.2017, FUN = median)
aggregate(hm ~ fleet + bank ,dat.2017, FUN = sum)
aggregate(cbind(numtow,avgtime,h,pro.repwt) ~ fleet + bank ,dat.2017, FUN = mean)

aggregate(cbind(numtow,avgtime,h,hm,pro.repwt)~fleet+year,fish.dat,FUN=max)



aggregate(cbind(hm,h,pro.repwt,kg.hm) ~ fleet + bank ,dat.2017, FUN = mean)
aggregate(cbind(hm) ~ tripnum+bank+fleet ,dat.2017, FUN = sum)

tmp <- dat.2017
tmp$fleet <- ifelse(tmp$fleet == "FT",1,2)
aggregate(cbind(hm,h,pro.repwt,kg.hm,fleet) ~ ves + bank ,tmp, FUN = mean)



# Perhaps some images might be helpful, first let's try a histogram, jusst looking at cpue per watch
  
p <- ggplot(dat.2017, aes(kg.hm)) +
  geom_histogram(color="blue") +                   # Now add lines?
  facet_wrap(~fleet ,ncol=2)+  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  #geom_abline(slope = 0,intercept = 10,linetype = 2) +
  geom_vline(xintercept = 0.2,linetype = 2,colour = "orange")
windows(11,11); p

# This is super interesting, compare the fleet dynamics over time and watch how the WF changes as the FT's roll in
# Important to break it at 2009 when they start reporting every 6 hours
# What about catch/watch, what's the most catch ever seen in a watch... Very rare does a WF vessel get above 700 kg/watch
p <- ggplot(fish.dat[fish.dat$year > 2001 & fish.dat$year <= 2008 & fish.dat$pro.repwt > 0 & !is.na(fish.dat$pro.repwt), ], aes(pro.repwt/4)) +
  geom_histogram(color="blue",binwidth = 50) +                   # Now add lines?
  facet_wrap(~year +fleet ,ncol=4)#+  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  #geom_abline(slope = 0,intercept = 10,linetype = 2) +
  #geom_vline(xintercept = 0.2,linetype = 2,colour = "orange")
windows(11,11); p

# What about catch/watch, what's the most catch ever seen in a watch... Very rare does a WF vessel get above 700 kg/watch
p <- ggplot(fish.dat[fish.dat$year >2008  & fish.dat$pro.repwt > 0 & !is.na(fish.dat$pro.repwt),], aes(pro.repwt)) +
  geom_histogram(color="blue",binwidth = 50) +    
  facet_wrap(~year + fleet ,ncol=6,scales= "free_x")#+  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
#geom_abline(slope = 0,intercept = 10,linetype = 2) +
#geom_vline(xintercept = 0.2,linetype = 2,colour = "orange")
windows(11,11); p


# What about catch/watch, what's the most catch ever seen in a watch... Very rare does a WF vessel get above 700 kg/watch
p <- ggplot(fish.dat[fish.dat$year <= 2001  & fish.dat$pro.repwt > 0 & !is.na(fish.dat$pro.repwt),], aes(pro.repwt/4)) +
  geom_histogram(color="blue",binwidth = 50) +                   
  coord_cartesian(xlim = c(0, 1200)) + # limit the x axis for these plots, you can check but there is nothing out there (maybe 1 or 2 points..)
  facet_wrap(~year ,ncol=6)#+  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
#geom_abline(slope = 0,intercept = 10,linetype = 2) +
#geom_vline(xintercept = 0.2,linetype = 2,colour = "orange")
windows(11,11); p

fish.dat$fleet.fac <- ifelse(fish.dat$fleet == "WF",1,2)

# What's the relationship between catch and effort on a watch.  You can see from these data that as effort on a watch 
# increases that the total catch generally is lower, and indication that in lower abundance scenarios they are limited by 
# their search time.  As abundance increased their search time tends to decline, for the FT vessels with automatci
# shucking machines it seems they can easily process 2000 tonnes of meats per watch, there the limitation seems less search time and
# more how quickly you can get the gear back in the water.  This suggests that these high abundance trips probably 
# have very short duration tows.
scat.plot <- ggplot(data=fish.dat[fish.dat$year > 2008,], aes(y=pro.repwt,x=hm,colour = fleet,group=fleet)) +
  geom_point(size=0.2,alpha=0.25) +
  geom_smooth(method = "auto") +
  facet_wrap(~year ,ncol=6) + # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  scale_color_manual("Fleet",values = c("WF" = "blue","FT" = "red"))
windows(11,11);scat.plot

# WHat is the relationship between catch and the average lenght of the tows.  Ha, check that out, as the shuckers come on line
# you can see the fisheries diverge, the FT's are runing very short duration tows when abundance is higher, whereas the freezer
# trawlers are generally doing what they always do, very little relationship between tow duration and abundance for WF
# very clear relationship for the FT's (a combo of sensors saying when gear is full and shuckers having processing capabilities)
windows(11,11)
scat.plot +  aes(y=pro.repwt,x=avgtime,colour = fleet,group=fleet)

# Can see that the average time on the bottom for the Freezer trawlers has declined which the number of tows in a watch has increased, this
# tradeoff has resulted in having their gear on the bottom for the same amount of time as in the past, but clearly from the above they
# can recognize when the gear is full in high density areas and they pull the gear back in earlier than the WF vessels do.
aggregate(avgtime~fleet+year,fish.dat,FUN=mean)
aggregate(numtow~fleet+year,fish.dat,FUN=mean)
aggregate(h~fleet+year,fish.dat,FUN=mean)
aggregate(hm~fleet+year,fish.dat,FUN=mean)


# WHat is the relationship between catch and number of tows.  Ha, check that out, as the shuckers come on line
# you can see the fisheries diverge, the FT's are runing very short duration tows when abundance is higher, whereas the freezer
# trawlers are generally doing what they always do, very little relationship between tow duration and abundance for WF
# very clear relationship for the FT's (a combo of sensors saying when gear is full and shuckers having processing capabilities)
windows(11,11)
scat.plot+ aes(y=pro.repwt,x=numtow,colour = fleet,group=fleet)

# What's the relationship between effort and the number of tows....
windows(11,11)
scat.plot+ aes(y=hm,x=numtow,colour = fleet,group=fleet)

# This is the one we want, what's the relationship between number of tows and tow time!
windows(11,11)
scat.plot+ aes(y=avgtime,x=numtow,colour = fleet,group=fleet)











###########  Other stuff...##############################  Other stuff...##############################  Other stuff...##############################  Other stuff
##########  Other stuff...##############################  Other stuff...##############################  Other stuff##############################  Other stuff
  
## Less interesting plots.....## Less interesting plots.....## Less interesting plots.....## Less interesting plots.....
# This looks at YPR for each year the catch is delayed.
p <- ggplot(ypr.res, aes(x= as.factor(M),y = ypr)) +
  geom_boxplot(color="blue") +                   # Now add the points
  facet_wrap(~delay.FM,nrow=4)  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  
 # geom_smooth(method = "lm") # This is the regression for each year, same thing as is done in the DeLury model.
windows(11,11); p

# This looks at YPR for each year the catch is delayed.
p <- ggplot(ypr.res, aes(as.factor(M),ypr)) +
  facet_wrap(~FM ,nrow=8)+  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  geom_boxplot(color="blue") +                   # Now add the points
  # geom_smooth(method = "lm") # This is the regression for each year, same thing as is done in the DeLury model.
  windows(11,11); p

# This looks at YPR vs M for year FM
p <- ggplot(ypr.res, aes(as.factor(M),ypr)) +
  facet_wrap(~FM ,nrow=8)+  # set up the default data, facet wrap makes it a grid plot, and scales let's x/y axis vary
  geom_boxplot(color="blue") +                   # Now add the points
  # geom_smooth(method = "lm") # This is the regression for each year, same thing as is done in the DeLury model.
  windows(11,11); p

# Need to make better figures, also would be nice to set it up so that I can hit certain age classes hard to see what that looks like
# Right now I can just turn on/off F for the early ages, but what if instead we target small sizes (or more generally some particular size)


boxplot(ypr.res$mc~ypr.res$delay.FM)


