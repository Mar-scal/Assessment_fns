##########  In this script we look at some different catch sceanarios and see how they impact the model results
##########  Case #1 looks at the impact of simply shifting what we classify as recruits/pre-recruits/and fully recruited scallop
##########  The remaining "cases" delve into the impact of accounting for the recruits being caught.  In these cases the
##########   model itself is revised to allow catch to be recruits as well.  The impact of different priors on the proportion of the
##########  catch which are recruits is explored in these cases.

options(stringsAsFactors = F)
############ Section 1 - Bring in the data   ############ Section 1 - Bring in the data   ############ Section 1 - Bring in the data
###############################
###############################
yr = as.numeric(format(Sys.time(), "%Y"))  # 
direct = "d:/r/"
library(RColorBrewer)
library(PBSmapping)
library(R2jags)
library(VGAMdata)
library(ggplot2)
# Load the survey data.


load(paste(direct,"Data/Survey_data/",yr-1,"/Survey_summary_output/Survey_all_results.RData",sep="")) 

source(paste(direct,"Assessment_fns/Framework/2017/Catch_recruits/projections.r",sep=""))
source(paste(direct,"Assessment_fns/Model/exploit.ggplt.R",sep=""))
direct <- "d:/r/"

# Load functions and external datafiles we might need
# Load in the functions needed for this function to run.
source(paste(direct,"Assessment_fns/Framework/2017/Catch_recruits/projections.r",sep=""))
source(paste(direct,"Assessment_fns/Model/exploit.ggplt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/decision.r",sep=""))
source(paste(direct,"Assessment_fns/Model/post.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/fit.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/biomass.plt.R",sep=""))

source(paste(direct,"Assessment_fns/Framework/2017/Catch_recruits/prediction_evaluation_function.r",sep="")) #load in the new prediction evaluation function
source(paste(direct,"Assessment_fns/Model/prediction_evaluation_figure.r",sep="")) #load in the new prediction evaluation figure functionsource(paste(direct,"Assessment_fns/Model/biomass.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/OSAC_fishery_figures.r",sep="")) #Source1
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Fishery/fishery.dat.r",sep=""))
source(paste(direct,"Assessment_fns/Survey_and_OSAC/survey.dat.r",sep="")) #Source21 Revised by DK September 2015
source(paste(direct,"Assessment_fns/Survey_and_OSAC/simple.surv.r",sep="")) 
source(paste(direct,"Assessment_fns/Model/Update_function_JAGS.r",sep=""))
#Read1 Bring in the VonB model parameters
cat("We read in the von B growth parameters from the file .../Data/Ageing/Von_B_growth_parameters.csv")
vonB <- read.csv(paste(direct,"Data/Ageing/Von_B_growth_parameters.csv",sep=""))

# Get the fishery data for everywhere...
logs_and_fish(loc="offshore",year = 1981:yr,un=un,pw=pwd,db.con=db.con,direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

# And subset the fish.dat so it's just gba.
gba.fish.dat <- fish.dat[fish.dat$bank == "GBa" & !is.na(fish.dat$bank),]
gba.fish.dat$EID<-1:nrow(gba.fish.dat)
gba.fish.dat$X<-gba.fish.dat$lon
gba.fish.dat$Y<-gba.fish.dat$lat
gba.fish.dat <- gba.fish.dat[!is.na(gba.fish.dat$EID),]

####################### Here we use a new model in which we allow a proportion of the catch to be taken from the recruits
####################### In this case the recruits are the 85-95 mm size classes we have historically identified are used
####################### We also build in another natural mortality term which is unrelated to the clapper index.
# I need to load in the new projection function which accounts for proportion of catch removed from recruits
# Bring in the vonB parameters..
vonB.par <-vonB[vonB$Bank =="GBa",]
# Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
cpue.dat <- fishery.dat(gba.fish.dat,bk="GBa",yr=c(1984:2017),method='jackknife',surv='August',
                        direct=direct,period = "survyr") 	

# Now on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
#for all other years we need to do this for Browns Bank North
# It really makes very little difference which way this is done as the catch in June-August
# has averaged around just 40 tonnes since about 1996.
#if(yr != 2015) cpue.dat <- fishery.dat(gba.fish.dat,bk="BBn",yr=min(years-1):years,surv='May',
#                                       method='jackknife',direct=direct,period = "survyr") 	
if(any(is.na(cpue.dat))) cpue.dat[is.na(cpue.dat)] <- 0 # Set any NA's to 0. 
# Combine the survey and Fishery data here.
survey.obj$GBa$model.dat$year <-  as.numeric(levels(survey.obj$GBa$model.dat$year))
mod.dat <- merge(survey.obj$GBa$model.dat,cpue.dat,by ="year")

# DK Note For years in which we have essentially no fishing effort on the bank we need to blow up the variance of those estimates.
# If < 20 tonnes assume low effort and blow up the CV to 1.
# Get the CV for the CPUE...
mod.dat$U.cv <- mod.dat$cpue.se/mod.dat$cpue
mod.dat$U.cv <- 50*mod.dat$U.cv

# Again on Browns North the survey usually happens in June so the projection is actually different
# But in 2015 the survey was messed so the above is the solution used for 2015, 
# for all other years we need to do this for Browns Bank North
# Note that June-August seems to be a pretty minimal fishery on BBn
proj.sub <- subset(gba.fish.dat,year %in% years & months(as.Date(gba.fish.dat$date)) %in% c("September","October","November","December"))
# Now in 2015 get rid of June-August data as it occured before the survey since the survey happened in August 

# Now calculate the fishery statistics for the projection period
proj.dat <- fishery.dat(proj.sub,bk="GBa",yr=(min(years)-1):max(years),method='jackknife',
                        direct=direct,period = "calyr") 	
# So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
# and the biomass from each tow to come up with an overall bank average condition factor.
# This is weight in this year, which becomes t-1 
waa.tm1 <- mod.dat$CF*(mod.dat$l.bar/100)^3
# Using this years average shell height we can find the exptected shell height for the scallops in the next year
# ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
# laa.t is the projected size of the current years scallops into next year.
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat$l.bar
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
# Of course we don't have next years condition thus th last condition is simply repeated
# waa.t is using the condition from next year and the growth from next year to get next years weight
waa.t <- c(mod.dat$CF[-1],mod.dat$CF[nrow(mod.dat)])*(laa.t/100)^3
# Here we use the current condition factor to calculate the weight next year (since we use laa.t)
# That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
# what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
# two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
waa.t2 <- mod.dat$CF*(laa.t/100)^3
# Now the growth, expected and realized.
mod.dat$g <- waa.t/waa.tm1
# This is using the actual condition factor and growing the scallops by laa.t
mod.dat$g2 <- waa.t2/waa.tm1

# same thing here but for the recruits
waa.tm1 <- mod.dat$CF*(mod.dat$l.k/100)^3
laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat$l.k
waa.t <- c(mod.dat$CF[-1],mod.dat$CF[nrow(mod.dat)])*(laa.t/100)^3
waa.t2 <- mod.dat$CF*(laa.t/100)^3
mod.dat$gR <- waa.t/waa.tm1
mod.dat$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")


# Now for the model...

# Set the working directory for figures and tables to be output
plotsGo <- paste(direct,yr+1,"/Framework/Process_error/",sep="")
#Read2 Get the managment data, note this file needs updated annually! 
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Grab the data, start model at either 1986 (note that BBn data starts in 1991 so anything earlier will default to 1991)
DD.dat <- subset(mod.dat,year %in% 1986:2017,
                 select = c("year","n.x","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                            "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS",  "RS","catch","effort","n.y","cpue",
                            "cpue.var","cpue.se","LCI","UCI","U.cv", "g","g2","gR","gR2"))

names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                    "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS",  "RS","C","E","n.trips","U",
                    "U.var","U.se","LCI","UCI","U.cv", "g","g2","gR","gR2") 
# Organize the data and set up the model priors/initialization data, then run the model.
yrs<-min(DD.dat$year):max(DD.dat$year)
NY<- length(yrs)
DD.lst<-as.list(subset(DD.dat,year %in% yrs,c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers",
                                              "clappersR")))
# DK NOTE: Downweight the CV for the CPUE data. This is done to be consistent with CV used
# Previously in the model assessments. This has been flagged as an action item to investigate 
# and resolve in the next framework.
cat(paste("*NOTE # 2* See code for message about the CPUE CV being downweighted artificially, this needs revised in next Framework as
          it ain't legit.\n",sep = " "))
#ifelse(names(DD.lst)[9] == "U.se", names(DD.lst)[9] <- "U.cv", DD.lst$U.cv <- DD.lst$U.cv*50)
# Also, if doing this we need to change the original data to represent what the model is seeing..
# So if we used the SE let's replace the U.cv data with the U.se data, if we are doing the
# 50x to match what we've done before than we need to change those data as well.
ifelse(names(DD.lst)[9] == "U.se", DD.dat$U.cv <- DD.dat$U.se, DD.dat$U.cv <- DD.dat$U.cv*50)

# Add a couple items to the DD.lst list...
DD.lst$NY<- length(DD.lst$C)
DD.lst$year<-min(DD.dat$year):max(DD.dat$year)
# Set up Priors.  This first bit is getting our variance correct for the CV's for Biomass, Recruit biomass, and catch rates.
# This is then added to our list of priors to get them correct.
# Biomass CV
uI=log(DD.lst$I.cv^2+1) # See Smith and Hubley 2014 for details, this is variance of the log of a CV
# DK Note:  Smith/Hubley suggest this should be 3, so why we setting it to 2???
Ip.a=2+(uI/uI)^2 # This is the alpha prior term for the prior (an inverse-gamma, i.e. gamma using 1/var); a rather funky way of setting alpha =2
Ip.b=1/(uI*((uI/uI)^2+1)) # This is the beta term for the prior, again a strangly complex way of saying 1/(2*(uI))
# Recruit biomass CV, see above comments for details.
uIR=log(DD.lst$IR.cv^2+1)
IRp.a=2+(uIR/uIR)^2
IRp.b=1/(uIR*((uIR/uIR)^2+1))
# Catch Rate CV, see above comments for details.
uU=log(DD.lst$U.cv^2+1)
Up.a=2+(uU/uU)^2
Up.b=1/(uU*((uU/uU)^2+1))

# For the proportion caught I want to have the flexiblity to change the prior and see what effect this
# has on the model results.  What I do for is have a low mortality on recruits early in the time series
# and high mortality on the recruit later in the time series, does this enable us to resolve the residual patterns in the data
# Absolutely no reason why I could do the exact same thing with m and mR and see how this impacts the model results as well!!
split <- length(yrs[yrs < 2004])

# Using Alpha of 498 and Beta of 6.02 sets the catch proportion to be centered at 0.99 with very little variability
# Using Alpha of 48 and Beta of 16.7 sets the catch proportion to be centered at 0.75 (range around 0.6-0.9)
# Using Alpha of 18 and Beta of 52 sets the catch proportion to be centered at 0.25 (range around 0.15-0.45)

# Scenario 1 prop = 0.99 FR at all times
C.p.a <- c(rep(498,split),rep(498,length((split+1):NY)))  # Scenario 1
C.p.b <- c(rep(6.02,split),rep(6.02,length((split+1):NY))) # Scenario 1

# Scenario 2 prop = 0.99 FR before 2004, and 0.75 after
#C.p.a <- c(rep(498,split),rep(48,length((split+1):NY)))  # Scenario 2
#C.p.b <- c(rep(6.02,split),rep(16.7,length((split+1):NY))) # Scenario 2

# Scenario 3 prop = 0.99 FR before 2004, and 0.25 after, used for "Scenario recruit" as well...
C.p.a <- c(rep(498,split),rep(18,length((split+1):NY)))  # Scenario 3
C.p.b <- c(rep(6.02,split),rep(52,length((split+1):NY))) # Scenario 3


# I can also manipulate the mortality terms for the recruits and full recruited.
# Scenario 1 - Fully recruited and recruit mortality, a = 1.05, b = 5.5
# this has a median of 0.125, but a mean of 0.16 and lots of range from 0 - 0.6 or so 
# very simliar to the usual model.
M.a <- c(rep(1.05,split),rep(1.05,length((split+1):NY))) # Scenario 1
MR.a <- c(rep(1.05,split),rep(1.05,length((split+1):NY))) # Scenario 1
M.b <- c(rep(5.5,split),rep(5.5,length((split+1):NY))) # Scenario 1
MR.b <- c(rep(5.5,split),rep(5.5,length((split+1):NY))) # Scenario 1

# Scenario 2 had very little impact on the model results
# Scenario 2 is no change in the natural mortality of the fully recruited scallop over the time series
# but a 50% increase in 2004 for the recruits, by 50% increase I mean a prior with a mean of 0.24 instead of 0.16
# range a bit wider from 0 - 0.8, but vast majority of the prior is < 0.5.

#M.a <- c(rep(1.05,split),rep(1.05,length((split+1):NY))) # Scenario 2
#MR.a <- c(rep(1.05,split),rep(1.75,length((split+1):NY))) # Scenario 2
#M.b <- c(rep(5.5,split),rep(5.5,length((split+1):NY))) # Scenario 2
#MR.b <- c(rep(5.5,split),rep(5.5,length((split+1):NY))) # Scenario 2

# Scenario 3 starts to impact the model, the predictions are more negative and the process error 
# starts to improve for the last several years, the large negative process residuals in and around 2004-2006
# interestingly do not change much in this scenario.  The recruit clapper index residuals start to get worse
# which makes some sense since this change in M really won't be picked up in the clapper index as this
# is a new source of mortality. This leads to a new model with a seperate M (as Hugues suggested...)
# Scenario 3 is no change in the natural mortality of the fully recruited scallop over the time series
# but a large increase in 2004 for the recruits, a prior with a mean of 0.5 instead of 0.16
# range a bit wider from 0 - 0.8, but vast majority of the prior is < 0.5.
#M.a <- c(rep(1.05,split),rep(1.05,length((split+1):NY))) # Scenario 3
#MR.a <- c(rep(1.05,split),rep(8,length((split+1):NY))) # Scenario 3
#M.b <- c(rep(5.5,split),rep(5.5,length((split+1):NY))) # Scenario 3
#MR.b <- c(rep(5.5,split),rep(8,length((split+1):NY))) # Scenario 3


#####################################
# Based on scenario 3 seperate soruce of unknown natural mortality to the model, for both recruits and 
# for fully recruited
nM.a <- c(rep(500,split),rep(500,length((split+1):NY))) # Base Scenario
nMR.a <- c(rep(500,split),rep(500,length((split+1):NY))) # Base Scenario 
nM.b <- c(rep(499000,split),rep(499000,length((split+1):NY))) # Base Scenario
nMR.b <- c(rep(499000,split),rep(499000,length((split+1):NY))) # Base Scenario

# Scenario 1, just toss in a small amount of non-clapper natural mortality to the model and see what happens. 
# This is a mean of about 10% with a range between 0 and 0.3
# This does help with our residual patterns
nM.a <- c(rep(3,split),rep(3,length((split+1):NY))) # Scenario 1
nMR.a <- c(rep(3,split),rep(3,length((split+1):NY))) # Scenario 1
nM.b <- c(rep(28,split),rep(28,length((split+1):NY))) # Scenario 1
nMR.b <- c(rep(28,split),rep(28,length((split+1):NY))) # Scenario 1

# Scenario 2, 
# This is a mean of about 10% with a range between 0 and 0.3
# This again helps a bit more with the residuals
nM.a <- c(rep(500,split),rep(3,length((split+1):NY))) # Scenario 2
nMR.a <- c(rep(500,split),rep(3,length((split+1):NY))) # Scenario 2
nM.b <- c(rep(499000,split),rep(28,length((split+1):NY))) # Scenario 2
nMR.b <- c(rep(499000,split),rep(28,length((split+1):NY))) # Scenario 2

# Scenario 3
# This is a mean of about 25% with a range between 0 and 0.6
# Residual pattern is eliminated, this suggests a large increase
# in some other form of mortality since 2004!!!!!!!!!!!!!!!!!!!
nM.a <- c(rep(500,split),rep(3,length((split+1):NY))) # Scenario 3
nMR.a <- c(rep(500,split),rep(3,length((split+1):NY))) # Scenario 3
nM.b <- c(rep(499000,split),rep(9,length((split+1):NY))) # Scenario 3
nMR.b <- c(rep(499000,split),rep(9,length((split+1):NY))) # Scenario 3

# Scenario 4 - Add mortality only to the fully recruited scallop
# Given that the recruit portion of the model seems to be under-estimating productivity Scenario 3 is a bit funny....
# This is a mean of about 20% with a range between 0 and 0.6
# in some other form of mortality since 2004!!!!!!!!!!!!!!!!!!!
nM.a <- c(rep(500,split),rep(3,length((split+1):NY))) # Scenario 4
nMR.a <- c(rep(500,split),rep(500,length((split+1):NY))) # Scenario 4
nM.b <- c(rep(499000,split),rep(12,length((split+1):NY))) # Scenario 4
nMR.b <- c(rep(499000,split),rep(499000,length((split+1):NY))) # Scenario 4


# Scenario Recruit, we've increased the non-clapper mortality of the recruits and above we run the model with a proportion of the catch being recruits...
nM.a <- c(rep(500,split),rep(500,length((split+1):NY))) # Base Scenario
nM.b <- c(rep(499000,split),rep(499000,length((split+1):NY))) # Base Scenario
nMR.a <- c(rep(500,split),rep(3,length((split+1):NY))) # Scenario 3
nMR.b <- c(rep(499000,split),rep(9,length((split+1):NY))) # Scenario 3


DDpriors=list(
  logK=			    list(a=7,		  b=7,		d="dnorm",	l=1		),		# scaler to total biomass, a= mean  b = sd, this gives a huge range of starting values
  r=				    list(a=0, 		b=1,		d="dlnorm",	l=NY	),		# scaled recruit biomass, a= meanlog  b = sdlog
  M=				    list(a=M.a,		b=M.b,		d="dbeta",	l=NY	),		# clapper natural mortality fully recruited a= meanlog  b = sdlog
  MR=				    list(a=MR.a,	b=MR.b,		d="dbeta",	l=NY	),		# clapper natural mortality  recruits a= meanlog  b = sdlog
  nM=				    list(a=nM.a,	b=nM.b,		d="dbeta",	l=NY	),		# non-clapper natural mortality fully recruited a= meanlog  b = sdlog
  nMR=			    list(a=nMR.a,	b=nMR.b,		d="dbeta",	l=NY	),		# non-clapper natural mortality  recruits a= meanlog  b = sdlog
  S=				    list(a=8, 		b=11,		d="dbeta",  l=1		),		# clapper dissolution rate a= shape1, b=shape2, 8 & 11 gives ~ normal mean of .45ish
  C.prop.FR=    list(a=C.p.a, b=C.p.b,d='dbeta',  l=NY   ),    # Proportion of the catch that is Fully recrutied
  q=				    list(a=20, 		b=40,		d="dbeta",	l=1		),		# survey catchability fully recruited a= shape1, b=shape2
  qU=				    list(a=0,		  b=1,	  d="dunif",	l=1		),		# fishery catchability CPUE a= min, b = max
  sigma=			  list(a=0, 		b=5,		d="dunif",	l=1		),		# process error (SD) a = min, b = max
  ikappa.tau2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error FR clappers  a = shape, b = scale (1/rate)
  ikappa.rho2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error recruit clappers a = shape, b = scale (1/rate)
  I.precision=	list(a=Ip.a,	b=Ip.b,	d="dgamma",	l=NY	),		# measurement error variance survey FR a = shape, b = scale (1/rate)
  IR.precision=	list(a=IRp.a,	b=IRp.b,d="dgamma",	l=NY	),		# measurement error variance survey recruits a = shape, b = scale (1/rate)
  U.precision=	list(a=Up.a,	b=Up.b,	d="dgamma",	l=NY	)		  # measurement error variance CPUE  a = shape, b = scale
)

#Prepare priors for JAGS
for(h in 1:length(DDpriors))
{
  # Get the variances for log-normal and normal converted to precisions, note that in BUGS language the precision is
  # the inverse of the squared standard deviation (which is what you specify in R).  The standard deviation is what
  # was specified in the Prior list (as it is more intuitive)
  if(DDpriors[[h]]$d%in%c("dlnorm","dnorm")) DDpriors[[h]]$b <- 1/DDpriors[[h]]$b^2
  # For a Gamma to convert to precision the precision term is  the inverse of the 'Scale" term in a typical 
  # gamma distribution parameterization, aka this is now knonwn as the rate.
  # Happily this is the same as the parameterization in R dgamma(x,shape,rate) so our b parameter is correct for posterior plots.
  if(DDpriors[[h]]$d=="dgamma")DDpriors[[h]]$b<-1/DDpriors[[h]]$b
} # end for(h in 1:length(DDpriors))
# Made a data.frame of the priors, unwrap the list and combine by row.
prior.dat<- data.frame(par=names(DDpriors),do.call("rbind",lapply(DDpriors,rbind)))
prior.lst<-list()
# Now turn this into a list
for(k in seq(1,nrow(prior.dat)*2,2))
{
  prior.lst[[k]]<-prior.dat$a[[ceiling(k/2)]]
  prior.lst[[k+1]]<-prior.dat$b[[ceiling(k/2)]]
} # end for(k in seq(1,nrow(prior.dat)*2,2))
# And give the list names
names(prior.lst)<-paste(rep(prior.dat$par,2)[order(rep(1:nrow(prior.dat),2))],rep(c('a','b'),nrow(prior.dat)),sep='.')

# Now if they haven't already been selected grab the parameters you want for the model.
parameters <- c(names(DDpriors),'K','P','B','R','mu','Imed','Ipred','Irep', 'IRmed','IRpred','IRrep',
                "Cmed","Crep","CRmed","CRrep",'sIresid','sIRresid','sPresid','Iresid',
                'IRresid','Presid',"Cresid","CRresid","sCresid","sCRresid","C.prop.FR","m","mR","nm","nmR")
# Run the model and see how long it takes.
# n = 400,000 and burn = 100,000, thin = 20 with 2 chains do not decrease these as retaining this much
# data is needed to stabilize the projections, it does lengthen the run time to 10-20 minutes in serial
# Running in parallel stick with that burn in but we can get away with n=200,000, burn = 100,000, thin = 20, and 6 chains
# they are longer chains than really are needed for the model to converge, but this is really being done just for the projections.
# Run the model now.
model.jags = "Assessment_fns/Framework/2017/Catch_recruits/DDwSE3_catch_recruits_with_additional_non_clapper_natural_mortality.bug"

start<-Sys.time()
out <- jags.parallel(data =  c(prior.lst,DD.lst), inits = NULL,parameters.to.save = parameters,  
                     model.file = paste(direct,model.jags,sep=""),n.chains = 8, n.iter = 100000, n.burnin = 40000, 
                     n.thin = 25)
print(Sys.time()-start)

# Rename the output so I retain the results 
DD.out <- list(data=c(prior.lst,DD.lst,yrs), sims.list=out$BUGSoutput$sims.list,median=out$BUGSoutput$median,
               mean=out$BUGSoutput$mean,summary=out$BUGSoutput$summary,priors = prior.lst,parameters=parameters)

# I will also retain the MCMC object produced in case I want it for something.
mod.out <- out

#source("fn/projections.r")
# The catch since the survey for the most recent year is
#proj.catch <- proj.dat$catch[proj.dat$year == max(DD.dat$year)] 
proj.catch <- 0 # just set it to 0 for the moment...
# Get the low and upper boundaries for the decision table (this might be a silly way to do this...)
D_low   <- 0
D_high <-  6000
# The increment size for the decision table.  500 for GBa and 50 for GBa
step <- 100
# Get the projection scenarios of interest
proj <- seq(D_low,D_high,step) + proj.catch
# The interim TAC is
TACi <- 3000

# Note that these plots don't account for growth next year, so are somewhat misleading compared to the decision table
# which does account for growth!!  This should be not too painful to fix I think?

# Now do the projections
DD.out<- projections(DD.out,C.p=proj) # C.p = potential catches in decision table

# The URP and LRP for the bank, for the moment only GBa has been accepted so it's the only one used.
URP <-  subset(manage.dat,year==(max(DD.dat$year)+1) & bank == "GBa")$URP
LRP <-  subset(manage.dat,year==(max(DD.dat$year)+1) & bank == "GBa")$LRP
### Generate Decision Table ###
### Note that from the 2015 SSR we have these definitely set at...
#Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
#The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
D.tab<-decision(DD.out,"GBa", mu=0.15,post.survey.C=proj.catch,refs = c(URP,LRP))
write.csv(D.tab,paste0(plotsGo,"Decision.csv",sep=""),row.names=F) #Write2



## Some summary stats...
# Some model outputs needed for the Update.  First the mortality
mort <- 1- exp(-DD.out$mean$m[length(DD.out$mean$m)])
# This lines up the column headers with the projected catch...
TACI<- which(DD.out$data$C.p==(TACi+proj.catch))
# This get us the predicted biomass for next year based on the projected catch
BM.proj.1yr <- DD.out$median$B.p[TACI]

# Here we can grab the Fully recruited and recruit biomass for the last 2 years and the median of the time series.
FR.bm <- DD.out$median$B[(length(DD.out$mean$B)-1):length(DD.out$median$B)]
# We exclude the current year from the median estimate
FR.ltm <- median(DD.out$median$B[-length(DD.out$median$B)])
# Recruit biomass
rec.bm <- DD.out$median$R[(length(DD.out$median$R)-1):length(DD.out$median$R)]
# We exclude the current year from the median estimate
rec.ltm <- median(DD.out$median$R[-length(DD.out$median$R)])

# Get the percent biomass change from the projection. 0 means unchanged, + means % increase, - means % decline
percent.B.change <- (BM.proj.1yr / DD.out$median$B[length(DD.out$median$B)]) -1



####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS 
##### Now we can run some model diagnostics.
# Some quick diagnoistics, the maximum should be < 1.05
rhat <- summary(DD.out$summary[,8])

# Effective number of observations.  
#Not sure what our minimum should be here, but using the Rhat + looking at the chains should indicate where there are problems...
neff <- range(DD.out$summary[,9])


## Make the figures
# posterior densities for model parameters
post.plt(DD.out,DDpriors,years=yrs, graphic="pdf",multi=T,path=plotsGo)
#dev.off()
##exploitaiton time series
exploit.plt(DD.out, years=yrs, var=c('mu','M','MR'),graphic="screen",path=plotsGo,plot.CI=F)
#dev.off()
# model biomass fit to survey
fit.plt(DD.out, years = yrs, CI=T,graphic="pdf",path=plotsGo,CV=T)
# diagnostic plot
diag.plt(DD.out, years = yrs,graphic="pdf",path=plotsGo)

# and our big old biomass plot
biomass.plt(DD.out,years=yrs, graphic="pdf",TAC=TACi+proj.catch,path=plotsGo,refs = c("LRP","URP","zones"),pred=1,
            URP =URP, LRP=LRP,avg.line=median)

#Prediction Evaluation using the current year CF, this isn't how we model it as we don't know g2/gR2 when we do our predictions
pe.years <- max(yrs):2000
#Prediction Evaluation using the growth we use in the model...
res <- pred.eval(input = DD.lst, priors = DD.out$priors, model = model.jags,  pe.years=pe.years,bank="GBa",growth="modelled",save.res = plotsGo,
                niter = 100000,nburn = 40000, nthin = 25,nchains=8,direct=direct,parameters = DD.out$parameters)
# Plot the prediction evaluation
pe.fig(res$PE.modelled,years=max(yrs),growth="modelled",graphic = "screen",direct= direct,bank = "GBa",plot="box",path=plotsGo,txt_size=18)


#save.image(paste(direct,"Data/Framework/",yr+1,"/Process_error/GBa_nM_recruits_0.25_Scenario_4.RData",sep=""))

#load(paste(direct,"Data/Framework/",yr+1,"/Process_error/GBa_nM_recruits_0.25_Scenario_4.RData",sep=""))
#load(paste(direct,"Data/Framework/",yr+1,"/Process_error/GBa_nM_recruits_0.25_Scenario_4.RData",sep=""))




