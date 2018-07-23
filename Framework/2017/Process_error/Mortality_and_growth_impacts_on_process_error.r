####################################################################
## This script builds on the script I developed for the Ecosystem Assessment Workgroup/shop.  Here we can
# control growth and mortality and look at how these terms impact our process error term
# Focus here is on Browns Bank North, should also work on Georges Bank as well.

## If you have your own data you can skip right to Section 2 and run your data assuming:
## 1:  The data is in the correct structure (see the mod.dat object in section 1 for the correct format)
## 2:  You have the correct folder structure (structure includes sub-folders for "models", "functions", "Data", and "Results")


####################################################################################################################################  
## Update history
## Created by DK July 2017

#####################
####  NOTE, LOOOK HERE!!, NOTE!!  HEADS UP!! YO YO, HOLD UP!! FAIS ATTENTION!! ACHTUNG!! CUIDADO!!!!!
####  IF YOU HAVE SWTICHED FROM R3.2.5 OR LOWER TO R3.3 OR HIGHER YOU WILL NEED TO RE-INSTALL JAGS
####  GO TO https://martynplummer.wordpress.com/2016/04/05/new-windows-binary-for-r-3-3-0/ FOR DETAILS
#### 
####################
#####################################  Function Summary ########################################################
####  
##  This script is used within these files:(a.k.a "dependent files") 
##  N/A
##
###############################################################################################################

###############################################################################################################
## This scripts needs these functions and models to work (a.k.a. "support files")
# 1: source(paste(direct,"/functions/DD_EAF_function.r",sep=""))
# 2: source(paste(direct,"/functions/biomass.plt.R",sep=""))
# 3 source(paste(direct,"/functions/projections.r",sep=""))
# 4: source(paste(direct,"/functions/decision.r",sep=""))
# 5: source(paste(direct,"/functions/diag.plt.R",sep=""))
# 6: source(paste(direct,"/functions/exploit.plt.R",sep=""))
# 7: source(paste(direct,"/functions/fit.plt.R",sep=""))
# 8: source(paste(direct,"/functions/post.plt.R",sep=""))
# 9: source(paste(direct,"/functions/projections.R",sep=""))
#
# Using this script you can run these models...
# 1: source(paste(direct,"/models/DD_base.bug",sep=""))
# 2: source(paste(direct,"/models/DD_with_CPUE.bug",sep=""))
###############################################################################################################


###############################################################################################################
###############  Section 1, Make your own data###############  Section 1, Make your own data###############  
###############  Section 1, Make your own data##############  Section 1, Make your own data###############  
###############################################################################################################
# First identify the directory you have all your EAF crap in
direct = "d:/r/"
seed <- 122
# Pick the bank and year to look at.
bnk <- "BBn"
yr <- 2017
######  Part A - If you don't have any data lets make some up, make a "sim.dat" object that simulates some data
#####            and a mod.dat which takes the simulated data and turns it into a fake survey time series.

load(paste(direct,"Data/Model/",yr,"/Model_input.Rdata",sep=""))

# Get the model data for the bank of interest
mod.dat <- mod.dat[[bnk]]
NY <- nrow(mod.dat)
years <- seq(min(mod.dat$year),max(mod.dat$year),by=1)
# I need to add two columns for the growth as I have a growth SE column, when not looking at SE
# I make this really really small, JAGS crashes if this is 0.
mod.dat$g.se <- 1e-6
mod.dat$gR.se <- 1e-6


###################################### M impacts ################################################################
# For the first set of trials we can vary M and see how this influences our process error
# m = instantaneous natural mortality of the "fully recruited" individuals...
# M = proportional natural mortality, in the model M is input as a beta distribution, so let's do the same here...
Ma <- 50  ; Mb <- 197 # This allows a little bit of varibilty with a mode of around 0.2.
# Ma <- 48 ; Mb  <- 424 # This gives minimal variation with a mode of 0.1
# Ma <- 1.4 ; Mb  <- 4.6 # This gives lots of variation with a mode of 0.1
# Ma <- 2 ; Mb <- 5 #This allows a ton of variability and gives a mode of around 0.2, but median/mean skewed higher...
# Ma <- 2.2 ; Mb <- 1.3 # This is a high mortality high variability prior, what does the model do here
# Ma <- 95 ; Mb <- 24.5 # This is a high mortality low variability prior, what does the model do here
M <- rbeta(NY,Ma,Mb) 
#sim.dat$M <- rbeta(NY,8,29) # This allows a fair bit of variability but gives a mode of around 0.2

m <- -log(1-M)
mn.m <- mean(m)

# mR = natural mortality of the "recruits", make them die a little more than the older folks
MRa <- 50 ; MRb <- 115 # This allows a little bit of varibilty with a mode of around 0.3.
# MRa <- 48 ; MRb  <- 424 # This gives minimal variation with a mode of 0.1
# MRa <- 1.4 ; MRb  <- 4.6 # This gives minimal variation with a mode of 0.1
# MRa <- 3 ; MRb <- 3.33 This allows a ton of variability and gives a mode of around 0.33
# MRa <- 2 ; MRb <- 5 This allows a ton of variability and gives a mode of around 0.33
# MRa <- 2.2 ; MRb <- 1.3 # This is a high mortality high variability prior, what does the model do here
# MRa <- 95 ; MRb <- 24.5 # This is a high mortality high variability prior, what does the model do here

MR <- rbeta(NY,MRa,MRb) 
mR <- -log(1-MR)
mn.mR <- mean(mR)



##############################  Growth Impacts #######################################
# Now this allows us to play around with growth and see how this impacts our process error.
# First lets look at the mean/variability of the actual data..
act.g <- mean(mod.dat$g)
act.g.sd <- sd(mod.dat$g)

act.gR <- mean(mod.dat$gR)
act.g.sd <- sd(mod.dat$gR)
# g  = growth of the "fully recruited" individuals.  1 = no growth (size next year = size this year), 2 means 
# they would double in size, and < 1 means they shrink.

# Next our growth terms, set to whatever I want to explore...
g.mean <- act.g
mod.dat$g <- rlnorm(NY,log(g.mean),0.1)
# gR = growth of the "recruits", they'll grow a bit faster than older fully recruited folks, set to whatever I want to explore...
gR.mean <- act.gR
mod.dat$gR <- rlnorm(NY,log(gR.mean),0.2)
# If I want uncertainty in the growth terms then I could do something like this (this is now part of the below function call...)
mod.dat$g.se <- rlnorm(NY,log(act.g.sd),0.2)
mod.dat$gR.se <- rlnorm(NY,log(act.gR.sd),0.2)


###############################################################################################################
###############  Section 2, RUN THE MODEL###############  Section 2, RUN THE MODEL###############  Section 2, RUN THE MODEL
###############  Section 2, RUN THE MODEL###############  Section 2, RUN THE MODEL###############  Section 2, RUN THE MODEL
###############################################################################################################
# Now you are all set to run the model, lucky!!  If you have your own data make sure it is set up like the mod.dat object above
# You will need all the variables that are in the mod.dat file and they need to have the same names as this object does.
# The exception is cpue and cpue.cv if you run DD_base.bug model it does not require any CPUE data.

# Load in the model function
source(paste(direct,"Assessment_fns/Framework/2017/Process_error/functions/Process_error_simulations_function.R",sep=""))

# Run the model, see the function for an explaination of all the model options.
update_JAGS(mod.dat = mod.dat, 
            direct = direct, yr = 2016,fig="pdf",export.tables = T, CPUE.mod =T,down.weight.CPUE=50,
            m.priors = data.frame(M.a=Ma,M.b=Mb,MR.a=MRa,MR.b=MRb),
            D_low = 0, D_high = NULL, step = NULL, URP = NULL, LRP = NULL,strt.mod.yr = 191,
            jags.model = "models/DD_jags_m_beta_and_clappers.bug",nchains=10,niter = 100000,nburn = 50000, convergence.check=F,
            proj.catch = 750,projection.paras =NULL,
            growth.var = 1)

#DD_jags_m_beta_and_clappers.bug
# Here are a quick summary of the interesting model results
load(paste(direct,"Results/Model_results_and_diagnostics.RData",sep=""))

load("D:/R/Data/Framework/2017/Process_error/Ma_1.4_Mb_4.6_MRa_1.4_MRb_4.6_Growth_var_1_Growth_rate__Survey_var__Model_results.RData")


# The fully recruited biomass for the previous year and current year
FR.bm
# Long term median fully recruited biomass (not including the most recent year)
FR.ltm
# Recruite Biomass for previous and current year
rec.bm
# Long term median recruitment
rec.ltm
# Range of effective sample size
neff
# Range of rhat values
rhat
# Median projection 1 year out.
BM.proj.1yr
# Natural mortality for the most recent year... 
mort
mort.R

# The probability of currently being below the USR.
prob.below.USR

# The "percent" change in biomass, to be a real percent multiply by 100.
100*percent.B.change

# We can also look at the paramters we used for our projections, hopefully they are as expected!
summary(DD.out$data$projection.parameters)
# How variable are these really...
apply(DD.out$data$projection.parameters,2,function(x) quantile(x,probs=seq(0,1,by=0.1)))


# If you have simulation data you can compare the simulation data with the data you used.
# Probably most importantly what does the simulated vs. real biomass look like?
plot(DD.out$median$B~sim.dat$B,pch=19)
abline(a=0,b=1,col="blue")
plot(DD.out$median$R~sim.dat$r,pch=19)
abline(a=0,b=1,col="blue")

# Another way to visualize this...
plot(DD.out$median$B~DD.out$data$year,pch=19,type="o",lwd=2) # modeled biomass
lines(sim.dat$B~sim.dat$year,col="blue",lty=2) # the actual biomass
# Now the same for the recruits...
plot(DD.out$median$R~DD.out$data$year,pch=19,type="o",lwd=2,ylim=c(min(c(min(sim.dat$r),min(DD.out$median$R))),
                                                                   max(c(max(sim.dat$r),max(DD.out$median$R)))))
lines(sim.dat$r~sim.dat$year,col="blue",lty=2) # the actual recruit biomass


# You can do this for any parameters of interest...
plot(sim.dat$m ~ DD.out$median$m,pch=19)
plot(sim.dat$mR ~ DD.out$median$mR,pch=19)
# The growth terms
plot(sim.dat$g ~ DD.out$median$G,pch=19)
plot(sim.dat$gR ~ DD.out$median$GR,pch=19)

# Catchability
DD.out$median$q
median(sim.dat$q)

# Process errors...
DD.out$median$sigma
median(sim.dat$sigma)

