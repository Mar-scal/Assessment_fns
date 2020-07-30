####################################################################
## GEORGES A AND BROWNS BANK NORTH.  This is a simple script to call the model functions and run the update for offshore.
#######################################################################################################################################  
## Update history
## Created by DK April 2016

#####################
#### IS IT STILL BEFORE 2018?? IF SO NOTE, LOOOK HERE!!, NOTE!!  HEADS UP!! YO YO, HOLD UP!! FAIS ATTENTION!! ACHTUNG!! CUIDADO!!!!!
####  IF YOU HAVE SWTICHED FROM R3.2.5 OR LOWER TO R3.3 OR HIGHER YOU WILL NEED TO RE-INSTALL JAGS
#### GO TO https://martynplummer.wordpress.com/2016/04/05/new-windows-binary-for-r-3-3-0/ FOR DETAILS
#### 
####################
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 1: source(paste(direct_fns, "Model/Update_function_JAGS.r",sep=""))
###############################################################################################################

#direct = "d:/r/"

# For linux
#direct = "/media/sf_data/r/"
#direct = "g:/r/"
fork <- "FK"

if(fork == "mar-scal") direct_fns <- "Y:/Offshore/Assessment/Assessment_fns/"
if(!fork == "mar-scal") direct_fns <- paste0("C:/Users/keyserf/Documents/Github/Assessment_fns/") #, fork, "/")

direct <- "C:/Users/keyserf/Documents/Version_control_pandemic/Offshore/Assessment/"

# All that is left to do is get the arguements for the final 3 model functions update and everything should be gold.
source(paste(direct_fns, "Model/Update_function_JAGS.r",sep=""))
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))

# Just because of how I have set this up to save objects, if running a bunch of sub-areas it is quicker to run
# them one at a time, otherwise you end up with a gigantic object to save, assuming you are running the full
# chain length.
bank = c("GBa", "BBn")
#bank <- c("BBn")
yr <- 2019 # the survey year, not the current year
num.banks <- length(bank)
for(k in 1:num.banks)
{
update_JAGS(preprocessed=T, # Do you want to load the preprocessed data, you should only need this set to F the first time you run this after logs are complete
            run.mod=F, # Do you want to re-run the model or use previously saved version
            use.final=T, # Did you do a final model run and is that the one you want to load
            final.run= F, # Is this your final run that will get results used in the model
            parallel=T, # Run in parallel, not sure why you wouldn't as it is quicker...
            make.diag.figs = T, # Do you want to make the diagnostic figures, these are not used for updates
            make.update.figs=F, # Do you want to make the figures for the update
            run.pred.eval.model = F, # Do you want to run the prediction evaluation model
            export.tables = F,  # Do you want to save the decision table
            bank=bank[k], # What bank and or sub-area do you want to run, possible choice shown below., I suggest you run this one by one
            # These are all the possible banks currently available.
            #bank = c("BBn","GBa","GBa-West","GBa-Central","GBa-East","GBa-SC","GBa-DS","GBa-North","GBa-South","GBa-Core","GBa-Large_core" ),
            yr=yr,
            fig="png",pred.eval.fig.type="box",
            #niter = 2000,nburn = 1000,nchains=2, # use the defaults for the real deal
            pe.years = yr,
            direct=direct,direct_fns=direct_fns,un=un.ID,pw=pwd.ID,db.con="ptran", 
            language="en") #Running both banks fully takes about 40 minutes.
}
# Run the model and see how long it takes.)

# Here are a quick summary of the interesting model results which we report in the model update for BBn and GBa.
load(paste(direct,"Data/Model/2020/GBa/Results/Model_results_and_diagnostics.RData",sep=""))
load(paste(direct,"Data/Model/2020/GBa/Results/Final_model_results.RData",sep=""))
load(paste(direct,"/Data/Model/2019/GBa/Results/Model_results_and_diagnostics.RData",sep=""))
load(paste(direct,"/Data/Model/2020/BBn/Results/Model_results_and_diagnostics.RData",sep=""))
load(paste(direct,"/Data/Model/2019/BBn/Results/Model_results_and_diagnostics.RData",sep=""))

mod.dat
# reference points for GBa (should be...):
refyears <- which(DD.out$GBa$dat$year %in% 1986:2009)

mean(DD.out$GBa$mean$B[refyears]) * 0.3 #4 807
mean(DD.out$GBa$mean$B[refyears]) * 0.8 #12 820

mean(DD.out$GBa$median$B[refyears]) * 0.3 #4 724
mean(DD.out$GBa$median$B[refyears]) * 0.8 #12 597


# The fully recruited biomass for the previous year and current year
FR.bm
# $GBa 2018
# [1] 20170.46 21272.45
# $GBa 2019
# [1] 21470.85 28830.86
# $GBa 2020
# [1] 29192.14 36757.09


# $BBn 2018
# [1] 4334.268 2835.921 
# $BBn 2019
# [1] 2857.803 2921.965
# $BBn 2020
# [1] 2890.488 3737.254

2800/4310# Long term median fully recruited biomass (not including the most recent year)
FR.ltm
# GBa 2018
# 15903.81 
# GBa 2019
# 16690.22 
# GBa 2020
# 18107.2 

# BBn 2018?
# 5550.483 
# BBn 2019
# 5548.013
# BBn 2020
# 5474.289 

# Recruite Biomass for previous and current year
rec.bm
# $GBa
# [1] 6787.132 3762.147

# $BBn
# [1] 724.7716 193.6675 ok

# (6787-3762)/6787 # percent decline
# (724.8-193.7)/724.8 # percent decline
##### ISSUE: NOT ABLE TO PULL INFO FROM FINAL RESULTS

# Long term median recruitment
rec.ltm
# GBa 
# 3244.358 

# BBn 
# 648.1341 ok

# Range of effective sample size
neff
# $GBa
# [1]   720 50000

# $BBn
# [1]  1500 50000 ok

# Range of rhat values
rhat
# $GBa
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.001   1.001   1.001   1.002   1.001   1.009 

# $BBn
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.001   1.001   1.001   1.001   1.001   1.005

# Median projection 1 year out.
BM.proj.1yr
# GBa 
# 25127.92  *** 

# BBn 
# 2621.141 ***
# (2836-2621)/2836 # percent decline

# Natural mortality for the most recent year... 
mort
# GBa 
# 0.08850937 

# BBn 
# 0.1405179

# The probability of currently being below the USR.
prob.below.USR
# GBa 
# "1%" 

NULL

# The condition this year.
DD.dat$CF[nrow(DD.dat)]
# GBa [1] 17.07225

# BBn [1] 11.26328

# The "percent" change in biomass, to be a real percent multiply by 100.
# Note that this doesn't exactly match the calculation in the decision table, very close, but occasionally
# the value will be off by 1% or so due to rounding, just due to the different ways these are calculated
100*percent.B.change
#      GBa 
# 18.12424 ***

#     BBn 
# -7.5735 ***

## Other crap....

colMeans(mod.out$BBn$BUGSoutput$sims.list$mR)

data <- mod.out
plot(colMeans(mod.out$GBa$BUGSoutput$sims.list$sCresid),type="b")

abline(h=0,lty=2,col="blue")
plot(colMeans(mod.out$GBa$BUGSoutput$sims.list$sCRresid),type="b")
colMeans(mod.out$GBa$BUGSoutput$sims.list$Crep)
mod.dat$GBa$clappers

hist(rlnorm(1000,mod.out$GBa$BUGSoutput$sims.list$CRmed[1,30],1/mod.out$GBa$BUGSoutput$sims.list$ikappa.rho2[1]))


# This gets the Biomass summary for each year, the last year being the last non-projected year of data
DD.out$GBa$summary[grep("B",rownames(DD.out$GBa$summary)),]
DD.out$BBn$summary[grep("B",rownames(DD.out$BBn$summary)),]

length(which((DD.out$BBn$sims.list$Bmed.p[,41] == DD.out$BBn$sims.list$K)))
range(DD.out$BBn$sims.list$logK)

range(DD.out$BBn$sims.list$B[which((DD.out$BBn$sims.list$Bmed.p[,34] == DD.out$BBn$sims.list$K)),27])
range(DD.out$BBn$sims.list$B[,27])
