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
# 1: source(paste(direct,"Assessment_fns/Model/Update_function_JAGS.r",sep=""))
###############################################################################################################

direct = "d:/r/"
#yr <- 2017
# For linux
#direct = "/media/sf_data/r/"
#direct = "g:/r/"
#direct <- "Y:/Offshore scallop/Assessment/"
# All that is left to do is get the arguements for the final 3 model functions update and everything should be gold.
source(paste(direct,"Assessment_fns/Framework/2017/Growth/Update_function_JAGS_new_growth_model.r",sep=""))
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))

update_JAGS(run.mod=T,preprocessed=T,make.figs=T,make.diag.figs = T,make.diag=T,run.pred.eval.model = F,use.final=F,final.run=F,fig="pdf",
            make.pred.eval.figs = F,pred.eval.fig.type="box",nchains=10,bank=c("GBa"),export.tables = T, 
            jags.model = "Assessment_fns/Framework/2017/Growth/DD_growth.bug",
            make.update.figs=F,yr=2016,niter = 150000,nburn = 100000,direct=direct,
            un=un.ID,pw=pwd.ID,db.con="ptran64")
# Run the model and see how long it takes.)

# Here are a quick summary of the interesting model results which we report in the model update for BBn and GBa.
load(paste(direct,"Data/Framework/2017/Growth/Model_results_and_diagnostics.RData",sep=""))
load(paste(direct,"Data/Framework/2017/Growth/Model_testing_results.RData",sep=""))
#load(paste(direct,"/Data/Model/2016/GBa/Results/Final_model_results.RData",sep=""))
#load(paste(direct,"/Data/Model/2016/BBn/Results/Final_model_results.RData",sep=""))


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
# The probability of currently being below the USR.
prob.below.USR
# The condition this year.
DD.dat$CF[nrow(DD.dat)]
# The "percent" change in biomass, to be a real percent multiply by 100.
100*percent.B.change

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

median(DD.out$GBa$sims.list$B.p)
median(DD.out$GBa$sims.list$B)


