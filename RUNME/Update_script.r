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

direct <- "Y:/Offshore/Assessment/"

# All that is left to do is get the arguements for the final 3 model functions update and everything should be gold.
source(paste(direct_fns, "Model/Update_function_JAGS.r",sep=""))
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))

# # Just because of how I have set this up to save objects, if running a bunch of sub-areas it is quicker to run
# # them one at a time, otherwise you end up with a gigantic object to save, assuming you are running the full
# # chain length.
# bank = c("GBa", "BBn")
# #bank <- c("BBn")
# yr <- 2021 # the survey year, not the current year if running in January-April
# num.banks <- length(bank)
# imputetype <- c("previous_year", "LTM", "midpoint")
# for(it in imputetype){
#   print(it)
#   for(k in 1:num.banks){
#     print(k)
#     update_JAGS(preprocessed=F, # Do you want to load the preprocessed data, you should only need this set to F the first time you run this after logs are complete
#                 run.mod=F, # Do you want to re-run the model or use previously saved version
#                 use.final=F, # Did you do a final model run and is that the one you want to load
#                 final.run= F, # Is this your final run that will get results used in the model
#                 parallel=F, # Run in parallel, not sure why you wouldn't as it is quicker...
#                 make.diag.figs = F, # Do you want to make the diagnostic figures, these are not used for updates
#                 make.update.figs=F, # Do you want to make the figures for the update
#                 run.pred.eval.model = F, # Do you want to run the prediction evaluation model
#                 export.tables = F,  # Do you want to save the decision table
#                 bank=bank[k], # What bank and or sub-area do you want to run, possible choice shown below., I suggest you run this one by one
#                 # These are all the possible banks currently available.
#                 #bank = c("BBn","GBa","GBa-West","GBa-Central","GBa-East","GBa-SC","GBa-DS","GBa-North","GBa-South","GBa-Core","GBa-Large_core" ),
#                 yr=yr,
#                 fig="png",pred.eval.fig.type="box",
#                 niter = 2000,nburn = 1000,nchains=2, # use the defaults for the real deal
#                 pe.years = yr,
#                 direct=direct,direct_fns=direct_fns, direct_out = "C:/Users/keyserf/Documents/",
#                 un=un.ID,pw=pwd.ID,db.con="ptran", 
#                 language="en",
#                 impute=it,
#                 nickname=it) #Running both banks fully takes about 40 minutes.
#   }
# }
# # Run the model and see how long it takes.)



### 2022: FK broke up the Update_function_JAGS.r code into 2 separate scripts, and the projections are run below!
direct <- "Y:/Offshore/Assessment/"
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"
direct_out <- "C:/Users/keyserf/Documents/"
source(paste0(direct_fns, "/Model/model_inputs.r"))
source(paste0(direct_fns, "/Model/run_model.r"))
source(paste(direct_fns,"Model/prediction_evaluation_function.r",sep="")) #The function to run the prediction evaluations
source(paste(direct_fns,"Model/prediction_evaluation_figure.r",sep="")) # The function to make the plots

imputetype<-c("mixed")#, # 2022 conclusion is to use "mixed" for 2020 imputation from here on.
  #"min", 
  #"max", 
  #"LTM")#, "previous_year", "midpoint"
              
banks <- c(#"GBa-West","GBa-Central","GBa-East","GBa-SC","GBa-DS","GBa-North","GBa-South","GBa-Core","GBa-Large_core",
  "GBa", "BBn")
yr <- 2022

for(b in banks){
  for(it in imputetype){
    print(b)
    print(it)
    # uncomment this the first time!
    model_inputs(bank=b,
                 yr=2022, # the survey year, not the current year if running in January-April
                 impute=it,
                 nickname=NULL,
                 direct,
                 direct_fns,
                 survey.obj="Y:/Offshore/Assessment/Data/Survey_data/2022/Survey_summary_output/Survey_all_results.RData")
    
    # runs the model IF run.model=T, and creates all figures
    run_model(banks=b,
              yr=2022,
              nickname=NULL,
              direct,
              direct_fns,
              direct_out = "C:/Users/keyserf/Documents/",
              run.model = T,
              parallel=T,
              model.dat = paste0(direct_out, "Data/Model/",(yr+1),"/",b,"/Results/Final_model_results.RData",sep=""),
              final.run=T,
              # model.dat = paste0(direct_out, "Data/Model/",(yr+1),"/",b,"/Results/Model_testing_results_", it, ".RData",sep=""),
              # final.run=F,
              nchains = 8,niter = 175000, nburn = 100000, nthin = 20,
              export.tables=T,
              make.diag.figs = T,
              make.update.figs = T,
              language="en",
              fig="png")
    
    # prediction evaluation
    
      load(paste0(direct_out, "Data/Model/",(yr+1),"/",b,"/Results/Final_model_results.RData",sep=""))

      if(!dir.exists(paste0(direct_out, "Data/Model/", yr+1, "/", b, "/Results/", it,"/"))) dir.create(paste0(direct_out, "Data/Model/", yr+1, "/", b, "/Results/", it,"/"))

      #Prediction Evaluation using the current year CF, this isn't how we model it as we don't know g2/gR2 when we do our predictions
      pred.eval(input = DD.lst[[b]], priors = DD.out[[b]]$priors, pe.years= yr:(yr-6), growth="both",
                model = paste0("/Assessment_fns/Model/DDwSE3_jags.bug"),  bank=b,
                parameters = DD.out[[b]]$parameters, niter = 175000, nburn = 100000, nthin = 20, nchains=8,
                direct=direct, save.res=paste0(direct_out, "Data/Model/", yr+1, "/", b, "/Results/", it,"/"))

      # modelled:
      # Up to 2016
      load(paste(direct,"Data/Model/",2017,"/",b,"/Results/Projection_evaluation_results_mod_growth.RData",sep=''))
      out.tmp <- out; rm(out)
      #2017-2019
      for(y in 2018:2020){
        load(paste(direct,"Data/Model/",y,"/",b,"/Results/Projection_evaluation_modelled_growth.RData",sep=''))
        out.tmp[[as.character(y-1)]] <- out[[as.character(y-1)]]
      }
      #2020
      out.tmp[["2020"]] <- NULL
      #2021+
      for(y in 2022:(yr+1)){
        load(paste(direct_out,"Data/Model/",y,"/",b,"/Results/", it, "/Projection_evaluation_modelled_growth.RData",sep=''))
        out.tmp[[as.character(y-1)]] <- out[[as.character(y-1)]]
      }
      out.modelled <- out.tmp[order(names(out.tmp))]

      # realized:
      # Up to 2016
      load(paste(direct,"Data/Model/",2017,"/",b,"/Results/Projection_evaluation_results_g2_growth.RData",sep=''))
      out.tmp <- out; rm(out)
      #2017-2019
      for(y in 2018:2020){
        load(paste(direct,"Data/Model/",y,"/",b,"/Results/Projection_evaluation_realized_growth.RData",sep=''))
        out.tmp[[as.character(y-1)]] <- out[[as.character(y-1)]]
      }
      #2020
      out.tmp[["2020"]] <- NULL
      #2021+
      for(y in 2022:(yr+1)){
        load(paste(direct_out,"Data/Model/",y,"/",b,"/Results/", it, "/Projection_evaluation_realized_growth.RData",sep=''))
        out.tmp[[as.character(y-1)]] <- out[[as.character(y-1)]]
      }
      out.realized <- out.tmp[order(names(out.tmp))]

      # Now we make the figures and save them...
      pe.fig(input=list(modelled=out.modelled, realized=out.realized), years=yr, growth="both", graphic = "png", direct= direct, bank = b, plot="box",
             path=paste0(direct, yr+1, "/Updates/", b, "/Figures_and_tables/", it,"/"))
      #pe.fig(years=max(yrs[[bnk]]),growth="modelled",graphic = "screen",direct= direct,bank = bnk,plot="box")
    #}
  }
}

 # we need to compare these results across scenarios





## 2021/2022 notes:
# because of VPN firewall blocking the saving of large files on NAS, need to try running this at BIO on network. Alternative is to copy all data to hard drive or C drive.



# Here are a quick summary of the interesting model results which we report in the model update for BBn and GBa.
load(paste(direct_out,"Data/Model/2023/GBa/Results/Model_results_and_diagnostics_mixed.RData",sep=""))
load(paste(direct_out,"Data/Model/2023/GBa/Results/Model_testing_results_mixed.RData",sep=""))
#load(paste(direct,"/Data/Model/2019/GBa/Results/Model_results_and_diagnostics.RData",sep=""))
load(paste(direct_out,"/Data/Model/2023/BBn/Results/Model_results_and_diagnostics_mixed.RData",sep=""))
#load(paste(direct,"/Data/Model/2019/BBn/Results/Model_results_and_diagnostics.RData",sep=""))

mod.dat
# reference points for GBa (should be...):
refyears <- which(DD.out$GBa$dat$year %in% 1986:2009)

# mean(DD.out$GBa$mean$B[refyears]) * 0.3 #4 807 # 4752
# mean(DD.out$GBa$mean$B[refyears]) * 0.8 #12 820 # 12671

mean(DD.out$GBa$median$B[refyears]) * 0.3 #4 724 # 4673 # 4680
mean(DD.out$GBa$median$B[refyears]) * 0.8 #12 597 # 12461 # 12480


# The fully recruited biomass for the previous year and current year
FR.bm
# $GBa 2018
# [1] 20170.46 21272.45
# $GBa 2019
# [1] 21470.85 28830.86
# $GBa 2020
# [1] 29192.14 36757.09
# $GBa 2022
# [1] 26781.13 18117.58
# $GBa 2023
# [1] 18489.06 22966.65


# $BBn 2018
# [1] 4334.268 2835.921 
# $BBn 2019
# [1] 2857.803 2921.965
# $BBn 2020
# [1] 2890.488 3737.254
# $BBn 2022
# [1] 3328.751 3188.192


# post-fishery catch
proj.dat[[1]]$catch[proj.dat[[1]]$year==yr]
#GBa 2022 - 1011.932
#BBn 2022 - 245.5611
#GBa 2023 - 617.4805

# Long term median fully recruited biomass (not including the most recent year)
FR.ltm
# GBa 2018
# 15903.81 
# GBa 2019
# 16690.22 
# GBa 2020
# 18107.2 
# $GBa 2022
# [1] 18487.7
# $GBa 2023
# [1] 18515.1

# BBn 2018?
# 5550.483 
# BBn 2019
# 5548.013
# BBn 2020
# 5474.289 
# $BBn 2022
# [1] 5065.094

# Recruite Biomass for previous and current year
rec.bm
# $GBa
# [1] 6787.132 3762.147
# $GBa 2022
# [1] 3327.189 3173.423
# $GBa 2023
# [1] 3206.003 3154.261

# $BBn
# [1] 724.7716 193.6675 ok
# $BBn 2022
# [1] 209.7853 212.2573

# (3206.003-3154.261)/3206.003 # percent decline
# (724.8-193.7)/724.8 # percent decline
##### ISSUE: NOT ABLE TO PULL INFO FROM FINAL RESULTS

# Long term median recruitment
rec.ltm
# GBa 
# 3244.358 
# $GBa 2022
# [1] 3511.671
# $GBa 2023
# [1] 3434.434

# BBn 
# 648.1341 ok
# $BBn 2022
# [1] 455.802

# Range of effective sample size
neff
# $GBa
# [1]   720 50000
# $GBa 2022
# [1]   340 30000
# $GBa 2023
# [1]   460 30000

# $BBn
# [1]  1500 50000 ok
# $BBn 2022
# [1]  2300 30000

# Range of rhat values
rhat
# $GBa 2022
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.001   1.001   1.001   1.002   1.001   1.016
# $GBa 2023
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.001   1.001   1.001   1.002   1.001   1.011 
# 


# $BBn
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.001   1.001   1.001   1.001   1.001   1.003 

# Median projection 1 year out.
BM.proj.1yr
# GBa 
# 25127.92  *** 
# $GBa 2022
# [1] 18001.77
# $GBa 2023
# [1] 22470.77

# BBn 
# 2621.141 ***
# $BBn 2022
# [1] 3273.113
# (2836-2621)/2836 # percent decline

# Natural mortality for the most recent year... 
mort
# GBa 
# 0.08850937 
# $GBa 2022
# [1] 0.1375507
# $GBa 2023
# [1] 0.1591694


# BBn 
# 0.1405179
# $BBn 2022
# [1] 0.03283034

# Fishing mortality (mu) for current year
# GBa
DD.out$GBa$median$mu
# 0.1577278 (2022)

# BBn
DD.out$BBn$median$mu
# 0.07226612 (2022)

# note, Fmort is calculated in the model, but not saved in output

# The probability of currently being below the USR.
prob.below.USR
# GBa 2022
# "2%"
# $GBa 2023
# [1] "0%"

NULL

# The condition this year.
DD.dat$CF[nrow(DD.dat)]
# GBa [1] 18.43368 2022
# [1] 17.12339 2023

# BBn [1] 11.70429

# The "percent" change in biomass, to be a real percent multiply by 100.
# Note that this doesn't exactly match the calculation in the decision table, very close, but occasionally
# the value will be off by 1% or so due to rounding, just due to the different ways these are calculated
100*percent.B.change[[1]]
#      GBa 
# -0.6392381 ***
# [1] -2.159137 2023

#     BBn 
# 2.663624 ***

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

