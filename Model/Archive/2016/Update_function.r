####################################################################
## Create the update for Georges A and Browns Bank North. This function replaces the script "Update.r" 
## All of the information necessary to create the update is now housed in this function.
###################################################################
# Update history
# January 2016 - Revised by DK 
# April 2016, "Update" script has been overhauled and converted to a function called "Update_function"

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
# 1:  delayBUGS.r
# 2:  post.plt.R
# 3:  exploit.plt.r
# 4:  fit.plt.R
# 5:  peR.r
# 6:  projections.r
# 7:  biomass.plt.r
# 8:  diag.plt.R
# 9:  logs_and_fishery_data.r
#10:  fishery.dat.r
#11:  ScallopMap.r
#12:  contour.gen.r 
##
###############################################################################################################


###############################################################################################################
# Arguments
# 1:  direct:                   The root directory to work from  Default = "Y:/Offshore scallop/Assessment", 
# 2:  yr:                       The year of the survey used to generate update. Default = as.numeric(format(Sys.time(), "%Y"))-1 
#                               Since the updates occur the year after the survey this gets confusing, for example the 2015
#                               survey was used to create the 2016 update, in this case you specifiy yr = 2015 even though the update is 
#                               the 2016 update and all the files related to the update are found in Update/2016/...  
# 3:  strt.mod.yr:              Start year for the models.  Set to 1986 but note that BBn doesn't have data back that far so starts in 1991.
# 4:  bank:                     The bank to run.  Default is both banks c("GBa","BBn"), but either one or the other should work.
# 5:  preprocessed:             If you have already run Section 1 of this script you can skip runing that section each time.T/F default = F
# 6:  make.diag:                Create diagnostic summary, model results, and model convergence pdf found in Section 3.  T/F default = T
# 7:  export.tables:            Export the Decision tables, should only be done when satisified with results.  T/F, default = F
# 8:  run.mod = T:              Run the model?  If a 'final model' has been saved you can skip re-running the model.  T/F, default = T
# 9:  nchains = 2:              Number of chains to run.  Default = 2
#10:  niter = 100000:           Number of iterations to run.  Default = 100000
#11:  nburn = 50000:            Number of initial iterations to ignore.  Default = 50000
#12:  nthin = 50:               Thinning rate of iterations.  Default = 50
#13:  final.run:                If you are happy with testing run the model as a final model.  T/F, default = F.  If truethis produces an R
#                               workspace called Final_model_results.RData, if False it saves to Model_testing_results.RData
#14:  de.bug=F:                 T/F.  If the model is crashing de.bug =T will keep the WinBUGS window open to allow for troubleshooting
#15:  make.figs:                T/F, default = T.  Do you want to make figures? This is a high level call that overwrites 
#                               make.diag.figs, make.pred.eval.figs and make update.figs.  No figures produced when =F
#16:  make.diag.figs:           Do you want to make the diagnositic figures.  T/F, default = T
#17:  make.pred.eval.figs:      Do you want to make the prediction evaluation figures.  T/F, default = F,
#18:  make.update.figs:         Do you want to make the figures used in the Update?  T/F, default = T,
#19:  fig:                      Print to 'pdf' or to screen.  default="screen":
###############################################################################################################



update <- function(direct = "Y:/Offshore scallop/Assessment", yr = as.numeric(format(Sys.time(), "%Y"))-1 , strt.mod.yr = 1986,
                   bank = c("GBa","BBn"),preprocessed = F,   make.diag=T,export.tables = F,
                   run.mod = T,nchains = 2,niter = 100000, nburn = 50000, nthin = 50,final.run = F,de.bug=F,
                   make.figs =T,make.diag.figs = T,make.pred.eval.figs = F,make.update.figs=T,fig="screen")
{
  
# Load in the functions needed for this function to run.
source(paste(direct,"Assessment_fns/Model/delayBUGS.r",sep=""))
source(paste(direct,"Assessment_fns/Model/post.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/exploit.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Model/fit.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/peR.r",sep=""))
source(paste(direct,"Assessment_fns/Model/projections.r",sep=""))
source(paste(direct,"Assessment_fns/Model/biomass_LN.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Model/decision.r",sep=""))
source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Fishery/fishery.dat.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) 
library ("R2WinBUGS")
library("R2jags")

#These are the model results, short cut to running the whole model again if you are satisfied with your results.
# Or you just want to look at what you have so far.
#load(paste(direct,(yr),"/Updates/GBa/Results/GBa_model_results.R",sep=""))

#############  Section 1  Compile the data for the banks ######  Section 1  Compile the data for the banks################## 
#############  Section 1  Compile the data for the banks ######  Section 1  Compile the data for the banks################## 
#############  Section 1  Compile the data for the banks ######  Section 1  Compile the data for the banks################## 
# If you have already run section 1 no need to do it again set preprocessed = T
# Make sure you have the up to date fishery data in here tho!!
if(preprocessed==F)
  {
    # Bring in the survey results for the current year, this also includes the fishery data..
    load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
    # Now bring in the latest fishery data
    logs_and_fish(loc="offshore",year = 1981:yr,un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
    # If you get any NA's related warnings it may be something is being treated as a Factor in one of the two files.  
    # This should combine without any warnings so don't ignore warnings here.
    fish.dat<-merge(new.log.dat,old.log.dat,all=T)
    fish.dat$ID<-1:nrow(fish.dat)
    
    # Bring in the VonB model parameters
    vonB <- read.csv(paste(direct,"Data/Ageing/Von_B_growth_parameters.csv",sep=""))
    
    # Run this for one or both banks
    mod.dat <- NULL
    cpue.dat <- NULL
    proj.dat <- NULL
    # Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
    for(i in 1:length(bank))
      {
        years <- min(survey.obj[[bank[i]]][[1]]$year):max(survey.obj[[bank[i]]][[1]]$year)
        # Bring in the vonB parameters..
        vonB.par <-vonB[vonB$Bank ==bank[i],]
        # Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
        cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk=bank[i],yr=(min(years)-1):max(years),method='jackknife',
                                           direct=direct,period = "survyr") 	
        # Now on Browns North the survey usually happens in June so the projection is actually different
        # But in 2015 the survey was messed so the above is the solution used for 2015, 
        #for all other years we need to do this for Browns Bank North
        # It really makes very little difference which way this is done as the catch in June-August
        # has averaged around just 40 tonnes since about 1996.
        if(yr != 2015 && bank[i] == "BBn") cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk=bank[i],yr=(min(years)-1):max(years),surv='May',
                                                                                  method='jackknife',direct=direct,period = "survyr") 	
        # Combine the survey and Fishery data here.
        mod.dat[[bank[i]]] <- merge(survey.obj[[bank[i]]][[1]],cpue.dat[[bank[i]]],by ="year")
        # Get the CV for the CPUE...
        mod.dat[[bank[i]]]$U.cv <- mod.dat[[bank[i]]]$cpue.se/mod.dat[[bank[i]]]$cpue
        # now get the catch data from end of survey until end of the year for the projection
        proj.sub <- subset(fish.dat,year %in% years & months(as.Date(fish.dat$date)) %in% c("September","October","November","December"))
        # Again on Browns North the survey usually happens in June so the projection is actually different
        # But in 2015 the survey was messed so the above is the solution used for 2015, 
        # for all other years we need to do this for Browns Bank North
        # Note that June-August seems to be a pretty minimal fishery on BBn
        if(yr != 2015 && bank[i] == "BBn") proj.sub <- subset(fish.dat,year %in% years & months(as.Date(fish.dat$date)) 
                                                              %in% c("June","July","August","September","October","November","December"))
        # Now calculate the fishery statistics for the projection period
        proj.dat[[bank[i]]] <- fishery.dat(proj.sub,bk=bank[i],yr=(min(years)-1):max(years),method='jackknife',
                                           direct=direct,period = "calyr") 	
        # This little snippet I ran to compare the median difference between using BBn from June-Dec vs
        # BBn from Sept-Dec.  On average about 22% more catch came out if including June-August while CPUE was essentially identical
        #tst <- fishery.dat(proj.sub,bk=bank[i],yr=(min(years)-1):max(years),method='jackknife',direct=direct,period = "calyr") 	
        # diff <- (tst -proj.dat$BBn) / tst
        # mdn.diff <- apply(diff,2,function(x) median(x,na.rm=T))
        # Total differnce in catch in June-August since 1991 is 2710 tonnes, which is a difference of 108 tonnes per year
        # catch.diff <- sum(tst$catch,na.rm=T) - sum(proj.dat$BBn$catch,na.rm=T)
        # ann.diff <- catch.diff / length(tst$catch)
        # mdn.diff <-  catch.diff / sum(tst$catch,na.rm=T) # about 33% different
        # But looking more closely you can see it is the first few years of the fishery where most of the difference is...
        # between 1991 and 1996, almost 1900 tonnes was removed in June-August (about 380 tonnes/year), 
        # In the 19 years since only 800 tonnes (or about 42 tonnes/year)
        # Thus why such a small difference in model results when using these different time series
        # frst.five <- sum(tst$catch[1:5],na.rm=T) - sum(proj.dat$BBn$catch[1:5],na.rm=T)
        ## End the little snippet
        
        # Back to real code
        # So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
        # and the biomass from each tow to come up with an overall bank average condition factor.
        # This is weight in this year, which becomes t-1 
        waa.tm1 <- mod.dat[[bank[i]]]$CF*(mod.dat[[bank[i]]]$l.bar/100)^3
        # Using this years weight we can find the exptected shell height for the scallops in the next year
        laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat[[bank[i]]]$l.bar
        # Now we can get an expected wight in the next year# Linf * (1-exp(-K)) + exp(-K) * height
        waa.t <- c(mod.dat[[bank[i]]]$CF[-1],mod.dat[[bank[i]]]$CF[nrow(mod.dat[[bank[i]]])])*(laa.t/100)^3
        # Here we use the actual data to calculate the weight, of course for the final year we don't have this
        # so we just use the expectation
        waa.t2 <- mod.dat[[bank[i]]]$CF*(laa.t/100)^3
        # Now the growth, expected and realized.
        mod.dat[[bank[i]]]$g <- waa.t/waa.tm1
        mod.dat[[bank[i]]]$g2 <- waa.t2/waa.tm1
        
        # for recruits
        waa.tm1 <- mod.dat[[bank[i]]]$CF*(mod.dat[[bank[i]]]$l.k/100)^3
        laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat[[bank[i]]]$l.k
        waa.t <- c(mod.dat[[bank[i]]]$CF[-1],mod.dat[[bank[i]]]$CF[nrow(mod.dat[[bank[i]]])])*(laa.t/100)^3
        waa.t2 <- mod.dat[[bank[i]]]$CF*(laa.t/100)^3
        mod.dat[[bank[i]]]$gR <- waa.t/waa.tm1
        mod.dat[[bank[i]]]$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
      } # end for(i in 1:length(bank))
    
    # Save the results so you don't have to do section 1 over and over.
    save(mod.dat,cpue.dat,proj.dat,file=paste(direct,"Data/Model/",(yr+1),"/Model_input.RData",sep=""))
  } # end if(preprocessed == F)
#############  End Section 1  Compile the data for the banks ######  End Section 1  Compile the data for the banks################## 
#############  End Section 1  Compile the data for the banks ######  End Section 1  Compile the data for the banks################## 
#############  End Section 1  Compile the data for the banks ######  End Section 1  Compile the data for the banks################## 



#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model ###########
#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model ###########
#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model ###########
# Load the data if the above has already been compiled.
if(preprocessed==T) load(file=paste(direct,"Data/Model/",(yr+1),"/Model_input.RData",sep=""))
# Initialize variables...
if(run.mod == T)
  {
    DD.out <- NULL
    DD.lst <- NULL
    DDpriors <- NULL
    D.tab <- NULL
    yrs <- NULL
    proj.catch <- NULL
    URP <- NULL
    LRP <- NULL
    DD.dat <- NULL
    proj <- NULL
    D_low <- NULL
    D_high <- NULL
    TACi <- NULL
  } # end if(run.mod== T)
# Initialize the variables for the diagnostics if needed.
if(make.diag==T)
  {
    mort <- NULL
    TACI <- NULL
    BM.proj.1yr <- NULL
    B.quantiles <- NULL
    percent.B.change <- NULL
    prob.below.USR <- NULL
    FR.bm <- NULL
    FR.ltm <- NULL
    rec.bm <- NULL
    rec.ltm <- NULL
    neff <- NULL
    rhat <- NULL
    
  }# end if(make.diag == T)

manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
num.banks <- length(bank)

for(j in 1:num.banks)
{
  # pick the bank
  bnk = bank[j]
  # Set the working directory for figures and tables to be output
  plotsGo <- paste(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/",sep="")
  
# If you need to run the model giver.
  if(run.mod == T)
    {
        # Grab the data, start model at either 1986 (note that BBn data starts in 1991 so anything earlier will default to 1991)
        DD.dat <- subset(mod.dat[[bnk]],year %in% strt.mod.yr:max(mod.dat[[bnk]]$year))
        names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                            "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS",  "RS","C","E","n.trips","U",
                            "U.var","U.se","LCI","UCI","U.cv", "g","g2","gR","gR2") 
        # Organize the data and set up the model priors/initialization data, then run the model.
        yrs[[bnk]]<-min(DD.dat$year):max(DD.dat$year)
        NY<- length(yrs[[bnk]])
        DD.lst[[bnk]]<-as.list(subset(DD.dat,year %in% yrs[[bnk]],c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers",
                                                    "clappersR")))
        # DK NOTE: Downweight the CV for the CPUE data. This is done to be consistent with CV used
        # Previously in the model assessments. This has been flagged as an action item to investigate 
        # and resolve in the next framework.
        ifelse(names(DD.lst[[bnk]])[9] == "U.se", names(DD.lst[[bnk]])[9] <- "U.cv", DD.lst[[bnk]]$U.cv <- DD.lst[[bnk]]$U.cv*50)
        # Also, if doing this we need to change the original data to represent what the model is seeing..
        # So if we used the SE let's replace the U.cv data with the U.se data, if we are doing the
        # 50x to match what we've done before than we need to change those data as well.
        ifelse(names(DD.lst[[bnk]])[9] == "U.se", DD.dat$U.cv <- DD.dat$U.se, DD.dat$U.cv <- DD.dat$U.cv*50)
        
        DD.lst[[bnk]]$NY<- length(DD.lst[[bnk]]$C)

        
        # Set up Priors.  This first bit is getting our variance correct for the CV's for Biomass, Recruit biomass, and catch rates.
        # Biomass CV
        uI=log(DD.lst[[bnk]]$I.cv^2+1)
        Ip.a=2+(uI/uI)^2
        Ip.b=uI*((uI/uI)^2+1)
        # Recruit biomass CV
        uIR=log(DD.lst[[bnk]]$IR.cv^2+1)
        IRp.a=2+(uIR/uIR)^2
        IRp.b=uIR*((uIR/uIR)^2+1)
        # Catch Rate CV
        uU=log(DD.lst[[bnk]]$U.cv^2+1)
        Up.a=2+(uU/uU)^2
        Up.b=uU*((uU/uU)^2+1)
        
        # Stuck at cannot bracket slice for node P[], if getting that error it's likely because your years aren't the right length l
        #ook at NY!
        #logK=			list(a=7,		b=7,		d="dnorm",		i1=8,	i2=10,	l=1		),		# scaler to total biomass
        
        DDpriors[[bnk]]=list(
          logK=			list(a=7,		  b=7,		d="dnorm",		i1=8,	i2=10,	l=1		),		# scaler to total biomass
          r=				list(a=0, 		b=1,		d="dlnorm",		i1=0.3,	i2=0.9,	l=NY	),		# recruit index
          q=				list(a=20, 		b=40, 		d="dbeta",		i1=0.2,	i2=0.5,	l=1		),		# catchability for survey fully recruited
          qU=				list(a=0, 		b=1,		d="dunif",		i1=0.4,	i2=0.7,	l=1		),		# catchability for commercial CPUE
          m=				list(a=-2,		b=2,		d="dlnorm",		i1=0.1,	i2=0.3,	l=NY	),		# natural mortality fully recruited
          mR=				list(a=-2,		b=2,		d="dlnorm",		i1=0.2,	i2=0.4,	l=NY	),		# natural mortality  recruits
          S=				list(a=8, 		b=11, 		d="dbeta",		i1=0.5,	i2=0.8,	l=1		),		# clapper dissolution rate
          sigma=			list(a=0, 		b=5,		d="dunif",		i1=2,	i2=3,	l=1		),		# process error (SD)
          sigma.tau=		list(a=2, 		b=1,		d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for fully recruited from survey (precision)
          sigma.rho=		list(a=2, 		b=1,		d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for recruits from survey (precision)
          sigma.upsilon=	list(a=2, 		b=1,		d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for recruits from survey (precision)
          ikappa.tau2=	list(a=3, 		b=0.44629,	d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for fully recruited clappers (precision)
          ikappa.rho2=	list(a=3, 		b=0.44629,	d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for recruit clappers (precision)
          I.precision=	list(a=Ip.a,	b=Ip.b,		d="dgamma",		i1=15,	i2=30,	l=NY	),		# measurement error for fully recruited from survey (precision)
          IR.precision=	list(a=IRp.a,	b=IRp.b,	d="dgamma",		i1=15,	i2=30,	l=NY	),		# measurement error for recruits from survey (precision)
          U.precision=	list(a=Up.a,	b=Up.b,		d="dgamma",		i1=15,	i2=30,	l=NY	)		# measurement error for recruits from survey (precision)
        #  P        =	  list(a=1,b=1,d="dgamma", i1 = 0.1,i2=0.1,l =0.1)		# Our P parameter
          )
 
      begTime <- Sys.time()
      # n = 500,000 and burn = 200,000, thin = 20 do not decrease these as retaining this much
      # data is needed to stabilize the projections, it does lengthen the run time from about 10 - 20 minutes per bank, and
      # they are longer chains than really are needed for the model to converge, but this is really being done just for the projections.
      # Run the model now.
      DD.out[[bnk]]<-delayBUGS(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst[[bnk]], DDpriors[[bnk]], yrs[[bnk]], n = niter, 
                        burn =nburn, thin = nthin, nchains= nchains,
                        debug=de.bug,add.parameters=c('Imed','Ipred','Irep','IRmed','IRpred','IRrep','sIresid',
                                                 'sIRresid','sPresid','Iresid','IRresid','Presid'))
      # Rename the output so I retain the results as a MCMC object...
    
      runTime <- Sys.time()-begTime
      runTime #just for curiosity
 
      
      #source("fn/projections.r")
      # The catch since the survey for the most recent year is
      proj.catch[[bnk]] <- proj.dat[[bnk]]$catch[proj.dat[[bnk]]$year == max(DD.dat$year)]
      # Get the low and upper boundaries for the decision table (this might be a silly way to do this...)
      D_low[[bnk]] <- subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$D_tab_low
      D_high[[bnk]] <-  subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$D_tab_high
      # The increment size for the decision table.  500 for GBa and 50 for BBn
      step <- ifelse(bnk == "GBa", 500,50)
      # The URP and LRP for the bank, for the moment only GBa has been accepted so it's the only one used.
      URP[[bnk]] <-  subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$URP
      LRP[[bnk]] <-  subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$LRP
      # Get the projection scenarios of interest
      proj[[bnk]] <- seq(D_low[[bnk]],D_high[[bnk]],step) + proj.catch[[bnk]]
 
      # The interim TAC is
      TACi[[bnk]] <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC
      #source("fn/biomass.plt.r") #biomass time series with ref points
      # Note that these plots don't account for growth next year, so are somewhat misleading compared to the decision table
      # which does account for growth!!  This should be not too painful to fix I think?
      bm.max <- ifelse(bnk == "GBa", 55000,25000)
      
      # Now do the projections
      DD.out[[bnk]]<- projections(DD.out[[bnk]],C.p=proj[[bnk]]) # C.p = potential catches in decision table
 
      ### Generate Decision Table ###
      ### Note that from the 2015 SSR we have these definitely set at...
      #Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
      #The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
      
      if (bnk == "GBa") 
      {
        D.tab[[bnk]]<-decision(bnk,DD.out[[bnk]], mu=0.15,refs=c(URP[[bnk]],LRP[[bnk]]),post.survey.C=proj.catch[[bnk]])
        if (export.tables == T) write.csv(D.tab[[bnk]],paste0(plotsGo,"Decision1_GBa.csv"),row.names=F)

      } # END if(bnk == "GBa")
      # Something wrong here for Browns North...
      if (bnk == "BBn") 
      {
        D.tab[[bnk]]<-decision(bnk,DD.out[[bnk]], mu=0.15,post.survey.C=proj.catch[[bnk]])
        if (export.tables == T) write.csv(D.tab[[bnk]],paste0(plotsGo,"Decision_1yrMeanm.csv"),row.names=F)
      } # END if(bnk == "BBn")
      
      # For i = 1 this will just get the first bank, unfortunately for the second back this will pull in results for both 
      #  (if running this as a loop) which is silly, but work arounds are dumber than this solution
      if(final.run == T) 
      {
        save(DD.lst, DDpriors,DD.out,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,manage.dat,proj.catch,
           URP,LRP,DD.dat,proj,bnk,TACi,yrs,
           file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))
      }
      if(final.run == F) 
      {
        save(DD.lst, DDpriors,DD.out,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,manage.dat,proj.catch,
             URP,LRP,DD.dat,proj,bnk,TACi,yrs,
             file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_testing_results.RData",sep=""))
      }
  } # end if(run.mod==T)

############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  
############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  
############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  




############# Section 3 Results for Update Doc and Model Diagonistics############# Section 3 Results for Update Doc and Model Diagonistics
############# Section 3 Results for Update Doc and Model Diagonistics############# Section 3 Results for Update Doc and Model Diagonistics
# If we want the diagnostics  this is the section.  First load the data if we didn't just run the model.
  if(make.diag == T && run.mod==F) load(file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))
  if(make.diag == T)
  {

      # Some model outputs needed for the Update.
      mort[[bnk]] <- 1- exp(-DD.out[[bnk]]$mean$m[length(DD.out[[bnk]]$mean$m)])
      # This lines up the column headers with the projected catch...
      TACI[[bnk]]<-which(DD.out[[bnk]]$data$C.p==(TACi[[bnk]]+proj.catch[[bnk]]))
      # This get us the predicted biomass for next year based on the projected catch
      BM.proj.1yr[[bnk]] <- DD.out[[bnk]]$median$B.p[TACI[[bnk]]]
      # This is only useful for GBa at the moment since BBn doesn't have reference points accepted yet...
      if(bnk == "GBa")
      {
        # Get the quantiles, this likely would need changed, but which quantile is > our URP (13,284 as of 2015)
        B.quantiles[[bnk]] <- quantile(DD.out[[bnk]]$sims.list$B[,length(DD.out[[bnk]]$sims.list$B[1,])],probs=seq(0,1,0.01))
        # This is the probability (well percentage) that Biomass is below the USR
        prob.below.USR[[bnk]] <- names((which(B.quantiles[[bnk]] > URP[[bnk]])[1]))
      } # end if(bnk=="GBa")
      
      # Here we can grab the Fully recruited and recruit biomass for the last 2 years and the median of the time series.
      FR.bm[[bnk]] <- DD.out[[bnk]]$median$B[(length(DD.out[[bnk]]$mean$B)-1):length(DD.out[[bnk]]$median$B)]
      # We exclude the current year from the median estimate
      FR.ltm[[bnk]] <- median(DD.out[[bnk]]$median$B[-length(DD.out[[bnk]]$median$B)])
      # Recruit biomass
      rec.bm[[bnk]] <- DD.out[[bnk]]$median$R[(length(DD.out[[bnk]]$median$R)-1):length(DD.out[[bnk]]$median$R)]
      # We exclude the current year from the median estimate
      rec.ltm[[bnk]] <- median(DD.out[[bnk]]$median$R[-length(DD.out[[bnk]]$median$R)])
      
      # Get the percent biomass change from the projection. 0 means unchanged, + means % increase, - means % decline
      percent.B.change[[bnk]] <- (BM.proj.1yr[[bnk]] / DD.out[[bnk]]$median$B[length(DD.out[[bnk]]$median$B)]) -1
  
  
  
  ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS 
  ##### Now we can run some model diagnostics.
  # Some quick diagnoistics, the maximum should be < 1.05
  rhat[[bnk]] <- summary(DD.out[[bnk]]$summary[,8])

  # Effective number of observations.  
  #Not sure what our minimum should be here, but using the Rhat + looking at the chains should indicate where there are problems...
  neff[[bnk]] <- range(DD.out[[bnk]]$summary[,9])


    save(mort,TACI,BM.proj.1yr,B.quantiles,percent.B.change,prob.below.USR,FR.bm,FR.ltm,rec.bm,rec.ltm,neff,rhat,
         file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_results_and_diagnostics.RData",sep=""))
  } # end (if make.diag == T)

 
##################### END Section 3 Model Diagnostics #####################  END Section 3 Model Diagnostics #################  
##################### END Section 3 Model Diagnostics #####################  END Section 3 Model Diagnostics #################  


#################  SECTION 4 Figures #################  SECTION 4 Figures #################  SECTION 4 Figures #################  
#################  SECTION 4 Figures #################  SECTION 4 Figures #################  SECTION 4 Figures #################  
# Now if I want to make the figures both from the model output and for the update document do these...

  if(make.figs == T)
  {
    # First make sure we have the model output data.
    if(run.mod==F && make.diag==F) 
      {
        load(file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))
      } # end if(make.figs == T && run.mod==F && make.diag==F) 
    #####Plot model diagnostics############## 
    # These plots include the posterior fits, exploitation estimate, Biomass fit to survey and CPUE, residual plot
    # and the model convergence plot (which is a 700+ page pdf of the convergence of each parameter + it's ACF.)
    if(make.diag.figs == T)
    {
      # posterior densities for model parameters
      post.plt(DD.out[[bnk]],DDpriors[[bnk]],years=yrs[[bnk]], graphic=fig,nr=3,nc=4,wd=15,multi=F,path=plotsGo)
      #dev.off()
      ##exploitaiton time series
      exploit.plt(DD.out[[bnk]], years=yrs[[bnk]], plt=c('f','m','mR'),graphic=fig,path=plotsGo)
      #dev.off()
      # model biomass fit to survey
      fit.plt(DD.out[[bnk]], CI=T,graphic=fig,path=plotsGo,CV=T)
      # diagnostic plot
      diag.plt(DD.out[[bnk]], graphic=fig,path=plotsGo)
      
      # Here we pull together all of the chains and look to see that they are both well mixed and that
      # there is no correlation.   This is a complete crap load of plots!!
      # Function to plot all the chains, this assumes we have 2 chains, if we had three this would need revised!
      
      # Get the bank pulled out and figure out how many parameters we have
      num.param <- length(names(DD.out[[bnk]]$sims.list))
      param.names <- names(DD.out[[bnk]]$sims.list)
      # Since our first parameter is a hyperparameter (there isn't one for every year) this works, 
      #if this was a matrix we'd get an error here.
      len <- length(DD.out[[bnk]]$sims.list[[1]])
      # Make the pdf, given the number of parameters in the model you don't get an option for making this plot print to screen
      # if you run diagnostics you get this pdf
      pdf(file=paste(plotsGo,"Model_convergence.pdf",sep=""),onefile=T)
      for(i in 1:num.param)
      {
        
        # This pulls out all the plot for parameters with only one values
        if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])==T)
        {
          # if(i == 1)
          par(mfrow = c(3,1),mar=c(2,2,3,1))
          # plot the chains
          plot(DD.out[[bnk]]$sims.list[[param.names[i]]][1:(len/2)],type="l",col="darkgreen", 
               main = paste(param.names[i], "Chain"),xlab="",ylab="")
          lines(DD.out[[bnk]]$sims.list[[param.names[i]]][(len/2+1):len],col="blue")
          # And look for any signs of autocorrelation in the chains...
          acf(DD.out[[bnk]]$sims.list[[param.names[i]]][1:(len/2)],lag.max = 10,main = "ACF chain 1",xlab="",ylab="",ylim=c(0,0.3))
          acf(DD.out[[bnk]]$sims.list[[param.names[i]]][(len/2+1):len],lag.max = 10,main="ACF chain 2",xlab="",ylab="",ylim=c(0,0.3))
        } # end if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[1]]])==T)
        
        # This pulls out all the plots for parameters with multiple values (i.e. annual estimates)
        if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])==F)
        {
          num.reps <- ncol(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])
          rep.names <- paste(names(DD.out[[bnk]]$sims.list)[i],1:num.reps,sep="_")
          # Run this loop for each chain for these parameters.
          for(k in 1:num.reps)
          {
            par(mfrow = c(3,1),mar=c(2,2,3,1))
            # plot the chains
            plot(DD.out[[bnk]]$sims.list[[param.names[i]]][1:(len/2),k],type="l",col="darkgreen", 
                 main = paste(rep.names[k], "Chain"),xlab="",ylab="")
            lines(DD.out[[bnk]]$sims.list[[param.names[i]]][(len/2+1):len,k],col="blue")
            # And look for any signs of autocorrelation in the chains...
            acf(DD.out[[bnk]]$sims.list[[param.names[i]]][1:(len/2),k],lag.max = 10,main = "ACF chain 1",xlab="",ylab="")
            acf(DD.out[[bnk]]$sims.list[[param.names[i]]][(len/2+1):len,k],lag.max = 10,main="ACF chain 2",xlab="",ylab="")
            
          } # end for(k in 1:num.reps)
        } # end if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])==F)
      }  # end for(i in 1:num.param)
      dev.off()
    
    } #end  if(make.diag.figs == T)
    
    # Do we want to do the prediction evaluation plots
    if(make.pred.eval.figs == T)
    {
      #Prediction Evaluation using the predicted growth, DK still needs to go over this code to ensure it is correct!!!
      peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst[[bnk]], DDpriors[[bnk]], yrs[[bnk]], pe=(max(yrs[[bnk]])):2000,
          n = niter,burn = nburn, thin = nthin, plot=0,g2=DD.dat$g2,gR2=DD.dat$gR2,lab="GBa",dirt=direct)
    
      # Plot the prediction evaluation which uses the predicted growth
      peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""),DD.lst[[bnk]], DDpriors[[bnk]], yrs[[bnk]], pe=(max(yrs[[bnk]])):2001,run=F, 
          plot=3,proj.from="BUGS",graphic=fig,lab=bnk,path=plotsGo,dirt=direct)
      #Prediction Evaluation using the modelled growth.
      peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst[[bnk]], DDpriors[[bnk]], yrs[[bnk]], pe=(max(yrs[[bnk]])):2000,n = niter,
          burn = nburn, thin = nthin, plot=0,lab=paste(bnk,"g1",sep = ""),dirt=direct)
    
      # Plot the prediction evaluation
      peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst[[bnk]], DDpriors[[bnk]], yrs[[bnk]], pe=(max(yrs[[bnk]])):2001,run=F, 
          plot=3,proj.from="BUGS",graphic=fig,lab=paste(bnk,"g1",sep = ""),path=plotsGo,dirt=direct)
    }# end if(make.pred.eval.figs == T)
      
      
      
    # Now we make the figures used in the update document  
    if(make.update.figs == T)
      {
    # Now make the biomass plots for Browns and Georges as necessary
    if(bnk == "BBn")
    {
      source(paste(direct,"Assessment_fns/Model/biomass.plt.r",sep="")) # I don't like loading functions like this, but best solution for now...
      biomass.plt(DD.out[[bnk]],years=yrs[[bnk]], graphic=fig,TAC=TACi[[bnk]]+proj.catch[[bnk]],path=plotsGo,refs=NULL,
                  URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median,Bymax=bm.max)
    } # end if(bnk == "BBn")
    
    if(bnk == "GBa")
    {
      biomass.plt(DD.out[[bnk]],years=yrs[[bnk]], graphic=fig,TAC=TACi[[bnk]]+proj.catch[[bnk]],path=plotsGo,refs = c("LRP","URP","zones"),
                  URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median,Bymax=bm.max)

    } # end if(bnk == "GBa")
      
    
      #  Now we transition to produce the figures used in the Update document that are not dependent on model output.
      # First up we need the fishery data and TAC here, we don't actually have the calendar year fishery data 
      # anywhere at this point so we grab that
      logs_and_fish(loc="offshore",year = 1998:max(mod.dat[[bnk]]$year),un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
      # If you get any NA's related warnings it may be something is being treated as a Factor in one of the two files.  
      # This should combine without any warnings so don't ignore warnings here.
      fish.dat<-merge(new.log.dat,old.log.dat,all=T)
      fish.dat$ID<-1:nrow(fish.dat)
      # Being lazy we get the data for each bank We are just looking for the annual values here so nothing fancy needed...
      dat <- fishery.dat(fish.dat,bk=bnk,yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct,period = "calyr") 	
      if(bnk == "GBa") dat1<-fishery.dat(fish.dat,bk="GBb",yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct,period = "calyr") 	
      
      if(fig== "screen") windows(8.5,8.5)
      if(fig == "pdf") pdf(paste(plotsGo,"TAC_landings.pdf",sep=""),width=8.5,height=8.5)
      # Here are the time series on Georges Bank
      if(bnk == "GBa") par(mfrow=c(2,1),cex=1.2,mar=c(2,5,1,1))
      if(bnk == "BBn") par(mfrow=c(1,1),cex=1.2,mar=c(2,5,1,1))
      plot(dat$catch~dat$year,type="n",ylab="",xlab="",las=1,xaxt="n",bty="n",ylim=c(0,max(dat$catch*1.1,na.rm=T)))
      axis(1,pos=0)
      abline(h=0)
      points(dat$catch~dat$year,  type='h',pch=15,lwd=16,lend=3,col="grey50")
      lines(subset(manage.dat,year %in% dat$year & bank ==bnk)$TAC~dat$year,lwd=2,col="blue")
      if(bnk == "GBa") legend("topright","TAC",title="Georges Bank A",bty="n",col="blue",lwd=2)
      if(bnk == "BBn")
      {
        legend("topright","TAC",title="Browns Bank North",bty="n",col="blue",lwd=2)
        mtext(side=2,"Landings (meat, t)",line=3.3,cex=1.5)
      }
      # Now GBb
      if(bnk=="GBa")
        {
          plot(dat1$catch~dat1$year,type="n",ylab="",xlab="",las=1,xaxt="n",bty="n",ylim=c(0,1300))
          axis(1,pos=0)
          abline(h=0)
          points(dat1$catch~dat1$year,  type='h',pch=15,lwd=16,lend=3,col="grey50")
          lines(subset(manage.dat,year %in% dat1$year & bank =="GBb")$TAC~dat1$year,lwd=2,col="blue")
          legend("topright","TAC",title="Georges Bank B",bty="n",col="blue",lwd=2)
          mtext(side=2,"Landings (meat, t)",line=3.3,adj=2,cex=1.5)
        } # end if(bnk=")
      # Turn off the plot device if making a pdf.
      if(fig == "pdf") dev.off()
  
  
    ## Now we bring in the spatial plots for fully recruited and recruits for the Update document.
    load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
    com.contours <- NULL
    rec.contours <- NULL
    # Run this for both BBn and GBa
    
    # Putting GBa and GBb on the same plot...
    if( bnk == "GBa") surv.Live2 <- rbind(surv.Live[[bnk]],surv.Live[["GBb"]])
    if( bnk == "BBn") surv.Live2 <- surv.Live[[bnk]]
    # default color scheme
    N.col <- "YlGn"
    # Get the contours set up for the figure...
    
    com.contours<-contour.gen(subset(surv.Live2,year==yr,
                                                 c('tow','lon','lat','com')),
                                          ticks=c(1,5,10,50,100,500,1000,5000,10000,20000,50000),
                                          str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',
                                          color.fun=tim.colors,id.par=5,units='#/tow',
                                          plot=F,subscale=0.1,direct =direct)
    rec.contours<-contour.gen(subset(surv.Live2,year==yr,
                                                 c('tow','lon','lat','rec')),
                                          ticks=c(1,5,10,50,100,500,1000,5000,10000,20000,50000),
                                          str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',
                                          color.fun=tim.colors,id.par=5,units='#/tow',
                                          plot=F,subscale=0.1,direct = direct)
    
    # Make both the plots, fully recuited and recruits...
    for(i in 1:2)
    {
    # First one is the fully recruited
    if(i ==1)con <- com.contours
    # Second one is the recruits
    if(i ==2) con <- rec.contours
    # Get the survey boundaries
    if(bnk == "GBa")
    {
    bound.poly.surv <- subset(survey.bound.polys,label %in% c("GBa","GBb")) 
    bound.poly.surv$SID[bound.poly.surv$label == "GBb"] <-2
    }
    if(bnk == "BBn") bound.poly.surv <- subset(survey.bound.polys,label == bnk) 
    # These need a different boundary polygon
    attr(bound.poly.surv,"projection")<-"LL"
    # Now make each figure, note that we include both GBa and GBb on these figures.
    # First I get the levels set up for the data...
    lvls1=c(1,50,100,500)
    CL <- contourLines(con$image.dat,levels=lvls1)
    CP <- convCP(CL)
    Cont1.poly <- joinPolys(CP$PolySet,bound.poly.surv)
    cont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F) 
    lvls2=c(2000,5000,10000,20000,50000)
    # If counts are high enough to need lvls2 we use it, if not we just use lvls1.
    if(max(con$image.dat$z)>=lvls2[1])
    {
      CL <- contourLines(con$image.dat,levels=lvls2)
      CP <- convCP(CL)
      Cont2.poly <- joinPolys(CP$PolySet,bound.poly.surv)
      Cont2.poly$PID<-Cont2.poly$PID+length(lvls1)
      Cont.poly<-rbind(Cont1.poly,Cont2.poly)
      lvls<-c(lvls1,lvls2)
      cont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
                              col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA,stringsAsFactors = F)
      cont.data<-rbind(cont1.data,cont2.data)
    } # end if(max(rec.contours$image.dat$z)>=lvls2[1])
    if(max(con$image.dat$z)<lvls2[1])
    {
      Cont.poly<-Cont1.poly
      cont.data<-cont1.data
      lvls <- lvls1
    } # end if(max(rec.contours$image.dat$z)<lvls2[1])
    
    # Make the plot
    if(i == 1 && fig == "pdf") pdf(paste(plotsGo,"Fully_recruited_survey_abundance_spatial.pdf",sep=""),width=11,height=8.5)
    if(i == 2 && fig == "pdf" ) pdf(paste(plotsGo,"Recruit_survey_abundance_spatial.pdf",sep=""),width=11,height=8.5)
    if(fig == "screen") windows(11,8.5)
    ScallopMap(bnk,title=NULL,contours=list(Cont.poly,cont.data),
               plot.bathy=T,plot.boundries=T,boundries="offshore",
               direct=direct,cex.mn=2,dec.deg = F,cex=1.3)
    legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
                          paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,
           title=N.tow.lab <- expression(frac(N,tow)), title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
           pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
      # Add the text
      if(bnk== "GBa") 
        {
          text(-66.8,42.2,"Zone a",cex=1.3)
          text(-66.2,42.2,"Zone b",cex=1.3)
          text(-66.66,41.4,"ICJ Line",cex=1.3)
        } # end if(bnk== "GBa") 
    # Turn off the plot device if making a pdf.
    if(fig == "pdf") dev.off()
    } # end for(i in 1:2)
  
  
  
  
    #############  FINALLY I WANT TO MAKE AN OVERALL PLOT OF THE BANKS AND THAT WILL BE THAT...
    # Also make the overall plot of the banks...
    if(fig== "screen") windows(11,8.5)
    if(fig == "pdf") pdf(paste(plotsGo,"Offshore_banks.pdf",sep=""),width=11,height=8.5)
    ScallopMap("NL",plot.bathy=T,plot.boundries=T,boundries="offshore",bound.color = T,label.boundries = T,offshore.names = T,
               direct=direct,cex.mn=2,dec.deg = F,cex=1.3,shore="nwatlHR")
    # Turn off the plot device if making a pdf.
    if(fig == "pdf") dev.off()
    } # end if(make.update.figs == T)
  
  }# end (if make.figs==T)


} # end for(j in 1:num.banks)

} # end function
