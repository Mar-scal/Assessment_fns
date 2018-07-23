####################################################################
## GEORGES A AND BROWNS BANK NORTH.
## This gets the final model data together and runs the model.  Revised by DK in January of 2016.
###################################################################
direct = "d:/r/"
#direct = "g:/r/"

yr = as.numeric(format(Sys.time(), "%Y")) -1
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
source(paste(direct,"Assessment_fns/Model/StephensFix.R", sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) # Source9 Revised by DK September 2015
source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) #Source19 Revised by DK September 2015
library ("R2WinBUGS")

#These are the model results, short cut to running the whole model again if you are satisfied with your results.
# Or you just want to look at what you have so far.
#load(paste(direct,(yr),"/Updates/GBa/Results/GBa_model_results.R",sep=""))



#############  Section 1  Compile the data for the banks ######  Section 1  Compile the data for the banks################## 
#############  Section 1  Compile the data for the banks ######  Section 1  Compile the data for the banks################## 
#############  Section 1  Compile the data for the banks ######  Section 1  Compile the data for the banks################## 
# If you have already run section 1 for the current year you can load the data in here and skip section 1
#load(file=paste(direct,"Data/Model/",(yr+1),"/Model_input.RData",sep=""))


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

# We have 2 banks which are modeled so 
bank <- c("GBa","BBn")
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
    cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk=bank[i],yr=(min(years)-1):max(years),method='jackknife',direct=direct,period = "survyr") 	
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
    proj.dat[[bank[i]]] <- fishery.dat(proj.sub,bk=bank[i],yr=(min(years)-1):max(years),method='jackknife',direct=direct,period = "calyr") 	
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
#############  End Section 1  Compile the data for the banks ######  End Section 1  Compile the data for the banks################## 
#############  End Section 1  Compile the data for the banks ######  End Section 1  Compile the data for the banks################## 
#############  End Section 1  Compile the data for the banks ######  End Section 1  Compile the data for the banks################## 



#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model ###########
#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model ###########
#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model ###########
# Load the data if already up to date.
#load(file=paste(direct,"Data/Model/",(yr+1),"/Model_input.RData",sep=""))
# Also bring in the reference point and TAC data. Make sure you update this each year with the interm TAC.
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
DD.out <- NULL
DD.out2 <- NULL
DD.out3<- NULL
bank <- unique(names(mod.dat))
num.banks <- length(bank)
D.tab <- NULL
D.tab.2m <- NULL
D.tab.3m <- NULL
proj.catch <- NULL
URP <- NULL
LRP <- NULL
DD.dat <- NULL
proj <- NULL
D_low <- NULL
D_high <- NULL
TACi <- NULL
for(i in 1:num.banks)
  {
    DD.dat <- subset(mod.dat[[bank[i]]],year %in% 1986:max(mod.dat[[bank[i]]]$year))
    names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                        "w.bar","l.bar", "l.k",  "w.k","CF","clappers","clappersR","CS",  "RS","C","E","n.trips","U",
                        "U.var","U.se","LCI","UCI","U.cv", "g","g2","gR","gR2") 
    # Organize the data and set up the model priors/initialization data, then run the model.
    
    bnk <- bank[i]
    yrs<-min(DD.dat$year):max(DD.dat$year)
    NY<- length(yrs)
    
    DD.lst<-as.list(subset(DD.dat,year%in%yrs,c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers",
                                                "clappersR")))
    # Downweight the CV for the CPUE data.  This is done to be consistent with CV used
    # Previously in the model assessments.  This has been flagged as an action item to investigate 
    # and resolve in the next framework.
    ifelse(names(DD.lst)[9] == "U.se", names(DD.lst)[9] <- "U.cv", DD.lst$U.cv <- DD.lst$U.cv*50)
    # Also, if doing this we need to change the original data to represent what the model is seeing..
    # So if we used the SE let's replace the U.cv data with the U.se data, if we are doing the
    # 50x to match what we've done before than we need to change those data as well.
    if(names(DD.lst)[9] == "U.se", DD.dat$U.cv <- DD.dat$U.se, DD.dat$U.cv <- DD.dat$U.cv*50)
    
    DD.lst$NY<- length(DD.lst$C)
    #DD.lst$C.p<-seq(2000,6000,500)+965
    #DD.lst$NC<-length(DD.lst$C.p)
    
    # Set up Priors
    uI=log(DD.lst$I.cv^2+1)
    Ip.a=2+(uI/uI)^2
    Ip.b=uI*((uI/uI)^2+1)
    
    uIR=log(DD.lst$IR.cv^2+1)
    IRp.a=2+(uIR/uIR)^2
    IRp.b=uIR*((uIR/uIR)^2+1)
    
    uU=log(DD.lst$U.cv^2+1)
    Up.a=2+(uU/uU)^2
    Up.b=uU*((uU/uU)^2+1)
    
    
    # Stuck at cannot bracket slice for node P[], if getting that error it's likely because your years aren't the right length look at NY!
    #logK=			list(a=7,		b=7,		d="dnorm",		i1=8,	i2=10,	l=1		),		# scaler to total biomass
    
    DDpriors=list(
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
    
    
  
  #source("delayBUGS.r")
    
  #begTime <- Sys.time()
  # n = 500,000 and burn = 200,000, thin = 20 do not decrease these as retaining this much
  # data is needed to stabilize the projections, it does lengthen the run time from about 10 - 20 minutes per bank, and
  # they are longer chains than really are needed for the model to converge, but this is really being done just for the projections.
  # Run the model now.
  DD.out[[bnk]]<-delayBUGS(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, yrs, n = 500000, 
                    burn =200000, thin = 20,
                    debug=F,add.parameters=c('Imed','Ipred','Irep','IRmed','IRpred','IRrep','sIresid',
                                             'sIRresid','sPresid','Iresid','IRresid','Presid'))
  # Rename the output so I retain the results as a MCMC object...

  #runTime <- Sys.time()-begTime
  #runTime #just for curiosity
  
  # Set the working directory to save the figures
  plotsGo <- paste(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/",sep="")
  
  ###Plot model diagnostics############## 
  # posterior densities for model parameters
  post.plt(DD.out[[bnk]],DDpriors,years=yrs, graphic='pdf',nr=3,nc=4,wd=15,multi=F,path=plotsGo)
  #dev.off()
  ##exploitaiton time series
  exploit.plt(DD.out[[bnk]], years=yrs, plt=c('f','m','mR'),graphic='pdf',path=plotsGo)
  #dev.off()
  # model biomass fit to survey
  fit.plt(DD.out[[bnk]], CI=T,graphic='pdf',path=plotsGo,CV=T)
  # diagnostic plot
  diag.plt(DD.out[[bnk]], graphic='pdf',path=plotsGo)
  
  #Prediction Evaluation using the predicted growth, DK still needs to go over this code to ensure it is correct!!!
#  peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, yrs, pe=(max(yrs)):2000,n = 60000,
#       burn = 30000, thin = 10, plot=0,g2=DD.dat$g2,gR2=DD.dat$gR2,lab="GBa")
  
  # Plot the prediction evaluation which uses the predicted growth
#  peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, yrs, pe=(max(yrs)):2001,run=F, 
#      plot=3,proj.from="BUGS",graphic='pdf',lab=bnk,path=plotsGo)
  #Prediction Evaluation using the modelled growth.
#  peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, yrs, pe=(max(yrs)):2000,n = 60000, 
#      burn = 30000, thin = 10, plot=0,lab=paste(bnk,"g1",sep = ""))
  
  # Plot the prediction evaluation
#  peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, yrs, pe=(max(yrs)):2001,run=F, 
#      plot=3,proj.from="BUGS",graphic='pdf',lab=paste(bnk,"g1",sep = ""),path=plotsGo)
  
  
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
  
  
  # Now do the projections, the code can run using last year 
  DD.out[[bnk]]<- projections(DD.out[[bnk]],C.p=proj[[bnk]]) # C.p = potential catches in decision table
  #testing out a new method for projecting using a 3-year mean of m rather than m from the current year. This was examined in 2014/15
  #because of very large increase in m from the model in that year
  # 2 year average mortality
  #DD.out2[[bnk]]<-projections.2015(DD.out[[bnk]],C.p=proj,mort=2) # C.p = potential catches in decision table
  # 3 year average mortality
  if(bnk == "GBa") DD.out3[[bnk]]<-projections.2015(DD.out[[bnk]],C.p=proj[[bnk]],mort=3) # C.p = potential catches in decision table
  
  
  # The interim TAC is
  TACi[[bnk]] <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC
  #source("fn/biomass.plt.r") #biomass time series with ref points
  # Note that these plots don't account for growth next year, so are somewhat misleading compared to the decision table
  # which does account for growth!!  This should be not too painful to fix I think?
  bm.max <- ifelse(bnk == "GBa", 55000,25000)
  
  ### Note that from the 2015 SSR we have these definitely set at...
  #Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
  #The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
  
  if(bnk == "BBn")
    {
        source(paste(direct,"Assessment_fns/Model/biomass.plt.r",sep="")) # I don't like loading functions like this, but best solution for now...
        biomass.plt(DD.out[[bnk]],years=yrs, graphic='pdf',TAC=TACi[[bnk]]+proj.catch[[bnk]],path=plotsGo,refs=NULL,
                URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median,Bymax=bm.max)
    } # end if(bnk == "BBn")
  
  if(bnk == "GBa")
    {
    biomass.plt(DD.out[[bnk]],years=yrs, graphic='pdf',TAC=TACi[[bnk]]+proj.catch[[bnk]],path=plotsGo,refs = c("LRP","URP","zones"),
                URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median,Bymax=bm.max)
    #biomass.plt(DD.out2[[bnk]],years=yrs, graphic='R',TAC=TACi+proj.catch,path=plotsGo,refs = c("LRP","URP"),URP =13284, LRP=7137,
    #            index=1:24,avg.line=median,Bymax=bm.max)
    biomass.plt(DD.out3[[bnk]],years=yrs, graphic='pdf',TAC=TACi[[bnk]]+proj.catch[[bnk]],path=plotsGo,refs = c("LRP","URP","zones"),
                URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median,Bymax=bm.max)
    } # end if(bnk == "GBa")
  
  ### Generate Decision Table ###
  ### Note that from the 2015 SSR we have these definitely set at...
  #Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
  #The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
  #source("fn/decision.r") #generates decision table
  if (bnk == "GBa") 
  {
    D.tab[[bnk]]<-decision(bnk,DD.out[[bnk]], mu=0.15,refs=c(13284,7137),post.survey.C=proj.catch[[bnk]])
    write.csv(D.tab[[bnk]],paste0(plotsGo,"Decision1_GBa.csv"),row.names=F)
    #D.tab.2m[[bnk]]<-decision(bnk,DD.out2[[bnk]], mu=0.15,refs=c(13284,7137),post.survey.C=proj.catch)
    #write.csv(D.tab.2m[[bnk]],paste0(plotsGo,"Decision_GBa_2yrMeanm.csv"),row.names=F)
    D.tab.3m[[bnk]]<-decision(bnk,DD.out3[[bnk]], mu=0.15,refs=c(13284,7137),post.survey.C=proj.catch[[bnk]])
    write.csv(D.tab.3m[[bnk]],paste0(plotsGo,"Decision_3yrMeanm.csv"),row.names=F)
  } # END if(bnk == "GBa")
  # Something wrong here for Browns North...
  if (bnk == "BBn") 
  {
    D.tab[[bnk]]<-decision(bnk,DD.out[[bnk]], mu=0.15,post.survey.C=proj.catch[[bnk]])
    write.csv(D.tab[[bnk]],paste0(plotsGo,"Decision_1yrMeanm.csv"),row.names=F)
  } # END if(bnk == "BBn")
  
  
  # For i = 1 this will just get the first bank, unfortunately for the second back this will pull in results for both 
  #  (if running this as a loop) which is silly, but work arounds are dumber than this solution
  save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,manage.dat,proj.catch,
       URP,LRP,DD.dat,proj,bnk,TACi,
       file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))

} # end for(i in 1:num.banks)

############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  
############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  
############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  




############# Section 3 Results for Update Doc and Model Diagonistics############# Section 3 Results for Update Doc and Model Diagonistics
############# Section 3 Results for Update Doc and Model Diagonistics############# Section 3 Results for Update Doc and Model Diagonistics

# Load in the model results if necessary
#load(file=paste(direct,"Data/Model/",(yr+1),"/","BBn","/Results/Final_model_results.RData",sep=""))
mort.3 <- NULL
mort <- NULL
TACI <- NULL
BM.proj.1yr <- NULL
BM.proj.3yr <- NULL
B.quantiles <- NULL
percent.B.change <- NULL
prob.below.USR <- NULL
FR.bm <- NULL
FR.ltm <- NULL
rec.bm <- NULL
rec.ltm <- NULL
banks <- c("GBa","BBn")
i=1

# Here we grab all the summary information that is used in the Update document.
for(i in 1:length(banks))
  {
    bnk <- banks[i]
    # Some model outputs needed for the Update.
    mort.3[[bnk]] <- 1- mean(exp(-DD.out[[bnk]]$mean$m[(length(DD.out[[bnk]]$mean$m)-2):length(DD.out[[bnk]]$mean$m)]))
    mort[[bnk]] <- 1- exp(-DD.out[[bnk]]$mean$m[length(DD.out[[bnk]]$mean$m)])
    
    # This lines up the column headers with the projected catch...
    TACI[[bnk]]<-which(DD.out[[bnk]]$data$C.p==(TACi[[bnk]]+proj.catch[[bnk]]))
    # This get us the predicted biomass for next year based on the projected catch
    BM.proj.1yr[[bnk]] <- DD.out[[bnk]]$median$B.p[TACI[[bnk]]]
    if(bnk == "GBa") BM.proj.3yr[[bnk]] <- DD.out3[[bnk]]$median$B.p[TACI[[bnk]]]
    
    # This is only useful for GBa at the moment since BBn doesn't have reference points accepted yet...
    if(bnk == "GBa")
      {
        # Get the quantiles, this likely would need changed, but which quantile is > our URP (13,284 as of 2015)
        B.quantiles[[bnk]] <- quantile(DD.out3[[bnk]]$sims.list$B[,length(DD.out3[[bnk]]$sims.list$B[1,])],probs=seq(0,1,0.01))
        # This is the probability (well percentage) that Biomass is below the USR
        prob.below.USR[[bnk]] <- names((which(B.quantiles[[bnk]] > URP[[bnk]])[1]))
      } # end if(bnk=="GBa")
    
    # Here we can grab the Fully recruited and recruit biomass for the last 2 years and the median of the time series.
    FR.bm[[bnk]] <- DD.out[[bnk]]$median$B[(length(DD.out[[bnk]]$mean$B)-1):length(DD.out[[bnk]]$median$B)]
    FR.ltm[[bnk]] <- median(DD.out[[bnk]]$median$B[-length(DD.out[[bnk]]$median$B)])
    
    rec.bm[[bnk]] <- DD.out[[bnk]]$median$R[(length(DD.out[[bnk]]$median$R)-1):length(DD.out[[bnk]]$median$R)]
    rec.ltm[[bnk]] <- median(DD.out[[bnk]]$median$R[-length(DD.out[[bnk]]$median$R)])
    
    # Interesting that this doesn't jive with the decision table...
    if(bnk == "GBa") percent.B.change[[bnk]] <- BM.proj.3yr[[bnk]] / DD.out[[bnk]]$median$B[length(DD.out[[bnk]]$median$B)]
    if(bnk == "BBn") percent.B.change[[bnk]] <- BM.proj.1yr[[bnk]] / DD.out[[bnk]]$median$B[length(DD.out[[bnk]]$median$B)]
  } # end for(i in 1:length(banks))


# We also need the fishery data and TAC here, we don't actually have the calendar year fishery data anywhere at this point so lets get that...

logs_and_fish(loc="offshore",year = 1998:max(mod.dat[[bnk]]$year),un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
# If you get any NA's related warnings it may be something is being treated as a Factor in one of the two files.  
# This should combine without any warnings so don't ignore warnings here.
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
# We are just looking for the annual values here so nothing fancy needed...
BBn.dat <- fishery.dat(fish.dat,bk="BBn",yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct,period = "calyr") 	
GBb.dat <- fishery.dat(fish.dat,bk="GBb",yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct,period = "calyr") 	
GBa.dat <- fishery.dat(fish.dat,bk="GBa",yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct,period = "calyr") 	

# Here are the time series on Georges Bank
windows(8.5,8.5)
par(mfrow=c(2,1),cex=1.2,mar=c(2,5,1,1))
plot(GBa.dat$catch~GBa.dat$year,type="n",ylab="",xlab="",las=1,xaxt="n",bty="n",ylim=c(0,7000))
axis(1,pos=0)
abline(h=0)
points(GBa.dat$catch~GBa.dat$year,  type='h',pch=15,lwd=16,lend=3,col="grey50")
lines(subset(manage.dat,year %in% GBa.dat$year & bank =="GBa")$TAC~GBa.dat$year,lwd=2,col="blue")
legend("topright","TAC",title="Georges Bank A",bty="n",col="blue",lwd=2)
# Now GBb
plot(GBb.dat$catch~GBb.dat$year,type="n",ylab="",xlab="",las=1,xaxt="n",bty="n",ylim=c(0,1300))
axis(1,pos=0)
abline(h=0)
points(GBb.dat$catch~GBb.dat$year,  type='h',pch=15,lwd=16,lend=3,col="grey50")
lines(subset(manage.dat,year %in% GBb.dat$year & bank =="GBb")$TAC~GBa.dat$year,lwd=2,col="blue")
legend("topright","TAC",title="Georges Bank B",bty="n",col="blue",lwd=2)
mtext(side=2,"Landings (meat, t)",line=3.3,adj=2,cex=1.5)

# And here is Browns Bank North
windows(8.5,8.5)
par(mfrow=c(1,1),cex=1.2,mar=c(2,5,1,1))
plot(BBn.dat$catch~BBn.dat$year,type="n",ylab="",xlab="",las=1,ylim=c(0,2200),xaxt="n",bty="n")
axis(1,pos=0)
abline(h=0)
points(BBn.dat$catch~BBn.dat$year,  type='h',pch=15,lwd=16,lend=3,col="grey50")
lines(subset(manage.dat,year %in% BBn.dat$year & bank =="BBn")$TAC~BBn.dat$year,lwd=2,col="blue")
legend("topright","TAC",title="Browns Bank North",bty="n",col="blue",lwd=2)
mtext(side=2,"Landings (meat, t)",line=3.3,cex=1.5)


### Here I bring in the spatial plots for fully recruited and recruits for the Update document.
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
com.contours <- NULL
rec.contours <- NULL
banks <- c("GBa","BBn")
# Run this for both BBn and GBa
for(j in 1:2)
{
bnk <- banks[j]
# Putting GBa and GBb on the same plot...
if( j == 1) surv.Live2 <- rbind(surv.Live[["GBa"]],surv.Live[["GBb"]])
if( j == 2) surv.Live2 <- surv.Live[["BBn"]]
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
if(j == 1)
{
bound.poly.surv <- subset(survey.bound.polys,label %in% c("GBa","GBb")) 
bound.poly.surv$SID[bound.poly.surv$label == "GBb"] <-2
}
if(j == 2) bound.poly.surv <- subset(survey.bound.polys,label == bnk) 
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

if(i ==1 )png(paste(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/Fully_recruited_survey_abundance_spatial.png",sep=""),
              width=11,height=8.5,units="in",res=480)
if(i ==2 )png(paste(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/Recruit_survey_abundance_spatial.png",sep=""),
              width=11,height=8.5,units="in",res=480)
ScallopMap(bnk,title=NULL,contours=list(Cont.poly,cont.data),
           plot.bathy=T,plot.boundries=T,boundries="offshore",
           direct=direct,cex.mn=2,dec.deg = F,cex=1.3)
legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
                      paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,
       title=N.tow.lab <- expression(frac(N,tow)), title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
       pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
# Add the text
text(-66.8,42.2,"Zone a",cex=1.3)
text(-66.2,42.2,"Zone b",cex=1.3)
text(-66.66,41.4,"ICJ Line",cex=1.3)
dev.off()
} # end for(i in 1:2)
} # end for(j in 1:2)

# Also make the overall plot of the banks...
windows(11,8.5)
ScallopMap("NL",plot.bathy=T,plot.boundries=T,boundries="offshore",bound.color = T,label.boundries = T,offshore.names = T,
           direct=direct,cex.mn=2,dec.deg = F,cex=1.3,shore="nwatlHR")


####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS 
##### Now we can run some model diagnostics.
# Some quick diagnoistics, the maximum should be < 1.05
range(DD.out[["GBa"]]$summary[,8])
range(DD.out[["BBn"]]$summary[,8])

# Not sure what our minimum should be here, but using the Rhat + looking at the chains should indicate where there are problems...
range(DD.out[["GBa"]]$summary[,9])
range(DD.out[["BBn"]]$summary[,9])
      

# This is the worst of the paramter value R-squared so if this looks decent the others should, hopefully...
worst <- which(DD.out[["GBa"]]$summary[,8] == max(DD.out[["GBa"]]$summary[,8]))
plot(DD.out$GBa$sims.list[[names(worst)]][1:1000],type="l")
lines(DD.out$GBa$sims.list[[names(worst)]][1001:2000],col="blue")
# No autocorrelation in there...
acf(DD.out$GBa$sims.list[[names(worst)]][1:1000])
acf(DD.out$GBa$sims.list[[names(worst)]][1001:2000])
# Now do the same for Browns Bank North
worst <- which(DD.out[["BBn"]]$summary[,8] == max(DD.out[["BBn"]]$summary[,8]))
plot(DD.out$BBn$sims.list[[names(worst)]][1:1000],type="l")
lines(DD.out$BBn$sims.list[[names(worst)]][1001:2000],col="blue")
# No autocorrelation in there...
acf(DD.out$BBn$sims.list[[names(worst)]][1:1000])
acf(DD.out$BBn$sims.list[[names(worst)]][1001:2000])

# Here we pull together all of the chains and look to see that they are both well mixed and that
# there is no correlation.   This is a lot of plots!  
num.banks <- length(names(DD.out))
banks <-names(DD.out)
# Function to plot all the chains, this assumes we have 2 chains, if we had three this would need revised!
for(j in 1:num.banks)
  {
   # Get the bank pulled out and figure out how many parameters we have
    bnk <- banks[j]
    DD.plt <- DD.out[[bnk]]
    num.param <- length(names(DD.plt$sims.list))
    param.names <- names(DD.plt$sims.list)
    pdf(file=paste(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/Model_convergence.pdf",sep=""),onefile=T)
    # Since our first parameter is a hyperparameter (there isn't one for every year) this works, 
    #if this was a matrix we'd get an error here.
    len <- length(DD.plt$sims.list[[1]])
    for(i in 1:num.param)
      {
      
        # This pulls out all the plot for parameters with only one values
        if(is.vector(DD.plt$sims.list[[names(DD.plt$sims.list)[i]]])==T)
          {
           # if(i == 1)
            par(mfrow = c(3,1),mar=c(2,2,3,1))
            # plot the chains
            plot(DD.plt$sims.list[[param.names[i]]][1:(len/2)],type="l",col="darkgreen", main = paste(param.names[i],
                                                                                      "Chain"),xlab="",ylab="")
            lines(DD.plt$sims.list[[param.names[i]]][(len/2+1):len],col="blue")
            # And look for any signs of autocorrelation in the chains...
            acf(DD.plt$sims.list[[param.names[i]]][1:(len/2)],lag.max = 10,main = "ACF chain 1",xlab="",ylab="")
            acf(DD.plt$sims.list[[param.names[i]]][(len/2+1):len],lag.max = 10,main="ACF chain 2",xlab="",ylab="")
          } # end if(is.vector(DD.plt$sims.list[[names(DD.plt$sims.list)[1]]])==T)
        
        # This pulls out all the plots for parameters with multiple values (i.e. annual estimates)
        if(is.vector(DD.plt$sims.list[[names(DD.plt$sims.list)[i]]])==F)
          {
            num.reps <- ncol(DD.plt$sims.list[[names(DD.plt$sims.list)[i]]])
            rep.names <- paste(names(DD.plt$sims.list)[i],1:num.reps,sep="_")
            # Run this loop for each chain for these parameters.
            for(k in 1:num.reps)
              {
                par(mfrow = c(3,1),mar=c(2,2,3,1))
                # plot the chains
                plot(DD.plt$sims.list[[param.names[i]]][1:(len/2),k],type="l",col="darkgreen", 
                     main = paste(rep.names[k], "Chain"),xlab="",ylab="")
                lines(DD.plt$sims.list[[param.names[i]]][(len/2+1):len,k],col="blue")
                # And look for any signs of autocorrelation in the chains...
                acf(DD.plt$sims.list[[param.names[i]]][1:(len/2),k],lag.max = 10,main = "ACF chain 1",xlab="",ylab="")
                acf(DD.plt$sims.list[[param.names[i]]][(len/2+1):len,k],lag.max = 10,main="ACF chain 2",xlab="",ylab="")
              
              } # end for(k in 1:num.reps)
          } # end if(is.vector(DD.plt$sims.list[[names(DD.plt$sims.list)[i]]])==F)
    }  # end for(i in 1:num.param)
  dev.off()
  } # end for(j in 1:length(num.banks))

#Biomass at interim TAC (4000 t in 2015)
Bmass.current <- DD.out[[bnk]]$median$B[length(DD.out[[bnk]]$median$B)] #25760 - current year's median Biomass
# Does this make sense since we are modelling on survey year our biomass estimates don't need to add in 
#th projected catch do they??
TACI<-which(DD.out[[bnk]]$data$C.p == 3500 + proj.catch)
DD.out[[bnk]]$median$B.p[TACI] #22619.41 #predicted biomass for next year assuming intermin TAC

# Just curious, is there a correlation between the process residuals on BBn and GBa.
# There is a pretty strong lag -2 signal, interesting... Looks like BBn process
# residuals are doing the same as the GBa just a couple years later...
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

# Doesn't this seem crazy bizarre low for the fishery catchability?
DD.out[["GBa"]]$mean$qU
# Is this CPUE?
DD.out[["GBa"]]$mean$Ipred




# A neat plot to look at correlation among terms, of course problem is with all these parameters there's no way to check 
#them all...
betterPairs(DD.out$GBa$sims.list[[2]][,1:5])
#betterPairs(DD.out$GBa$sims.list$)
library(IDPmisc)
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="blue4", ...)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}

betterPairs <- function(YourData){
  return(pairs(YourData, lower.panel=function(...) {par(new=TRUE);ipanel.smooth(...)}, 
               diag.panel=panel.hist, upper.panel=panel.cor))
}

