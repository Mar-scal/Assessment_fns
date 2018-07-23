#################################################################### DK January 2016
## This file is used to investigate the impact of several model inputs on our results.  
## The file is broken down into a number of "Problems":
##  Problem 1:  What is the effect of varying the CPUE CV?  We have 3 measures of CV, the "correct" one which is calculated
##              from our jackknife function, this CV is very small and causes the model all sorts of grief.  There is the "SE"
##              which uses the standard error instead of the CV.  Finally there is the "CV X 50 (or elevated CV)" case which puts 
##              the CV on the same scale as what was done previously (where CV was sd/mean instead of se/mean)
##
## Problem 2:   In 2014 we used the calendar year instead of survey year survey data.  In this problem we compare the model output
##              from this with the model output of using survey year as we should have.
##
## Problem 3:   The CPUE time series is horribly confounded because of changes to vessels, processing abilities offshore, and the influence
##              of changes in fishery behavour due to these changes.  Could looking at the WF compentent of the fleet be a better
##              model of the CPUE time series over time rather than having it confounded with the FT fleet.
##
## Problem 4:   Here we look at the impact of fudging the mortality from the survey in the last two years to be
##              the median over the time series, what impact does this have on the natural mortality estimates?
##
## Problem 5:   The final issue I'll get to look into s what happens when we split GBa into a north and south model
##              
## Code to answer problem questions is embedded but will need uncommented.  For example
# to answer Problem 1, seach for "Problem 1" and uncomment all code related to that to make the models 
# run as they should.  Yes it's a pain but puts everything in one file so you aren't digging around everywhere for the files
# and it makes sense to me.... so get over it...
####################################################################
## GEORGES A AND BROWNS BANK NORTH.
## This gets the final model data together and runs the model.  Revised by DK in January of 2016.
###################################################################
# Clear out everything
rm(list=ls())
# re-load rprofile if you have one...
source(".Rprofile")
direct = "d:/r/"
yr = as.numeric(format(Sys.time(), "%Y")) -1
source(paste(direct,"Assessment_fns/Model/delayBUGS.r",sep=""))
source(paste(direct,"Assessment_fns/Model/post.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/exploit.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Model/fit.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/peR.r",sep=""))
source(paste(direct,"Assessment_fns/Model/projections.r",sep=""))
source(paste(direct,"Assessment_fns/Model/biomass.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Model/decision.r",sep=""))
source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
source(paste(direct,"Assessment_fns/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/fishery.dat.r",sep=""))
source(paste(direct,"Assessment_fns/Model/StephensFix.R", sep=""))
library ("R2WinBUGS")

#These are the model results, short cut to running the whole model again if you are satisfied with your results.
# Or you just want to look at what you have so far.
#load(paste(direct,(yr),"/Updates/GBa/Results/GBa_model_results.R",sep=""))
#############  Section 1  load the necessary data  #############  Section 1  load the necessary data  #############  Section 1

#for Problem 1 Problem 2 and Problem 4 we have this data already, we can simply use the survey results for the most part.
#load(file=paste(direct,"Data/Model/",(yr+1),"/Model_input.RData",sep=""))
# End Problem 1 Problem 2 and Problem 4 

# For Problem 3 and Problem 5 we need to generate some of the data ourselves.
######### Problem 3 -  uncomment these lines to replace the CPUE for the whole fishery with the CPUE for the WF fleet only.
# Question 3 we need to get the time series of the WF and FT fleets and see how these impact the model.
#logs_and_fish(loc="offshore",year = 1981:yr,un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
# If you get any NA's related warnings it may be something is being treated as a Factor in one of the two files.  
# This should combine without any warnings so don't ignore warnings here.
#fish.dat<-merge(new.log.dat,old.log.dat,all=T)
#fish.dat$ID<-1:nrow(fish.dat)
#fish.WF <- subset(fish.dat,fleet == "WF")
#cpue.WF <- fishery.dat(fish.WF,bk="GBa",yr = unique(mod.dat[["GBa"]]$year),method='jackknife',direct=direct,period = "survyr") 	
# In case years aren't lining up...
#mod.dat[["GBa"]]$cpue[mod.dat[["GBa"]]$year %in% cpue.WF$year] <- cpue.WF$cpue
#mod.dat[["GBa"]]$U.cv[mod.dat[["GBa"]]$year %in% cpue.WF$year] <- cpue.WF$cpue.se/cpue.WF$cpue
######## End Problem 3

######## Problem 5
# In question 5 we need to get the data from the model in which we split the survey into 2 bits, this is done
# From the script... Asssessment_fns/Model/Framework/Split_GBa_north_south.r we can load this
#load(file = paste(direct,"Data/Model/",(yr+1),"/Framework/GBa_north_south_results.Rdata",sep=""))
# The clappers are 0 in the south, but that breaks the model so toss those and replace with really low values...
#mod.dat$GBa.south$clappersR[mod.dat$GBa.south$clappersR ==0] <- min(mod.dat$GBa.south$clappersR[mod.dat$GBa.south$clappersR >0])
######## End Problem 5

#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model ###########
#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model ###########
#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model#############  Section 2 Model ###########

# Also bring in the reference point and TAC data. Make sure you update this csv each year with the current interim TAC and the
# final TAC from the previous year!!!
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
DD.out <- NULL
DD.out2 <- NULL
DD.out3<- NULL
bank <- unique(names(mod.dat))
num.banks <- length(bank)
D.tab <- NULL
D.tab.2m <- NULL
D.tab.3m <- NULL
# Depending on which bank you want to look at, for most cases I have focused on GBa, so set i =1 if you don't want to run the loop
i=1
for(i in 1:num.banks)
{
  bnk <- bank[i]
  
  DD.dat <- subset(mod.dat[[bank[i]]],year %in% 1986:max(mod.dat[[bank[i]]]$year))
  
  # Problem 2 Uncomment this line to run the incorrect calander year data
  #DD.dat<- read.csv(paste(direct,2015,"/Updates/",bnk,"/Input/",bnk,"ModelData.csv",sep=""))
  # Uncomment the following line to subset the fishery data to 2014.
  #DD.dat <- subset(DD.dat,year %in% c(1986:2014))
  
  # Problem 4 Uncomment this line to get the clapper abundance lowered to time series median levels 
  #DD.dat$clappers[DD.dat$year %in% 2014:2015] <- rep(median(DD.dat$clappers[DD.dat$year < 2014]),2)
  #DD.dat$clappersR[DD.dat$year %in% 2014:2015] <- rep(median(DD.dat$clappersR[DD.dat$year < 2014]),2)
  
  names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                      "w.bar","l.bar", "l.k",  "w.k","CF","clappers","clappersR","CS",  "RS","C","E","n.trips","U",
                      "U.var","U.se","LCI","UCI","U.cv", "g","g2","gR","gR2") 
  # Organize the data and set up the model priors/initialization data, then run the model.
  
  yrs<-min(DD.dat$year):max(DD.dat$year)
  NY<- length(yrs)
  
  DD.lst<-as.list(subset(DD.dat,year%in%yrs,c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers","clappersR")))

  #Problem 1 - Run this line if we want to use the se instead of the CV.
  #DD.lst<-as.list(subset(DD.dat,year%in%yrs,c("I","I.cv","IR","IR.cv","g","gR","C","U","U.se","N","NR","clappers","clappersR")))
  
  # Problem 1 or Problem 2 - This line is run when using correct survey data in problem 1, or for either scenario 
  # (se and elevated CV) in problem 2                          
  # Problem 1, Problem 2, Problem 3, Problem 4, Problem 5 all may need this next line uncommented.
  # If comparing CV*50 or SE this line needs to be run, this turns the SE into the CV or multiplies the CV by 50.
  ifelse(names(DD.lst)[9] == "U.se", names(DD.lst)[9] <- "U.cv", DD.lst$U.cv <- DD.lst$U.cv*50)
  
  
  
  # Get the number of years
  DD.lst$NY<- length(DD.lst$C)
 
  # Set up Priors for the variance terms.
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
  # n = 300,000 and burn = 200,000
  # Run the model now.
  DD.out[[bnk]]<-delayBUGS(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, yrs, n = 200000, 
                           burn =100000, thin = 100,
                           debug=F,add.parameters=c('Imed','Ipred','Irep','IRmed','IRpred','IRrep','sIresid',
                                                    'sIRresid','sPresid','Iresid','IRresid','Presid'))
  
  ## DK_note, THE LAST RUN WAS WF data with low CV, you need to RUN the file from here down!!!!
  
  #runTime <- Sys.time()-begTime
  #runTime #just for curiosity
  
  # Set the working directory to save the figures
  plotsGo <- paste(direct,(yr+1),"/Updates/Framework/Figures/",sep="")
  
  ###Plot model diagnostics############## 
  # posterior densities for model parameters.  DON"T SAVE THESE AS PDF's unless you know what you are doing as it could
  # overwright other data and make everyone sad.
  post.plt(DD.out[[bnk]],DDpriors,years=yrs, graphic='R',nr=3,nc=4,wd=15,multi=F,
           path=paste(plotsGo,"Calendar_year_vs_survey_year_2014/",sep=""))
  ##exploitaiton time series
  exploit.plt(DD.out[[bnk]], years=yrs, plt=c('f','m','mR'),graphic='R',
              path=paste(plotsGo,"Calendar_year_vs_survey_year_2014/",sep=""))
  # model biomass fit to survey
  fit.plt(DD.out[[bnk]], CI=T,graphic='R',CV=T,
          path=paste(plotsGo,"Calendar_year_vs_survey_year_2014/",sep=""))
  # diagnostic plot
  diag.plt(DD.out[[bnk]], graphic='R',
           path=paste(plotsGo,"Calendar_year_vs_survey_year_2014/",sep=""))
  
  #Prediction Evaluation using the predicted growth, DK hasn't tested as of Jan 2016 and causing errors so leave for the moment.
  #peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, min(yrs):(max(yrs)), pe=(max(yrs)-1):2000,n = 60000,
  #    burn = 30000, thin = 10, plot=0,g2=DD.dat$g2[],gR2=DD.dat$gR2[],lab="GBa")
    # Plot the prediction evaluation which uses the predicted growth
  #peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, min(yrs):(max(yrs)-1), pe=(max(yrs)-1):2001,run=F, 
  #    plot=3,proj.from="BUGS",graphic='pdf',lab=bnk,path=plotsGo)
  #Prediction Evaluation using the modelled growth.
  #peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, min(yrs):(max(yrs)), pe=(max(yrs)-2):2000,n = 60000, 
  #    burn = 30000, thin = 10, plot=0,lab=paste(bnk,"g1",sep = ""))
    # Plot the prediction evaluation
  #peR(paste(direct,"Assessment_fns/Model/DDwSE3",sep=""), DD.lst, DDpriors, min(yrs):(max(yrs)-2), pe=(max(yrs)-2):2001,run=F, 
  #    plot=3,proj.from="BUGS",graphic='pdf',lab=paste(bnk,"g1",sep = ""),path=plotsGo)
  

  # The catch since the survey for the most recent year is
  proj.catch <- proj.dat[[bnk]]$catch[proj.dat[[bnk]]$year == max(DD.dat$year)]
  # Get the low and upper boundaries for the decision table (this might be a silly way to do this...)
  
    # For Problem 1 Problem 2 Problem 3 and Problem 4 run these 3 lines
  # D_low <- subset(manage.dat,year==(yr+1) & bank == bnk)$D_tab_low
  # D_high <-  subset(manage.dat,year==(yr+1) & bank == bnk)$D_tab_high
  #step <- ifelse(bnk == "GBa", 500,50)
  
  # For Problem 5 you'll need to run the next 3 lines
  #D_low <- subset(manage.dat,year==(yr+1) & bank == "GBa")$D_tab_low
  #D_high <-  ifelse(bnk == "GBa.north",subset(manage.dat,year==(yr+1) & bank == "GBa")$D_tab_high,3000)
  # step <- ifelse(bnk == "GBa.north",500,50)
  
  
  # Get the projection scenarios of interest
  proj <- seq(D_low,D_high,step)+proj.catch
  
  
  # Now do the projections, the code can run using last year 
  DD.out[[bnk]]<- projections(DD.out[[bnk]],C.p=proj) # C.p = potential catches in decision table
  #testing out a new method for projecting using a 3-year mean of m rather than m from the current year. This was examined in 2014/15
  #because of very large increase in m from the model in that year
  # 2 year average mortality
  DD.out2[[bnk]]<-projections.2015(DD.out[[bnk]],C.p=proj,mort=2) # C.p = potential catches in decision table
  # 3 year average mortality
  DD.out3[[bnk]]<-projections.2015(DD.out[[bnk]],C.p=proj,mort=3) # C.p = potential catches in decision table
  
  
  # The interim TAC is
  TACi <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC
  
  # Problem 5 - The above TAC would be too high, so do the below (note this only works if it matches our steps)
  # so would have to be 500, 1000... etc... if step = 500
  # TACi <-  ifelse(bnk == "GBa.north",(TACi-500),500)
  
  
  # Note that these plots don't account for growth next year, so are somewhat misleading compared to the decision table
  # which does account for growth!!  This should be not too painful to fix I think?
  ### Note that from the 2015 SSR we have these definitely set at...
  #Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
  #The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
  bm.max <- ifelse(bnk == "GBa" || bnk== "GBa.north", 55000,25000)
  #source("fn/biomass.plt.r") #biomass time series with ref points
  biomass.plt(DD.out[[bnk]],years=yrs, graphic='R',TAC=TACi+proj.catch,path=plotsGo,refs = c("LRP","URP","zones"),URP =13284, LRP=7137,
              index=1:24,avg.line=median,Bymax=bm.max)
  #index=1:24 is the time perios (in years) used to establish USR and LRP (1986-2009), this does not change.
  biomass.plt(DD.out2[[bnk]],years=yrs, graphic='R',TAC=TACi+proj.catch,path=plotsGo,refs = c("LRP","URP"),URP =13284, LRP=7137,
              index=1:24,avg.line=median,Bymax=bm.max)
  biomass.plt(DD.out3[[bnk]],years=yrs, graphic='R',TAC=TACi+proj.catch,path=plotsGo,refs = c("LRP","URP"),URP =13284, LRP=7137,
              index=1:24,avg.line=median,Bymax=bm.max)
  
  # For Problem 5 I want to tweak the LRP and URP to more reasonable numbers, based on what we did for the whole bank this 
  # seems pretty reasonble.
  
  #URP <- 0.8*mean(DD.out3[[bnk]]$median$B[DD.dat$year %in% 1986:2009])
  #LRP <- 0.3*mean(DD.out3[[bnk]]$median$B[DD.dat$year %in% 1986:2009])
  #biomass.plt(DD.out3[[bnk]],years=yrs, graphic='R',TAC=TACi+proj.catch,path=plotsGo,refs = c("LRP","URP"),URP =URP, LRP=LRP,
  #            index=1:24,avg.line=median,Bymax=bm.max)
  
 
  ### Generate Decision Table ###
  ### Note that from the 2015 SSR we have these definitely set at...
  #Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
  #The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
  #source("fn/decision.r") #generates decision table
  if (bnk == "GBa") 
  {
    D.tab[[bnk]]<-decision(bnk,DD.out[[bnk]], mu=0.15,refs=c(13284,7137),post.survey.C=proj.catch)
    # The below write is for the decision table in 2015 if we had used 1 year mortality instead of 3 year mean
    #write.csv(D.tab[[bnk]],"d:/r/2016/updates/framework/Figures/Ginette_meeting/Decision1_GBa.csv",row.names=F)
    D.tab.2m[[bnk]]<-decision(bnk,DD.out2[[bnk]], mu=0.15,refs=c(13284,7137),post.survey.C=proj.catch)
    #write.csv(DtabGBa,paste0(plotsGo,"Decision_GBa_Meanm.csv"),row.names=F)
    D.tab.3m[[bnk]]<-decision(bnk,DD.out3[[bnk]], mu=0.15,refs=c(13284,7137),post.survey.C=proj.catch)
    #write.csv(DtabGBa,paste0(plotsGo,"Decision_GBa_2yrMeanm.csv"),row.names=F)
  } # END if(bnk == "GBa")
  if (bnk == "BBn") 
  {
    D.tab[[bnk]]<-decision(bnk,DD.out[[bnk]], mu=0.15,post.survey.C=proj.catch)
    #write.csv(DtabGBa,paste0(plotsGo,"Decision1_GBa.csv"),row.names=F)
    D.tab.2m[[bnk]]<-decision(bnk,DD.out2[[bnk]], mu=0.15,post.survey.C=proj.catch)
    #write.csv(DtabGBa,paste0(plotsGo,"Decision_GBa_Meanm.csv"),row.names=F)
    D.tab.3m[[bnk]]<-decision(bnk,DD.out3[[bnk]], mu=0.15,post.survey.C=proj.catch)
    #write.csv(DtabGBa,paste0(plotsGo,"Decision_GBa_2yrMeanm.csv"),row.names=F)
  } # END if(bnk == "GBa")
  
    
    # For problem 5 decision tables need to look like this...
    
    #  D.tab[[bnk]]<-decision("GBa.south",DD.out[[bnk]], mu=0.15,refs=c(URP,LRP),post.survey.C=proj.catch)
    #  #write.csv(DtabGBa,paste0(plotsGo,"Decision1_GBa.csv"),row.names=F)
    #  D.tab.2m[[bnk]]<-decision("GBa",DD.out2[[bnk]], mu=0.15,refs=c(URP,LRP),post.survey.C=proj.catch)
    #  #write.csv(DtabGBa,paste0(plotsGo,"Decision_GBa_Meanm.csv"),row.names=F)
    #  D.tab.3m[[bnk]]<-decision("GBa",DD.out3[[bnk]], mu=0.15,refs=c(URP,LRP),post.survey.C=proj.catch)
    #  #write.csv(DtabGBa,paste0(plotsGo,"Decision_GBa_2yrMeanm.csv"),row.names=F)
} # end for(i in 1:num.banks)

### DONE WITH MODEL RUNNING, NOW SAVE YOUR DATA BEFORE YOU REGRET IT!!



#####################  HERE IS A WACK OF CODE FOR SAVING ALL OF OUR DIFFERENT PROBLEMS
########  PROBLEM #1, the fake CV, what are implications of having wrong CV and what to do about that? #########

# Here the results are a saved for the model in which
# The CV increased by 50 times (matching closely the old CV calculations)
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/updated_survey_updated_fishery_CV_elevated_data.RData",sep=""))
# If wanting to look at results load these data
#load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/updated_survey_updated_fishery_CV_elevated_data.RData",sep=""))

# This save grabs the data for GBa with updated survey data and using the correct survey year fishery data with 
# The se is used instead of the CV
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/updated_survey_updated_fishery_se.RData",sep=""))
#load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/updated_survey_updated_fishery_se.RData",sep=""))

# This save grabs the data for GBa using the "correct" data all around, but only a short run (200,000) 
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/right_data_short_run.RData",sep=""))
#load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/right_data_short_run.RData",sep=""))

########  END PROBLEM #1, the fake CV, what are implications of having wrong CV and what to do about that? #########


########  PROBLEM #2, THE CALENDAR YEAR FRIG UP #################  PROBLEM #2, THE CALENDAR YEAR FRIG UP #########
#This save grabs the data for GBa with the updated survey data but using the calendar year fishery data with elevated CV. up to 2014
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-calendar_year.RData",sep=""))
#   load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-calendar_year.RData",sep=""))
# This save grabs the data for GBa with updated survey data and using the correct survey year fishery data with 
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-survey_year_low_CV.Rdata",sep=""))
#load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-survey_year_low_CV.Rdata",sep=""))

# This save grabs the data for GBa with updated survey data and using the correct survey year fishery data with 
# This uses CV * 50 for the CV (which closely matches the CV used in the calander year data).
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-survey_year_CVx50.Rdata",sep=""))
#load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-survey_year_CVx50.Rdata",sep=""))

########  END PROBLEM #2, END OF THE THE CALENDAR YEAR FRIG UP #################  PROBLEM #2, END OF THE CALENDAR YEAR FRIG UP #########




########  Problem #3, WF fishery ######  Problem #3, WF fishery ######  Problem #3, WF fishery ######  Problem #3, WF fishery
# This save grabs the data for GBa using the WF fleet data
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/Results_using_WF_fleet_low_CV.RData",sep=""))
# Same thing but downweighting the CPUE data...
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/Results_using_WF_fleet_CVx50.RData",sep=""))

########  END Problem #3, WF fishery ######  END Problem #3, WF fishery ######  END Problem #3, WF fishery ######  END Problem #3, WF fishery


###### Problem #4, elevated clapper numbers  ### Problem #4, elevated clapper numbers  ### Problem #4, elevated clapper numbers
# This save grabs the data for GBa with clapper numbers reset to the median of the time series.
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/Median_clapper_abundance.RData",sep=""))
###### End Problem #4, elevated clapper numbers  ### End Problem #4, elevated clapper numbers  ### End Problem #4, elevated clapper numbers


###### Problem #5, North vs. South  ###### Problem #5, North vs. South  ###### Problem #5, North vs. South 
# This save grabs the north and south data, if you ran this as a loop you'd have both in one object
# but I didn't and don't want to waste 30 minutes re-running the model so this is what I got!
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/GBa_South_model_results.RData",sep=""))
#save(DD.out,DD.out2,DD.out3,mod.dat,cpue.dat,bank,proj.dat,yr,D.tab,D.tab.2m,D.tab.3m,
#     file=paste(direct,"Data/Model/",(yr+1),"/Framework/GBa_North_model_results.RData",sep=""))
###### END Problem #5, North vs. South ###### END Problem #5, North vs. South ###### END Problem #5, North vs. South



#################  END SECTION 2 MODELS#################  END SECTION 2 MODELS#################  END SECTION 2 MODELS ##################






################### Section 3 ################### Section 3 ################### Section 3 ################### Section 3 ###################
### Section 3.  Comparing the different "problems" to see the effect each has...
### OK. so now lets look at some of these results, first comparing how the change in CV impacts our results.
### Three models to put together here...


################# PROBLEM #1, WHAT ARE THE EFFECTS OF USING DIFFERENT CV'S #####################################
# Bring in the model results with the CV x 50 model.
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
DD.dat <- subset(mod.dat[["GBa"]],year %in% 1986:max(mod.dat[["GBa"]]$year))
TACi <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/updated_survey_updated_fishery_CV_elevated_data.RData",sep=""))
# I want to grab this decision table as it's kinda what I expect the final decision table will look like...
write.csv(D.tab.3m$GBa, file = paste(plotsGo,"/CV_effect/Decision_table_CVx50.csv",sep=""))
# Grab the biomass
biomass.res <- cbind(DD.dat$year,DD.out$GBa$median$B)
# Get the decision table for 3500 tonnes (the 2016 interim catch)
d.tab3m.3500 <- D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==TACi,]

# Now bring in the se data and results.
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/updated_survey_updated_fishery_se.RData",sep=""))
# add the se biomass
biomass.res <- cbind(biomass.res,DD.out$GBa$median$B)
# Get the decision table for 3500 tonnes (the 2016 interim catch)
d.tab3m.3500 <- rbind(d.tab3m.3500,D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==TACi,])

# Finally the "correct" data and results
# I could use either of these here, but for the moment we have the full model so let's use that!
load(file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))
#load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/right_data_short_run.RData",sep=""))
# add the "correct" biomass
biomass.res <- cbind(biomass.res,DD.out$GBa$median$B)
# Get the decision table for 3500 tonnes (the 2016 interim catch)
d.tab3m.3500 <- rbind(d.tab3m.3500,D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==TACi,])
# get the right names of the biomass time series
colnames(biomass.res) <- c("year","BM_elevated_CV","BM_SE","BM_correct")
biomass.res <- as.data.frame(biomass.res)
# Get the right row-names for the decision table output
rownames(d.tab3m.3500) <- c("BM_elevated_CV","BM_SE","BM_correct")

## Make a plot of the biomass trends for each scenario
png(filename = paste(plotsGo,"/CV_effect/BM_ts_various_CPUE_variance_models.png",sep=""),
    width=11,height=8.5,units="in",res=200)
#windows(11,8.5)
with(biomass.res,plot(year,BM_SE,type="o",col="blue",pch=16,ylim=c(0,50000)))
with(biomass.res,lines(year,BM_elevated_CV,type="o",pch=16,col="green"))
with(biomass.res,lines(year,BM_correct,type="o",pch=16,col="black"))
legend("topleft",c("SE model","CV x 50 model","CV correct model"),lty=1,pch=21,bty="n",
       pt.bg = c("blue","green","black"),col = c("blue","green","black"),cex=2)
dev.off()

# This is the figure comparing the TAC with the Survey year catch, noting how they differ, which is fine!
par(mar=c(4,6,1,1))
plot(DD.dat$catch[DD.dat$year %in% 1995:2014]~DD.dat$year[DD.dat$year %in% 1995:2014],type="h",lwd=15,xlab="",ylim=c(0,8000),las=1,ylab="",bty="L",col="grey30")
lines(subset(manage.dat,bank=="GBa" & year %in% 1995:2014)$TAC~DD.dat$year[DD.dat$year %in% 1995:2014],col="blue",lwd=2)
mtext(side=2,"Catch (tonnes)",line=4)
################# END OF PROBLEM 1 LOOKING AT EFFECT OF USING DIFFERENT CV'S #####################################



################ PROBLEM 2, WHAT IS THE EFFECT OF USING THE CALENDAR YEAR DATA INSTEAD OF THE SURVEY YEAR DATA
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-calendar_year.RData",sep=""))
# Now bring in the se data and results.
manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# Note this is the survey year DD.dat (fine for the below purposes, shouldn't need the calendar year data here...)
DD.dat <- subset(mod.dat[["GBa"]],year %in% 1986:2014)
TACi <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-survey_year_low_CV.Rdata",sep=""))
biomass.res <- cbind(DD.dat$year,DD.out$GBa$median$B)
# Get the decision table for 4000 tonnes (the 2015 TAC)
d.tab3m.4000 <- D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==as.character(TACi),]
which(D.tab.3m$GBa$Catch == 4000)
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-survey_year_CVx50.Rdata",sep=""))

# add the se biomass
biomass.res <- cbind(biomass.res,DD.out$GBa$median$B)
# Get the decision table for 4000 tonnes (the 2015 TAC)
d.tab3m.4000 <- rbind(d.tab3m.4000,D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==as.character(TACi),])

load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/Calendar_year_problem-calendar_year.RData",sep=""))
# add the "correct" biomass
biomass.res <- cbind(biomass.res,DD.out$GBa$median$B)
# Get the decision table for 4000 tonnes (the 2015 TAC)
d.tab3m.4000 <- rbind(d.tab3m.4000,D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==as.character(TACi),])
# get the right names of the biomass time series
colnames(biomass.res) <- c("year","Survey_year_low_CV","Survey_year_CVx50","Calendar_year")
biomass.res <- as.data.frame(biomass.res)
# Get the right row-names for the decision table output
rownames(d.tab3m.4000) <- c("Survey_year_low_CV","Survey_year_CVx50","Calendar_year")
plotsGo <- paste(direct,(yr+1),"/Updates/Framework/Figures/",sep="")
png(filename = paste(plotsGo,"/Calendar_year_vs_survey_year_2014/BM_ts_various_CPUE_variance_models.png",sep=""),
    width=11,height=8.5,units="in",res=200)
#windows(11,8.5)
#with(biomass.res,plot(year,Survey_year_low_CV,type="o",col="slateblue1",pch=16,ylim=c(0,45000)))
with(biomass.res,plot(year,Survey_year_CVx50,type="o",pch=16,col="slateblue1",ylim=c(0,40000),bty="L"))
with(biomass.res,lines(year,Calendar_year,type="o",pch=16,col="black"))
legend("topleft",c("Survey_year","Calendar_year"),lty=1,pch=21,bty="n",
       pt.bg = c("slateblue1","black"),col = c("slateblue1","black"),cex=2)
dev.off()




################ END PROBLEM 2, WHAT IS THE EFFECT OF USING THE CALENDAR YEAR DATA INSTEAD OF THE SURVEY YEAR DATA




################ PROBLEM 3 - The effect of using CPUE of wet fishery instead of complete CPUE data...
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/Results_using_WF_fleet_CVx50.RData",sep=""))
# Note this is the survey year DD.dat (fine for the below purposes, shouldn't need the calendar year data here...)
DD.dat <- subset(mod.dat[["GBa"]],year %in% 1986:2015)
TACi <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC
biomass.res <- cbind(DD.dat$year,DD.out$GBa$median$B)
# Get the decision table for 3500 tonnes (the 2016 TAC)
d.tab3m.3500 <- D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==as.character(TACi),]
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/updated_survey_updated_fishery_CV_elevated_data.RData",sep=""))
# Grab the biomass
biomass.res <- cbind(biomass.res,DD.out$GBa$median$B)
# Get the decision table for 3500 tonnes (the 2016 interim catch)
d.tab3m.3500 <- rbind(d.tab3m.3500,D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==TACi,])
# get the right names of the biomass time series
colnames(biomass.res) <- c("year","Wet_Fleet","Full_fleet")
biomass.res <- as.data.frame(biomass.res)
# Get the right row-names for the decision table output
rownames(d.tab3m.3500) <- c("Wet_Fleet","Full_fleet")

png(filename = paste(plotsGo,"/WF_cpue/BM_ts_various_CPUE_variance_models.png",sep=""),
    width=11,height=8.5,units="in",res=200)
#windows(11,8.5)
with(biomass.res,plot(year,Wet_Fleet,type="o",col="slateblue1",pch=16,ylim=c(0,45000)))
with(biomass.res,lines(year,Full_fleet,type="o",pch=16,col="green"))
legend("topleft",c("WF_fleet","Full_fleet"),lty=1,pch=21,bty="n",
       pt.bg = c("slateblue1","green"),col = c("slateblue1","green"),cex=2)
dev.off()
################ END PROBLEM 3 


################ PROBLEM 4 - Clapper effect
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/Median_clapper_abundance.RData",sep=""))
# Note this is the survey year DD.dat (fine for the below purposes, shouldn't need the calendar year data here...)
DD.dat <- subset(mod.dat[["GBa"]],year %in% 1986:2015)
TACi <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC
biomass.res <- cbind(DD.dat$year,DD.out$GBa$median$B)
# Get the decision table for 3500 tonnes (the 2016 TAC)
d.tab3m.3500 <- D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==as.character(TACi),]
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/updated_survey_updated_fishery_CV_elevated_data.RData",sep=""))
# Grab the biomass
biomass.res <- cbind(biomass.res,DD.out$GBa$median$B)
# Get the decision table for 3500 tonnes (the 2016 interim catch)
d.tab3m.3500 <- rbind(d.tab3m.3500,D.tab.3m[["GBa"]][D.tab.3m$GBa$Catch==TACi,])
# get the right names of the biomass time series
colnames(biomass.res) <- c("year","Low_mort","Survey_mort")
biomass.res <- as.data.frame(biomass.res)
# Get the right row-names for the decision table output
rownames(d.tab3m.3500) <- c("Low_mort","Survey_mort")

png(filename = paste(plotsGo,"/Clappers/BM_ts_various_CPUE_variance_models.png",sep=""),
    width=11,height=8.5,units="in",res=200)
#windows(11,8.5)
with(biomass.res,plot(year,Low_mort,type="o",col="slateblue1",pch=16,ylim=c(0,45000)))
with(biomass.res,lines(year,Survey_mort,type="o",pch=16,col="green"))
legend("topleft",c("Low_mort","Survey_mort"),lty=1,pch=21,bty="n",
       pt.bg = c("slateblue1","green"),col = c("slateblue1","green"),cex=2)
dev.off()
################ END PROBLEM 4 - Clapper effect




### Problem 5  - South vs. North

# Make a plot of the survey estimates of south and north along with biomass, I think south and north split result in more overall biomass
# being estimated on the bank, that's odd??

load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/GBa_South_model_results.RData",sep=""))
plotsGo <- paste(direct,(yr+1),"/Updates/Framework/Figures/",sep="")
south.mort <- exp(-DD.out$GBa.south$median$m)
DD.dat <- subset(mod.dat[["GBa.south"]],year %in% 1986:2015)
TACi <- 500
biomass.res <- cbind(DD.dat$year,DD.out$GBa$median$B)
growth <- cbind(DD.dat$year,DD.dat$g)
# Get the decision table for 300-1500 tonnes (my pre 2016 TAC)
d.tab3m.south <- D.tab.3m[["GBa.south"]][D.tab.3m$GBa.south$Catch%in%seq(300,1500,by=100),]
write.csv(d.tab3m.south,paste(plotsGo,"North_vs_south/South_decision_table.csv",sep=""))
load(file=paste(direct,"Data/Model/",(yr+1),"/Framework/GBa_North_model_results.RData",sep=""))
north.mort <- exp(-DD.out$GBa.north$median$m)
DD.dat <- subset(mod.dat[["GBa.north"]],year %in% 1986:2015)
TACi <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC-500
biomass.res <- cbind(biomass.res,DD.out$GBa.north$median$B)
# get the right names of the biomass time series
colnames(biomass.res) <- c("year","South","North")
biomass.res <- as.data.frame(biomass.res)
d.tab3m.north <- D.tab.3m[["GBa.north"]][D.tab.3m$GBa.north$Catch %in% as.character(seq(1500,6500,by=500)),]
write.csv(d.tab3m.north,paste(plotsGo,"North_vs_south/North_decision_table.csv",sep=""))

morts <- data.frame(year = DD.dat$year, South = south.mort,North = north.mort)

png(filename = paste(plotsGo,"/North_vs_south/FR_mortality_and_ccf.png",sep=""),
    width=11,height=8.5,units="in",res=200)
#windows(11,8.5)
par(mfrow=c(1,2))
with(morts,plot(year,South,type="o",col="slateblue1",pch=16,ylim=c(0.4,1)))
with(morts,lines(year,North,type="o",pch=16,col="green"))
legend("topleft",c("South","North"),lty=1,pch=21,bty="n",
       pt.bg = c("slateblue1","green"),col = c("slateblue1","green"),cex=2)
with(morts,ccf(South,North))
dev.off()

### Problem 5  - South vs. North
