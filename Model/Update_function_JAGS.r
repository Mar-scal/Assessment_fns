####################################################################
## Create the update for Georges A and Browns Bank North. This function replaces the script "Update.r" 
## All of the information necessary to create the update is now housed in this function.
###################################################################
# Update history
# January 2016 - Revised by DK 
# April 2016, "Update" script has been overhauled and converted to a function called "Update_function_JAGS"
# May 16, 2016, updated to include options to allow different databases/usernames in the call to the fishery data.
# Sept 20, 2016, updated to allow for different loader files to be called in from the output of Survey_Summary_data.r. Removed spatial figures
# July 6, 2017:  Minor update to make sure the DD.dat function is selecting the correct data (I broke it when adding the SH bin data...)
# January, 2018: Tidied up script so that if we have the survey results for both banks we will preprocess both banks, some other edits to 
#                make the prediction evaluations and running the model with both banks more stable. 
#                removed the option for calulcating diagnostics (make.diag), you now always make them.
#                removed over-arching make.figs option, you can use the switchs for making the diagnostic and update figures to directly control this
#                Also removed option for the prediction evaluation figures, if you run the pred-eval model you make these figures automatically.
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
# logs_and_fishery_data.r
# fishery.dat.r
# projections.r
# decision.r
# post.plt.R
# exploit.plt.r
# fit.plt.R
# diag.plt.R
# peR_jags.r
# biomass.plt.r
# ScallopMap.r
# contour.gen.r 
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
# 4:  bank:                     The bank to run.  Default is both banks c("GBa","BBn"), but specifying either works as well.
# 5:  use.final                 If run.mod = F do you want to use the "final results" for figures or use the temporary results. 
#                               (T/F) default = F which loads Model_testing_results.RData.  If set to T it will load Final_model_results.RData 
# 6:  fig:                      Print to 'pdf' or to screen.  default="screen":

###########  Output options.  These options are specified to determine which bit of the function you want to run
###########  The default setting will process all the data, run the models, make the diagnostics, do the prediction evaluation,and make all figures
# 7:  preprocessed:             If you have already run Section 1 of this script you can skip runing that section each time. (T/F) default = F
# 8:  run.mod = T:              Run the model?  If a 'final model' has been saved you can skip re-running the model.  T/F, default = T
#10:  make.update.figs:         Do you want to make the figures used in the Update?  T/F, default = T,
#12:  make.diag.figs:           Do you want to make the diagnositic figures.  T/F, default = T.  Make sure make.figs=T
#13:  run.pred.eval.model:      For the prediction evalutions do you need to (re)run the models.(T/F) Default = T (these take a while if you've 
#                               run them once the results are saved so only set to T if needed a fresh run.  Figures will be produced whenever this is run
########################################################################################################################

###########  Model options.  By and large these options will only be used if run.mod=T

#15:  nchains:                  Number of chains to run.  Default = 8.  When running in parallel each chain gets it's own thread.
#                               The best way to get more saved replicates if the model has converged is to increase the number of chains run.
#16:  niter:                    Number of iterations to run.  Default = 175000
#17:  nburn:                    Number of initial iterations to ignore.  Default = 50000
#18:  nthin:                    Thinning rate of iterations.  Default = 20
#19:  final.run:                If you are happy with testing results run the model as a final model.  T/F, default = F.  If true 
#                               this saves an R workspace called Final_model_results.RData, if False it saves to Model_testing_results.RData
#20:  para:                     Run in parallel using jags.parallel? T/F, default =T. Number of processors = nchains
#21:  jags.model:               Get the model to use (same model used in prediction evaluations).  By deFault it looks the the folder
#                               "direct"/Assessment_fns/Model/DDwSE3_jags.bug where direct was specified above.
#22:  parallel:                 Do you want to run JAGS in parallel.  (T/F), F will run JAGS but just using one core.
#23:  seed:                     If running JAGS in parallel you can set a "seed" so that your model results are reproducable.  Default = 123
#24:  parameters:               Model parameters to output.  Default = NULL which will produce all of the priors + the following parameters
#                               'K','P','B','R','mu','Imed','Ipred','Irep', 'IRmed','IRpred',
#                               'IRrep','sIresid','sIRresid','sPresid','Iresid','IRresid','Presid'
#25  export.tables:             Export the Decision tables, should only be done when satisified with results.  T/F, default = F
#######################################################################################################################################

###########  Prediction Evaluation options.  To run the prediction evaluation model or make the figures make sure that make.figs=T.  

#26:  pred.eval.fig.type:       If making the prediction evalulation figures which figure do you want. Default ="box" (options "ts" or "ts_all")
#27:  pe.years:                 The years to run the prediction evaluation plots. Default = NULL which selects the current year back to 2000.
#28:  pe.iter:                  The number of iterations to run the prediction evaluation runs. Default = NULL which uses the nchains for the model
#29:  pe.burn:                  The number of iterations to drop from prediction evaluation runs. Default = NULL which uses the model nchains
#30:  pe.thin:                  The thinning rate for the prediction evaluation runs. Default = NULL which uses the nchains use for the model
#31:  pe.chains:                The number of chains to run for the prediction evaluation runs. Default = NULL which uses the model nchains

### May 16, 2016 added options for calling the database which I should have had in before...
#32 db.con:   The database to connect to.  Default ="ptran",
#33 un:       Your username to connect to SQL database.  Default = un.ID
#34 pw:       Your password to connect to SQL database.  Default = pwd.ID
###############################################################################################################

update_JAGS <- function(direct = "Y:/Offshore scallop/Assessment/", yr = as.numeric(format(Sys.time(), "%Y"))-1 , strt.mod.yr = 1986,
                   bank = c("GBa","BBn") , use.final = F,fig="screen",
                   # The output options (note that export tables only works if run.mod=T)
                   preprocessed = F, run.mod = T, make.update.figs=T,make.diag.figs = T,
                   run.pred.eval.model=T,export.tables = F,
                   # The main model options, these are only used if run.mod = T. (Tho the options starting with "n" could be sent to 
                   # be used with the prediction evaluation model runs if the "pe." options are not specified and run.pre.eval.model=T)
                   nchains = 8,niter = 175000, nburn = 100000, nthin = 20,final.run = F,parallel = T,
                   jags.model = "Assessment_fns/Model/DDwSE3_jags.bug",seed = 123,parameters = NULL,
                   #Prediction evalulation options (only used when make.figs =T and at least one of make.pred.eval.figs
                   # and run.pred.eval.model = T)
                   pred.eval.fig.type = "box",pe.years = NULL, pe.iter = NULL,pe.burn= NULL,pe.thin = NULL,pe.chains = NULL ,
                   un=NULL,pw=NULL,db.con="ptran"
                  )
{
  
# Load in the functions needed for this function to run.
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep="")) #logs_and_fish is function call
source(paste(direct,"Assessment_fns/Fishery/fishery.dat.r",sep=""))  
source(paste(direct,"Assessment_fns/Model/projections.r",sep=""))
source(paste(direct,"Assessment_fns/Model/decision.r",sep=""))
source(paste(direct,"Assessment_fns/Model/post.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/exploit.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Model/fit.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))
source(paste(direct,"Assessment_fns/Model/prediction_evaluation_function.r",sep="")) #The function to run the prediction evaluations
source(paste(direct,"Assessment_fns/Model/prediction_evaluation_figure.r",sep="")) # The function to make the plots
source(paste(direct,"Assessment_fns/Model/biomass.plt.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) 
# The necesary library
require(R2jags) || stop("You need the R2jags package installed or this ain't gonna work")
require(maptools)  || stop("Maptools, MAPtools, MAPTOOLS, install this package, please, now, HURRY!!!!")
require(sp)  || stop("You shall not pass until you install the *sp* package... you've been warned...")
  
#############  Section 1  Compile the data for the banks ######  Section 1  Compile the data for the banks################## 
#############  Section 1  Compile the data for the banks ######  Section 1  Compile the data for the banks################## 
#############  Section 1  Compile the data for the banks ######  Section 1  Compile the data for the banks################## 
# If you have already run section 1 no need to do it again set preprocessed = T

if(preprocessed==F)
  {
  direct.real <- direct  
  # Bring in the survey results for the current year, this also includes the fishery data..
    # If we have run the whole survey we can pull in from this file, if not we will have to pull them in below (in the loop)
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))==T)
    {
      # If we have all survey results we preprocess both banks...
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
    } # end if(file.exists(paste(direct,"Data/... == T
  
    # If we haven't created this file then there are a couple of places to look for the data... Note that we need to do
    # this up here to avoid overwriting the logs/fishery data...
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))==F)
    {
      # If looking at BBn the data should be housed in this file
      if(bank == "BBn")
      { 
        # If we have this file of spring data load it...
        #if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.RData"))==T)
        #{
          load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.RData",sep="")) 
        # If we don't load this....
        
      } # if(bank[i] == "BBn")
      
      # If looking at GBa the data should be housed in this file
      if(bank == "GBa")
      { 
        load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_summer_results.Rdata",sep="")) 
      } # if(bank[i] == "GBa")
      
      # If we want to run both banks I want to trip out an error message here saying you need to create the Survey_all_results.Rdata file
      # if you want to run the both banks at once.  I could rig something up to work but it gets very complicated so seems easier
      # to be lazy and make them make this file....
      if(length(bank) > 1) 
      {
        stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_all_results.Rdata' gets created, Thanks eh!!")
      } # if(length(bank) > 1) 
        
    }# end if(file.exists(paste(direct,"Data/... == F
  
  direct <- direct.real
  
    # Now bring in the latest fishery data
    logs_and_fish(loc="offshore",year = 1981:yr,un=un,pw=pwd,db.con=db.con,direct.off=direct)
    # If you get any NA's related warnings it may be something is being treated as a Factor in one of the two files.  
    # This should combine without any warnings so don't ignore warnings here.
    dat.fish<-merge(new.log.dat,old.log.dat,all=T)
    dat.fish$ID<-1:nrow(dat.fish)
    
    #Read1 Bring in the VonB model parameters
    cat("We read in the von B growth parameters from the file .../Data/Ageing/Von_B_growth_parameters.csv \n")
    vonB <- read.csv(paste(direct,"Data/Ageing/Von_B_growth_parameters.csv",sep=""))
    
    # Run this for one or both banks
    mod.dat <- NULL
    cpue.dat <- NULL
    proj.dat <- NULL
    # Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
    for(i in 1:length(bank))
    {
      # If we are running a sub-area we need to make sure we have the correct bank to pull the data from
      master.bank <-ifelse(grepl(pattern="GBa",x=bank[i])==T , "GBa","BBn")
      years <- min(survey.obj[[bank[i]]][[1]]$year):max(survey.obj[[bank[i]]][[1]]$year)
     
      # First off we subset the data to the area of interest using the survey boundary polygon, only do this for the sub-areas though
      if(!bank[i] %in% c("GBa","BBn"))
      {
        bound.surv.sp <- PolySet2SpatialPolygons(bound.surv.poly[[bank[i]]])
        # Get to get rid of all the crap spatial data in here.
        fish.tmp <- dat.fish[dat.fish$bank == master.bank  & !is.na(dat.fish$bank) & dat.fish$lon < 0 & dat.fish$lat > 0 ,]
        coordinates(fish.tmp) <- ~ lon + lat
        proj4string(fish.tmp) <- proj4string(bound.surv.sp)
        fish.pts <- over(fish.tmp,bound.surv.sp)
        fish.dat <- cbind(fish.tmp@data,fish.tmp@coords)
        # Just extract the fish data from the spatial object, don't need the spatial GIS component from this
        fish.dat <- fish.dat[which(fish.pts ==1),]
      } # end if(!bank[i] %in% c("GBa","BBn"))
      
      # If we're just running the regular old GBa/BBn banks, then no need to do the above, but we need to create fish.dat
      if(bank[i] %in% c("GBa", "BBn")) fish.dat <- dat.fish[dat.fish$bank == master.bank  & !is.na(dat.fish$bank) & dat.fish$lon < 0 & dat.fish$lat > 0 ,]
      
      # Bring in the vonB parameters..
      vonB.par <-vonB[vonB$Bank == master.bank,]
      # Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
      cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk=master.bank,yr=(min(years)-1):max(years),method='jackknife',
                                         direct=direct,period = "survyr") 	
      
      # Now on Browns North the survey usually happens in June so the projection is actually different
      # But in 2015 the survey was messed so the above is the solution used for 2015, 
      #for all other years we need to do this for Browns Bank North
      # It really makes very little difference which way this is done as the catch in June-August
      # has averaged around just 40 tonnes since about 1996.
      if(yr != 2015 &&  master.bank== "BBn") cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk=master.bank,yr=(min(years)-1):max(years),surv='May',
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
      if(yr != 2015 &&  master.bank == "BBn") proj.sub <- subset(fish.dat,year %in% years & months(as.Date(fish.dat$date)) 
                                                                 %in% c("June","July","August","September","October","November","December"))
      # Now calculate the fishery statistics for the projection period
      proj.dat[[bank[i]]] <- fishery.dat(proj.sub,bk=master.bank,yr=(min(years)-1):max(years),method='jackknife',
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
      # Using this years average shell height we can find the exptected shell height for the scallops in the next year
      # ht = (Linf * (1-exp(-K)) + exp(-K) * height(last year))
      # laa.t is the projected size of the current years scallops into next year.
      laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat[[bank[i]]]$l.bar
      # The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
      # the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
      # This gets us the predicted weight of the current crop of scallops next year based on next years CF * laa.t^3
      # Of course we don't have next years condition thus th last condition is simply repeated
      # waa.t is using the condition from next year and the growth from next year to get next years weight
      waa.t <- c(mod.dat[[bank[i]]]$CF[-1],mod.dat[[bank[i]]]$CF[nrow(mod.dat[[bank[i]]])])*(laa.t/100)^3
      # Here we use the current condition factor to calculate the weight next year (since we use laa.t)
      # That's really the only difference between waa.t and waa.t2, waa.t uses next years condition to project growth
      # what waa.t2 uses the current condition to project growth.  So that's really what we are comparing here with these
      # two growth metrics isn't it, this is really just comparing impact of using current vs. future condition factor on our growth estimates.
      
      waa.t2 <- mod.dat[[bank[i]]]$CF*(laa.t/100)^3
      # Now the growth, expected and realized.
      mod.dat[[bank[i]]]$g <- waa.t/waa.tm1
      # This is using the actual condition factor and growing the scallops by laa.t
      mod.dat[[bank[i]]]$g2 <- waa.t2/waa.tm1
      
      # same thing here but for the recruits
      waa.tm1 <- mod.dat[[bank[i]]]$CF*(mod.dat[[bank[i]]]$l.k/100)^3
      laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat[[bank[i]]]$l.k
      waa.t <- c(mod.dat[[bank[i]]]$CF[-1],mod.dat[[bank[i]]]$CF[nrow(mod.dat[[bank[i]]])])*(laa.t/100)^3
      waa.t2 <- mod.dat[[bank[i]]]$CF*(laa.t/100)^3
      mod.dat[[bank[i]]]$gR <- waa.t/waa.tm1
      mod.dat[[bank[i]]]$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
      
    } # end for(i in 1:length(bank))

    # Save the results so you don't have to do section 1 over and over.
    save(mod.dat,cpue.dat,proj.dat,file=paste(direct,"Data/Model/",(yr+1),"/Model_input.RData",sep=""))
    print("done pre-processing")
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
    mod.out <- NULL
  } # end if(run.mod== T)


# Get the number of banks we are running across.

num.banks <- length(bank)

for(j in 1:num.banks)
{
  # pick the bank
  bnk = bank[j]
  # If we are running a sub-area we need to make sure we have the correct bank to pull the data from.
  master.bank <-ifelse(grepl("GBa",bank[j])==T , "GBa","BBn")
  # We need to create the working directories needed to save the results...
  # Get the plot directory
  plotsGo <- paste(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/",sep="")
  # If the above directory does not exist then we need to create it and we will also need to create a similar data directory
  # Based on the assumption that directory doesn't exist either.
  if(dir.exists(plotsGo)==F)
  {
    # This enables us to create the base specified directory on up...
    if(dir.exists(direct) ==F) dir.create(direct)
    if(dir.exists(paste0(direct,(yr+1))) ==F) dir.create(paste0(direct,(yr+1)))
    if(dir.exists(paste0(direct,(yr+1),"/Updates")) ==F) dir.create(paste0(direct,(yr+1),"/Updates"))
    if(dir.exists(paste0(direct,(yr+1),"/Updates/",bnk)) ==F) dir.create(paste0(direct,(yr+1),"/Updates/",bnk))
    #dir.create(paste0(direct,(yr+1),"/Updates/Figures_and_tables/"))
    dir.create(paste0(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/"))
    # Similarly I need to make sure we have the data directories
    if(dir.exists(paste0(direct,"Data")) ==F) dir.create(paste0(direct,"Data"))
    if(dir.exists(paste0(direct,"Data/Model")) ==F) dir.create(paste0(direct,"Data/Model"))
    if(dir.exists(paste0(direct,"Data/Model/",(yr+1))) ==F) dir.create(paste0(direct,"Data/Model/",(yr+1)))
    if(dir.exists(paste0(direct,"Data/Model/",(yr+1),"/",bnk)) ==F) dir.create(paste0(direct,"Data/Model/",(yr+1),"/",bnk))
    dir.create(paste0(direct,"Data/Model/",(yr+1),"/",bnk,"/Results"))
  } # end if(dir.exists(plot.dir)==F)
  
  
  #Read2 Get the managment data, note this file needs updated annually! 
  cat(paste("*NOTE #1* Please ensure the file Ref_pts_and_tac.csv is updated with the interim TAC (from the Interim Fishing Plan sent after OSAC) or you won't have information for the", 
        yr+1,"prediction, wouldn't you feel silly without that.\n",sep = " "))
  manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
# If you need to run the model giver.
  if(run.mod == T)
  {
    # There is a possiblitly that some of the sub-area values will be 0's, for those cases I'm going to use the value from the 
    # following year as the value and spit out a warning
    if(any(mod.dat[[bnk]] == 0))
    {
      # Here are the columns that have 0's in them, there must be a better way to do this, but we also want to
      # go from most recent to oldest just in case we have back to back years with 0's...
      rows.of.interest.survey <- unique(unlist(unique(apply(mod.dat[[bnk]],2,FUN = function(x) which(x == 0)))))
      rows.of.interest.catch <- unique(unlist(unique(apply(mod.dat[[bnk]],2,FUN = function(x) which(is.na(x))))))
      rows.of.interest <- unique(rev(sort(c(rows.of.interest.catch,rows.of.interest.survey))))
      for(p in 1:length(rows.of.interest)) 
      {
        tmp <- mod.dat[[bnk]][rows.of.interest[p],]
        colms <- c(which(tmp==0),which(is.na(tmp)))
        mod.dat[[bnk]][rows.of.interest[p],colms] <- mod.dat[[bnk]][(rows.of.interest[p]+1),colms]
      # Also can have trouble if only 1 fishing trip...
      } # end for(p in 1:length(rows.of.interst)) 
  cat(paste("WHOA, check this out, for",bnk," we had to replace some years that had 0's in them with non-zero values, check your data and the script
            for details if this is news to you!! \n"))    
  } # end if(any(mod.dat[[bnk]] == 0)
    
    # Grab the data, start model at either 1986 (note that BBn data starts in 1991 so anything earlier will default to 1991)
    DD.dat <- subset(mod.dat[[bnk]],year %in% strt.mod.yr:max(mod.dat[[bnk]]$year),
                     select = c("year","n.x","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                                "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS",  "RS","catch","effort","n.y","cpue",
                                "cpue.var","cpue.se","LCI","UCI","U.cv", "g","g2","gR","gR2"))
    
    names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                        "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS",  "RS","C","E","n.trips","U",
                        "U.var","U.se","LCI","UCI","U.cv", "g","g2","gR","gR2") 
    # Organize the data and set up the model priors/initialization data, then run the model.
    yrs[[bnk]]<-min(DD.dat$year):max(DD.dat$year)
    NY<- length(yrs[[bnk]])
    DD.lst[[bnk]]<-as.list(subset(DD.dat,year %in% yrs[[bnk]],c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers",
                                                "clappersR","g2","gR2")))
    # DK NOTE: Downweight the CV for the CPUE data. This is done to be consistent with CV used
    # Previously in the model assessments. This has been flagged as an action item to investigate 
    # and resolve in the next framework.
    cat(paste0("*NOTE # 2* See code for message about the CPUE CV being downweighted artificially, this needs revised in next Framework as it ain't legit.\n"))
    ifelse(names(DD.lst[[bnk]])[9] == "U.se", names(DD.lst[[bnk]])[9] <- "U.cv", DD.lst[[bnk]]$U.cv <- DD.lst[[bnk]]$U.cv*50)
    # Also, if doing this we need to change the original data to represent what the model is seeing..
    # So if we used the SE let's replace the U.cv data with the U.se data, if we are doing the
    # 50x to match what we've done before than we need to change those data as well.
    ifelse(names(DD.lst[[bnk]])[9] == "U.se", DD.dat$U.cv <- DD.dat$U.se, DD.dat$U.cv <- DD.dat$U.cv*50)
    
    # Add a couple items to the DD.lst list...
    DD.lst[[bnk]]$NY<- length(DD.lst[[bnk]]$C)
    DD.lst[[bnk]]$year<-min(DD.dat$year):max(DD.dat$year)
    # Set up Priors.  This first bit is getting our variance correct for the CV's for Biomass, Recruit biomass, and catch rates.
    # This is then added to our list of priors to get them correct.
    # Biomass CV
    uI=log(DD.lst[[bnk]]$I.cv^2+1) # See Smith and Hubley 2014 for details, this is variance of the log of a CV
    # DK Note:  Smith/Hubley suggest this should be 3, so why we setting it to 2???
    Ip.a=2+(uI/uI)^2 # This is the alpha prior term for the prior (an inverse-gamma, i.e. gamma using 1/var); a rather funky way of setting alpha =2
    Ip.b=1/(uI*((uI/uI)^2+1)) # This is the beta term for the prior, again a strangly complex way of saying 1/(2*(uI))
    # Recruit biomass CV, see above comments for details.
    uIR=log(DD.lst[[bnk]]$IR.cv^2+1)
    IRp.a=2+(uIR/uIR)^2
    IRp.b=1/(uIR*((uIR/uIR)^2+1))
    # Catch Rate CV, see above comments for details.
    uU=log(DD.lst[[bnk]]$U.cv^2+1)
    Up.a=2+(uU/uU)^2
    Up.b=1/(uU*((uU/uU)^2+1))
      
    DDpriors[[bnk]]=list(
      logK=			    list(a=7,		  b=7,		d="dnorm",	l=1		),		# scaler to total biomass, a= mean  b = sd, this gives a huge range of starting values
      r=				    list(a=0, 		b=1,		d="dlnorm",	l=NY	),		# scaled recruit biomass, a= meanlog  b = sdlog
      m=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality fully recruited a= meanlog  b = sdlog
      mR=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality  recruits a= meanlog  b = sdlog
      S=				    list(a=8, 		b=11,		d="dbeta",  l=1		),		# clapper dissolution rate a= shape1, b=shape2, 8 & 11 gives ~ normal mean of .45ish
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
      for(h in 1:length(DDpriors[[bnk]]))
      {
        # Get the variances for log-normal and normal converted to precisions, note that in BUGS language the precision is
        # the inverse of the squared standard deviation (which is what you specify in R).  The standard deviation is what
        # was specified in the Prior list (as it is more intuitive)
        if(DDpriors[[bnk]][[h]]$d%in%c("dlnorm","dnorm")) DDpriors[[bnk]][[h]]$b <- 1/DDpriors[[bnk]][[h]]$b^2
        # For a Gamma to convert to precision the precision term is  the inverse of the 'Scale" term in a typical 
        # gamma distribution parameterization, aka this is now knonwn as the rate.
        # Happily this is the same as the parameterization in R dgamma(x,shape,rate) so our b parameter is correct for posterior plots.
        if(DDpriors[[bnk]][[h]]$d=="dgamma")DDpriors[[bnk]][[h]]$b<-1/DDpriors[[bnk]][[h]]$b
      } # end for(h in 1:length(DDpriors[[bnk]]))
      # Made a data.frame of the priors, unwrap the list and combine by row.
      prior.dat<- data.frame(par=names(DDpriors[[bnk]]),do.call("rbind",lapply(DDpriors[[bnk]],rbind)))
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
      ifelse(is.null(parameters) == T, parameters <- c(names(DDpriors[[bnk]]),'K','P','B','R','mu','Imed','Ipred','Irep', 'IRmed','IRpred','IRrep',
                                                                              "Cmed","Crep","CRmed","CRrep",'sIresid','sIRresid','sPresid','Iresid',
                                                                              'IRresid','Presid',"Cresid","CRresid","sCresid","sCRresid"),parameters)
      # Run the model and see how long it takes.
      # n = 400,000 and burn = 100,000, thin = 20 with 2 chains do not decrease these as retaining this much
      # data is needed to stabilize the projections, it does lengthen the run time to 10-20 minutes in serial
      # Running in parallel stick with that burn in but we can get away with n=200,000, burn = 100,000, thin = 20, and 6 chains
      # they are longer chains than really are needed for the model to converge, but this is really being done just for the projections.
      # Run the model now.
      start<-Sys.time()
      ## Call to JAGS, do you want to run in parallel?
      
      if(parallel==F)
      {
        out <- jags(data =  c(prior.lst,DD.lst[[bnk]]), inits = NULL,parameters.to.save = parameters,  
                    model.file = paste(direct,jags.model,sep=""),n.chains = nchains, n.iter = niter, n.burnin = nburn, 
                    n.thin = nthin)
      }
      
      if(parallel==T)
      {
        out <- jags.parallel(data =  c(prior.lst,DD.lst[[bnk]]), inits = NULL,parameters.to.save = parameters,  
                             model.file = paste(direct,jags.model,sep=""),n.chains = nchains, n.iter = niter, n.burnin = nburn, 
                             n.thin = nthin,jags.seed = seed)
      }
      # How long did that take?
      print(Sys.time()-start)
      
      # Rename the output so I retain the results 
      DD.out[[bnk]] <- list(data=c(prior.lst,DD.lst[[bnk]],yrs[[bnk]]), sims.list=out$BUGSoutput$sims.list,median=out$BUGSoutput$median,
                            mean=out$BUGSoutput$mean,summary=out$BUGSoutput$summary,priors = prior.lst,parameters=parameters)
                       
      # I will also retain the MCMC object produced in case I want it for something.
      mod.out[[bnk]] <- out

      #source("fn/projections.r")
      # The catch since the survey for the most recent year is this, if there was no catch set this to 0.
      proj.catch[[bnk]] <- max(proj.dat[[bnk]]$catch[proj.dat[[bnk]]$year == max(DD.dat$year)],0)
      # Get the low and upper boundaries for the decision table (this might be a silly way to do this...)
      if(bnk %in% c("GBa","BBn"))
      {
        D_low[[bnk]] <- subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$D_tab_low
        D_high[[bnk]] <-  subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$D_tab_high
      } # end if(bnk %in% c("GBa","BBn"))
      # If we are looking at one of the sub-areas we will go for 1/3 of the mean biomass estimate for the current year...
      if(!bnk %in% c("GBa","BBn")) {D_low[[bnk]] <- 0; D_high[[bnk]] <- out$BUGSoutput$mean$B[length(out$BUGSoutput$mean$B)]/3}
      # The increment size for the decision table.  500 for GBa and 50 for BBn
      step <- ifelse(bnk == "GBa", 500,50)
      # The URP and LRP for the bank, for the moment only GBa has been accepted so it's the only one used.
      if(bnk %in% c("GBa","BBn"))
      {
        URP[[bnk]] <-  subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$URP
        LRP[[bnk]] <-  subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$LRP
      } # end if(bnk %in% c("GBa","BBn"))
      
      # For the sub-areas just make these NA.
      if(!bnk %in% c("GBa","BBn")) {URP[[bnk]] <- NA; LRP[[bnk]]<- NA}
      
      # Get the projection scenarios of interest
      if(length(proj.catch[[bnk]]) > 0) proj[[bnk]] <- seq(D_low[[bnk]],D_high[[bnk]],step) + proj.catch[[bnk]]
      # If we don't have projected catch data yet (i.e. I'm running the model before the logs have data in them..)
      if(length(proj.catch[[bnk]]) == 0) 
      {
        # Set projected catch to 0
        proj.catch[[bnk]] <- 0
        proj[[bnk]] <- seq(D_low[[bnk]],D_high[[bnk]],step) + proj.catch[[bnk]]
        writeLines("YO YO LOOK HERE!!  The projected catch used in this model is 0, this should only happen in preliminary runs!!")
      }
      # The interim TAC is known for GBa and BBn,
      if(bnk %in% c("GBa","BBn")) TACi[[bnk]] <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC
      # For the sub-areas let's just make this last years catch from the area, not perfect but should be reasonable
      if(!bnk %in% c("GBa","BBn")) TACi[[bnk]] <- DD.lst[[bnk]]$C[DD.lst[[bnk]]$NY]
      
      # Now do the projections
      DD.out[[bnk]]<- projections(DD.out[[bnk]],C.p=proj[[bnk]]) # C.p = potential catches in decision table
 
      ### Generate Decision Table ###
      ### Note that from the 2015 SSR we have these definitely set at...
      #Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
      #The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
      
      if (bnk == "GBa") 
      {
        D.tab[[bnk]]<-decision(DD.out[[bnk]],bnk, mu=0.15,refs=c(URP[[bnk]],LRP[[bnk]]),post.survey.C=proj.catch[[bnk]])
        if (export.tables == T) write.csv(D.tab[[bnk]],paste0(plotsGo,"Decision_GBa.csv",sep=""),row.names=F) #Write1

      } # END if(bnk == "GBa")
      
      # Now Browns North or the sub areas
      if (bnk != "GBa") 
      {
        D.tab[[bnk]]<-decision(DD.out[[bnk]],bnk, mu=0.15,post.survey.C=proj.catch[[bnk]])
        if (export.tables == T) write.csv(D.tab[[bnk]],paste0(plotsGo,"Decision_",bnk,".csv",sep=""),row.names=F) #Write2
      } # END if(bnk == "BBn")
      
      # For i = 1 this will just get the first bank, unfortunately if i =2 then this will pull in results for both 
      #  (if running this as a loop) which is silly, but work arounds are dumber than this solution
      # If you are happy and want to keep these results 
      if(final.run == T) 
      {
        save(DD.lst, DDpriors,DD.out,DD.dat,mod.out,mod.dat,cpue.dat,proj.dat,yr,D.tab,manage.dat,proj.catch,
           URP,LRP,proj,bnk,TACi,yrs,j,
           file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))
      } # end if(final.run == T) 
      # If you are still testing results the model will save here, 
      if(final.run == F) 
      {
        save(DD.lst, DDpriors,DD.out,DD.dat,mod.out,mod.dat,cpue.dat,proj.dat,yr,D.tab,manage.dat,proj.catch,
             URP,LRP,proj,bnk,TACi,yrs,j,
             file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_testing_results.RData",sep=""))
      } # if(final.run == F) 
      
      print("done running model. Results saved in Data/Model/year/bank/Results/")
  } # end if(run.mod==T)

############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  
############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  
############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  




############# Section 3 Results for Update Doc and Model Diagonistics############# Section 3 Results for Update Doc and Model Diagonistics
############# Section 3 Results for Update Doc and Model Diagonistics############# Section 3 Results for Update Doc and Model Diagonistics
# We now make diagnostics automatically, it takes no time so just do it...
    if(run.mod==F) # First load the data if we didn't just run the model.
    {
      # If we have the final run run we pull the data from that files
      if(file.exists(paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))==T && use.final==T)
      {
        load(file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))
        writeLines("Good day!  You are using the final model results \'Final_model_results.RData\', great idea! \n")
      } # end if(file.exists(paste(direct,"Data/... == T
      
      # If we haven't yet run the final model we'll pull the data from here and print a warning that these results aren't final yet!
      if(file.exists(paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_testing_results.RData",sep=""))==T && use.final==F)
      {
        load(file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_testing_results.RData",sep=""))
        writeLines("YO HEADS UP!!  You are using the testing results \'Model_testing_results.RData\' results NOT the \'FINAL_model_results.RData\'
               please treat these results as exploratory. Always remember.. you're the best! \n")
        
      } # end if(file.exists(paste(direct,"Data/... == F
    } # end (fi(run.mod=F))

    # Initialize necessary variables
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
        
    # Some model outputs needed for the Update.  First the mortality
    mort[[bnk]] <- 1- exp(-DD.out[[bnk]]$mean$m[length(DD.out[[bnk]]$mean$m)])
    # This lines up the column headers with the projected catch...
    TACI[[bnk]]<- which(DD.out[[bnk]]$data$C.p==(TACi[[bnk]]+proj.catch[[bnk]]))
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


    print("done running diagnostics")
 
##################### END Section 3 Model Diagnostics #####################  END Section 3 Model Diagnostics #################  
##################### END Section 3 Model Diagnostics #####################  END Section 3 Model Diagnostics #################  


#################  SECTION 4 Figures #################  SECTION 4 Figures #################  SECTION 4 Figures #################  
#################  SECTION 4 Figures #################  SECTION 4 Figures #################  SECTION 4 Figures #################  
# Now if I want to make the figures both from the model output and for the update document do these...

     #####Plot model diagnostics############## 
    # These plots include the posterior fits, exploitation estimate, Biomass fit to survey and CPUE, residual plot
    # and the model convergence plot (which is a 700+ page pdf of the convergence of each parameter + it's ACF.)
    if(make.diag.figs == T)
    {
      # posterior densities for model parameters
      post.plt(DD.out[[bnk]],DDpriors[[bnk]],years=yrs[[bnk]], graphic=fig,multi=T,path=plotsGo)
      #dev.off()
      ##exploitaiton time series
      exploit.plt(DD.out[[bnk]], years=yrs[[bnk]], plt=c('f','m','mR'),graphic=fig,path=plotsGo)
      #dev.off()
      # model biomass fit to survey
      fit.plt(DD.out[[bnk]], years = yrs[[bnk]], CI=T,graphic=fig,path=plotsGo,CV=T)
      # diagnostic plot
      diag.plt(DD.out[[bnk]], years = yrs[[bnk]],graphic=fig,path=plotsGo)
      
      # Here we pull together all of the chains and look to see that they are both well mixed and that
      # there is no correlation.   This is a complete crap load of plots!!
      # Function to plot all the chains.
      # Get the bank pulled out and figure out how many parameters we have
      num.param <- length(names(DD.out[[bnk]]$sims.list))
      param.names <- names(DD.out[[bnk]]$sims.list)
      # Make the pdf, given the number of parameters in the model you don't get an option for making this plot print to screen
      # if you run diagnostics you get this pdf
      
      # First we can set the dimensions for the plotting device.
      # Set the number of rows
      nr <- ceiling(sqrt(nchains))
      # Set the number of columns, the funky little command is used to add one to the nc if nchains is a perfect square
      # As we are printing nchains + 1
      ifelse(sqrt(nchains)%%1==0,  nc <- ceiling(sqrt(nchains))+1, nc <- ceiling(sqrt(nchains)))
      # I always force this to make a pdf because it is a bazillion pages long...
      pdf(file=paste(plotsGo,"Model_convergence.pdf",sep=""),onefile=T)
      # Set up the plotting device.
      par(mfrow = c(nr,nc),mar=c(2,2,3,1))
      for(i in 1:num.param)
      {
        # This pulls out all the plot for parameters with only one value
        if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])==T)
        {
          # Since our all of these parameters are hyperparameter (there isn't one for every year) this works, 
          #if this was a matrix we'd get an error here.
          len <- length(DD.out[[bnk]]$sims.list[[param.names[i]]])
          # This sets us up to pull out the right "chains" from the data using the k loop below.
          bins <- seq(1,len,by = len/nchains)
          # Get the ylimits for the plot.
          ylims <- range(DD.out[[bnk]]$sims.list[[param.names[i]]])
          colr <- rainbow(nchains) # set up a color ramp
          count <- 0 # Set up a counter
          # I need to run this loop twice, once for the first figure so we get all the lines added and a second time to 
          # add in the ACF's.  Probably a nicer way to do this, but it'll do...
          # Now run the loop and make the figure showing the mixing of the chains
          for(k in bins)
          {
            count <- count+1 # used for the color ramp
            # Get the data
            dat <-DD.out[[bnk]]$sims.list[[param.names[i]]][k:(k+len/nchains-1)] 
            if(k ==1) plot(dat,type="l",col=colr[count], main = paste(param.names[i], "Chain"),xlab="",ylab="",ylim=ylims)
            if(k > 1) lines(dat,col=colr[count])
          } # end for(k in bins)
          
          # Now to make the ACF figures
          count  <- 0 # Reset the counter
          for(k in bins)
          {
            count <- count+1 # used to ID the chain
            # Pick it up from here.
            dat <-DD.out[[bnk]]$sims.list[[param.names[i]]][k:(k+len/nchains-1)] 
            # And look for any signs of autocorrelation in the chains...
            acf(dat,lag.max = 10,main = paste("ACF chain",count),xlab="",ylab="",ylim=c(0,0.3))
          }# end for(k in bins)
          
        } # end if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[1]]])==T)

        # This pulls out all the plots for parameters with multiple values (i.e. annual estimates)
        if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])==F)
        {
          num.reps <- ncol(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])
          rep.names <- paste(names(DD.out[[bnk]]$sims.list)[i],1:num.reps,sep="_")
          # Run this loop for each chain for these parameters.
          for(p in 1:num.reps)
          {
            # Get the length again (this could probably be tidied up as this number has to be the same for all parameters.)
            len <- length(DD.out[[bnk]]$sims.list[[param.names[i]]][,p])
            # Get the bins for the loop
            bins <- seq(1,len,by = len/nchains)
            # Get the ylimits for the plot.
            ylims <- range(DD.out[[bnk]]$sims.list[[param.names[i]]][,p])
            colr <- rainbow(nchains) # set up a color ramp
            count <- 0 # Set up a counter
            # Set up the plotting device.
            #par(mfrow = c(nr,nc),mar=c(2,2,3,1))
            # I need to run this loop twice, once for the first figure so we get all the lines added and a second time to 
            # add in the ACF's.  Probably a nicer way to do this, but it'll do...
            # Now run the loop and make the figure showing the mixing of the chains
            for(k in bins)
            {
              count <- count+1 # used for the color ramp
              # Get the data
              dat <-DD.out[[bnk]]$sims.list[[param.names[i]]][k:(k+len/nchains-1),p]
              if(k ==1)  plot(dat,type="l",col=colr[count], main = paste(rep.names[p], "Chain"),xlab="",ylab="",ylim=ylims)
              if(k > 1) lines(dat,col=colr[count])
            } # end for(k in bins)
            count <- 0 # Reset the counter
            # And now for the ACF figures
            for(k in bins)
            {
              count <- count+1 # used for the chain index
              # Get the data
              dat <-DD.out[[bnk]]$sims.list[[param.names[i]]][k:(k+len/nchains-1),p]
              # And look for any signs of autocorrelation in the chains...
              acf(dat,lag.max = 10,main = paste("ACF chain ",count),xlab="",ylab="",ylim=c(0,0.3))
            } # end for(k in bins)
          }# end for(p in 1:num.reps)
        } # end if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])==F)
      }  # end for(i in 1:num.param)
      dev.off()
    
      print("done making diagnostic figures")
      
    } #end  if(make.diag.figs == T)
    
    # Now we make the figures used in the update document  
    if(make.update.figs == T)
    {
      bm.max <- ifelse(bnk == "GBa", 55000,25000)
      
      # Now make the biomass plots for the areas as necessary
      if(bnk != "GBa")
      {
        biomass.plt(DD.out[[bnk]],years=yrs[[bnk]], graphic=fig,TAC=TACi[[bnk]]+proj.catch[[bnk]],path=plotsGo,refs=NULL,pred=1,
                    URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median,Bymax=bm.max)
      } # end if(bnk == "BBn")
      
      if(bnk == "GBa")
      {
        biomass.plt(DD.out[[bnk]],years=yrs[[bnk]], graphic=fig,TAC=TACi[[bnk]]+proj.catch[[bnk]],path=plotsGo,refs = c("LRP","URP","zones"),pred=1,
                    URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median,Bymax=bm.max)
        
      } # end if(bnk == "GBa")
    
      # Only make these figures for GBa or BBn
      if(bnk %in% c("GBa","BBn"))
      {
        #  Now we transition to produce the figures used in the Update document that are not dependent on model output.
        # First up we need the fishery data and TAC here, we don't actually have the calendar year fishery data 
        # anywhere at this point so we grab that
        logs_and_fish(loc="offshore",year = 1998:max(mod.dat[[bnk]]$year),un=un,pw=pwd,db.con=db.con,direct.off=direct)
        # If you get any NA's related warnings it may be something is being treated as a Factor in one of the two files.  
        # This should combine without any warnings so don't ignore warnings here.
        fish.dat<-merge(new.log.dat,old.log.dat,all=T)
        fish.dat$ID<-1:nrow(fish.dat)
        # Being lazy we get the data for each bank We are just looking for the annual values here so nothing fancy needed...
        dat <- fishery.dat(fish.dat,bk=bnk,yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct,period = "calyr") 	
        if(bnk=="GBa")dat1<-fishery.dat(fish.dat,bk="GBb",yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct,period = "calyr") 	
        
        if(fig== "screen") windows(8.5,8.5)
        if(fig == "pdf") pdf(paste(plotsGo,"TAC_landings.pdf",sep=""),width=8.5,height=8.5)
        if(fig == "png") png(paste(plotsGo,"TAC_landings.png",sep=""),width=8.5,height=8.5,res=920,units="in")
        # Here are the time series on Georges Bank
        if(bnk == "GBa") par(mfrow=c(2,1),cex=1.2,mar=c(2,5,1,1))
        if(bnk != "GBa") par(mfrow=c(1,1),cex=1.2,mar=c(2,5,1,1))
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
        if(fig!="screen") dev.off()

  
      #############  FINALLY I WANT TO MAKE AN OVERALL PLOT OF THE BANKS AND THAT WILL BE THAT...
      # Also make the overall plot of the banks...
      if(fig== "screen") windows(11,8.5)
      if(fig == "pdf") pdf(paste(plotsGo,"Offshore_banks.pdf",sep=""),width=11,height=8.5)
      if(fig == "png") png(paste(plotsGo,"Offshore_banks.png",sep=""),width=11,height=8.5,res=920,units="in")
      ScallopMap("NL",plot.bathy=T,plot.boundries=T,boundries="offshore",bound.color = T,label.boundries = T,offshore.names = T,
                 direct=direct,cex.mn=2,dec.deg = F,cex=1.3,shore="nwatlHR")
      # Turn off the plot device if making a pdf.
      if(fig != "screen") dev.off()
      } # end if(bnk %in% c("GBa","BBn"))
  print("done making document figures")
    } # end if(make.update.figs == T)
  
####################  END SECTION 4 Figures ####################  END SECTION 4 Figures####################  END SECTION 4 Figures####################  
    ####################  END SECTION 4 Figures ####################  END SECTION 4 Figures####################  END SECTION 4 Figures####################  
    
################## Section 5 Prediction evaluation Model################## Section 5 Prediction evaluation Model##################
################## Section 5 Prediction evaluation Model################## Section 5 Prediction evaluation Model################## 
  # Do we want to run the prediction evaluation model
  if(run.pred.eval.model == T)
  {
    # Specify the parameters for the prediction evalulation plots.
    if(is.null(pe.years)) pe.years <- max(yrs[[bnk]])
    if(is.null(pe.iter)) pe.iter <- niter
    if(is.null(pe.burn)) pe.burn <- nburn
    if(is.null(pe.thin)) pe.thin <- nthin
    if(is.null(pe.chains)) pe.chains <- nchains
    
    # Make sure the pe.years is sorted from most recent year to past
    pe.years <- rev(sort(pe.years))
    #Prediction Evaluation using the current year CF, this isn't how we model it as we don't know g2/gR2 when we do our predictions
    pred.eval(input = DD.lst[[bnk]], priors = DD.out[[bnk]]$priors, pe.years= pe.years, growth="both",model = jags.model,  bank=bnk,
              parameters = DD.out[[bnk]]$parameters,niter = pe.iter,nburn = pe.burn, nthin = pe.thin,nchains=pe.chains,direct=direct)

    # Now we make the figures and save them...
    pe.fig(years=max(yrs[[bnk]]),growth="both",graphic = fig,direct= direct,bank = bnk,plot=pred.eval.fig.type,path=plotsGo)
    #pe.fig(years=max(yrs[[bnk]]),growth="modelled",graphic = "screen",direct= direct,bank = bnk,plot="box")
    
    print("done running prediction evaluation")
  }  # if(run.pred.eval.model == T)

} # end for(j in 1:num.banks)

} # end function
