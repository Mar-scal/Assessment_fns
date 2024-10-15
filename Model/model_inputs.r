#### Data processing for model inputs
#### FK 2021/2022
# conclusion of 2022 process was to use "mixed" approach for imputation. midpoints for everything except growth, which uses LTM, so I have made that the default param.
model_inputs <- function(bank, yr, impute="mixed", nickname, direct, direct_fns, survey.obj=NULL){
  
  require(PBSmapping)
  require(maptools)
  
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Fishery/logs_and_fishery_data.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/projections.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/decision.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Model/post.plt.R",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Model/exploit.plt.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/fit.plt.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/diag.plt.R",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Model/prediction_evaluation_function.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/prediction_evaluation_figure.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/biomass.plt.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Fishery/fishery.dat.R"
    )
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}
  } else {  source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep="")) #logs_and_fish is function call
    source(paste(direct_fns,"Fishery/fishery.dat.r",sep="")) 
    source(paste(direct_fns,"Model/projections.r",sep=""))
    source(paste(direct_fns,"Model/decision.r",sep=""))
    source(paste(direct_fns,"Model/post.plt.R",sep=""))
    source(paste(direct_fns,"Model/exploit.plt.r",sep=""))
    source(paste(direct_fns,"Model/fit.plt.R",sep=""))
    source(paste(direct_fns,"Model/diag.plt.R",sep=""))
    source(paste(direct_fns,"Model/prediction_evaluation_function.r",sep="")) #The function to run the prediction evaluations
    source(paste(direct_fns,"Model/prediction_evaluation_figure.r",sep="")) # The function to make the plots
    source(paste(direct_fns,"Model/biomass.plt.r",sep=""))
    source(paste(direct_fns,"Maps/pectinid_projector_sf.r",sep=""))
  }
  
  direct_fns1 <- direct_fns
  direct.real <- direct 
  nickname1 <- nickname
  
  if(!is.null(survey.obj)) {
    load(survey.obj)
  }
  
  if(is.null(survey.obj)){
    # Bring in the survey results for the current year, this also includes the fishery data..
    # If we have run the whole survey we can pull in from this file, if not we will have to pull them in below (in the loop)
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))==T)
    {
      # If we have all survey results we preprocess both banks...
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
    } # end if(file.exists(paste(direct,"Data/... == T
    
    direct <- direct.real
    nickname <- nickname1
    
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
    }  # end if(file.exists(paste(direct,"Data/... == F
    
    direct <- direct.real
    nickname <- nickname1
    
    if(mwsh.test == T && !is.null(nickname) &&  use.final==F && final.run== F) {
      # read in data
      load(paste(direct,"Data/Survey_data/" ,yr, "/Survey_summary_output/testing_results_", nickname, ".Rdata",sep=""))
    }
  }
  direct <- direct.real
  nickname <- nickname1
  
  if(!bank %in% names(survey.obj)) {
    message("The bank you want is not in the survey.obj. Specify a different path to a survey.obj Rdata file or re-run survey summary to include the bank (or subarea) you requested")
    #browser()
    load(paste0(direct, "Data/Survey_data/", yr, "/Survey_summary_output/testing_results_SCALOFF_LE10.RData"))
    if(!bank %in% names(survey.obj)) {
      stop("Still not there... try again.")
    }
  }
  
  direct <- direct.real
  nickname <- nickname1
  direct_fns <- direct_fns1
  
  # Now bring in the latest fishery data
  if(!missing(direct_fns)) logs_and_fish(loc="offshore",year = 1981:yr,direct=direct, direct_fns=direct_fns)
  if(missing(direct_fns)) logs_and_fish(loc="offshore",year = 1981:yr,direct=direct)
  # If you get any NA's related warnings it may be something is being treated as a Factor in one of the two files.  
  # This should combine without any warnings so don't ignore warnings here.
  dat.fish<-merge(new.log.dat,old.log.dat,all=T)
  dat.fish$ID<-1:nrow(dat.fish)
  
  #Read1 Bring in the VonB model parameters
  cat("We read in the von B growth parameters from the file .../Data/Ageing/Von_B_growth_parameters.csv \n")
  vonB <- read.csv(paste(direct,"Data/Ageing/Von_B_growth_parameters_framework.csv",sep=""))
  
  # Run this for one or both banks
  mod.dat <- NULL
  cpue.dat <- NULL
  proj.dat <- NULL
  # Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
  for(i in 1:length(bank))
  {
    #browser()
    # If we are running a sub-area we need to make sure we have the correct bank to pull the data from
    master.bank <-ifelse(grepl(pattern="GBa",x=bank[i])==T , "GBa","BBn")
    years <- min(survey.obj[[bank[i]]][[1]]$year):max(survey.obj[[bank[i]]][[1]]$year)
    
    # in 2020, the covid-19 pandemic prevented the DFO survey from occurring. An industry-lead survey of limited scope occurred, but is not suitable for inclusion in the 
    # assessment models. As such, we need to fill-in the blank row for 2020 with some data. We'll try out different options for doing that here. 
    if(yr>2020) {
      # what if we add an empty row full of NAs... NO. jags model fails. the 2021 row is complete, but it doesn't know what to do with NA's in 2019 and 2020. Try something else.
      # empty <- survey.obj[[bank[i]]][[1]][1,]
      # empty[1,] <- NA
      # empty$year <- 2020
      # survey.obj[[bank[i]]][[1]] <- merge(survey.obj[[bank[i]]][[1]], empty, all=T)
      
      if(impute=="previous_year") {
        year2020 <- data.frame(year=2020, survey.obj[[bank[i]]][[1]][survey.obj[[bank[i]]][[1]]$year==2019,c(2:ncol(survey.obj[[bank[i]]][[1]]))])
        names(year2020) <- names(survey.obj[[bank[i]]][[1]])
        survey.obj[[bank[i]]][[1]] <- merge(survey.obj[[bank[i]]][[1]], year2020, all=T)
      }
      if(impute=="LTM") {
        year2020 <- as.data.frame(lapply(X = survey.obj[[bank[i]]][[1]][survey.obj[[bank[i]]][[1]]$year<2020,], MARGIN = 2, median))
        year2020$year <- 2020
        names(year2020) <- names(survey.obj[[bank[i]]][[1]])
        survey.obj[[bank[i]]][[1]] <- merge(survey.obj[[bank[i]]][[1]], year2020, all=T)
      }
      if(impute=="midpoint" | impute=="mixed") {
        year2020 <- as.data.frame(lapply(X = survey.obj[[bank[i]]][[1]][survey.obj[[bank[i]]][[1]]$year %in% 2019:2021,], MARGIN = 2, mean))
        year2020$year <- 2020
        names(year2020) <- names(survey.obj[[bank[i]]][[1]])
        survey.obj[[bank[i]]][[1]] <- merge(survey.obj[[bank[i]]][[1]], year2020, all=T)
      }
      
      if(impute=="min") {
        year2020 <- as.data.frame(t(apply(X = survey.obj[[bank[i]]][[1]][survey.obj[[bank[i]]][[1]]$year<2020,], MARGIN = 2, min)))
        year2020$year <- 2020
        names(year2020) <- names(survey.obj[[bank[i]]][[1]])
        survey.obj[[bank[i]]][[1]] <- merge(survey.obj[[bank[i]]][[1]], year2020, all=T)
        survey.obj[[bank[i]]][[1]]$RS <- 85
        survey.obj[[bank[i]]][[1]]$CS <- 95
      }
      if(impute=="max") {
        year2020 <- as.data.frame(t(apply(X = survey.obj[[bank[i]]][[1]][survey.obj[[bank[i]]][[1]]$year<2020,], MARGIN = 2, max)))
        year2020$year <- 2020
        names(year2020) <- names(survey.obj[[bank[i]]][[1]])
        survey.obj[[bank[i]]][[1]] <- merge(survey.obj[[bank[i]]][[1]], year2020, all=T)
        survey.obj[[bank[i]]][[1]]$RS <- 85
        survey.obj[[bank[i]]][[1]]$CS <- 95
      }
      if(impute=="no") {
        # browser()
        # year2020 <- as.data.frame(t(data.frame(vals = rep(NA, length(names(survey.obj[[bank[i]]][[1]]))))))
        # names(year2020) <- names(survey.obj[[bank[i]]][[1]])
        # year2020$year <- 2020
        # survey.obj[[bank[i]]][[1]] <- merge(survey.obj[[bank[i]]][[1]], year2020, all=T)
        # survey.obj[[bank[i]]][[1]]$RS <- 85
        # survey.obj[[bank[i]]][[1]]$CS <- 95
      }
      
    }
    
    # First off we subset the data to the area of interest using the survey boundary polygon, only do this for the sub-areas though
    if(!bank[i] %in% c("GBa","BBn"))
    {
      bound.surv.sp <- PolySet2SpatialPolygons(bound.surv.poly[[bank[i]]])
      # Get to get rid of all the crap spatial data in here.
      fish.tmp <- dat.fish[dat.fish$bank == master.bank  & !is.na(dat.fish$bank) & dat.fish$lon < 0 & dat.fish$lat > 0 & !is.na(dat.fish$lon) & !is.na(dat.fish$lat),]
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
    if(!missing(direct_fns)) cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk=master.bank,yr=(min(years)-1):max(years),method='jackknife',
                                                                period = "survyr", direct=direct, direct_fns=direct_fns) 	
    if(missing(direct_fns)) cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk=master.bank,yr=(min(years)-1):max(years),method='jackknife',
                                                               period = "survyr", direct=direct) 	
    
    # Now on Browns North the survey usually happens in June so the projection is actually different
    # But in 2015 the survey was messed so the above is the solution used for 2015, 
    #for all other years we need to do this for Browns Bank North
    # It really makes very little difference which way this is done as the catch in June-August
    # has averaged around just 40 tonnes since about 1996.
    
    if(yr != 2015 &&  master.bank== "BBn") 
    {
      if(!missing(direct_fns)) cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk=master.bank,yr=(min(years)-1):max(years),surv='May',
                                                                  method='jackknife',period = "survyr", direct=direct, direct_fns=direct_fns) 	
      if(missing(direct_fns)) cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk=master.bank,yr=(min(years)-1):max(years),surv='May',
                                                                 method='jackknife',period = "survyr", direct=direct) 	
    }
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
    if(!missing(direct_fns)) proj.dat[[bank[i]]] <- fishery.dat(proj.sub,bk=master.bank,yr=(min(years)-1):max(years),method='jackknife',
                                                                period = "calyr", direct=direct, direct_fns=direct_fns) 	
    if(missing(direct_fns))proj.dat[[bank[i]]] <- fishery.dat(proj.sub,bk=master.bank,yr=(min(years)-1):max(years),method='jackknife',
                                                              period = "calyr", direct=direct) 	
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
    
    ### overwrite imputation for growth here using whichever method
    if(yr>2020 & impute=="mixed") {
      # in 2020, the covid-19 pandemic prevented the DFO survey from occurring. An industry-lead survey of limited scope occurred, but is not suitable for inclusion in the 
      # assessment models. As such, we need to fill-in the blank row for 2020 with some data. We'll try out different options for doing that here. 
      # We imputed the values in the survey data earlier, but for the "mixed" imputation method, we'll handle growth separately.
      # Maybe it makes more sense to use the LTM for growth but midpoint for other values. 
      
      # change 2019 and 2020 values to NA
      mod.dat[[bank[i]]]$g[which(mod.dat[[bank[i]]]$year %in% 2019:2020)] <- NA
      mod.dat[[bank[i]]]$g2[which(mod.dat[[bank[i]]]$year %in% 2020)] <- NA
      
      mod.dat[[bank[i]]]$gR[which(mod.dat[[bank[i]]]$year %in% 2019:2020)] <- NA
      mod.dat[[bank[i]]]$gR2[which(mod.dat[[bank[i]]]$year %in% 2020)] <- NA
      
      # replace the NAs with long term medians
      mod.dat[[bank[i]]]$g[which(mod.dat[[bank[i]]]$year %in% 2019:2020)] <- median(mod.dat[[bank[i]]]$g[mod.dat[[bank[i]]]$year<2020], na.rm=T)
      mod.dat[[bank[i]]]$g2[which(mod.dat[[bank[i]]]$year %in% 2020)] <- median(mod.dat[[bank[i]]]$g2[mod.dat[[bank[i]]]$year<2020], na.rm=T)
      
      mod.dat[[bank[i]]]$gR[which(mod.dat[[bank[i]]]$year %in% 2019:2020)] <- median(mod.dat[[bank[i]]]$gR[mod.dat[[bank[i]]]$year<2020], na.rm=T)
      mod.dat[[bank[i]]]$gR2[which(mod.dat[[bank[i]]]$year %in% 2020)] <- median(mod.dat[[bank[i]]]$gR2[mod.dat[[bank[i]]]$year<2020], na.rm=T)
      
    }
    
    
  } # end for(i in 1:length(bank))
  
  # Save the results so you don't have to do section 1 over and over.
  if(is.null(nickname)) save(mod.dat,cpue.dat,proj.dat,file=paste(direct,"Data/Model/",(yr+1),"/", bank, "/Model_input.RData",sep=""))
  if(!is.null(nickname)) save(mod.dat,cpue.dat,proj.dat,file=paste(direct,"Data/Model/",(yr+1),"/", bank, "/Model_input_", nickname, ".RData",sep=""))
  
  
  
}