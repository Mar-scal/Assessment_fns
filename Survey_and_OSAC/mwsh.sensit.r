## MWSH Sensitivity Analysis Functions: 
## mwsh.sensit()
## mwsh.survey.extract()
## mwsh.DDmodel.extract()

## Use these three functions, along with Survey_Summary_Data and Update_function_JAGS to run sensitivity analyses 
## on our survey and model outputs related to variations in the MWSH model/sampling protocol. 

## 1: mwsh.sensit function
## Use this function to test the effects of different subsets of MWSH data on the Condition and Biomass Estimates using their variance.
## sub.size: lower and upper bounds of size subset for MWSH data. Must enter NA if no number provided. c(89, NA) means keep everything equal to or above 89mm.
## sub.year: lower and upper bounds of year subset for MWSH data. Must enter NA if no number provided. c(NA, 2008) means keep everything equal to or below 2008.
## sub.tows: proportion of tows to randomly exclude from the MWSH model
## sub.samples: proportion of samples to randomly exlcude from the MWSH model
## bank: matches Survey Summary bank options. Must enter as c(a, b, c, d) (not "all")
## mwdat: name of dataframe with meat weight data
## shfdat: name of dataframe with all tow data
## direct: specify the location of the Assessment_fns folder
## ** Note if dropping tows via sub.tows or sub.samples, it doesn't seem to matter whether you predict on only the excluded tows, or if you predict on the entire dataset
## ** The point is that the model itself was built with a reduced sample size, and reduced sample size should mean increased uncertainty. We want to know just how much it increases. 

# examples
# runfull <- mwsh.sensit(bank="Ger", mwdat=mwger_0, shfdat=shfger_0, direct=direct, direct_fns=direct_fns)
# rundropped <- mwsh.sensit(bank="Ger", mwdat=mwger_0, shfdat=shfger_0, sub.tows=0.20, direct=direct, direct_fns=direct_fns)

# outputs:
# mwshmod and condmod objects
# suggestion: compare condmod$CFyrs$CFse.fit between runs (error bars on the CF time series plot. how do they change with diff data subsets?)

mwsh.sensit <- function(mwdat, shfdat, bank, sub.size=NULL, sub.year=NULL, sub.tows=NULL, sub.samples=NULL, 
                        plot = T, seed=1234, direct, direct_fns) {
  source(paste0(direct_fns, "Survey_and_OSAC/shwt.lme.r"))
  source(paste0(direct_fns, "Survey_and_OSAC/condFac.R"))
  source(paste0(direct_fns, "Survey_and_OSAC/shwt.plt1.R"))
  source(paste0(direct_fns, "Survey_and_OSAC/stdts.plt.R"))
  browser()
  if(missing(mwdat) & missing(shfdat)) {
    print("mwdat and shfdat not specified, using pre-loaded mw.dat.all[[bank]] and bank.dat[[bank]] from Survey Summary RData.")
    mwdat <- mw.dat.all[[bank]]
    shfdat <- bank.dat[[bank]]
  }
  
  print("Starting dimensions mwdat")
  print(dim(mwdat))
  
  mwdat$ID <- as.character(mwdat$ID)
  shfdat$ID <- paste0(shfdat$cruise, ".", shfdat$tow)
  
  # drop a proportion of the tows. the un-used tows will be used as a prediction dataset later.
  if(!is.null(sub.tows)) {
    keep <- length(unique(mwdat$ID))*(1-sub.tows)
    
    set.seed(seed)
    keep <- sample(unique(mwdat$ID), keep)
    removed <- mwdat[!mwdat$ID %in% keep,]
    mwdat <- mwdat[mwdat$ID %in% keep,]
    print("sub.tows dimensions mwdat")
    print(dim(mwdat))
  }
  
  # drop a proportion of the samples the un-used samples will be used as a prediction dataset later.
  if(!is.null(sub.samples)) {
    subs <- NULL
    nonsubs <- NULL
    for (i in 1:length(unique(mwdat$ID))){
      sub <- mwdat[mwdat$ID == unique(mwdat$ID)[i],]
      sub$num <- 1:nrow(sub)
      keep2 <- length(sub$num)*(1-sub.samples)
      
      set.seed(seed)
      keep2 <- sample(sub$num, keep2)
      removed2 <- sub[!sub$num %in% keep2,]
      sub <- sub[sub$num %in% keep2,]
      subs <- rbind(subs, sub)
      nonsubs <- rbind(nonsubs, removed2)
    }
    mwdat <- subs
    print("sub.samples dimensions mwdat")
    print(dim(mwdat))
  }
  
  # subset the data based on the size range
  if(!is.null(sub.size)) {
    if(is.na(sub.size[1])) mwdat <- mwdat[mwdat$sh <= sub.size[2],]
    if(is.na(sub.size[2])) mwdat <- mwdat[mwdat$sh >= sub.size[1],]
    if(!is.na(sub.size[1]) & !is.na(sub.size[2])) mwdat <- mwdat[mwdat$sh >= sub.size[1] && mwdat$sh <= sub.size[2],]
    print("sub.size dimensions mwdat")
    print(dim(mwdat))
  }
  
  # subset the data based on the year range
  if(!is.null(sub.year)) {
    if(is.na(sub.year[1])) mwdat <- mwdat[mwdat$year <= sub.year[2],]
    if(is.na(sub.year[2])) mwdat <- mwdat[mwdat$year >= sub.year[1],]
    if(!is.na(sub.year[1]) & !is.na(sub.year[2])) mwdat <- mwdat[mwdat$year >= sub.year[1] && mwdat$year <= sub.year[2],]
    print("sub.year dimensions mwdat")
    print(dim(mwdat))
    }
    
  # Run condition model first because we're going to convert sh next. CFs are predicted for entire tow dataset
  if(bank %in% c("Mid", "Ban", "BanIce", "GBa-Large_core")) condmod <- condFac(na.omit(mwdat),shfdat,model.type='glm',dirct=direct)
  if(!bank %in% c("Mid", "Ban", "BanIce", "GBa-Large_core")) condmod <- condFac(na.omit(mwdat),shfdat,model.type='gam_f',dirct=direct)

  
  # fill in any missing years with NAs
  condmod$CFyrs <- join(condmod$CFyrs, data.frame(year=min(condmod$CFyrs$year):max(condmod$CFyrs$year)), type="right")
  
  #######  

  # Run mwsh model (same for all banks. this is the one done INSIDE condFac normally)
  if(any(mwdat$sh>10)) mwdat$sh <- mwdat$sh/100 # convert sh
  mwshmod <- shwt.lme(mwdat, random.effect='ID', b.par=3, verbose = T)
  
  # SE from mwshmod is in mwshmod$summary$tTable
  # 
  # # predict MWSH relationship for dropped MWSH data
  # if(pred=="dropped") {
  #   run$mwshmod$
  # }
  
  if(plot==T) {
    # labels for plotting
    cf.lab <-expression(paste("CF:",bgroup("(",frac(g,dm^3)   ,")")))
    
    windows(11, 8.5)
    par(mfrow=c(1,2))
    shwt.plt1(mwshmod,lw=3,ht=10,wd=12,cx=1.5,cex.mn = 2,las=1, titl="MW-SH Relationship", 
              axis.cx = 1)#, yl=c(0,90), xl=c(.60,1.90))
    stdts.plt(condmod$CFyrs,y=c('CF'),pch=c(23),col=c('black'), ylab=cf.lab,
              mean.line=T,graphic='none',xlab='Year',ylim=c(4,25),las=1,
              titl = "Condition factor",cex.mn=2,tx.ypos=4, error=T)
  }
  
  return(list(mwshmod = mwshmod, condmod=condmod))
}


## 2: mwsh.survey.extract function
## Use this after running survey summary data with sensitivity=T to extract variance information from survey indices
## borrows from Survey summary figures functions such as survey.ts() to get SE and CI numbers used in plots
## nickname: the nickname of the testing_results Rdata file you want to extract values from
## direct: the directory in which all files are based (e.g. "Y:/Offshore scallop/Assessment/")
## year: the year of the survey data (folder where testing_results found)
## bank: the bank of interest
# example: 
# mwsh.survey.extract(nickname="GBaFinal", direct=direct, direct_fns=direct_fns, year=2019, bank="GBa")
mwsh.survey.extract <- function(nickname, direct, year, bank) {

  # read in data
  load(paste(direct,"Data/Survey_data/" ,year, "/Survey_summary_output/testing_results_", nickname, ".Rdata",sep=""))
  
  # set up naming scheme
  mn.tmp <- c("I", "IR", "IPR")
  cv.names <- paste(mn.tmp,".cv",sep="")
  se.names <- paste(mn.tmp,".se",sep="")
  
  ## biomass survey estimate CV's (stratified where applicable) (not kg per tow)
  CV <- survey.obj[[bank]][[1]][, cv.names]
  
  ## biomass survey estimate error bars (not kg per tow)
  for(i in 1:length(se.names)) survey.obj[[bank]][[1]][,se.names[i]] <- survey.obj[[bank]][[1]][,mn.tmp[i]]*survey.obj[[bank]][[1]][,cv.names[i]]
  SE <- survey.obj[[bank]][[1]][, se.names]
  
  # put them together in a nice little table 
  var.table <- data.frame(cbind(year=survey.obj[[bank]][[1]]$year, CV, SE))
  
  return(var.table) 
}


## 3: mwsh.DDmodel.extract function
## Use this after running Update_function_JAGS to extract variance information from model estimates
## borrows from Update_function_JAGS functions such as biomass.plt() and fit.plt() to get CI numbers used in plots
## nickname: the nickname of the Model_testing_results Rdata file you want to extract values from
## direct: the directory in which all files are based (e.g. "Y:/Offshore scallop/Assessment/")
## year: the year of the model run (folder where Model_results found (not survey year))
## bank: the bank of interest
# example: 
# mwsh.DDmodel.extract(nickname="GBaFinal", direct=direct, direct_fns=direct_fns, year=2019, bank="GBa")

mwsh.DDmodel.extract <- function(nickname, direct, year, bank) {
  
  # read in data
  load(paste(direct,"Data/Model/" ,year, "/", bank, "/Results/Model_testing_results_", nickname, ".Rdata",sep=""))
  load(paste(direct,"Data/Model/",year,"/",bnk,"/Results/Model_results_and_diagnostics_", nickname, ".RData",sep=""))
  
  # set up naming scheme
  mn.tmp <- c("I", "IR", "IPR")
  cv.names <- paste(mn.tmp,".cv",sep="")
  se.names <- paste(mn.tmp,".se",sep="")
  
  ## CI model estimates for FR survey biomass (accounts for catchability)
  CI.lower.survey <- apply(sweep(DD.out[[bank]]$sims.list$B,2,FUN='*',DD.out[[bank]]$median$q), 2, quantile, 0.025)
  CI.upper.survey <- apply(sweep(DD.out[[bank]]$sims.list$B,2,FUN='*',DD.out[[bank]]$median$q), 2, quantile, 0.975)

  ## CI model estimates for FR biomass
  CI.lower.model <- apply(DD.out[[bank]]$sims.list$B.p, 2, quantile, 0.025)
  CI.upper.model <- apply(DD.out[[bank]]$sims.list$B, 2, quantile, 0.975)
  
  ## CI model estimates for FR biomass projection - NOTE this is only for 80% of data, as in biomass.plt.R
  TACI<-which(DD.out[[bank]]$data$C.p == proj.catch[[bank]] + TACi[[bank]])
  CI.lower.proj <- quantile(DD.out[[bank]]$sims.list$B.p[,TACI], 0.1)
  CI.upper.proj <- quantile(DD.out[[bank]]$sims.list$B.p[,TACI], 0.9)
  
  # put them together in a nice little table
  var.table <- data.frame(cbind(year=DD.out[[bank]]$data$year, CI.lower.survey, CI.upper.survey, CI.lower.model, CI.upper.model))
  
  return(var.table) 
}

