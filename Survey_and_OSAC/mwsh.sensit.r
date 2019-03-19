## MWSH Sensitivity Analysis

## Use this function to test the effects of different subsets of MWSH data on the Condition and Biomass Estimates using their variance.

## sub.size: lower and upper bounds of size subset for MWSH data. Must enter NA if no number provided. c(89, NA) means keep everything equal to or above 89mm.
## sub.year: lower and upper bounds of year subset for MWSH data. Must enter NA if no number provided. c(NA, 2008) means keep everything equal to or below 2008.
## sub.tows
## sub.samples
## bank: matches Survey Summary bank options. Must enter as c(a, b, c, d) (not "all")
## mwdat: name of dataframe with meat weight data
## shfdat: name of dataframe with all tow data
## direct: specify the location of the Assessment_fns folder

### drop 20% of tows - done
### drop 20% of samples from all tows - done
### need to predict on the dropped data?
### propagate the error into the Biomass estimate!

mwdat <- mwger_0 
shfdat <- shfger_0

run <- mwsh.sensit(mwdat=mwdat, shfdat=shfdat, bank="Ger", plot=T, 
                   sub.size=NULL, sub.year=NULL, sub.tows=NULL, sub.samples=NULL, 
                   direct=direct, seed=1234)

mwsh.sensit <- function(mwdat, shfdat, bank, sub.size=NULL, sub.year=NULL, sub.tows=NULL, sub.samples=NULL, plot = T, seed=1234, direct=direct) {
  source("C:/Documents/Offshore scallop/Assessment/Assessment_fns/Survey_and_OSAC/shwt.lme.r")
  source("C:/Documents/Offshore scallop/Assessment/Assessment_fns/Survey_and_OSAC/condFac.R")
  source("C:/Documents/Offshore scallop/Assessment/Assessment_fns/Survey_and_OSAC/shwt.plt1.R")
  source("C:/Documents/Offshore scallop/Assessment/Assessment_fns/Survey_and_OSAC/stdts.plt.R")
  
  if(missing(mwdat) & missing(shfdat)) {
    print("mwdat and shfdat not specified, using pre-loaded mw.dat.all[[bank]] and bank.dat[[bank]] from Survey Summary RData.")
    mwdat <- mw.dat.all[[bank]]
    shfdat <- bank.dat[[bank]]
  }
  
  print("Starting dimensions mwdat")
  print(dim(mwdat))
  
  mwdat$ID <- as.character(mwdat$ID)
  
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
  
  # drop a proportion of the tows. the un-used tows will be used as a prediction dataset later.
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
    
  # Run condition model first because we're going to convert sh next.
  if(bank %in% c("Mid", "Ban", "GBa-Large_core")) condmod <- condFac(na.omit(mwdat),shfdat,model.type='glm',dirct=direct)
  if(!bank %in% c("Mid", "Ban", "GBa-Large_core")) condmod <- condFac(na.omit(mwdat),shfdat,model.type='gam_f',dirct=direct)
  
  # fill in any missing years with NAs
  condmod$CFyrs <- join(condmod$CFyrs, data.frame(year=min(condmod$CFyrs$year):max(condmod$CFyrs$year)), type="right")
  
  #######  

  # Run mwsh model (same for all banks. this is the one done INSIDE condFac normally)
  mwdat$sh <- mwdat$sh/100 # convert sh
  mwshmod <- shwt.lme(mwdat, random.effect='ID', b.par=3, verbose = T)

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
