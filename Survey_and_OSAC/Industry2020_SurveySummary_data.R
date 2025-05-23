### yr: current year
### survey.year: year of survey
### surveydata: dataframe containing all SHF data for survey (similar to all.surv.dat from SurveySummary_data, but only for BanIce)
### meatweightdata: dataframe containing all MW data for survey
### positionsdata: dataframe containing positions for MW data
### commercial sampling: should commercial samples be included (T or F)


## Heads up that DK made some changes to 'direct_fns which should be silent.  Note that 'direct' is used in the body of this function but
## not specified as an arguement so that is reliaing on the direct objecting being in your environment.  I've left it be as it obviously worked
## when it had to...

Industry2020_SurveySummary_data <- function(yr=yr, survey.year=survey.year, years=years, surveydata=IndSurvey2020, 
                                            commercialsampling=commercialsampling, meatweightdata=Ind_MW_new, mwsh.test=F,
                                            direct_fns,
                                            bins=NULL, RS=NULL, CS=NULL, survey.bound.polys=NULL, survey.detail.polys=NULL){
  
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/mwsh.sensit.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/surv.by.tow.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/simple.surv.r")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
} # end for(un in funs)
  } # end  if(missing(direct_fns))
  
  if(!missing(direct_fns))
  {
  source(paste0(direct_fns, "Survey_and_OSAC/mwsh.sensit.R"))
  source(paste0(direct_fns, "Survey_and_OSAC/surv.by.tow.r"))
  source(paste0(direct_fns, "Survey_and_OSAC/simple.surv.R"))
  } #end if(!missing(direct_fns))
  
  require(plyr)
  bnk <- unique(surveydata$bank)
  bank.4.spatial <- bnk
  
  # Now we can set up our more detailed SHF bins as well
  if(bins == "bank_default") bin <- c(50,70,RS[length(RS)],CS[length(CS)],120) 
  # If you have specified the bins then do this...
  if(bins != "bank_default") bin <- bins 
  
  #Initialize some variables
  bank.dat <- NULL
  strata.mis.match <- NULL
  mw <- NULL
  SpatHtWt.fit <- NULL
  HtWt.fit <- NULL
  mw.dat.all <- NULL
  cf.data <- NULL
  surv.dat <- NULL
  surv.Clap <-NULL
  surv.Live <- NULL
  surv.Clap.from2018 <- NULL
  surv.Clap.from2019 <- NULL
  surv.Live.from2018 <- NULL
  surv.Live.from2019 <- NULL
  survey.obj <- NULL
  clap.survey.obj <- NULL
  SS.summary <- NULL
  SHF.summary <- NULL
  CF.current <- NULL
  #tow.dis <- NULL
  seedbox.obj <- NULL
  lined.survey.obj <- NULL
  merged.survey.obj <- NULL
  pot.grow <- NULL
  survey.strata.table <- NULL
  detail.surv.poly <- NULL
  bound.surv.poly <- NULL
  
  # put surveydata into bank.dat. Note, this may already contain calculated values.
  bank.dat[[bnk]] <- surveydata
  
  ########## SPATIAL ######################
  
  # Set up the survey polys
  bound.poly.surv <- subset(survey.bound.polys,label==bnk & startyear == max(survey.bound.polys$startyear[survey.bound.polys$label==bnk])) 
  attr(bound.poly.surv,"projection")<-"LL" 
  
  #Read4 Read drooped #Detailed Survey polygons
  detail.poly.surv <- subset(survey.detail.polys,label=="Ban")
  attr(detail.poly.surv,"projection")<-"LL"
  
  # Save the survey strata table so we have it for later, this is mostly needed for when we have user defined areas carved out.
  survey.strata.table[[bnk]] <- NULL
  detail.surv.poly[[bnk]] <- detail.poly.surv
  bound.surv.poly[[bnk]]<- bound.poly.surv
  
  # Give each tow a unique identifier.
  bank.dat[[bnk]]$ID<-paste(bank.dat[[bnk]]$year,bank.dat[[bnk]]$tow,sep='.')
  
  
  ############# SAMPLING DATA #####################
  
  if(bnk %in% c("GBa", "GBb")){
    ################ current year MW-SH model ################
    mw.dm <- meatweightdata
    # convert SH for MW-SH fit
    if(any(mw.dm$sh > 10)) 
      mw.dm$sh<-mw.dm$sh/100
    
    # we only have BanIce mw data from 2012 on, so no need to use import.hyd.dat for BanIce. Furthermore, we shouldn't need to remove tow ID 0's since the 
    # commercial surveys are over, but just in case.
    if(commercialsampling==F) mw.dm <- subset(mw.dm, tow>0)
    
    # move the full data (all years) to mw.dat.all
    mw.dat.all[[bnk]] <- mw.dm
    
    # now run the MWSH model for the current year. USE ALL 2020 TOWS FOR THIS
    SpatHtWt.fit[[bnk]] <- shwt.lme(mw.dm[mw.dm$year==yr,],random.effect='tow',b.par=3)
    
    # depth.f to metres?
    if("depth.f" %in% names(mw.dat.all[[bnk]])) mw.dat.all[[bnk]]$depth <- mw.dat.all[[bnk]]$depth.f * 1.8288
    
    mw.dat.all[[bnk]]$ID <-paste(mw.dat.all[[bnk]]$year,mw.dat.all[[bnk]]$tow,sep='.')
    
    # write this now
    if(file.exists(paste0(direct,"Data/Survey_data/",yr,"/Spring/",bank.4.spatial))==F) {
      dir.create(path = paste0(direct,"Data/Survey_data/",yr,"/Spring/",bank.4.spatial))}
    
    if(!any(mw.dat.all[[bnk]]$sh > 10)) mw.dat.all[[bnk]]$sh <- mw.dat.all[[bnk]]$sh*100
    write.csv(mw.dat.all[[bnk]],paste0(direct,"Data/Survey_data/",yr,"/Spring/",bank.4.spatial,"/mw_Data.csv"),row.names=F) # Write1
    
    if(any(mw.dat.all[[bnk]]$sh > 10)) mw.dat.all[[bnk]]$sh <- mw.dat.all[[bnk]]$sh/100
    
    ############### Annual Condition Model #####################
    
    mw.dat.all[[bnk]]$year[!is.na(mw.dat.all[[bnk]]$pastyear)] <- paste0(20, ".", substr(mw.dat.all[[bnk]]$pastyear[!is.na(mw.dat.all[[bnk]]$pastyear)], 3, 4))
    
    # the years in the data, sorted. and set up the prediction objs. 
    yrs<-sort(unique(mw.dat.all[[bnk]]$year))
    pred.loc <- NULL
    # predict on mean location for entire 2020 survey
    pred.loc[["depth"]] <- mean(subset(mw.dat.all[[bnk]], year %in% c("20.19", "20.18"))$depth,na.rm=T)
    pred.loc[["lat"]] <- mean(subset(mw.dat.all[[bnk]], year %in% c("20.19", "20.18"))$lat,na.rm=T)
    pred.loc[["lon"]] <- mean(subset(mw.dat.all[[bnk]], year %in% c("20.19", "20.18"))$lon,na.rm=T)
    message(paste0("Using mean location in 2020 (all tows) for condition prediction."))
    # now run the model to predict condition for all years with MW data using the model that includes this year's data
    HtWt.fit[[bnk]] <- shwt.lme(mw.dat.all[[bnk]],random.effect='ID',b.par=3, verbose=F)
    
    # Merge the raw data with the model fit, just keep the first sample for each tow in which we have multiple samples.
    CF.data<-merge(mw.dat.all[[bnk]][!duplicated(mw.dat.all[[bnk]]$ID),c('ID','lon','lat','year','depth','tow')],HtWt.fit[[bnk]]$fit,all=T)
    # Make sure the names are what we want
    names(CF.data)<-c('ID','lon','lat','year','depth','tow','CF')
    
    # Predict condition factor over bank using one of 5 models.
    # This model assumes CF varies only with depth and year, Gaussian and linear relationship, no random effects (year might be best treated as such)
    if(length(unique(CF.data$year))>1) 
      CF.fit<-glm(CF~depth+as.factor(year),data=CF.data)
    if(length(unique(CF.data$year))==1) {
      CF.fit<-glm(CF~depth,data=CF.data)
      message("careful, condition is being modelled with only one year of data")
    }
    
    # Make a new object to build predictions from, previously we were using predictions for mean data for our data
    # But those change every year, this has been revised to predict on the same location every year.
    CFyrs<-data.frame(year=yrs,depth=pred.loc[["depth"]],lon=pred.loc[["lon"]],lat=pred.loc[["lat"]])
    # Now do the prediction
    CFyrs$CF=predict(CF.fit,CFyrs)
    
    
    # Give object a new name
    bank.dat[[bnk]]$year[!is.na(bank.dat[[bnk]]$pastyear)] <- paste0(20, ".", substr(bank.dat[[bnk]]$pastyear[!is.na(bank.dat[[bnk]]$pastyear)], 3, 4))
    pre.dat<-bank.dat[[bnk]]
    
    # Make predictions based on model.
    pred.dat <- bank.dat[[bnk]]
    
    pred.dat$CF<- predict(CF.fit,pre.dat)
    if(any(pred.dat$CF <0, na.rm=T)) message("You have negative condition factor predictions. Maybe you should re-think your model?")
    cf.data[[bnk]] <- list(CFyrs=CFyrs,CF.data=CF.data, HtWt.fit=HtWt.fit, CF.fit=CF.fit,pred.dat=pred.dat)	
    
    if(mwsh.test == T) {
      
      mwsh.test.dat <- mw.dat.all[[bnk]]
      if(!any(mwsh.test.dat$sh > 10)) mwsh.test.dat$sh <- mwsh.test.dat$sh * 100
      mwshtest <- mwsh.sensit(mwdat=na.omit(mwsh.test.dat[, !names(mwsh.test.dat) %in% c("month", "species")]), shfdat=bank.dat[[bnk]], bank=bnk, plot=F, 
                              sub.size=NULL, sub.year=c(NA, 2012), sub.tows=NULL, sub.samples=NULL, 
                              direct=direct,  seed=1234)
      cf.data[[bnk]] <- mwshtest$condmod
    }
    
    # Output the predictions for the bank
    surv.dat[[bnk]] <- cf.data[[bnk]]$pred.dat
    surv.dat[[bnk]] <- full_join(bank.dat[[bnk]], surv.dat[[bnk]])
    # Pull out the ID and condition factor
    tmp.dat<- cf.data[[bnk]]$CF.data[, c("ID","CF")]
    # Rename CF to CFh
    names(tmp.dat)[2]<-"CFh"
    # merge the two data sets, keeping all x values
    surv.dat[[bnk]]<-merge(surv.dat[[bnk]],tmp.dat,all.x=T)
    # Replace any NA's in CFh with the original Condition Factor.
    surv.dat[[bnk]]$CFh[is.na(surv.dat[[bnk]]$CFh)]<-surv.dat[[bnk]]$CF[is.na(surv.dat[[bnk]]$CFh)]

    ######## Calculate biomasses by tow ###########################################################
    surv.dat[[bnk]]$year[surv.dat[[bnk]]$year %in% c("20.19", "20.18")] <- 2020
    surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS,type = "ALL",mw.par = "CF",user.bins = bin)
    
  } # end of the GBa/GBb loop
  
  if(bnk=="BBn"){ 
    surv.dat[[bnk]] <- bank.dat[[bnk]]
    surv.dat[[bnk]] <- surv.by.tow(surv.dat[[bnk]], years, pre.ht=RS, rec.ht=CS,type = "N", user.bins = bin)
  }
  ######## Clappers vs live #################################################################
  
  # Subset the data into the clappers (dead) and live scallops.  Use only random survey tows for Clappers...
  # For GB spring the survey of interest are the comparative tows...
  surv.Clap[[bnk]]<-subset(surv.dat[[bnk]],state=='dead')
  surv.Live[[bnk]]<-subset(surv.dat[[bnk]],state=='live')
  # surv.Clap.from2018[[bnk]]<-subset(surv.dat[[bnk]],state=='dead' & pastyear==2018)
  # surv.Clap.from2019[[bnk]]<-subset(surv.dat[[bnk]],state=='dead' & pastyear==2019)
  # surv.Live.from2018[[bnk]]<-subset(surv.dat[[bnk]],state=='live' & pastyear==2018)
  # surv.Live.from2019[[bnk]]<-subset(surv.dat[[bnk]],state=='live' & pastyear==2019)
  
  surv.Clap[[bnk]]$year[!is.na(surv.Clap[[bnk]]$pastyear)] <- paste0(20, ".", substr(surv.Clap[[bnk]]$pastyear[!is.na(surv.Clap[[bnk]]$pastyear)], 3, 4))
  surv.Live[[bnk]]$year[!is.na(surv.Live[[bnk]]$pastyear)] <- paste0(20, ".", substr(surv.Live[[bnk]]$pastyear[!is.na(surv.Live[[bnk]]$pastyear)], 3, 4))
  
  # Clappers the banks for each size class
  # Do the same for all the tows...
  surv.Clap[[bnk]]$clap.prop<-surv.Clap[[bnk]]$tot/(surv.Live[[bnk]]$tot+surv.Clap[[bnk]]$tot)*100
  surv.Clap[[bnk]]$clap.prop[is.na(surv.Clap[[bnk]]$clap.prop)]<-0
  surv.Clap[[bnk]]$clap.propCom<-surv.Clap[[bnk]]$com/(surv.Live[[bnk]]$com+surv.Clap[[bnk]]$com)*100
  surv.Clap[[bnk]]$clap.propCom[is.na(surv.Clap[[bnk]]$clap.propCom)]<-0
  surv.Clap[[bnk]]$clap.propRec<-surv.Clap[[bnk]]$rec/(surv.Live[[bnk]]$rec+surv.Clap[[bnk]]$rec)*100
  surv.Clap[[bnk]]$clap.propRec[is.na(surv.Clap[[bnk]]$clap.propRec)]<-0
  surv.Clap[[bnk]]$clap.propPre<-surv.Clap[[bnk]]$pre/(surv.Live[[bnk]]$pre+surv.Clap[[bnk]]$pre)*100
  surv.Clap[[bnk]]$clap.propPre[is.na(surv.Clap[[bnk]]$clap.propPre)]<-0
  surv.Clap[[bnk]]$clap.prop[is.na(surv.Clap[[bnk]]$clap.prop)]<-0
  browser()
  ########### Make survey.obj (mimicking Middle bank method) ###########################
  if(bnk=="BBn") {
    survey.obj[[bnk]] <- simple.surv(select(surv.Live[[bnk]], -pastyear),years=sort(as.numeric(unique(surv.Live[[bnk]]$year))),user.bins=bin, B=F)
    clap.survey.obj[[bnk]]<-simple.surv(select(surv.Clap[[bnk]], -pastyear),years=sort(as.numeric(unique(surv.Clap[[bnk]]$year))), B=F)
  }
  if(!bnk=="BBn") {
    survey.obj[[bnk]] <- simple.surv(select(surv.Live[[bnk]], -pastyear),years=sort(as.numeric(unique(surv.Live[[bnk]]$year))),user.bins=bin, B=T)
    survey.obj[[bnk]][[1]]$CF <- sapply(1:length(sort(as.numeric(unique(surv.Live[[bnk]]$year)))),
                                        function(x){with(subset(select(surv.Live[[bnk]], -pastyear),year == sort(as.numeric(unique(surv.Live[[bnk]]$year)))[x]),
                                                         weighted.mean(CF,com.bm,na.rm=T))})
    clap.survey.obj[[bnk]]<-simple.surv(select(surv.Clap[[bnk]], -pastyear),years=sort(as.numeric(unique(surv.Live[[bnk]]$year))), B=T)
  }
  
  # add in the RS and CS sizes
  survey.obj[[bnk]][[1]]$CS <- CS
  survey.obj[[bnk]][[1]]$RS <- RS
  clap.survey.obj[[bnk]][[1]]$CS <- CS
  clap.survey.obj[[bnk]][[1]]$RS <- RS
  
  
  ############ Make the summaries ##################################################
  
  SS.summary[[bnk]] <- survey.obj[[bnk]][[1]]
  SS.summary[[bnk]]$bank <- bank.4.spatial
  # Same for the SHF data.
  SHF.summary[[bnk]] <- as.data.frame(cbind(survey.obj[[bnk]][[1]]$year,survey.obj[[bnk]][[2]]$n.yst))
  SHF.summary[[bnk]]$bank <- bank.4.spatial
  
  if(!bnk=="BBn") {
    CF.current[[bnk]]<-na.omit(merge(unique(subset(bank.dat[[bnk]],bank == bnk & year%in%c("20.19", "20.18"),c('tow','lon','lat'))),
                                     SpatHtWt.fit[[bnk]]$fit))
    names(CF.current[[bnk]])[4]<-"CF"
    CF.current[[bnk]]<-merge(CF.current[[bnk]],
                             subset(surv.Live[[bnk]],
                                    year%in%c("20.19", "20.18", "2020"),
                                    c('year','tow','lon','lat',"com","com.bm"))
                             )
    
    # Meat count per 500g
    CF.current[[bnk]]$meat.count <- 0.5/(CF.current[[bnk]]$com.bm/CF.current[[bnk]]$com)
    if(any(CF.current[[bnk]]$meat.count < 0, na.rm=T)) message("uhoh, you have negative meat counts...")

    surv.Live[[bnk]]$year[!is.na(surv.Live[[bnk]]$pastyear)] <- "2020"
    pot.grow[[bnk]] <- grow.pot(dat= surv.Live[[bnk]],mwsh.fit = SpatHtWt.fit[[bnk]],bank = bank.4.spatial)
  }
  
  ############# Return all objects created here to the main SurveySummary_data script. 
  returnlist <- list(bank.dat=bank.dat[[bnk]],
                     mw.dat.all=mw.dat.all[[bnk]],
                     SpatHtWt.fit=SpatHtWt.fit[[bnk]],
                     cf.data=cf.data[[bnk]],
                     surv.dat=surv.dat[[bnk]],
                     surv.Clap=surv.Clap[[bnk]],
                     surv.Live=surv.Live[[bnk]],
                     survey.obj=survey.obj[[bnk]],
                     clap.survey.obj=clap.survey.obj[[bnk]],
                     SS.summary=SS.summary[[bnk]],
                     SHF.summary=SHF.summary[[bnk]],
                     CF.current=CF.current[[bnk]],
                     seedbox.obj=seedbox.obj[[bnk]],
                     lined.survey.obj=lined.survey.obj[[bnk]],
                     merged.survey.obj=merged.survey.obj[[bnk]],
                     pot.grow=pot.grow[[bnk]],
                     survey.strata.table=survey.strata.table[[bnk]],
                     detail.surv.poly=detail.surv.poly[[bnk]],
                     bound.surv.poly=bound.surv.poly[[bnk]])
  return(returnlist)
  
}
