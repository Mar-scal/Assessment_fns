####  Commented and checked by DK starting on July 28, 2015. This function calculates the spatial and/or annual
###  variability in condition factor.
# Update history
#Commented, checked  and revised by DK March 31, 2016
# DK update Jan 2021, switched default behaviour to grab function from github.  Change should have no effect on old code.
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary_data.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    1:  shwt.lme.r
#    2:  shwt.glmer.r
#      
##
###############################################################################################################

###############################################################################################################
#Arguments
# wgt.dat:     The data to model
# pred.dat:    Generate model predictions.  Default is NULL.  Would need to be a data frame of appropriate (depth, year and/or lon/lat) data to predict on.
# b.par:       Do we want to use the data/model to 'estimate' the allometric relationship or force it to be a specific number (i.e. 3).  
#              Default is b.par=3
# model.type:  What type of model should we fit.  Default ='glm', Options include: 
#             "glm" which has covariates depth and year, Gaussian family and identity link.
#             "gam_d" which has both depth and year fit as thin plate regression splines, Gaussian family and identity link.
#             "gam_s" which has location (lat/lon), depth, and year fit as thin plate regression splines, Gaussian family and identity link.
#             "gam_f" which has location (lat/lon) and depth fit as thin plate regression splines, year is a factor, Gaussian family and identity link.
#             "gam_sy" which has location (lat/lon) by year and year fit as thin plate regression splines, Gaussian family and identity link.
# plt:         Make a plot.  (T/F) defalut = F
# y2:          Use alternate year data.  (T/F) default = F
# ADJ_depth:   Use column ADJ_depth.  (T/F) default = F
#pred.loc:     The depth, lat, and lon that you are predicting on (GAM selection dependent).  These should be user specified
#              although I do have a backup method set up for these.
#dirct:        The directory in which the shwt.lme.r function is located.  Default is now missing and pulls from github (DK Jan 2021)
#
###############################################################################################################

condFac<-function(wgt.dat,pred.dat=NULL,model.type='glm',y2=F,ADJ_depth=F,pred.loc=NULL,b.par=3,plt=F,
                  dirct, error=T)
{
  require(mgcv)  || stop("Install mgcv package needed for gam's")
  require(lme4)  || stop("Install lme4 package needed for glmer's")
  require(arm)  || stop("Install arm package needed for glmer's se.ranef")
  require(dplyr)  || stop("Install dplyr package")
  #Source1 Load in our missed effects model.
  if(missing(dirct))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/shwt.lme.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/shwt.glmer.r")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
} # end for(un in funs)
  }# end  if(missing(direct_fns))
  
  # If you supply dirct...
  if(!missing(dirct)) {
    source(paste(dirct,"/Survey_and_OSAC/shwt.lme.r",sep=""),local=T)	
    source(paste(dirct,"/Survey_and_OSAC/shwt.glmer.r",sep=""),local=T)	
  }
  
  # the years in the data, sorted
  yrs<-sort(unique(wgt.dat$year))
  # Which depth should we use, this options uses depth in meters
  if(ADJ_depth  == T) wgt.dat$depth<-wgt.dat$ADJ_depth
  if(y2 ==T && model.type=='gam_s') wgt.dat$year<-wgt.dat$y2
  # If preds is not specified the predictions will be based the mean values of the data between 2005 and 2014
  # As we want to pick the same number every year, I would prefer that these are user inputs but this is a decent backup plan
  if(is.null(pred.loc) == T & !model.type == "glmer")
  {
    print("Careful, you didn't specify the location for prediction of CF so I have picked mean depth, lat, and lon between 2005 and 2014 be sure this is how this has been done in the past!")
    pred.loc[["depth"]] <- mean(subset(wgt.dat, year >=2005 & year <2015)$depth,na.rm=T)
    pred.loc[["lat"]] <- mean(subset(wgt.dat, year >=2005 & year <2015)$lat,na.rm=T)
    pred.loc[["lon"]] <- mean(subset(wgt.dat, year >=2005 & year <2015)$lon,na.rm=T)
    if(all(unique(wgt.dat$year)>2015) | all(unique(wgt.dat$year)< 2005)) {
      message("wgt.dat years are not between 2005 and 2015, use max year instead (e.g. for BanIce)")
      pred.loc[["depth"]] <- mean(subset(wgt.dat, year ==max(wgt.dat$year))$depth,na.rm=T)
      pred.loc[["lat"]] <- mean(subset(wgt.dat, year==max(wgt.dat$year))$lat,na.rm=T)
      pred.loc[["lon"]] <- mean(subset(wgt.dat, year==max(wgt.dat$year))$lon,na.rm=T)
    }
    
  } # end if(pred.loc == NULL)
  
  # Convert shell heights to decimeters
  wgt.dat$sh<-wgt.dat$sh/100
  
  # Calculate the meat weight shell height relationship, remember if b.par = 3 this assumes an allometric realtionship.
  # Notice that we use a different random effect here, it is ID not tow, this is done since we may have the same tow # in different years.

  if(!model.type=="glmer") {
    
    SpatHtWt.fit<-shwt.lme(wgt.dat,random.effect='ID',b.par=b.par,verbose=F)
    
    # Merge the raw data with the model fit, just keep the first sample for each tow in which we have multiple samples.
    CF.data<-merge(wgt.dat[!duplicated(wgt.dat$ID),c('ID','lon','lat','year','depth','tow')],SpatHtWt.fit$fit,all=T)
    # Make sure the names are what we want
    names(CF.data)<-c('ID','lon','lat','year','depth','tow','CF')
    
    # Predict condition factor over bank using one of 5 models.
    # This model assumes CF varies only with depth and year, Gaussian and linear relationship, no random effects (year might be best treated as such)
    if(model.type=='glm' & length(unique(CF.data$year))>1)CF.fit<-glm(CF~depth+as.factor(year),data=CF.data)
    if(model.type=='glm' & length(unique(CF.data$year))==1)CF.fit<-glm(CF~depth,data=CF.data)
    # Here we fit has both depth and year fit as thin plate regression splines, Gaussian family and identity link.
    if(model.type=='gam_d')CF.fit<-gam(CF~s(depth)+s(year),data=CF.data)
    # gam_s has location (lat/lon), depth, and year fit as thin plate regression splines, Gaussian family and identity link.
    if(model.type=='gam_s')CF.fit<-gam(CF~s(lon,lat)+s(depth)+s(year),data=CF.data)
    # gam_f which has location (lat/lon) and depth fit as thin plate regression splines, year is a factor, Gaussian family and identity link.
    if(model.type=='gam_f')CF.fit<-gam(CF~s(lon,lat)+s(depth)+as.factor(year),data=CF.data)
    # gam_sy which has location (lat/lon) by year and year fit as thin plate regression splines, Gaussian family and identity link.
    if(model.type=='gam_sy')CF.fit<-gam(CF~s(lon,lat,by=as.factor(year))+s(year),data=CF.data)
    
    # Make a new object to build predictions from, previously we were using predictions for mean data for our data
    # But those change every year, this has been revised to predict on the same location every year.
    CFyrs<-data.frame(year=yrs,depth=pred.loc[["depth"]],lon=pred.loc[["lon"]],lat=pred.loc[["lat"]])
    # Now do the prediction
    CFyrs$CF <- predict(CF.fit,CFyrs, se=T)$fit
    if(error==T) CFyrs$CFse.fit <- predict(CF.fit, CFyrs, se=T)$se.fit
    # If we want to make the plot and our model is a glm do this.
    if(plt == T && model.type=='glm') plot(CF~year,CFyrs,type='o',pch=16)
    # If we have a gam model and want a plot do this.
    if(plt == T && model.type!='glm')
    {
      par(mfrow=c(2,2))
      plot(CF.fit,T,F,T)
    } # end if(plt == T && model.type!='glm')
    weight.matrix <- NULL
  }
  
  
  if(model.type=="glmer") {
    if(all(wgt.dat$sh<3)) wgt.dat$sh <- wgt.dat$sh*100 
    
    # let's only model sh >= 65 mm here, that gets rid of just 150 shells (going to 70 drops us by 750 which seems like too much)
    wgt.dat <- wgt.dat %>% dplyr::filter(sh >= 65)
    # centering this makes no difference for our predictions but if make 100 = 0, then our intercept is our condition
    # which may come in handy later (or may not)
    sh.cond <- 100
    wgt.dat$log.sh.cen <- log(wgt.dat$sh) - log(sh.cond)
    # Same idea here, if we center depth on the bank median, then our intercept is the condition at the median
    # Bank depth, which should be our measure of condition
    #Going to say median depth of the survey tows between 2010 and 2022 see above
    # For BBn this is 75 meters, which makes loads of sense. So Center the depth at the bank mean
    med.depth <- pred.dat %>% dplyr::filter(year %in% 2010:2022) %>% dplyr::summarise(med = median(depth,na.rm=T)) %>% signif(digits=2)
    wgt.dat$depth.cen <- wgt.dat$depth - med.depth$med
    
    # # Also need to update the survey data. 
    pred.dat$depth.cen <- pred.dat$depth - med.depth$med
    
    # We need tow to be unique by year, ooh, so we have the 0 tow problem here, need
    # to make up something that gets us a unique tow for the 0s..
    wgt.dat$temp_ID <- as.factor(paste(wgt.dat$year,wgt.dat$lon,wgt.dat$lat,sep = "_"))
    unique_tows <- unique(wgt.dat$temp_ID)
    num.levels <- paste0("x",1:length(unique_tows))
    wgt.dat$new_ID <- factor(wgt.dat$temp_ID,levels = unique_tows,labels=num.levels)
    wgt.dat$new_ID <- as.character(wgt.dat$new_ID)
    # Use the survey tow ID's where they exist and use the fake new_ID's where they don't.
    wgt.dat$new_ID[wgt.dat$tow != "0"] <- wgt.dat$tow[wgt.dat$tow != "0"]
    # so the new_ID column will contain the tow number if it's from the survey, and "x___" if it's from commercial samples. 
    
    # 1991  only has 3 tows so is problematic for many of our models, so let's start in 1992, if we want we can
    # start in 1994 as that's the first year of the model.
    yrs <-sort(unique(wgt.dat$year))
    ntows <- wgt.dat %>% dplyr::group_by(year) %>% dplyr::summarize(ntow=length(unique(tow)))
    yrs <- ntows$year[ntows$ntow>1]
    yrs <- yrs[yrs > 1991]
    
    SpatHtWt.fit <- shwt.glmer(wgt.dat, yrs) # list object with model and residuals for each year
  }
  
  if(is.null(pred.dat) & model.type=="glmer") stop("The glmer model requires that you provide a prediction dataset. This is normally bank.dat for Survey summary.")
  
  # If we want to make predictions on some new data...
  if(!is.null(pred.dat) ==T)
  {
    if(length(pred.dat$year[!pred.dat$year %in% yrs]) >0 ) {
      message("The following years are survey years without sampling data. Make sure they are not plotted in biomass figures or condition figures because these are very uncertain estimates!")
      print(unique(pred.dat$year[!pred.dat$year %in% yrs]))
    }
    
    # Which depth are we using, this would be making sure we are using meters.
    if(ADJ_depth==T) pred.dat$depth<-pred.dat$ADJ_depth
    # Give object a new name
    pre.dat<-as.data.frame(pred.dat)
    # Make sure we have data for a year, if we don't put in the minimum year as the year.
    # if(sum(!unique(pre.dat$year) %in% yrs) > 0) pre.dat$year[!pre.dat$year %in% yrs]<-min(yrs)
    # above disappears because we don't want to predict for years without sampling data!
    
    if(!model.type=="glmer") {# Make predictions based on model.
      pred.dat$CF<- predict(CF.fit,pre.dat)
      pred.dat$CFse.fit <- predict(CF.fit,pre.dat, se=T)$se.fit
    }
    
    if(model.type=="glmer"){
      log.sh.cen <- log(seq(2.5, 197.5, by = 5)) - log(100) #each shell height bin to predict on, making 0 being 100 mm
      
      # Initialize some stuff
      n.yrs <- length(yrs)
      mw.res.t <- NULL
      CFtow <- NULL
      cond.pred <- NULL
      all.coef <- NULL
      mwsh.curve <- NULL
      # Takes about 1 second per year
      for(i in 1:n.yrs)
      {
        mw.dat <- wgt.dat %>% dplyr::filter(year== yrs[i])
        s.dat <- pre.dat %>% dplyr::filter(year == yrs[i])
        ntows <- length(unique(mw.dat$tow))
        mod.r <- SpatHtWt.fit$mod.res[[as.character(yrs[i])]]
        # make places for predictions to go:
        temp <- NULL
        CF<-NULL
        # Now try and do the predictions
        
        # First, by tow:
        # note that we are using live and dead rows for the prediction data, so the random.pred and fixed.pred values are 
        # NOT tow numbers - they are row numbers in the s.dat object (which is typically bank.dat if arriving here via Survey Summary code)
        #get IDs for the sampled tows. Use the random effects for those.
        if(ntows>4){
          #get IDs for the sampled tows. Use the random effects for those.
          random.pred <- (1:nrow(s.dat))[is.element(s.dat$tow,unique(mw.dat$new_ID))]
          #get IDs for the unsampled tows. Use fixed effects for IDs that weren't sampled for meat weight shell height
          fixed.pred <- (1:nrow(s.dat))[!is.element(s.dat$tow,unique(mw.dat$new_ID))]
          
          for(j in random.pred)
          {
            tempj <- cbind(data.frame(tow=s.dat$tow[j], depth.cen=s.dat$depth.cen[j], bin=paste0("h", seq(5,200,5))),
                           pred=as.vector(predict(object=mod.r,newdata=data.frame(log.sh.cen=log.sh.cen,
                                                                                  depth.cen=rep(s.dat$depth.cen[j] ,40),
                                                                                  new_ID=s.dat$tow[j]),
                                                  re.form=NULL,type="response"))) %>%
              tidyr::pivot_wider(values_from = "pred", names_from = "bin", id_cols = c("tow", "depth.cen"))
            temp <- rbind(temp, tempj)
            
            CFj <- cbind(data.frame(tow=s.dat$tow[j], depth.cen=s.dat$depth.cen[j]),
                         CF=as.vector(predict(object = mod.r,newdata=data.frame(log.sh.cen=0,
                                                                                depth.cen=s.dat$depth.cen[j],
                                                                                new_ID=s.dat$tow[j]),
                                              re.form=NULL,type="response")))
            CF <- rbind(CF, CFj)
          } # end the random loop
          
          #Predict using fixed effects for IDs that weren't sampled for meat weight shell height
          for(j in fixed.pred)
          {
            tempj <- cbind(data.frame(tow=s.dat$tow[j], depth.cen=s.dat$depth.cen[j], bin=paste0("h", seq(5,200,5))),
                           pred=as.vector(predict(object=mod.r,newdata=data.frame(log.sh.cen=log.sh.cen,
                                                                                  depth.cen=rep(s.dat$depth.cen[j] ,40)),
                                                  re.form=~0,type="response"))) %>%
              tidyr::pivot_wider(values_from = "pred", names_from = "bin", id_cols = c("tow", "depth.cen"))
            temp <- rbind(temp, tempj)
            
            CFj <- cbind(data.frame(tow=s.dat$tow[j], depth.cen=s.dat$depth.cen[j]),
                         CF=as.vector(predict(object=mod.r,newdata=data.frame(log.sh.cen=0,
                                                                              depth.cen=s.dat$depth.cen[j]),
                                              re.form=~0,type="response")))
            CF <- rbind(CF, CFj)
          } # end the fixed loop
          
        }
          
        
        if(ntows<5){
          
          for(j in 1:nrow(s.dat))
          {
            tempj <- cbind(data.frame(tow=s.dat$tow[j], depth.cen=s.dat$depth.cen[j], bin=paste0("h", seq(5,200,5))),
                           pred=as.vector(predict(object=mod.r,newdata=data.frame(log.sh.cen=log.sh.cen,
                                                                                  depth.cen=rep(s.dat$depth.cen[j] ,40)),
                                                  type="response"))) %>%
              tidyr::pivot_wider(values_from = "pred", names_from = "bin", id_cols = c("tow", "depth.cen"))
            temp <- rbind(temp, tempj)
            
            CFj <- cbind(data.frame(tow=s.dat$tow[j], depth.cen=s.dat$depth.cen[j]),
                         CF=as.vector(predict(object=mod.r,newdata=data.frame(log.sh.cen=0,
                                                                              depth.cen=s.dat$depth.cen[j]),
                                              type="response")))
            CF <- rbind(CF, CFj)
          } # end the glm loop
        }
        
        # weight matrix for use in surv.by.tow
        mw.res.t[[i]] <- cbind(year=as.numeric(yrs[i]), dplyr::arrange(temp, tow))
        
        # MW at 100mm for each tow
        CFtow[[i]] <- cbind(year=as.numeric(yrs[i]), dplyr::arrange(CF, tow))
        
        # So finally we need to use our models to predict condition on the bank, seemingly the easiest way is to pick a depth and MW to predict at
        # for the bank and just leave it at that.  SH will be 100 mm, Going to say median depth (which is 0 as set up above) of the survey tows between 2010 and 2022 see above
        # For BBn this is 75 meters, which makes loads of sense. Downside is only way to get an SE is to try and bootstrap one, which I'm too lazy to do
        cond.est <-   as.vector(predict(object=mod.r, newdata=data.frame(log.sh.cen=0,
                                                                         depth.cen=0),
                                        re.form=~0,type="response"))
        # We can pull out the intercept now as well and see how this compares
        inter <- summary(mod.r)$coefficients[1,1]
        inter.se <- summary(mod.r)$coefficients[1,2]
        # While we are at this, let's pull out the fixed slope and the random intercepts for the mw-sh figure and the depth terms.
        slope <- summary(mod.r)$coefficients[2,1]
        slope.se <- summary(mod.r)$coefficients[2,2]
        dep.1 <- summary(mod.r)$coefficients[3,1]
        dep.1.se <- summary(mod.r)$coefficients[3,2]
        
        #fixed effects or glm results
        if(ntows<5) {
          all.coef[[i]] <- data.frame(fix.int = inter, fix.int.se = inter.se, fix.slope = slope, fix.slope.se = slope.se,
                                      depth1 = dep.1, depth1.se = dep.1.se,
                                      tow = as.character(sort(unique(s.dat$tow))),year = yrs[i], rand.int=NA, rand.se=NA)
        }
        
        if(ntows>4) {
          fix.coef <- data.frame(fix.int = inter, fix.int.se = inter.se, fix.slope = slope, fix.slope.se = slope.se,
                                 depth1 = dep.1, depth1.se = dep.1.se,
                                 tow = as.character(sort(unique(s.dat$tow))),year = yrs[i])
          rand.coef <- data.frame(rand.int = ranef(mod.r)$new_ID[[1]], rand.se = se.ranef(mod.r)$new_ID[[1]],
                                  tow = attr(se.ranef(mod.r)$new_ID,'dimnames')[[1]])
          all.coef[[i]] <- left_join(fix.coef,rand.coef,by='tow')
        } 
        # this is the object with coefficients ^^^^
        
        # this object is for plotting annual MWSH curves, which may not be necessary here
        # mwsh.curve[[i]] <- data.frame(sh = 65:200, year=yrs[i], pred = predict(object=mod.r, newdata=expand.grid(log.sh.cen=log(seq(0.65,2, 0.01)),
        #                                                                                                          depth.cen=median(s.dat$depth.cen)),
        #                                                                        re.form=~0,type="response"))           
        # 
        # Object with condition prediction
        cond.pred[[i]] <- data.frame(cond = cond.est,year = yrs[i],intercept = inter,inter.se = inter.se)
        
      } # end the i loop
      
      # Let's rename this from whatever lousy name we use to something that makes sense, feel free to make it better.
      weight.matrix <- do.call("rbind",mw.res.t)
      CFyrs <- do.call('rbind',cond.pred)
      mw.sh.coef <- do.call('rbind',all.coef)
      CF.data <- do.call("rbind", CFtow)
     
      # Add some variables
      CFyrs$CF <- exp(CFyrs$intercept)
      CFyrs <- dplyr::select(CFyrs, -cond)
      CFyrs$CF.LCI <- exp(CFyrs$intercept - 1.96*CFyrs$inter.se)
      CFyrs$CF.UCI <- exp(CFyrs$intercept + 1.96*CFyrs$inter.se)
      
      # Get the actual random intercepts
      mw.sh.coef$ran.int.act <- mw.sh.coef$rand.int+ mw.sh.coef$fix.int
      
      # predictions by tow at 100mm = CF by tow
      # use unique on CF.data because of live and dead
      pred.dat <- left_join(pre.dat, unique(CF.data)) 
      CF.fit <- list(mw.sh.coef=mw.sh.coef, weight.matrix=weight.matrix)
    }
    # 
    # CFyrs: year depth lon lat CF CFse.fit
    # CF.data:       ID       lon      lat year depth tow       CF
    # pred.dat: pred.dat + "CF"        "CFse.fit" 
    # 
    # 
    # surv.dat$CF eventually needed for plot, which is from CF.data
    # 
  } # end if(!is.null(pred.dat))
  
  # return the results to the function calling this.
  list(CFyrs=CFyrs,CF.data=CF.data, HtWt.fit=SpatHtWt.fit, CF.fit=CF.fit,pred.dat=pred.dat, weight.matrix=weight.matrix)
  
}	# End function





