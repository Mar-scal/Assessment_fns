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
  #Source1 Load in our missed effects model.
  if(missing(dirct))
  {
      funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/shwt.lme.r")
      # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
      for(fun in funs) 
      {
        download.file(fun,destfile = basename(fun))
        source(paste0(getwd(),"/",basename(fun)))
        file.remove(paste0(getwd(),"/",basename(fun)))
      } # end for(un in funs)
  }# end  if(missing(direct_fns))
  
  # If you supply dirct...
  if(!missing(dirct)) source(paste(dirct,"/Survey_and_OSAC/shwt.lme.r",sep=""),local=T)	
  
  # the years in the data, sorted
  yrs<-sort(unique(wgt.dat$year))
  # Which depth should we use, this options uses depth in meters
	if(ADJ_depth  == T) wgt.dat$depth<-wgt.dat$ADJ_depth
	if(y2 ==T && model.type=='gam_s') wgt.dat$year<-wgt.dat$y2
  # If preds is not specified the predictions will be based the mean values of the data between 2005 and 2014
  # As we want to pick the same number every year, I would prefer that these are user inputs but this is a decent backup plan
  if(is.null(pred.loc) == T)
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
  		if(sum(!unique(pre.dat$year) %in% yrs) > 0) pre.dat$year[!pre.dat$year %in% yrs]<-min(yrs)
  		# Make predictions based on model.
  		pred.dat$CF<- predict(CF.fit,pre.dat)
  		pred.dat$CFse.fit <- predict(CF.fit,pre.dat, se=T)$se.fit
  		
	  } # end if(!is.null(pred.dat))
  
	# return the results to the function calling this.
	list(CFyrs=CFyrs,CF.data=CF.data, HtWt.fit=SpatHtWt.fit, CF.fit=CF.fit,pred.dat=pred.dat)	
}	# End function





