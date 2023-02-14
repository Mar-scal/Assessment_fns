####  Commented and checked by DK starting on July 28, 2015. Linear Mixed effects model used to calculate meat weight shell height
#  realtionship for the offshore.  We are stuck with this model until next Framework (2018?)
## Update history
## March 31 2016 by DK, tidying up structure and comments
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
#    ok
#      
##
###############################################################################################################

# ARGUMENTS 
# wt.dat:        The meat weight shell height data.
# random.effect: What is the random effect.  Options are "tow" and "year".  Default is 'year' but we appear to overwrite and use 'tow'.
#                If using multple years it really should be both...
# verbose:      Do we want to print the results to screen.  (T/F) default is T
# GBmodel:      Not clear what this option is for, it combines the fixed and random effects into one object for each parameter. (T/F) F = default
# b.par:        Do we want to use the data/model to 'estimate' the allometric relationship or force it to be a specific number (i.e. 3).  
#               Default is 'estimate', but we are currently overwriting this and setting it to 3.


shwt.lme<-function(wt.dat,random.effect="year",verbose=T, GBmodel=F, b.par='estimate')
{
	# I think the idea here is to grab any header with "wmw" or "sh" in it and rename it 'wmw' or 'sh'.
  # But the command given if these statements is true only works if the name is already correct...
	if("wmw" %in% names(wt.dat)) wt.dat$wmw <- wt.dat$wmw
	if("sh" %in% names(wt.dat)) wt.dat$sh <- wt.dat$sh

	# Load the nlme package, we may want to change to lme4...
	require(nlme) || stop("How don't you have nlme package???  Please install 'nlme'!")
	
	# Create a random effect column picking whatever we I.D'ed above.  This limits us to pick 1, can't have tow within year as random effect
	# but I think we should at some point...
	wt.dat$raneff<-wt.dat[,random.effect]
  # Grab the levels of our random effect
	ran.effects<-unique(wt.dat$raneff)
	
	# Make an object which contains our data and model.  Not entirely sure why this is done?
	wt.gdat <- groupedData(wmw ~ sh | raneff, data = wt.dat)
	a <- c()
	b <- c()
	
	# if we are estimating the b parameter this is our model.  Note that we have not centered
	# our shell height data and it is leading to a pretty high correlation for the intercept and I think is 
	# making it difficult get a great estimate of the intercept, centering seems to alivated that problem.
	if(b.par=='estimate')
	  {
	    # the model.
  		wt.lme <- lme(fixed = log(wmw) ~ log(sh), data = wt.gdat, random = ~ log(sh) | raneff, method="REML")
  		# Pull out the random coefficents and make into a dataframe
  		if(is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
  		# If this formats turns out to be non-numeric Convert any characters to numbers and make the  dataframe.
  		if(!is.character(wt.dat[,random.effect]))fit <- data.frame(raneff=sort(row.names(coef(wt.lme))),
  		                                                           a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),1],
  		                                                           b=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),2])
  		
  		# Pull out the random effects estimates for each tow(or year or whatever)
  		for(i in 1:length(ran.effects))
  		  {
    			a[i] <- exp(fit[i,2])
    			b[i] <- fit[i,3]
  		  } # end for(i in 1:length(ran.effects))
		  
  		# Grab the fixed effects estimates
  	  A <- wt.lme$coef$fixed[1]
		  B <- wt.lme$coef$fixed[2]
  		
		  # I'm not sure what the GBmodel is but if TRUE then we combine the fixed and random effects for each term into one object.
		  if(GBmodel==T)
  		  {
  			  # No hydration samples for 1981, so fill a[[1]], b[[1]] in with fixed effect means
  			  a <- c(exp(as.numeric(wt.lme$coef$fixed[1])),a)
  			  b <- c(as.numeric(wt.lme$coef$fixed[2]),b)
  		  } # end if(GBmodel==T)
  		
	} # end if(b.par=='estimate')
	
		# if we are assuming we know b (i.e. b=3) then this is our model.
	if(b.par!='estimate')
	  {
	    # Convert the shell height to it's cube.
  		wt.gdat$sh<-wt.gdat$sh^b.par
  		# Now fit a linear model on the wmw v.s. shell height to find the "a" intercept for the linearized MWSH relationship, 
  		# which will be the random slopes from this model. Note the model intercept is forced through 0.
  	  
  		wt.lme <- lme(fixed = wmw ~ sh -1, data = wt.gdat, random = ~ sh -1 | raneff, method="REML",control=lmeControl(opt='optim'))
  		# Pull out the random intercepts and make into a dataframe
  		if(is.character(wt.dat[,random.effect])) fit <- data.frame(raneff=row.names(coef(wt.lme)),coef(wt.lme))
  		
  		# If this formats turns out to be non-numeric Convert any characters to numbers and make the  dataframe.
  		if(!is.character(wt.dat[,random.effect])) fit <- data.frame(raneff=sort(as.numeric(row.names(coef(wt.lme)))),
  		                                                           a=coef(wt.lme)[order(as.numeric(row.names(coef(wt.lme)))),])
      # Make a new object which contains the random coefficients.
  		a <- c()
  		
  		for(i in 1:length(ran.effects))
  		  {
  		  	a[i] <- fit[i,2]
  		  	b[i] <- b.par
  		  } # end 	for(i in 1:length(ran.effects))
  		
  		# Grab the fixed effects estimates
  		A <- wt.lme$coef$fixed[1]
  		B <- b.par
  	} # end if(b.par!='estimate')
	
	# Rename the first column to match the random effect.
	names(fit)[1]<- random.effect

	# If requested print the model summary
	if(verbose== T) print(summary(wt.lme))
	# Add a label column to the wt.dat object
	wt.dat$label<-wt.dat[,random.effect]
	
	# If our random effect is month pull out the Month and put them in as the label.
	if(random.effect=="month")wt.dat$label<-months(as.Date(paste("2009-",1:12,"-01",sep="")))[wt.dat$raneff]
	summy <- summary(wt.lme)
	
	# Send the model results and the wt.dat object back to the function calling this one.
	return(list(A=A,B=B,a=a,b=b,data=wt.dat,fit=fit,summary = summy))
	#browser()
} # end shwt.lme function
	