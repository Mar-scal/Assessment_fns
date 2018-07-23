####################################################################
## This function is used to project the population into the following year based on the results of a full model run.
## This can be used to project 2 years out (but with no catch) which is used in the decision table to look at scenarios 
# using various exploitation rates.  You can run as many projections as your heart desires.  This gets fed into the decision table.
###################################################################
# Update history
# April 2016 - Revised by DK 

#####################################  Function Summary ########################################################
####  
##This function is used within these files:(a.k.a "dependent files") 
#
# 1:  Update_function_JAGS.r
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# N/A
#
###############################################################################################################


###############################################################################################################
# Arguments
# 1:  model.out:   The model run results 
# 2:  C.p:         The projected catch from the start of the survey year (i.e. September for GBa, July for BBn) to the end of a calendar year
#                  If a second value is specified this is just a second catch scenario to look at.  Default = c(200,300).
# 3:  proj.para:   If you want to specify the projection parameters.  Default = NULL which uses last years data for the projection.
#                  You need to specify them in a data frame it should look something like 
#                  proj.para = data.frame(m = 0.2,mR =0.3, g= 1.2, gR = 1.5, r = 300)
#                  Note that you only need to include the parameters you want to specify 
#                  Note that m is the instaneous mortality, to convert to proportion  1-exp(-.m)
#                  proj.para = data.frame(g=1.2) would set the the commercial sized growth parameter to 1.2, all others
#                  would be based on the data...
#                  Mortality, growth, and recruit abundance are the only parameters you can alter.
# 4:  proj.uncert: Do you want your user specified projections to include uncertainty.
#                  Options are 
#                     a:  "NULL" (default) If you want your user specified parameters to be fixed values leave this as NULL. 
#                     b:  "posterior" If you want your the uncertainty to be based on the model results.
#                          This will grab the standard deviation of the last year of data for each parameter.
#                     c:  "prior" If you'd like to use the uncertainty based "roughly" on the priors for the parameters 
#                     d:  user specified uncertainty.  This should be a dataframe the same size as "proj.para.  Two examples
#                         proj.uncert = data.frame(m = 0.05,mR =0.05, g= 0.1, gR = 0.1, r = 0.2)
#                         proj.uncert = data.frame(g = 0.1)                  
#                  
###############################################################################################################



projections<-function(model.out,C.p=c(200,300),proj.para = NULL,proj.uncert = NULL)
{
  # Add the projected Catch from the start of survey year to the end of the calendar year
	model.out$data$C.p<-C.p
	# The length of this allows for multi-year projections, the code is constrained to only project out 2 years
	model.out$data$NC <-length(C.p)
	
	# Initialize the variables used in the loop
	Pmed.p<-list()
	P.p<-list()
	B.p<-list()
	Bmed.p<-list()
	mu.p<-list()
	B.change<-list()
	pB0<-list()
	Pmed.p2<-list()
	P.p2<-list()
	B.p2<-list()
	# Combine the data and simulation results into one object.
	d<-c(model.out$data,model.out$sims.list)
	
	# This set up the projection parameters to be the values for the last year, they will be
	# overwritten as applicable below.
  d$projection.parameters <- data.frame(m = d$m[,d$NY],G=d$G[d$NY],mR = d$mR[,d$NY],
                                                                          GR = d$GR[d$NY],r = d$r[,d$NY])
  
  # If we have specified your own projection parameters do this...
  if(!is.null(proj.para))
  {
    print(cat("HEADS UP!  User supplied data was used for the projections, see model.out$data$projection.parameters 
      for the parameters used"))
    
    # Set up the uncertainty for each of the terms...
    # First if it is null just set all relavent terms to 0.
    # The the number of samples to pull down
    posterior.size <- length(d$GR[,d$NY])
    if(is.null(proj.uncert)) # if set to NULL that means they are all 0's.
    {
      if(any(names(proj.para)=="G")) proj.para$G.se <- 0
      if(any(names(proj.para)=="GR")) proj.para$GR.se <- 0
      if(any(names(proj.para)=="m")) proj.para$m.se <- 0
      if(any(names(proj.para)=="mR")) proj.para$mR.se <- 0
      if(any(names(proj.para)=="r"))proj.para$r.se <- 0
    } # if(is.null(proj.uncert))
    
    # Run this bit only if proj.uncert is not null
    if(!is.null(proj.uncert))
    {
      # If set to "posterior" this means we will pull the sd from the data
      if(proj.uncert == "posterior") 
      {
        if(any(names(proj.para)=="G")) proj.para$G.se <- sd(d$projection.parameters$G)
        if(any(names(proj.para)=="GR")) proj.para$GR.se <- sd(d$projection.parameters$GR)
        if(any(names(proj.para)=="m")) proj.para$m.se <- sd(d$projection.parameters$m)
        if(any(names(proj.para)=="mR")) proj.para$mR.se <- sd(d$projection.parameters$mR)
        if(any(names(proj.para)=="r"))proj.para$r.se <- sd(d$projection.parameters$r)
      } # end if(proj.uncert == "posterior") 
      
      # If set to "priors" we will get a rough estimate from the prior
      if(proj.uncert == "prior") 
      {
        # The priors in BUGS for log-normals are precisions sd = 1/(precision)^0.5
        if(any(names(proj.para)=="G")) proj.para$G.se <- 1/sqrt(model.out$priors$G.b[d$NY])
        if(any(names(proj.para)=="GR")) proj.para$GR.se <- 1/sqrt(model.out$priors$GR.b[d$NY])
        if(any(names(proj.para)=="r"))proj.para$r.se <- 1/sqrt(model.out$priors$r.b)
        # We don't have priors for the m or mR, so we take the beta distribution used for the m prior and find the 
        # standard deviation of those data...
        if(any(names(proj.para)=="m")) proj.para$m.se <- sd(-log(1-(rbeta(posterior.size,model.out$priors$M.a[d$NY],
                                                                          model.out$priors$M.b[d$NY]))))
        if(any(names(proj.para)=="mR")) proj.para$mR.se <- sd(-log(1-(rbeta(posterior.size,model.out$priors$M.a[d$NY],
                                                                            model.out$priors$M.b[d$NY]))))
      } # end if(proj.uncert == "prior") 
      
      # Finally, if you want to be in control of the uncertainty you attribute to these variables you can do that too...
      # just make sure the projection uncertainty is a dataframe!
      if(is.data.frame(proj.uncert) ==T) 
      {
        # O.k. so here we are putting in the user entered uncertainty, if it doesn't exist (i.e. you )
        # supplied paramters for g and gR but only the uncertainty for g, it treat gR as a fixed parameter
        # and g will have variability.
        if(any(names(proj.para)=="G")) proj.para$G.se <- ifelse(!is.null(proj.uncert$G),proj.uncert$G,0)
        if(any(names(proj.para)=="GR")) proj.para$GR.se <- ifelse(!is.null(proj.uncert$GR),proj.uncert$GR,0)
        if(any(names(proj.para)=="r"))proj.para$r.se <- ifelse(!is.null(proj.uncert$r),proj.uncert$r,0)
        if(any(names(proj.para)=="m")) proj.para$m.se <- ifelse(!is.null(proj.uncert$m),proj.uncert$m,0)
        if(any(names(proj.para)=="mR")) proj.para$mR.se <- ifelse(!is.null(proj.uncert$mR),proj.uncert$mR,0)
      } # end if(is.data.frame(proj.uncert) ==T) 
    } # end  if(!is.null(proj.uncert)) 

    # I'm using a log normal for all of these since they are need to be + and none are bounded.
    if(any(names(proj.para)=="G")) d$projection.parameters$G <- rlnorm(posterior.size,log(proj.para$G),proj.para$G.se)
    if(any(names(proj.para)=="GR")) d$projection.parameters$GR <- rlnorm(posterior.size,log(proj.para$GR),proj.para$GR.se)
    # Here we are changing the shape of our m, in the prior we use a beta for M then convert those values to 
    # m, here we will simplify to just assume that m is log-normal, which should be fine as long as we
    # keep the se relatively small.
    if(any(names(proj.para)=="m")) d$projection.parameters$m <- rlnorm(posterior.size,log(proj.para$m),proj.para$m.se)
    if(any(names(proj.para)=="mR")) d$projection.parameters$mR <- rlnorm(posterior.size,log(proj.para$mR),proj.para$mR.se)
    # Note that b/c we are dividing r by K to make sure that "R" remains constant it will appear that r varies
    # hope that makes sense...
    if(any(names(proj.para)=="r")) d$projection.parameters$r <- rlnorm(posterior.size,log(proj.para$r/d$K),proj.para$r.se)
  } # end if(!is.null(proj.para))

  
  

	# Run projection for however many catch scenarios you'd like.  This will project out 2 year but we never use the second year projection.
	for(i in 1:length(C.p))
	{
		Pmed.p[[i]]<- log(exp(-d$projection.parameters$m)*(d$projection.parameters$G)*(d$P[,d$NY] - d$C.p[i]/d$K) + 
		                    exp(-d$projection.parameters$mR)*(d$projection.parameters$GR) * d$projection.parameters$r)
		# Now get P.  If the sigma is high and the Pmed.p low this can lead to some ridiculous results...
		P.p[[i]] <- sapply(1:length(d$K),function(x){rlnorm(1,Pmed.p[[i]][x], d$sigma[x])})
		# Convert P to a Biomass based on K.
		B.p[[i]] <- P.p[[i]] * d$K
		# Using the P from the process equation convert to a biomass
		Bmed.p[[i]] <- exp(Pmed.p[[i]]) * d$K
		# Caluclate the exploitation rate for next year using the projected catch. 
		mu.p[[i]] <- C.p[i] / (B.p[[i]] + C.p[i])
		# This is the projected biomass change (%) from this year to next.  > 0 = increase.
		B.change[[i]] <- (B.p[[i]] - d$B[,d$NY]) / d$B[,d$NY] * 100
		# Probability of biomass decline.  What runs are less than 0.
		pB0[[i]] <- 0 > (B.p[[i]]-d$B[,d$NY])
		
		# This is the year 2 projection, but importantly note that it is the projection with no catch in year 2, in the 
		# decision table the catch is removed based on what "mu" is set to.  
		Pmed.p2[[i]]<-log(exp(-d$projection.parameters$m)*(d$projection.parameters$G)*P.p[[i]]+
		                    exp(-d$projection.parameters$mR)*(d$projection.parameters$GR)*d$projection.parameters$r)
		# Get P
		P.p2[[i]] <- sapply(1:length(d$K),function(x){rlnorm(1,Pmed.p2[[i]][x], d$sigma[x])})
		# Convert P to a biomass
		B.p2[[i]] <- P.p2[[i]] * d$K
		#browser()
	} # end for(i in 1:length(C.p))
# Now add to the model output these projections.  One column for every C.p scenario.  Do this for Biomass, 2 year biomass, 
# Median predicted biomass, fishing mortality and biomass change
model.out$sims.list$B.p<-do.call("cbind",B.p)
model.out$sims.list$B.p2<-do.call("cbind",B.p2)
model.out$sims.list$Bmed.p<-do.call("cbind",Bmed.p)
model.out$sims.list$mu.p<-do.call("cbind",mu.p)
model.out$sims.list$B.change<-do.call("cbind",B.change)
# pB0 is true/false so don't need it as a sims.list.

#Calculate the mean for the projections, biomass, 2 year biomass, median biomass, fishing mortality, biomass change, and probabiliy of decline
model.out$mean$B.p<-unlist(lapply(B.p,mean))
model.out$mean$B.p2<-unlist(lapply(B.p2,mean))
model.out$mean$Bmed.p<-unlist(lapply(Bmed.p,mean))
model.out$mean$mu.p<-unlist(lapply(mu.p,mean))
model.out$mean$B.change<-unlist(lapply(B.change,mean))
model.out$mean$pB0<-unlist(lapply(pB0,mean)) # This gives the mean proportion of trues.

#Calculate median for the projections, biomass, 2 year biomass, median biomass, fishing mortality, biomass change, and probabiliy of decline    
model.out$median$B.p<-unlist(lapply(B.p,median))
model.out$median$B.p2<-unlist(lapply(B.p2,median))
model.out$median$Bmed.p<-unlist(lapply(Bmed.p,median))
model.out$median$mu.p<-unlist(lapply(mu.p,median))
model.out$median$B.change<-unlist(lapply(B.change,median))
# The median of pB0 is meaingless so don't bother with it...
# Finally we can look at what our "projection" parameters look like
model.out$data$projection.parameters <- d$projection.parameters

# Return the results
model.out
} # end function
	