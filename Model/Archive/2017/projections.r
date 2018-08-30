####################################################################
## This function is used to project the population into the following year based on the results of a full model run.
## This can be used to project 2 years out (but with no catch) which is used in the decision table to look at scenarios 
# using various exploitation rates.  You can run as many projections as your heart desires.  This gets fed into the decision table.
###################################################################
# Update history
# April 2016 - Revised by DK 
# March 2017 - Found a mistake in the process equation, we had m instead of mR in the recruit mortality term, whooops!!!
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
# 1:  model.out:  The model run results 
# 2:  C.p:        The projected catch from the start of the survey year (i.e. September for GBa, July for BBn) to the end of a calendar year
#                 If a second value is specified this is just a second catch scenario to look at.  Default = c(200,300).
###############################################################################################################



projections<-function(model.out,C.p=c(200,300))
{
  # Add the projected Catch from the start of survey year to the end of the calendar year
	model.out$data$C.p<-C.p
	# The length of this allows for multi-year projections, the code is constrained to only project out 2 years
	model.out$data$NC<-length(C.p)
	
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
	# Run projection for however many catch scenarios you'd like.  This will project out 2 year but we never use the second year projection.
	for(i in 1:length(C.p))
	{
		# year 1 projection.  First the process equation.
		Pmed.p[[i]]<- log(exp(-d$m[,d$NY])*(d$g[d$NY])*(d$P[,d$NY]-C.p[i]/d$K)+exp(-d$mR[,d$NY])*(d$gR[d$NY])*d$r[,d$NY])
		# Now get P.
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
		Pmed.p2[[i]]<-log(exp(-d$m[,d$NY])*(d$g[d$NY])*P.p[[i]]+exp(-d$mR[,d$NY])*(d$gR[d$NY])*d$r[,d$NY])
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

# Return the results
model.out
} # end function
	