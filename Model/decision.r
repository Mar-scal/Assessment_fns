####################################################################
## This function is used to make the decision tables based on the projection function and model output.
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
# 
#
###############################################################################################################


###############################################################################################################
# Arguments
# 1:  model.out:      The model run results 
# 2:  bank:           The bank of interest, currently only GBa and BBn are modeled/possible
# 3:  mu:             Exploitation rate used for the future catch scenarios in the decision table.
# 4:  pr:             The quantiles used for the second year projections (only used if vers == 1).  Default is seq(0.1,0.6,0.1).
# 5:  vers:           Which decision table to use.  Default is version 1 includes the catch scenario projections, 2 excludes this.
# 6:  post.survey.C:  The catch from after the survey until the end of the calendar year.
# 7:  yrs.ref.calc:   The years to use to calculate the reference points (only used if refs = 'varied').  Default = missing
#                     which will used all years in the calculation (again only if refs = 'varied'). 
#                     If specified this should be entered as something like 1:10 which would use year 1 to 10 to calculate ref points.
# 8:  refs:           Upper and lower reference points.  Currently only valid for GBa.  Default = "varied" and are calculated
#                     from the data (see code for details).  Otherwise a numerical value is needed for the LRP and USR.
# 9:  yr:             The final year from the model(not the projection year). Default = as.numeric(format(Sys.time(), "%Y"))-1, i.e. last year.
###############################################################################################################

decision<-function(model.out,bank,mu=0.15,pr=seq(0.1,0.6,0.1),vers=1,post.survey.C=0,yrs.ref.calc,refs='varied',
                   yr= as.numeric(format(Sys.time(), "%Y"))-1)
{
	
  # Load xtable package
	require(xtable)
  # If yrs.ref.calc isn't supplied and the references are set to 'varied' then all the data are used to calculate the reference points
  if(missing(yrs.ref.calc)) yrs.ref.calc<-1:length(model.out$median$B)
  
  # The ratio of the projected biomass to the supplied USR for each catch scenario using the projected data
  # If > 1 then the population is above the USR (USR).
	if(refs[1]!='varied') RatioUSR<-sweep(model.out$sims.list$B.p,1,FUN='/',refs[1])
	# If reference points are not supplied the USR(USR) is calclated as 80% of the mean.  If yrs.ref.calc is not supplied
	# then all the data are used, if yrs.ref.calc is supplied than only the years selected are used.
	if(refs[1]=='varied')RatioUSR<-sweep(model.out$sims.list$B.p,1,FUN='/',(apply(model.out$sims.list$B[,yrs.ref.calc],1,mean)*0.8))
	# The probability if being above the USR(USR) for each catch scenario used in the projection.
	PrHealth<-sapply(1:model.out$data$NC,function(x){sum(RatioUSR[,x]>1)/nrow(RatioUSR)})

	# The ratio of the projected biomass to the supplied LRP for each catch scenario using the projected data
	# If > 1 then the population is above the LRP.
	if(refs[1]!='varied') RatioLRP<-sweep(model.out$sims.list$B.p,1,FUN='/',refs[2])
	# If reference points are not supplied the LRP is calclated as 30% of the mean.  If yrs.ref.calc is not supplied
	# then all the data are used, if yrs.ref.calc is supplied than only the years selected are used.
	if(refs[1]=='varied')RatioLRP<-sweep(model.out$sims.list$B.p,1,FUN='/',(apply(model.out$sims.list$B[,yrs.ref.calc],1,mean)*0.3))
	# The probability if being above the LRP for each catch scenario used in the projection.
	PrCrit<-sapply(1:model.out$data$NC,function(x){sum(RatioLRP[,x]>1)/nrow(RatioLRP)})

	# If calculating the reference points from the data  for the current year.
	if(refs[1]=='varied')
	{
		# The ratio of current (not projected) biomass to the USR.
	  RatioUSR.current<-model.out$sims.list$B[,model.out$data$NY]/(apply(model.out$sims.list$B[,yrs.ref.calc],1,mean)*0.8)
	  # The ratio of current (not projected) biomass to the LRP
	  RatioLRP.current<-model.out$sims.list$B[,model.out$data$NY]/(apply(model.out$sims.list$B[,yrs.ref.calc],1,mean)*0.3)
	  print(paste("USR (calc) =",round(mean(apply(model.out$sims.list$B[,yrs.ref.calc],1,mean)*0.8)))) # Print the USR to the screen
	  print(paste("LRP (calc)=",round(mean(apply(model.out$sims.list$B[,yrs.ref.calc],1,mean)*0.3)))) # Print the LRP to screen
	} # end if(refs[1]=='varied')
	
	# If the reference points are supplied this gets our current year probabilities.
	if(refs[1]!='varied') 
	{  
	  # calculate the ratio of current (not projected) biomass to the USR
	  RatioUSR.current<-model.out$sims.list$B[,model.out$data$NY]/refs[1]
	  # Now for the LRP get the ratio of current(not projected) biomass to LRP
	  RatioLRP.current<-model.out$sims.list$B[,model.out$data$NY]/refs[2]
	  print(paste("USR (defined)=",refs[1])) # print the USR to screen.
	  print(paste("LRP (defined)=",refs[2])) # print the LRP to screen.
	} # end if(refs[1]!='varied') 
	  # The current probability of being healthy.
	PrHealth.current<-sum(RatioUSR.current>1)/length(RatioUSR.current)
	PrCrit.current<-sum(RatioLRP.current>1)/length(RatioLRP.current)
	print(paste("Probability Biomass in",yr,"is in healthy zone = ",PrHealth.current)) # Print the probability of being healthy to screen.
	print(paste("Probability Biomass in", yr, "is above critical zone = ",PrCrit.current)) # Print probabilty of being below the LRP to  screen.
browser()
	# Make the decision table.  There are 2 versions of the decision table. This version includes the expected Biomass 2 years out
	# at a exploitation rate of mu (default = 0.15). ## FK 2021 thought... pretty sure this is showing expected CATCH 2 years out, not biomass. 
	if(vers==1) tab1<-cbind(data.frame(Catch=model.out$data$C.p-post.survey.C,
	                                  mu=model.out$median$mu.p,B.change=model.out$median$B.change,
	                                  p.decline=model.out$mean$pB0,Pr.above.USR=PrHealth,Pr.above.LRP=PrCrit),
	                       t(sapply(1:length(model.out$data$C.p),function(i){quantile(mu*model.out$sims.list$B.p2[,i],pr)})))
  # The version 2 dection table which doesn't include the projection scenarios
	if(vers==2) tab1<-data.frame(Catch=model.out$data$C.p-post.survey.C,mu=model.out$median$mu.p,B.change=model.out$median$B.change,
	                            p.decline=model.out$mean$pB0,Pr.above.USR=PrHealth,Pr.above.LRP=PrCrit)
	# Turn this into a nice table for latex
	tabtex <- xtable(tab1)
	# Print the table to a tex file 
	print(tabtex, type='latex', file=paste('Decision_',bank,'.tex',sep=''), include.rownames=F)
	tab1 # Output the original table object
} # end the function

		