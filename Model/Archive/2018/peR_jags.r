###################################################################################################
# This function produces the prediction evaluation plots.  
###################################################################################
# Update history
# Feb 2016 DK made some minor edits
# April 2015 DK overhauled this function to work with JAGS and to produce box-plots removed some of code as no evidence it is ever used.

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##
##  1:  Update_function_JAGS.r
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
##
###############################################################################################################


###############################################################################################################
# Arguments
#
# 1:  input:      The input used for the model run.  Default is missing, should be a list (same list as used to run the main model)
# 2:  priors:     The priors used for the model run.  Default is missing, should be a list (same list as used to run the main model)
# 3:  years:      The years for the model runs.  Default is missing, should be the same as you used to run the main model 
# 4:  model:      The JAGS model.  Default is missing, should be the same model as you used for the main model run. 
# 5:  pe:         The years to evalulate, note this should be specified in reverse order with the most recent year first. Missing by default
# 6:  niter:      Number of iterations to run.  Default = 60000
# 7:  nburn:      The burnin for the model.  Default = 40000 
# 8:  nthin       Thinning rate for the model output.  Deafult = 20
# 9:  nchains     The number of chains Default = 8. If running in parallel increasing this will return more replicates with no computing time cost.
# 9:  graphic:    What to do if making a figure.  Print to "screen" by default, optionally save as a "pdf".
#10:  run:        Do you need to run the model?  (T/F), Default = T, if you have run this once the results are saved and you can set to F
#12:  plot:       The plot to produce the various potential figures.  There are 3 options here, 
#                 1: default = NULL, no plot is produced
#                 2: "box", this produces the nice box plot of the predicted vs modeled results for each year of interest
#                 3:  "ts", this produces a point summary of the time series of the differences. 
#                 4:  "ts_all", produces a very busy plot of the predictive vs. modelled estimates.
#13:  lab:        If you want to add a specific label to the output figure (only useful if graphics = "pdf")
#14:  g2:         If specified the growth term for FR's used in the prediction evals is the current year condition * expected SH next year
#15:  gR2:        If specified the growth term for recruits used in the prediction evals is the current year condition * expected SH next year
#16:  path:       The path to put the pdf, will default to whatever is specified (dirt is not used here)
#17:  dirt:       The path to put the output results (not including the figures).  Default = Y:/Offshore scallop/Assessment
#18:  bank:       The bank to run the prediction evaluations on.  Default = "GBa" ("BBn" is only other option that should work currently)
#19:  parallel:   If running the predictin evaluations do you want to run in parallel?  Number of clusters used set by nchains.
#20:  j.seed:     If running in parallel you can specify a seed (for initial values) so the results are reproducable.  Default = 123
#21:  parameters: The parameters from the original model output.  Default is DD.out[[bnk]]$parameters
#22:  wd:         The width of the figure.  Default =11
#23:  ht:         The height of the figure.  Default = 8.5
#
###############################################################################################################


peR <- function(input, priors, years,model,  pe, niter = 60000, nburn = 40000, nthin = 20,nchains=8, graphic="screen",run=T,
                plot= NULL, lab='',g2,gR2,path='',dirt="Y:/Offshore scallop/Assessment/",bank="GBa",j.seed=123,parallel=T,
                parameters = DD.out[[bnk]]$parameters,wd=11,ht=8.5)
{

	# The functions to load
	source(paste(dirt,"Assessment_fns/Model/projections.r",sep=""))
	# If we need to run the model here's where it happens.
	if(run==T)
	{
		# Define an out object
		out<-NULL
		# The input data is the years and all the data excluding the number of years
		input.dat<-data.frame(Year=years,input[names(input)!="NY"])
    # Start the performance evalulations for each year requested
		for(i in 1:length(pe))
		{
		  # Run the model from the first year of the data to the PE year.	
		  # Modify the input list to the proper number of years and remove the year column
		  input.lst<-as.list(subset(input.dat,Year %in% (years[1]:pe[i]),-1))
		  # Get the Number of years correct.
		  input.lst$NY <- length(input.lst$C)
		  
		  # Need to do a little stick handling to get the right priors for the ones specified each year
		  # We need to do this for the predictions in previous years
	    priors[which(lapply(priors,length)>1)]<-as.list(data.frame(priors[which(lapply(priors,length)>1)])[1:input.lst$NY,]) 	
	    
		  # If g2 and gR2 are not missing use them as the growth terms for the model for the final year used in prediciton evaluation
		  if(!missing(g2))input.lst$g[input.lst$NY] <- g2[input.lst$NY]
		  if(!missing(gR2))input.lst$gR[input.lst$NY] <- gR2[input.lst$NY]
		  
		  # Run model either in parallel...
	    start<-Sys.time()
	    if(parallel == T)
	    {
	      outp <- jags.parallel(data =  c(priors,input.lst), inits = NULL,parameters.to.save = parameters,  
	                          model.file=paste(dirt,model,sep=""),n.chains = nchains, n.iter = niter, n.burnin = nburn, 
	                          n.thin = nthin,jags.seed = j.seed)
	    }# end if(para = T)
	    
	    # ...or not
	    if(parallel == F)
	    {
	      outp <- jags(data =  c(priors,input.lst), inits = NULL,parameters.to.save = parameters,  
	                          model.file=model,n.chains = nchains, n.iter = niter, n.burnin = nburn, 
	                          n.thin = nthin)
	    } # end if(para = F)
	    print(Sys.time()-start)
	    DD <- list(data=c(priors,input.lst), sims.list=outp$BUGSoutput$sims.list,median=outp$BUGSoutput$median,
	                          mean=outp$BUGSoutput$mean,summary=outp$BUGSoutput$summary,priors = priors)
 			# Run projections from 0 up to catch in the final year
			DD<-projections(DD,C.p = c(0,input.lst$C[input.lst$NY]))

 			# Save medians and the projected biomass for the actual catch in the given year.
			out[[as.character(pe[i])]]<-list(median = DD$median, proj.bm.sims = DD$sims.list$B.p[,2],
			                                 bm.sims = DD$sims.list$B)
		} # end for(i in 1:length(pe))
		# Now save the output so we don't have to run this every time.
	  if(missing(g2)== T)save(out,file= paste(dirt,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_results_mod_growth.RData",sep=''))
		if(missing(g2)== F)save(out,file= paste(dirt,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_results_g2_growth.RData",sep=''))
	} # end if(run==T)
	
	# If run is false then we need to load in the results
	if(run==F) 
	  {
	    # If the files don't exist yet print a warning saying you have to run the models first
	    if(file.exists(paste(dirt,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_results_mod_growth.RData",sep=''))==F &&
	       file.exists(paste(dirt,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_results_g2_growth.RData",sep=''))==F)
	    {
	      print("Yo dude, you gotta run the prediction evaluation models before you can plot them...rookie...")
	    } # end if if(file.exists(...) = F)
	    
	    # If g2 is specified then we plot the g2 model results
	    if(missing(g2) == F &&
	       file.exists(paste(dirt,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_results_g2_growth.RData",sep=''))==T)
	    {
	      load(paste(dirt,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_results_g2_growth.RData",sep=''))
	    }# end if(missing(g2) == F && file.exts...
	  
	    # If g2 is not specified than we plot the basic model results.
	    if(missing(g2) == T &&
	      file.exists(paste(dirt,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_results_mod_growth.RData",sep=''))==T)
	    {
	      load(paste(dirt,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_results_mod_growth.RData",sep=''))
	    } # end if(missing(g2) == T && file.exts...
	  }# end if(run==F)
	
	# Now we move to plotting the results...
	if(is.null(plot) == F) # If we have specified we want a plot than make a plot.
	{
	  out2 <- NULL # define our new output
	  # If the prediction evaluation is a different length than the output (i.e. we've asked for the plot of different years
	  # Then we need to make them the same length and make sure the years line up.
	  if(length(pe) != length(out)) 
	  {
	    
	    for(i in 1:length(out)) if(is.element(as.numeric(names(out)[i]), pe)==T) out2[names(out)[i]] <- out[names(out)[i]]
	    writeLines(paste("Greatings kind scientist!!  Note that you asked to plot years",min(pe),"-",max(pe),
	                "but the prediction evalulation model ran for the years",min(as.numeric(names(out))), "-",max(as.numeric(names(out))), 
	                "No big whoop but I thought you'd like to know"))
	  } # end if(length(pe) != length(out)) 
	  if(length(pe) == length(out)) out2 <- out # if the lengths are the same then we can rename out to out2, they are the same thing..
	  
	} # if(plot %in% 1:3)
	
	# This plot is the median biomass trend from:
	# a: the model predictions (e.g. the biomass is generated for 2012 from the model that ran from 1986:2011)
	# b: from the actual model estimates (e.g. the biomass is generated for 2011 from the model that runs from 1986:2011)
	# and finally from the from the model using all data (e.g. the 2011 biomass is extracted from the model than ran from 1986:CurrentYear)
	if("ts" %in% plot)
	{
	  # Get the number of years we are plotting
	  n <- length(out2)
	  # The predicted biomass was this (the 2 represents the actual catch values from the projection)
	  # But remember threse are the predictions from a given year (so are actually the following year values)
	  # that's dealt with below...
		preds <- sapply(names(out2), function(i){out2[[i]]$median$B.p[2]})
		# The actual (modelled) biomass was actually this...
		ests <- sapply(names(out2), function(i){rev(out2[[i]]$median$B)[1]})
		# The years are....
		years<-as.numeric(names(out2))
		 
		# Changed out2put name to include year rather than model as our model is pretty set...
		if(graphic=='pdf') pdf(paste(path,"pred_eval_point_estimates_",max(years),"_",lab,".pdf",sep=''), width=wd, height=ht)
		if(graphic=="screen")windows(8.5,11)
		# Set margins
		par(omi=c(0.1,0.1,0.1,0.1))
		# Plot the predictions, note the +1 is due to predictions being lined up with the year the prediction was made, not the year of the pred.
		plot(years+1, preds/1000, type = 'b', ylim = c(0, max(c(preds,ests))*0.0011), xlim=c(min(years)-1,max(years)+2), las=1, 
		     mgp = c(1,0.5,0), tcl=-0.3, pch=16, ylab="", xlab="")
		# Add the actual values
		points(years, ests/1000, type = 'b', pch = 1, lty=1)
		# This gets the median estimates using the most recent model (so the last year will be identical) previous years should differ
		# That is the difference between the "full" estimate and the "estimate" which I have renames to Model (excluding future years)
		# and Model (using all years).
		# names(out2)[1] gets the most recent year in the data and the length bits pull out the years of interest from within this.
		points(years, rev(out2[[names(out2)[1]]]$median$B[(length(out2[[names(out2)[1]]]$median$B)-n+1):
		                                                    length(out2[[names(out2)[1]]]$median$B)])/1000, pch=3, cex=0.8)
		# Add tick marks to the Y axis on right side and the next two lines add some text explaining what just happened...
		axis(4, lab = F, tcl=-0.3)
		mtext("Fully recruited biomass ('000 t)", 2, 2.5, cex=1.25)
		legend('topright', c("prediction", "Model (excluding future years)", "Model (using all years)"), pch = c(16, 1, 3), bty = "n")
		if(graphic=='pdf')dev.off()
	} # end if(ts %in% plot)
	
	# Now the second plot which plots the time series median biomass along with the predicted for every model (year)
	# This could get really messy really quickly!!
	if("ts_all" %in% plot)
	{
	  # The number of years of data
		N <- length(out2)
		# The predicted biomass for each model
		preds <- sapply(names(out2), function(i){out2[[i]]$median$B.p[2]})
		# The actual (modelled) biomass was actually this...
		ests <- sapply(names(out2), function(i){rev(out2[[i]]$median$B)[1]})
		years<-as.numeric(names(out2))
		# This gets the biomass time series from each model run for any years that we have the data for from that model.
		Biomass <- sapply(names(out2),
		                function(i){out2[[i]]$median$B[(length(out2[[1]]$median$B)-length(years)+1):length(out2[[i]]$median$B)]})
		 
		# Get the graphics set up
		if(graphic=='pdf') pdf(paste(path,"pred_eval_all_models_",max(years),lab,".pdf",sep=''), width=8.5, height=11)
		if(graphic=="screen")windows(8.5,11)
    par(omi=c(0.1,0.1,0.1,0.1))
		# Make a blank plot with the boundaries all nice and tidy
    plot(years,years, type = 'n', ylim = c(0, (max(c(ests,preds))+min(c(ests,preds)))/1000), xlim=c(min(years)-1,max(years)+2), 
		     las=1, mgp = c(1,0.5,0), tcl=-0.3, pch=16, ylab="", xlab="")
		# Now add the points to the plot
    for(i in years)
		{
      # This is the median biomass from the model, it plots this for every model (year) we request.
			points(years[1:length(Biomass[[as.character(i)]])], Biomass[[as.character(i)]]/1000, type = 'b', pch = 16, lty=1)
			# So this compares the estimate with the prediction
      points(c(i,i+1), c(ests[as.character(i)],preds[as.character(i)])/1000, pch=3, cex=0.8,lty=3, type = 'b')
		} # for(i in 1:N)
		axis(4, lab = F, tcl=-0.3)
		mtext("Fully recruited biomass ('000 t)", 2, 2.5, cex=1.25)
		if(graphic=='pdf')dev.off()
	} # if(ts_all %in% plot)
	
	# This final plot is the boxplot of the data along with the prediction, DK modified to make this
	# the boxplot of both the estimate and the prediction for each model.
	if("box" %in% plot)
	{
		n <- length(out2)
		preds <- sapply(names(out2), function(i){out2[[i]]$median$B.p[2]})
		# The actual (modelled) biomass was actually this...
		ests <- sapply(names(out2), function(i){rev(out2[[i]]$median$B)[1]})
		years<-as.numeric(names(out2))
		# Get the maximum for y, convert to tonnes.
		ymax<-max(c(preds,ests,sapply(names(out2),function(i){quantile(out2[[i]]$proj.bm.sims,0.9)})))*0.001
		 
		# Get the graphics all set up.
		if(graphic=='pdf') pdf(paste(path,"pred_eval_boxplot_",max(years),lab,".pdf",sep=''), width=11, height=8.5)
		if(graphic=="screen") windows(11,8.5)
		par(omi=c(0.1,0.1,0.1,0.1))
		# Get the plot, offset the predictions by 1.25 years so we have room for the actual estimates...
		# This is simply ploting the median value for the projections
		plot(years+1.25, preds/1000, ylim = c(0, ymax), xlim=c(min(years),max(years)+2), las=1, mgp = c(1,0.5,0), xaxt="n", pch=16, ylab="", xlab="")
		axis(1,at=c(years,max(years)+1))
		
		for(i in years)
		{
		  # Get the Projected biomass for each year.
			pB<-out2[[as.character(i)]]$proj.bm.sims/1000
			# Get the modeled biomass for the current year
			aB<-out2[[as.character(i)]]$bm.sims[,ncol(out2[[as.character(i)]]$bm.sims)]/1000
			# Set up a box plot, include only 80% of the data, so an 80% box
			pB.box<-pB[ pB > quantile(pB,0.1) & pB < quantile(pB,0.9)]
			aB.box<-aB[ aB > quantile(aB,0.1) & aB < quantile(aB,0.9)]
			# Add the boxplot to the figure.
			boxplot(pB.box,add=T,at=i+1.25,axes=F,range=0,lty=1)
			# Add in the boxplot, but don't include the first year as we don't have a projection for that year so nothing really to compare.
			if(i != min(years)) boxplot(aB.box,add=T,at=i-.25,axes=F,range=0,lty=1,col="red")
		}
		# Tidy up the axes
		axis(4, lab = F, tcl=-0.3)
		mtext("Fully recruited biomass (kt)", 2, 2.5, cex=1.25)
		#legend('topright', c("prediction", "estimate", "estimate (full)"), pch = c(16, 1, 3), bty = "n")
		if(graphic=='pdf')dev.off()
	} # end if("box" %in% plot)
	
  # if we are making the plot output some information, some of which might be interesting...
	if(is.null(plot) ==F)
	{
		# Get the model estimates
	  ests <- rev(out2[[as.character(max(years))]]$median$B)[1:length(years)]
		# Get the difference between the predictions and the model estiamtes
	  resids<-rev(preds[-1]-ests[-length(ests)])
		# Same thing but the ratio.
	  prec<-rev(preds[-1]/ests[-length(ests)])
		# When we over project how bad is it?
	  print(paste("Mean over-projection",round(mean(prec[prec>1]),2),"times estimate"))
		# When we under project how bad is it?
	  print(paste("Mean under-projection",round(mean(prec[prec<1]),2),"times estimate"))
		# Where is the typical projection landing
	  print(paste("Mean projection",round(mean(prec),2),"times estimate"))
		# How far from the actual biomass are our projections on average (in terms of percetage differences.)
	  print(paste("Mean residual =",round(mean(abs(resids))/mean(ests)*100,2),"%"))
		# Make a table of these results.
	  tab1<-data.frame(Year=rev(pe)[-1], Residual=resids, Proportion=prec)
	} # end if(sum(plot) > 0)
	if(is.null(plot) ==F) list(tab1) # export the table if we make the plot
} # end function