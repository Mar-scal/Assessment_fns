###################################################################################################
# This function produces the diagnostic plots from the model output.
###################################################################################
# Update history
# Commented and checked by DK in December of 2015
# DK revisions April 2016

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
# 1:  data:       Data from the MCMC model.
# 2:  years:      The years of interest, this must align with the model years. Default = missing, must be supplied
# 2:  graphic:    Plot to screen or a pdf.  Default = "screen", option is "pdf".
# 3:  path:       The path to put the pdf output if graphic ='pdf'.  Default is blank ""
# 4:  wd:         The width of the figure, default = 8.5 
# 5:  ht:         The height of the figure, default = 11
#
###############################################################################################################

diag.plt <- function(data, years, graphic="screen",path='',wd=8.5,ht=11)
{

  # If making a pdf output image this is where the figure will go.
  if(graphic=="pdf") pdf(paste(path,"diag.pdf", sep=""), width = wd, height = ht, pointsize=16,onefile=T)
  # if the data has a location with the name "sPresid" (standardized process residuals)
  if("sPresid" %in% names(data$median))	
  {
    # Size of the screen plot device if opened.
    if(graphic=="screen") windows(width = wd, height = ht)
    # If "qU" is in model output then we need a 4 panel plot
    if("qU" %in% names(data$median))par(mfrow=c(4, 1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
    # if it isn't then we just need  a 3 panel figure.
    else par(mfrow=c(3, 1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
    
    # Calculate the min/max for the y axis essentially using the 95% credible limits for sPresid's
  	ymin <- min(sapply(2:length(years), function(i){with(data$sims.list, quantile(sPresid[,i-1], 0.025))}))
  	ymax <- max(sapply(2:length(years), function(i){with(data$sims.list, quantile(sPresid[,i-1], 0.975))}))
  	# make the plot for the process standardized residuals.
  	plot(years, data$median$sPresid, las = 1, pch = 16, ylab = "",  ylim = c(ymin, ymax), mgp = c(0.5, 0.5, 0), xlab = "", 
  	     tcl = -0.3, cex.axis=1.2,xlim=range(years))
  	# Add axis and labels
  	axis(4, lab = F, tcl = -0.3)
  	mtext("Process", 2, 2.5)
  	mtext("Residuals (standardized)", 3, 0.5, cex =1.25)
  	# Toss in a dotted line at 0.
  	abline(h = 0, lty = 2)
  	# Add the 95% Credible intervals
  	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), 
  	                                          with(data$sims.list, c(quantile(sPresid[,i], 0.025), quantile(sPresid[,i], 0.975))))})
  	# Now get ymin/max for the biomass of fully recruited individuals.
  	ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(sIresid[,i], 0.025))}))
  	ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(sIresid[,i], 0.975))}))
  	# plot the biomass residuals
  	plot(years, data$median$sIresid, las = 1, pch = 16, ylab = "",  ylim = c(ymin, ymax), mgp = c(0.5, 0.5, 0), 
  	     xlab = "", tcl = -0.3, cex.axis=1.2,xlim=range(years))
  	axis(4, lab = F, tcl = -0.3)
  	mtext("Fully-recruited Biomass", 2, 2.5)
  	abline(h = 0, lty = 2)
  	# Add the 95% Credible intervals
  	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), 
  	                                          with(data$sims.list, c(quantile(sIresid[,i], 0.025), quantile(sIresid[,i], 0.975))))})
  	# Now get ymin/max for the biomass of recruit residuals
  	ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(sIRresid[,i], 0.025))}))
  	ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(sIRresid[,i], 0.975))}))
  	# plot the biomass residuals of the recruits.
  	plot(years, data$median$sIRresid, las = 1, pch = 16, ylab = "",  ylim = c(ymin, ymax), mgp = c(0.5, 0.5, 0), 
  	     xlab = "", tcl = -0.3, cex.axis=1.2,xlim=range(years))
  	axis(4, lab = F, tcl = -0.3)
  	mtext("Recruit Biomass", 2, 2.5)
  	abline(h = 0, lty = 2)
  	# Add the 95% Credible intervals
  	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), 
  	                                          with(data$sims.list, c(quantile(sIRresid[,i], 0.025), quantile(sIRresid[,i], 0.975))))})
    
  	# if we have CPUE as part of the data add this plot
  	# DK Note:  I commented this out as there is no evidence we've done this since 2011.  Was giving an error as "qU" exists
  	# but sresid.U does not.  Something to revist perhaps...
  	#if("qU" %in% names(data$median))
  	#  {
    #		ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(sresid.U[,i], 0.025))}))
    #		ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(sresid.U[,i], 0.975))}))
    #		plot(years, data$median$sresid.U, las = 1, pch = 16, ylab = "",  ylim = c(-3, 3), mgp = c(0.5, 0.5, 0), xlab = "", 
  	#                                          tcl = -0.3,xlim=range(years))
    #		axis(4, lab = F, tcl = -0.3)
    #		mtext("CPUE", 2, 2.5)
    #		abline(h = 0, lty = 2)
    #		sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), 
  	#                  with(data$sims.list, c(quantile(sresid.U[,i], 0.025), quantile(sresid.U[,i], 0.975))))})
    # } # end if("qU"%in%names(data$median))
  } # end if("sPresid" %in% names(data$median))	
  
  # if the data has a location with the name "Presid" (Process residuals)
  # this is identical to above just using the unstandarized residuals, so no detailed comments below here
  if("Presid"%in%names(data$median))	
  {
    if(graphic=="screen") windows(width = wd, height = ht)
    if("qU"%in%names(data$median))par(mfrow=c(4, 1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
    else par(mfrow=c(3, 1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
  
  	ymin <- min(sapply(2:length(years), function(i){with(data$sims.list, quantile(Presid[,i-1], 0.025))}))
  	ymax <- max(sapply(2:length(years), function(i){with(data$sims.list, quantile(Presid[,i-1], 0.975))}))
  	plot(years, data$median$Presid, las = 1, pch = 16, ylab = "",  ylim = c(ymin, ymax), mgp = c(0.5, 0.5, 0), xlab = "",
  	     tcl = -0.3, cex.axis=1.2,xlim=range(years))
  	axis(4, lab = F, tcl = -0.3)
  	mtext("Process", 2, 2.5)
  	mtext("Residuals", 3, 0.5, cex =1.25)
  	abline(h = 0, lty = 2)
  	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), 
  	                                          with(data$sims.list, c(quantile(Presid[,i], 0.025), quantile(Presid[,i], 0.975))))})
    	
  	ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(Iresid[,i], 0.025))}))
  	ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(Iresid[,i], 0.975))}))
  	plot(years, data$median$Iresid, las = 1, pch = 16, ylab = "",  ylim = c(ymin, ymax), mgp = c(0.5, 0.5, 0), 
  	     xlab = "", tcl = -0.3, cex.axis=1.2,xlim=range(years))
  	axis(4, lab = F, tcl = -0.3)
  	mtext("Fully-recruited Biomass", 2, 2.5)
  	abline(h = 0, lty = 2)
  	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), 
  	                                          with(data$sims.list, c(quantile(Iresid[,i], 0.025), quantile(Iresid[,i], 0.975))))})
  
  	ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(IRresid[,i], 0.025))}))
  	ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(IRresid[,i], 0.975))}))
  	plot(years, data$median$IRresid, las = 1, pch = 16, ylab = "",  ylim = c(ymin, ymax), mgp = c(0.5, 0.5, 0), xlab = "", 
  	     tcl = -0.3, cex.axis=1.2,xlim=range(years))
  	axis(4, lab = F, tcl = -0.3)
  	mtext("Recruit Biomass", 2, 2.5)
  	abline(h = 0, lty = 2)
  	sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), 
  	                                          with(data$sims.list, c(quantile(IRresid[,i], 0.025), quantile(IRresid[,i], 0.975))))})
  
  	
  	# if we have CPUE as part of the data add this plot
  	# DK Note:  I commented this out as there is no evidence we've done this since 2011.  Was giving an error as "qU" exists
  	# but sresid.U does not.  Something to revist perhaps...
  	#if("qU"%in%names(data$median))
  	#  {
    #		ymin <- min(sapply(1:length(years), function(i){with(data$sims.list, quantile(sresid.U[,i], 0.025))}))
    #		ymax <- max(sapply(1:length(years), function(i){with(data$sims.list, quantile(sresid.U[,i], 0.975))}))
    #		plot(years, data$median$sresid.U, las = 1, pch = 16, ylab = "",  ylim = c(-1, 1), mgp = c(0.5, 0.5, 0), 
  	#                                             xlab = "", tcl = -0.3,xlim=range(years))
    #		axis(4, lab = F, tcl = -0.3)
    #		mtext("CPUE", 2, 2.5)
    #		abline(h = 0, lty = 2)
    #		sapply(1:length(years), function(i){lines(c((years[1]-1) + i, (years[1]-1) + i), 
  	#                    with(data$sims.list, c(quantile(sresid.U[,i], 0.025), quantile(sresid.U[,i], 0.975))))})
  	#  } # end if("qU"%in%names(data$median))
  } # end if("Presid"%in%names(data$median))
  
  # if plot device was opened other than to print to screen then close it.
  if(graphic=="pdf") dev.off()
	
}	# end function