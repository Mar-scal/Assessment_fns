#####################################
### This function is used to show the fit of the model to the actual data for Biomas, Recruits, and CPUE.
#####################################

# Update history
# DK revisions April 2016
# updated in Jan 2018 to allow for png's to be saved.

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
# 1:  data.out:     The model output. Default = missing, must be supplied
# 2:  name:         An optional name to give the plot output.  Potentially useful for saving test runs. Default = ""
# 3:  years:        The years of interest, thismust align with the model years. Default = missing, must be supplied
# 4:  CI:           Plot the confidence intervals:  (T/F) default = F
# 5:  CV:           Using the Coefficient of variation plot the standard error.  (T/F) default = F
# 6:  Iadj:         Determines whether the survey biomass is in bank biomass or biomass/tow. Default=1 gives bank biomass
#                   I am not sure what you would enter to get the biomass/tow (the obvious division by # of tows doesn't actually make sense)
# 7:  Uadj:         Some sort of adjustment to CPUE, not sure what values this could take.  Default=1
# 8:  graphic:      Where to plot the figures.  Options are 'screen' (default) and 'pdf'
# 9:  ymaxB:        Maximum y value for fully recruited biomass.  Default is missing
#10:  ymaxR:        Maximum y value for recruit biomass. Default is missing
#11:  alpha:        Default =0.05
#12:  path:         Path to save if producing a pdf, default = ""
#13:  wd:           The width of the figure, default = 8.5 
#14:  ht:           The height of the figure, default = 11
#15:  language:     default is english ("en"), for french put "fr"

#
###############################################################################################################

fit.plt <- function(data.out,name="",years, CI=F,CV=F,Iadj=1,Uadj=1,graphic='screen',ymaxB,ymaxR,alpha=0.05,path='',wd=8.5,ht=11, language="en")
{
  # Fit plots
  
  # Plot options, either a pdf or to the screen
  if(graphic=='pdf')pdf(paste(path,"fit", name, ".pdf", sep=""),width = wd, height = ht)
  if(graphic=='png') png(paste0(path,"fit",name,".png"), width = wd, height = ht,res=920,units="in")
  
  if(graphic=="screen")windows(width = wd, height = ht)
  
  # If we are ploting the CPUE time series set the figure up like so.
  if("qU"%in%names(data.out$median))par(mfrow = c(3,1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.3, 0, 0.2))
  # If not plotting CPUE then this is our set up.
  else par(mfrow = c(2,1), mar = c(1, 4, 1, 1), omi = c(0.2, 0.3, 0, 0.2))
  
  # If ymax for the biomass hasn't been supplied calculate it
  if(missing(ymaxB)==T)
  {
    # The ymax will vary depending on whether we are plotting the confidence intervals, this takes care of that...
    # Note the pmax function which grabs the maximum for each year then we grab the maximum of that
    # The apply statment is getting us the 1-aplha/2 BCI (default of 0.05 gives the upper 95% CI)
    ymaxB<-ifelse(CI==T,max(pmax(apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$q/Iadj), 2, quantile, 1-alpha/2), 
            data.out$data$I/Iadj)),max(c((as.numeric(data.out$median$B[1:length(years)])*data.out$median$q)/Iadj,data.out$data$I/Iadj)))
                                                            
  } # end if(missing(ymaxB)==T)
  # Now plot the data
  plot(years, (as.numeric(data.out$median$B[1:length(years)])*data.out$median$q)/Iadj, 
       type = 'l', lwd = 2, ylim = c(0, ymaxB), ylab = "", las = 1, xlim = c(min(years)-1, max(years)+1),
       mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', cex.axis=1.2)
  # Add tick marks to the yaxis on right side
  axis(4, lab = F, tcl = -0.3)
  
  # Add the appropriate y axis label
  if(Iadj!=1 & language=="en")mtext("Survey Biomass \n (kg/tow)", 2, 2.5, cex = 1.25)
  if(Iadj==1 & language=="en")mtext("Survey Biomass", 2, 3.5, cex = 1.25)
  if(Iadj!=1 & language=="fr")mtext("Survey Biomass \n (kg/tow)", 2, 2.5, cex = 1.25)
  if(Iadj==1 & language=="fr")mtext("Biomasse (relev\u00E9)", 2, 3.5, cex = 1.25)
  # If plotting credible intervals these are them.  Note they are 1-alpha/2 CI's, defaults to 95%.
  if(CI==T)
  {
  	lines(years, apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$q/Iadj), 2, quantile, alpha/2), lty = 2)
  	lines(years, apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$q/Iadj), 2, quantile, 1-alpha/2), lty = 2)
  } # end if(CI==T)
  # Add the actual datapoints to the figure.
  points(years, data.out$data$I/Iadj, col = 'red', pch = 16,cex=1.2)
  # If adding the standard error to the points (via the CV) here's the calculation
  if(CV==T)
  { 
    segments(years, data.out$data$I/Iadj+data.out$data$I.cv*(data.out$data$I/Iadj), years, 
                    data.out$data$I/Iadj-data.out$data$I.cv*(data.out$data$I/Iadj),col='red')
  } # if(CV==T)
  
  # Here is some sort of gear correction, I believe this is not in use (possibly an inshore thing, could be useful for German someday?)
  # This doesn't do anything bad so I'll leave it in for now
  if(!"ratiolined"%in%names(data.out$data))rl<-1
  if("ratiolined"%in%names(data.out$data))rl<-data.out$data$ratiolined
  # If maximum for recruits isn't specified use the data to calculate it.
  if(missing(ymaxR)==T)
  {  
    ymaxR<-ifelse(CI==T,max(pmax(apply(sweep(data.out$sims.list$R,2,FUN='*',data.out$median$q*rl/Iadj), 2, quantile, 1-alpha/2), 
                data.out$data$IR/Iadj)),max(c((as.numeric(data.out$median$R[1:length(years)])*data.out$median$q*rl)/Iadj,data.out$data$IR/Iadj)))
  } # if(missing(ymaxR)==T)

  # Plot the recruit data
  plot(years, (as.numeric(data.out$median$R[1:length(years)])*data.out$median$q*rl)/Iadj, type = 'l', lwd=2, ylim = c(0,ymaxR), 
       ylab = "", las = 1, xlim = c(min(years)-1, max(years)+1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', cex.axis=1.2)
  axis(4, lab = F, tcl = -0.3)
  # Add the appropriate y axis label
  if(Iadj!=1 & language=="en")mtext("Survey Recruit Biomass \n (kg/tow)", 2, 2.5, cex = 1.25)
  if(Iadj==1 & language=="en")mtext("Survey Recruit Biomass", 2, 3.5, cex = 1.25)
  if(Iadj!=1 & language=="fr")mtext("Survey Recruit Biomass \n (kg/tow)", 2, 2.5, cex = 1.25)
  if(Iadj==1 & language=="fr")mtext("Biomasse des recrues (relev\u00E9)", 2, 3.5, cex = 1.25)
  # If we asked for credbile limits plot them.
  if(CI==T)
  {
  	  lines(years, apply(sweep(data.out$sims.list$R,2,FUN='*',data.out$median$q*rl/Iadj), 2, quantile, alpha/2), lty = 2)
  	  lines(years, apply(sweep(data.out$sims.list$R,2,FUN='*',data.out$median$q*rl/Iadj), 2, quantile, 1-alpha/2), lty = 2)
  }# if(CI==T)
  # Add the recruit time series points.
  points(years, data.out$data$IR/Iadj, col = 'red',pch=16,cex=1.2)
  # If adding the standard error to the point (via the CV) here's the calculation
  if(CV==T)
  {
    segments(years, data.out$data$IR/Iadj+data.out$data$IR.cv*(data.out$data$IR/Iadj), years, 
             data.out$data$IR/Iadj-data.out$data$IR.cv*(data.out$data$IR/Iadj),col='red')
  }# end if(CV==T)
  
  # Finally if we want to plot the CPUE
  if("qU" %in% names(data.out$median))
  {
    # Get the max y value, depends if using the credible limits or not for the moment I have removed the CI's for the 
    # CPUE is they are not correct.
    #ymax<-ifelse(CI==T,max(pmax(apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$qU/Uadj), 2, quantile, 1-alpha/2), 
  	#                      data.out$data$U)),max(c((data.out$median$B[1:length(years)]*data.out$median$qU)/Uadj,data.out$data$U)))
    
    # For now we will just calculate ymax as just the maximum of the time series
    ymax<- max(c((as.numeric(data.out$median$B[1:length(years)])*data.out$median$qU)/Uadj,data.out$data$U))
    # Make the plot
  	plot(years, (as.numeric(data.out$median$B[1:length(years)])*data.out$median$qU)/Uadj, type = 'l', lwd=2, ylim = c(0, ymax), ylab = "", 
  	     las = 1, xlim = c(min(years)-1, max(years)+1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -0.3, asp = 'xy', cex.axis=1.2)
  	axis(4, lab = F, tcl = -0.3)
  	
  	if(language=="en") mtext("Commercial CPUE \n (kg/hm)", 2, 2.5, cex = 1.25)
  	if(language=="fr") mtext("Prises commerciales par\n unit\u00E9 d'effort (kg/hm) ", 2, 2.5, cex = 1.25)
  	#DK Note: I have removed the confidence intervals around the CPUE data as they are not correct.  It's something we need to
  	# revisit going forward.
  	#if(CI==T)
  	#{
  	 #lines(years, apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$qU/Uadj), 2, quantile, alpha/2), lty = 2)
  	 #lines(years, apply(sweep(data.out$sims.list$B,2,FUN='*',data.out$median$qU/Uadj), 2, quantile, 1-alpha/2), lty = 2)
  	 #} # end if(CI==T)
  	# Add the actual CPUE points.
  	points(years, data.out$data$U, col = 'red',pch=16,cex=1.5)
  	#segments(years, data.out$U+data.out$U.cv*data.out$U, years, data.out$U-data.out$U.cv*data.out$U,col='red')
  } # if("qU" %in% names(data.out$median))
  # If we've made the pdf shut'er down
  if(graphic!="screen")dev.off()
} # end function


