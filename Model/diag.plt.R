###################################################################################################
# This function produces the diagnostic plots from the model output.
###################################################################################
# Update history
# Commented and checked by DK in December of 2015
# DK revisions April 2016
# DK overhaul into a loop + allows for the clapper residual plots to be added, added flexibility to the function.
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
  # A little function we'll use later to determine how to set up the ploting device.
  is.even <- function(x) x %% 2 == 0
  # If making a pdf output image this is where the figure will go.
  if(graphic=="pdf") pdf(paste(path,"diag.pdf", sep=""), width = wd, height = ht, pointsize=16,onefile=T)
  
  # Size of the screen plot device if opened.
  if(graphic=="screen") windows(width = wd, height = ht)
  
  # Now I can set this up to plot the standardized and raw residuals
  plots <- NULL
  # These are the columns in the data pulling to make the standardized residual plots
  plots[["stan"]] <- grep("resid",names(data$median))[(grep("resid",names(data$median)) %in% grep("^s",names(data$median)))]
  # These are the columns in the data that I'm pulling to make the raw residual plots.
  plots[["raw"]] <- grep("resid",names(data$median))[-which((grep("resid",names(data$median)) %in% grep("^s",names(data$median))))]
  
  # Now I can loop through if we have both standardized and raw residuals, or this jsut runs once if we just have one or the other.
  for(i in 1:length(plots))
  {
    if(graphic=='png') png(paste0(path,"diag_",names(plots)[i],".png"), width = wd, height = ht,res=920,units="in")
    n.plots <- length(plots[[i]])
    # If it is an even number it's simple enough to layout the figure
    if(is.even(n.plots) == T) par(mfrow = c(n.plots/2,n.plots/2),mar = c(4, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
    # If we have an odd number of plots we need to be a little funky...
    if(is.even(n.plots) == F) 
    {
      if(n.plots ==1) par(mfrow = c(1,1),mar = c(4, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
      if(n.plots > 1)
      {
        lay <- layout(matrix(c(1,1,(seq(2,n.plots))),length(0:n.plots)/2,2,byrow=T))
        par(mar = c(4, 4, 1, 1), omi = c(0.2, 0.1, 0.2, 0.2))
      } # end if(n.plots > 1)
    } # end if(is.even(n.plots) == F) 
    
    
    for(j in 1:n.plots)
    {
      
      resid.name <- names(data$sims.list)[plots[[i]][j]]
      # Calculate the min/max for the y axis essentially using the 95% credible limits for sPresid's
      ymin <- min(sapply(2:length(years), function(p){quantile(data$sims.list[resid.name][[1]][,p-1], 0.025)}))
      ymax <- max(sapply(2:length(years), function(p){quantile(data$sims.list[resid.name][[1]][,p-1], 0.975)}))
      # make the plot for the process standardized residuals.
      plot(years, data$median[[resid.name]], las = 1, pch = 16, ylab = "",  ylim = c(ymin, ymax), mgp = c(0.5, 0.5, 0), xlab = "", 
           tcl = -0.3, cex.axis=1.2,xlim=range(years))
      # Add axis and labels
      axis(4, lab = F, tcl = -0.3)
      # For the residuals
      if(resid.name %in% c("sCRresid","CRresid")) mtext("Recruit clappers", 2, 2.5)
      if(resid.name %in% c("sCresid","Cresid")) mtext("Fully-recruited clappers", 2, 2.5)
      if(resid.name %in% c("sIRresid","IRresid")) mtext("Recruit biomass", 2, 2.5)
      if(resid.name %in% c("sIresid","Iresid")) mtext("Fully-recruited biomass", 2, 2.5)
      if(resid.name %in% c("sPresid","Presid")) mtext("Process", 2, 2.5)
      if(names(plots)[i] == "stan") mtext("Residuals (standardized)", 3, 0.5, cex =1.25)
      if(names(plots)[i] == "raw") mtext("Residuals (raw)", 3, 0.5, cex =1.25)
      # Toss in a dotted line at 0.
      abline(h = 0, lty = 2)
      # Add the 95% Credible intervals
      sapply(1:length(years), function(p){lines(c((years[1]-1) + p, (years[1]-1) + p), 
                                                c(quantile(data$sims.list[resid.name][[1]][,p], 0.025), 
                                                  quantile(data$sims.list[resid.name][[1]][,p], 0.975)))})
      
    } # end for(j in 1:n.plots)
    if(graphic =="png") dev.off()
    
  }  # end for(i in 1:length(plots))
  if(graphic=="pdf") dev.off()
}	# end function