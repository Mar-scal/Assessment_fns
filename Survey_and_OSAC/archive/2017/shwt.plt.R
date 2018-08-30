####  Commented and checked by DK starting on July 28, 2015. This makes a plot of each year of data 
####  with the model fit in a gridded window.  This is the same data as used in shwt.plt1 just shows
####  the annual trends more easily.
## Update history
## March 31 2016 by DK, tidying up structure and comments
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "Survey_Summary_figures.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################

# Arguments
#htwt.fit:  The model fit
#graphic:   The plotting device to use.  Default ='R', 'pdf' is also acceptable
#nr:        The number of rows to plot. Default = 6
#ht:        Height of the plot device.  Default = 7
#wd:        Width of the plot device.  Default = 11
#fixed.b:   Is the model using a fixed value for 'b', i.e. the allometric assumption. (T/F) Default = T
#labcx      Label size multiplier.  Default = 1
#pt.col:    The color of the data points.  Default = rgb(0,0,0,0.05)


#		source("fn/shwt.plt.r")

shwt.plt <- function(htwt.fit,graphic='R',nr=6,ht=7,wd=11,fixed.b=T,labcx=1,pt.col= rgb(0,0,0,0.05))
{
  # define some variable names
	x <- list(NULL)
	y <- list(NULL)
	y.all <- list(NULL)
	
	# What graphic device to use
	if(graphic=='pdf')pdf("plots/shwt.pdf", width = wd, height = ht, pointsize = 14)
	if(graphic=='R')windows(wd,ht)

	# Pull out the random effects
	ran.effects<-htwt.fit$fit[,1]
	# set up the multi-windowed plot device
	par(mfrow=c(nr, ceiling(length(ran.effects)/nr)), mar = c(2,2,0,0), omi = c(0.5, 0.5, 0.1, 0.1))
	
	
	# Run the model through each random effect term.
	for(i in 1:length(ran.effects))
	  {
	    # make the plot.
  		plot(wmw ~ sh, data = htwt.fit$data, subset = raneff == ran.effects[i],pch=16, xlim = range(sh), 
  		     ylim = c(0, max(wmw) + 5), col = pt.col, las = 1, mgp = c(0.5, 0.5, 0), xlab ="", ylab = "", tcl = -0.3)
  	  # add a grid
	  	grid(col = "grey30")
		  
	  	# If using a full model this is the proper model fit
  		if(fixed.b==F)
  		  {
    			x[[i]] <- with(subset(htwt.fit$data, raneff == ran.effects[i]), 
    			               seq(min(sh, na.rm = T), max(sh, na.rm = T), length = 40))
    			y[[i]] <- exp(log(x[[i]]) * htwt.fit$fit$b[i] + htwt.fit$fit$a[i])
    			y.all[[i]] <- exp(log(x[[i]]) * htwt.fit$B + htwt.fit$A)
  		  }	# end if(fixed.b==F)
  		
	  	# If using the allometric model this is the model fit
  		if(fixed.b==T)
  		  {
  		    # Calculate the x and y data using the model results
    			x[[i]] <- with(subset(htwt.fit$data, raneff == ran.effects[i]), 
    			               seq(min(sh, na.rm = T), max(sh, na.rm = T), length = 40))
    			y[[i]] <- x[[i]]^htwt.fit$B * htwt.fit$fit[i,2]
    			y.all[[i]] <- x[[i]]^htwt.fit$B * htwt.fit$A
  		  } # end if(fixed.b==T)

	  	  # Add the line of best fit for the random effect and for the main effects.
    		lines(x[[i]], y[[i]], lwd = 2, col = 'red')
    		lines(x[[i]], y.all[[i]], lwd = 2, col = 'blue')
    		# and some nice test.
    		text(min(htwt.fit$data$sh), max(htwt.fit$data$wmw) - 2, 
    		     unique(htwt.fit$data$label[htwt.fit$data$raneff==ran.effects[i]]), cex = labcx,pos=4)
	
		}# end for(i in 1:length(ran.effects)){
		
	  # Add teh MW shell height lables.
	  mtext("Shell height (mm)", 1, 1, outer = T)
		mtext("Meat weight (g)", 2, 1, outer = T)
	
	# Turn off plot object if necessary
	if(graphic != 'R') dev.off()
		
}	# end function