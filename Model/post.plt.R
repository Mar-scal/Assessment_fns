### This function is used to make the posterior historgrams and to compare these with the priors used when applicable.  This also
### plots the prediction posteriors if they are available tho they are far less interesting...

# Update history
# DK revisions April 2016
# updated in Jan 2018 to allow for png's to be saved. for the single posterior plot, for the annuals this needs to stay a pdf due to the number of pages

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
# 1:  model.out:  The model output, works with raw model output and with the projections. Default = missing, must be supplied
# 2:  priors:     The model priors.  Default = missing, must be supplied
# 3:  years:      The years of interest, this must align with the model years. Default = missing, must be supplied
# 4:  nr:         Number of rows for the figures.  Default = Null, rows determined from the number of years (approximately square figure) 
# 5:  nc:         Number of rows for the figures.  Default = Null, columns determined from the number of years (approximately square figure) 
# 6:  wd:         The width of the figure, default = 8.5 
# 7:  ht:         The height of the figure, default = 11
# 8:  graphic:    Where to plot the figures.  Options are 'screen' (default) and 'pdf'
# 9:  xl.type:    The x axis range for multiple year priors/posteriors.  Default of 1 has a common x axis range 
#                 for all years, 2 allows x-axis range to vary 
#10:  multi:      Plot the posteriors for parameters estimated each year.  (T/F) default=T
#11:  path:       Path to save if producing a pdf, default = ""

post.plt <- function(model.out, priors, years, nr=NULL, nc=NULL, wd=8.5, ht=11, graphic='screen',xl.type=1, multi=T,path='')
                     
{

  # If graphic is a pdf make the pdf file
	if(graphic=='pdf') pdf(paste0(path,"post_single.pdf"), width = wd, height = ht, pointsize = 16)
  if(graphic=='png') png(paste0(path,"post_single.png"), width = wd, height = ht,res=920,units="in")
  
	if(graphic=='screen')windows(record=T,wd, ht) # if printing to screen than print to screen.
  
  # We don't want the deviance
  model.out$sims.list <- subset(model.out$sims.list,names(model.out$sims.list) != "deviance")
  # This picks out the model parameters for which there is only 1 parameter for the model (rather than the parameters estimated annually)
  posts<- model.out$sims.list[lapply(model.out$sims.list,ncol)==1]
	# If you don't specify both the number of rows (nr) and columns (nc) then use the data to determine them, 
	# if you only specify one of them it will default to calculating from the number of parameters we have...
	# Note that deviance isn't plotted but is part of the "posts"
	if(is.null(nr) == T || is.null(nc) == T)
	{
	  # If less than 15 posts we'll make it a 2 column figure, if more than 14 I'll go with 3 columns and hope it works...
	  ifelse(length(posts) <= 14,nc <- 2,nc <- 3)
	  nr <- ceiling(length(posts)/nc)
	} # if(is.null(nr) == T && is.null(nc) == T)
	  
	# Set up the plot device
	par(mfrow = c(nr, nc), mar = c(2, 3, 1, 1), omi = c(0.4, 0.6, 0, 0.2))
	
	# Now run the for each parameter calculated once
	for (i in 1:(length(posts)))
	{
		  # If the name in posts[i] is found in the prior list do the below
		  # Set the range from min-max in the data, this is reset for beta, log-normal, and gamma distributions if we have the prior at the next step
		  xl<-range(posts[[i]])
      # If we have a posterior and a prior for a parameter.
		  if(names(posts)[i] %in% names(priors))
			{
			  # If the distribution is beta x goes from 0-1
				if(priors[[names(posts)[i]]]$d =="dbeta") xl=c(0,1)
				# If dlnorm or dgamma it runs from 0 to maximum observed
				if(priors[[names(posts)[i]]]$d%in%c("dlnorm","dgamma")) xl<-c(0,max(posts[[i]]))
				# Make a fake x that goes from the min-max with as many values as there are posterior samples.
				x<-seq(xl[1], xl[2], l = length(posts[[i]]))
				# If we have a log-normal or normal prior our precision term needs to be converted back to a standard deviation
				if(priors[[names(posts)[i]]]$d %in% c("dnorm","dlnorm")) priors[[names(posts)[i]]]$b <- 1/sqrt(priors[[names(posts)[i]]]$b) 
				# Using the prior specified distribution get values across the range of the data.
				p<-get(priors[[names(posts)[i]]]$d)(x,  priors[[names(posts)[i]]]$a,  priors[[names(posts)[i]]]$b)
			} # if(names(posts)[i] %in% names(priors))
			# The histogram of the posteriors
			tmp <- hist(posts[[i]], breaks = 25, main = "", prob = T, ylab = "", las = 1, mgp=c(1,0.4,0), tcl=-0.3, xlab="",cex.axis=1.2,xlim=xl)
			# If we have a uniform prior we need to adjust it so the uniform line shows up at the proper density
			if(names(posts)[i] %in% names(priors) && priors[[names(posts)[i]]]$d == "dunif") 
			{
			  p <- rep((length(posts[[i]])/length(tmp$breaks))/length(posts[[i]])/(tmp$breaks[2]-tmp$breaks[1]),length(posts[[i]]))
			} # end if(names(posts)[i] %in% names(priors) && priors[[names(posts)[i]]]$d == "dunif") 
			
 			# If the posterior had a prior assigned then add the line.
			if(names(posts)[i] %in% names(priors)) lines(x, p, col = 'red')
			# Add the x-axis label.
			mtext(names(posts)[i], 1, 2, cex=1)
			# The y-axis label.
			if(i == 1) mtext("Posterior density", 2, 2, outer = T, adj = 0.5, cex=1.25)
	} # end for (i in 1:(length(posts)))
  # Stick a y axis label on here, hopefully at a nice location...
	mtext("Posterior density", 2, 2, outer = T, adj = 0.5, cex=1.25)
	# If making a pdf shut it down.
	if(graphic!="screen") dev.off()	
		
	# Now for the posteriors for which we have more than one estimate (generally crap we add annually)
	if(multi==T)
	{
	  # Grab the multiple posteriors, including the projections.
	  multiposts<-model.out$sims.list[lapply(model.out$sims.list,ncol)>1]
		
		# This will make a pdf of the annually estimated posteriors.
		if(graphic !='screen') pdf(paste0(path,"post_annual.pdf"), width = wd, height = ht, pointsize = 12,onefile=T)
		
		# This runs through each of the posteriors with multiple input attached.
		for (i in 1:length(multiposts))
		{
		  # Open a screen for each of these, 
		  if(graphic=='screen') windows(wd, ht)
		  # Plot device for a page, all years will go on one page
		  par(mfrow = c(ceiling(length(years)/ceiling(sqrt(length(years)))), 
		                ceiling(sqrt(length(years)))), mar = c(2, 4, 2, 1), omi = c(0.4, 0.4, 0, 0.2))
			# Now run through each of these parameters by year.
			for(y in 1:ncol(multiposts[[i]]))
			{
			  
			  # Set the range from min-max in the data, this is reset for beta, log-normal, and gamma distributions if we have the prior
			  if(xl.type==1) xl<-range(multiposts[[i]]) ;  x<-seq(xl[1], xl[2], l = length(multiposts[[i]][,y]))
			  # If we set axis range to 2 then the range for each plot is the maxium for the year.
			  if(xl.type==2) xl<-range(multiposts[[i]][,y]) ;  x <-seq(xl[1], xl[2], l = length(multiposts[[i]][,y]))
			  
			  # So grab all the posteriors for these that we have priors for
				if(names(multiposts)[i] %in% names(priors) && xl.type==1)
				{
	        
				  # If it's a beta go from 0-1
					if(priors[[names(multiposts)[i]]]$d=="dbeta") xl=c(0,1)
					# constrain x-axis of log-normals and gamma's to be positive
					if(priors[[names(multiposts)[i]]]$d%in%c("dlnorm","dgamma"))xl<-c(0,max(multiposts[[i]]))
					# For priors that are the same each year repeat the prior value for each year so we can make the plots pretty.
					if(length(priors[[names(multiposts)[i]]]$a) == 1) 
					{
					  priors[[names(multiposts)[i]]]$a <- rep(priors[[names(multiposts)[i]]]$a,priors[[names(multiposts)[i]]]$l)
					  priors[[names(multiposts)[i]]]$b <- rep(priors[[names(multiposts)[i]]]$b,priors[[names(multiposts)[i]]]$l)
					} # end if(length(priors[[names(multiposts)[i]]]$a) == 1) 
					# If we have a log-normal or normal prior our precision term needs to be converted back to a standard deviation
					if(priors[[names(multiposts)[i]]]$d %in% c("dnorm","dlnorm")) 
					{  
					  priors[[names(multiposts)[i]]]$b[y] <- 1/sqrt(priors[[names(multiposts)[i]]]$b[y]) 
					}  # end if(priors[[names(multiposts)[i]]]$d %in% c("dnorm","dlnorm")) 
					# x goes from min to max
					x<-seq(xl[1], xl[2], l = length(multiposts[[i]][,y]))
					# Get the predicted p values for each distribution.
					p<-get(priors[[names(multiposts)[i]]]$d)(x,  priors[[names(multiposts)[i]]]$a[y],  priors[[names(multiposts)[i]]]$b[y])
				} # end if(names(multiposts)[i]%in%names(priors)&&xl.type==1)

			  tmp <- hist(multiposts[[i]][,y], breaks = 30, main = as.character(years[y]), prob = T, las = 1,ylab="",xlim= xl)
			  
			  # If we have a uniform prior we need to adjust it so the uniform line shows up at the proper density
			  if(names(multiposts)[i] %in% names(priors) && priors[[names(multiposts)[i]]]$d == "dunif") 
			  {
			    p <- rep((length(multiposts[[i]][,y])/length(tmp$breaks))/length(multiposts[[i]][,y])/
			                                         (tmp$breaks[2]-tmp$breaks[1]),length(multiposts[[i]][,y]))
			  } # end if(names(posts)[i] %in% names(priors) && priors[[names(posts)[i]]]$d == "dunif") 
				# if we have priors draw the prior line
				if(names(multiposts)[i]%in%names(priors))lines(x, p, col = 'red')
			  # Add the y axis label.
			  if(y ==1) mtext("Posterior density", 2, 1, outer = T, adj = 0.5, cex=1.25)
			} # end for(y in 1:ncol(multiposts[[i]]))
			mtext(names(multiposts)[i], 1, 1, outer = T, adj = 0.5, cex=1.25)
		} # end for (i in 1:length(posts))
	  if(graphic !="screen")dev.off()	 # Turn off the pdf device
	} # end if(multi==T)
}# end function
	


