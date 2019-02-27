####  Commented and checked by DK starting on July 28, 2015. Time series plot of condition factor, average shell height, and 
####  average meat weight, potentially could be used with others.
# Update history
#Commented, checked  and revised by DK March 31, 2016
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
# ok
#    
#      
##
###############################################################################################################

# Arguments

# data:      The data to plot
# x:          name of the x data.  Default = names(data[1]), 
# y:          name of the y data.  Default = names(data[2]), 
# ylab:       y axis label.  Default = y 
# xlab:       x axis label.  Default = x
# mean.line:  Plot the mean of the lines(T/F) default = F,
# graphic:    What plotting device to open.  Default ="R", "pdf" will produce a figure and save it to specified directory.
# width:      width of the plot window.  Default = 11 
# height:     height of the plot window.  Default = 8.5
# labcx:      Magnification of labels. Default = 1.25
# ylim:       the upper and lower y bounds, missing by default so automatically added by R
# xlim:       the upper and lower x bounds, missing by default so automatically added by R
# col:        The color of the points/lines added to figure.  Unique color for each by default, which is rep(1,length(y)), 
# pch:        The symbols to add to the figure. Unique symbols for each point by default, which is 1:length(y) 
# lty:        The line types to add to the figure. Unique line types for each point by default, which is 1:length(y) 
# type:       The type of plot, default ='o' which is overplotting (draws full line and points).  See ?plot for options.
#titl:        Title for the plot.  Default is blank
#cex.mn:      Magnificaiton of the plot title.  Default = 1.2
#axis.cx:     Magnification of the axes.  Default = 1
#tx.ypos:     Position of the margin text for y axis.  Default = 5.3

stdts.plt <- function(data, x=names(data[1]), y=names(data[2]), ylab=y, xlab=x, mean.line=F,graphic="R",width = 11, 
                      height = 8.5, labcx=1.25, ylim, xlim, col=c("blue","red","grey50","orange"), pch=1:length(y), lty=1:length(y),type='o',
                      titl="",cex.mn = 1.2,axis.cx=1,tx.ypos = 5.3, ...)
{
  # Do we want to make pdf out plot.
  if(graphic=='pdf')pdf("plots/cfindex.pdf", width = width, height = height, pointsize = 14)
	# Or use an R window
  #if(graphic=="R") windows( width = width, height = height)
	# Calculate the mean of the response variable (y's)
  meany<-colMeans(data[y],na.rm=T)
	print(meany)
  # Add in xlim/ylim in not specified.
	if(missing(xlim))xlim=range(data[x],na.rm=T)
	if(missing(ylim))ylim=range(data[y],na.rm=T)
	# Set margins and leave the plot window open
  par(mar=c(3.6,6,2,2))
  par(...)
  # Make the plot, using the parameters specified in function call.  
	plot(unlist(data[x[1]]),unlist(data[y[1]]), type="n", las=1, ylim=ylim, xlim=xlim, ylab="", xlab="", xaxt="n",,yaxt="n",
	     mgp=c(1,0.5,0), lty=lty[1], pch=pch[1], col=col[1],cex=labcx)
	points(unlist(data[x[1]]),unlist(data[y[1]]),type=type,lty=lty[1], pch=pch[1], col=col[1],bg=col[1])
	# If there is just 1 name for x, repeat x for all y values.
	if(length(x)==1)x<-rep(x[1],length(y))
	# if there is more than 1 y name then add the lines and points to the figure for each x/y combination.
	if(length(y)>1)for(i in 2:length(y)){points(unlist(data[x[i]]),unlist(data[y[i]]), type='o', lty=lty[i], 
	                                            pch=pch[i], col=col[i],bg=col[i])}
	# Draw the mean line for the data
	
	if(mean.line)abline(h=meany,lty=3,lwd=2,cex=1, col=col)
	# Make the rest of the plot pretty.
	axis(4,lab=F, cex.axis=labcx)
	axis(1,at=data$year,lab=F,tcl=-0.3, cex.axis=labcx)
	axis(1,label=T,cex.axis=axis.cx)
	axis(2,label=T,cex.axis=axis.cx)
	spc <- (axTicks(2)[2]-axTicks(2)[1])
	rng <- seq(axTicks(2)[1]-spc/2,axTicks(2)[length(axTicks(2))] + spc/2,by=spc)
	axis(2,at=rng,label=F,cex.axis=axis.cx,tcl=-0.3)
	mtext(xlab, 1, 2.5, cex=labcx)
	mtext(ylab, 2, cex=labcx,las=1,adj=0.5,line=tx.ypos)
	title(titl,cex.main=cex.mn)
	# If we opened a pdf device shutter down.
	if(graphic=='pdf')dev.off()
}	# end function
