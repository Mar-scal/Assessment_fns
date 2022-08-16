####  Commented and checked by DK starting on July 28, 2015.Plots the Meat Weight shell height data + model fit
####  obtained from the function shwt.lme.r
## Update history
## 1: March 31 2016 by DK, tidying up structure and comments
## 2: July 31, 2019 by DK, changed the size, and colors/size of the points and the random effects fits
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

# ARGUMENTS 
# htwt.fit: A model fit from the shwt.lme.r function, specifically MW-SH 
# graphic: Plot in an 'R' window or output to a 'pdf'.  Default is "R".
# ht:      height of the graphic window.  Default = 7
# wd:      width of the graphic window.  Default = 7
# cx:      text/point size magnification in plot.  Default = 1
# lw:      Thickness of the line of best fit plotted.  Default =2
# xl:      Optional specify the x limits for the plot  
# yl:      Optional specify the y limist for the plot
#titl:     Title for the plot.  Default is blank
#cex.mn:   Magnificaiton of the plot title.  Default = 1.2
#axis.cx:     Magnification of the axes.  Default = 1

#		source("fn/shwt.plt1.r")

shwt.plt1 <- function(htwt.fit,graphic='R',ht=8,wd=11.5,cx=1.2,lw=2,xl,yl,titl="",cex.mn = 1.2,axis.cx=1, ...)
{
browser()
	# Open appropriate plot device
	if(graphic=='pdf')pdf("plots/shwt.pdf", width = wd, height = ht, pointsize = 14)
	#if(graphic=='R')windows(wd,ht)

  # Set margins and leave the plot window open
  if(!nchar(titl) > 35) par(mar=c(5,6,2,2))
  if(nchar(titl) > 35) par(mar=c(5,6,6,2))
  par(...)	
	# If not specified use the data to determine the x and y axis limits.
	if(missing(xl))xl<-range(htwt.fit$data$sh)
	if(missing(yl))yl<-c(0, max(htwt.fit$data$wmw) + 5)
	
	# Draw the plot of raw data
	plot(wmw ~ sh, data = htwt.fit$data, xlim = xl, ylim = yl, col = rgb(1,0,0,0.3), 
	     las = 1, mgp = c(0.5, 0.5, 0), xlab ="", ylab = "",xaxt="n",yaxt="n",pch=19,cex=0.5)
	axis(1,at=seq(0,2,0.2),label=seq(0,200,20),cex.axis=axis.cx)
	axis(1,at=seq(0,2,0.1),label=F,cex.axis=axis.cx,tcl=-0.3)
	axis(2,at=seq(0,100,by=10),label=T,cex.axis=axis.cx)
	axis(2,at=seq(0,100,by=5),label=F,cex.axis=axis.cx,tcl=-0.3)
	
	# Add a grid to the plot
	#grid(col = "grey30")
	abline(h=seq(0,100,by=10),v=seq(0,2,by=0.2),col="grey30",lty=3,lwd=0.7)
	# Make an object covering the range of shell heights (for plotting the line of best fit for each 
	# random effect (tow,year,etc) + overall fixed effect)		
	x <- with(htwt.fit$data, seq(min(sh, na.rm = T), max(sh, na.rm = T), length = 40))
	
	# Draw thin lines showing fit of each random effect (tow)
	for(i in 1:nrow(htwt.fit$fit))
	  {
	  	lines(x, x^htwt.fit$B * htwt.fit$fit[i,2], col = rgb(1,0,0,0.2),lwd=1.5)
	  } # end for(i in 1:nrow(fit))
	
	# Draw a thick blue line with fixed effect estimate
	lines(x, x^htwt.fit$B * htwt.fit$A, lwd = lw, col = 'blue')
		
	# Add the axis labels the adj = 0.5 effectively centers the text
	mtext("Shell height (mm)", 1, 2.5,cex=cx)
	mtext(side=2,line=4,cex=cx,"Meat\n weight\n(g)",adj=0.5,las=1)
	title(titl,cex.main=cex.mn)
  # Shut down the R plot device
	if(graphic != 'R') dev.off()
		
}	# End shwt.plt1