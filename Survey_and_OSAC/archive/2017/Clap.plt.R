####  Commented and checked by DK starting September 2015. Simple plot of the clapper time series. Note that this is ALL clappers!!

## Update history
## 1: March 31 2016 by DK, tidying up structure and comments
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

# source(".../Assessment_fns/Clap.plt.R")

#Arguments
#data:        The clapper time series
#years:       The years to plot.  Default = 1981:2009
#mean.line:   Add the average over the time series.  (T/F) Default = T
#graphic:     The plot device type.  Options are 'pdf' or "R", Default = "R"
#width:       Width of the plot device.  Default = 11
#height:      Height of the plot device.  Default = 8.5
#yl:          The y axis min/max.  Default is missing and is based on the range of the data
#data2:       A secondary clapper time series to add to the plot.
#add.title:   Add a title to the plot. (T/F) default = F
#titl:        Plot title.  (T/F) default = F
#cex.mn:      Magnification of plot title.  Default =1
#axis.cx:    Magnifcation of axies.  Default =1
#lab.cx:    Magnifcation of axes labels.  Default =1

Clap.plt <- function(data, years=1981:2009,mean.line=T,graphic="R",width = 11, height = 8.5,yl, data2,axis.cx = 1,lab.cx=1,
                     add.title=F,titl="",cex.mn=1,...)
{

# set up the plotting device
if(graphic=='pdf')pdf("plots/Clappers.pdf", width = width, height = height, pointsize = 14)
#if(graphic=="R")windows( width = width, height = height)

  # Subset the data into the years of interest
claps<-with(subset(data,year %in% years),tapply(clap.prop,year,mean))

# If yl is not specified calculate the y axis limits using the data
if(missing(yl) == T)yl<-range(claps)
par(mar = c(3,6,3,2))

# Plot the data, type changed to "o" from "b" by DK in September 2015.
plot(years,claps, type='o', las=1, ylim=yl, ylab="", xlab="", mgp=c(1,0.5,0), pch=16,xaxt="n",yaxt="n")
if(add.title==T) title(titl,cex.main = cex.mn)
# If there is a second data object subset it for the years of interest and add it to the plot as a grey line
if(missing(data2)==F )
  {
	  claps2<-with(subset(data2,year%in%years),tapply(clap.prop,year,mean))
	  lines(as.numeric(names(claps2)),claps2, type='o', col='grey50')
  } # END if(missing(data2)==F )

# Add the mean line if requested.
if(mean.line == T) abline(h=median(claps[-length(years)]),lty=2)

# Add some nice axis tick marks and the y axis text and the title
axis(1, labels=T,cex.axis=axis.cx)
axis(1, at=years,labels=F,tcl=-0.3)
axis(2,labels=T,cex.axis=axis.cx,las=1)
axis(4, labels=F, tcl=-0.3)
mtext(expression(frac("% dead",tow)), 2, 2, cex=lab.cx,las=1,padj=0.5)


# Shut off the device and we're done.
if(graphic != "R") dev.off()
	
}	# End function