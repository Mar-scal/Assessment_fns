###############  This function creates the Meat count, shell height "breakdown" figure used in OSAC
###############  This figure shows the biomass of each shell height class with the meat count of that size class
###############  Generally it is useful for showing the biomass in the smaller size classes and their expected
###############  meat count if we let them grow.
#####
# # Update history
#Commented, checked  and revised by DK March 31, 2016
# July 11, 2016:  Added the option to leave out the title and tidied up the function call (some old un-used arguements...)

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  OSAC_summary.r 
##  Survey_summary_figures.r)
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files"), 
##
###############################################################################################################

###############################################################################################################
# Arguments
# 1:  surv.obj: The processed survey results, particularly the shell height frequency data
# 2:  yr:       The year of interest
# 3:  CS:       The minimum size of commercial scallop.  Default is NULL and gets extracted from the surv.obj
# 4:  RS:       The maximum size of commercial scallop  Default is NULL and gets extracted from the surv.obj
# 5:  xlim:     The x axis limits.  Default is NULL and lower bound is set as 15 mm smaller than RS, upper bound = 160 mm
# 6:  y1max:    The maximum for the BM axis.  Default is NULL and is 10% above maximum biomass for a bin
# 7:  y2max:    The maximum for the MC axis.  Default is based on MC.
# 8:  col1:     Color of the histogram
# 9:  mc:       Meat count regulation for the bank.  Default = 40.
# 10: title:    Figure title.  Default is blank
# 11: cx.axs:   Axis number size magnification
# 12: cx.lab:   Axis label size magnification
# 13: cx.mn:    Title size magnification
# 14: add.title:Add a title to the figure.  T/F, default = T.
###############################################################################################################


breakdown<-function(surv.obj,yr=2010,CS=NULL,RS = NULL,xlim=NULL,y1max=NULL,y2max=NULL,col1='grey',
                    mc=40,title=paste("Breakdown",yr),cx.axs=1.5,cx.lab=1.5,cx.mn=2,add.title=T, value=F)
{
    # Grab the biomass and numbers for each bin size from the survey, this is in kg per tow and number per tow.
  	bm<-surv.obj$shf.dat$w.yst[which(surv.obj[[1]]$year==yr),which(seq(5,200,5) >= 5)]/1000
  	num<-surv.obj$shf.dat$n.yst[which(surv.obj[[1]]$year==yr),which(seq(5,200,5) >= 5)]
  	# Get the Commercial size if it hasn't been specified
  	if(is.null(CS)==T) CS <- surv.obj$model.dat$CS[length(surv.obj$model.dat$CS)]
  	if(is.null(RS)==T) RS <- surv.obj$model.dat$RS[length(surv.obj$model.dat$RS)]
	
  # Set up the plot margins
	par(mar=c(5,6,3,10))
	# Set up a vector of all size classes
	vec<-seq(0,195,5)
	# Take 100 samples from that vector
	sh<-sample(vec,100,replace=T)
	# Make a historgram object with appropriate breaks
	tmp<-hist(sh,plot=F,breaks=vec)
	# Get the biomass in kg/tow into the histogram object
	tmp$counts <- bm
	# Set the maximum for the y axis if not specified, this is for the shf data.
	if(is.null(y1max) ==T) y1max<-max(tmp$counts,na.rm=T)*1.1
	# If the xlim is NULL make the xlim 15 mm smaller than the the recruits and out to 200 mm size.
	if(is.null(xlim)==T) xlim <- c(RS-15,160)
	
	# Plot the tmp object (note this is just the plot object, nothing is plotted)
	plot(tmp,xlab="",ylab="",xlim=xlim,col=col1,yaxt='n',xaxt='n',ylim=c(0,y1max),main="")
	# Add the x axis.
	axis(1,at=seq(0,200,by=20),cex.axis=cx.axs,tcl=0.6)
	axis(1,at=seq(10,200,by=20),cex.axis=cx.axs,labels=F,tcl=0.3)
	# add the y-axis
	y.ax <- axTicks(2)
	mn.ax <- (y.ax[2]-y.ax[1])/2
	axis(2,at=y.ax,las=1,cex.axis=cx.axs,tcl=-0.6)
	axis(2,at=seq(mn.ax,(max(y.ax)+mn.ax),2*mn.ax), las=1,cex.axis=cx.axs,tcl=-0.3,labels=F)	
	# Draw a box around image
	box()
	# Now we add the Meat count line
	par(new=T)
	# Get the meat count into meats per 500 grams.
	count=num/bm*0.5
	# Specifiy the y axis for the meat count if not done already.  Using xlim pick a meat count in line with the data.
	if(is.null(y2max)==T) y2max<-max(count[(min(xlim,na.rm=T)/5-1):length(vec)],na.rm=T)*1.1
	# And plot the data
	plot(vec+2.5,count,type='o',pch=16,cex=1,axes=F,xlab='',ylab='',xlim=xlim,col='firebrick',ylim=c(0,y2max),lwd=2,main="")
	# We also should be adding a vertical lines to show our Fully recruited and recruit dvision falls at.
	abline(v=CS,lwd=2,col="blue",lty=1)
	abline(v=RS,lwd=2,col="blue",lty=1)

	# Now we add the lines to connect to our MC regulation
	for(i in 1:length(mc))
	{
	  
	   #Set up a dataframe
	  cntdt <- data.frame(x=c(vec[which(count==min(count[count>mc[i]],na.rm=T))],
		                      vec[which(count==max(count[count<mc[i]],na.rm=T))])+2.5,y=c(min(count[count>mc[i]],na.rm=T),
		                                                                                  max(count[count<mc[i]],na.rm=T)))
	# Get the points for the line
	  sht.cnt<-with(cntdt,(mc[i]-y[1])/((y[2]-y[1])/(x[2]-x[1]))+x[1])
		  
		# Add the point of interestion
		points(sht.cnt,mc[i],col='firebrick',pch=16)	
		# Draw the lines up to and over to the point of interestion
		segments(sht.cnt,0,sht.cnt,mc[i],col='firebrick',lty=2,lwd=2)
		segments(xlim[2]+10,mc[i],sht.cnt,mc[i],col='firebrick',lty=2,lwd=2)
	} # end for(i in 1:length(mc))

	# Add the axis for the meat count and make sub ticks...
	y.ax <- axTicks(4) # y.ax <- c(0, 20, 40, 60, 80, 100, 120, 140)
	mn.ax <- (y.ax[2]-y.ax[1])/2
	axis(4,at=y.ax,las=1,cex.axis=cx.axs,tcl=0.6,col.axis ="firebrick")
	axis(4,at=seq(mn.ax,(max(y.ax)+mn.ax),2*mn.ax), las=1,cex.axis=cx.axs,tcl=0.3,labels=F,col.axis ="firebrick")
	# Add the axis labels...
	mtext("Shell height (mm)", 1, line=3, cex = cx.lab)	
	mtext(expression(frac(kg,tow)), 2, line=3, cex = cx.lab,las=1)	
	mtext(expression(paste("MC:",bgroup("(",frac(N,"500 g"),")"))) , 4, line=3, cex = cx.lab,las=1,col="firebrick")
	# Finally add the title.
	if(add.title == T) title(main=title,cex.main=cx.mn,adj=0.35)
	
	if(value==T) return(list(sht.cnt=sht.cnt))
	
} # end function
	
	

