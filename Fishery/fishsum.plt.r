####  Commented and checked by DK starting on July 28, 2015.
####
#   Revision history
#   Oct 2017:  DK update to get notation up to date + enabled the option of including the FT and WF fishery information in the fishery time series.
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# ok
#  
#      
##
###############################################################################################################

#1.	  dat:         The clapper time series
#2.	  years:       The years to plot.  Default = 1955:2007
#3.	  clr:         colors for ploting.  Default =  c('lightblue3','grey','black')
#4.	  pch:         The plot symbols.  Default = 17:16
#5.	  lwd:         Line thickness.  Default = 16 (which I note seems crazy thick!)
#6.	  wd:          Width of the plot device.  Default = 11
#7.	  ht:          Height of the plot device.  Default = 8
#8.	  d2:          A secondary dataset to add to plot.  Default = NULL
#9.	  se:          Add standard error bars to plot.  (T/F) default = T
#10.	ltm:         Add the median to the plot.  (T/F) default = T
#11.	pdf:         The plot device type.  (T/F) default = F which sends plot to an "R" plot device
#12.	lab:         Plot label.  Default = ""
#13   logged:      Do you want to log transform the data, can help visualizations of low years.  T/F, default = F.
#13.  wf.cpue:     If you want to add the wf fleet specific CPUE trend you need to add the data here.  Data needs to have
#                  a column named "year", and the FT fishery data.
#14   ft.cpue:     If you want to add the ft fleet specific CPUE trend you need to add the data here.  Data needs to have
#                  a column named "year", and the FT fishery data.

# source("Y:/Assessment/2010/r/fn/fishery.plt.r")

fishsum.plt<-function(dat,years=1955:2007,cx.mn =2,add.title=F,bnk = NULL,cx.lab = 1.25,cx.axis=1.5,
                      pch=16,lwd=16,wd=8.5,ht=11,d2=NULL,se=T,ltm=T,lab='',logged=F, wf.cpue = NULL,ft.cpue = NULL, ...)
{
	
	if(!is.null(wf.cpue)) wf.cpue <- wf.cpue[wf.cpue$year %in% years,]
	if(!is.null(ft.cpue)) ft.cpue <- ft.cpue[ft.cpue$year %in% years,]
	
	# force catch to be NA if effort is also NA
	dat$catch[is.na(dat$effort)] <- NA
	
  # Set up the plot window 
	par(mfrow=c(3,1), mar = c(1, 14, 3, 1), omi = c(1, 0.25, 0.1, 0.25))
  # Subset the data into the years of interest
  d<-subset(dat,year%in%years)
  year<-d$year
  
  # Plot the catch data either original scale, or log scale
  if(logged ==T) 
    {
    plot(catch~year,data=d, ylab="",xlab='', type='n',las=1, xaxt = "n",cex.lab=cx.lab,cex.axis=cx.axis,
       ylim=c(0.5*min(catch, na.rm = T),max(catch, na.rm = T)*2),log="y")
    } # end if(logged ==T) 
  if(logged == F) 
  {
    plot(catch~year,data=d, ylab="",xlab='', type='n',las=1, xaxt = "n",cex.lab=cx.lab, cex.axis=cx.axis,
         ylim=c(0,max(catch, na.rm = T)*1.2))
  } # end if(logged ==F) 
  # Make axis ticks and labels
  axis(1, tcl = -0.6, mgp = c(0.5, 0.7, 0), cex.axis=cx.axis,lab=F)
  axis(1, at = year, lab = F, tcl = -0.3, cex.axis=cx.axis)
  axis(4, lab = F, tcl = -0.6, cex.axis=cx.axis)
  # Now add the "points", in this case they are added as histogram bars.
  points(catch~ year,data=d,  type='h',pch=15,lwd=lwd,lend=3,col="grey50")
  # Why do we use bars? I dunno. Just wanted to look at points instead... points(catch~ year,data=d,col="grey50")
  # Add the axis label
  mtext("Catch (t)", 2, 5, outer = F, cex = cx.lab,las=1)	
  # Add the median if requested, this is the average catch in years the fleet actually fishes out there!
  if(ltm==T) lines(year,rep(median(d$catch,na.rm=T),length(year)),lty=2)
  
  
  # Plot the Effort	data either the oringal scale or the log scale
  if(logged ==T) 
    {
    plot(d$year,d$effort,ylab='',xlab='', type='h', lwd=lwd,pch=15, xaxt="n",yaxt="n",cex.lab=cx.lab, lend=3,
       ylim=c(0.5*min(d$effort,na.rm=T),max(d$effort,na.rm=T)),log="y", cex.axis=cx.axis,col="grey50")
    }#end if(logged ==T) 
  if(logged ==F) 
    {
        plot(d$year,d$effort,ylab='',xlab='', type='h', lwd=lwd,pch=15, xaxt="n",yaxt="n",cex.lab=cx.lab, lend=3, 
         ylim=c(0,max(d$effort,na.rm=T)), cex.axis=cx.axis,col="grey50")
    }#end if(logged ==F) 
  
  # Set up the pretty axis locations
  loc <- axTicks(side=2)
  # This forces the large numbers to include a comma, and ensures they don't get exponentiated for the label
  labs <- formatC(loc,big.mark=",",format="d")
  axis(2, at=loc, labels=labs,tcl = -0.6,las=1, mgp = c(0.6, 1, 0), cex.axis=cx.axis)
  axis(4, lab = F, tcl = -0.6, cex.axis=cx.axis)
  axis(1, tcl = -0.6, mgp = c(0.5, 0.7, 0), lab=F, cex.axis=cx.axis)
  axis(1, at = year, lab = F, tcl = -0.3, cex.axis=cx.axis)
  
  # Add the long term median if requested
  if(ltm==T) lines(year,rep(median(d$effort,na.rm=T),length(year)),lty=2)
  # add the axis label
  mtext(expression(paste("Effort (hm)")),2,5.5 , outer = F, cex = cx.lab,las=1)	
  
  # Plot the CPUE	data either the oringal scale or the log scale.  It is added to the effort plot.
  #par(new=T)
  ymax<-ifelse(se,max(d$cpue+sqrt(d$cpue.var), na.rm = T),max(d$cpue, na.rm = T))
  if(!is.null(ft.cpue)) ymax <- max(ymax,max(ft.cpue$cpue,na.rm=T)) # In case ymax for FT is higher...
  if(logged ==T) 
    {
      ymin<-0.5*(ifelse(se,min(d$cpue-sqrt(d$cpue.var), na.rm = T),min(d$cpue, na.rm = T)))
      plot(cpue~year,data=d,ylab='',xlab='', type='n',pch=pch,las=1, xaxt = "n", cex.axis=cx.axis,
           cex.lab=cx.lab,ylim=c(ymin,ymax),log="y")
    } #end if(logged ==T) 
  if(logged ==F) 
  {
    ymin<-0
    plot(cpue~year,data=d,ylab='',xlab='', type='n',pch=pch,las=1, xaxt = "n", cex.axis=cx.axis,
         cex.lab=cx.lab,ylim=c(ymin,ymax),lwd=2)
  } #end if(logged == F)
  # Add the median if requested
  if(ltm==T) lines(year,rep(median(d$cpue,na.rm=T),length(year)),lty=2)
  
  # If you have the WF and FT data it goes in here...
  if(!is.null(wf.cpue)) lines(cpue~year,wf.cpue,col="orange")
  if(!is.null(ft.cpue)) lines(cpue~year,ft.cpue,col = "blue")
  
  # Now put the acutal plot line in
  lines(cpue~year,data=d,pch=pch,type="o",lwd=2)
  
  # Add the se bars if requested
  if(se==T) segments(d$year,d$cpue+sqrt(d$cpue.var),d$year,d$cpue-sqrt(d$cpue.var),lwd=2)	
  
  axis(1, tcl = -0.6,las=1,  cex.axis=cx.axis)
  axis(1, at = year, lab = F, tcl = -0.3, cex.axis=cx.axis)
  axis(4, lab = F, tcl = -0.6, cex.axis=cx.axis)
  
  # Axis labes	
  mtext(expression(paste("CPUE ", bgroup("(", frac(kg, hm), ")"))), 2, line=4.5, outer = F, cex = cx.lab,las=1)	
  mtext("Year", side=1, line = 2, outer = T, cex = cx.lab,adj=0.6)
  if(add.title == T & is.null(bnk) ==F)title(paste("Fishery Time Series (",bnk,")",sep=""),outer=T, cex.main=cx.mn,line=-2,adj=0.7)
  if(add.title == T & is.null(bnk) ==T) title("Fishery Time Series",outer=T,cex.main=cx.mn,line=-2,adj=0.7)

  if(!is.null(wf.cpue) && !is.null(ft.cpue)) legend("topleft",c("Overall","FT","WF"),lwd=c(2,1,1),pch=c(pch,NA,NA),col=c("black","blue","orange"),bty="n",
                                                    y.intersp = 0.8,cex=1.5)
	
}