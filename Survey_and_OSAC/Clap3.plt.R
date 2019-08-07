####  Commented and checked by DK September 2015. This plots the clapper abundance time series for the recruits 
####  pre-recruits, and commercial sized clappers (thus clap3 name), 3 panel plot.
# Update history
#Commented, checked  and revised by DK March 31, 2016
####
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

#Arguments
#data:        The clapper time series, should include proportion of the three categories.
#years:       The years to plot.  Default = 1981:2009
#mean.line:   Add the average over the time series.  (T/F) Default = T
#graphic:     The plot device type.  Options are 'pdf' or "R", Default = "R"
#width:       Width of the plot device.  Default = 11
#height:      Height of the plot device.  Default = 8.5
#yl:          The y axis min/max.  Default is missing and is based on the range of the data (maximum of y-axis is set to be at least 20%)
#data2:       An optional secondary clapper time series to add to the plot.
#ypos:        Location to plot the text of the years
#add.title:   Add a title to the plot?  (T/F) default = F.
#cex.mn:      Magnification of title plot.  Default = 1.
#titl:        The title name.  Default is blank.
#CS:          Commercial Size of Scallops.  Default = NULL
#RS:          Recruit size of Scallops.  Deafult = NULL
#axis.cx:     Magnification of the axes.  Default = 1
#clr:         Color of the line/points and color of the median line.  Default = c("blue","darkgrey")

Clap3.plt <- function(data, years=1981:2009,mean.line=T,graphic="R",width = 11, height = 8.5,yl, 
                      data2,ypos=1,add.title = F, cex.mn = 1, titl = "", CS = NULL, RS=NULL,axis.cx=1,clr=c("blue","darkgrey"),...)
{

  # Plot device and figure margin options.
	if(graphic=='pdf')pdf("plots/Clappers.pdf", width = width, height = height, pointsize = 14)
#	if(graphic=="R")windows( width = width, height = height)
	par(mfrow = c(3, 1), mar = c(0, 2, 0, 1), omi = c(1, 1, 0.5, 0.5))

	# If Proportion is > 100 than make it an NA.
	data$clap.propPre[data$clap.propPre>100]<-NA
	data$clap.propRec[data$clap.propRec>100]<-NA
	data$clap.propCom[data$clap.propCom>100]<-NA

	# Subset the clapper time series and calculate the annual average
	dat <- as.data.frame(years)
	ts <- aggregate(clap.propPre~year,subset(data,year %in% years),mean)
	ts$clap.propRec <- aggregate(clap.propRec~year,subset(data,year %in% years),mean)$clap.propRec
	ts$clap.propCom <- aggregate(clap.propCom~year,subset(data,year %in% years),mean)$clap.propCom
	dat$clapsPre[dat$year %in% ts$year]  <- ts$clap.propPre
	dat$clapsRec[dat$year %in% ts$year]  <- ts$clap.propRec
	dat$clapsCom[dat$year %in% ts$year]  <- ts$clap.propCom
	
  # if y limits not specified go from 0 to max found in data or at least 20%
	if(missing(yl)==T) 
	{
	  yl<-c(0,max(c(dat$clapsPre,dat$clapsRec,dat$clapsCom),na.rm=T))
	  if(max(yl) < 20) yl[2] <- 20
	} # if(missing(yl)==T) 
	#The pre-recruit time series
	plot(years,dat$clapsPre, type='o', ylim=yl, ylab="", xlab="", tcl=-0.3,  pch=16,xaxt="n",yaxt="n",col=clr[1])
	#Add optional mean line
	if(mean.line==T)  {abline(h=median(dat$clapsPre[-length(years)],na.rm = T),lty=2,col=clr[2],lwd=2)
	  LTMpre <- median(dat$clapsPre[-length(years)],na.rm = T)
	  LTMrec <- median(dat$clapsRec[-length(years)],na.rm = T)
	  LTMcom <- median(dat$clapsCom[-length(years)],na.rm = T)
	}
	
	# Add axis tick marks and label this "pre-recruits".
	axis(4, lab=F, tcl=-0.3,cex.axis=axis.cx)
	axis(1,lab=F,cex.axis=axis.cx)
	axis(1,at=years,lab=F,tcl=-0.3,cex.axis=axis.cx)
	axis(2,cex.axis=axis.cx,las=1)
	#	If not given the min size of the recruits just add this text.
	if(is.null(RS) ==T) text(years[ypos], max(yl)*0.9, "Pre-recruits", cex=1.5, adj = 0)
	#	If given the min size of the recruits add this text. 
	if(is.null(RS) == F) text(years[ypos], max(yl)*0.9, 
	                               substitute(paste("Pre-", recruits < a, " mm",sep=""),list(a=RS)), cex=1.5, adj = 0)
	
	if(add.title==T) title(titl,cex.main = cex.mn,outer=T)
	
	# The recruit time series
	plot(years,dat$clapsRec, type='o', las=1, ylim=yl, ylab="", xlab="", tcl=-0.3,  pch=16,xaxt="n",yaxt="n",col=clr[1])
	#Add optional mean line
	if(mean.line==T) abline(h=median(dat$clapsRec[-length(years)],na.rm=T),lty=2,col=clr[2],lwd=2)
	# Add axis tick marks and label this "recruits".
	axis(4, lab=F, tcl=-0.3,cex.axis=axis.cx)
	axis(1,lab=F,cex.axis=axis.cx)
	axis(1,at=years,lab=F,tcl=-0.3,cex.axis=axis.cx)
	axis(2,cex.axis=axis.cx,las=1)
	# If not given the min size of the recruits just add this text.  if given it do the else if statement
	if(is.null(RS) == T) text(years[ypos], max(yl)*0.9, "Recruits", cex=1.5, adj = 0)
	#	If given the min size of the recruits add this text. 
	if(is.null(RS)==F) text(years[ypos], max(yl)*0.9, paste("Recruits",RS,"-",CS-1,"mm"), cex=1.5, adj = 0)
	mtext(expression(frac("% dead",tow)), 2, 2.5, cex=1.25,las=1,padj=0.5)
	
	plot(years,dat$clapsCom, type='o', las=1, ylim=yl, ylab="", xlab="", tcl=-0.3, pch=16,xaxt="n",yaxt="n",col=clr[1])
	#Add optional mean line
	if(mean.line)abline(h=median(dat$clapsCom[-length(years)],na.rm=T),lty=2,col=clr[2],lwd=2)
	# Add axis tick marks and label this "fully recruited". Also add y axis lable.
	axis(4, lab=F, tcl=-0.3,cex.axis=axis.cx)
	axis(1,cex.axis=axis.cx)
	axis(1,at=years,lab=F,tcl=-0.3,cex.axis=axis.cx)
	axis(2,cex.axis=axis.cx,las=1)
	if(is.null(CS)== T) text(years[ypos], max(yl)*0.9, "Fully recruited", cex = 1.5, adj = 0)
	if(is.null(CS)==F) text(years[ypos], max(yl)*0.9, 
	                        substitute(paste("Fully ", recruited>=a, "mm"),list(a=CS)), cex = 1.5, adj = 0)

	
	# shut off graphic device and that's all she wrote.
	if(graphic != "R")dev.off()
	
	# Save the LTMs
	assign("clap.propLTMs", c(LTMpre, LTMrec, LTMcom), pos = 1)
}	# end function
