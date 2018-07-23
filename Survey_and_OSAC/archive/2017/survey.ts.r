################################################################################################################
####  Commented and checked by DK September 2015.  This function is used to create a time series of the
####  abundnace or biomass of the each of the scallop classes (Commerical, Recruits, Pre-recruits)
####
################################################################################################################
#  Change log
#  July 2016:  Added option to change color of the median line for the primary data also forced the Npt =T minimums for biomass
#  and abundance to be 1.2 and 120 respectively if you didn't supply yl2.
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################

# shf:   The shell height frequency data
#years:  The years to run across. default = 1981:2008
#Bank:   The bank to choose. Used for path for output file only so doesn't matter if pdf = F Default = 'GBa'
#type:   Do we want to plot the Abundance (N) or biomass (B).  Default = "N"
#pdf:    Save plot as a pdf.  (T/F) default=F
#plots:  Which size classes should be printed.  Default = c('pre','rec','com'), any combination of these is acceptable.
#        "pre" = pre-recruits, "rec" = recruits, "com" = commercial
#CS:     Commercial size.  Default = NULL
#RS      Recruit size.  Default = NULL
#clr:    Color for the data.  Main data is slot 1, if a secondary line added, it is slot 2, the 3rd slot is for the color of the median line
#        for the first data (if a second set of data are provided the median line for that data will be same color as the secondard data line)
#cx:     Plot magnification. Default =1
#pch:    Symbol used for points.  Default = 1:2
#lty:    The line type.  Default = 1:2
#wd:     Width of plot window.  Default = 10
#ht:     Height of plot window. Default = 8
#Npt:    Are the data Numbers (Biomass) per tow or total numbers (Biomass).  Default is Npt = T so numbers per tow.
#se:     Plot the standard errors, this is only correct if Npt = T. (T/F)  Default = F
#ys:     Scaling for maximum of y axis.  Default = 1
#yl2:    y axis upper limit. when Npt = T specify as Number per tow, or Biomass(kg) per tow  
#        when Npt = F specify as tonnes or total numbers (millions). Default is missing and calculated from data.
#ymin:   minimum for y axis.  Default = 0
#dat2    A second dataset used in SE calculations.  Default = NULL
#areas   A numeric vector of area sizes.  Default = NULL
#ypos    Picks the year above which we want to add text to the plots.  Default = 1 which gives nice results generally.
#add.title:  Add a title to the plot?  (T/F) default = F.
#cx.mn:      Magnification of title plot.  Default = 1.
#titl:       The title name.  Default is blank.
#axis.cx:    The axis magnification. Default = 1


survey.ts <- function(shf, years=1981:2008, Bank='GBa', type = "N",pdf=F, plots=c('pre','rec','com'),CS=NULL,RS=NULL,
                      clr=c(1,1,1),cx=1.2,pch=1:2,lty=1:2,wd=10,ht=8,Npt=T,se=F,ys=1,yl2,ymin=0,dat2=NULL,areas=NULL,
                      ypos=1, add.title = F, cx.mn = 1, titl = "", axis.cx=1, ...)
{

	# Subset the data into the years of interest
	shf<-subset(shf,year %in% years)
	
	# Check if any years are missing, if so we'll need to fill them in with NA's so we aren't drawing lines
	# between years with no data... DK added on Nov 23, 2015, if getting a weird error it may be
	# due to this!!
	missing.years <-  years[!is.element(years,shf$year)]
	if(length(missing.years > 0))
	  {
    	fill <- data.frame(matrix(NA,nrow=length(missing.years),ncol=ncol(shf)))
    	fill[,1] <-missing.years
    	names(fill) <- names(shf)
    	# I will give this a new name as this messing up the survey object for the SHF plot...
    	shf<- rbind(shf,fill)
    	# And now re-order the data and everything will be wonderful!
    	shf <- shf[order(shf$year),]
	  } # end if(length(missing.years > 0))
	
	
	# Set up the pdf plot device + filename
	if(pdf == T) pdf(paste("figures/",Bank,type,min(years),"-",max(years),".pdf",sep=""), width = wd, height = ht)
	
	# If looking for abundance do this
	if(type == "N")
	  {
	  
	  # If areas has been specified and Number per tow is True (Npt=T) then we want to scale the data from bank to tow
		if(is.null(areas) == F && Npt == T)
		  {
		    shf[c("I","IR","IPR","N","NR","NPR")] <- shf[c("I","IR","IPR","N","NR","NPR")] / sum(areas) * 10^6 
		  } # end if(!is.null(areas) == T && Npt == T)
		
	  # Ithere is a dat2 two object and areas is specified and Npt is true this is conversion from bank to tow numbers.
	  if(is.null(dat2) == F && is.null(areas)==F && Npt == T) 
		  {
	      # scale down from bank to tow
		    dat2[c("I","IR","IPR","N","NR","NPR")]<-dat2[c("I","IR","IPR","N","NR","NPR")] / sum(areas) * 10^6 
		  } # if(!is.null(dat2)==T && !is.null(areas)==T && Npt == T) 
		  
		  # If not making a pdf plot open a plot window with these dimensions
#		  if(pdf==F) windows(wd,ht)	
		  # Set plot device options.
		  par(mfrow = c(length(plots), 1), mar = c(0, 2, 0, 1), omi = c(1, 1, 0.5, 0.5))
		
		  
		  #Calculate standard errors based on mean and CV.
		  if(se==T)
  		  {
  		    shf$NPR.se <- shf$NPR*shf$NPR.cv
  		    shf$N.se <- shf$N*shf$N.cv
  		    shf$NR.se <- shf$NR*shf$NR.cv
  		    # Do the same if there is dat2 around.
  		    if(missing(dat2) ==F)
  		    {
  		      dat2$NPR.se <- dat2$NPR*dat2$NPR.cv
  		      dat2$N.se <- dat2$N*dat2$N.cv
  		      dat2$NR.se <- dat2$NR*dat2$NR.cv
  		      
  		    } # end if(missing(dat2) ==F)
  		    
  		    
  		  } #end if(se==T)  
  		  
  		# Set plot y axis maximum if not provided 
  		if(missing(yl2) ==T)
  		  {
  		  # if ploting the standard error set the max y as this
  		  # Add the standard error bars 
  		  if(se==T)
  			  {
  		  	  ymax<-c(max(shf$NPR,shf$NPR+shf$NPR.se, na.rm = T),max(shf$NR,shf$NR+shf$NR.se, na.rm = T),
  				          max(shf$N,shf$N+shf$N.se, na.rm = T))*1.2
  				  if(is.null(dat2)==F) ymax<-c(max(c(shf$NPR,shf$NPR+shf$NPR.se,dat2$NPR,dat2$NPR+dat2$NPR.se), na.rm = T),
  				                               max(c(shf$NR,shf$NR+shf$NR.se,dat2$NR,dat2$NR+dat2$NR.se), na.rm = T),
  				                               max(c(shf$N,shf$N+shf$N.se,dat2$N,dat2$N+dat2$N.se), na.rm = T))*1.2
  			  } # end if(se==T)
  		 
  			# If not ploting the standard error the max y should be this.
  		  if(se==F)
  			  {
  				  ymax<-c(max(shf$NPR, na.rm = T),max(shf$NR, na.rm = T),max(shf$N, na.rm = T))*1.2
  				  if(is.null(dat2)==F) ymax<-c(max(c(shf$NPR,dat2$NPR), na.rm = T),max(c(shf$NR,dat2$NR), na.rm = T),
  				                               max(c(shf$N,dat2$N), na.rm = T))*1.2
  			  } # end if(se==F)
  		  # Here I'm forcing the y axis to have a minimum of 1kg/tow...
  		  # Tidying up the y-max for the plots so they look nicer...
  		  if(Npt == T)
  		  {
  		    # First rule accounts for cases when we have very high numbers in one category compared to the minimum if so
  		    # we keep that maximum as is but make the other two equivalent, 0.33 is arbitray but seems to look nice to me...
  		    if((min(ymax)/max(ymax)) < 0.33) ymax[which(ymax != max(ymax))] <- rep(max(ymax[which(ymax != max(ymax))]),length(which(ymax != max(ymax))))
  		    # If this ratio is > 25% then let's make all the axis the same...
  		    if((min(ymax)/max(ymax)) >= 0.33) ymax <- rep(max(ymax),length(plots))
  		    # And forcing the minimum y axis for biomass to be a minimum of 50/tow 
  		    if(max(ymax) < 50) ymax <- rep(50,length(plots))
  		  } # end if(Npt == T )
  		  
  		  } # end if(missing(yl2) ==T)
  		
		# If y limit is provided make a copy of it for each plot.
		if(missing(yl2)==F && length(yl2) == 1 ) ymax<-rep(yl2,length(plots))
		if(missing(yl2)==F && length(yl2) > 1 ) ymax<-yl2
		  
		# If we want the Pre-recruit plot this is how it's done.  
		if('pre'%in%plots == T)
		  {
		    # Open the plot window.
		    par(...)
		    # Set up the plot, no points are added yet
  			plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(ymin, ymax[1]), mgp = c(0.5, 0.5, 0), 
  			     tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=axis.cx)
  		  # Axis, revised to ensure min year is as low as it possibly could be (but will never be plotted) and the max is 
		    # the latest year, every 5th year plotted
		    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5),lab = F, tcl = -0.6,cex.axis=axis.cx)
		    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),1), lab = F, tcl = -0.3,cex.axis=axis.cx)
		  	# Left and right y-axes, note that we scale the maximum by ys for some reason.
  		  axis(2, pretty(c(0,ymax[1]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
  			axis(4, pretty(c(0,ymax[1]*ys)),lab = F, tcl = -0.3,cex.axis=axis.cx)

  			# Now add the median line, but only for the years we have data, allow the lines to 
  			# have their own unique color.
  			if(any(is.na(shf$NPR))==T) 
  			{
  			  lines(shf$year[-which(is.na(shf$NPR))],rep(median(shf$NPR,na.rm=T),length(shf$year[-which(is.na(shf$NPR))])),
  			        col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$NPR))==T) 
  			if(any(is.na(shf$NPR))==F) 
  			{
  			  lines(shf$year,rep(median(shf$NPR,na.rm=T),length(shf$year)),col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$NPR))==F) 
  			# now add the points
  			points(shf$year, shf$NPR, type = "o", pch = pch[1],cex=cx,col=clr[1])
  			# If dat2 is provided then we add these points and possibly se (well the se/mean) as well. 
  			if(is.null(dat2) == F)
  			  {
  				  points(dat2$year, dat2$NPR, type = "o", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
  			    # add the median line, if statement handling cases with/without NA's, if we have a second line we keep the median line the same clr.
  			    if(any(is.na(dat2$NPR))==T) 
  			      {
  			        lines(dat2$year[-which(is.na(dat2$NPR))],rep(median(dat2$NPR,na.rm=T),length(dat2$year[-which(is.na(dat2$NPR))])),
  			              col=clr[2],lty=2,lwd=2)
  			      } # end if(any(is.na(dat2$NPR))==T) 
  			    if(any(is.na(dat2$NPR))==F) 
  			      {
  			          lines(dat2$year,rep(median(dat2$NPR,na.rm=T),length(dat2$year)),col=clr[2],lty=2,lwd=2)
  			      } # end if(any(is.na(dat2$NPR))==F) 
  			  
  			    # Add the se
  			    if(se == T && "NPR.se" %in% names(dat2)==T) segments(dat2$year,dat2$NPR+dat2$NPR.se,
  			                                                       dat2$year,dat2$NPR-dat2$NPR.se,col=clr[2])
  			  } # end if(!is.null(dat2))
  			
        #Add the standard error bars 
  			if(se==T && "NPR.se" %in% names(shf)==T) segments(shf$year,shf$NPR+shf$NPR.se,shf$year,
  			                                                shf$NPR-shf$NPR.se,col=clr[1])
  		  #	If not given the min size of the recruits just add this text.
  			if(is.null(RS) ==T) text(years[ypos], ymax[1]*1, "Pre-recruits", cex=1.5, adj = 0)
  			#	If given the min size of the recruits add this text. 
  			else if(is.null(RS) == F) text(years[ypos], ymax[1]*1, 
  			                               substitute(paste("Pre-", recruits < a, " mm",sep=""),list(a=RS)), cex=1.5, adj = 0)
  			# Add a title?
  			if(add.title == T) title(titl,cex.main = cx.mn,outer=T)
		  } # END if('pre'%in%plots)
		
		# If we want the Pre-recruit plot this is how it's done.    
		if('rec'%in%plots == T)
		  {
  		  # Set up the plot, no points are added yet
  		  plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(ymin, ymax[2]), mgp = c(0.5, 0.5, 0), 
  		       tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=axis.cx)
  		  # Axis, revised to ensure min year is as low as it possibly could be (but will never be plotted) and the max 
  		  # is the latest year, every 5th year plotted
		    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5),lab = F, tcl = -0.6,cex.axis=axis.cx)
		    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),1), lab = F, tcl = -0.3,cex.axis=axis.cx)
		    # Left and right y-axes, note that we scale the maximum by ys for some reason.
  		  axis(2, pretty(c(0,ymax[2]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
  			axis(4, pretty(c(0,ymax[2]*ys)),lab = F, tcl = -0.3,cex.axis=axis.cx)
  			# add the median line, if statement handling cases with/without NA's
  			if(any(is.na(shf$NR))==T) 
  			{
  			  lines(shf$year[-which(is.na(shf$NR))],rep(median(shf$NR,na.rm=T),length(shf$year[-which(is.na(shf$NR))])),
  			        col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$NR))==T) 
  			if(any(is.na(shf$NR))==F) 
  			{
  			  lines(shf$year,rep(median(shf$NR,na.rm=T),length(shf$year)),col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$NR))==F) 
  			# now add the points
  			points(shf$year, shf$NR, type = "o", pch = pch[1],cex=cx,col=clr[1])
  			# If dat2 is provided then we add these points and possibly se (well the se/mean) as well. 
  			if(is.null(dat2) == F)
  			{
  			  points(dat2$year, dat2$NR, type = "o", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
  			  # add the mean line, if statement handling cases with/without NA's
  			  if(any(is.na(dat2$NR))==T) 
  			  {
  			    lines(dat2$year[-which(is.na(dat2$NR))],rep(median(dat2$NR,na.rm=T),length(dat2$year[-which(is.na(dat2$NR))])),
  			          col=clr[2],lty=2,lwd=2)
  			  } # end if(any(is.na(dat2$NR))==T) 
  			  if(any(is.na(dat2$NR))==F) 
  			  {
  			    lines(dat2$year,rep(median(dat2$NR,na.rm=T),length(dat2$year)),col=clr[2],lty=2,lwd=2)
  			  } # end if(any(is.na(dat2$NR))==F) 
  			  	if(se==T && "NR.se"%in%names(dat2)==T) segments(dat2$year,dat2$NR+dat2$NR.se,dat2$year,
    				                                                dat2$NR-dat2$NR.se,col=clr[2])
  			  }
  			#Add the standard error bars 
  			if(se==T && "NR.se" %in% names(shf)==T) segments(shf$year,shf$NR+shf$NR.se,shf$year,
  			                                               shf$NR-shf$NR.se,col=clr[1])
  			# If not given the min size of the recruits just add this text.  if given it do the else if statement
  			if(is.null(RS) == T) text(years[ypos], ymax[2]*0.9, "Recruits", cex=1.5, adj = 0)
  			#	If given the min size of the recruits add this text. 
  			else if(is.null(RS)==F) text(years[ypos], ymax[2]*0.9, paste("Recruits",RS,"-",CS-1,"mm"), cex=1.5, adj = 0)
  		} # if('rec'%in%plots == T)

		# Finally add in the commercial scallops if we want them.  
		if('com'%in%plots==T)
		  {
		    # Set up the plot, no points are added yet
		  	plot(years, years, type = "n", ylab = "", xlab = "", las = 1, ylim = c(ymin, ymax[3]), mgp = c(0.5, 0.5, 0), 
		  	     tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=axis.cx)
  		  # Axis, revised to ensure min year is as low as it possibly could be (but will never be plotted) and the max 
  		  # is the latest year, every 5th year plotted
		    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5),lab = F, tcl = -0.6,cex.axis=axis.cx)
		    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),1), lab = F, tcl = -0.3,cex.axis=axis.cx)
  		  # Left and right y-axes, note that we scale the maximum by ys for some reason.
  			axis(2, pretty(c(0,ymax[3]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
  			axis(4, pretty(c(0,ymax[3]*ys)),lab = F, tcl = -0.3,cex.axis=axis.cx)
  			# add the mean line, if statement handling cases with/without NA's
  			if(any(is.na(shf$N))==T) 
  			{
  			  lines(shf$year[-which(is.na(shf$N))],rep(median(shf$N,na.rm=T),length(shf$year[-which(is.na(shf$N))])),col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$N))==T) 
  			if(any(is.na(shf$N))==F) 
  			{
  			  lines(shf$year,rep(median(shf$N,na.rm=T),length(shf$year)),col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$N))==F) 
  			# now add the points
  			points(shf$year, shf$N, type = "o", pch = pch[1],cex=cx,col=clr[1])
  			# If dat2 is provided then we add these points and possibly se (well the se/mean) as well. 
  			if(is.null(dat2) == F)
  			{
  			  points(dat2$year, dat2$N, type = "o", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
  			  # add the mean line, if statement handling cases with/without NA's
  			  if(any(is.na(dat2$N))==T) 
  			  {
  			    lines(dat2$year[-which(is.na(dat2$N))],rep(median(dat2$N,na.rm=T),length(dat2$year[-which(is.na(dat2$N))])),
  			          col=clr[2],lty=2,lwd=2)
  			  } # end if(any(is.na(dat2$N))==T) 
  			  if(any(is.na(dat2$N))==F) 
  			  {
  			    lines(dat2$year,rep(median(dat2$N,na.rm=T),length(dat2$year)),col=clr[2],lty=2,lwd=2)
  			  } # end if(any(is.na(dat2$N))==F) 
  			    #Add the standard error bars 
  			    if(se==T && "N.se" %in% names(dat2)==T) segments(dat2$year,dat2$N+dat2$N.se,dat2$year,
    				                                               dat2$N-dat2$N.se,col=clr[2])
  			  } # end if(!is.null(dat2))
  			#Add the standard error bars 
  			if(se==T && "N.se"%in%names(shf)==T) segments(shf$year,shf$N+shf$N.se,shf$year,shf$N-shf$N.se,col=clr[1])
  			# If not given the min size of the recruits just add this text.  if given it do the else if statement
  			if(is.null(CS)== T) text(years[ypos], ymax[3]*0.9, "Fully recruited", cex = 1.5, adj = 0)
  			if(is.null(CS)==F) text(years[ypos], ymax[3]*0.9, 
  			                             substitute(paste("Fully ", recruited>=a, "mm"),list(a=CS)), cex = 1.5, adj = 0)
  		} # end if('com'%in%plots)
	  
		# Add the axis label to whichever plot comes last and include the labels (years)
		axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5), tcl = -0.6,cex.axis=axis.cx)
		# Is data in Numbers per tow or total numbers.bgroup("(",frac(N,tow),")")
		if(Npt==T) mtext(side=2,substitute(paste("",frac(N,tow),),list(N="N",tow="tow")), line=2,
		                 outer = T, cex = 1.2,las=1)
		if(Npt==F) mtext("Number of scallops (millions)", 2, 3, outer = T, cex = 1.2)	
		
		# Add the year to the bottom of the final plot
		mtext("Year", 1, 4, outer = T, cex = 1.2)	
			
		}# end if(type == "N")
	
	
	####################################################################################################################
	# If looking for Biomass do this, this is largely identical to what we did for N so see above for detailed comments.
	####################################################################################################################
	if(type == "B")
	  {
		if(is.null(areas) == F && Npt == T) 
		  {
		    # scale down from bank to tow
		    shf[c("I","IR","IPR","N","NR","NPR")]<- shf[c("I","IR","IPR","N","NR","NPR")] / sum(areas,na.rm=T) * 10^6
		  } # end if(is.null(areas)==F && Npt == T) 
		
	  if(is.null(dat2)==F && is.null(areas)==F && Npt==T)
		  {
	      # scale down from bank to tow
	      dat2[c("I","IR","IPR","N","NR","NPR")]<-dat2[c("I","IR","IPR","N","NR","NPR")] / sum(areas,na.rm=T) * 10^6
		  } # end if(is.null(dat2)==F && is.null(areas)==F && Npt==T)
		
	  #if(pdf==F) windows(wd,ht)	
		
	  par(mfrow = c(length(plots), 1), mar = c(0, 2, 0, 1), omi = c(1, 1, 0.5, 0.5))
	
	  
	  #Calculate standard errors based on mean and CV.
	  if(se==T)
	  {
	    shf$IPR.se <- shf$IPR*shf$IPR.cv
	    shf$I.se <- shf$I*shf$I.cv
	    shf$IR.se <- shf$IR*shf$IR.cv
	    if(missing(dat2) ==F)
	      {
	        dat2$IPR.se <- dat2$IPR*dat2$IPR.cv
	        dat2$I.se <- dat2$I*dat2$I.cv
	        dat2$IR.se <- dat2$IR*dat2$IR.cv
	      
	      } # end if(missing(dat2) ==F)
	    
	  } #end if(se==T)
	  
	  # if we have yl2 specified make sure it's the right lenght and turn it into the ymax...
	  if(missing(yl2) == F && length(yl2) == 1)  ymax<-rep(yl2,length(plots))
	  if(missing(yl2) == F && length(yl2) > 1)  ymax<-yl2

	  
		if(missing(yl2)==T)
		  {
			if(se==T)
			  {

				  ymax<-c(max(shf$IPR,shf$IPR+shf$IPR.se, na.rm = T),max(shf$IR,shf$IR+shf$IR.se, na.rm = T),
				          max(shf$I,shf$I+shf$I.se, na.rm = T))*1.2
				  if(is.null(dat2) == F ) ymax<-c(max(c(shf$IPR,shf$IPR+shf$IPR.se,dat2$IPR,dat2$IPR+dat2$IPR.se), na.rm = T),
				                            max(c(shf$IR,shf$IR+shf$IR.se,dat2$IR,dat2$IR+dat2$IR.se), na.rm = T),
				                            max(c(shf$I,shf$I+shf$I.se,dat2$I,dat2$I+dat2$I.se), na.rm = T))*1.2
			  } # end if(se==T)
		  if(se==F)
			  {
			  	ymax<-c(max(shf$IPR, na.rm = T),max(shf$IR, na.rm = T),max(shf$I, na.rm = T))*1.2
			  	if(!is.null(dat2))ymax<-c(max(c(shf$IPR,dat2$IPR), na.rm = T),max(c(shf$IR,dat2$IR), na.rm = T),
		  		                          max(c(shf$I,dat2$I), na.rm = T))*1.2
			  } # end if(se==F)
		  # Tidying up the y-max for the plots so they look nicer...
		  if(Npt == T)
		  {
		    # If we are over 500 set the maximum biomass to the same for all plots
		    if(max(ymax) >= 500) ymax <- rep(max(ymax),length(plots))
		    # And forcing the minimum y axis for biomass to be 500 g/tow (500 g/tow)
		    if(max(ymax) < 500) ymax <- rep(500,length(plots))
		  } # end if(Npt == T )
		  } # end if(missing(yl2)==T)
	  
		# Prerecruit Biomass plots.
		if('pre'%in%plots==T)
		  {
		       # Open the plot window.
		      par(...)
    			plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(0, ymax[1]), mgp = c(0.5, 0.5, 0),
    			     tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=1.3)
		      axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5),lab = F, tcl = -0.6,cex.axis=axis.cx)
		      axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),1), lab = F, tcl = -0.3,cex.axis=axis.cx)
		  
    			# Note the labels command gets our y axis labels to scale to kg rather than grams, only necessary for Num per tow.
    			if(Npt == T) axis(2, at = pretty(c(0,ymax[1]*ys)), labels = pretty(c(0,ymax[1]*ys)/1e3), 
    			                  mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
    			if(Npt == F) axis(2, at = pretty(c(0,ymax[1]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
    			axis(4, pretty(c(0,ymax[1]*ys)),lab = F, tcl = -0.3,cex.axis=axis.cx)
    			# add the mean line, if statement handling cases with/without NA's
    			if(any(is.na(shf$IPR))==T) 
    			{
    			  lines(shf$year[-which(is.na(shf$IPR))],rep(median(shf$IPR,na.rm=T),length(shf$year[-which(is.na(shf$IPR))])),col=clr[3],
    			        lty=2,lwd=2)
    			} # end if(any(is.na(shf$IPR))==T) 
    			if(any(is.na(shf$IPR))==F) 
    			{
    			  lines(shf$year,rep(median(shf$IPR,na.rm=T),length(shf$year)),col=clr[3],lty=2,lwd=2)
    			} # end if(any(is.na(shf$IPR))==F) 
    			# now add the points
    			points(shf$year, shf$IPR, type = "o", pch = pch[1],cex=cx,col=clr[1])
    			# If dat2 is provided then we add these points and possibly se (well the se/mean) as well. 
    			if(is.null(dat2) == F)
    			{
    			  points(dat2$year, dat2$IPR, type = "o", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
    			  # add the mean line, if statement handling cases with/without NA's
    			  if(any(is.na(dat2$IPR))==T) 
    			  {
    			    lines(dat2$year[-which(is.na(dat2$IPR))],rep(median(dat2$IPR,na.rm=T),length(dat2$year[-which(is.na(dat2$IPR))])),
    			          col=clr[2],lty=2,lwd=2)
    			  } # end if(any(is.na(dat2$IPR))==T) 
    			  if(any(is.na(dat2$IPR))==F) 
    			  {
    			    lines(dat2$year,rep(median(dat2$IPR,na.rm=T),length(dat2$year)),col=clr[2],lty=2,lwd=2)
    			  } # end if(any(is.na(dat2$IPR))==F) 
    				if(se==T && "IPR.cv" %in% names(dat2)==T) segments(dat2$year,dat2$IPR+dat2$IPR.se,dat2$year,
    				                                                 dat2$IPR-dat2$IPR.se,col=clr[2])
    			  } # end if(is.null(dat2)==F)
          
    			if(se==T && "IPR.cv" %in% names(shf) ==T) segments(shf$year,shf$IPR+shf$IPR.se,
    			                                                shf$year,shf$IPR-shf$IPR.se,col=clr[1])
    		  if(is.null(RS)==T) text(years[ypos], ymax[1]*1, "Pre-recruits", cex=1.5, adj = 0)
    			else if(is.null(RS)==F) text(years[ypos], ymax[1]*1, 
    			                             substitute(paste("Pre-", recruits < a, " mm",sep=""),list(a=RS)), cex=1.5, adj = 0)
    			# Add a title?
    			if(add.title == T) title(titl,cex.main = cx.mn,outer=T)
		  } # end if('pre'%in%plots)
		
	  # Biomass for recruit scallops.
	  if('rec'%in%plots==T)
		  {
  			plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(0, ymax[2]), mgp = c(0.5, 0.5, 0), 
  			     tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=1.3)
	      axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5),lab = F, tcl = -0.6,cex.axis=axis.cx)
	      axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),1), lab = F, tcl = -0.3,cex.axis=axis.cx)
	    
  			# Note the labels command gets our y axis labels to scale to kg rather than grams, only necessary for Num per tow.
  			if(Npt == T) axis(2, at = pretty(c(0,ymax[2]*ys)), labels = pretty(c(0,ymax[2]*ys)/1e3), 
  			                  mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
  			if(Npt == F) axis(2, at = pretty(c(0,ymax[2]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
  			axis(4, pretty(c(0,ymax[2]*ys)),lab = F, tcl = -0.3,cex.axis=axis.cx)
  			# add the mean line, if statement handling cases with/without NA's
  			if(any(is.na(shf$IR))==T) 
  			{
  			  lines(shf$year[-which(is.na(shf$IR))],rep(median(shf$IR,na.rm=T),length(shf$year[-which(is.na(shf$IR))])),
  			        col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$IR))==T) 
  			if(any(is.na(shf$IR))==F) 
  			{
  			  lines(shf$year,rep(median(shf$IR,na.rm=T),length(shf$year)),col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$IR))==F) 
  			# now add the points
  			points(shf$year, shf$IR, type = "o", pch = pch[1],cex=cx,col=clr[1])
  			# If dat2 is provided then we add these points and possibly se (well the se/mean) as well. 
  			if(is.null(dat2) == F)
  			{
  			  points(dat2$year, dat2$IR, type = "o", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
  			  # add the mean line, if statement handling cases with/without NA's
  			  if(any(is.na(dat2$IR))==T) 
  			  {
  			    lines(dat2$year[-which(is.na(dat2$IR))],rep(median(dat2$IR,na.rm=T),length(dat2$year[-which(is.na(dat2$IR))])),
  			          col=clr[2],lty=2,lwd=2)
  			  } # end if(any(is.na(dat2$IR))==T) 
  			  if(any(is.na(dat2$IR))==F) 
  			  {
  			    lines(dat2$year,rep(median(dat2$IR,na.rm=T),length(dat2$year)),col=clr[2],lty=2,lwd=2)
  			  } # end if(any(is.na(dat2$IR))==F) 
  				  if(se==T && "IR.cv"%in%names(dat2)==T) segments(dat2$year,dat2$IR+dat2$IR.se,
  				                                                  dat2$year,dat2$IR-dat2$IR.se,col=clr[2])
  			  }# end if(is.null(dat2)==F)
  			
  			if(se==T && "IR.cv"%in%names(shf)==T) segments(shf$year,shf$IR+shf$IR.se,shf$year,
  			                                               shf$IR-shf$IR.se,col=clr[1])
  			if(is.null(RS)==T) text(years[ypos], ymax[2]*0.9, "Recruits", cex=1.5, adj = 0)
  			else if(is.null(RS)==F) text(years[ypos], ymax[2]*0.9, paste("Recruits",RS,"-",CS-1,"mm"), cex=1.5, adj = 0)
		}# end if('rec'%in%plots)
	
	  # Finally biomass for commercial scallops.
		if('com'%in%plots==T)
		  {
		  	plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(0, ymax[3]), mgp = c(0.5, 0.5, 0), 
		  	     tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=1.3)
		    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5),lab = F, tcl = -0.6,cex.axis=axis.cx)
		    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),1), lab = F, tcl = -0.3,cex.axis=axis.cx)
		  
  			# Note the labels command gets our y axis labels to scale to kg rather than grams, only necessary for Num per tow.
  			if(Npt == T) axis(2, at = pretty(c(0,ymax[3]*ys)), labels = pretty(c(0,ymax[3]*ys)/1e3), 
  			                  mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
  			if(Npt == F) axis(2, at = pretty(c(0,ymax[3]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
  			axis(4, pretty(c(0,ymax[3]*ys)),lab = F, tcl = -0.3,cex.axis=axis.cx)
  			# add the mean line, if statement handling cases with/without NA's
  			if(any(is.na(shf$I))==T) 
  			{
  			  lines(shf$year[-which(is.na(shf$I))],rep(median(shf$I,na.rm=T),length(shf$year[-which(is.na(shf$I))])),col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$I))==T) 
  			if(any(is.na(shf$I))==F) 
  			{
  			  lines(shf$year,rep(median(shf$I,na.rm=T),length(shf$year)),col=clr[3],lty=2,lwd=2)
  			} # end if(any(is.na(shf$I))==F) 
  			# now add the points
  			points(shf$year, shf$I, type = "o", pch = pch[1],cex=cx,col=clr[1])
  			# If dat2 is provided then we add these points and possibly se (well the se/mean) as well. 
  			if(is.null(dat2) == F)
  			{
  			  points(dat2$year, dat2$I, type = "o", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
  			  # add the mean line, if statement handling cases with/without NA's
  			  if(any(is.na(dat2$I))==T) 
  			  {
  			    lines(dat2$year[-which(is.na(dat2$I))],rep(median(dat2$I,na.rm=T),length(dat2$year[-which(is.na(dat2$I))])),
  			          col=clr[2],lty=2,lwd=2)
  			  } # end if(any(is.na(dat2$I))==T) 
  			  if(any(is.na(dat2$I))==F) 
  			  {
  			    lines(dat2$year,rep(median(dat2$I,na.rm=T),length(dat2$year)),col=clr[2],lty=2,lwd=2)
  			  } # end if(any(is.na(dat2$I))==F) 
  			  	if(se==T && "I.cv"%in%names(dat2)==T)segments(dat2$year,dat2$I+dat2$I.se,dat2$year,
  			  	                                              dat2$I-dat2$I.se,col=clr[2])
  			  }# end if(is.null(dat2)==F)

  			if(se==T && "I.cv" %in% names(shf)==T)segments(shf$year,shf$I+shf$I.se,shf$year,shf$I-shf$I.se,col=clr[1])
  			if(is.null(CS)==T)text(years[ypos], ymax[3]*0.9, "Fully recruited", cex=1.5, adj = 0)
  			else if(is.null(CS)==F)text(years[ypos], ymax[3]*0.9, 
  			                            substitute(paste("Fully ", recruited>=a, "mm"),list(a=CS)), cex=1.5, adj = 0)
		  }# end if ('com'%in%plots)
		
	  axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5),tcl = -0.6, cex.axis=axis.cx)
		# Lots of options to change this from current if we want (if we want brackets bgroup("(",frac(bar(kg),tow),")"))
	  if(Npt==T)  mtext(side=2,substitute(paste("",frac(kg,tow)),list(kg="kg",tow="tow")), line=1.5,
		                  outer = T, cex = 1.2,las=1)	
		if(Npt==F)  mtext("Total Biomass (t)", 2, 3, outer = T, cex = 1.2)	
		mtext("Year", 1, 4, outer = T, cex = 1.2)	
			
	} # end if(type == "B")

	# if pdf =T  then shut down the plotting device
	if(pdf==T) dev.off()
	# Return the object to R, not entirely sure why?
	shf
}

