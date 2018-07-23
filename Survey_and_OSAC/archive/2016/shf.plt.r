####  Commented and checked by DK starting on July 28, 2015. This function makes a plot of the shell height frequencies for a bank
####  You can control the number of years plotted, More than 6 years gets kinda messy.
# Update history
#Commented, checked  and revised by DK March 31, 2016
####
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

#ps.dat:       Old MW-SH data from 1981-2012.  
#survey.obj:   Survey data including shell height frequencies and summary of size classes, for data since 2012 and possibly earlier
#from:         Where to pull the data.  Default ='surv'.  Options:
#              "ps"- use old data (up to 2012) only: 
#              "special" is survey data but with a special structure in which each list level is a different year
#              "survwt" is survey data using weights rather than numbers.
#yr:           Years of interest.  If missing it takes all data from 1981-current,
#type:         Shell height (sh) or meat weight (wt) plot.  Default ='sh'
#col1:         Color #1.  Default = 'seagreen2'
#col2:         Color #2.  Default ='red'
#col3:         Color #3.  Default ='navy'
#rows:         Number of rows in the plotting device, a row = 1 year of data.  Default = 7
#pdf:          Produce a pdf plot?  Default = F
#xl:           X axis limit.  Default is missing
#rel:          Relative frequency (%) or Frequency of data in figures.  (T/F) Default =T
#mean.line:    Plot the mean.  (T/F) default = F
#adj:          adj location of text on figure.  Default =0.1
#bx:           Add a box around figure.  Default = F
#strata:        Strata to choose.=1:7,
#ymax:          y axis maximum.  Default is missing 
#recline:       Draw a line to seperate the Pre recruits recruits and commercial scallops size classes.  Default = NULL
#recline2:      An additional line to seperate the Pre recruits recruits and commercial scallops size classes.  Default = NULL,
#wd:            Width of the plot device.  Default = 10,
#ht:            Height of the plot device.  Default = 12
#ylab:          Label for the y- axis.  Default ="Mean N / standard tow",
#sample.size:   Sample size from the survey each year.  Default = F.  The sample sizes must be specified if not = false.
#add.title:     Add a title to the plot?  (T/F) default = F.
#cex.mn:        Magnification of title plot.  Default = 1.
#titl:          The title name.  Default is blank.
#cx.axis:       The axis text size.  Default = 1.5.


shf.plt<-function(ps.dat,survey.obj,from='surv',yr,type='sh',col1='seagreen2',col2='red',col3='navy',rows=7,pdf=F,xl,rel=T,
                  mean.line=F,adj=0.1,bx=F,strata=1:7,ymax,recline=NULL,recline2=NULL,wd=10,ht=12,ylab,
                  sample.size=F, add.title = F, cex.mn = 1, titl = "",cx.axis=1.5, ...)
  {
	
	# If year is not specified all the years in teh ps.dat file are used.
	if(missing(yr)==T) yr<-sort(unique(ps.dat$year))
	# If ymax is specified and is length one repeat it for all years.
	if(missing(ymax)==F && length(ymax)==1) ymax<-rep(ymax,length(yr))
	
	# Open the plotting device depending on whether pdf == T or not.
	if(pdf==T) pdf(paste("SHF",from,min(yr),"-",max(yr),".pdf",sep=""), width = wd, height = ht)
#	if(pdf==F) windows(wd,ht)

	# Set up the plotting device
	par(mfrow=c(rows,ceiling(length(yr)/rows)), mar = c(0,2,0,0.5), omi = c(0.85, 0.75, 0.5, 0.5))
	# Are we using the sample size, if so extract it from survey.obj
	if(sample.size==T)sample.size<-survey.obj[[1]]$n[survey.obj[[1]]$year%in%yr]
	if(sample.size==F)sample.size<-NULL
	
	# If we are looking at shell height data this is our plot to make
	if(type=='sh')
	  {
	 	  # Get the x limits for the plot.
  		if(missing(xl)==T)xlm<-c(min(ps.dat$sh),max(ps.dat$sh))
  		else if(missing(xl)==F)xlm<-xl
  		
  		# divide up the data into bins of 5
  		vec<-seq(xlm[1],xlm[2],5)
  		# Run the plot across all the years.
  		# Open the plot window.
  		par(...)
  		for(i in 1:length(yr))
  		  {
    			# make the historgram object from the ps data.
    			tmp<-hist(ps.dat$sh[ps.dat$year==yr[i] & ps.dat$sh<xlm[2]],plot=F,breaks=vec)
    			# Now if we want to data from the survey fill in the data for the plot from either the survey.
    			if(from=='surv') tmp$counts<-survey.obj[[2]]$n.yst[survey.obj[[1]]$year==yr[i],] 
    			# from a survey object that has as many levels as years.
    			if(from=='special') tmp$counts<-survey.obj[i,] 
    			# Or from the survey object with weights instead of numbers, divide by 1000 to get into tonnes
    			if(from=='survwt') tmp$counts<-survey.obj[[2]]$w.yst[survey.obj[[1]]$year==yr[i],]/1000
    			# Determine plot maximum
    			yl2 <-ifelse(missing(ymax),max(tmp$counts)*1.2,ymax[i])
    			# Now plot the data and set up the axes.
    			plot(tmp,main="",xlab="",ylab="",xlim=xlm,col=col1,yaxt='n',xaxt='n',ylim=c(0,yl2))
    		  # add tick marks and such
    			axis(1,vec,lab=F,tcl=-0.3)
    			axis(1,lab=F,tcl=-0.6)
    			if(i>1) axis(3,lab=F,tcl=0)
    			# Determine the axis based on whether we are looking at relative frequecy or actual counts.
    			if(rel==F) axis(2, pretty(c(0,yl2/1.3)),las=1,cex.axis=cx.axis)
    			if(rel==T) axis(2,seq(0,max(tmp$counts),l=6),lab=seq(0,100,20),las=1,cex.axis=cx.axis)
    			if(rel==F) axis(4, pretty(c(0,yl2/1.3)),lab=F)
    			if(rel==T) axis(4,seq(0,max(tmp$counts),l=6),lab=F,las=1)
    			if(i == 1 && add.title==T) title(titl,cex.main = cex.mn,outer=T)
    			# For the final year we need to do a few little tricks.
    			if(i==length(yr))
    			  {
      			  #Plot the axis labels at final step
      				axis(1,cex.axis=cx.axis)
      			  # If we want to show the mean over the course of the data plotted this grabs that data 
    			    #and puts it on the last years plot.
      				if(mean.line==T)
  				     {
                  # depending on where we want to get the data for the mean line.  
        				  if(from=='special')lines(tmp$mids,colMeans(survey.obj[-nrow(survey.obj),]),lwd=1)
        					if(from=='surv')lines(tmp$mids,colMeans(survey.obj[[2]]$n.yst[(nrow(survey.obj[[2]]$n.yst)-length(yr)):
        					                                                                (nrow(survey.obj[[2]]$n.yst)-1),]),lwd=1)
        					if(from=='ps')
      					    {
    						      tmp2<-hist(ps.dat$sh[ps.dat$year%in%yr[-length(yr)]&ps.dat$sh<xlm[2]],breaks=vec,plot=F)
    						      lines(tmp2$mids,tmp2$counts/max(tmp2$counts)*max(tmp$counts),lwd=1)
    					      } # end if(from=='ps')
      				    # add the legend
      					  legend(xlm[2]*0.8,yl2/1.3,paste(yr[1],'-',yr[length(yr)-1]),bty='n',lty=1)
  				      } # end if(mean.line==T)
  			    } # end if(i == length(yr))
  			
    			# Add the lines showing the minimum size of the recruit and commercial scallops.
    			if(is.null(recline) == F)
    			  {
    		  		abline(v=recline,lwd=2,col=col2)
    			  	abline(v=recline2,lty=2,col=col3)
    			  } # end if(!is.null(recline))
          # Add the year to each plot
  		  	mtext(as.character(yr[i]), 3, -3, adj=adj,outer = F,cex=1,col=col2)
  			
  			  # If we want to show the sample size on plot.
  			  if(is.null(sample.size)==F) mtext(paste("N =",sample.size[i]), 3, -3, adj=0.9,outer = F,cex=1,col=col2)
  			
  			  # Draw a box around the plot
  			  if(bx==T)box()
  			  #
  		  } # end for(i in 1:length(yr))
		  # Add the title to the plot.
		  mtext("Shell height (mm)", 1, 3, outer = T, cex = 1.2)	
		  if(missing(ylab) == T) mtext(side=2,substitute(paste("",frac(N,tow),),list(N="N",tow="tow")), line=2,
		                                     outer = T, cex = 1.2,las=1)	
		  if(missing(ylab) == F) mtext(ylab, 2, 2, outer = T, cex = 1.2)	
	  } # end if(type=='sh')
	
	# Create the plot for the meat weight data.  This is very much similar to the sh plot so no detailed commenting.  Note that
	# only from == 'surv' or == 'ps' works with this option.
	if(type=='wt')
	  {
	
  		if(missing(xl))xlm<-c(min(ps.dat$weight),max(ps.dat$weight))
  		else if(!missing(xl))xlm<-xl
  		
  		vec<-seq(xlm[1],xlm[2],2)
  		# Open the plot window.
  		par(...)
  		for(i in 1:length(yr))
  		  {
    			tmp<-hist(ps.dat$weight[ps.dat$year==yr[i]&ps.dat$weight<xlm[2]],breaks=vec,plot=F)
    			if(from=='surv'){tmp$counts<-survey.obj[[2]]$w.yst[nrow(survey.obj[[2]]$w.yst)-(length(yr)-i),] }
    			yl2<-ifelse(missing(ymax),max(tmp$counts)*1.2,ymax)
    			plot(tmp,main="",xlab="",ylab="",xlim=xlm,col=col1,yaxt='n',xaxt='n',ylim=c(0,yl2))
    			mtext(as.character(yr[i]), 3, -2, adj=adj,outer = F,cex=0.75,col=col2)
    			axis(1,vec,lab=F,tcl=-0.3)
    			axis(1,lab=F,tcl=-0.6)
    			if(i>1)axis(3,lab=F,tcl=0)
    			if(rel==F)axis(2, pretty(c(0,yl2/1.3)),las=1,cex.axis=cx.axis)
    			if(rel==T)axis(2,seq(0,max(tmp$counts),max(tmp$counts)/5),lab=seq(0,100,20),las=1,cex.axis=cx.axis)
    			if(rel==F)axis(4, pretty(c(0,yl2/1.3)),lab=F)
    			if(rel==T)axis(4,seq(0,max(tmp$counts),max(tmp$counts)/5),lab=F,las=1)
    			if(i==length(yr))
    			  {
  			    	axis(1,cex.axis=cx.axis)
  			    	if(mean.line==T)
  			    	  {
        					if(from=='surv')lines(tmp$mids,colMeans(survey.obj[[2]]$w.yst[-nrow(survey.obj[[2]]$w.yst),]),lwd=1)
        					if(from=='ps')
        				    {
    			  			    tmp2<-hist(ps.dat$weight[ps.dat$year%in%yr[-length(yr)]&ps.dat$weight<xlm[2]],breaks=vec,plot=F)
    			  			    lines(tmp2$mids,tmp2$counts/max(tmp2$counts)*max(tmp$counts),lwd=1)
    					      } # end if(from=='ps')
    					    legend(xlm[2]*0.8,yl2/1.3,paste(yr[1],'-',yr[length(yr)-1]),bty='n',lty=1)
  				      } # end if(mean.line==T)
  			     } # end if(i==length(yr))
  					
  			  if(bx)box()
   		  } # end for(i in 1:length(yr))
  		# Add the title to the plot.
  		mtext("Meat weight (g)", 1, 3, outer = T, cex = 1.2)	
  		# Alter the y label depending on 
  		if(rel==F) mtext("Frequency", 2, 2, outer = T, cex = 1.2)	
  		if(rel==T) mtext("Relative frequency (%)", 2, 2, outer = T, cex = 1.2)	

	  }# end if(type=="wt")
	# shut down pdf device if using it.
  if(pdf==T)dev.off()	
} # end function

