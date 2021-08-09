####  Commented and checked by DK starting on July 28, 2015. This function makes a plot of the shell height frequencies for a bank
####  You can control the number of years plotted, More than 6 years gets kinda messy.
# Update history
#Commented, checked  and revised by DK March 31, 2016
# July 7, 2016:  Massive overhaul of the figure.  Removed some options that don't seem to be used, if you need the "type" - MW plot
# use the archived shf.plt.r, not sure why you would as we have the option to make this kind of plot by setting from = survwt.
# But maybe you want to use the hydration sampling data or something I've never done.  If so I'm sorry to have been a jerk and removed
# that.
# July 17, 2017:  A minor change to allow different y limits for each figure, not something you'd usually want!
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

#survey.obj:   Survey data including shell height frequencies and summary of size classes, for data since 2012 and possibly earlier
#from:         Where to pull the data.  Default ='surv'.  Options:
#              "survwt" is survey data using weights rather than numbers.
#yr:            Years of interest.  If missing it takes all data from 1981-current,
#col1:          Color for the histogram..  Default = 'seagreen2'
#col2:          Color for the text and any supplementary lines.  Default ='red'
#rows:          Number of rows in the plotting device, a row = 1 year of data.  Default = 7
#rel:           Relative frequency (%) or Frequency of data in figures.  (T/F) Default =T
#split:         Where to split the plot between size classes, this allows for different scales in this figure... You specific the size
#               bin that you want to split out.  Default = NA which means no split and all data on one scale.
#adj:           adj location of text on figure.  Default =0.9 (puts text towards the left), don't set this to be less than 0.2 or you lose year.
#bx:            Add a box around figure.  Default = F
#ymax:          y axis maximum.  Default is NA
#recline:       Draw a line to seperate the Pre recruits recruits and commercial scallops size classes.  Default = NULL
#ylab:          Label for the y- axis.  Default is missing which results in "Mean N / standard tow",
#sample.size:   Sample size from the survey each year.  Default = F.  The sample sizes must be specified if not = false.
#add.title:     Add a title to the plot?  (T/F) default = F.
#cex.mn:        Magnification of title plot.  Default = 1.
#titl:          The title name.  Default is blank.
#cx.axis:       The axis text size.  Default = 1.5.
shf.plt<-function(survey.obj,from='surv',yr,type='sh',col1='seagreen2',col2='red',rows=7,rel=T,split=NA, bins = seq(5,200,by=5),
                  select = 5,adj=0.9,bx=F,ymax=NA,recline=NULL,ylab,sample.size=F, add.title = F, cex.mn = 1, titl = "",cx.axis=1.5, ...)
{
  if(missing(yr)==T) yr<-sort(unique(survey.obj$model.dat$year))
  
  # Are we using the sample size, if so extract it from survey.obj
  if(sample.size==T) sample.size<-survey.obj[[1]]$n[survey.obj[[1]]$year%in%yr]
  #if(sample.size==F) sample.size<-NULL
  
  # If we are looking at shell height data this is our plot to make
  # Get the bins
  if("years" %in% colnames(survey.obj[[2]]$n.yst)) survey.obj[[2]]$n.yst <- survey.obj[[2]]$n.yst[, which(colnames(survey.obj[[2]]$n.yst) %in% bins)]
  if("years" %in% colnames(survey.obj[[2]]$w.yst)) survey.obj[[2]]$w.yst <- survey.obj[[2]]$w.yst[, which(colnames(survey.obj[[2]]$w.yst) %in% bins)]
  
  if(from=='surv') colnames(survey.obj[[2]]$n.yst) <- bins
  if(from=='survwt') colnames(survey.obj[[2]]$w.yst) <- bins
  # Now we select the bins we are interested in...
  if(from=='surv')  surv.obj <- as.data.frame(survey.obj[[2]]$n.yst[,which(as.numeric(colnames(survey.obj[[2]]$n.yst)) >= select)])
  if(from=='survwt') surv.obj <- as.data.frame(survey.obj[[2]]$n.yst[,which(as.numeric(colnames(survey.obj[[2]]$w.yst)) >= select)])
  surv.obj$year <- survey.obj[[1]]$year
  # Reset bins usually unecessary...
  bins <- seq(select-5,max(bins),by=5)
  # Set the ymax if not preset
  if(is.na(split) == T)
  {
    if(is.na(ymax) == T) ymax <- max(max(apply(surv.obj[surv.obj$year %in% yr,-ncol(surv.obj)],2,function(x){max(x,na.rm=T)})),10)
    # if ymax is length 1, repeat it so that it is the same length as the number of plots.
    if(length(ymax) ==1) ymax <- rep(ymax,length(yr))
  } # end if(is.na(split) == T)
  
  # Now start to  make the plot.
  for(i in 1:length(yr))
  {
    # Now if we want to data from the survey fill in the data for the plot from either the survey.
    counts<-surv.obj[surv.obj$year==yr[i],-ncol(surv.obj)]
    
    # Or from the survey object with weights instead of numbers, divide by 1000 to get into kg
    #A do it yourself historgram object based on our data...
    # I use mids to place the location of the tick marks, currently using 5 which puts the ticks for a size class at the
    # end of each histogram bar (i.e. number represents previous histogram)
    obj <- list(breaks = bins,counts = counts, intensities = counts/sum(counts),density = counts/sum(counts),mids = bins[-length(bins)]+5,
                xname = "Counts",equidist = T)
    class(obj) <- "histogram"
    
    # If we aren't splitting the plot then do this
    if(is.na(split) == T)
    {
      # Set up the plot device
      if(i ==1) par(mfrow=c(rows,ceiling(length(yr)/rows)), mar = c(0.25,2,0.25,0.5), omi = c(0.85, 0.75, 0.5, 0.5))
      # Don't let the maximum be less than 10 ever!
      
      # Now plot the data and set up the axes
      # add tick marks and such
      if(rel== F) plot(obj,main="",xlab="",ylab="",xlim=c((select-5),max(bins)),col=col1,yaxt='n',xaxt='n',ylim=c(0,ymax[i]),freq=T)
      if(rel==T)  plot(obj,main="",xlab="",ylab="",xlim=c((select-5),max(bins)),col=col1,yaxt='n',xaxt='n',freq=F)
      # if we put in a ymax, and there are bins that exceed the ymax, let's put a little arrow on them to show that they are actually larger than ymax.
      if(dim(counts[which(counts>ymax[i])])[2]>0) text(obj$mids[which(counts>ymax[i])]-2.5, rep(ymax[i], length(which(counts>ymax[i]))), adj =c(1, 0.4), label=round(counts[which(counts>ymax[i])], 0), srt=90, cex=1) 
      axis(1,obj$mids,lab=F,tcl=-0.3)
      axis(1,seq(min(obj$mids),max(obj$mids),by=20),lab=F,tcl=-0.6)
      if(rel==F) axis(2,las=1,at=pretty(c(0,ymax[i])),labels=pretty(c(0,ymax[i])),cex.axis=cx.axis)
      if(rel==T) axis(2,las=1,at=pretty(c(0,0.2)),labels=pretty(c(0,0.2)),cex.axis=cx.axis)
      # Add the year to the figure
      mtext(as.character(yr[i]), 3, -3, adj=(adj-.2),outer = F,cex=1,col=col2)
      # Add the lines showing the minimum size of the recruit and commercial scallops, or whatever turns your crank...
      if(is.null(recline) == F) abline(v=recline,lwd=2,col=col2)
      # If we want to show the sample size on plot.
      if(is.null(sample.size)==F) mtext(paste("N =",sample.size[i]), 3, -3, adj=adj,outer = F,cex=1,col=col2)
      
      # #Plot the axis labels at final step
      if(i==length(yr)) axis(1,at = seq(min(obj$mids),(max(obj$mids)),by=20),
                             labels = seq(min(obj$breaks)+5,max(obj$breaks),by=20),cex.axis=cx.axis)
      
      # Draw a box around the plot
      if(bx==T) box()
    } # end if(is.na(split) == T)
    
    # If we are splitting the plot then do this
    if(is.na(split) == F)
    {
      # This will be used to divide the data into the small and large size bins.
      small <- which(as.numeric(colnames(surv.obj[,-ncol(surv.obj)])) <= split)
      large <- which(as.numeric(colnames(surv.obj)) > split) # the year column results in a warning but that's fine...
      # I'm forcing the maximum here to be no less than 10 for the y-axis.
      ymax.small <- ifelse(is.na(ymax)==T, max(max(apply(surv.obj[surv.obj$year%in%yr,small],
                                                         2,function(x){max(x,na.rm=T)})),10),ymax)
      ymax.large <- ifelse(is.na(ymax)==T, max(max(apply(surv.obj[surv.obj$year%in%yr,large],
                                                         2,function(x){max(x,na.rm=T)})),10),ymax)
      # Make the small and large objects 
      obj.small <- obj
      obj.large <- obj
      # And split the object into two bits...
      for( k in 1:5) obj.small[[k]] <- obj[[k]][small]
      for( k in 1:5) obj.large[[k]] <- obj[[k]][large]
      # The breaks needs to have a value on either side of the min so we have a bin for each, so do this...
      obj.small$breaks <- seq(select-5,split,by=5)
      obj.large$breaks <- seq(split,max(bins),by=5)
      ### I NEED TO MAKE THIS PROPORTIONAL TO THE NUMBER OF BINS ON EACH SIDE, I'LL NEED MY BUDDY LAYOUT FOR THIS...
      if(i==1) 
      {
        tst <- layout(matrix(1:(rows*2), rows, 2, byrow = T),widths = c(length(obj.small$counts),length(obj.large$counts)))
        par(mar = c(0.25,2,0.25,0.5), omi = c(0.85, 0.75, 0.5, 0.5))
        #  layout.show(tst)
      }
      
      # Now plot the data and set up the axes
      # add tick marks and such
      # Here's the small size classes
      if(rel== F) plot(obj.small,main="",xlab="",ylab="",xlim=c(select-5,split-2),col=col1,yaxt='n',xaxt='n',ylim=c(0,ymax.small),freq=T)
      if(rel==T)  plot(obj.small,main="",xlab="",ylab="",xlim=c(select-5,split-2),col=col1,yaxt='n',xaxt='n',freq=F)
      # Add the axes and labels.
      axis(1,obj.small$mids,lab=F,tcl=-0.3)
      axis(1,seq(min(obj.small$mids),max(obj.small$mids),by=20),lab=F,tcl=-0.6)
      if(rel==F) axis(2,las=1,at=pretty(c(0,ymax.small)),labels=pretty(c(0,ymax.small)),cex.axis=cx.axis)
      if(rel==T) axis(2,las=1,at=pretty(c(0,0.2)),labels=pretty(c(0,0.2)),cex.axis=cx.axis)
      # Add the year to the figure
      mtext(as.character(yr[i]), 3, -3, adj=adj,outer = F,cex=1,col=col2)
      # For the final year we Plot the axis labels at final step
      if(i==length(yr)) axis(1,at = seq(min(obj.small$mids),(max(obj.small$mids)+5),by=20),
                             labels = seq(min(obj.small$breaks)+5,(max(obj.small$breaks)+5),by=20),cex.axis=cx.axis)
      # Add the lines showing the minimum size of the recruit and commercial scallops.
      if(is.null(recline) == F) abline(v=recline,lwd=2,col=col2)
      
      # Add the title to the plot
      if(i == 1 && add.title==T) title(titl,cex.main = cex.mn,outer=T)
      # On the other side of the plot we add the large sizes.
      if(rel== F) plot(obj.large,main="",xlab="",ylab="",xlim=c(split+5,200),col=col1,yaxt='n',xaxt='n',ylim=c(0,ymax.large),freq=T)
      if(rel==T)  plot(obj.large,main="",xlab="",ylab="",xlim=c(split+5,200),col=col1,yaxt='n',xaxt='n',freq=F)
      axis(1,obj.large$mids,lab=F,tcl=-0.3)
      axis(1,seq(min(obj.large$mids),max(obj.large$mids),by=20),lab=F,tcl=-0.6)
      if(rel==F) axis(2,las=1,at=pretty(c(0,ymax.large)),labels=pretty(c(0,ymax.large)),cex.axis=cx.axis)
      if(rel==T) axis(2,las=1,at=pretty(c(0,0.2)),labels=pretty(c(0,0.2)),cex.axis=cx.axis)
      # For the final year we Plot the axis labels at final step
      if(i==length(yr)) axis(1,at = seq(min(obj.large$mids),(max(obj.large$mids)+5),by=20),
                             labels = seq(min(obj.large$breaks)+5,(max(obj.large$breaks)+5),by=20),cex.axis=cx.axis)
      # Add the lines showing the minimum size of the recruit and commercial scallops.
      if(is.null(recline) == F) abline(v=recline,lwd=2,col=col2)
      # If we want to show the sample size on plot.
      if(is.null(sample.size)==F) mtext(paste("N =",sample.size[i]), 3, -3, adj=0.9,outer = F,cex=1,col=col2)
      # Draw a box around the plot
      if(bx==T)box()
    } # end if(is.na(split) == F)
    if(i == 1 && add.title==T) title(titl,cex.main = cex.mn,outer=T)
  } # end for(i in 1:length(yr))
  
  # Add the title to the plot.
  mtext("Shell height (mm)", 1, 3, outer = T, cex = 1.2)	
  if(missing(ylab) == T) mtext(side=2,substitute(paste("",frac(N,tow),),list(N="N",tow="tow")), line=2,
                               outer = T, cex = 1.2,las=1)	
  if(missing(ylab) == F) mtext(ylab, 2, 2, outer = T, cex = 1.2)	
} # end function  
