
#survey.obj:   Survey data including shell height frequencies and summary of size classes, for data since 2012 and possibly earlier
#from:         Where to pull the data.  Default ='surv'.  Options:
#              "survwt" is survey data using weights rather than numbers.
#yr:            Years of interest.  If missing it takes all data from 1981-current,
#type:          Shell height (sh) or meat weight (wt) plot.  Default ='sh'
#col1:          Color #1.  Default = 'seagreen2'
#col2:          Color #2.  Default ='red'
#col3:          Color #3.  Default ='navy'
#rows:          Number of rows in the plotting device, a row = 1 year of data.  Default = 7
#pdf:           Produce a pdf plot?  Default = F
#xl:            X axis limit.  Default is missing
#rel:           Relative frequency (%) or Frequency of data in figures.  (T/F) Default =T
#split:         Where to split the plot between size classes, this allows for different scales in this figure... You specific the size
#               bin that you want to split out.  Default = NA which means no split and all data on one scale.
#mean.line:     Plot the mean.  (T/F) default = F
#adj:           adj location of text on figure.  Default =0.1
#bx:            Add a box around figure.  Default = F
#ymax:          y axis maximum.  Default is NA
#recline:       Draw a line to seperate the Pre recruits recruits and commercial scallops size classes.  Default = NULL
#wd:            Width of the plot device.  Default = 10,
#ht:            Height of the plot device.  Default = 12
#ylab:          Label for the y- axis.  Default ="Mean N / standard tow",
#sample.size:   Sample size from the survey each year.  Default = F.  The sample sizes must be specified if not = false.
#add.title:     Add a title to the plot?  (T/F) default = F.
#cex.mn:        Magnification of title plot.  Default = 1.
#titl:          The title name.  Default is blank.
#cx.axis:       The axis text size.  Default = 1.5.
yr=2016
direct = "d:/r/"
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
windows(11,8.5)
survey.obj <-  survey.obj[["BBn"]]
windows(11,8.5)
select = 5
split = 70
shf.plt(survey.obj[["BBn"]],yr=2010:2016,rel=F,recline=c(85,95),select=5,sample.size=T,split=NA,add.title=T,titl="Awesome")

shf.plt<-function(survey.obj,from='surv',yr,type='sh',col1='seagreen2',col2='red',col3='navy',rows=7,xl,rel=T,split=NA, bins = seq(5,200,by=5),
                  select = 5,adj=0.9,bx=F,ymax=NA,recline=NULL,ylab,sample.size=F, add.title = F, cex.mn = 1, titl = "",cx.axis=1.5, ...)
{
   
  if(missing(yr)==T) yr<-sort(unique(survey.obj$model.dat$year))
  
  # Are we using the sample size, if so extract it from survey.obj
  if(sample.size==T) sample.size<-survey.obj[[1]]$n[survey.obj[[1]]$year%in%yr]
  if(sample.size==F) sample.size<-NULL
  
  # If we are looking at shell height data this is our plot to make
  if(type=='sh')
  {
    # Get the bins
    if(from=='surv') colnames(survey.obj[[2]]$n.yst) <- bins
    if(from=='survwt') colnames(survey.obj[[2]]$w.yst) <- bins
    # Now we select the bins we are interested in...
    if(from=='surv')  surv.obj <- as.data.frame(survey.obj[[2]]$n.yst[,which(as.numeric(colnames(survey.obj[[2]]$n.yst)) >= select)])
    if(from=='survwt') surv.obj <- as.data.frame(survey.obj[[2]]$n.yst[,which(as.numeric(colnames(survey.obj[[2]]$w.yst)) >= select)])
    surv.obj$year <- survey.obj[[1]]$year
    # Reset bins usually unecessary...
    bins <- seq(select-5,max(bins),by=5)
    for(i in 1:length(yr))
    {
      # Now if we want to data from the survey fill in the data for the plot from either the survey.
      counts<-surv.obj[surv.obj$year==yr[i],-ncol(surv.obj)]
      # Or from the survey object with weights instead of numbers, divide by 1000 to get into kg
      #A do it yourself historgram object based on our data...
      obj <- list(breaks = bins,counts = counts, intensities = counts/sum(counts),density = counts/sum(counts),mids = bins[-length(bins)]+2.5,
                  xname = "Counts",equidist = T)
      class(obj) <- "histogram"
      # If we aren't splitting the plot then do this
      if(is.na(split) == T)
      {
        # Set up the plot device
        if(i ==1) par(mfrow=c(rows,ceiling(length(yr)/rows)), mar = c(0.25,2,0.25,0.5), omi = c(0.85, 0.75, 0.5, 0.5))
        if(is.na(ymax) == T) ymax <- max(apply(surv.obj[surv.obj$year %in% yr,-ncol(surv.obj)],2,function(x){max(x,na.rm=T)}))
        # Now plot the data and set up the axes
        # add tick marks and such
        if(rel== F) plot(obj,main="",xlab="",ylab="",xlim=c((select-5),max(bins)),col=col1,yaxt='n',xaxt='n',ylim=c(0,ymax),freq=T)
        if(rel==T)  plot(obj,main="",xlab="",ylab="",xlim=c((select-5),max(bins)),col=col1,yaxt='n',xaxt='n',freq=F)
        axis(1,obj$mids,lab=F,tcl=-0.3)
        axis(1,seq(min(obj$mids),max(obj$mids),by=25),lab=F,tcl=-0.6)
        if(rel==F) axis(2,las=1,at=pretty(c(0,ymax)),labels=pretty(c(0,ymax)),cex.axis=cx.axis)
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
        ymax.small <- ifelse(is.na(ymax)==T, max(apply(surv.obj[surv.obj$year%in%yr,small],
                                                       2,function(x){max(x,na.rm=T)})),ymax)
        ymax.large <- ifelse(is.na(ymax)==T, max(apply(surv.obj[surv.obj$year%in%yr,large],
                                                       2,function(x){max(x,na.rm=T)})),ymax)
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
        if(rel== F) plot(obj.small,main="",xlab="",ylab="",xlim=c(select-5,split+2.5),col=col1,yaxt='n',xaxt='n',ylim=c(0,ymax.small),freq=T)
        if(rel==T)  plot(obj.small,main="",xlab="",ylab="",xlim=c(select,split+2.5),col=col1,yaxt='n',xaxt='n',freq=F)
        # Add the axes and labels.
        axis(1,obj.small$mids,lab=F,tcl=-0.3)
        axis(1,seq(min(obj.small$mids),max(obj.small$mids),by=25),lab=F,tcl=-0.6)
        if(rel==F) axis(2,las=1,at=pretty(c(0,ymax.small)),labels=pretty(c(0,ymax.small)),cex.axis=cx.axis)
        if(rel==T) axis(2,las=1,at=pretty(c(0,0.2)),labels=pretty(c(0,0.2)),cex.axis=cx.axis)
        # Add the year to the figure
        mtext(as.character(yr[i]), 3, -3, adj=adj,outer = F,cex=1,col=col2)
                # For the final year we Plot the axis labels at final step
        if(i==length(yr)) axis(1,at = seq(min(obj.small$mids),(max(obj.small$mids)+5),by=20),
                               labels = seq(min(obj.small$breaks)+5,(max(obj.small$breaks)),by=20),cex.axis=cx.axis)
        # Add the lines showing the minimum size of the recruit and commercial scallops.
        if(is.null(recline) == F) abline(v=recline,lwd=2,col=col2)
        
        # Add the total to the plot
        if(i == 1 && add.title==T) title(titl,cex.main = cex.mn,outer=T)
        # On the other side of the plot we add the large sizes.
        if(rel== F) plot(obj.large,main="",xlab="",ylab="",xlim=c(split,200),col=col1,yaxt='n',xaxt='n',ylim=c(0,ymax.large),freq=T)
        if(rel==T)  plot(obj.large,main="",xlab="",ylab="",xlim=c(split,200),col=col1,yaxt='n',xaxt='n',freq=F)
        axis(1,obj.large$mids,lab=F,tcl=-0.3)
        axis(1,seq(min(obj.large$mids),max(obj.large$mids),by=25),lab=F,tcl=-0.6)
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
  } # end if(type=='sh')
} # end function  
