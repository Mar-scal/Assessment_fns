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
                    mc=40,title.txt=paste("Breakdown",yr), add.title=T, value=F)
{
  
  require(patchwork)
  
  # Grab the biomass and numbers for each bin size from the survey, this is in kg per tow and number per tow.
  if("years" %in% colnames(surv.obj$shf.dat$w.yst)) bm<-surv.obj$shf.dat$w.yst[which(surv.obj[[1]]$year==yr),which(colnames(surv.obj$shf.dat$w.yst) %in% seq(5,200,5))]/1000
  if("years" %in% colnames(surv.obj$shf.dat$n.yst)) num<-surv.obj$shf.dat$n.yst[which(surv.obj[[1]]$year==yr),which(colnames(surv.obj$shf.dat$n.yst) %in% seq(5,200,5))]
  
  if(!"years" %in% colnames(surv.obj$shf.dat$w.yst)) bm<-surv.obj$shf.dat$w.yst[which(surv.obj[[1]]$year==yr),]/1000
  if(!"years" %in% colnames(surv.obj$shf.dat$n.yst)) num<-surv.obj$shf.dat$n.yst[which(surv.obj[[1]]$year==yr),]
  
  # Get the Commercial size if it hasn't been specified
  if(is.null(CS)==T) CS <- surv.obj$model.dat$CS[length(surv.obj$model.dat$CS)]
  if(is.null(RS)==T) RS <- surv.obj$model.dat$RS[length(surv.obj$model.dat$RS)]
  
  # Set up a vector of all size classes
  vec<-seq(0,195,5)
  
  # Get the meat count into meats per 500 grams.
  count=num/bm*0.5
  
  # If the xlim is NULL make the xlim 15 mm smaller than the the recruits and out to 200 mm size.
  if(is.null(xlim)==T) xlim <- c(RS-15,200)
  
  # Set the maximum for the y axis if not specified, this is for the shf data.
  if(is.null(y2max) ==T) y2max<-max(count[(min(xlim,na.rm=T)/5-1):length(vec)],na.rm=T)*1.1
  # Specifiy the y axis for the meat count if not done already.  Using xlim pick a meat count in line with the data.
  if(is.null(y1max)==T) y1max<-max(bm[(min(xlim,na.rm=T)/5-1):length(vec)],na.rm=T)*1.1
  
  
  # And plot the data
 
  # Lines to connect to our MC regulation
  for(i in 1:length(mc))
  {
    #Set up a dataframe
    cntdt <- data.frame(x=c(vec[which(count==min(count[count>mc[i]],na.rm=T))],
                            vec[which(count==max(count[count<mc[i]],na.rm=T))])+2.5,y=c(min(count[count>mc[i]],na.rm=T),
                                                                                        max(count[count<mc[i]],na.rm=T)))
    # Get the points for the line
    sht.cnt<-with(cntdt,(mc[i]-y[1])/((y[2]-y[1])/(x[2]-x[1]))+x[1])
  } # end for(i in 1:length(mc))
  browser()
  xlabs <- seq(0,200,5)
  xlabs[which(!xlabs %in% seq(0,200,10))] <- ""
  
  ylabs <- pretty(c(0, y2max))
  ylabs2 <- max(ylabs)/((length(ylabs)-1)*2)
  ylabs2 <- seq(0, max(ylabs), ylabs2)
  ybreaks <- ylabs2
  ylabs2[which(!ylabs2 %in% ylabs)] <- ""
  
  mcplot <- ggplot() +
    geom_point(aes(x=vec+2.5, y=count), col="firebrick") +
    geom_line(aes(x=vec+2.5, y=count), col="firebrick") +
    geom_segment(aes(x=sht.cnt, xend=Inf, y=mc, yend=mc), lty="dashed", col="firebrick")+
    geom_segment(aes(x=sht.cnt, xend=sht.cnt, y=-Inf, yend=mc), lty="dashed", col="firebrick")+
    theme_bw() +
    coord_cartesian(xlim=xlim, ylim=c(0, y2max))+
    scale_x_continuous(breaks=seq(0,200,5), labels=NULL)+
    scale_y_continuous(breaks=ybreaks, labels=ylabs2)+
    xlab(NULL) +
    ylab(expression(paste("MC: ", frac("N", "500g")))) +
    theme(axis.title.y=element_text(angle=0, vjust=0.5),
          panel.grid=element_blank()) +
    geom_vline(aes(xintercept=RS), col="blue") +
    geom_vline(aes(xintercept=CS), col="blue")
  
  # # Add the axis labels...
  
  ylabs <- pretty(c(0, y1max))
  ylabs2 <- max(ylabs)/((length(ylabs)-1)*2)
  ylabs2 <- seq(0, max(ylabs), ylabs2)
  ybreaks <- ylabs2
  ylabs2[which(!ylabs2 %in% ylabs)] <- ""
  
  bmplot <- ggplot() + geom_bar(aes(x=vec+2.5, y=bm), 
                                stat="identity",
                                fill="grey",
                                col="black",
                                width=5) +
    theme_bw() + 
    coord_cartesian(xlim=xlim, ylim=c(0, y1max))+
    scale_x_continuous(breaks=seq(0,200,5), labels=xlabs)+
    scale_y_continuous(breaks=ybreaks, labels=ylabs2)+
    xlab("Shell Height (mm)") +
    ylab(expression(frac("kg", "tow"))) +
    theme(axis.title.y=element_text(angle=0, vjust=0.5),
          panel.grid=element_blank()) +
    geom_vline(aes(xintercept=sht.cnt), lty="dashed", col="firebrick") +
    geom_vline(aes(xintercept=RS), col="blue") +
    geom_vline(aes(xintercept=CS), col="blue") #+
  #annotate(geom="text", x=sht.cnt*1.01, y=ybreaks[round(length(ybreaks)*0.75)], label="MC\nregulation", col="firebrick",
  #  hjust=0)
  
  # Finally add the title.
  if(add.title == T) {
    mcplot <- mcplot + ggtitle(title.txt) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  print(
    (mcplot/bmplot) + 
      plot_layout(height = c(1, 2))
  )
  
  if(value==T) return(list(sht.cnt=sht.cnt))
  
} # end function



