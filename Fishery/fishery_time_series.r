####  Created by DK in October of 2017.  This function creates a time series of catch, effort, and/or CPUE, with options to plot for
####  just the fleet average, or for the WF or FT components of the fleet.  This is largely an update of fish_sum, though
####  is the first function I've transitions to use ggplot2.
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

#1.	  dat:         The fishery time series.  This needs to be in ggplot2 format, so it'll need to be set up like this...
#                  year   fleet     catch   effort    cpue  cpue.LCI  cpue.UCI
#                  1981   Overall   22.111  2103.22   0.01  0.005     0.15 
#                  1981    WF       22.111  2103.22   0.01  0.005     0.15
#                  1981    FT       NA      NA        NA    NA        NA
#2.   fleet:       Which fleet do you want to plot.  4 options, default is to show them all fleet = "all".  Other options are "Overall", "FT", and or "WF"
#     plots:       Which plots would you like to make.  Default = "all", which makes Catch, Effort, and CPUE time series.  Other options are
#                  "catch", "effort","cpue".  To make 2 of the 3 figures you'd do something like  plots = c("catch","effort")
#2.	  years:       The years to plot.  Default = 1955:2007
#9.	  add.CIs:     Add the confidence interval for the CPUE data.  (T/F) default = T
#    yr.labels    # How many years between the x axis ticks, default shows every 5 years.  yr.labels=5

# source("Y:/Assessment/2010/r/fn/fishery.plt.r")

fishery.ts<-function(dat,plots = "all",fleet = "Overall",years=1955:2007,add.title=F,bnk = NULL,add.CIs=T, yr.labels=5)
{
  require(ggplot2) || stop("You'll need to install ggplot2 if you want to run this marvelous function.")
  require(Rmisc)  || stop("You'll need to install Rmisc if you want to run this marvelous function.")
  require(gridExtra) || stop("You'll need to install gridExtra if you want to run this marvelous function.")
  require(grid) || stop("You'll need to install grid if you want to run this marvelous function.")
  require(cowplot) || stop("You'll need to install cowplot if you want to run this marvelous function.")
  
  # Subset to the data you want to look at.
  sub.dat <- dat[dat$year %in% years,]
  if(fleet == "WF") sub.dat <- sub.dat[sub.dat$fleet == 'WF',]
  if(fleet == "FT") sub.dat <- sub.dat[sub.dat$fleet == 'FT',]
  if(fleet == "Overall") sub.dat <- sub.dat[sub.dat$fleet == 'Overall',]
  
  # Now make the plots, first the catch data
  if(plots == "all" || any(plots == "catch"))
  {
    p1 <- ggplot(sub.dat,aes(year,catch,colour = fleet)) +
                  labs(x = "",y = "Catch (tonnes) ") +
                  geom_line(size=1.25) + theme(axis.text.x = element_blank(),panel.spacing.x = unit(10,"lines")) +
                  scale_colour_manual(values = c("blue","black","orange")) +
                  scale_x_continuous(breaks = seq(1900,2100,by=yr.labels)) +
                  theme(legend.position = c(0,0.9),legend.title = element_blank())+
                  guides(colour = guide_legend(override.aes = list(size=1.5)))
     
    if(add.title == T & !is.null(bnk)) p1 <- p1 + ggtitle(paste("Fishery Time Series (",bnk,")",sep=""))
    if(add.title == T & is.null(bnk))  p1 <- p1 + ggtitle("Fishery Time Series")
  # If this is the plot you want than make the plot...
  if(length(plots) == 1 && any(plots == "catch")) p1
  } # end if(plots == "all" || grep("catch",plots))
  
  if(plots == "all" || any(plots == "effort"))
  {
    p2 <- ggplot(sub.dat,aes(year,effort/1000,colour = fleet)) +
                 labs(x = "",y = "Effort (hour-meter) x 1000") +
                 geom_line(size=1.25) + theme(axis.text.x = element_blank()) +
                 scale_colour_manual(values = c("blue","black","orange"))+
                 scale_x_continuous(breaks = seq(1900,2100,by=yr.labels)) +
                 theme(legend.position = "none")
    # If this is the plot you want than make the plot...
    if(length(plots) == 1 && any(plots == "effort")) 
    {
      p2 <- p2 + theme(legend.position = c(0.7,0.9),legend.title = element_blank()) +
                 guides(colour = guide_legend(override.aes = list(size=1.5))) + ggtitle("Fishery Time Series")
      p2
    } # end if(length(plots) == 1 && any(plots == "effort")) 
  } # end if(plots == "all" || grep("catch",plots))
  
  # now make the cpue plot if we want that one.
  if(plots == "all" || any(plots == "cpue"))
  {
    p3 <- ggplot(sub.dat,aes(year,cpue,group = fleet)) +
                  xlab(NULL) +
                  scale_y_continuous("CPUE (kg/h-m)") +
                  scale_x_continuous(breaks = seq(1900,2100,by=yr.labels))
    if(add.CIs ==T) # add in the CI's
    {
       p3 <- p3 + geom_ribbon(aes(ymin = cpue.LCI,ymax=cpue.UCI),fill = alpha("grey",0.3) ) #,fill = alpha("grey",0.5), , colour = alpha("grey",0.3)
    } # end if(add.se==T)

    p3 <- p3 + geom_line(aes(year,cpue,colour = fleet),size=1.25) + scale_colour_manual(values = c("blue","black","orange"))+
                       theme(legend.position = "none")
    # If this is the plot you want than make the plot...
    if(length(plots) == 1 && any(plots == "cpue")) 
    {
      p3 <- p3 + theme(legend.position = c(0,0.9),legend.title = element_blank()) +
        guides(colour = guide_legend(override.aes = list(size=1.5))) + ggtitle("Fishery Time Series")
      p3
    } # end if(length(plots) == 1 && any(plots == "effort")) 
  
  # Now make the plots, if we just want 2 of the plots it's a bit annoying...
  if(length(plots) == 2 && any(plots=="catch") && any(plots =="effort"))  plot_grid(p1, p2, align="hv",nrow=2)
  if(length(plots) == 2 && any(plots=="catch") && any(plots =="cpue"))  plot_grid(p1, p3, align="hv",nrow=2)
  if(length(plots) == 2 && any(plots=="effort") && any(plots =="cpue"))  plot_grid(p2, p3, align="hv",nrow=2)
      
  if(plots == "all") plot_grid(p1, p2, p3, align="hv",nrow=3)
  } #end  if(plots == "all" || any(plots == "cpue"))
} # end function
