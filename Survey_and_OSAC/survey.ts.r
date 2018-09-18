################################################################################################################
####  Commented and checked by DK September 2015.  This function is used to create a time series of the
####  abundnace or biomass of the each of the scallop classes (Commerical, Recruits, Pre-recruits)
####
################################################################################################################
#  Change log
#  July 2016:  Added option to change color of the median line for the primary data also forced the Npt =T minimums for biomass
#  and abundance to be 1.2 and 120 respectively if you didn't supply yl2.
#  June 2017:  Added option to plot the user bins as a stand alone figure.  I also did a pretty major overhaul to the
#              script to cut down on repeative code...
#  September 2017:  Minor change to user.SH.bins so that max category was >= not >
#  July 2018: This is an edited version of survey.ts used for assessing restratified banks. It isn't good enough to just run survey.ts 
#   with two variables, because the abundances and SE won't be calculated properly for the second dataset. That's why this function now exists.
#   In 2018, it was used for Sable.
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
#        "pre" = pre-recruits, "rec" = recruits, "com" = commercial, A fourth option of "user" is specified if looking for user specified SH plots, the 
#        "user" option cannot be specified with any of 'pre','rec','com' and you need to provide the "user.bins" below.
#clr:    Color for the data.  Main data is slot 1, if a secondary line added, it is slot 2, the 3rd slot is for the color of the median line
#        for the first data (if a second set of data are provided the median line for that data will be same color as the secondard data line)
#cx:     Plot magnification. Default =1
#pch:    Symbol used for points.  Default = 1:2
#lty:    The line type.  Default = 1:2
#wd:     Width of plot window.  Default = 10
#ht:     Height of plot window. Default = 8
#Npt:    Are the data Numbers (Biomass) per tow or total numbers (Biomass).  Default is Npt = T so numbers per tow.
#se:     Plot the standard errors, this is only correct if Npt = T. (T/F)  Default = F
#ys:     Scaling for maximum of y axis.  Default = 1.2 (I think it looks nicer this way..)
#yl2:    y axis upper limit. when Npt = T specify as Number per tow, or Biomass(kg) per tow  
#        when Npt = F specify as tonnes or total numbers (millions). Default is NULL and calculated from data.
#ymin:   minimum for y axis.  Default = 0
#dat2    A second dataset used in SE calculations.  Default = NULL
#areas   A numeric vector of area sizes.  Default = NULL
#areas2  A numeric vector of area sizes for the second set of data.  Default = NULL
#ypos    Picks the year above which we want to add text to the plots.  Default = 1 which gives nice results generally.
#add.title:  Add a title to the plot?  (T/F) default = F.
#cx.mn:      Magnification of title plot.  Default = 1.
#titl:       The title name.  Default is blank.
#axis.cx:    The axis magnification. Default = 1
#user.bins:  If we want to plot the user defined bins instead of the pre-rec-com time series (it is an either or option)
#            this needs to be set to whatever the user.bins names are within the shf object.  Default = NULL which doesn't create these plots
#            should work with user.bins = survey.obj[[banks[i]]][[1]]$user.bins if this exists.

survey.ts <- function(shf, years=1981:2008, Bank='GBa', type = "N",pdf=F, plots=c('pre','rec','com'),
                              clr=c(1,1,1),cx=1.2,pch=1:2,lty=1:2,wd=10,ht=8,Npt=T,se=F,ys=1.2,yl2=NULL,ymin=0,dat2=NULL,areas=NULL,areas2=NULL,
                              ypos=1, add.title = F, cx.mn = 1, titl = "", axis.cx=1,user.bins = NULL, ...)
{
  
  # Subset the data into the years of interest
  shf<-subset(shf,year %in% years)
  # Get the current years RS and CS for the bank, this is slighly dis-engenious for GB since we actually calculate the biomass for the RS/CS
  # for a given year and this has changed over time on GB, solution isn't straightforward so I leave it how we've always done it for the moment...
  RS <- shf$RS[nrow(shf)]
  CS <- shf$CS[nrow(shf)]
  # Check if any years are missing, if so we'll need to fill them in with NA's so we aren't drawing lines
  # between years with no data... DK added on Nov 23, 2015, if getting a weird error it may be
  # due to this!!
  # FK 2018: need to do this for the dat2 element as well.
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
  
  # If we have dat2...
  if(!is.null(dat2))
  {
  missing.years.dat2 <-  years[!is.element(min(dat2$year):max(dat2$year),dat2$year)]
  if(length(missing.years.dat2 > 0))
  {
    fill.dat2 <- data.frame(matrix(NA,nrow=length(missing.years.dat2),ncol=ncol(dat2)))
    fill.dat2[,1] <-missing.years.dat2
    names(fill.dat2) <- names(dat2)
    # I will give this a new name as this messing up the survey object for the SHF plot...
    dat2<- rbind(dat2,fill.dat2)
    # And now re-order the data and everything will be wonderful!
    dat2 <- dat2[order(dat2$year),]
  } # end if(length(missing.years > 0))
  }
  # If making user SH bin plots I need to get the correct names...
  if(!is.null(user.bins))
  {
    # Get the names for the user bins and good names for the final results...
    bnames <- paste0("bin_lt_",user.bins[1])
    mean.names <- paste0("mean_lt_",user.bins[1])
    CV.names <- paste0("CV_lt_",user.bins[1])
    for(p in 1:length(user.bins)+1) 
    {
      if(p > 1 && p < length(user.bins)+1) 
      {
        bnames[p] <- paste0("bin_",user.bins[p-1],"-",user.bins[p])
        mean.names[p] <- paste0("mean_",user.bins[p-1],"-",user.bins[p])
        CV.names[p] <- paste0("CV_",user.bins[p-1],"-",user.bins[p])
      } # end if(p > 1 && p < length(user.bins)+1) 
      
      if(p == length(user.bins)+1) 
      {
        bnames[p] <- paste0("bin_",user.bins[p-1],"_plus")
        mean.names[p] <- paste0("mean_",user.bins[p-1],"_plus")
        CV.names[p] <- paste0("CV_",user.bins[p-1],"_plus")
      } # end if(p == length(user.bins)+1) 
    } # End for(p in 1:length(user.bins)+1) 
    bnames <- c(bnames,paste0(bnames,"_bm"))
    mean.names <- c(mean.names,paste0(mean.names,"_bm"))
    CV.names <- c(CV.names,paste0(CV.names,"_bm"))
  } # end if(!is.null(user.bins))
  
  # If we aren't using the user bins then we just grab the pre/rec/com data as requested
  if(is.null(user.bins)) 
  {
    nm.tmp <- NA
    if(any(plots %in% "pre")) nm.tmp <- c(nm.tmp,"NPR","IPR")
    if(any(plots %in% "rec")) nm.tmp <- c(nm.tmp,"NR","IR")
    if(any(plots %in% "com")) nm.tmp <- c(nm.tmp,"N","I")
    mean.names <- as.character(na.omit(nm.tmp))
    CV.names <- paste(mean.names,".cv",sep="") 
  } # end if(is.null(user.bins))  
  
  # Set up the pdf plot device + filename
  if(pdf == T & is.null(user.bins)) pdf(paste("figures/",Bank,type,min(years),"-",max(years),".pdf",sep=""), width = wd, height = ht)
  if(pdf == T & !is.null(user.bins)) pdf(paste("figures/",Bank,"User_SH_bins",type,min(years),"-",max(years),".pdf",sep=""), width = wd, height = ht)
  # If looking for abundance do this
  
  # If areas has been specified and Number per tow is True (Npt=T) then we want to scale the data from bank to tow
  if(is.null(areas) == F && Npt == T)
  {
    if(is.null(user.bins)) shf[,mean.names] <- shf[,mean.names] / sum(areas) * 10^6 
    if(!is.null(user.bins)) shf[,mean.names] <- shf[,mean.names] / sum(areas) * 10^6 
  } # end if(!is.null(areas) == T && Npt == T)
  
  # If you have provided a second set of data, but only 1 area we assume the area is the same b/t the datasets.
  if(is.null(areas2) && !is.null(dat2) && Npt==T) areas2 <- areas
  # If there is a dat2 two object and areas2 is specified (i.e. dat2 has different areas than dat1, such as a restratification case!) and Npt is true this is conversion from bank to tow numbers.
  if(!is.null(dat2) && !is.null(areas2) && Npt == T) 
  {
    # scale down from bank to tow
    if(is.null(user.bins)) dat2[,mean.names] <- dat2[,mean.names] / sum(areas2) * 10^6 
    if(!is.null(user.bins)) dat2[,mean.names] <- dat2[,mean.names] / sum(areas2) * 10^6 
  } # if(!is.null(dat2)==T && !is.null(areas)==T && Npt == T) 
  
  # Subset the data to what we will use...
  # If making the abudance plots without the user specified SH bins.
  if(type == "N" && is.null(user.bins)) 
  {
    shf <-  shf[,c(which(names(shf) %in% c("n","year")),grep("N",names(shf)))]
    if(!is.null(dat2))  dat2 <-  dat2[,c(which(names(dat2) %in% c("n","year")),grep("N",names(dat2)))]
  }  # if(type == "N" & is.null(user.bins)) 
  # If making the biomass plots without the user specified SH bins.
  if(type == "B" && is.null(user.bins)) 
  {
    shf <-  shf[,c(which(names(shf) %in% c("n","year")),grep("I",names(shf)))]
    if(!is.null(dat2))  dat2 <-  dat2[,c(which(names(dat2) %in% c("n","year")),grep("I",names(dat2)))]
  }  # if(type == "B" & is.null(user.bins)) 
  
  # If making the abudance plots  but using the user specified SH bins.
  if(type == "N" && !is.null(user.bins)) 
  {
    shf <-  shf[,c(which(names(shf) %in% c("n","year",mean.names,CV.names)))]
    shf <- shf[,-grep("bm",names(shf))]
    if(!is.null(dat2))  
    {
      dat2 <-  dat2[,c(which(names(dat2) %in% c("n","year",mean.names,CV.names)))]
      dat2 <- dat2[,-grep("bm",names(dat2))]
    } # end if(!is.null(dat2))
  }  # end if(type == "N" & is.null(user.bins)) 
  
  ## If making the biomass plots using the user specified SH bins.
  if(type == "B" && !is.null(user.bins)) 
  {
    shf <-  shf[c(which(names(shf) %in% c("n","year")),grep("bm",names(shf)))]
    if(!is.null(dat2))  dat2 <-  dat2[c(which(names(dat2) %in% c("n","year")),grep("bm",names(dat2)))]
  }  # if(type == "B" & is.null(user.bins)) 
  
  # Use these as the names for the mean and CV from here out , this will subset it to the names that we want to plot...
  if(!is.null(user.bins)) mn.tmp <- names(shf)[(which(names(shf) %in% mean.names))]
  if(!is.null(user.bins)) CV.tmp <- names(shf)[(which(names(shf) %in% CV.names))]
  # The reverse puts them in order so that pre plots first if not making the user bin plots...
  if(is.null(user.bins)) mn.tmp <- names(shf)[rev(which(names(shf) %in% mean.names))]
  if(is.null(user.bins)) CV.tmp <- names(shf)[rev(which(names(shf) %in% CV.names))]
  
  
  # Get the names for the plots...
  plot.names <- NULL
  for(i in 1:length(mn.tmp))
  {
    # If we have user bins this is our naming convention
    if(!is.null(user.bins))
    {
      if(i == 1) plot.names[[i]] <- substitute(paste(phantom(x)< a," mm",sep=""),list(a = user.bins[i]))
      if(i > 1 && i < length(mn.tmp)) plot.names[[i]] <- substitute(paste(a ,"-", b, " mm",sep=""),list(a = user.bins[i-1],b=user.bins[i]-1))
      if(i == length(mn.tmp)) plot.names[[i]] <- substitute(paste(phantom(x)>=a, " mm",sep=""),list(a = user.bins[i-1])) #phantom needed to make a leading math symbol...
      
    } # end if(!is.null(user.bins))
    if(is.null(user.bins))
    {
      # If the recruit and fully recruited size aren't specified...
      if(mn.tmp[i] %in% c("NPR","IPR")) plot.names[[i]] <- substitute(paste("Pre-", recruits < a, " mm",sep=""),list(a=RS))
      if(mn.tmp[i] %in% c("NR","IR")) plot.names[[i]] <-  substitute(paste("Recruits ",a,"-",b,"mm"),list(a=RS,b=CS-1))
      if(mn.tmp[i] %in% c("N","I")) plot.names[[i]] <- substitute(paste("Fully ", recruited>=a, "mm"),list(a=CS))
    } # end if(is.null(user.bins))
  } # end for(i in 1:length(mn.tmp))
  #Calculate standard errors based on mean and CV.
  if(se==T)
  {
    se.names <- paste(mn.tmp,".cv",sep="")
    # Calculate the Standard errors 
    for(i in 1:length(se.names)) shf[,se.names[i]] <-shf[,mn.tmp[i]]*shf[,CV.tmp[i]]
    # Do the same if there is dat2 around.
    if(!is.null(dat2)) for(i in 1:length(se.names)) dat2[se.names[i]] <-dat2[,mn.tmp[i]]*dat2[,CV.tmp[i]]
  } #end if(se==T)  
  
  # Set plot y axis maximum if set as NULL
  if(is.null(yl2))
  {
    # if ploting the standard error set the max y as this
    # Add the standard error bars 
    if(se==T)
    {
      ymax <- NA
      # I make it 20% larger b/c it seems to look decent that way...
      for(i in 1:length(se.names)) ymax[i] <- max(shf[,se.names[i]] + shf[,mn.tmp[i]],na.rm=T)
      # Do the same if a second set of data...
      if(!is.null(dat2)) 
      {
        ymax2 <- NA
        # the 1.2 is to give the buffer on the y axis, looks good in general, already applied to ymax
        for(i in 1:length(se.names)) ymax2[i] <- max(dat2[,se.names[i]] + dat2[,mn.tmp[i]],na.rm=T)
        y.tmp <- cbind(ymax,ymax2) 
        ymax <- apply(y.tmp,1,max) # Pick the biggest number and use that as your ymax.
      }# end if(!is.null(dat2)) 
    }  #end if(se==T)
    
    # If not ploting the standard error the max y should be this.
    if(se==F)
    {
      ymax <- NA
      for(i in 1:length(mn.tmp)) ymax[i] <- max(shf[,mn.tmp[i]],na.rm=T)
      # Do the same if a second set of data...
      if(!is.null(dat2)) 
      {
        ymax2 <- NA
        # the 1.2 is to give the buffer on the y axis, looks good in general, already applied to ymax
        for(i in 1:length(mn.tmp)) ymax2[i] <- max(dat2[,mn.tmp[i]],na.rm=T)
        y.tmp <- cbind(ymax,ymax2) 
        ymax <- apply(y.tmp,1,max) # Pick the biggest number and use that as your ymax.
      }# end if(!is.null(dat2)) 
    } # end if(se==F)
    # Here tidying up the y-max for the plots so they look nicer...
    # T
    if(Npt == T)
    {
      if(is.null(user.bins))
      {
        # First rule accounts for cases when we have very high numbers in one category compared to the minimum if so
        # we keep that maximum as is but make the other two equivalent, 0.33 is arbitray but seems to look nice to me...
        if((min(ymax)/max(ymax)) < 0.33) ymax[which(ymax != max(ymax))] <- rep(max(ymax[which(ymax != max(ymax))]),length(which(ymax != max(ymax))))
        # If this ratio is > 25% then let's make all the axis the same...
        if((min(ymax)/max(ymax)) >= 0.33) ymax <- rep(max(ymax),length(mn.tmp))
        # And forcing the minimum y axis for biomass to be a minimum of 50/tow 
        if(max(ymax) < 50) ymax <- rep(50,length(mn.tmp))
      } # end if(is.null(user.bins))
    } # end if(Npt == T )
    
  } # end if(is.null(yl2))
  
  # If y limit is provided make a copy of it for each plot.
  if(!is.null(yl2) && length(yl2) == 1 ) ymax<-rep(yl2,length(mn.tmp))
  if(!is.null(yl2) && length(yl2) > 1 )  ymax<-yl2
  
  
  # Set up the plot.
  par(mfrow = c(length(mn.tmp), 1), mar = c(0, 2, 0, 1), omi = c(1, 1, 0.5, 0.5))
  
  # If we want the Pre-recruit plot this is how it's done.  
  # Set up the plot, no points are added yet
  for(i in 1:length(mn.tmp))
  {
    plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(ymin, ymax[i]*ys), mgp = c(0.5, 0.5, 0), 
         tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=axis.cx,bty="U")
    # Axis, revised to ensure min year is as low as it possibly could be (but will never be plotted) and the max is 
    # the latest year, every 5th year plotted
    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5),lab = F, tcl = -0.6,cex.axis=axis.cx)
    axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),1), lab = F, tcl = -0.3,cex.axis=axis.cx)
    # Left and right y-axes, note that we scale the maximum by ys for some reason.
    y.axis.ticks <- pretty(c(0,ymax[i]))
    
    if(type =="N") axis(2, y.axis.ticks,labels = y.axis.ticks, mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
    if(type =="B") axis(2, y.axis.ticks,labels = y.axis.ticks/1000, mgp = c(0.5, 0.7, 0),las=1,cex.axis=axis.cx)
    axis(4, pretty(c(0,ymax[i]*ys)),lab = F, tcl = -0.3,cex.axis=axis.cx)
    # Now add the median line, but only for the years we have data, allow the lines to have their own unique color.
    if(any(is.na(shf[,mn.tmp[i]]))==T)  lines(shf$year[-which(is.na(shf[,mn.tmp[i]]))],rep(median(shf[,mn.tmp[i]],na.rm=T),
                                                                                           length(shf$year[-which(is.na(shf[,mn.tmp[i]]))])),col=clr[3],lty=2,lwd=2)
    if(any(is.na(shf[,mn.tmp[i]]))==F)  lines(shf$year,rep(median(shf[,mn.tmp[i]],na.rm=T),length(shf$year)),col=clr[3],lty=2,lwd=2)
    # now add the points
    points(shf$year, shf[,mn.tmp[i]] , type = "o", pch = pch[1],cex=cx,col=clr[1])
    
    # If dat2 is provided then we add these points and possibly se (well the se/mean) as well. 
    if(!is.null(dat2))
    {
      points(dat2$year, dat2[,mn.tmp[i]], type = "o", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
      # add the median line, if statement handling cases with/without NA's, if we have a second line we keep the median line the same clr.
      if(any(is.na(dat2[,mn.tmp[i]]))==T)  lines(dat2$year[-which(is.na(dat2[,mn.tmp[i]]))],rep(median(dat2[,mn.tmp[i]],na.rm=T),
                                                                                                length(dat2$year[-which(is.na(dat2[,mn.tmp[i]]))])),col=clr[2],lty=2,lwd=2)
      if(any(is.na(dat2$NPR))==F) lines(dat2$year,rep(median(dat2[,mn.tmp[i]],na.rm=T),length(dat2$year)),col=clr[2],lty=2,lwd=2)
      
      # Add the se
      if(se == T) segments(dat2$year,dat2[,mn.tmp[i]]+dat2[,se.names[i]],dat2$year,dat2[,mn.tmp[i]]-dat2[,se.names[i]],col=clr[2])
    } # end if(!is.null(dat2))
    
    # Add the SE to the main data...
    if(se==T) segments(shf$year,shf[,mn.tmp[i]]+shf[,se.names[i]],shf$year,shf[,mn.tmp[i]]-shf[,se.names[i]],col=clr[1])
    
    # Add ID's to each panel
    text(years[ypos], ymax[i]*ys*0.9, plot.names[[i]], cex=1.5, adj = 0)
    
    # Add a title?
    if(add.title == T) title(titl,cex.main = cx.mn,outer=T)                 
    
    # For the final figure add some more information  
    if(i == length(mn.tmp))
    {
      # Add the axis label to whichever plot comes last and include the labels (years)
      axis(1, at = seq(1955,as.numeric(format(Sys.time(),"%Y")),5), tcl = -0.6,cex.axis=axis.cx)
      # Is data in Numbers per tow or total numbers.bgroup("(",frac(N,tow),")")
      if(Npt==T && type == "N") mtext(side=2,substitute(paste("",frac(N,tow),),list(N="N",tow="tow")), line=2,
                                      outer = T, cex = 1.2,las=1)
      if(Npt==F && type == "N") mtext("Number of scallops (millions)", 2, 3, outer = T, cex = 1.2)	
      # Lots of options to change this from current if we want (if we want brackets bgroup("(",frac(bar(kg),tow),")"))
      if(Npt==T && type == "B")  mtext(side=2,substitute(paste("",frac(kg,tow)),list(kg="kg",tow="tow")), line=1.5,
                                       outer = T, cex = 1.2,las=1)	
      if(Npt==F && type == "B")  mtext("Total Biomass (t)", 2, 3, outer = T, cex = 1.2)	
      # Add the year to the bottom of the final plot
      #mtext("Year", 1, 4, outer = T, cex = 1.2)	
    } # end if(i == length(mn.tmp))  
  } # end for(i in 1:length(mn.tmp))
  # if pdf =T  then shut down the plotting device
  if(pdf==T) dev.off()
  # Return the object to R, not entirely sure why?
  shf
} # end function

