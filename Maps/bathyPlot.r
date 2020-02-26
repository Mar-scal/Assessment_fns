################################################################################################################
##### This function will retrieve the bathymetry for the inshore and offshore Scallop fishery using either 
##### the CHS SQL database or our own flat files
# This used to be part of ScallopMap.r, I have made it a stand alone function for ploting the Bathymetry, much
# like has been done with strataPlot
####  Version 1 created by Dave Keith August 6th, 2015
################################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#      1: "get.bathy.r"
################################################################################################################
## Function Arguements
##  1:  db:  Where do we want to get our bathymetry data.  Four options, CHS, topex, usgs, or quick
##  2:  un:  Username for your SQL call, please set this up in your R-profile and do not enter it directly into the function
##  3:  pwd:  Password for your SQL call, please set this up in your R-profile and do not enter it directly into the function 
##  4:  db.con:  SQL database connection name, user specific, default is ptran
##  5:  isobath:  isopleths (contour lines) for the bathymetry, default is 50,100,150 and 200 meters.
##  6:  plot.add: True (default) is used if function is being called by another function with a plot device already opened,  
##      False used if making a unique plot with just land and survey strata shown, to work as a stand-alone function set this to False
##  7:  bounds:  Boundaries for the bathymetry, needed for quick option only
##  8:  b.col:  Colour of the bathymetry isopleths (contour lines)
#   9:  direct:  The working directory.  default = "Y:/Offshore scallop/Assessment/Assessment_fns/"



# rm(list=ls(all=T))
bathyPlot<-function(db = "quick", un = un.ID, pw = pwd.ID, db.con = "ptran",isobath = seq(50,200,50), plot.add=T, 
                    bounds = "inshore", b.col = 'lightblue',direct="Y:/Offshore scallop/Assessment/")
{
  
######################################  Section 1 Set up plot if plot.add=F ##########################################  
  
  
  # Set up the boundaries, this is used by CHS or if plot.add= F
  
  if(bounds =='all')      { ylim=c(40,48);         xlim=c(-68,-54) } 
  if(bounds=='offshore') { ylim=c(40,46);         xlim=c(-68,-55)}  
  if(bounds =='inshore')	{ ylim=c(43.1,45.8);	xlim=c(-67.5,-64.3) }
  
# If we want a standalone plot for the bathymetry start with this
  if(plot.add==F)
  {
    
    #Read1 Add in the land, use the high resolution Maritimes data and make it into a PolySet
    land<-read.table(paste(direct,"Data/Maps/approved/Coastline/nwatlHR.ll",sep=""),header=T)
    attr(land,"projection")<-"LL"
    
    # Plot the land
    plotMap(land,xlim=xlim,ylim=ylim,col="wheat")
    
    # Add a thick box + title to plot
    box(lwd=2)
    
    # Put the right title on plot
    title("Regional Bathymetry")
    
  } # end if(plot.add==F)
  
  ######################################  End Section 1 ##########################################  
  
  
  
  ######################################  Section 2 Bathymetry data is called in here ##########################################  
  
  
# Bathymetry data is called in here. Only the CHS data needs custom get.bathy.r function. This section consists of
# a series of if statements which are triggered depending on the value of the arguement db.

# Do we want to plot bathymetry? New addition to file, DK, July 31, 2015

  
  # if using Canadian Hydrographic survey (CHS) data.
  if(db=='CHS')
    {
      # Source1 CHS needs to processed by local get.bathy.r function this doesn't work with inshore set up!
      source(paste(direct_fns,"Maps/get.bathy.r",sep=""),local=T)
      # Use the "custom" section of get.bathy function.  Sets up the xlim and ylim to be slightly larger than specified
      # in this function, likely to ensure  contour lines converage is complete on this map.  Data extracted via SQL call in get.bathy.r
      bathy.dat<-get.bathy('custom',un=un, pw=pw,db.con= db.con,xl=c(xlim[1]-0.5,xlim[2]+0.5),yl=c(ylim[1]-0.5,ylim[2]+0.5))
      # Convert bathy.dat into a list for contourLines function
      bathy.lst<-makeTopography(bathy.dat)
      # Make object which contains bathymetric contour lines
      bathy.cl<-contourLines(bathy.lst,levels=isobath)
      # Converts the contour lines from bathy.cl into a PolySet that can be used in PBSMapping functions
      bathy.cp <- convCP(bathy.cl)
      # Extract the PolySet and give it a unique name
      bathy.poly <- bathy.cp$PolySet
      # Make sure this is a projection and is Latitude/Longitude.
      attr(bathy.poly,"projection") <- "LL"
      # Add bathy.poly to the plot. I believe the polyProps is getting the PID for the bathy.poly data so the function knows
      # which polygons belong to which PID.
      addLines(bathy.poly,polyProps=data.frame(PID=unique(bathy.poly$PID)),col=b.col)
    } # end if(db=='CHS')
  
  # if using the topex data, I believe original source of this data is http://topex.ucsd.edu/cgi-bin/get_data.cgi
  if(db=='topex')
    {
      # Flip sign of isobath so negative means below sea level I believe (or vice versa)
      isobath=-isobath
      #Read2 bring in the data
      bathy.dat<-read.table(paste(direct,"Data/Maps/approved/Bathymetry/topex/AtlCanbathy.xyz",sep=""),header=F) # DK revised directory August 4 2015
      # Give names to bathy.dat columns
      names(bathy.dat)<-c("X","Y","Z")
      # Adjust data to get longitude into proper format (i.e. -70(ish) rather than 290(ish))
      bathy.dat$X<-bathy.dat$X-360
      # Trim the data into the region we are interested in using xlim/ylim
      bathy.dat<-subset(bathy.dat,X>xlim[1]&X<xlim[2]&Y>ylim[1]&Y<ylim[2])
      # Convert bathy.dat into a list for contourLines function
      bathy.lst<-makeTopography(bathy.dat)
      # Make object which contains bathymetric contour lines
      bathy.cl<-contourLines(bathy.lst,levels=isobath)
      # Converts the contour lines from bathy.cl into a PolySet that can be used in PBSMapping functions
      bathy.cp <- convCP(bathy.cl)
      # Extract the PolySet and give it a unique name
      bathy.poly <- bathy.cp$PolySet
      # Make sure this is a projection and is Latitude/Longitude.
      attr(bathy.poly,"projection") <- "LL"
      # Add bathy.poly to the plot. I believe the polyProps is getting the PID for the bathy.poly data so the function knows
      # which polygons belong to which PID.
      df <- data.frame(PID=unique(bathy.poly$PID)) # DK added July 31, 2015 to remove a silly R thing where it was making df a factor.
      addLines(bathy.poly,polyProps=df,col=b.col) # DK changed July 31, 2015 in conjunction with above.
    } # end if(db=='topex')
  
  # if using USGS data
  if(db=='usgs')
    {
      #Read3 bring in the data
      bathy.poly<-read.table(paste(direct,"Data/Maps/approved/Bathymetry/usgs/bathy15m.csv",sep=""),sep=",",header=T) # DK revised directory August 4 2015
      # Make sure this is a projection and is Latitude/Longitude.
      attr(bathy.poly,"projection") <- "LL"
      # Add bathy.poly to the plot. 
      addLines(bathy.poly,col=b.col)
    } # End if(db=='USGS')
  
  
  # if using local isobath flat files, the source of this data is unclear to DK as of July 2015.
  if(db=='quick')
    {
      
      # If we only specified on color for bathcol make it into a vector the same length as isobath.
      if(length(b.col)==1)b.col<-rep(b.col,length=length(isobath))
     
      # If we are dealing with offshore
      if(bounds=='offshore' || bounds == "all")
        {
        # repeat the code inside the for loop for each isobath specified.
        for(i in 1:length(isobath))
          {
        
            #Read4 Bring in the files
            d.ll<-read.table(paste(direct,"Data/Maps/approved/Bathymetry/quick/offshore/d",isobath[i],".ll",sep=''),header=T)
            # remove any "NA" values
            d.ll<-na.omit(d.ll)
            # Make sure data is a projection and it is Latitude/longitude
            attr(d.ll,"projection") <- "LL"
            # Add the lines to the plot
            addLines(d.ll,col=b.col[i])
          } # end for(i in 1:length(isobath))  
        } # end if(bounds=='offshore')
        
        # if we are dealing with inshore
        if(bounds=='inshore')
         {
          # repeat the code inside the for loop for each isobath specified.
          for(i in 1:length(isobath))
            {
            
            #Read5 Bring in the files
            d.ll<-read.table(paste(direct,"Data/Maps/approved/Bathymetry/quick/inshore/d",isobath[i],".ll",sep=''),header=T)
            # Notice we use lines rather than addLines here, these data are simply lat/long coordinates.  Why didn't we make these 
            #  into a PBS style data frame when creating the dXX.ll files (we did with offshore for example)
           lines(lat~lon,d.ll,col=b.col[i])
            } #end   for(i in 1:length(isobath))
          } # end if(bounds=='inshore'| | bounds == "all")
        
     
    } # end if(db=='quick')
  
  ######################################  End Section 2 ##########################################  
  
  
} # end Function