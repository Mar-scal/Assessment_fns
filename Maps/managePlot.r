################################################################################################################
##### This function will retrieve and plot the management boundaries for the inshore and/or offshore fisheries
# This used to be part of ScallopMap.r, I have made it a stand alone function for ploting the Management boundaries as well, much
# like has been done with strataPlot and bathyPlot
####  Version 1 created by Dave Keith August 6th, 2015
####  Minor edit to exclude plotting of "sub-areas" found in the Offshore csv...
################################################################################################################
################################################################################################################
## Function Arguements
##  1: bounds:  Are we looking at 'offshore', 'inshore', or 'all' boundaries
##  2: plot.add:  Add the boundaries to a plot already active, defaults to True, False will make a stand alone plot
##  3: area.labels:  Add labels (T/F), defaults to False
##  4:  offshore.names:  Add the common names of the offshore areas to the plot (T/F), defaults to F 
##  5:  add.EEZ:  Draw the the EEZ boundaries drawn (T/F), defaults to F
##  6:  add.color:  Add color to the shelf polygons (T/F), defaults to F
##  7:  manage.colors:  Choose colors for the polygons, defaults to pastel.colors(n=64, seed=2 from RPMG package
#   8:  direct:  The working directory.  default = "Y:/Offshore scallop/Assessment/Assessment_fns/"
# rm(list=ls(all=T))
managePlot<-function(bounds = "inshore",plot.add=T,area.labels=F,offshore.names=F,plot.EEZ=F,plot.color=F,
                     manage.colors = pastel.colors(n=60,seed=2),manage.cex = 0.7, 
                     direct = "Y:/Offshore scallop/Assessment/")
{
  # Load the necesasry librarys
  require(PBSmapping)|| stop("Install PBSmapping Package")
  require(RPMG)|| stop("Install RPMG Package")
  
  
######################## Section 1 - Stand alone plot settings, only used if plot.add=F  ########################
  
  # If we want a standalone plot for the bathymetry start with this
  if(plot.add==F)
  {
    
    # Set up the boundaries for the plot depending on whether dealing with inshore or offshore
    if(bounds == "all")
    {
      ylim=c(40,48)         
      xlim=c(-68,-54)
    } # end if(bounds == "all")
    if(bounds == "offshore") 
      {
      ylim=c(40,46) 
      xlim=c(-68,-55)
      } # end if(bounds == "offshore") 
    if(bounds == "inshore") 
      {
      ylim=c(43.1,45.8) 
      xlim=c(-67.5,-64.3)
      } # end if(bounds == "inshore") 
    
    #Read1 Add in the land, use the high resolution NWAtlantic data and make it into a PolySet
    land<-read.table(paste(direct,"Data/Maps/approved/Coastline/nwatlHR.ll",sep=""),header=T)
    attr(land,"projection")<-"LL"
    
    # Plot the land
    plotMap(land,xlim=xlim,ylim=ylim,type="n")
    
    # Add a thick box + title to plot
    box(lwd=2)
    
    # Put the right title on plot
    
    if(bounds == "offshore") title("Offshore Management Boundaries")
    if(bounds == "inshore")  title("Inshore Management Boundaries")
    if(bounds == "all")      title("Regional Management Boundaries")
  } # end if(plot.add==F)  

  
  ######################## End Section 1 #################################### ########################
  
    
######################## Section 2 - Add boundaries and labels as called in the function  ########################
  
    # if we are looking at offshore or everywhere.
    if(bounds=='offshore' || bounds == "all")
    {
      
      #Read2 Bring in the offshore boundries
      offshore<-read.table(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""),sep=",",header=T) 
      attr(offshore,"projection") <- "LL" # DK added August 4 2015
      # Draw the offshore polygons as lines (Ass of August 4 2015 these aren't polygons or even close to it)
      # I need to remove any of the new "sub-areas" so the plot doesn't get messy...
      offshore <- offshore[offshore$subarea == "N",]
      if(plot.color == F) addLines(offshore) # Added DK July 31, 2015
      if(plot.color == T) addPolys(offshore,col=manage.colors) # Added DK July 31, 2015
      
        
      # If we want to add the offshore labels. DK August 2015
      if(area.labels==T)
        {
        # Find the centre of each SFA
        centres <- calcCentroid(offshore,rollup=1)
        # Grab the corresponding label ID's
        labs<- offshore[c("PID","label")][!duplicated(offshore[c("PID","label")]),]
        # Put them together
        offshore.labels <- merge(centres,labs)
        attr(offshore.labels,"projection") <- "LL"
        
        # Add the labels to the plot if making a stand alone plot
        # plot if making a stand alone plot
        if(plot.add==F)  addLabels(offshore.labels,cex=manage.cex)
        #if(plot.add == T) 
        } # end if(labels==T)
      
      # Add the common names of the offshore banks to the plot.
      if(offshore.names == T)
        {
          #Read3 
          common.names <- read.table(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/offshore_Labels.csv",sep=""),
                                     sep=",",header=T)
          names(common.names)
          # change names and attributes so 
          colnames(common.names) <- c("PID","X","Y","label")
          attr(common.names,"projection") <- "LL"
          # Define and create PBS objects to draw arrows to for Georges A and B banks
          g.arrows <- rbind(c(2,1,-65,41.2),c(2,2,-65.9,41.4),c(1,1,-65,41.7),c(1,2,-65.9,41.9))
          colnames(g.arrows) <- c("PID","POS","X","Y")
          attr(g.arrows,"projection") <- "LL"
          
          # plot if making a stand alone plote
        if(plot.add==F)
          {
          # Add the labels to the plot
          addLabels(common.names,cex=manage.cex)
          addLines(g.arrows,arrows=T,angle=20,length=0.05)
          } # end if(plot.add==F)
          
        # If not true return them to the main function
        #if(plot.add== T) return(offshore.names)
          
        } # end if(offshore.names == T)
        
      
    } # end if(boundries=='offshore')
    
    # if we are looking at the inshore or everywhere
    if(bounds=='inshore' || bounds == "all")
    {  
      
      
      #Read4 - Bring in up to date SPA 1 through 6
      inshore<-read.table(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Inshore.csv",sep=""),sep=",",header=T) 
      attr(inshore,"projection") <- "LL" # DK added August 4
      #Read5- Bring in the mid bay and upper bay lines.
      mb.ub.lines <- read.table(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Mid_Bay_and_Upper_Bay_Lines.csv",sep=""),
                                header=T,sep=",") # Added August 4 DK
      attr(mb.ub.lines,"projection") <- "LL" # Added August 4 DK
      
     
      # Add the areas, I have changed this to addPOlys so we can plot colors
      if(plot.color == F) addLines(inshore) # Added DK July 31, 2015
      if(plot.color == T) addPolys(inshore,col=manage.colors) # Added DK July 31, 2015    
      # Add in the Mid bay and upper bay lines. DK August 4, 2015
      # Might be nice to give these a different color or something so they stand out better
      addLines(mb.ub.lines,lwd=2,cex=manage.cex) 

      # If we want to add the inshore labels. DK August 2015
      if(area.labels==T)
      {
        # The centroid method doesn't work well for inshore so
        # we have to do this part the old fashioned way...
        #Read6
        inshore.labels <- read.table(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/inshore_labels.csv",sep=""),sep =",",
                                     header=T)
        # Put them together
        attr(inshore.labels,"projection") <- "LL"
        # Add the labels if making a self contained plot, custom job needed to squeeze in SPA5
        if(plot.add== F)
          {
          # Note that SPA5 is only plotted when bounds = inshore due to how small an area it consists of.
          addLabels(inshore.labels[inshore.labels$label != "SPA5" & inshore.labels$label != "SPA4",],cex=manage.cex)
          addLabels(inshore.labels[inshore.labels$label == "SPA4",],srt=40,cex = manage.cex)
          } # end if(plot.add== F)
                #addLabels(SFA.labels,cex=0.5)
        
        #if(plot.add== T) return(inshore.labels)
        
        # This plots detailed labels for the inshore that is nothing but clutter when looking at the whole picture.
        if(bounds == "inshore")
          {
          #Read7
          det.labels <- read.table(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/inshore_detailed_labels.csv",sep=""),
                                   sep =",",header=T)
          #labs.SFA<- SFA.Areas[c("PID","label")][!duplicated(SFA.Areas[c("PID","label")]),]
          # Put them together
          attr(det.labels,"projection") <- "LL"
          
          
          #SFA.labels <- merge(centres.SFA,labs.SFA)
          #attr(SFA.labels,"projection") <- "LL"
          # Add the labels to the plot
          if(plot.add== F)
            {
            addLabels(det.labels[det.labels$label != "St. Marys Bay" & det.labels$label != "Upper Bay Line" & 
                                   det.labels$label != "SFA26",],cex=(0.75*manage.cex))
            # Rotate a few of these to look better.
            addLabels(det.labels[det.labels$label == "St. Marys Bay",],srt=60,cex=(0.75*manage.cex))
            addLabels(det.labels[det.labels$label == "Upper Bay Line",],srt=95,cex=(0.75*manage.cex))
            addLabels(det.labels[det.labels$label == "SFA26",],cex=manage.cex)
            # Note we only plot SPA5 when plotting inshore, doesn't show up when plotting all.
            addLabels(inshore.labels[inshore.labels$label == "SPA5",],srt=40,cex=0.75*(manage.cex))
            # 6D doesn't fit in the tiny box so moved outside and draw arrow in
            arrows.6 <- rbind(c(1,1,-66.57,44.72),c(1,2,-66.69,44.665))
            colnames(arrows.6) <- c("PID","POS","X","Y")
            attr(arrows.6,"projection") <- "LL"
            addLines(arrows.6,arrows=T,angle=20,length=0.05)
            } # End if(plot.add== F)
          
         # if(plot.add== T) return(det.labels)
          
          } # end if(bounds == "inshore")
        
      } # end if(labels==T)

      
      
    } # end if(boundries=='inshore'){
###########################  End Section 2  ################################################################ 

    
  
###########################  Section 3 - Final Data layer additions and return (assign) several variables for 
###########################use in other functions ########    
    # Now actually plot the land if making a standalone boundary plot.
    if(plot.add==F) 
    {
      # Final option, do we want to print the EEZ in this plot?
      if(plot.EEZ == T)
        {
        #Read8# Changed from EEZ to current file DK August 5, 2015
        EEZ<-read.csv(paste(direct,"Data/Maps/approved/Other_Borders/AtlanticCdnBoundary_subset.csv",sep="")) 
        attr(EEZ,"projection") <- "LL" # DK added July 31, 2015 to remove the last warning from the function call.
        addLines(EEZ,lty=4,lwd=2)
        
        } # End if(plot.EEZ == T)
      
      addPolys(land,col="wheat")
    } # end if(plot.add==F) 
  
  
  
  
# Now here is what we want to return from this function for use in other functions (particularly ScallopMap), 
# this gets overly complicated...
  if(plot.add==T)
    {
    if(offshore.names== T) 
      {
        assign("common.names", common.names,pos=1)    
        assign("g.arrows", g.arrows,pos=1)    
      } # end if(offshore.names== T)   
    if(area.labels==T)
      {
      # First pick the offshores if appropriate
      if(bounds=='offshore' || bounds == "all")
        {
        assign("offshore.labels", offshore.labels,pos=1) 
        # If we want the common names plotted return this...
       
         } # end if(bounds=='offshore' || bounds == "all")
      
       # Now bring in the inshore labels
      if(bounds=='inshore' || bounds == "all")
        {
          assign("inshore.labels", inshore.labels,pos=1) 
         if(bounds=='inshore') assign("det.labels", det.labels,pos=1) 
        } # End if(bounds=='inshore' || bounds == "all")
      } #end if(area.labels==T)
    } # end if(plot.add==T && area.labels == T)
  
# End Section 3  
  
    
} # end Function