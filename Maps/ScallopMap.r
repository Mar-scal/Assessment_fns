# DK July 29, 2015:  Please Note that this is the Inshore version of ScallopMap, the offshore version is subtly different 
####  Commented and checked by DK 
### August 2017:  DK update to allow for the addition of a scale to the map + the use of the projection option in plotMap call.  Also added
###               a return call at the end to return the x and y limits used in the plot (helpful for adding on text after plotting)
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
# 
#      1: "get.bathy.r"
#      2: "strataPlot.r
#      3: "bathyPlot.r"
#      4: "managePlot.r"
#      5: "convert.dd.dddd.r"
##
###############################################################################################################


### MAPS SCALLOP FISHING AREAS IN R!

# ARGUMENTS
# area: What area to you want to print, default is 'custom' where xlim & ylim are specified or select from area list below
# plot.strata:  Plot the strata? (T/F) default is False
# plot.bathy:   Plot the Bathymetry? (T/F) default is False  
# plot.boundries:  Plot the management area boudaries? (T/F) default is False
# direct:     the directory to choose.  Default is "Y:/Offshore/Assessment/Assessment_fns/"
#             If running locally copy the "Maps" folder, and the "data/maps" folder to a common location then change direct
#             to that location.  For example, if you copy these 2 folders to d:/maps, set direct = d:/maps and this should work 


############  Begin SQL credentials  ##############
# These are needed if you a plotting Strata or if using the CHS option for the Bathymetry, not required otherwise
# NB!!: If you do not have objects called un.ID and pwd.ID loaded in your R workspace this will trip and cause an annoying 
#      error even if you are not using this option.  
#      I suggest you just add these to your .rprofile (located in your default working directory)
###
# un:  your SQL username.  default = "un.ID" (if set in your r.Profile this will run automatically)
#	pw:  Your SQL password.  default is "pwd.ID"  (if set in your r.Profile this will run automatically)
# db.con:  Database to connect to.  Default is  "ptran"
############  End SQL credentials  ##############


####### If plot.strata = T these are your options ################
# strata:  What Strata to plot.  Options are "inshore", "offshore", and "all", default is all
# strata.colors:  The color palette for the strata.  default = pastel.colors(64,seed=2)
# strata.cex:  Size of the strata labels, strata labels only printed if "strata = inshore".  Default is 0.5
########  End Strata Arguments ################################

######## If plot.bathymetry = T these are your options
# isobath:  The isobaths to plot.  Default is 50,100,150,200.  NB "bathy.source=quick"  will only work with 
#             combinations of these 4 isobaths
# bathy.source:  Bathymetry sources, options are "CHS", "usgs", "quick", "topex". Default is "quick".  
#                NB:  To use CHS you need to have access to SQL mflib database, see documenation for get.bathy.r function
# bathcol:  Color of the bathymetry lines.  Default is "blue".
########  End Bathymetry Arguments ################################

####### If plot.boundries = T these are your options ################
# boundries:  Which management boundaries are to be plotted?  Options are 'inshore', "offshore", "all".  Default is "all".  
#             NB:  selecting "inshore" will plot high detailed inshore boundaries.
# label.boundries:  Plot the Management Boundary names?  (T/F) default is F 
# offshore.names:  Plot the common names for the offshore management areas? (T/F) default is F         
# bound.color:  Plot the management boundaries in color?  (T/F) default is F
# manage.colors: Colors for management boundaries if "bound.color=T".  Default is pastel.colors(n=64,seed=2)
# manage.cex: Size of the text if "label.boundries = T".  Default is 0.7     
########  End Management Boundary Arguments ################################

####  Additional Overlays which can be added #############################
# points.lst = points to overlay on map in PBSmapping format - list with 2 elements: 1st element is eventSet (EID, POS, X, Y), 
#             2nd element is eventData (EID, pch, col, etc.) 
# lines.lst = lines to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 
#             2nd element is polyData (PID, SID, lty, col, etc.) 
# poly.lst = polygons to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 
#            2nd element is polyData (PID, SID, border, col, etc.) 
# contours = plots overlaping polygons as contours (same format as poly.lst)
# image.lst = image to overlay on map - list with 3 elements (x, y, z), 'bathymetry' produces image from bathymetry data 
# color.fun = color function for image
# zlim: zlim for image
# poly.border = If adding a polygon to the plot and you want a border, specify the color here.  Default = NULL (no border)
####   End Additional Overlays options #############################

####  Land and Other offshore Elements #############################
# shore = shoreline detail ('marHR' = very fine martimes only, 'nwatlHR' = fine NW Atlantic, 'nwatlMR' = medium NW Atlantic, 
#         'worldLR' = coarse world)
# plot.land:  Plot the land.  (T/F) default is T
# land.col:  Color of the land.  default is 'wheat', 
# lol:   Land outline (T/F) F = default (outline is in same color as the bathymetry)
# stippling:  Plot stippling (dots) on the land.  (T/F) default is F  
# nafo:  If plotting NAFO regions which ones do you plot.  Default is NULL, there are 42 regions to choose from
# nafo.bord: Plot the NAFO borders. (T/F) default is F
# nafo.lab:  Plot the NAFO area names. (T/F) default is F
# banks:  Plot the outline of the offshore banks.  (T/F) default is F
# plot.EEZ:  Plot the Exclusive Economic Zone.  (T/F) default is T
# grid:  Add a grid to the plot.  Default is NULL (no grid), grid size is in degrees.   

####### Plotting Parameters
# title:          Plot title.  Default is empty
# cex:            Magnification for plot text/symbols.  Default is 1
# xlab:           Label for x axis.  Default is empty and uses PlotMap's default 
# ylab:           Label for y axis.  Default is empty and uses PlotMap's default 
# output.pdf:     Save a copy of the plot as a pdf file.  (T/F) default is F     
# output.png:     Save a copy of the plot as a png image. (T/F) default is F  NB:  if this and "output.pdf" 
#                 are both TRUE only a pdf file is produced.                                    
# plot.directory: Directory of saved plot.  Default is current working directory.
# filename:       Filename of saved plot. Default is "testplot"
# width:          Width (inches) of the plot image to be saved.  Default is 8
# height:         Height (inches) of the plot image to be saved.  Default is 8        
# dec.deg:        Do you want the plot labels in decimal degrees or degree minute seconds.  (T/F) Default = T which plots decimal degrees
# proj:           What projection is the data in (if it isn't specified in the data).  Default = T which checks the data for a projection.
#                 Can also be "LL" (typical of Scallop Unit data), "UTM", or a numerical value
# add.scale:      Do you want to add a scale to the figure.  T/F, default = F.   
# Name + defining arguements (and their defaults) for the ScallopMap function
# language:       default is english ("en"), french is "fr"
ScallopMap<-function(area='custom',
                     ylim=c(40,46),xlim=c(-68,-55), 
                     plot.strata=F,  plot.bathy=F, plot.boundries=F,  # plot strata, bathymetry, or management boundaries?
                     direct, direct_fns,  # The default working directory, note that
                     # this is set a level above the "Maps" subfolder.
                    
                     # Strata Options
                     strata = "all",  un = un.ID, pw = pwd.ID, db.con = "ptran",  # strataPlot Options (if plot.strata = T)
                     strata.colors = pastel.colors(64,seed=2), strata.cex=0.5,    # strataPlot Options (if plot.strata = T)
                     # Note that to use the strataPlot function you need a username/password/connection to the SQL database
                     # the defaults for un/pw are dummy variables (they don't work) so if you have no connection ensure plot.strata=F
                     
                     # Bathymetry Options
                     isobath=seq(50,200,50),bathy.source="quick",            # bathyPlot function options (if plot.bathy=T)
                     bathcol='lightblue',                                    # bathyPlot function options (if plot.bathy=T)
                     
                     # Management Boundary Options
                     boundries='all', label.boundries = F, offshore.names=F,         # managePlot options (if plot.boundries = T)
                     bound.color=F,  manage.colors = pastel.colors(n=60,seed=2),     # managePlot options (if plot.boundries = T)
                     manage.cex = 0.7,                                               # managePlot options (if plot.boundries = T)
                     
                     # Additional Overlay options
                     points.lst=NULL,lines.lst=NULL,poly.lst=NULL, contours=NULL,        # Additional overlays onto map
                     image.lst=NULL, color.fun=tim.colors, zlim,                       # Image overlay + color options 
                     poly.border = NULL,                                                # Add a border to the poly.lst
                     # Options for plotting the land details
                     shore='marHR', plot.land=T,land.col='wheat', lol=F, stippling=F,    # land details
                     
                     # Options for plotting NAFO boundaries
                     nafo=NULL,nafo.bord=T,nafo.lab=T,                                   # NAFO details
                     
                     # Options for plotting the banks, EEZ, and overlaying a grid
                     banks=F, plot.EEZ=T, grid=NULL,                                     # EEZ, Banks, or grid options
                     
                     # Title and other plot specific options
                     title='',cex=1,xlab,ylab,                                           # Change title, magnification, x/y labels
                     
                     # If you want to save the plot you can specify where and the plot name + size of the plot
                     output.pdf = F, output.png  = F,                               # Create save a plot as either  a pdf or a jpg.
                     plot.directory = getwd(), filename = "testplot" ,              # Filename and directory of your plot
                     width = 8, height = 8, cex.mn =1,               # WIdth and height of plot, in inches + title magnification
                     dec.deg = T  ,                              # The axes labels, decimal degrees or degree minute seconds
                     proj = T,add.scale = F, language="en",...                  # The projection to use and do you want to add a scale...
                     ) 

                     # These arguments were removed from the function 1: plot.lines=T, 2: land.twice = T, color.adj = 100
{

  # Load these librarys, don't always need them but clarifies required functions for full functionality
  # if not installed stop and produce error message
  require(PBSmapping)|| stop("Install PBSmapping Package")
	require(fields)|| stop("Install fields Package")
  require(RPMG) || stop("Install RPMG Package")
  require(RODBC) || stop("Install RODBC Package")
  #require(GISTools) ||  stop("Install GISTools Package")

  # Load required packages and local functions.
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Fishery/fishsum.plt.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Fishery/fishery.dat.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/gridPlot.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/ScallopMap.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/strataPlot.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/managePlot.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/bathyPlot.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
    for(fun in funs) 
    {
      download.file(fun,destfile = basename(fun))
      source(paste0(getwd(),"/",basename(fun)))
      file.remove(paste0(getwd(),"/",basename(fun)))
    } # end for(fun in funs)
  }# end if(missing(direct_fns))
  
  
  if(!missing(direct_fns))
  {
    source(paste(direct_fns,"/Fishery/fishsum.plt.r",sep="")) #Source1
    source(paste(direct_fns,"/Survey_and_OSAC/gridPlot.r",sep="")) #Source2
    source(paste(direct_fns,"/Fishery/fishery.dat.r",sep="")) #Source3 
    source(paste(direct_fns,"/Maps/ScallopMap.r",sep="")) #Source3 
    # Load the 4 custom functions
    source(paste(direct_fns,"/Maps/strataPlot.r",sep="")) #Source1 add the strata from the SQL database
    source(paste(direct_fns,"/Maps/managePlot.r",sep="")) #Source2 add the managment details
    source(paste(direct_fns,"/Maps/bathyPlot.r",sep="")) #Source3 add the bathymetry
    source(paste(direct_fns,"/Survey_and_OSAC/convert.dd.dddd.r",sep="")) #Source4 convert from decimal degrees to degree-minute-seconds.
  } # end if(!missing(direct_fns))
  
######################  Section 1 - This relates to the "area"  and  "shore" arguements in the function call + 
#######################    Opens the plotting device ###################################  
	

  # Custom area used if you want to create your own ylim and xlim for the plot created below
	  if(area=='custom')	{ ylim=ylim; 			xlim=xlim			}
	
	## These are the predfined lists used to define ylim and xlim for the plot created below
	
  #offshore
  if(area =='NL')      { ylim=c(40,48);         xlim=c(-68,-54) } # DK added August 7, 2015, option to include Southern NewFoundland
  if(area=='offshore') { ylim=c(40,46);         xlim=c(-68,-55)}
	if(area=='SS')		   { ylim=c(40.5,47); 		  xlim=c(-68,-57)		}
  if(area=='WOB')		   { ylim=c(40.5,44); 		  xlim=c(-68,-64)		} # Western offshore Banks, DK added
  if(area=='ESS')		   { ylim=c(43,45.4); 	    xlim=c(-62.5,-57.4)	}
	if(area=='WSS')		   { ylim=c(41,44); 		    xlim=c(-67,-64)		}
	if(area=='BBn')		   { ylim=c(42.4,43); 		  xlim=c(-66.6,-65.6)	}
	if(area=='BBs')		   { ylim=c(42.25,42.75); 	xlim=c(-66,-65.25)	}
  #Have a weird location way up in the corner for BBs in 2023, so changing up the ylim
  # if(area=='BBs')		   { ylim=c(42.25,43.35); 	xlim=c(-66.4,-65.25)	}
	if(area=='BB')		   { ylim=c(42.25,43);      xlim=c(-66.5,-65.25)	}
	if(area=='GB')		   { ylim=c(41.1,42.3); 	  xlim=c(-67.3,-65.6)	}
  if(area=='GBa')      {ylim=c(41.2,42.3);      xlim=c(-67.2,-65.85)}
	if(area=='GBb')		   { ylim=c(41.6,42.3); 	  xlim=c(-66.7,-65.6)	}
	if(area=='Ger')		   { ylim=c(42.8,43.825); 	  xlim=c(-67.1,-65.5)		}
	if(area=='Sab')		   { ylim=c(43,44.35); 	    xlim=c(-62.5,-60.5)	}
	if(area=='SPB')		   { ylim=c(44.5,47.5);	    xlim=c(-58,-55)		}
  if(area=='SPB-banks'){ ylim=c(45.25,46.25);	  xlim=c(-57.25,-55.5)		}
	# These are from offshore ScallopMap.r and I believe we'll need them
  if(area=='West')	   {  ylim=c(43,44.1); 		xlim=c(-62.2,-60.4)	}
  if(area=='Mid')		   { ylim=c(44.2,44.9);	xlim=c(-61.3,-60.1) }
  if(area=='Ban')		   { ylim=c(43.9,44.9); 	xlim=c(-60.25,-58.5)	}
  if(area=='Sab-West') { ylim=c(42.8,44.5); 		xlim=c(-62.5,-58.8)	}
  if(area=='Ban-Wide') { ylim=c(43.7,45.2); 	xlim=c(-60.5,-57)	}
  
  
	#inshore
	if(area=='sfa29')	{ ylim=c(43.1,43.8);	xlim=c(-66.5,-65.45) }
	if(area=='gm')		{ ylim=c(44.4,45.2);	xlim=c(-67.2,-66.3) }
	if(area=='inshore')	{ ylim=c(43.1,45.8);	xlim=c(-67.5,-64.3) }
	if(area=='bof')		{ ylim=c(44.25,45.8);	xlim=c(-66.5,-64.3) }
	if(area=='upper')	{ ylim=c(45,46);		xlim=c(-65.2,-64.3) }
	if(area=='mid')		{ ylim=c(44.3,45.5);	xlim=c(-66.6,-64.7) }
	if(area=='spa3')	{ ylim=c(43.62,44.6);	xlim=c(-66.82,-65.8) }
	if(area=='spa4')	{ ylim=c(44.48,44.96);	xlim=c(-66.2,-65.51) }
	if(area=='spa1')	{ ylim=c(44.5,45.8);	xlim=c(-66.4,-64.3) }
	if(area=='spa6')	{ ylim=c(44.3,45.25);	xlim=c(-67.4,-65.9) }
  if(area=='spa1A')	{ ylim=c(44.5,45.3);	xlim=c(-66.4,-64.8) }
  if(area=='spa1B')	{ ylim=c(44.8,45.7);	xlim=c(-66.2,-64.3) }
  if(area=='spa5')	{ ylim=c(44.56,44.78);	xlim=c(-65.82,-65.51) }
  

  # This creates an object that will be used to draw in the shoreline, 
  # each file is at a different scale of resolution/detail, see function arguements for details
  #Read1
  if(shore=='marHR')land<-read.table(paste(direct,"Data/Maps/approved/Coastline/martimesHIGH.ll",sep=""),header=T)
  #Read2
  if(shore=='nwatlHR')land<-read.table(paste(direct,"Data/Maps/approved/Coastline/nwatlHR.ll",sep=""),header=T) 
  #Read3
  if(shore=='nwatlMR')land<-read.table(paste(direct,"Data/Maps/approved/Coastline/nwatlMR.ll",sep=""),header=T) 
  #Read4
  if(shore=='worldLR')land<-read.table(paste(direct,"Data/Maps/approved/Coastline/worldLR.ll",sep=""),header=T) 
	
	# This sets the attribute for 'land' as a projection longitude/latitude (this is what the 'LL' is doing)
	# Options include "UTM" and possibly others
  attr(land,"projection")<-"LL"

 # Now open the plotting device and make the plot call
  # if we are automatically saving a png we do this...
  if(output.png == T && output.pdf == F) png(file= paste(plot.directory,"/",filename,".png",sep=""),
                                             width=width,height=height,units="in",res=1000)
  # if we are automatically saving a pdf we do this...
  if(output.pdf == T && output.png == F) pdf(file= paste(plot.directory,"/",filename,".pdf",sep=""),width=width,height=height)
  # if the call is ambiguous make it a pdf and print a warning
  if(output.pdf == T && output.png == T) 
  {  
    message("Both output.pdf and output.png = TRUE, only the pdf will be produced")
    pdf(file= paste(directory,"/",filename,".pdf",sep=""),width=width,height=height)
  } #end if(output.pdf == T && output.png == T) 
  
  
  #  Function arguements land.col, xlab, ylab, cex, lol, bathcol, are used in this section
  	
  #  If ylab & xlab are specified in the function call use these for xlab/ylab from 'plotMap' function (PSBmapping)	
  #  The labels for each will be based on the attribute given with land.
  #  DK July 31, 2015, I have supressed the ploting of the land and have made it an option in the function call
  # Open the plot window.
  par(...)
  plotMap(land,xlim=xlim,ylim=ylim,cex=cex,xlab="",ylab="",col=land.col,type="n",xaxt="n",yaxt="n",projection = T)
	
	

  #################################          End Section 1         ##################################################
  	
  	
  #################################          Section 2        ##################################################
  
  #  If banks == true add them to plot
	if(banks)
	  {
		#Read6 Bring in the banks data
    banks.xy<-read.table(paste(direct,"Data/Maps/approved/Other_Borders/banks.xy",sep=""),header=T) 
    #Read7 This contains labels for the banks + the color to be used for border when ploting
    banks.dat<-read.table(paste(direct,"Data/Maps/approved/Other_Borders/banks.dat",sep=""),header=T,stringsAsFactors = F) 
    attr(banks.xy,"projection")<-"LL" # DK added July 31, 2015
     
    # add it to our plot.
    addPolys(banks.xy,polyProps=banks.dat)
	  } # end if(banks){
	
	  ##  DK added August 6, 2015, option to add in the strata if we want it.  Looks better if loaded before the land is plotted
  
  if(plot.strata==T & plot.boundries==F)
  {
    #Source1 source(paste(direct,"Maps/strataPlot.r",sep=""))
    strataPlot(loc = strata, plot.add=T, un = un, pw=pw, db.con = db.con,strata.colors=strata.colors,strata.cex = strata.cex,
               direct = direct, direct_fns = direct_fns)
  } # end if(plot.strata==T)
  
  # Now we can bring in the bathymetry
  
 
  ###############################  Section 2 Layering the Plot       ##################################################  		
  
  # Image to overlay on map.  Defaults to null, user to specify x,y,z co-ordinates 
  # Supposedly  using "bathymetry"  to specify an image overlay will give an image from bathymetry, but there is no code for that!
  # If image.1st has been called in the function (it is not NULL) then do everything in curly brackets, if not skip this step.
  # DK July 31, 2015, there is potential I will need to move this loop towards the end of the function.
  # I need an example to figure out what it is exactly doing, my gut feeling is it is o.k. here but not tested.
  if(!is.null(image.lst))
  {
    # Pause function and allow custom r script to be read in, not sure why we would need this, remains untested by DK (July 31, 2015)
    browser()
    # check if we are missing zlim, if so take it from the max-min of image.1st$z
    if(missing(zlim))zlim<-range(image.lst$z,na.rm=T)
    # Add the image to the plotMap
    image(image.lst,add=T,col=color.fun(2*(zlim[2]-zlim[1])),zlim=zlim)
  } # end if loop for !is.null(image.lst) statement
  
  
  
  # if area is SPB we plot this special polygons, plots a hashed out area at the top of SPB, ah ha, St. Pierre Bank this is.
  if(area=='SPB')polygon(c(-58,-55.98333,-55.98333333,-55.5,-54.9,-58),c(46.20028,46.20028,46.95,47,47.5,47.5),col=rgb(1,0,0,0.4),
                         density=20)
  
  # If contours has been specified in the function call then do everything in the curly bracket, if NULL skip this step
  # DK July 31, 2015 Again I need to test this as I don't have an example of these contours.
  if(!is.null(contours))
  {
    
    # Subsets and replaces the second list in contours by the position of matches with PID (position ID) in first contour, 
    # I believe this is just created to index the for loop below and nothing more.
    contours[[2]]<-subset(contours[[2]],PID%in%contours[[1]]$PID)
    # create a new dataframe junk, this related to the 1st PID, I believe this is a polygon (square in this case) but since I 
    # don't know the areas very well yet unsure of what it is encompasing.
    junk<-data.frame(PID=1,POS=1:4,X=c(-62,-61,-61,-62),Y=c(41,41,40,40))
    # a simple for loop through each contour ID created above, clearly this is where we are adding the contour lines to our plot.
    for(i in unique(contours[[2]]$PID))
    {
      addPolys(joinPolys(subset(contours[[1]],PID==i),junk,operation="DIFF"),polyProps=contours[[2]])
    } # end for loop
  } # end if(!is.null(contours))
  
  # If poly.1st has been specified in the function call then add the poly.1st data to the plot, if NULL skip this step
  # Note it needs to be a list with 2 elements (1st the polySet, second is the polyData)
  # DK July 31, 2015 Again I need to test this as I don't have an example of these contours.
  if(!is.null(poly.lst))
  {
    addPolys(poly.lst[[1]],polyProps=poly.lst[[2]],border=poly.border)
  } # end if(!is.null(poly.lst))

  # if nafo is not NULL then do the below
  if(!is.null(nafo))
  {
    
    #Read5 Bring in nafo boundries
    nafo.xy<-read.csv(paste(direct,"Data/Maps/approved/Other_Borders/nafo.csv",sep="")) # Revised directory August 4 DK
    
    # Do we want all nafo boundries or just selected
    if(nafo[1]=='all')nafo<-unique(nafo.xy$label)
    # subset nafo.xy into the boundries we selected.
    nafo.sel<-subset(nafo.xy,label%in%nafo)
    
    # Here we combine the calculated centroid of each nafo region and attached the PID and label to that 
    # I believe PID is repeated there just to be sure the label is landing where it should be.
    # Note that this is a nice way to avoid using unique!
    nafo.dat<-merge(calcCentroid(nafo.sel),nafo.sel[c("PID","label")])[!duplicated(nafo.sel[c("PID","label")]),]
    
    # Switch the nafo label to one we use.
    nafo.dat$label[nafo.dat$label=="5ZC"]<-"5ZEM"
    
    attr(nafo.xy,"projection")<-"LL" # DK added July 31, 2015
    attr(nafo.dat,"projection")<-"LL" # DK added July 31, 2015
    
    # Put in the NAFO borders if nafo.bord == T
    if(nafo.bord == T) addLines(nafo.xy,border='grey') # DK added if on July 31, 2015
    # And label them, if nafo.lab == T
    #if(nafo.lab == T ) addLabels(nafo.dat,col=rgb(0.5,0.5,0.5,0.5),cex=2) # DK moved statement and added if on July 31, 2015
  } # end if(!is.null(nafo)){
  
  
   # DK addition to function so boundries are not always added. These are now lines so are added at the 
   # end should not overwrite previous layers.
   if(plot.boundries ==T) 
     {
      #Source2 source(paste(direct,"Assessment_fns/Maps/managePlot.r",sep=""))
       managePlot(bounds = boundries,plot.add=T,area.labels = label.boundries,offshore.names = offshore.names, 
                plot.color = bound.color,direct=direct, direct_fns=direct_fns,manage.colors = manage.colors)
      } # end if(plot.strata==T)

  if(plot.bathy==T)
  {
    #Source3 source(paste(direct,"Maps/bathyPlot.r",sep=""))
    bathyPlot(db = bathy.source,isobath = isobath, plot.add=T, un = un, pw = pw, db.con = db.con,bounds = boundries,
              b.col=bathcol,direct=direct, direct_fns=direct_fns)
  } # end if(plot.bathy==T)
  
  # Replot the managment boundries This just makes the boundary lines show up a bit better, could switch order
  # of these but if you want the management regions in color it won't work, this is a ?nice? general solution...
  if(plot.boundries ==T && plot.bathy==T) 
    
  {
    #Source2 source(paste(direct,"Maps/managePlot.r",sep=""))
    managePlot(bounds = boundries,plot.add=T,area.labels = label.boundries,offshore.names = offshore.names, 
               plot.color = F,direct=direct, direct_fns=direct_fns,manage.colors=manage.colors)
  } # end if(plot.strata==T)
  
  if(plot.strata==T && plot.boundries==T)
  {
    #Source1 source(paste(direct,"Maps/strataPlot.r",sep=""))
    strataPlot(loc = strata, plot.add=T, un = un, pw=pw, db.con = db.con,strata.colors=strata.colors,strata.cex = strata.cex,
               direct = direct, direct_fns = direct_fns)
  } # end if(plot.strata==T)
  
  
   #  DK revision July 31, 2015 if plot.land == T.  This overlays the land on top of the boundries ploted above.	
	if(plot.land == T)addPolys(land,col=land.col)
	
  
  #################################          End Section 2         ##################################################
  
  
  ###############################  Section 3 Add Labels and remaining plot elements       #####################################  
  
  
  ##################  LABELS #######################################################################################################
  # DK July 31, 2015.  I have moved NAFO labels to after the boundaries, it looks terrible if you do both original way, but this
  # puts the labels of NAFO region on top of the boundaries, looks better.
  # if nafo is not NULL then do the below
  if(!is.null(nafo))
  {
    # And label them, if nafo.lab == T
    if(nafo.lab == T ) addLabels(nafo.dat,col=rgb(0.5,0.5,0.5,0.5),cex=1.5) # DK added if on July 31, 2015
  } # end if(!is.null(nafo)){
  
    # This will plot the strata labels for the inshore, needed to be placed here because the land layer will overwrite it
  # and  the strata polygons cover the land, either scenario = ugly... this scenario is lovely
  if(strata == "inshore" && plot.strata == T) 
  {
    # strata.labels should be returned from the strata function for this call.
    addLabels(strata.labels[strata.labels$label != "SPA 4:    0 - 2 miles" & strata.labels$label != "SPA 1: 0-2 miles", ],
              cex = strata.cex)
    # Need to get fancy to add these two labels
    addLabels(strata.labels[strata.labels$label == "SPA 4:    0 - 2 miles",] ,cex=strata.cex,srt =38)
    addLabels(strata.labels[strata.labels$label == "SPA 1: 0-2 miles", ],cex= strata.cex,srt =33)
    
  } # end if(loc == "inshore") 
  
  ########################  This bit will add in the boundary labels overtop of the other layers so that it looks 
  ########################   decent when plotting bathymetry
  # If we want the common offshore names plotted return this...
  if(offshore.names == T) 
  {
    addLabels(common.names,cex=manage.cex)
    addLines(g.arrows,arrows=T,angle=20,length=0.05)
  }
  
  
  if(label.boundries == T)
  {
    # First plot the offshore if appropriate
    if(boundries=='offshore' || boundries == "all")
      {
      addLabels(offshore.labels,cex=manage.cex)
      } # end if(bounds=='offshore' || bounds == "all")
    
   
    # Now plot the inshore labels
    if(boundries=='inshore' || boundries == "all")
    {
      # Note that SPA5 is only plotted when bounds = inshore due to how small an area it consists of.
      addLabels(inshore.labels[inshore.labels$label != "SPA5" & inshore.labels$label != "SPA4",],cex=manage.cex)
      addLabels(inshore.labels[inshore.labels$label == "SPA4",],srt=40,cex = manage.cex)
      
      # If just plotting inshore add more detailed information
      if(boundries =='inshore') 
        {
        addLabels(det.labels[det.labels$label != "St. Marys Bay" & 
                               det.labels$label != "Upper Bay Line" & det.labels$label != "SFA26",],cex=(0.75*manage.cex))
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
        
        } # end if(boundries =='inshore')
    } # End if(bounds=='inshore' || bounds == "all")
  } # end if(plot.add==T && area.labels == T)
  
  ##################  End the LABELS ###########################################################################
  
  
  # if lol= T a layer is added in the color specified by "bathcol".  This gives the land a thick
  # outline in the same color as the bathymetry called above  
  if(lol)addPolys(land,border=bathcol,lwd=6) # DK July 31, 2015 moved from immediately after plotMap call.
  
  #  This adds stipples to the land, stipples being small dots which, in this case helps the land to stand out in the plot better.
  if(stippling == T) addStipples (land, pch='.')
	
  #Read8 Add the Canadian EEZ to the map, this is on by default DK July 31, 2015 revision to make this optional 
  if(plot.EEZ == T) # DK Added July 31, 2015
    { # DK Added July 31, 2015
    EEZ<-read.csv(paste(direct,"Data/Maps/approved/Other_Borders/AtlanticCdnBoundary_subset.csv",sep="")) 
    attr(EEZ,"projection") <- "LL" # DK added July 31, 2015 to remove the last warning from the function call.
	  addLines(EEZ,lty=1,lwd=3)
    } # DK added July 31, 2015


	# if points.lst is not NULL then we have specified points that we want added to the plot
	if(!is.null(points.lst))
	  {
		addPoints(points.lst[[1]],polyProps=points.lst[[2]])
	  }

	# if lines.1st is not NULL then we have specified lines that we want added to the plot
	if(!is.null(lines.lst))
	  {
		addLines(lines.lst[[1]],polyProps=lines.lst[[2]])
	  }
	
	# add grid lines, this work fine.
	if(!is.null(grid))
	  {
		# make grids for x and y
	  x<-seq(floor(xlim[1]),ceiling(xlim[2]),grid)
		y<-seq(floor(ylim[1]),ceiling(ylim[2]),grid)
		# Make the grid
		gridlines<-makeGrid(x,y,byrow=TRUE,addSID=TRUE,projection="LL",zone=NULL)
		# And plot it
		addLines(gridlines,col='grey80',lwd=1)
	  } # end if(!is.null(grid))

  
	# Add a thicker box line around the plot and plot the two major axes and make 'pretty' axes (I hope)...
	box(lwd=2)
	# Allow the following axes to go off graph if needed.
	#par(xpd=T)
	# Make a nice major axis 
	
	if(dec.deg==T) 
	  {
	    x.axs <- axisTicks(xlim,log=F)
	    axis(side=1,at = x.axs,cex.axis=cex,lwd.ticks = 2,tcl=0.5)
  	  # Now make a nice minor axis tick location
  	  mids <- NULL
  	  for(i in 1:(length(x.axs)-1)) mids[i] <- mean(c(x.axs[i],x.axs[i+1]))
  	  # Now even finer scale axes tick marks.
  	  quarts <- NULL
  	  all <- sort(c(x.axs,mids))
  	  for(i in 1:(length(all)-1)) quarts[i] <- mean(c(all[i],all[i+1]))
  	  axis(side=1,at = mids,cex.axis=cex,lwd.ticks = 1.5,labels=NA,tcl=0.45)
  	  axis(side=1,at = quarts,cex.axis=cex,lwd.ticks = 1,labels=NA,tcl=0.35)
	  } # end if(dec.deg==T) 
	  # If we want the axis labels in degree-minute seconds.
	if(dec.deg == F) 
	  { 
	    x.axs <- axisTicks(xlim,log=F)
	    x.labels <- convert.dd.dddd(x.axs,format = "deg.min.sec")$DM_label
	    x.s.labels <- convert.dd.dddd(x.axs,format = "deg.min.sec")$S_label
      
	    # If there are no seconds in the axis tick locations then just plot the Degrees and minutes
	    if(all(x.s.labels==0)==T) axis(side=1,at = x.axs,labels = x.labels,cex.axis=cex,lwd.ticks = 2,tcl=0.5)
	    # If on the other hand there area seconds in the axis tick locations we need to get fancy and plot the seconds too.
	    if(all(x.s.labels== 0)==F)
	    {
	      # Make x.s labels line up nicely with x.labels, weird workaround yes but as good as a solution as I could come up with.
	      xlim[1] <- (floor((xlim[1] - floor(xlim[1]))*60))/60 + floor(xlim[1])
	      xlim[2] <- (ceiling((xlim[2] - floor(xlim[2]))*60))/60 + floor(xlim[2])
	      x.ax <- seq(xlim[1],xlim[2],by=1/60)
	      # This should hopefully keep the axis labels farily "pretty"
	      if(length(x.ax) < 12) x.axs <- x.ax[seq(1,length(x.ax),by=2)]
	      if(length(x.ax) >=12 && length(x.ax) < 24) x.axs <- x.ax[seq(1,length(x.ax),by=4)]
	      if(length(x.ax) >=24 && length(x.ax) < 48) x.axs <- x.ax[seq(1,length(x.ax),by=8)]
	      if(length(x.ax) >=48 && length(x.ax) <=60) x.axs <- x.ax[seq(1,length(x.ax),by=12)]
	      if(length(x.ax) >60) x.ax <- axisTicks(xlim,log=F)
	      x.labels <- convert.dd.dddd(x.axs,format = "deg.min.sec")$DM_label
	      axis(side=1,at = x.axs,labels = x.labels,cex.axis=cex,lwd.ticks = 2,tcl=0.5)

	    } # end if(all(y.s.labels== 0)==F)
	    # Now make a nice minor axis tick location
	    mids <- NULL
	    for(i in 1:(length(x.axs)-1)) mids[i] <- mean(c(x.axs[i],x.axs[i+1]))
	    # Now even finer scale axes tick marks.
	    quarts <- NULL
	    all <- sort(c(x.axs,mids))
	    for(i in 1:(length(all)-1)) quarts[i] <- mean(c(all[i],all[i+1]))
	    axis(side=1,at = mids,cex.axis=cex,lwd.ticks = 1.5,labels=NA,tcl=0.45)
	    axis(side=1,at = quarts,cex.axis=cex,lwd.ticks = 1,labels=NA,tcl=0.35)
	  } # End if(dec.deg == F) 

	
	# Repeat for the y axis

	# If we want the axis labeled with decimal degrees.
	if(dec.deg==T) 
	  {
	    y.axs <- axisTicks(ylim,log=F)
  	  mids <- NULL
  	  for(i in 1:(length(y.axs)-1)) mids[i] <- mean(c(y.axs[i],y.axs[i+1]))
  	  quarts <- NULL
  	  all <- sort(c(y.axs,mids))
  	  for(i in 1:(length(all)-1)) quarts[i] <- mean(c(all[i],all[i+1]))
	    axis(side=2,at = y.axs,cex.axis=cex,lwd.ticks = 2,las=1,tcl=0.5)
	    axis(side=2,at = mids,cex.axis=cex,lwd.ticks = 1.5,labels=NA,tcl=0.45)
	    axis(side=2,at = quarts,cex.axis=cex,lwd.ticks = 1,labels=NA,tcl=0.35)
	  } # end if(dec.deg==T)
	# If we want the axis labelled with degree-minute-seconds.
	if(dec.deg == F) 
  	{ 
	    y.axs <- axisTicks(ylim,log=F)
	    y.labels <- convert.dd.dddd(y.axs,format = "deg.min.sec")$DM_label
	    y.s.labels <- convert.dd.dddd(y.axs,format = "deg.min.sec")$S_label
  	  # If there are no seconds in the axis tick locations then just plot the Degrees and minutes
  	  if(all(y.s.labels==0)==T) axis(side=2,at = y.axs,labels = y.labels,cex.axis=cex,las=1,lwd.ticks = 2,tcl=0.5)
      # If on the other hand there area seconds in the axis tick locations we need to get fancy and plot the seconds too.
  	  if(all(y.s.labels== 0)==F)
  	    {
  	      ylim[1] <- (floor((ylim[1] - floor(ylim[1]))*60))/60 + floor(ylim[1])
  	      ylim[2] <- (ceiling((ylim[2] - floor(ylim[2]))*60))/60+ floor(ylim[2])
  	      y.ax <- seq(ylim[1],ylim[2],by=1/60)
  	      # This should hopefully keep the axis labels farily "pretty"
  	      if(length(y.ax) < 12) y.axs <- y.ax[seq(1,length(y.ax),by=2)]
  	      if(length(y.ax) >=12 && length(y.ax) < 24) y.axs <- y.ax[seq(1,length(y.ax),by=4)]
  	      if(length(y.ax) >=24 && length(y.ax) < 48) y.axs <- y.ax[seq(1,length(y.ax),by=8)]
  	      if(length(y.ax) >=48 && length(y.ax) <=60) y.axs <- y.ax[seq(1,length(y.ax),by=12)]
  	      if(length(y.ax) >60) y.axs <- axisTicks(ylim,log=F)
  	      y.labels <- convert.dd.dddd(y.axs,format = "deg.min.sec")$DM_label  	      
      	  axis(side=2,at = y.axs,labels = y.labels,cex.axis=cex,las=1,lwd.ticks = 2,tcl=0.5)
      	    } # end if(all(y.s.labels== 0)==F)
  	  mids <- NULL
  	  for(i in 1:(length(y.axs)-1)) mids[i] <- mean(c(y.axs[i],y.axs[i+1]))
  	  quarts <- NULL
  	  all <- sort(c(y.axs,mids))
  	  for(i in 1:(length(all)-1)) quarts[i] <- mean(c(all[i],all[i+1]))
  	  #axis(side=2,at = y.axs,cex.axis=cex,lwd.ticks = 2,las=1,tcl=0.5)
  	  axis(side=2,at = mids,cex.axis=cex,lwd.ticks = 1.5,labels=NA,tcl=0.45)
  	  axis(side=2,at = quarts,cex.axis=cex,lwd.ticks = 1,labels=NA,tcl=0.35)
  	} # End if(dec.deg == F) 

	# Now custom add the axis labels if not already supplied (this can be done in PlotMap, 
	# but with the axis altered above this isn't always nice)
	if(missing(ylab) && dec.deg==T) mtext(side=2,expression("Latitude" ~ group("(", degree,")")),cex=cex,line=4)
	if(missing(ylab) && dec.deg==F) mtext(side=2,expression("Latitude" ~ group("(", degree,")")),cex=cex,line=4)
	if(missing(xlab)) mtext(side=1,expression("Longitude" ~ group("(", degree,")")),cex=cex,line=3)
	
	if(!missing(ylab) && dec.deg==T) mtext(side=2,ylab,cex=cex,line=4)
	if(!missing(ylab) && dec.deg==F) mtext(side=2,ylab,cex=cex,line=7)
	if(!missing(xlab)) mtext(side=1,xlab,cex=cex,line=3)
	
	# Add plot title
	title(main=title,cex.main=cex.mn,font.main=2)
	
	# Do you want to add a scale to the figure
	if(add.scale == T) maps::map.scale(mean(xlim)-0.1*(max(xlim)-min(xlim)),max(ylim)-0.02*(max(ylim)-min(ylim)),relwidth = 0.15,cex=cex,ratio=F)
  
	# Final step, turn of the plotting device if outputing a figure
		if(output.png == T || output.pdf == T) dev.off()
	###############################  End Section 3      ##################################################  		
	# Return the xlimits and y limits from the plot, helpful in adding additional annotations to the figure...
	assign("smap.xlim",xlim,pos=1)
	assign("smap.ylim",ylim,pos=1)
} # End ScallopMap function

