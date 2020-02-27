####  Commented and checked by DK starting on July 28, 2015. This is the function used to generate countours for spatial
#### plots, mostly called into ScallopMap function.  This allows for numerous different "types" of data to be "interpoloated"

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary_figures.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#   1:  area.cal.r
#   2:  blank.bank.r
#   3:  grid.data.r
#   4:  image.prep.r
#   5:  smooth.bank.r
#   6:  tick.def.r
##
###############################################################################################################

#	source("Y:/Offshore scallop/Assessment/Assessment_fns/Contour/contour.gen.r")

## Arguments
#contour.dat:      The data to be interpolated/kriged.
#interp.method:    Interpolation method.  Default  = 'interp' which uses interp function from akima library, 
#                 'gstat' uses the gstat function from gstat library 
#                 'krige' uses the kriging function from akima library
#                 'none' = no interpolation function
#direct:           The directory in which grid.data is located: default = "Y:/Offshore scallop/Assessment/Assessment_fns/"
#res:              resolution for interpolation.  Default = 0.01 (slightly less than 1 minute)
#log.data:         log transform the variable.  Default =T
#subset.eff:        If data are subset change all values outside the subset range to this value.  Default = NA
#subscale:          Adds a small buffer around the edges of the x and y values Default = res
#plot:              plot the results.  (T/F) Default = F

# These calls are needed for the smooth function
#smooth:           Smooth data data, if TRUE then smooth.bank function is called, takes average across grid.  (T/F), Default = F 
#sres:             The resolution for the smoothing (grid), default = 1/60.1
#smooth.fun:       The function used to smooth data.  Default = median, want a measure of central tendency (mean or median)
#                  Note that smooth.bank function itself has this set to the mean by default.
#no.data:          How to treat missing data when smoothing, Options are "NA" or "0"  default is to assume zero (not letter 'O')
#subset.poly:      The polygon region to include, a subset of contour.dat. Used in smooth.bank + needed elsewhere.  Default == NULL
#procedure:        Used only in the smooth function.  Options for how to calculate the smooth.  
#                  Can be any number from 1-6 Default = 1. see smooth.bank.r for details of what each option does.
#expand:           Makes X and Y data a little bit bigger which helps with calculations around the edges.  default = 0.1
#aspr:             aspect ratio needed for calculations, missing by default which aligns with the calculation used in PBSmapping.
#smooth.matrix:    Do we want to return the data as a dataframe or a list. Default = T which returns a list

# These calls are needed for the function blank.bank which adds 0's to the data where we have no information
#blank:            Do we want to add blanks to the data beyond where they exist.  (T/F), default = F
#blank.dist:       The blanking distance, beyond which if no data are present zeros are assigned, default is missing
#blank.eff:        Fills in the Z values for returned dataframe.  Default = 0,
#blank.type:       The calculation used to insert blanks.  See blank.bank for details  Options are 1 or 2. Default =1
#blank.scale:      Makes X and Y data a little bit bigger which helps with calculations around the edges.  Default = 0.1,

# These calls are needed for the function tick.def.r
#nstrata:          Number of strata.  Default = 4
#place:            Number of decimal places to round the data to. Default = 0
#str.max:          maximum value for stratifying variable (all values greater will be set at maximum)
#str.min:          minimum value for stratifying variable (all values lesser will be set to 'subset.eff')

# This call is needed for area function
#area.units:      The area calculation.  Default = 'towable" which is towable units.  "km2" will calculate total area.
#ticks:           contour lines or strata definitions to be plotted.  Default = missing which leads to ticks being 
#                 calculated from nstrata if specified tick.def.r is run and replaces this value.  
#                 The nstrata argument should align with ticks if both specified

# These calls are needed for image.prep
#id.par:           inverse distance weighting, used when method = "gstat".  Default = 0.5
#linear:           type of interpolation to run, used when method = "interp". (T/F) Default = T which run linear interpolation
#                  if set to F it runs a cubic-spline interpolation
#max.dist:         the maximum distance for which kriging methods will use data.  Default = Inf which uses all data (see nmax)
#nmax:             the number of nearest observations that should be used for a kriging prediction.  Default = 7 (see maxdist)
#covariate.dat     covariate data to assist with interpolation, only used for method = "krige"  Default=NULL
#regrid:           If using covariate data should the grid be reploted using these data.  Default = F
#mod.type:         Model type for the variogram model (vgm).  only used for method = "krige" 
#                  Options include "Exp", "Sph", "Gau", "Mat".  Default = "Sph"

# Plot function arguments, if plot = F these are not used.
#yl:              The y axis boundaries, default is missing so set by the data
#title:           title for contour plot
#color:           colors for contour plot
#title:           title for contour plot
#unit.lab:        Label for the units used 
#lab:             Label for the plots
#units:           Unit label for the plots
#points:          Add datapoints to the plot. points = 1 adds EID's before smoothing/blanking. If points = 2 adds EID's from after.
#key:             The key for the axis labels (only used if plot=T).  Default = strata.  Options are:
#                 strata = key diveded by ticks 
#                 cont = continuous scale 
#                 log.cont = continuous log scale


contour.gen<-function(contour.dat,interp.method='interp', direct = "Y:/Offshore scallop/Assessment/",
                      res=0.01,log.dat=F,  subset.eff=NA,subscale=res,plot=F,
                      # These function calls are needed for smooth.bank subfunction
                      smooth=F,smooth.fun=median, sres=1/60.1, no.data='0', subset.poly=NULL, procedure=1, 
                      expand =0.1, aspr, smooth.matrix = T,
                      # These function calls are needed for blank.bank
                      blank=T, blank.dist, blank.eff=0, blank.type=1, blank.scale = 0.1,
                      # These function calls are needed for tick.def.r
                      nstrata,str.max,str.min,place=0,#place
                      # These function calls are needed for area.cal.r
                      area.units = "towable", ticks,
                      # These function calls are needed for image.prep
                      linear=T, maxdist=Inf, nmax=8, id.par=0.5,covariate.dat=NULL,
                      regrid=F, mod.type="Sph",
                      # These are only needed if plot = T (i.e. you are making a stand alone plot)
                      color="YlGn", color.fun=tim.colors, yl, xl , title='',unit.lab=NULL, # Plot related arguments
                      key='strata',units='kg/hm', lab='',points=1 # Plot related arguments
                      )
  
# Start the Contour.gen function  
{
 
  #################################################  Section 1, Contour Generation ##########################################	
  
  # Load the required packages, not all packages are required depending on options chosen.
  require (PBSmapping)   || stop("You need to install the PBSmapping package for this to work!")
  require (akima)        || stop("You need to install the akima package for this to work!")
  require (gstat)        || stop("You need to install the gstat package for this to work!")
  require (fields)       || stop("You need to install the fields package for this to work!")
  require (splancs)      || stop("You need to install the splancs package for this to work!") 
  require (RColorBrewer) || stop("You need to install the RColorBrewer package for this to work!")
  require(CircStats)     || stop("You need to install the CircStats package for this to work!")
  require(spatstat)      || stop("You need to install the spatstat package for this to work!")
  
  # Load the required subfunctions. #Source1 - #Source5
  source(paste(direct_fns,"Contour/smooth.bank.r",sep=""))
  source(paste(direct_fns,"Contour/tick.def.r",sep=""))
  source(paste(direct_fns,"Contour/blank.bank.r",sep=""))
  source(paste(direct_fns,"Contour/image.prep.r",sep=""))
  source(paste(direct_fns,"Contour/area.cal.r",sep=""))
  
  print("contour start")
	print(Sys.time())
	# Make a image object
	image.mod<-NULL

	# Give the countour data a name, needed for interpolation routine
	names(contour.dat)[1:4]<-c("EID","X","Y","Z")
	# Add a covariate name if there is a covariate in our dataset
	if(is.null(covariate.dat)==F) names(contour.dat)[5]<-"CoV"
	# make a data object with just the ID and X,Y coordinates
	dataPoints1<-contour.dat[,1:3]
	# Make sure ID column is numbers.
	if(is.numeric(dataPoints1$EID)==F)dataPoints1$EID<-1:nrow(dataPoints1)
	
	# Make an object called blank which is logically false when kirging
	if(interp.method=='krige') blank=F
	# Set label for unit.lab if not specified, is whatever is specified for units in function call
	if(is.null(unit.lab)  == T )unit.lab <- units
	
	# If Aspect ratio is not specified then add it, calculated based on Y axis values, 
	# this is how PBS map calculates the aspect ratio for ""LL" projection data, which is what we are using for our maps.  
	if(missing(aspr)==T)
	  {
			aspr=1/cos(rad(mean(contour.dat$Y)))
	  	print(paste('Aspect ratio',aspr))
  	} # end if(missing(aspr))
	
	# If we have included a subset polygon and it the subset polygon is "sqaure" we can define the polygon boundaries
	if(is.null(subset.poly)==F && subset.poly == 'square')
	{
	  # inset again adds a little buffer around the edges of teh x and y values
	  inset=subscale*0.8
	  # Make a square with X and Y coordinates slightly larger than we have in the data.
	  subset.poly<-with(contour.dat,data.frame(X=sort(rep(c(min(X)-inset,max(X)+inset),2)),
	                                           Y=c(min(Y)-inset,rep(max(Y)+inset,2),min(Y)-inset)))
	} # end if(is.null(subset.poly)==F && subset.poly == 'square')
	
	
	# If we are smoothing the data	we need to call in our smooth.bank function
	if(smooth==T)
	  {
	    # if using interpolation get the contour data right from this smooth.
  		if(interp.method !='none')
  		  {
  		    #Source1 source(paste(direct,"Contour/smooth.bank.r",sep="")) # note smooth.matrix 
  		   #not defined here as we want the default of smooth.bank
  		  	contour.dat<-smooth.bank(contour.dat,fun=smooth.fun,res=sres,aspr=aspr,
  		  	                         no.data=no.data,subset.poly=subset.poly,procedure=procedure)
  	  		contour.dat<-contour.dat[!is.na(contour.dat$Z),]
  		  } # end if(interp.method !='none')
	  
  		if(interp.method=='none')
  		  {
  		    #Source1 source(paste(direct,"Contour/smooth.bank.r",sep="")) 
  		  	image.dat<-smooth.bank(contour.dat,fun=smooth.fun,res=sres,aspr=aspr,matrix = smooth.matrix,
  		  	                       no.data=no.data,subset.poly=subset.poly,procedure=procedure)
  		  	names(image.dat)<-c('x','y','z')
  		  } # end if(interp.method=='none')
	} # end if(smooth==T)
	
	# Add blanks to regions we don't have any information.
	if(blank==T) 
	  {
	    # here the blank distance is defined within the blank.bank function, in second call it is user defined.
	    #Source2 source(paste(direct,"Contour/blank.bank.r",sep=""))
  		if(missing(blank.dist)==T)contour.dat<-blank.bank(contour.dat,aspr=aspr,type=blank.type,eff=blank.eff)
  		if(missing(blank.dist)==F)contour.dat<-blank.bank(contour.dat,blank.dist=blank.dist,aspr=aspr,type=blank.type,eff=blank.eff)
	  } # end if(blank==T)
		
	# X,Y,Z values are in columns 1-3 so grab those
	dataPoints2<-contour.dat[,1:3]
	# ANd make an EID column of numbers from 1:length of data
	dataPoints2$EID<-1:nrow(dataPoints2)
	
	# Now we define how many strata are in our data and define where in our Z value we want to split the strata.  
	# So we only run this if ticks == "define", note that we must have defined nstrata in the function call for this to work!
	if(ticks[1] == 'define')
	  {
	    #Source3 source(paste(direct,"Contour/tick.def.r",sep=""))
	    ticks<-unique(tick.def(contour.dat$Z,nstrata,str.min,str.max,place))
	    nstrata<-length(ticks)-1
	  } # end if(missing(ticks)==F && ticks[1]=='define')
	
	# If we have not specified nstrata then the number of strata is simply the length of "ticks" -1.  
	# Note we need to have ticks or nstrata specified or we will get an error at this point.
	if(missing(nstrata)==T)
	  {
		  if(missing(ticks) == T )print("Whoops!  Either 'ticks' or 'nstrata' must be specified in contour.gen()")
		  nstrata<-length(ticks)-1
	  } # end if(missing(nstrata))
	
	# Chose teh color palette if we have color just set to 1 value, differs for > or < 10 strata
	if(length(color)==1)
	  {
		  if(nstrata<10)color<-brewer.pal(nstrata,color)
	  	if(nstrata>=10)color<-color.fun(nstrata)
	  } # end if(length(color)==1)
	

	
# Now we are ready to perform the interpolation, if we want to.	
	if(interp.method != 'none')
	  {
  	  # Using method set in function run the interpolation function
  	  #Source4 source(paste(direct,"Contour/image.prep.r",sep=""))
  		image.lst<-image.prep(dat=contour.dat,method=interp.method,nmax=nmax,id.par=id.par,log.dat=log.dat,res=res,
  		                      linear=linear,covariate.dat=covariate.dat,regrid=regrid,mod.type=mod.type,
  		                      subscale=subscale, subset.poly=subset.poly,asp=aspr,direct=direct, direct_fns=direct_fns) # aspr=aspr,
  		
  		# Make an object of the data 
  		image.dat<-image.lst[[1]]
  		# Make an object with the model fits 
  		image.mod<-image.lst[[2]]
  		# If we have specified the tick values (i.e. the strata boundary values of our variable of interest) grab the min and maximum
  		if(missing(ticks)==F)
  		  {
  		  	if(missing(str.min))str.min<-min(ticks)
  		  	if(missing(str.max))str.max<-max(ticks)
  		  } # end if(missing(ticks)==F)
  		
  		# If we have not defined ticks get the data from the interpolated data, bound it by the strata minimum and strata maximum
  		# which may be user defined or taken from the data.
  		if(missing(ticks)==T)
  		  {
  			  if(missing(str.min)==T)str.min<-min(image.dat$z,na.rm=T)
  			  if(missing(str.max)==T)str.max<-max(image.dat$z,na.rm=T)
  			  ticks<-seq(str.min,str.max,length=nstrata+1)
  		  } # end if(missing(ticks)==T)
  		# For any values greater than the strata maximum value specified the values are replaced by the maximum.
  		image.dat$z[image.dat$z>str.max]<-str.max
  		# For any values smaller than the minimum strata value specified the values are replaced 
  		# with subset.eff (which by default = NA)
	  	image.dat$z[image.dat$z<str.min]<-subset.eff		##### not tested for other applications!!!
  	} # end if(interp.method!='none')
	
		
	# Subset the data
	if(is.null(subset.poly)==F)
	  {
  	  # Make the x's and y's
  		Y<-sort(rep(image.dat$y,length(image.dat$x)))
  		X<-rep(image.dat$x,length(image.dat$y))
  		# Make a temporary data frame with all the data
  		tmp<-data.frame(X,Y,Z=as.vector(image.dat$z))
  		# Replace the data outside the boundaries with "subset.eff" which defaults to NA
  		tmp$Z[!with(tmp, inout(cbind(X,Y), subset.poly[c("X","Y")], bound = T))]<-subset.eff
  		# Replace the z values in the image.dat matrix/data.frame.
  		image.dat$z<-matrix(tmp$Z,length(image.dat$x),length(image.dat$y))
	  } # end if(is.null(subset.poly)==F)
	
#################################################  End Section 1, Contour Generation ######################################
	
	
#################################################  Section 2, Optional Plot and post processing ###########################	
		
	# Make the plot if requested.
	if(plot==T)
	  {
		
  		if(missing(yl))yl=range(image.dat$y)
  		if(missing(xl))xl=range(image.dat$x)
  		
  		#Read1 Bring in the land polygon
  		land<-read.table(paste(direct,"Data/Maps/approved/Coastline/martimesHIGH.ll",sep=""),header=T)
  		
  		#Read2 AtlCanbathy.xyz Read in the bathymetry data
  		#bathy<-read.csv(paste(direct,"Data/Maps/approved/Bathymetry/topex/bathyPoly.csv",sep=""))
  		bathy<-read.csv(paste(direct,"Data/Maps/approved/Bathymetry/topex/AtlCanbathy.xyz",sep=""))
  		# This is neat, The filled.contour command runs over the next number of lines and through several if statements.
  		# First we bring in the image data, define the levels, col, y limits, etc.  The axes are then defined based
  		# on what 'key' has been set to.
  		filled.contour(
  		image.dat$x,image.dat$y,image.dat$z,
  		levels = ticks,
  		col = color,
  		ylim=yl,xlim=xl,asp=aspr,
  		key.axes  = 
  		  {
  		    # If scale is continuous log scale these are our axis, draw a rectangle using "ticks"
  			  if(key=='log.cont')
  			    {
  				    axis(4,log(c(2:10,seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),
  				                 seq(20000,100000,10000)))*(max(ticks)/max(log(ticks))),lab=F,tcl=-0.2)
  				    axis(4,c(1,log(10^(1:4))*(max(ticks)/max(log(ticks)))),lab=10^(0:4))
  				    rect(0, log(ticks[-length(ticks)])*(max(ticks)/max(log(ticks))), 1, 
  				         log(ticks[-1])*(max(ticks)/max(log(ticks))), col = color, border=NA)
  			    } # end if(key=='log.cont')
  			  # If scale is no log scale this is our axis, draw a rectangle using "ticks"
  		    if(key=='cont')
  			   {
    				  axis(4)
    				  rect(0, ticks[-length(ticks)], 1, ticks[-1], col = color, border=NA)
    			  } # end if(key=='cont')
  			  # If based on strata just plot the axis at tick locations.
  		    if(key=='strata')
  			    {
  			  	  axis(4,ticks)
  			   } # end if(key=='strata')
  		   }, # end key.axes = 
  		   key.title=title(main=unit.lab),
  		   plot.title=title(main=title),
  		    
  		   plot.axes = 
  		     {
        	    axis(1)
        		  axis(2)
        		  legend('topright',lab,bty='n',cex=1.5,adj=c(0,0))
        		  addPolys(land,col="wheat")
        		  addLines(bathy,col=rgb(0,0,1,0.5))
        		  # add datapoints 1 and 2 if they exist, give each different types.
        		  if(1 %in% points) addPoints(dataPoints1,pch=16,cex=0.5)
        		  if(2 %in% points) addPoints(dataPoints2,pch=1,cex=0.5)
        	 } # end plot.axes = 
  		) # End the filled contour() call.
	  } # end if(plot==T)

	
	
	# print that function has completed with run time
	print("contour end")
	print(Sys.time())

	# Calculate the area's using the areacal function
	areas<-areacal(image.dat,units=area.units,strata.def=ticks)
	
	# Put the results of interest into a list and return it to function calling this
	output<-list(contour.dat=contour.dat,image.dat=image.dat,image.mod=image.mod,str.def=ticks,areas=areas)
	return(output)
	#################################################  End Section 2, Optional Plot and post processing ###########################	
	
} # end function


