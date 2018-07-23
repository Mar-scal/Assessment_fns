####  Created as standalone by DK September 2015. This incorporates blanking distance by including zeros spaced eqully at the average nearest 
#     nieghbour distance, default blanking distance is the nearest neighbour distance of the most isolated point

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "contour.gen.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################

#	source(".../contour/blank.bank.r")

# Arguments
#surv.dat:    data to add blanks to
#blank.dist:  The blanking distance, beyond which if no data are present zeros are assigned, default is missing
#aspr:        Default = aspr (i.e. called from another function)
#type:        The calculation to determine blanking spacing.  Options are 1 or 2. Default = 1 
#               type = 1 divides the y (and x) range by the mean nearest neighbour distance (multiplied by aspect ratio for latitude.
#               type = 2 divides the y (and x) range by the blanking distance (multiplied by aspect ratio for latitude.
#eff:         Fills in the Z values for returned dataframe.  Default = 0,
#scale:       Make X and Y data a little bit bigger which helps with calculations around the edges.  Default = 0.1


blank.bank<-function(surv.dat,blank.dist,aspr=aspr, type=1, eff=0,scale=0.1)
{
  
  require(spatstat) ||  stop("You need to install the spatstat package for this to work!")
  
  # Just grab the X and Y values from the data
  surv.pts<-subset(surv.dat,select=c('X','Y'))
  # Get the x and y minimum values
  xmin<-min(surv.pts$X)
  xmax<-max(surv.pts$X)
  ymin<-min(surv.pts$Y)
  ymax<-max(surv.pts$Y)
  # Create an object with these values scaled, essentially this is creating a buffer around the data, identical to 'expand' in smooth.bank.r.
  W<-owin(c(xmin-scale,xmax+scale),c(ymin-scale,ymax+scale))
  # converts the data into a spatstat object 
  surv.ppp<-as.ppp(surv.pts,W)
  # If we don't specify the blank distance is it calculated as maximum of the nearest neighbour distance.
  if(missing(blank.dist)==T) blank.dist<-max(nndist(surv.ppp))
  #type 1 divides the y (and x) range by the mean nearest neighbour distance (multiplied by aspect ratio for latitude.
  if(type==1) dims<-c(round((ymax-ymin)/mean(nndist(surv.ppp))*aspr),round((xmax-xmin)/mean(nndist(surv.ppp))))
  #type 2 divides the y (and x) range by the blank.dist (multiplied by aspect ratio for latitude.
  if(type==2) dims<-c(round((ymax-ymin)/blank.dist*aspr),round((xmax-xmin)/blank.dist))
  # Calculate the distance between points, the dimensions are specified by dims.
  blank.map<-distmap(surv.ppp,dim=dims)
  # make a dataframe
  blank.dat<-data.frame(X=sort(rep(blank.map$xcol,blank.map$dim[1])),Y=rep(blank.map$yrow,blank.map$dim[2]),dist=as.vector(blank.map$v))
  #subset it so only dist values > blank.dist are retained.
  blank.dat<-subset(blank.dat,dist>blank.dist,c('X','Y'))
  #merge with the original data and print the blanking distance
  blank.dat<-merge(surv.dat,data.frame(EID=1:nrow(blank.dat)+1000,blank.dat,Z=eff),all=T)
  print(paste("blanking distance",blank.dist))
  
  # return the blank.dat object
  blank.dat
  
} # end function
