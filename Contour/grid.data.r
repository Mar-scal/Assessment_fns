####  Created as standalone by DK September 2015. This uses ordinary(simple) kriging for interpolation of data.

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "image.prep.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################

#	source(".../contour/grid.data.r")

# Arguments
#vdata:  The covariate data used to perform the kriging with X/Y locations of the covariate metric.
#gdata:  The grid of X and Y values to extrapolate the kriging results across


grid.data <- function(vdata,gdata)
{
  
  require (gstat)  || stop("You need to install the gstat package for this to work!")
  
  # Output this function is running
  print("grid.data start")
  print(Sys.time())
  # Take 3rd point in vdata as vnames
  vname<-names(vdata)[3]
  # rename that point "Z" for gstat
  names(vdata)[3]<-"Z"
  # Make a gstat object, using ordinary/simple kriging with teh vdata
  Z.gstat <- gstat(id = "Z", formula = Z ~ 1, locations = ~ X + Y, data = vdata)
  # Now using the gstat data map this onto the gdata.
  Z.pre<- predict(Z.gstat, gdata)
  # rename "Z" to original name
    names(Z.pre)[3]<-vname
  # Drop column 4 and return results to main function
  Z.pre[,-4]
  # Print the run time and end of function.
  print("grid.data end")
  print(Sys.time())
} # end grid.data function.


