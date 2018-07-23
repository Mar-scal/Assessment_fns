####  Created as standalone by DK September 2015. This calculates the strata areas.

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

###############################################################################################################
# Arguments
#strata.dat: The data, a vector of Z values.
#strata.def: The strata boundaries.  Default = c(0,5,10,20,40)
#units:      The value of the units.  Default = 'km2' which is total area.  Optionally can be "towable' which divides by area of an individual tow.
###############################################################################################################


# areacal.r	("Y:/Assessment/Georges a/2009/r/fn/design functions/areacal.r"):

# Start the function
areacal <- function(strata.dat,strata.def=c(0,5,10,20,40),units='km2')
{
  # Define object and set the length of the for loop.
  strata.area<-c()
  n<-length(strata.def)-1
  
  # Define the strata area for each strata
  for(i in 1:n)
    {
    #find the number of points that are not NA, Z is less than strata i+1 or Z >= stratra i
    strata.area[i]<-length(strata.dat$z[!is.na(strata.dat$z) & strata.dat$z < strata.def[i+1] & strata.dat$z>=strata.def[i]])
    # For the last point be sure in include  strata i+1 value using less than or equal to.
    if(i==n) strata.area[i]<-length(strata.dat$z[!is.na(strata.dat$z) & strata.dat$z <= strata.def[i+1] & strata.dat$z >= strata.def[i]])
    
    }
  
  # DK Note:  This is converting the degree minutes to km.  I don't think this is very good, though I don't think we really use it for anything.
  km <- (strata.dat$y[2]-strata.dat$y[1])*111.2
  # Strata area
  unit.area<-km^2
  # Area of a standard tow.  DK Note the width here is different from that we'd use for a survey, and does this align with what we do inshore?
  # This is correct for offshore but if we use this code for other data this could be bad.
  atow<-800*2.4384/10^6 # area of standard tow in km2
  # If units = 'towable' convert area to toawable area.
  if(units=='towable') unit.area<-unit.area/atow
  
  # Total strata area = number of points within the strata * area (be it total area or towable area).  
  strata.area<-strata.area*unit.area
  # Give strata names 1:however many strata their are
  names(strata.area)<-1:n	
  
  #return the strata.area object.
  strata.area
  
}

