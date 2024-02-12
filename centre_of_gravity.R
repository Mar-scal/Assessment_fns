# Here's a function to calculate the weighted mean (center of gravity) in 1 or two dimensionas along with the standard deviation
# Adapted from SDMTools package but just using standard weighted mean and weighted variance calcs... 
# You only need to specify one of x or y.  This could fairly easily be generalized to be an n-dimensional COG if the need ever arose...
# Arguements:

## The general mapping inputs if you are just wanting to produce a spatial map without an INLA surface

#1: x     The locations of your data in the 'x' direction, a vector.  missing by default
#2: y     The locations of the data in the 'y' direction, a vector.  missing by default
#3: wt    The weigths for your x and y data. In 2-D this assumes your weighting is the same in each direction 
#           (i.e. think a coordinate with a weighting value associated with it)


cog.calc <- function(x,y,wt)
{
  if(!missing(x)) 
  {
    wt.mean.x <- x * wt
    cog.x <- sum(wt.mean.x) / sum(wt)
    sd.x <- sqrt(sum(wt) / (sum(wt)^2 - sum(wt^2) ) * (sum(wt * (x - cog.x)^2)))
    se.x <-  sd.x/sqrt(length(wt))
    sum(wt * (wt.mean.x - cog.x)^2)/ (sum(wt))#^2 - sum(wt^2)))#
    if(missing(y)) return(data.frame(x = cog.x, y = NA,sd.x  = sd.x,sd.y = NA,se.x  = se.x,se.y = NA))
  }
  if(!missing(y)) 
  {
    wt.mean.y <- y * wt
    cog.y <- sum(wt.mean.y) / sum(wt)
    sd.y <- sqrt(sum(wt) / (sum(wt)^2 - sum(wt^2) ) * (sum(wt * (y - cog.y)^2)))
    se.y <- sd.y /sqrt(length(wt))
    if(missing(x)) return(data.frame(x = NA, y = cog.y,sd.x  = NA,sd.y = sd.y,se.x  = NA,se.y = se.y))
  }
  # This will only happen if both x and y are not missing...
  return(data.frame(x = cog.x, y = cog.y,sd.x  = sd.x,sd.y  = sd.y,se.x  = se.x,se.y = se.y))
} # end cog.fun <- function(x,y,wt)