# This allocates tows (or anything really) for a straified random design.  
# This can be based on bottom type (or another polygon based non-continuous stratifying variable)
#####  DK August 26th, 2016
# Update history
# Commented, checked  and revised by DK August 2016 (Added in seed option to the function)


#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
# 1: source(paste(direct_fns,"Survey_design/Survey_design_test.r",sep=""))
# 2: source(paste(direct_fns,"Survey_design/alloc.poly.r",sep=""))
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
## None.
###############################################################################################################

###############################################################################################################
# Arguments
# npoints:         The number of points to generate
# bounding.poly:  The boundary polygon for the bank.  
# projection:     The projection of the data.  Default = "LL"
# mindist:        The minimum distance between points.  Default = 1, this is used in genran and if repeated tows to weed out tows too close to each other
# seed:           Set a seed so that your results can be reproduced.  Default = NULL which uses R random number generation.  Any integer will result
#                 in producing a reproducable random number draw.
###############################################################################################################

genran<-function(npoints,bounding.poly,mindist=NULL,seed = NULL)
{
  # Load required packages
	require(spatstat)|| stop("Install spatstat Package")
	require(splancs)|| stop("Install splancs Package")
	require(PBSmapping)|| stop("Install PBSmapping Package")
  require(sf)|| stop("Install sf Package")
	
  # If seed is specified set the random number generator.
  if(!is.null(seed)) set.seed(seed)
	# create pool of points based on the bounding polygon
	# bound.pts<-as.points(list(x=bounding.poly$X,y=bounding.poly$Y))
	
	# If mindist is not specified we can simply generate some random points within the polygons.
	if(is.null(mindist))
	{
	  # This generates the random points from within our boundary polygon
	  pool.EventData <- st_sample(bounding.poly,size=npoints, type="random", exact=T) %>%
	    st_sf('EID' = seq(length(.)), 'geometry' = .) %>%
		  st_intersection(., bounding.poly) %>%
	    cbind(st_coordinates(.))
	} # end if(is.null(mindist))
	
	# If we have a minimum distance between points this gets a little more complex...
	if(!is.null(mindist))
	{
	  # This generates the random points from within our boundary polygon
	  pool.EventData <- st_sample(bounding.poly,size=npoints, type="random", exact=T) %>%
	    st_sf('EID' = seq(length(.)), 'geometry' = .) %>%
	    st_intersection(., bounding.poly) %>%
	    cbind(st_coordinates(.))
		
		#Make sure it's in UTM for distance calcs
	  pool.EventData<-st_transform(pool.EventData,32620)

		# Get the nearest neighbour distances
		nearest<-st_nearest_feature(pool.EventData)
		pool.EventData$nndist <- as.numeric(st_distance(pool.EventData, pool.EventData[nearest,], by_element = TRUE))/1000
	
		# Now run a loop for all of the points	
		for(i in 1:npoints)
		{
			# If a particular point is < mindist we repeatedly sample this point until it is no longer < mindist.
		  if(pool.EventData$nndist[i] < mindist)
			{
		    # To make a repeatable survey design we need to get a "set list" of random seeds to pull from, starting with the seed we specify
		    # in the initial call
		    if(!is.null(seed)) 
		    {
		      set.seed(seed)
		      seeds <- round(runif(10000,1,1e9)) # gets a large number of potential seeds we can sample from (10000 options between 1 and 1e9)
		    } # end if(!is.null(seed)) 
		    # Run this code until we hit the "break"
		    repeat
				{
				  # Again if we are trying to make a reproducable sample design we enter this if statement
				  if(!is.null(seed))
				  {
				    # We set the seed again, the first time will use our original seed value, the rest of the repeats will get a seed number from
				    # our seed list.
				    set.seed(seed)
				    # We then sample
				    seed <- sample(seeds,size=1)
				    set.seed(seed)
				  }# end if(!is.null(seed)) 
				  
				  # Get a new point
				  st_geometry(pool.EventData[i,]) <- st_sample(bounding.poly,1, type="random", exact=T) %>% st_transform(st_crs(pool.EventData))
				  pool.EventData[i, c("X","Y")] <- st_geometry(pool.EventData[i,]) %>% st_transform(4326) %>% st_coordinates()
		 
# old way
				  # # Fix the projection to UTM
				  # # Convert the data to spatstat happy
				  # pool <- pool.EventData 
				  # st_geometry(pool) <- NULL
				  # attr(pool,"projection")<-"LL"
				  # # Get the window for the whole bank
				  # pool<- convUL(pool)
				  # W<-owin(range(pool$X),range(pool$Y))
				  # pool.ppp<-as.ppp(subset(pool,select=c('X','Y')),W)
				  # # Get the nearest neighbour distance
				  # pool$nndist<-nndist(pool.ppp)
				  # # Again make sure the projection isn't LL
				  # pool$nndist[i]
				  
				  # Get the nearest neighbour distance
					nearest<-st_nearest_feature(pool.EventData)
					pool.EventData$nndist <- as.numeric(st_distance(pool.EventData, pool.EventData[nearest,], by_element = TRUE))/1000
					#pool.EventData$nndist[i]
					
					# Once the distance is >= mindist break out of the repeat command.
					if(pool.EventData$nndist[i] >= mindist) break
				} # end repeat
			} # end if(pool.EventData$nndist[i]<mindist)
		} # end for(i in 1:npoints)
	} # end if(!is.null(mindist))
  # Return the results
	pool.EventData
		
} # end function
