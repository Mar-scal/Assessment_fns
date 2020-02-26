# image prep.r ("/Contour/image.prep.r"): preforms an interpolation and returns image data



# Arguments:
#X:             a vector of longitude data
#Y:             a vector of latitude data
#Z:             a vector of the variable we are mapping.
# dat:          a dataframe with 3 columns (longitude, latitude, variable to be mapped)
# res:          resolution of image in decimal degrees.  Default = 0.02
#summary.dat:   print a summary of the results to the screen.  Default = F
#log.dat:       log transform the variable.  Default =T
#method:        the interpolation method.  Default = 'gstat'. Options include
#               gstat:  ordinary kriging
#               krige:  kriging using a covariate
#               interp:  Simple linear or cubic spline interpolation
#matrix.dat:    return the data as a matrix or dataframe.  Default=T, data is returned as a matrix.
#id.par:        inverse distance weighting, used with when method = "gstat".  Default = 0.5
#nmax:          the number of nearest observations that should be used for a kriging prediction.  Default = 7 (see maxdist)
#maxdist:       the maximum distance for which kriging methods will use data.  Default = Inf which uses all data (see nmax)
#linear:        in method = "interp" use linear or spline interpolation.  Default = F which is cubic-spline interpolation. 
#subset.poly:   a polygon containing boundary are locations.  Default = NULL
#covariate.dat: covariate data to assist with interpolation, only used for method = "krige"  Default=NULL
#regrid:        If using covariate data should the grid be reploted using these data.  Default = F
#mod.type:      Model type for the variogram model (vgm).  only used for method = "krige" 
#               Options include "Exp", "Sph", "Gau", "Mat".  Default = "Sph"
#subscale:      How much extra to add to maximum/minimum X and Y values.  Default = 0.01 (slightly less than 1 minute)
#direct:        The directory in which grid.data is located: default = "Y:/Offshore scallop/Assessment/"
#asp:          The aspect ratio:  Default is aspr.
image.prep<-function(X,Y,Z,dat,res=0.02,summary.dat=F,log.dat=T,method='gstat',matrix.dat=T,id.par=0.5,nmax=7,maxdist=Inf,
                     linear=F, subset.poly=NULL, covariate.dat=NULL,regrid=F,mod.type="Sph",subscale=0.01,asp=aspr,
                     direct = "Y:/Offshore scallop/Assessment/")
{
  
  require (splancs)     || stop("You need to install splancs... thanks!")
  require (akima)       || stop("You need to install the akima package for this to work!")
  require (gstat)       || stop("You need to install the gstat package for this to work!")
  require (fields)      || stop("You need to install the fields package for this to work!")
  require (PBSmapping)  || stop("You need to install PBSmapping for this function to work!")
  #Source1 source(paste(direct_fns,"Contour/grid.data.r",sep=""))
  source(paste(direct_fns,"Contour/grid.data.r",sep=""))
  
  # Print information about run time.
  print("image.prep start")
  print(Sys.time())
  
  # If no "dat" specified take X,Y, and Z vectors.
  if(missing(dat)==T) dat<-data.frame(X,Y,Z)
  
  # if we want the log of the data, note we add a tiny little number to the 0's so they aren't errors.
  # DK Note:  I'd suggest we add something larger (more like 0.01) as this can lead to very skewed data 
  # if there are many 0's (i.e. this would be -23, while rest of the data would be far larger, could lead to funny interpolation)
  if(log.dat==T) dat$Z<-log(dat$Z+0.0000000001)
  # get grid for prediction if we don't have a subset or covariate data.  Use subscale to make boundaries slightly larger/smaller
  # than min/max X and Y values
  if(is.null(covariate.dat) == T && is.null(subset.poly)== T)
    {
       dis <- res*asp
       Xs<-seq(min(dat$X)-subscale,max(dat$X)+subscale,dis)
       Ys<-seq(min(dat$Y)-subscale,max(dat$Y)+subscale, res)

    } # end if(is.null(covariate.dat) && is.null(subset.poly))
  
  # If we are subseting the data for the grid use those boundaries to create X and Y values
  if(is.null(subset.poly) == F)
    {
      dis <- res*asp
      Xs<-seq(min(subset.poly$X)-subscale,max(subset.poly$X)+subscale,dis)
      Ys<-seq(min(subset.poly$Y)-subscale,max(subset.poly$Y)+subscale, res)
    } # end if(is.null(subset.poly == F))
  
  # If we are using covariate data for the grid use those boundaries to create X and Y values
  if(is.null(covariate.dat) ==F)
    {
      names(covariate.dat)<-c("X","Y","CoV")
      dis <- res*asp
      Xs<-seq(min(covariate.dat$X),max(covariate.dat$X),dis)
      Ys<-seq(min(covariate.dat$Y),max(covariate.dat$Y), res)
    } # end if(!is.null(covariate.dat))
  
  # Make a dataframe of the x and y data.
  tow.xy <- data.frame(X = dat$X, y = dat$Y)
  # This makes a polygon that bounds our data (using the "convex hull')
  poly <- tow.xy[chull(tow.xy), ]
  names(poly) <- c("X", "Y")
  # include only the points that are found inside the polygon.
  tow.xy <- data.frame(X = dat$X, y = dat$Y)
  poly <- tow.xy[chull(tow.xy), ]
  names(poly) <- c("X", "Y")
  grid.dat <- with(poly, expand.grid(X = Xs, Y = Ys))

  # If we have covariate data use that as our grid.dat object or call the grid.data function and krige the covariate data.
  if(is.null(covariate.dat) == F)
      {
        #Source1 source(paste(direct_fns,"Contour/grid.data.r",sep=""))
        if(regrid==T)grid.dat <- grid.data(covariate.dat,grid.dat)
        if(regrid==F)grid.dat <- covariate.dat
      }	# end if(is.null(covariate.dat) == F)
  
  # interpolation methods
  # If we are using simple/ordinary kriging do this.
  if(method=='gstat')
    {
      # Krige the full dataset
      Z.gstat <- gstat(id = "Z", formula = Z ~ 1, locations = ~ X + Y, data = dat,maxdist=maxdist, 
                       nmax = nmax, set = list(idp = id.par))
      # Predict Z values based on grid.dat subset
      Z.dat<- predict(Z.gstat, grid.dat)
      # Convert teh results into an image for further processing in 'contour'.
      image.data<-makeTopography(Z.dat[c('X','Y','Z.pred')],digits=5)
      # print a summary of the data 
      if(summary.dat==T) print(summary(Z.dat$Z.pred))
      # Return the data as a data.frame.
      if(matrix.dat==F)image.data<-data.frame(X=Z.dat[,1],Y=Z.dat[,2],Z=Z.dat[,4])
      # Return the spatial model results
      spatial.model<-Z.gstat
    } # end if(method=='gstat')
  
  # If we are kriging using a covariate
  if(method=='krige')
    {
      # first calculate the variogram
      v <- variogram(Z ~ CoV, locations = ~ X + Y, data = dat)
      # Get the fit from a vgm model object using the max(v$gamma) from the variogram above.
      # max(v$gamma) = sill, mod.type any of "Exp", "Sph", "Gau", "Mat", v$dist = range, and nugget = min(v$gamma)
      v.fit <- fit.variogram(v, model = vgm(max(v$gamma), mod.type, median(v$dist), min(v$gamma)))
      # Krige the data using the Covariate and the v.fit model
      Z.krige <- krige(formula = Z ~ CoV, locations = ~ X + Y, data = dat, newdata = grid.dat, model=v.fit)
      # Turn this data into something we can use for contour function
      image.data<-makeTopography(Z.krige[,-4])
      
      # Print a summary of data?
      if(summary.dat==T) print(summary(image.data$z))
      # Return the data as a data.frame.
      if(matrix.dat==F)image.data<-data.frame(X=Z.krige[,1],Y=Z.krige[,2],Z=Z.krige[,3])
      
      spatial.model<-v.fit
    } # end if(method=='krige')
  
  # If we are using simple interpolation...
  if(method=='interp')
    {
      # do simple interpolation, either linear or cubic spline.
      image.data<-with(dat, interp(X,Y,Z,linear=linear,xo = Xs, duplicate = "mean", yo = Ys ))
      # Print a summary of data?
      if(summary.dat==T) print(summary(image.data$z))
      # Combine the data into a dataframe with x,y, and z values.
      if(matrix.dat==F)
        {
          y<-sort(rep(image.data$y,length(image.data$x)))
          x<-rep(image.data$x,length(image.data$y))		
          image.data<-data.frame(X=x,Y=y,Z=as.vector(image.data$z))
        }# end if(matrix.dat==F)
        # There is no spatial model.
        spatial.model<-NULL
      } # end if(method=='interp')
  
  # if data was log transformed return it to original state.
  if(log.dat)image.data$z<-exp(image.data$z)
  # Print to screen functin is finished
  print("image.prep end")
  print(Sys.time())
  
  # Return results to function calling.
  return(list(image.data,spatial.model))
  
} # end function
