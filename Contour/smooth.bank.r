####  Created as standalone by DK September 2015. This function is used within contour.gen.r to smooth
#### the data by one of several possible methods.

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

#	source(".../contour/smooth.bank.r")

# Arguments
#dat:              spatial data to smooth.
#fun:              The function used to smooth data.  Default = mean, want a measure of central tendency so really either mean or median are options
#res:              The resolution for the smoothing (grid), default = 0.01
#aspr:             aspect ratio needed for calculations.  Default = 1.345640 which is very nearly equivalent to a latitude of 42 degrees
#no.data:          How to treat missing data when smoothing,  Options are "NA" or "0" default is to assume zero (not the letter O).
#matrix:           Determines whether our output is a dataframe or a list.  Default = F which is a dataframe
#procedure:        Options for how to calculate the smooth.  Can be any number from 1-6 Default = 1
#                  Procedure =1:  calculate the "fun" (i.e. mean/median) for Z[i,j] , if no.data = "0"  take the sum, if no.data = NA just take fun' result
#                  Procedure =2:  divide the sum of the sum of Z data and divide by sum of the covariance
#                  Procedure =3:  simply the sum of the of the Z data
#                  Procedure =4:  simply calculate the sum of the Covariance data
#                  Procedure =5:  Use the sum of the mean difference between CPUE data.
#subset.poly:      The polygon region to include, a subset of 'dat'  default == NULL
#expand:           used to make X and Y data a little bit bigger than in the raw data helps with calculations around the edges.  default = 0.1




smooth.bank<-function(dat,fun=mean,res=0.01,aspr=1.345640,no.data='0',matrix=F,procedure=1,subset.poly=NULL,expand=0.1)
{
  
  require(splancs) || stop("You need the package called 'splancs'... thanks!")
# Print some output  
  print("smooth.bank start")
  print(Sys.time())
  
  # IF subset.poly is not specified use all the data
  if(is.null(subset.poly) ==T)
    {
      Xs<-seq(min(dat$X)-expand,max(dat$X)+expand,res*aspr)
      Ys<-seq(min(dat$Y)-expand,max(dat$Y)+expand, res)
    } # end if(is.null(subset.poly))
  
  # If subset.poly is specified subset data accordingly.
  if(is.null(subset.poly)==F)
    {
      Xs<-seq(min(subset.poly$X)-res,max(subset.poly$X)+res,res*aspr)
      Ys<-seq(min(subset.poly$Y)-res,max(subset.poly$Y)+res, res)
    } # end if(is.null(subset.poly)==F)
  
  # make Z and Covariance matrices of proper size.
  Z<-matrix(NA,length(Xs),length(Ys))
  CoV<-matrix(NA,length(Xs),length(Ys))
  
  # Loop through the X data then the Y data...
  for(i in 1:(length(Xs)-1))
    {
      for(j in 1:(length(Ys)-1))
        {
          # Make a data from of the X's and Y's, basically a polygon of the boundary for this smooth.
          square<-data.frame(X=c(Xs[i],Xs[i+1],Xs[i+1],Xs[i],Xs[i]),Y=c(Ys[j],Ys[j],Ys[j+1],Ys[j+1],Ys[j]))
          # Now subset the data to be only that data within these boundaries
          sq.dat<-dat[with(dat, inout(cbind(X,Y), square, bound = T)),]
          # here we take the "fun" and grab the sum of that but there should be only one mean output, why the sum?
          if(procedure==1)
            {
              if(no.data=='0') Z[i,j] <- sum(fun(sq.dat$Z),na.rm=T)
              if(no.data=='NA') Z[i,j] <- fun(sq.dat$Z)
            } # end if(procedure==1)
          # here we take the sum of the sum (again the inner sum should just be one number so why sum that), then divide by the Covariance
          if(procedure==2)
            {
              if(no.data=='0') Z[i,j]<-sum(sum(sq.dat$Z,na.rm=T)/sum(sq.dat$CoV,na.rm=T),na.rm=T)
              if(no.data=='NA') Z[i,j]<-sum(sq.dat$Z,na.rm=T)/sum(sq.dat$CoV,na.rm=T)
            } # end if(procedure==2)
          # here we just take the sum of the Z values
          if(procedure==3)
            {
              Z[i,j]<-sum(sq.dat$Z,na.rm=T)
            }# end if(procedure==3)
          # here we take the sum of the covariances
          if(procedure==4)
            {
              Z[i,j]<-sum(sq.dat$CoV,na.rm=T)
            } # end if(procedure==4)
          # Here we calculate the sum of this odd beast n*CPUE - (n-1)*CPUE[-j], this last term excludes the jth observation.
          if(procedure==5)
            {
              n<-nrow(sq.dat)
              if(no.data=='0') Z[i,j] <-sum(mean(sapply(1:nrow(sq.dat),function(j){nrow(sq.dat)*sum(sq.dat$catch,na.rm=T)/sum(sq.dat$effort,na.rm=T) - 
                  (nrow(sq.dat)-1)*(sum(sq.dat$catch[-j])/sum(sq.dat$effort[-j]))})),na.rm=T)
              if(no.data=='NA') Z[i,j]<-mean(sapply(1:nrow(sq.dat),function(j){nrow(sq.dat)*sum(sq.dat$catch,na.rm=T)/sum(sq.dat$effort,na.rm=T) - 
                  (nrow(sq.dat)-1)*(sum(sq.dat$catch[-j])/sum(sq.dat$effort[-j]))}))
            } # end if(procedure==5)
          
          # Now sort repeat each Y value by the total number of Xs and sort from smallest to largest.
          Y<-sort(rep(Ys,length(Xs)))
          # and vice versa
          X<-rep(Xs,length(Ys))
          # If we want our result as a data frame do the first line, and as a list is the second.  Notice the difference between the X values
          # using these different methods, this may be incorrect given what we do with procedure =6 below.
          if(matrix==F)	result<-data.frame(X=X+0.5*res,Y=Y+0.5*res,Z=as.vector(Z))
          # DK Note:  Why do we return the un-modified Xs here but in procedure 6 we return the modified values?
          if(matrix==T)	result<-list(X=Xs,Y=Ys,Z=Z)
          
          # The final option, take the "fun" of the Z and Covariance data, in this case we also return the covariance back with the data.
          if(procedure==6)
            {
              Z[i,j]<-fun(sq.dat$Z)
              CoV[i,j]<-fun(sq.dat$CoV)
              if(matrix==F)	result<-data.frame(X=X+0.5*res,Y=Y+0.5*res,Z=as.vector(Z),CoV=as.vector(CoV))
              # DK note:  Noticed difference here with above DK note, is this a mistake?
              if(matrix==T)	result<-list(X=Xs+0.5*res,Y=Ys+0.5*res,Z=Z,CoV=CoV)
             } # end if(procedure==6)
        } # end j loop
        # Print the results when i matches the below.
       if(i %in% round(seq(length(Xs)/100,length(Xs),length(Xs)/100)))print(paste(round(i/length(Xs)*100),"%"))
      } # end i loop
  
  # Print that function is finished.
  print("smooth.bank end")
  print(Sys.time())
  
  # return the restuls
  return(result)
} # end function
