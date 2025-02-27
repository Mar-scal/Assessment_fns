####  Commented and checked by DK starting on July 28, 2015. This function is used to grab the survey tow tracks
####  and calculate the distance of the tows
# Update history
#Commented, checked  and revised by DK March 31, 2016
# DK update in 2021 to get direct_fns default pointing to Github.
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary_data.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#     1: convert.dd.dddd.r
#      
##
###############################################################################################################


# ARGUMENTS

# tows:       Vector containing the tow track numbers we want to extract
# path:       location of the tow track log files.  defaults to .../data/Tow_tracks/2015/GBa/
# w:          used in the sub-function mave, a means of weighting the tow track data if running a moving average across them.
# rule:       the number of sec between records, Options that work are 4,8,20 and "SE". Default is 8 seconds.  SE means the tow
#               location was only taken at the start and end of the tow.
# smooth:     Smooth the tow track data using the mave (see sub-function) moving average function.
# plt:        create a standalone plot. (T/F) default is F
# meh:        If >0 this number is added to the tow numbers so the correct ones are read.  If = 0 it just runs a simple paste command.
#               there is no clear indication of when we would need meh to be > 0.  Default meh=0
# printtow:   prints the tow information to the screen to track progress.
# direct:     Directory to grab the function from.  Defaults = "Y:/Offshore scallop/Assessment/"
# direct_fns: Directory to grab functions from, if missing it goes to github...
### NOTE: the default arguments are used for calculating offshore scallop survey distance coefficients from tows tracked by Ocean Vision
######################  Section 1 - The getdis function ###################################  



dist.coef<-function(tows,path="data/Tow_tracks/2015/GBa/",w=c(1:10,9:1),rule=8,smooth=T,plt=F,meh=0,
                    printtow=F,direct, direct_fns)
{
  #browser()
  require(PBSmapping)  || stop("Install PBSmapping Package")
  #Source1 
  #Now defaults to looking at Github if not specified.
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}# end for(un in funs)
  } # end  if(missing(direct_fns))
  
  if(!missing(direct_fns)) source(paste(direct_fns,"Survey_and_OSAC/convert.dd.dddd.r",sep="")) 
  
  # Initialize objects to use in for loop.
	towtracks<-list(NULL)
	SE<-list(NULL)
	
	# Loop to call in each tow and process the data accordingly.
	for(i in 1:length(tows))
	{
	  # Display on screen the tow being processed in the loop.  
	  if(printtow)print(tows[i])
	  
	  #Read1 Special call to read in oddly formated tow data flat files.
	  if(meh>0) towtrack <- read.table(paste0(path,substr(meh+tows[i],2,nchar(meh)),".log"),skip=5)
	  
	  #Read2 When meh = 0 we just grab the flat files from here sequentially.
	  if(meh==0) towtrack <- read.table(paste(path,tows[i],".log",sep=''),skip=5)
	  
	  # Take all rows except the first, then replace all commas with a space.
	  towtrack<-data.frame(sapply(2:ncol(towtrack),function(x){as.numeric(gsub(',','',towtrack[,x]))}))
	  # Give towtrack some good anmes
	  names(towtrack)<-c("X","Y","Time","Date","Speed","HDG")
	  # Give it a PID, needed for PBSmapping function.
	  towtrack$PID<-i
	  # This is essentially counting the row numbers and giving a name... rownames(towtrack) would accomplish much the same result
	  towtrack$POS<-1:nrow(towtrack)
	  # rename towtrack
	  unsmoothed<-towtrack
	  
	  # If we want to turn our towtracks into smoothed moving averages. This runs the function for both the X and Y data
	  if(smooth==T)
	  {
	    # Run the mave function (see below) w indicates thw weights to give the datapoints
	    towtrack$X<-mave(towtrack$X,w)
	    towtrack$Y<-mave(towtrack$Y,w)
	  } # end if smooth = T
	  # This plot compares the smoothed and the original towtracks
	  if(plt==T)
	  {
	    # Make the objects PBSmapping projections, Lat/Long is their map projection attribute 
	    attr(towtrack,"projection")<-"LL"
	    attr(unsmoothed,"projection")<-"LL"
	    # Ask to open the plot when ready make the plot.  Using PBSmapping Plot here.
	    par(ask=T)
	    plotLines(unsmoothed,xlim=c(min(towtrack$X)-0.001,max(towtrack$X)+0.001),ylim=c(min(towtrack$Y)-0.001,max(towtrack$Y)+0.001))
	    addLines(towtrack,lwd=2)
	  } # end if plt = T
	  # Make a list which has the first and last points of the tow track, a new list item for each file read.
	  SE[[i]]<-cbind(towtrack[1,1:2],towtrack[nrow(towtrack),1:2])
	  # Update the towtracks based on the time between tow readings.  
	  # Take all readings
	  if(rule==4)towtracks[[i]]<-towtrack
	  # Take every second reading
	  if(rule==8) towtracks[[i]]<-towtrack[seq(1,nrow(towtrack),2),]
	  # Take every 5th
	  if(rule==20)towtracks[[i]]<-towtrack[seq(1,nrow(towtrack),5),]
	  # Just take the first and last.
	  if(rule=="SE")towtracks[[i]]<-towtrack[c(1,nrow(towtrack)),]
	} # end for(i in 1:length(tows)){
	
	# turn the SE list into a dataframe
	SE.dat<-do.call("rbind",SE)
	# Same with towtracks
	tracks.dat<-do.call("rbind",towtracks)
	#switch the order of the tracks data around
	tracks.dat<-cbind(tracks.dat[,7:8],tracks.dat[,1:6])
	# Make it a LL projection for PBSmapping
	attr(tracks.dat,"projection")<-"LL"
	# From PBS mapping calculate the track length (rather important later!)
	trackLen<-calcLength(tracks.dat)
	
	# Fix any problematic date information for tracks.dat.
	tracks.dat$Date[nchar(tracks.dat$Date)==7]<-paste(0,tracks.dat$Date[nchar(tracks.dat$Date)==7],sep='')
	
	#Source1 source("Assessment_fns/Survey/convert.dd.dddd.r") Convert the locations to decimal degrees.
	SE.dat<-apply(SE.dat,2,convert.dd.dddd,'deg.min')
	
	
	# Make a list of all the data. Note there is a sneaky little 0.8/trackLen$length why are we doing this here?
	# We are also calculating the bearing with a tapply statement, lots going on in here!
	list(data.frame(tow=tows,dis=0.8/trackLen$length,date=as.Date(subset(tracks.dat,!duplicated(PID))$Date,'%d%m%Y'),
	                brg=with(tracks.dat,tapply(HDG,PID,mean)),slon=SE.dat[,1],slat=SE.dat[,2],elon=SE.dat[,3],elat=SE.dat[,4]),
	                tracks.dat)
} # end get.dis function

######################  End Section 1 - The getdis function ###################################  


######################  Section 2 - The mave function ###################################  

# moving average of x with weigts w and tapered ends by Bob Moh converted to R by Brad Hubley
# Could be replaced with the TTR packages WMA function, tested and results are virtually identical.

mave<-function(x,w){
  # This adds 0's to the end of the  towtrack, length-1 of the weights (w) 
  t = c(x,rep(0,(length(w) - 1)))
  # Indicator variables, 1's for real x's, 0 for the 0's filled in above
  z = c(rep(1,length(x)),rep(0,(length(w) - 1)))
  # Giant transposed crossproduct matrix dim = (length(w), length(t))
  tm = tcrossprod(w,t)
  # Giant transposed crossproduct matrix dim = (length(w), length(z))
  zm = tcrossprod(w,z)
  # Run the loop, note index is by the length of w-1
  for(i in 1:(length(w)-1))
  {
    # Both of these (tm and zm) grab 2 sets of values: first value is an ever increasing number of points from row i+1 and a variable number 
    #of columns (increasing), the second grabs an ever decreasing number of points the values from row i+1 and a variable number 
    #of columns (decreasing)
    # Essentially this is a huge covariance type matrix with original points multiplied by the weights they should get at each step along the way
    # The sum of each column of tm divided by the sum of the same zm column gives the smoothed weighted moving average 
    tm[i+1,] =c(tm[i+1,(length(t)-i+1):length(t)],tm[i+1,1:(length(t)-i)])
    zm[i+1,] =c(zm[i+1,(length(t)-i+1):length(t)],zm[i+1,1:(length(t)-i)])
  } # end for(i in 1:(length(w)-1))
  #  Add the columns and divide the results for tm by zm
  zt = colSums(tm)/colSums(zm)
  # Used to trim the data that was added to facilitate the calculations
  id = trunc(length(w)/2)
  # Return the smoothed data.
  return(zt[-c(1:id,(length(zt)-id+1):length(zt))])
}# end mave function


 ##example
 #d1.216=dist.coef(1:216,"Y:/Offshore scallop/Amy/Surveys/TE10-GB10/Alan/Tow Tracks/")
