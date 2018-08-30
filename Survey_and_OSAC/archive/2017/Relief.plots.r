# This function is used to produce relief maps of the bottom to see if it's too crappy to make a tow on a given bottom.
# Currently I believe it is only used for German Bank.
#####  DK August 25th, 2016
# Update history
# Commented, checked  and revised by DK August 25th, 2016


#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1:  survey_design.r
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 1: source("fn/getdis.r")
###############################################################################################################

###############################################################################################################
# Arguments
#tows:      A dataframe containing the tow locations needed to make the relief plots.  Should contain an EID, X, and Y coordinate
#MBdata:    Where to get the Bathymetry data from.  Default = "from.file" which gets the data from a file, I am only aware of this data existing
#           for German Bank at the moment, but seemingly this could be done for SFA29 as well.  Alternative is to supply the information explicitly
#           format would be EID,X,Y,Z, where Z is the depth.
#expd:      Used to expand or shrink the Z axis to make the plots look nicer.  Default = 1 which is no expansion or contraction in scale.
#fig:       What type of figure to produce.  Default = "pdf", Options are to print to "screen", or "png".
#digits:    The precision used for the X and Y coordinates.  Default = 4
#dirt:      The base directory to run from.  Default is whatever you set "direct" to be.
#save.dir:  The directory to save the data to.  Default is paste(direct,yr,"/Survey_Design/",bnk,"/",sep=""), note
#           that "direct", "yr" and "bnk" need to  be defined in your workspace for this to work.
#kms:       Used to rescale the distances, not sure when this would be anything other than the default which is 1
#gerfiles:  The files from which to get the MBdata.  This only works for German Bank whose files are numbered from 1:77 (the default).
#key.dir:   This key is used to determine which of the above gerfiles you should be pulling from.  
#           It is housed on the network at Y:/Offshore scallop/Assessment_fns/Data/Environmental/Ger/Bathymetry/fileKey.csv
#           Default = paste(direct,"Data/Environmental/Ger/Bathymetry/fileKey.csv",sep="") Note you need to define "direct" in your workspace
#tracks:    Do you have the tow tracks?  T/F and defult is F (meaning you don't have the tow tracks)
#trackPath: If you have the tow tracks where are they located? Default paste(direct,"Data/Tow_tracks/",yr,"/Spring/German/",sep="")
#           note you need direct and  yr defined in your workspace.
###############################################################################################################



Relief.plots<-function(tows,MBdata='from.file',expd=1,fig="pdf",digits=4,dirt = direct,
                       save.dir=paste(direct,yr,"/Survey_Design/",bnk,"/",sep=""),
                       kms=1,gerfiles=1:77,key.dir=paste(direct,"Data/Environmental/Bathymetry/Ger/Bathymetry/fileKey.csv",sep=""),
                       tracks=F,trackPath=paste(direct,"Data/Tow_tracks/",yr,"/Spring/German/",sep=""))
{
  
# First load required packages
require(PBSmapping)
require(CircStats)
  
############################# LOAD FUNCTIONS ########################################################
  
# Now load all functions in the program in one location.  
# These are the functions used to within the heart of the code to make stuff happen
source(paste(dirt,"Assessment_fns/Survey_and_OSAC/getdis.r",sep="")) 


# DK doesn't have a clue what this conversion is but something distancy
dpkm <- 0.008983346
# This gets us the aspect ratio accounting for our location on the big ol' sphere
aspr=1/cos(rad(mean(tows$Y)))

# Run this loop for each tow.
for(i in 1:nrow(tows))
{
	print(tows$EID[i]) # Print the tows being plotted
	# Set up the plotting device
	if(fig=="pdf") pdf(paste(save.dir,tows$EID[i],".pdf",sep=''),width = 8.5, height = 8.5,bg = "transparent")
  if(fig=="png") png(paste(save.dir,tows$EID[i],".png",sep=''),width = 8.5, units="in", res=420,height = 8.5,bg = "transparent")
  if(fig=="screen") windows(8.5,8.5)
	# If the Bathyemtry data is coming from a file do this...
	if(MBdata=='from.file')
	{
    if(i ==1) key <- read.csv(key.dir) #Read1
	  # This determines which of the German bank bathymetry files are needed for a particular tow location
		gets<-gerfiles[key$X1 < tows$X[i] & key$X2>tows$X[i] & key$Y1<tows$Y[i] & key$Y2>tows$Y[i]]
		# Set up the location object
		locdata.lst<-NULL
		# If there is data in gets run this 
		if(length(gets)>0)
		{
		  # Run this for each of the files we need to access
			for(f in 1:length(gets))
			{
			  #Read2 Bring in the file with the bathymetry data in it
				tmp <-read.table(paste(dirt,"Data/Environmental/Bathymetry/Ger/Bathymetry/gerbk_5/german_",gets[f],".txt",sep=''),header=T)
				# Now set up the location for the tow, note the dpkm, aspr, and kms adjustments that are necessary.
				locdata.lst[[f]]<-subset(tmp,X<(tows$X[i]+dpkm*aspr*kms)&X>(tows$X[i]-dpkm*aspr*kms)&Y<(tows$Y[i]+dpkm*kms)&Y>(tows$Y[i]-dpkm*kms))
			} # end for(f in 1:length(gets))
		    # Unwrap the location list into a dataframe.
				loc.data<-do.call('rbind',locdata.lst)[,2:4]
		} # end if(length(gets)>0)
		  # If length(gets) = 0 then make loc.data an empty data.frame.
			else loc.data<-data.frame()
			
	} # end if(MBdata=='from.file')
		# If MBdata is not coming from those bathymetry plots then get the location this way...
    else 
		{
		  # Set the names up for the MBdata to be coordinate names
			names(MBdata)<-c("X","Y","Z")
			# Get the boundary for this data
			loc.data<-subset(MBdata,X<(tows$X[i]+dpkm*kms*aspr)&X>(tows$X[i]-dpkm*kms*aspr)&Y<(tows$Y[i]+dpkm*kms)&Y>(tows$Y[i]-dpkm*kms))
		} # end else
		# if there is location data then we can make our plots.
    if(nrow(loc.data)>0)
    {
      # First make the topography from the location data given.
			towMB.lst<-makeTopography(loc.data,digits=digits)
			# If we have more than 1 X and Y coordinate we make our figure
			if(length(towMB.lst$x)>1 && length(towMB.lst$y)>1)
			{
			  # Make a perspective plot based on the towMB data.
				res<-persp(towMB.lst$x,towMB.lst$y,towMB.lst$z,col="lightblue",shade=0.5,border=NA,zlim=c(-120,0),phi=50,expand=expd,
				           ticktype="detailed",zlab="Depth",xlab='',ylab='')
				# if we are provided with the start latitude in the Tows obejct we can add in the tracks
				if('slat'%in% names(tows))
				{
				  # If we are pulling in actual tow track data we do this...
					if(tracks==T)
					{
					  # Now get the tow track
						dis<-dist.coef(tows$EID[i]-1000,path=trackPath,w=c(1:10,9:1),rule=8,smooth=T,plt=F)[[2]]
						# Add the tow track to the figure
						with(dis,lines(trans3d(X,Y,-tows$depth[i],pmat=res),col='red',lwd=2))
					} # end if(tracks == T)
				  # If we aren't pulling in actual tow track data we take the start and end lat/lon to get the tow track.
					else with(tows,lines(trans3d(c(slon[i],elon[i]),c(slat[i],elat[i]),-depth[i],pmat=res)))
					# Add the start points to figure
				  with(tows,points(trans3d(slon[i],slat[i],-depth[i],pmat=res),pch=16,col='red'))
					# Add the end points to figure
				  with(tows,points(trans3d(elon[i],elat[i],-depth[i],pmat=res),pch=16))
				} # end if('slat'%in%names(tows))
				# If we don't have the start lat/lon we just add a point in the middle of the tow location.
				else with(tows,points(trans3d(X[i],Y[i],mean(towMB.lst$z,na.rm=T),pmat=res),pch=16,col='red'))
				# Add a title to the figure, Tow # and it's EID
				title(paste("Tow",i),cex=1.5,adj=0.9)
				title(paste("EID",tows$EID[i]),cex=1.5,adj=0.1)
			} # end if(length(towMB.lst$x)>1&&length(towMB.lst$y)>1)
		} # end if(nrow(loc.data)>0)
	if(fig != "screen") dev.off() # shut down the plotting device if not plotting to screen
} # end for(i in 1:nrow(tows))
} # end function


