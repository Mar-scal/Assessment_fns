################################################################################################################
##### This function will retrieve the survey strata for the inshore and offshore Scallop fishery from their 
##### respective SQL databases.  
####  Version 1 created by Dave Keith August 6th, 2015
#	Revision history
# May 16, 2016, revised to enable RODBC to work with R-64 bit. (DK)
################################################################################################################
## Function Arguements
##  1:  loc:  Three options, inshore, offshore, all, defaults to all
##  2:  plot.add: True (default) is used if function is being called by another function with a plot device already opened,  
##      False used if making a unique plot with just land and survey strata shown, to work as a stand-alone function set this to False
##  3:  un:  Username for your SQL call, please set this up in your R-profile and do not enter it directly into the function, 
##  4:  pwd:  Password for your SQL call, please set this up in your R-profile and do not enter it directly into the function, 
##  5:  db.con:  SQL database connection name, user specific, default is ptran
##  6:  strata.color:  Currently uses pastel.colors(n=64) from RPMG library to generate the color table for the strata
##  7:  direct:  the directory to choose.  Default is "Y:/Offshore scallop/Assessment/"

# rm(list=ls(all=T))
strataPlot<-function(loc = "inshore", plot.add=T, un = un.ID, pw=pwd.ID, db.con = "ptran",strata.colors = pastel.colors(64,seed=2),
                     strata.cex=0.5,direct = "Y:/Offshore scallop/Assessment/")
{
################################### Start Load Packages ##############################################################
###
require(PBSmapping) || stop("Install PBSmapping Package")
require(RPMG)|| stop("Install RPMG Package")
require(RODBC) || stop("Install RODBC Package")
###
################################## End Load Packages   #######################################################



################################### Section 1  Import + Process Data ##############################################################
# Load and process the data from the SQL database.  This ensures that we loc using the latest strata boundaries for both
# the inshore and offshore fisheries.
##########################################################################################################


################################################################################################################################
# Now import and process the inshore strata 
#################################################################################################################################

if(loc == "inshore" || loc == "all")
{

  # Open the SQL connections.  
  RODBCconn <- odbcConnect(db.con, uid=un , pwd=pw,believeNRows=FALSE)  
  
  
# This is the query to grab the strata for the inshore fishery.
quer.in <- paste(
  "SELECT STRATA_ID, ORDINAL, LATITUDE, LONGITUDE ",
  "FROM SCALLSUR.SCSTRATADEFS       ",
  sep=""
)

# Run the query and make it an object in R, then close the connections
strata.in <- sqlQuery(RODBCconn, quer.in)  
odbcClose(RODBCconn)

# Now do some pre-processing so we can convert data into polysets for PBSmapping
# Rename the headers in the strata for PBSmapping
colnames(strata.in) <- c("PID","POS","Y","X")

# Check for NA's and remove any found, the is.finite identifies which parts of the matrix are NA's (not finite)
# The !rowSums then logically identifies all rows with non-finite sums making them FALSE and allowing for them to be removed from data.frame
strata.in <- strata.in[!rowSums(!is.finite(as.matrix(strata.in))),]

# Get PID's in numerical order, with no SID this is all that is required for the Inshore data
strata.in.PBS <- strata.in[order(strata.in$PID),]

# Now make this a polyset for PBSmapping
attr(strata.in.PBS,"projection") <- "LL"

} # End if(loc == "inshore" || loc == "all")

################################################################################################################################
# Now import and process the offshore strata 
#################################################################################################################################

if(loc == "offshore" || loc == "all")
{
  # Open the SQL connections.  
  RODBCconn <- odbcConnect(db.con, uid=un , pwd=pw,believeNRows=FALSE)  
  
  

# This is the query to grab the strata for the offshore fishery
quer.off <- paste(
  "SELECT STRATA_ID, SECONDARY_ID, ORDINAL, LATITUDE, LONGITUDE  ",
  "FROM SCALOFF.OSSTRATA          ",
  #"WHERE SURVEY_NAME = 'GB2014.2' ",
  sep=""
)

# Run the query and make it an object in R, then close the connections
strata.off <- sqlQuery(RODBCconn, quer.off)  
odbcClose(RODBCconn)

# Now do some pre-processing so we can convert data into polysets for PBSmapping
# Rename the headers in the strata for PBSmapping
# Notice the offshore has SID's due to complex nature of the survey strata.
colnames(strata.off) <- c("PID","SID","POS","Y","X")

# Check for NA's and remove any found, the is.finite identifies which parts of the matrix are NA's (not finite)
# The !rowSums then logically identifies all rows with non-finite sums making them FALSE and allowing for them to be removed from data.frame
strata.off <- strata.off[!rowSums(!is.finite(as.matrix(strata.off))),]

# Unforntunately for the offshore strata the SQL database is not set up to put the data how PBSmapping needs it.
# Since I don't have access to the tables the temporary (permenant?) fix is the convoluted for loop to reorder the data 
# as needed by PBSmapping
# For offshore this is just the first step before starting the loop
strata.off <- strata.off[order(strata.off$PID),]

# Setting up variables for a loop.
PIDs <- unique(strata.off$PID)
num.PID <- length(PIDs)
strata.off.PBS <- NULL

# For loop used to reorder the data in the offshore strata so that in conforms to PBSmapping requirements.
for (i in 1:num.PID)
{
  # Make a temporary object to store the [i]th primary Strata ID at a time
  temp1 <- strata.off[strata.off$PID == PIDs[i],]
  # Get all of the secondary ID's for this [i]th primary Strata ID.
  SIDs <- sort(unique(temp1$SID))
  num.SID <- length(SIDs)
  # Now move through the [j]th secondary ID's for within [i]th primary Strata ID
  for(j in 1:num.SID)
  {
    # create a second temporary object with the [i]th primary Strata ID and [j]th secondary ID data
    temp2 <- temp1[temp1$SID == SIDs[j],]
    # If the POS value is increasing this is a polygon and we want these ordered with from least to greatest
    if(temp2$POS[2] > temp2$POS[1]) temp2 <- temp2[order(temp2$POS),]
    # if the POS value is declining this is a hole and we want these ordered from greatest to least 
    if(temp2$POS[2] < temp2$POS[1]) temp2 <- temp2[order(temp2$POS,decreasing =T),]
    
    strata.off.PBS <- rbind(strata.off.PBS,temp2)
  } # end for(j in 1:num.SID)
} # end for (i in 1:num.PID)

# Finally make these into PolySet's for PBSmapping
attr(strata.off.PBS,"projection") <- "LL"


} # end if(loc == "offshore" || loc == "all")

  

################################### End Section 1  Import + Process Data ##############################################################


################################### Section 2  Plot Data ##############################################################
#  Plot the survey strata either as an addition to a already existing plot or as a stand alone plot.
#
################################### Section 2  Plot Data ##############################################################


# If we want to create a unique plot of the inshore survey strata + land
if(plot.add == F)
{
  
  # Set the boundaries up for inshore only
  if(loc =="inshore")
    {
    # Generic inshore boundaries
    ylim=c(43.1,45.8);	xlim=c(-67.5,-64.3)
    } # end   if(loc =="inshore")

  # Set the boundaries up for offshore/all
  if(loc !="inshore")
    {
    # Generic offshore boundaries
    ylim=c(40,46);         xlim=c(-68,-55)
    
    }  #end if(loc !="inshore")

  #Read1 Add in the land, use the high resolution Maritimes data and make it into a PolySet
  land<-read.table(paste(direct,"Data/Maps/approved/Coastline/martimesHIGH.ll",sep=""),header=T)
  attr(land,"projection")<-"LL"

  # Plot the land
  plotMap(land,xlim=xlim,ylim=ylim,col="wheat")

  # Add a thick box + title to plot
  box(lwd=2)
  
  # Put the right title on plot
  
  if(loc == "all") title("All Survey Strata")
  if(loc == "inshore") title("Inshore Survey Strata")
  if(loc == "offshore") title("Offshore Survey Strata")
  } # end if(plot.add = F)


# Add the inshore survey strata to the currently called plot (either external to the function or internal to function). 
# This will return an error if plot.add=T but no plot has been called yet.
if(loc !="offshore")
  {
  addPolys(strata.in.PBS,col=strata.colors)
  } # end if(loc !="offshore")

# Add the offshore survey strata to the currently called plot (either external to the function or internal to function). 
# This will return an error if plot.add=T but no plot has been called yet.
if(loc != "inshore")
  {
  addPolys(strata.off.PBS,col=strata.colors)#,col=c("blue","red","grey","green","yellow","pink"))


  
    } # end if(loc !="inshore")

  # Plot the inshore strata boundaries, and return the strata.labels, 
  # this is primarily for use in ScallopMap to overlay strata labels overtop of the land layer
if(loc == "inshore") 
{
  
  #Read2 I also want to add in the strata labels for the inshore, at least the ones I have!
  strata.labels <- read.table(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/inshore_strata_labels.csv",sep=""),sep =",",header=T)
  attr(strata.labels,"projection") <- "LL"
  
  # If we are creating a stand alone plot do this
  if(plot.add==F)
    {
    # Add most of the labels
    addLabels(strata.labels[strata.labels$label != "SPA 4:    0 - 2 miles" & strata.labels$label != "SPA 1: 0-2 miles", ],cex = strata.cex)
    # Need to get fancy to add these two labels
    addLabels(strata.labels[strata.labels$label == "SPA 4:    0 - 2 miles",] ,cex=strata.cex,srt =38)
    addLabels(strata.labels[strata.labels$label == "SPA 1: 0-2 miles", ],cex= strata.cex,srt =33)
    } # end if(plot.add==F)
  
  # if we are adding the strata to a seperate plot we need to export the strata.labels so they can be plotted appropriately
  if(plot.add== T) assign("strata.labels",strata.labels,pos=1)
  
  
} # end if(loc == "inshore") 

  
  
  
  
} # End StrataPlot Function
