####  Commented and checked by DK September, 2015. Calculates metrics for data within
#### grids of specified sizes (mean, sum, median, etc).
# Update history
#Commented, checked  and revised by DK March 31, 2016
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "Survey_Summary_figures.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#  
#      
##
###############################################################################################################

# Arguments
#Data:          The data, the data should be structured in 4 columns, "ID", "X coordinates", "Y coordinates", and
#               "Variable of interest (Z)"
#domain.poly:   The spatial extent of the data we are interested in.  A polygon of some sort covering the area of interest.
#lvls:          The categories of the variable of interest (Z), used for creating a bin of colors for the plot.
#bcol:          The bin color palette, see ?brewer.pal help for options.  Default ="YlGnBu"
#border:        Adds a column to the output, not sure what this is used for.  Default = 1
#FUN:           How to summarize the data within a grid cell.  Default = mean any function could be called.
#grid.size      The size of the grid.  Default = 1/60 (i.e. 1 minute if data are lat/lon)


gridPlot <- function(Data,domain.poly,lvls,bcol="YlGnBu",border=1,FUN=mean,grid.size=1/60) 
{  
  # Load in the packages
	require(PBSmapping)  || stop("You need to install PBSmapping package to run this function")
	require(RColorBrewer) || stop("You need to install RColorBrewer package to run this function")
  # Give it happy PBSmapping names.
	names(Data)[1:4]<-c("EID","X","Y","Z")
	
	# subset the data to be withing the area of interest
	Data <- subset(Data,EID %in% findPolys(Data,domain.poly)$EID)
#	grid   <- makeGrid(x=seq(floor(min(Data$X))-0.5/60,ceiling(max(Data$X))+0.5/60,1/60),y=seq(floor(min(Data$Y))-0.5/60,ceiling(max(Data$Y))+0.5/60,1/60),projection="LL")
	# make the grid.
	grid   <- makeGrid(x=seq(floor(min(Data$X)),ceiling(max(Data$X)),grid.size),y=seq(floor(min(Data$Y)),ceiling(max(Data$Y)),grid.size),projection="LL")
	
	# Note that in findCells() if we do not use includedBdry it adds the data on the boundaries to all cells, this
	# means that boundary value will be used in up to 4 different cells, this would also lead to 
	# neighbouring cell clusters showing activity in all boundaries where the data only came from one location.
	# If includeBdry = 1 this sends data to PID of the cell with lower PID value (=2 sends to highest PID cell).
	# 0 excludes boundary data, NULL is the default which places data in all cells, bad!!
	locData<- findCells(Data, grid,includeBdry=1) 
	# Now take all the data within a cell and create some sort of cell summary (mean, sum, median, variance, 
	# whatever value FUN is given in the function call.)
	pdata  <- combineEvents(Data, locData, FUN=FUN)
	
	# Reset the levels giving the max level a value 100* actual value.
	lvls<-c(lvls,max(lvls)*100)
	# Make the color palette
	cols   <- brewer.pal(length(lvls),bcol) 
	# Give the values of the data the appropriate color for the spatial plot, lvls gives the cut point for the variable
	pdata  <- makeProps(pdata, lvls, "col", cols) 
	# Add a string of whatever border is set to for the data.
	pdata$border<-border
	
	# Return the data to function calling it.
	list(grid, pdata, cols)
	
} # end function