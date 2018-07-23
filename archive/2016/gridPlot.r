####  Commented and checked by DK starting on July 28, 2015. Calculates metrics for data within
#### grids of specified sizes (mean, sum, median, etc).

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#  
#      
##
###############################################################################################################




gridPlot <- function(Data,domain.poly,lvls,bcol="YlGnBu",border=1,FUN=mean,grid.size=1/60) 
{  
  # Load in the packages
	require(PBSmapping)
	require(RColorBrewer)
  # Give it happy PBSmapping names.
	names(Data)[1:4]<-c("EID","X","Y","Z")
	
	# subset the data to be withing the area of interest
	Data <- subset(Data,EID %in% findPolys(Data,domain.poly)$EID)
#	grid   <- makeGrid(x=seq(floor(min(Data$X))-0.5/60,ceiling(max(Data$X))+0.5/60,1/60),y=seq(floor(min(Data$Y))-0.5/60,ceiling(max(Data$Y))+0.5/60,1/60),projection="LL")
	# make the grid.
	grid   <- makeGrid(x=seq(floor(min(Data$X)),ceiling(max(Data$X)),grid.size),y=seq(floor(min(Data$Y)),ceiling(max(Data$Y)),grid.size),projection="LL")
	
	# locate EventData in grid, if we do not use includedBdry it adds the data on the boundaries to all cells, this
	# means that boundary value will be used in up to 4 different cells, this would also lead to 
	# neighbouring cell clusters showing activity in all boundaries where the data only came from one location.
	# If includeBdry = 1 this sends data to PID of the cell with lower PID value (=2 sends to highest PID cell).
	# 0 excludes boundary data, NULL is the default which places data in all cells, bad!!
	locData<- findCells(Data, grid,includeBdry=1) 
	# Now take all the data within a cell and create some sort of cell summary (mean, sum, median, variance, 
	# whatever value FUN is given in the function call.)
	pdata  <- combineEvents(Data, locData, FUN=FUN)
	#browser()
	lvls<-c(lvls,max(lvls)*100)
	cols   <- brewer.pal(length(lvls),bcol) 
	pdata  <- makeProps(pdata, lvls, "col", cols) 
	pdata$border<-border
	
	list(grid, pdata, cols)
	
}