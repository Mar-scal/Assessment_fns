#	source("fn/addMiniMap.r")

addMiniMap<-function(xlim,ylim,...,pos='topright',size=2){
	# Aspect ratio
	require(CircStats)
	aspr=1/cos(rad(mean(ylim)))
	print(paste('Aspect ratio',aspr))
	source("fn/MiniMap.r")
	subplot(MiniMap(xlim=xlim,ylim=ylim,...),x=pos,size=c( diff(xlim)/diff(ylim)*size,size*aspr))
}
	



