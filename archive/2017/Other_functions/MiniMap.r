#	source("fn/MiniMap.r")

### MAPS SCALLOP FISHING AREAS IN R!

# ARGUMENTS
# title = plot title
# banks = plots outline of banks
# nafo = plots 'all' or specified nafo areas
# boundaries = plots 'inshore' or 'offshore' management areas
# isobath = plots bathymetry from saved polylines (50,100,150,200m) or specified depths from bathymetry data (bathy.source)
# bathy.source = specifies bathymetry data source, current options are 'CHS' or 'topex'
# shore = shoreline detail ('marHR' = very fine martimes only, 'nwatlHR' = fine NW Atlantic, 'nwatlMR' = medium NW Atlantic, 'worldLR' = coarse world)
# bathcol = isobath line color, default is transparent blue
# grid = size of grid in degrees, default is no grid


MiniMap<-function(ylim=c(41,44),xlim=c(-67.3,-64),xbox,ybox,title='',banks=F,nafo=NULL,boundries='none',isobath=c(100,500),bathy.source='none',shore='nwatlMR',bathcol=rgb(0,0,1,0.3),grid=NULL,plot.lines=T,cex=1,stippling=F,lol=F,land.col='wheat',ocean.col='white',land.twice=T,...){

		
	require(PBSmapping)|| stop("Install PBSmapping Package")
	require(fields)|| stop("Install fields Package")

	if(shore=='marHR')land<-read.table("C:/Maps/data/martimesHIGH.ll",header=T)
	if(shore=='nwatlHR')land<-read.table("C:/Maps/data/nwatlHR.ll",header=T)
	if(shore=='nwatlMR')land<-read.table("C:/Maps/data/nwatlMR.ll",header=T)
	if(shore=='worldLR')land<-read.table("C:/Maps/data/worldLR.ll",header=T)
	attr(land,"projection")<-"LL"

	#par(...)
	
	plotPolys(land,xlim=xlim,ylim=ylim,cex=cex,col=land.col,axes=F,xlab='',ylab='',...)
	polygon(c(xlim*c(1.1,0.9),rev(xlim*c(1.1,0.9))),sort(rep(ylim*c(0.9,1.1),2)),col=ocean.col,border=NA)
	if(lol)addPolys(land,border=bathcol,lwd=6)
	if(!missing(xbox))polygon(c(xbox,rev(xbox)),sort(rep(ybox,2)))	
		
	
	# Bathymetry
	
		if(bathy.source=='topex'){
			isobath=-isobath
			bathy.dat<-read.table("C:/Maps/r/topex/AtlCanbathy.xyz",header=F)
			if(!"bathy.dat"%in%ls(pos=1))assign("bathy.poly",read.csv("C:/Maps/r/usgs/bathy15m.csv"),pos=1, immediate = T)
			names(bathy.dat)<-c("X","Y","Z")
			bathy.dat$X<-bathy.dat$X-360
			bathy.dat<-subset(bathy.dat,X>xlim[1]&X<xlim[2]&Y>ylim[1]&Y<ylim[2])
			bathy.lst<-makeTopography(bathy.dat)
			bathy.cl<-contourLines(bathy.lst,levels=isobath)
			bathy.cp <- convCP(bathy.cl)
			bathy.poly <- bathy.cp$PolySet
			attr(bathy.poly,"projection") <- "LL"
			addLines(bathy.poly,polyProps=data.frame(PID=unique(bathy.poly$PID),col=bathcol))
		}
		if(bathy.source=='USGS'){
			if(!"bathy.poly"%in%ls(pos=1))assign("bathy.poly",read.csv("C:/Maps/r/usgs/bathy15m.csv"),pos=1, immediate = T)
			attr(bathy.poly,"projection") <- "LL"
			addLines(bathy.poly,col=bathcol)
		}
	
		if(bathy.source=='quick'){
			
			if(length(bathcol)==1)bathcol<-rep(bathcol,length=length(isobath))
			for(i in 1:length(isobath)){
				if(boundries=='offshore'){
					d.ll<-read.table(paste("C:/Maps/r/bathy/d",isobath[i],".ll",sep=''),header=T)
					d.ll<-na.omit(d.ll)
					attr(d.ll,"projection") <- "LL"
					addLines(d.ll,col=bathcol[i])
				}
					
				if(boundries!='offshore'){
					d.ll<-read.table(paste("C:/Maps/gmtBase/bathy/d",isobath[i],".ll",sep=''),header=T)
					lines(lat~lon,d.ll,col=bathcol[i])
				}
			}
		}
		

	# NAFO
	if(!is.null(nafo)){
        nafo.xy<-read.csv("C:/Maps/data/nafo.csv")
        if(nafo[1]=='all')nafo<-unique(nafo.xy$label)
        nafo.sel<-subset(nafo.xy,label%in%nafo)
        nafo.dat<-merge(calcCentroid(nafo.sel),nafo.sel[c("PID","label")])[!duplicated(nafo.sel[c("PID","label")]),]
        nafo.dat$label[nafo.dat$label=="5ZC"]<-"5ZEM"
		addPolys(nafo.xy,border='grey')
		addLabels(nafo.dat,col=rgb(0.5,0.5,0.5,0.5),cex=2)
	}
	
	# Banks
	if(banks){
        banks.xy<-read.table("C:/Maps/data/banks.xy",header=T)
        banks.dat<-read.table("C:/Maps/data/banks.dat",header=T)
		addPolys(banks.xy,polyProps=banks.dat)
	}
	
	
	# Boundries
	if(boundries=='scallop'){
		SFA<-read.csv("C:/Maps/data/SFA.csv")
		addLines(SFA)
		SPA<-read.csv("C:/Maps/data/SPA.csv")
		addPolys(SPA)
	}
			
	if(boundries=='offshore'){
		SFA<-read.csv("C:/Maps/data/SFA.csv")
		if(plot.lines)addLines(SFA)
	}
	
	if(boundries=='inshore'){
		inoffdiv<-read.table("C:/Maps/data/inoffdiv.ll",header=T)
		SPA<-read.csv("C:/Maps/data/SPA.csv")
		if(plot.lines){
			addPolys(SPA)
			lines(Y~X,data=inoffdiv,lwd=2)
		} 
	}
	
	if(land.twice)addPolys(land,col=land.col)
	if(stippling)addStipples (land, pch='.')
	EEZ<-read.csv("C:/Maps/data/EEZ.csv")
	addLines(EEZ,lty=4,lwd=2)


	
	# add grid lines
	if(!is.null(grid)){
		x<-seq(floor(xlim[1]),ceiling(xlim[2]),grid)
		y<-seq(floor(ylim[1]),ceiling(ylim[2]),grid)
		gridlines<-makeGrid(x,y,byrow=TRUE,addSID=TRUE,projection="LL",zone=NULL)
		addLines(gridlines,col='grey80',lwd=1)
	}

	box(lwd=2)
	

	title(main=title)
	

}

