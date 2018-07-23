####  Commented and checked by DK starting on July 27, 2015.

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
#      1: "get.bathy.r"
#      
##
###############################################################################################################



#	source("fn/ScallopMap.r")

### MAPS SCALLOP FISHING AREAS IN R!

# ARGUMENTS
# area = 'custom' where xlim & ylim are specified or select from area list below
# title = plot title
# banks = plots outline of banks
# nafo = plots 'all' or specified nafo areas
# boundaries = plots 'inshore' or 'offshore' management areas
# isobath = plots bathymetry from saved polylines (50,100,150,200m) or specified depths from bathymetry data (bathy.source)
# bathy.source = specifies bathymetry data source, current options are 'CHS' or 'topex'
# points.lst = points to overlay on map in PBSmapping format - list with 2 elements: 1st element is eventSet (EID, POS, X, Y), 2nd element is eventData (EID, pch, col, etc.) 
# lines.lst = lines to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, lty, col, etc.) 
# poly.lst = polygons to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, border, col, etc.) 
# image.lst = image to overlay on map - list with 3 elements (x, y, z), 'bathymetry' produces image from bathymetry data 
# color.fun = color function for image
# color.adj = color adjustment for bathymetry image
# zlim = zlim for image
# shore = shoreline detail ('marHR' = very fine martimes only, 'nwatlHR' = fine NW Atlantic, 'nwatlMR' = medium NW Atlantic, 'worldLR' = coarse world)
# bathcol = isobath line color, default is transparent blue
# grid = size of grid in degrees, default is no grid
# contours = plots overlaping polygons as contours (same format as poly.lst)


ScallopMap<-function(area='custom',ylim=c(41,44),xlim=c(-67.3,-64),title='',banks=F,nafo=NULL,boundries='offshore',isobath=seq(50,200,50),bathy.source='quick',points.lst=NULL,lines.lst=NULL,poly.lst=NULL,image.lst=NULL,color.fun=tim.colors,color.adj=100,zlim,shore='marHR',bathcol='lightblue',grid=NULL,contours=NULL,plot.lines=T,cex=1,stippling=F,lol=F,land.col='wheat',land.twice=T,...){

		
	require(PBSmapping)|| stop("Install PBSmapping Package")
	require(fields)|| stop("Install fields Package")
	
	# Custom area
	if(area=='custom')	{ ylim=ylim; 			xlim=xlim			}
	
	## Area List
	#offshore
	if(area=='SS')		{ ylim=c(41,47); 		xlim=c(-68,-57)		}
	if(area=='ESS')		{ ylim=c(43,45.4); 		xlim=c(-62.5,-57.4)	}
	if(area=='WSS')		{ ylim=c(41,44); 		xlim=c(-67.3,-64)		}
	if(area=='BBn')		{ ylim=c(42.4,43); 		xlim=c(-66.6,-65.6)	}
	if(area=='BBs')		{ ylim=c(42.25,42.75); 	xlim=c(-66,-65.25)	}
	if(area=='BB')		{ xlim=c(-66.5,-65.25); ylim=c(42.25,43)	}
	if(area=='GB')		{ ylim=c(41.1,42.3); 	xlim=c(-67.3,-65.6)	}
	if(area=='GBb')		{ ylim=c(41.6,42.3); 	xlim=c(-66.7,-65.6)	}
	if(area=='Ger')		{ ylim=c(42.8,43.8); 	xlim=c(-67,-65)		}
	if(area=='Sab')		{ ylim=c(42.8,44.5); 	xlim=c(-62.5,-58.8)	}
	if(area=='West')	{ ylim=c(43,44.1); 		xlim=c(-62.2,-60.4)	}
	if(area=='Mid')		{ ylim=c(44.2,44.9);	xlim=c(-61.3,-60.1) }
	if(area=='Ban')		{ ylim=c(43.7,45.2); 	xlim=c(-60.5,-57)	}
	if(area=='SPB')		{ ylim=c(44.5,47.5);	xlim=c(-58,-55)		}
	
	#inshore
	if(area=='sfa29')	{ ylim=c(43.1,43.8);	xlim=c(-66.5,-65.4) }
	if(area=='gm')		{ ylim=c(44.4,45.2);	xlim=c(-67.2,-66.3) }
	if(area=='inshore')	{ ylim=c(43.1,45.8);	xlim=c(-67.5,-64.3) }
	if(area=='bof')		{ ylim=c(44.25,45.8);	xlim=c(-66.5,-64.3) }
	if(area=='upper')	{ ylim=c(45,46);		xlim=c(-65.2,-64.3) }
	if(area=='mid')		{ ylim=c(44.3,45.5);	xlim=c(-66.6,-64.7) }
	if(area=='spa3')	{ ylim=c(43.62,44.6);	xlim=c(-66.82,-65.8) }
	if(area=='spa4')	{ ylim=c(44.48,44.96);	xlim=c(-66.2,-65.51) }
	if(area=='spa5')	{ ylim=c(44.56,44.78);	xlim=c(-65.82,-65.51) }
	if(area=='spa1')	{ ylim=c(44.5,45.8);	xlim=c(-66.4,-64.3) }
	if(area=='spa1A')	{ ylim=c(44.5,45.3);	xlim=c(-66.4,-64.8) }
	if(area=='spa1B')	{ ylim=c(44.8,45.7);	xlim=c(-66.2,-64.3) }
	if(area=='spa6')	{ ylim=c(44.3,45.25);	xlim=c(-67.4,-66.3) }


  #Read1
	if(shore=='marHR')land<-read.table("Y:/Maps/data/martimesHIGH.ll",header=T)
	
	#Read2
	if(shore=='nwatlHR')land<-read.table("Y:/Maps/data/nwatlHR.ll",header=T)
	
	#Read3
	if(shore=='nwatlMR')land<-read.table("Y:/Maps/data/nwatlMR.ll",header=T)
	
	#Read4
	if(shore=='worldLR')land<-read.table("Y:/Maps/data/worldLR.ll",header=T)
	attr(land,"projection")<-"LL"

	#par(...)
	plotMap(land,xlim=xlim,ylim=ylim,cex=cex,col=land.col,...)
	
	if(lol)addPolys(land,border=bathcol,lwd=6)
	
	# Image
	if(!is.null(image.lst)){
		if(missing(zlim))zlim<-range(image.lst$z,na.rm=T)
		image(image.lst,add=T,col=color.fun(100),zlim=zlim)
	}

	# plot polygons
	if(area=='SPB')polygon(c(-58,-55.98333,-55.98333333,-55.5,-54.9,-58),c(46.20028,46.20028,46.95,47,47.5,47.5),col=rgb(1,0,0,0.4),density=20)

	if(!is.null(contours)){
		contours[[2]]<-subset(contours[[2]],PID%in%contours[[1]]$PID)
		junk<-data.frame(PID=1,POS=1:4,X=c(-62,-61,-61,-62),Y=c(41,41,40,40))
		for(i in unique(contours[[2]]$PID)){
			addPolys(joinPolys(subset(contours[[1]],PID==i),junk,operation="DIFF"),polyProps=contours[[2]])
		}
	}

	if(!is.null(poly.lst)){
		addPolys(poly.lst[[1]],polyProps=poly.lst[[2]])
	}
	
	
	# Bathymetry
	
		if(bathy.source=='CHS'){
		  
		  #Source1
			source("fn/get.bathy.r",local=T)
			bathy.dat<-get.bathy('custom',xl=c(xlim[1]-0.5,xlim[2]+0.5),yl=c(ylim[1]-0.5,ylim[2]+0.5))
			bathy.lst<-makeTopography(bathy.dat)
	 		bathy.cl<-contourLines(bathy.lst,levels=isobath)
			bathy.cp <- convCP(bathy.cl)
			bathy.poly <- bathy.cp$PolySet
			attr(bathy.poly,"projection") <- "LL"
			addLines(bathy.poly,polyProps=data.frame(PID=unique(bathy.poly$PID),col=bathcol))
		}
		if(bathy.source=='topex'){
			isobath=-isobath
			
			#Read5
			bathy.dat<-read.table("Y:/Maps/r/topex/AtlCanbathy.xyz",header=F)
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
		  
		  #Read6
			if(!"bathy.poly"%in%ls(pos=1))assign("bathy.poly",read.csv("Y:/Maps/r/usgs/bathy15m.csv"),pos=1, immediate = T)
			attr(bathy.poly,"projection") <- "LL"
			addLines(bathy.poly,col=bathcol)
		}
	
		if(bathy.source=='quick'){
			
			if(length(bathcol)==1)bathcol<-rep(bathcol,length=length(isobath))
			for(i in 1:length(isobath)){
				if(boundries=='offshore'){
				
				  #Read7
				  d.ll<-read.table(paste("Y:/Maps/r/bathy/d",isobath[i],".ll",sep=''),header=T)
					d.ll<-na.omit(d.ll)
					attr(d.ll,"projection") <- "LL"
					addLines(d.ll,col=bathcol[i])
				}
					
				if(boundries=='inshore'){
				  
				  #Read8
					d.ll<-read.table(paste("Y:/Maps/gmtBase/bathy/d",isobath[i],".ll",sep=''),header=T)
					lines(lat~lon,d.ll,col=bathcol[i])
				}
			}
		}
			
		

	# NAFO
	if(!is.null(nafo)){
		
		    #Read9
        nafo.xy<-read.csv("Y:/Maps/data/nafo.csv")
        if(nafo[1]=='all')nafo<-unique(nafo.xy$label)
        nafo.sel<-subset(nafo.xy,label%in%nafo)
        nafo.dat<-merge(calcCentroid(nafo.sel),nafo.sel[c("PID","label")])[!duplicated(nafo.sel[c("PID","label")]),]
        nafo.dat$label[nafo.dat$label=="5ZC"]<-"5ZEM"
        
		addPolys(nafo.xy,border='grey')
		addLabels(nafo.dat,col=rgb(0.5,0.5,0.5,0.5),cex=2)
	}
	
	# Banks
	if(banks){
		
	      #Read10
        banks.xy<-read.table("Y:/Maps/data/banks.xy",header=T)
       
        #Read11
        banks.dat<-read.table("Y:/Maps/data/banks.dat",header=T)
        
		addPolys(banks.xy,polyProps=banks.dat)
	}
	
	
	# Boundries
	if(boundries=='scallop'){
		
	  #Read12
		SFA<-read.csv("Y:/Maps/data/SFA.csv")
		addLines(SFA)
		
		#Read13
		SPA<-read.csv("Y:/Maps/data/SPA.csv")
		addPolys(SPA)
	}
			
	if(boundries=='offshore'){
		
		#cRead1
		#icj<-read.table("Y:/Maps/data/icj.ll",header=T)
		
	  #cRead2
	  #GBab<-read.table("Y:/Maps/data/GB/GBab.ll",header=T)
		
	  #cRead3
	  #GBa5zejm<-read.table("Y:/Maps/data/GB/GBa5zejm.ll",header=T)
		
	  #cRead4
	  #inoffdiv<-read.table("Y:/Maps/data/inoffdiv.ll",header=T)
		#BBns <- data.frame(lon=c(-65.62486,-65.62486,-65.975),lat=c(43.17905,43,42.308333))
	  
	  #Read14
		SFA<-read.csv("Y:/Maps/data/SFA.csv")
		
		#addLines(subset(SFA,lab!='Ban'))
		if(plot.lines)addLines(SFA)
		

		#lines(Y~X,data=inoffdiv) 
		#lines(lat~lon,data=GBab)
		#lines(lat~lon,data=BBns) 
		#lines(lat~lon,data=icj,lty=4,lwd=3)
		
	}
	
	if(boundries=='inshore'){
		
	  #Read15
		inoffdiv<-read.table("Y:/Maps/data/inoffdiv.ll",header=T)
		
		#cRead5
		#Areas<-read.csv("Y:/Maps/data/NewAreaDefsforISAREADEFS.csv")
		
		#Read16
		SPA<-read.csv("Y:/Maps/data/SPA.csv")

		if(plot.lines){
			addPolys(SPA)
			#addPolys(Areas)
			lines(Y~X,data=inoffdiv,lwd=2)
		} 
		
	}
	
	if(land.twice)addPolys(land,col=land.col)
	if(stippling)addStipples (land, pch='.')
	
	#Read17
	EEZ<-read.csv("Y:/Maps/data/EEZ.csv")
	addLines(EEZ,lty=4,lwd=2)



	# plot points
	if(!is.null(points.lst)){
		addPoints(points.lst[[1]],polyProps=points.lst[[2]])
	}

	# plot lines
	if(!is.null(lines.lst)){
		addLines(lines.lst[[1]],polyProps=lines.lst[[2]])
	}
	
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

