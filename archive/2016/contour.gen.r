####  Commented and checked by DK starting on July 28, 2015.

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



#	source("Y:/Assessment/2009/r/fn/contour.gen.r")


	#|------------|#
   ##| Arguements |##
	#|------------|#

# ticks = contour lines or strata definitions to be plotted
# color = colors for contour plot
# title = title for contour plot
# key = strata = key diveded by ticks, cont = continuous scale, log.cont = continuous log scale
# aspr = aspect ratio for plot
# interp.method = 'interp' = interp function from akima library, 'gstat' = gstat function from gstat library, 'none' = no interpolation function
# res = resolution for interpolation
# linear, maxdist, nmax, id.par = arguments to be passed to interpolation function
# smooth = logical, default TRUE is to take average over a grid
# sres = resolution for smoothing (grid)
# no.data = how to treat missing data when smoothing, default is to assume zero
# log.data = logical
# surv.z = what survey variable to plot: pre, rec, com, tot, pre.bm, rec.bm, com.bm, tot.bm 
# str.max = maximum value for stratifying variable (all values greater will be set at maximum)
# str.min = minimum value for stratifying variable (all values lesser will be set to NA )
# subset.poly = inclusion polygon to subset spatially
# blank = included blanking distance, beyond which if no data are present zeros are assigned





contour.gen<-function(contour.dat,yl,xl,ticks,nstrata,Polys,Points,Lines,place=0,aspr,res=0.01,smooth=F,smooth.fun=median,blank=T,blank.dist,blank.eff=0,blank.type=1,sres=1/60.1,no.data='0',log.dat=F,interp.method='interp',linear=T,maxdist=Inf,nmax=8,id.par=0.5,covariate.dat=NULL,color="YlGn",color.fun=tim.colors,title='',key='strata',plot=F,units='kg/hm',str.max,str.min,subset.poly=NULL,lab='',unit.lab=NULL,regrid=F,points=1,procedure=1,mod.type="Sph",subset.eff=NA,subscale=res){
	

	print("contour start")
	print(Sys.time())
	image.mod<-NULL

	names(contour.dat)[1:4]<-c("EID","X","Y","Z")
	if(!is.null(covariate.dat))names(contour.dat)[5]<-"CoV"
	dataPoints1<-contour.dat[,1:3]
	if(is.numeric(dataPoints1$EID)==F)dataPoints1$EID<-1:nrow(dataPoints1)
	
	if(interp.method=='krige')blank=F
	
	# required inputs
	require (PBSmapping)
	require (akima)
	require (gstat)
	require (fields)
	require (splancs)
	require (RColorBrewer)
	
	if(is.null(unit.lab))unit.lab=units
	
	# Aspect ratio
	if(missing(aspr)){
		require(CircStats)
		aspr=1/cos(rad(mean(contour.dat$Y)))
		print(paste('Aspect ratio',aspr))
	}
	
	
	# SMOOTHING	
	if(smooth==T){
		if(interp.method!='none'){
			contour.dat<-smooth.bank(contour.dat,fun=smooth.fun,res=sres,aspr=aspr,no.data=no.data,subset.poly=subset.poly,procedure=procedure)
			contour.dat<-contour.dat[!is.na(contour.dat$Z),]
		}
		if(interp.method=='none'){
			image.dat<-smooth.bank(contour.dat,fun=smooth.fun,res=sres,aspr=aspr,matrix=T,no.data=no.data,subset.poly=subset.poly,procedure=procedure)
			names(image.dat)<-c('x','y','z')
		}
	}
	
	# BLANKING
	if(blank==T) {
		if(missing(blank.dist))contour.dat<-blank.bank(contour.dat,aspr=aspr,type=blank.type,eff=blank.eff)
		if(!missing(blank.dist))contour.dat<-blank.bank(contour.dat,blank.dist=blank.dist,aspr=aspr,type=blank.type,eff=blank.eff)
	}
		
	dataPoints2<-contour.dat[,1:3]
	dataPoints2$EID<-1:nrow(dataPoints2)
	# INTERPOLATION	
	
	if(!missing(ticks))if(ticks[1]=='define'){
		ticks<-unique(tick.def(contour.dat$Z,nstrata,str.min,str.max,place))
		nstrata<-length(ticks)-1
	}
	if(missing(nstrata)){
		if(missing(ticks))print("ticks or nstrata must be specified")
		nstrata<-length(ticks)-1
	}
	if(length(color)==1){
		if(nstrata<10)color<-brewer.pal(nstrata,color)
		if(nstrata>=10)color<-color.fun(nstrata)
	}
	
	if(!is.null(subset.poly)){
		if(subset.poly=='square'){
			inset=subscale*0.8
			subset.poly<-with(contour.dat,data.frame(X=sort(rep(c(min(X)-inset,max(X)+inset),2)),Y=c(min(Y)-inset,rep(max(Y)+inset,2),min(Y)-inset)))
		}
	}

	if(interp.method!='none'){
		image.lst<-image.prep(dat=contour.dat,method=interp.method,nmax=nmax,id.par=id.par,log.dat=log.dat,res=res,aspr=aspr,linear=linear,covariate.dat=covariate.dat,regrid=regrid,mod.type=mod.type,subscale=subscale, subset.poly=subset.poly)
		image.dat<-image.lst[[1]]
		image.mod<-image.lst[[2]]
		if(!missing(ticks)){
			if(missing(str.min))str.min<-min(ticks)
			if(missing(str.max))str.max<-max(ticks)
		}
		if(missing(ticks)){
			if(missing(str.min))str.min<-min(image.dat$z,na.rm=T)
			if(missing(str.max))str.max<-max(image.dat$z,na.rm=T)
			ticks<-seq(str.min,str.max,length=nstrata+1)
		}
		image.dat$z[image.dat$z>str.max]<-str.max
		image.dat$z[image.dat$z<str.min]<-subset.eff		##### not tested for other applications!!!
	}
	
		
	# SUBSET POLYGON
	if(!is.null(subset.poly)){
		if(subset.poly=='square'){
			inset=subscale*0.8
			subset.poly<-with(contour.dat,data.frame(X=sort(rep(c(min(X)-inset,max(X)+inset),2)),Y=c(min(Y)-inset,rep(max(Y)+inset,2),min(Y)-inset)))
		}
		Y<-sort(rep(image.dat$y,length(image.dat$x)))
		X<-rep(image.dat$x,length(image.dat$y))
		tmp<-data.frame(X,Y,Z=as.vector(image.dat$z))
		tmp$Z[!with(tmp, inout(cbind(X,Y), subset.poly[c("X","Y")], bound = T))]<-subset.eff
		image.dat$z<-matrix(tmp$Z,length(image.dat$x),length(image.dat$y))
	}

	
	# PLOT
	if(plot){
		
		if(missing(yl))yl=range(image.dat$y)
		if(missing(xl))xl=range(image.dat$x)
		
		#Read1
		land<-read.table("Y:/Maps/data/martimesHIGH.ll",header=T)
		
		#Read2
		bathy<-read.csv("Y:/Maps/r/topex/bathyPoly.csv")

		
		filled.contour(
		image.dat$x,image.dat$y,image.dat$z,
		levels = ticks,
		col = color,
		ylim=yl,xlim=xl,asp=aspr,
		key.axes  = {
			if(key=='log.cont'){
				axis(4,log(c(2:10,seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000)))*(max(ticks)/max(log(ticks))),lab=F,tcl=-0.2)
				axis(4,c(1,log(10^(1:4))*(max(ticks)/max(log(ticks)))),lab=10^(0:4))
				rect(0, log(ticks[-length(ticks)])*(max(ticks)/max(log(ticks))), 1, log(ticks[-1])*(max(ticks)/max(log(ticks))), col = color, border=NA)
			}
			if(key=='cont'){
				axis(4)
				rect(0, ticks[-length(ticks)], 1, ticks[-1], col = color, border=NA)
			}
			if(key=='strata'){
				axis(4,ticks)
			}
		},
		key.title=title(main=unit.lab),
		plot.title=title(main=title),
		plot.axes = {
			axis(1)
			axis(2)
			legend('topright',lab,bty='n',cex=1.5,adj=c(0,0))
			addPolys(land,col="wheat")
			addLines(bathy,col=rgb(0,0,1,0.5))
			if(1%in%points)addPoints(dataPoints1,pch=16,cex=0.5)
			if(2%in%points)addPoints(dataPoints2,pch=1,cex=0.5)
		}
		)
	}

	print("contour end")
	print(Sys.time())

	areas<-areacal(image.dat,units='towable',strata.def=ticks)
	
	output<-list(contour.dat=contour.dat,image.dat=image.dat,image.mod=image.mod,str.def=ticks,areas=areas)
	
	
	return(output)
}

#########################################################
#	 _______________________________________________	#
#	|												|	#
#	|	 ADDITIONAL REQUIRED FUNCTIONS AND OBJECTS	|	#
#	|_______________________________________________|	#
#														#
#########################################################


# tick.def function: ("Y:/Development/Georges/Survey Design/r/fn/tick.def.r"): 
# defines ticks or strata boundaries using the sqrt(f(y)) rule

tick.def<-function(char,nstrata=4,min.str=0,max.str,place=0){
	
	
	bin<-round(char,place)
	if(missing(max.str))max.str<-max(bin)

	bins<-sort(unique(bin[bin>0]))
	vars<-c()
	avg<-c()
	N<-c()
	srN<-c()
 
 
 	for(i in 1:length(bins)){
	 	
		avg[i]<-mean(char[bin==bins[i]])
		vars[i]<-var(char[bin==bins[i]])
		N[i]<-length(char[bin==bins[i]])
		srN[1]<-sqrt(N[1])
		if(i>1) srN[i]<-sqrt(N[i])+srN[i-1]
 	}
 
  	ideal.div<-max(srN)/nstrata*(1:nstrata)
  	ind<-c()	
  	for(i in 1:length(ideal.div)){
		ind[i] <- which(srN==srN[abs(srN-ideal.div[i])==min(abs(srN-ideal.div[i]))])
	}	

	str<-c(min.str,bins[ind]) 
	str[length(str)]<-max.str
	
	str
	
}
# blank.bank.r ("Y:/Development/Georges/Survey Design/r/fn/blank.bank.r"): 
# incorporates blanking distance by including zeros spaced eqully at the average nearest nieghbour distance, default blanking distance is the nearest neighbour distance of the most isolated point

blank.bank<-function(surv.dat,blank.dist,aspr=aspr, type=1, eff=0,scale=0.1){
	
	require(spatstat)
#    browser()
    surv.pts<-subset(surv.dat,select=c('X','Y'))
    xmin<-min(surv.pts$X)
    xmax<-max(surv.pts$X)
    ymin<-min(surv.pts$Y)
    ymax<-max(surv.pts$Y)
    W<-owin(c(xmin-scale,xmax+scale),c(ymin-scale,ymax+scale))
    surv.ppp<-as.ppp(surv.pts,W)
    if(missing(blank.dist))blank.dist<-max(nndist(surv.ppp))
    if(type==1)dims<-c(round((ymax-ymin)/mean(nndist(surv.ppp))*aspr),round((xmax-xmin)/mean(nndist(surv.ppp))))
    if(type==2)dims<-c(round((ymax-ymin)/blank.dist*aspr),round((xmax-xmin)/blank.dist))
    blank.map<-distmap(surv.ppp,dim=dims)
    blank.dat<-data.frame(X=sort(rep(blank.map$xcol,blank.map$dim[1])),Y=rep(blank.map$yrow,blank.map$dim[2]),dist=as.vector(blank.map$v))
    blank.dat<-subset(blank.dat,dist>blank.dist,c('X','Y'))
    blank.dat<-merge(surv.dat,data.frame(EID=1:nrow(blank.dat)+1000,blank.dat,Z=eff),all=T)
    print(paste("blanking distance",blank.dist))

    blank.dat
    
}    

# smooth.bank.r ("Y:/Development/Georges/Survey Design/r/fn/smooth.bank.r"):
# applies a function on spatial data over a grid

smooth.bank<-function(dat,fun=mean,res=0.01,aspr=1.345640,no.data='0',matrix=F,procedure=1,subset.poly=NULL,expand=0.1){
	
	print("smooth.bank start")
	print(Sys.time())
	if(is.null(subset.poly)){
	   	Xs<-seq(min(dat$X)-expand,max(dat$X)+expand,res*aspr)
		Ys<-seq(min(dat$Y)-expand,max(dat$Y)+expand, res)
	}
	
   	if(!is.null(subset.poly)){
	   	Xs<-seq(min(subset.poly$X)-res,max(subset.poly$X)+res,res*aspr)
		Ys<-seq(min(subset.poly$Y)-res,max(subset.poly$Y)+res, res)
	}
	Z<-matrix(NA,length(Xs),length(Ys))
	CoV<-matrix(NA,length(Xs),length(Ys))
	
	for(i in 1:(length(Xs)-1)){
		for(j in 1:(length(Ys)-1)){
			square<-data.frame(X=c(Xs[i],Xs[i+1],Xs[i+1],Xs[i],Xs[i]),Y=c(Ys[j],Ys[j],Ys[j+1],Ys[j+1],Ys[j]))
			sq.dat<-dat[with(dat, inout(cbind(X,Y), square, bound = T)),]
			if(procedure==1){
				if(no.data=='0') Z[i,j]<-sum(fun(sq.dat$Z),na.rm=T)
				if(no.data=='NA') Z[i,j]<-fun(sq.dat$Z)
			}
			if(procedure==2){
				if(no.data=='0') Z[i,j]<-sum(sum(sq.dat$Z,na.rm=T)/sum(sq.dat$CoV,na.rm=T),na.rm=T)
				if(no.data=='NA') Z[i,j]<-sum(sq.dat$Z,na.rm=T)/sum(sq.dat$CoV,na.rm=T)
			}
			if(procedure==3){
				Z[i,j]<-sum(sq.dat$Z,na.rm=T)
			}
			if(procedure==4){
				Z[i,j]<-sum(sq.dat$CoV,na.rm=T)
			}
			if(procedure==5){
				n<-nrow(sq.dat)
				if(no.data=='0') Z[i,j]<-sum(mean(sapply(1:nrow(sq.dat),function(j){nrow(sq.dat)*sum(sq.dat$catch,na.rm=T)/sum(sq.dat$effort,na.rm=T) - (nrow(sq.dat)-1)*(sum(sq.dat$catch[-j])/sum(sq.dat$effort[-j]))})),na.rm=T)
				if(no.data=='NA') Z[i,j]<-mean(sapply(1:nrow(sq.dat),function(j){nrow(sq.dat)*sum(sq.dat$catch,na.rm=T)/sum(sq.dat$effort,na.rm=T) - (nrow(sq.dat)-1)*(sum(sq.dat$catch[-j])/sum(sq.dat$effort[-j]))}))
			}
			Y<-sort(rep(Ys,length(Xs)))
			X<-rep(Xs,length(Ys))
			if(matrix==F)	result<-data.frame(X=X+0.5*res,Y=Y+0.5*res,Z=as.vector(Z))
			if(matrix==T)	result<-list(X=Xs,Y=Ys,Z=Z)
			if(procedure==6){
				Z[i,j]<-fun(sq.dat$Z)
				CoV[i,j]<-fun(sq.dat$CoV)
				if(matrix==F)	result<-data.frame(X=X+0.5*res,Y=Y+0.5*res,Z=as.vector(Z),CoV=as.vector(CoV))
				if(matrix==T)	result<-list(X=Xs+0.5*res,Y=Ys+0.5*res,Z=Z,CoV=CoV)

			}
		}
		if(i %in% round(seq(length(Xs)/100,length(Xs),length(Xs)/100)))print(paste(round(i/length(Xs)*100),"%"))
	}

	
	print("smooth.bank end")
	print(Sys.time())
	
	return(result)
}



# image prep.r ("Y:/Maps/r/fn/image.prep.r"): preforms an interpolation and returns image data

# Arguments:
# dat = a dataframe with 3 columns (longitude, latitude, variable to be mapped)
# aspr = aspect ratio for a given latitude (default is for 45 deg.)
# res = resolution of image in decimal degrees

image.prep<-function(X,Y,Z,dat,aspr=1.345640,res=0.02,summary.dat=F,log.dat=T,method='gstat',matrix.dat=T,id.par=0.5,nmax=7,maxdist=Inf,linear=F, subset.poly=NULL, covariate.dat=NULL,regrid=F,mod.type="Sph",subscale=0.01){

	require (splancs)
	require (akima)
	require (gstat)
	require (fields)
	
	print("image.prep start")
	print(Sys.time())

	if(missing(dat))dat<-data.frame(X,Y,Z)
#	if(ncol(dat)==4)names(dat)[4]<-"co.v"

	if(log.dat)dat$Z<-log(dat$Z+0.0000000001)
   	# get grid for prediction
	if(is.null(covariate.dat)&&is.null(subset.poly)){
		Xs<-seq(min(dat$X)-subscale,max(dat$X)+subscale,res*aspr)
		Ys<-seq(min(dat$Y)-subscale,max(dat$Y)+subscale, res)
	}
	if(!is.null(subset.poly)){
		Xs<-seq(min(subset.poly$X)-subscale,max(subset.poly$X)+subscale,res*aspr)
		Ys<-seq(min(subset.poly$Y)-subscale,max(subset.poly$Y)+subscale, res)
	}
	if(!is.null(covariate.dat)){
		names(covariate.dat)<-c("X","Y","CoV")
		Xs<-seq(min(covariate.dat$X),max(covariate.dat$X),res*aspr)
		Ys<-seq(min(covariate.dat$Y),max(covariate.dat$Y), res)
	}
	tow.xy <- data.frame(X = dat$X, y = dat$Y)
	poly <- tow.xy[chull(tow.xy), ]
	names(poly) <- c("X", "Y")
	grid.dat <- with(poly, expand.grid(X = Xs, Y = Ys))
		
	if(!is.null(covariate.dat)){
		if(regrid==T)grid.dat<-grid.data(covariate.dat,grid.dat)
		if(regrid==F)grid.dat<-covariate.dat
	}	

	# interpolation methods
	if(method=='gstat'){
		Z.gstat <- gstat(id = "Z", formula = Z ~ 1, locations = ~ X + Y, data = dat,maxdist=maxdist, nmax = nmax, set = list(idp = id.par))
		Z.dat<- predict(Z.gstat, grid.dat)
		image.data<-makeTopography(Z.dat[c('X','Y','Z.pred')],digits=5)
		if(summary.dat)print(summary(Z.dat$Z.pred))
		if(matrix.dat==F)image.data<-data.frame(X=Z.dat[,1],Y=Z.dat[,2],Z=Z.dat[,4])
		spatial.model<-Z.gstat
	}
		
	if(method=='krige'){
		v <- variogram(Z ~ CoV, locations = ~ X + Y, data = dat)
		v.fit <- fit.variogram(v, model = vgm(max(v$gamma), mod.type, median(v$dist), min(v$gamma)))
		Z.krige <- krige(formula = Z ~ CoV, locations = ~ X + Y, data = dat, newdata = grid.dat, model=v.fit)
		image.data<-makeTopography(Z.krige[,-4])
		if(matrix.dat==F)image.data<-data.frame(X=Z.krige[,1],Y=Z.krige[,2],Z=Z.krige[,3])
		spatial.model<-v.fit
	}
		
	
	if(method=='interp'){
		image.data<-with(dat, interp(X,Y,Z,linear=linear,xo = Xs, duplicate = "mean", yo = Ys ))
		if(summary.dat)print(summary(image.data$z))
		if(matrix.dat==F){
			y<-sort(rep(image.data$y,length(image.data$x)))
			x<-rep(image.data$x,length(image.data$y))		
			image.data<-data.frame(X=x,Y=y,Z=as.vector(image.data$z))
		}
		spatial.model<-NULL
	}

	
	if(log.dat)image.data$z<-exp(image.data$z)
	print("image.prep end")
	print(Sys.time())

	return(list(image.data,spatial.model))
	
}

# grid.data

grid.data <- function(vdata,gdata){
	print("grid.data start")
	print(Sys.time())
	vname<-names(vdata)[3]
	names(vdata)[3]<-"Z"
	Z.gstat <- gstat(id = "Z", formula = Z ~ 1, locations = ~ X + Y, data = vdata)
	Z.pre<- predict(Z.gstat, gdata)
	names(Z.pre)[3]<-vname
	Z.pre[,-4]
	print("grid.data end")
	print(Sys.time())
}
	


# areacal.r	("Y:/Assessment/Georges a/2009/r/fn/design functions/areacal.r"):
# calculate strata areas

areacal <- function(strata.dat,strata.def=c(0,5,10,20,40),units='km2'){
	strata.area<-c()
	n<-length(strata.def)-1
	
	for(i in 1:n){
		
		strata.area[i]<-length(strata.dat$z[!is.na(strata.dat$z)&strata.dat$z<strata.def[i+1]&strata.dat$z>=strata.def[i]])
		if(i==n) strata.area[i]<-length(strata.dat$z[!is.na(strata.dat$z)&strata.dat$z<=strata.def[i+1]&strata.dat$z>=strata.def[i]])

	}
	
	km<-(strata.dat$y[2]-strata.dat$y[1])*111.2
	unit.area<-km^2
	atow<-800*2.4384/10^6 # area of standard tow in km2
	if(units=='towable') unit.area<-unit.area/atow
	
	strata.area<-strata.area*unit.area
	names(strata.area)<-1:n	
	
	strata.area
	
}



