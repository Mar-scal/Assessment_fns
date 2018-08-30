VMSgif<-function(VMSdat,fish.dat,tail=7,pie.scale=500,wd=600,ht=800,...){
	
	require(animation)# for creating gif
	require(TeachingDemos) # for adding pie charts to map
	
	# VMS data
	VMSdat$julian<-julian(as.Date(VMSdat$vmsdate),origin=min(as.Date(VMSdat$vmsdate))-1)
	
	# log data
	if(!missing(fish.dat)){
		fishTmp.dat<-subset(fish.dat,year%in%unique(VMSdat$year))
		fishTmp.dat$julian<-julian(as.Date(fishTmp.dat$date),origin=min(as.Date(VMSdat$vmsdate))-1)
		GBaSum<-sum(subset(fishTmp.dat,bank=="GBa",pro.repwt))
		GBbSum<-sum(subset(fishTmp.dat,bank=="GBb",pro.repwt))
		BBnSum<-sum(subset(fishTmp.dat,bank=="BBn",pro.repwt))
		GerSum<-sum(subset(fishTmp.dat,bank=="Ger",pro.repwt))
		GBaS<-sqrt(GBaSum/pi)/pie.scale
		GBbS<-sqrt(GBbSum/pi)/pie.scale
		BBnS<-sqrt(BBnSum/pi)/pie.scale
		GerS<-sqrt(GerSum/pi)/pie.scale
	}
	
	source("fn/ScallopMap.r")
	boxes<-read.csv("data/GBa/boxes.csv")

	### GIF animations ###
	## set some options first
	oopt = ani.options(interval = 0.4, nmax = length(unique(VMSdat$vmsdate)), outdir=getwd())
	## use a loop to create images one by one
	saveGIF({
	for (i in 1:ani.options("nmax")) {
	ScallopMap(...,title=min(as.Date(VMSdat$vmsdate))+i-1)
	addPolys(subset(boxes,PID%in%9:10),border=rgb(0,0,0,0.5))
	points(lat~lon,subset(VMSdat,julian<=i&julian>i-tail),pch=16,cex=0.1,col=rgb(1,0,0,0.3)) # add VMS
	 
	if(!missing(fish.dat)){
		 # Catch pie charts
		 GBaFSF<-sum(subset(fishTmp.dat,julian<i&bank=="GBa")$pro.repwt)
		 subplot(pie(c(GBaSum-GBaFSF,GBaFSF),labels=NA),-65,41.5,size=rep(GBaS,2))
		 
		 GBbFSF<-sum(subset(fishTmp.dat,julian<i&bank=="GBb")$pro.repwt)
		 subplot(pie(c(GBbSum-GBbFSF,GBbFSF),labels=NA),-66.3,42.25,size=rep(GBbS,2))
		 
		 GerFSF<-sum(subset(fishTmp.dat,julian<i&bank=="Ger")$pro.repwt)
		 subplot(pie(c(GerSum-GerFSF,GerFSF),labels=NA),-66.8,43.25,size=rep(GerS,2))
		 
		 BBnFSF<-sum(subset(fishTmp.dat,julian<i&bank=="BBn")$pro.repwt)
		 subplot(pie(c(BBnSum-BBnFSF,BBnFSF),labels=NA),-66.8,42.7,size=rep(BBnS,2))
	 }
	if(i == ani.options("nmax"))	 points(lat~lon,VMSdat,pch=16,cex=0.1,col=rgb(1,0,0,0.3)) # add VMS
 
	 
	ani.pause() ## pause for a while (’interval’)
	}
	}, interval = 0.05, movie.name = "VMS.gif", ani.width = wd, ani.height = ht)
}


