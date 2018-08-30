#source("fn/FishBub.r")
FishBub<-function(dat,vari="hm",scaler=10,...){
	datvar<- as.vector(unlist(dat[vari]))
	source("fn/ScallopMap.r")
	ScallopMap(...)
	for(i in 1:nrow(dat)){
		points(lat~lon,dat[i,],pch=21,bg=rgb(0,1,0,0.1),col=rgb(0,1,0,1),cex=sqrt(datvar[i])/scaler)
	}
}


#FishBub(subset(fish.dat,bank=="GBa"&months(as.Date(date))=="July"&year==2001),area="GB")

