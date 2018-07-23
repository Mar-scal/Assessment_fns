##### SURVEY DESIGN
options(stringsAsFactors=F)
require(PBSmapping)
require(RColorBrewer)

setwd("Y:/Alan/2016/May_stations")
		
atow<-800*2.4384/10^6 # area of standard tow in km2


###### Sable (historical survey - 5 strata):


		Sab.surv.polyset<-read.csv("Sab.surv.polyset.csv")
		attr(Sab.surv.polyset,"projection")<-"LL"
		str.def<-c(5, 28, 66, 114, 174, 300)
		Sab.surv.polydata <- data.frame(PID=1:5,col=brewer.pal(6,"YlGn")[-1],border=NA,PName=paste("strata",1:(length(str.def)-1),': >=',str.def[-length(str.def)],' & <',str.def[-1],' scallops/tow',sep=''))
		
		source("fn/alloc.poly.r")
		Sab.towlst<-alloc.poly(poly.lst=list(Sab.surv.polyset, Sab.surv.polydata),ntows=100,bank.plot="Sab",pool.size=3,mindist=3,lplace='bottomright')

		write.csv(Sab.towlst$Tows,"SabTowlist2016.csv",row.names=F)
		
		#Sab.towlst<-read.csv("SabTowlist2016.csv")
	#	#pdf("sab.2016.pdf")
#		source("fn/ScallopMap.r")
#		ScallopMap("Sab",bathcol=rgb(0,0,1,0.3),poly.lst=list(Sab.surv.polyset,Sab.surv.polydata),cex=1.2,title="Sable Survey 2016")
#		legend('bottomright',legend=Sab.surv.polydata$PName,pch=21,pt.bg=Sab.surv.polydata$col,bty='n',cex=1, inset = .01)
#		addPoints(Sab.towlst,pch=21, cex=1,bg=Sab.surv.polydata$col[Sab.towlst$Poly.ID])
#    #dev.off()
    #source("fn/ScallopMap.r")
#		ScallopMap(ylim=c(43,44.1),xlim=c(-62.2,-59.8),bathcol=rgb(0,0,1,0.3),contour=list(Sab.surv.polyset,Sab.surv.polydata),title='Sable Survey 2016',cex=1.2)
#	   legend('bottomright',legend=Sab.surv.polydata$PName,pch=21,pt.bg=Sab.surv.polydata$col,bty='n',cex=1, inset = .02)
#		addPoints(Sab.towlst,pch=21, cex=1,bg=Sab.surv.polydata$col[Sab.towlst$Poly.ID])


		#Sab.surv.polyset<-read.csv("C:/Assessment/2011/r/data/Sab/Sab.surv.polyset.csv")
		#attr(Sab.surv.polyset,"projection")<-"LL"
		calcArea(Sab.surv.polyset,1)

   
###### Middle (fixed station - 15)
  	
    Mid.towlst<-read.csv("MidTowlst2016.csv")
    
    source("fn/ScallopMap.r")
		ScallopMap("Mid",nafo='all', title='Middle Bank Survey')
		addPoints(Mid.towlst,pch=21,cex=1.5,bg='orange')


 ##### Browns Bank North: 
           ## Brad: Are the polyset and polydata fixed now for BBN (historical cpue in kg/hm ??  Dates???  problem with points fills
		
		## HCR
		
		survBBnHCR.dat<-read.csv("BBnSurveyHCR9112.csv")
		BBn.HCRsurv.polyset<-read.csv("BBn.HCRsurv.polyset.csv")
		BBn.HCRsurv.polydata<-read.csv("BBn.HCRsurv.polydata.csv")
		BBn.HCRstrata.areas<-read.csv("BBnStrataAreasHCR.csv")
		boxes<-read.csv("BBnBoxes2015.csv")

		str.def<-c(1, 16, 23, 30, 39, 154)		
		BBn.HCRsurv.polydata <- data.frame(PID=1:5,col=brewer.pal(5,"YlGnBu"),border=NA,PName=paste("strata",1:(length(str.def)-1),': ',str.def[-length(str.def)],' - ',str.def[-1],' kg/hm',sep=''))
		
		BBnHCR.towlst<-alloc.poly(poly.lst=list(BBn.HCRsurv.polyset, BBn.HCRsurv.polydata),ntows=100,pool.size=3,mindist=1,bank.plot="BBn")
		write.csv(BBnHCR.towlst$Tows,"BBnTowlist_HCR_2016.csv",row.names=F)
    BBnHCR.towlst<-read.csv("BBnTowlist_HCR_2016.csv")
    BBnHCR.towlst<-as.EventData(BBnHCR.towlst)
		source("fn/ScallopMap.r")
		ScallopMap(ylim=c(42.5,42.9),xlim=c(-66.5,-65.75),bathcol=rgb(0,0,1,0.3),bathy.source='USGS',isobath='usgs',poly.lst=list(BBn.HCRsurv.polyset,BBn.HCRsurv.polydata),cex=1.2,title="BBn Survey 2016")
		addPolys(subset(boxes,PID%in%c(10:11)))
		legend('bottomleft',legend=BBn.HCRsurv.polydata$PName,fill=BBn.HCRsurv.polydata$col,bty='n',cex=1, inset = .02)
		addPoints(BBnHCR.towlst,pch=20, col='red',cex=1.5,bg=BBn.HCRsurv.polydata$col[BBnHCR.towlst$Poly.ID])
		#BBnextra.towlst<-read.csv("BBNextras2015.csv")
    #BBnextra.towlst<-as.EventData(BBnextra.towlst)
    #addPoints(BBnextra.towlst,pch=21, col='black',cex=1.5)
		
			#bathy.dat<-read.table("Y:/Maps/r/topex/AtlCanbathy.xyz",header=F)
			#names(bathy.dat)<-c("X","Y","Z")
			#bathy.dat$X<-bathy.dat$X-360
			#ylim=c(42.5,42.9); 		xlim=c(-66.6,-65.7)
			#bathy.dat<-subset(bathy.dat,X>xlim[1]&X<xlim[2]&Y>ylim[1]&Y<ylim[2])
			#bathy.lst<-makeTopography(bathy.dat)
			#bathy.cl<-contourLines(bathy.lst,levels=-120)
			#bathy.cp <- convCP(bathy.cl)
			#BBnbathy.poly <- bathy.cp$PolySet
			#attr(BBnbathy.poly,"projection") <- "LL"
			#addPolys( bathy.poly,col='blue')
				
			
		#BBn.HCRsurv.polyset<-read.csv("BBn.HCRsurv.polyset.csv")
	
		#BBn.HCRsurv.polyset<-joinPolys(BBn.HCRsurv.polyset,joinPolys(BBnbathy.poly,subset(boxes,PID==3),operation="UNION"),operation="INT")
		#write.csv(BBn.HCRsurv.polyset,"BBn.HCRsurv.polyset.csv",row.names=F)
		#BBn.HCRsurv.polyset<-read.csv("BBn.HCRsurv.polyset.csv")
		
		
		#BBnHCR.towlst<-alloc.poly(poly.lst=list(BBn.HCRsurv.polyset, BBn.HCRsurv.polydata),ntows=100,pool.size=3,mindist=1,bank.plot="BBn")
		#write.csv(BBnHCR.towlst,"BBnTowlist_HCR_2015.csv",row.names=F)
		#BBnHCR.towlst<-read.csv("BBnTowlist_HCR_2015.csv")

	#	source("fn/ScallopMap.r")
#		ScallopMap(ylim=c(42.5,42.9),xlim=c(-66.5,-65.75),bathcol=rgb(0,0,1,0.3),bathy.source='USGS',isobath='usgs',poly.lst=list(BBn.HCRsurv.polyset,BBn.HCRsurv.polydata),cex=1.2,title="BBn Survey 2015")
#		addPolys(subset(boxes,PID%in%c(10,11)),density=0)
#		legend('bottomleft',legend=BBn.HCRsurv.polydata$PName,fill=BBn.HCRsurv.polydata$col,bty='n',cex=1, inset = .02)
#		addPoints(BBnHCR.towlst$Tows,pch=21, col='red',cex=1)
#		,bg=BBn.HCRsurv.polydata$col[BBnHCR.towlst$Tows$Poly.ID]
#		
		
##### German Bank:

		  #update survGer.dat; polys are fixed

		survGer.dat<-read.csv("GerSurvey842015.csv")
		GerSurvPoly1<-read.csv("GerSurvPoly1.csv")
		#GerSurvPoly2<-read.csv("GerSurvPoly2.csv")
		
		#gerbk<-read.csv("Y:/Jessica/German/Bathy/gerbk_50.txt")
	#	gerbk<-na.omit(gerbk[,c(2,3,5)])
	#	gerbk$DEPTH<-gerbk$DEPTH*-1
		
		lastyearstows<-subset(survGer.dat,state=='live'&year==2015,c('tow','slon','slat','stratum'))
		lastyearstows$stratum<-1
		source("fn/alloc.poly.r")
		Ger.tow.lst<-alloc.poly(poly.lst=list(GerSurvPoly1, data.frame(PID=1,PName="Ger",border=NA,col=rgb(0,0,0,0.2),repeats=20)),ntows=80,pool.size=3,mindist=1,bank.plot="Ger",repeated.tows=lastyearstows)

		Ger.tow.lst$Tows$new.tows$STRATA="new"
		Ger.tow.lst$Tows$repeated.tows$STRATA="repeated"
		Ger.tow.lst$Tows$new.tows$Poly.ID=1
		Ger.tow.lst$Tows$repeated.tows$Poly.ID=2
		Ger.tow.dat<-do.call("rbind",Ger.tow.lst$Tows)

		file.key<-read.csv("fileKey.csv")
		source("fn/Relief.plots.r")
		Relief.plots(Ger.tow.dat,gerbk,graphic="pdf",digits=3)
		source("fn/Relief.plots.r")
		Relief.plots(GerTows2013,graphic="pdf",digits=4,gerfiles=1:77,key=file.key)

		#
    #subset(Ger.tow.dat,STRATA=='repeated')
		

		source("fn/convert.dd.dddd.r")
		Ger.tow.dat$LON.DDMM<-convert.dd.dddd(Ger.tow.dat$X,'deg.min')
		Ger.tow.dat$LAT.DDMM<-convert.dd.dddd(Ger.tow.dat$Y,'deg.min')
		#write.csv(subset(Ger.tow.dat,STRATA=='repeated'),"GerTowlist2016repeats.csv",row.names=F)
#		Ger.tow.lst<-alloc.srs(GerSurvPoly1,ntows=50,bank.plot='Ger',repeated.tows=lastyearstows)
#		write.csv(Ger.tow.lst$new.tows,"C:/Assessment/2014/survey/ExtraGerTows.csv",row.names=F)


    write.csv(Ger.tow.dat,"GerTowdat2016.csv",row.names=F)
	  
     	# Map of survey area
		source("fn/ScallopMap.r")
		windows(10,9)
		pdf('GerMap2016.pdf',width=10,height=9)
		ScallopMap(ylim=c(42.9,43.7),xlim=c(-66.8,-65.6),bathy.source="USGS",isobath='usgs',bathcol=rgb(0,0,1,0.2),cex=1.2, title="German Survey 2016")
		addPolys(GerSurvPoly1,border=NA,col=rgb(0,0,0,0.2))
		addPoints(Ger.tow.dat,polyProps=data.frame(EID=Ger.tow.dat$EID,pch=Ger.tow.dat$Poly.ID+15))
		legend('right',legend=c('new','repeated'),bty='n',pch=16:17, inset = .02)
		dev.off()
		
		
		file.key<-read.csv("fileKey.csv")
		source("fn/Relief.plots_ar.r") # has hard coded files!!!!!!
 
		#lastyearstows<-subset(survGer.dat,state=='live'&bank=='Ger'&year==2013,c('tow','slon','slat','elon','elat','depth'))
#		lastyearstows$EID<-lastyearstows$tow+1000
#		repeated.tows<-merge(Ger.tow.dat,lastyearstows)
	
		#source("fn/Relief.plots.r")
		#Relief.plots(repeated.tows,graphic="pdf",digits=4,gerfiles=1:77,key=file.key)
		
    Ger.tow.dat<- read.csv("GerTowdat2016.csv")
    
    Relief.plots(Ger.tow.dat[91:100,],graphic="pdf",file="", digits=4,gerfiles=1:77,key=file.key)
		
	###GB	
#setwd("C:/Assessment/2013/r")
		load("Y:/Offshore scallop/Assessment/2014/r/SurveySummary.Rdata")
		survGBMay.dat<-subset(springSurv.dat,bank%in%c('GB',"GBa","GBb")&year>1989)
		survGBLive.dat<-subset(survGBMay.dat,state=='live') # live only
		survGBa.dat<-read.csv("data/GBa/GBaSurvey8114.csv")
		survGBaLive.dat<-subset(survGBa.dat,state=='live')
		boxes<-read.csv("data/GBa/boxes.csv")
		
	ScallopMap("GB",bathy.source="USGS",isobath='usgs',bathcol=rgb(0,0,1,0.3),title="GB Spring Survey 2016, fixed stations", cex=1.2)		
		points(lat~lon,survGBLive.dat,subset=year==2014&tow<325,pch=21,cex=1,bg='red')	
	 	addPolys(subset(boxes,PID%in%13:13))
	 	GBspringextra.towlst<-read.csv("GBspringextras2015.csv")
   GBspringextra.towlst<-as.EventData(GBspringextra.towlst)
    addPoints(GBspringextra.towlst,pch=21, col='black',cex=1, bg='yellow')
  
  #
#  	pdf('GBMay.pdf',width=9,height=9)
#		for(y in 2009:2014){
#			source("fn/ScallopMap.r")
#			ScallopMap("GB",bathy.source="USGS",isobath='usgs',bathcol=rgb(0,0,1,0.3),title=y)
#			#addPolys(subset(boxes,PID%in%11:13))
#			points(lat~lon,survGBLive.dat,subset=year==2014&tow<325,pch=21,cex=1,bg='red')
#			points(lat~lon,survGBLive.dat,subset=year==y&tow>324,pch=21,cex=1,bg='yellow')
#		}
#		dev.off()
#		
#		pdf('GBAug.pdf',width=9,height=9)
#		for(y in 2009:2013){
#			source("fn/ScallopMap.r")
#			ScallopMap("GB",bathy.source="USGS",isobath='usgs',bathcol=rgb(0,0,1,0.3))
#			addPolys(subset(boxes,PID%in%11:13))
#			points(lat~lon,survGBaLive.dat,subset=year==y&random==1,pch=21,cex=1,bg='red')
#			points(lat~lon,survGBaLive.dat,subset=year==y&random!=1,pch=21,cex=1,bg='yellow')
#		}
#		dev.off()
#		
#			boxes<-read.csv("data/GBa/boxes.csv")
#			addPolys(subset(boxes,PID%in%c(2:3,9)),lty=2)
#		


 ###### Browns Bank South
		
		BBs.surv.polydata<-read.table("data/BBs/surfgeoPolyData.txt",header=T)
		BBs.surv.polyset<-read.csv("data/BBs/BBs.surv.polyset.csv")
				
		source("fn/alloc.poly.r")
		BBs.tow.dat<-alloc.poly(poly.lst=list(BBs.surv.polyset,BBs.surv.polydata),ntows=25,bank.plot="BBs")
				
		BBs.surv.polydata$border=NA
		windows(8,8)
		ScallopMap(ylim=c(42.35,42.75),xlim=c(-65.95,-65.45),bathy.source="USGS",isobath='usgs',bathcol=rgb(0,0,1,0.3),poly.lst=list(BBs.surv.polyset,BBs.surv.polydata),title="BBs survey 2015",cex=1.2)
		addPoints(BBs.tow.dat$Tows,pch=21,cex=1.5,bg=with(subset(BBs.surv.polydata,!is.na(strata)),unique(col))[-1][BBs.tow.dat$Tows$Poly.ID])
		bg.col<-tapply(BBs.surv.polydata$col,BBs.surv.polydata$PName,unique)
		legend('bottomleft',legend=names(bg.col[unique(as.character(BBs.tow.dat$Tows$STRATA))]),pch=21,pt.bg=bg.col[unique(as.character(BBs.tow.dat$Tows$STRATA))],bty='n',pt.cex=1.5, inset = .02)
		
		write.table(BBs.tow.dat$Tows,"BBsTowlist_2015.csv",sep=',',row.names=F)
		