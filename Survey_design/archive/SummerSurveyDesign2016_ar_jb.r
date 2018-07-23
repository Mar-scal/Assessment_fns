

### Note new alloc.poly function. This has been updated Aug 2016 to correct/bypass error which caused duplication of stations if on generated on strata boundary - placing one in each strata with same position. ###


##### SURVEY DESIGN
setwd("C:/Users/Broomej/Documents/R/Survey_Design_2016")
#setwd("Y:/Offshore scallop/Assessment/Assessment_fns/")

options(stringsAsFactors=F)
require(PBSmapping)
require(RColorBrewer)

db.con = "ptran"
pwd.ID = "M1ck3y923"
#pw = "M1ck3y923"
un.ID = "Broomej"
#un = "Broomej"


atow<-800*2.4384/10^6 # area of standard tow in km2


##### August Survey #####
##### Georges Bank:

      GBa.strPolys<-read.csv("data/GBa/GBaStrataBoundaries.csv")
			GBa.strData<-data.frame(PID=c(7:5,1:4),PName=c("Very High South (>431 scallops/tow)","High South (251-431 scallops/tow)","Medium South (115-251 scallops/tow)","Low (50-115 scallops/tow)","Medium North (115-251 scallops/tow)","High North (251-431 scallops/tow)","Very High North (>431 scallops/tow)" ),col=brewer.pal(7,"RdYlGn"),border=NA)
			source("fns/ScallopMap.r")
			#windows(10,10)
			ScallopMap(direct="C:/Users/Broomej/Documents/R/Survey_Design_2016/", "GB",poly.lst=list(GBa.strPolys,GBa.strData),nafo='all')

			GBarea.poly<-data.frame(PID=1,POS=1:7,X=c(-67.30935,-66.754167,-66,-65.666667,-65.666667,-66.16895,-67.30935),Y=c(42.333333,42.333333,42.333333,42,41.583333,41,42.333333))

			survGBaLive.dat<-subset(read.csv("data/GBa/GBaSurvey1981-2015.csv"),state=='live')
			alloc=with(subset(survGBaLive.dat,year==2010),tapply(tow,stratum,length))[c(7:5,1:4)]
			names(alloc)<-c('Very High South (>431 scallops/tow)','High South (251-431 scallops/tow)','Medium South (115-251 scallops/tow)','Low (50-115 scallops/tow)','Medium North (115-251 scallops/tow)','High North (251-431 scallops/tow)','Very High North (>431 scallops/tow)')
			#repeats<-round(alloc*0.2)
			#GBa.strData$repeats<-repeats
			#GBa.strData$allocation<-alloc-repeats
			GBa.strData$allocation<-alloc

			#GBa2010tows<-subset(survGBaLive.dat,year==2010&stratum<8,c('tow',"slon","slat","stratum"))

			
### Note updated alloc.poly function. ####
      source("fns/alloc.poly_new.R")
			#GBa.towlst<-alloc.poly(poly.lst=list(GBa.strPolys,GBa.strData),bounding.poly=GBarea.poly[-(3:4),],bank.plot=NULL,pool.size=5,repeated.tows=GBa2010tows)
			#GBa.towlst$Tows$repeated.tows$EID<-GBa.towlst$Tows$repeated.tows$EID+1000
			
      #GBa.towlst<-alloc.poly(poly.lst=list(GBa.strPolys,GBa.strData),bounding.poly=GBarea.poly[-(3:4),],bank.plot="GB",pool.size=5)
			# Run line below as correct stations for 2016 - 
      GBa.towlst<-read.csv("~/R/Survey_Design_2016/AugGBaSurveyStations_good.csv")
      
      GBa.towlist <- as.EventData(GBa.towlst)
      is.EventData(GBa.towlst)
      #boxes<-read.csv("data/GBa/boxes.csv")
			#addPolys(subset(boxes,PID%in%11:13))
			#addPolys(subset(boxes,PID%in%9:10))
			#addPolys(subset(boxes,PID%in%2:3),lty=2)

			GBa.towdat<-do.call("rbind",GBa.towlst$Tow)

			write.csv(GBa.towlst$Tows,"AugGBaSurveyStations.csv",row.names=F)
			GBa.towlst<-read.csv("AugGBaSurveyStations_good.csv")

			windows(10,8)
			ScallopMap(direct="C:/Users/BroomeJ/Documents/R/Survey_Design_2016/","GB",bathy.source="usgs",isobath='usgs',bathcol=rgb(0,0,1,0.3),poly.lst=list(GBa.strPolys,GBa.strData),title="GBa August Survey 2016",cex=1.2)
			#addPoints(GBa.towlst,pch=21, cex=1,bg=bg.col[as.character(GBa.towlst$STRATA)])
			addPoints(GBa.towlst,cex=1)
      #addPolys(subset(boxes,PID%in%11:13))

			bg.col<-tapply(GBa.strData$col,GBa.strData$PName,unique)

			windows(10,8)
			ScallopMap(direct="C:/Users/BroomeJ/Documents/R/Survey_Design_2016/", ylim=c(41.833,42.2),xlim=c(-67.2,-66.6),bathy.source="usgs",isobath='usgs',bathcol=rgb(0,0,1,0.3),poly.lst=list(GBa.strPolys,GBa.strData),title="GBa August Survey 2016 Northwest",cex=1.2)
			#addPoints(GBa.towlst,pch=21, cex=1,bg=bg.col[as.character(GBa.towlst$STRATA)])
			#addPoints(GBa.towlst,cex=1)
      text(GBa.towlst$X,GBa.towlst$Y,label=GBa.towlst$label,col='black', cex=0.6)
      #addPolys(subset(boxes,PID%in%13:13))
			
      windows(10,8)
			ScallopMap(direct="C:/Users/BroomeJ/Documents/R/Survey_Design_2016/",ylim=c(41.833,42.2),xlim=c(-66.6,-66),bathy.source="usgs",isobath='usgs',bathcol=rgb(0,0,1,0.3),poly.lst=list(GBa.strPolys,GBa.strData),title="GBa August Survey 2016 Northeast",cex=1.2)
			#addPoints(GBa.towlst,pch=21, cex=1,bg=bg.col[as.character(GBa.towlst$STRATA)])
			addPoints(GBa.towlst,cex=1)
			#addPolys(subset(boxes,PID%in%11:13))
      text(GBa.towlst$X,GBa.towlst$Y,label=GBa.towlst$label,col='black', cex=0.6)
      

      windows(10,8)
			ScallopMap(direct="C:/Users/BroomeJ/Documents/R/Survey_Design_2016/",ylim=c(41.25,41.833),xlim=c(-66.6,-65.85),bathy.source="usgs",isobath='usgs',bathcol=rgb(0,0,1,0.3),poly.lst=list(GBa.strPolys,GBa.strData),title="GBa August Survey 2016 South",cex=1.2)
			#addPoints(GBa.towlst,pch=21, cex=1,bg=bg.col[as.character(GBa.towlst$STRATA)])
			#addPoints(GBa.towlst,cex=1)
      text(GBa.towlst$X,GBa.towlst$Y,label=GBa.towlst$label,col='black', cex=0.6)


      ##GBb####

			GBb.strPolys<-read.csv("data/GBb/GBbStrataBoundaries.csv")
			bounds.poly<-read.csv("data/GBb/GBbBoundingPoly.csv")

			require(RColorBrewer)
			GBb.strData<-data.frame(PID=5:1,PName=c("Very High South (>562 scallops/tow)","Very High North (>562 scallops/tow)","High (298-562 scallops/tow)","Medium (138-298 scallops/tow)","Low (10-138 scallops/tow)"),col=brewer.pal(7,"GnBu")[c(7:6,4:2)],border=NA)
			windows(10,8)
      ScallopMap(direct="C:/Users/BroomeJ/Documents/R/","GBb",poly.lst=list(GBb.strPolys,GBb.strData),nafo='all', cex=1.2)


			source("fns/alloc.poly_new.r")
			GBb.towlst<-alloc.poly(poly.lst=list(GBb.strPolys,GBb.strData),bounding.poly=bounds.poly,ntows=30,bank.plot="GBb",pool.size=5,show.pool=F)


			write.csv(GBb.towlst,"AugGBbSurveyStations.csv",row.names=F)
			GBb.towlst<-read.csv("AugGBbSurveyStations.csv")
      attr(GBb.towlst,"projection")<-"LL"

windows(10,8)
ScallopMap(direct="C:/Users/BroomeJ/Documents/R/", ylim=c(41.6,42.3),xlim=c(-66.6,-65.6),bathy.source="usgs", bathcol=rgb(0,0,1,0.3),poly.lst=list(GBb.strPolys,GBb.strData),title="GBb August Survey 2016",cex=1.2)
#addPoints(GBa.towlst,pch=21, cex=1,bg=bg.col[as.character(GBa.towlst$STRATA)])
addPoints(GBb.towlst,cex=1)
#addPolys(subset(boxes,PID%in%13:13))


  #	towsGBa<-read.csv("Y:/Offshore scallop/Amy/Surveys/TE10-GB10/positionsGBa.csv")
#			names(towsGBa)[1]<-"EID"
#			towsGBaStart<-towsGBa
#			source("fn/convert.dd.dddd.r")
#			towsGBaStart$X<-convert.dd.dddd(towsGBaStart$slon)*-1
#			towsGBaStart$Y<-convert.dd.dddd(towsGBaStart$slat)
#			towsGBaEnd<-towsGBa
#			towsGBaEnd$X<-convert.dd.dddd(towsGBaEnd$elon)*-1
#			towsGBaEnd$Y<-convert.dd.dddd(towsGBaEnd$elat)
#
#			ScallopMap("GB",poly.lst=list(GBa.strPolys,GBa.strData),nafo='all')
#			addPoints( GBa.towlst$Tows)
#			addPoints( towsGBaStart,col='red')
#
#			segments(towsGBaStart$X,towsGBaStart$Y,towsGBaEnd$X,towsGBaEnd$Y)
#
#			identify(towsGBaStart$X,towsGBaStart$Y)
#
#


  #	towsGBb<-na.omit(read.csv("Y:/Offshore scallop/Amy/Surveys/TE10-GB10/positionsGBb.csv"))
#
#			names(towsGBb)[1]<-"EID"
#			towsGBbStart<-towsGBb
#			source("fn/convert.dd.dddd.r")
#			towsGBbStart$X<-convert.dd.dddd(towsGBbStart$slon)*-1
#			towsGBbStart$Y<-convert.dd.dddd(towsGBbStart$slat)
#			towsGBbEnd<-towsGBb
#			towsGBbEnd$X<-convert.dd.dddd(towsGBbEnd$elon)*-1
#			towsGBbEnd$Y<-convert.dd.dddd(towsGBbEnd$elat)
#
#			ScallopMap("GBb",poly.lst=list(GBb.strPolys,GBb.strData),nafo='all')
#			#addPoints( GBb.towlst$Tows)
#			addPoints( towsGBbStart,col='red')
#
#			segments(towsGBbStart$X,towsGBbStart$Y,towsGBbEnd$X,towsGBbEnd$Y)
#
#			identify(towsGBbStart$X,towsGBbStart$Y)
#
#
#			 alloc1<-with(subset(survGBaLive.dat,year==2010),tapply(stratum,stratum,length))[1:7]
#			 alloc2<-alloc[c(4:7,3:1)]
#			 surv200<-subset(survGBaLive.dat,year==2010)
#
#			 surv150.lst<-list(NULL)
#			 for(i in 1:7){
#				 surv150.lst[[i]]<-subset(surv200,stratum==i)[sample(1:alloc1[i],alloc2[i]),]
#			 }
#			 surv150<-do.call("rbind",surv150.lst)
#