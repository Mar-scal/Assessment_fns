i=1
banks <- "BBn"
source(paste(direct,"Assessment_fns/Survey_and_OSAC/survey.ts.r",sep=""),local=T) 
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/shf.plt.r",sep=""))
N.col <- "YlGn"
X.lvl <- "PuBuGn" # For any time we have an usually large event this is the color of the extra levels required.
N.tow.lab <- expression(frac(N,tow))

library(PBSmapping)

if(any(plots) %in% "seedboxes")    
{
  sb <- subset(seedboxes,Bank == banks[i] & Open >= paste(yr,"-01-01",sep=""))
  if(nrow(sb) > 0) # only run the rest of this if we have data...
  {
    bound.poly.surv <- subset(survey.bound.polys,label==banks[i]) 
    attr(bound.poly.surv,"projection")<-"LL"
    n.box <- length(seedbox.obj[[banks[i]]])
    boxes <- as.PolySet(sb,projection = "LL")
    box.dat <- data.frame(EID=1:nrow(surv.Live[[banks[i]]]),X=surv.Live[[banks[i]]]$lon,Y=surv.Live[[banks[i]]]$lat)
    box.names <- unique(boxes$SCALLOP_Group_ID)
    fig.box.name <- unique(boxes$Common_name)
  
  
  
    for(j in 1:n.box)
    {
      # Get the data for the box of interest
      key <-findPolys(box.dat, subset(boxes,SCALLOP_Group_ID == box.names[j]))  
      this.box <- subset(boxes,SCALLOP_Group_ID == box.names[j])
      boxy <- seedbox.obj[[banks[i]]][[j]][[1]]
      surv.seed <- surv.Live[[banks[i]]][1:nrow(surv.Live[[banks[i]]]) %in% key$EID,]
      
      # Titles for the seedbox plots....
      seedbox.bm.title <- substitute(bold(paste(box,"-Seedbox Biomass time series (",bank,")",sep="")),
                                     list(year=as.character(yr),bank=banks[i],box = fig.box.name[j]))
      seedbox.abund.title <-substitute(bold(paste(box,"-Seedbox Abundance time series (",bank,")",sep="")),
                                       list(year=as.character(yr),bank=banks[i],box = fig.box.name[j]))
      seedbox.SHF.title <- substitute(bold(paste(box,"-Seedbox Shell Height Frequency (",bank,")",sep="")),
                                      list(year=as.character(yr),bank=banks[i],box = fig.box.name[j]))
      pre.title.seed <-substitute(bold(paste("Pre-recruit scallops (" ,""<a, " mm)",sep="")),
                                  list(a=as.character(RS),year=as.character(yr),bank=banks[i]))
      rec.title.seed <- substitute(bold(paste("Recruit scallops (",b- a, " mm)",sep="")),
                                   list(a=as.character(CS-1),b=as.character(RS),year=as.character(yr),bank=banks[i]))
      fr.title.seed <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm)",sep="")),
                                  list(a=as.character(CS),year=as.character(yr),bank=banks[i]))
      
      # Make the abundance time series for the box
      png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-abundance_ts.png",sep=""),units="in",
          width = 8.5, height = 11,res=420,bg = "transparent") 
      survey.ts(boxy,min(boxy$year,na.rm=T):yr,pdf=F, RS=RS, CS=CS,Npt=T,
                areas=NULL,ys=.99,clr=c('blue',"blue"),se=T,pch=16,
                add.title = T,titl = seedbox.abund.title,cx.mn=3,axis.cx = 1.5)
      dev.off()
      
      # Make the biomass time series for the box
      png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-biomass_ts.png",sep=""),units="in",
          width = 8.5, height = 11,res=420,bg = "transparent") 
      survey.ts(boxy,min(boxy$year,na.rm=T):yr,pdf=F, RS=RS, CS=CS,Npt=T,type="B",
                areas=NULL,ys=.99,clr=c('blue',"blue"),se=T,pch=16,
                add.title = T,titl = seedbox.bm.title,cx.mn=3,axis.cx = 1.5)
      dev.off()
      
      # Now the Shell height frequency plot.
      shf.years <- boxy$year[(length(boxy$year)-6):
                               length(boxy$year)]
      maxy <- max(apply(seedbox.obj[[banks[i]]][[j]][[2]]$n.yst,2,function(x){max(x,na.rm=T)}))
      s.size <- seedbox.obj[[banks[i]]][[j]][[1]]$n[seedbox.obj[[banks[i]]][[j]][[1]]$year %in% shf.years]
      png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-SHF.png",sep=""),units="in",
          width = 8.5, height = 11,res=420,bg = "transparent") 
      shf.plt(ps.dat,seedbox.obj[[banks[i]]][[j]],from='surv',yr=shf.years, col1='grey80',type='sh',col2=1,col3=1,xl=c(0,200),rel=F,ymax=maxy,
              recline=c(RS,CS),wd=7,ht=8,add.title = T,titl = seedbox.SHF.title,cex.mn=3,adj=0.05,sample.size = s.size)	
      dev.off()
      
      # Finally the spatial abundance figure.  
      png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-spatial.png",sep=""),units="in",
          width = 11, height = 8.5,res=420,bg = "transparent")
      par(mfrow=c(2,2),omi=c(0.1,0.2,0.5,0.5),xpd=F)
      
      # Loop through each size category
      for(b in 1:3) 
      {
        if(b == 1) 
        {
            con <-  pre.contours[[banks[i]]]
            fig.title <- pre.title.seed 
        } # end  if(b == 1)
        if(b == 2) 
        {
          con <-  rec.contours[[banks[i]]] 
          fig.title <- rec.title.seed 
        } # end  if(b == 2)
        if(b == 3)
        {
          con <-  com.contours[[banks[i]]]
          fig.title <- fr.title.seed 
        } # end  if(b==3)
        # If counts are high enough to need lvls2 we use it, if not we just use lvls1.
        lvls1=c(1,5,10,50,100,200,500,1000)
        CL <- contourLines(con$image.dat,levels=lvls1)
        CP <- convCP(CL)
        Cont1.poly <- joinPolys(CP$PolySet,bound.poly.surv)
        cont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F) 
        lvls2=c(5000,10000,20000,50000)
        plt.lvls <- c(lvls1,lvls2)
        plt.colors <- rbind(cont1.data,data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
                                      col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA,stringsAsFactors = F))
        # If counts are high enough to need lvls2 we use it, if not we just use lvls1.
        if(max(con$image.dat$z)>=lvls2[1])
        {
          CL <- contourLines(con$image.dat,levels=lvls2)
          CP <- convCP(CL)
          Cont2.poly <- joinPolys(CP$PolySet,bound.poly.surv)
          Cont2.poly$PID<-Cont2.poly$PID+length(lvls1)
          Cont.poly<-rbind(Cont1.poly,Cont2.poly)
          lvls<-c(lvls1,lvls2)
          cont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
                                  col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA,stringsAsFactors = F)
          cont.data<-rbind(cont1.data,cont2.data)
        } # end if(max(rec.contours$image.dat$z)>=lvls2[1])
        if(max(con$image.dat$z)<lvls2[1])
        {
          Cont.poly<-Cont1.poly
          cont.data<-cont1.data
          lvls <- lvls1
        } # end if(max(rec.contours$image.dat$z)<lvls2[1])
  
        # and make the map for each size category.
        ScallopMap(ylim=c(min(this.box$Y),max(this.box$Y)),xlim=c(min(this.box$X),max(this.box$X)),bathy.source="usgs",
                   isobath = c(seq(40,140,by=20)),contour=list(Cont.poly,cont.data),plot.bathy = T,plot.boundries = T,direct=direct,
                   title=fig.title,dec.deg = F,ylab="",xlab="",cex.mn=1.3)
        
        addPolys(this.box,lty=2,lwd=2)
        # Add the regular survey tows.
        points(slat~slon,surv.seed,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1.3)
        # Add the exploratory survey tows
        points(slat~slon,surv.seed,subset=year==yr&state =='live' & random==5,pch=24,bg="darkorange",cex=1.3)
      } # end for(b in 1:3)
      # Now add the legend.
      par(xpd=T)
      plot(1:10,type='n',axes=F,xlab='',ylab='',main="",cex.main=1)
      legend("left",c(paste(plt.lvls[-length(plt.lvls)],'-',plt.lvls[-1],sep=''),
                      paste(plt.lvls[length(plt.lvls)],'+',sep='')),fill=c(plt.colors$col),
             border="black",pch=c(rep(NA,length(lvls))),title = N.tow.lab,title.adj = 0.2,
             pt.bg = c(rep(NA,length(lvls2))),bg=NA,bty="n")
      
      legend("topright",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",inset=0.01,
             legend = c(paste('regular (n =',
                              length(subset(surv.seed,year==yr & state=='live'& random==1)$ID),")",sep=""),
                        paste('exploratory (n =',
                              length(subset(surv.seed,year==yr & state=='live'& random==5)$ID),")",sep="")),
             bg=NA,box.col=NA,bty="n")
      title(paste("Seedbox ",fig.box.name[j]," (",banks[i],"-",yr,")",sep=""),cex.main=2,outer=T,line=-0.5)
      dev.off()
      
    } # end for(j in 1:n.box)
  } # end (if nrow(sb))
} # end the if(any(plots) %in% "seedboxes")


