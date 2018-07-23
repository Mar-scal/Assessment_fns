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



# source("Y:/Assessment/2010/r/fn/survey.ts.r")

survey.ts <- function(shf, years=1981:2008, Bank='GBa', type = "N",pdf=T, plots=c('pre','rec','com'),CS=NULL,RS=NULL,clr=c(1,1),cx=1,pch=1:2,lty=1:2,wd=10,ht=8,Npt=T,se=F,ys=1,yl2,ymin=0,dat2=NULL,areas=NULL,ypos=1){

	
	shf<-subset(shf,year%in%years)
	
	if(pdf) pdf(paste("figures/",Bank,type,min(years),"-",max(years),".pdf",sep=""), width = wd, height = ht)
	
	if(type == "N"){
	
		if(!is.null(areas))if(Npt)shf[c("I","IR","IPR","N","NR","NPR")]<-shf[c("I","IR","IPR","N","NR","NPR")] / sum(areas) * 10^6 # scale down from bank to tow
		if(!is.null(dat2))if(!is.null(areas))if(Npt)dat2[c("I","IR","IPR","N","NR","NPR")]<-dat2[c("I","IR","IPR","N","NR","NPR")] / sum(areas) * 10^6 # scale down from bank to tow
		if(pdf==F) windows(wd,ht)	
		par(mfrow = c(length(plots), 1), mar = c(0, 2, 0, 1), omi = c(1, 1, 0.5, 0.5))
		
		if(missing(yl2)){
			if(se==T){
				ymax<-c(max(shf$NPR,shf$NPR+shf$NPR*shf$NPR.cv, na.rm = T),max(shf$NR,shf$NR+shf$NR*shf$NR.cv, na.rm = T),max(shf$N,shf$N+shf$N*shf$N.cv, na.rm = T))*1.2
				if(!is.null(dat2))ymax<-c(max(c(shf$NPR,shf$NPR+shf$NPR*shf$NPR.cv,dat2$NPR,dat2$NPR+dat2$NPR*dat2$NPR.cv), na.rm = T),max(c(shf$NR,shf$NR+shf$NR*shf$NR.cv,dat2$NR,dat2$NR+dat2$NR*dat2$NR.cv), na.rm = T),max(c(shf$N,shf$N+shf$N*shf$N.cv,dat2$N,dat2$N+dat2$N*dat2$N.cv), na.rm = T))*1.2
			}
			if(se==F){
				ymax<-c(max(shf$NPR, na.rm = T),max(shf$NR, na.rm = T),max(shf$N, na.rm = T))*1.2
				if(!is.null(dat2))ymax<-c(max(c(shf$NPR,dat2$NPR), na.rm = T),max(c(shf$NR,dat2$NR), na.rm = T),max(c(shf$N,dat2$N), na.rm = T))*1.2
			}
		}
		if(!missing(yl2))ymax<-rep(yl2,3)
	
		if('pre'%in%plots){
			plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(ymin, ymax[1]), mgp = c(0.5, 0.5, 0), tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=1.3)
			axis(1, seq(1985,2010,5),lab = F, tcl = -0.6)
			axis(1, at = years, lab = F, tcl = -0.3)
			axis(2, pretty(c(0,ymax[1]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=1.3)
			axis(4, pretty(c(0,ymax[1]*ys)),lab = F, tcl = -0.3)
			abline(h = median(shf$NPR[-length(shf$NPR)], na.rm = T), col = grey(0.5),lty=2)
			points(shf$year, shf$NPR, type = "b", pch = pch[1],cex=cx,col=clr[1])
			if(!is.null(dat2)){
				points(dat2$year, dat2$NPR, type = "b", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
				if(se)if("NPR.cv"%in%names(dat2))segments(dat2$year,dat2$NPR+dat2$NPR*dat2$NPR.cv,dat2$year,dat2$NPR-dat2$NPR*dat2$NPR.cv,col=clr[2])
			}
#			points(shf$year, shf$NPR, type = "p", cex=cx)
			if(se)if("NPR.cv"%in%names(shf))segments(shf$year,shf$NPR+shf$NPR*shf$NPR.cv,shf$year,shf$NPR-shf$NPR*shf$NPR.cv,col=clr[1])
		#	mtext("Pre-recruits", 2, 3.5, cex = 1.25)
			if(is.null(RS))text(years[ypos], ymax[1]*0.9, "pre-recruits", cex = 2, adj = 0)
			else if(!is.null(RS))text(years[ypos], ymax[1]*0.9, paste("pre-recruits <",RS,"mm"), cex = 2, adj = 0)
		}
		
		if('rec'%in%plots){
			plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(ymin, ymax[2]), mgp = c(0.5, 0.5, 0), tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=1.3)
			axis(1, seq(1985,2010,5),lab = F, tcl = -0.6)
			axis(1, at = years, lab = F, tcl = -0.3)
			axis(2, pretty(c(0,ymax[2]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=1.3)
			axis(4, pretty(c(0,ymax[2]*ys)),lab = F, tcl = -0.3)
			abline(h = median(shf$NR[-length(shf$NR)], na.rm = T), col = grey(0.5),lty=2)
			points(shf$year, shf$NR, type = "b", pch = pch[1],cex=cx,col=clr[1])
			if(!is.null(dat2)){
				points(dat2$year, dat2$NR, type = "b", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
				if(se)if("NR.cv"%in%names(dat2))segments(dat2$year,dat2$NR+dat2$NR*dat2$NR.cv,dat2$year,dat2$NR-dat2$NR*dat2$NR.cv,col=clr[2])
			}
#			points(shf$year, shf$NR, type = "p", cex=cx)
			if(se)if("NR.cv"%in%names(shf))segments(shf$year,shf$NR+shf$NR*shf$NR.cv,shf$year,shf$NR-shf$NR*shf$NR.cv,col=clr[1])
		#	mtext("Recruits", 2, 3.5, cex = 1.25)
			if(is.null(RS))text(years[ypos], ymax[2]*0.9, "recruits", cex = 2, adj = 0)
			else if(!is.null(RS))text(years[ypos], ymax[2]*0.9, paste("recruits",RS,"-",CS-1,"mm"), cex = 2, adj = 0)
		}
#	browser()
		if('com'%in%plots){
			plot(years, years, type = "n", ylab = "", xlab = "", las = 1, ylim = c(ymin, ymax[3]), mgp = c(0.5, 0.5, 0), tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=1.3)
			axis(1,  tcl = -0.6, mgp = c(0.5, 0.7, 0), cex.axis=1.3)
			axis(1, at = years, lab = F, tcl = -0.3)
			axis(2, pretty(c(0,ymax[3]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=1.3)
			axis(4, pretty(c(0,ymax[3]*ys)),lab = F, tcl = -0.3)
			abline(h = median(shf$N[-length(shf$N)], na.rm = T), col = grey(0.5),lty=2)
			points(shf$year, shf$N, type = "b", pch = pch[1],cex=cx,col=clr[1])
			if(!is.null(dat2)){
				points(dat2$year, dat2$N, type = "b", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
				if(se)if("N.cv"%in%names(dat2))segments(dat2$year,dat2$N+dat2$N*dat2$N.cv,dat2$year,dat2$N-dat2$N*dat2$N.cv,col=clr[2])
			}
#			points(shf$year, shf$N, type = "p", cex=cx)
			if(se)if("N.cv"%in%names(shf))segments(shf$year,shf$N+shf$N*shf$N.cv,shf$year,shf$N-shf$N*shf$N.cv,col=clr[1])
		#	mtext("Commercial size", 2, 3.5, cex = 1.25)
			if(is.null(CS))text(years[ypos], ymax[3]*0.9, "fully recruited", cex = 2, adj = 0)
			else if(!is.null(CS))text(years[ypos], ymax[3]*0.9, paste("fully recruited >=",CS,"mm"), cex = 2, adj = 0)
		}
	
		axis(1, tcl = -0.6, mgp = c(0.5, 0.7, 0), cex.axis=1.3)
		if(Npt)mtext("Mean N / standard tow", 2, 3, outer = T, cex = 1.5)	
		if(Npt==F)mtext("Number of scallops (millions)", 2, 3, outer = T, cex = 1.5)	
		mtext("Year", 1, 4, outer = T, cex = 1.5)	
			
		}
	
	
	if(type == "B"){
		
	
		if(!is.null(areas))if(Npt)shf[c("I","IR","IPR","N","NR","NPR")]<-shf[c("I","IR","IPR","N","NR","NPR")] / sum(areas) * 10^3 # scale down from bank to tow
		if(!is.null(dat2))if(!is.null(areas))if(Npt)dat2[c("I","IR","IPR","N","NR","NPR")]<-dat2[c("I","IR","IPR","N","NR","NPR")] / sum(areas) * 10^3 # scale down from bank to tow

		if(pdf==F) windows(wd,ht)	
		par(mfrow = c(length(plots), 1), mar = c(0, 2, 0, 1), omi = c(1, 1, 0.5, 0.5))
		
		if(missing(yl2)){
			if(se==T){
				ymax<-c(max(shf$IPR,shf$IPR+shf$IPR*shf$IPR.cv, na.rm = T),max(shf$IR,shf$IR+shf$IR*shf$IR.cv, na.rm = T),max(shf$I,shf$I+shf$I*shf$I.cv, na.rm = T))*1.2
				if(!is.null(dat2))ymax<-c(max(c(shf$IPR,shf$IPR+shf$IPR*shf$IPR.cv,dat2$IPR,dat2$IPR+dat2$IPR*dat2$IPR.cv), na.rm = T),max(c(shf$IR,shf$IR+shf$IR*shf$IR.cv,dat2$IR,dat2$IR+dat2$IR*dat2$IR.cv), na.rm = T),max(c(shf$I,shf$I+shf$I*shf$I.cv,dat2$I,dat2$I+dat2$I*dat2$I.cv), na.rm = T))*1.2
			}
			if(se==F){
				ymax<-c(max(shf$IPR, na.rm = T),max(shf$IR, na.rm = T),max(shf$I, na.rm = T))*1.2
				if(!is.null(dat2))ymax<-c(max(c(shf$IPR,dat2$IPR), na.rm = T),max(c(shf$IR,dat2$IR), na.rm = T),max(c(shf$I,dat2$I), na.rm = T))*1.2
			}
		}
		if(!missing(yl2))ymax<-rep(yl2,3)
	
		if('pre'%in%plots){
			plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(0, ymax[1]), mgp = c(0.5, 0.5, 0), tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=1.3)
			axis(1, seq(1985,2010,5),lab = F, tcl = -0.6)
			axis(1, at = years, lab = F, tcl = -0.3)
			axis(2, pretty(c(0,ymax[1]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=1.3)
			axis(4, pretty(c(0,ymax[1]*ys)),lab = F, tcl = -0.3)
			abline(h = median(shf$IPR[-length(shf$IPR)], na.rm = T), col = grey(0.5),lty=2)
			points(shf$year, shf$IPR, type = "b", pch = pch[1],cex=cx,col=clr[1])
			if(!is.null(dat2)){
				points(dat2$year, dat2$IPR, type = "b", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
				if(se)if("IPR.cv"%in%names(dat2))segments(dat2$year,dat2$IPR+dat2$IPR*dat2$IPR.cv,dat2$year,dat2$IPR-dat2$IPR*dat2$IPR.cv,col=clr[2])
			}
#			points(shf$year, shf$IPR, type = "b", cex=cx)
			if(se)if("IPR.cv"%in%names(shf))segments(shf$year,shf$IPR+shf$IPR*shf$IPR.cv,shf$year,shf$IPR-shf$IPR*shf$IPR.cv,col=clr[1])
		#	mtext("Pre-recruits", 2, 3.5, cex = 1.25)
			if(is.null(RS))text(years[ypos], ymax[1]*0.9, "pre-recruits", cex = 2, adj = 0)
			else if(!is.null(RS))text(years[ypos], ymax[1]*0.9, paste("pre-recruits <",RS,"mm"), cex = 2, adj = 0)
		}
		if('rec'%in%plots){
			plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(0, ymax[2]), mgp = c(0.5, 0.5, 0), tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=1.3)
			axis(1, seq(1985,2010,5),lab = F, tcl = -0.6)
			axis(1, at = years, lab = F, tcl = -0.3)
			axis(2, pretty(c(0,ymax[2]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=1.3)
			axis(4, pretty(c(0,ymax[2]*ys)),lab = F, tcl = -0.3)
			abline(h = median(shf$IR[-length(shf$IR)], na.rm = T), col = grey(0.5),lty=2)
			points(shf$year, shf$IR, type = "b", pch = pch[1],cex=cx,col=clr[1])
			if(!is.null(dat2)){
				points(dat2$year, dat2$IR, type = "b", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
				if(se)if("IR.cv"%in%names(dat2))segments(dat2$year,dat2$IR+dat2$IR*dat2$IR.cv,dat2$year,dat2$IR-dat2$IR*dat2$IR.cv,col=clr[2])
			}
#			points(shf$year, shf$IR, type = "b", cex=cx)
			if(se)if("IR.cv"%in%names(shf))segments(shf$year,shf$IR+shf$IR*shf$IR.cv,shf$year,shf$IR-shf$IR*shf$IR.cv,col=clr[1])
		#	mtext("Recruits", 2, 3.5, cex = 1.25)
			if(is.null(RS))text(years[ypos], ymax[2]*0.9, "recruits", cex = 2, adj = 0)
			else if(!is.null(RS))text(years[ypos], ymax[2]*0.9, paste("recruits",RS,"-",CS-1,"mm"), cex = 2, adj = 0)
		}
	
		if('com'%in%plots){
			plot(years, years, type = "n",  ylab = "", xlab = "", las = 1, ylim = c(0, ymax[3]), mgp = c(0.5, 0.5, 0), tcl = -0.3, xaxt = "n", yaxt="n", cex.axis=1.3)
			axis(1, tcl = -0.6, mgp = c(0.5, 0.7, 0), cex.axis=1.3)
			axis(1, at = years, lab = F, tcl = -0.3)
			axis(2, pretty(c(0,ymax[3]*ys)), mgp = c(0.5, 0.7, 0),las=1,cex.axis=1.3)
			axis(4, pretty(c(0,ymax[3]*ys)),lab = F, tcl = -0.3)
			abline(h = median(shf$I[-length(shf$I)], na.rm = T), col = grey(0.5),lty=2)
			points(shf$year, shf$I, type = "b", pch = pch[1],cex=cx,col=clr[1])
			if(!is.null(dat2)){
				points(dat2$year, dat2$I, type = "b", cex=cx,col=clr[2],lty=lty[2],pch=pch[2])
				if(se)if("I.cv"%in%names(dat2))segments(dat2$year,dat2$I+dat2$I*dat2$I.cv,dat2$year,dat2$I-dat2$I*dat2$I.cv,col=clr[2])
			}
#			points(shf$year, shf$I, type = "b", cex=cx)
			if(se)if("I.cv"%in%names(shf))segments(shf$year,shf$I+shf$I*shf$I.cv,shf$year,shf$I-shf$I*shf$I.cv,col=clr[1])
		#	mtext("Commercial size", 2, 3.5, cex = 1.25)
			if(is.null(CS))text(years[ypos], ymax[3]*0.9, "fully recruited", cex = 2, adj = 0)
			else if(!is.null(CS))text(years[ypos], ymax[3]*0.9, paste("fully recruited >=",CS,"mm"), cex = 2, adj = 0)
		}
		axis(1, tcl = -0.6, mgp = c(0.5, 0.7, 0), cex.axis=1.3)
	
		if(Npt)mtext("Mean kg / standard tow", 2, 3, outer = T, cex = 1.5)	
		if(Npt==F)mtext("Total Biomass (t)", 2, 3, outer = T, cex = 1.5)	
		mtext("Year", 1, 4, outer = T, cex = 1.5)	
			
	}
	if(pdf)dev.off()
	shf
}

