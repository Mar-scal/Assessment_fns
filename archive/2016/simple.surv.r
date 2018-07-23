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

# Arguments

# shf:    The survey data
# years:  The years of interest.  Default = 1981:2008
# CS:     Commerical scallop shell size.  Default = 95
# B:      Caluclate biomass as well. (T/F) Default = T



# source("Y:/Assessment/2009/r/fn/simple.surv.r")

simple.surv<-function(shf, years=1981:2008, CS=95,B=T){
	
	
	n<-with(shf,tapply(com,year,length))
	N<-with(shf,tapply(com,year,mean))
	NR<-with(shf,tapply(rec,year,mean))
	NPR<-with(shf,tapply(pre,year,mean))
	N.cv<-with(shf,tapply(com,year,sd))/sqrt(n)/N
	NR.cv<-with(shf,tapply(rec,year,sd))/sqrt(n)/NR
	NPR.cv<-with(shf,tapply(pre,year,sd))/sqrt(n)/NPR
	I=NA
	IR=NA
	IPR=NA
	I.cv=NA
	IR.cv=NA
	IPR.cv=NA

	if(B == T){
		I<-with(shf,tapply(com.bm,year,mean))
		IR<-with(shf,tapply(rec.bm,year,mean))
		IPR<-with(shf,tapply(pre.bm,year,mean))
		I.cv<-with(shf,tapply(com.bm,year,sd))/sqrt(n)/I
		IR.cv<-with(shf,tapply(rec.bm,year,sd))/sqrt(n)/IR
		IPR.cv<-with(shf,tapply(pre.bm,year,sd))/sqrt(n)/IPR
	}
	
	bin<-as.numeric(substr(names(shf),2,nchar(names(shf))))
	n.yst<-t(sapply(years, function(x){colMeans(shf[shf$year==x,which(bin==5):which(bin==200)])}))
	
		
	#l.bar[i] <- sapply(1:length(years),function(i){sum((n.yst[i,]*seq(2.5,200,5))[which(mw.bin==CS[i]):which(mw.bin==200)])/ sum(n.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])

	surv.dat<-merge(data.frame(year=as.numeric(row.names(N)),n,I,I.cv,IR,IR.cv,IPR,IPR.cv,N,N.cv,NR,NR.cv,NPR,NPR.cv),data.frame(year=years),all=T)
	
	return(list(subset(surv.dat,year%in%years),list(n.yst=n.yst)))	
	
}
