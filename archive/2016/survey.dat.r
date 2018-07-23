####  Commented and checked by DK starting on July 28, 2015.

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: ""SurveySummary.r""survey
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################



# source("Y:/Assessment/2009/r/fn/survey.dat.r")

survey.dat <- function(shf, htwt.fit, years, RS=80, CS=100, bk="GBa", areas, orig.areas, mw.par='annual',err='str'){

	
	require(PEDstrata)
	require(survey)
	require(splancs)
	
	if(missing(years))years<-sort(unique(shf$year))
	domain<-rep(F,length(years))
	if(!missing(orig.areas))domain[years%in%orig.areas$year]<-T
	
	# Create strata objects for BIOsurvey package
	if(sum(domain)>0)ACRstrata.obj <- lapply(1:nrow(orig.areas), function(i){data.frame(Strata=1:5, NH=as.numeric(orig.areas[i,1:5]))})
	#browser()
	HSIstrata.obj <- data.frame(Strata=areas[,1], NH=areas[,2])[order(areas[,1]),]
	
	print(HSIstrata.obj)
	N.h <- HSIstrata.obj$NH

	# for easier indexing of shell height bins in shf
	bin <- as.numeric(substr(names(shf),2,nchar(names(shf))))

	
	n <- NULL
	I <- NULL
	I.cv <- NULL
	IR <- NULL
	IR.cv <- NULL
	IPR <- NULL
	IPR.cv <- NULL
	l.bar <-c()
	l.k <-c()
	w.bar <-c()
	w.k <-c()
	N <- NULL
	N.cv <- NULL
	NR <- NULL
	NR.cv <- NULL
	NPR <- NULL
	NPR.cv <- NULL
	w.yst <- matrix(NA, length(years), 40)
	n.yst <- w.yst
	n.stratmeans <-list(NULL)
	w.stratmeans <-list(NULL)
	Strata.obj<-list(NULL)
	mw<-list(NULL)

	if(length(CS) == 1)	CS <- rep(CS, length(years))
	if(length(RS) == 1)	RS <- rep(RS, length(years))
		
	for(i in 1:length(years)){
		
		mw.bin<-seq(5,200,5)
		ann.dat<-subset(shf,year==years[i])
		if(mw.par=='annual') mw[[i]] <- matrix(exp(log(seq(2.5,200,5))*htwt.fit$b[i]+log(htwt.fit$a[i])),nrow(ann.dat),40,byrow=T,dimnames=list(ann.dat$tow,mw.bin))
		if(mw.par=='fixed') mw[[i]]<-matrix(exp(log(seq(2.5,200,5))*htwt.fit$B+htwt.fit$A),nrow(ann.dat),40,byrow=T,dimnames=list(ann.dat$tow,mw.bin))
		if(mw.par=='annual'||mw.par!='fixed') mw[[i]]<-sweep(matrix((seq(2.5,200,5)/100)^3,nrow(ann.dat),40,byrow=T,dimnames=list(ann.dat$tow,mw.bin)),1,FUN='*',ann.dat[,mw.par])
		
		num <- data.frame(subset(shf, year==years[i], which(bin==5):which(bin==200)), STRATA.ID=shf$new.stratum[shf$year==years[i]])
		num<-na.omit(num)
		
			num$pre <- rowSums(num[, which(mw.bin==5):(which(mw.bin==RS[i])-1)],na.rm=T)
			num$rec <- rowSums(num[, which(mw.bin==RS[i]):(which(mw.bin==CS[i])-1)],na.rm=T)
			num$com <- rowSums(num[, which(mw.bin==CS[i]):which(mw.bin==200)],na.rm=T)
#		if(years[i]==2001)browser()
		w <- data.frame(subset(shf, year==years[i], which(bin==5):which(bin==200))*mw[[i]], STRATA.ID=shf$new.stratum[shf$year==years[i]])
		w<-na.omit(w)
			
			w$pre <- rowSums(w[, which(mw.bin==5):(which(mw.bin==RS[i])-1)],na.rm=T)
			w$rec <- rowSums(w[, which(mw.bin==RS[i]):(which(mw.bin==CS[i])-1)],na.rm=T)
			w$com <- rowSums(w[, which(mw.bin==CS[i]):which(mw.bin==200)],na.rm=T)

		pstrat <- as.numeric(N.h/sum(N.h))
		n.stratmeans[[i]] <- with(num, sapply(1:40, function(x){tapply(num[,x],STRATA.ID,mean)}))
		w.stratmeans[[i]] <- with(w, sapply(1:40, function(x){tapply(w[,x],STRATA.ID,mean)}))

		n.yst[i,] <- apply(sapply(1:nrow(n.stratmeans[[i]]), function(x){n.stratmeans[[i]][x,] * pstrat[x]}),1,sum)
		n.Yst <- n.yst[i,] * sum(N.h) # scale up to fishable area of the Bank
		w.yst[i,] <- apply(sapply(1:nrow(w.stratmeans[[i]]), function(x){w.stratmeans[[i]][x,] * pstrat[x]}),1,sum)
		w.Yst <- w.yst[i,] * sum(N.h)
		
		# Stratified means variances 
			# Biomass
	#		browser()
			Strata.obj$I[[i]]<-PEDstrata(w, HSIstrata.obj,'STRATA.ID',w$com)
			n[i]<-sum(Strata.obj$I[[i]]$nh)
			I.tmp <- summary(Strata.obj$I[[i]],effic=T)
			I[i] <- I.tmp$yst* sum(N.h)/10^6			#g to t
			if(err=='str')I.cv[i] <- I.tmp$se.yst * sum(N.h)/10^6/ I[i]
			if(err=='ran')I.cv[i] <- sqrt(I.tmp$var.ran) * sum(N.h)/10^6/ I[i]
		
			Strata.obj$IR[[i]]<-PEDstrata(w, HSIstrata.obj,'STRATA.ID',w$rec)
			IR.tmp <- summary(Strata.obj$IR[[i]],effic=T)
			IR[i] <- IR.tmp$yst* sum(N.h)/10^6			#g to t
			if(err=='str')IR.cv[i] <- IR.tmp$se.yst * sum(N.h)/10^6 / IR[i]
			if(err=='ran')IR.cv[i] <- sqrt(IR.tmp$var.ran) * sum(N.h)/10^6 / IR[i]
		
			Strata.obj$IPR[[i]]<-PEDstrata(w, HSIstrata.obj,'STRATA.ID',w$pre)
			IPR.tmp <- summary(Strata.obj$IPR[[i]],effic=T)
			IPR[i] <- IPR.tmp$yst* sum(N.h)/10^6			#g to t
			if(err=='str')IPR.cv[i] <- IPR.tmp$se.yst* sum(N.h)/10^6 / IPR[i]
			if(err=='ran')IPR.cv[i] <- sqrt(IPR.tmp$var.ran) * sum(N.h)/10^6 / IPR[i]
			
			# Abundance
			Strata.obj$N[[i]]<-PEDstrata(num, HSIstrata.obj,'STRATA.ID',num$com)
			N.tmp <- summary(Strata.obj$N[[i]],effic=T)
			N[i] <- N.tmp$yst* sum(N.h)/10^6			#in millions
			if(err=='str')N.cv[i] <- N.tmp$se.yst * sum(N.h)/10^6 / N[i]
			if(err=='ran')N.cv[i] <- sqrt(N.tmp$var.ran) * sum(N.h)/10^6/ N[i]
		
			Strata.obj$NR[[i]]<-PEDstrata(num, HSIstrata.obj,'STRATA.ID',num$rec)
			NR.tmp <- summary(Strata.obj$NR[[i]],effic=T)
			NR[i] <- NR.tmp$yst* sum(N.h)/10^6			#in millions
			if(err=='str')NR.cv[i] <- NR.tmp$se.yst * sum(N.h)/10^6 / NR[i]
			if(err=='ran')NR.cv[i] <- sqrt(NR.tmp$var.ran) * sum(N.h)/10^6/ NR[i]
		
			Strata.obj$NPR[[i]]<-PEDstrata(num, HSIstrata.obj,'STRATA.ID',num$pre)
			NPR.tmp <- summary(Strata.obj$NPR[[i]],effic=T)
			NPR[i] <- NPR.tmp$yst* sum(N.h)/10^6			#in millions
			if(err=='str')NPR.cv[i] <- NPR.tmp$se.yst * sum(N.h)/10^6 / NPR[i]
			if(err=='ran')NPR.cv[i] <- sqrt(NPR.tmp$var.ran) * sum(N.h)/10^6/ NPR[i]
		
	# Average weight of fully recruited scallop by year
	w.bar[i] <- sum(w.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])/sum(n.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])							
	
	# Average shell height of fully recruited scallop by year
	l.bar[i] <- sum((n.yst[i,]*seq(2.5,200,5))[which(mw.bin==CS[i]):which(mw.bin==200)])/ sum(n.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])
	l.k[i] <- sum((n.yst[i,]*seq(2.5,200,5))[which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)])/ sum(n.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)])
			
	# Weight at size of recruitment by year
	w.k[i] <- sum(w.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)])/sum(n.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)])	
	
	}	



	# Data for delay-difference model
	model.dat <- merge(data.frame(year=min(years):max(years)),data.frame(year=years, n, I, I.cv, IR, IR.cv, IPR, IPR.cv, l.bar, l.k, w.bar, w.k, N, N.cv, NR, NR.cv, NPR, NPR.cv, CS, RS),all=T)
	
	# Data for shf plots
	shf.dat <- list(n.yst=n.yst,w.yst=w.yst,n.stratmeans=n.stratmeans,w.stratmeans=w.stratmeans)
	
	return(list(model.dat=model.dat,shf.dat=shf.dat,Strata.obj=Strata.obj))


}

