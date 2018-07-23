################################################################################################################
####  Commented and checked by DK starting September 2015.  This function used PEDstrata to get the stratified
####  mean and variance (CV) from the survey data.  
####
# Update history
#Commented, checked  and revised by DK March 31, 2016
# Revised May 5, 2017, edited so that this script can work with only "1 strata", i.e. a non-stratatifed survey.
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary_data.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################

###############################################################################################################
# source(".../Assessment_fns/Survey/survey.dat.r") ok

# Arguments
#shf:          Shell height frequency data
#htwt.fit:     Shell height Meat Weight model fit
#years:        Years of interest, not required.
#RS:           Size a scallop becomes a recruit:  Defalut = 80
#CS:           Size a scallop becomes commercial:  Default = 100
#bk:           The bank of interest.  Default = "GBa".  Potenial options are (GBa, GBb, BBn, BBs,Ger, Mid, Sab)
#areas:        An object with the strata numbers/names and the area covered by each stratum.          
#mw.par:       How is meat weight to be calculated.  Default = 'annual', options are ('fixed' or 'annual') alternatively
#              some variant of "CF" is used if the meat weight is being calculated from condition factor.  Need to have
#              mw.par = column name that includes CF data for this option to work properly.
#err:          What CV to calculate. Default ='str' for stratified design, "rnd" will calculate the random survey design CV.
###############################################################################################################

survey.dat <- function(shf, htwt.fit, years, RS=80, CS=100, bk="GBa", areas,  mw.par='annual',err='str')
{
  
  # load the PEDstrata package, note this is a locally developed package not available from R repositories
	require(PEDstrata)  || stop("PEDstrata package required please obtain a copy and install this locally developed package")
	require(survey)     || stop("survey package required please install package and try again")
	require(splancs)    || stop("splancs package required please install package and try again")
	
  # This is silly, but for the below code to work we need to increase the RS/CS by 5
  RS <- RS + 5
  CS <- CS + 5
  
  # If years is not supplied than obtain from the data
	if(missing(years)==T) years<-sort(unique(shf$year))

	# Create strata object for PEDstrata package, includes Strata and number of towable units in that strata.
	HSIstrata.obj <- data.frame(Strata=areas[,1], NH=areas[,2])[order(areas[,1]),]
	
	# Output the object to screen and determine the number of towable units for this bank.
	print(HSIstrata.obj)
	N.tu <- HSIstrata.obj$NH

	# for easier indexing of shell height bins in shf
	bin <- as.numeric(substr(names(shf),2,nchar(names(shf))))

	# intialize objects for upcoming for loop.

	w.yst <- matrix(NA, length(years), 40)
	n.yst <- w.yst
	n.stratmeans <-list(NULL)
	w.stratmeans <-list(NULL)
	strat.res <- data.frame(year=years)
	Strata.obj <- NULL
	mw <- NULL

	# If CS and RS are just one value turn them into a vector the same length as the number of years of data.
	if(length(CS) == 1)	CS <- rep(CS, length(years))
	if(length(RS) == 1)	RS <- rep(RS, length(years))
		
	# For loop to do the calculations of meat weight
	for(i in 1:length(years))
	 {
		# Set the bins
		mw.bin<-seq(5,200,5)
		# select the current years data.
		ann.dat<-subset(shf,year==years[i])
		# Use the MW-SH model fit to calculate the meat weight, assumes that year was a random effect in the model
		# Remember mw is in grams here.
		if(mw.par=='annual') mw[[i]] <- matrix(exp(log(seq(2.5,200,5))*htwt.fit$b[i]+log(htwt.fit$a[i])),
		                                       nrow(ann.dat),40,byrow=T,dimnames=list(ann.dat$tow,mw.bin))
		# Use the MW-SH model fit to calculate the meat weight, assumes that year was not included in the model
		# Remember mw is in grams here.
		
		if(mw.par=='fixed') mw[[i]]<-matrix(exp(log(seq(2.5,200,5))*htwt.fit$B+htwt.fit$A),nrow(ann.dat),40,
		                                    byrow=T,dimnames=list(ann.dat$tow,mw.bin))
		# DK Note:  So as this was it would overwright the calculations from mw.par=="annual" but this
		# would actually cause an error if ever this was specified as annual
		# Use some other data to estimate Meat Weight, Condition factor generally used for this option.
		# Remember mw is in grams here.
		
		if(mw.par !='annual' && mw.par !='fixed') mw[[i]]<-sweep(matrix((seq(2.5,200,5)/100)^3,nrow(ann.dat),
		                                          40,byrow=T,dimnames=list(ann.dat$tow,mw.bin)),1,FUN='*',ann.dat[,mw.par])
		
		# Make a dataframe subseting the shf data, select the current year and all the bins, add a column with the strata ID's
		num <- data.frame(subset(shf, year==years[i], which(bin==5):which(bin==200)), 
		                  STRATA.ID=shf$new.stratum[shf$year==years[i]])
		# Remove rows with strata ID's which are NA's
		num<-na.omit(num)
		
		# Add up the numbers of Scallops in each size category.
		num$pre <- rowSums(num[, which(mw.bin==5):(which(mw.bin==RS[i])-1)],na.rm=T)
		num$rec <- rowSums(num[, which(mw.bin==RS[i]):(which(mw.bin==CS[i])-1)],na.rm=T)
		num$com <- rowSums(num[, which(mw.bin==CS[i]):which(mw.bin==200)],na.rm=T)
    
		# Make a dataframe with the biomasses for each bin and tow, add the strata ID's as well
		# This is in grams per tow
		w <- data.frame(subset(shf, year==years[i], which(bin==5):which(bin==200))*mw[[i]], 
		                STRATA.ID=shf$new.stratum[shf$year==years[i]])
		# Remove any rows in which the strata is NA
		w<-na.omit(w)
		# Add up the biomass of Scallops in each size category, again this is in grams per tow
		w$pre <- rowSums(w[, which(mw.bin==5):(which(mw.bin==RS[i])-1)],na.rm=T)
		w$rec <- rowSums(w[, which(mw.bin==RS[i]):(which(mw.bin==CS[i])-1)],na.rm=T)
		w$com <- rowSums(w[, which(mw.bin==CS[i]):which(mw.bin==200)],na.rm=T)

		# The proportion of towable area in each strata.
		pstrat <- as.numeric(N.tu/sum(N.tu))
		# Calculate the mean abundance and mean biomass (grams) per tow (for each strata. (ybar_h)
		n.stratmeans[[i]] <- with(num, sapply(1:40, function(x){tapply(num[,x],STRATA.ID,mean)}))
		w.stratmeans[[i]] <- with(w, sapply(1:40, function(x){tapply(w[,x],STRATA.ID,mean)}))
		
		#Multiply the mean abundance(biomass) in each shell height category in a strata by the proportion of towable area
		#in that strata.  Sum this product for each strata resulting in an estimate of total abundance (biomass) for each
		#shell height category in a given year. (ybar_st)
		if(is.null(nrow(n.stratmeans[[i]]))) n.yst[i,] <- n.stratmeans[[i]]
		if(!is.null(nrow(n.stratmeans[[i]]))) n.yst[i,] <- apply(sapply(1:nrow(n.stratmeans[[i]]), function(x){n.stratmeans[[i]][x,] * pstrat[x]}),1,sum)
		#  Now multiply by the total bank area to determine the survey estimated abundance(biomass).
		# The abundance is actual numbers 
		n.Yst <- n.yst[i,] * sum(N.tu) 
		if(is.null(nrow(w.stratmeans[[i]])))  w.yst[i,] <- w.stratmeans[[i]]
		if(!is.null(nrow(w.stratmeans[[i]]))) w.yst[i,] <- apply(sapply(1:nrow(w.stratmeans[[i]]), function(x){w.stratmeans[[i]][x,] * pstrat[x]}),1,sum)
		w.Yst <- w.yst[i,] * sum(N.tu)
		
		
		# Strata calculations for biomass for commerical size Scallops
		Strata.obj$I[[i]]<-PEDstrata(w, HSIstrata.obj,'STRATA.ID',w$com)
		# total number of tows
		strat.res$n[i] <- sum(Strata.obj$I[[i]]$nh)
		# summary of stratified design, returns a number of useful survey design results and optimization summaries.
		I.tmp <- summary(Strata.obj$I[[i]],effic=T)
		# Convert to Biomass estiamte for the bank in tonnes

		strat.res$I[i] <- I.tmp$yst * sum(N.tu)/10^6			#g to t
		# Calculate the CV, 'str' is the stratified CV, the 'ran' option gives the random design CV.
		if(err=='str') strat.res$I.cv[i] <- I.tmp$se.yst / I.tmp$yst
		# Note here that the variance from the summary is more like a variance of an s.e. rather than a variance of a s.d.
		if(err=='ran') strat.res$I.cv[i] <- sqrt(I.tmp$var.ran) / I.tmp$yst
		  
		# Strata calculations for biomass for recruit sized Scallops
		Strata.obj$IR[[i]]<-PEDstrata(w, HSIstrata.obj,'STRATA.ID',w$rec)
		IR.tmp <- summary(Strata.obj$IR[[i]],effic=T)
		strat.res$IR[i] <- IR.tmp$yst* sum(N.tu)/10^6			#g to t
		if(err=='str')  strat.res$IR.cv[i] <- IR.tmp$se.yst / IR.tmp$yst
		# Note here that the variance from the summary is more like a variance of an s.e. rather than a variance of a s.d.
		if(err=='ran') strat.res$IR.cv[i] <- sqrt(IR.tmp$var.ran) / IR.tmp$yst
		  
		# Strata calculations for biomass for pre-recruit sized Scallops
		Strata.obj$IPR[[i]]<-PEDstrata(w, HSIstrata.obj,'STRATA.ID',w$pre)
		IPR.tmp <- summary(Strata.obj$IPR[[i]],effic=T)
		strat.res$IPR[i] <- IPR.tmp$yst* sum(N.tu)/10^6			#g to t
		if(err=='str') strat.res$IPR.cv[i] <- IPR.tmp$se.yst /  IPR.tmp$yst
		if(err=='ran') strat.res$IPR.cv[i] <- sqrt(IPR.tmp$var.ran) /  IPR.tmp$yst
		  
		
		# Strata calculations for abundance for commerical size Scallops
		Strata.obj$N[[i]]<-PEDstrata(num, HSIstrata.obj,'STRATA.ID',num$com)
		N.tmp <- summary(Strata.obj$N[[i]],effic=T)
		strat.res$N[i] <- N.tmp$yst * sum(N.tu)/10^6			#in millions
		if(err=='str') strat.res$N.cv[i] <- N.tmp$se.yst / N.tmp$yst
		if(err=='ran') strat.res$N.cv[i] <- sqrt(N.tmp$var.ran) / N.tmp$yst
		  
		
		# Strata calculations for abundance for recruit size Scallops
		Strata.obj$NR[[i]]<-PEDstrata(num, HSIstrata.obj,'STRATA.ID',num$rec)
		NR.tmp <- summary(Strata.obj$NR[[i]],effic=T)
		strat.res$NR[i] <- NR.tmp$yst* sum(N.tu)/10^6			#in millions
		if(err=='str')  strat.res$NR.cv[i] <- NR.tmp$se.yst / NR.tmp$yst
		if(err=='ran') strat.res$NR.cv[i] <- sqrt(NR.tmp$var.ran) / NR.tmp$yst
		  
		# Strata calculations for abundance for pre-recruit size Scallops
		Strata.obj$NPR[[i]]<-PEDstrata(num, HSIstrata.obj,'STRATA.ID',num$pre)
		NPR.tmp <- summary(Strata.obj$NPR[[i]],effic=T)
		strat.res$NPR[i] <- NPR.tmp$yst * sum(N.tu)/10^6			#in millions
		if(err=='str') strat.res$NPR.cv[i] <- NPR.tmp$se.yst / NPR.tmp$yst
		if(err=='ran') strat.res$NPR.cv[i] <- sqrt(NPR.tmp$var.ran) / NPR.tmp$yst
	
		# Average weight of fully recruited scallop by year
		strat.res$w.bar[i] <- sum(w.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)]) /
		  sum(n.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])							
		
		# Average shell height of fully recruited scallop by year
		strat.res$l.bar[i] <- sum((n.yst[i,]*seq(2.5,200,5))[which(mw.bin==CS[i]):which(mw.bin==200)]) / 
		  sum(n.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])
		strat.res$l.k[i] <- sum((n.yst[i,]*seq(2.5,200,5))[which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)]) / 
		  sum(n.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)])
		
		# Weight at size of recruitment by year
		strat.res$w.k[i] <- sum(w.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)]) /
		  sum(n.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)])		
		
		
	}	# end for(i in 1:length(years))

	# Data for the delay-difference stock assessment model and survey summary
	model.dat <- strat.res
	# Data for shf plots used in the survey summary
	shf.dat <- list(n.yst=n.yst,w.yst=w.yst,n.stratmeans=n.stratmeans,w.stratmeans=w.stratmeans)
	# Return the data to function calling it.
	return(list(model.dat=model.dat,shf.dat=shf.dat,Strata.obj=Strata.obj))

} # end function

