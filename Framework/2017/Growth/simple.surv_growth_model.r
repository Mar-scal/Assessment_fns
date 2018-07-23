####  Commented and checked by DK starting on July 28, 2015. Calculate several simple summary survey statistics!
####  
## Update history
## March 31 2016 by DK, tidying up structure and comments
## March 13, 2017 by DK, updated to work with new growth data model...
####
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
# Arguments
# shf:    The survey data
# years:  The years of interest.  Default = 1981:2008
# B:      Calculate biomass as well. (T/F) Default = T
###############################################################################################################


simple.surv<-function(shf, years=1981:2008, B=T)
{
	
  # Make some nice bin names
  #bin<-na.omit(as.numeric(substr(names(shf),2,nchar(names(shf)))))
  #bin.loc <- which(is.na(as.numeric(substr(names(shf),2,nchar(names(shf)))))==F)
  bin <- na.omit(as.numeric(substr(names(shf),2,nchar(names(shf)))))
  bin.name <- paste0("h",bin)  # the number bins
  wbin.name <- paste0("w",bin) # the weight bins
	# Number of data points each year
	n<-with(shf,tapply(com,year,length))
	# Mean Abundance of commerical scallops each year.
	N<-with(shf,tapply(com,year,mean))
	# Mean abundance of recruit scallops each year.
	NR<-with(shf,tapply(rec,year,mean))
	# Mean abundance of pre-recruit scallops each year.
	NPR<-with(shf,tapply(pre,year,mean))
	
	# Mean abundance of pre-recruit scallops each year. Note this is SE/mean!
	N.cv<-with(shf,tapply(com,year,sd))/sqrt(n)/N
	# CV is sd/mean (old version had se/NR). Note this is SE/mean!
	NR.cv<-with(shf,tapply(rec,year,sd))/sqrt(n)/NR
	# CV is sd/mean (old version had se/NPR). Note this is SE/mean!
	NPR.cv<-with(shf,tapply(pre,year,sd))/sqrt(n)/NPR

  # Do the same as above but for Biomass data if B == T
	if(B == T)
	  {
	    # Find mean and convert to grams! DK Note:  Careful this *1000 doesn't have ramifications elsewhere!
		  I<-with(shf,tapply(com.bm,year,mean))*1000
  		IR<-with(shf,tapply(rec.bm,year,mean))*1000
  		IPR<-with(shf,tapply(pre.bm,year,mean))*1000
  		# Note this is SE/mean (note the *1000 gets it on same scale as I)
  		I.cv<-with(shf,tapply(com.bm,year,sd))/sqrt(n)/I*1000
  		#Note this is SE/mean (note the *1000 gets it on same scale as IR)
  		IR.cv<-with(shf,tapply(rec.bm,year,sd))/sqrt(n)/IR*1000
  		#Note this is SE/mean (note the *1000 gets it on same scale as IPR)
  		IPR.cv<-with(shf,tapply(pre.bm,year,sd))/sqrt(n)/IPR*1000
 
  	} # end if(B == T)
	
	
	# Now get the abundance and biomass in each SHF bin for each year.
	n.yst <- matrix(NA,nrow=length(years),ncol=length(bin)) 
	colnames(n.yst) <- bin.name
	w.yst <- matrix(NA,nrow=length(years),ncol=length(bin))
	colnames(w.yst) <- wbin.name
	for(i in 1:length(years))
	{
  	ann.dat <- shf[shf$year==years[i],]
  	if(nrow(ann.dat) > 0)
  	{
    	#Get the abundance and biomass data...
    	# Fill the bins with the shell height frequency data.
    	n.yst[i,] <- colMeans(ann.dat[,bin.name],na.rm=T)
    	# We can also get an estimate of the biomass in each bin...
    	w.yst[i,]<- colMeans(ann.dat[,wbin.name],na.rm=T)
  	} # end if(nrow(ann.dat) > 0)
  }	# end for(i in 1:length(years))
  # Put the data together
	surv.dat<-merge(data.frame(year=as.numeric(row.names(N)),n,I,I.cv,IR,IR.cv,IPR,IPR.cv,N,N.cv,NR,NR.cv,NPR,NPR.cv),data.frame(year=years),all=T)
	# Return data for use elsewhere
	shf.dat <- list(n.yst=n.yst,w.yst=w.yst)
	return(list(model.dat = subset(surv.dat,year%in%years),shf.dat=shf.dat))
	
}
