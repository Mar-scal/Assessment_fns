################################################################################################################
####  Commented and checked by DK starting on July 28, 2015. The compares the survey results 
####  for matched and unmatched tows across years and estimates the mean and variance
####  for the current year from these data.  The calculations utilize information 
####  from both the matched and unmatched tows and the differences in these between years to get 
##### the esimated abundance/biomass for each category.
####
################################################################################################################
## Update history
## 1: March 31 2016 by DK, tidying up structure and comments
## 2:  June 2017 by DK:  Enabling caluclations for specific size bins
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary_data.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# ok
#    
#      
##
###############################################################################################################

###############################################################################################################
# Arguments
#dat:    The survey data from spring survey
#years:       The years to subset across.
#towlist:     List of the tows for each year.  Default = list(NULL)
#print:       Print the results to the screen.  Default = F
#chng:        Default = F
#user.bins:  Calculate the biomass of specified user bins, these bins will have had to already been calculated with the surv.by.tow function
#             for this to work!!  Default is NULL which doesn't do any calculations.  If not null user.bins should look something like 
#             user.bins <- c(50,70,90,110)
###############################################################################################################

sprSurv<-function(dat,years,towlist=NULL,print=F,chng=F,user.bins = NULL)
{
  # load the spr package, note this is a locally developed package not available from R repositories
  require(spr)  || stop("spr package required please obtain a copy and install this locally developed package")

  # Make the first element in the list a data.frame with appropriate headers.
  out.obj<-list(NULL)
  out.obj[[1]]<-data.frame(year=years,n=with(subset(dat,year%in%years),tapply(tow,year,length)),
                           I=NA, I.cv=NA, IR=NA, IR.cv=NA, IPR=NA, IPR.cv=NA, N=NA, N.cv=NA, NR=NA, NR.cv=NA, NPR=NA, NPR.cv=NA)

  
  # Initialize a bunch of objects.
  pre.spr.obj<-   list(NULL)
  rec.spr.obj<-   list(NULL)
  com.spr.obj<-   list(NULL)
  tot.spr.obj<-   list(NULL)
  pre.bm.spr.obj<-list(NULL)
  rec.bm.spr.obj<-list(NULL)
  com.bm.spr.obj<-list(NULL)
  tot.bm.spr.obj<-list(NULL)
  
  # And make some more if we have user specified bins...
  if(!is.null(user.bins))
  {
    # Get the names for the user bins and good names for the final results...
    bnames <- paste0("bin_lt_",user.bins[1])
    mean.names <- paste0("mean_lt_",user.bins[1])
    CV.names <- paste0("CV_lt_",user.bins[1])
    for(p in 1:length(user.bins)+1) 
    {
      if(p > 1 && p < length(user.bins)+1) 
      {
        bnames[p] <- paste0("bin_",user.bins[p-1],"-",user.bins[p])
        mean.names[p] <- paste0("mean_",user.bins[p-1],"-",user.bins[p])
        CV.names[p] <- paste0("CV_",user.bins[p-1],"-",user.bins[p])
      } # end if(p > 1 && p < length(user.bins)+1) 
      
      if(p == length(user.bins)+1) 
      {
        bnames[p] <- paste0("bin_",user.bins[p-1],"_plus")
        mean.names[p] <- paste0("mean_",user.bins[p-1],"_plus")
        CV.names[p] <- paste0("CV_",user.bins[p-1],"_plus")
      } # end if(p == length(user.bins)+1) 
    } # End for(p in 1:length(user.bins)+1) 
    bnames <- c(bnames,paste0(bnames,"_bm"))
    mean.names <- c(mean.names,paste0(mean.names,"_bm"))
    CV.names <- c(CV.names,paste0(CV.names,"_bm"))
    tmp <- as.data.frame(matrix(NA,ncol = (length(CV.names) + length(mean.names)+1) ,nrow=length(years)))
    names(tmp) <- c(mean.names,CV.names,"year")
    tmp$year <- years
  } # end if(!is.null(user.bins))
  
  # Make this data.frame if chng == T
  if(chng == T)
	{
	    chng.LY <- as.data.frame(matrix(NA,(length(years)-1),4))
	    names(chng.LY) = c("tot","rec","pre","com")
	} # ebd if(chng == T)
	# Run a loop for each year of interest
	for(i in 1:(length(years)-1))
	{
	  #subset the data for this year and the following year, if there are no user specified bins this will do the trick
	  SurvY1<-subset(dat,year==years[i],c('year','tow','stratum','lat','lon','pre','rec','com','tot',
	                                           'pre.bm','rec.bm','com.bm','tot.bm'))
	  SurvY2<-subset(dat,year==years[i+1],c('year','tow','stratum','lat','lon','pre','rec','com','tot',
	                                             'pre.bm','rec.bm','com.bm','tot.bm'))
	
	  
	  # Grab only the stratum 2 data from towlist
	  # Note that EID in towlist is used by crossref to determine if we have a matched tow or not.
	  # EID being the townumber from the previous year for matched tows.
	  if(is.null(towlist) == T) crossref<-subset(towlist[[i]],stratum==2,c('EID','tow'))
	  if(is.null(towlist) == F) crossref<-subset(towlist[towlist$year == years[i+1],],stratum==2,c('EID','tow'))
	  # rename the tow headers.
	  names(crossref)<-c('tow.y1','tow.y2')
	  
		# The numbers and biomass from the survey with a sampling with partial replacement survey design.
		# this compares the survey results from matched and unmatched tows between years.
		pre.spr.obj[[i]] <-spr(SurvY1$tow,SurvY1$pre,SurvY2$tow,SurvY2$pre,crossref=crossref)
		rec.spr.obj[[i]] <-spr(SurvY1$tow,SurvY1$rec,SurvY2$tow,SurvY2$rec,crossref=crossref)
		com.spr.obj[[i]] <-spr(SurvY1$tow,SurvY1$com,SurvY2$tow,SurvY2$com,crossref=crossref)
		tot.spr.obj[[i]] <-spr(SurvY1$tow,SurvY1$tot,SurvY2$tow,SurvY2$tot,crossref=crossref)
		pre.bm.spr.obj[[i]] <-spr(SurvY1$tow,SurvY1$pre.bm,SurvY2$tow,SurvY2$pre.bm,crossref=crossref)
		rec.bm.spr.obj[[i]] <-spr(SurvY1$tow,SurvY1$rec.bm,SurvY2$tow,SurvY2$rec.bm,crossref=crossref)
		com.bm.spr.obj[[i]] <-spr(SurvY1$tow,SurvY1$com.bm,SurvY2$tow,SurvY2$com.bm,crossref=crossref)
		tot.bm.spr.obj[[i]] <-spr(SurvY1$tow,SurvY1$tot.bm,SurvY2$tow,SurvY2$tot.bm,crossref=crossref)
		
		# For the first year calculate summary statistics from the survey data
		if(i==1)
		{
		  # This pulls the mean for the all tows from the current year and convert to grams
		  #DK Note:  Careful this *1000 doesn't have ramifications elsewhere!
			out.obj[[1]]$I[1]<-spr:::spr.Sumstats(com.bm.spr.obj[[i]])[7,1] *1000
			out.obj[[1]]$IR[1]<-spr:::spr.Sumstats(rec.bm.spr.obj[[i]])[7,1]*1000
			out.obj[[1]]$IPR[1]<-spr:::spr.Sumstats(pre.bm.spr.obj[[i]])[7,1]*1000
			out.obj[[1]]$N[1]<-spr:::spr.Sumstats(com.spr.obj[[i]])[7,1]
			out.obj[[1]]$NR[1]<-spr:::spr.Sumstats(rec.spr.obj[[i]])[7,1]
			out.obj[[1]]$NPR[1]<-spr:::spr.Sumstats(pre.spr.obj[[i]])[7,1]
			
			# This gets the coefficent of variation for that same data, note this uses sd not se
			out.obj[[1]]$I.cv[1]<-sqrt(spr:::spr.Sumstats(com.bm.spr.obj[[i]])[9,1])   / spr:::spr.Sumstats(com.bm.spr.obj[[i]])[7,1]  
			out.obj[[1]]$IR.cv[1]<-sqrt(spr:::spr.Sumstats(rec.bm.spr.obj[[i]])[9,1])  / spr:::spr.Sumstats(rec.bm.spr.obj[[i]])[7,1] 
			out.obj[[1]]$IPR.cv[1]<-sqrt(spr:::spr.Sumstats(pre.bm.spr.obj[[i]])[9,1]) / spr:::spr.Sumstats(pre.bm.spr.obj[[i]])[7,1]
			out.obj[[1]]$N.cv[1]<-sqrt(spr:::spr.Sumstats(com.spr.obj[[i]])[9,1])      / spr:::spr.Sumstats(com.spr.obj[[i]])[7,1]     
			out.obj[[1]]$NR.cv[1]<-sqrt(spr:::spr.Sumstats(rec.spr.obj[[i]])[9,1])     / spr:::spr.Sumstats(rec.spr.obj[[i]])[7,1]    
			out.obj[[1]]$NPR.cv[1]<-sqrt(spr:::spr.Sumstats(pre.spr.obj[[i]])[9,1])    / spr:::spr.Sumstats(pre.spr.obj[[i]])[7,1]   
		} #end if i == 1

		# Now obtain the mean for the rest of the years.
		#DK Note:  Careful this *1000 doesn't have ramifications elsewhere!
		out.obj[[1]]$I[i+1]<-summary(com.bm.spr.obj[[i]])$Yspr *1000
		out.obj[[1]]$IR[i+1]<-summary(rec.bm.spr.obj[[i]])$Yspr *1000
		out.obj[[1]]$IPR[i+1]<-summary(pre.bm.spr.obj[[i]])$Yspr *1000
		out.obj[[1]]$N[i+1]<-summary(com.spr.obj[[i]])$Yspr
		out.obj[[1]]$NR[i+1]<-summary(rec.spr.obj[[i]])$Yspr
		out.obj[[1]]$NPR[i+1]<-summary(pre.spr.obj[[i]])$Yspr
		
		# and grab the CV for the rest of the years.            
		out.obj[[1]]$I.cv[i+1]<-with(summary(com.bm.spr.obj[[i]]),sqrt(var.Yspr.corrected)/Yspr)
		out.obj[[1]]$IR.cv[i+1]<-with(summary(rec.bm.spr.obj[[i]]),sqrt(var.Yspr.corrected)/Yspr)
		out.obj[[1]]$IPR.cv[i+1]<-with(summary(pre.bm.spr.obj[[i]]),sqrt(var.Yspr.corrected)/Yspr)
		out.obj[[1]]$N.cv[i+1]<-with(summary(com.spr.obj[[i]]),sqrt(var.Yspr.corrected)/Yspr)
		out.obj[[1]]$NR.cv[i+1]<-with(summary(rec.spr.obj[[i]]),sqrt(var.Yspr.corrected)/Yspr)
		out.obj[[1]]$NPR.cv[i+1]<-with(summary(pre.spr.obj[[i]]),sqrt(var.Yspr.corrected)/Yspr)
	
		# Now print the year by year results to the screen.
		if(print==T)
		{
  		print(years[i+1])
  		print(tot.spr.obj[[i]])
  	 } # end if(print==T)
		
		# Make another object with only stratum 2 data
		out.obj[[2]]<-crossref
		
		# Calculate the mean and se of change between years from the survey data.
		# This change take the estimate from the previous year and compares this to the current years data to
		# get an estimate fo the mean survey abundance accounting for their being matched tows.
		# for the first year of data it calculates the estimate fo change in the survey mean between the current/previous year
		# with the previous year being estimated as a simple random design sample mean.
		if(chng==T)
		{
  		chng.LY$year<-years[-1]
  		if(i==1)
  		{
    		chng.LY$tot[i] <- change(tot.spr.obj[[i]])[1]
    		chng.LY$com[i] <- change(com.spr.obj[[i]])[1]
    		chng.LY$rec[i] <- change(rec.spr.obj[[i]])[1]
    		chng.LY$pre[i] <- change(pre.spr.obj[[i]])[1]
  	  } # end if(i==1)                 
  	  if(i>1)
  	  {          
    		chng.LY$tot[i] <- change(tot.spr.obj[[i]],summary(tot.spr.obj[[i-1]]))[1]
    		chng.LY$com[i] <- change(com.spr.obj[[i]], summary(com.spr.obj[[i-1]]))[1]
    		chng.LY$rec[i] <- change(rec.spr.obj[[i]],summary(rec.spr.obj[[i-1]]))[1]
    		chng.LY$pre[i] <- change(pre.spr.obj[[i]],summary(pre.spr.obj[[i-1]]))[1]
  		} # end if(i>1)
  		out.obj[[3]]<-chng.LY
		}# end if(chng==T)
		
		# So I need to get the results for the user specified SH bins if they are requested.
		if(!is.null(user.bins))
		{
		  
		  # Now this needs rejigged to grab the user bins if they exist.
		  SurvY1<-subset(dat,year==years[i],c('year','tow','stratum','lat','lon','pre','rec','com','tot',
		                                      'pre.bm','rec.bm','com.bm','tot.bm',bnames))
		  SurvY2<-subset(dat,year==years[i+1],c('year','tow','stratum','lat','lon','pre','rec','com','tot',
		                                        'pre.bm','rec.bm','com.bm','tot.bm',bnames))
		  
		  # Now get the annual estimate for each of our user bins.
		  user.bin.res <- NULL
		  for(k in 1:length(bnames))
		  {
		  # The numbers and biomass from the survey with a sampling with partial replacement survey design.
		  # this compares the survey results from matched and unmatched tows between years.
		    user.bin.res[[bnames[k]]] <- spr(SurvY1$tow,SurvY1[,bnames[k]],SurvY2$tow,SurvY2[,bnames[k]],crossref=crossref)
		    if(i==1) 
		    {
		      tmp[i,(k)] <- spr:::spr.Sumstats(user.bin.res[[bnames[k]]])[7,1]
		      tmp[i,(k + length(bnames))] <- sqrt(spr:::spr.Sumstats(user.bin.res[[bnames[k]]])[9,1])   / spr:::spr.Sumstats(user.bin.res[[bnames[k]]])[7,1]  
		    }
		    # Now obtain the mean for the rest of the years, something screwy here...
		    tmp[i+1,(k)] <- summary(user.bin.res[[bnames[k]]])$Yspr 
		    tmp[i+1,(k + length(bnames) )] <-  with(summary(user.bin.res[[bnames[k]]]),sqrt(var.Yspr.corrected)/Yspr)
		  } # end for(k in 1:length(mean.names))
		} # end if(!is.null(user.bins))
	} # end the for(i in 1:(length(years)-1))
  

  # If we calculated the user bins we need to merge these 2 objects together.
  if(!is.null(user.bins)) 
  {
    # First convert the biomasses into grams to be consistent with the biomass calculations below, CV's are fine as is since it is scale invariant
    tmp[,which(grep("mean",names(tmp)) %in% grep("bm",names(tmp)))] <-
      tmp[,which(grep("mean",names(tmp)) %in% grep("bm",names(tmp)))]*1000
    out.obj[[1]] <- merge(out.obj[[1]],tmp)
  } # end if(!is.null(user.bins)) 
  # Return the results.
  if(is.null(user.bins)) return(out.obj)
  if(!is.null(user.bins)) return(list(out.obj = out.obj,bin.names = bnames,user.bins = user.bins))
	
}# end function




