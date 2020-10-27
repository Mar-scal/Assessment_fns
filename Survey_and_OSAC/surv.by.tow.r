####  Commented and checked by DK starting on July 28, 2015.  This is adding the calculation of the Pre-recruits
#### Recruits, total number of scallops, and l.bar and possibly meat count into our survey object.
# Update history
# Cleaned up and commented by DK March 31, 2016
# June 2017:  Added the ability to caluculate these for user specified SH bins.
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

# Arguments
#shf.dat:   The data of interest
#years:     The years of interest
#type:      The type of results of interest.  'N' = default and returns numbers.  'B' returns biomass.  'MC' returns the meat count.
#           DK Sept 2015 added an argument type="ALL" which calculates N, B and MC, where the MC is calculated as (0.5?N)/BM
#pre.ht:    Maximum size of pre-recruits.  Default = 80, 
#rec.ht:    Maximum size of recruits.  Default = 100,  
#htwt.fit:  Height/weight model fit.  Default = NULL
#A:         'A' coefficent of Height/weight model.  Default = htwt.fit$A
#B:         'B' coefficient of Height/weight model.  Default = htwt.fit$B
#mw.par:    Meat weight parameter.  Default ='fixed'.  'annual' is a second options,
#           other options are allowed (e.g. 'CF') and must match a column name in shf.dat.
#bar.ht:    Height range of scallops.  Default ='com' which selects the size range of commerical scallops
#mc:        Meat count, this needs to be carefully specified. if type = "MC"  Default = 33
#user.bins: This allows for user specified bin sizes to be selected.


# source("Y:/Assessment/2009/r/fn/surv.by.tow.r")


surv.by.tow<-function(shf.dat, years, type='N', pre.ht=80, rec.ht=100, htwt.fit=NULL, A=htwt.fit$A , user.bins = c(50,70,85,95,110),
                      B=htwt.fit$B, mw.par='fixed',bar.ht='com', mc=33)
{

  # Get the names for the user bins
  bnames <- paste0("bin_lt_",user.bins[1])
  for(i in 1:length(user.bins)+1) 
  {
    if(i > 1 && i < length(user.bins)+1) bnames[i] <- paste0("bin_",user.bins[i-1],"-",user.bins[i])
    if(i == length(user.bins)+1) bnames[i] <- paste0("bin_",user.bins[i-1],"_plus")
  } # End for(i in 1:length(user.bins)+1) 
  bnames <- c(bnames,paste0(bnames,"_bm"))
  
  # Make a temporary object of correct length and make sure it has proper names
  tmp <- as.data.frame(matrix(NA,nrow=nrow(shf.dat),ncol = length(bnames)))
  names(tmp) <- bnames
  # Now merge it with the shf.dat object
  shf.dat <- as.data.frame(cbind(shf.dat,tmp))
  
	# THis is silly but needed to adjust size bins given how the bins are picked below.
  pre.ht <- pre.ht+5
	rec.ht <- rec.ht+5
	user.bins <- user.bins + 5
  # This grabs the "bin' numbers for the shell height data
	bin<-as.numeric(substr(names(shf.dat),2,nchar(names(shf.dat))))
	# If years isn't entered it is all years in shf.dat
	if(missing(years))years<-sort(unique(shf.dat$year))
	#  If just 1 number entered for pre-recruit height make it an object with that same value for all years
	if(length(pre.ht)==1)pre.ht<-rep(pre.ht,length(years))
	#  If just 1 number entered for recruit height make it an object with that same value for all years
	if(length(rec.ht)==1)rec.ht<-rep(rec.ht,length(years))
	# Make a mw object if needed later.
	mw<-list(NULL)
	
	
	# Run the loop through all the years, this is run as year specific in cae the size classifications has changed over the years.
	for(i in 1:length(years))
	  {
  	  # If we are looking for the numbers
  		if(type=='N' || type == "ALL")
  		  {
  			  # Grab all the data from the smallest bin to the bin before the pre-recruit maximum and add those together.
    			shf.dat$pre[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i], which(bin==5):(which(bin==pre.ht[i])-1)))
    			# Grab all the recruit sized data and add those together
    			shf.dat$rec[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i], which(bin==pre.ht[i]):(which(bin==rec.ht[i])-1)))
    			# Grab all the commerical sized data and add those together
    			shf.dat$com[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i], which(bin==rec.ht[i]):which(bin==200)))
    			# Add together all of the scallop data, irrespective of size.
    			shf.dat$tot[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i], which(bin==5):which(bin==200)))
    			
    			# This get the abundance estimate for each of the user specified SH bins.
    			for(j in 1:(length(user.bins)+1))
    			{
    			  # Grab all the dat from the smallest bin to the first specified bin.
    			  if(j == 1) shf.dat[shf.dat$year==years[i],][bnames[j]] <- rowSums(subset(shf.dat,year==years[i], which(bin==5):(which(bin==user.bins[j])-1)))
    			  # Grab all the data for the intermediate user supplied bins
    			  if(j > 1 && j < (length(user.bins)+1))   
    			  {
    			    shf.dat[shf.dat$year==years[i],][bnames[j]] <- rowSums(subset(shf.dat,year==years[i], which(bin==user.bins[j-1]):(which(bin==user.bins[j])-1)))
    			  }# end if(j > 1 && j < length(user.bins))   
    			  # Grab all the data for the largest user supplied bin.
    			  if(j == length(user.bins)+1) 
    			  {
    			    shf.dat[shf.dat$year==years[i],][bnames[j]] <- rowSums(subset(shf.dat,year==years[i], which(bin==user.bins[j-1]):which(bin == 200)))
    			  } #end if(j == length(user.bins)) 
    			    
    			} # end for(j in 1:length(user.bins))
    			
    			# Range of the commercial scallop size classes.
    			if(bar.ht=='com') lbar.ht <- c(rec.ht[i],200)
    			# Otherwise use the range given in the function call.
    			else lbar.ht<-bar.ht
    			# 
    			# The sweep command multiplies each point in a column by the average shell height in that columns shell height bin for commerical sized scallops
    			# each row is then added together.  This is divided by the sum of each row (without doing the shell-height multiplication) 
    			# This gives us the mean shell height from each tow
    			shf.dat$l.bar[shf.dat$year==years[i]] <- rowSums(sweep(subset(shf.dat,year==years[i], 
    			                                         which(bin==lbar.ht[1]):which(bin==lbar.ht[2])),2,FUN='*',seq(lbar.ht[1]-2.5,lbar.ht[2],5)))/
    			                                         rowSums(subset(shf.dat,year==years[i], which(bin==lbar.ht[1]):which(bin==lbar.ht[2])))
    		
  		  } # end if(type=='N')
	  
	  # If we are looking for the biomass
		if(type=='B' || type == "ALL")
		  {
			# Grab the meat weight bins
			mw.bin<-seq(5,200,5)
			# Now subset the data to the current year.
			ann.dat<-subset(shf.dat,year==years[i])
			# If we have set mw.par to be annual we need results from a model fit, I assume this is a Meat Weight shell height model with
			# different coefficients for each year.  I believe this would be with year as a random effect given the lower case 'a' and 'b'
			if(mw.par=='annual') mw[[i]] <- matrix(exp(log(seq(2.5,200,5))*htwt.fit$b[i]+log(htwt.fit$a[i])),nrow(ann.dat),40,byrow=T,
			                                       dimnames=list(ann.dat$tow,mw.bin))
			# If mw.par is fixed this appears to be a MW-SH model in which we are interested only in the 
			# fixed effects, these results could well be from the same model and here we just want the overall trend 'igroring' year-year vunerability.
			if(mw.par=='fixed') mw[[i]]<-matrix(exp(log(seq(2.5,200,5))*B+A),nrow(ann.dat),40,byrow=T,dimnames=list(ann.dat$tow,mw.bin))
			
			# If neither of these options we make a matrix of the cube of the average shell height.  We then muliply this by the condition factor 
			# to find the meat weight of each bin
			# D.K. Note:  If we don't assume the allometric relationship (i.e. CF = W/H^3) can we still do this, I feel like not so much, but maybe
			# would could just change the 3 to whatver the real B paramter is and do it that way, need to think more about it.
			if(mw.par!='annual'&& mw.par!='fixed') mw[[i]]<-sweep(matrix((seq(2.5,200,5)/100)^3,nrow(ann.dat),40,byrow=T,dimnames=list(ann.dat$tow,mw.bin)),
			                                                      1,FUN='*',ann.dat[,mw.par])
			# Now we can grab the biomass of the pre-recruits
			shf.dat$pre.bm[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i],which(bin==5):(which(bin==pre.ht[i])-1)) *
			                                                    mw[[i]][,which(mw.bin==5):(which(mw.bin==pre.ht[i])-1)])/1000
			# Now we can grab the biomass of the recruits
			shf.dat$rec.bm[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i],which(bin==pre.ht[i]):(which(bin==rec.ht[i])-1)) *
			                                                    mw[[i]][,which(mw.bin==pre.ht[i]):(which(mw.bin==rec.ht[i])-1)])/1000
			# Now we can grab the biomass of the commerical scallops
			shf.dat$com.bm[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i],which(bin==rec.ht[i]): which(bin==200)) *
			                                                    mw[[i]][,which(mw.bin==rec.ht[i]): which(mw.bin==200)])/1000
			# Total biomass
			shf.dat$tot.bm[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i],which(bin==5):which(bin==200))*
			                                                    mw[[i]][,which(mw.bin==5):which(mw.bin==200)])/1000
			
			# This get the biomass estimate for each of the user specified SH bins.
			count <- 0 # I need a couple of indexes here so I'll use count as the seconardary index.
			for(j in (length(user.bins)+2):(length(bnames))) # Weird index but should do the trick to get all the bm ones.
			{
			  count <- count +1
			  # Grab all the dat from the smallest bin to the first specified bin.
			  if(count == 1) shf.dat[shf.dat$year==years[i],][bnames[j]] <-rowSums(subset(shf.dat,year==years[i],which(bin==5):(which(bin==user.bins[count])-1)) *
			                                                                       mw[[i]][,which(mw.bin==5):(which(mw.bin==user.bins[count])-1)])/1000
			  # Grab all the data for the intermediate user supplied bins
			  if(count > 1 && count < (length(user.bins)+1))   
			  {
			    shf.dat[shf.dat$year==years[i],][bnames[j]]<-rowSums(subset(shf.dat,year==years[i],which(bin==user.bins[count-1]):(which(bin==user.bins[count])-1))*
			                                                             mw[[i]][,which(mw.bin==user.bins[count-1]):(which(mw.bin==user.bins[count])-1)])/1000
			  }# end if(j > 1 && j < length(user.bins))   
			  # Grab all the data for the largest user supplied bin.
			  if(count == length(user.bins)+1) 
			  {
			    shf.dat[shf.dat$year==years[i],][bnames[j]] <-  rowSums(subset(shf.dat,year==years[i],which(bin==user.bins[count-1]): which(bin==200)) *
			                                                              mw[[i]][,which(mw.bin==user.bins[count-1]): which(mw.bin==200)])/1000
			  } #end if(j == length(user.bins)) 
			  
			} # end for(j in 1:length(user.bins))

			# Range of the commercial scallop size classes.
			if(bar.ht=='com')wbar.ht<-c(rec.ht[i],200)
			# Range user specified
			else wbar.ht<-bar.ht
			# Grab the mean weight for each tow
			shf.dat$w.bar[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i],which(bin==wbar.ht[1]): which(bin==wbar.ht[2])) * 
			                                                   mw[[i]][,which(mw.bin==wbar.ht[1]): which(mw.bin==wbar.ht[2])]) /
			                                         rowSums(subset(shf.dat,year==years[i], which(bin==wbar.ht[1]):which(bin==wbar.ht[2])))
	
		  } # end if(type=='B')
	  } # end for(i in 1:length(years))
	
	# Finally if we want to pull in the meat counts. Note this is not done within the for loop as there is no pre-recruit groups here.
	# Not clear whether this option is used any longer.
	if(type=='MC')
	  {
		
  		# meat weight matrix
  		mw.bin<-seq(5,200,5)
  		# We pull out the Meat Weights in each bin based on the mw.par column (often this is condition factor)
  		mw<-sweep(matrix((seq(2.5,200,5)/100)^3,nrow(shf.dat),40,byrow=T,dimnames=list(shf.dat$tow,mw.bin)),1,FUN='*',shf.dat[,mw.par])
      # This is the meat count shell height relationship, important here to have properly selected 'mc'.
  		mcsh<-(500/mc/shf.dat[,mw.par])^(1/3)
  		# Multiply mcsh by 20 round it up, then multiple by 5 to get it into bins by 5.
  		shf.dat$sh<-ceiling(mcsh*20)*5
  		# Anything larger than 195 should be 195.
  		shf.dat$sh[shf.dat$sh>195|is.na(shf.dat$sh)]<-195
      # A proportional adjustment to the data based up the mcsh calculation
  		propadj<-sapply(1:nrow(shf.dat),function(i){c(ceiling(mcsh[i]*20)-mcsh[i]*20,rep(1,40-shf.dat$sh[i]/5))})
  		# A slightly different adjustment to data based on mcsh calculation
  		propadj2<-sapply(1:nrow(shf.dat),function(i){c(rep(1,shf.dat$sh[i]/5-1),mcsh[i]*20-floor(mcsh[i]*20))})
  		
  		# Now loop through each row of shf.dat.
  		for(i in 1:nrow(shf.dat))
  		  {
    			# Meat count calculation using propadj
  		    shf.dat$uMC[i] <- sum(shf.dat[i,which(bin==shf.dat$sh[i]): which(bin==200)]*propadj[[i]])/1000
  		    # Meat count calculation using propadj2
  		    shf.dat$oMC[i] <- sum(shf.dat[i,which(bin==5):which(bin==shf.dat$sh[i])]*propadj2[[i]])/1000
    			# As above but using biomass.
  		    shf.dat$uMC.bm[i] <- sum(shf.dat[i,which(bin==shf.dat$sh[i]): which(bin==200)] * 
    			                           mw[i,which(mw.bin==shf.dat$sh[i]): which(mw.bin==200)] *propadj[[i]])/1000
    			shf.dat$oMC.bm[i] <- sum(shf.dat[i,which(bin==5):which(bin==shf.dat$sh[i])] * 
    			                           mw[i,which(mw.bin==5):which(mw.bin==shf.dat$sh[i])]*propadj2[[i]])/1000
  		  } # end for(i in 1:nrow(shf.dat))

	  } # end if(type=='MC')

# Finally to get the meat counts, DK added Sept 2015.
	if(type == "ALL") shf.dat$meat.count <- 0.5* shf.dat$tot / shf.dat$tot.bm
	
	# return the object..
	shf.dat
	
} # end function
	

