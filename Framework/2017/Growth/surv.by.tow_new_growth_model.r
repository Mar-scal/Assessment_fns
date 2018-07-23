####  Commented and checked by DK starting on July 28, 2015.  This is adding the calculation of the Pre-recruits
#### Recruits, total number of scallops, and l.bar and possibly meat count into our survey object.
# Update history
# Cleaned up and commented by DK March 31, 2016
# March 10, 2017 - Started revisions, but decided to do the calcs in-line rendering this function obsolete...
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
#shf.dat:       The data of interest
#years:         The years of interest
#type:          The type of results of interest.  'N' = default and returns numbers.  'B' returns biomass.  
#               argument type="ALL" which calculates N, B and MC, where the MC is calculated as (0.5×N)/BM
#pre.ht:        Maximum size of pre-recruits.  Default = 80, 
#rec.ht:        Maximum size of recruits.  Default = 100,  
#mw.pred.dat:   The predictions from the MW-SH model!
#bar.ht:    Height range of scallops.  Default ='com' which selects the size range of commerical scallops




# source("Y:/Assessment/2009/r/fn/surv.by.tow.r")


surv.by.tow<-function(shf.dat, years, type='N', pre.ht=80, rec.ht=100, mw.pred.dat = NULL ,bar.ht='com')
{
	# THis is silly but needed to adjust size bins given how the bins are picked below. Note that the bin names correspond
  # to the maximum height in the bin, so bin h80 has 75-79.9 mm scallop in it.
  pre.ht <- pre.ht+5
	rec.ht <- rec.ht+5
  # This grabs the "bin' numbers for the shell height data
	bin<-as.numeric(substr(names(shf.dat),2,nchar(names(shf.dat))))
	# If years isn't entered it is all years in shf.dat
	if(missing(years))years<-sort(unique(shf.dat$year))
	#  If just 1 number entered for pre-recruit height make it an object with that same value for all years
	if(length(pre.ht==1))pre.ht<-rep(pre.ht,length(years))
	#  If just 1 number entered for recruit height make it an object with that same value for all years
	if(length(rec.ht==1))rec.ht<-rep(rec.ht,length(years))
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
	  
	  # If we are looking for the biomass we need the data to predict on!!
	  if((type=='B' || type == "ALL") && is.null(mw.pred.dat)) 
	  {
      print("Hey bub, if you want Biomass and meat count you need to supply me the Meat-Weight Shell height data to predict on!!")
	  }	# end if((type=='B' || type == "ALL") && is.null(mw.pred.dat)) 
	  
	  # Only run this if requested and if we have data to predict on!!
		if((type=='B' || type == "ALL") && !is.null(mw.pred.dat))
		  {
			
		  pred.dat <- mw.pred.dat[mw.pred.dat$year == years[i]]
		  
			# Grab the meat weight bins
			mw.bin<-na.omit(bin)
			# Now subset the data to the current year.
			ann.dat<-subset(shf.dat,year==years[i])
      

			# D.K. Note:  If we don't assume the allometric relationship (i.e. CF = W/H^3) can we still do this, I feel like not so much, but maybe
			# would could just change the 3 to whatver the real B paramter is and do it that way, need to think more about it.
			if(mw.par!='annual'&& mw.par!='fixed') mw[[i]]<-sweep(matrix((seq(2.5,200,5)/100)^3,nrow(ann.dat),40,byrow=T,dimnames=list(ann.dat$tow,mw.bin)),
			                                                      1,FUN='*',ann.dat[,mw.par])
			
			
			# Now we can grab the biomass of the pre-recruits
			shf.dat$pre.bm[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i],which(bin==5):which(bin==pre.ht[i])) *
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
			# Range of the commercial scallop size classes.
			if(bar.ht=='com') wbar.ht<-c(rec.ht[i],200)
			# Range user specified
			else wbar.ht<-bar.ht
			# Grab the mean weight for each tow
			shf.dat$w.bar[shf.dat$year==years[i]] <- rowSums(subset(shf.dat,year==years[i],which(bin==wbar.ht[1]): which(bin==wbar.ht[2])) * 
			                                                   mw[[i]][,which(mw.bin==wbar.ht[1]): which(mw.bin==wbar.ht[2])]) /
			                                         rowSums(subset(shf.dat,year==years[i], which(bin==wbar.ht[1]):which(bin==wbar.ht[2])))
	
		  } # end if(type=='B')
	  } # end for(i in 1:length(years))
	
# Finally to get the meat counts, DK added Sept 2015 and revised March 2017 to get meat counts for each size, only run if requesting
# everything and you have data to predict on above!
	if(type == "ALL" && !is.null(mw.pred.dat)) 
	{
	  shf.dat$meat.count.tot <- 0.5* shf.dat$tot / shf.dat$tot.bm
	  shf.dat$meat.count.com <- 0.5* shf.dat$com / shf.dat$com.bm
	  shf.dat$meat.count.rec <- 0.5* shf.dat$rec / shf.dat$rec.bm
	  shf.dat$meat.count.pre <- 0.5* shf.dat$pre / shf.dat$pre.bm
	}
	  
	
	
	# return the object..
	shf.dat
	
} # end function
	

