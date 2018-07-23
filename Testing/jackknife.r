####  Commented and checked by DK starting on July 28, 2015. This is the jackknife estimator from Smith 1980 (CJFAS)

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "/fn/fishery.dat.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#   ok
#      
##
###############################################################################################################

# Arguments
# data:  The data needed for the calculation
# The err has been removed as it just causes bad things to happen...
# err:   What error are we calculating, the 'sd' which is simply the variance of the Jacknife estimator, or 'se' which is the unbiased variance estimator
#        which is outlined in Smith 1980. Default is 'se' Remember se = sd/sqrt(n), so these are equivalent just different ways of presenting results. 
#        The sd method is surprisingly about 20% slower (if a big dataset expect this to take approximately 10 minutes, smaller can converge very quickly)

#data <- data.frame(effort = c(25,46,58,NA),catch = c(25,46,NA,22),year = c(1820,1821,1822,1823),cats = c(NA,NA,NA,NA))
# source("Y:/Assessment/2010/r/fn/jackknife.r")

jackknife<-function(data)
{
	# Just used to display start/stop times of function
	print(Sys.time())
  # remove any data with NA's.
  data <- na.omit(subset(data,select = c("effort","catch","year")))
	#data<-na.omit(data)
	# Sort the data by year just in case
	yr<-sort(unique(data$year))
	
	# Calculate the annual catch and efforts
	catch<-with(data,tapply(catch,year,sum))	
	effort<-with(data,tapply(effort,year,sum))
	# Then calculate CPUE,used as the starting place for our estimator.
	Cf<-catch/effort
	# The sample size for each year.
	n<-with(data,tapply(effort,year,length))
  # Set up the data.frame for the output.
	out.dat<-data.frame(year=yr,n,catch,effort,cpue=rep(NA,length(yr)),cpue.var=rep(NA,length(yr)))
	
	for (i in 1:length(yr))
	  {
		  # Set Rj = CF from a specific year, basically initializing the variable, though if n[i] = 1 this is our estimator for that year.
		  Rj<-Cf[i]
		  if(n[i]>1)
		    {
		      # Reset Rj if more than one datum this year
			    Rj<-c()
			    # Run the loop for all data from current year.
			    for (j in 1:n[i])
			      {
			      # So run through the data and sequentially remove the "jth" point from the sum of the catch and effort data.
				      Rj[j]<-n[i]*Cf[i] - (n[i]-1)*(sum(data$catch[data$year==yr[i]][-j])/sum(data$effort[data$year==yr[i]][-j]))
			      } # end for (j in 1:n[i])
		    } # end if(n[i]>1)
  		# The mean of the Rj data is our unbiased estimator for year i
  		out.dat$cpue[i]<-mean(Rj)
  		# Now we can extract the variance as simply the variance (sum(obs-mean)^2/(n-1) , or as the Smith(1980) variance, which really just the variance of a s.e.
  		# than the variance of an sd, the difference b/t calcs comes down to only the denomiator (n-1) vs. n*(n-1).  They are related in the usual way...
  		# sqrt(var_se) = sqrt(var_sd)/sqrt(n))
  		# This is = var, to convert to se you must take sqrt(cpue.var)/sqrt(n)
  		# We have removed the sd option from this calculation as it was being used improperly!
  		#if(err=='sd')out.dat$cpue.var[i]<-var(Rj)
  		# This is = var/n to convert to se you just take sqrt(cpue.var)
  		out.dat$cpue.var[i]<-1/(n[i]*(n[i]-1))*sum((Rj-mean(Rj))^2)

  	 } # end for (i in 1:length(yr))
	 # Add CI's
	out.dat$cpue.se <- sqrt(out.dat$cpue.var)
	# Make these t distribution error terms in case we have years where n is really low, as per Smith 1980, pretty much these are normals for our data.
	out.dat$LCI <- out.dat$cpue + qt(0.025,(n-1))*out.dat$cpue.se
	out.dat$UCI <- out.dat$cpue + qt(0.975,(n-1))*out.dat$cpue.se
	# Just used to display start/stop times of function
	print(Sys.time())
	
	# Return the out.dat object
	return(out.dat)
	
} # end jackknife function
	
	
	
	
	
	
	
	