####  Created as standalone by DK September 2015. This defines ticks/strata boundaries using sqrt(f(y)) rule

####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "contour.gen.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################

#	source(".../contour/tick.def.r")

# Arguments
#char:       The data, a vector of Z values.
#nstrata:    Number of strata.  Default = 4
#min.str:    Set the min value for str. Default = 0
#max.str:    Set the max value for str.  Default = missing which takes the maximum of char data
#place:      Number of decimal places to round the data to. Default = 0




tick.def<-function(char,nstrata=4,min.str=0,max.str,place=0)
  {
  
  # Round the data to accuracy of place
  bin<-round(char,place)
  # if max.str is not specified set it from the data
  if(missing(max.str)==T) max.str<-max(bin)
  
  # sort the data into bins for bins > 0
  bins<-sort(unique(bin[bin>0]))
  # Set some variable names
  vars<-c()
  avg<-c()
  N<-c()
  srN<-c()
  
  # Loop through the data.
  for(i in 1:length(bins))
    {
      # Take the mean, variance and sample size for each bin.
      avg[i]<-mean(char[bin==bins[i]])
      vars[i]<-var(char[bin==bins[i]])
      N[i]<-length(char[bin==bins[i]])
      srN[1]<-sqrt(N[1])
      # For i > 1 add square root of sample size to square root of previous sample size.
      if(i>1) srN[i]<-sqrt(N[i])+srN[i-1]
    } # end for(i in 1:length(bins))
  
  # The best split is to take the maximum of SrN, divide by the number of strata and multiply by 1:nstrata
  ideal.div<-max(srN)/nstrata*(1:nstrata)
  ind<-c()	
  # Loop through each of the ideal.div and find which srN is closest to the ideal division point for each strata
  for(i in 1:length(ideal.div))
    {
      ind[i] <- which(srN==srN[abs(srN-ideal.div[i])==min(abs(srN-ideal.div[i]))])
    }	# end for(i in 1:length(ideal.div))
  
  # Move from the minimum bin value specified and set the bin values based on how the strata were specified.
  str<-c(min.str,bins[ind]) 
  # Set the final value to the maximum.
  str[length(str)] <- max.str
  
  # Return str to the function calling this.
  str
  
}# end function
