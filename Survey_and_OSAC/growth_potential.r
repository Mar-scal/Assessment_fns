################################################################################################################
####  This function is used to obtain the average size of scallop in a tow and to estimate their growth potential
###   Based on the MW-SH relationship and the average size of the scallop.
####
# Update history
# Sept 13, 2018:  Version 1
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
#dat:          The survey data for an area of interest, by default the data is subset to the current year
#mwsh.fit:     Meat weight shell height model fit, required only if pred.mw =T
#von.b:        The von.b parameters.  If entering here these need entered as c(Linf,K,to).
#year:         The year you want to calculate this for.  By default this runs only the most recent year in the data you provide.
#              I chose this behaviour for the growth poential part of the code b/c we'd typically only have the MW-SH relationship
#              for the current year.  
#pred.mw       Do you want to get the predictied meat weight for next year.  You can only do this for the year that your MW-SH relationship is valid
#              and you'll have to provide the mwsh fit object (specifically the relationship for each tow)
###############################################################################################################

grow.pot <- function(dat= NULL, mwsh.fit=NULL, von.b = NULL, year=NULL,bank = NULL,dirt = direct,pred.mw =T)
{

  if(is.null(dat))
  {
    cat("Ummm...  In the growth potential function you need to provide the 'dat' object 
          which contains the abundances and shell height bins, nice try tho...")
    stop()
  }
  if(is.null(mwsh.fit) && pred.mw == T) 
  {
    cat("Hallo... In the growth potential function if you area trying to predict mw next year you need to provide the 'mwsh.fit' object 
          which is the MW-SH relationship. Blessed be the meat.")
    stop()
  }
  if(is.null(von.b) && bank != "GBa") 
  {
  cat("Heads up!!!  In the growth potential function you didn't provide the von B parameters and specified a bank other than GBa, therefore
             we are using the von B parameters for Browns Bank North... Under his eyes")
  }
  # If year is not supplied than obtain from the data, this is meant to run for just one year of data, by default it picks the most recent year of data
	if(is.null(year)==T) year<-max(unique(dat$year))
	# You shouldn't be calculating the mw.predicted for more than 1 year at a time...
	if(length(year) > 1 && pred.mw == T) cat("Whoa, hold up bud...  In the growth potential function you shouldn't be calculating the predicted meat weight 
	                                         for multiple years as you only have the MW-SH for 1 year, I'll let you do it, but it's not a good plan dude")
	
	# Subset the data to the year(s) of interest, noting the above warning...
  dat <- dat[dat$year %in% year,]
	# for easier indexing of shell height bins in dat, this will work until we name another column starting with an h...
	# Note we take off half the difference b/t size bins, which effectively gives us the average size for each bin.
	shf.bins <- names(dat[grep("^h",names(dat))])
	bin <- as.numeric(substr(shf.bins,2,nchar(shf.bins)))
	bin <- bin - c(bin[1]/2,diff(bin/2))

	# If you provided the Von B parameters
	if(!is.null(von.b)) Von.B <- von.b
	# If you didn't provide the von.b object then we go and get the values from the one place we have them stored at the moment.
	if(is.null(von.b)) 
	{
	  Von.B <- read.csv(paste(direct,"Data/Ageing/Von_B_growth_parameters.csv",sep=""))
	  # If you want the GBa data then we grab it, if you specify any other bank then you get BBn growth parameters b/c that's all we got right now for offshore
	  # This could easily be expanded to work for inshore if we want...
	  if(bank == "GBa") Von.B <- Von.B[Von.B$Bank == "GBa",c("Linf","K","to")]
	  if(bank != "GBa") Von.B <- Von.B[Von.B$Bank == "BBn",c("Linf","K","to")]
	} # if(is.null(von.b)) 
	
	# Now we get the average size for each tow. it's a weighted sum of the size of each bin.
	dat$cur.sh <- apply(dat[,shf.bins],1,function(x) sum(x*bin)/sum(x))
	dat$cur.sh[is.nan(dat$cur.sh)] <- NA # Tows with no scallop in them
	# Now we can predict how much we expect the shell height to increase next year
	dat$pred.sh <- Von.B$Linf*(1-exp(-Von.B$K)) + exp(-Von.B$K) * dat$cur.sh
	# In case the average size is larger than the von B's L inf, allow the SH to grow but just a little bit...
	# I went with positive growth rather than no growth as it makes the modeling much cleaner (I can use a Gamma or log-normal model..)
	dat$pred.sh[which(dat$pred.sh < dat$cur.sh)] <- dat$cur.sh[which(dat$pred.sh < dat$cur.sh)] +0.5
	# Basically saying that size and above has very slow growth, which should be find for the very few times we have scallop on average > 151 mm!
	dat$gp.sh <- (dat$pred.sh - dat$cur.sh) / dat$cur.sh
	
	# For the tows we have the MW-SH relationship we could also calculate the MW growth potential which assumes the MW-SH relationship doesn't
	# change in this tow over the next year, the nice bit is this accounts for depth, the crappy bit is we lose information for tows 
	# that weren't sampled.  We could do better, but this is o.k. as a start I think...
	# So for the tows we have mw-sh model we can predict MW for next year based on our expected sh growth
	# Note that this should only be done for the year in which the MW-SH data is applicable
	if(pred.mw == T)
	{
	 # This is the assumed offshore allometry, we'd have to change this up for inshore...
	 dat$cur.mw[dat$tow %in% mwsh.fit$fit$tow] <- mwsh.fit$fit$a*(dat$cur.sh[dat$tow %in% mwsh.fit$fit$tow]/100)^3
	 dat$pred.mw[dat$tow %in% mwsh.fit$fit$tow] <- mwsh.fit$fit$a*(dat$pred.sh[dat$tow %in% mwsh.fit$fit$tow]/100)^3
	 dat$gp.mw <- (dat$pred.mw - dat$cur.mw) / dat$cur.mw
	}

cols <- c("ID","year","tow","cruise","bank","date","slat","slon","elat","elon","depth","state","random","month","survey",
          "cur.sh" ,"pred.sh", "gp.sh", "cur.mw","pred.mw","gp.mw")
potential.growth <- dat[,cols]

return(potential.growth)

} # end function
