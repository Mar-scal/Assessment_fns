# Commented by DK September 2015.
# This is the summary method for PEDstrata strata package, you would never run this function 
# on it's own, it would simply be run as summary() of a PEDstrata object.
# See a myriad of papers by Smith for details about these calculations


#Arguments
#object:   The survey object which has been processed by PEDstrata.
#alpha.t:  width of confidence intervals (1-alpha.t) based on t distrubuted data.  Default = 0.05 (95% CI) 
#effic    Calculate the survey design efficiency.  (T/F)   Default = FALSE
#nopt:    Calculate the optimatal allocation design.  (T/F) Default = FALSE 
#...:     Other possible options

function (object, alpha.t = 0.05, effic = FALSE, nopt = FALSE, ...) 
{
  # Take the mean catch from each tow in a strata and find the strata mean
  yh <- as.vector(sapply(object$yhi, mean))
  # overall survey estimated mean, multiply strata estimates by proportion of towable area in each strata and sum these.
  yst <- sum(object$Wh * yh, na.rm = TRUE)
  # Calculate the variance within each survey strata based on mean catch from each tow in strata.
  sh <- as.vector(sapply(object$yhi, var))
  # Calculate the survey standard error.  In words... The Total number of towable units in a strata * difference 
  # between Total number of towable units and the actual number of tows in the strata divided by square of total number 
  # of towable units on the bank, multiply all this by variance in each strata, and divided everything by 
  # the number of tows in each strata
  # Finally add this up across all strata and take the square root.  In math this is...
  # (sum((Nh_i * (Nh_i-nh_i)/sum(Nh_i)^2) * var_i) / nh_i)^0.5
  se.yst <- sqrt(sum((((object$Nh * (object$Nh - object$nh))/sum(object$Nh)^2) * 
                        sh)/object$nh, na.rm = TRUE))
  # The difference between Total number of towable units and number of actual tows, multiplied by Number of towable units
  # all divided by actual number of tows.  Returns a value for each strata and is used for the degrees of freedom calculation
  ah <- (object$Nh * (object$Nh - object$nh))/object$nh
  # Degrees of freedom in survey the the sum of 'ah' multiplied by the strata variance all squared divided by
  # the sum of (the square of ah and the variance dvided by number of towable units per strata - 1)
  df.yst <- (sum(ah * sh, na.rm = TRUE)^2)/(sum(((ah * sh)^2)/(object$nh - 1), na.rm = TRUE))
  
  # The 1-alpha.t confidence interval around our mean estimate, CI's are t distributed.                                                              
  ci.yst <- yst + (c(qt(alpha.t/2, df.yst), -qt(alpha.t/2, df.yst)) * se.yst)
  
  # Put results into a list.                                              
  res <- list(yst = yst, se.yst = se.yst, Yst = yst * sum(object$Nh), 
              df.yst = df.yst, alpha = alpha.t, ci.yst = ci.yst)
  # If interested in returning the efficiency of the survey.
  if (effic == TRUE) {
    # Total number of possible tows on the bank
    N <- sum(object$Nh)
    # total number of tows on the bank
    n <- sum(object$nh)
    # Variance if we simply assumed the tows were distributed randomly across the bank.
    # this is the sum of - the proportion of towable area in each strata multiplied by the variance
    # of each strata, multiplied by (1/n - 1/N) which is same as (N-n)/(n*N) - 
    vran <- ((N - n)/(n * N)) * sum(object$Wh * sh, na.rm = TRUE)
    # The strata component of the estimated efficiency of the stratified survey is calculated here, basically   this is 
    #the differencein the variance between strata subtracted from the variance within strata and adjusted for sample size.
    effic.str <- ((N - n)/(n * (N - 1))) * (sum(object$Wh * 
                                                  (yh - yst)^2) - sum((object$Wh * (1 - object$Wh) * 
                                                                         sh)/object$nh, na.rm = TRUE))
    # This is an estimate of the variance of the random survey design
    vran <- vran + effic.str
    # The allocation component of the efficiency.
    effic.alloc <- sum(((1/n) - (object$Wh/object$nh)) * 
                         object$Wh * sh, na.rm = TRUE)
    # Turn the strata component of efficiency into a percentage of the random sampling design variance
    effic.str <- (100 * effic.str)/vran
    # Turn the allocation component of efficiency into a percentage of the random sampling design variance
    effic.alloc <- (100 * effic.alloc)/vran
    # For an optimal allocation survey the minimum variance is the sum of the variance * (proportion of area)^2
    # This is a slick way of combining equations 8 and 9 from Smith 1998 Design/analysis course notes from Norway.
    min.var <- ((1/n) * sum(object$Wh * sqrt(sh), na.rm = TRUE)^2) - 
      sum(object$Wh * sh, na.rm = TRUE)/N
    # Add the efficiency calculations to the results.
    res <- c(res, list(effic.alloc = effic.alloc, effic.str = effic.str, 
                       var.ran = vran, max.eff = (100 * (vran - min.var))/vran))
  }
  # Now if we want to figure out the optimal allocation scheme.
  if (nopt == T) 
    {
    
      # make a dataframe of NA's
      n.opt.out <- as.data.frame(matrix(NA, length(yh) + 1,  6))
      # Give the columns names                                     
      names(n.opt.out) <- c("Strata", "Observed", "Optimal", 
                            "Perc.Increase.Var.opt", "Compromise", "Perc.Increase.Var.comp")
      
      # If the strata is a character or factor add another row
      if (is.character(object$Strata)) n.opt.out$Strata <- c(object$Strata, "Total")
      if (is.factor(object$Strata))    n.opt.out$Strata <- c(levels(object$Strata), "Total")
      # number of tows per strata + totale
      n.opt.out$Observed <- c(object$nh, n.tot <- sum(object$nh))
      
      # Make sure no strata have just one tow.
      if (all(object$nh != 1)) 
        {
          # Calculate the optimal number of tows per strata
          n.opt <- (((sqrt(sapply(object$yhi, var)) * object$Wh)/sum(sqrt(sapply(object$yhi, 
                                                      var)) * object$Wh, na.rm = TRUE)) * sum(object$nh))
          # Round this to a whole number
          n.opt <- round(n.opt)
          # If total number of obeserved tows doesn't match (due to rounding)
          # add the necessary number of tows to the strata with the largest number of tows (this would never be more than the
          # number of strata, typically probably only 1-2 max)
          if (n.tot > sum(n.opt)) 
            {
              n.opt[n.opt == max(n.opt)] <- n.opt[n.opt == 
                                                  max(n.opt)] + (n.tot - sum(n.opt))
            } # end if (n.tot > sum(n.opt)) 
          # If too many tows due to rounding remove extras from strata with most recommended tows.
            if (n.tot < sum(n.opt)) 
              {
                n.opt[n.opt == max(n.opt)] <- n.opt[n.opt == 
                                                  max(n.opt)] - (n.tot - sum(n.opt))
              } # end if (n.tot < sum(n.opt)) 
          
          # The optimal survey design
          n.opt.out$Optimal <- c(n.opt, sum(n.opt))
          # The increase in variance due to the current sub-optimal design.
          perc.increase <- (100 * (((object$nh - n.opt)^2)/object$nh))/sum(object$nh)
          n.opt.out$Perc.Increase.Var.opt <- c(perc.increase, 
                                               sum(perc.increase))
          # If any of the strata have 0 or 1 estimate we need to compromise so that each strata gets at least 2 points
          # and the variance can be calculated.
          n.comp.ind <- (n.opt <= 1)
          # How many strata need tows
          n.comp.opt <- sum(n.comp.ind)
          # make a new compromise N
          n.opt2 <- n.opt
          # For every strata with less than 2 tows give it 2 tows
          n.opt2[n.comp.ind] <- 2
          # Remove tows from strata with the most tows an allocate them to the other strata
          n.opt2[n.opt2 == max(n.opt2)] <- n.opt2[n.opt2 == 
                                                    max(n.opt2)] - (sum(n.opt2) - sum(n.opt))
          # The compromise allocation scheme.
          n.opt.out$Compromise <- c(n.opt2, sum(n.opt2))
          # The increase in variance from the Optimal scenario
          Perc.Increase.Var.comp <- (100 * (((n.opt2 - n.opt)^2)/n.opt2))/sum(object$nh)
          n.opt.out$Perc.Increase.Var.comp <- c(Perc.Increase.Var.comp, 
                                                sum(Perc.Increase.Var.comp))
        } # end if (all(object$nh != 1)) 
      # Add the optimal allocation results to the output.
      res <- c(res, list(n.opt = n.opt.out))
    } # end if(nopt = T)
  options(digits = max(options()$digits - 5, 5))
  c(res, list(descrip = "Stratified Analysis"))
}
<environment: namespace:PEDstrata>