## Scallop Number Formatting

##### x is the number to be formatted. Can only do one number at a time!
##### digits is the number of digits desired (while ensuring number is accurate)
##### options can be "presentation", "int" or "round". You will have to test these out to pick your preferred option

# Examples
x <- c(4.02233, 52.04, 0.256, 4530.44, -0.401, -1.74, -1203.55)
# > ScallopRound(x, digits=3, option="round")
# [1] "4.02"   "52.04"  "0.256"  "4530"   "-0.401" "-1.74"  "-1200" 
# > ScallopRound(x, digits=3, option="int")
# [1] "4.02"   "52.04"  "0.256"  "4530"   "-0.401" "-1.74"  "-1203" 
# > ScallopRound(x, digits=3, option="presentation")
# [1] "4.02"  "52.0"  "0.26"  "4530"  "-0.40" "-1.74" "-1204"

ScallopRound <- function(x, digits=3, option="presentation"){
  
  if(option=="presentation"){
    f <- function(x, digits){
      lvls <- 10^(1:(digits-1))
      lvls <- c(0,lvls)
      
      for(i in 1:length(lvls)){
        digs <- digits-nchar(lvls[i])
        if(i < length(lvls) &
           (abs(x) < lvls[i] | abs(x) >=lvls[i] & abs(x) < lvls[i+1]) & !is.na(x) & !x == 0) {
          return(formatC(x, digits=digs, format="f"))
        }
        if(i < length(lvls) & (abs(x) == 0 & !is.na(x))) {
          return(formatC(x, digits=0, format="f"))
        }
        if(i == length(lvls) & !is.na(x)){
          if(abs(x) >= lvls[i]) return(formatC(x, digits=digs, format="f"))
        }
        if(is.na(x)) return(NA)
      }
    }
    return(unlist(purrr::map(x, function(x) f(x, digits))))
  }
  
  if(option %in% c("round", "int")) {
    y <- rep(NA,length(x))
    for(i in 1:length(x))
    {
      # Stuff between 1 and 10 is easy peasy
      if((x[i] <= -1 & x[i] > -10) | (x[i] >= 1 & x[i] < 10)) y[i] <- formatC(x[i],digits = digits-1,format='f')
      # Stuff between 10 and 100, here is where I have some 'illogical' behaviour, but I think the 10-99 stuff should have (or at least usually has) 1 decimal place for reporting.
      if((x[i] <= -10 & x[i] > -100) | (x[i] >= 10 & x[i] < 100)) y[i] <-  format(x[i],digits = digits+1,nsmall = digits-1)
      # Stuff that is between -1 and 1, the nsmall forces it to print 2 decimal places
      if(x[i] > -1 & x[i] < 1) y[i] <- format(x[i],digits = digits,nsmall=digits)
      # Now for the 100 and larger numbers.  Here we can round them to x digits, or just make them an integer and drop any decimal places.
      if(option == 'round')  if(x[i] <= -100 | x[i] >= 100)  y[i] <- signif(x[i],digits) 
      if(option == 'int')       if(x[i] <= -100 | x[i] >= 100) y[i] <- as.integer(x[i])
    }
    return(y)
  }    
}

