## Scallop Number Formatting

##### x is the number to be formatted. Can only do one number at a time!
##### width is the number of digits desired (while ensuring number is accurate)
##### width=4 will give, 0.010, 0.100, 1.000, 10.00, 100.0, 1000, 10000
##### width=3 will give: 0.01, 0.10, 1.00, 10.0, 100, 1000, 10000
##### width=2 will give: 0.0, 0.1, 1.0, 10, 100, 1000, 10000
##### width=1 will give 0, 0, 1, 10, 100, 1000, 10000

# Examples
# ScallopRound(1235.23, width=3)
#
# ScallopRound(c(0.00010, 0.1, 1.000, 34932, 839.428, 5.2843, 2334.0), 4)


ScallopRound <- function(x, width=3){
  lvls <- 10^(1:(width-1))
  lvls <- c(0,lvls)

  out <- NULL
  for(j in 1:length(x)){
    if(x[j] < lvls[1]){
      digs <- width-nchar(lvls[1])
      rounded <- formatC(x[j], digits=digs, format="f")
    }
    if(x[j] >= lvls[1]){
      for(i in 1:(length(lvls)-1)){
        digs <- width-nchar(lvls[i])
        if(x[j] >=lvls[i] & x[j] < lvls[i+1]) {rounded <- formatC(x[j], digits=digs, format="f")}
      }
      if(x[j] >= lvls[length(lvls)]) {rounded <- formatC(x[j], digits=digs, format="f")}
    }
    out <- c(out, rounded)
  }
  return(out)
}





