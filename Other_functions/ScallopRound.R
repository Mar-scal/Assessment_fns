## Scallop Number Formatting

##### x is the number to be formatted. Can only do one number at a time!
##### width is the number of digits desired (while ensuring number is accurate)
##### width=4 will give, 0.010, 0.100, 1.000, 10.00, 100.0, 1000, 10000
##### width=3 will give: 0.01, 0.10, 1.00, 10.0, 100, 1000, 10000
##### width=2 will give: 0.0, 0.1, 1.0, 10, 100, 1000, 10000
##### width=1 will give 0, 0, 1, 10, 100, 1000, 10000

# Examples
# ScallopRound(4748.34, width=3)
# ScallopRound(0.0001, width=3)
# ScallopRound(0.1, width=3)
#
# purrr::map(c(0.00010, 0.1, 1.000, 34932, 839.428, 5.2843, 2334.0), function(x) ScallopRound(x, width=3))


### single value version

# ScallopRound <- function(x, width=3){
#   lvls <- 10^(1:(width-1))
#   lvls <- c(0,lvls)
#
#   for(i in 1:length(lvls)){
#     digs <- width-nchar(lvls[i])
#     if(i < length(lvls) &
#        (x < lvls[i] | x >=lvls[i] & x < lvls[i+1])) {
#       return(formatC(x, digits=digs, format="f"))
#     }
#     if(i == length(lvls)){
#       if(x >= lvls[i]) return(formatC(x, digits=digs, format="f"))
#     }
#   }
# }


#### purrr version for vector (not working)


ScallopRound <- function(x, width=3){

  f <- function(x, width){
    lvls <- 10^(1:(width-1))
    lvls <- c(0,lvls)

    for(i in 1:length(lvls)){
      digs <- width-nchar(lvls[i])
      if(i < length(lvls) &
         (x < lvls[i] | x >=lvls[i] & x < lvls[i+1]) & !is.na(x)) {
        return(formatC(x, digits=digs, format="f"))
      }
      if(i == length(lvls) & !is.na(x)){
        if(x >= lvls[i]) return(formatC(x, digits=digs, format="f"))
      }
      if(is.na(x)) return(NA)
    }
  }

  unlist(purrr::map(x, function(x) f(x, width)))

}






# vector version (not working!)

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


# ScallopRound <- function(x, width=3){
#   lvls <- 10^(1:(width-1))
#   lvls <- c(0,lvls)
#
#   out <- NULL
#   for(j in 1:length(x)){
#     if(x[j] < lvls[1]){
#       digs <- width-nchar(lvls[1])
#       rounded <- formatC(x[j], digits=digs, format="f")
#     }
#     if(x[j] >= lvls[1]){
#       for(i in 1:(length(lvls)-1)){
#         digs <- width-nchar(lvls[i])
#         if(x[j] >=lvls[i] & x[j] < lvls[i+1]) {rounded <- formatC(x[j], digits=digs, format="f")}
#       }
#       if(x[j] >= lvls[length(lvls)]) {rounded <- formatC(x[j], digits=digs, format="f")}
#     }
#     out <- c(out, rounded)
#   }
#   return(out)
# }
#
#



