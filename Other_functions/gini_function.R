# THis is my Gini function, which I developed because I couldn't find a function that accounted for 
# the X-axis having unequal proprotions (Default Gini just assumes the y values are spaced evenly) 
# Which they never are for fishery data.  Nobody has really explained this very well in any of the
# papers I've looked at so I'm not entirely convinced they did this correctly, but I am entirely 
# convinced that I just did this correctly.

# y: A series of proportions which should add 1 one. These can be sorted (smallest to largest) or unsorted. This is the variable you are interested in
# x: A series of proportions which should add 1 one. This is the variable (not the cumulative sum tho) that would go on the x-axis of the 
#    cumulative y vs cumulative x figure 

gini <- function(y , x=NULL)
{
  
  if(is.null(x)) "You have not provided an x, which means your x data are evenly spaced"
  if(!is.null(x) & length(x) != length(y)) "Your y and x variables need to be the same length bub"
  # Now I need to do the Gini calcs...
  if(is.null(x)) dat <- data.frame(y = y,ys = NA)
  if(!is.null(x)) dat <- data.frame(y = y,x = x,ys=NA,xs=NA,auc = NA)
  n <- length(dat$y)
  # order the data by the y's, least to greatest
  dat <- dat[order(dat$y),]
  # If you supply the x values do the calculation the hard way.
  if(!is.null(x)) 
  {  
    dat$x <- cumsum(dat$x)
    dat$y <- cumsum(dat$y)
    for(i in 1:n)
    {
      if( i == 1) 
      {
        if(!is.null(x)) dat$xs[i] <- dat$x[i]
        dat$ys[i] <- dat$y[i]
      } else {
        if(!is.null(x)) dat$xs[i] <- dat$x[i] - dat$x[i-1]
        dat$ys[i] <- dat$y[i] - dat$y[i-1]
      }
    }  

  # OK so add up the auc's and you get the "B" in the Gini = 1 - 2B (see Wikipedia figure, which is how I figured out how to calculate this)
  # The auc's are just a bunch of triangles (well triangles stacked on rectangles) that give the area under the curve of our B.
    for(i in 1:n)
    {
      if(i ==1) dat$auc[i] <- 0.5 * dat$xs[i]*dat$ys[i] else{
        dat$auc[i] <- 0.5 * dat$xs[i]*dat$ys[i] + dat$auc[i-1] + 0.5 * dat$xs[i-1]*dat$ys[i-1] 
      }
    }
  } # end if(!is.null(x))
 
  if(is.null(x))
  {
    y <- as.numeric(sort(dat$y))
    G <- sum(y * 1L:n)
    Gini <- (2*G/sum(y) - (n + 1L))/n
  }
  
  if(!is.null(x)) Gini <- round(1 - 2*sum(dat$auc),digits=2)
  
  return(Gini)
}