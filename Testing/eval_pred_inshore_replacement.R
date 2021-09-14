# Here's a quick version of the SSModle pred.eval function. DK should add comments and make it pretty...

eval.predict <- function (x, plot = TRUE, Year = 2000, Box.col = "blue", 
                          pred.lim = NULL) 
{
  no.years = length(x)
  temp <- x[[1]]$B.next
  for (i in 2:no.years) {
    temp <- cbind(temp, x[[i]]$B.cur, x[[i]]$B.next)
  }
  no.box <- dim(temp)[2]
  out1 <- boxplot(temp[, seq(1, no.box, by = 2)], range = 0, 
                  plot = F)
  out2 <- boxplot(temp[, seq(2, no.box, by = 2)], range = 0, 
                  plot = F)
  if (plot) {
    if (is.null(pred.lim)) {
      bxp(out1, xaxt = "n", ylab = "Commercial biomass (t, meats)", 
          boxwex = 0.2)
      bxp(out2, add = TRUE, boxwex = 0.2, at = 0.25 + 1:floor(no.box/2), 
          xaxt = "n", boxfill = Box.col)
      axis(side = 1, labels = Year + (0:floor(no.box/2)), 
           at = (1:ceiling(no.box/2)) + 0.12)
      mtext("Year", 1, 2.5)
    }
    else {
      out1$stats[c(1, 5), ] <- apply(temp[, seq(1, no.box, 
                                                by = 2)], 2, FUN = quantile, probs = c(pred.lim/2, 1 - pred.lim/2),na.rm=T)
      out2$stats[c(1, 5), ] <- apply(temp[, seq(2, no.box, 
                                                by = 2)], 2, FUN = quantile, probs = c(pred.lim/2, 1 - pred.lim/2),na.rm=T)
      bxp(out1, xaxt = "n", ylab = "Commercial biomass (t, meats)", 
          boxwex = 0.2)
      bxp(out2, add = TRUE, boxwex = 0.2, at = 0.25 + 1:floor(no.box/2), 
          xaxt = "n", boxfill = Box.col)
      axis(side = 1, labels = Year + (0:floor(no.box/2)), 
           at = (1:ceiling(no.box/2)) + 0.12)
      mtext("Year", 1, 2.5)
    }
  }
  else {
    if (!is.null(pred.lim)) {
      out1$stats[c(1, 5), ] <- apply(temp[, seq(1, no.box, 
                                                by = 2)], 2, quantile, probs = c(pred.lim/2, 
                                                                                 1 - pred.lim/2))
      out2$stats[c(1, 5), ] <- apply(temp[, seq(2, no.box, 
                                                by = 2)], 2, quantile, probs = c(pred.lim/2, 
                                                                                 1 - pred.lim/2))
    }
    out1$names <- out2$names <- ""
    return(list(Next = out1[1:3], Cur = out2[1:3]))
  }
}