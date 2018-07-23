SSModel_predict_summary_median <- function (object, LRP, USR, RRP = NULL, signif = 3, ...) 
{
  out <- data.frame(Catch = object$Catch)
  if (is.matrix(object$B.next)) {
    postmedianB <- apply(object$B.next, 2, median)
    out$Exploit <- round(object$Catch/(postmedianB + object$Catch), 
                         signif)
    out$B.change <- round(apply(100 * sweep(object$B.next - 
                                              object$B.cur, 1, object$B.cur, "/"), 2, median), signif)
    out$pB0 <- round(apply((object$B.next - object$B.cur) > 
                             0, 2, sum)/length(object$B.cur), signif)
    if (!(missing(LRP) | missing(USR))) {
      out$p.LRP <- round(apply(object$B.next > LRP, 2, 
                               sum)/length(object$B.cur), signif)
      out$p.USR <- round(apply(object$B.next > USR, 2, 
                               sum)/length(object$B.cur), signif)
    }
    if (!is.null(RRP)) {
      out2 <- t(apply(object$B.next * RRP, 2, quantile, 
                      probs = seq(0.1, 0.6, by = 0.1)))
      out2 <- cbind(object$Catch, round(out2))
      dimnames(out2)[[2]] <- c("Catch", as.character(seq(0.1, 
                                                         0.6, by = 0.1)))
    }
  }
  else {
    postmedianB <- median(object$B.next)
    out$Exploit <- round(object$Catch/(postmedianB + object$Catch), 
                         signif)
    out$B.change <- round(median(100 * (object$B.next - object$B.cur)/object$B.cur), 
                          signif)
    out$pB0 <- round(median((object$B.next - object$B.cur) > 
                            0), signif)
    if (!(missing(LRP) | missing(USR))) {
      out$p.LRP <- round(median(object$B.next > LRP), signif)
      out$p.USR <- round(median(object$B.next > USR), signif)
    }
    if (!is.null(RRP)) {
      out2 <- c(object$Catch, round(quantile(object$B.next * 
                                               RRP, probs = seq(0.1, 0.6, by = 0.1)), 0))
      names(out2) <- c("Catch", as.character(seq(0.1, 0.6, 
                                                 by = 0.1)))
    }
  }
  if (exists("out2")) 
    return(list(Next.year = out, Interim.RRP = out2))
  else return(list(Next.year = out))
}