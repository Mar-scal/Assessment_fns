SSModel.plot.median <- function (x, type = "Model.est", cred.lim = 0.05, ref.pts = NULL, log.R = T,
          Catch.next.year = NULL, pred.lim = NULL, g=1.15,gR = 1.7, ...) 
{
  old.par <- par(no.readonly = TRUE)
  if (any(type == c("Model.est", "Prior.Post", "Exploit", "Survey.est")) == 
      FALSE) 
    return("Not a valid plot type")
  if (type == "Prior.Post") {
    prior.nodes <- c("K", "q", "S", "sigma", "kappa.tau")
    node.pos <- is.element(dimnames(x$sims.matrix)[[2]], 
                           c("K", "q", "S", "sigma", "kappa.tau"))
    node.num <- sum(node.pos)
    post.matrix <- x$sims.matrix[, node.pos]
    par(mfrow = c(ceiling(node.num/2), 2), mar = c(2, 2, 
                                                   0, 0), omi = c(0.5, 0.75, 0.5, 1), plt = c(0.2, 0.9, 
                                                                                              0.25, 0.9))
    plot.postprior(post.matrix[, "K"], Prior.LN(seq(1, 5000, 
                                                    by = 10), x$priors$logK.a, 1/sqrt(x$priors$logK.b)), 
                   0.1, 5000, "K")
    plot.postprior(post.matrix[, "q"], Prior.Beta(seq(0.1, 
                                                      1, by = 0.01), x$priors$q.a, x$priors$q.b), 10, 1, 
                   "q")
    plot.postprior(post.matrix[, "S"], Prior.Unif(seq(x$priors$S.a, 
                                                      x$priors$S.b, by = 0.01), x$priors$S.a, x$priors$S.b), 
                   10, 1, "S")
    plot.postprior(post.matrix[, "sigma"], Prior.Unif(seq(0, 
                                                          10, by = 0.1), 0, 10), 3, 10, expression(sigma))
    plot.postprior(post.matrix[, "kappa.tau"], Prior.Unif(seq(0, 
                                                              10, by = 0.1), 0, 10), x$priors$kappa.tau.a, x$priors$kappa.tau.b, 
                   expression(kappa))
    on.exit(par(old.par))
    return()
  }
  if (type == "Exploit") {
    par(mar = c(5, 4, 4, 5) + 0.1)
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 3), "mu[")]
    plot(x$Years, c(NA, apply(post.matrix, 2, median)), type = "n", 
         pch = 1, ylim = c(0, 1), xlab = "Year", ylab = "Exploitation rate", 
         las = 1, cex.axis = 0.8)
    lines(x$Years[-1], apply(post.matrix, 2, median), type = "b", 
          pch = 16)
    bounds <- apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 
                                                        1 - cred.lim/2))
    lines(x$Years[-1], bounds[1, ], lty = 2)
    lines(x$Years[-1], bounds[2, ], lty = 2)
    par(new = T)
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 2), "m[")]
    plot(x$Years, apply(exp(-post.matrix), 2, median), type = "n", 
         pch = 1, ylim = c(0, 1), xlab = "", ylab = "", las = 1, 
         axes = F)
    lines(x$Years, apply(exp(-post.matrix), 2, median), type = "b", 
          pch = 16, col = "gray")
    bounds <- (apply(exp(-post.matrix), 2, quantile, probs = c(cred.lim/2, 
                                                               1 - cred.lim/2)))
    lines(x$Years, bounds[1, ], lty = 2, col = "gray")
    lines(x$Years, bounds[2, ], lty = 4, col = "gray")
    axis(side = 4, las = 1, cex.axis = 0.8)
    mtext(side = 4, line = 2.2, "Survival rate")
    return()
  }
  if (type == "Model.est") {
    par(mfrow = c(2, 1), mar = c(2, 2, 0, 0), omi = c(0.95, 
                                                      0.95, 0.1, 0.1), plt = c(0.05, 0.95, 0.025, 1))
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 2), "B[")]
    bounds <- (apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 
                                                         1 - cred.lim/2)))/1000
    ylim <- c(0, max(bounds[2, ]))
    if (is.null(Catch.next.year)) {
      plot(x$Years, apply(post.matrix, 2, median)/1000, type = "b", 
           pch = 16, ylim = ylim, xlab = "", ylab = "", 
           las = 1, cex.lab = 0.8, cex.axis = 0.8, xaxt = "n")
      mtext(text = "Commercial biomass (meats,Kt)", side = 2, 
            cex = 0.8, line = 3.5)
      lines(x$Years, bounds[1, ], lty = 2)
      lines(x$Years, bounds[2, ], lty = 2)
      if (!is.null(ref.pts)) {
        abline(h = ref.pts[1], col = rgb(1, 0, 0, 0.2))
        abline(h = ref.pts[2], col = rgb(1, 1, 0, 0.2))
        rect(min(x$Years) - 5, -ylim[2]/5, max(x$Years) + 
               5, ref.pts[1]/1000, border = NA, col = rgb(1, 
                                                          0, 0, 0.2))
        rect(min(x$Years) - 5, ref.pts[1]/1000, max(x$Years) + 
               5, ref.pts[2]/1000, border = NA, col = rgb(1, 
                                                          1, 0, 0.2))
        rect(min(x$Years) - 5, ref.pts[2]/1000, max(x$Years) + 
               5, ylim[2] * 1.2, border = NA, col = rgb(0, 
                                                        1, 0, 0.2))
      }
    }
    else {
      Biomass.next.year <- predict(x, Catch.next.year, 
                                   g.parm = g, gr.parm = gR)
      Biomass.next.year <- Biomass.next.year$B.next/1000
      if (ylim[2] < max(Biomass.next.year)) {
        if (is.null(pred.lim)) ylim[2] <- max(pretty(Biomass.next.year))
        else ylim[2] <- max(ylim[2],max(pretty(quantile(Biomass.next.year, 
                                            probs = c(pred.lim/2, 1 - pred.lim/2)))))
      }
      plot(x$Years, apply(post.matrix, 2, median)/1000, type = "b", 
           pch = 16, ylim = ylim, xlim = c(min(x$Years), 
                                           max(x$Years) + 1), xlab = "Year", ylab = "", 
           las = 1, cex.lab = 0.8, cex.axis = 0.8, xaxt = "n")
      mtext(text = "Commercial biomass (meats,Kt)", side = 2, 
            cex = 0.8, line = 3.5)
      if (is.null(pred.lim)) 
        boxplot(Biomass.next.year, add = T, at = max(x$Years) + 
                  1, range = 0, yaxt = "n", xaxt = "n", xlab = "")
      else {
        temp.box <- boxplot(Biomass.next.year, plot = F)
        temp.box$stats[c(1, 5)] <- quantile(Biomass.next.year, 
                                            probs = c(pred.lim/2, 1 - pred.lim/2))
        temp.box$out <- temp.box$group <- numeric(0)
        temp.box$names <- ""
        bxp(temp.box, add = T, at = max(x$Years) + 1, 
            range = 0, yaxt = "n", xaxt = "n")
      }
      abline(h = ref.pts[1], col = "red")
      abline(h = ref.pts[2], col = "Orange")
      lines(x$Years, bounds[1, ], lty = 2)
      lines(x$Years, bounds[2, ], lty = 2)
      if (!is.null(ref.pts)) {
        abline(h = ref.pts[1], col = rgb(1, 0, 0, 0.2))
        abline(h = ref.pts[2], col = rgb(1, 1, 0, 0.2))
        rect(min(x$Years) - 5, -ylim[2]/5, max(x$Years) + 
               5, ref.pts[1]/1000, border = NA, col = rgb(1, 
                                                          0, 0, 0.2))
        rect(min(x$Years) - 5, ref.pts[1]/1000, max(x$Years) + 
               5, ref.pts[2]/1000, border = NA, col = rgb(1, 
                                                          1, 0, 0.2))
        rect(min(x$Years) - 5, ref.pts[2]/1000, max(x$Years) + 
               5, ylim[2] * 1.2, border = NA, col = rgb(0, 
                                                        1, 0, 0.2))
      }
    }
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 2), "R[")]
    bounds <- (apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 
                                                         1 - cred.lim/2)))/1000
    ylim <- c(min(bounds[1, ]), max((bounds[2, ])))
    if (!is.null(Catch.next.year)) 
      xlim.r <- c(min(x$Years), max(x$Years) + 1)
    else xlim.r <- range(x$Years)
    if(log.R == T)
    {
      plot(x$Years, apply(post.matrix, 2, median)/1000, type = "b", 
         pch = 16, ylim = ylim, xlim = xlim.r, xlab = "", log="y",
         ylab = "", las = 1, cex.lab = 0.8, cex.axis = 0.8)
    }
    if(log.R == F)
    {
      plot(x$Years, apply(post.matrix, 2, median)/1000, type = "b", 
           pch = 16, ylim = ylim, xlim = xlim.r, xlab = "",
           ylab = "", las = 1, cex.lab = 0.8, cex.axis = 0.8)
    }
    mtext(text = "Recruit biomass (meats,Kt)", side = 2, 
          cex = 0.8, line = 3.5)
    mtext("Year", side = 1, line = 2, outer = FALSE, cex = 0.8)
    lines(x$Years, bounds[1, ], lty = 2)
    lines(x$Years, bounds[2, ], lty = 2)
    on.exit(par(old.par))
    return()
  }
  if (type == "Survey.est") {
    par(mfrow = c(2, 1), mar = c(2, 2, 0, 0), omi = c(0.95, 
                                                      0.95, 0.1, 0.1), plt = c(0.05, 0.95, 0.025, 1))
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 5), "Irep[")]
    bounds <- (apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 
                                                         1 - cred.lim/2)))/1000
    ylim <- c(0, max(pretty(bounds[2, ])))
    plot(x$Years, apply(post.matrix, 2, median)/1000, type = "l", 
         pch = 16, ylim = ylim, xlab = "", ylab = "", las = 1, 
         cex.lab = 0.8, cex.axis = 0.8, xaxt = "n")
    mtext(text = "Commercial biomass (meats,Kt)", side = 2, 
          cex = 0.8, line = 3)
    points(x$Years, x$data$I/1000, col = "red", pch = 16)
    lines(x$Years, bounds[1, ], lty = 2)
    lines(x$Years, bounds[2, ], lty = 2)
    post.matrix <- x$sims.matrix[, is.element(substr(dimnames(x$sims.matrix)[[2]], 
                                                     1, 6), "IRrep[")]
    bounds <- (apply(post.matrix, 2, quantile, probs = c(cred.lim/2, 
                                                         1 - cred.lim/2)))/1000
    ylim <- c(0, max(pretty(bounds[2, ])))
    plot(x$Years, apply(post.matrix, 2, median)/1000, type = "l", 
         pch = 16, ylim = ylim, xlab = "", ylab = "", las = 1, 
         cex.lab = 0.8, cex.axis = 0.8)
    mtext(text = "Recruit biomass (meats,Kt)", side = 2, 
          cex = 0.8, line = 3)
    points(x$Years, x$data$IR/1000, col = "red", pch = 16)
    lines(x$Years, bounds[1, ], lty = 2)
    lines(x$Years, bounds[2, ], lty = 2)
    mtext("Year", side = 1, line = 2, outer = FALSE, cex = 0.8)
    on.exit(par(old.par))
    return()
  }
}