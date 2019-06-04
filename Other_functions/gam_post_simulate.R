## To calculate confidence intervals for a GAM using a posterior simulation approach
## Refer to: 
## https://www.fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
## https://people.maths.bris.ac.uk/~sw15190/mgcv/tampere/mgcv-advanced.pdf
## https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam


gam_post_simulate <- function(gamobj, moddat){
  
  ## posterior simulation for CI's from https://stats.stackexchange.com/questions/190348/can-i-use-bootstrapping-to-estimate-the-uncertainty-in-a-maximum-value-of-a-gam 
  # Xp <- predict(gamobj, moddat, type="lpmatrix") ## map coefs to fitted curves
  # #Next we collect the fitted model coefficients and their (Bayesian) covariance matrix
  # beta <- coef(gamobj)
  # Vb   <- vcov(gamobj) ## posterior mean and cov of coefs
  # 
  # n <- 10000
  # library("MASS") ## for mvrnorm
  # set.seed(10)
  # mrand <- mvrnorm(n, beta, Vb) ## simulate n rep coef vectors from posterior
  # 
  # opt <- rep(NA, n)
  # for (i in seq_len(n)) { 
  #   pred   <- inv.logit(Xp %*% mrand[i, ])
  #   opt[i] <- 
  #   
  #   moddat_sp$month[which.max(pred)]
  # }
  # 
  # ci <- quantile(opt, c(.025,.975)) ## get 95% CI
  # ci

  fits <- cbind(as.data.frame(predict(gamobj, se.fit=T, unconditional=T)), # unconditional=T based on advice from predict.gam/plot.gam (sewithMean option)
                          gamobj$model) 
  
  names(fits) <- c("fit", "se.fit", "y", "x")
  
  if(gamobj$family$link == "logit") ilink <- function(x) inv.logit(x)
  
  modplot <- ggplot() + geom_point(data=fits, aes(x, y)) +
    geom_line(data=fits, aes(x, ilink(fit)), colour="red") +
    geom_line(data=fits, aes(x, ilink(fit-1.96*se.fit)), lty="dashed", colour="red")+
    geom_line(data=fits, aes(x, ilink(fit+1.96*se.fit)), lty="dashed", colour="red") + 
    theme_bw() +
    theme(panel.grid=element_blank()) +
    ggtitle(gamobj$call) #+
  # geom_vline(data=moddat_sp, aes(xintercept=moddat_sp$month[which.max(inv.logit(fits_s$fit))]), linetype="dashed") +
  # geom_point(data=data.frame(ci), aes(x=ci, y=c(0,0)), colour="blue")
  
  #png(filename=paste0(direct, "reallocation/Figures/GAM - ", speciesID, "_seTRUE.png"), type="cairo", width=30, height=15, units = "cm", res=100)
  print(modplot)
  #dev.off()
  
  # # from https://www.fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
  # rmvn <- function(n, mu, sig) { ## MVN random deviates
  #   L <- mroot(sig)
  #   m <- ncol(L)
  #   t(mu + L %*% matrix(rnorm(m*n), m, n))
  # }
  # #Next we extract a few things that we need from the fitted GAM
  # 
  # Vb <- vcov(gamobj)
  # se.fit <- fits$se.fit
  # # The first is the Bayesian covariance matrix of the model coefficients, Vb. This Vb is conditional upon the smoothing parameter(s). If you want a version that adjusts for the smoothing parameters being estimated rather than known values, add unconditional = TRUE to the vcov() call. Second, we define our grid of x values over which we want a confidence band. Then we generate predictions and standard errors from the model for the grid of values. The last line just extracts out the standard errors of the fitted values for use later.
  # 
  # # Now we are ready to generate simulations of the maximum absolute standardized deviation of the fitted model from the true model. We set the pseudo-random seed to make the results reproducible and specify the number of simulations to generate.
  # 
  # set.seed(42)
  # N <- 100
  # 
  # BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
  # 
  # Cg <- predict(gamobj, gamobj$model, type = "lpmatrix")
  # simDev <- Cg %*% t(BUdiff)
  # # The first line evaluates the basis function at g and the second line computes the deviations between the fitted and true parameters. Then we find the absolute values of the standardized deviations from the true model. Here we do this in a single step for all simulations using sweep()
  # 
  # absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  # 
  # masd <- apply(absDev, 2L, max)
  # 
  # # The last step is to find the critical value used to scale the standard errors to yield the simultaneous interval; here we calculate the critical value for a 95% simultaneous confidence interval/band
  # crit <- quantile(masd, prob = 0.95, type = 8)
  # #The critical value estimated above is 2.816688. 
  # # Now that we have the critical value, we can calculate the simultaneous confidence interval. In the code block below I first add the grid of values (newd) to the fitted values and standard errors at those new values and then augment this with upper and lower limits for
  # # a 95% simultaneous confidence interval (uprS and lwrS), as well as the usual 95% point-wise intervals for comparison (uprP and lwrP). Then I plot the two intervals:
  # 
  # pred <- transform(cbind(data.frame(fits), gamobj$model),
  #                   uprP = inv.logit(fit + (2 * se.fit)),# pointwise (narrower, same as before)
  #                   lwrP = inv.logit(fit - (2 * se.fit)),#pointwise 
  #                   uprS = inv.logit(fit + (crit * se.fit)), # simultaneous (wider)
  #                   lwrS = inv.logit(fit - (crit * se.fit))) # simultaneous
  # 
  # sims <- rmvn(N, mu = coef(gamobj), sig = Vb)
  # fits2 <- ilink(Cg %*% t(sims))
  # nrnd <- 30
  # rnd <- sample(N, nrnd)
  # stackFits <- stack(as.data.frame(fits2[, rnd]))
  # stackFits <- transform(stackFits, x=unique(fits$x))
  # stackFits <- unique(stackFits[, c("values", "x")])
  # stackFits$ind <- rep(1:(nrow(stackFits)/length(unique(fits$x))), each=length(unique(fits$x)))
  # 
  # modplot_CI <- modplot + geom_ribbon(data=pred, aes(x, ymin = lwrS, ymax = uprS), alpha = 0.2, fill = "red") +
  #         geom_ribbon(data=pred, aes(x, ymin = lwrP, ymax = uprP), alpha = 0.2, fill = "red") +
  #         geom_path(data = stackFits, mapping = aes(y = values, x=x, group=ind),
  #                   alpha = 0.4, colour = "grey20")
  # print(modplot_CI)
  # 
  # inCI <- function(x, upr, lwr) {
  #   all(x >= lwr & x <= upr)
  # }
  # 
  # fitsInPCI <- apply(fits2, 2L, inCI, upr = pred$uprP, lwr = pred$lwrP)
  # fitsInSCI <- apply(fits2, 2L, inCI, upr = pred$uprS, lwr = pred$lwrS)
  # 
  # checkSimCI <- sum(fitsInSCI) / length(fitsInSCI)
  # checkPntWiseCI = sum(fitsInPCI) / length(fitsInPCI)
  # 
  # print(checkSimCI)   # Simultaneous
  # print(checkPntWiseCI)  # Point-wise
  # 
  # modplot_final <- ggplot() + geom_point(data=fits, aes(x, y)) +
  #   geom_line(data=fits, aes(x, ilink(fit)), colour="red") +
  #   theme_bw() +
  #   theme(panel.grid=element_blank()) +
  #   ggtitle(gamobj$call) +
  #   geom_line(data=pred, aes(x, y = lwrS), linetype="dashed", colour = "red") +
  #   geom_line(data=pred, aes(x, y = uprS), linetype="dashed", colour = "red")
  # print(modplot_final)
  # 
  return(list(modplot=modplot#, modplot_CI=modplot_CI, modplot_final=modplot_final, checkSimCI = checkSimCI, checkPntWiseCI=checkPntWiseCI
              ))
}