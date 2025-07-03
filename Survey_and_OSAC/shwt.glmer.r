shwt.glmer <- function(wt.dat, yrs)
{
  # Load the nlme package, we may want to change to lme4...
  require(lme4) || stop("How don't you have nlme package???  Please install 'nlme'!")
  
  # with depth across all years (random effect is ID)
  sub <- wt.dat[complete.cases(wt.dat),]

  n.yrs <- length(yrs)
  mod.res <- NULL
  resids <- NULL
  qq.plt <- NULL
  warningsdf <- NULL
  for(i in 1:n.yrs)
  {
    #print(yrs[i])
    dat.tmp <- sub %>% dplyr::filter(year == yrs[i])
    n.tows <- length(unique(dat.tmp$new_ID))
    n.tows <- length(unique(dat.tmp$new_ID))
    # Could have had more complex depth smooth, but our residuals look fine with the linear smooth so sticking with that.
    #if(yrs[i] >= 2011) mod.res[[as.character(yrs[i])]] <- glmer(wmw ~ log.sh.cen + poly(depth.cen,3) + (1| new_ID),data = dat.tmp,family=Gamma(link=log))
    # No non-linearity if we don't have 10 tows to use, to complex...
    # ONLY RUN RANDOM EFFECTS IF AT LEAST 5 TOWS
    if(n.tows>4)  {
      mod.res[[as.character(yrs[i])]] <- glmer(wmw ~ log.sh.cen + depth.cen + (1| new_ID),data = dat.tmp,family=Gamma(link=log))
      if(any(grepl("failed to converge",  mod.res[[as.character(yrs[i])]]@optinfo$conv$lme4$messages))|
         any(grepl("unidentifiable",  mod.res[[as.character(yrs[i])]]@optinfo$conv$lme4$messages))){
        #if(yrs[i] == max(yrs)) browser()# this browser is here so that you actually realize it didn't converge!
        warning <- data.frame(year = yrs[i], warn = as.character(mod.res[[as.character(yrs[i])]]@optinfo$conv$lme4$messages))
        warningsdf <- rbind(warningsdf, warning)
      }
    }
    if(n.tows<5)  {
      mod.res[[as.character(yrs[i])]] <- glm(wmw ~ log.sh.cen + depth.cen,data = dat.tmp,family=Gamma(link=log))
      
      if(mod.res[[as.character(yrs[i])]]$converged==F){
        warning <- data.frame(year = yrs[i], warn = "check_glm")
        warningsdf <- rbind(warningsdf, warning)
      }
    }
    
    dat.tmp$residuals <- residuals(mod.res[[as.character(yrs[i])]])
    resids[[as.character(yrs[i])]] <- dat.tmp
    
    #qq.plt[[as.character(yrs[i])]] <- qqplot.data(dat.tmp$residuals)
  }
  # 1991 doesn't converge and we don't need it for the models, so I suggest we only use 1992 onwards for this.
  # check warningsdf for convergence issues
  print(warningsdf)
  resid <- do.call('rbind',resids)
  
 return(list(mod.res=mod.res, resid=resid)) 
}
  