# for plotting 2024 framework glmer MWSH model curve for a single year (as for MWSH plot in SS)

# mw.sh.coef <- cf.data[[bnk]]$CF.fit$mw.sh.coef
# wgt.dat <- cf.data[[bnk]]$HtWt.fit$resid
# yr=2022

shwt.plt2 <- function(mw.sh.coef, wgt.dat, yr, french=F){
  
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(rosettafish)
  
  sh <- min(wgt.dat$sh[wgt.dat$year==yr]):max(wgt.dat$sh[wgt.dat$year==yr])/100

  slope <- mw.sh.coef %>% dplyr::filter(year == yr) %>% dplyr::pull(fix.slope); slope <- slope[1]
  int <- mw.sh.coef %>% dplyr::filter(year == yr) %>% dplyr::pull(fix.int); int <- int[1]
  rand.int <- mwsh.fit[mwsh.fit$year==year & !is.na(mwsh.fit$ran.int.act),] %>% dplyr::pull(ran.int.act,tow)
  
  rt <- NULL
  for(i in 1:length(rand.int)) {
    rt[[i]] <- data.frame(sh = 100*sh,mw = exp(rand.int)[i] * sh^slope,tow = names(rand.int)[i])
  }
  rts <- do.call("rbind",rt)
  
  r.tows <- data.frame(rts,year = yr)
  f.mws <- data.frame(sh = 100* sh, mw = exp(int) * sh^slope,year = yr)

  ggplot() +
    geom_point(data=wgt.dat[wgt.dat$year==yr,],aes(x=sh,y=wmw),color=rgb(1,0,0,0.3)) +
    geom_line(data = r.tows, aes(x=sh,y=mw,group=tow),color=rgb(1,0,0,0.3), size=0.25)+
    geom_line(data = f.mws, aes(x=sh,y=mw),color='blue',size=2) +
    scale_x_continuous(name = paste0(en2fr("Shell height",  custom_terms=rosetta_terms, translate=french), " (mm)"),
                       breaks = seq(0,200,by=20), expand = c(0.05,0.05)) +
    scale_y_continuous(name = paste0(en2fr("Meat weight",  custom_terms=rosetta_terms, translate=french), " (g)"),
                       breaks = seq(0,100,by=20)) +
    theme_bw()
}
