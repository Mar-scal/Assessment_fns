# for plotting 2024 framework glmer MWSH model curve for a single year (as for MWSH plot in SS)

# mw.sh.coef <- cf.data[[bnk]]$CF.fit$mw.sh.coef
# wgt.dat <- cf.data[[bnk]]$HtWt.fit$resid
# yr=2022

shwt.plt2 <- function(mw.sh.coef, wgt.dat, yr, french=F,cx=1.2,lw=2,titl="",cex.mn = 1.2,axis.cx=1, ...){
  
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(rosettafish)
  
  sh <- min(wgt.dat$sh[wgt.dat$year==yr]):max(wgt.dat$sh[wgt.dat$year==yr])/100

  slope <- mw.sh.coef %>% dplyr::filter(year == yr) %>% dplyr::pull(fix.slope); slope <- slope[1]
  int <- mw.sh.coef %>% dplyr::filter(year == yr) %>% dplyr::pull(fix.int); int <- int[1]
  rand.int <- mw.sh.coef[mw.sh.coef$year==yr & !is.na(mw.sh.coef$ran.int.act),] %>% dplyr::pull(ran.int.act,tow)
  
  rt <- NULL
  if(length(rand.int)>0){
    for(i in 1:length(rand.int)) {
      rt[[i]] <- data.frame(sh = 100*sh,mw = exp(rand.int)[i] * sh^slope,tow = names(rand.int)[i])
    }
    rts <- do.call("rbind",rt)
  }
  
  if(length(rand.int)==0) rts <- NA
  r.tows <- data.frame(rts,year = yr)
  f.mws <- data.frame(sh = 100* sh, mw = exp(int) * sh^slope,year = yr)
  
  # Draw the plot of raw data
  base::plot(x = wgt.dat[wgt.dat$year==yr,]$sh, y=wgt.dat[wgt.dat$year==yr,]$wmw, col=rgb(1,0,0,0.3),
             las = 1, mgp = c(0.5, 0.5, 0), xlab ="", ylab = "",xaxt="n",yaxt="n",pch=19,cex=0.5)
  axis(1,at=seq(0,200,20),label=seq(0,200,20),cex.axis=axis.cx)
  axis(1,at=seq(0,200,10),label=F,cex.axis=axis.cx,tcl=-0.3)
  axis(2,at=seq(0,100,by=10),label=T,cex.axis=axis.cx)
  axis(2,at=seq(0,100,by=5),label=F,cex.axis=axis.cx,tcl=-0.3)
  
  # Add a grid to the plot
  #grid(col = "grey30")
  abline(h=seq(0,100,by=10),v=seq(0,200,by=20),col="grey30",lty=3,lwd=0.7)
  
  # Draw thin lines showing fit of each random effect (tow)
  for(i in 1:length(unique(r.tows$tow)))
  {
    lines(x=r.tows[r.tows$tow == unique(r.tows$tow)[i],]$sh, 
          y=r.tows[r.tows$tow == unique(r.tows$tow)[i],]$mw, 
          col = rgb(1,0,0,0.2),lwd=1.5)
  } # end for(i in 1:nrow(fit))
  lines(x=f.mws$sh, y=f.mws$mw, lwd = 2, col = 'blue')
  
  # Add the axis labels the adj = 0.5 effectively centers the text
  mtext("Shell height (mm)", 1, 2.5,cex=cx)
  mtext(side=2,line=4,cex=cx,"Meat\n weight\n(g)",adj=0.5,las=1)
  title(titl,cex.main=cex.mn)

  # ggplot() +
  #   geom_point(data=wgt.dat[wgt.dat$year==yr,],aes(x=sh,y=wmw),color=rgb(1,0,0,0.3)) +
  #   geom_line(data = r.tows, aes(x=sh,y=mw,group=tow),color=rgb(1,0,0,0.3), size=0.25)+
  #   geom_line(data = f.mws, aes(x=sh,y=mw),color='blue',size=2) +
  #   scale_x_continuous(name = paste0(en2fr("Shell height",  custom_terms=rosetta_terms, translate=french), " (mm)"),
  #                      breaks = seq(0,200,by=20), expand = c(0.05,0.05)) +
  #   scale_y_continuous(name = paste0(en2fr("Meat weight",  custom_terms=rosetta_terms, translate=french), " (g)"),
  #                      breaks = seq(0,100,by=20)) +
  #   theme_bw()
}
