
yr <- 2017
direct <- "d:/r/"

# This will get us the results for GB survey history
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
years <- 2008:yr
towlist <- ger.tows
print=F
chng=F
surv.dat <- lined.dat[-which(lined.dat$random %in% c(2,4,5)),]


  #subset the data for this year and the following year.
  SurvY1<-subset(surv.dat,year==2016,c('year','tow','stratum','lat','lon','pre','rec','com','tot',
                                           'pre.bm','rec.bm','com.bm','tot.bm'))
  SurvY2<-subset(surv.dat,year==2017,c('year','tow','stratum','lat','lon','pre','rec','com','tot',
                                             'pre.bm','rec.bm','com.bm','tot.bm'))
  
  crossref<-subset(towlist[towlist$year == 2017,],stratum==2,c('EID','tow'))
  # just curious...
  dat <- data.frame(tow_2016 = rep(NA,19),tow_2017 = rep(NA,19))
  for(i in 1:19)
  {
    dat$tow_2016[i] <- SurvY1$pre[SurvY1$tow == crossref$tow.y1[i]]
    dat$tow_2017[i] <- SurvY2$pre[SurvY2$tow == crossref$tow.y2[i]]
  }
  plot(dat$tow_2017~dat$tow_2016)
  median(dat$tow_2017/dat$tow_2016)
  
  # rename the tow headers.
  names(crossref)<-c('tow.y1','tow.y2')
  
  # The numbers and biomass from the survey with a sampling with partial replacement survey design.
  # this compares the survey results from matched and unmatched tows between years.
  pre.spr.obj <-spr(SurvY1$tow,SurvY1$pre,SurvY2$tow,SurvY2$pre,crossref=crossref)



object <- pre.spr.obj
temp.obj <- spr.Sumstats(object)
match.reg <- lm(object$matched.t1t2$y.this.year ~ object$matched.t1t2$y.last.year)
#match.reg <- glm(object$matched.t1t2$y.this.year ~ object$matched.t1t2$y.last.year,family=quasipoisson(link="log"))
plot(match.reg)
#plot(log(object$matched.t1t2$y.this.year) ~ object$matched.t1t2$y.last.year)
#abline(match.reg)
coeff.beta <- as.vector(coef(match.reg))
# This is the key step to understand...
yt2m <- temp.obj$This.year[5] + coeff.beta[2] * (temp.obj$Last.year[7] - 
                                                   temp.obj$Last.year[5])
var.res <- summary(match.reg)$sigma^2
inv.wt2m <- var.res * ((1/temp.obj$This.year[4]) - (1/(temp.obj$This.year[1] + 
                                                         temp.obj$This.year[4]))) + temp.obj$Last.year[8]/(temp.obj$Last.year[1] + 
                                                                                                             temp.obj$Last.year[4])
wt2m <- 1/inv.wt2m
wt2u <- temp.obj$This.year[1]/temp.obj$This.year[3]
wSPR <- wt2m + wt2u
Y.spr <- (wt2m * yt2m + wt2u * temp.obj$This.year[2])/wSPR
var.Yspr <- 1/wSPR
phi <- (wt2m/wSPR) * (wt2u/wSPR) * ((1/temp.obj$This.year[4]) + 
                                      (1/temp.obj$This.year[1]))
var.Yspr.corrected <- (1 + 4 * phi)/wSPR
out <- list(Yspr = Y.spr, var.Yspr = var.Yspr, var.Yspr.corrected = var.Yspr.corrected, 
            wt2m = wt2m, wt2u = wt2u, rho = temp.obj[10, 2])