
load(paste(direct,"/Data/Model/2017/BBn/Results/Model_testing_results.RData",sep=""))

C.p <- 1000
d <- mod.out$BBn$BUGSoutput$median
d$g <- DD.dat$g
d$gR <- DD.dat$gR
d$NY <- 26
d$B[d$NY]
d$m[d$NY]
d$mR[d$NY]
d$q
(exp(-d$m[d$NY])*(d$g[d$NY])*(d$P[d$NY]-C.p/d$K)+exp(-d$mR[d$NY])*(d$gR[d$NY])*d$r[d$NY]) * d$K

(exp(-d$m[d$NY])*(1.05)*(d$P[d$NY]-C.p/d$K)+exp(-d$mR[d$NY])*(1.25)*d$r[d$NY]) * d$K

# So the impact of condition appears to be around 10%, where is the rest of the decline coming from???


load(paste(direct,"/Data/Model/2018/BBn/Results/Model_testing_results.RData",sep=""))

dn <- mod.out$BBn$BUGSoutput$median
dn$g <- DD.dat$g
dn$gR <- DD.dat$gR
dn$NY <- 14
dn$B[dn$NY]
dn$m[dn$NY]
dn$mR[dn$NY]
dn$R
dn$q
C.p2 <- 0
Pmed <- exp(log(dn$P) -dn$Presid)

(exp(-dn$m[dn$NY])*(dn$g[dn$NY])*(dn$P[dn$NY]-C.p2/dn$K)+exp(-dn$mR[dn$NY])*(dn$gR[dn$NY])*dn$r[dn$NY]) * dn$K




exp(-0.2) * 1.05 * (4310-1000) # FR biomass in a year of crap growth, normal mortality, and 1000 tonnes removed.
exp(-0.17) * 1.25 * 192        # R biomass in a year of crap growth and normal mortality
# This accouts for about half of the decline (4300 - 3600), where did we lose the other 800 tonnes???


(exp(-dn$m[dn$NY]) * dn$g[dn$NY-1] * (dn$P[dn$NY-1] - C.p / dn$K) + exp(-dn$mR[dn$NY])*dn$gR[dn$NY-1] * dn$r[dn$NY-1]) * dn$K
    
# The missing 800 tonnes in 2017 is found below.  The process model has been predicting higher biomasses than the survey is expecting us to see
# This has been happening consistently since the eearly 2000's.  The process model wants the biomass to be higher, but the observation are saying it isn't
# important also to note that the process residual plots, while hinting of a systematic overprediction by the model really doesn't get a good
# indication of the scale of this, a residual of -1 means that the Biomass is 30% lower than the process model predition, that's a pretty big difference IMHO!
# In fact is should result is us consistently over predicting the biomass for the next year on Browns, our projections are on average 10% to rosy, and in 
# some years this is as much as 30% likely (such as our 2016 prediction for 2017).  In terms of this model there are several things we could be getting wrong
# 1:  We could be predicting growth to be faster than actually occurs, our growth data is rather old and based on relatively few samples.
# 2:  Mortality may be higher than we are accounting for (either natural or catch)
# 3:  We could be overestimating the abundance of recruits hitting the bank.
# 4:  Condition during the fishery may be lower than condition from the survey (I doubt this one)
# 5:  Spatial processes that we average across we shouldn't average across!
# 6:  This could be a function of poor survey coverage in the earlier years, starting back in the 1990's, if this is an issue here it isn't the whole story
#     the process error does decline if we reduce the number of years we run the model, but it is still significant and the trend of low process residuals remains.
# 7:  We may also not be allowing enough "error" in our survey results, we are getting excellent fits to the data, but these fits could be masking the lack of
#     fit between our process model and the actual biomass estimates for the bank.
Bmed <- Pmed * as.numeric(dn$K)
B <- dn$P * as.numeric(dn$K)
plot(B ~ Bmed,type="n")
text(Bmed,B,labels= 2004:2017,cex=0.6)
abline(0,1)
Bmed/B
Bmed - B
