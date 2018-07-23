# This is a little script to compare JAGS, BUGS, and JAGS in parallel
# DK, April 2016.  The upshot of this is I don't see any issues with moving to JAGS from BUGS
# All the output I compared are similar and there doesn't seem to be any bias with JAGS in parallel
# This also means that we don't need to use inits anymore, JAGS appears to handle the intial values nicely.

# First we bring in SSModle, and the two JAGS variant functions needed to run SSModel in JAGS
library(SSModel)

library(R2jags)
library(R2WinBUGS)

# Load in the SSModel.jags and BoFmodel need to run JAGS
SSModel.jags <-
  function(data,priors,inits,parms,model.file,Years,nchains,niter,nburnin,nthin,para=F,seed= 125)
  {
    ssmodel.call<-match.call()
    if(missing(parms)) parms<-c("B","R","q","K","P","sigma","S","m","kappa.tau","r","Fmort","mu","Irep","IRrep")
    
    write.model(model.file,con="model.txt")
    if(para == F)
    {
      out<-jags(data, inits = NULL,parameters.to.save=parms, model.file="model.txt",
                n.chains=nchains, n.iter=niter, n.burnin=nburnin,
                n.thin=nthin)
    }
    
    if(para == T)
    {
      out<-jags.parallel(data, inits = NULL,parameters.to.save=parms, model.file="model.txt",
                         n.chains=nchains, n.iter=niter, n.burnin=nburnin,
                         n.thin=nthin,jags.seed = seed)
    }
    out<-c(out$BUGSoutput[c("sims.matrix","summary")],data = list(data), 
    priors = list(priors), Years = list(Years), ssmodel.call = ssmodel.call)
    class(out)<-"SSModel"
    temp<-file.remove("model.txt")
    out
  }


BoFmodel <-
  function()
  {
    # Priors
    # Population scaling factor
    K ~ dlnorm(logK.a, logK.b)
    
    
    # priors for survey recruitment index
    for(t in 1:NY){
      r[t] ~ dlnorm(r.a, r.b)
    }
    
    # priors for natural mortality
    # prior for annual m
    for(t in 1:NY){
      m[t] ~ dlnorm(m.a, m.b)
    }
    
    #S=dissolution rate in days/365
    S~dunif(S.a,S.b)
    
    # priors for catchabilities
    # Use prior based on Bourdages' field work
    q ~ dbeta(q.a, q.b)
    
    # prior for process noise
    sigma ~ dunif(sigma.a, sigma.b)
    isigma2 <- pow(sigma, -2)
    
    # prior for clappers
    
    # clappers
    kappa.tau ~ dunif(kappa.tau.a, kappa.tau.b)
    ikappa.tau2 <- pow(kappa.tau,-2)
    
    # priors for observation errors
    
    for(t in 1:NY)
    {
      I.var[t]<-2*log(pow(I.cv[t],2)+1)
      I.precision[t] ~ dgamma(3,I.var[t])
      I.vsam[t]<-pow(I.precision[t],-0.5)
      IR.var[t]<-2*log(pow(IR.cv[t],2)+1)
      IR.precision[t] ~ dgamma(3,IR.var[t])
      IR.vsam[t]<-pow(IR.precision[t],-0.5)
    }
    
    # State equation
    Pmed[1] <- -pow(sigma,2)/2 
    P[1] ~ dlnorm(Pmed[1], isigma2)
    
    for(t in 2:NY){	
      Pmed[t] <- log(max(exp(-m[t]) * g[t-1] * (P[t-1] - C[t-1] / K) + exp(-m[t])*gR[t-1] * r[t-1], 0.001))
      P[t] ~ dlnorm(Pmed[t], isigma2)
    }
    
    #Natural mortality from survey clapper index, revised for popcorn model
    
    Cmed[1]<-log(m[1]*S*N[1])
    
    for(t in 2:NY){
      Cmed[t]<-log(m[t]*S*(S*N[t-1]+(2-S)*N[t])/2)
    }
    
    for(t in 1:NY){
      clappers[t]~dlnorm(Cmed[t],ikappa.tau2)
    }
    
    
    
    # Observation equations
    
    for(t in 1:NY){
      # Survey biomass
      Imed[t] <- log(q * K * P[t])
      I[t] ~ dlnorm(Imed[t], I.precision[t])
      
      # Survey recruitment
      IRmed[t] <- log(ratiolined[t]* q * K * r[t])
      IR[t] ~ dlnorm(IRmed[t], IR.precision[t])	
      
      
      # Predicted survey estimates	
      Ipred[t] <- q * B[t]
      IRpred[t] <-ratiolined[t]* q * R[t]
      Irep[t] ~ dlnorm(Imed[t], I.precision[t])#I(0,Ireplim)
      IRrep[t] ~ dlnorm(IRmed[t], IR.precision[t])#I(0,IRreplim)
      
    }
    
    
    # Population commercial and recruit biomass posteriors
    for(t in 1:NY){
      B[t] <- P[t] * K
      R[t] <- r[t] * K
    }
    
    # Exploitation and fishing mortality
    for(t in 1:(NY-1)){
      mu[t] <- C[t]/(B[t+1]+C[t])
      Fmort[t] <- -log(max(1 - mu[t], 0.0001))
    }
    
    # Diagnostics: raw and scaled residuals
    for(t in 1:NY){
      Iresid[t] <- log(I[t]) - Imed[t]
      Presid[t] <- log(P[t]) - Pmed[t]
      IRresid[t] <- log(IR[t]) - IRmed[t]
      
      sPresid[t] <- Presid[t] * 1/sigma
      sIresid[t] <- Iresid[t] * pow(I.precision[t],0.5)
      sIRresid[t] <- IRresid[t] * pow(IR.precision[t],0.5)
      
      
    }
  }

# give the BoF data nice names
data <- DDspa4.dat
priors <-BoFSPA4.priors
inits <- BoFSPA4.inits
model.file=BoFmodel

# Run a bunch of models and compare
test<-SSModel.jags(c(data,priors),priors = priors,model.file=BoFmodel,
                   Years=1983:2014, nchains=2,niter=50000,nburnin=25000,nthin=10)

summary(test)
data.frame(B= mean(test$sims.matrix[,colnames(test$sims.matrix)=="B[32]"]), R= mean(test$sims.matrix[,colnames(test$sims.matrix)=="R[32]"]),
      P=mean(test$sims.matrix[,colnames(test$sims.matrix)=="P[32]"]), K = mean(test$sims.matrix[,colnames(test$sims.matrix)=="K"]),
      m=mean(test$sims.matrix[,colnames(test$sims.matrix)=="m[32]"]),r=mean(test$sims.matrix[,colnames(test$sims.matrix)=="r[32]"]),
      Fmort=mean(test$sims.matrix[,colnames(test$sims.matrix)=="Fmort[31]"]),mu=mean(test$sims.matrix[,colnames(test$sims.matrix)=="mu[31]"]),
      I=mean(test$sims.matrix[,colnames(test$sims.matrix)=="Irep[32]"]),IR=mean(test$sims.matrix[,colnames(test$sims.matrix)=="IRrep[32]"])
)
# What are the rhat ranges
max(test$summary[,8])
# what are the neff ranges
min(test$summary[,9])

test2 <- SSModel(DDspa4.dat,BoFSPA4.priors,BoFSPA4.inits,model.file=BoFmodel,Years=1983:2014,
                  nchains=2,niter=50000,nburnin=25000,nthin=10)

summary(test2)
# What are the rhat ranges
max(test2$summary[,8])
# what are the neff ranges
min(test2$summary[,9])


test3<-SSModel.jags(c(DDspa4.dat,BoFSPA4.priors),priors=priors,model.file=BoFmodel,seed=123,
                   Years=1983:2014, nchains=2,niter=50000,nburnin=25000,nthin=10,para=T)
summary(test3)
# What are the rhat ranges
max(test3$summary[,8])
# what are the neff ranges
min(test3$summary[,9])


# Same thing...
data.frame(B= mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="B[32]"]), R= mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="R[32]"]),
           P=mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="P[32]"]), K = mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="K"]),
           m=mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="m[32]"]),r=mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="r[32]"]),
           Fmort=mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="Fmort[31]"]),mu=mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="mu[31]"]),
           I=mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="Irep[32]"]),IR=mean(test3$sims.matrix[,colnames(test3$sims.matrix)=="IRrep[32]"])
)


test4<-SSModel.jags(c(DDspa4.dat,BoFSPA4.priors),priors=priors,model.file=BoFmodel,seed=100,
                    Years=1983:2014, nchains=2,niter=50000,nburnin=25000,nthin=10,para=T)
summary(test4)

# These do not seem to start at the same spot, they are similar but that's because it picks good inits...
plot(test4$sims.matrix[1:50,colnames(test4$sims.matrix)=="K"],type="l",ylim=c(2000,3500))
for(i in 1:9) lines(test4$sims.matrix[(50*i+1):(50*i+50),colnames(test4$sims.matrix)=="K"],col=i)


test5<-SSModel.jags(c(DDspa4.dat,BoFSPA4.priors),model.file=BoFmodel,seed=52,
                    Years=1983:2014, nchains=10,niter=50,nburnin=0,nthin=1,para=T)
# These do not seem to start at the same spot, they are similar but that's because it picks good inits...
plot(test5$sims.matrix[1:50,colnames(test5$sims.matrix)=="K"],type="l",ylim=c(2000,3500))
for(i in 1:9) lines(test5$sims.matrix[(50*i+1):(50*i+50),colnames(test5$sims.matrix)=="K"],col=i)

# Trying to look how different first value is from initial...
test6 <- SSModel(DDspa4.dat,BoFSPA4.priors,inits = BoFSPA4.inits,model.file=BoFmodel,Years=1983:2014,
                 nchains=2,niter=50,nburnin=0,nthin=1)


### Now try this with SSmodeljags package Stephen has, need to remove everything else since package names are the same...
rm(list=ls(all=T))
data <- DDspa4.dat
priors <-BoFSPA4.priors
inits <- BoFSPA4.inits
model.file=BoFmodel
detach(package:SSModel)# unload if loaded...
library(SSModeljags)
library(R2WinBUGS)
null.inits <- SSModel(DDspa4.dat,BoFSPA4.priors,inits = NULL,model.file=BoFmodel,Years=1983:2014,
                 nchains=2,niter=50000,nburnin=25000,nthin=10,Parallel = F)
summary(null.inits)

default.inits <- SSModel(DDspa4.dat,BoFSPA4.priors,inits = inits,model.file=BoFmodel,Years=1983:2014,
                                       nchains=2,niter=50000,nburnin=25000,nthin=10,Parallel = F)
summary(default.inits)

parallel.run <-  SSModel(DDspa4.dat,BoFSPA4.priors,inits = NULL,model.file="D:/R/Assessment_fns/Model/Archive/JAGS/BoF_model_from_SSModeljags.txt",
                     Years=1983:2014,nchains=2,niter=50000,nburnin=25000,nthin=10,Parallel = T)
summary(parallel.run)
