#####################################################################################################################################################################
### A function that will calculate our decision table for TLM or SEAM.  Your main inputs are the model data.  The TRP/USR/LRP/RR can all be included if they exist.
### Post survey landings can also be added.  The Recruitment, mortality, and growth rates can all be adjusted using a 'multiplier' or taking the median of the
### time series. 
#####################################################################################################################################################################
# As of August 2023 I haven't made the r/m/g adjustments super flexible, it'd be nice to be able to add in custom values for these eventually I think.


#mod.select:      The model you are running, "TLM" and "SEAM" are current options

#data:            The model output

#catch.scenarios  The catch removals you want to build the table for, a vector of numbers is what it needs

#n.sims           The number of simulations to run, default is 1e6 which runs in seconds 

#TRP              The target reference point, if it exists

#USR              The Upper Stock Reference point, if it exists

#LRP              The Limit(lower) Reference Point, if it exists

#RR               The Removal Reference Point, if it exists. In % so use 10 not 0.1!
#RR.TRP           In case we decided we have a different removal reference above the TRP (i.e. we really use the harvest control rules)
#PSL              The landings from the fishery after the survey and up to the end of the calendar year (e.g. BBN survey is in May, so catch from June - December)

#r.adj            Do we want to reweight the recruitment estimate. The default of 1 uses current value, this is a multiplier, thus 0 = no recruit, 2 means twice as many.
#                 Setting to 'avg' will take the time series median

#m.adj            Do we want to reweight the natural mortality estimate. The default of 1 does nothing, this is a multiplier, thus 0 = no mortality, 2 means double mortality
#                 Setting to 'avg' will take the time series median

#g.adj            Do we want to reweight the fully recruited growth estimate. Note that setting this one to 0 sets growth to 1 (i.e. 0 growth). Otherwise this is a multiplier, just be
#                 careful because here as a growth of 1 = no growth, thus a small multiplier will result in a value < 1 which means 'negative growth', which you
#                 don't want unless you are exploring a unique scenario. Setting to 'avg' will take the time series median

#gR.adj           Do we want to reweight the recruit growth estimate. Note that setting this one to 0 sets growth to 1 (i.e. 0 growth). Otherwise this is a multiplier, just be
#                 careful because here as a growth of 1 uses last years growth thus a small multiplier will result in a value < 1 which means 'negative growth', which you
#                 don't want unless you are exploring a unique scenario. Setting to 'avg' will take the time series median
dec.tab <- Vfunction(mod.select = "SEAM",data = NULL, catch.scenarios = seq(0,10000,by=50),n.sims = 1e6,TRP = NULL,USR = NULL,LRP = NULL, RR = NULL,RR.TRP = NULL,PSL = 0,
                    r.adj =1,m.adj=1,g.adj=1,gR.adj=1)
{
library(SEBDAM)

# Number of catch scenarios
n.catch.scenarios <- length(catch.scenarios)
# Catch adjusted for the catch coming after the survey
adj.catch.scenarios <- catch.scenarios + PSL

pred.proc <- get_processes(data)

# Growth data.
gs <- data$obj$env$data$gI[length(data$obj$env$data$gI)]
gRs <- data$obj$env$data$gR[length(data$obj$env$data$gR)]

# The growth scenarios, first on is 0 growth
if(g.adj == 0) gs <- gs/gs
if(gR.adj == 0) gRs <- gRs/gRs
# Next is the multiplier on current growth rate
if(g.adj != 0 & g.adj != 'avg') gs <- gs*g.adj
if(gR.adj != 0 & gR.adj != 'avg') gRs <- gRs*gR.adj
# Final is just using the average
if(g.adj == "avg")  gs <- median(data$obj$env$data$gI,na.rm=T)
if(gR.adj == "avg")  gRs <- median(data$obj$env$data$gR,na.rm=T) 




if(mod.select == "TLM")
{
  B.log <- pred.proc$log_processes$log_B[length(pred.proc$log_processes$log_B)-1]
  B.log.se <- pred.proc$log_processes$se_log_B[length(pred.proc$log_processes$se_log_B)-1]
  # Recruitment
  if(r.adj != "avg") R.log <- log(exp(pred.proc$log_processes$log_R[length(pred.proc$log_processes$log_R)-1])*r.adj)
  if(r.adj == "avg") R.log <- median(pred.proc$log_processes$log_R,na.rm=T)
  if(r.adj != "avg") R.log.se <- pred.proc$log_processes$se_log_R[length(pred.proc$log_processes$se_log_R)-1]
  if(r.adj == "avg") R.log.se <- median(pred.proc$log_processes$se_log_R,na.rm=T)
  
  if(m.adj != "avg") m.log <- log(exp(pred.proc$log_processes$log_m[length(pred.proc$log_processes$log_m)-1])*m.adj)
  if(m.adj == "avg") m.log <- median(pred.proc$log_processes$log_m,na.rm=T)
  if(m.adj != "avg") m.log.se <- pred.proc$log_processes$se_log_m[length(pred.proc$log_processes$se_log_m)-1]
  if(m.adj == "avg") m.log.se <- median(pred.proc$log_processes$se_log_m,na.rm=T)
}

if(mod.select != "TLM")
{
  #browser()  
  B.log <- pred.proc$log_tot_frame$log_totB[length(pred.proc$log_tot_frame$log_totB)-1]
  B.log.se <- pred.proc$log_tot_frame$se_log_totB[length(pred.proc$log_tot_frame$se_log_totB)-1]
  #Recrutiment
  if(r.adj != "avg") R.log <- log(exp(pred.proc$log_tot_frame$log_totR[length(pred.proc$log_tot_frame$log_totR)-1])*r.adj)
  if(r.adj == "avg") R.log <- median(pred.proc$log_tot_frame$log_totR,na.rm=T)
  if(r.adj != "avg") R.log.se <- pred.proc$log_tot_frame$se_log_totR[length(pred.proc$log_tot_frame$se_log_totR)-1]
  if(r.adj == "avg") R.log.se <- median(pred.proc$log_tot_frame$se_log_totR,na.rm=T)
  #Natural mortality
  if(m.adj != "avg") m.log <- log(exp(pred.proc$log_tot_frame$log_mean_m[length(pred.proc$log_tot_frame$log_mean_m)-1])*m.adj)
 
  if(m.adj == "avg") m.log <- median(pred.proc$log_tot_frame$log_mean_m,na.rm=T)
  if(m.adj != "avg") m.log.se <- pred.proc$log_tot_frame$se_log_mean_m[length(pred.proc$log_tot_frame$se_log_mean_m)-1]
  if(m.adj == "avg") m.log.se <- median(pred.proc$log_tot_frame$se_log_mean_m,na.rm=T)
}


# Initialize some data...
decision.table <- data.frame(catch = rep(NA,n.catch.scenarios), exploit = rep(NA,n.catch.scenarios),
                             Biomass = rep(NA,n.catch.scenarios),per.diff = rep(NA,n.catch.scenarios), B.change = rep(NA,n.catch.scenarios), 
                             prob.decline = rep(NA,n.catch.scenarios))

B.obj <- NULL
for(i in 1:n.catch.scenarios) 
{
  #browser()
  # So now sample from the log normals using the above data. I looked at doing the lognormal bias correction but it didn't seem helpful here (lead to a positive bias in the 
  # 0 surplus production scenario)
  Bs.tot <- rlnorm(n.sims,B.log,B.log.se)
  Rs.tot <- rlnorm(n.sims,R.log,R.log.se)
  ms     <- rlnorm(n.sims,m.log,m.log.se)
  # Run the model through the catch scenarios, using the adjusted catch scenario that includes the Post Survey Catch.
  #browser()
  Bst <- (exp(-ms))*gs*(Bs.tot-adj.catch.scenarios[i]) 
  Rst <- (exp(-ms))*gRs*(Rs.tot) 
  B2 <- Bst + Rst
  B.obj[[i]] <- data.frame(B2 = B2,scenario = rep(catch.scenarios[i],length(B2)))
  # We don't put the adjustment for the Post Survey Landings in the exploitation calculation, as this exploitation is for the following calendar year 
  exploit <- 100* (catch.scenarios[i]/(B2+ catch.scenarios[i])) 
  B.diff <- B2 - exp(B.log) 
  B.per.diff <- 100*((B2 - exp(B.log) )/exp(B.log) )
  decision.table$catch[i] <- as.numeric(catch.scenarios[i]) # We don't put in the adjustment from Post Survey Landings into the table.
  decision.table$Biomass[i] <- signif(median(B2,na.rm=T),digits=3)
  decision.table$per.diff[i] <- signif(median(B.per.diff),digits=3)
  decision.table$B.change[i]   <- signif(median(B.diff),digits=3)
  decision.table$prob.decline[i] <- signif(length(B2[B2 < exp(B.log)]) / n.sims,digits = 2)
  # Making these so decimal places look nice
  if(median(exploit) == 0) decision.table$exploit[i]  <- "0.0"
  if(median(exploit) < 1 & median(exploit) <= 0.95 & median(exploit) != 0) decision.table$exploit[i]  <- as.character(signif(median(exploit),digits=1))
  if(median(exploit) < 1 & median(exploit) >=0.95) decision.table$exploit[i]  <- "1.0"
  if(median(exploit) >= 1) decision.table$exploit[i]  <- as.character(signif(median(exploit),digits=2))
  # If we have reference points add these to the table.
  if(!is.null(LRP)) 
  {
    raw.LRP <- length(B2[B2 < LRP]) / n.sims
    decision.table$prob.below.LRP[i] <- signif(raw.LRP,digits = 2)
    if(raw.LRP > 0.995) decision.table$prob.below.LRP[i] <- "> 0.99"
    if(raw.LRP < 0.00995) decision.table$prob.below.LRP[i] <- "< 0.01"
    if(raw.LRP >= 0.00995 & raw.LRP < 0.0995) decision.table$prob.below.LRP[i] <- signif(raw.LRP, digits = 1)
  }
  if(!is.null(USR)) 
  {
    raw.USR <- length(B2[B2 < USR])/ n.sims
    decision.table$prob.below.USR[i] <- signif(raw.USR,digits = 2)
    if(raw.USR > 0.995) decision.table$prob.below.USR[i] <- "> 0.99"
    if(raw.USR < 0.00995) decision.table$prob.below.USR[i] <- "< 0.01"
    if(raw.USR >= 0.00995 & raw.USR < 0.0995) decision.table$prob.below.USR[i] <- signif(raw.USR, digits = 1)
  }
  if(!is.null(RR)) 
  {
    # Now we need to define what the Removal Reference point is at different biomass levels, here I am
    # defining a harvest control rule in the Cautious zone, simple linear HCR.
    
    if(decision.table$Biomass[i] >= USR) RR.tmp <- RR
    if(decision.table$Biomass[i] < LRP) RR.tmp <- 0
    if(decision.table$Biomass[i] >= LRP & decision.table$Biomass[i] < USR) RR.tmp <- RR*(1-((USR-decision.table$Biomass[i])/(USR-LRP)))
    #browser()
    raw.RR <- length(exploit[exploit < RR.tmp])/ n.sims
    decision.table$prob.below.RR[i] <- signif(raw.RR, digits = 2)
    decision.table$RR[i] <- round(RR.tmp, digits = 2)
    if(raw.RR > 0.995) decision.table$prob.below.RR[i] <- "> 0.99"
    if(raw.RR < 0.00995) decision.table$prob.below.RR[i] <- "< 0.01"
    if(raw.RR >= 0.00995 & raw.RR < 0.0995) decision.table$prob.below.RR[i] <- signif(raw.RR, digits = 1)
  }
  
  if(!is.null(TRP)) 
  {
    raw.TRP <- length(B2[B2 < TRP]) / n.sims
    decision.table$prob.below.TRP[i] <- signif(raw.TRP, digits = 2)
    if(raw.TRP > 0.995) decision.table$prob.below.TRP[i] <- "> 0.99"
    if(raw.TRP < 0.00995) decision.table$prob.below.TRP[i] <- "< 0.01"
    if(raw.TRP >= 0.00995 & raw.TRP < 0.0995) decision.table$prob.below.TRP[i] <- signif(raw.TRP, digits = 1)
  }

  # If we have a RR TRP, which we will only add if the projected biomass in the 0 exploitation scenario is > TRP
  #browser()
  if(!is.null(RR.TRP)) 
  {
    if( decision.table$Biomass[1] > TRP)
    {
      # Add a TRP column when these crtieria are met...
      raw.RR.TRP <- length(exploit[exploit < RR.TRP])/ n.sims
      decision.table$prob.below.RR.TRP[i] <- signif(raw.RR.TRP, digits = 2)
      decision.table$RR.TRP[i] <- round(RR.TRP, digits = 2)
      if(raw.RR.TRP > 0.995) decision.table$prob.below.RR.TRP[i] <- "> 0.99"
      if(raw.RR.TRP < 0.00995) decision.table$prob.below.RR.TRP[i] <- "< 0.01"
      if(raw.RR.TRP >= 0.00995 & raw.RR < 0.0995) decision.table$prob.below.RR.TRP[i] <- signif(raw.RR.TRP, digits = 1)
    }
  }
  
}  # End table loop
ndt <- c("Catch (tonnes)", "Exploitation (%)", "Biomass (tonnes)","Biomass change (%)", "Biomass change (tonnes)", "Probability of Decline")
if(!is.null(LRP)) ndt <- c(ndt,"Probability biomass is below LRP")
if(!is.null(USR)) ndt <- c(ndt,"Probability biomass is below USR")
if(!is.null(RR)) ndt <- c(ndt,"Probability exploitation is below RR","Removal Reference (%)")
if(!is.null(TRP)) ndt <- c(ndt,"Probability biomass is below TRP")
if(!is.null(RR.TRP))
{
 if(decision.table$Biomass[1] > TRP) ndt <- c(ndt,"Probability exploitation is below RR @ TRP","TRP Removal Reference (%)")
}
    
B.obj <- do.call("rbind",B.obj)
names(decision.table) <- ndt

return(list(table = decision.table,Biomass= B.obj))

} # end function
