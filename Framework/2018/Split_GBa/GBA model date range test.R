# checking date ranges

#direct = "d:/r/"
yr <-2017
# For linux
#direct = "/media/sf_data/r/"
#direct = "d:/r"
direct <- "Y:/Offshore scallop/Assessment/"
# All that is left to do is get the arguements for the final 3 model functions update and everything should be gold.
source(paste(direct,"Assessment_fns/Model/Update_function_JAGS.r",sep=""))
#source(paste(direct,"Assessment_fns/Model/diag.plt.R",sep=""))

update_JAGS(preprocessed=T, # Do you want to load the preprocessed data, you should only need this set to T the first time you run this after logs are complete
            run.mod=T, # Do you want to re-run the model or use previously saved version
            use.final=F, # Did you do a final model run and is that the one you want to load
            final.run=F, # Is this your final run that will get results used in the model
            parallel=T, # Run in parallel, not sure why you wouldn't as it is quicker...
            make.diag.figs = T, # Do you want to make the diagnostic figures, these are not used for updates
            make.update.figs=T, # Do you want to make the figures for the update
            run.pred.eval.model = F, # Do you want to run the prediction evaluation model
            export.tables = T,  # Do you want to save the decision table
            bank=c("GBa"),yr=2017,strt.mod.yr=2000,
            fig="pdf",pred.eval.fig.type="box",
            niter = 200000,nburn = 100000,nchains=10,
            direct=direct,un=un.ID,pw=pwd.ID,db.con="ptran64") #Running both banks fully takes about 40 minutes.
# Run the model and see how long it takes.) 

# started at 11:39AM

# how to run up to 1999?
# must run Update_function_JAGS.r line by line and set the maximum year to 1999

# Now we can compare
load(paste(direct,"/Data/Framework/2018/Split_GBa/1986-1999/Model_results_and_diagnostics_test.RData",sep=""))
load(paste(direct,"/Data/Framework/2018/Split_GBa/1986-1999/Model_testing_results_test.RData",sep=""))
older.mod.dat <- mod.dat
older.DD.out <- DD.out

load(paste(direct,"/Data/Framework/2018/Split_GBa/2000-2017/Model_results_and_diagnostics.RData",sep=""))
load(paste(direct,"/Data/Framework/2018/Split_GBa/2000-2017/Model_testing_results.RData",sep=""))
newer.mod.dat <- mod.dat
newer.DD.out <- DD.out

load(paste(direct,"/Data/Model/2018/GBa/Results/FINAL/Final_model_results_and_diagnostics.RData",sep=""))
load(paste(direct,"/Data/Model/2018/GBa/Results/FINAL/Final_model_results.RData",sep=""))
full.mod.dat <- mod.dat
full.DD.out <- DD.out

## what if we start from 2004 instead of 2000?

update_JAGS(preprocessed=T, # Do you want to load the preprocessed data, you should only need this set to T the first time you run this after logs are complete
            run.mod=T, # Do you want to re-run the model or use previously saved version
            use.final=F, # Did you do a final model run and is that the one you want to load
            final.run=F, # Is this your final run that will get results used in the model
            parallel=T, # Run in parallel, not sure why you wouldn't as it is quicker...
            make.diag.figs = T, # Do you want to make the diagnostic figures, these are not used for updates
            make.update.figs=T, # Do you want to make the figures for the update
            run.pred.eval.model = F, # Do you want to run the prediction evaluation model
            export.tables = T,  # Do you want to save the decision table
            bank=c("GBa"),yr=2017,strt.mod.yr=2004,
            fig="pdf",pred.eval.fig.type="box",
            niter = 200000,nburn = 100000,nchains=10,
            direct=direct,un=un.ID,pw=pwd.ID,db.con="ptran64") #Running both banks fully takes about 40 minutes.
# Run the model and see how long it takes.) 

load(paste(direct,"/Data/Framework/2018/Split_GBa/2004-2017/Model_results_and_diagnostics.RData",sep=""))
load(paste(direct,"/Data/Framework/2018/Split_GBa/2004-2017/Model_testing_results.RData",sep=""))
recent.mod.dat <- mod.dat
recent.DD.out <- DD.out

test<-data.frame(biomass=c(older.DD.out$GBa$median$B,
                           newer.DD.out$GBa$median$B, 
                           recent.DD.out$GBa$median$B), 
                 year=c(older.DD.out$GBa$data$year, 
                        newer.DD.out$GBa$data$year, 
                        recent.DD.out$GBa$data$year), 
                 type=c(rep("old", 14), rep("new", 18), rep("recent", 14)), 
                 full=c(full.DD.out$GBa$median$B, rep(NA, 14)), 
                 fullyear=c(full.DD.out$GBa$data$year,  rep(NA, 14)))

ggplot() + geom_line(data=test[test$type=="old",], aes(year, biomass)) +
  geom_point(data=test[test$type=="old",], aes(year, biomass)) +
  geom_line(data=test[test$type=="new",], aes(year, biomass)) +
  geom_point(data=test[test$type=="new",], aes(year, biomass)) +
  geom_line(data=test[test$type=="recent",], aes(year, biomass), colour="darkgrey") +
  geom_point(data=test[test$type=="recent",], aes(year, biomass), colour="darkgrey") +
  geom_line(data=test[!is.na(test$fullyear),], aes(year, full),lty="dashed")+
  #geom_point(data=test, aes(year, full))+
  theme_bw() + 
  theme(panel.grid=element_blank())

fishcatch_old <- older.DD.out$GBa$median$qU
survcatch_old <- older.DD.out$GBa$median$q  
fishcatch_new <- newer.DD.out$GBa$median$qU
survcatch_new <- newer.DD.out$GBa$median$q  
fishcatch_recent <- recent.DD.out$GBa$median$qU
survcatch_recent <- recent.DD.out$GBa$median$q
fishcatch_full <- full.DD.out$GBa$median$qU
survcatch_full <- full.DD.out$GBa$median$q

catchab <- data.frame(fishcatch = c(fishcatch_old, fishcatch_new, fishcatch_recent, fishcatch_full),
                      survcatch = c(survcatch_old, survcatch_new, survcatch_recent, survcatch_full),
                      type=c("old", "new", "recent", "full"),
                      year=c(1986, 2000, 2004, 2017))

ggplot() + geom_line(data=test[test$type=="old",], aes(year, biomass)) +
  geom_point(data=test[test$type=="old",], aes(year, biomass)) +
  geom_line(data=test[test$type=="new",], aes(year, biomass)) +
  geom_point(data=test[test$type=="new",], aes(year, biomass)) +
  geom_line(data=test[test$type=="recent",], aes(year, biomass), colour="darkgrey") +
  geom_point(data=test[test$type=="recent",], aes(year, biomass), colour="darkgrey") +
  geom_line(data=test[!is.na(test$fullyear),], aes(year, full),lty="dashed")+
  #geom_point(data=test, aes(year, full))+
  theme_bw() + 
  geom_point(data=catchab, aes(year, fishcatch*20000000), colour="red")+
  scale_y_continuous(sec.axis = sec_axis(~./20000000, name="Fishery catchability"))+
  theme(panel.grid=element_blank())


# Let's get the survey q's pulled together....
survey.q <- data.frame(q = c(older.DD.out$GBa$sims.list$q,newer.DD.out$GBa$sims.list$q,full.DD.out$GBa$sims.list$q),
                       m = c(older.DD.out$GBa$sims.list$m[,1],newer.DD.out$GBa$sims.list$m[,1],full.DD.out$GBa$sims.list$m[,1]), # Natural mortality in a year...
                                 period = c(rep("old",length(older.DD.out$GBa$sims.list$q)),rep("new",length(newer.DD.out$GBa$sims.list$q)),rep("full",length(full.DD.out$GBa$sims.list$q))))
survey.q$m <- 1-exp(-survey.q$m)
# Yes I know these are all in the BUGS ouput, but wanted to use all the same data...
survey.q.sum <- aggregate(q~period,survey.q,mean)
survey.q.sum$se <- aggregate(q~period,survey.q,sd)$q
survey.q.sum$LCI <- survey.q.sum$q - 2*survey.q.sum$se
survey.q.sum$UCI <- survey.q.sum$q + 2*survey.q.sum$se

windows(11,11)
ggplot(survey.q.sum,aes(period,q)) + geom_point() + geom_errorbar(aes(ymin = LCI, ymax=UCI),width =0) + theme_bw() +  theme(panel.grid=element_blank()) + ylim(values=c(0,0.5)) +
                                     ylab("Survey Catchability") + xlab("")
# The relationship between q and natural mortality in whatever year I selected above.
ggplot(survey.q,aes(q,m)) + geom_point(size = 0.1) +  theme_bw() +  theme(panel.grid=element_blank()) + ylim(values=c(0,0.5)) + geom_smooth()+ facet_wrap(~period,scales="free")+
  ylab("Natural Mortality") + xlab("Survey Catchability")



# Let's get the survey K's pulled together....
survey.K <- data.frame(K = c(older.DD.out$GBa$sims.list$K,newer.DD.out$GBa$sims.list$K,full.DD.out$GBa$sims.list$K),
                       period = c(rep("old",length(older.DD.out$GBa$sims.list$K)),rep("new",length(newer.DD.out$GBa$sims.list$K)),rep("full",length(full.DD.out$GBa$sims.list$K))))
# Yes I know these are all in the BUGS ouput, but wanted to use all the same data...
survey.K.sum <- aggregate(K~period,survey.K,mean)
survey.K.sum$se <- aggregate(K~period,survey.K,sd)$K
survey.K.sum$LCI <- survey.K.sum$K - 2*survey.K.sum$se
survey.K.sum$UCI <- survey.K.sum$K + 2*survey.K.sum$se

windows(11,11)
ggplot(survey.K.sum,aes(period,K)) + geom_point() + geom_errorbar(aes(ymin = LCI, ymax=UCI),width =0) + theme_bw() +  theme(panel.grid=element_blank()) + 
  ylab("K parameter") + xlab("")
