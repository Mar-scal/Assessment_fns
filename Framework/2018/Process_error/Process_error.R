## THis little script grabs the latest mosdel results and plots up the relationship b/t process error and mortality and growth...
options(stringsAsFactors = F)
direct <- "d:/r/"
library(tidyverse)
# This has BBn and GBa in it...
#load(paste(direct,"/Data/Model/2018/GBa/Results/Final_Model_results_and_diagnostics.RData",sep=""))
load(paste(direct,"/Data/Model/2018/GBa/Results/Final_model_results.RData",sep=""))
#load(paste(direct,"/Data/Model/2017/GBa/Results/Final_model_results.RData",sep=""))
# Make a gba.dat object that is GGplot friendly.
gba.dat <- data.frame(year = yrs$GBa, yr = substr(yrs$GBa,3,4),process = apply(DD.out$GBa$sims.list$sPresid,2,median),
                      nat_mort = apply(DD.out$GBa$sims.list$m,2,function(x) median(1-exp(-x))), rec_mort =  apply(DD.out$GBa$sims.list$mR,2,function(x) median(1-exp(-x))),
                      f_mort = apply(DD.out$GBa$sims.list$mu,2,median), period = "Pre 2004", catch = mod.dat$GBa$catch[mod.dat$GBa$year > 1985],
                      growth = mod.dat$GBa$g[mod.dat$GBa$year > 1985],bank = "GBa")

bbn.dat <- data.frame(year = yrs$BBn,yr = substr(yrs$BBn,3,4),process = apply(DD.out$BBn$sims.list$sPresid,2,median),
                      nat_mort = apply(DD.out$BBn$sims.list$m,2,function(x) median(1-exp(-x))), rec_mort = apply(DD.out$BBn$sims.list$mR,2,function(x) median(1-exp(-x))),
                      f_mort = apply(DD.out$BBn$sims.list$mu,2,median),period = "Pre 2004",catch = mod.dat$BBn$catch[mod.dat$BBn$year > 1990],
                      growth = mod.dat$BBn$g[mod.dat$BBn$year > 1990],bank="BBn")
all.dat <- rbind(gba.dat,bbn.dat)
all.dat$period[all.dat$year %in% 2004:2009] <- "2004-2008"
all.dat$period[all.dat$year >= 2009] <- "2009-2017"
all.dat$period[all.dat$year %in% 2009 & all.dat$bank == "BBn"] <- "Limited Fishing"

# Now plot the results, we could include the uncertainty in the process error terms, but I don't think that's entirely necessary here...
ggplot() + geom_text(data=all.dat, aes(nat_mort,process,colour = period,label=yr),show.legend = FALSE,size=4) + theme_bw(base_size = 24) + facet_wrap(~bank,scales = "free")+
  #geom_smooth(data = all.dat, aes(nat_mort,process, colour = period), size=1,method = "lm",se=F)+
  geom_hline(data=all.dat, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Process error") + xlab("FR Natural Mortality")+ #theme(legend.position="none") + 
  scale_color_manual(values = c("black","blue","green","grey"))#+
ggsave(paste0(direct, yr+1, "/Framework/Process_error/fr_nat_mortality_vs_process_error_no_trendline.png"),width=11,height=6,units="in")

ggplot() + geom_text(data=all.dat, aes(rec_mort,process,colour = period,label=yr),show.legend = FALSE,size=4) + theme_bw(base_size = 24) + facet_wrap(~bank,scales = "free")+
  geom_smooth(data = all.dat, aes(rec_mort,process, colour = period), size=1,method = "lm",se=F)+
  geom_hline(data=all.dat, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Process error") + xlab("Recruit Natural Mortality")+ #theme(legend.position="none") + 
  scale_color_manual(values = c("black","blue","green","grey"))#+
ggsave(paste0(direct, yr+1, "/Framework/Process_error/rec_mortality_vs_process_error.png"),width=11,height=6,units="in")


ggplot() + geom_text(data=all.dat, aes(f_mort+nat_mort,process,colour = period,label=yr),show.legend = FALSE,size=4) + theme_bw(base_size=24) + facet_wrap(~bank,scales = "free")+
  #geom_smooth(data = all.dat, aes(f_mort+nat_mort,process, colour = period), size=1,method = "lm",se=F)+
  geom_hline(data=all.dat, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Process error") + xlab("Total Mortality")+ #theme(legend.position="none") + 
  scale_color_manual(values = c("black","blue","green","grey"))#+
ggsave(paste0(direct, yr+1, "/Framework/Process_error/total_fr_mort_vs_process_error.png"),width=11,height=6,units="in")


ggplot() + geom_text(data=all.dat, aes(f_mort,process,colour = period,label=yr),show.legend = FALSE,size=4) + theme_bw(base_size = 24) + facet_wrap(~bank,scales = "free")+
  #geom_smooth(data = all.dat, aes(f_mort+nat_mort,process, colour = period), size=1,method = "lm",se=F)+
  geom_hline(data=all.dat, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Process error") + xlab("Fishing Mortality")+ #theme(legend.position="none") + 
  scale_color_manual(values = c("black","blue","green","grey"))#+
ggsave(paste0(direct, yr+1, "/Framework/Process_error/fishing_mort_vs_process_error.png"),width=11,height=6,units="in")

# Process error v.s. catch...
ggplot() + geom_text(data=all.dat, aes(catch,process,colour = period,label=yr),show.legend = FALSE,size=2.5) + theme_bw() + facet_wrap(~bank,scales = "free")+
  #geom_smooth(data = all.dat, aes(f_mort+nat_mort,process, colour = period), size=1,method = "lm",se=F)+
  geom_hline(data=all.dat, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Process error") + xlab("Catch")+ #theme(legend.position="none") + 
  scale_color_manual(values = c("black","blue","green","grey"))#+
ggsave(paste0(direct, yr+1, "/Framework/Process_error/catch_vs_process_error.png"),width=11,height=6,units="in")


ggplot() + geom_text(data=all.dat, aes((nat_mort+rec_mort)/2,process,colour = period,label=yr),show.legend = FALSE,size=2.5) + theme_bw() + facet_wrap(~bank,scales = "free")+
  #geom_smooth(data = all.dat, aes(nat_mort,process, colour = period), size=1,method = "lm",se=F)+
  geom_hline(data=all.dat, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Process error") + xlab("Mean Natural Mortality")+ #theme(legend.position="none") + 
  scale_color_manual(values = c("black","blue","green","grey"))#+
ggsave(paste0(direct, yr+1, "/Framework/Process_error/fr_rec_mean_nat_mortality_vs_process_error_no_trendline.png"),width=11,height=6,units="in")


ggplot(data=all.dat,aes(growth,process,label=yr,colour=period)) + geom_text( show.legend = F,size=4) + theme_bw(base_size=24) + facet_wrap(~bank,scales = "free")+
  #geom_smooth(data = all.dat, aes(growth,process),method = "lm",se=F,size=0)+
  geom_hline(data=all.dat, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Process error") + xlab("Growth")+ #theme(legend.position="none") + 
  scale_color_manual(values = c("black","blue","green","grey"))#+
ggsave(paste0(direct, yr+1, "/Framework/Process_error/growth_vs_process_error.png"),width=11,height=6,units="in")

windows(11,11)
ggplot() + geom_text(data=all.dat, aes(growth,nat_mort,colour = period,label=yr),show.legend = FALSE,size=2.5) + theme_bw() + facet_wrap(~bank,scales = "free")+
  geom_smooth(data = all.dat, aes(growth,nat_mort, colour = period), size=1,method = "lm",se=T)+
  geom_hline(data=all.dat, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Mortality") + xlab("Growth")+ #theme(legend.position="none") + 
  scale_color_manual(values = c("black","blue","green","grey"))#+
ggsave(paste0(direct, yr+1, "/Framework/Process_error/growth_vs_mortality.png"),width=11,height=6,units="in")



#windows(11,11)
##ggplot() + geom_text(aes(process[1:18], n.mort[1:18],label=yrs$GBa[1:18]),size=2.5) + 
#  ggtitle("Mortality v.s. Process Error  1986-2003") + 
#  xlab("Process Error") + ylab("Recruit + Fully Recruited natural mortality")
#geom_vline(data=procerr, aes(xintercept=2003.5), lty="dashed") +
#geom_vline(data=procerr, aes(xintercept=2008.5), lty="dashed") +
#geom_rect(data=procerr[procerr$era=="pre",], aes(xmin = min(year)-0.5, xmax=max(year)+0.5, ymin=-Inf, ymax=Inf), fill="red", alpha=0.01) +
#geom_rect(data=procerr[procerr$era=="FTs",], aes(xmin = min(year)-0.5, xmax=max(year)+0.5, ymin=-Inf, ymax=Inf), fill="orange", alpha=0.1) +
#geom_rect(data=procerr[procerr$era=="ASMs",], aes(xmin = min(year)-0.5, xmax=max(year)+0.5, ymin=-Inf, ymax=Inf), fill="yellow", alpha=0.05) 

require(viridis)
B <- ggplot() + 
  geom_smooth(data=procerr, aes(sPresid, mumR, colour=era), size=1,  method="lm") +
  geom_point(data=procerr, aes(sPresid, mumR, colour=era, shape=era), size=3) + 
  theme_bw() +
  geom_vline(data=procerr, aes(xintercept=0), lty="dashed") +
  scale_colour_viridis(discrete=TRUE, name=NULL, labels=c("Automatic shucking\n(2009-2017)\n", 
                                                          "Freezer trawlers\n(2004-2008)\n",
                                                          "Wet fish only\n(1986-2003)")) +
  scale_shape_discrete(name=NULL, labels=c("Automatic shucking\n(2009-2017)\n", 
                                           "Freezer trawlers\n(2004-2008)\n",
                                           "Wet fish only\n(1986-2003)")) +
  scale_linetype_discrete(name=NULL, labels=c("Automatic shucking\n(2009-2017)\n", 
                                              "Freezer trawlers\n(2004-2008)\n",
                                              "Wet fish only\n(1986-2003)")) +
  theme(panel.grid=element_blank(), legend.position=c(0.8, 0.8)) +
  ylab("Recruits + Fully recruited natural mortality") +
  xlab("Standardized process error")

require(gridExtra)
grid.arrange(A, B, nrow=2)
dev.off()



load(paste(direct,"/Data/Model/2018/BBn/Results/Model_testing_results.RData",sep=""))
load(paste(direct,"/Data/Model/2018/BBn/Results/Model_results_and_diagnostics.RData",sep=""))

procerr <- data.frame(Presid = mod.out$BBn$BUGSoutput$median$Presid, sPresid = mod.out$BBn$BUGSoutput$median$sPresid, 
                      mumR = mod.out$BBn$BUGSoutput$median$m + mod.out$BBn$BUGSoutput$median$mR, year = 1991:2017, era = c(rep("pre", 13), rep("FTs", 5), rep("ASMs", 9)))

Q1 <- NULL
Q2 <- NULL
for (i in 1:length(procerr$year)){
  Q1i <- quantile(DD.out[["BBn"]]$sims.list["sPresid"][[1]][,i], 0.025) 
  Q2i <- quantile(DD.out[["BBn"]]$sims.list["sPresid"][[1]][,i], 0.975) 
  Q1 <- c(Q1, Q1i)
  Q2 <- c(Q2, Q2i)}

procerr$Q1 <- Q1
procerr$Q2 <- Q2

png(paste0(direct, yr+1, "/Updates/BBn/Figures_and_tables/mortality vs process error panel fig.png"), type="cairo", height=12, width=8, res=100, units="in")
A <- ggplot() + geom_point(data=procerr, aes(year, sPresid)) + theme_bw() +
  geom_errorbar(data=procerr, aes(year, ymin=sPresid - Q1, ymax=sPresid - Q2), width=0) +
  geom_hline(data=procerr, aes(yintercept=0),linetype="dashed") +
  theme(panel.grid=element_blank()) +
  ylab("Process error") +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1990, 2015, 5)) +
  scale_y_continuous(breaks=seq(-1,1.5, 0.5)) 

require(viridis)
B <- ggplot() + 
  geom_smooth(data=procerr, aes(sPresid, mumR, colour=era), size=1,  method="lm") +
  geom_point(data=procerr, aes(sPresid, mumR, colour=era, shape=era), size=3) + 
  theme_bw() +
  geom_vline(data=procerr, aes(xintercept=0), lty="dashed") +
  scale_colour_viridis(discrete=TRUE, name=NULL, labels=c("Automatic shucking\n(2009-2017)\n", 
                                                          "Freezer trawlers\n(2004-2008)\n",
                                                          "Wet fish only\n(1986-2003)")) +
  scale_shape_discrete(name=NULL, labels=c("Automatic shucking\n(2009-2017)\n", 
                                           "Freezer trawlers\n(2004-2008)\n",
                                           "Wet fish only\n(1986-2003)")) +
  scale_linetype_discrete(name=NULL, labels=c("Automatic shucking\n(2009-2017)\n", 
                                              "Freezer trawlers\n(2004-2008)\n",
                                              "Wet fish only\n(1986-2003)")) +
  theme(panel.grid=element_blank(), legend.position=c(0.8, 0.8)) +
  ylab("Recruits + Fully recruited natural mortality") +
  xlab("Standardized process error")

require(gridExtra)
grid.arrange(A, B, nrow=2)
dev.off()



