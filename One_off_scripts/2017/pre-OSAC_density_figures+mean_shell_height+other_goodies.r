###  This is a little script to get me a bit more information on some specific questions for 2017 OSAC

yr <- 2017
direct <- "d:/r/"


load(paste(direct,"Data/Survey_data/",2017,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))

#Here's a quick peak at the average size of scallop above whatever size I'm interested in
mw.bin<-seq(5,200,5)
n.yst <- survey.obj$GBa$shf.dat$n.yst[nrow(survey.obj$GBa$shf.dat$n.yst),]
sum((n.yst*seq(2.5,200,5))[which(mw.bin==105):which(mw.bin==200)]) /
  sum(n.yst[which(mw.bin==105):which(mw.bin==200)])
n.yst <- survey.obj$BBn$shf.dat$n.yst[nrow(survey.obj$BBn$shf.dat$n.yst),]
sum((n.yst*seq(2.5,200,5))[which(mw.bin==100):which(mw.bin==200)]) /
  sum(n.yst[which(mw.bin==100):which(mw.bin==200)])
n.yst <- survey.obj$GBb$shf.dat$n.yst[nrow(survey.obj$GBb$shf.dat$n.yst),]
sum((n.yst*seq(2.5,200,5))[which(mw.bin==120):which(mw.bin==200)]) /
  sum(n.yst[which(mw.bin==120):which(mw.bin==200)])

# This bit is to determine now many scallop there are in a given strata, rather than by tow I'm going for
# a density
atow.m2 <- atow * 1e6
inshore.m2 <- 800*17.5/3.2808
q <- 0.25
round.conversion <- 
# Here's the mean number per m2 of each size in each strata in 2017, from the survey and corrected for catchability (very conservatively using 0.25 which will lead to high estiamtes of N and B per tow)
mean.npt <- aggregate(cbind(pre,rec,com,tot)/atow.m2/q~Strata_ID+year,surv.Live$GBa,mean)
max.npt <- aggregate(cbind(pre,rec,com,tot)/atow.m2/q~Strata_ID + year,surv.Live$GBa,max)


#windows(11,11)
pdf(paste(direct,"2017/One_time_issues/Densities_per_m2_for_OSAC/N_by_strata.pdf",sep=""),width=12,height = 6,onefile = T)
ggplot(mean.npt, aes(year,pre)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Mean Pre-recruit Abundance (N/m2) by Strata")
ggplot(mean.npt, aes(year,rec)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Mean Recruit Abundance (N/m2) by Strata")
ggplot(mean.npt, aes(year,com)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Mean Fully-recruited Abundance (N/m2) by Strata")
ggplot(mean.npt, aes(year,tot)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Mean Overall Abundance (N/m2) by Strata")
ggplot(max.npt, aes(year,pre)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Maximum Pre-recruit Abundance (N/m2) by Strata")
ggplot(max.npt, aes(year,rec)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Maximum Recruit Abundance (N/m2) by Strata")
ggplot(max.npt, aes(year,com)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Maximum Fully-recruited Abundance (N/m2) by Strata")
ggplot(max.npt, aes(year,tot)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Maximum Overall Abundance (N/m2) by Strata")

dev.off()


# Now the same thing for the biomass
mean.bpt <- aggregate(cbind(pre.bm,rec.bm,com.bm,tot.bm)/atow.m2*8.3/q ~ year + Strata_ID,surv.Live$GBa,mean)
mean.bpt
max.bpt <- aggregate(cbind(pre.bm,rec.bm,com.bm,tot.bm)/atow.m2*8.3/q ~ year + Strata_ID,surv.Live$GBa,max)
max.bpt


pdf(paste(direct,"2017/One_time_issues/Densities_per_m2_for_OSAC/Biomass_by_strata.pdf",sep=""),width=12,height = 6,onefile = T)
ggplot(mean.bpt, aes(year,pre.bm)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Mean Pre-recruit Biomass (kg-round/m2) by Strata")
ggplot(mean.bpt, aes(year,rec.bm)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Mean Recruit Biomass (kg-round/m2)  by Strata")
ggplot(mean.bpt, aes(year,com.bm)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Mean Fully-recruited Biomass (kg-round/m2)  by Strata")
ggplot(mean.bpt, aes(year,tot.bm)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Mean Overall Biomass (kg-round/m2) by Strata")
ggplot(max.bpt, aes(year,pre.bm)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Maximum Pre-recruit Biomass (kg-round/m2)  by Strata")
ggplot(max.bpt, aes(year,rec.bm)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Maximum Recruit Biomass (kg-round/m2)  by Strata")
ggplot(max.bpt, aes(year,com.bm)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Maximum Fully-recruited Biomass (kg-round/m2)  by Strata")
ggplot(max.bpt, aes(year,tot.bm)) +geom_point() + facet_wrap(~Strata_ID) + ggtitle("Maximum Overall Biomass (kg-round/m2)  by Strata")

dev.off()

