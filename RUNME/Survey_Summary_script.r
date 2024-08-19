#####################################  The script is used to create the data objects and figures for Survey Summary##############################  
##########################################################################  #####################################  ###############################  
## Created by DK December 2015
## Update history
## 1: March 31 2016 by DK, tidying up structure and removing excess code
## 2:  May 16, 2016 by DK, now can use 64 bit version of R when querying database.
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 1: source(paste(direct,"Assessment_fns/Survey/SurveySummary_data.r",sep=""))
# 2: source(paste(direct,"Assessment_fns/Survey/Survey_summary_figures.r",sep=""))
###############################################################################################################


# Load your directory and the survey year
#direct <- "d:/r/"
#direct <- "C:/Users/keyserf/Documents/Version_control_pandemic/Offshore/Assessment/"
direct <- "Y:/Offshore/Assessment/"
#direct_fns <- "C:/Documents/Assessment_fns/"
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"
yr <- 2024
un.ID <- "ENTER UN HERE"
pwd.ID <- "ENTER PW HERE"



# The two functions that get the data and produce the survey figures
# This function only needs to be run once to compile all of the data
source(paste(direct_fns,"Survey_and_OSAC/SurveySummary_data.r",sep="")) #Source1
# This function is used to pull out the figures of interest.
source(paste(direct_fns,"Survey_and_OSAC/Survey_summary_figures_sf.r",sep="")) #Source1

#source(paste(direct,"Assessment_fns/Survey_and_OSAC/archive/Survey_summary_figures.r",sep="")) #Source1
# Here's the pre-INLA version of this if interested, should work, I hope...
#source("Y:/Offshore scallop/Assessment/Assessment_fns/Survey_and_OSAC/archive/2017/Survey_summary_figures (2).r") #Source1
# You only need to run this once to get all the survey data compiled
# This take about 3-4 minutes to run with all the banks included.
# So you have them here are thethe bank/survey combos you can use for survey.data() function. 
#surveys = c("BBnspring" ,"BBsspring" ,"Gerspring", "Midspring", "Sabspring", "GBspring" ,"GBbsummer", "GBasummer")

# This runs the survey assuming that you need to compile the survey data
# Make sure surveys and season match if using spring or summer (both doesn't matter)
# Use preprocessed = F if running for the first time,
# Use preprocessed = T if we already have the data and you just want to load it, saves about 2 minutes...
# 
#
#
# NOTE: BAN 2019 needs extras included!

res <- survey.data(direct = direct,
                   direct_fns = direct_fns,
                   un.ID=un.ID, pwd.ID=pwd.ID, preprocessed=F, yr=yr,
                   surveys = c(#"Banspring", 
                    # #"BanIcespring", 
                      "BBnspring",
                      #"BBsspring",
                     "Gerspring",
                      "Midspring",
                       "Sabspring",
                        "GBspring"#,
                         # "GBbsummer"#,
                       #   "GBasummer"
                     ), 
                   db.con="ptran",testing=T, season="spring", spatial=F, mwsh.test=F,
                   commercialsampling=T, nickname="spring2024", bins=c(50,65,85,95,120),
                   size.cats="Size_categories_by_bank_75-90.csv")

#res <- survey.data(direct = direct,un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=T,yr=2016,
#                   surveys =  c("BBnspring"),
#                   db.con="PTRAN64",season="both",survey.year = 2016,testing=T)

# Running this to get number to use in a simple model in which the recruit sizes are 60-80 mm and the 
# fully recruited are 80+ mm.
#res <- survey.data(direct = direct,un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=T,yr=2017,
#                   surveys =  c("BBnspring"),
#                   db.con="PTRAN64",season="spring",survey.year = 2017,testing=T)


#surveys = c("Midspring","Sabspring")
# This runs the survey assuming you have already run the script before and don't need to recreate the raw data from the databases
#res <- survey.data(direct = direct,db.con="ptran64",un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=T,yr=2015)

# Just so you have them are the figure/bank combinations that you can chose from for the figures
#plots = c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial","Survey", user.SH.bins,"seedboxes",
#          "MW-SH","abund-ts","biomass-ts","SHF","clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown),
#banks = c("BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB")
source(paste(direct_fns,"Survey_and_OSAC/Survey_summary_figures_sf.r",sep="")) #Source1

# Did this work... NO :-/  It is related to the loading of the MW-SH data from the previsou year for the MW-SH plot... annoyingly!!

str <- Sys.time()
survey.figs(direct = direct, direct_fns=direct_fns, fig="png",
            yr=2024, 
            banks = c(
                    "BBn",
                      "Ger",
                  "Mid",
              "Sab",
              # "GBa",
              # "GBb",
              "GB"#,
              #"BBs"#,
              #"Ban",
              # "BanIce"
            ),
            s.res="high",
            plots = c(
            #  "Survey",
            # "abund-ts",
            #  "biomass-ts",
            #   "SHF",
            # "user.SH.bins",
            "MW-SH",
            #  "clapper-abund-ts",
            # "clapper-per-ts",
            "SH-MW-CF-ts"#,
            #"breakdown"#,
            #     "PR-spatial",
            #    "Rec-spatial",
            #    "FR-spatial",
            # "CF-spatial",
            #   "MC-spatial",
            #   "Clap-spatial",
            #    "MW-spatial", "SH-spatial",
            # "MW.GP-spatial",
            #   "SH.GP-spatial",
            # "SHF-large",
            # "seedboxes"
              ), 
            bathy=c(10,'c'), 
            sub.area=F, INLA="load", season="testing", nickname="spring2024", layout="portrait")

Sys.time() -str

#mean SH FR:
survey.obj$GBa$model.dat$l.bar[survey.obj$GBa$model.dat$year==year]


year=2018
# load(paste0("C:/Documents/Offshore scallop/Assessment/Data/Survey_data/", year, "/Survey_summary_output/Survey_all_results.Rdata"))
load(paste0("C:/Documents/Offshore scallop/Assessment/Data/Survey_data/", year, "/Survey_summary_output/testing_results_all.Rdata"))
aggregate(com~year +  Strata_ID,FUN = mean,data=surv.Live$`GBa-West`[surv.Live$`GBa-West`$year >= 2017,])
aggregate(com~year +  Strata_ID,FUN = mean,data=surv.Live$`GBa-Central`[surv.Live$`GBa-Central`$year >= 2017,])
aggregate(com~year +  Strata_ID,FUN = mean,data=surv.Live$`GBa-Core`[surv.Live$`GBa-Core`$year >= 2017,])
aggregate(com~year +  Strata_ID,FUN = mean,data=surv.Live$`GBa-East`[surv.Live$`GBa-East`$year >= 2017,])
aggregate(com~year +  Strata_ID,FUN = mean,data=surv.Live$`GBa`[surv.Live$`GBa`$year >= 2017,])

north <- aggregate(com~year +  Strata_ID,FUN = mean,data=surv.Live$`GBa-North`[surv.Live$`GBa-North`$year >= 2017,])
require(reshape2)
north_wide <- dcast(north, Strata_ID~year)
north_wide$rel.increase <- north_wide$`2018`/north_wide$`2017` - 1
north_wide$difference <- north_wide$`2018` - north_wide$`2017`

east <- aggregate(com~year +  Strata_ID,FUN = mean,data=surv.Live$`GBa-East`[surv.Live$`GBa-East`$year >= 2017,])
east_wide <- dcast(east, Strata_ID~year)
east_wide$rel.increase <- east_wide$`2018`/east_wide$`2017` - 1
east_wide$difference <- east_wide$`2018` - east_wide$`2017`


source("C:/Documents/Offshore scallop/Assessment/Assessment_fns/RUNME_Survey_OSAC_Model/Survey_Summary_Word.R")

Survey_Summary_Word(year=2018, reportseason="spring", data="C:/Users/keyserf/Documents/Offshore scallop/Assessment/Data/Survey_data/2018/Survey_summary_output/testing_results.Rdata")
# objects: "bankcheck" df, ntows" df and "highlights" df

#l.bar (mean shell height of FR)
survey.obj$Mid$model.dat$l.bar[nrow(survey.obj$Mid$model.dat)]
survey.obj$Sab$model.dat$l.bar[nrow(survey.obj$Sab$model.dat)]
survey.obj$Ger$model.dat$l.bar[nrow(survey.obj$Ger$model.dat)]
survey.obj$BBs$model.dat$l.bar[nrow(survey.obj$BBs$model.dat)]
survey.obj$BBn$model.dat$l.bar[nrow(survey.obj$BBn$model.dat)]
survey.obj$GBa$model.dat$l.bar[nrow(survey.obj$GBa$model.dat)]
survey.obj$GBb$model.dat$l.bar[nrow(survey.obj$GBb$model.dat)]

# mean(pot.grow$GBa$cur.sh[pot.grow$GBa$year==2014], na.rm=T)

# so I want to grow the 2014 pre-recruits up each year until now. The mode of the 2014 pre-recruits were 30-40mm SH. If they followed the VonB, how big would they be each following year?
Von.B <- read.csv(paste(direct,"Data/Ageing/Von_B_growth_parameters.csv",sep=""))
Von.B <- Von.B[Von.B$Bank == "GBa",c("Linf","K","to")]

preds <- expand.grid(year=2015:2018)
init <- data.frame(min.sh=25, max.sh=55, mode.sh=35, year=2014)
min.sh <- NULL
mode.sh <- NULL
max.sh <- NULL
for (i in 1:length(unique(preds$year))){
  if(i==1) min.sh[i] <- Von.B$Linf*(1-exp(-Von.B$K)) + exp(-Von.B$K) * init$min.sh
  if(i==1) mode.sh[i] <- Von.B$Linf*(1-exp(-Von.B$K)) + exp(-Von.B$K) * init$mode.sh
  if(i==1) max.sh[i] <- Von.B$Linf*(1-exp(-Von.B$K)) + exp(-Von.B$K) * init$max.sh
  
  if(i>1) min.sh[i] <- Von.B$Linf*(1-exp(-Von.B$K)) + exp(-Von.B$K) * min.sh[i-1]
  if(i>1) mode.sh[i] <- Von.B$Linf*(1-exp(-Von.B$K)) + exp(-Von.B$K) * mode.sh[i-1]
  if(i>1) max.sh[i] <- Von.B$Linf*(1-exp(-Von.B$K)) + exp(-Von.B$K) * max.sh[i-1]
}


preds$min.sh <- min.sh 
preds$mode.sh <- mode.sh 
preds$max.sh <- max.sh
preds <- rbind(init, preds) # use these bin ranges for the spatial inla plots below:
preds$min.sh.r <- floor(preds$min.sh/5)*5
preds$mode.sh.r <- floor(preds$mode.sh/5)*5
preds$max.sh.r <- ceiling(preds$max.sh/5)*5
preds$diff.min <-c(preds$min.sh.r[2:5] - preds$min.sh.r[1:4], NA)
preds$diff.mode <-c(preds$mode.sh.r[2:5] - preds$mode.sh.r[1:4], NA)
preds$diff.max <-c(preds$max.sh.r[2:5] - preds$max.sh.r[1:4], NA)
preds$diff <- (preds$diff.min +preds$diff.mode +preds$diff.max)/3

preds$act.min.sh[preds$year==2014] <- 25
preds$act.max.sh[preds$year==2014] <- 55
preds$act.min.sh[preds$year==2015] <- 65
preds$act.max.sh[preds$year==2015] <- 85
preds$act.min.sh[preds$year==2016] <- 80
preds$act.max.sh[preds$year==2016] <- 100
preds$act.min.sh[preds$year==2017] <- 90
preds$act.max.sh[preds$year==2017] <- 110
preds$act.min.sh[preds$year==2018] <- 95
preds$act.max.sh[preds$year==2018] <- 110

min.sh2 <- NULL
max.sh2 <- NULL
for (i in 3:length(unique(preds$year))){
 min.sh2[i] <- Von.B$Linf*(1-exp(-Von.B$K)) + exp(-Von.B$K) * preds$act.min.sh[i-1]
 max.sh2[i] <- Von.B$Linf*(1-exp(-Von.B$K)) + exp(-Von.B$K) * preds$act.max.sh[i-1]
}

preds$min.sh2 <- floor(min.sh2/5)*5
preds$max.sh2 <- ceiling(max.sh2/5)*5

source(paste(direct,"Assessment_fns/Survey_and_OSAC/SurveySummary_data.r",sep="")) #Source1
for(i in 1:nrow(preds)){
  survey.data(direct = direct,un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=F,yr=preds$year[i],
              surveys = c("GBasummer", "GBbsummer"), 
              db.con="ptran",survey.year = preds$year[i],testing=F, season="summer", spatial=F,
              bins=c(preds$act.min.sh[i], preds$act.max.sh[i]))
}

preds$offset <- c(0.15, 0.06, 0.35, 0.10, 0.10)
#preds$offset <- c(0.15, 0.06, 0.35, 0.10, 0.10) # if running for entire bank (keep.full.GB = T)

source(paste(direct,"Assessment_fns/Survey_and_OSAC/Survey_summary_figures.r",sep="")) #Source1
for(i in 5:nrow(preds)){
  survey.figs(direct = direct, fig="screen",
              yr=preds$year[i], 
              banks = c("GBa"),
              s.res="high",
              plots = c("PR-spatial", #"Rec-spatial","FR-spatial", 
                "user.SH.bins"
              ), keep.full.GB = F,# note to keep full GB in the plot, you have to run survey.data for GBb and GBa
              sub.area=F, offset=preds$offset[i],
              colour.bins=c(0,5,10,50,100,200,300,500,700,1000,2000,5000, 1e6), # note, you must put a massive bin, greater than any possible tow estimate
              add.scale=T, INLA="run", contour=T, season="summer")
}

# testing out including exp tows. I just made these by manually typing in the different subareas and renaming the ggtitle and the pdf name... lazy
require(plyr)
survlivedat <- surv.Live$`GBa-East`
means <- ddply(.data=survlivedat, .(year),
               summarize,
               meanpertow = mean(com))
meansrand <- ddply(.data=survlivedat[survlivedat$random==1,], .(year),
                   summarize,
                   meanpertow = mean(com))

require(ggplot2)
pdf(paste0(direct, "/2018/Presentations/Survey_summary/Exploratory_figures/Oct24/GBa subareas with and without exp tows_com.pdf"), onefile=T)
ggplot() + geom_point(data=means, aes(year, meanpertow), colour="red") +
  geom_line(data=means, aes(year, meanpertow), group=1, colour="red")  +
  geom_point(data=meansrand, aes(year, meanpertow), colour="blue") +
  geom_line(data=meansrand, aes(year, meanpertow), group=1, colour="blue") + theme_bw() + theme(panel.grid=element_blank()) +
  ylab("simple mean per tow, not stratified")+
  ggtitle("GBa-East")
dev.off()


spat.name <- unique(spat.names$label)[c(1,2,3,6,7)]
survlivedatbind <- NULL
for (i in 1:length(spat.name)){
  survlivedat <- surv.Live[[spat.name[i]]]
  survlivedat$bank <- spat.name[i]
  survlivedatbind <- rbind(survlivedatbind, survlivedat)
}

require(dplyr)
require(plyr)
survlivedatbind <- select(survlivedatbind, year, bank, pre, rec, com, tot, random)
survlivedatbind <- melt(survlivedatbind, id.vars = c("year", "bank", "random"))
means <- ddply(.data=survlivedatbind, .(year, bank, variable, random),
               summarize,
               meanpertow = mean(value))
means_all <- ddply(.data=survlivedatbind, .(year, variable, random),
                   summarize,
                   meanpertow = mean(value))
means$year <- as.numeric(means$year)
means_all$year <- as.numeric(means_all$year)

require(ggplot2)
pdf(paste0(direct, "/2018/Presentations/Survey_summary/Exploratory_figures/Oct24/GBa subareas exp tows only.pdf"), onefile=T, height=8.5, width=11)
ggplot() + geom_point(data=means[means$random==5,], aes(as.numeric(year), meanpertow)) +
  geom_line(data=means[means$random==5,], aes(as.numeric(year), meanpertow), group=1, lty="dashed")  +
  ylab("simple mean per tow, not stratified\nexploratory tows only")+
  facet_grid(bank~variable) + 
  theme_bw() + theme(panel.grid=element_blank())
dev.off()

levels(means$variable) <- c("Pre-recruits", "Recruits", "Fully-recruited", "Overall")
means$random <- as.factor(means$random)
levels(means$random) <- c(NA, "random", NA, "exploratory")
names(means)[4] <- "Tow Type"

png(paste0(direct, "2018/Presentations/Survey_summary/Exploratory_figures/Oct24/GBa subareas tow type.png"), height=8.5, width=11, units = "in", res=420)
ggplot() + geom_point(data=means[means$`Tow Type`%in% c("random", "exploratory") & means$year>1999 ,], aes(year, meanpertow, colour=`Tow Type`, shape=`Tow Type`)) +
  geom_line(data=means[means$`Tow Type`%in% c("random", "exploratory")& means$year>1999,], aes(year, meanpertow, colour=`Tow Type`, group=`Tow Type`))  + 
  scale_colour_manual(values = c("black", "red"))+
  ylab("scallops per tow (simple mean)")+
  facet_grid(bank~variable) + 
  theme_bw() + theme(panel.grid=element_blank())
dev.off()

levels(means_all$variable) <- c("Pre-recruits", "Recruits", "Fully-recruited", "Overall")
means_all$random <- as.factor(means_all$random)
levels(means_all$random) <- c(NA, "random", NA, "exploratory")
names(means_all)[3] <- "Tow Type"

png(paste0(direct, "2018/Presentations/Survey_summary/Exploratory_figures/Oct24/GBa tow type.png"), height=8.5, width=11, units = "in", res=420)
ggplot() + geom_point(data=means_all[means_all$`Tow Type`%in% c("random", "exploratory") & means_all$year>1999 ,], aes(year, meanpertow, colour=`Tow Type`, shape=`Tow Type`)) +
  geom_line(data=means_all[means_all$`Tow Type`%in% c("random", "exploratory")& means_all$year>1999,], aes(year, meanpertow, colour=`Tow Type`, group=`Tow Type`))  + 
  scale_colour_manual(values = c("black", "red"))+
  ylab("scallops per tow (simple mean)")+
  facet_wrap(~variable) + 
  theme_bw() + theme(panel.grid=element_blank())
dev.off()