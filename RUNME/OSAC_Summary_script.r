#######################################################################################################################################  
############## The script will be used to create summary information for the OSAC presentation                       ######################## 
#######################################################################################################################################  
## Created by DK April 2016
## Update history
## May 16, 2016:  Revised to work with new OSAC_summary function, I tested the function for 1 afternoon... it needs more testing!

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  
##
###############################################################################################################

###############################################################################################################
## This script needs these functions to work (a.k.a. "support files")
# 1: source(paste(direct,"Assessment_fns/Survey/OSAC_summary.r",sep=""))
# 
###############################################################################################################

# Load your directory and the survey year
#direct <- "d:/r/"
direct <- "Y:/Offshore/Assessment/"
direct_fns <- "C:/Users/mcdonaldra/Documents/Github/Assessment_fns/"
yr <- 2023

# Load the function...
source(paste(direct_fns,"Survey_and_OSAC/OSAC_summary.r",sep = ""))

# This will take more than 10 minutes when using GBa as it takes a while to do the jackknife for the CPUE on GBa.
# Note that this will lead to an error if there was no fishing on a bank in a given year so make sure you remove any
# banks that don't have fishery data for the current year!
OSAC_summary(direct = direct,direct_fns=direct_fns,un=un.ID,pw=pwd.ID,db.con="ptran",yr=yr,
             bank ="BBs",
             save.fig = T,save.res=F,export=F, calc.mc = T, rdata.logs=F)

#NEED TO GO CHECK GERMAN, HAS SOME DIFFERENCES IN SPATIAL CATCH, histogram is fine

# OSAC_summary(direct = direct,un=un.ID,pw=pwd.ID,db.con="ptran",yr=2019,
#              bank = c("Sab", "Ban"),
#              save.fig = F,save.res=F,export=F, calc.mc = F, rdata.logs=F)

# GBa <- OSAC_summary(direct = direct,direct_fns=direct_fns,un=un.ID,pw=pwd.ID,db.con="ptran",yr=2019,
#              bank = c("GBa"#"SPB", "Mid","Sab","Ger","BBn","GBa","GBb","BBs"
#                       ),
#              save.fig = F,save.res=F,export=F, calc.mc=T, rdata.logs=F)
# Get rid of some clutter...
# rm("fleet_data","new.log.dat","old.log.dat","slip.dat")
# And the data is all summarized in the OSAC_res object
# fish.res
# meat.count

load(paste(direct,"Data/Fishery_data/Summary/2023/OSAC_summary.RData",sep = ""))
# object names in OSAC_summary.RData and in OSAC_res (if it's immediately after running): fish.res,surv.res,sum.stat,fish.cells,extreme.catch,high.catch, cpue.ts,mctable
names(OSAC_res)

fish.res
surv.res
sum.stat
extreme.catch
high.catch
cpue.ts
meat.count

#mean SH FR is in survey summary script and OSAC_word.RMD
#survey.obj$GBa$model.dat$l.bar[survey.obj$GBa$model.dat$year==year]

length(which(OSAC_res$fish.cells$catch >=1))

# inspect this for weirdness. open the word doc for any weird rows, and make sure it's clean. then re-run until mctable$meatcounts is flawless.
summary(meat.count$meatcounts) # there should be no NA's here anywhere.
meat.count$meatcounts$month <- month(ymd(meat.count$meatcounts$land))
max(ymd(meat.count$meatcounts$land), na.rm=T) # includes everything up to end of July

# if above is clean, then put the following into the ppt. But please pay attention to any really odd values.
meat.count$summarytable
