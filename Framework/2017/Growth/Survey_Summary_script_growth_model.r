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
direct <- "d:/r/"
# direct <- "Y:/Offshore scallop/Assessment/"
yr <- 2016
# If the data has been created load it here
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
# BBs MW data Alan requested once upon a time.
#write.csv(mw.dat.all$BBs,paste("D:/Alan/Database/BBS_For_alan.csv"))

# The two functions that get the data and produce the survey figures
# This function only needs to be run once to compile all of the data
source(paste(direct,"Assessment_fns/Framework/2017/Growth/SurveySummary_data_growth.r",sep="")) #Source1
# This function is used to pull out the figures of interest.
source(paste(direct,"Assessment_fns/Survey_and_OSAC/Survey_summary_figures.r",sep="")) #Source1

# You only need to run this once to get all the survey data compiled
# This take about 3-4 minutes to run with all the banks included.
# So you have them here are thethe bank/survey combos you can use for survey.data() function. 
#surveys = c("BBnspring" ,"BBsspring" ,"Gerspring", "Midspring", "Sabspring", "GBspring" ,"GBbsummer", "GBasummer")

# This runs the survey assuming that you need to compile the survey data
# Make sure surveys and season match if using spring or summer (both doesn't matter)
res <- survey.data(direct = direct,un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=T,yr=2016,
            surveys =  c("GBasummer"),
            db.con="ptran64",season="both",survey.year = 2016)

#surveys = c("Midspring","Sabspring")
# This runs the survey assuming you have already run the script before and don't need to recreate the raw data from the databases
res <- survey.data(direct = direct,dbcon="ptran64",un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=T,yr=2015)
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.Rdata",sep="")) 

# Just so you have them are the figure/bank combinations that you can chose from for the figures
#plots = c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial","Survey",
#          "MW-SH","abund-ts","biomass-ts","SHF","clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown),
#banks = c("BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB")
# This get creates the survey figures, can only be done once the data has been created using the survey.data() function
# Make sure banks and season match if using spring or summer (both doesn't matter)
#,"BBs", "Ger", "Mid", "Sab", "GB")
survey.figs(direct = direct,fig="file",season="both",yr=2016)

survey.figs(direct = direct,fig="screen",banks = c("GB"),season="both",yr=2016,"abund-ts")

survey.figs(direct = direct,fig="file",yr=2016,banks = c("Sab"),season="spring","CF-spatial")

boxy <- seedbox.obj[["BBn"]][[1]]
survey.ts(boxy$model.dat,min(boxy$model.dat$year,na.rm=T):yr,pdf=F, RS=85, CS=95,Npt=T,
          areas=NULL,ys=.99,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
          add.title = F,titl = "",cx.mn=3,axis.cx = 1.5,type="B")

survey.ts(survey.obj$BBn$model.dat,min(survey.obj$BBn$model.dat$year,na.rm=T):yr,pdf=F, RS=85, CS=95,Npt=F,
          areas=NULL,ys=.99,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
          add.title = F,titl = "",cx.mn=3,axis.cx = 1.5,type="B")