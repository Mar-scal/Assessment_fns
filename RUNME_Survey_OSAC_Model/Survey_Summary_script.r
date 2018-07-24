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
yr <- 2018

# The two functions that get the data and produce the survey figures
# This function only needs to be run once to compile all of the data
source(paste(direct,"Assessment_fns/Survey_and_OSAC/SurveySummary_data.r",sep="")) #Source1
# This function is used to pull out the figures of interest.
source(paste(direct,"Assessment_fns/Survey_and_OSAC/Survey_summary_figures.r",sep="")) #Source1

#source(paste(direct,"Assessment_fns/Survey_and_OSAC/archive/Survey_summary_figures.r",sep="")) #Source1
# Here's the pre-INLA version of this if interested, should work, I hope...
#source("Y:/Offshore scallop/Assessment/Assessment_fns/Survey_and_OSAC/archive/2017/Survey_summary_figures (2).r") #Source1
# You only need to run this once to get all the survey data compiled
# This take about 3-4 minutes to run with all the banks included.
# So you have them here are thethe bank/survey combos you can use for survey.data() function. 
#surveys = c("BBnspring" ,"BBsspring" ,"Gerspring", "Midspring", "Sabspring", "GBspring" ,"GBbsummer", "GBasummer")

# This runs the survey assuming that you need to compile the survey data
# Make sure surveys and season match if using spring or summer (both doesn't matter)
# Leave preprocessed = F (it takes about 1 minute to run) unless you haven't changed any of the other options
# 
res <- survey.data(direct = direct,un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=F,yr=2018,
                   surveys = c("BBnspring" ,"Gerspring", "Midspring", "Sabspring", "GBspring" ,"BBsspring"),
                   db.con="ptran",survey.year = 2018,testing=T)

#res <- survey.data(direct = direct,un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=T,yr=2016,
#                   surveys =  c("BBnspring"),
#                   db.con="PTRAN64",season="both",survey.year = 2016,testing=T)

# Running this to get number to use in a simple model in which the recruit sizes are 60-80 mm and the 
# fully recruited are 80+ mm.
#res <- survey.data(direct = direct,un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=T,yr=2017,
#                   surveys =  c("BBnspring"),
#                   db.con="PTRAN64",season="spring",survey.year = 2017,testing=T)
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))

survey.obj$GBa$Strata.obj
mean(survey.obj$GBa$Strata.obj$N[[34]]$yhi$`7`)

#surveys = c("Midspring","Sabspring")
# This runs the survey assuming you have already run the script before and don't need to recreate the raw data from the databases
#res <- survey.data(direct = direct,db.con="ptran64",un.ID=un.ID,pwd.ID=pwd.ID,preprocessed=T,yr=2015)

# Just so you have them are the figure/bank combinations that you can chose from for the figures
#plots = c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial","Survey", user.SH.bins,"seedboxes",
#          "MW-SH","abund-ts","biomass-ts","SHF","clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown),
#banks = c("BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB")
str <- Sys.time()
survey.figs(direct = direct,fig="png",yr=2018,banks = c("BBn" ,"BBs", "Ger", "Mid", "Sab","GB"),s.res="low",add.scale=T,INLA="run.full",
            contour=T,season="testing",offset = c(0.12, 0.12, 0.12, 0.12, 0.10, 0.35))
Sys.time() -str

str <- Sys.time()
survey.figs(direct = direct,fig="png",yr=2018,banks = c("Mid" ),s.res="high",add.scale=T,INLA="run",contour=T,season="testing",offset = 0.12,
            plots = c("breakdown"))
Sys.time() -str

survey.figs(direct = direct,fig="screen",yr=2017,banks = c("Sab"),season="both",s.res="high",add.scale=T,INLA="load",contour=T,plots = c("Survey"))

survey.figs(direct = direct,fig="png",yr=2017,banks = c("BBn","Ger","Mid"),season="testing",s.res="high",add.scale=T,INLA="load",contour=T)

survey.figs(direct = direct,fig="screen",yr=2017,banks = c("Sab"),season="both",s.res="high",add.scale=T,INLA="run",contour=T,plots = c("FR-spatial"))

