# Try to take inshore results and make predictions!!

direct <- "d:/r/"


#load(file=paste(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_testing_results.RData",sep=""))
load(file="Y:/INSHORE SCALLOP/BoF/2016/SPA 1A/ModelOutput/SPA1AModel.RData")
load(file="Y:/INSHORE SCALLOP/BoF/2016/SPA6/ModelOutput/GM_Model_2016.RData")
load(file="Y:/INSHORE SCALLOP/BoF/2016/SPA4 and 5/ModelOutput/SPA4Model.RData")

# You'll need to put in your directory here!
source(paste(direct_fns,"Inshore/SSModel_plot_median.r",sep=""))
#source("/SSModel_predict.r")
source(paste(direct_fns,"Inshore/SSModel_predict_summary_median.r",sep=""))

# Load in the data you want
dat <- Spa1A.2016 # Spa6.2016 Spa4.2016
input.dat <- DDspa1A.2016.dat # DDspa6.2016.dat   DDspa4.2016.dat

# Note I also made the recruit Y-axis log scaled, and make you enter the g and gR terms.
windows(11,11)
SSModel.plot.median(dat, ref.pts=c(480,1000), Catch.next.year=200,pred.lim=0.1,
                    g = input.dat$g[length(input.dat$g)], 
                    gR = input.dat$gR[length(input.dat$gR)],log.R=F)

# Use the predict function like usual, this just gets a result for each of the saved MCMC samples
decision<-predict(dat, Catch=c(seq(100,600,20)), g.parm=input.dat$g[length(input.dat$g)],
                  gr.parm=input.dat$gR[length(input.dat$gR)])

# It's the summary where this decision is turned into means rather than medians.
table.2016<- SSModel_predict_summary_median (decision, RRP=0.15)


plot(dat,Catch.next.year=200,pred.lim=0.1)
