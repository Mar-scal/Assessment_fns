#### DK made Oct 12, 2017.  just the beginnings of looking at the fleet dynamics.  Right now this script simply makes a time series of 
#  each of the fleets on each bank, in terms of catch, effort, and cpue, needs lots of work but does the trip for the moment
#  This also has the Meat counts by fleet and bank as well, got this from the port sampling data for 2017

direct <- "d:/r/"
library(ggplot2)
library(lubridate)
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Fishery/fishery_time_series.r",sep=""))
source(paste(direct,"Assessment_fns/Survey_and_OSAC/OSAC_summary.r",sep = ""))

OSAC_summary(direct = direct,un=un.ID,pw=pwd.ID,db.con="ptran64",yr=2017,
             bank = c("BBn","GBa","GBb"),save.fig = F,save.res=F,export=F)

bbn.cpue.ts <- OSAC_res$cpue.ts$BBn
GBa.cpue.ts <- OSAC_res$cpue.ts$GBa
GBb.cpue.ts <- OSAC_res$cpue.ts$GBb
# First we want to extract the columns of interest from the bbn data
cols <- c(1,grep("catch",names(bbn.cpue.ts)))
catch <- bbn.cpue.ts[,cols]        
cols <- c(1,grep("effort",names(bbn.cpue.ts)))
effort <- bbn.cpue.ts[,cols]
cols <- c(1,5,13,21,grep("LCI",names(bbn.cpue.ts)),grep("UCI",names(bbn.cpue.ts)))    
cpue <- bbn.cpue.ts[,cols]   

dat <- data.frame(year = rep(catch$year,3),fleet = c(rep("Overall",length(catch$year)),rep("WF",length(catch$year)),rep("FT",length(catch$year))),
                  catch = c(catch[,2],catch[,3],catch[,4]),effort = c(effort[,2],effort[,3],effort[,4]),
                  cpue = c(cpue[,2],cpue[,3],cpue[,4]),cpue.LCI = c(cpue[,5],cpue[,6],cpue[,7]),cpue.UCI = c(cpue[,8],cpue[,9],cpue[,10]))      



windows(11,11)
fishery.ts(dat,years = 2000:2017,fleet="all",plots="all")

# Now do the exact same thing for the GBa data... yes I could do this much more nicerly...

cols <- c(1,grep("catch",names(GBa.cpue.ts)))
catch <- GBa.cpue.ts[,cols]        
cols <- c(1,grep("effort",names(GBa.cpue.ts)))
effort <- GBa.cpue.ts[,cols]
cols <- c(1,5,13,21,grep("LCI",names(GBa.cpue.ts)),grep("UCI",names(GBa.cpue.ts)))    
cpue <- GBa.cpue.ts[,cols]   

dat <- data.frame(year = rep(catch$year,3),fleet = c(rep("Overall",length(catch$year)),rep("WF",length(catch$year)),rep("FT",length(catch$year))),
                  catch = c(catch[,2],catch[,3],catch[,4]),effort = c(effort[,2],effort[,3],effort[,4]),
                  cpue = c(cpue[,2],cpue[,3],cpue[,4]),cpue.LCI = c(cpue[,5],cpue[,6],cpue[,7]),cpue.UCI = c(cpue[,8],cpue[,9],cpue[,10]))      


windows(11,11)
fishery.ts(dat,years = 2000:2017,fleet="all",plots="all")

# Now do the exact same thing for the GBb data... yes I could do this much more nicerly...

cols <- c(1,grep("catch",names(GBb.cpue.ts)))
catch <- GBb.cpue.ts[,cols]        
cols <- c(1,grep("effort",names(GBb.cpue.ts)))
effort <- GBb.cpue.ts[,cols]
cols <- c(1,5,13,21,grep("LCI",names(GBb.cpue.ts)),grep("UCI",names(GBb.cpue.ts)))    
cpue <- GBb.cpue.ts[,cols]   

dat <- data.frame(year = rep(catch$year,3),fleet = c(rep("Overall",length(catch$year)),rep("WF",length(catch$year)),rep("FT",length(catch$year))),
                  catch = c(catch[,2],catch[,3],catch[,4]),effort = c(effort[,2],effort[,3],effort[,4]),
                  cpue = c(cpue[,2],cpue[,3],cpue[,4]),cpue.LCI = c(cpue[,5],cpue[,6],cpue[,7]),cpue.UCI = c(cpue[,8],cpue[,9],cpue[,10]))      


windows(11,11)
fishery.ts(dat,years = 2004:2017,fleet="all",plots="all")




#Here are the meat counts for each fleet in 2017 from January to August on GBa, GBb, and BBn

port.sampling <- read.csv(paste(direct,"Data/Framework/2017/Fleet_dynamics/MC_by_trip_2017.csv",sep=""))

port.sampling$month <- month(port.sampling$Date.start,abbr = T,label=F)

trips.by.fleet.bank <- aggregate(Meat_count ~ Type + Bank,port.sampling,length)
bank.by.fleet <- aggregate(Meat_count~ Type + Bank,port.sampling,range)
bank.by.fleet <- aggregate(Meat_count~ Type +Bank,port.sampling,mean)
bank.by.fleet$meat_weight <- 500/bank.by.fleet$Meat_count
mc.by.bank <- aggregate(Meat_count~Bank,port.sampling,mean)
bank.by.company <- aggregate(Meat_count~ Bank+Company,port.sampling,mean)
bank.by.company$meat_weight <- 500/bank.by.company$Meat_count


# Quick plot of the port sampling by date and fleet....
pdf(paste(direct,"2017/Framework/Fleet_dynamics/MC_by_date_2017.pdf",sep=""),width=12,height = 6)
#windows(11,11)
p <- ggplot(port.sampling, aes(month,Meat_count,colour = Type)) + geom_point() +facet_wrap(~Bank,scales="free") + geom_smooth(method = "gam",se=F)+
       scale_color_manual(values = c("black","blue")) +ylab("Meat count")# 
p
dev.off()

# Quick plot of the port sampling by bank and fleet....
pdf(paste(direct,"2017/Framework/Fleet_dynamics/MC_by_bank_2017.pdf",sep=""),width=12,height = 6)
#windows(11,11)
p <- ggplot(port.sampling, aes(Type,Meat_count)) + geom_boxplot() +facet_wrap(~Bank) +ylab("Meat count") 
p
dev.off()


# Quick plot of the port sampling by date and company
pdf(paste(direct,"2017/Framework/Fleet_dynamics/MC_by_company_2017.pdf",sep=""),width=12,height = 6)
#windows(11,11)
p <- ggplot(port.sampling, aes(Company,Meat_count,colour = Type)) + geom_point() +facet_wrap(~Bank,scales="free") +
   ylab("Meat count")# 
p
dev.off()

