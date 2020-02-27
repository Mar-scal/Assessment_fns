################################################################################################################
##### ####################  The fishery summary plots necessary for Pre-OSAC and OSAC ###################################
###########################  November 18, 2015 - DK
################################################################################################################
#Update History
# Revised and commented by DK, April 1, 2016

###############################################################################################################
## This script needs these local functions to work (a.k.a. "support files"), 
# see "Source_relation_OSAC_fishery_summary.docx" for complete details
#  1: fishery.dat.r
#  2: OSAC_fishery_figures.r
#  3: logs_and_fishery_data.r
#  4: ScallopMap.r
###############################################################################################################


############################# LOAD DATA 
################################### START LOAD Data and Functions ############################################
##########################################################################################################

# The year of survey data you want to pull. The Sys.time gives the current year, just subtract from that to get the year of interest
yr = as.numeric(format(Sys.time(), "%Y"))-1  # 
years = 1981:yr # The years of interest 
# Where are you running the script from?
#direct = "Y:/Offshore scallop/Assessment/"
direct = "d:/r/"
#direct = "e:/fn/"
# Load functions and external datafiles we might need
source(paste(direct_fns,"Survey_and_OSAC/OSAC_fishery_figures.r",sep="")) #Source1
source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct_fns,"Fishery/fishery.dat.r",sep=""))
source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))

# The fishery regulations are here
fish.regs <- read.csv(paste(direct,"data/Fishery_regulations_by_bank.csv",sep=""))
fish.regs <- subset(fish.regs,year==yr)
# There are various sources of data you may need to pull here.
# This will load the data from the year/directory combo above if you've run the survey

# Datasource #1:  If you have run all of survey summary (which is necessary to run the scripts in part 2 related to Survey summary)
#                 and you have saved that output you can simply load that data, this is sufficient to run all the analyses
#                 in this document, though the fishery data may not be the latest (see Datasource #3)
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  

# Datasource #2:  You can use this IF YOU HAVE ALREADY RUN  at least SECTION 1 of survey summary and saved the results
#                 This will only run the code in part 1 of this code and may not contain the latest fishery log data.
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_preprocessed.R",sep=""))
#source("d:/r/.Rprofile") # If you happen to need to reload your rprofile...


# Datasource #3: If you need the latest fishery data you will get that here, the survey summary fishery data
#                may not have the latest logs in it so I can't hurt to run this everytime.  This data is sufficient to run
#                the code in part 1 of the document
#Source1, source(paste(direct,"logs_and_fishery_data.r",sep="")) 

# This queries the offshore so gets the most up to date fishery information which we should be sure we are using!
logs_and_fish(loc="offshore",year = 1981:yr,un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)


# If some of the logs have not be through QA/QC then specify the most recent log data you will use.
mx.dt <- as.Date("2015-12-10")		
# Pick the bank(s) The function defaults to all banks so you don't need to specify necessarily, also can plot individual banks.
# If you just want a bank or two use this.
bnk=c("Sab","Ger","BBn","GBa","GBb")
# This selects all banks.
bnk <- as.character(unique(fish.regs$Bank))
#bnk="BBn"

############################# END GENERAL DATA #######################################################
#################  PART I Fishery summary #################### PART I Fishery summary  #################### 
#################  PART I Fishery summary #################### PART I Fishery summary  #################### 
#################  PART I Fishery summary #################### PART I Fishery summary  #################### 
# Now run the function to generate the plots of interest.
# If you want the figures saved as image files make sure "save.fig=T" and also make sure the directories for the current year exist
#  exist.
# This object also contains the catch in each cell for each bank...
bank.spatial <- fishery_figures(fish.dat=fish.dat,bnk=bnk,max.date=mx.dt,dirct=direct,poly.brd="black",
                                years=years,save.fig=F,add.titles = T)
#fishery_figures(fish.dat=fish.dat,bnk=bank,max.date=mx.dt,dirct=direct,years=years,save.fig=F,log.ts=T)

cpue.dat <- bank.spatial[[2]]
bank.spatial <- bank.spatial[[1]]
###################  Fishery Data for OSAC ################################## ###################  Fishery Data for OSAC ##########
###################  Fishery Data for OSAC ################################## ###################  Fishery Data for OSAC ##########
###################  Fishery Data for OSAC ################################## ###################  Fishery Data for OSAC ##########

#########  First up make a table of the fishery catch in each cell on each bank.
########  IF there was no fishing in the current year the output for that bank is garbage in here
res <- NULL
mid <- NULL
#bnk=c("Sab","Ger","BBn","GBa","GBb")

for(i in 1:length(bnk))
  {
    # This only works for the banks we have spatial data for...
    if(is.null(bank.spatial[[bnk[i]]])==F)
      {
        test <- NULL
        for(j in 1:length(bank.spatial[[bnk[i]]][[2]]$PID)) test[[j]] <- subset(bank.spatial[[bnk[i]]][[1]],
                                                                    PID %in% bank.spatial[[bnk[i]]][[2]]$PID[j] & 
                                                                    SID %in% bank.spatial[[bnk[i]]][[2]]$SID[j])
      
        res[[i]] <- do.call("rbind",test)
        mid[[i]] <- cbind(calcCentroid(res[[i]],rollup=3),bank.spatial[[bnk[i]]][[2]]$Z,rep(bnk[i],length(bank.spatial[[bnk[i]]][[2]]$Z)))
      } # end     if(is.null(bank.spatial[[bnk[i]]]==F))
  } # end for(i in 1:length(bnk))


fish.cells <- do.call("rbind",mid)
colnames(fish.cells) <- c("PID","SID","lon","lat","catch","bank")
# Turn catch into tonnage...
fish.cells$catch <- fish.cells$catch/1000
# Now in 2015 Ban, Mid, and BBs did not have any fishery so I can toss those...
#bnk.2015 <-  as.character(unique(fish.regs$Bank))[c(-1,-2,-5)]
#fish.cells <- subset(fish.cells, bank %in% (bnk.2015))
#fish.cells$bank <- as.character((fish.cells$bank))

big.catch <- subset(fish.cells, catch >=50 )
mid.catch <- subset(fish.cells, catch >= 10 & catch < 50)

# Quick check of numbers in each category
dim(subset(mid.catch, bank=="GBb"))
range(subset(mid.catch, bank=="GBb")$catch)
mean(subset(mid.catch, bank=="GBb")$catch)

# Some basic summary stats...
med.catch <- aggregate(catch~bank,fish.cells,median)
men.catch <- aggregate(catch~bank,fish.cells,mean)
rg.catch <- aggregate(catch~bank,fish.cells,range)

# Look at each bank in more detail...
sum.stat <- as.data.frame(matrix(NA,ncol=3,nrow=length(unique(fish.cells$bank))))
colnames(sum.stat) <- c("bank","prop.spatial","prop.of.total")
low.catch <- 1.5
high.catch <- 10
for(i in 1:length(unique(fish.cells$bank)))
  {
  catch <- subset(fish.cells, bank == unique(fish.cells$bank)[i])
  # for the lousy banks 1 tonne is a good catch, you might need to change these a bit each year, but for 2015 it would be...
  if(unique(fish.cells$bank)[i] %in% c("Ban","Mid","Sab","Ger","BBS"))
  {
  sum.stat$bank[i] <- as.character(unique(fish.cells$bank)[i])
  # Proportion of the cells with catch that had a catch > 1 tonne
  sum.stat$prop.spatial[i] <- length(catch$catch[catch$catch>low.catch])/nrow(catch)
  # Proportion of the total catch caught in these high catch areas.
  sum.stat$prop.of.total[i] <- sum(catch$catch[catch$catch>low.catch])/sum(catch$catch)
  } # end if(unique(fish.cells$bank)[i] %in% c("Ban","Mid","Sab","Ger","BBS"))
  
  # for the good banks 10 tonne is a good catch, you might need to change these a bit each year, but for 2015 it would be...
  if(unique(fish.cells$bank)[i] %in% c("BBn","GBa","GBb"))
  {
    sum.stat$bank[i] <- as.character(unique(fish.cells$bank)[i])
    # Proportion of the cells with catch that had a catch > 1 tonne
    sum.stat$prop.spatial[i] <- length(catch$catch[catch$catch>high.catch])/nrow(catch)
    # Proportion of the total catch caught in these high catch areas.
    sum.stat$prop.of.total[i] <- sum(catch$catch[catch$catch>high.catch])/sum(catch$catch)
  } # end if(unique(fish.cells$bank)[i] %in% c("BBn","GBa","GBb"))
  
} # end for(i in 1:length(unique(fish.cells$bank)))
sum.stat <- cbind(sum.stat,med.catch$catch,men.catch$catch,rg.catch$catch)
names(sum.stat) <- c("Bank","prop.spatial","prop.of.total","median.catch","mean.catch","min.catch","max.catch")



# Now moving along we can also include summary stats of interesting areas...
# First we can generate interesting fishery related stats for each bank in the most recently fished year.
#bnk=c("GBa")
#bnk <- as.character(unique(fish.regs$Bank))
fishery.results <- NULL
for(i in 1:length(bnk))
  {
  # Now subset the fishery data if we need to remove some data (usually b/c recent data is not QA/QC ready) ...
bank.fish <- subset(fish.dat, bank == bnk[i] & date < mx.dt)

cpue.ts    <-fishery.dat(bank.fish,bk=bnk[i],yr=years,method='jackknife',direct=direct) 			
cpue.ts.WF <-fishery.dat(subset(bank.fish,fleet=="WF"),bk=bnk[i],yr=years,method='jackknife',direct=direct) 			
cpue.ts.FT <-fishery.dat(subset(bank.fish,fleet=="FT"),bk=bnk[i],yr=years,method='jackknife',direct=direct) 			

effort <- c(cpue.ts$effort[cpue.ts$year == max(cpue.ts$year)],
            ifelse(max(cpue.ts$year) %in% cpue.ts.WF$year, cpue.ts.WF$effort[cpue.ts.WF$year == max(cpue.ts$year)],NA),
            ifelse(max(cpue.ts$year) %in% cpue.ts.FT$year, cpue.ts.FT$effort[cpue.ts.FT$year == max(cpue.ts$year)],NA)) 
catch  <- c(cpue.ts$catch[cpue.ts$year == max(cpue.ts$year)],
            ifelse(max(cpue.ts$year) %in% cpue.ts.WF$year, cpue.ts.WF$catch[cpue.ts.WF$year == max(cpue.ts$year)],NA),
            ifelse(max(cpue.ts$year) %in% cpue.ts.FT$year, cpue.ts.FT$catch[cpue.ts.FT$year == max(cpue.ts$year)],NA)) 
cpue   <- c(cpue.ts$cpue[cpue.ts$year == max(cpue.ts$year)],
            ifelse(max(cpue.ts$year) %in% cpue.ts.WF$year, cpue.ts.WF$cpue[cpue.ts.WF$year == max(cpue.ts$year)],NA),
            ifelse(max(cpue.ts$year) %in% cpue.ts.FT$year, cpue.ts.FT$cpue[cpue.ts.FT$year == max(cpue.ts$year)],NA)) 

bank.fish.latest <- subset(bank.fish, year==max(bank.fish$year,na.rm=T))

WF.trips <- length(which(tapply(bank.fish.latest$fleet,bank.fish.latest$tripnum,unique)=="WF"))
FT.trips <- length(which(tapply(bank.fish.latest$fleet,bank.fish.latest$tripnum,unique)=="FT"))

fishery.results[[i]] <- c(bnk[i],max(bank.fish$year,na.rm=T),effort,catch,cpue,WF.trips,FT.trips)

  } # end for(i in 1:length(bank))

fish.res <- do.call("rbind",fishery.results)
fish.res <- as.data.frame((fish.res))
colnames(fish.res) <- c("bank","year","effort","WF.effort","FT.effort","catch","WF.catch","FT.catch",
                        "cpue","WF.cpue","FT.cpue","WF.trips","FT.trips")
# Turn the factors numbers... you'll need the cool function
factor.2.number <- function(x) {as.numeric(levels(x))[x]}
fish.res[,2:13] <- sapply(fish.res[,2:13],factor.2.number)


# Output the results of interest
write.csv(sum.stat,file=paste(direct,"Data/Fishery_data/Summary/",yr,"/Fishery_summary_stats.csv",sep=""))
write.csv(fish.cells,file=paste(direct,"Data/Fishery_data/Summary/",yr,"/Fishery_cell_by_cell.csv",sep=""))
write.csv(fish.res,file=paste(direct,"Data/Fishery_data/Summary/",yr,"/Fishery_by_bank_and_fleet.csv",sep=""))
save.image(file =paste(direct,"Data/Fishery_data/Summary/",yr,"/OSAC_summary.RData",sep=""))



###################  Survey Data for OSAC ##################################
###################  Survey Data for OSAC ##################################
###################  Survey Data for OSAC ##################################
###################  Survey Data for OSAC ##################################
#load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.R",sep=""))

# You can't do this section for 2015 until the survey summary has been run and all the results are saved.
# Here we are grabbing the mean survey data on the bank, this is being pulled from the detailed sampling information
# DK Note, notice that we will need to clean up MidCF and SabCF names here

# Create the data frame
surv.res <- data.frame(bank = rep(NA,length(bnk)),sh = rep(NA,length(bnk)),cf = rep(NA,length(bnk)),mc = rep(NA,length(bnk)))
# Run this for each bank
for(i in 1:length(bnk))
{
# Name the bank
surv.res$bank[i] <- bnk[i]
# Note that we don't have any information for Banquereau (the first bank in the list) so I skip it 
if(i > 1) 
{  
# For the rest of the banks calculate the shell height and meat counts  
surv.res$sh[i] <- with(CF.current[[bnk[i]]],weighted.mean(l.bar,com,na.rm=T))
surv.res$mc[i] <- with(CF.current[[bnk[i]]],weighted.mean(meat.count,com,na.rm=T))
# For German Bank we are looking for CF2 as it is split between lined/unlinded tows...
if (surv.res$bank[i] != "Ger") surv.res$cf[i] <-  cf.data[[bnk[i]]]$CFyrs$CF[length(cf.data[[bnk[i]]]$CFyrs$CF)] 
if (surv.res$bank[i] == "Ger") surv.res$cf[i] <-  cf.data[[bnk[i]]]$CFyrs$CF2[length(cf.data[[bnk[i]]]$CFyrs$CF2)] 
} # end if(i > 1)

} # end for(i in 1:length(bnk))
   


##############
#############
###########
##############  OSAC Mini survey figs (for the top corner of presentation)  ###################
##############  This is similar to what we'd show in the survey summaries but optimized slightly for this presentation
##############  And includes all banks, not just where we surveyed this year.
bnk <- c(as.character(unique(fish.regs$Bank)),"GB") # Strictly speaking we don't need GB in there...

# Make a spatial survey summary figure for each bank of interest.
for(j in 1:length(bnk))
{
  # The directory for the figures, wouldn't hurt to make this into an "option" when converting to a function so that
  # the figures could be output to the screen and saved wherever you'd like
  # Note that the directory needs to exist for this to work!!!
  png(paste(direct,yr,"/Presentations/OSAC/",bnk[j],"/Survey_strata.png",sep=""),units="in",width = 11, height = 8.5,
      res=420)

  # Grab the survey strata if available  
  surv.dets <- subset(survey.detail.polys,label==bnk[j])
  surv.info <- subset(survey.info,label==bnk[j])
  # If no survey strata map scallop map like so...
  if(nrow(surv.dets)==0)
    {
      ScallopMap(area=bnk[j],plot.bathy=T,bathy.source = "quick",boundries = "offshore",shore="nwatlHR",
              bound.color = T,plot.boundries = T,label.boundries = F,offshore.names=F,xlab="",ylab="",bathcol = "darkblue",
              title="",cex.mn=2,dec.deg = F,direct=direct,manage.colors = pastel.colors(60,seed=2))
    } # end if(nrow(surv.dets)==0)
  # If there is survey strata we do this...
  if(nrow(surv.dets)>0)
    {
      ScallopMap(area=bnk[j],plot.bathy=T,bathy.source = "quick",boundries = "offshore",shore="nwatlHR",
                 poly.lst=list(surv.dets,surv.info),
                 bound.color = F,plot.boundries = T,label.boundries = F,offshore.names=F,xlab="",ylab="",bathcol = "darkblue",
                 title="",cex.mn=2,dec.deg = F,direct=direct,manage.colors = pastel.colors(60,seed=2))
    } # end if(nrow(surv.dets)==0)
  
# This excludes banks (i.e. Banqueareau..) for which we don't have surv.Live, really is a temporary fix until we have Banquereau
# included in surv.Live which will be needed whenever we next sample Banqueareau.
  if(is.null(surv.Live[[bnk[j]]])==F)
   {
   if(bnk[j] != c("Ger"))
    {
      # Add the regular survey tows.
      points(lat~lon,subset(surv.Live[[bnk[j]]],year== max(surv.Live[[bnk[j]]]$year) & random==1),pch=20,bg='black',cex=1.5)
      # Add other survey tows
      points(lat~lon,surv.Live[[bnk[j]]],subset=year==max(surv.Live[[bnk[j]]]$year) & random %in% c(2,3,4,5),pch=24,bg="darkorange",cex=0.8)
      # Add the legend
      legend('topleft',legend=
            c(paste('regular (n =',
               length(unique(subset(surv.Live[[bnk[j]]],year==max(surv.Live[[bnk[j]]]$year) & random==1)$tow)),")", sep=""),
              paste('exploratory (n =',
               length(unique(subset(surv.Live[[bnk[j]]],year==max(surv.Live[[bnk[j]]]$year) & random %in% c(2,3,4,5))$tow)),")", sep="")),
             pch=c(20,24), pt.bg = c("black","darkorange"),bty='n',cex=1, inset = .02)
    } # end if(bnk[j] != c("Ger"))
    if(bnk[j] == c("Ger"))
      {
        # Add the regular survey tows.
       points(lat~lon,subset(surv.Live[[bnk[j]]],year== max(surv.Live[[bnk[j]]]$year) & random==1),pch=20,bg='black',cex=1.5)
       # Add the Matched tows
       points(lat~lon,subset(surv.Live[[bnk[j]]],year== max(surv.Live[[bnk[j]]]$year) & random==3),pch=22,bg="yellow",cex=1.5)
         # Add the legend
         legend('topleft',legend=
                c(paste('regular (n =',
                    length(unique(subset(surv.Live[[bnk[j]]],year==max(surv.Live[[bnk[j]]]$year) & random==1)$tow)),")", sep=""),
                  paste('repeated (n =',
                    length(unique(subset(surv.Live[[bnk[j]]],year==max(surv.Live[[bnk[j]]]$year) & random==3)$tow)),")", sep="")),
                pch=c(20,22), pt.bg = c("black","yellow"),bty='n',cex=1, inset = .02)
      }# end if(bnk[j] == c("Ger"))
   }# end is.null(surv.Live[[bnk[j]]]==F
   # For any banks in which we don't have surv.Live data (these would be banks we sample infrequently, i.e. Banqureau!!)
   if(is.null(surv.Live[[bnk[j]]])==T)
    {
     # Need to grab the data (this is what pops out directly from the SQL database data)
     dat <- subset(SurvDB$SHF, bank == bnk[j])
     # This makes sure that ALL the data have the lat/long calculated in the same way
     dat$lon<-with(dat,apply(cbind(elon,slon),1,mean))
     dat$lat<-with(dat,apply(cbind(elat,slat),1,mean))
     # Plot the random and non-random tows, non-random are commented out as there probably will never be any in one of these banks...
     points(lat~lon,subset(dat,year == max(dat$year) & random==1),pch=20,bg='black',cex=1.5)
     #points(lat~lon,dat,subset(dat,year== max(dat$year) & random %in% c(2,3,4,5)),pch=24,bg="darkorange",cex=0.8)
     # Add the legend
     legend("topright",pch=c(20), pt.bg = c("black"), title="Tow type",
            legend = paste('regular (n =',
                           length(unique(subset(dat,year==max(dat$year) & random==1)$tow)),")",sep=""),
            inset=0.01,bg='white',box.col='white')
    } # end if(is.null(surv.Live[[bnk[j]]])==T)
  dev.off()
} # end for(j in 1:length(bnk))








