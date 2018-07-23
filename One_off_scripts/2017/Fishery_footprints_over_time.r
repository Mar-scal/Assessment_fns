################################################################################################################
##### ####################  This script is used to get teh fishery footprint over time and to see how it changes.
#####  Created by DK:  Sept 2017

direct <- "D:/r/"
yr <- 2017
years.t = 1981:yr # The years of interest make temporary as this could be overwritten when I bring in the survey results...

# Load functions and external datafiles we might need
library(ggplot2)
source(paste(direct,"Assessment_fns/Survey_and_OSAC/OSAC_fishery_figures.r",sep="")) #Source1
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Fishery/fishery.dat.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))

# Read1 The fishery regulations are located here.
fish.regs <- read.csv(paste(direct,"data/Fishery_regulations_by_bank.csv",sep=""))
fish.regs <- subset(fish.regs,year==yr)
# Read2 Get the survey boundary polygons for all banks.
#survey.bound.polys<-read.csv(paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
#                            header=T,stringsAsFactors = F)
# There are various sources of data you may need to pull here.
# This will load the data from the year/directory combo above if you've run the survey

# Datasource #1:  If you have run all of survey summary (which is necessary to run the scripts in part 2 related to Survey summary)
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
# So we update the fishery data with the lastest for this analysis in case the above is out of date.
# This queries the offshore so gets the most up to date fishery information which we should be sure we are using!
logs_and_fish(loc="offshore",year = 1981:yr,un=un,pw=pwd,db.con=db.con,direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

############################# END GENERAL DATA #######################################################

bnk <- "GBa" # Pick the bank you are interested in.
years <- years.t
yr <- max(years)
bnk.survey.bound.poly <- subset(survey.bound.polys,label==bnk)
lvls <-  c(10,50,100,500,1000,5000,10000,50000)   # Set the levels, not important for the moment...
lvls.eff <- c(10,50,100,1000)
grids = 1/60 # 1/60 = 1 minute grids
fish.cells <- NULL
eff.cells <- NULL
wf.fish.cells <- NULL
wf.eff.cells <- NULL
ft.fish.cells <- NULL
ft.eff.cells <- NULL
for(k in 1:length(years))
{
  bnk.fish.dat <- subset(fish.dat, bank== bnk & date >= as.Date(paste(years[k],"-01-01",sep="")) & date < as.Date(paste(years[k]+1,"-01-01",sep="")),
                         select = c('ID','lon','lat','pro.repwt'))
  bnk.effort.dat <- subset(fish.dat, bank== bnk & date >= as.Date(paste(years[k],"-01-01",sep="")) & date < as.Date(paste(years[k]+1,"-01-01",sep="")),
                         select = c('ID','lon','lat','hm'))
  wf.fish.dat <- subset(fish.dat, bank== bnk & date >= as.Date(paste(years[k],"-01-01",sep="")) & 
                          date < as.Date(paste(years[k]+1,"-01-01",sep="")) & fleet == "WF",select = c('ID','lon','lat','pro.repwt'))
  ft.fish.dat <- subset(fish.dat, bank== bnk & date >= as.Date(paste(years[k],"-01-01",sep="")) & 
                          date < as.Date(paste(years[k]+1,"-01-01",sep="")) & fleet == "FT",select = c('ID','lon','lat','pro.repwt'))
  wf.effort.dat <- subset(fish.dat, bank== bnk & date >= as.Date(paste(years[k],"-01-01",sep="")) & 
                          date < as.Date(paste(years[k]+1,"-01-01",sep="")) & fleet == "WF",select = c('ID','lon','lat','hm'))
  ft.effort.dat <- subset(fish.dat, bank== bnk & date >= as.Date(paste(years[k],"-01-01",sep="")) & 
                          date < as.Date(paste(years[k]+1,"-01-01",sep="")) & fleet == "FT",select = c('ID','lon','lat','hm'))
  
  names(bnk.fish.dat)[1:4]<-c("EID","X","Y","Z")
  names(bnk.effort.dat)[1:4]<-c("EID","X","Y","Z")
  names(wf.fish.dat)[1:4]<-c("EID","X","Y","Z")
  names(wf.effort.dat)[1:4]<-c("EID","X","Y","Z")
  names(ft.fish.dat)[1:4]<-c("EID","X","Y","Z")
  names(ft.effort.dat)[1:4]<-c("EID","X","Y","Z")
  
  # Remove any NA's from the data and give a warning
  if(any(is.na(bnk.fish.dat)==T)) 
  {
    message(paste("Heads up", bnk[i], "has NA's in it, check your data!! I will remove and continue calculations for now"))
    bnk.fish.dat <- na.omit(bnk.fish.dat)
    bnk.effort.dat <- na.omit(bnk.effort.dat)
    ft.fish.dat <- na.omit(ft.fish.dat)
    ft.effort.dat <- na.omit(ft.effort.dat)
    wf.fish.dat <- na.omit(wf.fish.dat)
    wf.effort.dat <- na.omit(wf.effort.dat)
    
  } # if(any(is.na(bnk.fish.dat)==T)) 
  
  
  #Get the total removals from each 1 minute cell if there was a fishery in a given year...
  if(nrow(bnk.fish.dat) > 0)
  {
    bank.spatial <- gridPlot(bnk.fish.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
    eff.spatial <- gridPlot(bnk.effort.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
    
    
    #########  Now make a summary of the  fishery catch in each cell on each bank and for the WF and FT fleets.
    test <- NULL
    test.eff <- NULL
    for(j in 1:length(bank.spatial[[2]]$PID)) test[[j]] <- subset(bank.spatial[[1]],PID %in% bank.spatial[[2]]$PID[j] & SID %in% bank.spatial[[2]]$SID[j])
    for(j in 1:length(eff.spatial[[2]]$PID))  test.eff[[j]] <- subset(eff.spatial[[1]],PID %in% eff.spatial[[2]]$PID[j] & SID %in% eff.spatial[[2]]$SID[j])
    
    # Pull the data from list into a dataframe
    res <- do.call("rbind",test)
    res.eff <- do.call("rbind",test.eff)
    # Then punch that into a list with some other info included.
    fish.cells[[as.character(years[k])]] <- cbind(calcCentroid(res,rollup=3),bank.spatial[[2]]$Z,
                                                  rep(bnk,length(bank.spatial[[2]]$Z)),rep(years[k],length(bank.spatial[[2]]$Z)))
    eff.cells[[as.character(years[k])]] <- cbind(calcCentroid(res.eff,rollup=3),eff.spatial[[2]]$Z,
                                                 rep(bnk,length(eff.spatial[[2]]$Z)),rep(years[k],length(eff.spatial[[2]]$Z)))
  } # if(nrow(bnk.fish.dat) > 0)
  
  # Wet fishery...
  if(nrow(wf.fish.dat) > 0)
  {
    wf.bank.spatial <- gridPlot(wf.fish.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
    wf.eff.spatial <- gridPlot(wf.effort.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)

    wf.test <- NULL
    wf.test.eff <- NULL
    for(j in 1:length(wf.bank.spatial[[2]]$PID)) wf.test[[j]] <- subset(wf.bank.spatial[[1]],PID %in% wf.bank.spatial[[2]]$PID[j] & 
                                                                          SID %in% wf.bank.spatial[[2]]$SID[j])
    for(j in 1:length(wf.eff.spatial[[2]]$PID))  wf.test.eff[[j]] <- subset(wf.eff.spatial[[1]],PID %in% wf.eff.spatial[[2]]$PID[j] & 
                                                                              SID %in% wf.eff.spatial[[2]]$SID[j])
    wf.res <- do.call("rbind",wf.test)
    wf.res.eff <- do.call("rbind",wf.test.eff)
    wf.fish.cells[[as.character(years[k])]] <- cbind(calcCentroid(wf.res,rollup=3),wf.bank.spatial[[2]]$Z,
                                                     rep(bnk,length(wf.bank.spatial[[2]]$Z)),rep(years[k],length(wf.bank.spatial[[2]]$Z)))
    wf.eff.cells[[as.character(years[k])]] <- cbind(calcCentroid(wf.res.eff,rollup=3),wf.eff.spatial[[2]]$Z,
                                                    rep(bnk,length(wf.eff.spatial[[2]]$Z)),rep(years[k],length(wf.eff.spatial[[2]]$Z)))
  } # end if(nrow(wf.fish.dat) > 0)
  
    # Freezers......
  if(nrow(ft.fish.dat) > 0)
  {
    ft.bank.spatial <- gridPlot(ft.fish.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
    ft.eff.spatial <- gridPlot(ft.effort.dat,bnk.survey.bound.poly,lvls,FUN=sum,grid.size=grids)
    ft.test <- NULL
    ft.test.eff <- NULL
    for(j in 1:length(ft.bank.spatial[[2]]$PID)) ft.test[[j]] <- subset(ft.bank.spatial[[1]],PID %in% ft.bank.spatial[[2]]$PID[j] & 
                                                                          SID %in% ft.bank.spatial[[2]]$SID[j])
    for(j in 1:length(ft.eff.spatial[[2]]$PID))  ft.test.eff[[j]] <- subset(ft.eff.spatial[[1]],PID %in% ft.eff.spatial[[2]]$PID[j] & 
                                                                              SID %in% ft.eff.spatial[[2]]$SID[j])
    
    ft.res <- do.call("rbind",ft.test)
    ft.res.eff <- do.call("rbind",ft.test.eff)
    ft.fish.cells[[as.character(years[k])]] <- cbind(calcCentroid(ft.res,rollup=3),ft.bank.spatial[[2]]$Z,
                                                  rep(bnk,length(ft.bank.spatial[[2]]$Z)),rep(years[k],length(ft.bank.spatial[[2]]$Z)))
    ft.eff.cells[[as.character(years[k])]] <- cbind(calcCentroid(ft.res.eff,rollup=3),ft.eff.spatial[[2]]$Z,
                                                 rep(bnk,length(ft.eff.spatial[[2]]$Z)),rep(years[k],length(ft.eff.spatial[[2]]$Z)))
  } # end if(nrow(bnk.fish.dat) > 0)

} # end for(k in 1:length(years))


# And unwrap all this crap into something useful..
spatial.fishery.history <- do.call("rbind",fish.cells)
spatial.effort.history <-  do.call("rbind",eff.cells)
spatial.ft.fishery.history <- do.call("rbind",ft.fish.cells)
spatial.ft.effort.history <-  do.call("rbind",ft.eff.cells)
spatial.wf.fishery.history <- do.call("rbind",wf.fish.cells)
spatial.wf.effort.history <-  do.call("rbind",wf.eff.cells)


colnames(spatial.fishery.history) <- c("PID","SID","lon","lat","catch","bank","year")
colnames(spatial.effort.history) <- c("PID","SID","lon","lat","effort","bank","year")
colnames(spatial.ft.fishery.history) <- c("PID","SID","lon","lat","catch","bank","year")
colnames(spatial.ft.effort.history) <- c("PID","SID","lon","lat","effort","bank","year")
colnames(spatial.wf.fishery.history) <- c("PID","SID","lon","lat","catch","bank","year")
colnames(spatial.wf.effort.history) <- c("PID","SID","lon","lat","effort","bank","year")


# Turn catch into tonnage...
spatial.fishery.history$catch <- spatial.fishery.history$catch/1000
spatial.ft.fishery.history$catch <- spatial.ft.fishery.history$catch/1000
spatial.wf.fishery.history$catch <- spatial.wf.fishery.history$catch/1000


## Now create some annual data summaries
# Quick check that I'm getting what I want...
tot.catch <- aggregate(catch~year,data = spatial.fishery.history,FUN=sum)
tot.effort <- aggregate(effort~year,data = spatial.effort.history,FUN=sum)
tot.ft.catch <- aggregate(catch~year,data = spatial.ft.fishery.history,FUN=sum)
tot.ft.effort <- aggregate(effort~year,data = spatial.ft.effort.history,FUN=sum)
tot.wf.catch <- aggregate(catch~year,data = spatial.wf.fishery.history,FUN=sum)
tot.wf.effort <- aggregate(effort~year,data = spatial.wf.effort.history,FUN=sum)

# Now how many cells were fished each year...
cells.fished <- aggregate(catch~year,data = spatial.fishery.history,FUN=length)
cells.ft.fished <- aggregate(catch~year,data = spatial.ft.fishery.history,FUN=length)
cells.wf.fished <- aggregate(catch~year,data = spatial.wf.fishery.history,FUN=length)
names(cells.fished) <- c("year","cells_fished")
names(cells.ft.fished) <- c("year","cells_fished")
names(cells.wf.fished) <- c("year","cells_fished")
# considering any cells in which more than 10 tonnes was caught
high.catch <- subset(spatial.fishery.history, catch >= 10)
high.ft.catch <- subset(spatial.ft.fishery.history, catch >= 10)
high.wf.catch <- subset(spatial.wf.fishery.history, catch >= 10)
# The number of high catch cells in a year.
n.high <- aggregate(catch~ year,high.catch,length)
ft.n.high <- aggregate(catch~ year,high.ft.catch,length)
wf.n.high <- aggregate(catch~ year,high.wf.catch,length)
# Get the proportion of cells that experienced high catches...
cells.fished$prop_high <- n.high$catch/cells.fished$cells_fished
cells.ft.fished$prop_high <- ft.n.high$catch/cells.ft.fished$cells_fished
cells.wf.fished$prop_high <- wf.n.high$catch/cells.wf.fished$cells_fished
# Some basic summary stats...
med.catch <- aggregate(catch~year,spatial.fishery.history,median)
med.effort <- aggregate(effort~year,spatial.effort.history,median)
med.ft.catch <- aggregate(catch~year,spatial.ft.fishery.history,median)
med.ft.effort <- aggregate(effort~year,spatial.ft.effort.history,median)
med.wf.catch <- aggregate(catch~year,spatial.wf.fishery.history,median)
med.wf.effort <- aggregate(effort~year,spatial.wf.effort.history,median)



## Now take a summary of these summaries...
annual.summary <- cbind(tot.catch,tot.effort$effort,cells.fished$cells_fished,cells.fished$prop_high,med.catch$catch,med.effort$effort,
                        tot.wf.effort$effort,cells.wf.fished$cells_fished,cells.wf.fished$prop_high,med.wf.catch$catch,med.wf.effort$effort)
                        
names(annual.summary) <- c("year","tot_catch","tot_effort","cells_fished","prop_high","median_catch","median_effort",
                                 ,"ft_tot_catch","ft_tot_effort","ft_cells_fished","ft_prop_high","ft_median_catch","ft_median_effort",
                                  ,"wf_tot_catch","wf_tot_effort","wf_cells_fished","wf_prop_high","wf_median_catch","wf_median_effort")

# This produces a plot of the median effort in cells that are fished in a year.  
p <- ggplot(annual.summary, aes(year,median_effort)) + geom_smooth() + geom_point()
p

# This produces a plot of the median catch within each cell over time...
p <- ggplot(annual.summary, aes(year,median_catch)) + geom_smooth() + geom_point()
p

#This produces a plot of the median catch vs median effort, 
p <- ggplot(annual.summary, aes(median_effort,median_catch,label = year)) + geom_smooth() + geom_text(size=3)
p

# This produces a plot of the number of cells fished over time.
p <- ggplot(annual.summary,aes(year,cells_fished)) + geom_smooth() + geom_point()
p

# This produces a plot of the number of cells fished versus the total catch
p <- ggplot(annual.summary,aes(tot_catch,cells_fished,label = year)) + geom_smooth() + geom_text(size=3)
p

# This produces a plot of the number of cells fished versus the total effort
p <- ggplot(annual.summary,aes(tot_effort,cells_fished,label = year)) + geom_smooth() + geom_text(size=3)
p


# This produces a plot of the number of cells fished versus the median catch
p <- ggplot(annual.summary,aes(median_catch,cells_fished,label = year)) + geom_smooth() + geom_text(size=3)
p

# This produces a plot of the number of cells fished versus the median effort
p <- ggplot(annual.summary,aes(median_effort,cells_fished,label = year)) + geom_smooth() + geom_text(size=3)
p

# Look at each bank in more detail...
sum.stat <- as.data.frame(matrix(NA,ncol=3,nrow=length(unique(spatial.fishery.history$bank))))
colnames(sum.stat) <- c("bank","prop.spatial","prop.of.total")

for(i in 1:length(unique(fish.cells$bank)))
  {
  catch <- subset(fish.cells, bank == unique(fish.cells$bank)[i])
  # for the lousy banks 1 tonne is a good catch, you might need to change these a bit each year, but for 2015 it would be...
  if(unique(fish.cells$bank)[i] %in% low.banks)
  {
  sum.stat$bank[i] <- as.character(unique(fish.cells$bank)[i])
  # Proportion of the cells with catch that had a catch > low
  sum.stat$prop.spatial[i] <- length(catch$catch[catch$catch>low])/nrow(catch)
  # Proportion of the total catch caught in these low catch areas.
  sum.stat$prop.of.total[i] <- sum(catch$catch[catch$catch>low])/sum(catch$catch)
  } # end if(unique(fish.cells$bank)[i] %in% c("Ban","Mid","Sab","Ger","BBS"))
  
  # for the good banks 10 tonne is a good catch, you might need to change these a bit each year, but for 2015 it would be...
  if(unique(fish.cells$bank)[i] %in% high.banks)
  {
    sum.stat$bank[i] <- as.character(unique(fish.cells$bank)[i])
    # Proportion of the cells with catch that had a catch > high
    sum.stat$prop.spatial[i] <- length(catch$catch[catch$catch>high])/nrow(catch)
    # Proportion of the total catch caught in these high catch areas.
    sum.stat$prop.of.total[i] <- sum(catch$catch[catch$catch>high])/sum(catch$catch)
  } # end if(unique(fish.cells$bank)[i] %in% c("BBn","GBa","GBb"))
  
} # end for(i in 1:length(unique(fish.cells$bank)))
sum.stat <- cbind(sum.stat,med.catch$catch,men.catch$catch,rg.catch$catch)
names(sum.stat) <- c("Bank","prop.spatial","prop.of.total","median.catch","mean.catch","min.catch","max.catch")
} # end if(is.null(mid) == F)

# If we don't have any spatial data set sum.stat, fish.cells, extreme.catch, and high.catch to null
if(is.null(mid) == T) { sum.stat <- NULL ; fish.cells <- NULL ;extreme.catch <- NULL; high.catch <- NULL }


# Now moving along we can also include summary stats of interesting areas...
# First we can generate interesting fishery related stats for each bank in the most recently fished year.
#bnk=c("GBa")
#bnk <- as.character(unique(fish.regs$Bank))
fishery.results <- NULL
for(i in 1:length(bnk))
  {
  # Now subset the fishery data if we need to remove some data (usually b/c recent data is not QA/QC ready) ...
bank.fish <- subset(fish.dat, bank == "GBa")

cpue.ts.WF <-fishery.dat(subset(bank.fish,fleet=="WF"),bk="GBa",yr=years,method='simple',direct=direct) 			

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
if(export==T)
{
  write.csv(sum.stat,file=paste(direct,"Data/Fishery_data/Summary/",yr,"/Fishery_summary_stats.csv",sep="")) # Write1
  write.csv(fish.cells,file=paste(direct,"Data/Fishery_data/Summary/",yr,"/Fishery_cell_by_cell.csv",sep="")) # Write2
  write.csv(fish.res,file=paste(direct,"Data/Fishery_data/Summary/",yr,"/Fishery_by_bank_and_fleet.csv",sep="")) # Write3
} # end if(export==T)




###################  Survey Data for OSAC ##################################
# Here we are grabbing the mean survey data on the bank, this is being pulled from the detailed sampling information

# Create the data frame
surv.res <- data.frame(bank = rep(NA,length(bnk)),sh = rep(NA,length(bnk)),cf = rep(NA,length(bnk)),mc = rep(NA,length(bnk)))
# Run this for each bank
for(i in 1:length(bnk))
{
  # Name the bank
  surv.res$bank[i] <- bnk[i]
  # We don't have data for some banks in some years, so if we don't skip this bank and do nothing!
  if(length(survey.obj[[bnk[i]]][[1]]$l.bar[survey.obj[[bnk[i]]][[1]]$year == yr]) > 0)
  {  
  # For all banks but middle calculate the shell height and meat counts  
    if(bnk[i] != "Mid") surv.res$sh[i] <- survey.obj[[bnk[i]]][[1]]$l.bar[survey.obj[[bnk[i]]][[1]]$year == yr]
    if(bnk[i] != "Mid") surv.res$mc[i] <- 500/(survey.obj[[bnk[i]]][[1]]$I[survey.obj[[bnk[i]]][[1]]$year == yr]/
                             survey.obj[[bnk[i]]][[1]]$N[survey.obj[[bnk[i]]][[1]]$year == yr])
    surv.res$cf[i] <- survey.obj[[bnk[i]]][[1]]$CF[survey.obj[[bnk[i]]][[1]]$year == yr]
    # We don't have this information in the survey object for middle bank...
    if(bnk[i] == "Mid")  surv.res$sh <- NA ; surv.res$mc <- NA
  } # end if(length(survey.obj[[bnk[i]]][[1]]$l.bar) > 0)

} # end for(i in 1:length(bnk))
   
# Save the results
if(save.res == T) save(fish.res,surv.res,sum.stat,fish.cells,extreme.catch,high.catch,
                       file = paste(direct,"Data/Fishery_data/Summary/",yr,"/OSAC_summary.RData",sep=""))

##############  OSAC Mini survey figs (for the top corner of presentation)  ###################
##############  This is similar to what we'd show in the survey summaries but optimized slightly for this presentation
##############  And includes all banks, not just where we surveyed this year.
if(make.mini.figs == T)
  {
bnk <- c(as.character(unique(fish.regs$Bank)),"GB") # Strictly speaking we don't need GB in there...

# Make a spatial survey summary figure for each bank of interest.
for(j in 1:length(bnk))
{
  # The directory for the figures, wouldn't hurt to make this into an "option" when converting to a function so that
  # the figures could be output to the screen and saved wherever you'd like
  # Note that the directory needs to exist for this to work!!!
  if(save.fig ==T) png(paste(direct,yr,"/Presentations/OSAC/",bnk[j],"/Survey_strata.png",sep=""),units="in",width = 11, height = 8.5,
      res=420)
  if(save.fig==F) windows(11,8.5)
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
  if(save.fig ==T)  dev.off()

} # end for(j in 1:length(bnk))
}# End if(make.mini.figs==T)


# Send back objects of interest...

OSAC_res <- list(fish.res = fish.res,surv.res=surv.res,sum.stat = sum.stat,fish.cells = fish.cells,
                        extreme.catch = extreme.catch,high.catch=high.catch)
assign("OSAC_res",OSAC_res,pos=1)

} #end function.





