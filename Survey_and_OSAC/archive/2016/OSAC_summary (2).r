################################################################################################################
##### ####################  The fishery summary plots necessary for Pre-OSAC and OSAC ###################################
###########################  November 18, 2015 - DK
################################################################################################################
#Update History
# Revised and commented by DK, April 1, 2016
# May 16, 2016:  Revised and turned into a proper function, large revision!!

###############################################################################################################
## This script needs these local functions to work (a.k.a. "support files"), 
# see "Source_relation_OSAC_summary.docx" for complete details
#  1: fishery.dat.r
#  2: OSAC_fishery_figures.r
#  3: logs_and_fishery_data.r
#  4: ScallopMap.r
###############################################################################################################

# Arguments
# yr:       The year of survey data you want to pull. Default = last year (current year -1)
# direct:   The directory where everything resides. Default = "Y:/Offshore scallop/Assessment"
# mx.dt:    The date of the logs you are pulling (sometimes the most recent aren't up to date), defaults to current date
#           make sure this is set as a "date" object so stick an "as.Date" in front of the data you specify.
#           If some of the logs have not be through QA/QC then specify the most recent log data you will use.
# bnk:      Pick the bank(s) you want, defaults to all banks. Options are any of c("Ban","Mid","Sab","Ger","BBs","BBn","GBa","GBb")
#poly.brd:  The border for the squares used in the fishery spatial figures.  Default = "black"
#save.fig:  Do you want to save the figures or just plot them to the screen.  (T/F) Default =F (plot to screen)
#add.titles:Do you want titles added to the figures.  Default = T
#low:       For the banks with lower catch rates (Sab,Mid,Ban,Ger,BBs) what is a good catch in a cell.  Default = 1.5 tonnes
#low.banks: These are the banks associated with "low.catch".  Default = c(Sab,Mid,Ban,Ger,BBs)
#high:      For the banks with higher catch rates (GBa,GBb,BBn) what is a good catch in a cell.  Default = 10 tonnes
#high.banks:These are the banks associated with "high.catch".  Default =  c("BBn","GBa","GBb")
# Both low and high catch are pretty relative depending on the bank of interest so want to think about that when setting these
#extreme:   Pull out extremely high catches (this should be larger than "high").  Default = 50 tonnes
#save.res:  Save the results as an r data object.  (T/F) Default = F
#export:    Export the results to a csv file.  (T/F) Default = F.
#make.mini.figs: Do you want to make the figures shown in the top left corner of the OSAC powerpoints?  (T/F) Default = F
# db.con:   The database to connect to.  Default ="ptran"
# un:       Your username to connect to SQL database.  Default = un.ID
# pw:       Your password to connect to SQL database.  Default = pwd.ID
############################# LOAD DATA 
################################### START LOAD Data and Functions ############################################
##########################################################################################################

OSAC_summary <- function(yr = as.numeric(format(Sys.time(), "%Y"))-1, direct = "Y:/Offshore scallop/Assessment",
                         mx.dt = as.Date(format(Sys.time(), "%Y-%m-%d")), bank = c("Ban","Mid","Sab","Ger","BBs","BBn","GBa","GBb"),
                         poly.brd = "black", save.fig = F, add.titles = T,low = 1.5, low.banks = c("Ban","Mid","Sab","Ger","BBs"),
                         high = 10,high.banks = c("BBn","GBa","GBb"), extreme = 50,save.res=F,export=F,make.mini.figs=F,
                         un=un.ID,pw=pwd.ID,db.con="ptran")
{

years.t = 1981:yr # The years of interest make temporary as this could be overwritten when I bring in the survey results...

# Load functions and external datafiles we might need
source(paste(direct_fns,"Survey_and_OSAC/OSAC_fishery_figures.r",sep="")) #Source1
source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct_fns,"Fishery/fishery.dat.r",sep=""))
source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))

# Read1 The fishery regulations are located here.
fish.regs <- read.csv(paste(direct,"data/Fishery_regulations_by_bank.csv",sep=""))
fish.regs <- subset(fish.regs,year==yr)
# There are various sources of data you may need to pull here.
# This will load the data from the year/directory combo above if you've run the survey

# Datasource #1:  If you have run all of survey summary (which is necessary to run the scripts in part 2 related to Survey summary)
#                 and you have saved that output you can simply load that data, this is sufficient to run all the analyses
#                 in this document, though the fishery data may not be the latest 
# This trick is needed because we don't have the Survey_all_results.Rdata file before 2015 (though it contains everything we want!)
if(yr <= 2015) load(paste(direct,"Data/Survey_data/2015/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
if(yr > 2015) load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
bnk <- bank # Reset the bank and year info
years <- years.t
yr <- max(years)
# So we update the fishery data with the lastest for this analysis in case the above is out of date.
# This queries the offshore so gets the most up to date fishery information which we should be sure we are using!
logs_and_fish(loc="offshore",year = 1981:yr,un=un,pw=pwd,db.con=db.con,direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

############################# END GENERAL DATA #######################################################
#################  PART I Fishery summary #################### PART I Fishery summary  #################### 
#################  PART I Fishery summary #################### PART I Fishery summary  #################### 
#################  PART I Fishery summary #################### PART I Fishery summary  #################### 
# Now run the function to generate the plots of interest.
# If you want the figures saved as image files make sure "save.fig=T" and also make sure the directories for the current year exist
#  exist.
# This object also contains the catch in each cell for each bank...
bank.spatial <- fishery_figures(fish.dat=fish.dat,bnk=bnk,max.date=mx.dt,dirct=direct,poly.brd=poly.brd,
                                years=years,save.fig=save.fig,add.titles = add.titles)

cpue.dat <- bank.spatial[[2]]
bank.spatial <- bank.spatial[[1]]
###################  Fishery Data for OSAC ################################## ###################  Fishery Data for OSAC ##########
###################  Fishery Data for OSAC ################################## ###################  Fishery Data for OSAC ##########
###################  Fishery Data for OSAC ################################## ###################  Fishery Data for OSAC ##########

#########  First up make a table of the fishery catch in each cell on each bank.
########  IF there was no fishing in the current year the output for that bank is garbage in here
res <- NULL
mid <- NULL

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
# If we have some spatial data grab the spatial information...
if(is.null(mid) == F)
{
fish.cells <- do.call("rbind",mid)
colnames(fish.cells) <- c("PID","SID","lon","lat","catch","bank")
# Turn catch into tonnage...
fish.cells$catch <- fish.cells$catch/1000

# Classify the catch in each cell. Anything over extreme.catch tonnes I want pulled out special.
extreme.catch <- subset(fish.cells, catch >= extreme )
# Mid catch uses "high.catch" as it's lower bound, considering any cells between 10 and 50 high.
high.catch <- subset(fish.cells, catch >= high & catch < extreme)

# Some basic summary stats...
med.catch <- aggregate(catch~bank,fish.cells,median)
men.catch <- aggregate(catch~bank,fish.cells,mean)
rg.catch <- aggregate(catch~bank,fish.cells,range)

# Look at each bank in more detail...
sum.stat <- as.data.frame(matrix(NA,ncol=3,nrow=length(unique(fish.cells$bank))))
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
if(is.null(mid) == T) sum.stat <- NULL ; fish.cells <- NULL ;extreme.catch <- NULL; high.catch <- NULL


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
# Note that we don't have any information for Banquereau (the first bank in the list) so we skip it 
# We are also missing Sable and Middle banks in 2015
if(bnk[i] != "Ban" && (yr == 2015 && is.element(bnk[i],c("Sab","Mid")))==F) 
{  
# For all banks but middle calculate the shell height and meat counts  
if(bnk[i] != "Mid") surv.res$sh[i] <- survey.obj[[bnk[i]]][[1]]$l.bar[survey.obj[[bnk[i]]][[1]]$year == yr]
if(bnk[i] != "Mid") surv.res$mc[i] <- 500/(survey.obj[[bnk[i]]][[1]]$I[survey.obj[[bnk[i]]][[1]]$year == yr]/
                         survey.obj[[bnk[i]]][[1]]$N[survey.obj[[bnk[i]]][[1]]$year == yr])
surv.res$cf[i] <- survey.obj[[bnk[i]]][[1]]$CF[survey.obj[[bnk[i]]][[1]]$year == yr]
# We don't have this information in the survey object for middle bank...
if(bnk[i] == "Mid")  surv.res$sh <- NA ; surv.res$mc <- NA
} # end if(bnk[i] != "Ban")

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





