################################################################################################################
##### ####################  The fishery summary plots necessary for Pre-OSAC and OSAC ###################################
###########################  November 18, 2015 - DK
################################################################################################################
#Update History
# Revised and commented by DK, April 1, 2016
# May 16, 2016:  Revised and turned into a proper function, large revision!!
# October 2018:  Revised to add in the meat count summary function and revisions related to this function
# Jan 2021:      DK revised behaviour if direct_fns
###############################################################################################################
## This script needs these local functions to work (a.k.a. "support files"), 
# see "Source_relation_OSAC_summary.docx" for complete details
#  1: fishery.dat.r
#  2: OSAC_fishery_figures.r
#  3: logs_and_fishery_data.r
#  4: ScallopMap.r
###############################################################################################################

# Arguments
#1:  yr         The year of survey data you want to pull. Default = last year (current year -1)
#2:  mx.dt      The date of the logs you are pulling (sometimes the most recent aren't up to date), defaults to current date
#               make sure this is set as a "date" object so stick an "as.Date" in front of the data you specify.
#               If some of the logs have not be through QA/QC then specify the most recent log data you will use.
#3:  bank       Pick the bank(s) you want, defaults to all banks. Options are any of c("Ban","Mid","Sab","Ger","BBs","BBn","GBa","GBb","SPB")
#               if you set bank = "NULL" you can skip all the bank calcs and just obtain the meat counts 

#Both low and high catch are pretty relative depending on the bank of interest so 4-8 are really a "gut" feel thing
#4:  low.banks  These are the banks associated with "low.catch".  Default = c(Sab,Mid,Ban,Ger,BBs)
#5:  high.banks These are the banks associated with "high.catch".  Default =  c("BBn","GBa","GBb")
#6:  low        For the banks with lower catch rates (Sab,Mid,Ban,Ger,BBs) what is a good catch in a cell.  Default = 1 tonnes
#7:  high       For the banks with higher catch rates (GBa,GBb,BBn) what is a good catch in a cell.  Default = 10 tonnes
#8:  extreme    Pull out extremely high catches (this should be larger than "high").  Default = 50 tonnes

#9:  poly.brd   The border for the squares used in the fishery spatial figures.  Default = "black"
#10: mini.figs  Do you want to make the figures shown in the top left corner of the OSAC powerpoints?  (T/F) Default = F
#11: add.titles Do you want titles added to the figures.  Default = T


#12: save.fig   Do you want to save the figures or just plot them to the screen.  (T/F) Default =F (plot to screen)
#13: save.res   Save the results as an r data object.  (T/F) Default = F
#14: export     Export the results to a csv/xlsx file.  (T/F) Default = F.
#15: calc.mc    Do you want to extract the meat counts (min/max) for each bank and fishery type (i.e. FT vs WF)? T/F, default =T
#16: mc.path    The location of the documents with the meat counts in them, these are "Joan's reports".  Note this is relative to "direct" 
#17: db.con     The database to connect to.  Default ="ptran"
#18: un         Your username to connect to SQL database.  Default = un.ID
#19: pw         Your password to connect to SQL database.  Default = pwd.ID
#20: direct     The directory where everything resides. Default = "Y:/Offshore scallop/Assessment"
#21: direct_fns Where you are sourcing the functions from.  Default is missing which will point to github.
#22: rdata.logs saves the logs to an rdata file. Default is F (do not save). 
############################# LOAD DATA 
################################### START LOAD Data and Functions ############################################
##########################################################################################################

OSAC_summary <- function(yr = as.numeric(format(Sys.time(), "%Y")), mx.dt = as.Date(format(Sys.time(), "%Y-%m-%d")),
                         bank = "all",low.banks = c("Ban","Mid","Sab","Ger","BBs","SPB"),high.banks = c("BBn","GBa","GBb"),
                         low = 1,high = 10, extreme = 50,
                         poly.brd = "black", mini.figs=F, add.titles = T,
                         save.fig = F,save.res=F, rdata.logs=F, export=F,calc.mc = T, 
                         mc.path = "default",direct, direct_fns,
                         un=NULL,pw=NULL,db.con="ptran")
{
  options(timeout=120)
  # Load functions and external datafiles we might need
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/OSAC_fishery_figures.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Fishery/logs_and_fishery_data.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Fishery/fishery.dat.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Fishery/meat.count.table.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/ScallopMap.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/github_spatial_import.R")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
    dir <- tempdir()
    for(fun in funs) 
    {
      temp <- dir
      download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
      source(paste0(dir,"/",basename(fun)))
      file.remove(paste0(dir,"/",basename(fun)))
    } # end for(fun in funs)
  }# end if(missing(direct_fns))  
  
  
  if(!missing(direct_fns))
  {
    source(paste(direct_fns,"Survey_and_OSAC/OSAC_fishery_figures.r",sep="")) #Source1
    source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep=""))
    source(paste(direct_fns,"Fishery/fishery.dat.r",sep=""))
    source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))
    source(paste0(direct_fns, "Fishery/meat.count.table.R"))
    source(paste0(direct_fns, "Maps/github_spatial_import.R"))
  }  # end  if(!missing(direct_fns))
  
  
  #require(xlsx) || stop("Hold up!  If you don't install the xlsx package, well ya know... spoilers... so I can't say what will happen, but it'll suck")
  require(plyr) || stop("Hold up!  If you don't install the plyr package, well ya know... spoilers... so I can't say what will happen, but it'll suck")
  require(splancs) || stop("Hold up!  If you don't install the splancs package, well ya know... spoilers... so I can't say what will happen, but it'll suck")
  require(sf) || stop("Please install sf.")
  require(collapse) || stop("Please install collapse.")
  require(zoo) || stop("Please install zoo.")
  require(ggplot2) || stop("Please install ggplot2")

  # If you set bank to be NULL we skip almost the entire function and just run the meat count bit, if you set to NULL and don't calc.mc then
  # a big nothing happens!
  if(!is.null(bank))
  {
    years.t = 1981:yr # The years of interest make temporary as this could be overwritten when I bring in the survey results...
    if(bank == "all") bank <- c("Ban","Mid","Sab","Ger","BBs","BBn","GBa","GBb","SPB")
    # Read1 The fishery regulations are located here.
    fish.regs <- read.csv(paste(direct,"data/Fishery_regulations_by_bank.csv",sep=""))
    fish.regs <- subset(fish.regs,year==yr)
    # There are various sources of data you may need to pull here.
    # This will load the data from the year/directory combo above if you've run the survey
    
    # Datasource #1:  If you have run all of survey summary (which is necessary to run the scripts in part 2 related to Survey summary)
    #                 and you have saved that output you can simply load that data, this is sufficient to run all the analyses
    #                 in this document, though the fishery data may not be the latest 
    # This trick is needed because we don't have the Survey_all_results.Rdata file before 2015 (though it contains everything we want!)
    direct1 <- direct
    direct2 <- direct_fns
    if(yr <= 2015) load(paste(direct,"Data/Survey_data/2015/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
    if(yr > 2015) load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
    direct <- direct1
    direct_fns <- direct2
    bnk <- bank # Reset the bank and year info
    years <- years.t
    yr <- max(years)
browser()
    # So we update the fishery data with the lastest for this analysis in case the above is out of date.
    # This queries the offshore so gets the most up to date fishery information which we should be sure we are using!
    logs_and_fish(loc="offshore",year = 1981:yr,un=un,pw=pwd,db.con=db.con,direct=direct, direct_fns=direct_fns, get.marfis=F)
    fish.dat<-merge(new.log.dat,old.log.dat,all=T)
    fish.dat$ID<-1:nrow(fish.dat)
    
    if(rdata.logs==T) save(fish.dat,
                           file = paste(direct,"Data/Fishery_data/Summary/",yr,"/OSAC_tidy_logs.RData",sep=""))
    
    ############################# END GENERAL DATA #######################################################
    #################  PART I Fishery summary #################### PART I Fishery summary  #################### 
    #################  PART I Fishery summary #################### PART I Fishery summary  #################### 
    #################  PART I Fishery summary #################### PART I Fishery summary  #################### 
    # Now run the function to generate the plots of interest.
    # If you want the figures saved as image files make sure "save.fig=T" and also make sure the directories for the current year exist
    #  exist.
    # This object also contains the catch in each cell for each bank...
    #browser()
    print("fishery_figures")
    
    bank.spatial <- fishery_figures(fish.dat=fish.dat,bnk=bnk,max.date=mx.dt,direct=direct, direct_fns = direct_fns,poly.brd=poly.brd,
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
      print(paste0("fishery catch table - ", bnk[i]))
      
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
      min.catch <- aggregate(catch~bank,fish.cells,min)
      max.catch <- aggregate(catch~bank,fish.cells,max)
      
      names(med.catch) <- c("bank", "median.catch")
      names(men.catch) <- c("bank", "mean.catch")
      names(min.catch) <- c("bank", "min.catch")
      names(max.catch) <- c("bank", "max.catch")
      
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
      require(plyr)
      sum.stat <- join(join(join(join(sum.stat,med.catch), men.catch), min.catch), max.catch)
      names(sum.stat) <- c("Bank","prop.spatial","prop.of.total","median.catch","mean.catch","min.catch","max.catch")
    } # end if(is.null(mid) == F)
    
    # If we don't have any spatial data set sum.stat, fish.cells, extreme.catch, and high.catch to null
    if(is.null(mid) == T) { sum.stat <- NULL ; fish.cells <- NULL ;extreme.catch <- NULL; high.catch <- NULL }
    
    
    # Now moving along we can also include summary stats of interesting areas...
    # First we can generate interesting fishery related stats for each bank in the most recently fished year.
    #bnk=c("GBa")
    #bnk <- as.character(unique(fish.regs$Bank))

    ### DK NOTE OCT 2017.   When I have a minute I can remove the call to fishery.dat from the below loop and make this function more efficient 
    #as I already have these data and from the fishery figures function.  Only problem is I don't have number of trips correct in that function 
    #  (is provides number of watches I think) for the cpue ts info, I'll need to encorporate something like the below line which gets that info...
    #tmp <- aggregate(tripnum~bank+year+fleet,fish.dat,FUN=function(x) length(unique(x)))
    fishery.results <- NULL
    for(i in 1:length(bnk))
    {
      print(paste0("fishery.results - ", bnk[i]))
      
      # Now subset the fishery data if we need to remove some data (usually b/c recent data is not QA/QC ready) ...
      bank.fish <- subset(fish.dat, bank == bnk[i] & date < mx.dt)
      
      cpue.ts    <- fishery.dat(bank.fish,bk=bnk[i],yr=years,method='jackknife',direct=direct, direct_fns=direct_fns) 			
      cpue.ts.WF <- fishery.dat(subset(bank.fish,fleet=="WF"),bk=bnk[i],yr=years,method='jackknife',direct=direct, direct_fns=direct_fns) 			
      cpue.ts.FT <- fishery.dat(subset(bank.fish,fleet=="FT"),bk=bnk[i],yr=years,method='jackknife',direct=direct, direct_fns=direct_fns) 			
      
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
    if(is.factor(fish.res[,2:13])){
      factor.2.number <- function(x) {as.numeric(levels(x))[x]}  
      fish.res[,2:13] <- sapply(fish.res[,2:13],factor.2.number)
    }
    if(is.character(fish.res[,2:13])){
      factor.2.number <- function(x) {as.numeric(x)}  
      fish.res[,2:13] <- sapply(fish.res[,2:13],factor.2.number)
    }
    
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
      print(paste0("survey data - ", bnk[i]))
      
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
      
      # Looks good.  I'd have the y axis go to 0. I don't love the point for the stock status value (but I do like that it is clear that in 2025 this is the value, whereas the lines I used made it muddy to know what year it was). Maybe just a totally different color for the point (like blue) would make it pop more and make me happy.
      
      # for banks with new survey-index reference points, we need to plot the index for OSAC
      # follow the  methods from 2025 Reference Point framework
      if(bnk[i] %in% c("BBs", "Ger", "Sab", "GBb")){
        q.index <- 0.33
        if(bnk[i] == "Sab"){
          # # Get the median, maximum, and maximum 3 year rolling mean, then get the LRP and USR from those
          # # SFA 25A
          # surv.25a <- survey.obj$Sab$model.dat
          # surv.25a <- surv.25a |> collapse::fsubset(year >=1994)
          # surv.25a <- rbind(surv.25a,c(2020,rep(NA,ncol(surv.25a)-1)))
          # surv.25a <- rbind(surv.25a,c(2015,rep(NA,ncol(surv.25a)-1)))
          # surv.25a <- surv.25a[order(surv.25a$year),] # get the years ordered correctly for the rolling mean
          # # q correct
          # surv.25a$I <- surv.25a$I/q.index
          # surv.25a$IR <- surv.25a$IR/q.index
          # gmean.25a <- exp(mean(log(surv.25a$I),na.rm=T))
          # max.25a <- max(surv.25a$I,na.rm=T)
          # # Geometric rolling mean
          # ma.three.mean.25a <- exp(c(na.omit(rollmean(log(na.omit(surv.25a$I)),3,'center'))))
          # max.three.25a <- max(ma.three.mean.25a,na.rm=T)
          # # Doing this with the last 3 years of data available, so if missing a survey we go back and use the information from the year before that
          # 
          # bm.last.3.25a <- round(ma.three.mean.25a[length(ma.three.mean.25a)],digits=0)
          # ratio.25a <- gmean.25a/max.three.25a
          # LRP.med.3.25a <- round(0.3*gmean.25a,digits = 0)
          # LRP.med.4.25a <- round(0.4*gmean.25a,digits = 0)
          # LRP.max.2.25a <- round(0.2*max.three.25a,digits = 0)
          # USR.med.8.25a <- round(0.8*gmean.25a,digits = 0)
          # USR.max.4.25a <- round(0.4*max.three.25a,digits = 0)
          # 
          # # The simple reference point figures
          # plt.dat.25a <- data.frame(year =2020:2022,cur.bm = bm.last.3.25a,
          #                           lrp.2 = LRP.max.2.25a,lrp.3 = LRP.med.3.25a,lrp.4 = LRP.med.4.25a,
          #                           usr.4 = USR.max.4.25a,usr.8 = USR.med.8.25a)
          # 
          # plt.25a.bmi.ts <- ggplot(surv.25a) + geom_line(aes(x=year,y=I)) +
          #   geom_line(data= plt.dat.25a,aes(x=year,y=cur.bm),color=u.colors[1],size=1.5) +
          #   geom_hline(yintercept = plt.dat.25a$lrp.2[1],color='firebrick2',linetype='solid') +
          #   geom_hline(yintercept = plt.dat.25a$lrp.3[1],color='firebrick2',linetype = 'dotted') +
          #   geom_hline(yintercept = plt.dat.25a$lrp.4[1],color='firebrick2',linetype='dashed') +
          #   geom_hline(yintercept = plt.dat.25a$usr.4[1],color=u.colors[2],linetype = 'solid') +
          #   geom_hline(yintercept = plt.dat.25a$usr.8[1],color=u.colors[2],linetype='dashed') +
          #   scale_x_continuous(name = '',breaks = seq(1995,2025,by=5),limits = c(1994,2022),labels = NULL) +
          #   scale_y_continuous(name = "",limits=c(0,7500)) #+
          # #theme(axis.title.y = element_text(hjust = 4))
          
        }
        
        # SFA 26A
        if(bnk[i] == "BBn"){
          # surv.26a <- survey.obj$BBn$model.dat
          # surv.26a <- rbind(surv.26a,c(2020,rep(NA,ncol(surv.26a)-1)))
          # surv.26a <- surv.26a |> collapse::fsubset(year >=1994)
          # surv.26a <- surv.26a[order(surv.26a$year),] # get the years ordered correctly for the rolling mean
          # # q correct
          # surv.26a$I <- surv.26a$I/q.index
          # surv.26a$IR <- surv.26a$IR/q.index
          # gmean.26a <- exp(mean(log(surv.26a$I),na.rm=T))
          # max.26a <- max(surv.26a$I,na.rm=T)
          # # Geometric rolling mean
          # ma.three.mean.26a <- exp(c(na.omit(rollmean(log(na.omit(surv.26a$I)),3,'center'))))
          # max.three.26a <- max(ma.three.mean.26a,na.rm=T)
          # # Current 3 year geometric mean biomass
          # bm.last.3.26a <- round(ma.three.mean.26a[length(ma.three.mean.26a)],digits=0)
          # ratio.26a <- gmean.26a/max.three.26a
          # LRP.26a <- 
          # USR.26a <- 
          #   
          # # Now BBn
          # plt.dat.26a <- data.frame(year=max(surv.26a$year), cur.bm = bm.last.3.26a,
          #                           lrp = LRP.26a, usr = USR.26a)
          # 
          # plt.26a.bmi.ts <- ggplot() + geom_line(data=surv.26a, aes(x=year,y=I), size=0.5) +
          #   geom_point(data= plt.dat.26a,aes(x=year,y=cur.bm), size=1.5) +
          #   geom_hline(yintercept = plt.dat.26a$lrp,color='firebrick2',linetype = 'dashed', size=1) +
          #   #scale_x_continuous(name = '',breaks = seq(1995,2025,by=5),limits = c(1994,2022)) +
          #   #scale_y_continuous(name = "",limits=c(0,6000)) +
          #   theme_bw() +
          #   ylab("Survey index (tonnes)") +
          #   xlab("Year")
        }
        
        if(bnk[i] == "Ger"){
          ger.shape <- github_spatial_import(subfolder="other_boundaries", zipname="other_boundaries.zip", specific_shp = "WGS_84_German.shp")
          #ger.shape <- st_read("C:/Users/keyserf/Documents/Github/GIS_layers/other_boundaries/WGS_84_German.shp")
          ger.shape <-  ger.shape %>% st_make_valid() %>% st_transform(32619)
          ger.area.km2 <- st_area(ger.shape)/1e6
          # I should be using the "merged.survey.obj here....
          # SFA 26C
          #surv.26c <- lined.survey.obj$model.dat # Has NA's for missing 2020 so good to use as is
          #surv.26c <- surv.26c |> collapse::fsubset(year >=1994)
          surv.26c <- merged.survey.obj # Need to add in 2020 missing...
          surv.26c[nrow(surv.26c)+1,] <- NA
          surv.26c$year[nrow(surv.26c)] <- 2020
          # and reorder
          surv.26c <- surv.26c[order(surv.26c$year),]
          # Now the units of german are....? If it is grams/tow... this puts us in total tonnes
          surv.26c$I <- as.numeric(surv.26c$I /atow*ger.area.km2/1e6)
          surv.26c$IR <- as.numeric(surv.26c$IR/atow*ger.area.km2/1e6)
          
          # Q correct
          surv.26c$I <- surv.26c$I/q.index
          surv.26c$IR <- surv.26c$IR/q.index
          gmean.26c <- exp(mean(log(surv.26c$I),na.rm=T))
          max.26c <- max(surv.26c$I,na.rm=T)
          # Geometric rolling mean
          ma.three.mean.26c <- exp(na.omit(c(rollmean(log(na.omit(surv.26c$I)),3,'center'))))
          max.three.26c <- max(ma.three.mean.26c,na.rm=T)
          # Current 3 year geometric mean biomass #note: do not use the code above for retrospective stock status indicators
          bm.last.3.26c <- round(ma.three.mean.26c[length(ma.three.mean.26c)],digits=0)
          ratio.26c <- gmean.26c/max.three.26c
          LRP.26c <- 890
          USR.26c <- NA
          
          # For the SAR we want the BM time series figure for just 26C and 27B with the 30% BMSY value
          # Now german
          plt.dat.26c <- data.frame(year=max(surv.26c$year), cur.bm = bm.last.3.26c,
                                    lrp = LRP.26c, usr = USR.26c)
          
          plt.26c.bmi.ts <- 
            ggplot() + 
            geom_ribbon(data=surv.26c, aes(x=year,ymin=I-(I*I.cv),ymax=I+(I*I.cv)), fill="grey") +
            geom_line(data=surv.26c, aes(x=year,y=I)) +
            geom_point(data= surv.26c,aes(x=year,y=I), colour="black", shape=16, size=1) +
            geom_point(data= plt.dat.26c,aes(x=year,y=cur.bm, colour="\n3-year\ngeometric\nmean\n"), shape=15, size=2) +
            scale_color_manual(values = c("\n3-year\ngeometric\nmean\n" = "blue"))+
            #geom_text(data= plt.dat.26c,aes(x=year,y=cur.bm, label="3y mean"), colour="blue", hjust=-0.1, vjust=0.5) +
            geom_hline(yintercept = plt.dat.26c$lrp,color="red",linetype = 'dashed') +
            geom_text(data=surv.26c, aes(x=min(year), y=plt.dat.26c$lrp, label="LRP"),color="red",hjust=0, vjust=1.2) +
            #xlim(c(min(surv.26c$year), max(surv.26c$year)+2))+
            #scale_x_continuous(name = '',breaks = seq(1995,2025,by=5),limits = c(1994,2022)) +
            ylim(c(0,NA))+
            theme_bw() +
            theme(panel.grid=element_blank(), legend.background = element_blank(),legend.title=element_blank())+
            ylab("Fully-recruited survey biomass index (tonnes)") +
            xlab("Year")
          
          png(paste(direct,yr,"/Presentations/OSAC/",bnk[i],"/survey_index_lrp.png",sep=""),units="in",width = 7, height = 4,
              res=420)
          print(plt.26c.bmi.ts)
          dev.off()
          
        }
        
        if(bnk[i] == "GBb"){
          # SFA 27B
          surv.27b <- survey.obj$GBb$model.dat # Need to add missing 2020 survey in here.
          surv.27b <- rbind(surv.27b,c(2020,rep(NA,ncol(surv.27b)-1)))
          surv.27b <- surv.27b |> collapse::fsubset(year >=1994)
          surv.27b <- surv.27b[order(surv.27b$year),] # get the years ordered correctly for the rolling mean
          # q corrected
          surv.27b$I <- as.numeric(surv.27b$I/q.index)
          surv.27b$IR <- as.numeric(surv.27b$IR/q.index)
          gmean.27b <- exp(mean(log(surv.27b$I),na.rm=T))
          max.27b <- max(surv.27b$I,na.rm=T)
          ma.three.mean.27b <- exp(c(na.omit(rollmean(log(na.omit(surv.27b$I)),3,'center'))))
          max.three.27b <- max(ma.three.mean.27b,na.rm=T)
          # Current 3 year geometric mean biomass #note: do not use the code above for retrospective stock status indicators
          bm.last.3.27b <- round(ma.three.mean.27b[length(ma.three.mean.27b)],digits=0)
          ratio.27b <- gmean.27b/max.three.27b
          LRP.27b <- 628
          USR.27b <- NA
          
          plt.dat.27b <- data.frame(year =max(surv.27b$year),cur.bm = bm.last.3.27b,
                                    lrp = LRP.27b,
                                    usr = USR.27b)
          
          plt.27b.bmi.ts <-  ggplot() + 
            geom_ribbon(data=surv.27b, aes(x=year,ymin=I-(I*I.cv),ymax=I+(I*I.cv)), fill="grey") +
            geom_line(data=surv.27b, aes(x=year,y=I)) +
            geom_point(data= surv.27b,aes(x=year,y=I), colour="black", shape=16, size=1) +
            geom_point(data= plt.dat.27b,aes(x=year,y=cur.bm, colour="\n3-year\ngeometric\nmean\n"), shape=15, size=2) +
            scale_color_manual(values = c("\n3-year\ngeometric\nmean\n" = "blue"))+
            #geom_text(data= plt.dat.27b,aes(x=year,y=cur.bm, label="3y mean"), colour="blue", hjust=-0.1, vjust=0.5) +
            geom_hline(yintercept = plt.dat.27b$lrp,color="red",linetype = 'dashed') +
            geom_text(data=surv.27b, aes(x=min(year), y=plt.dat.27b$lrp, label="LRP"),color="red",hjust=0, vjust=1.2) +
            #xlim(c(min(surv.27b$year), max(surv.27b$year)+4))+
            #scale_x_continuous(name = '',breaks = seq(1995,2025,by=5),limits = c(1994,2022)) +
            ylim(c(0,NA))+
            theme_bw() +
            theme(panel.grid=element_blank(), legend.background = element_blank(),legend.title=element_blank())+
            ylab("Fully-recruited survey biomass index (tonnes)") +
            xlab("Year")
    
          png(paste(direct,yr,"/Presentations/OSAC/",bnk[i],"/survey_index_lrp.png",sep=""),units="in",width = 7, height = 4,
              res=420)
          print(plt.27b.bmi.ts)
          dev.off()
          
        }
        
        # # We can make these a nice 4 by 4 plot...
        # plt.bmi.ts <- plot_grid(plt.25a.bmi.ts,plt.26a.bmi.ts,plt.26c.bmi.ts,plt.27b.bmi.ts,ncol=2,labels = c("SFA 25A","SFA 26A","SFA 26C","SFA 27B"),label_size=14,label_y=0.95,label_x=0.15) + theme(plot.margin = margin(0, 0, -275, 0))
        # 
        # plt.bmi.ts <- add_sub(plt.bmi.ts, "Survey Biomass Index (tonnes)",angle=90,y=2.25,x=0.01,size=18)
        # plt.bmi.ts <- ggdraw(plt.bmi.ts)
        # save_plot(plt.bmi.ts,file =paste0(plot.index.loc,"RPs_BM_Indices.png"),base_height = 8,base_width = 11)
        
        
        
        # 
        # # We can make these a nice 2 x 1 plot now...
        # plt.bmi.lrp <- plot_grid(plt.26c.bmi.lrp,plt.27b.bmi.lrp,ncol=2,labels = c("SFA 26C","SFA 27B"),label_size=14,label_y=0.95,label_x=0.15) + theme(plot.margin = margin(0, 0, -275, 0))
        # 
        # plt.bmi.lrp <- add_sub(plt.bmi.lrp, "Survey Biomass Index (tonnes)",angle=90,y=2.25,x=0.01,size=18)
        # plt.bmi.lrp <- ggdraw(plt.bmi.lrp)
        # save_plot(plt.bmi.lrp,file =paste0(plot.index.loc,"LRPs_BM_Indices_26C_27B.png"),base_height = 8.5,base_width = 16)
        # 
        
        
        
        
      }
      
    } # end for(i in 1:length(bnk))
    
    ### Create the meat count table
    ### Uses data from Port Sampling Reports. You must copy all of the word docs from Offshore scallop/Amy/PortSampling/PSYEAR/JoansOriginals into your direct's Data/Port_Sampling/2018 folder (whichever year you're on)
    ### Only copy over the word docs that have Fsh or Fzn in the names.
    if(calc.mc == T)
    {
      print("meat count")
      # Set up the path you want to go to pull the data in from, the default is the Port Sampling Reports folder
      if(mc.path == "default") mc.path <- paste0("Data/PortSampling/PS", yr, "/Reports/")
      path <-  paste0(direct,mc.path) 
      fnames <- list.files(path = path)
      meat.count <- meat.count.table(filenames=fnames, path=path)
      if(export == T) write.csv(meat.count$summarytable,paste0(direct,"Data/PortSampling/PS",yr,"/Meat_count_summary.csv"))
    } # end if(calc.mc == T)
    # Since we save the thing we need to make the object for the list, just make it a null if you haven't calculated it.
    if(calc.mc == F) meat.count <- NULL
    # Save the results
    
    if(save.res == T) save(fish.res,surv.res,sum.stat,fish.cells,extreme.catch,high.catch,cpue.dat,meat.count,
                           file = paste(direct,"Data/Fishery_data/Summary/",yr,"/OSAC_summary.RData",sep=""))
    
    ##############  OSAC Mini survey figs (for the top corner of presentation)  ###################
    ##############  This is similar to what we'd show in the survey summaries but optimized slightly for this presentation
    ##############  And includes all banks, not just where we surveyed this year.
    if(mini.figs == T)
    {
      bnk <- c(as.character(unique(fish.regs$Bank)),"GB") # Strictly speaking we don't need GB in there...
      
      # Make a spatial survey summary figure for each bank of interest.
      for(j in 1:length(bnk))
      {
        print(paste0("Survey_strata - ", bnk[i]))
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
                     title="",cex.mn=2,dec.deg = F,direct=direct,manage.colors = pastel.colors(60,seed=2), direct_fns=direct_fns)
        } # end if(nrow(surv.dets)==0)
        # If there is survey strata we do this...
        if(nrow(surv.dets)>0)
        {
          ScallopMap(area=bnk[j],plot.bathy=T,bathy.source = "quick",boundries = "offshore",shore="nwatlHR",
                     poly.lst=list(surv.dets,surv.info),
                     bound.color = F,plot.boundries = T,label.boundries = F,offshore.names=F,xlab="",ylab="",bathcol = "darkblue",
                     title="",cex.mn=2,dec.deg = F,direct=direct,manage.colors = pastel.colors(60,seed=2), direct_fns=direct_fns)
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
    }# End if(mini.figs==T)
    
    # Send back objects of interest...
    
    OSAC_res <- list(fish.res = fish.res,surv.res=surv.res,sum.stat = sum.stat,fish.cells = fish.cells,
                     extreme.catch = extreme.catch,high.catch=high.catch,cpue.ts = cpue.dat, meat.count=meat.count)
    assign("OSAC_res",OSAC_res,pos=1)
  } # end if(!is.null(bank))
  
  #### If only interested in the meat count table then just run this and save the output....
  ### Uses data from Port Sampling Reports. You must copy all of the word docs from Offshore scallop/Amy/PortSampling/PSYEAR/JoansOriginals into your direct's Data/Port_Sampling/2018 folder (whichever year you're on)
  ### Only copy over the word docs that have Fsh or Fzn in the names.
  if(is.null(bank) && calc.mc ==T)
  {
    # Set up the path you want to go to pull the data in, for now hard coded more or less, but we 
    if(mc.path == "default") mc.path <- paste0("Data/PortSampling/PS", yr, "/Reports/")
    path <-  paste0(direct,mc.path) 
    fnames <- list.files(path = path)
    meat.count <- meat.count.table(filenames=fnames, path=path)
    if(export == T) write.csv(meat.count$summarytable,paste0(direct,"Data/PortSampling/PS",yr,"/Meat_count_summary.csv"))
    assign("meat.count",meat.count,pos=1)
  } # end if(is.null(bank))
  
} #end function.





