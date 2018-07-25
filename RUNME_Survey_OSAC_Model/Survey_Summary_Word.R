Survey_Summary_Word <- function(year=2017, reportseason="spring", data="E:/Offshore scallop/Assessment/Data/Survey_data/2018/Survey_summary_output/testing_results.Rdata"){
  options(scipen=999)
  load(data)
  banks <- names(bank.dat)
  
  possiblebanks <- data.frame(banks=c("BBn", "BBs", "Ger", "Mid", "Sab", "GB", "Ban", "GBa", "GBb"),
                                season=c(rep("spring", 7), rep("summer", 2)))
  bankcheck <- data.frame(banks, year=year)
  bankcheck <- plyr::join(bankcheck, possiblebanks, type="full")
  bankcheck$word[bankcheck$banks=="BBn"] <- "Browns Bank North"
  bankcheck$word[bankcheck$banks=="BBs"] <- "Browns Bank South"
  bankcheck$word[bankcheck$banks=="Ger"] <- "German Bank"
  bankcheck$word[bankcheck$banks=="Mid"] <- "Middle Bank"
  bankcheck$word[bankcheck$banks=="Sab"] <- "Sable Bank"
  bankcheck$word[bankcheck$banks=="GB"] <- "Georges Bank"
  bankcheck$word[bankcheck$banks=="Ban"] <- "Banquereau Bank"
  bankcheck$word[bankcheck$banks=="GBa"] <- "Georges Bank 'a'"
  bankcheck$word[bankcheck$banks=="GBb"] <- "Georges Bank 'b'"
  
  bankcheck <<- bankcheck[bankcheck$season==reportseason,]
  
  ntows <- NULL
  highlights <- NULL
  for (i in 1:length(banks)){
    #print(i)
    # number of tows:
    ntowsy <- as.data.frame(table(unique(surv.dat[banks[i]][[1]][surv.dat[banks[i]][[1]]$year==year, c("tow", "random")])$random))
    ntowsy$type[ntowsy$Var1 == 1] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 1], "1", "fixed_regular")
    ntowsy$type[ntowsy$Var1 == 2] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 2], "2", "exploratory_2")
    ntowsy$type[ntowsy$Var1 == 3] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 3], "3", "repeated_spr")
    ntowsy$type[ntowsy$Var1 == 4] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 4], "4", "other")
    ntowsy$type[ntowsy$Var1 == 5] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 5], "5", "exploratory_5")
    ntowsy$bank <- banks[i]
    
    ntows <- rbind(ntows, ntowsy)
    
    # number per tow
    if(banks[i] %in% c("Mid", "GB", "Ger")){
      NPR_current <- SS.summary[banks[i]][[1]]$NPR[SS.summary[banks[i]][[1]]$year==year]
      NPR_prev <- SS.summary[banks[i]][[1]]$NPR[SS.summary[banks[i]][[1]]$year==year-1]
      NR_current <- SS.summary[banks[i]][[1]]$NR[SS.summary[banks[i]][[1]]$year==year]
      NR_prev <- SS.summary[banks[i]][[1]]$NR[SS.summary[banks[i]][[1]]$year==year-1]
      N_current <- SS.summary[banks[i]][[1]]$N[SS.summary[banks[i]][[1]]$year==year]
      N_prev <- SS.summary[banks[i]][[1]]$N[SS.summary[banks[i]][[1]]$year==year-1]
      NPR_LTM <- median(SS.summary[banks[i]][[1]]$NPR, na.rm=T)
      NR_LTM <- median(SS.summary[banks[i]][[1]]$NR, na.rm=T)
      N_LTM <- median(SS.summary[banks[i]][[1]]$N, na.rm=T)
      
      if(dim(SS.summary[banks[i]][[1]][SS.summary[banks[i]][[1]]$year==year-1,])[1]==0){
        NPR_prev <- NA
        NR_prev <- NA
        N_prev <- NA
      }
    }
    
    if(banks[i] %in% c("Sab", "BBn", "GBa", "GBb")){
      NPR_current <- survey.obj[banks[i]][[1]]$bankpertow$NPR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      NPR_prev <- survey.obj[banks[i]][[1]]$bankpertow$NPR[survey.obj[banks[i]][[1]]$bankpertow$year==year-1]
      NR_current <- survey.obj[banks[i]][[1]]$bankpertow$NR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      NR_prev <- survey.obj[banks[i]][[1]]$bankpertow$NR[survey.obj[banks[i]][[1]]$bankpertow$year==year-1]
      N_current <- survey.obj[banks[i]][[1]]$bankpertow$N[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      N_prev <- survey.obj[banks[i]][[1]]$bankpertow$N[survey.obj[banks[i]][[1]]$bankpertow$year==year-1]
      NPR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$NPR, na.rm=T)
      NR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$NR, na.rm=T)
      N_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$N, na.rm=T)
      
      if(dim(survey.obj[banks[i]][[1]]$bankpertow[survey.obj[banks[i]][[1]]$bankpertow$year==year-1,])[1]==0){
        NPR_prev <- NA
        NR_prev <- NA
        N_prev <- NA
      }
    }
    
    # BBs is only surveyed every other year, so change years to year-2
    if(banks[i] %in% c("BBs")){
      NPR_current <- survey.obj[banks[i]][[1]]$bankpertow$NPR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      NPR_prev <- survey.obj[banks[i]][[1]]$bankpertow$NPR[survey.obj[banks[i]][[1]]$bankpertow$year==year-2]
      NR_current <- survey.obj[banks[i]][[1]]$bankpertow$NR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      NR_prev <- survey.obj[banks[i]][[1]]$bankpertow$NR[survey.obj[banks[i]][[1]]$bankpertow$year==year-2]
      N_current <- survey.obj[banks[i]][[1]]$bankpertow$N[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      N_prev <- survey.obj[banks[i]][[1]]$bankpertow$N[survey.obj[banks[i]][[1]]$bankpertow$year==year-2]
      NPR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$NPR, na.rm=T)
      NR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$NR, na.rm=T)
      N_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$N, na.rm=T)
      
      if(dim(survey.obj[banks[i]][[1]]$bankpertow[survey.obj[banks[i]][[1]]$bankpertow$year==year-2,])[1]==0){
        NPR_prev <- NA
        NR_prev <- NA
        N_prev <- NA
      }
    }
    
    abundPT <- data.frame(variable=c("NPR", "NR", "N"), 
                          lastyear=c(NPR_prev, NR_prev, N_prev),
                          thisyear=c(NPR_current, NR_current, N_current),
                          LTM=c(NPR_LTM, NR_LTM, N_LTM))
    # ifelse for the wording
    abundPT$word <- ifelse(abundPT$thisyear - abundPT$lastyear >  10,
                           "increased",
                           ifelse(abundPT$thisyear - abundPT$lastyear < -10,
                                  "decreased",
                                  ifelse(abs(abundPT$thisyear - abundPT$lastyear) == 10 | 
                                           (abs(abundPT$thisyear - abundPT$lastyear) < 10 &
                                              abs(abundPT$thisyear - abundPT$lastyear) > 2),
                                         "changed slightly",
                                         ifelse(abs(abundPT$thisyear - abundPT$lastyear) < 2 |
                                                  abs(abundPT$thisyear - abundPT$lastyear) == 2, 
                                                "similar",
                                                "other"))))
    abundPT$nearLTM <- ifelse(abundPT$thisyear - abundPT$LTM >  10,
                              "greater than",
                              ifelse(abundPT$thisyear - abundPT$LTM < -10,
                                     "less than",
                                     ifelse(abs(abundPT$thisyear - abundPT$LTM) == 10 | 
                                              (abs(abundPT$thisyear - abundPT$LTM) < 10 & 
                                                 abs(abundPT$thisyear - abundPT$LTM) > 2),
                                            "slightly different from",
                                            ifelse(abs(abundPT$thisyear - abundPT$LTM) < 2|
                                                     abs(abundPT$thisyear - abundPT$LTM) == 2, 
                                                   "similar to",
                                                   "other"))))
    
    abundPT$bank <- banks[i]
    
    # biomass per tow
    if(banks[i] %in% c("Mid", "GB", "Ger")){
      IPR_current <- SS.summary[banks[i]][[1]]$IPR[SS.summary[banks[i]][[1]]$year==year]
      IPR_prev <- SS.summary[banks[i]][[1]]$IPR[SS.summary[banks[i]][[1]]$year==year-1]
      IR_current <- SS.summary[banks[i]][[1]]$IR[SS.summary[banks[i]][[1]]$year==year]
      IR_prev <- SS.summary[banks[i]][[1]]$IR[SS.summary[banks[i]][[1]]$year==year-1]
      I_current <- SS.summary[banks[i]][[1]]$I[SS.summary[banks[i]][[1]]$year==year]
      I_prev <- SS.summary[banks[i]][[1]]$I[SS.summary[banks[i]][[1]]$year==year-1]
      IPR_LTM <- median(SS.summary[banks[i]][[1]]$IPR, na.rm=T)
      IR_LTM <- median(SS.summary[banks[i]][[1]]$IR, na.rm=T)
      I_LTM <- median(SS.summary[banks[i]][[1]]$I, na.rm=T)
      
      if(dim(SS.summary[banks[i]][[1]][SS.summary[banks[i]][[1]]$year==year-1,])[1]==0){
        IPR_prev <- NA
        IR_prev <- NA
        I_prev <- NA
      }
    }
    
    if(banks[i] %in% c("Sab", "BBn", "GBa", "GBb")){
      IPR_current <- survey.obj[banks[i]][[1]]$bankpertow$IPR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      IPR_prev <- survey.obj[banks[i]][[1]]$bankpertow$IPR[survey.obj[banks[i]][[1]]$bankpertow$year==year-1]
      IR_current <- survey.obj[banks[i]][[1]]$bankpertow$IR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      IR_prev <- survey.obj[banks[i]][[1]]$bankpertow$IR[survey.obj[banks[i]][[1]]$bankpertow$year==year-1]
      I_current <- survey.obj[banks[i]][[1]]$bankpertow$I[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      I_prev <- survey.obj[banks[i]][[1]]$bankpertow$I[survey.obj[banks[i]][[1]]$bankpertow$year==year-1]
      IPR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$IPR, na.rm=T)
      IR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$IR, na.rm=T)
      I_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$I, na.rm=T)
      
      if(dim(survey.obj[banks[i]][[1]]$bankpertow[survey.obj[[1]]$bankpertow$year==year-1,])[1]==0){
        IPR_prev <- NA
        IR_prev <- NA
        I_prev <- NA
      }
    }
    
    if(banks[i] %in% c("BBs")){
      IPR_current <- survey.obj[banks[i]][[1]]$bankpertow$IPR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      IPR_prev <- survey.obj[banks[i]][[1]]$bankpertow$IPR[survey.obj[banks[i]][[1]]$bankpertow$year==year-2]
      IR_current <- survey.obj[banks[i]][[1]]$bankpertow$IR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      IR_prev <- survey.obj[banks[i]][[1]]$bankpertow$IR[survey.obj[banks[i]][[1]]$bankpertow$year==year-2]
      I_current <- survey.obj[banks[i]][[1]]$bankpertow$I[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      I_prev <- survey.obj[banks[i]][[1]]$bankpertow$I[survey.obj[banks[i]][[1]]$bankpertow$year==year-2]
      IPR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$IPR, na.rm=T)
      IR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$IR, na.rm=T)
      I_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$I, na.rm=T)
      
      if(dim(survey.obj[banks[i]][[1]]$bankpertow[survey.obj[[1]]$bankpertow$year==year-2,])[1]==0){
        IPR_prev <- NA
        IR_prev <- NA
        I_prev <- NA
      }
    }
    
    bmPT <- data.frame(variable=c("IPR", "IR", "I"), 
                       lastyear=c(IPR_prev, IR_prev, I_prev),
                       thisyear=c(IPR_current, IR_current, I_current),
                       LTM=c(IPR_LTM, IR_LTM, I_LTM))
    
    # ifelse for the wording
    bmPT$word <- ifelse(bmPT$thisyear - bmPT$lastyear >  15,
                        "increased",
                        ifelse(bmPT$thisyear - bmPT$lastyear < -15,
                               "decreased",
                               ifelse(abs(bmPT$thisyear - bmPT$lastyear) == 15 | 
                                        (abs(bmPT$thisyear - bmPT$lastyear) < 15 &
                                           abs(bmPT$thisyear - bmPT$lastyear) > 2),
                                      "slightly different from",
                                      ifelse(abs(bmPT$thisyear - bmPT$lastyear) < 2|
                                               abs(bmPT$thisyear - bmPT$lastyear) == 2, 
                                             "similar",
                                             "other"))))
    
    bmPT$nearLTM <- ifelse(bmPT$thisyear - bmPT$LTM >  15,
                           "greater than",
                           ifelse(bmPT$thisyear - bmPT$LTM < -15,
                                  "less than",
                                  ifelse(abs(bmPT$thisyear - bmPT$LTM) == 15 | 
                                           (abs(bmPT$thisyear - bmPT$LTM) < 15 &
                                              abs(bmPT$thisyear - bmPT$LTM) > 2),
                                         "slightly different from",
                                         ifelse(abs(bmPT$thisyear - bmPT$LTM) < 2|
                                                  abs(bmPT$thisyear - bmPT$LTM) == 2, 
                                                "similar to",
                                                "other"))))
    
    bmPT$bank <- banks[i]
    
    highlights <- rbind(highlights, abundPT)
    highlights <- rbind(highlights, bmPT)
    
    # shell height frequencies
    shsummary <- apply(surv.Rand[banks[i]][[1]][surv.Rand[banks[i]][[1]]$year==year, 14:53], 2, mean)
    maxbin <- names(shsummary[shsummary==max(shsummary)])
    maxbin <- gsub(x=maxbin, "h", "")
    maxbin <- paste0(as.numeric(maxbin)-5, "-", maxbin)
    
    maxPRtow <- max(surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==year])
    PR3Q <- round(summary(surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==year])[5], -1)
    ntowsabovePR3Q <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$pre>PR3Q &
                                                                   surv.Rand[banks[i]][[1]]$year==year]) - 1)

    maxRtow <- max(surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==year])
    R3Q <- round(summary(surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==year])[5], -1)
    ntowsaboveR3Q <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$rec>R3Q &
                                                                  surv.Rand[banks[i]][[1]]$year==year]) - 1)

    maxCtow <- max(surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==year])
    C3Q <- round(summary(surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==year])[5], -1)
    ntowsaboveC3Q <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$com>C3Q &
                                                                  surv.Rand[banks[i]][[1]]$year==year]) - 1)
    
    towsummary <- data.frame(variable=c("maxbin", "maxPRtow", "maxRtow", "maxCtow", "PR3Q", "R3Q", "C3Q"), 
                             lastyear=NA,
                             thisyear=c(max(shsummary), maxPRtow, maxRtow, maxCtow, PR3Q, R3Q, C3Q),
                             LTM=NA,
                             word=c(maxbin, NA, NA, NA,
                                    paste0(ntowsabovePR3Q, " tows"),
                                    paste0(ntowsaboveR3Q, " tows"),
                                    paste0(ntowsaboveC3Q, " tows")),
                             nearLTM=NA,
                             bank=banks[i])
    
    highlights <- rbind(highlights, towsummary)
    
    # mwsh and cf
    fittedmw100mm <- 1^SpatHtWt.fit[banks[i]][[1]]$B * SpatHtWt.fit[banks[i]][[1]]$A
    
    if(dim(cf.data[banks[i]][[1]]$CFyrs)[2] > 5){
      cfdat <- cf.data[banks[i]][[1]]$CFyrs[,c("year", "depth", "lon", "lat", "CF2")]
      names(cfdat) <- c("year", "depth", "lon", "lat", "CF")
    }
    if(dim(cf.data[banks[i]][[1]]$CFyrs)[2] == 5){
      cfdat <- cf.data[banks[i]][[1]]$CFyrs[,c("year", "depth", "lon", "lat", "CF")]
    }
    
    if(dim(cfdat[cfdat$year==year-1 & !is.na(cfdat$year),])[1]>0){
    
    cf <- data.frame(variable=c("CF",
                                "spatialCF",
                                "minCF", 
                                "maxCF"), 
                     lastyear=c(cfdat$CF[!is.na(cfdat$year) & cfdat$year==year-1],
                                mean(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year-1]),
                                min(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year-1]),
                                max(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year-1])),
                     thisyear=c(cfdat$CF[!is.na(cfdat$year) & cfdat$year==year],
                                mean(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
                                min(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
                                max(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year])),
                     LTM=NA)
    }
    
    if(banks[i] %in% "BBs"){
      
      cf <- data.frame(variable=c("CF",
                                  "spatialCF",
                                  "minCF", 
                                  "maxCF"), 
                       lastyear=c(cfdat$CF[!is.na(cfdat$year) & cfdat$year==year-2],
                                  mean(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year-2]),
                                  min(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year-2]),
                                  max(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year-2])),
                       thisyear=c(cfdat$CF[!is.na(cfdat$year) & cfdat$year==year],
                                  mean(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
                                  min(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
                                  max(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year])),
                       LTM=NA)
    }
    
    # if(dim(cfdat[cfdat$year==year-1 & !is.na(cfdat$year),])[1]==0){
    #   
    #   cf <- data.frame(variable=c("CF",
    #                               "spatialCF",
    #                               "minCF", 
    #                               "maxCF"), 
    #                    lastyear=rep(NA, 4),
    #                    thisyear=c(cfdat$CF[!is.na(cfdat$year) & cfdat$year==year],
    #                               mean(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
    #                               min(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
    #                               max(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year])),
    #                    LTM=NA)
    # }
    
    # ifelse for the wording
    cf$word <- ifelse(cf$thisyear - cf$lastyear >  0.5,
                      "increased",
                      ifelse(cf$thisyear - cf$lastyear < -0.5,
                             "decreased",
                             ifelse(abs(cf$thisyear - cf$lastyear) == 0.5 | 
                                      abs(cf$thisyear - cf$lastyear) < 0.5,
                                    "was similar",
                                    "other")))
    
    mwshcf <- rbind(data.frame(variable=c("fittedmw100mm"),
                               lastyear=NA,
                               thisyear=fittedmw100mm,
                               LTM=NA,
                               word=NA),
                    cf)
    mwshcf$nearLTM <- NA
    mwshcf$bank <- banks[i]
    
    highlights <- rbind(highlights, mwshcf)
    
    # meat count
    mc <- data.frame(variable=c("meanMC", "minMC", "maxMC", "lqMC", "uqMC"), 
                     lastyear=NA,
                     thisyear=c(mean(CF.current[[banks[i]]]$meat.count, na.rm=T),
                                min(CF.current[[banks[i]]]$meat.count, na.rm=T),
                                max(CF.current[[banks[i]]]$meat.count, na.rm=T),
                                summary(CF.current[[banks[i]]]$meat.count)[2],
                                summary(CF.current[[banks[i]]]$meat.count)[5]),
                     LTM=NA,
                     word=NA,
                     nearLTM=NA,
                     bank=banks[i])
    
    highlights <- rbind(highlights, mc)
    
    # clapper abundance
    if(!banks[i] %in% "BBs"){
    clap <- data.frame(variable=c("NPRclap", "NRclap", "Nclap",
                                  "PRpercentclap", "Rpercentclap", "Cpercentclap"),
                       lastyear=c(clap.survey.obj[banks[i]][[1]]$model.dat$NPR[clap.survey.obj[banks[i]][[1]]$model.dat$year==year-1],
                                  clap.survey.obj[banks[i]][[1]]$model.dat$NR[clap.survey.obj[banks[i]][[1]]$model.dat$year==year-1],
                                  clap.survey.obj[banks[i]][[1]]$model.dat$N[clap.survey.obj[banks[i]][[1]]$model.dat$year==year-1],
                                  NA, NA, NA),
                       thisyear=c(clap.survey.obj[banks[i]][[1]]$model.dat$NPR[clap.survey.obj[banks[i]][[1]]$model.dat$year==year],
                                  clap.survey.obj[banks[i]][[1]]$model.dat$NR[clap.survey.obj[banks[i]][[1]]$model.dat$year==year],
                                  clap.survey.obj[banks[i]][[1]]$model.dat$N[clap.survey.obj[banks[i]][[1]]$model.dat$year==year],
                                  mean(surv.Clap.Rand[[banks[i]]]$clap.propPre[surv.Clap.Rand[[banks[i]]]$year==year]),
                                  mean(surv.Clap.Rand[[banks[i]]]$clap.propRec[surv.Clap.Rand[[banks[i]]]$year==year]),
                                  mean(surv.Clap.Rand[[banks[i]]]$clap.propCom[surv.Clap.Rand[[banks[i]]]$year==year])),
                       LTM=NA)
    }
    
    if(banks[i] %in% "BBs"){
      clap <- data.frame(variable=c("NPRclap", "NRclap", "Nclap",
                                    "PRpercentclap", "Rpercentclap", "Cpercentclap"),
                         lastyear=c(clap.survey.obj[banks[i]][[1]]$model.dat$NPR[clap.survey.obj[banks[i]][[1]]$model.dat$year==year-2],
                                    clap.survey.obj[banks[i]][[1]]$model.dat$NR[clap.survey.obj[banks[i]][[1]]$model.dat$year==year-2],
                                    clap.survey.obj[banks[i]][[1]]$model.dat$N[clap.survey.obj[banks[i]][[1]]$model.dat$year==year-2],
                                    NA, NA, NA),
                         thisyear=c(clap.survey.obj[banks[i]][[1]]$model.dat$NPR[clap.survey.obj[banks[i]][[1]]$model.dat$year==year],
                                    clap.survey.obj[banks[i]][[1]]$model.dat$NR[clap.survey.obj[banks[i]][[1]]$model.dat$year==year],
                                    clap.survey.obj[banks[i]][[1]]$model.dat$N[clap.survey.obj[banks[i]][[1]]$model.dat$year==year],
                                    mean(surv.Clap.Rand[[banks[i]]]$clap.propPre[surv.Clap.Rand[[banks[i]]]$year==year]),
                                    mean(surv.Clap.Rand[[banks[i]]]$clap.propRec[surv.Clap.Rand[[banks[i]]]$year==year]),
                                    mean(surv.Clap.Rand[[banks[i]]]$clap.propCom[surv.Clap.Rand[[banks[i]]]$year==year])),
                         LTM=NA)
    }
    
    clap$thisyear <- as.numeric(as.character(clap$thisyear))
    clap$lastyear <- as.numeric(as.character(clap$lastyear))
    # ifelse for the wording
    clap$word <- ifelse(clap$thisyear - clap$lastyear >  0.05,
                        "increased",
                        ifelse(clap$thisyear - clap$lastyear < -0.05,
                               "decreased",
                               ifelse(abs(clap$thisyear - clap$lastyear) == 0.05 | 
                                        abs(clap$thisyear - clap$lastyear) < 0.05,
                                      "was similar",
                                      "other")))  
    clap$nearLTM <- NA
    clap$bank <- banks[i]
    highlights <- rbind(highlights, clap)
    
    #seedboxes
    if(banks[i] %in% unique(names(seedbox.obj))){
      boxy <- seedbox.obj[[banks[i]]][[1]]
      
      SeedPR_current <- boxy$model.dat$NPR[boxy$model.dat$year==year]
      SeedPR_prev <- boxy$model.dat$NPR[boxy$model.dat$year==year-1]
      SeedR_current <- boxy$model.dat$NR[boxy$model.dat$year==year]
      SeedR_prev <- boxy$model.dat$NR[boxy$model.dat$year==year-1]
      Seed_current <- boxy$model.dat$N[boxy$model.dat$year==year]
      Seed_prev <- boxy$model.dat$N[boxy$model.dat$year==year-1]
      
      seedPT <- data.frame(variable=c("SeedNPR", "SeedNR", "SeedN"), 
                            lastyear=c(SeedPR_prev, SeedR_prev, Seed_prev),
                            thisyear=c(SeedPR_current, SeedR_current, Seed_current),
                            LTM=NA)
      # ifelse for the wording
      seedPT$word <- ifelse(seedPT$thisyear - seedPT$lastyear >  5,
                             "increased",
                             ifelse(seedPT$thisyear - seedPT$lastyear < -5,
                                    "decreased",
                                    ifelse(abs(seedPT$thisyear - seedPT$lastyear) == 5 | 
                                             (abs(seedPT$thisyear - seedPT$lastyear) < 5 &
                                                abs(seedPT$thisyear - seedPT$lastyear) > 1),
                                           "changed slightly",
                                           ifelse(abs(seedPT$thisyear - seedPT$lastyear) < 1 |
                                                    abs(seedPT$thisyear - seedPT$lastyear) == 1, 
                                                  "similar",
                                                  "other"))))
      seedPT$nearLTM <- NA
      seedPT$bank <- banks[i]
      
      bmSeedPR_current <- boxy$model.dat$IPR[boxy$model.dat$year==year]
      bmSeedPR_prev <- boxy$model.dat$IPR[boxy$model.dat$year==year-1]
      bmSeedR_current <- boxy$model.dat$IR[boxy$model.dat$year==year]
      bmSeedR_prev <- boxy$model.dat$IR[boxy$model.dat$year==year-1]
      bmSeed_current <- boxy$model.dat$I[boxy$model.dat$year==year]
      bmSeed_prev <- boxy$model.dat$I[boxy$model.dat$year==year-1]
      
      bmseedPT <- data.frame(variable=c("SeedIPR", "SeedIR", "SeedI"), 
                           lastyear=c(bmSeedPR_prev, bmSeedR_prev, bmSeed_prev),
                           thisyear=c(bmSeedPR_current, bmSeedR_current, bmSeed_current),
                           LTM=NA)
      # ifelse for the wording
      bmseedPT$word <- ifelse(bmseedPT$thisyear - bmseedPT$lastyear >  5,
                            "increased",
                            ifelse(bmseedPT$thisyear - bmseedPT$lastyear < -5,
                                   "decreased",
                                   ifelse(abs(bmseedPT$thisyear - bmseedPT$lastyear) == 5 | 
                                            (abs(bmseedPT$thisyear - bmseedPT$lastyear) < 5 &
                                               abs(bmseedPT$thisyear - bmseedPT$lastyear) > 1),
                                          "changed slightly",
                                          ifelse(abs(bmseedPT$thisyear - bmseedPT$lastyear) < 1 |
                                                   abs(bmseedPT$thisyear - bmseedPT$lastyear) == 1, 
                                                 "similar",
                                                 "other"))))
      bmseedPT$nearLTM <- NA
      bmseedPT$bank <- banks[i]
      
      highlights <- rbind(highlights, seedPT)
      highlights <- rbind(highlights, bmseedPT)
      
    }
    
  }
  
  #print(bankcheck)
  print(ntows)
  print(highlights)

  highlights[,c(2,3,4)] <- apply(highlights[,c(2,3,4)], 2, function(x) round(as.numeric(x), 2))
  
  ntows <<- ntows
  highlights <<- highlights
  
}

