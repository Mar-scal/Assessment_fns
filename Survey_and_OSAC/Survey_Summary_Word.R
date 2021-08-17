## summarizes survey summary into a nice table for use in Rmarkdown

## REQUIRES:
## survey summary Rdata file (e.g. testing_results.Rdata)
## direct/Data/Figure_regulations_by_bank.csv

## OPTIONAL:
## INLA model RData files (from survey_summary_figures.R)


## use model.dat cf for all cf measurements!
Survey_Summary_Word <- function(year=2017, reportseason="spring", subarea=F, data="E:/Offshore scallop/Assessment/Data/Survey_data/2018/Survey_summary_output/testing_results.Rdata", direct=direct, direct_fns=direct_fns){
  options(scipen=999)
  require(lubridate)
  require(plyr)
  source(paste0(direct_fns, "Survey_and_OSAC/meat_count_shell_height_breakdown_figure.r"))
  
  direct.tmp <- direct
  load(data)
  direct <- direct.tmp
  banks <- names(bank.dat)

  if(any(grepl(x=banks, pattern="GBa-")) & subarea==F) banks <- banks[-which(grepl(x=banks, pattern = "GBa-"))]

  fish.reg <- read.csv(paste(direct,"Data/Fishery_regulations_by_bank.csv",sep=""))

  possiblebanks <- data.frame(banks=c("BBn", "BBs", "Ger", "Mid", "Sab", "GB", "Ban", "BanIce", "GBa", "GBb"),
                                season=c(rep("spring", 8), rep("summer", 2)))
  bankcheck <- data.frame(banks, year=year)
  bankcheck <- plyr::join(bankcheck, possiblebanks, type="full")
  bankcheck$word[bankcheck$banks=="BBn"] <- "Browns Bank North"
  bankcheck$word[bankcheck$banks=="BBs"] <- "Browns Bank South"
  bankcheck$word[bankcheck$banks=="Ger"] <- "German Bank"
  bankcheck$word[bankcheck$banks=="Mid"] <- "Middle Bank"
  bankcheck$word[bankcheck$banks=="Sab"] <- "Sable Bank"
  bankcheck$word[bankcheck$banks=="GB"] <- "Georges Bank"
  bankcheck$word[bankcheck$banks=="Ban"] <- "Banquereau Bank (Sea Scallop)"
  bankcheck$word[bankcheck$banks=="BanIce"] <- "Banquereau Bank (Icelandic)"
  bankcheck$word[bankcheck$banks=="GBa"] <- "Georges Bank 'a'"
  bankcheck$word[bankcheck$banks=="GBb"] <- "Georges Bank 'b'"

  bankcheck <<- bankcheck[bankcheck$season==reportseason,]

  ntows <- NULL
  highlights <- NULL
  sizes <- NULL
  spatial.sum.stats <- NULL
  dates <- NULL
  yeartable <- NULL
  for (i in 1:length(banks)){

    if(!banks[i] == "Ger") years <- survey.obj[[banks[i]]]$model.dat$year[!is.na(survey.obj[[banks[i]]]$model.dat$n)]
    if(banks[i] == "Ger") years <- lined.survey.obj$model.dat$year[!is.na(lined.survey.obj$model.dat$n)]
    
    lastyear <- years[which(years==year)-1]
    
    yeartable <- rbind(yeartable, 
                       data.frame(bank=banks[i], lastyear = lastyear, currentyear=year))

    # must use surv.Live instead of surv.Rand for German!!
    if(banks[i] == "Ger") surv.Rand$Ger <- surv.Live$Ger

    sched <- data.frame(start = min(surv.dat[banks[i]][[1]]$date[surv.dat[banks[i]][[1]]$year==year]),
                        end = max(surv.dat[banks[i]][[1]]$date[surv.dat[banks[i]][[1]]$year==year]),
                        bank = banks[i])
    
    dates <- rbind(dates, sched)
    
    size<-NULL
    size$RS <- survey.obj[banks[i]][[1]]$model.dat$RS[survey.obj[banks[i]][[1]]$model.dat$year==max(survey.obj[banks[i]][[1]]$model.dat$year)]
    size$CS <- survey.obj[banks[i]][[1]]$model.dat$CS[survey.obj[banks[i]][[1]]$model.dat$year==max(survey.obj[banks[i]][[1]]$model.dat$year)]
    if(!banks[i] == "GB") size$mc <- unique(fish.reg$MC_reg[fish.reg$Bank==banks[i] & fish.reg$year==year])
    if(banks[i] == "GB") size$mc <- unique(fish.reg$MC_reg[fish.reg$Bank=="GBa" & fish.reg$year==year])

    size$bank <- banks[i]

    sizes <- rbind(sizes, size)

    #print(i)
    # number of tows:

    if(class(surv.dat[banks[i]][[1]]) == "SpatialPointsDataFrame") surv.dat[banks[i]][[1]] <- as.data.frame(surv.dat[banks[i]][[1]])
    ntowsy <- as.data.frame(table(unique(surv.dat[banks[i]][[1]][surv.dat[banks[i]][[1]]$year==year, c("tow", "random")])$random))
    ntowsy$type[ntowsy$Var1 == 1] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 1], "1", "fixed_regular")
    ntowsy$type[ntowsy$Var1 == 2] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 2], "2", "exploratory_2")
    ntowsy$type[ntowsy$Var1 == 3] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 3], "3", "repeated_spr")
    ntowsy$type[ntowsy$Var1 == 4] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 4], "4", "other")
    ntowsy$type[ntowsy$Var1 == 5] <- gsub(x=ntowsy$Var1[ntowsy$Var1 == 5], "5", "exploratory_5")
    ntowsy$type[!ntowsy$Var1 %in% 1:5] <- "unclassified"
    ntowsy$bank <- banks[i]

    ntows <- rbind(ntows, ntowsy)

    # number per tow
    if(banks[i] %in% c("Mid", "GB", "Ger","Ban", "BanIce")){
      NPR_current <- SS.summary[banks[i]][[1]]$NPR[SS.summary[banks[i]][[1]]$year==year]
      NPR_prev <- SS.summary[banks[i]][[1]]$NPR[SS.summary[banks[i]][[1]]$year==lastyear]
      NR_current <- SS.summary[banks[i]][[1]]$NR[SS.summary[banks[i]][[1]]$year==year]
      NR_prev <- SS.summary[banks[i]][[1]]$NR[SS.summary[banks[i]][[1]]$year==lastyear]
      N_current <- SS.summary[banks[i]][[1]]$N[SS.summary[banks[i]][[1]]$year==year]
      N_prev <- SS.summary[banks[i]][[1]]$N[SS.summary[banks[i]][[1]]$year==lastyear]
      NPR_LTM <- median(SS.summary[banks[i]][[1]]$NPR, na.rm=T)
      NR_LTM <- median(SS.summary[banks[i]][[1]]$NR, na.rm=T)
      N_LTM <- median(SS.summary[banks[i]][[1]]$N, na.rm=T)

      if(dim(SS.summary[banks[i]][[1]][SS.summary[banks[i]][[1]]$year==lastyear,])[1]==0){
        NPR_prev <- NA
        NR_prev <- NA
        N_prev <- NA
      }
    }

    if(banks[i] %in% c("Sab", "BBn", "GBa", "GBb") | grepl(x=banks[i], pattern="GBa")){
      NPR_current <- survey.obj[banks[i]][[1]]$bankpertow$NPR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      NPR_prev <- survey.obj[banks[i]][[1]]$bankpertow$NPR[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      NR_current <- survey.obj[banks[i]][[1]]$bankpertow$NR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      NR_prev <- survey.obj[banks[i]][[1]]$bankpertow$NR[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      N_current <- survey.obj[banks[i]][[1]]$bankpertow$N[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      N_prev <- survey.obj[banks[i]][[1]]$bankpertow$N[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      NPR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$NPR, na.rm=T)
      NR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$NR, na.rm=T)
      N_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$N, na.rm=T)

      if(dim(survey.obj[banks[i]][[1]]$bankpertow[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear,])[1]==0){
        NPR_prev <- NA
        NR_prev <- NA
        N_prev <- NA
      }
    }

    # BBs is only surveyed every other year, so change years to year-2
    if(banks[i] %in% c("BBs")){
      NPR_current <- survey.obj[banks[i]][[1]]$bankpertow$NPR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      NPR_prev <- survey.obj[banks[i]][[1]]$bankpertow$NPR[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      NR_current <- survey.obj[banks[i]][[1]]$bankpertow$NR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      NR_prev <- survey.obj[banks[i]][[1]]$bankpertow$NR[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      N_current <- survey.obj[banks[i]][[1]]$bankpertow$N[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      N_prev <- survey.obj[banks[i]][[1]]$bankpertow$N[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      NPR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$NPR, na.rm=T)
      NR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$NR, na.rm=T)
      N_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$N, na.rm=T)

      if(dim(survey.obj[banks[i]][[1]]$bankpertow[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear,])[1]==0){
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
    abundPT$word <- ifelse(abundPT$thisyear/abundPT$lastyear >  1.10,
                           "increased",
                           ifelse(abundPT$thisyear/abundPT$lastyear < 0.90 & abundPT$thisyear/abundPT$lastyear > 0.10,
                                  "decreased",
                                  ifelse(abundPT$thisyear/abundPT$lastyear < 0.10 |
                                           (abundPT$thisyear/abundPT$lastyear > 0.90 &
                                              abundPT$thisyear/abundPT$lastyear < 1.10),
                                         "similar", "other")))
    abundPT$nearLTM <- ifelse(abundPT$thisyear/abundPT$LTM >  1.10,
                              "above",
                              ifelse(abundPT$thisyear/abundPT$LTM < 0.90 & abundPT$thisyear/abundPT$LTM > 0.10,
                                     "below",
                                     ifelse(abundPT$thisyear/abundPT$LTM < 0.10 |
                                              (abundPT$thisyear/abundPT$LTM > 0.90 &
                                                 abundPT$thisyear/abundPT$LTM < 1.10),
                                            "near", "other")))

    abundPT$bank <- banks[i]

    # biomass per tow
    if(banks[i] %in% c("Mid", "GB", "Ger", "Ban", "BanIce")){
      IPR_current <- SS.summary[banks[i]][[1]]$IPR[SS.summary[banks[i]][[1]]$year==year]
      IPR_prev <- SS.summary[banks[i]][[1]]$IPR[SS.summary[banks[i]][[1]]$year==lastyear]
      IR_current <- SS.summary[banks[i]][[1]]$IR[SS.summary[banks[i]][[1]]$year==year]
      IR_prev <- SS.summary[banks[i]][[1]]$IR[SS.summary[banks[i]][[1]]$year==lastyear]
      I_current <- SS.summary[banks[i]][[1]]$I[SS.summary[banks[i]][[1]]$year==year]
      I_prev <- SS.summary[banks[i]][[1]]$I[SS.summary[banks[i]][[1]]$year==lastyear]
      IPR_LTM <- median(SS.summary[banks[i]][[1]]$IPR, na.rm=T)
      IR_LTM <- median(SS.summary[banks[i]][[1]]$IR, na.rm=T)
      I_LTM <- median(SS.summary[banks[i]][[1]]$I, na.rm=T)

      if(dim(SS.summary[banks[i]][[1]][SS.summary[banks[i]][[1]]$year==lastyear,])[1]==0){
        IPR_prev <- NA
        IR_prev <- NA
        I_prev <- NA
      }
    }

    if(banks[i] %in% c("Sab", "BBn", "GBa", "GBb")| grepl(x=banks[i], pattern="GBa")){
      IPR_current <- survey.obj[banks[i]][[1]]$bankpertow$IPR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      IPR_prev <- survey.obj[banks[i]][[1]]$bankpertow$IPR[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      IR_current <- survey.obj[banks[i]][[1]]$bankpertow$IR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      IR_prev <- survey.obj[banks[i]][[1]]$bankpertow$IR[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      I_current <- survey.obj[banks[i]][[1]]$bankpertow$I[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      I_prev <- survey.obj[banks[i]][[1]]$bankpertow$I[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      IPR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$IPR, na.rm=T)
      IR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$IR, na.rm=T)
      I_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$I, na.rm=T)

      if(dim(survey.obj[banks[i]][[1]]$bankpertow[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear,])[1]==0){
        IPR_prev <- NA
        IR_prev <- NA
        I_prev <- NA
      }
    }

    if(banks[i] %in% c("BBs")){
      IPR_current <- survey.obj[banks[i]][[1]]$bankpertow$IPR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      IPR_prev <- survey.obj[banks[i]][[1]]$bankpertow$IPR[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      IR_current <- survey.obj[banks[i]][[1]]$bankpertow$IR[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      IR_prev <- survey.obj[banks[i]][[1]]$bankpertow$IR[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      I_current <- survey.obj[banks[i]][[1]]$bankpertow$I[survey.obj[banks[i]][[1]]$bankpertow$year==year]
      I_prev <- survey.obj[banks[i]][[1]]$bankpertow$I[survey.obj[banks[i]][[1]]$bankpertow$year==lastyear]
      IPR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$IPR, na.rm=T)
      IR_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$IR, na.rm=T)
      I_LTM <- median(survey.obj[banks[i]][[1]]$bankpertow$I, na.rm=T)

      if(dim(survey.obj[banks[i]][[1]]$bankpertow[survey.obj[[1]]$bankpertow$year==lastyear,])[1]==0){
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
    bmPT$word <- ifelse(bmPT$thisyear/bmPT$lastyear >  1.10,
                        "increased",
                        ifelse(bmPT$thisyear/bmPT$lastyear < 0.90 & bmPT$thisyear/bmPT$lastyear > 0.10,
                               "decreased",
                               ifelse(bmPT$thisyear/bmPT$lastyear < 0.10 |
                                        (bmPT$thisyear/bmPT$lastyear > 0.90 &
                                           bmPT$thisyear/bmPT$lastyear < 1.10),
                                      "similar", "other")))

    bmPT$nearLTM <- ifelse(bmPT$thisyear/bmPT$LTM >  1.10,
                           "above",
                           ifelse(bmPT$thisyear/bmPT$LTM < 0.90 & bmPT$thisyear/bmPT$LTM > 0.10,
                                  "below",
                                  ifelse(bmPT$thisyear/bmPT$LTM < 0.10 |
                                           (bmPT$thisyear/bmPT$LTM > 0.90 &
                                              bmPT$thisyear/bmPT$LTM < 1.10),
                                         "near", "other")))

    bmPT$bank <- banks[i]

    highlights <- rbind(highlights, abundPT)
    highlights <- rbind(highlights, bmPT)

    # shell height frequencies
    shsummary <- apply(surv.Rand[banks[i]][[1]][surv.Rand[banks[i]][[1]]$year==year, 14:53], 2, mean)
    maxbin <- names(shsummary[shsummary==max(shsummary)])
    maxbin <- gsub(x=maxbin, "h", "")
    maxbin <- paste0(as.numeric(maxbin)-5, "-", maxbin)

    if(!banks[i] == "BBs"){
      shsummary_LY <- apply(surv.Rand[banks[i]][[1]][surv.Rand[banks[i]][[1]]$year==lastyear, 14:53], 2, mean)
      maxbin_LY <- names(shsummary_LY[shsummary_LY==max(shsummary_LY)])
      maxbin_LY <- gsub(x=maxbin_LY, "h", "")
      maxbin_LY <- paste0(as.numeric(maxbin_LY)-5, "-", maxbin_LY)
      }
    if(banks[i] == "BBs"){
      shsummary_LY <- apply(surv.Rand[banks[i]][[1]][surv.Rand[banks[i]][[1]]$year==lastyear, 14:53], 2, mean)
      maxbin_LY <- names(shsummary_LY[shsummary_LY==max(shsummary_LY)])
      maxbin_LY <- gsub(x=maxbin_LY, "h", "")
      maxbin_LY <- paste0(as.numeric(maxbin_LY)-5, "-", maxbin_LY)
      }

    if(file.exists(paste0(direct, "Data/Survey_data/", year, "/Survey_summary_output/", banks[i], "_figures_res_250-250.Rdata"))){
      load(paste0(direct, "Data/Survey_data/", year, "/Survey_summary_output/", banks[i], "_figures_res_250-250.Rdata"))
      fitted.x <- fitted
    }
    if(!file.exists(paste0(direct, "Data/Survey_data/", year, "/Survey_summary_output/", banks[i], "_figures_res_250-250.Rdata"))){
      fitted.x <- NULL
    }

    maxPRtow <- max(surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==year])
    if(!banks[i]=="BBs") maxPRtow_LY <- max(surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==lastyear])
    if(banks[i]=="BBs") maxPRtow_LY <- max(surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==lastyear])
    PR3Q <- round(summary(surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==year])[5], -1)
    PR75 <- c(quantile(x=surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==year], c(0.125, 0.5, 0.875, 1))[1], quantile(x=surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==year], c(0.125, 0.5, 0.875, 1))[3])
    PR75_LY <- c(quantile(x=surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==lastyear], c(0.125, 0.5, 0.875, 1))[1], quantile(x=surv.Rand[banks[i]][[1]]$pre[surv.Rand[banks[i]][[1]]$year==lastyear], c(0.125, 0.5, 0.875, 1))[3])
    PR75 <- paste0(round_any(PR75[1], 5), "-", round_any(PR75[2], 5))
    PR75_LY <- paste0(round_any(PR75_LY[1], 5), "-", round_any(PR75_LY[2], 5))
    if(!is.null(fitted.x)) PR75_f <- c(quantile(x=fitted.x$`PR-spatial`$fitted, c(0.125, 0.5, 0.875, 1))[1], quantile(x=fitted.x$`PR-spatial`$fitted, c(0.125, 0.5, 0.875, 1))[3])
    if(!is.null(fitted.x)) PR75_f <- paste0(round_any(PR75_f[1], 5), "-", round_any(PR75_f[2], 5))
    if(is.null(fitted.x)) PR75_f <- NA
    PR75 <- paste0(PR75, " (INLA fit = ", PR75_f, ")")
    ntowsabovePR3Q <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$pre>PR3Q &
                                                                   surv.Rand[banks[i]][[1]]$year==year]) - 1)
    if(!banks[i]=="BBs") ntowsabovePR3Q_LY <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$pre>PR3Q &
                                                                   surv.Rand[banks[i]][[1]]$year==lastyear]) - 1)
    if(banks[i]=="BBs") ntowsabovePR3Q_LY <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$pre>PR3Q &
                                                                   surv.Rand[banks[i]][[1]]$year==lastyear]) - 1)

    maxRtow <- max(surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==year])
    if(!banks[i]=="BBs") maxRtow_LY <- max(surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==lastyear])
    if(banks[i]=="BBs") maxRtow_LY <- max(surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==lastyear])
    R3Q <- round(summary(surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==year])[5], -1)
    R75 <- c(quantile(x=surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==year], c(0.125, 0.5, 0.875, 1))[1], quantile(x=surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==year], c(0.125, 0.5, 0.875, 1))[3])
    R75_LY <- c(quantile(x=surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==lastyear], c(0.125, 0.5, 0.875, 1))[1], quantile(x=surv.Rand[banks[i]][[1]]$rec[surv.Rand[banks[i]][[1]]$year==lastyear], c(0.125, 0.5, 0.875, 1))[3])
    R75 <- paste0(round_any(R75[1], 5), "-", round_any(R75[2], 5))
    R75_LY <- paste0(round_any(R75_LY[1], 5), "-", round_any(R75_LY[2], 5))
    if(!is.null(fitted.x)) R75_f <- c(quantile(x=fitted.x$`Rec-spatial`$fitted, c(0.125, 0.5, 0.875, 1))[1], quantile(x=fitted.x$`Rec-spatial`$fitted, c(0.125, 0.5, 0.875, 1))[3])
    if(!is.null(fitted.x)) R75_f <- paste0(round_any(R75_f[1], 5), "-", round_any(R75_f[2], 5))
    if(is.null(fitted.x)) R75_f <- NA
    R75 <- paste0(R75, " (INLA fit = ", R75_f, ")")
    ntowsaboveR3Q <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$rec>R3Q &
                                                                  surv.Rand[banks[i]][[1]]$year==year]) - 1)
    if(!banks[i]=="BBs") ntowsaboveR3Q_LY <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$rec>R3Q &
                                                                  surv.Rand[banks[i]][[1]]$year==lastyear]) - 1)
    if(banks[i]=="BBs") ntowsaboveR3Q_LY <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$rec>R3Q &
                                                                                          surv.Rand[banks[i]][[1]]$year==lastyear]) - 1)
    maxCtow <- max(surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==year])
    if(!banks[i]=="BBs") maxCtow_LY <- max(surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==lastyear])
    if(banks[i]=="BBs") maxCtow_LY <- max(surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==lastyear])
    C3Q <- round(summary(surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==year])[5], -1)
    C75 <- c(quantile(x=surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==year], c(0.125, 0.5, 0.875, 1))[1], quantile(x=surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==year], c(0.125, 0.5, 0.875, 1))[3])
    C75_LY <- c(quantile(x=surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==lastyear], c(0.125, 0.5, 0.875, 1))[1], quantile(x=surv.Rand[banks[i]][[1]]$com[surv.Rand[banks[i]][[1]]$year==lastyear], c(0.125, 0.5, 0.875, 1))[3])
    C75 <- paste0(round_any(C75[1], 5), "-", round_any(C75[2], 5))
    C75_LY <- paste0(round_any(C75_LY[1], 5), "-", round_any(C75_LY[2], 5))
    if(!is.null(fitted.x)) C75_f <- c(quantile(x=fitted.x$`FR-spatial`$fitted, c(0.125, 0.5, 0.875, 1))[1], quantile(x=fitted.x$`FR-spatial`$fitted, c(0.125, 0.5, 0.875, 1))[3])
    if(!is.null(fitted.x)) C75_f <- paste0(round_any(C75_f[1], 5), "-", round_any(C75_f[2], 5))
    if(is.null(fitted.x)) C75_f <- NA
    C75 <- paste0(C75, " (INLA fit = ", C75_f, ")")
    ntowsaboveC3Q <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$com>C3Q &
                                                                  surv.Rand[banks[i]][[1]]$year==year]) - 1)
    if(!banks[i]=="BBs") ntowsaboveC3Q_LY <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$com>C3Q &
                                                                  surv.Rand[banks[i]][[1]]$year==lastyear]) - 1)
    if(banks[i]=="BBs") ntowsaboveC3Q_LY <- length(unique(surv.Rand[banks[i]][[1]]$tow[surv.Rand[banks[i]][[1]]$com>C3Q &
                                                                                          surv.Rand[banks[i]][[1]]$year==lastyear]) - 1)

    spatial.sum.stats.b <- t(apply(surv.Rand[banks[i]][[1]][surv.Rand[banks[i]][[1]]$year==year, c("pre", "rec", "com")], 2, summary))[,c(1,3,4,6)]
    spatial.sum.stats.b <- as.data.frame(apply(spatial.sum.stats.b, 2, function(x) round(x, 2)))
    spatial.sum.stats.b$bank <- banks[i]
    spatial.sum.stats.b$year <- year

    spatial.sum.stats$abund <- rbind(spatial.sum.stats$abund, spatial.sum.stats.b)

    # size range quartiles. This outputs a range that includes 75% of the scallops
    # total number per tow caught this year
    if(!banks[i] == "Ger"){
      df <- as.data.frame(round(survey.obj[[banks[i]]]$shf.dat$n.yst))
      df$year <- survey.obj[[banks[i]]][[1]]$year
    }
    if(banks[i] == "Ger"){
      df <- as.data.frame(round(lined.survey.obj$shf.dat$n.yst))
      df$year <- lined.survey.obj[[1]]$year
    }

    sizerange75 <- NULL
    sizerange75PR <- NULL
    sizerange75Rec <- NULL
    sizerange75FR <- NULL

    for(y in c(lastyear, year)){
      shf.ty <- as.data.frame(t(df[df$year==y, which(!names(df) %in% c("year", "years"))]))
      shf.ty$bin <- seq(0,195,5)
      shf.ty$size <- cut(shf.ty$bin, c(0,size$RS, size$CS, 200), include.lowest = T, right = F)
      sizeclass <- data.frame(size=unique(shf.ty$size), class=c("PR", "Rec", "FR"))
      shf.ty <- join(shf.ty, sizeclass, type="full")
      names(shf.ty) <- c("npertow", "bin", "size", "class")
      expanded <- shf.ty[rep(seq_len(nrow(shf.ty)), shf.ty$npertow), 1:2]
      #hist(expanded$bin)
      expanded <- join(expanded, shf.ty[, c("bin", "class")])
      sevfiveperc <- c(quantile(x=expanded$bin, c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin, c(0.125, 0.5, 0.875, 1))[3])
      sizerange75[paste0(y)] <- paste0(round_any(sevfiveperc[1], 5), "-", round_any(sevfiveperc[2], 5))
      sevfivepercPR <- c(quantile(x=expanded$bin[expanded$class %in% c("PR")], c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin[expanded$class %in% c("PR")], c(0.125, 0.5, 0.875, 1))[3])
      sizerange75PR[paste0(y)] <- paste0(round_any(sevfivepercPR[1], 5), "-", round_any(sevfivepercPR[2], 5))
      sevfivepercRec <- c(quantile(x=expanded$bin[expanded$class %in% c("Rec")], c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin[expanded$class %in% c("Rec")], c(0.125, 0.5, 0.875, 1))[3])
      sizerange75Rec[paste0(y)] <- paste0(round_any(sevfivepercRec[1], 5), "-", round_any(sevfivepercRec[2], 5))
      sevfivepercFR <- c(quantile(x=expanded$bin[expanded$class %in% c("FR")], c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin[expanded$class %in% c("FR")], c(0.125, 0.5, 0.875, 1))[3])
      sizerange75FR[paste0(y)] <- paste0(round_any(sevfivepercFR[1], 5), "-", round_any(sevfivepercFR[2], 5))
    }

    # breakdown plot biomass size ranges
    # size range quartiles. This outputs a range that includes 75% of the scallops
    # total number per tow caught this year

if(!banks[i] == "GB") mcreg <- fish.reg$MC_reg[fish.reg$Bank==banks[i] & fish.reg$year==y]
if(banks[i] == "GB") mcreg <- fish.reg$MC_reg[fish.reg$Bank=="GBa" & fish.reg$year==y]

    if(!banks[i] %in% c("Ger", "BanIce")){
      df <- as.data.frame(round(survey.obj[[banks[i]]]$shf.dat$w.yst))
      df$year <- survey.obj[[banks[i]]][[1]]$year
      sht.cnt <- breakdown(survey.obj[[banks[i]]],yr=y,mc=mcreg,add.title = F, value=T)
    }
    if(banks[i] == "Ger"){
      df <- as.data.frame(round(lined.survey.obj$shf.dat$w.yst))
      df$year <- lined.survey.obj[[1]]$year
      sht.cnt <- breakdown(lined.survey.obj,yr=y,mc=fish.reg$MC_reg[fish.reg$Bank==banks[i] & fish.reg$year==y], add.title = F, value=T)
    }
    sizerange75_bm_65up <- NULL
    for(y in c(lastyear, year)){
      shf.ty <- as.data.frame(t(df[df$year==y, which(!names(df) %in% c("year", "years"))]))
      shf.ty$bin <- seq(0,195,5)
      names(shf.ty) <- c("npertow", "bin")
      shf.ty <- shf.ty[shf.ty$bin > 60,]
      expanded <- shf.ty[rep(seq_len(nrow(shf.ty)), shf.ty$npertow), 1:2]
      #hist(expanded$bin)
      sevfiveperc <- c(quantile(x=expanded$bin, c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin, c(0.125, 0.5, 0.875, 1))[3])
      sizerange75_bm_65up[paste0(y)] <- paste0(round_any(sevfiveperc[1], 5), "-", round_any(sevfiveperc[2], 5))
    }

    print('check1')

    if(!banks[i] %in% "BanIce") towsummary <- data.frame(variable=c("maxbin", "maxPRtow", "maxRtow", "maxCtow", "PR3Q", "R3Q", "C3Q", "PR75", "R75", "C75", "sizerange75", "sizerange75PR", "sizerange75Rec", "sizerange75FR", "sizerange75_bm_65up", "sh_for_mcreg"),
                             lastyear=c(max(shsummary_LY), maxPRtow_LY, maxRtow_LY, maxCtow_LY, PR3Q, R3Q, C3Q, PR75_LY, R75_LY, C75_LY, sizerange75[paste0(lastyear)], sizerange75PR[paste0(lastyear)], sizerange75Rec[paste0(lastyear)], sizerange75FR[paste0(lastyear)], sizerange75_bm_65up[paste0(lastyear)], NA),
                             thisyear=c(max(shsummary), maxPRtow, maxRtow, maxCtow, PR3Q, R3Q, C3Q, PR75, R75, C75, sizerange75[paste0(year)], sizerange75PR[paste0(year)], sizerange75Rec[paste0(year)], sizerange75FR[paste0(year)], sizerange75_bm_65up[paste0(year)], round(sht.cnt$sht.cnt, 3)),
                             LTM=NA,
                             word=c(paste0(maxbin, "(LY=", maxbin, ")"), NA, NA, NA,
                                    paste0(ntowsabovePR3Q, " tows (LY=", ntowsabovePR3Q_LY, " tows)"),
                                    paste0(ntowsaboveR3Q, " tows (LY=", ntowsaboveR3Q_LY, " tows)"),
                                    paste0(ntowsaboveC3Q, " tows (LY=", ntowsaboveC3Q_LY, " tows)"), NA, NA, NA, NA, NA, NA, NA, NA, NA),
                             nearLTM=NA,
                             bank=banks[i])

    if(banks[i] %in% "BanIce") towsummary <- data.frame(variable=c("maxbin", "maxPRtow", "maxRtow", "maxCtow", "PR3Q", "R3Q", "C3Q", "PR75", "R75", "C75", "sizerange75", "sizerange75PR", "sizerange75Rec", "sizerange75FR", "sizerange75_bm_65up", "sh_for_mcreg"),
                             lastyear=c(max(shsummary_LY), maxPRtow_LY, maxRtow_LY, maxCtow_LY, PR3Q, R3Q, C3Q, PR75_LY, R75_LY, C75_LY,sizerange75[paste0(lastyear)], sizerange75PR[paste0(lastyear)], sizerange75Rec[paste0(lastyear)], sizerange75FR[paste0(lastyear)], sizerange75_bm_65up[paste0(lastyear)], NA),
                             thisyear=c(max(shsummary), maxPRtow, maxRtow, maxCtow, PR3Q, R3Q, C3Q, PR75, R75, C75, sizerange75[paste0(year)], sizerange75PR[paste0(year)],  sizerange75Rec[paste0(year)], sizerange75FR[paste0(year)], sizerange75_bm_65up[paste0(year)], NA),
                             LTM=NA,
                            word=NA,
                             nearLTM=NA,
                             bank=banks[i])

    highlights <- rbind(highlights, towsummary)

    # mwsh and cf

    fittedmw100mm <- 1^SpatHtWt.fit[banks[i]][[1]]$B * SpatHtWt.fit[banks[i]][[1]]$A

    if(!banks[i] =="Ger"){
      cfdat <- survey.obj[banks[i]][[1]]$model.dat[,c("year", "CF")]
    }

    if(banks[i] == "Ger"){
      cfdat <- cf.data[banks[i]][[1]]$CFyrs[,c("year", "CF2")]
      names(cfdat) <- c("year", "CF")
    }

    if(dim(cfdat[cfdat$year==lastyear & !is.na(cfdat$year),])[1]>0){

      cfdat$CF[is.nan(cfdat$CF)] <- NA

      cf_ltm <- median(cfdat$CF[!is.na(cfdat$year) & !cfdat$year == year], na.rm=T)

      cf <- data.frame(variable=c("CF",
                                "spatialCF",
                                "minCF",
                                "maxCF"),
                     lastyear=c(cfdat$CF[!is.na(cfdat$year) & cfdat$year==lastyear],
                                median(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==lastyear]),
                                min(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==lastyear]),
                                max(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==lastyear])),
                     thisyear=c(cfdat$CF[!is.na(cfdat$year) & cfdat$year==year],
                                median(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
                                min(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
                                max(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year])),
                     LTM=cf_ltm)
    }

    if(banks[i] %in% "BBs"){

      cf_ltm <- median(cfdat$CF[!is.na(cfdat$year)])

      cf <- data.frame(variable=c("CF",
                                  "spatialCF",
                                  "minCF",
                                  "maxCF"),
                       lastyear=c(cfdat$CF[!is.na(cfdat$year) & cfdat$year==lastyear],
                                  median(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==lastyear]),
                                  min(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==lastyear]),
                                  max(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==lastyear])),
                       thisyear=c(cfdat$CF[!is.na(cfdat$year) & cfdat$year==year],
                                  median(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
                                  min(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year]),
                                  max(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year==year])),
                       LTM=cf_ltm)
    }

spatial.sum.stats.c <- as.data.frame(rbind(summary(cf.data[banks[i]][[1]]$CF.data$CF[cf.data[banks[i]][[1]]$CF.data$year == year])[c(1,3,4,6)]))
spatial.sum.stats.c <- as.data.frame(t(apply(spatial.sum.stats.c, 2, function(x) round(x, 2))))
spatial.sum.stats.c$bank <- banks[i]
spatial.sum.stats.c$year <- year

spatial.sum.stats$cf <- rbind(spatial.sum.stats$cf, spatial.sum.stats.c)

    print('check2')
    # if(dim(cfdat[cfdat$year==lastyear & !is.na(cfdat$year),])[1]==0){
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
    ltmtest <- ifelse(cf$thisyear > cf$LTM, "greater than",
                      ifelse(cf$thisyear < cf$LTM, "less than", NA))

    cf$nearLTM <- paste0(ltmtest, " (LTM=", round(cf$LTM,2), ")")

    mwshcf <- rbind(data.frame(variable=c("fittedmw100mm"),
                               lastyear=NA,
                               thisyear=fittedmw100mm,
                               LTM=NA,
                               word=NA,
                               nearLTM=NA),
                    cf)
    mwshcf$bank <- banks[i]

    highlights <- rbind(highlights, mwshcf)

    # growth potential stuff
    growpot <- data.frame(variable=c("minSH", "maxSH", "meanSH",
                                     "minSH.GP", "maxSH.GP", "meanSH.GP",
                                     "minMW", "maxMW", "meanMW",
                                     "minMW.GP", "maxMW.GP", "meanMW.GP"),
                          lastyear=NA,
                          thisyear=c(min(pot.grow[[banks[i]]]$cur.sh[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     max(pot.grow[[banks[i]]]$cur.sh[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     mean(pot.grow[[banks[i]]]$cur.sh[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     min(pot.grow[[banks[i]]]$gp.sh[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     max(pot.grow[[banks[i]]]$gp.sh[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     mean(pot.grow[[banks[i]]]$gp.sh[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     min(pot.grow[[banks[i]]]$cur.mw[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     max(pot.grow[[banks[i]]]$cur.mw[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     mean(pot.grow[[banks[i]]]$cur.mw[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     min(pot.grow[[banks[i]]]$gp.mw[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     max(pot.grow[[banks[i]]]$gp.mw[pot.grow[[banks[i]]]$year == year], na.rm=T),
                                     mean(pot.grow[[banks[i]]]$gp.mw[pot.grow[[banks[i]]]$year == year], na.rm=T)),
                                     LTM=NA,
                                     word=NA,
                                     nearLTM=NA,
                                     bank=banks[i])


    highlights <- rbind(highlights, growpot)
    print('check3')
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

    spatial.sum.stats.m <- as.data.frame(rbind(summary(CF.current[[banks[i]]]$meat.count)[c(1,3,4,6)]))
    spatial.sum.stats.m <- as.data.frame(t(apply(spatial.sum.stats.m, 2, function(x) round(x, 2))))
    spatial.sum.stats.m$bank <- banks[i]
    spatial.sum.stats.m$year <- year

    spatial.sum.stats$mc <- rbind(spatial.sum.stats$mc, spatial.sum.stats.m)

    highlights <- rbind(highlights, mc)
    
    # clapper abundance
    
    # for proportional LTMs (as in Clap3.plt.R)
    ts <- aggregate(clap.propPre~year,surv.Clap.Rand[[banks[i]]],mean)
    ts$clap.propRec <- aggregate(clap.propRec~year,surv.Clap.Rand[[banks[i]]],mean)$clap.propRec
    ts$clap.propCom <- aggregate(clap.propCom~year,surv.Clap.Rand[[banks[i]]],mean)$clap.propCom
    
    if(!banks[i] %in% "BBs"){
    clap <- data.frame(variable=c("NPRclap", "NRclap", "Nclap",
                                  "PRpercentclap", "Rpercentclap", "Cpercentclap"),
                       lastyear=c(clap.survey.obj[banks[i]][[1]]$bankpertow$NPR[clap.survey.obj[banks[i]][[1]]$bankpertow$year==lastyear],
                                  clap.survey.obj[banks[i]][[1]]$bankpertow$NR[clap.survey.obj[banks[i]][[1]]$bankpertow$year==lastyear],
                                  clap.survey.obj[banks[i]][[1]]$bankpertow$N[clap.survey.obj[banks[i]][[1]]$bankpertow$year==lastyear],
                                  ts$clap.propPre[ts$year==lastyear],
                                  ts$clap.propRec[ts$year==lastyear],
                                  ts$clap.propCom[ts$year==lastyear]),
                       thisyear=c(clap.survey.obj[banks[i]][[1]]$bankpertow$NPR[clap.survey.obj[banks[i]][[1]]$bankpertow$year==year],
                                  clap.survey.obj[banks[i]][[1]]$bankpertow$NR[clap.survey.obj[banks[i]][[1]]$bankpertow$year==year],
                                  clap.survey.obj[banks[i]][[1]]$bankpertow$N[clap.survey.obj[banks[i]][[1]]$bankpertow$year==year],
                                  ts$clap.propPre[ts$year==year],
                                  ts$clap.propRec[ts$year==year],
                                  ts$clap.propCom[ts$year==year]),
                       LTM=c(median(clap.survey.obj[banks[i]][[1]]$bankpertow$NPR[!clap.survey.obj[banks[i]][[1]]$bankpertow$year==year], na.rm=T),
                             median(clap.survey.obj[banks[i]][[1]]$bankpertow$NR[!clap.survey.obj[banks[i]][[1]]$bankpertow$year==year], na.rm=T),
                             median(clap.survey.obj[banks[i]][[1]]$bankpertow$N[!clap.survey.obj[banks[i]][[1]]$bankpertow$year==year], na.rm=T),
                             median(ts$clap.propPre[!ts$year==year], na.rm=T),
                             median(ts$clap.propRec[!ts$year==year], na.rm=T),
                             median(ts$clap.propCom[!ts$year==year], na.rm=T)))
    }

    if(banks[i] %in% "BBs"){
      clap <- data.frame(variable=c("NPRclap", "NRclap", "Nclap",
                                    "PRpercentclap", "Rpercentclap", "Cpercentclap"),
                         lastyear=c(clap.survey.obj[banks[i]][[1]]$bankpertow$NPR[clap.survey.obj[banks[i]][[1]]$bankpertow$year==lastyear],
                                    clap.survey.obj[banks[i]][[1]]$bankpertow$NR[clap.survey.obj[banks[i]][[1]]$bankpertow$year==lastyear],
                                    clap.survey.obj[banks[i]][[1]]$bankpertow$N[clap.survey.obj[banks[i]][[1]]$bankpertow$year==lastyear],
                                    ts$clap.propPre[ts$year==lastyear],
                                    ts$clap.propRec[ts$year==lastyear],
                                    ts$clap.propCom[ts$year==lastyear]),
                         thisyear=c(clap.survey.obj[banks[i]][[1]]$bankpertow$NPR[clap.survey.obj[banks[i]][[1]]$bankpertow$year==year],
                                    clap.survey.obj[banks[i]][[1]]$bankpertow$NR[clap.survey.obj[banks[i]][[1]]$bankpertow$year==year],
                                    clap.survey.obj[banks[i]][[1]]$bankpertow$N[clap.survey.obj[banks[i]][[1]]$bankpertow$year==year],
                                    ts$clap.propPre[ts$year==year],
                                    ts$clap.propRec[ts$year==year],
                                    ts$clap.propCom[ts$year==year]),
                         LTM=c(median(clap.survey.obj[banks[i]][[1]]$bankpertow$NPR[!clap.survey.obj[banks[i]][[1]]$bankpertow$year==year], na.rm=T),
                                    median(clap.survey.obj[banks[i]][[1]]$bankpertow$NR[!clap.survey.obj[banks[i]][[1]]$bankpertow$year==year], na.rm=T),
                                    median(clap.survey.obj[banks[i]][[1]]$bankpertow$N[!clap.survey.obj[banks[i]][[1]]$bankpertow$year==year], na.rm=T),
                                    median(ts$clap.propPre[!ts$year==year], na.rm=T),
                                    median(ts$clap.propRec[!ts$year==year], na.rm=T),
                                    median(ts$clap.propCom[!ts$year==year], na.rm=T)))
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
    print('check4')

    #seedboxes
    if(banks[i] %in% unique(names(seedbox.obj))){
      seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),
                           stringsAsFactors = F,header=T)
      seedboxes$Closed <- dmy(seedboxes$Closed)
      seedboxes$Open <- dmy(seedboxes$Open)
      # Dump the commments they are just messy..
      seedboxes <- seedboxes[,-grep("comment",names(seedboxes))]
      sb <- subset(seedboxes,Bank == banks[i] & Active=="Yes")
      if(dim(sb)[1] == 0) sb <- subset(seedboxes,Bank == banks[i] & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr-1,"-01-01",sep=""))
      if(banks[i] =="GB") sb <- subset(seedboxes,Bank %in% c("GBa","GBb") & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep=""))
      box.names <- unique(sb$SCALLOP_Group_ID)

      SeedPR_current<- NULL
      SeedPR_prev<- NULL
      SeedR_current<- NULL
      SeedR_prev<- NULL
      Seed_current<- NULL
      Seed_prev<- NULL
      bmSeedPR_current<- NULL
      bmSeedPR_prev<- NULL
      bmSeedR_current<- NULL
      bmSeedR_prev<- NULL
      bmSeed_current<- NULL
      bmSeed_prev<- NULL
      sizerange75_seed <- NULL
      sizerange75_seed_bm <- NULL
      towdat <- NULL
      PR75_seed <- NULL
      R75_seed <- NULL
      C75_seed <- NULL

      for(k in 1:length(seedbox.obj[banks[i]][[1]])) {
        boxy <- seedbox.obj[[banks[i]]][[k]]

        SeedPR_current[k] <- boxy$model.dat$NPR[boxy$model.dat$year==year]
        SeedPR_prev[k] <- boxy$model.dat$NPR[boxy$model.dat$year==lastyear]
        SeedR_current[k] <- boxy$model.dat$NR[boxy$model.dat$year==year]
        SeedR_prev[k] <- boxy$model.dat$NR[boxy$model.dat$year==lastyear]
        Seed_current[k] <- boxy$model.dat$N[boxy$model.dat$year==year]
        Seed_prev[k] <- boxy$model.dat$N[boxy$model.dat$year==lastyear]

        bmSeedPR_current[k] <- boxy$model.dat$IPR[boxy$model.dat$year==year]
        bmSeedPR_prev[k] <- boxy$model.dat$IPR[boxy$model.dat$year==lastyear]
        bmSeedR_current[k] <- boxy$model.dat$IR[boxy$model.dat$year==year]
        bmSeedR_prev[k] <- boxy$model.dat$IR[boxy$model.dat$year==lastyear]
        bmSeed_current[k] <- boxy$model.dat$I[boxy$model.dat$year==year]
        bmSeed_prev[k] <- boxy$model.dat$I[boxy$model.dat$year==lastyear]

        # box tow data for spatial figure comments
        towdat <- boxy$box.tow.data

        # seedbox size range quartiles. This outputs a range that includes 75% of the scallops
        # total number per tow caught this year
        df <- as.data.frame(boxy$shf.dat$n.yst)
        df$year <- boxy$model.dat$year
        df2 <- as.data.frame(round(boxy$shf.dat$w.yst))
        df2$year <- boxy$model.dat$year

        sizerange75_seed_y <- NULL
        sizerange75PR_seed_y <- NULL
        sizerange75FR_seed_y <- NULL
        sizerange75_seed_bm_y <- NULL
        sizerange75PR_seed_bm_y <- NULL
        sizerange75FR_seed_bm_y <- NULL
        PR75_seed_y <- NULL
        R75_seed_y <- NULL
        C75_seed_y <- NULL

         for(y in c(lastyear, year)){

           if(any(!is.na(df[df$year==y, !names(df) %in% c("year", "years")]))){
             if(dim(df[df$year==y,])[1]==0) sizerange75_seed_y[paste0(y)] <- NA
             if(dim(df[df$year==y,])[1]>0){
               shf.ty <- as.data.frame(t(df[df$year==y, which(!names(df) %in% c("year", "years"))]))
               shf.ty$bin <- seq(0,195,5)
               shf.ty$size <- cut(shf.ty$bin, c(0,size$RS, size$CS, 200), include.lowest = T, right = F)
               sizeclass <- data.frame(size=unique(shf.ty$size), class=c("PR", "Rec", "FR"))
               shf.ty <- join(shf.ty, sizeclass, type="full")
               names(shf.ty) <- c("npertow", "bin", "size", "class")
               expanded <- shf.ty[rep(seq_len(nrow(shf.ty)), shf.ty$npertow), 1:2]
               #hist(expanded$bin)
               expanded <- join(expanded, shf.ty[, c("bin", "class")])

               sevfiveperc <- c(quantile(x=expanded$bin, c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin, c(0.125, 0.5, 0.875, 1))[3])
               sizerange75_seed_y[paste0(y)] <- paste0(round_any(sevfiveperc[1], 5), "-", round_any(sevfiveperc[2], 5))
               sevfivepercPR <- c(quantile(x=expanded$bin[expanded$class %in% c("PR")], c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin[expanded$class %in% c("PR")], c(0.125, 0.5, 0.875, 1))[3])
               sizerange75PR_seed_y[paste0(y)] <- paste0(round_any(sevfivepercPR[1], 5), "-", round_any(sevfivepercPR[2], 5))
               sevfivepercFR <- c(quantile(x=expanded$bin[expanded$class %in% c("Rec", "FR")], c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin[expanded$class %in% c("Rec", "FR")], c(0.125, 0.5, 0.875, 1))[3])
               sizerange75FR_seed_y[paste0(y)] <- paste0(round_any(sevfivepercFR[1], 5), "-", round_any(sevfivepercFR[2], 5))
               #hist(expanded$bin)

               shf.bm.ty <- as.data.frame(t(df2[df2$year==y, which(!names(df2) %in% c("year", "years"))]))
               shf.bm.ty$bin <- seq(0,195,5)
               shf.bm.ty$size <- cut(shf.bm.ty$bin, c(0,size$RS, size$CS, 200), include.lowest = T, right = F)
               sizeclass <- data.frame(size=unique(shf.bm.ty$size), class=c("PR", "Rec", "FR"))
               shf.bm.ty <- join(shf.bm.ty, sizeclass, type="full")
               names(shf.bm.ty) <- c("npertow", "bin", "size", "class")
               shf.bm.ty <- shf.bm.ty[shf.bm.ty$bin > 60,]
               shf.bm.ty$npertow[is.na(shf.bm.ty$npertow)] <- 0
               expanded <- shf.bm.ty[rep(seq_len(nrow(shf.bm.ty)), shf.bm.ty$npertow), 1:2]
               expanded <- join(expanded, shf.bm.ty[, c("bin", "class")])
               #hist(expanded$bin)
               sevfiveperc <- c(quantile(x=expanded$bin, c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin, c(0.125, 0.5, 0.875, 1))[3])
               sizerange75_seed_bm_y[paste0(y)] <- paste0(round_any(sevfiveperc[1], 5), "-", round_any(sevfiveperc[2], 5))
               sevfiveperc <- c(quantile(x=expanded$bin[expanded$class %in% c("PR")], c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin[expanded$class %in% c("PR")], c(0.125, 0.5, 0.875, 1))[3])
               sizerange75PR_seed_bm_y[paste0(y)] <- paste0(round_any(sevfiveperc[1], 5), "-", round_any(sevfiveperc[2], 5))
               sevfiveperc <- c(quantile(x=expanded$bin[expanded$class %in% c("Rec","FR")], c(0.125, 0.5, 0.875, 1))[1], quantile(x=expanded$bin[expanded$class %in% c("Rec", "FR")], c(0.125, 0.5, 0.875, 1))[3])
               sizerange75FR_seed_bm_y[paste0(y)] <- paste0(round_any(sevfiveperc[1], 5), "-", round_any(sevfiveperc[2], 5))

               # for spatial figs
               PR75_seed_t <- c(quantile(x=towdat$pre[towdat$year==y], c(0.125, 0.5, 0.875, 1))[1], quantile(x=towdat$pre[towdat$year==y], c(0.125, 0.5, 0.875, 1))[3])
               PR75_seed_y[paste0(y)] <- paste0(round_any(PR75_seed_t[1], 5), "-", round_any(PR75_seed_t[2], 5))

               R75_seed_t <- c(quantile(x=towdat$rec[towdat$year==y], c(0.125, 0.5, 0.875, 1))[1], quantile(x=towdat$rec[towdat$year==y], c(0.125, 0.5, 0.875, 1))[3])
               R75_seed_y[paste0(y)] <- paste0(round_any(R75_seed_t[1], 5), "-", round_any(R75_seed_t[2], 5))

               C75_seed_t <- c(quantile(x=towdat$com[towdat$year==y], c(0.125, 0.5, 0.875, 1))[1], quantile(x=towdat$com[towdat$year==y], c(0.125, 0.5, 0.875, 1))[3])
               C75_seed_y[paste0(y)] <- paste0(round_any(C75_seed_t[1], 5), "-", round_any(C75_seed_t[2], 5))
             }
           }
           if(!any(!is.na(df[df$year==y, !names(df) %in% c("year", "years")]))) {
             sizerange75_seed_y[paste0(lastyear)] <- NA
             sizerange75_seed_y[paste0(year)] <- NA
             sizerange75_seed_bm_y[paste0(lastyear)] <- NA
             sizerange75_seed_bm_y[paste0(year)] <- NA
             PR75_seed_y[paste0(lastyear)] <- NA
             PR75_seed_y[paste0(year)] <- NA
             R75_seed_y[paste0(lastyear)] <- NA
             R75_seed_y[paste0(year)] <- NA
             C75_seed_y[paste0(lastyear)] <- NA
             C75_seed_y[paste0(year)] <- NA
           }
         }

        sizerange75_seed[[box.names[k]]] <- c(sizerange75_seed_y[paste0(lastyear)], sizerange75_seed_y[paste0(year)])
        sizerange75_seed_bm[[box.names[k]]] <- c(sizerange75_seed_bm_y[paste0(lastyear)], sizerange75_seed_bm_y[paste0(year)])
        PR75_seed[[box.names[k]]] <- c(PR75_seed_y[paste0(lastyear)], PR75_seed_y[paste0(year)])
        R75_seed[[box.names[k]]] <- c(R75_seed_y[paste0(lastyear)], R75_seed_y[paste0(year)])
        C75_seed[[box.names[k]]] <- c(C75_seed_y[paste0(lastyear)], C75_seed_y[paste0(year)])

        # sizerange75_seed_prev <- sizerange75_seed[paste0(lastyear)]
      }

      sizerange75_seed <- unlist(sizerange75_seed)
      sizerange75_seed_bm <- unlist(sizerange75_seed_bm)
      PR75_seed <- unlist(PR75_seed)
      R75_seed <- unlist(R75_seed)
      C75_seed <- unlist(C75_seed)

      seedPT <- data.frame(variable=c(rep("SeedNPR", length(SeedPR_current)), rep("SeedNR", length(SeedR_current)),
                                      rep("SeedN", length(Seed_current))),
                            lastyear=c(SeedPR_prev, SeedR_prev, Seed_prev),
                            thisyear=c(SeedPR_current, SeedR_current, Seed_current),
                            LTM=rep(box.names, 3))
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

      seedPT <- rbind(seedPT, data.frame(variable=c(rep("sizerange75_seed", length(Seed_current)),rep("sizerange75_seed_bm", length(Seed_current)),
                                                    rep("PR75_seed", length(Seed_current)),
                                                    rep("R75_seed", length(Seed_current)),
                                                    rep("C75_seed", length(Seed_current))),
                                         lastyear= c(unname(sizerange75_seed[grep(x=names(sizerange75_seed), pattern=paste0(".",lastyear))]),
                                                     unname(sizerange75_seed_bm[grep(x=names(sizerange75_seed_bm), pattern=paste0(".",lastyear))]),
                                                     unname(PR75_seed[grep(x=names(PR75_seed), pattern=paste0(".",lastyear))]),
                                                     unname(R75_seed[grep(x=names(R75_seed), pattern=paste0(".",lastyear))]),
                                                     unname(C75_seed[grep(x=names(C75_seed), pattern=paste0(".",lastyear))])),
                                         thisyear= c(unname(sizerange75_seed[grep(x=names(sizerange75_seed), pattern=paste0(".",year))]),
                                                     unname(sizerange75_seed_bm[grep(x=names(sizerange75_seed_bm), pattern=paste0(".",year))]),
                                                     unname(PR75_seed[grep(x=names(PR75_seed), pattern=paste0(".",year))]),
                                                     unname(R75_seed[grep(x=names(R75_seed), pattern=paste0(".",year))]),
                                                     unname(C75_seed[grep(x=names(C75_seed), pattern=paste0(".",year))])),
                                         LTM=rep(box.names, 5),
                                         word=NA))
      seedPT$nearLTM <- NA
      seedPT$bank <- banks[i]

      bmseedPT <- data.frame(variable=c(rep("SeedIPR", length(SeedPR_current)), rep("SeedIR", length(SeedR_current)), rep("SeedI", length(Seed_current))),
                           lastyear=c(bmSeedPR_prev, bmSeedR_prev, bmSeed_prev),
                           thisyear=c(bmSeedPR_current, bmSeedR_current, bmSeed_current),
                           LTM=rep(box.names, 3))
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
  highlights[!highlights$variable%in% c("PR75", "R75", "C75", "sizerange75", "sizerange75PR", "sizerange75Rec", "sizerange75FR", "sizerange75_bm_65up", "sizerange75_seed", "sizerange75_seed_bm", "PR75_seed", "R75_seed", "C75_seed", "minCF", "maxCF"),c(2,3,4)] <-
    apply(highlights[!highlights$variable%in% c("PR75", "R75", "C75", "sizerange75",  "sizerange75PR", "sizerange75Rec", "sizerange75FR", "sizerange75_bm_65up", "sizerange75_seed", "sizerange75_seed_bm", "PR75_seed", "R75_seed", "C75_seed", "minCF", "maxCF") ,c(2,3,4)], 2, function(x) round(as.numeric(x), 2))

  highlights[highlights$variable%in% c("minCF", "maxCF"),c(2,3,4)] <-
    apply(highlights[highlights$variable%in% c("minCF", "maxCF") ,c(2,3,4)], 2, function(x) round(as.numeric(x), 1))


  print(sizes)
  print(ntows)
  print(highlights)

  sizes <<- as.data.frame(sizes)
  ntows <<- ntows
  highlights <<- highlights
  spatial.sum.stats <<- spatial.sum.stats
  dates<<-dates
  yeartable <<-yeartable

}

