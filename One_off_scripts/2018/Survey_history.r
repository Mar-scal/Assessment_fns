# Quick script to grab the survey history for offshore on all banks...

library(plyr)
library(openxlsx)
direct <- "d:/r/"
direct <- "y:/Offshore_scallop/Assesment/"
yr = 2017
load(paste(direct,"Data/Survey_data/",yr-1,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))
tmp <- surv.Live$BBs
yr = 2017
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))
surv.Live$BBs <- tmp

num.banks <- length(surv.Live)
banks <- names(surv.Live)
final.res <- NULL
for(i in 1:num.banks)
{
  dat <- surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year >= 2010,]
  dat$random[dat$random != 1] <- "Non-random"
  dat$random[dat$random == 1] <- "Random"
  res <- aggregate(tow ~ year,dat,function(x) length(unique(x)))
  res$n_days <- aggregate(date ~ year,dat,function(x) length(unique(x)))$date
  tmp <- aggregate(tow ~ year+random,dat,length)
  res$bank <- banks[i]
  # Since there aren't any random stations on GB....
  if(banks[i]=="GB") final.res[[banks[i]]] <- data.frame(year = as.numeric(res$year),tow = res$tow,
                                                         n_days = res$n_days,n_random = NA,n_non_random = res$tow,bank = res$bank)
  # For everywhere else we do this...
  if(banks[i] != "GB")
  {
  res$n_random <- tmp$tow[tmp$random == "Random"]
  tmp2 <- tmp[tmp$random == "Non-random",c("year","tow")]
  names(tmp2) <- c("year","n_non_random")
  final.res[[banks[i]]] <- join(res,tmp2,by="year",type="left")
  }
}

result <- do.call("rbind",final.res)
# Spring vs Summer
result$season <- "Spring"
result$season[result$bank %in% c("GBa","GBb")] <- "Summer"

by.season <- aggregate(tow ~ year+season,result,sum)
by.season$n_days <- aggregate(n_days ~ year+season ,result,sum)$n_days
by.season$n_random <- aggregate(n_random ~ year+season ,result,sum)$n_random
tmp <- aggregate(n_non_random ~ year+season ,result,sum)
by.season <- join(by.season,tmp,by=c("year","season"),type="left")

# Put the data in a list for the excel spreadsheet
output <- list(by_bank =result,by_season = by.season)
# Mpw output to an excel file.
write.xlsx(output,paste0(direct,"2018/Survey_Design/Survey_history.xlsx"))

