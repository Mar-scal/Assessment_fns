#### Run the industry report in R to compare against the SQL developer view

source(paste(direct,"Assessment_fns/Survey_and_OSAC/get.offshore.survey.r",sep=""))
cruise="LE09"
yr=2019

indreport <- get.offshore.survey(direct=direct, cruise="LE09", yr=2019, industry.report = T)

industryreport <- indreport$industryreport

# data for the summary table
df <- NULL
for(i in 1:length(unique(industryreport$MGT_AREA_CD))){
  print(unique(industryreport$MGT_AREA_CD)[i])
  
  sub <- industryreport[industryreport$MGT_AREA_CD==unique(industryreport$MGT_AREA_CD)[i],]
  
  print(length(unique(sub$TOW_NO[sub$SPECIES_ID==1])))
  print(length(unique(sub$TOW_NO[sub$SPECIES_ID==2])))
  
  sub$sums <- rowSums(sub[, c("L_0-70", "L_70-100", "L_100+")])
  
  sub$greaterthan2[sub$catchbaskets>=2] <- 1
  sub$greaterthan200[sub$`L_100+`>=200] <- 1
  sub$greaterthan500[sub$`L_0-70`>=500] <- 1

  df[[i]] <- ddply(.data=sub, .(MGT_AREA_CD, TOW_TYPE_ID, SPECIES_ID),
        summarize,
        total= sum(sums),
        total_2 = sum(greaterthan2, na.rm=T),
        total_200 = sum(greaterthan200, na.rm=T),
        total_500 = sum(greaterthan500, na.rm=T))
      
}

sum(df[[1]]$total) + sum(df[[2]]$total) + sum(df[[3]]$total)+ sum(df[[4]]$total) + sum(df[[5]]$total)  + sum(df[[6]]$total)  + sum(df[[7]]$total) 
