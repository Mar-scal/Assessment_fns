#### Run the industry report in R to compare against the SQL developer view
direct <- "Y:/Offshore/Assessment/" 
# direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/" 
# source(paste(direct_fns,"Survey_and_OSAC/get.offshore.survey.r",sep=""))

funs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/get.offshore.survey.r",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} # end for(un in funs)



require(plyr)
require(reshape2)
require(dplyr)
require(ggplot2)

# scaloff username and pwd needed (keyserf doesn't have the right access)
indreport <- get.offshore.survey(direct=direct, cruise="LE16", yr=2022, industry.report = T, un = "scaloff", pw=pwd.id)


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

sum(df[[1]]$total) + sum(df[[2]]$total) #+ sum(df[[3]]$total)+ sum(df[[4]]$total) + sum(df[[5]]$total)  + sum(df[[6]]$total)  + sum(df[[7]]$total) 

df

nrow(industryreport)

industryreport$lon <- convert.dd.dddd(industryreport$START_LON)
industryreport$lat <- convert.dd.dddd(industryreport$START_LAT)
ggplot() + geom_text(data=industryreport[industryreport$MGT_AREA_CD=="GBb",], aes(lon,lat, label=TOW_NO)) + coord_map()


ggplotly

olex_se[olex_se$ID==363,]

-66.21633173	41.962685


# checking out the german rake stuff. No obvious signs of bias. 
# png(paste0(direct, "2019/Presentations/Survey_summary/test_figures/Ger/numberspertow_rake.png"), height=4, width=8, units="in", res=200)
# ggplot() + geom_point(data=melt(industryreport[industryreport$MGT_AREA_CD=="Ger",], measure.vars = c("L_0-70", "L_70-100", "L_100+")), aes(TOW_NO, value)) + facet_wrap(~variable, scales="free") +
#   geom_vline(data=melt(industryreport[industryreport$MGT_AREA_CD=="Ger",], measure.vars = c("L_0-70", "L_70-100", "L_100+")), aes(xintercept=411.5), linetype="dashed") + 
#   theme_bw() + theme(panel.grid=element_blank()) +
#   ylab("Number of scallops") +
#   xlab("Tow number")
# dev.off()
# 
# test <- melt(industryreport[industryreport$MGT_AREA_CD=="Ger",], measure.vars = c("L_0-70", "L_70-100", "L_100+"))
# test$rake[test$TOW_NO <= 411] <- "German rake"
# test$rake[test$TOW_NO > 411] <- "Georges rake"
# png(paste0(direct, "2019/Presentations/Survey_summary/test_figures/Ger/numberspertow_rake_spatial.png"), height=3, width=8, units="in", res=200)
# ggplot() + geom_point(data=test, 
#                       aes(convert.dd.dddd(START_LON), convert.dd.dddd(START_LAT), colour=rake, size=value)) + facet_wrap(~variable) +
#   theme_bw() + theme(panel.grid=element_blank()) +
#   ylab("Latitude") +
#   xlab("Longitude")
# dev.off()
# 
# load("C:/Documents/Offshore scallop/Assessment/Data/Survey_data/2019/Survey_summary_output/testing_results_SCALOFF_LE09.RData")
# source(paste(direct,"Assessment_fns/Survey_and_OSAC/shf.plt.r",sep=""))
# shf.years <-  2018:2019
# s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
# shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
#         recline=c(RS,CS),add.title = T,titl = NA,cex.mn=3,sample.size = T)
#            