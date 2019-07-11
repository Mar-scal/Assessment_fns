#### Run the industry report in R to compare against the SQL developer view

source(paste(direct,"Assessment_fns/Survey_and_OSAC/get.offshore.survey.r",sep=""))
cruise="LE09"
yr=2019
get.offshore.survey(direct=direct, cruise="LE09", yr=2019, industry.report = T)
