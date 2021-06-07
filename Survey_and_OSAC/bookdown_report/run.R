library(xfun)
library(rmarkdown)

######## prep input data if you haven't already
reportyear <- 2019

source("C:/Documents/Assessment_fns/Survey_and_OSAC/Survey_Summary_Word.R")

Survey_Summary_Word(year=reportyear, reportseason="spring", data=paste0("Y:/Offshore/Assessment/Data/Survey_data/", reportyear, "/Survey_summary_output/Survey_all_results.Rdata"), direct="Y:/Offshore/Assessment/", direct_fns = "C:/Documents/")

summary_data <- list(sizes=sizes, ntows=ntows, highlights=highlights)

save(summary_data, file = "summary.Rdata")
#############

#### build the book!
source("R/render_markdowns.R")

