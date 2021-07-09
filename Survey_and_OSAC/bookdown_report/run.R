library(xfun)
library(rmarkdown)

######## prep input data if you haven't already
reportyear <- 2019


#################################################
### if you need to re-create the summary.RData

direct_fns <- "C:/Users/keyserf/Documents/Github/FK/"
#direct_fns <- "C:/Documents/"

source(paste0(direct_fns, "Assessment_fns/Survey_and_OSAC/Survey_Summary_Word.R"))

Survey_Summary_Word(year=reportyear, reportseason="spring",
                    data=paste0("Y:/Offshore/Assessment/Data/Survey_data/", reportyear, "/Survey_summary_output/Survey_all_results.Rdata"),
                    direct="Y:/Offshore/Assessment/",
                    direct_fns = direct_fns)

summary_data <- list(sizes=sizes, ntows=ntows, highlights=highlights, spatial.sum.stats=spatial.sum.stats)

save(summary_data, file = "summary.Rdata")
#######################################################

######################################################
#### build the book!
source("R/render_markdowns.R")
########################################################
