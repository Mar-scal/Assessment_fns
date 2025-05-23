library(xfun)
library(rmarkdown)
require(ggplot2)

######## prep input data if you haven't already
year <- 2024
reportyear <- 2024
#direct_fns <- "C:/Documents/Assessment_fns/"

#################################################
### if you need to re-create the summary.RData

#direct_fns <- "C:/Users/keyserf/Documents/Github/FK/Assessment_fns/"
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"

source(paste0(direct_fns, "Survey_and_OSAC/Survey_Summary_Word.R"))

Survey_Summary_Word(year=reportyear, reportseason="summer",
                    data=paste0("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/", reportyear, "/Survey_summary_output/Survey_all_results.Rdata"),
                    direct="C:/Users/keyserf/Documents/temp_data/",
                    direct_fns = direct_fns)

summary_data <- list(sizes=sizes, ntows=ntows, highlights=highlights, spatial.sum.stats=spatial.sum.stats, dates=dates, yeartable=yeartable)

save(summary_data, file = "summary4.Rdata")
#######################################################

######################################################
#### build the book!
# Modify render_markdowns.R based on the banks/survey you desire. Also make sure the Rdata file in parameterised_report.Rmd is right.
source("officedown/render_markdowns_word.R")
########################################################

write.csv(x = highlights, "officedown/highlights.csv")

### feedback
# Change "Return to links" to "Return to bank summary" (be really cool if you could make it "Return to Middle Bank Summary" but only if you could automate the naming for the bank)
#
# Make sure 'return to links' is on same page as figure
#
# I assume it wouldn't be possible, but if the 'return to links' was in the footer on the bottom left it would save you space on the page.  I can't think how you could easily automate that tho.
#
# I think having a subsection with links to each figure in each bank in the "Table of Contents" would be too much, but I throw it out as an idea
#
# Make spatial figures larger so they fill the width of the page and don't put 2 spatial figures on one page, aim to maximize their size.
#
# Clarify years used for the LTMs, thinking anything repetitive like this could be summarized on the first 'page' for each bank to minimze text?
#
# Reduce decimal places as appropriate, my rule of thumb...
#
#
#
# >100 = 0 decimal places
#
# 1-99 = 1 decimal place
#
# <1 = 2 decimal places
