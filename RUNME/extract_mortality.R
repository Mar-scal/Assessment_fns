# Extracting exploitation, F, natural mortality (for sustainable fisheries framework survey)
require(tidyverse)
require(dplyr)
require(ggplot2)
direct_out <- "C:/Users/keyserf/Documents/"
direct_out 
load("C:/Users/keyserf/Documents/Data/Model/2022/GBa/Results/Final_model_results.Rdata")
load("C:/Users/keyserf/Documents/Data/Model/2022/GBa/Results/Model_results_and_diagnostics_mixed.RData")
# natural mortality:
mort$GBa
#1-exp(-DD.out$GBa$mean$m[which(DD.out$GBa$data$year==2021)])

# exploitation rate
DD.out$GBa$median$mu[which(DD.out$GBa$data$year==2021)]
# instantaneous fishing mortality
1-exp(-DD.out$GBa$median$mu[which(DD.out$GBa$data$year==2021)])

# FROM BUGS
# it allows for some of the recruits to be caught in the fishery.  Not sure the result will be stable mind you.
mu[t] <- C[t]/(B[t]+C[t])
# Instantaneous fishing mortality
-log(1 - DD.out$GBa$median$mu[which(DD.out$GBa$data$year==2021)])


load("C:/Users/keyserf/Documents/Data/Model/2022/BBn/Results/Final_model_results.Rdata")
load("C:/Users/keyserf/Documents/Data/Model/2022/BBn/Results/Model_testing_results_mixed.Rdata")
load("C:/Users/keyserf/Documents/Data/Model/2022/BBn/Results/Model_testing_results_mixed.RData")
# natural mortality:
mort$BBn
#1-exp(-DD.out$GBa$mean$m[which(DD.out$GBa$data$year==2021)])

# exploitation rate
DD.out$BBn$median$mu[which(DD.out$BBn$data$year==2021)]
# instantaneous fishing mortality
1-exp(-DD.out$BBn$median$mu[which(DD.out$BBn$data$year==2021)])
