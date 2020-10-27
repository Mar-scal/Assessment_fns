# Annual landings summary:

direct_fns <- "C:/Users/keyserf/Documents/Github/FK/Assessment_fns/"
direct <- "C:/Users/keyserf/Documents/Version_control_pandemic/Offshore/Assessment/"
source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep=""))
logs_and_fish(loc="offshore",year = 2009:2019,un=un,pw=pwd,db.con=db.con,direct=direct, direct_fns=direct_fns)

require(tidyverse)
new.log.dat %>%
  group_by(year, bank) %>%
  summarise(total=sum(pro.repwt)/1000) %>%
  pivot_wider(values_from = total, names_from = bank)

summary(new.log.dat)

unique(new.log.dat$year)
