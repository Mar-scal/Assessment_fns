# So here we'll try and get all the data in the correct structure for the spatial model.

funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Fishery/logs_and_fishery_data.r",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/projections.r",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/decision.r",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Model/post.plt.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Model/exploit.plt.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/fit.plt.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/diag.plt.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/biomass.plt.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/combo_shp.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/run_model.R",
          "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Fishery/fishery.dat.r"
)

for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
}

library(sf)
library(sp)
library(tidyverse)
library(ggthemes)
library(cowplot)
library(R2jags)
library(SSModel)


###################  SECTION 2 Make the Retro plots ##############################  SECTION 2 Make the Retro plots ###########
# Now make the retrospective plots...

# Set parameters for the run...
#mod.loc <- "Y:/Offshore/Assessment/Data/Model/"
mod.loc <- "D:/testing_folder/Data/Model/retros/"
retro.years <- c(2017:2020,2022:2025)
n.retro.years <- length(retro.years)
base.year <- max(retro.years)

# Load the correct base model...

trends <- NULL
for(y in retro.years)
{
  
  # load the data objects for the appropriate run...
  if(y == 2025)load(paste0(mod.loc,y,"/Model_testing_results.RData"))
  
  if(y != 2018 && y != 2025) load(paste0(mod.loc,y,"/Final_model_results.RData"))
  
  if(y == 2018) load(paste0(mod.loc,y,"/Model_testing_results_mixed.RData"))
  
  # From the NAS
  #if(j == 2025)load(paste0(mod.loc,j,"/GBa/Results/Model_testing_results.RData"))
  #if(j != 2018 & j != 2025) load(paste0(mod.loc,j,"/GBa/Results/Final_model_results.RData"))
  #if(j == 2018) load(paste0(mod.loc,j,"/GBa/Results/Final_model_results_mixed.RData"))
  
  trends[[as.character(y)]] <-data.frame(years = 1986:(y-1),
                              B =     apply(DD.out$GBa$sims.list$B,2,median),
                              B.LCI = apply(DD.out$GBa$sims.list$B,2,function(x){quantile(x,probs=0.025)}),
                              B.UCI = apply(DD.out$GBa$sims.list$B,2,function(x){quantile(x,probs=0.975)}),
                              R =     apply(DD.out$GBa$sims.list$R,2,median),
                              R.LCI = apply(DD.out$GBa$sims.list$R,2,function(x){quantile(x,probs=0.025)}),
                              R.UCI = apply(DD.out$GBa$sims.list$R,2,function(x){quantile(x,probs=0.975)}),
                              m =    apply(DD.out$GBa$sims.list$m,2,median),
                              m.LCI = apply(DD.out$GBa$sims.list$m,2,function(x){quantile(x,probs=0.025)}),
                              m.UCI = apply(DD.out$GBa$sims.list$m,2,function(x){quantile(x,probs=0.025)}),
                              retro.year = y)
  rm(DD.out)
  if(y == base.year) trends[[as.character(y)]]$retro.year <- "Full model"
}    
retro.base <- do.call("rbind",trends)
retro.base$retro.year[retro.base$retro.year != "Full model"] <- as.character(as.numeric(retro.base$retro.year[retro.base$retro.year != "Full model"])-1)
cols <- c(rep('#005BBB',2),rep('firebrick2',2),rep('darkgrey',2),rep('#FFD500',1),'black')
points <- c(rep(21:24,2)) 
b.retro <- ggplot(data=retro.base ,aes(x= years, y = B/1000,group=retro.year,color=retro.year,shape = retro.year,fill = retro.year)) +
  geom_line(size=1) + 
  #geom_line(data=retro.base %>% dplyr::filter(years %in% 2022:2025),size=1)+
  geom_point(size=3) + 
  scale_shape_manual("",values = points) + 
  scale_color_manual("",values =cols) + scale_fill_manual("",values =cols) +
  scale_x_continuous(breaks = seq(1990,2030,by=5)) + xlab("") + 
  ylab("Fully recruited biomass (tonnes x 1000)")
save_plot(paste0(mod.loc,"GBa_FR_retro.pdf"),b.retro,base_width = 10,base_height =7)


r.retro <- ggplot(data=retro.base ,aes(x= years, y = R/1000,group=retro.year,color=retro.year,shape = retro.year,fill = retro.year)) +
  geom_line(size=1) + 
  geom_point(size=3) + 
  scale_shape_manual("",values = points) + 
  scale_color_manual("",values =cols) + scale_fill_manual("",values =cols)  + 
  scale_x_continuous(breaks = seq(1990,2030,by=5)) + xlab("") + 
  ylab("Recruit biomass (tonnes x 1000)")
save_plot(paste0(mod.loc,"Rec_retro.pdf"),r.retro,base_width = 10,base_height =7)



m.retro <- ggplot(data=retro.base ,aes(x= years, y = m,group=retro.year,color=retro.year,shape = retro.year,fill = retro.year)) +
  geom_line(size=1) + 
  geom_point(,size=3) + 
  scale_shape_manual("",values = points) + 
  scale_color_manual("",values =cols) + scale_fill_manual("",values =cols)  + 
  scale_x_continuous(breaks = seq(1990,2030,by=5)) + xlab("") + 
  ylab("Natural mortality (instantaneous)")
save_plot(paste0(mod.loc,"mort_retro.pdf"),m.retro,base_width = 10,base_height =7)


# Calculate mohn's rho, it is simply the Relative bias of the estimate...
# So now bring in the final model run to get the 'true' value for the calculation

bias <- NULL

# Do a 5 year peel as recommended
#act.retro.years <- c(2010:2014,2016:2019,2021)
for(j in retro.years[-length(retro.years)])
{
  retro.dat <- retro.base %>% dplyr::filter(retro.year==j-1,years == j-1)
  base.dat <- retro.base %>% dplyr::filter(years == j-1,retro.year == "Full model")
  bias[[as.character(j)]] <- data.frame(B = (retro.dat$B - base.dat$B),
                                        rel.B = (retro.dat$B - base.dat$B) / base.dat$B,
                                        R = (retro.dat$R - base.dat$R),
                                        rel.R = (retro.dat$R - base.dat$R) / base.dat$R,
                                        m = (retro.dat$m - base.dat$m),
                                        rel.m = (retro.dat$m - base.dat$m) / base.dat$m,
                                        mod = "BSSM")
}


# So now we can use this to calculate mohn's rho
bias <- do.call('rbind',bias)
# mohn's rho being fine when it is < 0.2 is found in Hutrtado-Ferro 2015 paper.
# Note we are removing 2020 from this since we don't have any survey data.
mohns.rhos <- data.frame(mr.B = sum(bias$rel.B)/n.retro.years,
                         mr.R = sum(bias$rel.R)/n.retro.years,
                         mr.m = sum(bias$rel.m)/n.retro.years,
                         mr.B5 = sum(bias$rel.B[(nrow(bias)-4):nrow(bias)])/5,
                         mr.R5 = sum(bias$rel.R[(nrow(bias)-4):nrow(bias)])/5,
                         mr.m5 = sum(bias$rel.m[(nrow(bias)-4):nrow(bias)])/5,
                         mod = "BSSM")

saveRDS(mohns.rhos,paste0(mod.loc,"mohns_rose_.Rds"))
saveRDS(bias,paste0(mod.loc,"bias_SSModel.Rds"))
saveRDS(retro.base,paste0(mod.loc,"retro_SSModel.Rds"))
