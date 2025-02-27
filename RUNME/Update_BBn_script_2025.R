# So here we'll try and get all the data in the correct structure for the spatial model.
#devtools::install_github("RaphMcDo/SEBDAM@dev-branch") # to install a specific branch of SEBDAM
# Process flow for this framework...
#Step 1: Run the SEBDAM ("Bnk"_SEBDAM.R) model
#Step2: Run the Decision Tables ('bnk'_Decision_Table.R)


library(SEBDAM)
library(tidyverse)
library(sf)
library(stringr)
library(optimx)
library(parallel)
library(INLA)
library(ggthemes)
library(cowplot)
library(shape)
library(kableExtra)
library(readr)
library(ggthemes)

theme_set(theme_few(base_size = 22))

# Inital set up.
assess.year <- 2025
direct <- "Y:/Offshore/Assessment/"
direct.fns <- "D:/Github/"
# Set parameters for the run...
repo.loc <- "D:/Framework/SFA_25_26_2024/Model/"
mod.select <- "SEAM"
atow<-800*2.4384/10^6 # area of standard tow in km2
NY <- length(years)
R.size <- "75"
FR.size <- "90"
num.knots <- 20 # Going to test 10, 15, and 20
qR <- 0.33# This is for TMB (log recruit catchability) testing catchability of 0.5, test 0.3 and 0.1. Can be used with SEAM too in place of the m and R initiailztion.
vary.q <- T

source(paste0(direct.fns,"Assessment_fns/Fishery/logs_and_fishery_data.r"))
source(paste0(direct.fns,"Assessment_fns/Maps/pectinid_projector_sf.R"))
source(paste0(direct.fns,"Assessment_fns/Maps/convert_inla_mesh_to_sf.R"))
source(paste0(direct.fns,"Assessment_fns/Model/SFA_26_Decision_Table_function.R")) 
# This function is in the dev-branch of the repo, so make sure you have the repo cloned and it's pointing to the dev-branch for this to work.
source("D:/Github/SEBDAM/R/data_setup.R")


########################################################################################################
# Bring in the data and tidy it up for the analysis
bbn.shape <- st_read(paste0(direct.fns,"GIS_layers/survey_boundaries/BBn.shp"), quiet=T)
bbn.shape <- bbn.shape %>% st_transform(crs = 32619) # BBn is right on the 19/20 border so think they are basically equivalent options here
# Bring in the survey data

load(paste0(direct,"/Data/Survey_data/",(assess.year-1),"/Survey_summary_output/Survey_all_results.Rdata"))
# Need to reset this
years <- 1994:(assess.year-1)

#surv.dat <- surv.dat$BBn
#saveRDS(surv.dat,'D:/Github/BBn_model/Results/BBn_surv.dat.RDS')
#surv.dat <- readRDS('D:/Framework/SFA_25_26_2024/Model/Data/BBn_surv.dat.RDS')
surv.dat <- surv.dat$BBn
mod.dat <- survey.obj$BBn$model.dat

# For now we need to get a 2024 growth term just recycling the growth for 2021 for the moment
#mod.dat <- rbind(mod.dat,mod.dat[nrow(mod.dat),])
#mod.dat$year[nrow(mod.dat)] <- 
# Bring in the fishery data
logs_and_fish(loc="offshore",year = 1991:(assess.year-1),un=un.ID,pw=pwd.ID,db.con=db.con,direct="Y:/Offshore/Assessment/", get.marfis=F)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
# 
fish.dat <- fish.dat[!is.na(fish.dat$lon),]
fish.dat <- fish.dat[!is.na(fish.dat$lat),]
fish.dat <- fish.dat[!fish.dat$lon==0,]
fish.dat <- fish.dat[!fish.dat$lat==0,]
# #
# 
# # Now subset to BBn and add in the missing years of data
bbn.fish <- fish.dat %>% dplyr::filter(bank == "BBn")
bbn.fish <- fish.dat[fish.dat$bank == "BBn",]
bbn.fish <- bbn.fish[!is.na(bbn.fish$lon),]
# # There are 12 data points at 0,0 that we remove, I'm not worried about accounting for these 12 points!
bbn.fish <- bbn.fish %>% dplyr::filter(lat !=0 | lon != 0)
bbn.fish$month <- lubridate::month(bbn.fish$date)
# # Now I want to put a 'survey year' on these because that's what we're gonna need for our modelling... start by porting over the year
bbn.fish$survey.year <- bbn.fish$year
# DK NOTE: Now this is going to get confusing for us and we may want to tweak SEBDAM for this, but that's a down the road job, not a playing around with model job
# But based on the indexing in SEBDAM, I am going to change how we index the survey year data from what we have done with offshore traditionally.
# Historically anything from the last half of the year goes into the following years, eg. survey.year 2002 = June 2001- May 2002.
# But in SEBDAM we have (B(t-1) - C(t-1)), so let's say we have year 2002 survey biomass, this says we remove the 20002 catch from that
# we want that catch to be the catch from June 2002 to May 2003, i.e. we remove the catch before we allow the population to grow
# This is what we do in our current model, but we have a different index (C(t) on our model.
# Basically survey year 2002 = June 2002 - May 2003 now
#DK note: We probably should think more about the survey year fun and how exactly we want to handle removal of catch in our models.
# We don't have removals for 2009, we need something for that, so we're adding that in here...
# Note that in 2015 the survey was delayed until July, but there was no fishing on BBn in 2015 in June or July, so this system still works for the
# survey year despite that.... happily!
bbn.fish$survey.year[bbn.fish$month %in% 1:5] <- bbn.fish$survey.year[bbn.fish$month %in% 1:5] -1
# # Need to add fake data for 2009
bbn.fish[nrow(bbn.fish)+1,] <- NA
# 
 bbn.fish$pro.repwt[nrow(bbn.fish)] <- 0
 bbn.fish$year[nrow(bbn.fish)] <- 2009
# # See DK NOte below
bbn.fish$survey.year[nrow(bbn.fish)] <- 2009
bbn.fish$month[nrow(bbn.fish)] <- 6
# # Getting fake lat/lon coords that are on BBn
bbn.fish$lat[nrow(bbn.fish)] <- 42.85600
bbn.fish$lon[nrow(bbn.fish)]  <- -65.90183
saveRDS(bbn.fish,paste0(direct,'Data/Model/',assess.year,"/BBn/BBn_fish.dat.RDS"))
# 
#bbn.fish <- readRDS('D:/Framework/SFA_25_26_2024/Model/Data/BBn_fish.dat.RDS')
bbn.fish$pro.repwt <- bbn.fish$pro.repwt/1000
#### Finished Data prep and clean up!
###############################################################################################


# The survey biomass index for 1994 says there were 249 tonnes of recruits that year.
#l.init.R <- log(5) # I think this is initializing the knots, i.e. each knots with this number of recruits.  Aim for 250 total
#qR.par <- log(0.9) # Trying with a fixed qR for SEAM instead

live.subset <- surv.dat %>% dplyr::filter(state == 'live' & random ==1)
dead.subset <- surv.dat %>% dplyr::filter(state== "dead" & random ==1)

live.input <- data.frame(I = live.subset$com.bm, IR = live.subset$rec.bm,year = live.subset$year,tow = live.subset$tow,tot.live.com = live.subset$com,lat = live.subset$lat,lon=live.subset$lon,tow_type = live.subset$random)
clap.input <- data.frame(L = dead.subset$com,tow = dead.subset$tow,year = dead.subset$year,tow_type = dead.subset$random)
mod.input <- left_join(live.input,clap.input,by=c('tow','year','tow_type'))
mod.input$N <- round(mod.input$tot.live.com + mod.input$L)
# Looks like there are no values > 0 but < 0.5, so the low clapper numbers should all round up to 1 (which makes sense as you'd only get < 0.5 if we had tows twice as long as they should be)
mod.input$L <- round(mod.input$L) 
mod.input.sf <- st_as_sf(mod.input,coords = c('lon','lat'),remove=F,crs = 4326)
mod.input.sf <- mod.input.sf %>% st_transform(crs=32619)
mod.input.sf <- st_intersection(mod.input.sf,bbn.shape)
mod.input.sf$Year <- mod.input.sf$year - (min(years)-1)
mod.input.sf$I <- mod.input.sf$I/atow
mod.input.sf$IR <- mod.input.sf$IR/atow
mod.input.sf <- mod.input.sf %>% dplyr::filter(year %in% years)

# We need to recalculate the growth data using the new von B curves and size bins... breaking out the ugly code to do this...
vonB.par <- data.frame(Linf = 164.4,K = 0.2, t0 = -0.2)


# Back to real code
# So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
# and the biomass from each tow to come up with an overall bank average condition factor.
# This is weight in this year, which becomes t-1 
w.fr.current  <- mod.dat$CF*(mod.dat$l.bar/100)^3
w.rec.current <- mod.dat$CF*(mod.dat$l.k/100)^3

# Using this years average shell height we can figure out how old the scallop are on average and then we use the 
# von B and allow them to grow by 1 year, that's our average projected size next year.
len.fr.next <- NA #laa.t <- NA
len.rec.next <- NA #laa.t <- NA
for(y in 1:nrow(mod.dat))
{
age= data.frame(age = seq(2,8,by=0.01),len = NA,l.fr = mod.dat$l.bar[y],l.rec = mod.dat$l.k[y])
age$len <- vonB.par$Linf*(1-exp(-vonB.par$K*(age$age-vonB.par$t0)))
age$fr.diff <- abs(age$len - age$l.fr)
age$rec.diff <- abs(age$len - age$l.rec)
age.fr.next <- age$age[which.min(age$fr.diff)] + 1
age.rec.next <- age$age[which.min(age$rec.diff)] + 1
len.fr.next[y] <-  vonB.par$Linf*(1-exp(-vonB.par$K*(age.fr.next-vonB.par$t0)))
len.rec.next[y] <-  vonB.par$Linf*(1-exp(-vonB.par$K*(age.rec.next-vonB.par$t0)))
} # end for y in 1:NY
# The c() term in the below offsets the condition so that current year's condition slots into the previous year and repeats 
# the condition for the final year), this effectively lines up "next year's condition" with "predictied shell height next year (laa.t)
# This gets us the predicted weight of the current crop of scallops next year based on next years CF * length^3
# Get the weight of scallop next year
w.fr.next <- c(mod.dat$CF[-1],mod.dat$CF[nrow(mod.dat)])*(len.fr.next/100)^3
w.rec.next <- c(mod.dat$CF[-1],mod.dat$CF[nrow(mod.dat)])*(len.rec.next/100)^3
# Also calculate it based on 'known' condition, used for prediction evaluation figures only as we don't know it when we run the models, only
# after we get survey data.
w.fr.next.alt <- mod.dat$CF*(len.fr.next/100)^3
w.rec.next.alt <- mod.dat$CF*(len.rec.next/100)^3


# The new growth model using w.bar for everything over 100 mm, which will be default exclude the vast majority of the
# recruits as 90 mm scallop will grow by about 12 cm, so might have a few recruits in there, but tracking the changes in that size class tells
# us what the realized growth was for the FRs that excludes the recruits
# So what we do is take the ratio of the w.bar for everything bigger than 100 mm in year 2, to the w.bar for all FR scallop in year one
# Based on the von.B the vast majority of the scallop in that ratio be the same individuals.
# So to calculate the 100 mm thing I'll need to use the shf in surv.dat...
# I can do the same with recruit growth can't I, everything from 90 to 100 were probably recruits last year
# so look at 75-90 last year and compare with 90 to 100 this year...

sizes <- seq(0.025,2,by=0.05) # So I'd be using the 1.075 bin and everything bigger
# The w.yst object is exactly proportional to mod.dat$I, there is an offset, but given I need proportions I think this object is perfectly fine to use.
mw.per.bin <- data.frame(mw.per.bin = rbind(survey.obj$BBn$shf.dat$w.yst/survey.obj$BBn$shf.dat$n.yst,rep(NA,40)),year = c(mod.dat$year,2020))
N.per.bin <- data.frame(N.per.bin = rbind(survey.obj$BBn$shf.dat$n.yst,rep(NA,40)),year = c(mod.dat$year,2020))
#reorder them
mw.per.bin <- mw.per.bin[order(mw.per.bin$year),]
N.per.bin <- N.per.bin[order(N.per.bin$year),]
# Get the right bins for the FRs
max.bin <- length(sizes)
bin.frs.plus <- which(sizes == 1.025):max.bin
bin.90.plus <- which(sizes == 0.925):max.bin
bin.rec <- which(sizes == 0.775):min((bin.90.plus-1))
bin.frs.minus <- min(bin.90.plus):(min(bin.90.plus)+1)

# and the right bins for the recruits

# Now make a new object
g.proper <- data.frame(year = mw.per.bin$year)
g.proper$total.abun.90 <- rowSums(N.per.bin[,bin.90.plus])
g.proper$total.abun.frs <- rowSums(N.per.bin[,bin.frs.plus])
g.proper$total.rec.abun <- rowSums(N.per.bin[,bin.rec])
g.proper$total.frs.minus <- rowSums(N.per.bin[,bin.frs.minus])
# Propotions in each bin, FRs and
N.prop.per.bin.90 <- N.per.bin[,bin.90.plus]/g.proper$total.abun.90
N.prop.per.bin.frs <- N.per.bin[,bin.frs.plus]/g.proper$total.abun.frs
# Recs
N.prop.per.bin.rec       <- N.per.bin[,bin.rec]/g.proper$total.rec.abun
N.prop.per.bin.frs.minus <- N.per.bin[,bin.frs.minus]/g.proper$total.frs.minus

# And the average mw in each of the bins of interest, first for the FRs
g.proper$mw.frs.plus <-  rowSums(mw.per.bin[,bin.frs.plus] * N.prop.per.bin.frs,na.rm=T)
g.proper$mw.90.plus <-   rowSums(mw.per.bin[,bin.90.plus] * N.prop.per.bin.90,na.rm=T)
# and for the rec
g.proper$mw.recs <-      rowSums(mw.per.bin[,bin.rec] * N.prop.per.bin.rec,na.rm=T)
g.proper$mw.frs.minus <- rowSums(mw.per.bin[,bin.frs.minus] * N.prop.per.bin.frs.minus,na.rm=T)

g.proper$g.proper <- c(g.proper$mw.frs.plus[2:length(g.proper$mw.frs.plus)]/g.proper$mw.90.plus[1:(length(g.proper$mw.90.plus)-1)],NA)
g.proper$gR.proper<- c(g.proper$mw.frs.minus[2:length(g.proper$mw.frs.minus)]/g.proper$mw.recs[1:(length(g.proper$mw.recs)-1)],NA)


g.proper[g.proper$year %in% c(1991,2020),-1] <- NA
g.proper[g.proper$year %in% c(2019),which(names(g.proper) %in% c("g.proper","gR.proper"))] <- NA

# Fill in the mean for the missing years
g.proper$g.proper[g.proper$year %in% c(1991,2019,2020,(assess.year-1))] <- median(g.proper$g.proper,na.rm=T)
g.proper$gR.proper[g.proper$year %in% c(1991,2019,2020,(assess.year-1))] <- median(g.proper$gR.proper,na.rm=T)

# now need to add in 2020 to mod.dat...
mod.dat.tmp <- mod.dat
mod.dat.tmp[nrow(mod.dat.tmp)+1,] <- NA
mod.dat.tmp$year[nrow(mod.dat.tmp)] <- 2020
mod.dat.tmp <- mod.dat.tmp[order(mod.dat.tmp$year),]

growth <- data.frame(year = mod.dat.tmp$year,g.proper = g.proper$g.proper,gR.proper = g.proper$gR.proper)
# Now addin the missing growth years for g and gR
growth <- growth %>% dplyr::filter(year >= min(years))

# THis is the growth term used in the model!
g <- data.frame(g=growth$g.proper,gR = growth$gR.proper) #if(g.mod == 'proper_g') 

# Now we can clip both of these to subset it to the data that I think we need for the analysis....
# If we run with random == 1 then we need to fill in 2020...
mod.input.sf[nrow(mod.input.sf)+1,] <- mod.input.sf[nrow(mod.input.sf),]
#mod.input.sf[nrow(mod.input.sf),] <- mod.input.sf[nrow(mod.input.sf)-1,]
mod.input.sf$year[nrow(mod.input.sf)] <- 2020
mod.input.sf$Year[nrow(mod.input.sf)] <- which(years == 2020)
mod.input.sf$I[nrow(mod.input.sf)] <- NA
mod.input.sf$IR[nrow(mod.input.sf)] <- NA
mod.input.sf$tot.live.com[nrow(mod.input.sf)] <- NA
mod.input.sf$L[nrow(mod.input.sf)] <- 0
mod.input.sf$N[nrow(mod.input.sf)] <- 0

mod.input.sf <- mod.input.sf[order(mod.input.sf$year),]

bbn.fish.sf <- st_as_sf(bbn.fish,coords = c("lon","lat"),remove =F, crs = 4326)
bbn.fish.sf <- bbn.fish.sf %>% st_transform(crs= 32619)


# Now lets clip this to be data inside of our bbn boundary.
bbn.fish.sf <- st_intersection(bbn.fish.sf,bbn.shape)

# Check removals each fishing year calculated using this data
bbn.fish.by.year <- bbn.fish.sf %>% dplyr::group_by(year) %>% dplyr::summarise(tot = sum(pro.repwt,na.rm=T))
bbn.fish.by.survey.year <- bbn.fish.sf %>% dplyr::group_by(survey.year,.drop=F) %>% dplyr::summarise(tot = sum(pro.repwt,na.rm=T))
# So this looks reasonable in the most recent years, but I probably need to check the early years to see if we are missing any of the removals, from above check (only 12 points removed) it 
# seems like we might be fine, but need to check against our historical Removals estimates...
#tail(bbn.fish.by.year)
#tail(bbn.fish.by.survey.year)

# Subset the fishery data as necessary
bbn.fish <- bbn.fish |>collapse::fsubset(survey.year %in% years)
bbn.fish.sf <- bbn.fish.sf |> collapse::fsubset(survey.year %in% years)
# OK, so now let's see if we can use the catch knot thing Raph made to split this up withing the BBn domain
#We just need 3 columns for this
catch.sf <- bbn.fish.sf %>% dplyr::select(pro.repwt,survey.year)
names(catch.sf) <- c("Catch","Year","geometry")
#catch.sf$geometry <- catch.sf$geometry/1000
#catch.sf |> data.frame() |> collapse::fgroup_by(Year) |> collapse::fsummarize(sum = sum(Catch))
# Set up the mesh

bbn.mesh <- setup_mesh(mod.input.sf,model_bound = bbn.shape,nknot=num.knots, max.edge = c(3,10),cutoff=2,seed=66) # Seeds 20 and 66 work
bbn.mesh.sf <- inla.mesh2sf(bbn.mesh$mesh)
bbn.mesh.sf$triangles$geometry <- bbn.mesh.sf$triangles$geometry*1000
bbn.mesh.sf$vertices$geometry <- bbn.mesh.sf$vertices$geometry*1000
st_crs(bbn.mesh.sf$triangles) <- 32619
st_crs(bbn.mesh.sf$vertices) <- 32619
knots.sf <- st_as_sf(as.data.frame(bbn.mesh$knots$centers), coords = c("X","Y")) 
knots.sf$geometry <- knots.sf$geometry*1000
st_crs(knots.sf) <- 32619

# Plot the mesh
#ggplot(bbn.mesh.sf$triangles) + geom_sf() + geom_sf(data= bbn.shape,fill = NA,color = 'darkblue',size=2) + geom_sf(data = knots.sf,fill = NA)
# Now make the prediction grid
pred.grid<-setup_pred_grid(knots=bbn.mesh$knots,model_bound=bbn.mesh$utm_bound)
# Plot the grid
#ggplot(pred.grid$grid) + geom_sf(aes(fill = as.factor(knotID))) + scale_fill_viridis_d()

# For the moment we need to have this starting at year 1.
catch.sf$Year <-  catch.sf$Year - (min(years)-1)
catchy <- catch_spread(catch = catch.sf,knots = bbn.mesh$knots)
catchy$sum_catches[,ncol(catchy$sum_catches)+1] <- 0

# Setup the data for the  model run
set_data<-data_setup(data=mod.input.sf,growths=data.frame(g = g$g,gR = g$gR),catch=as.data.frame(catchy$sum_catches),
                     model="SEBDAM",mesh=bbn.mesh$mesh,obs_mort=T,prior=T,prior_pars=c(20,40),
                     mult_qI=vary.q,spat_approach="spde",
                     knot_obj=bbn.mesh$knots,knot_area=pred.grid$area,separate_R_aniso = T,
                     all_se=T,weighted_mean_m = T)

  # So this will fix the mean value of m0 to be whatever the initial value is set at in the data_setup step.  Let's see what happens!
  #set_data$par$log_m0 <- log(init.m)
  #set_data$par$log_R0 <- l.init.R 
  set_data$par$log_qR <- log(qR)
  set_data$par$log_S <- log(0.0695)
  # #set_data$map <-list(log_m0=factor(NA),log_R0 = factor(NA),log_qR = factor(NA))
  set_data$map <-list(log_qR = factor(NA),
                      log_S = factor(NA))
  #set_data$map <-list(log_qR = factor(NA))
  #set_data$map <-list(log_m0=factor(NA))



mod.fit<-fit_model(set_data,silent=F)

  # And save the model
  saveRDS(mod.fit,paste0(direct,"Data/Model/",assess.year,"/BBn/Results/BBn_model_results.Rds"))
  saveRDS(bbn.mesh,paste0(direct,"Data/Model/",assess.year,"/BBn/Results/BBn__mesh.Rds"))
  saveRDS(pred.grid,paste0(direct,"Data/Model/",assess.year,"/BBn/Results/BBn__predict_grid.Rds"))
  saveRDS(mod.input.sf,paste0(direct,"Data/Model/",assess.year,"/BBn__model_input.Rds"))



################################################### End the initial model runs ###########################################
################################################### End the initial model runs ###########################################
################################################### End the initial model runs ###########################################
### Make the figures for the models


#if(mod.select == "TLM")  scenario.select <- paste0(min(years),"_",max(years),"_qR_",exp(lqr),"_new_g")
# If we are going with the no extra station model this is our scenario.
#scenario.select <- "1994_2022_vary_m_m0_1_R0_150_10_knots_No_extra_stations"
mod.fit <- readRDS(paste0(direct,"Data/Model/",assess.year,"/BBn/Results/BBn_model_results.Rds"))
catchy <- mod.fit$obj$env$data$C*mod.fit$obj$env$data$area # Get this into tonnes from catch density.

#This is only needed for SEAM.
  pred.grid <- readRDS(paste0(repo.loc,"Results/BBn/R_",R.size,"_FR_",FR.size,"/BBn_",mod.select,"_model_output_",scenario.select,"_predict_grid.Rds"))
  
  # Now set up to run the figures
  matYear<-c(rep(c(years,(max(years)+1)),each=num.knots))
  matYear1<-c(rep(years,each=num.knots))
  knots<-rep(1:num.knots,NY+1)
  knots1<-rep(1:num.knots,NY)
  
  grid.gis <- pred.grid$grid
  grid.gis$geometry <- grid.gis$geometry*1000
  st_crs(grid.gis) <- 32620
  # Now simplify the grid for the spatial plots...
  knot.gis <- aggregate(grid.gis, list(grid.gis$knotID), function(x) x[1])
  
  
  # Get the spatial data output
  B<-data.frame(B=as.vector(mod.fit$report$B),Year=matYear,knotID=knots)
  B.dat.plot<-left_join(knot.gis,B,by=c("knotID"))
  # Recruits
  R<-data.frame(R=as.vector(mod.fit$report$R),Year=matYear1, knotID=knots1)
  R.dat.plot<-left_join(knot.gis,R,by=c("knotID"))
  #Natural Mortality
  m<-data.frame(m=as.vector(mod.fit$report$m),Year=matYear,knotID=knots)
  m.dat.plot<-left_join(knot.gis,m,by=c("knotID"))
  #Spatial q's
  qI<-data.frame(qI=as.vector(mod.fit$report$qI),knotID=unique(knots))
  q.dat.plot<-left_join(knot.gis,qI,by=c("knotID"))
  
  # Explotation
  # The catch data
  # So catches from June 2021-May 2022 are called 2021 and removed from the 2021 survey biomass (this is different indexing from how we used to handle this for offshore)
  
  F.dat<-data.frame(B=as.vector(mod.fit$report$areaB[,-ncol(mod.fit$report$areaB)]/1000),
                    C = c(as.vector(as.matrix(catchy[,-c((ncol(catchy)-1),ncol(catchy))])),rep(NA,num.knots)), Year=matYear1, knotID=knots1)
  F.dat <- F.dat %>% dplyr::mutate(exploit = C/(B+C)) # Sticking with how offshore does this (C/(B+C)) C/B or some variant may be more realistic
  F.dat.plot<-left_join(knot.gis,F.dat,by=c("knotID"))
  
  F.dat.plot <- F.dat.plot %>% dplyr::filter(Year != max(years) )

  # Smaller text for spatial figures.
  theme_set(theme_few(base_size = 14))
  
  #Spatial predictions
  #B
  #B_plot <- st_transform(B_plot,crs = 4326)
  # Set up pretty breaks for the figure
  b.brk <- pretty(log(B.dat.plot$B))
  b.lab <- signif(exp(b.brk),digits=2)
  spatial.B.plot<- ggplot() + geom_sf(data=B.dat.plot%>% dplyr::filter(!Year %in% c(2023)),aes(fill=log(B)),color='grey')+
    facet_wrap(~Year)+ 
    scale_x_continuous(breaks = c(-60.3,-60.1,-59.9), labels = c("60B018'W","60B06'W","59B054'W")) +
    scale_y_continuous(breaks = c(42.6, 42.75, 42.9),labels = c("42B036'N","42B045'N","42B054'N")) +
    scale_fill_viridis_c(breaks = b.brk, labels=b.lab, name="Predicted Biomass \nDensity (kg\U2022km\U207B\U00B2)",option = "A",begin=0.2) + 
    theme(axis.text.x=element_text(angle=-45,hjust=0))
  save_plot(paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/BBn_Spatial_biomass.png"),spatial.B.plot,base_width = 10,base_height = 10)
 
  #Recruits
  r.brk <- pretty(log(R.dat.plot$R))
  r.lab <- signif(exp(r.brk),digits=2)
  
  spatial.R.plot<-  ggplot() + geom_sf(data=R.dat.plot,aes(fill=log(R)),col='grey')+
    scale_x_continuous(breaks = c(-60.3,-60.1,-59.9), labels = c("60B018'W","60B06'W","59B054'W")) +
    scale_y_continuous(breaks = c(42.6, 42.75, 42.9),labels = c("42B036'N","42B045'N","42B054'N")) +
    facet_wrap(~Year)+ 
    scale_fill_viridis_c(breaks = r.brk, labels = r.lab,name="Predicted Recruit \nDensity (kg\U2022km\U207B\U00B2)",end=0.8)+ 
    theme(axis.text.x=element_text(angle=-45,hjust=0))
  save_plot(paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/BBn_Spatial_recruits.png"),spatial.R.plot,base_width = 10,base_height = 10)
 
  #m
  m.brk <- log(c(0.003,0.007,0.02,0.05,0.15,0.4,1))
  #m.brk <- pretty(log(m.dat.plot$m))
  m.lab <- signif(exp(m.brk),digits=2)
  
  spatial.m.plot <-  ggplot() + geom_sf(data=m.dat.plot %>% dplyr::filter(!Year %in% c(2023)),aes(fill=log(m)),color='grey')+
    scale_x_continuous(breaks = c(-60.3,-60.1,-59.9), labels = c("60B018'W","60B06'W","59B054'W")) +
    scale_y_continuous(breaks = c(42.6, 42.75, 42.9),labels = c("42B036'N","42B045'N","42B054'N")) +
    facet_wrap(~Year)+ 
    scale_fill_viridis_c(breaks = m.brk, labels = m.lab,name="Predicted Natural \nMortality (Inst)",option = "B",direction =1,begin = 0.2,end=1) + 
    theme(axis.text.x=element_text(angle=-45,hjust=0))
  save_plot(paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/BBn_Spatial_mort.png"),spatial.m.plot,base_width = 10,base_height = 10)
  
  # q 
  spatial.q.plot <- ggplot() + geom_sf(data=q.dat.plot,aes(fill=qI),col=NA)+
    scale_x_continuous(breaks = c(-60.3,-60.1,-59.9), labels = c("60B018'W","60B06'W","59B054'W")) +
    scale_y_continuous(breaks = c(42.6, 42.75, 42.9),labels = c("42B036'N","42B045'N","42B054'N")) +
    scale_fill_viridis_c(name="Predicted catchability (qI)",option = "C",begin = 0.2,end =0.8) + 
    theme(axis.text.x=element_text(angle=-45,hjust=0))
  save_plot(paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/BBn_Spatial_catchability.png"),spatial.q.plot,base_width = 10,base_height = 10)
  
  
  # OK, so lets try an make a map of the spatial exploitation rates, not sure if this is all correct yet.
  # Log rescalling doesn't work because we have 0's here.
  # OK, so lets try an make a map of the spatial exploitation rates, not sure if this is all correct yet.
  F.dat.plot$exp.na <- NA
  F.dat.plot$exp.na[F.dat.plot$exploit != 0] <- F.dat.plot$exploit[F.dat.plot$exploit != 0]
  
  #e.brk <- log(c(0.0015,0.01,0.08,0.4))
  e.brk <- pretty(log(F.dat.plot$exp.na))
  e.lab <- signif(exp(e.brk),digits=2)
  
  
  spatial.exploit.plot<- ggplot() + geom_sf(data=F.dat.plot,aes(fill=log(exp.na)),color='grey') +
    scale_x_continuous(breaks = c(-60.3,-60.1,-59.9), labels = c("60B018'W","60B06'W","59B054'W")) +
    scale_y_continuous(breaks = c(42.6, 42.75, 42.9),labels = c("42B036'N","42B045'N","42B054'N")) +
    facet_wrap(~Year) + 
    scale_fill_viridis_c(breaks = e.brk,labels = e.lab,name="Exploitation (Prop)",option = "D") + 
    theme(axis.text.x=element_text(angle=-45,hjust=0))
  save_plot(paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/Spatial_exploit.png"),spatial.exploit.plot,base_width = 10,base_height = 10)
  
  
# Now to make the time series plots of the summarized data  

pred.proc <- get_processes(mod.fit)

  # Get the overall estimates + the 95% CI
  pred.proc$log_processes$year <- c(years,max(years)+1)
  pred.proc$log_processes$log_B <- pred.proc$log_tot_frame$log_totB
  pred.proc$log_processes$log_R <- pred.proc$log_tot_frame$log_totR
  pred.proc$log_processes$log_m <- pred.proc$log_tot_frame$log_mean_m
  pred.proc$log_processes$totB.LCI <- exp(pred.proc$log_tot_frame$log_totB - 1.96*pred.proc$log_tot_frame$se_log_totB)
  pred.proc$log_processes$totB.UCI <- exp(pred.proc$log_tot_frame$log_totB + 1.96*pred.proc$log_tot_frame$se_log_totB)
  pred.proc$log_processes$totR.LCI <- exp(pred.proc$log_tot_frame$log_totR - 1.96*pred.proc$log_tot_frame$se_log_totR)
  pred.proc$log_processes$totR.UCI <- exp(pred.proc$log_tot_frame$log_totR + 1.96*pred.proc$log_tot_frame$se_log_totR)
  pred.proc$log_processes$m.LCI <- exp(pred.proc$log_tot_frame$log_mean_m - 1.96*pred.proc$log_tot_frame$se_log_mean_m)
  pred.proc$log_processes$m.UCI <- exp(pred.proc$log_tot_frame$log_mean_m + 1.96*pred.proc$log_tot_frame$se_log_mean_m)
  pred.proc$log_processes <- as.data.frame(pred.proc$log_processes)
# SEBDAM Version
# Annual explotation
catch.annual <- data.frame(totC = colSums(catchy[,-ncol(catchy)]), Year = years)
pred.proc$log_processes <- pred.proc$log_processes %>% dplyr::filter(year < (assess.year-1))

# The catch data
# So catches from June 2021-May 2022 are called 2021 and removed from the 2021 survey biomass (this is different indexing from how we used to handle this for offshore)
# SS model mu(t) <- C(t) / (B(t) + C(t)) because our model is B(t) <- B(t-1) - C(t) and C(2017) is June 2016-Aug 2017.
#  mu[2017] <- C[June 2016-Aug 2017]/(B[2017]+C[June 2016-Aug 2017]) 
# TLM and SEAM don't calculate mu, so we do it manually here, to be analogous...
# SEAM/TLM mu(t) <- C(t-1) / (B(t) + C(t-1)) because our model is B(t) <- B(t-1) - C(t-1) and C(2016) is now June 2016-Aug 2017.
# mu[2017] <- C[June 2016-Aug 2017]/(B[2017]+C[June 2016-Aug 2017]) 
ann.exploit <- data.frame(year = years,B = exp(pred.proc$log_processes$log_B), Catch = c(colSums(catchy[,-c((ncol(catchy)-1),ncol(catchy))]),NA),
                          B.LCI = pred.proc$log_processes$totB.LCI, B.UCI = pred.proc$log_processes$totB.UCI)


ann.exploit$exploit <- c(ann.exploit$Catch[1:(nrow(ann.exploit)-1)]/(ann.exploit$B[2:nrow(ann.exploit)]+ann.exploit$Catch[1:(nrow(ann.exploit)-1)]),NA)
ann.exploit$exploit.UCI <- c(ann.exploit$Catch[1:(nrow(ann.exploit)-1)]/(ann.exploit$B.UCI[2:nrow(ann.exploit)]+ann.exploit$Catch[1:(nrow(ann.exploit)-1)]),NA)
ann.exploit$exploit.LCI <- c(ann.exploit$Catch[1:(nrow(ann.exploit)-1)]/(ann.exploit$B.LCI[2:nrow(ann.exploit)]+ann.exploit$Catch[1:(nrow(ann.exploit)-1)]),NA)
ann.exploit$FM <- 1-exp(-ann.exploit$exploit)
ann.exploit$FM.LCI <- 1-exp(-ann.exploit$exploit.LCI)
ann.exploit$FM.UCI <- 1-exp(-ann.exploit$exploit.UCI)


# Biomass time series
bm.ts.plot <- ggplot(pred.proc$log_processes) + geom_line(aes(year,exp(log_B)),color='firebrick2',linewidth=1.5) + 
  geom_ribbon(aes(ymin=totB.LCI,ymax=totB.UCI,x=year),alpha=0.2,fill='darkblue',color='darkblue') +
  xlab("") + ylab("Fully Recruited Biomass (tonnes)") + scale_x_continuous(breaks = seq(1980,2030,by=3)) + ylim(c(0,3.1e4))
save_plot(paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/BBn_Biomass_time_series.png"),bm.ts.plot,base_width = 11,base_height = 8.5)
# Recruit time series
rec.ts.plot <- ggplot(pred.proc$log_processes) + geom_line(aes(year,exp(log_R)),color='firebrick2',linewidth=1.5) + 
  geom_ribbon(aes(ymin=totR.LCI,ymax=totR.UCI,x=year),alpha=0.2,fill='darkblue',color='darkblue') + 
  xlab("") + ylab("Recruit Biomass (tonnes)")  + scale_x_continuous(breaks = seq(1980,2030,by=3)) + ylim(c(0,7e3))
save_plot(paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/BBn_Recruit_time_series.png"),rec.ts.plot,base_width = 11,base_height = 8.5)
# Natural mortality time series...
mort.ts.plot <- ggplot(pred.proc$log_processes) + geom_line(aes(year,exp(log_m)),color='firebrick2',linewidth=1.5) + 
  geom_ribbon(aes(ymin=m.LCI,ymax=m.UCI,x=year),alpha=0.2,fill='darkblue',color='darkblue') + 
  xlab("") + ylab("Natural mortality (Instantaneous)") + scale_x_continuous(breaks = seq(1980,2030,by=3)) + ylim(c(0,0.35))
save_plot(paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/BBn_nat_mort_time_series.png"),mort.ts.plot,base_width = 11,base_height = 8.5)
# Explotation Rate Time Series
exploit.plot <- ggplot(ann.exploit) + geom_line(aes(x=year,y=exploit),size=1.5) + geom_ribbon(aes(ymin=exploit.LCI,ymax=exploit.UCI,x=year),alpha=0.2,fill='darkblue',color='darkblue') +
  xlab("") + ylab("Exploitation Rate (Proportional)") + scale_x_continuous(breaks = seq(1980,2030,by=3)) + ylim(c(0,0.3))
save_plot(paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/BBn_exploit_time_series.png"),exploit.plot,base_width = 11,base_height = 8.5)



# Annual exploitation
saveRDS(ann.exploit,paste0(direct,"Data/Model/",assess.year,"/BBn__annual.exploit.Rds"))
# The nicely summarized model object
saveRDS(pred.proc,paste0(direct,"Data/Model/",assess.year,"/BBn_pred_proc.Rds"))
# The summarized spatial bits.
saveRDS(F.dat.plot,paste0(direct,"Data/Model/",assess.year,"/BBn_F_spatial.Rds"))
# q spatial
saveRDS(q.dat.plot,paste0(direct,"Data/Model/",assess.year,"/BBn_q_spatial.Rds"))
# m spatial
saveRDS(m.dat.plot,paste0(direct,"Data/Model/",assess.year,"/BBn_m_spatial.Rds"))
# R spatial
saveRDS(R.dat.plot,paste0(direct,"Data/Model/",assess.year,"/BBn_R_spatial.Rds"))
# B spatial
saveRDS(B.dat.plot,paste0(direct,"Data/Model/",assess.year,"/BBn_B_spatial.Rds"))


# Now for the decision Table
# We need to get the landings for the previous year after the survey (in this case the Landings from June-December 2022)
bbn.fish$months <- month(bbn.fish$date)
psl.months <-  6:12
bbn.psl <- bbn.fish %>% dplyr::filter(year == 2022 , months %in% psl.months)
bbn.psl.total <- sum(bbn.psl$pro.repwt,na.rm=T)

# So we can now make our decision table
catchs <- seq(0,600,by=25)
# Reference Points
TRP <- NULL # Insert number once available
USR <- NULL #  Insert number once available
LRP <- NULL # Insert number once available
RR  <-   NULL # Insert number once available, in this case it is a % value (i.e., exploitation rate, not F) 
n.sims <- 1e6

# Decision Table, setting the g terms to 1 means it uses the most recent years growth estimate.
bbn.dt <- dec.tab(mod.select = "SEAM",data = mod.fit, catch.scenarios = catchs,PSL = bbn.psl.total,
                  n.sims = n.sims,TRP = TRP,USR = USR,LRP = LRP, RR = RR,RR.TRP = NULL,g.adj=1,gR.adj=1)


saveRDS(bbn.dt,paste0(direct,"/",assess.year,"Updates/BBn/decision_table.Rds"))
write_csv(bbn.dt$table,paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/bbn_decision_table.csv"))

# Turn the decision table into something nicely formated for Word 
tab <- kableExtra::kbl(bbn.dt$table, booktabs = TRUE, escape =F, format = 'pipe',align = c('l','l','l','r','l'))#,
#caption = cap) %>%
#kable_styling(full_width = F) %>% row_spec(c(2:10,12,14:18,20), bold = T) %>%
#kable_styling(full_width = F) %>% row_spec(c(2,4,5,8:12,14,17,20), italic = T) %>%
#add_footnote(notation = 'number',ft.note,escape=F)
saveRDS(tab,paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/bbn_decision_table_word.Rds"))

# pdf version of the same
tab.pdf <- kableExtra::kbl(bbn.dt$table, booktabs = TRUE, escape =F, format='latex',align = c('l','l','l','r','l'))#,
#caption = cap) %>%
#kable_styling(full_width = F) %>% row_spec(c(2:10,12,14:18,20), bold = T) %>%
#kable_styling(full_width = F) %>% row_spec(c(2,4,5,8:12,14,17,20), italic = T) %>%
#kable_styling(latex_options = c("hold_position","scale_down")) %>%
#add_footnote(notation = 'number',ft.note,escape=F)
saveRDS(tab.pdf,paste0(direct,"/",assess.year,"Updates/BBn/Figures_and_tables/bbn_decision_table_pdf.Rds"))



