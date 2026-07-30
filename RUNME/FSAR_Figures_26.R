## Browns Bank SFA 26A

### Set up
#Notes/Comments/Changes
# showtext() library is being used to help solve issues with unicode characters - need to be turned off when using ggsave() 
# to create figures, then back on when finished saving image. 

# showtext() NOTE: I couldn't properly render unicodes for special character's in the figures or body text, 
# with the only work around being using library(showtext) and showtext_auto(). **HOWEVER** while this fixes display issues in 
# RStudio and the knitted output, it causes major scaling issues when saving plots (i.e., ggsave()) which I use to maintain graphics quality 
# for the panel images.  showtext, can sometimes scale text differently in ggsave() than in the RStudio plotting window. Since showtext_auto() 
# affects all plots, you can temporarily disable it when saving: 
#showtext_auto(FALSE)  # Disable show text for saving
#ggsave()
#showtext_auto(TRUE)


#This code creates the panel figures for the FSAR reports for both GBa and BBn, and additionally saves each subpanel as an individual figure for use in presentations, etc. 

#Created by Claire Haar (in spring 2025), 

# DK split into a 26 and 27, made it an R script instead of an R markdown in summer 2025

### Set up
require(ggrepel)
library(boot)
library(cowplot)
library(dplyr)
library(fields)
library(flextable)
library(ggplot2)
library(ggthemes)
library(ggpattern)
library(knitr)
library(kableExtra)
library(maps)
library(mapdata)
library(officer)
library(optimx)
library(pander)
library(PBSmapping)
library(plyr)
library(reshape2)
library(scales)
library(sp)
library(sf)
library(splancs)
library(stringr)
library(SEBDAM)
library(tidyr)
library(tidyverse)
library(TMB)
library(viridis)
library(showtext)
showtext_auto()



funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/convert_inla_mesh_to_sf.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/French_west_labeller.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Fishery/CPUE_monthly_or_observer.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Fishery/logs_and_fishery_data.r",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/github_spatial_import.R"
          )
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
}

# Directories
direct <- "Y:/Offshore/Assessment/"
#direct_off <- "C:/Users/mcdonaldra/Documents/"
#direct_fns <- "C:/Users/HAARC/Documents/Github/Assessment_fns/"
direct_out <- "C:/Users/keyserf/Documents/"
repo <- direct_out 

# Set up
bank <- "BBn" # set your bank for data/directory filtering
banks <- c("BBn") # pick your banks for TAC/Landings plot
fleets <- c("FT", "WF") # pick your fleets for TAC/Landings plot
assess.year <- 2026 # current year - use for database filtering
alpha<-0.05 # sets the CI's (0.5 is set, and most code uses alpha/2 for 95% CIs)
language <- "french"
#language <- "english"
year <- 2026 # current year - used for directories and `r ` calls in text
yr <- 2025 # fishery year - used for data searching and `r ` calls in text
years<-1994:yr 
years.ribbon <- years
years.ribbon[length(years.ribbon)] <- years.ribbon[length(years.ribbon)]+0.15

#Whenever reference points are set:
#URP<-NULL
LRP<-2000 # set manually for now, no USR
#RR<-NULL
SFA<- "SFA 26A"

# Sources
shpf <- st_read(paste0(repo,"Github/GIS_layers/survey_boundaries/BBn.shp")) # BBN shapefile
TACs <- read.csv(paste0(direct_out, "/Data/Model/Ref_pts_and_tac.csv"))
interim.tac <- TACs$TAC[TACs$bank==bank & TACs$year == year]
mod.fit<-readRDS(paste0(direct_out,"Data/Model/",year,"/BBn/Results/BBn_model_results.RDS"))
#mod.dat<-readRDS(paste0(direct_out,"Data/Model/",year,"/BBn/Results/Model_input.RData"))
#mod.dat<-mod.dat[which(mod.dat$year>1993),]
bbn.psl<-readRDS(paste0(direct_out,assess.year,"/Updates/BBn/Figures_and_tables/bbn_post_survey_landings.Rds"))
bbn.psl.total <- round(sum(bbn.psl$pro.repwt,na.rm=T),0)
#barp<-data.frame(year=1994:2024)
#mod.dat<-left_join(barp,mod.dat)
#fish.dat<-readRDS("Y:/Offshore/Assessment/Framework/SFA_25_26_2024/Model/Data/BBn_fish.dat.RDS")
#pred.grid<-readRDS("Y:/Offshore/Assessment/Framework/SFA_25_26_2024/Model/Results/BBn/R_75_FR_90/BBn_SEAM_model_output_1994_2022_qR_0.33_20_knots_predict_grid.RDS")
# I suspect these two thing may not have made it onto the network from Claires computer sadly.

pred.grid <-readRDS(paste0(direct_out,"Data/Model/",year,"/BBn/Results/BBn_predict_grid.Rds"))
#projection<-readRDS("Y:/Offshore/Assessment/Framework/SFA_25_26_2024/Model/Results/BBn/R_75_FR_90/SEAM_1994_2022_qR_0.33_20_knots/Decision_Table/TRP_USR_LRP_RR_g_adj_1_gR_adj_1.Rds")
projection<-readRDS(paste0(direct_out,"Data/Model/",year,"/BBn/Results/R_75_FR_90/SEAM_1994_2026_qR_0.33_20_knots/Decision_Table/TRP_USR_LRP_2000_RR_g_adj_avg_gR_adj_avg.RDS"))

# B.projection<-comma(round(projection$table[which(projection$table[,1]==TACs$TAC[TACs$year==year & TACs$bank==bank]),3], 0))
# CH: Fk had it set up that projection and D.tab use the same source file? Changed D.tab to have file with explicit LRP of 2000
# D.tab<-readRDS(paste0(direct,"Data/Model/",year,"/BBn/Results/R_75_FR_90/SEAM_1994_2025_qR_0.33_20_knots/Decision_Table/TRP_USR_LRP_2000_RR_g_adj_avg_gR_adj_avg.RDS"))

# Old Notes (FK)
# load(paste(direct,"Data/Model/", year, "/BBn/Results/Model_testing_results_mixed.RData",sep=""))
# load(paste(direct_off,"Data/Model/", year, "/BBn/Results/Final_model_results.RData",sep=""))
# load(paste(direct,"Data/Model/", year, "/BBn/Results/Model_results_and_diagnostics_mixed.RData",sep=""))
# load(paste(direct_off,"Data/Model/", year, "/BBn/Results/Model_results_and_diagnostics_mixed.RData",sep=""))
# mod.fit<-readRDS("Y:/Offshore/Assessment/Framework/SFA_25_26_2024/Model/Results/BBn/R_75_FR_90/BBn_SEAM_model_output_1994_2022_qR_0.33_20_knots.RDS")
#Take out of 75 90
#The model run and the plots are in the Github repo, SEAM_BBn_Step_1_model
#Need predict grid in there
#B.rec is recruitment for sure

# Fish.dat
# Method above has incorrect values for early (<2000) catches - obtain from logs_and_fish
logs_and_fish(loc="offshore",year = 1994:yr,un=un,pw=pw,db.con=db.con,direct=direct_out)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
bbn.fish.dat <- fish.dat |> collapse::fsubset(bank=="BBn") |> collapse::fgroup_by(year) |> collapse::fsummarise(C = sum(pro.repwt,na.rm=T)/1000)
bbn.fish <- fish.dat |> collapse::fsubset(bank=="BBn") 

options(scipen=999)

### Model domain map
theme_set(theme_few(base_size=22))

## Define map set-up in pecjector
bbnloc <- pecjector(area =list(y = c(42.1,43.81),x = c(-67.9,-65.1),crs = 4326), plot = T, txt.size = 10, axes = "DM",
                    add_layer = list(land = 'grey', eez = 'eez' , sfa = 'offshore', bathy = c(50,'both', 500), 
                                     scale.bar = c('br',0.4,-1.35,-1.35)),language = language) + coord_sf(expand=F)
#sf package update for spherical geometry results in slightly different area calcs than before, so force it NOT to use spherical geometry here.
sf::sf_use_s2(FALSE)
shpf$are_km2 <- as.numeric(st_area(shpf)/1000000)
shpf$are_km2 <- round(shpf$are_km2)
shpf$`Area (km^2)` <- factor(shpf$are_km2,levels = shpf$are_km2)

# all we want here is SFA labels with subarea distinctions (eg. SFA 26A) so were making/adding them manually rather than altering pecjector to have a new label option
labels_df <- data.frame(
  x = c(-65.6, -66.75, -66.75, -66.275, -66.95, -65.4),
  y = c(43.7, 43.1, 42.9, 42.25, 42.25, 42.6))
if(language == "english") labels_df$label <- c("Nova \nScotia", "SFA 26C", "SFA 26A", "SFA 27B", "SFA 27A", "SFA 26B")
if(language != "english") labels_df$label <- c("Nouvelle \nÉcosse", "ZPP 26C", "ZPP 26A", "ZPP 27B", "ZPP 27A", "ZPP 26B")
labels_sf <- st_as_sf(labels_df, coords = c("x", "y"), crs = 4326)

if(language == "english") fig.lab <- "Model domain"
if(language != "english") fig.lab <- "Domaine du modèle"

## Add customizations to the map to suit needs  
bbnloc2 <- bbnloc  + new_scale("fill") + 
  geom_sf_text(data = labels_sf, aes(label = label), size = 3)+ # adds the labels we defined above
  geom_sf_pattern(data=shpf, aes(fill=fig.lab),
                  pattern = "stripe", # direct pattern assignment (not mapped)
                  pattern_spacing = 0.02,
                  pattern_density = 0.4,
                  pattern_fill = "sienna2",
                  pattern_colour = "sienna2",
                  colour = NA,
                  alpha=0.2) + # plots the model domain
  scale_fill_manual(name="",values="sienna2") + # specifies the colour of the model domain
  coord_sf(crs = 4326, default_crs = 4326, xlim = c(-67.9,-65.1), ylim = c(42.1,43.81), clip = "on", expand = FALSE) + # was having issue with plot collpasing on itself - these conditions help maintain the proper plot area set up in pecjector
  theme(legend.position = c(0.001, 0.965), # though 0.955 seems strange, this level of accuracy is indeed needed for the scaling/pos of the legend
        legend.justification = 'left',legend.key.size = unit(0.75,"line"),
        legend.margin = margin(4,4,4,4),
        #legend.box.margin = margin(0,0,-10,0),
        legend.key = element_rect(fill = NA),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black", size=0.4),
        panel.border = element_rect(linewidth = 0.5, fill = NA),
        axis.title = element_blank(),
        plot.margin = margin(1, 5, 0, 0, "points"))
bbnloc2
## Save the map to get a high-quality figures to include later
showtext_auto(FALSE)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_SurveyAreaMap.png"), bbnloc2, dpi = 600, width = 5.5, height = 4.5)
if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_SurveyAreaMap_french.png"), bbnloc2, dpi = 600, width = 5.5, height = 4.5)

showtext_auto(TRUE)

#############  AN OVERALL PLOT OF THE BANKS AND THAT WILL BE THAT...
# Also make the overall plot of the banks...
# set up the labels first

labels <- github_spatial_import(subfolder = "other_boundaries/labels", zipname = "labels.zip")
offshore <- github_spatial_import(subfolder = "offshore", zipname = "offshore.zip")

labels <- labels[grepl('offshore_detailed',labels$region),]

sfa.labels <- labels[grep(x=labels$lab_short, "SFA"),]

sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="A ", replacement="A", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern=" (", replacement="\n", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern=")", replacement="", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern=" Bank", replacement="", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="-BAN", replacement="", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="north", replacement="North", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="south", replacement="South", fixed=T)
sfa.labels$lab_short[sfa.labels$lab_short=="SFA27A"] <- "SFA27A\nGeorges 'a'"
sfa.labels$lab_short[sfa.labels$lab_short=="SFA27B"] <- "SFA27B\nGeorges 'b'"
sfa.labels <- sfa.labels[-grep(pattern = "Includes", x=sfa.labels$lab_short),]
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="26\nG", replacement="26C\nG", fixed=T)
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="26\nBrowns North", replacement="26A\nBrowns North", fixed=T)
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="26\nBrowns South", replacement="26B\nBrowns South", fixed=T)
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="25\nBa", replacement="25B\nBa", fixed=T)
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="25\nEa", replacement="25A\nEa", fixed=T)

sfa.labels <- sfa.labels %>%
  tidyr::separate(lab_short, into=c("SFA", "bank"), sep="\n", remove=F)
sfa.labels <- sfa.labels[!is.na(sfa.labels$bank),]

sf_use_s2(FALSE)
joined <- st_join(offshore, sfa.labels)

joined <- joined[!(joined$bank=="Banquereau" & joined$ID.x=="SFA25A"),]
joined <- joined[!is.na(joined$EID),]
joined$fr <- joined$lab_short
joined$fr <- gsub(x=joined$fr, pattern="Browns South", replacement ="Sud de Brown")
joined$fr <- gsub(x=joined$fr, pattern="Browns North", replacement ="Nord de Brown")
joined$fr <- gsub(x=joined$fr, pattern="Georges 'a'", replacement ="Georges \u00ABa\u00BB")
joined$fr <- gsub(x=joined$fr, pattern="Georges 'b'", replacement ="Georges \u00ABb\u00BB")
joined$fr <- gsub(x=joined$fr, pattern="Eastern Scotian Shelf", replacement ="Est du plateau n\u00E9o-\u00E9cossais")
joined$fr <- gsub(x=joined$fr, pattern="SFA", replacement ="ZPP")

nonGB <- joined[!joined$SFA %in% c("SFA27A", "SFA27B", "SFA10", "SFA11", "SFA12"),]
GBalab <- joined[joined$SFA %in% c("SFA27A"),]
GBblab <- joined[joined$SFA %in% c("SFA27B"),]

showtext_auto(FALSE)

p <-  pecjector(area = list(y = c(40,48),x = c(-68,-54),crs = 4326), add_layer = list(land = 'grey',
                                              eez = 'eez',
                                              sfa='offshore',
                                              bathy=c(100, 'c', 200),  scale.bar = c('br',0.4,-1.35,-1.35)),
                                              language = language,
                c_sys = 4326, quiet=T, plot=F)

if(language=="english") {
  png(paste0(direct_out, year,"/Updates/", bank, 
             "/Figures_and_tables/Offshore_banks.png",sep=""),width=10,height=7,res=920,units="in")
  p <-  p + geom_sf_text(data = nonGB[!(nonGB$SFA%in% c("SFA11", "SFA26B")),], 
                         aes(label = lab_short), 
                         fun.geometry = sf::st_centroid) +
    geom_sf_text(data = nonGB[nonGB$SFA=="SFA11",], 
                 aes(label = lab_short), 
                 fun.geometry = sf::st_centroid, 
                 nudge_x=-0.5, nudge_y=-0.5) +
    geom_sf_text(data = nonGB[nonGB$SFA=="SFA26B",], 
                 aes(label = lab_short), 
                 fun.geometry = sf::st_centroid, 
                 nudge_y=0.5) +
    geom_text_repel(data = GBalab, 
                    aes(x=as.data.frame(st_coordinates(st_centroid(GBalab)))$X,
                        y=as.data.frame(st_coordinates(st_centroid(GBalab)))$Y,
                        label = lab_short), 
                    nudge_x=-1, nudge_y=-0.5, direction = "x", min.segment.length=0, box.padding = 0) +
    geom_text_repel(data = GBblab, 
                    aes(x=as.data.frame(st_coordinates(st_centroid(GBblab)))$X,
                        y=as.data.frame(st_coordinates(st_centroid(GBblab)))$Y,
                        label = lab_short), 
                    nudge_x=1.25, nudge_y=-0.5, direction = "x", min.segment.length=0, box.padding = 0) +
    coord_sf(crs = 4326, default_crs = 4326, xlim = c(-68,-54), ylim = c(40,48), clip = "on", expand = FALSE) +
    #scale_x_continuous(limits=c(-68, -54)) +
    #scale_y_continuous(limits=c(40, 48)) + 
    theme_bw() +
    xlab(NULL) + ylab(NULL)
  print(p)
  dev.off()
}

if(!language=="english") {
  png(paste0(direct_out, year,"/Updates/", bank, 
             "/Figures_and_tables/Offshore_banks_french.png",sep=""),width=10,height=7,res=920,units="in")
  p <- p + geom_sf_text(data = nonGB[!(nonGB$SFA%in% c("SFA11", "SFA26B")),], 
                        aes(label = fr), 
                        fun.geometry = sf::st_centroid) +
    geom_sf_text(data = nonGB[nonGB$SFA=="SFA11",], 
                 aes(label = fr), 
                 fun.geometry = sf::st_centroid, 
                 nudge_x=-0.5, nudge_y=-0.5) +
    geom_sf_text(data = nonGB[nonGB$SFA=="SFA26B",], 
                 aes(label = fr), 
                 fun.geometry = sf::st_centroid, 
                 nudge_y=0.5) +
    geom_text_repel(data = GBalab, 
                    aes(x=as.data.frame(st_coordinates(st_centroid(GBalab)))$X,
                        y=as.data.frame(st_coordinates(st_centroid(GBalab)))$Y,
                        label = fr), 
                    nudge_x=-1, nudge_y=-0.5, direction = "x", min.segment.length=0, box.padding = 0) +
    geom_text_repel(data = GBblab, 
                    aes(x=as.data.frame(st_coordinates(st_centroid(GBblab)))$X,
                        y=as.data.frame(st_coordinates(st_centroid(GBblab)))$Y,
                        label = fr), 
                    nudge_x=1.25, nudge_y=-0.5, direction = "x", min.segment.length=0, box.padding = 0) +
    coord_sf(crs = 4326, default_crs = 4326, xlim = c(-68,-54), ylim = c(40,48), clip = "on", expand = FALSE) +
    #scale_x_continuous(limits=c(-68, -54)) +
    #scale_y_continuous(limits=c(40, 48)) + 
    theme_bw() +
    xlab(NULL) + ylab(NULL)
  print(p)
  dev.off()
}

showtext_auto(TRUE)

### Spatial plots

#####################################################################################################
######### SPATIAL BIOMASS FIGURES ###################################################################
#####################################################################################################
atow<-800*2.4384/10^6
num.knots<-20
R.size <- "75"
FR.size <- "90"
NY<-length(years)
c_sys <- 32619
catchy<-mod.fit$obj$env$data$C*mod.fit$obj$env$data$area

matYear<-c(rep(c(years,(max(years)+1)),each=num.knots))
matYear1<-c(rep(years,each=num.knots))
knots<-rep(1:num.knots,NY+1)
knots1<-rep(1:num.knots,NY)

grid.gis <- pred.grid$grid
grid.gis$geometry <- grid.gis$geometry*1000
st_crs(grid.gis) <- 32619
# Now simplify the grid for the spatial plots...
knot.gis <- aggregate(grid.gis, list(grid.gis$knotID), function(x) x[1])

# Get the spatial data output
B<-data.frame(B=as.vector(mod.fit$report$B),Year=matYear,knotID=knots)
# Dave's code does nit have multiple=all - may not need
B.dat.plot<-left_join(knot.gis,B,by=c("knotID"),multiple="all")
# Recruits
R<-data.frame(R=as.vector(mod.fit$report$R),Year=matYear1, knotID=knots1)
R.dat.plot<-left_join(knot.gis,R,by=c("knotID"),multiple="all")
#Natural Mortality
m<-data.frame(m=as.vector(mod.fit$report$m),Year=matYear,knotID=knots)
m.dat.plot<-left_join(knot.gis,m,by=c("knotID"),multiple="all")
#Natural Mortality (prop)
mu<-data.frame(mu=as.vector(1-exp(-mod.fit$report$m)),Year=matYear,knotID=knots)
mu.dat.plot<-left_join(knot.gis,mu,by=c("knotID"),multiple="all")
#Spatial q's
qI<-data.frame(qI=as.vector(mod.fit$report$qI),knotID=unique(knots))
q.dat.plot<-left_join(knot.gis,qI,by=c("knotID"),multiple="all")

# Exploitation
# The catch data
# So catches from June 2021-May 2022 are called 2021 and removed from the 2021 survey biomass (this is different indexing from how we used to handle this for offshore)
catchy <- cbind(catchy,0)
F.dat<-data.frame(B=as.vector(mod.fit$report$areaB[,-ncol(mod.fit$report$areaB)]/1000),
                  C = c(as.vector(as.matrix(catchy[,-c((ncol(catchy)-1),ncol(catchy))])),rep(NA,num.knots)), Year=matYear1, knotID=knots1)
F.dat <- F.dat %>% dplyr::mutate(exploit = C/(B+C)) # Sticking with how offshore does this (C/(B+C)) C/B or some variant may be more realistic
F.dat.plot<-left_join(knot.gis,F.dat,by=c("knotID"),multiple="all")

# != max(years) would be 1994-2023 for an assessment year of 2025? (Is this correct)
F.dat.plot <- F.dat.plot %>% dplyr::filter(Year != max(years) )

# Spatial figures ##################################################################
########### B - Biomass ############################################################
theme_set(theme_few(base_size = 11))
# Set up pretty breaks for the figure
if(language == 'english') leg.label <- "Biomass\nDensity \n(kg\U2022km\U02C9\U00B2)"
if(language != 'english') leg.label <- "Densité de \nla biomass \n(kg\U2022km\U02C9\U00B2)"
b.brk <- pretty(log(B.dat.plot$B))
b.lab <- signif(exp(b.brk),digits=2)
# spatial.B.plot<- ggplot() + geom_sf(data=B.dat.plot%>% dplyr::filter(!Year %in% c(2023)),aes(fill=log(B)),color='grey')+
spatial.B.plot<- ggplot() + geom_sf(data=B.dat.plot%>% dplyr::filter(Year %in% c((yr-3):yr)),aes(fill=log(B)),color='grey')+
  facet_wrap(~Year)+ 
 scale_x_continuous(breaks = c(-66.3, -66.1, -65.9) )+#, labels = c("60\u00b018'W","60\u00b06'W","59\u00b054'W")) +
 scale_y_continuous(breaks = c(42.55, 42.7, 42.85) )+#,labels = c("42\u00b036'N","42\u00b045'N","42\u00b054'N")) +
  scale_fill_viridis_c(breaks = b.brk, labels=b.lab, name=leg.label,option = "A",begin=0.2) 
  #+# superscript minus U207B has been phased out, use U02C9 instead
  #theme(axis.text.x=element_text(angle=45,hjust=1))
# Save for higher quality image
showtext_auto(FALSE)
if(language != 'english') spatial.B.plot <- spatial.B.plot + 
                                            scale_x_continuous(name = "", breaks = c(-66.3, -66.1, -65.9),labels = french.west)

if(language == 'english') ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/spatial.Biomass.FullyRecruited.png"), 
                                 spatial.B.plot, dpi = 600, width = 6.5, height = 3.5)

if(language != 'english') ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/spatial.Biomass.FullyRecruited_french.png"), 
                                 spatial.B.plot, dpi = 600, width = 7, height = 4)


showtext_auto(TRUE)

########## R - Recruit ################################
r.brk <- pretty(log(R.dat.plot$R))
# pretty() creates 6 breaks that didn’t align well with the data distribution
#hist(log(R.dat.plot$R), breaks = 30)
# Need to manually add more breaks, because data is clustered near the mean
r.brk <- seq(min(log(R.dat.plot$R)), max(log(R.dat.plot$R)), length.out = 12)
# By increasing the number of breaks (length.out = 12), you ensure that more ticks/breaks fall within the range where most data is clustered, making them appear in the color legend.
r.lab <- signif(exp(r.brk),digits=2)

if(language == 'english') leg.label <- "Recruit \nDensity \n(kg\U2022km\U02C9\U00B2)"
if(language != 'english') leg.label <- "Densité des \n recrues \n(kg\U2022km\U02C9\U00B2)"

# spatial.R.plot<-  ggplot() + geom_sf(data=R.dat.plot,aes(fill=log(R)),col='grey')+
theme_set(theme_few(base_size = 11))
spatial.R.plot<-  ggplot() + geom_sf(data=R.dat.plot %>% dplyr::filter(Year %in% c((yr-3):yr)),aes(fill=log(R)),col='grey')+
  facet_wrap(~Year)+ 
  scale_x_continuous(breaks = c(-66.3, -66.1, -65.9) )+#, labels = c("60\u00b018'W","60\u00b06'W","59\u00b054'W")) +
  scale_y_continuous(breaks = c(42.55, 42.7, 42.85) )+#,labels = c("42\u00b036'N","42\u00b045'N","42\u00b054'N")) +
  scale_fill_viridis_c(breaks=r.brk, labels=r.lab, name=leg.label, end=0.8) #+ # superscript minus U207B has been phased out, use U02C9 instead 
  #theme(axis.text.x=element_text(angle=45,hjust=1))

if(language != 'english') spatial.R.plot <- spatial.R.plot + 
                                            scale_x_continuous(name = "", breaks = c(-66.3, -66.1, -65.9),labels = french.west)


# Save for higher quality image
showtext_auto(FALSE)
if(language == 'english') ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/spatial.Biomass.Recruit.png"), 
                                 spatial.R.plot, dpi = 600, width = 6.5, height = 3.5)

if(language != 'english') ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/spatial.Biomass.Recruit_french.png"), 
                                 spatial.R.plot, dpi = 600, width = 6.5, height = 3.5)
showtext_auto(TRUE)

######### M - Natural Mortality ######################################
# m.brk <- log(c(0.003,0.007,0.02,0.05,0.15,0.4,1))
# #m.brk <- pretty(log(m.dat.plot$m))
# m.lab <- signif(exp(m.brk),digits=2)
# 
# # spatial.m.plot <-  ggplot() + geom_sf(data=m.dat.plot %>% dplyr::filter(!Year %in% c(2023)),aes(fill=log(m)),color='grey')+
# spatial.m.plot <-  ggplot() + geom_sf(data=m.dat.plot %>% dplyr::filter(Year %in% c((yr-3):yr)),aes(fill=log(m)),color='grey')+
#   scale_x_continuous(breaks = c(-60.3,-60.1,-59.9) )+#, labels = c("60\u00b018'W","60\u00b06'W","59\u00b054'W")) +
#   scale_y_continuous(breaks = c(42.6, 42.75, 42.9) )+#,labels = c("42\u00b036'N","42\u00b045'N","42\u00b054'N")) +
#   facet_wrap(~Year)+ 
#   scale_fill_viridis_c(breaks = m.brk, labels = m.lab,name="Natural \nMortality \n(Inst)",option = "B",direction =1,begin = 0.2,end=1) 
# 
# # Save for higher quality image
# showtext_auto(FALSE)
# ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/spatial.NaturalMortality.png"), spatial.m.plot, dpi = 600, width = 6.5, height = 3.5)
# showtext_auto(TRUE)
######################################################################
######### M - Natural Mortality (proportional) #######################
mu.brk <- pretty(log(mu.dat.plot$mu))    
#mu.brk <- log(c(0.003,0.007,0.02,0.05,0.15,0.4,1))
#m.brk <- pretty(log(m.dat.plot$m))
mu.lab <- signif(exp(mu.brk),digits=2)

if(language == 'english') leg.label <- "Natural \nmortality \n(Proportional)"
if(language != 'english') leg.label <- "Mortalité \nnaturelle \n(proportionnelle)"

# spatial.m.plot <-  ggplot() + geom_sf(data=m.dat.plot %>% dplyr::filter(!Year %in% c(2023)),aes(fill=log(m)),color='grey')+
spatial.mu.plot <-  ggplot() + geom_sf(data=mu.dat.plot %>% dplyr::filter(Year %in% c((yr-3):yr)),aes(fill=log(mu)),color='grey')+
  scale_x_continuous(breaks = c(-66.3, -66.1, -65.9) )+#, labels = c("60\u00b018'W","60\u00b06'W","59\u00b054'W")) +
  scale_y_continuous(breaks = c(42.55, 42.7, 42.85) )+#,labels = c("42\u00b036'N","42\u00b045'N","42\u00b054'N")) +
  facet_wrap(~Year)+ 
  scale_fill_viridis_c(breaks = mu.brk, labels = mu.lab,name=leg.label,option = "B",direction =1,begin = 0.2,end=1) 
# Save for higher quality image
showtext_auto(FALSE)

if(language != 'english') spatial.mu.plot <- spatial.mu.plot + 
  scale_x_continuous(name = "", breaks = c(-66.3, -66.1, -65.9),labels = french.west)



if(language == 'english')  ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                  "/Figures_and_tables/spatial.NaturalMortality_prop.png"), 
                                  spatial.mu.plot, dpi = 600, width = 6.5, height = 3.5)

if(language != 'english')  ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                  "/Figures_and_tables/spatial.NaturalMortality_prop_french.png"), 
                                  spatial.mu.plot, dpi = 600, width = 6.5, height = 3.5)


showtext_auto(TRUE)

####### q - catchability ############################################
spatial.q.plot <- ggplot() + geom_sf(data=q.dat.plot,aes(fill=qI),col=NA)+
  scale_x_continuous(breaks = c(-66.3, -66.1, -65.9) )+#, labels = c("60\u00b018'W","60\u00b06'W","59\u00b054'W")) +
  scale_y_continuous(breaks = c(42.55, 42.7, 42.85) )+#,labels = c("42\u00b036'N","42\u00b045'N","42\u00b054'N")) +
  scale_fill_viridis_c(name="Predicted catchability (qI)",option = "C",begin = 0.2,end =0.8) 

# Save for higher quality image
showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/spatial.Catchability.png"), spatial.q.plot, dpi = 600, width = 6.5, height = 3.5)
showtext_auto(TRUE)

######## Spatial exploitation rates ###################################
F.dat.plot$exp.na <- NA
F.dat.plot$exp.na[F.dat.plot$exploit != 0] <- F.dat.plot$exploit[F.dat.plot$exploit != 0]

if(language == 'english') leg.label <- "Exploitation Rate\n(Proportional)"
if(language != 'english') leg.label <- "Taux d’exploitation \n(proportionnel)"


#e.brk <- log(c(0.0015,0.01,0.08,0.4))
e.brk <- pretty(log(F.dat.plot$exp.na))
e.lab <- signif(exp(e.brk),digits=2)

# spatial.exploit.plot<- ggplot() + geom_sf(data=F.dat.plot,aes(fill=log(exp.na)),color='grey') +
#spatial.exploit.plot<- ggplot() + geom_sf(data=F.dat.plot %>% dplyr::filter(Year %in% c((yr-4):(yr-1))),aes(fill=log(exp.na)),color='grey') +
spatial.exploit.plot<- ggplot() + geom_sf(data=F.dat.plot %>% dplyr::filter(Year %in% c((yr-4):yr)),aes(fill=log(exp.na)),color='grey') +
  scale_x_continuous(breaks = c(-66.3, -66.1, -65.9) )+#, labels = c("60\u00b018'W","60\u00b06'W","59\u00b054'W")) +
  scale_y_continuous(breaks = c(42.55, 42.7, 42.85) )+#,labels = c("42\u00b036'N","42\u00b045'N","42\u00b054'N")) +
  facet_wrap(~Year) + 
  scale_fill_viridis_c(breaks = e.brk,labels = e.lab,name=leg.label,option = "D") 

if(language != 'english') spatial.exploit.plot <- spatial.exploit.plot + 
                                            scale_x_continuous(name = "", breaks =  c(-66.3, -66.1, -65.9),labels = french.west)


# Save for higher quality image
showtext_auto(FALSE)
if(language == 'english') ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/spatial.Exploit.png"), 
                                 spatial.exploit.plot, dpi = 600, width = 6.5, height = 3.5)

if(language != 'english') ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/spatial.Exploit_french.png"), 
                                 spatial.exploit.plot, dpi = 600, width = 6.5, height = 3.5)
showtext_auto(TRUE)

#Getting totals
theme_set(theme_few(base_size = 18))

pred.proc <- get_processes(mod.fit)

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
# Annual explotation
catch.annual <- data.frame(totC = colSums(catchy[,-ncol(catchy)]), Year = years)
pred.proc$log_processes <- pred.proc$log_processes %>% dplyr::filter(year < (assess.year))

ann.exploit <- data.frame(year = years,B = exp(pred.proc$log_processes$log_B), Catch = c(colSums(catchy[,-c((ncol(catchy)-1),ncol(catchy))]),NA),
                          B.LCI = pred.proc$log_processes$totB.LCI, B.UCI = pred.proc$log_processes$totB.UCI)
ann.exploit$exploit <- c(ann.exploit$Catch[1:(nrow(ann.exploit)-1)]/(ann.exploit$B[2:nrow(ann.exploit)]+ann.exploit$Catch[1:(nrow(ann.exploit)-1)]),NA)
ann.exploit$exploit.UCI <- c(ann.exploit$Catch[1:(nrow(ann.exploit)-1)]/(ann.exploit$B.UCI[2:nrow(ann.exploit)]+ann.exploit$Catch[1:(nrow(ann.exploit)-1)]),NA)
ann.exploit$exploit.LCI <- c(ann.exploit$Catch[1:(nrow(ann.exploit)-1)]/(ann.exploit$B.LCI[2:nrow(ann.exploit)]+ann.exploit$Catch[1:(nrow(ann.exploit)-1)]),NA)
ann.exploit$FM <- 1-exp(-ann.exploit$exploit)
ann.exploit$FM.LCI <- 1-exp(-ann.exploit$exploit.LCI)
ann.exploit$FM.UCI <- 1-exp(-ann.exploit$exploit.UCI)

#First the figures comparing model to observed
# B.index.plot<-ggplot()+
#   geom_line(aes(x=years,y=exp(pred.proc$log_processes$log_B)*median(mod.fit$report$qI)),col="black")+
#   geom_ribbon(aes(x=years.ribbon,ymax=pred.proc$log_processes$totB.UCI*median(mod.fit$report$qI),
#                   ymin=pred.proc$log_processes$totB.LCI*median(mod.fit$report$qI)),
#               fill=NA,col="black",lty="dashed")+
#   geom_point(aes(x=years,y=mod.dat$I),col="red")+
#   geom_errorbar(aes(x=years,ymax=mod.dat$I+1.96*(mod.dat$I*mod.dat$I.cv),
#                     ymin=mod.dat$I-1.96*(mod.dat$I*mod.dat$I.cv)),
#                 col="red",width=0)+
#   ylab("Fully-Recruited Survey \nBiomass Index (tonnes)")+
#   theme(axis.title.x = element_blank())
# 
# R.index.plot<-ggplot()+
#   geom_line(aes(x=years,y=exp(pred.proc$log_processes$log_R)*mod.fit$report$qR),col="black")+
#   geom_ribbon(aes(x=years.ribbon,ymax=pred.proc$log_processes$totR.UCI*mod.fit$report$qR,
#                   ymin=pred.proc$log_processes$totR.LCI*mod.fit$report$qR),
#               fill=NA,col="black",lty="dashed")+
#   geom_point(aes(x=years,y=mod.dat$IR),col="red")+
#   geom_errorbar(aes(x=years,ymax=mod.dat$IR+1.96*(mod.dat$IR*mod.dat$IR.cv),
#                     ymin=mod.dat$IR-1.96*(mod.dat$IR*mod.dat$IR.cv)),
#                 col="red",width=0)+
#   ylab("Recruited Survey Biomass Index \n(tonnes)")+
#   theme(axis.title.x = element_blank())
# 
# # Add na.rm = TRUE for missing years
# #temp_cpue<-aggregate(kg.hm~survey.year,data=fish.dat,FUN=mean, na.rm = TRUE)
# temp_cpue<-aggregate(kg.hm~year,data=fish.dat,FUN=mean, na.rm = TRUE)
# #temp_cpue<-temp_cpue[which(temp_cpue$survey.year>1993),]
# temp_cpue<-temp_cpue[which(temp_cpue$year>1993),]
# 
# #Making the assumption here that a tow takes ~2.5h to scale it approximately to the same as CPUE
# cpue.index.plot<-ggplot()+
#   geom_line(aes(x=years,y=c(exp(pred.proc$log_processes$log_B)*atow*2.5)),col="black")+
#   geom_point(aes(x=years,y=temp_cpue$kg.hm),col="red")+
#   ylab("Commercial CPUE \n(kg/hm)")+
#   theme(axis.title.x = element_blank())

#NOTE ON BIOMASS PROJECTION UNCERTAINTY (MAKE BETTER)
#Used the projection's mean as that will be the approach chosen by the framework, unlike what hte model directly provides. Basically a bootstrapped mean
#However, that bootstrap does not include any uncertainty around the biomass
#Therefore, for the error bar, I use the SE provided by the model for its projection to calculate a CI
#This is not exactly correct, but will give you a closer CI than what the bootstrap provides using the quantile method because this will guaranteed to be too small
#For the future, when doing projections and decision tables in the delay difference, simply use rlnorm on B_t-1 (total biomass for last year with data) with the SE from the model fit (se_log_totB).

### Panel plots

# PANEL 1 FIGURES #############################################################################################

# Year to year, you may want to change the axis label intervals for specific plots in scale_x/y_cont (some have been manually set below using breaks = seq())
theme_set(theme_few(base_size = 18)) # DK changed this to 18 from 14
###### (A) Tac Landings Plots ######################################################################################
# [5:nrow(bbn.fish.dat)] for b=geom_bar and years[5:length(years)] for geom_line + coord_cartesian used to only show data from 1998 to current yr (when TAC for BBn/Bs split)
# Change language as necessary
if(language == "english") y.lab <- "Landings (meat, kt)"
if(language != "english") y.lab <- "Débarquements (chair, kt)"

tac_land_plot<-ggplot()+
  geom_bar(aes(x=bbn.fish.dat$year[5:nrow(bbn.fish.dat)],y=bbn.fish.dat$C[5:nrow(bbn.fish.dat)]/1000),
           fill="grey50",stat="identity",alpha = 0.6, width = 0.75) +
  geom_line(data=TACs[TACs$bank==bank & TACs$year %in% years[5:length(years)],],
            aes(x=as.numeric(year),y=as.numeric(TAC/1000),col="TAC"),
            linewidth=0.5, linetype = "solid")+
  scale_color_manual(name="",values="black")+
  ylab(y.lab)+
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,round(max(bbn.fish.dat$C/1000),0)+0.15))+ #ylim could be automated in the future - manually setting max now
  # There's no way to add minor ticks, so we set the 'major ticks' at 5yr intervals. Normally this would also add labels at 5yr int's. So we have to manually set the labels to be @ 10yr int's.
  scale_x_continuous(breaks=seq(1995,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1995, "", 2005, "", 2015, "", 2025), # , 2025), # add this back in 2026
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  annotate("text", x = yr, y = Inf, label = "(A)", size = 5, vjust = 1.5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 2)),
        #axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = c(0.12, 1), legend.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"), 
        plot.margin = margin(10, 5, 5, 1, "points"))
tac_land_plot
showtext_auto(FALSE)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_TAClandings_panelA.png"), tac_land_plot, dpi = 600, width = 6.5, height =5)
if(language != "english")
ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_TAClandings_panelA_french.png"), tac_land_plot, dpi = 600, width = 6.5, height =5)

showtext_auto(TRUE)
########### (B) F-R Biomass time series ##################################################################
# Get CI's for projection boxplot
pB <- projection$Biomass$B2[projection$Biomass$scenario == TACs$TAC[TACs$year==yr+1 & TACs$bank==bank]]
    #pB.box<-data.frame(pB = pB[pB>quantile(pB,alpha/2)&pB<quantile(pB,1-(alpha/2))])
pB.box <- as.data.frame(t(quantile(pB, probs = c(0.025, 0.25, 0.50, 0.75, 0.975))))
names(pB.box) <- c("ymin", "lower", "middle", "upper", "ymax")
pB.box$year <- year


if(language == "english") 
{
  y.lab <- "Fully-recruited biomass (kt)"
  lrp.lab <- "LRP"
}
if(language != "english") 
{
  y.lab <- "Biomasse des pétoncles \npleinement recrutés (kt)"
  lrp.lab <- "PRL"
}


########### (B) F-R Biomass time series ###################################################################
bm.ts.plot <- ggplot() +
  geom_hline(aes(yintercept=LRP/1000, col=lrp.lab), lty="longdash", alpha=0.7)+
  #guides(col = guide_legend(reverse = TRUE)) + # for if you want to reorder the legend
  geom_line(aes(pred.proc$log_processes$year,exp(pred.proc$log_processes$log_B)/1000), col="black", linewidth = 0.4) + 
  scale_color_manual(name="",values=c("firebrick"))+
  geom_point(aes(pred.proc$log_processes$year,exp(pred.proc$log_processes$log_B)/1000), size=1) + 
  #geom_boxplot(data = pB.box,aes(2025,y=pB/1000),outlier.shape=NA) +
  geom_boxplot(data = pB.box[1:5]/1000, stat = "identity",
    aes(x = year,ymin = ymin,lower = lower,middle = middle,upper = upper,ymax = ymax),
    outlier.shape=NA) +
  geom_point(aes(year,projection$table[which(projection$table[,1]==TACs$TAC[TACs$year==yr+1 & TACs$bank==bank]),3]/1000), size=1)+
  geom_ribbon(aes(ymin=pred.proc$log_processes$totB.LCI/1000,
                  ymax=pred.proc$log_processes$totB.UCI/1000,
                  x=years.ribbon),
              alpha=0.2,fill='grey20') +
  xlab("") + ylab(y.lab) +
  annotate("text", x = yr, y = Inf, label = "(B)", size = 5, vjust = 1.5) +
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,max(pred.proc$log_processes$totB.UCI/1000)+3))+
  scale_x_continuous(breaks=seq(1995,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1995, "", 2005, "", 2015, "", 2025), # , 2025), # add this back in 2026
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(breaks = seq(0, 28, 5), expand = c(0, 0), limits = c(0, NA)) +
  theme(legend.position = c(0.12,1), legend.key.width = unit(0.8, "cm"),
        legend.background = element_blank(), legend.key = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 2)),
        plot.margin = margin(10, 10, 5, 1, "points"))
bm.ts.plot
showtext_auto(FALSE)
if(language == "english")  ggsave(filename=paste0(direct_out, year,"/Updates/", bank,
                                                  "/Figures_and_tables/FSAR_FRbiomass_panelB.png"), 
                                  bm.ts.plot, dpi = 600, width = 6.5, height =5)


if(language != "english")  ggsave(filename=paste0(direct_out, year,"/Updates/", bank,
                                                  "/Figures_and_tables/FSAR_FRbiomass_panelB_french.png"), 
                                  bm.ts.plot, dpi = 600, width = 6.5, height =5)

showtext_auto(TRUE)
#For long-term medians, not including last year

###### (D) Recruit Biomass time series #########################################################################

if(language == "english") 
{
  y.lab <- "Recruit biomass (kt)"
  ltm.lab <- "LTM"
}
if(language != "english") 
{
  y.lab <- "Biomasse des recrues (t)"
  ltm.lab <- "MLT"
}

rec.ts.plot <- ggplot(pred.proc$log_processes) + 
  geom_hline(aes(yintercept=median(exp(log_R[-length(log_R)]))/1000,col=ltm.lab),lty="dashed", alpha=0.9)+
  geom_line(aes(year,exp(log_R)/1000), col="black", linewidth = 0.4) + 
  geom_point(aes(year,exp(log_R)/1000), col="black", size=1) + 
  geom_ribbon(aes(ymin=totR.LCI/1000,
                  ymax=totR.UCI/1000,
                  x=years.ribbon),
              alpha=0.2,fill='grey20',lty="dashed") + 
  scale_color_manual(name="",values=c("grey20"))+
  guides(col = guide_legend(reverse = TRUE)) + # for if you want to reorder the legend
  xlab("") + ylab(y.lab)  + 
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(-0.2,max(pred.proc$log_processes$totR.UCI/1000)+1.1))+
  scale_x_continuous(breaks=seq(1995,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1995, "", 2005, "", 2015, "", 2025), # , 2025), # add this back in 2026
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(breaks = seq(0, 11, 2), expand = c(0, 0), limits = c(0, NA)) +
  annotate("text", x = yr, y = Inf, label = "(D)", size = 5, vjust = 1.5) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.12,1), legend.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.text.x = element_text(margin = margin(t = 2)),
        plot.margin = margin(5, 10, 5, 1, "points"))
rec.ts.plot
showtext_auto(FALSE)
if(language == "english")  ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                  "/Figures_and_tables/FSAR_Rbiomass_panelD.png"), 
                                  rec.ts.plot, dpi = 600, width = 6.5, height =5)

if(language != "english")  ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                  "/Figures_and_tables/FSAR_Rbiomass_panelD_french.png"), 
                                  rec.ts.plot, dpi = 600, width = 6.5, height =5)
showtext_auto(TRUE)

## (C) Estimated proportional exploitation rate / natural mortality plot ######################################

if(language == "english") 
{
  y.lab <- "Exploitation (proportional rate)"
  ltm.lab <- "LTM"
}
if(language != "english") 
{
  y.lab <- "Taux d'exploitation \n(proportionnel)"
  ltm.lab <- "MLT"
}


exploit.plot <- ggplot() + 
  geom_ribbon(data=ann.exploit[-nrow(ann.exploit),],
              aes(ymin=exploit.LCI,ymax=exploit.UCI,x=years.ribbon[-1]-1),alpha=0.2,fill="grey10") +
  geom_hline(data=ann.exploit, aes(yintercept=median(exploit, na.rm=T),col=ltm.lab),lty="dashed", alpha=0.7) +
  geom_line(data=ann.exploit,aes(x=year,y=exploit), col="black", linewidth = 0.4) + 
  geom_point(data=ann.exploit,aes(x=year,y=exploit),col="black", size=1) +
  scale_color_manual(name="",values=c("grey20"))+
  xlab("") + ylab(y.lab) + 
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,max(ann.exploit$exploit.LCI, na.rm=T)+0.03)) +
  scale_x_continuous(breaks=seq(1995,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1995, "", 2005, "", 2015, "", 2025), # , 2025), # add this back in 2026
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  annotate("text", x = yr, y = Inf, label = "(C)", size = 5, vjust = 1.5) +
  theme(legend.position = c(0.12,1), legend.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 2)),
        plot.margin = margin(5, 5, 5, 1, "points"))
exploit.plot
showtext_auto(FALSE)

if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_Exploitation_panelC.png"), 
                                 exploit.plot, dpi = 600, width = 6.5, height =5)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_Exploitation_panelC_french.png"), 
                                 exploit.plot, dpi = 600, width = 6.5, height =5)


showtext_auto(TRUE)

### Use this if plotting exploitation and natural mortality together ############################
#e.m.plot <- ggplot() + 
#  geom_ribbon(data=ann.exploit,aes(ymin=exploit.LCI,ymax=exploit.UCI,x=years.ribbon),alpha=0.2,fill="grey10") +
#  geom_line(data=ann.exploit,aes(x=year,y=exploit,col="F"), linewidth = 0.4) + 
#  geom_point(data=ann.exploit,aes(x=year,y=exploit,col="F"), size=1) +
#  geom_ribbon(data=pred.proc$log_processes,aes(ymin=1-exp(-m.LCI),ymax=1-exp(-m.UCI),x=years.ribbon),alpha=0.12,fill="blue") +
#  geom_line(data=pred.proc$log_processes,aes(year,1-exp(-exp(log_m)),col="M"), lty="dashed", linewidth = 0.4) + 
#  geom_point(data=pred.proc$log_processes,aes(year,1-exp(-exp(log_m)),col="M"),shape=17, size=1.5) + 
#  scale_color_manual(name="",values=c("black","blue"))+
#  xlab("") + ylab("Proportional rate") + 
#  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,0.65))+
#  scale_x_continuous(breaks=seq(1995,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [, ""]. In 2035 add: [,2035].
#                     labels = c(1995, "", 2005, "", 2015, "", 2025), # , 2025), # add this back in 2026
#                     expand = c(0, 0), limits = c(0, NA)) + 
#  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
#  annotate("text", x = yr, y = Inf, label = "(C)", size = 5, vjust = 1.5) +
#  theme(legend.position = c(0.1,0.95), legend.background = element_blank(),
#        panel.border = element_rect(linewidth = 1, fill = NA),
#        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
#        axis.title.x = element_blank(),
#        axis.text.x = element_text(margin = margin(t = 2)),
#        plot.margin = margin(5, 0, 5, 0, "points"))


####### SAVE COWPLOT TO PNG TO MAINTAIN HIGH IMAGE RESOLUTION
theme_set(theme_few(base_size = 14)) # DK changed this to 14 from 18

panel_1 <- cowplot::plot_grid(tac_land_plot, bm.ts.plot, exploit.plot, rec.ts.plot, align="v",ncol=2,axis="lr")
showtext_auto(FALSE)

if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_panel1_tacland_biom_recruitb_exploit.png"), panel_1, dpi = 600, width = 9, height = 9)

if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_panel1_tacland_biom_recruitb_exploit_wider.png"), panel_1, dpi = 600, width = 9, height = 7)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_panel1_tacland_biom_recruitb_exploit_french.png"), panel_1, dpi = 600, width = 9, height = 9)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_panel1_tacland_biom_recruitb_exploit_wider_french.png"), panel_1, dpi = 600, width = 9, height = 7)
showtext_auto(TRUE)

# UNUSED....

###################### PANEL 2 - Natural Mortality #########################################################
## Fully-recruited natural mortality plot
#max(pred.proc$log_processes$m.UCI, na.rm=T)

if(language == "english") 
{
  y.lab <- "Natural mortality (proportional rate)"
  ltm.lab <- "LTM"
}
if(language != "english") 
{
  y.lab <-"Mortalité naturelle (taux proportionnel)"
  ltm.lab <- "MLT"
}

m.dat <- data.frame(
  year = pred.proc$log_processes$year,
  lower.ci = 1-exp(-pred.proc$log_processes$m.LCI),
  upper.ci = 1-exp(-pred.proc$log_processes$m.UCI),
  median = 1-exp(-exp(pred.proc$log_processes$log_m))
)
m.ltm <- round(median(1-exp(-exp(pred.proc$log_processes$log_m[-length(pred.proc$log_processes$log_m)]))),2)
FR.nat.mort.plot <- ggplot() +
  geom_ribbon(data = m.dat, aes(x = years.ribbon, ymin = lower.ci, ymax = upper.ci), alpha = 0.2, fill = "grey10") +
  # long-term median (1991–`r yr-1`)
  geom_hline(data=pred.proc$log_processes, aes(yintercept=m.ltm,col=ltm.lab),lty="dashed", alpha=0.9) +
  geom_line(data=pred.proc$log_processes,aes(year,1-exp(-exp(log_m))),col="black", linewidth = 0.4) +
  geom_point(data=pred.proc$log_processes,aes(year,1-exp(-exp(log_m))),col="black", size=1) +
  xlab("") + ylab(y.lab) +
  scale_color_manual(name="",values=c("grey20"))+
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,0.65))+
  scale_x_continuous(breaks=seq(1995,max(years)+2,5),
  # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1995, "", 2005, "", 2015, "", 2025), # , 2025), # add this back in 2026
                     expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  #annotate("text", x = yr, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
  theme(legend.position = c(0.12,1), legend.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 2)),
        plot.margin = margin(5, 10, 5, 2, "points"))
FR.nat.mort.plot
showtext_auto(FALSE)

if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_natmort.png"), 
                                 FR.nat.mort.plot, dpi = 600, width = 6.5, height =5)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel2_natmort.png"), 
                                 FR.nat.mort.plot, dpi = 600, width = 4.5, height = 3.5)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_natmort_french.png"), 
                                 FR.nat.mort.plot, dpi = 600, width = 6.5, height =5)
if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel2_natmort_french.png"), 
                                 FR.nat.mort.plot, dpi = 600, width = 4.5, height = 3.5)



showtext_auto(TRUE)
# 
# ###################### PANEL 3 - Condition #########################################################
# 
# cf.max <- max(mod.dat$CF, na.rm=T)
# cf.ltm <- round(median(mod.dat$CF[-length(mod.dat$CF)]/cf.max, na.rm=T),2)
# cf.plot <- ggplot() + 
#   geom_hline(aes(yintercept=cf.ltm,col="LTM"),lty="dashed", alpha=0.9) +
#   geom_line(aes(x=mod.dat$year,y=mod.dat$CF/cf.max),col="black", linewidth = 0.4) + 
#   geom_point(aes(x=mod.dat$year,y=mod.dat$CF/cf.max),col="black", size=1) + 
#   xlab("") + ylab("Condition") + 
#   scale_color_manual(name="",values=c("grey20"))+
#   coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,1.1))+
#   scale_x_continuous(breaks=seq(1995,max(years)+2,5),
#   # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [, ""]. In 2035 add: [,2035].
#                      labels = c(1995, "", 2005, "", 2015, "", 2025), # , 2025), # add this back in 2026
#                      expand = c(0, 0), limits = c(0, NA)) + 
#   scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
#   annotate("text", x = yr, y = Inf, label = "(B)", size = 5, vjust = 1.5) +
#   theme(legend.position = c(0.12,1), legend.background = element_blank(),
#         panel.border = element_rect(linewidth = 1, fill = NA),
#         axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(margin = margin(t = 2)),
#         plot.margin = margin(5, 10, 5, 2, "points"))
# cf.plot
# 
# panel_2 <- cowplot::plot_grid(FR.nat.mort.plot, cf.plot, align="h",ncol=2,axis="l")
# showtext_auto(FALSE)
# # panel with nat mort and condition for FSAR
# ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_panel2_FmortCondition.png"), panel_2, dpi = 600, width = 9, height = 3.5)
# # figure with just condition for presentations
# ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_condition.png"), cf.plot, dpi = 600, width = 6.5, height =5)
# # alternative figure with just condition for FSAR doc - include nat mort and condition as seperate figures if doing this
# ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_panel3_condition.png"), cf.plot, dpi = 600, width = 4.5, height = 3.5)
# showtext_auto(TRUE)
