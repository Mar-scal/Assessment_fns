## Georges Bank SFA 27A

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

#github link for model run and figures shown at framework is
#https://github.com/Dave-Keith/Framework/blob/master/Model/SEAM_BBn_Step_1_model.R
#Taking that code to make figures
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
require(ggrepel)
showtext_auto()
# Note: with lots of packages, tools sometimes get masked. If something isn't working the way you expect, try explicitly referencing the package (ex: dplyr::[...]) - this happened a lot with dplyr and may occur with other packages. 

# Set up
#language = 'english'
language = 'french'
bank <- "GBa"
banks <- c("GBa") # pick your banks for TACs/Landings table
fleets <- c("FT", "WF") # pick your fleets for TACs/Landings table
alpha<-0.05
year <- 2026 # current year
yr <- 2025 # fishery year
years<-1986:yr
years.ribbon <- years
years.ribbon[length(years.ribbon)] <- years.ribbon[length(years.ribbon)]+0.2



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

# Sources
direct <- "C:/Users/keyserf/Documents/"
direct_out <- "C:/Users/keyserf/Documents/"
repo <- "C:/Users/keyserf/Documents/Github/"


load(paste0(direct_out,"Data/Model/",year,"/GBa/Results/Model_testing_results.RData"))
load(paste0(direct_out,"Data/Model/",year,"/GBa/Results/Model_results_and_diagnostics.RData"))
D.tab<-read.csv(paste0(direct,"/", year, "/Updates/GBa/Figures_and_tables/Decision_GBa.csv"))
TACs <- read.csv(paste0(direct, "Data/Model/Ref_pts_and_tac.csv"))
# TACs[TACs$bank==bank & TACs$year==year,4]<-20

# Ref pts
URP<-round(URP[[bank]],0) # these were rounded to -1... I don't know why!
LRP<-round(LRP[[bank]],0) # these were rounded to -1... I don't know why!
RR<-0.25
interim.tac <- TACs$TAC[TACs$bank==bank & TACs$year == year]
# load(paste(direct,"Data/Model/", year, "/BBn/Results/Model_testing_results_mixed.RData",sep=""))
# load(paste(direct_off,"Data/Model/", year, "/BBn/Results/Final_model_results.RData",sep=""))
# load(paste(direct,"Data/Model/", year, "/BBn/Results/Model_results_and_diagnostics_mixed.RData",sep=""))
# load(paste(direct_off,"Data/Model/", year, "/BBn/Results/Model_results_and_diagnostics_mixed.RData",sep=""))
# load(paste0(direct,"Framework/SFA_25_26_2024/Model/Results/Sab_SS_model/R_75_FR_90/Sable_SSmodel_results.RData"))

## tac/landing data; 1986 is year model starts, but technically this database goes back to 1984
logs_and_fish(loc="offshore",year = 1986:yr,get.local = T, get.marfis = F, direct=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
gba.fish.dat <- fish.dat |> collapse::fsubset(bank=="GBa") |> collapse::fgroup_by(year) |> collapse::fsummarise(C = sum(pro.repwt,na.rm=T)/1000)

options(scipen=999)

### MAP
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
             "/Figures_and_tables/Offshore_banks.png",sep=""),width=11,height=8,res=920,units="in")
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
             "/Figures_and_tables/Offshore_banks_french.png",sep=""),width=11,height=8,res=920,units="in")
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

### Panel plots

###### ADDED EXTRA THEME CONDITIONS TO ALL PLOTS SO COWPOT::PLOT_GRID DOESN'T MISALIGN FIGURES

if(language == "english") {
  y.lab <- "Landings (meat, kt)"
  tac.lab <- "TAC"
}
if(language != "english") {
  y.lab <- "Débarquements (chair, kt)"
  tac.lab <- "TAC" 
}
###### (A) Tac Landings Plots
theme_set(theme_few(base_size = 22)) # DK changed from 14 to 22 for French figures
# decades <- seq(1990, max(DD.out$GBa$data$year, na.rm = TRUE), by = 10)
tac_land_plot <- ggplot() +
  geom_bar(aes(x = gba.fish.dat$year, y = gba.fish.dat$C/1000),stat = "identity", fill="grey50", alpha = 0.6, width = 0.75) + 
  geom_line(data = TACs[TACs$bank == bank & TACs$year %in% DD.out$GBa$data$year,],
            aes(x = as.numeric(year), y = as.numeric(TAC/1000), col = tac.lab), 
            linewidth = 0.5, linetype = "solid") + 
  scale_color_manual(name = "", values = "black") +
  ylab(y.lab) +
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,round(max(gba.fish.dat$C/1000),0)+2.1))+ #ylim could be automated in the future - manually setting max now
  # There's no way to add minor ticks, so we set the 'major ticks' at 5yr intervals. Normally this would also add labels at 5yr int's. So we have to manually set the labels to be @ 10yr int's by adding blanks - "".
  scale_x_continuous(breaks=seq(1985,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2030. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1985, "", 1995, "", 2005, "", 2015, "", 2025),
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  annotate("text", x = yr, y = Inf, label = "(A)", size = 5, vjust = 1.5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = c(0.12, 1), legend.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        plot.margin = margin(5, 5, 5, 1, "points"))
tac_land_plot

showtext_auto(FALSE)
if(language == 'english') ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_taclandings_panelA.png"), tac_land_plot, dpi = 600, width = 6.5, height =5)

if(language == 'french') ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_taclandings_panelA_french.png"), tac_land_plot, dpi = 600, width = 11, height =8)



showtext_auto(TRUE)
########### (B) F-R Biomass time series
##########################################################################
# Setup for french/english
if(language == "english") 
{
  y.lab <- "Fully-recruited biomass (kt)"
  usr.lab <- "USR"
  lrp.lab <- "LRP"
}
if(language != "english") 
{
  y.lab <- "Biomasse des pétoncles \npleinement recrutés (kt)"
  usr.lab <- "PRS"
  lrp.lab <- "PRL"
}

# Get CI's for projection boxplot
pB<-DD.out$GBa$sims.list$B.p[,which(DD.out$GBa$data$C.p==(interim.tac)+proj.catch$GBa)]
pB.box <- as.data.frame(t(quantile(pB, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))))
names(pB.box) <- c("ymin", "lower", "middle", "upper", "ymax")
pB.box$year <- year
###### had to change saturaion/transparency of colours to have higher contrast visible in b/w
# Removing status labels from fig b/c causes translation issues, FSAR doesn't want words on plots, & not necessary
#geom_text(aes(x=2010,y=45,label="HEALTHY"),col="chartreuse2",cex=6)+
#geom_text(aes(x=2010,y=8,label="CAUTION"),col="goldenrod1",cex=6)+
#geom_text(aes(x=2010,y=2,label="CRITICAL"),col="firebrick2",cex=6)+
B.dat <- data.frame(
  year = years,
  lower.ci = apply(DD.out$GBa$sims.list$B / 1000, 2, quantile, probs = alpha / 2),
  upper.ci = apply(DD.out$GBa$sims.list$B / 1000, 2, quantile, probs = 1 - alpha / 2),
  median = DD.out$GBa$median$B/1000
)
bm.ts.plot <- ggplot() +
  geom_rect(aes(xmin=min(years)-5,xmax=max(years)+5,ymin=URP/1000,ymax=round(max(B.dat$upper.ci),0)+3),fill=rgb(0,1,0,0.2),col=NA)+
  geom_rect(aes(xmin=min(years)-5,xmax=max(years)+5,ymin=LRP/1000,ymax=URP/1000),fill=rgb(1,1,0,0.3),col=NA)+
  geom_rect(aes(xmin=min(years)-5,xmax=max(years)+5,ymin=0,ymax=LRP/1000),fill=rgb(1,0,0,0.4),col=NA)+
  geom_hline(aes(yintercept=LRP/1000, col=lrp.lab), lty="longdash")+
  geom_hline(aes(yintercept=URP/1000, col=usr.lab), lty="longdash")+
  geom_ribbon(data=B.dat, aes(x = years.ribbon, ymin = lower.ci, ymax = upper.ci),
              alpha=0.2,fill="grey20") +
  geom_line(data=B.dat, aes(years,median),color='black', linewidth = 0.4) + 
  geom_point(data=B.dat, aes(years,median), size=1) + 
  geom_boxplot(data = pB.box, stat = "identity",
               aes(x = year,ymin = ymin/1000,lower = lower/1000,middle = middle/1000,upper = upper/1000,ymax = ymax/1000),
               outlier.shape=NA) +
  geom_point(aes(year,pB.box$middle/1000))+
  scale_color_manual(name="",values=c("firebrick","goldenrod1"))+
  guides(col = guide_legend(reverse = TRUE)) +
  xlab("") + ylab(y.lab) +
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,round(max(B.dat$upper.ci),0)+3))+
  scale_x_continuous(breaks=seq(1985,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2035. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1985, "", 1995, "", 2005, "", 2015, "", 2025),
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  annotate("text", x = yr, y = Inf, label = "(B)", size = 5, vjust = 1.5) +
  theme(legend.position = c(0.12,0.95), legend.key.width = unit(0.8, "cm"),
        legend.background = element_blank(), legend.key = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        plot.margin = margin(5, 10, 5, 1, "points"))
bm.ts.plot
showtext_auto(FALSE)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_FRbiomass_panelB.png"),
                                 bm.ts.plot, dpi = 600, width = 6.5, height =5)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_FRbiomass_panelB_French.png"),
                                 bm.ts.plot, dpi = 600, width = 6.5, height =5)

showtext_auto(TRUE)


###### (D) Recruit Biomass time series
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


R.dat <- data.frame(
  year = years,
  lower.ci = apply(DD.out$GBa$sims.list$R / 1000, 2, quantile, probs = alpha[1] / 2),
  upper.ci = apply(DD.out$GBa$sims.list$R / 1000, 2, quantile, probs = 1 - alpha[1] / 2),
  median = DD.out$GBa$median$R/1000
)
rec.ts.plot <- ggplot(data=R.dat) + 
  geom_hline(aes(yintercept=rec.ltm$GBa/1000,col=ltm.lab), lty="dashed", alpha=0.7)+ # long-term median (1994-current yr-1)
  geom_ribbon(aes(x = years.ribbon, ymin = lower.ci, ymax = upper.ci),
              alpha=0.2,fill='grey20') +
  geom_line(aes(year,median),col="black", linewidth = 0.4) + 
  geom_point(aes(year,median), col="black", size=1) + 
  scale_color_manual(name="",values=c("black"))+
  guides(col = guide_legend(reverse = TRUE)) +
  xlab("") + ylab(y.lab)  + 
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,max(R.dat$upper.ci)+1.1))+
  scale_x_continuous(breaks=seq(1985,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2035. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1985, "", 1995, "", 2005, "", 2015, "", 2025),
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  annotate("text", x = yr, y = Inf, label = "(D)", size = 5, vjust = 1.5) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.12,1), legend.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.text.x = element_text(margin = margin(t = 4)),
        plot.margin = margin(5, 5, 5, 1, "points"))
rec.ts.plot
showtext_auto(FALSE)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_Rbiomass_panelD.png"), 
                                 rec.ts.plot, dpi = 600, width = 6.5, height =5)
if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_Rbiomass_panelD_french.png"), 
                                 rec.ts.plot, dpi = 600, width = 6.5, height =5)
showtext_auto(TRUE)
###### (D) Recruit B
######### (C) Estimated proportional exploitation rate / natural mortality plot
# Annual Exploit ######### NOT A PLOT
if(language == "english")  {
  y.lab <- "Exploitation (proportional rate)"
  rr.lab <- "RR"
}

if(language != "english") {
  y.lab <- "Exploitation (taux proportionnel)"
  rr.lab <- "ER"
}

mu.dat <- data.frame(
  year = years,
  lower.ci = apply(DD.out$GBa$sims.list$mu, 2, quantile, probs = alpha[1] / 2),
  upper.ci = apply(DD.out$GBa$sims.list$mu, 2, quantile, probs = 1 - alpha[1] / 2),
  median = DD.out$GBa$median$mu
)
exploit.latest <- round(mu.dat$median[mu.dat$year==yr],2)
# plot
e.m.plot <- ggplot(data=mu.dat) + 
  geom_ribbon(aes(ymin=lower.ci,ymax=upper.ci,x=years.ribbon),alpha=0.2,fill="grey10") +
  geom_hline(aes(yintercept=RR,col=rr.lab), lty="dotted", alpha=0.7, linewidth=0.8)+
  geom_line(aes(x=year,y=median),col="black" , linewidth = 0.4) + 
  geom_point(aes(x=year,y=median),col="black" , size=1) +
  scale_color_manual(name="",values=c("grey20"))+
  ylab(y.lab) + 
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,max(mu.dat$upper.ci))+0.051)+
  scale_x_continuous(breaks=seq(1985,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2035. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1985, "", 1995, "", 2005, "", 2015, "", 2025),
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1), expand = c(0, 0), limits = c(0, NA))+
  annotate("text", x = yr, y = Inf, label = "(C)", size = 5, vjust = 1.5) +
  theme(legend.position = c(0.12,1), legend.background = element_blank(),
        legend.key.width = unit(0.8, "cm"), 
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 4)),
        plot.margin = margin(5, 5, 5, 1, "points"))
e.m.plot
showtext_auto(FALSE)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_Exploitation_panelC.png"), 
                                 e.m.plot, dpi = 600, width = 6.5, height =5)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_Exploitation_panelC_french.png"), 
                                 e.m.plot, dpi = 600, width = 6.5, height =5)


showtext_auto(TRUE)
# Use this if plotting exploitation and natural mortality together:
#e.m.plot <- ggplot() + 
#  geom_ribbon(data=ann.exploit,aes(ymin=exploit.LCI,ymax=exploit.UCI,x=years.ribbon),alpha=0.2,fill="grey10") +
#  geom_hline(aes(yintercept=0.25,col="RR"), lty="longdash", alpha=0.7)+
#  geom_line(data=ann.exploit,aes(x=year,y=exploit,col="F"), linewidth = 0.4) + 
#  geom_point(data=ann.exploit,aes(x=year,y=exploit,col="F"),shape=16, size=1) +
#  geom_ribbon(aes(ymin=1-exp(-apply(DD.out$GBa$sims.list$m, 2, quantile, (alpha[1]/2))),
#                  ymax=1-exp(-apply(DD.out$GBa$sims.list$m, 2, quantile, 1-(alpha[1]/2))),x=years.ribbon),
#              alpha=0.12,fill="blue") +
# convert inst nat mort to prop nat mort
#  geom_line(aes(years,1-exp(-DD.out$GBa$median$m),col="M"), lty="dashed", linewidth = 0.4) + 
#  geom_point(aes(years,1-exp(-DD.out$GBa$median$m),col="M"), shape=17, size=1.3) +
#  scale_color_manual(name="",values=c("black","blue","firebrick"))+
#  ylab("Proportional rate") + 
#  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,1))+
#  scale_x_continuous(breaks=seq(1985,max(years)+2,5),
# (hello future modelers!) Need to adjust labels once we get to 2035. In 2030 add [, ""]. In 2035 add: [,2035].
#                     labels = c(1985, "", 1995, "", 2005, "", 2015, "", 2025),
#                     expand = c(0, 0), limits = c(0, NA)) + 
#  scale_y_continuous(breaks = seq(0, 1, 0.2), expand = c(0, 0), limits = c(0, NA))+
#  annotate("text", x = yr, y = Inf, label = "(C)", size = 5, vjust = 1.5) +
#  theme(legend.position = c(0.15,0.925), legend.background = element_blank(),
#        panel.border = element_rect(linewidth = 1, fill = NA),
#        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
#        axis.title.x = element_blank(),
#        axis.text.x = element_text(margin = margin(t = 4)),
#        plot.margin = margin(5, 0, 5, 0, "points"))

####### SAVE COWPLOT TO PNG TO MAINTAIN HIGH IMAGE RESOLUTION #################################################
# Change theme text size so it fits on the 4 panel figure
theme_set(theme_few(base_size = 14))

panel_1 <- cowplot::plot_grid(tac_land_plot, bm.ts.plot, e.m.plot, rec.ts.plot, align="v",ncol=2,axis="lr")
dev.off()
showtext_auto(FALSE)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel1_tacland_biom_recruitb_exploit.png"), 
                                 panel_1, dpi=600, width=9, height=9)

if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel1_tacland_biom_recruitb_exploit_wider.png"), 
                                 panel_1, dpi=600, width=9, height=7)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel1_tacland_biom_recruitb_exploit_french.png"), 
                                 panel_1, dpi=600, width=9, height=9)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel1_tacland_biom_recruitb_exploit_wider_french.png"), 
                                 panel_1, dpi=600, width=9, height=7)

showtext_auto(TRUE)

###################### PANEL 2 - Natural Mortality #########################################################
theme_set(theme_few(base_size = 12))

if(language == "english") 
{
  y.lab <- "Natural mortality\n(proportional rate, recruit)"
  ltm.lab <- "LTM"
}
if(language != "english") 
{
  y.lab <-"Mortalité naturelle\n(taux proportionnel, recrues)"
  ltm.lab <- "MLT"
}

## Recruit natural mortality plot
mR.dat <- data.frame(
  year = years,
  lower.ci = 1 - exp(-apply(DD.out$GBa$sims.list$mR, 2, quantile, probs = alpha[1] / 2)),
  upper.ci = 1 - exp(-apply(DD.out$GBa$sims.list$mR, 2, quantile, probs = 1 - alpha[1] / 2)),
  median = 1-exp(-DD.out$GBa$median$mR)
)
mR.ltm <- median(mR.dat$median[-length(mR.dat$median)])
mR.thisyear <- round(mR.dat$median[length(mR.dat$median)],2)
mR.lastyear <- round(mR.dat$median[length(mR.dat$median)-1],2)

R.nat.mort.plot <- ggplot() + 
  geom_ribbon(data = mR.dat, aes(x = years.ribbon, ymin = lower.ci, ymax = upper.ci),
              alpha=0.2,fill="grey10") +
  # long-term median (1991–`r yr-1`)
  geom_hline(aes(yintercept=mR.ltm,col=ltm.lab),lty="dashed", alpha=0.9) +
  geom_line(data=mR.dat, aes(year,median),col="black", linewidth = 0.4) + 
  geom_point(data=mR.dat, aes(year,median),col="black", size=1) + 
  xlab("") + ylab(y.lab) + 
  scale_color_manual(name="",values=c("grey20"))+
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(-0.01,(round(max(mR.dat$upper.ci),1)+0.11)))+
  scale_x_continuous(breaks=seq(1985,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2035. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1985, "", 1995, "", 2005, "", 2015, "", 2025),
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), expand = c(0, 0), limits = c(0, NA))+
  annotate("text", x = yr, y = Inf, label = "(A)", size = 5, vjust = 1.5) +
  theme(legend.position = c(0.12,0.95), legend.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.title.y = element_text(margin = margin(r = 7)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 2)),
        plot.margin = margin(5, 10, 5, 2, "points"))
R.nat.mort.plot
showtext_auto(FALSE)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_Rnaturalmort_panel2A.png"), 
                                 R.nat.mort.plot, dpi = 600, width = 6.5, height =5)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_Rnaturalmort_panel2A_french.png"), 
                                 R.nat.mort.plot, dpi = 600, width = 6.5, height =5)

showtext_auto(TRUE)

## Fully-recruited natural mortality plot

if(language == "english") 
{
  y.lab <- "Natural mortality\n(proportional rate, fully-recruited)"
  ltm.lab <- "LTM"
}
if(language != "english") 
{
  y.lab <-"Mortalité naturelle\n(taux proportionnel, pleinement recrutés)"
  ltm.lab <- "MLT"
}

m.dat <- data.frame(
  year = years,
  lower.ci = 1 - exp(-apply(DD.out$GBa$sims.list$m, 2, quantile, probs = alpha[1] / 2)),
  upper.ci = 1 - exp(-apply(DD.out$GBa$sims.list$m, 2, quantile, probs = 1 - alpha[1] / 2)),
  median = 1-exp(-DD.out$GBa$median$m)
)
m.ltm <- median(m.dat$median[-length(m.dat$median)])
m.thisyear <- round(m.dat$median[length(m.dat$median)],2)
m.lastyear <- round(m.dat$median[length(m.dat$median)-1],2)

FR.nat.mort.plot <- ggplot() + 
  geom_ribbon(data = m.dat, aes(x = years.ribbon, ymin = lower.ci, ymax = upper.ci),
              alpha=0.2,fill="grey10") +
  # long-term median (1991–`r yr-1`)
  geom_hline(aes(yintercept=m.ltm,col=ltm.lab),lty="dashed", alpha=0.9) +
  geom_line(data = m.dat, aes(year,median),col="black", linewidth = 0.4) + 
  geom_point(data = m.dat, aes(year,median),col="black", size=1) + 
  xlab("") + ylab(y.lab) + 
  scale_color_manual(name="",values=c("grey20"))+
  coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(-0.01,(round(max(mR.dat$upper.ci),1)+0.11)))+
  # changed so that y-axes would be the same between the 2 plots
  #coord_cartesian(xlim=c(min(years)-1,max(years)+2),ylim=c(0,(round(max(m.dat$upper.ci),1)+0.11)))+
  scale_x_continuous(breaks=seq(1985,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2035. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1985, "", 1995, "", 2005, "", 2015, "", 2025),
                     expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), expand = c(0, 0), limits = c(0, NA))+
  annotate("text", x = yr, y = Inf, label = "(B)", size = 5, vjust = 1.5) +
  theme(legend.position = c(0.12,0.95), legend.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.title.y = element_text(margin = margin(r = 7)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 2)),
        plot.margin = margin(5, 10, 5, 2, "points"))
FR.nat.mort.plot
showtext_auto(FALSE)
ggsave(filename=paste0(direct_out, year,"/Updates/", bank, "/Figures_and_tables/FSAR_FRnaturalmort_panel2B.png"), FR.nat.mort.plot, dpi = 600, width = 6.5, height =5)
showtext_auto(TRUE)
#save 
# panel with only natural mortality
panel_2 <- cowplot::plot_grid(R.nat.mort.plot, FR.nat.mort.plot, align="h",ncol=2,axis="l")
showtext_auto(FALSE)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel2_FRnatmort.png"), 
                                 panel_2, dpi = 600, width = 9, height = 3.5)

if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel2_FRnatmort_french.png"), 
                                 R.nat.mort.plot, dpi = 600, width = 6.5, height =5)


showtext_auto(TRUE)

###################### PANEL 3 - Condition #########################################################
if(language == "english") 
{
  y.lab <- expression(paste("Condition factor (", NULL, frac(g,dm^3) ,")"))
  ltm.lab <- "LTM"
}
if(language != "english") 
{
  y.lab <- expression(paste("Coefficient de condition (", NULL, frac(g,dm^3) ,")"))
  ltm.lab <- "MLT"
}

cf.max <- max(mod.dat$GBa$CF, na.rm=T)
cf.ltm <- round(median(mod.dat$GBa$CF[-length(mod.dat$GBa$CF)]),3)
cf.plot <- ggplot() + 
  geom_hline(aes(yintercept=cf.ltm,col=ltm.lab),lty="dashed", alpha=0.9) +
  geom_line(aes(x=mod.dat$GBa$year[-1:-2],y=mod.dat$GBa$CF[-1:-2]),col="black", linewidth = 0.4) + 
  geom_point(aes(x=mod.dat$GBa$year[-1:-2],y=mod.dat$GBa$CF[-1:-2]),col="black", size=1) + 
  xlab("") + ylab(y.lab) + 
  scale_color_manual(name="",values=c("grey20"))+
  coord_cartesian(xlim=c(min(years)-1,max(years)+2))+
  scale_x_continuous(breaks=seq(1985,max(years)+2,5),
                     # (hello future modelers!) Need to adjust labels once we get to 2035. In 2030 add [, ""]. In 2035 add: [,2035].
                     labels = c(1985, "", 1995, "", 2005, "", 2015, "", 2025),
                     expand = c(0, 0), limits = c(0, NA)) +
  annotate("text", x = yr, y = Inf, label = "(C)", size = 5, vjust = 1.5) +
  theme(legend.position = c(0.12,0.95), legend.background = element_blank(),
        panel.border = element_rect(linewidth = 1, fill = NA),
        axis.ticks = element_line(linewidth = 0.3), axis.ticks.length = unit(5, "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 2)),
        plot.margin = margin(5, 10, 5, 2, "points"))
cf.plot

panel_2 <- cowplot::plot_grid(R.nat.mort.plot, FR.nat.mort.plot, cf.plot, align="v",ncol=2,axis="lr")
showtext_auto(FALSE)
# panel with natural mortality and condition
if(language == "english")  ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                  "/Figures_and_tables/FSAR_panel2_FmortCondition.png"), panel_2, dpi = 600, width = 9, height = 7)
# panel with natural mortality and condition
if(language == "english")  ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                  "/Figures_and_tables/FSAR_panel2_FmortCondition.png"), panel_2, dpi = 600, width = 9, height = 7)
# figure for presentations (larger scale)
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_condition.png"), cf.plot, dpi = 600, width = 6.5, height =5)
# figure for FSAR document
if(language == "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel3_condition.png"), cf.plot, dpi = 600, width = 4.5, height = 3.5)
# Frenched

if(language != "english")  ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                  "/Figures_and_tables/FSAR_panel2_FmortCondition_french.png"), 
                                  panel_2, dpi = 600, width = 9, height = 7)
# figure for presentations (larger scale)
if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_condition_french.png"), 
                                 cf.plot, dpi = 600, width = 6.5, height =5)
# figure for FSAR document
if(language != "english") ggsave(filename=paste0(direct_out, year,"/Updates/", bank, 
                                                 "/Figures_and_tables/FSAR_panel3_condition_french.png"), 
                                 cf.plot, dpi = 600, width = 4.5, height = 3.5)

showtext_auto(TRUE)

