#####################################  This function is used to generate all of the figures used in survey summary  #####################
#####################################  So additional figures are created in this script, the seedboxes are taken care of ###########
#####################################  in this function.  Banquerau is  not included in this function at this time ######
## Created by DK December 2015
# Update history
#Commented, checked  and revised by DK March 31, 2016
# June 16th 2016, revised to add the "season" variable so we could save results for the spring survey.
# July 3rd(ish) 2016, revised to include the "seedboxes" plots + some changes to GB spring to get MW-SH figure fixed up and get all the tows included.
# July 7th(ish) 2016, revised abund/biomass/clap-ts plots to get axes more consistent (set a low threshold on the y-axes for the plots), revised CF
# color ramp, and made some major modification to the SHF plots scales (split the plot into a PR and rec/FR)
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
# 1:  ScallopMap.r
# 2:  stdts.plt.R
# 3:  survey.ts.r
# 4:  shf.plt.r
# 5:  contour.gen.r
# 6:  shwt.plt1.r
# 7:  Clap3.plt.R
# 8:  gridPlot.r
# 9:  meat_count_shell_height_breakdown_figure.r    
##
###############################################################################################################

###############################################################################################################
# Arguments
# 1:  plots:    What plots do you want to make.  Defaults to all plots options include
  # a:  PR-spatial        - Spatial Pre-recruits 
  # b:  Rec-spatial       - Spatial Recruits
  # c:  FR-spatial        - Spatial fully recruited
  # d:  CF-spatial        - Spatial condition factor
  # e:  MC-spatial        - Spatial Meat count
  # f:  Clap-spatial      - Spatial clappers (%), Recruits + Fully recruited
  # g:  Survey            - Spatial survey tow locations + strata where applicable
  # h:  MW-SH             - Meat Weight Shell Height 2 panel plot, MW-SH by tow (left panel) + Condition time series (right panel)
  # i:  abund-ts          - Survey estimated abundance (#/tow) time series of Pre recruits, recruits and fully recruited
  # j:  biomass-ts        - Survey estimated biomass (kg/tow) time series of Pre recruits, recruits and fully recruited
  # k:  SHF               - Shell height frequency for the last 6 years
  # l:  SHF-large         - SHell height frequency for the last 6 years for SH bins > 70 mm.
  # m:  SHF-split         - Shell height frequency for the last 6 years, split at 60 mm to enable re-scaling of large SHF size bins
  # n:  clapper-abund-ts  - Clapper abundance (#/tow) time series of Pre recruits, recruits and fully recruited
  # o:  clapper-per-ts    - Average % clappers per tow time series all 3 size classes
  # p:  SH-MW-CF-ts       - Time series of average shell height, meat weight, and condition factor.
  # q:  breakdown         - Plot of biomass by shell height + meat count
  # r:  seedboxes         - Plot of the seedboxes, this includes several plots which will all be produced if there is currently an open seedbox
# 2:  banks:    The banks to create figures for.  Option are "BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB" 
#                (note Banquereau is not supported yet and the GB is Georges Bank Spring
# 3:  direct:   The working directory to put figures are from which to grab data.  Default = "Y:/Offshore scallop/Assessment/", 
# 4:  yr:       The survey year of interest.  Default = as.numeric(format(Sys.time(), "%Y")) which attempts to produce plots for the current year.
#               NB: If trying to reproduce figures from previous years make sure the Rdata output from SurverySummary_data.r for that year exists!
# 5:  add.title:Add titles to the figures; T/F.  Default = T
# 6:  fig:      Plot the figures to "screen" or to a "file". If fig = "file" figures are placed in the following directory (which must exist!!)
#               direct/yr/Presentations/Survey_summary/bank) (e.g. ...Y:/Offshore scallop/Assessment/2015/Presentations/Survey_summary/GBa/)
# 7:  season:   For the spring survey we need to identify that we don't have all the results in yet.  When running the scripts with only spring  
#               survey data set to "spring".  If just running GBa and GBb you can set this to "summer" if you've already created the Rdata file.
#               When summer survey is complete you can also set this to the default of "both".  Used to determine name of saved results.

###############################################################################################################

survey.figs <- function(plots = c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial","Survey","MW-SH",
                                  "abund-ts","biomass-ts","SHF","SHF-large","SHF-split",
                                  "clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown","seedboxes"),
                       banks = c("BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB"),
                       direct = "Y:/Offshore scallop/Assessment/", yr = as.numeric(format(Sys.time(), "%Y"))  ,
                       add.title = T,fig="screen",season="both"
                       )
{ 
  tmp <- direct
    # Load the appropriate data.
    if(season == "both") load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
    if(season == "spring") 
    {
      # If we are making the MW/SH plot & looking at GBa we need to get the spring GB survey data...
      if(any(plots %in% "MW-SH") && any(banks %in% "GB"))
      {
        # This loads last years Survey object results.
        load(paste(direct,"Data/Survey_data/",(yr-1),"/Survey_summary_output/Survey_object.Rdata",sep=""))  
        survey.obj.last <- survey.obj
      } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
    }
      
    if(season == "summer") 
    {
      # If we are making the MW/SH plot & looking at GBa we need to get the spring GB survey data...
      if(any(plots %in% "MW-SH") && any(banks %in% "GBa"))
      {
        # This loads the springs survey results.
        load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
        survey.obj.last <- survey.obj
      } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_summer_results.Rdata",sep=""))  
    }# if(season == "summer")

  direct <- tmp

  # These are the functions used to within the heart of the code to make stuff happen
  source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/stdts.plt.R",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/survey.ts.r",sep=""),local=T) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/shf.plt.r",sep="")) 
  source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/shwt.plt1.r",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/Clap3.plt.R",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/gridPlot.r",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/meat_count_shell_height_breakdown_figure.r",sep="")) 
  require(viridis) || stop("Load the viridis package for the color ramps")

# If necessary bring in the fishery regulations (only used for seedbox and breakdown figures)
if(any(plots %in% c("seedboxes","breakdown"))) fish.reg <- read.csv(paste(direct,"Data/Fishery_regulations_by_bank.csv",sep="")) # Read1
  
#Initialize some variables I need.
len <- length(banks)
com.contours <- NULL
pre.contours <- NULL
rec.contours <- NULL
mc.contours <- NULL
cf.contours <- NULL
clap.contours <- NULL
# Set up objects for plot labels.  
N.tow.lab <- expression(frac(N,tow))
cf.lab <-    expression(paste("CF:",bgroup("(",frac(g,dm^3)   ,")")))
mc.lab <-    expression(paste("MC:",bgroup("(",frac(N,"500 g"),")"))) 

# Set up default colors for our spatial plot for survey strata, Tow Number, Condition Factor, and Meat Count.
N.col <- "YlGn"
cf.col <- "YlOrBr"
mc.col <- "RdYlBu"
X.lvl <- "PuBuGn" # For any time we have an usually large event this is the color of the extra levels required.
clap.col <- "YlOrRd"

# Clean up any open plot devices.
if(is.null(dev.list())==F) dev.off()
# Run through all of the banks of interest.
for(i in 1:len)
{
  # If not specified use the latest year in each time series.
  #yr <- max(surv.Live[[banks[i]]]$year,na.rm=T)
  # Get the RS and CS for the bank...
  RS <- size.cats$RS[size.cats$Bank == banks[i]]
  CS <- size.cats$CS[size.cats$Bank == banks[i]]
  # Grab any seedboxes for this bank from this year
  sb <- subset(seedboxes,Bank == banks[i] & Open >= paste(yr,"-01-01",sep=""))
  if(banks[i] == "GB") sb <- subset(seedboxes,Bank == "GBa" & Open >= paste(yr,"-01-01",sep=""))
  
  ###  Now for the plots, first the survey data...
  
  # Get the  bank survey boundary polygon
  bound.poly.surv <- subset(survey.bound.polys,label==banks[i]) 
  # These need a different boundary polygon
  attr(bound.poly.surv,"projection")<-"LL"
  
  #Detailed survey polygones
  detail.poly.surv <- subset(survey.detail.polys,label==banks[i])
  attr(detail.poly.surv,"projection")<-"LL"
  # Get the strata areas.
  strata.areas <- subset(survey.info,label==banks[i],select =c("PID","towable_area"))
  #Get all the details of the survey strata
  surv.info <- subset(survey.info,label== banks[i])
  
  ### If we are missing years in the data I want to add those years in as NA's so the plots see those as NA's  ####
  check.year <- min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(survey.obj[[banks[i]]][[1]]$year,na.rm=T)
  missing.year <- check.year[!is.element(check.year,survey.obj[[banks[i]]][[1]]$year)]
  if(length(missing.year > 0))
  {
    # Before doing anything I need to add a dummy row to the SHF data with the year in it
    survey.obj[[banks[i]]][[2]]$n.yst <- cbind(survey.obj[[banks[i]]][[1]]$year,survey.obj[[banks[i]]][[2]]$n.yst)
    survey.obj[[banks[i]]][[2]]$w.yst <- cbind(survey.obj[[banks[i]]][[1]]$year,survey.obj[[banks[i]]][[2]]$w.yst)
    # Make a dataframe of NA's to add to the end of survey object, then add it.
    tst <- data.frame(matrix(NA,ncol=ncol(survey.obj[[banks[i]]][[1]]),nrow=length(missing.year)))
    tst[,1] <- missing.year
    survey.obj[[banks[i]]][[1]][nrow(survey.obj[[banks[i]]][[1]])+(1:length(missing.year)),] <- tst
    # Now re-order the data so it is sequential, likely not strictly necessary...
    survey.obj[[banks[i]]][[1]] <- survey.obj[[banks[i]]][[1]][order(survey.obj[[banks[i]]][[1]]$year),]
    # Now do the same for the second part of the survey data (the counts/biomasses per size bin)
    tst <- matrix(NA,ncol=ncol(survey.obj[[banks[i]]][[2]]$n.yst),nrow=length(missing.year))
    tst[,1] <- missing.year
    survey.obj[[banks[i]]][[2]]$n.yst <- rbind(survey.obj[[banks[i]]][[2]]$n.yst,tst)
    survey.obj[[banks[i]]][[2]]$w.yst<-  rbind(survey.obj[[banks[i]]][[2]]$w.yst,tst)
    survey.obj[[banks[i]]][[2]]$n.yst <- survey.obj[[banks[i]]][[2]]$n.yst[order(survey.obj[[banks[i]]][[2]]$n.yst[,1]),]
    survey.obj[[banks[i]]][[2]]$w.yst <- survey.obj[[banks[i]]][[2]]$w.yst[order(survey.obj[[banks[i]]][[2]]$w.yst[,1]),]
    survey.obj[[banks[i]]][[2]]$n.yst <- survey.obj[[banks[i]]][[2]]$n.yst[,-1]
    survey.obj[[banks[i]]][[2]]$w.yst <- survey.obj[[banks[i]]][[2]]$w.yst[,-1]
  } # if(length(missing.year > 0))
  
 ############  Now start getting the spatial data for this year for the specific bank.
  
  # The banks[i] == "Mid" is a cheat we use to get the Middle bank contours to look decent for CF and MC data.
  if(any(plots %in% c("FR-spatial","seedboxes")) || banks[i]=="Mid")
    {
  # Fully recruited scallops  
    if(banks[i] != "GB"&& banks[i] != "BBs") com.contours[[banks[i]]]<-contour.gen(subset(surv.Live[[banks[i]]],year==yr,
                                               c('tow','lon','lat','com')),
                                 ticks=c(1,5,10,50,100,500,1000,2000,5000,10000,20000,50000),
                                 str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',
                                 color.fun=tim.colors,id.par=5,units='#/tow',
                                 plot=F,subscale=0.1,direct =direct)
    
    # Adding June 23, 2016 to minimize artifacts in these figures... 
    if(banks[i] %in% c("GB")) com.contours[[banks[i]]]<-contour.gen(subset(surv.Live[[banks[i]]],year==yr,
                                                                    c('tow','lon','lat','com')),
                                                             ticks=c(1,5,10,50,100,500,1000,2000,5000,10000,20000,50000),
                                                             str.min=0,place=2,id.par=3.5,
                                                             interp.method='gstat',key='strata',blank=F,subset.poly='square',
                                                             plot=F,subset.eff=0,subscale=0.25,direct = direct)
    
    if(banks[i] %in% c("BBs")) com.contours[[banks[i]]]<-contour.gen(subset(surv.Live[[banks[i]]],year==yr,
                                                                            c('tow','lon','lat','com')),
                                                                     ticks=c(1,5,10,50,100,500,1000,2000,5000,10000,20000,50000),
                                                                     str.min=0,place=2,id.par=3.5,
                                                                     interp.method='gstat',key='strata',blank=T,subset.poly='square',
                                                                     plot=F,subset.eff=0,direct = direct)
    
    fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
                                  list(a=as.character(CS),year=as.character(yr),bank=banks[i]))
    if(banks[i] == "GB") fully.rec.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-Spr-",year,")",sep="")),
                                                       list(a=as.character(CS),year=as.character(yr),bank=banks[i]))
    } # end   if(any(plots %in% "FR-spatial"))
  
  # PRE RECRUIT AND RECRUIT SIZED SCALLOP
  if(any(plots %in% c("PR-spatial","seedboxes")))
    {
      if(banks[i] != "GB" && banks[i] != "BBs") pre.contours[[banks[i]]]<-contour.gen(subset(surv.Live[[banks[i]]],year==yr,
                                                   c('tow','lon','lat','pre')),
                                     ticks=c(1,5,10,50,100,500,1000,2000,5000,10000,20000,50000),
                                     str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',
                                     color.fun=tim.colors,id.par=5,plot=F,subscale=0.1,direct = direct)
      
      # Adding June 23, 2016 to minimize artifacts in these figures... 
      if(banks[i] %in% c("GB")) pre.contours[[banks[i]]]<-contour.gen(subset(surv.Live[[banks[i]]],year==yr,
                                                                       c('tow','lon','lat','pre')),
                                                                ticks=c(1,5,10,50,100,500,1000,2000,5000,10000,20000,50000),
                                                                str.min=0,place=2,id.par=3.5,
                                                                interp.method='gstat',key='strata',blank=F,subset.poly='square',
                                                                plot=F,subset.eff=0,subscale=0.25,direct = direct)
      
      if(banks[i] %in% c("BBs")) pre.contours[[banks[i]]]<-contour.gen(subset(surv.Live[[banks[i]]],year==yr,
                                                                                   c('tow','lon','lat','pre')),
                                                                            ticks=c(1,5,10,50,100,500,1000,2000,5000,10000,20000,50000),
                                                                            str.min=0,place=2,id.par=3.5,
                                                                            interp.method='gstat',key='strata',blank=T,subset.poly='square',
                                                                            plot=F,subset.eff=0,direct = direct)
      
      pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
                                  list(b=as.character(RS),year=as.character(yr),bank=banks[i]))
      if(banks[i] == "GB") pre.rec.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-Spr-",year,")",sep="")),
                                                      list(b=as.character(RS),year=as.character(yr),bank=banks[i]))
    } # end if(any(plots %in% "PR-spatial"))
  
  if(any(plots %in% c("Rec-spatial","seedboxes")))
    {
    if(banks[i] != "GB" && banks[i] != "BBs") rec.contours[[banks[i]]]<-contour.gen(subset(surv.Live[[banks[i]]],year==yr,
                                                   c('tow','lon','lat','rec')),
                                     ticks=c(1,5,10,50,100,500,1000,2000,5000,10000,20000,50000),
                                     str.min=0,interp.method='gstat',points=T,blank=T,res=0.01,key='log.cont',
                                     color.fun=tim.colors,id.par=5,units='#/tow',
                                     plot=F,subscale=0.1,direct = direct)
      
      # Adding June 23, 2016 to minimize artifacts in these figures... 
      if(banks[i] %in% c("GB")) rec.contours[[banks[i]]]<-contour.gen(subset(surv.Live[[banks[i]]],year==yr,
                                                                c('tow','lon','lat','rec')),
                                                                ticks=c(1,5,10,50,100,500,1000,2000,5000,10000,20000,50000),
                                                                str.min=0,place=2,id.par=3.5,
                                                                interp.method='gstat',key='strata',blank=F,subset.poly='square',
                                                                plot=F,subset.eff=0,subscale=0.25,direct = direct)
      
      if(banks[i] %in% c("BBs")) rec.contours[[banks[i]]]<-contour.gen(subset(surv.Live[[banks[i]]],year==yr,
                                                                              c('tow','lon','lat','rec')),
                                                                       ticks=c(1,5,10,50,100,500,1000,2000,5000,10000,20000,50000),
                                                                       str.min=0,place=2,id.par=3.5,
                                                                       interp.method='gstat',key='strata',blank=T,subset.poly='square',
                                                                       plot=F,subset.eff=0,direct = direct)
      
      rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
                              list(a=as.character(CS-1),b=as.character(RS),year=as.character(yr),bank=banks[i]))
      if(banks[i] == "GB") rec.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-Spr-",year,")",sep="")),
                                                   list(a=as.character(CS-1),b=as.character(RS),year=as.character(yr),bank=banks[i]))
    } # end if(any(plots %in% "Rec-spatial"))
    
  # CONDITION FACTOR and MEAT COUNT OF SCALLOP ON MIDDLE BANK in 2014
  #Source19  source("fn/contour.gen.r") 
  if(any(plots %in% "MC-spatial"))
    {
    
      mc.contours[[banks[i]]]<-contour.gen(na.omit(subset(CF.current[[banks[i]]],select=c('tow','lon','lat','meat.count'))),
                                ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,
                                interp.method='gstat',key='strata',blank=F,subset.poly='square',
                                plot=F,subset.eff=0,subscale=0.25,direct = direct)
  
      mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
                         list(m=as.character(CS),year=as.character(yr),bank=banks[i]))
      if(banks[i] == "GB") mc.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-Spr-",year,")",sep="")),
                                                  list(m=as.character(CS),year=as.character(yr),bank=banks[i]))
      
    } # end if(any(plots %in% "MC-spatial"))
  
  if(any(plots %in% "CF-spatial"))
    {
      cf.contours[[banks[i]]]<-contour.gen(subset(CF.current[[banks[i]]],select=c('tow','lon','lat','CF')),ticks='define',
                                nstrata=7,str.min=0,place=2,id.par=3.5,
                                interp.method='gstat',key='strata',blank=F,plot=F,
                                subset.poly='square',subset.eff=0,subscale=0.25,direct = direct)
  
      cf.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
                         list(year=as.character(yr),bank=banks[i]))
      if(banks[i] == "GB") cf.title <- substitute(bold(paste("Condition factor (", bank,"-Spr-",year,")",sep="")),
                                                  list(year=as.character(yr),bank=banks[i]))
    } # end if(any(plots %in% "CF-spatial"))
  
  # Clapper contour plot
  if(any(plots %in% "Clap-spatial"))
    {
      if(banks[i] == "GB") yrs <-  max(surv.Clap[[banks[i]]]$year,na.rm=T)
      if(banks[i] != "GB") yrs <-  yr
      if(banks[i] != "Mid") 
        {
          clap.contours[[banks[i]]]<-contour.gen(na.omit(subset(surv.Clap[[banks[i]]],year==yrs,
                                                        c('tow','lon','lat','clap.prop'))),
                             ticks='define',nstrata=9,str.min=0,interp.method='gstat',direct=direct,
                             points=T,blank=T,res=0.005,key='log.cont',color.fun=tim.colors,id.par=3.5,
                             units='#/tow',plot=F,blank.dist=0.08,subset.poly='square',subset.eff=0,subscale=0.25)
  
          clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-",year,")",sep="")),
                               list(c=as.character(RS),bank=banks[i],year=as.character(yr)))
          if(banks[i] == "GB") clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-Spr-",year,")",sep="")),
                                                            list(c=as.character(RS),bank=banks[i],year=as.character(yr)))
        } # end if(banks[[i]] != "Mid")
    } # end if(any(plots %in% "CF-spatial"))

### End Contour generation ###

  
#####  Set up the survey figure #############  Set up the survey figure #############  Set up the survey figure ########
#####  Set up the survey figure #############  Set up the survey figure #############  Set up the survey figure ########

  #Do we want to plot the survey?
  if(any(plots %in% "Survey"))
  {
    survey.title <- substitute(bold(paste("Survey strata (",bank,"-",year,")",sep="")),
                               list(year=as.character(yr),bank=banks[i]))
# Now we'll need to do a few things differently if there are no survey strata on a bank, this will work for Middle and German
  
    if(banks[i] %in% c("Ger","Mid"))
      {
        # No survey strata so this bank gets a unique Survey title
        survey.title <- substitute(bold(paste("Survey (",bank,"-",year,")",sep="")),
                                           list(year=as.character(yr),bank=banks[i]))
    
      } # end if(banks[i] %in% c("Ger","Mid"))
    if(banks[i] == "GB")  survey.title <- substitute(bold(paste("Survey (",bank,"-Spr-",year,")",sep="")),
                                                     list(year=as.character(yr),bank=banks[i]))
  
  
    if(fig == "file")
      {
        png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/survey_strata.png",sep=""),units="in",width = 11, 
        height = 8.5,res=420,bg = "transparent")
      }
    
    if(fig == "screen") windows(11,8.5)
    par(mfrow=c(1,1))
    if(add.title == F) survey.title <- ""
    # Make the plot, this one is for cases in which we have survey strata
      if(length(strata.areas[,1]) > 0)
      {
        ScallopMap(banks[i],poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
               plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
               nafo.bord = F,nafo.lab = F,title=survey.title,dec.deg = F)
      } # end if(length(strata.areas[,1]) > 0)
    # For the banks without any strata
    if(length(strata.areas[,1]) == 0)
      {
        ScallopMap(banks[i],direct = direct,cex.mn=2,boundries="offshore",
                   plot.bathy=T,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
                   nafo.bord = F,nafo.lab = F,title=survey.title,dec.deg = F)
      }
    # Add the regular survey tows.
    #bg.col<-tapply(GBb.surv.info$col,GBb.surv.info$PName,unique)[c(2,3,1,4,5)]
    
    # Add the regular survey tows.
    points(lat~lon,surv.Live[[banks[i]]],subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
    
    if(banks[i] %in% c("GBa","BBn"))  
      {
      
      points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
                                             & state =='live' & random==5,pch=24,bg="darkorange",cex=0.8)
      legend("topright",legend = c(paste('exploratory (n =',
                                         length(unique(subset(surv.Live[[banks[i]]],year==yr & random==5)$tow)),
                                         ")",sep=""),
                                   paste('regular (n =',
                                         length(unique(subset(surv.Live[[banks[i]]],year==yr & random==1)$tow)),
                                         ")",sep="")),title="Tow type",
             pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
      } # end if(banks[i] %in% c("GBa","BBn"))
    
    # Add in the bank specific survey information, for some this is easier than for others.
    if(banks[i] == "GBa") 
      {
    
        legend("bottomleft",legend=c(surv.info$PName),
               fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
               pch=c(rep(NA,length(surv.info$PName))),title = "Strata",title.adj=0.1,
               pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
        legend(-67.15,41.82,legend = round(surv.info$area_km2),
               fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
               pch=c(rep(NA,length(surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=1.1,
               pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
        legend(-66.12,41.55,legend = as.numeric(with(subset(surv.Live[[banks[i]]],year==yr),tapply(tow,new.stratum,length))),
               fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
               pch=c(rep(NA,length(surv.info$PName))),title = "Number of tows",title.adj=0.1,
               pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
      } # End if(banks[i] == "GBa") 
    
    if(banks[i] == "Ger") 
      {
        points(lat~lon,surv.Live[[banks[i]]],subset=year==yr &
                                  random==3,pch=22,bg="yellow",cex=0.8)
       legend('topleft',legend=
               c(paste('regular (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                            random==1)$tow)),")", sep=""),
                 paste('repeated (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                             random==3)$tow)),")", sep="")),
             pch=c(20,22), pt.bg = c("black","yellow"),bty='n',cex=1, inset = .02)
    
      } # end if(banks[i] == "Ger") 
      
    if(banks[i] == "BBn" ) 
      {
        legend("bottomleft",legend=surv.info$PName,fill=surv.info$col,bty='n',cex=1, title = "Strata")
        legend("bottom",legend = round(surv.info$area_km2),
               fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
               pch=c(rep(NA,length(surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.1,
               pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
        
        legend("bottomright",legend = as.numeric(with(subset(surv.Live[[banks[i]]],
                                    year== yr & random ==1),tapply(tow,new.stratum,length))),
               fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
               pch=c(rep(NA,length(surv.info$PName))),title = "Number of tows",title.adj=0.1,
               pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
      } # end if(banks[i] == "BBn" || banks[i] == "Sab" || banks[i] == "GBb") 
    
    if(banks[i] == "Sab" || banks[i] == "Mid"|| banks[i] == "BBs" || banks[i] == "Ban"|| banks[i] == "GBb") 
    {
      legend("left",pch=c(20), pt.bg = c("black"), title="Tow type",
             legend = paste('regular (n =',length(unique(subset(surv.Live[[banks[i]]],
                                                                year==yr & random==1)$tow)),")",sep=""), inset=0.01,bg=NA,box.col=NA)	
    } # end if(banks[i] == "Sab" || banks[i] == "Mid"|| banks[i] == "BBs" || banks[i] == "Ban"|| banks[i] == "GBb" || banks[i] == "GB") 
    
    
    if(banks[i] == "GB") 
      {
      points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
             & state =='live' & random%in% c(2,4,5),pch=24,bg="darkorange",cex=0.8)
      points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
             & state =='live' & random==3,pch=22,bg="yellow",cex=0.8)
      
      legend("topright",legend = c(paste('exploratory (n =',
                                         length(unique(subset(surv.Live[[banks[i]]],year==yr & random%in% c(2,4,5))$tow)),")",sep=""),
                                   paste('repeated (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                                                random==3)$tow)),")", sep="")),title="Tow type",
                                         pt.bg = c("darkorange","yellow"),pch=c(24,22),bg = "white",inset=0.01,box.col="white")
      } # end if(banks[i] == "Sab" || banks[i] == "Mid"|| banks[i] == "BBs" || banks[i] == "Ban"|| banks[i] == "GBb" || banks[i] == "GB") 
    
    
    
    if(banks[i] == "Sab" ||  banks[i] == "GBb") 
      {
        legend("topleft",legend=surv.info$PName,fill=surv.info$col,bty='n',cex=1, title = "Strata")
        legend("top",legend = round(surv.info$area_km2),
               fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
               pch=c(rep(NA,length(surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.1,
               pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
        
        legend("topright",legend = as.numeric(with(subset(surv.Live[[banks[i]]],
                                                             year== yr & random ==1),
                                                   tapply(tow,new.stratum,length))),
               fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
               pch=c(rep(NA,length(surv.info$PName))),title = "Number of tows",title.adj=0.1,
               pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
      } # end if(banks[i] == "BBn" || banks[i] == "Sab" || banks[i] == "GBb") 
    
    if(banks[i] == "BBs")
      {
        legend("bottomleft",legend=surv.info$PName,fill=surv.info$col,bty='n',cex=1, title = "Strata")
        legend("bottom",legend = round(surv.info$area_km2),
               fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
               pch=c(rep(NA,length(surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.1,
               pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
        
        # I need to add a 0 here as we have never surveyed in the tiny strata 2...
        tow.leg <- c(aggregate(tow~new.stratum,subset(surv.Live[[banks[i]]],year==yr &
                                                       random==1 & state=="live"),length)$tow[1],0,
                     aggregate(tow~new.stratum,subset(surv.Live[[banks[i]]],year==yr &
                                                       random==1 & state=="live"),length)$tow[2:3])
        legend("bottomright",legend = tow.leg,
               fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
               pch=c(rep(NA,length(surv.info$PName))),title = "Number of tows",title.adj=0.1,
               pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
      } # End if(banks[i] == "BBs")
    # Add the survey boxes if they exist.
    if(length(sb[,1]) > 0) addPolys(sb,lty=2,lwd=2)
     
    if(fig == "file") dev.off()
    
  } # end if(length(plots[grep("Survey",plots)]>0))

####################################  END SURVEY PLOT ####################################  END SURVEY PLOT
####################################  END SURVEY PLOT ####################################  END SURVEY PLOT
####################################  END SURVEY PLOT ####################################  END SURVEY PLOT



################  Next up are the rest of the spatial plots ###########################
################  Next up are the rest of the spatial plots ###########################
################  Next up are the rest of the spatial plots ###########################

    spatial.maps <- plots[grep("spatial",plots)]
    if(length(spatial.maps > 0))
    {
    
      for(k in 1:length(spatial.maps))
      {
          # Now select what we are calculating and get the figure an legend titles correct
        if(spatial.maps[k] == "PR-spatial")    
          {
            con <-  pre.contours[[banks[i]]] ; fig.title <- pre.rec.title ;   leg.title <- N.tow.lab
          }
        if(spatial.maps[k] == "Rec-spatial")        
          {
            con <-  rec.contours[[banks[i]]]; fig.title <- rec.title ;        leg.title <- N.tow.lab
          }
        if(spatial.maps[k] == "FR-spatial") 
          {
            con <-  com.contours[[banks[i]]]; fig.title <- fully.rec.title ;  leg.title <- N.tow.lab
          }
        if(spatial.maps[k] == "CF-spatial")       
          {
            con <-  cf.contours[[banks[i]]]; fig.title <- cf.title ;          leg.title <- cf.lab
          }
        if(spatial.maps[k] == "MC-spatial")      
          {
            con <-  mc.contours[[banks[i]]]; fig.title <- mc.title ;          leg.title <- mc.lab
          }
        if(spatial.maps[k] == "Clap-spatial")        
          {
          if(banks[i] != "Mid")  
            {
              con <-  clap.contours[[banks[i]]]; 
              fig.title <- clap.dis.title ;  
              leg.title <- "% Dead"
            } #end if(banks[i] != "Mid")  
          }
    
    # Don't add the titles
    if(add.title == F) fig.title <- ""
        
    # Get the contours for the Abundance maps.
    #range(pre.contours[[banks[i]]]$image.dat$z)
    if(spatial.maps[k] %in% c("PR-spatial", "Rec-spatial", "FR-spatial"))
      {
        lvls1=c(1,5,10,50,100,500,1000)
        CL <- contourLines(con$image.dat,levels=lvls1)
        CP <- convCP(CL)
        Cont1.poly <- joinPolys(CP$PolySet,bound.poly.surv)
        cont1.data<- data.frame(PID=1:length(lvls1),col=rev(magma(length(lvls1),alpha=1,begin=0.6,end=1)),border=NA,stringsAsFactors = F) 
        #cont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F) 
        lvls2=c(2000,5000,10000,20000,50000)
        # If counts are high enough to need lvls2 we use it, if not we just use lvls1.
        if(max(con$image.dat$z)>=lvls2[1])
        {
          CL <- contourLines(con$image.dat,levels=lvls2)
          CP <- convCP(CL)
          Cont2.poly <- joinPolys(CP$PolySet,bound.poly.surv)
          Cont2.poly$PID<-Cont2.poly$PID+length(lvls1)
          Cont.poly<-rbind(Cont1.poly,Cont2.poly)
          lvls<-c(lvls1,lvls2)
          #cont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
          #                           col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA,stringsAsFactors = F)
          #cont.data <-rbind(cont1.data,cont2.data)
          cont.data <- data.frame(PID=1:length(lvls),col=c(cont1.data$col,
                                    rev(magma(length(lvls2),alpha=1.0,begin=0.1,end=0.5))),border=NA,stringsAsFactors = F)
        } # end if(max(rec.contours$image.dat$z)>=lvls2[1])
        if(max(con$image.dat$z)<lvls2[1])
        {
          Cont.poly<-Cont1.poly
          cont.data<-cont1.data
          lvls <- lvls1
        } # end if(max(rec.contours$image.dat$z)<lvls2[1])
      } # end if(spatial.maps[k] %in% c("Pre-recruits", "Recruits", "Fully_Recruited"))
    
    
    
    # The abundance spatial plots are working, now get Condition, MC and Clappers going
    if(spatial.maps[k] %in% c("CF-spatial"))
      {
        lvls=c(5,8,10,12,14,16,18)
        CL <- contourLines(con$image.dat,levels=lvls)
        CP <- convCP(CL)
        Cont.poly <- joinPolys(CP$PolySet,bound.poly.surv)
        if(banks[i] == "Mid") Cont.poly <- joinPolys(CP$PolySet,subset(convCP(contourLines(com.contours[[banks[i]]]$image.dat,
                                                                              levels=lvls))$PolySet,PID==1))
        
        cont.data<- data.frame(PID=1:length(lvls),col=rev(inferno(length(lvls),alpha=0.8,begin=0.35,end=1)),lvls=lvls,border=NA,stringsAsFactors = F) 
        #cont.data<- data.frame(PID=1:length(lvls),col=rev(heat.colors(length(lvls),alpha=1)),lvls=lvls,border=NA,stringsAsFactors = F) 
        #dat <- range(con$image.dat$z[con$image.dat$z >0])
        dat <- lvls
        #if(dat[2] < max(lvls)) dat <- lvls[which(lvls < max(dat[1]))]#seq(lvls[max(which(lvls <= floor(dat[1])))],lvls[min(which(lvls >= (dat[2])))-1],by=1)
        #if(dat[2] > max(lvls)) dat <- lvls[which(lvls >= floor(min(dat[1])))]#dat <- lvls[]seq(lvls[max(which(lvls <= floor(dat[1])))],max(lvls),by=1)        
      } # end if(spatial.maps[k] %in% c("Condition"))
    
    if(spatial.maps[k] %in% c("MC-spatial"))
      {
      lvls=seq(5,50,5)
      CL <- contourLines(con$image.dat,levels=lvls)
      CP <- convCP(CL)
      Cont.poly <- joinPolys(CP$PolySet,bound.poly.surv)
      if(banks[i] == "Mid") Cont.poly <- joinPolys(CP$PolySet,subset(convCP(contourLines(com.contours[[banks[i]]]$image.dat,
                                                                        levels=lvls))$PolySet,PID==1))
      cont.data<- data.frame(PID=1:length(lvls),col=(viridis(length(lvls),alpha=0.75,begin=0,end=1)),lvls=lvls,border=NA,stringsAsFactors = F) 
      dat <- range(con$image.dat$z[con$image.dat$z >0])
      if(dat[2] < max(lvls)) dat <- seq(lvls[max(which(lvls <= floor(dat[1])))],lvls[min(which(lvls >= (dat[2])))-1],by=5)
      if(dat[2] > max(lvls)) dat <- seq(lvls[max(which(lvls <= floor(dat[1])))],max(lvls),by=5)
      } # end if(spatial.maps[k] %in% c("MC-spatial"))
    # Clapper spatial, but not for Middle bank, it does't work...
    if(spatial.maps[k] %in% c("Clap-spatial") && banks[i] != "Mid")
      {		
        lvls=c(1,5,10,15,20,50)
        CL <- contourLines(con$image.dat,levels=lvls)
        CP <- convCP(CL)
        Cont.poly <- joinPolys(CP$PolySet,bound.poly.surv)
        cont.data<- data.frame(PID=1:length(lvls),col=rev(plasma(length(lvls))),border=NA,stringsAsFactors = F) 
      }  # end if(spatial.maps[k] %in% c("Clappers"))
    # If it is not Clapper spatial and not Middle bank do the following
    if((spatial.maps[k] == c("Clap-spatial") && banks[i] == "Mid")==F)
      {
      
    ### Now we move to the figure....  
    # Do we want to save the figure to a file or just output to the screen?  
    if(fig == "file")
      {
        png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",spatial.maps[k],".png",sep=""),units="in",width = 11,
            height = 8.5,res=420,bg = "transparent")
      } # end if(fig == "file")
    if(fig == "screen") windows(11,8.5)
        par(mfrow=c(1,1))
        
    # Where to get the bathymetry data, note this overwrites any call in the function above
    ifelse(banks[i] %in% c("Mid","Sab","Ban"), bath <- "quick", bath <- "usgs")
    ifelse(banks[i] %in% c("Mid","Sab","Ban"), iso <- c(seq(50,200,by=50)), iso <- c(seq(40,140,by=20)))
    # This is one figure to rule all the contours!
    ScallopMap(banks[i],contour=list(Cont.poly,cont.data),title=fig.title,
               bathy.source=bath,isobath = iso,plot.bathy=T,plot.boundries=T,boundries="offshore",
               direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
    # Add the regular survey tows.
    
    if(spatial.maps[k] != "MC-spatial" && spatial.maps[k] != "CF-spatial")
      {
        points(lat~lon,surv.Live[[banks[i]]],subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
      # On Georges and BBn we have exploratory tows, no other bank has those currently.
      if(banks[i] %in% c("GBa","BBn"))  
        {
          
          points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
                 & state =='live' & random==5,pch=24,bg="darkorange",cex=0.8)
          legend("topright",legend = c(paste('exploratory (n =',
                                             length(unique(subset(surv.Live[[banks[i]]],year==yr & random==5)$tow)),
                                             ")",sep=""),
                                       paste('regular (n =',
                                             length(unique(subset(surv.Live[[banks[i]]],year==yr & random==1)$tow)),
                                             ")",sep="")),title="Tow type",
                 pt.bg = c("darkorange","black"),pch=c(24,20),bg = "white",inset=0.01,box.col="white")
        } # end if(banks[i] %in% c("GBa","BBn"))  
      
      # Add the legends, for German we also have to add the "matched tows"
      if(banks[i] == "Ger") 
        {
          points(lat~lon,surv.Live[[banks[i]]],subset=year==yr &
                   random==3,pch=22,bg="yellow",cex=0.8)
          legend('topleft',legend=
                   c(paste('regular (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                                 random==1)$tow)),")", sep=""),
                     paste('repeated (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                                  random==3)$tow)),")", sep="")),
                 pch=c(20,22), pt.bg = c("black","yellow"),bty='n',cex=1, inset = .02)
        } # end if(banks[i] == "Ger") 
      
      # For the banks without exploratory tows we add this legend.
      if( banks[i] == "BBs" || banks[i] == "Ban") 
        {
          legend("left",pch=c(20), pt.bg = c("black"), title="Tow type",
              legend = paste('regular (n =',length(unique(subset(surv.Live[[banks[i]]],
               year==yr & random==1)$tow)),")",sep=""), inset=0.01,bg=NA,box.col=NA)	
        } # end if(banks[i] == "Sab" || banks[i] == "Mid"|| banks[i] == "BBs" || banks[i] == "Ban"|| banks[i] == "GBb" || banks[i] == "GB") 
      
      if(banks[i] == "GB") 
      {
        points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
               & state =='live' & random%in% c(2,4,5),pch=24,bg="darkorange",cex=0.8)
        points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
               & state =='live' & random==3,pch=22,bg="yellow",cex=0.8)
        
        legend("topright",legend = c(paste('exploratory (n =',
                                           length(unique(subset(surv.Live[[banks[i]]],year==yr & random %in% c(2,4,5))$tow)),")",sep=""),
                                     paste('repeated (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                                                  random==3)$tow)),")", sep="")),title="Tow type",
               pt.bg = c("darkorange","yellow"),pch=c(24,22),bg = "white",inset=0.01,box.col="white")
      } # end if(banks[i] == "Sab" || banks[i] == "Mid"|| banks[i] == "BBs" || banks[i] == "Ban"|| banks[i] == "GBb" || banks[i] == "GB") 
      
      
      # Sable needs to be in the bottom right
      if(banks[i] == "Sab" || banks[i] == "Mid" || banks[i] == "GBb" ) 
        {
          legend("bottomright",pch=c(20), pt.bg = c("black"), title="Tow type",
                 legend = paste('regular (n =',length(unique(subset(surv.Live[[banks[i]]],
                                                                    year==yr & random==1)$tow)),")",sep=""), inset=0.01,bg=NA,box.col=NA)	
        } # end if(banks[i] == "Sab" || banks[i] == "Mid"|| banks[i] == "BBs" || banks[i] == "Ban"|| banks[i] == "GBb" || banks[i] == "GB") 
        
      
      # For these plots the legend goes like this     
      if(spatial.maps[k] %in% c("PR-spatial", "Rec-spatial", "FR-spatial"))
        {
          legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
                              paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,
               title=leg.title, title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
               pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
        } # END if(spatial.maps[k] %in% c("Pre-recruits", "Recruits", "Fully_Recruited","Clappers"))
      
      # For these plots the legend goes like this     
      if(spatial.maps[k] %in% c("Clap-spatial") && banks[i] != "Mid")
        {
          legend("bottomleft",c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),
                                paste(lvls[length(lvls)],'+',sep='')),fill=cont.data$col,
                 title=leg.title, title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
                 pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg='white',box.col='white')
        } # END if(spatial.maps[k] %in% c("Pre-recruits", "Recruits", "Fully_Recruited","Clappers"))
      } # end if(spatial.maps[k] != "MC-spatial" && spatial.maps[k] != "CF-spatial")

      
    # For condition and meat count we set things up a little bit differently.
    if(spatial.maps[k] %in% c("CF-spatial","MC-spatial"))
      {
        points(lat~lon,CF.current[[banks[i]]],pch=21,bg='grey50',cex=0.8)
        legend("bottomright",pch=c(21), pt.bg = c("grey50"), title="Tow type",
             legend = paste('Detailed Sampling (n =',length(CF.current[[banks[i]]]$tow),")",sep=""), inset=0.01,bg=NA,box.col=NA)	
        legend("bottomleft",c(paste(dat[-length(dat)],'-',dat[-1],sep=''),
                          paste(dat[length(dat)],'+',sep='')),fill=cont.data$col[cont.data$lvls %in% dat],
           title=leg.title, title.adj = 0.2,border="black",pch=c(rep(NA,length(dat))),
           pt.bg = c(rep(NA,length(dat))),inset=0.01,bg='white',box.col='white')
      } # end if(spatial.maps[k] %in% c("Condition","Meat Count"))
    # Add the survey boxes if they exist.
    if(length(sb[,1]) > 0) addPolys(sb,lty=2,lwd=2)
    # If saving as a png turn off the plot device
    if(fig == "file") dev.off()
  
    }   # end    if(length(spatial.maps > 0))
      } # end the if to plot if we are looking at spatial clappers and middle bank...
  } # end the k for loop

####################################  END SPATIAL PLOTS ####################################  END SPATIAL PLOTS
####################################  END SPATIAL PLOTS ####################################  END SPATIAL PLOTS

      
      
      
####################################  MWSH and CF Time series plot #################################### 
####################################  MWSH and CF Time series plot ####################################       
####################################  MWSH and CF Time series plot #################################### 
      if(any(plots=="MW-SH"))
        {
        MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-",year,")",sep="")),
                                 list(year=as.character(yr),bank=banks[i]))
        CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank,")",sep="")),
                                  list(year=as.character(yr),bank=banks[i]))
        if(banks[i] == "GB")
          {
           MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-Spr-",year,")",sep="")),
                                   list(year=as.character(yr),bank=banks[i]))
           CF.ts.title <- substitute(bold(paste("Condition factor (",bank,-"Spr)",sep="")),
                                    list(year=as.character(yr),bank=banks[i]))
          } # end if(banks[i] == "GB")
          
          
        # Don't add the titles
        if(add.title == F) MWSH.title <- ""
        if(add.title == F) CF.ts.title <- ""
        
        ############
      #Source12 Meat Height Shell weight plot on Slide 13  source("fn/shwt.plt1.r") 
        if(fig == "screen") windows(15,8)
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/MWSH_and_CF_ts.png",sep=""),
                              units="in",width = 13,height = 8.5,res=420,bg = "transparent")
      par(mfrow=c(1,2))
      shwt.plt1(SpatHtWt.fit[[banks[i]]],lw=3,ht=10,wd=12,cx=1.5,titl = MWSH.title,cex.mn = 2,las=1)
      
      # now the condition factor figure..
      if(banks[i] != "Ger" && banks[i] != "GBa" && banks[i] != "GB")
        {
          stdts.plt(survey.obj[[banks[i]]][[1]],x=c('year'),y=c('CF'),pch=16,ylab=cf.lab,las=1,col=c("blue"),
                mean.line=T,graphic='none',xlab='Year',ylim=c(4,25),titl=CF.ts.title,cex.mn=2)
        }
      
      # DK Note that German is still calculated the "old way" using the cf.data at a specific location
      # There is no model for german and we are only interested in relative CF here so this is not 
      # A problem (i.e. this is just for comparitive purposes) but when we have some time this should be fixed.
      if(banks[i] == "Ger")
        {
          stdts.plt(cf.data[[banks[i]]]$CFyrs,y=c('CF','CF2'),pch=c(23,24),col=c('blue','red'),ylab=cf.lab,
                  mean.line=T,graphic='none',xlab='Year',ylim=c(4,25),las=1,titl = CF.ts.title,cex.mn=2,tx.ypos=4)
          legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
        }
      # Have to add in the CF for May into the data
      if(banks[i] == "GBa")
        {
          stdts.plt(survey.obj[[banks[i]]][[1]],x=c('year'),y=c('CF'),pch=16,ylab=cf.lab,
                mean.line=T,graphic='none',xlab='Year',ylim=c(4,25),titl = CF.ts.title,cex.mn=2,las=1)
          if(season=="both")
          {
            points(survey.obj[["GB"]][[1]]$year-0.25,survey.obj[["GB"]][[1]]$CF,col="red", lty=2,pch=22,type="o",bg="red")
            abline(h=mean(survey.obj[["GB"]][[1]]$CF,na.rm=T),col="red",lty=3)
          } # end if(season="both")
        if(season=="summer")
        {
          lines(survey.obj.last[["GB"]][[1]]$year-0.25,survey.obj[["GB"]][[1]]$CF,col="red", lty=2)
          abline(h=mean(survey.obj.last[["GB"]][[1]]$CF,na.rm=T),col="red",lty=3)
        } # end if(season="both")
            legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"),pt.bg=c("blue","red"))		
        } # end if(banks[i] == "GBa")
      # Here I'm adding in the cf for August into the spring survey data.
      if(banks[i] == "GB")
        {
          stdts.plt(survey.obj[[banks[i]]][[1]],x=c('year'),y=c('CF'),pch=22,ylab=cf.lab,col="red",lty=2,
                    mean.line=T,graphic='none',xlab='Year',ylim=c(4,25),titl = CF.ts.title,cex.mn=2,las=1)
        if(season=="both")
        {
          points(survey.obj[["GBa"]][[1]]$year+0.25,survey.obj[["GBa"]][[1]]$CF,col="blue", lty=1, pch=16,type="o")
          abline(h=mean(survey.obj[["GBa"]][[1]]$CF,na.rm=T),col="blue",lty=3)
        }
        if(season=="spring")
        {
          points(survey.obj.last[["GBa"]][[1]]$year-0.25,survey.obj.last[["GBa"]][[1]]$CF,col="blue", lty=1, pch=16,type="o")
          abline(h=mean(survey.obj.last[["GBa"]][[1]]$CF,na.rm=T),col="blue",lty=3)
        }
          legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,22),bty='n',inset=0.02,col=c("blue","red"),pt.bg=c("blue","red"))	
          
        } # end if(banks[i] == "GB")
      #legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
      if(fig == "file") dev.off()
  
        } # end if(any(plots=="MW-SH"))
############  END THE MW SHELL HEIGHT FIGURE #######      ############  END THE MW SHELL HEIGHT FIGURE #######
############  END THE MW SHELL HEIGHT FIGURE ###################  END THE MW SHELL HEIGHT FIGURE #######

      
#####   THE ABUNDANCE TIME SERIES FIGURE #####   THE ABUNDANCE TIME SERIES FIGURE#####   THE ABUNDANCE TIME SERIES FIGURE
#####   THE ABUNDANCE TIME SERIES FIGURE #####   THE ABUNDANCE TIME SERIES FIGURE#####   THE ABUNDANCE TIME SERIES FIGURE      
      
    if(any(plots=="abund-ts"))
      {
        survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
                                        list(year=as.character(yr),bank=banks[i]))
        if(banks[i] == "GB") survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,"-Spr)",sep="")),
                                                             list(year=as.character(yr),bank=banks[i]))
       
        
        if(add.title == F) survey.ts.N.title <- ""
        if(fig == "screen") windows(11,8.5)
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/abundance_ts.png",sep=""),units="in",
                              width = 8.5, height = 11,res=420,bg="transparent")
        par(mfrow=c(1,1))
        if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "GB")
          {
          survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F, RS=RS, CS=CS,
                    areas=surv.info$towable_area,ys=.99,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                    add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
          }# end if(banks[i] != "Ger" && banks[i] != "Mid")
        # For german bank
        if(banks[i] == "Ger")
          {
            survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F, CS=CS, RS=RS,
                      ymin=-5,dat2=merged.survey.obj,clr=c('blue','red',"blue"),pch=c(16,17),se=T,yl2=400,
                  add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
            legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
          } # end if(banks[i] == "Ger")
          if(banks[i] == "Mid" || banks[i] == "GB")
          {
            survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F, RS=RS,
                  CS=CS,ys=.8,ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl =survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
          } # end if(banks[i] == "Mid")
          
        if(fig == "file") dev.off()
        
      }# end if(any(plots=="abund-ts"))
      
#####  END THE ABUNDANCE TIME SERIES FIGURE #####  END THE ABUNDANCE TIME SERIES FIGURE#####  END THE ABUNDANCE TIME SERIES FIGURE
#####  END THE ABUNDANCE TIME SERIES FIGURE #####  END THE ABUNDANCE TIME SERIES FIGURE#####  END THE ABUNDANCE TIME SERIES FIGURE

      
#####   THE BIOMASS TIME SERIES FIGURE #####   THE BIOMASS TIME SERIES FIGURE#####   THE BIOMASS TIME SERIES FIGURE
#####   THE BIOMASS TIME SERIES FIGURE #####   THE BIOMASS TIME SERIES FIGURE#####   THE BIOMASS TIME SERIES FIGURE      
      
      if(any(plots =="biomass-ts"))
      {
      
      survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
                                       list(bank=banks[i]))
      if(banks[i] == "GB") survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,"-Spr)",sep="")),
                                                           list(year=as.character(yr),bank=banks[i]))
      if(add.title == F) survey.ts.BM.title <- ""
      
      if(fig == "screen") windows(11,8.5)
      if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/biomass_ts.png",sep=""),
                            units="in",width = 8.5, height = 11,res=420,bg="transparent")
          
      if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "GB")
        {
          survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,type='B', 
                RS=RS, CS=CS,areas=surv.info$towable_area,ys=.99,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
        } # end if(banks[i] != "Ger")
      if(banks[i] == "Ger")
        {
          survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F,type='B', CS=CS,
                    RS=RS,dat2=merged.survey.obj,clr=c('blue','red',"blue"),se=T,pch=c(16,17),
                add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx=1.5)
          legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
        } # end if(banks[i] == "Ger")
      if(banks[i] == "Mid"|| banks[i] == "GB")
      {
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F, RS=RS, type='B',
            CS=CS,ys=.8,ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
      } # end if(banks[i] == "Mid")
      if(fig == "file") dev.off()
      } # end if(any(plots=="biomass-ts"))
        
        
#####   END THE BIOMASS TIME SERIES FIGURE #####   END THE BIOMASS TIME SERIES FIGURE#####   END THE BIOMASS TIME SERIES FIGURE
#####   END THE BIOMASS TIME SERIES FIGURE #####   END THE BIOMASS TIME SERIES FIGURE#####   END THE BIOMASS TIME SERIES FIGURE      


##### Shell height frequency figure            ##### Shell height frequency figure ##### Shell height frequency figure
##### Shell height frequency figure            ##### Shell height frequency figure ##### Shell height frequency figure      
##### Shell height frequency figure            ##### Shell height frequency figure ##### Shell height frequency figure

  if(any(plots=="SHF"))
    {
    SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
                             list(bank=banks[i]))  
    if(banks[i] == "GB")  SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,"-Spr)",sep="")),
                                                   list(bank=banks[i])) 
    if(add.title == F) SHF.title <- ""
    if(fig == "screen") windows(8.5,11)
    if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/SHF.png",sep=""),units="in",width = 8.5, 
                          height = 11,res=420,bg="transparent")
    # Grab the last 7 years of data
    
    if(banks[i] != "Ger")
      {
        shf.years <- survey.obj[[banks[i]]][[1]]$year[(length(survey.obj[[banks[i]]][[1]]$year)-6):
                                                        length(survey.obj[[banks[i]]][[1]]$year)]
        s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
        shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
            recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
        if(fig == "file") dev.off()
      } # end  if(banks[i] != "Ger")
    
    if(banks[i]=="Ger")
      {
        shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
        s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
        shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
                recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
        if(fig == "file") dev.off()
        # We also want to grab the matched tows figure
        s.size <- matched.survey.obj[[1]]$n
        
        if(fig == "screen") windows(8.5,11)
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/shell_height_freqency-repeated.png",sep=""),
                               units="in",width = 11, height = 8.5,res=420,bg="transparent")
        if(add.title == F) SHFm.title <- ""
        if(add.title == T) SHFm.title <- "SHF - Repeated Tows (Ger)"
        shf.plt(matched.survey.obj,from='surv',yr=(yr-1):yr,col1='grey80',col2=1,rel=F,rows=2,
                recline=c(RS,CS),add.title = T,titl = SHFm.title,cex.mn=2, sample.size = s.size)	   
        if(fig == "file") dev.off()
      } # end if(banks[i]=="Ger")
    } # end if(any(plots=="SHF"))    
                 
##### END Shell height frequency figure   ##### END Shell height frequency figure ##### END Shell height frequency figure
##### END Shell height frequency figure   ##### END Shell height frequency figure ##### END Shell height frequency figure
      
  
  
  
  ##### Shell height frequency Large Sizes      ##### Shell height frequency Large Sizes ##### Shell height frequency Large Sizes 
  ##### Shell height frequency Large Sizes      ##### Shell height frequency Large Sizes ##### Shell height frequency Large Sizes   
  
  if(any(plots=="SHF-large"))
  {
    SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
                             list(bank=banks[i]))  
    if(banks[i] == "GB")  SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,"-Spr)",sep="")),
                                                   list(bank=banks[i])) 
    if(add.title == F) SHF.title <- ""
    if(fig == "screen") windows(8.5,11)
    if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/SHF-large.png",sep=""),units="in",width = 8.5, 
                          height = 11,res=420,bg="transparent")
    # Grab the last 7 years of data
    
    if(banks[i] != "Ger")
    {
      shf.years <- survey.obj[[banks[i]]][[1]]$year[(length(survey.obj[[banks[i]]][[1]]$year)-6):
                                                      length(survey.obj[[banks[i]]][[1]]$year)]
      s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
      shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F, select=70,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
      if(fig == "file") dev.off()
    } # end  if(banks[i] != "Ger")
    
    if(banks[i]=="Ger")
    {
      shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
      s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
      shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,select=70,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
      if(fig == "file") dev.off()
    } # end if(banks[i]=="Ger")
  } # end if(any(plots=="SHF-large"))    
  
  ##### END Shell height frequency split figure   ##### END Shell height splitfrequency figure ## END Shell height frequency split figure
  ##### END Shell height frequency split figure   ##### END Shell height frequency splitfigure ##### END Shell height frequency split figure
  
  ##### Shell height frequency split Sizes      ##### Shell height frequency split Sizes ##### Shell height frequency split Sizes 
  ##### Shell height frequency split Sizes      ##### Shell height frequency split Sizes ##### Shell height frequency split Sizes   
  
  if(any(plots=="SHF-split"))
  {
    SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
                             list(bank=banks[i]))  
    if(banks[i] == "GB")  SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,"-Spr)",sep="")),
                                                   list(bank=banks[i])) 
    if(add.title == F) SHF.title <- ""
    if(fig == "screen") windows(8.5,11)
    if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/SHF-split.png",sep=""),units="in",width = 8.5, 
                          height = 11,res=420,bg="transparent")
    # Grab the last 7 years of data
    
    if(banks[i] != "Ger")
    {
      shf.years <- survey.obj[[banks[i]]][[1]]$year[(length(survey.obj[[banks[i]]][[1]]$year)-6):
                                                      length(survey.obj[[banks[i]]][[1]]$year)]
      s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
      shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F, split=60,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
      if(fig == "file") dev.off()
    } # end  if(banks[i] != "Ger")
    
    if(banks[i]=="Ger")
    {
      shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
      s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
      shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F, split=60,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
      if(fig == "file") dev.off()
    } # end if(banks[i]=="Ger")
  } # end if(any(plots=="SHF-split"))    
  
  ##### END Shell height frequency Large figure   ##### END Shell height Largefrequency figure ## END Shell height frequency Large figure
  ##### END Shell height frequency Large figure   ##### END Shell height frequency Largefigure ##### END Shell height frequency Large figure
  
      
##### Clapper abundance time series      ##### Clapper abundance time series##### Clapper abundance time series
##### Clapper abundance time series ##### Clapper abundance time series ##### Clapper abundance time series
      
  if(any(plots== "clapper-abund-ts"))
    {
      
      clap.abund.ts.title <- substitute(bold(paste("Clapper abundance time series (",bank,")",sep="")),
                                                   list(bank=banks[i]))
      if(banks[i] == "GB")  clap.abund.ts.title <- substitute(bold(paste("Clapper abundance time series (",bank,"-Spr)",sep="")),
                                                              list(bank=banks[i]))
      if(add.title == F) clap.abund.ts.title <- ""
      if(fig == "screen") windows(8.5,11)
      if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/Clapper_abund_ts.png",sep=""),
                            units="in",width = 8.5, height = 11,res=420,bg = "transparent")
      if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "GB")
        {
       yrs <- min(clap.survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(clap.survey.obj[[banks[i]]][[1]]$year,na.rm=T)
       survey.ts(clap.survey.obj[[banks[i]]][[1]], Bank=bank[i],pdf=F, RS=RS, CS=CS, years=yrs,
               titl = clap.abund.ts.title,add.title=T, cx.mn=3,areas=strata.areas$towable_area,
               ys=.8,ht=7,wd=10,clr=c('blue',"blue","darkgrey"),se=T,pch=16, plots=c("pre",'rec','com'))
        } # if(banks[i] != "Ger" && banks[i] != "Mid")
                 
      if(banks[i] == "Ger" || banks[i] == "Mid" || banks[i] == "GB")
        {
          
          yrs <- min(surv.Clap.Rand[[banks[i]]]$year,na.rm=T):max(surv.Clap.Rand[[banks[i]]]$year,na.rm=T)
          survey.ts(clap.survey.obj[[banks[i]]][[1]],Bank=bank[i],pdf=F, RS=RS, CS=CS,
                  titl = clap.abund.ts.title,add.title=T, cx.mn=3, years=yrs,
                  ys=.8,ht=7,wd=10,clr=c('blue',"blue","darkgrey"),se=T,pch=16, plots=c("pre",'rec','com'))
        } # end if(banks[i] == "Ger" || banks[i] == "Mid")
      if(fig == "file") dev.off()                 
    } # end  if(any(plots== "clapper-abund-ts"))  

##### END Clapper abundance time series      ##### END Clapper abundance time series##### END Clapper abundance time series
##### END Clapper abundance time series ##### END Clapper abundance time series ##### END Clapper abundance time series      
            

#####  Clapper % time series      #####  Clapper % time series#####  Clapper % time series
#####  Clapper % time series #####  Clapper % time series #####  Clapper % time series      
      
              
  if(any(plots== "clapper-per-ts"))
    {
      clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,")",sep="")),
                                    list(bank=banks[i]))
      if(banks[i] == "GB")  clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,"-Spr)",sep="")),
                                                            list(bank=banks[i]))
      if(add.title == F) clap.per.ts.title <- ""
      
      if(fig == "screen") windows(8.5,11)
      if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/Clapper_per_ts.png",sep=""),units="in",width = 8.5, 
          height = 11,res=420,bg = "transparent")
      yrs <- min(surv.Clap.Rand[[banks[i]]]$year,na.rm=T):max(surv.Clap.Rand[[banks[i]]]$year,na.rm=T)
      Clap3.plt(surv.Clap.Rand[[banks[i]]],years=yrs,add.title = T,cex.mn = 3, mean.line=T,
                titl = clap.per.ts.title ,CS=CS,RS=RS,axis.cx = 1.5)
      if(fig == "file") dev.off()                 
    } # end if(any(plots== "clapper-per-ts"))   
    
##### END Clapper % time series      ##### END Clapper % time series##### END Clapper % time series
##### END Clapper % time series ##### END Clapper % time series ##### END Clapper % time series      
      

##### Shell height, Meat weight, condition factor times series ##### Shell height, Meat weight, condition factor times series             
##### Shell height, Meat weight, condition factor times series ##### Shell height, Meat weight, condition factor times series
      
      if(any(plots== "SH-MW-CF-ts"))
        {
          # This only works for the banks we have thse data for...
          if(banks[i] %in% c("BBn" ,"BBs", "Sab" ,  "GBb", "GBa"))
            {
          if(fig == "screen") windows(11,8.5)
          if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/SH_MW_CF_ts.png",sep=""),units="in",
                                width = 8.5,height = 11,res=420,bg = "transparent")
          par(mfrow=c(3,1),omi=c(0.3,0.6,0.3,0.2))
          yrs <- min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(survey.obj[[banks[i]]][[1]]$year,na.rm=T)
          yrs2 <-min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(survey.obj[[banks[i]]][[1]]$year,na.rm=T)
          yrs <- min(yrs,yrs2):max(yrs,yrs2)
          # This fills the missing years with NA's so the plot looks better...
          tmp <- as.data.frame(cbind(yrs,matrix(NA,nrow=length(yrs),ncol=ncol(survey.obj[[banks[i]]][[1]][,-1]))))
          tmp[tmp$yrs %in% survey.obj[[banks[i]]][[1]]$year,2:ncol(survey.obj[[banks[i]]][[1]])] <- survey.obj[[banks[i]]][[1]][,-1]
          names(tmp) <- names(survey.obj[[banks[i]]][[1]])
          
          stdts.plt(subset(tmp,year %in% yrs),y="l.bar",pch=17,lty=1,ylab="Average\n shell\n height\n (mm)",las=1,
                    mean.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
          stdts.plt(subset(survey.obj[[banks[i]]][[1]],year %in% yrs),y="CF",pch=17,lty=1,las=1, ylab=cf.lab,
                    mean.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
          stdts.plt(subset(tmp,year %in% yrs),y="w.bar",pch=17,lty=1,ylab="Average\n meat\n weight\n(g)",
                    mean.line=T,graphic='none',xlab='',labcx=1.2,las=1,axis.cx=1.2)
          if(add.title ==T) title(paste("Shell height, Condition factor, Meat weight (",banks[i],")",sep=""), cex.main=3,outer=T)
          if(fig == "file") dev.off()   
            } # end if(banks[i] %in% c("BBn" ,"BBs", "Sab" ,  "GBb", "GBa"))
          
         }  # end if(any(plots== "SH-MW-CF-ts"))
        

  
############  Breakdown figures for BBn and GB############  Breakdown figures for BBn and GB############  Breakdown figures for BBn and GB
############  Breakdown figures for BBn and GB############  Breakdown figures for BBn and GB############  Breakdown figures for BBn and GB  
  
  if(any(plots== "breakdown"))
  {
    # This only works for the banks we have thse data for...
    #if(banks[i] %in% c("BBn" , "GBb", "GBa","GB"))
    #{
      if(fig == "screen") windows(11,8.5)
      if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/breakdown.png",sep=""),units="in",
                            width = 11,height = 8.5,res=420,bg = "transparent")
      
      if(banks[i] != "GB") mc <- subset(fish.reg, year == yr & Bank %in% banks[i])$MC_reg
      if(banks[i] == "GB") mc <- fish.reg$MC_reg[fish.reg$Bank == "GBa"]
      if(banks[i] %in% c("Sab","Mid","BBs")) breakdown(survey.obj[[banks[i]]],yr=yr,mc=mc,cx.axs=1,add.title = F)
      if(banks[i] %in% c("Ger")) breakdown(lined.survey.obj,yr=yr,mc=mc,cx.axs=1,y1max = 0.3,add.title = F)
      if(banks[i] %in% c("GBa","GBb","GB","BBn")) breakdown(survey.obj[[banks[i]]],yr=yr,mc=mc,cx.axs=1,y1max = 1.2,add.title = F)
      if(add.title ==T) title(paste("Biomass & Meat Count by Height (",banks[i],"-",yr,")",sep=""), cex.main=2,adj=0.35)
      if(fig == "file") dev.off()   
    #} # end if(banks[i] %in% c("BBn" , "GBb", "GBa"))
    
  }  # end iif(any(plots== "breakdown"))
############  End Breakdown figures for BBn and GBa############  End Breakdown figures for BBn and GBa############  
############  End Breakdown figures for BBn and GBa############  End Breakdown figures for BBn and GBa############  

  
  
  
  
############  Seedboxes###########  Seedboxes ############  Seedboxes ############  Seedboxes ############  Seedboxes ############  
############  Seedboxes###########  Seedboxes ############  Seedboxes ############  Seedboxes ############  Seedboxes ############  
############  Seedboxes###########  Seedboxes ############  Seedboxes ############  Seedboxes ############  Seedboxes ############  
  
  
  if(any(plots %in% "seedboxes"))    
  {
    sb <- subset(seedboxes,Bank == banks[i] & Open >= paste(yr,"-01-01",sep=""))
    if(nrow(sb) > 0) # only run the rest of this if we have data...
    {
      bound.poly.surv <- subset(survey.bound.polys,label==banks[i]) 
      attr(bound.poly.surv,"projection")<-"LL"
      n.box <- length(seedbox.obj[[banks[i]]])
      boxes <- as.PolySet(sb,projection = "LL")
      box.dat <- data.frame(EID=1:nrow(surv.Live[[banks[i]]]),X=surv.Live[[banks[i]]]$lon,Y=surv.Live[[banks[i]]]$lat)
      box.names <- unique(boxes$SCALLOP_Group_ID)
      fig.box.name <- unique(boxes$Common_name)
      
      
      
      for(j in 1:n.box)
      {
        # Get the data for the box of interest
        key <-findPolys(box.dat, subset(boxes,SCALLOP_Group_ID == box.names[j]))  
        this.box <- subset(boxes,SCALLOP_Group_ID == box.names[j])
        boxy <- seedbox.obj[[banks[i]]][[j]]
        surv.seed <- surv.Live[[banks[i]]][1:nrow(surv.Live[[banks[i]]]) %in% key$EID,]
        
        # Titles for the seedbox plots....
        seedbox.bm.title <- substitute(bold(paste(box,"-Seedbox Biomass time series (",bank,")",sep="")),
                                       list(year=as.character(yr),bank=banks[i],box = fig.box.name[j]))
        seedbox.abund.title <-substitute(bold(paste(box,"-Seedbox Abundance time series (",bank,")",sep="")),
                                         list(year=as.character(yr),bank=banks[i],box = fig.box.name[j]))
        seedbox.SHF.title <- substitute(bold(paste(box,"-Seedbox Shell Height Frequency (",bank,")",sep="")),
                                        list(year=as.character(yr),bank=banks[i],box = fig.box.name[j]))
        pre.title.seed <-substitute(bold(paste("Pre-recruit scallops (" ,""<a, " mm)",sep="")),
                                    list(a=as.character(RS),year=as.character(yr),bank=banks[i]))
        rec.title.seed <- substitute(bold(paste("Recruit scallops (",b- a, " mm)",sep="")),
                                     list(a=as.character(CS-1),b=as.character(RS),year=as.character(yr),bank=banks[i]))
        fr.title.seed <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm)",sep="")),
                                    list(a=as.character(CS),year=as.character(yr),bank=banks[i]))
        breakdown.title.seed <- substitute(bold(paste("Biomass & Meat Count by Height (",fn,"-",bank,"-",year,")",sep="")),
                                           list(fn = fig.box.name[j],year=as.character(yr),bank=banks[i]))
        
        # Make the abundance time series for the box
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-abundance_ts.png",sep=""),units="in",
            width = 8.5, height = 11,res=420,bg = "transparent") 
        if(fig == "screen") windows(8.5,11)
        survey.ts(boxy$model.dat,min(boxy$model.dat$year,na.rm=T):yr,pdf=F, RS=RS, CS=CS,Npt=T,
                  areas=NULL,ys=.99,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                  add.title = add.title,titl = seedbox.abund.title,cx.mn=3,axis.cx = 1.5)
        if(fig == "file") dev.off()
        
        # Make the biomass time series for the box
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-biomass_ts.png",sep=""),units="in",
            width = 8.5, height = 11,res=420,bg = "transparent") 
        if(fig == "screen") windows(8.5,11)
        
        survey.ts(boxy$model.dat,min(boxy$model.dat$year,na.rm=T):yr,pdf=F, RS=RS, CS=CS,Npt=T,type="B",
                  areas=NULL,ys=.99,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                  add.title = add.title,titl = seedbox.bm.title,cx.mn=3,axis.cx = 1.5)
        if(fig == "file") dev.off()
        
        # A seedbox breakdown figure, this would include all tows not just proper tows...
        if(fig == "screen") windows(11,8.5)
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-breakdown.png",sep=""),units="in",
                              width = 11,height = 8.5,res=420,bg = "transparent")
        
        mc <- fish.reg$MC_reg[fish.reg$Bank == banks[i]]
        breakdown(boxy,yr=yr,mc=mc,cx.axs=1,add.title="F",y1max=2,
                  CS = survey.obj[[banks[i]]][[1]]$CS[length(survey.obj[[banks[i]]][[1]]$CS)],
                  RS = survey.obj[[banks[i]]][[1]]$RS[length(survey.obj[[banks[i]]][[1]]$RS)])
        if(add.title==T) title(breakdown.title.seed, cex.main=2,adj=0.35)
        if(fig == "file") dev.off()   
        
        
        # Now the Shell height frequency plots.
        shf.years <- boxy$model.dat$year[(length(boxy$model.dat$year)-6):
                                 length(boxy$model.dat$year)]
        s.size <- boxy$model.dat$n[boxy$model.dat$year %in% shf.years]
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-SHF.png",sep=""),units="in",
            width = 8.5, height = 11,res=420,bg = "transparent") 
        if(fig == "screen") windows(8.5,11)
        shf.plt(boxy,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
                recline=c(RS,CS),add.title = add.title,titl = seedbox.SHF.title,cex.mn=3,sample.size = s.size)	
        if(fig == "file") dev.off()
        # Now zoom in on the larger size classes...
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-SHF-large.png",sep=""),units="in",
                              width = 8.5, height = 11,res=420,bg = "transparent") 
        if(fig == "screen") windows(8.5,11)
        shf.plt(boxy,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,select=60,
                recline=c(RS,CS),add.title = add.title,titl = seedbox.SHF.title,cex.mn=3,sample.size = s.size)
        if(fig == "file") dev.off()
        # Now do the SHF with the panels split classes...
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-SHF-split.png",sep=""),units="in",
                              width = 8.5, height = 11,res=420,bg = "transparent") 
        if(fig == "screen") windows(8.5,11)
        shf.plt(boxy,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,split=70,
                recline=c(RS,CS),add.title = add.title,titl = seedbox.SHF.title,cex.mn=3,sample.size = s.size)
        if(fig == "file") dev.off()
        
        
        
        # Finally the spatial abundance figure.  
        if(fig == "file") png(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",box.names[j],"-spatial.png",sep=""),units="in",
            width = 11, height = 8.5,res=420,bg = "transparent")
        if(fig == "screen") windows(11,8.5)
        
        par(mfrow=c(2,2),omi=c(0.1,0.2,0.5,0.5),xpd=F)
        
        # Loop through each size category
        for(b in 1:3) 
        {
          if(b == 1) 
          {
            con <-  pre.contours[[banks[i]]]
            fig.title <- pre.title.seed 
          } # end  if(b == 1)
          if(b == 2) 
          {
            con <-  rec.contours[[banks[i]]] 
            fig.title <- rec.title.seed 
          } # end  if(b == 2)
          if(b == 3)
          {
            con <-  com.contours[[banks[i]]]
            fig.title <- fr.title.seed 
          } # end  if(b==3)
          # If counts are high enough to need lvls2 we use it, if not we just use lvls1.
          lvls1=c(1,5,10,50,100,500,1000)
          CL <- contourLines(con$image.dat,levels=lvls1)
          CP <- convCP(CL)
          Cont1.poly <- joinPolys(CP$PolySet,bound.poly.surv)
          cont1.data <- data.frame(PID=1:length(lvls1),col=rev(magma(length(lvls1),alpha=1,begin=0.6,end=1)),border=NA,stringsAsFactors = F) 
          lvls2=c(2000,5000,10000,20000,50000)
          plt.lvls <- c(lvls1,lvls2)
          plt.colors <- data.frame(PID=1:length(plt.lvls),col=c(cont1.data$col,
                          rev(magma(length(lvls2),alpha=1.0,begin=0.1,end=0.5))),border=NA,stringsAsFactors = F)
          
                                                    
          # If counts are high enough to need lvls2 we use it, if not we just use lvls1.
          #cont1.data<- data.frame(PID=1:length(lvls1),col=brewer.pal(length(lvls1),N.col),border=NA,stringsAsFactors = F) 
          if(max(con$image.dat$z)>=lvls2[1])
          {
            CL <- contourLines(con$image.dat,levels=lvls2)
            CP <- convCP(CL)
            Cont2.poly <- joinPolys(CP$PolySet,bound.poly.surv)
            Cont2.poly$PID<-Cont2.poly$PID+length(lvls1)
            Cont.poly<-rbind(Cont1.poly,Cont2.poly)
            lvls<-c(lvls1,lvls2)
            #cont2.data<- data.frame(PID=(length(lvls1)+1):(length(lvls1)+length(lvls2)),
            #                           col=rev(brewer.pal(8,X.lvl))[(1:length(lvls2)+1)],border=NA,stringsAsFactors = F)
            #cont.data <-rbind(cont1.data,cont2.data)
            cont.data <- data.frame(PID=1:length(lvls),col=c(cont1.data$col,
                          rev(magma(length(lvls2),alpha=1.0,begin=0.1,end=0.5))),border=NA,stringsAsFactors = F)
               } # end if(max(rec.contours$image.dat$z)>=lvls2[1])
          if(max(con$image.dat$z)<lvls2[1])
          {
            Cont.poly<-Cont1.poly
            cont.data<-cont1.data
            lvls <- lvls1
          } # end if(max(rec.contours$image.dat$z)<lvls2[1])
          
          # and make the map for each size category.
          ScallopMap(ylim=c(min(this.box$Y),max(this.box$Y)),xlim=c(min(this.box$X),max(this.box$X)),bathy.source="usgs",
                     isobath = c(seq(40,140,by=20)),contour=list(Cont.poly,cont.data),plot.bathy = T,plot.boundries = T,direct=direct,
                     title=fig.title,dec.deg = F,ylab="",xlab="",cex.mn=1.3)
          
          addPolys(this.box,lty=2,lwd=2)
          # Add the regular survey tows.
          points(slat~slon,surv.seed,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1.3)
          # Add the exploratory survey tows
          points(slat~slon,surv.seed,subset=year==yr&state =='live' & random==5,pch=24,bg="darkorange",cex=1.3)
        } # end for(b in 1:3)
        # Now add the legend.
        par(xpd=T)
        plot(1:10,type='n',axes=F,xlab='',ylab='',main="",cex.main=1)
        legend("left",c(paste(plt.lvls[-length(plt.lvls)],'-',plt.lvls[-1],sep=''),
                        paste(plt.lvls[length(plt.lvls)],'+',sep='')),fill=c(plt.colors$col),
               border="black",pch=c(rep(NA,length(lvls))),title = N.tow.lab,title.adj = 0.2,
               pt.bg = c(rep(NA,length(lvls2))),bg=NA,bty="n")
        
        legend("topright",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",inset=0.01,
               legend = c(paste('regular (n =',
                                length(subset(surv.seed,year==yr & state=='live'& random==1)$ID),")",sep=""),
                          paste('exploratory (n =',
                                length(subset(surv.seed,year==yr & state=='live'& random==5)$ID),")",sep="")),
               bg=NA,box.col=NA,bty="n")
        if(add.title == T) title(paste("Seedbox ",fig.box.name[j]," (",banks[i],"-",yr,")",sep=""),cex.main=2,outer=T,line=-0.5)
        if(fig == "file") dev.off()
        
      } # end for(j in 1:n.box)
    } # end (if nrow(sb))
  } # end the if(any(plots) %in% "seedboxes")
  
  
  
  
} # end the i for loop 

} # end the function
