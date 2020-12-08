#####################################  This function is used to generate all of the figures used in survey summary  #####################
#####################################  A major update occured in May of 2017, moving from contour plots to INLA spatial models ###########
#####################################  Banquereau is  not included in this function at this time ######
## Created by DK December 2015
# Update history
#Commented, checked  and revised by DK March 31, 2016
# June 16th 2016, revised to add the "season" variable so we could save results for the spring survey.
# July 3rd(ish) 2016, revised to include the "seedboxes" plots + some changes to GB spring to get MW-SH figure fixed up and get all the tows included.
# July 7th(ish) 2016, revised abund/biomass/clap-ts plots to get axes more consistent (set a low threshold on the y-axes for the plots), revised CF
# color ramp, and made some major modification to the SHF plots scales (split the plot into a PR and rec/FR)
# May 25-31st 2017, Major update of the spatial plots, all spatial figures now using INLA modelling framework for spatial patterns.  For the moment
#     these are VERY simple INLA models, more thought to these should be given, but this is a first step towards moving our modelled to be spatial.
#     Also enabled saving figures as PDF's (the old contour scripts caused weird plotting artifacts on these)
# June 2017:  An additional upgrade to allow for plotting of user specified Shell height bins, these bins are specified in the call to the
#     SurveySummary_data.r function so all that needs to be done here is to recognize that we want to plot those results.
# August 2017:  Building on the above revisions the INLA models were re-implemented to use a proper mesh so as to minimize
#               any edge effects.  This is far slower than the previous method so several updates were required to the script
#               1:  An option to save and load the INLA results was added.  This means the INLA portion of the script only needs to be run once.
#               2:  An option to change the resolution of the prediction grid (not the mesh but the model results on an underlying grid) was added
#               3:  An option to quickly make the spatial figures was added, these are quick implementations of INLA with the meshed developed for 
#                   the June 2017 update, these will run in minutes (rather than the hours for full INLA models) and provide decent maps
#               4:  An option to add a scale bar to the figure was also added to the function.  
#               5:  The color scheme for the spatial abundance figures was changed from the "magma" to "plasma" color scale of the viridis package.  
#               6:  An option to add a countour outline the the spatial figures was also added to the function to help define the boundaries
#                   for each level shown on the spatial figures.
#               7:  Added a cute little figure zoomed in on any seedboxes.
# Sept 2017:    Tiny change to the legend for the survey figure on GBa (it was including non-random tows in the # of tows legend)
# Oct 2017:     Minor change to the add.scale for seedboxes and added an if statement for add.scale with GBb and Sable.
# Nov 2017:     Updated the INLA based on what we did with SPERA project, the models now run many times quicker than they did...
# July 2018:    Numererous changes, see Github for full description/history of modifications
# Oct 2018:     Updated to enable plotting of small user defined sub-areas.
# Nov 2018:     Created keep.full.GB option (dk changed to full.GB in 2020) which allows you to create INLA spatial maps for ALL of GB, not just GBa and GBb separately. 
# Aug 2019:     Various updates, see github, included moving models to negative binomials for abundance spatial figures, and tidying up seedboxes
# April 2020:   Massive SF overhaul, removed seedboxes spatial figures, started saving all gg.objects so these can be pulled in elsewhere               
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
# 6:  shwt.plt1.r
# 7:  Clap3.plt.R
# 8:  gridPlot.r
# 9:  meat_count_shell_height_breakdown_figure.r    
##
###############################################################################################################

###############################################################################################################
# Arguments
#1: plots     What plots do you want to make.  Defaults to all plots options include plots = 'all'
####  a:  all               - This makes all the plots so you don't have to mess about
####  b:  spatial           - This makes all the spatial plots, just a wrapper to save you from writting all these out every time
####  c:  simple            - This will make all non-spatial figures, like time series, SHF, breakdown fig
####  d:  PR-spatial        - Spatial Pre-recruits 
####  e:  Rec-spatial       - Spatial Recruits
####  f:  FR-spatial        - Spatial fully recruited
####  g:  CF-spatial        - Spatial condition factor
####  h:  MC-spatial        - Spatial Meat count
####  i:  Clap-spatial      - Spatial clappers (%), Recruits + Fully recruited
####  j:  Survey            - Spatial survey tow locations + strata where applicable
####  k:  MW-SH             - Meat Weight Shell Height 2 panel plot, MW-SH by tow (left panel) + Condition time series (right panel)
####  l:  abund-ts          - Survey estimated abundance (#/tow) time series of Pre recruits, recruits and fully recruited
####  m:  biomass-ts        - Survey estimated biomass (kg/tow) time series of Pre recruits, recruits and fully recruited
####  n:  SHF               - Shell height frequency for the last 6 years
####  o:  SHF-large         - SHell height frequency for the last 6 years for SH bins > 70 mm.
####  p:  SHF-split         - Shell height frequency for the last 6 years, split at 60 mm to enable re-scaling of large SHF size bins
####  q:  clapper-abund-ts  - Clapper abundance (#/tow) time series of Pre recruits, recruits and fully recruited
####  r:  clapper-per-ts    - Average % clappers per tow time series all 3 size classes
####  s:  SH-MW-CF-ts       - Time series of average shell height, meat weight, and condition factor.
####  t:  breakdown         - Plot of biomass by shell height + meat count
####  u:  seedboxes         - Plot of the seedboxes, this includes several plots which will all be produced if there is currently an open seedbox
####  v:  user.SH.bins      - Make plots for the selected SH bins by the you (the user), Shell height bins that you set up
####  w:  MW-spatial        - Make a plot of the average meat weight across the bank (includes all individuals)
####  x:  SH-spatial        - Make a plot of the average shell height across the bank (includes all individuals)
####  y:  MW.GP-spatial     - Make a plot of the meat weight growth potential across the bank (all individuals).
####  z:  SH.GP-spatial     - Make a plot of the shell height growth potential across the bank (all individuals).
#########################################

#2: banks     The banks to create figures for.  Option are "BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB" 
######          (note Banquereau is not supported yet and the GB is Georges Bank Spring

#3: yr        The survey year of interest.  Default = as.numeric(format(Sys.time(), "%Y")) which attempts to produce plots for the current year.

#4: fig       Plot the figures to "screen", to a "pdf" or a "png" file. If fig = "pdf" or "png" figures are placed in this directory (which must exist!!) 
###              (direct/yr/Presentations/Survey_summary/bank - e.g.Y:/Offshore scallop/Assessment/2015/Presentations/Survey_summary/GBa/)
###              unless season == "testing" in which they get saved to a testing folder (see below for exact directory).

#5: scale.bar  Do you want to add a scale bar, it also pops in a fancy north arrow . Default of NULL does nothing.
###              If you want one you put in same options as what is in pectinid.
###              To add it you specify what corner you want it in and optionally it's size as a second option.
###              scale.bar = 'bl' will put it in bottom left (options are bl,bc,br,tl,tc,tr) 
###              scale.bar = c('bl',0.5) will put in a scale bar that is half the length of the figure in the bottom left corner.

#6: bathy      Do you want to add the bathymetry, options are identical to the options used in pectinid for easy compatability
###             See pectinid help for more details.  Default is bathy = 50 which is same as bathy = c(50,'both',500)  which will plot smooth surface + contour lines at a 50 meter intervals and 500 is the maximum depth
###             If you don't want to plot the bathy go with bathy = NULL              

#7: add.title Add titles to the figures; T/F.  Default = T

#8: INLA:       What to you want to do with the spatial modelling.  Three options, the default is INLA= "run.full"  
###               1:  "run"       This will run the INLA for the spatial plot requested, 
###               2: "run.full"   THIS WILL RUN AND SAVE the models for all of the specified spatial figures, not just the models specified by the "plots" arguement above.
###                               This can be slow (depends on computer), but shouldn't take more than an hour for all banks based on my testing
###                               The results are saved with the Bank name and spatial resolution in the file name so that high and low runs can be saved and 
###                               recalled as necessary.  This uses the prediction stack in INLA and provides the results on a fine spatial mesh
###                               but with a proper mesh which minimizes any edge effects.
###               3: "load"  This loads in the spatial models saved and avoids having to re-run the "run.full" option whenever you want to
###                               remake figures.  These will be saved by bank and will include the spatial resolution used in model. This excludes any 
###                               special monitoring boxes (i.e. the "Starbox on Sable).  Regular seeedbox results will typically be 
###                               saved as subset of the bank.

#9: s.res:      The spatial resolution for any spatial plots.  Default = "low" which will quickly produce the spatial plots, but the detail will be low.  Options are
###                1: "low"    Quick low resolution plots, this options sets up a 50 X 50 prediction grid.
###                2: "high"   Slow high resolution plots, this option sets up a 250 X 250 prediction grid, this can take a while so only run for final figures.
###                3: c(X,Y)   Where X and Y are numbers that user inputs, (e.g. s.res = c(250,250) would give the same result as setting this to "high")

#10: direct     The working directory to put figures are from which to grab data.  Default = "Y:/Offshore scallop/Assessment/", 

#11: direct_fns The working that the functions are located in.  Default = "Y:/Offshore scallop/Assessment/", 

#12: save.gg    Do you want to save the ggplots you made for later in life....

#13: season    For the spring survey we need to identify that we don't have all the results in yet.  When running the scripts with only spring  
###              survey data set to "spring".  If just running GBa and GBb you can set this to "summer" if you've already created the Rdata file.
###              When summer survey is complete you can also set this to the default of "both".  I've also added the option during preliminary
###              runs or when altering the function to use a "testing" option so it loads the proper data.  You'll need to have created the 
###              testing R data when running the SurveySummary_data.r.  Additionally when set to "testing" the figures (if being saved) will be
###              saved in the folder... direct,yr,"/Presentations/Survey_summary/test_figures/",banks[i].

#14: nickname  This is used if you have a specific output from Survey_summary_data call.  You want this to be the same as what you used there, e.g. "GB_special_run"

#15: sub.area  Do you want to make plots of the user specfied sub areas, currently only relevant for GBa.  T/F, default = F

#16: full.GB    Set to true (T) if you want to plot all of GB on one INLA spatial map (instead of GBa and GBb separately). Default is F


#############################################################################################################################################
##### NB: If trying to reproduce figures from previous years make sure the Rdata output from SurverySummary_data.r for that year exists! ####
#############################################################################################################################################

Ind2020.survey.figs <- function(plots = plots, banks = banks , yr = yr,
                                fig=fig, scale.bar = NULL, bathy = 50, add.title = T, INLA = "run" , s.res = "low",
                                direct = direct, direct_fns = direct_fns,
                                save.gg = F, season="both",nickname=NULL, sub.area=F, full.GB=F,
                                se=F)
{ 
  tmp.dir <- direct ; tmp.season <- season; tmp.yr <- yr 
  
  # Freya was here... so you need more packages. Sorry. 
  require(tidyverse)
  require(ggrepel)
  
  # Load the appropriate data.
  # If you used a plot shortcut, get the correct names for the plots you
  if(any(plots == 'all')) 
  {
    plots <- c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial","Survey","MW-SH",
               "abund-ts","biomass-ts","SHF","SHF-large","SHF-split",
               "clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown","seedboxes","user.SH.bins",
               "MW-spatial","SH-spatial","MW.GP-spatial","SH.GP-spatial")
  } # end if(plots == 'all')
  
  if(any(plots == 'spatial')) 
  {
    plots <- c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial",
               "MW-spatial","SH-spatial","MW.GP-spatial","SH.GP-spatial")
  } # end if(plots == 'spatial')
  
  if(any(plots == 'simple'))
  {
    plots <- c("MW-SH","abund-ts","biomass-ts","SHF","SHF-large","SHF-split",
               "clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown")
  }# end if(plots == 'simple'
  if(season == "testing") 
  {
    # Some temporary assignments so we can load in old data without overwritting more important crap
    temp.nick <- nickname
    season <- tmp.season 
    yr <- tmp.yr
    dir.temp <- direct
    
    if(any(plots %in% "MW-SH") && any(banks %in% "GBa"))
    {
      # This loads last years Survey object results.
      load(paste(direct,"Data/Survey_data/",yr-1,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
      survey.obj.last <- survey.obj
    } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
    
    nickname <- temp.nick
    direct <- dir.temp
    season <- tmp.season 
    yr <- tmp.yr
    
    # THEN bring in this year's file:
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/testing_results.Rdata",sep=""))==T ||
       file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/testing_results_", nickname, ".Rdata",sep=""))==T) 
    {                   
      if(is.null(nickname)) load(paste(direct,"Data/Survey_data/",yr,
                                       "/Survey_summary_output/testing_results.Rdata",sep=""))
      if(!is.null(nickname)) load(paste(direct,"Data/Survey_data/",yr,
                                        "/Survey_summary_output/testing_results_", nickname, ".Rdata",sep=""))
      
    } else stop("Please re-run Survey_Summary_script and set it so that the file 'testing_results.Rdata' gets created, Thanks eh!!") # end if/else file...
    season <- tmp.season 
    yr <- tmp.yr
  } # end if(season == "testing") 
  
  if(season == "both") 
  {
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))==T)
    {
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
      survey.obj.last <- survey.obj
      season <- tmp.season # Needed b/c there is a season in the object I'm loading too..
    } else stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_all_results.Rdata' gets created, Thanks eh!!")
    yr <- tmp.yr
  } # end if(season == "both") 
  
  # if(season == "spring") 
  # {
  #   # If we are making the MW/SH plot & looking at GB we need to get both the spring and summer GB data
  #   if(any(plots %in% "MW-SH") && any(banks %in% "GB"))
  #   {
  #     # This loads last years Survey object results.
  #     load(paste(direct,"Data/Survey_data/",(yr-1),"/Survey_summary_output/Survey_all_results_FINAL.Rdata",sep=""), )  
  #     if(dim(survey.obj$GBa$model.dat)[1]==0) message("Edit line 159 to pull in last year's Survey summary object for the GB MWSH plot.")
  #     survey.obj.last <- survey.obj
  #   } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
  #   season <- tmp.season 
  #   yr <- tmp.yr
  #   if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))==T)
  #   {
  #     load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
  #     season <- tmp.season # Needed b/c there is a season in the object I'm loading too..
  #   } else stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_spring_results.Rdata' gets created, Thanks eh!!") # end if/else file.
  # } # end if(season == "spring") 
  # season <- tmp.season 
  # yr <- tmp.yr
  if(season == "summer") 
  {
    # If we are making the MW/SH plot & looking at GB we need to get both the spring and summer GB data
    if(any(plots %in% "MW-SH") && any(banks %in% "GBa"))
    {
      # This loads the springs survey results.
      load(paste(direct,"Data/Survey_data/",yr-1,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
      survey.obj.last <- survey.obj
      season <- tmp.season
      yr <- tmp.yr
    } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
  }# if(season == "summer")
  
  # Now get the banks to plot set up.
  if(any(banks == "all")) banks <- c("BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB")
  # This is useful for testing...
  if(any(banks == 'core')) banks <- c("BBn" , "GBb", "GBa")
  
  # Since BBs is only surveyed occasionally we need to make sure it exists, if it doesn't toss it...
  if(is.null(bank.dat$BBs) && "BBs" %in% banks) banks <- banks[-which(grepl(x=banks, "BBs"))]
  # If we are plotting the sub-areas we wanna do this...
  if(sub.area == T) {spat.name <- unique(spat.names$label); banks <- c(banks,spat.name)}
  if(sub.area == F) spat.name <- NULL
  # We may need to load in the Sable pre-stratified data for the Sable figure, but only if it is 2018 as we won't plot this after that time.
  # Load in the pre 2018 data for Sable
  
  direct <- tmp.dir # I need this so that the directory isn't overwritten when I load the above
  
  # These are the functions used to within the heart of the code to make stuff happen
  source(paste(direct_fns,"Maps/pectinid_projector_sf.R",sep="")) 
  source(paste(direct_fns,"Maps/ScallopMap.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/stdts.plt.R",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/survey.ts.r",sep=""),local=T)
  source(paste(direct_fns,"Survey_and_OSAC/shf.plt.r",sep=""))
  source(paste(direct_fns,"Survey_and_OSAC/shwt.plt1.r",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/Clap3.plt.R",sep="")) 
  source(paste(direct_fns,"Survey_and_OSAC/gridPlot.r",sep="")) 
  source(paste0(direct_fns, "Maps/github_spatial_import.R", sep=""))
  source(paste(direct_fns,"Survey_and_OSAC/meat_count_shell_height_breakdown_figure.r",sep="")) 
  require(viridis) || stop("Install the viridis package for the color ramps")
  require(INLA) || stop("Install the INLA package for the spatial plots")
  require(maps)|| stop("Install the maps package for the spatial plots")
  require(maptools)|| stop("Install the maptools package for the spatial plots")
  require(mapdata)|| stop("Install the mapdata package for the spatial plots")
  require(rgeos)|| stop("Install the rgeos package for the spatial plots")
  require(splancs)|| stop("Install the splancs package for the spatial plots")
  require(boot)|| stop("Install the boot package for the spatial plots")
  require(fields)|| stop("Install the fields package for the spatial plots")
  require(PBSmapping)|| stop("Install the PBSmapping package for the spatial plots")
  require(ggplot2)|| stop("Install the ggplot2 package for the subarea plots")
  require(plyr)|| stop("Install the plyr package for the subarea plots")
  require(reshape2)|| stop("Install the reshape2 package for the subarea plots")
  require(sf) || stop("It's 2020. We have entered the world of sf. ")
  # Function used for any beta models to transform 0's and 1's to near 0 and near 1 (beta doesn't allow for 0's and 1's.)
  beta.transform <- function(dat,s=0.5)  (dat*(length(dat)-1) + s) / length(dat)
  
  # If necessary bring in the fishery regulations (only used for seedbox and breakdown figures)
  
  if(any(plots %in% c("seedboxes","breakdown"))) fish.reg <- read.csv(paste(direct,"Data/Fishery_regulations_by_bank.csv",sep="")) # Read1
  
  #Initialize some variables I need.
  len <- length(banks)
  # Set up objects for plot labels.  
  N.tow.lab <- expression(frac(N,tow))
  B.tow.lab <- expression(frac(kg,tow))
  cf.lab <-    expression(paste("CF:",bgroup("(",frac(g,dm^3)   ,")")))
  mc.lab <-    expression(paste("MC:",bgroup("(",frac(N,"500 g"),")"))) 
  
  # add an entry into the run log
  if(!file.exists(paste0(direct_fns, "/Survey_and_OSAC/SurveySummaryRunLog.csv"))) runlog <- data.frame(X=NULL, runfunction=NULL, runassigned=NULL, rundefaults=NULL)
  if(file.exists(paste0(direct_fns, "/Survey_and_OSAC/SurveySummaryRunLog.csv"))) runlog <- read.csv(paste0(direct_fns, "/Survey_and_OSAC/SurveySummaryRunLog.csv"))
  runlog <- runlog[, !names(runlog) %in% "X"]
  rundate <- as.character(Sys.time())
  runfunction <- "figures"
  runassigned <- paste(as.character(deparse(match.call())), collapse="")
  rundefaults <- paste(as.character(deparse(args(survey.figs))), collapse="")
  runlog <- rbind(runlog, cbind(rundate, runfunction, runassigned, rundefaults))
  write.csv(runlog, file = paste0(direct_fns, "/Survey_and_OSAC/SurveySummaryRunLog.csv"))
  
  # Clean up any open plot devices.
  if(!is.null(dev.list())) dev.off()
  
  # Run through all of the banks of interest.  Note we are going to create a crap load of directories when doing sub-area plots.
  for(i in 1:len)
  {
    print(banks[i])
    # Here we set up where the plots will go and if the directory doesn't exist yet we create it.
    if(season == "testing" && fig != "screen") 
    {
      # Get the plot directory
      plot.dir = paste(direct,yr,"/Presentations/Survey_summary/test_figures/",banks[i],"/",sep="")
      # If the above directory does not exist then we need to create it.
      if(dir.exists(plot.dir)==F)
      {
        # This enables us to create the base specified directory on up...
        if(dir.exists(direct) ==F) dir.create(direct)
        if(dir.exists(paste(direct,yr,sep="")) ==F) dir.create(paste(direct,yr,sep=""))
        if(dir.exists(paste(direct,yr,"/Presentations",sep="")) ==F) dir.create(paste(direct,yr,"/Presentations",sep=""))
        if(dir.exists(paste(direct,yr,"/Presentations/Survey_summary",sep="")) ==F) dir.create(paste(direct,yr,"/Presentations/Survey_summary",sep=""))
        dir.create(paste(direct,yr,"/Presentations/Survey_summary/test_figures/",sep=""))
        dir.create(paste(direct,yr,"/Presentations/Survey_summary/test_figures/",banks[i],sep=""))
      } # end if(dir.exists(plot.dir)==F)
    } # end if(testing == T) 
    if(season != "testing" && fig != "screen") 
    {
      # Get the plot directory
      plot.dir = paste(direct,yr,"/Presentations/Survey_summary/",banks[i],"/",sep="")
      # If the above directory does not exist then we need to create it.
      if(dir.exists(plot.dir)==F)
      {
        # This enables us to create the base specified directory on up...
        if(dir.exists(direct) ==F) dir.create(direct)
        if(dir.exists(paste(direct,yr,sep="")) ==F) dir.create(paste(direct,yr,sep=""))
        if(dir.exists(paste(direct,yr,"/Presentations",sep="")) ==F) dir.create(paste(direct,yr,"/Presentations",sep=""))
        if(dir.exists(paste(direct,yr,"/Presentations/Survey_summary",sep="")) ==F) dir.create(paste(direct,yr,"/Presentations/Survey_summary",sep=""))
        dir.create(paste(direct,yr,"/Presentations/Survey_summary/",banks[i],sep=""))
      } # end if(dir.exists(plot.dir)==F)
    } # end if(season != "testing")  
    
    # If not specified use the latest year in each time series.
    #yr <- max(surv.Live[[banks[i]]]$year,na.rm=T)
    # Get the RS and CS for the bank...
    RS <- size.cats$RS[size.cats$Bank == banks[i]]
    CS <- size.cats$CS[size.cats$Bank == banks[i]]
    
    # Grab any seedboxes of interest...
    # I'm picking November 1st of current year b/c if they close a box at this point none of our presentations
    # will have information about it.  The only reason I'm putting this closed bit in is for cases in which
    # I am making plots from previous years, so a box closed in Nov or December never would have been included in one of our
    # presentations (maybe OSAC, but this isn't OSAC)....
    sb <- subset(seedboxes,#Bank == banks[i] & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep="") | 
                 Bank == banks[i] & Active=="Yes")
    
    ###  Now for the plots, first the survey data...
    # Get the  bank survey boundary polygon
    
    bound.poly.surv <- as.PolySet(bound.surv.poly[[banks[i]]],projection ="LL")
    
    #Detailed survey polygons # NOT A STRATIFIED SURVEY!
    detail.poly.surv <- NULL
    
    # Get the strata areas.  For most areas we use the survey.strata.table which is output from the data function
    # strata.areas <- subset(survey.strata.table[[banks[i]]],select =c("PID","towable_area"))
    
    # NOT A STRATIFIED SURVEY!
    # if(banks[i] %in% c("Sab") & !yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
    #   strata.areas <- subset(survey.info[!(survey.info$startyear==1900 & survey.info$label=="Sab"),], label==banks[i],select =c("PID","towable_area"))}
    # if(banks[i] %in% c("Sab") & yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
    #   strata.areas <- subset(survey.info[!(survey.info$startyear==2018 & survey.info$label=="Sab"),], label==banks[i],select =c("PID","towable_area"))}
    # if(banks[i] %in% c("GB", "Mid", "Ger", "Ban", "BanIce")) 
    strata.areas <- NULL
    
    #Get all the details of the survey strata
    # if(banks[i] %in% c("Sab") & !yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
    #   surv.info <- survey.info[survey.info$startyear==2018 & survey.info$label=="Sab",]}
    # if(banks[i] %in% c("Sab") & yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
    #   surv.info <- survey.info[!(survey.info$startyear==2018) & survey.info$label=="Sab",]}
    # 
    # if(!banks[i] %in% c("Sab")) 
    surv.info <- survey.strata.table[[banks[i]]]
    
    ################################# START MAKING FIGURES################################# START MAKING FIGURES################################# 
    
    ################  The non-survey spatial plots ###########################
    ################  Next up are the rest of the spatial plots ###########################
    ################  Next up are the rest of the spatial plots ###########################
    ############  Now start getting the spatial data for this year for the specific bank.
    
    # If we are making the seedbox or any of the spatial plots we need to set up our mesh...
    # We need to make a mesh for the plots from which we had data from all tows
    # and a mesh for the plots from which we only had data for select tows (i.e. condition factor)
    # Only run this for the full banks, we don't want to run this for the "sub-areas"
    
    # If we are making spatial plots do this
    spatial.maps <- plots[grep("spatial",plots)]
    
    # Some stuff I need for both the Survey and spatial figures...
    if(length(spatial.maps)> 0 || grepl("Survey",plots))
    {
      # Now convert this to an object for sp, this gets our bounding area for the survey.
      
      bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)
      
      if(banks[i]=="BBn"){
        surv.Live[[banks[i]]]$year <- ifelse(!is.na(surv.Live[[banks[i]]]$pastyear), 2020, surv.Live[[banks[i]]]$year)
        surv.Clap[[banks[i]]]$year <- ifelse(!is.na(surv.Clap[[banks[i]]]$pastyear), 2020, surv.Clap[[banks[i]]]$year)
      }
      
      if(banks[i] %in% c("GBa", "GBb")){
        surv.Live[["GBa"]]$year <- ifelse(!is.na(surv.Live[["GBa"]]$pastyear), 2020, surv.Live[["GBa"]]$year)
        surv.Clap[["GBa"]]$year <- ifelse(!is.na(surv.Clap[["GBa"]]$pastyear), 2020, surv.Clap[["GBa"]]$year)
        surv.Live[["GBb"]]$year <- ifelse(!is.na(surv.Live[["GBb"]]$pastyear), 2020, surv.Live[["GBb"]]$year)
        surv.Clap[["GBb"]]$year <- ifelse(!is.na(surv.Clap[["GBb"]]$pastyear), 2020, surv.Clap[["GBb"]]$year)
      }
      
      
      # Next we get the survey locations
      if(banks[i] %in% c("BBn"))
      {   
        loc <- data.frame(lon = surv.Live[[banks[i]]]$lon[surv.Live[[banks[i]]]$year == yr],
                          lat=surv.Live[[banks[i]]]$lat[surv.Live[[banks[i]]]$year == yr])
      }# end if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","SPB","GB"))
      # I want 1 mesh for all of Georges bank summer survey.
      if(banks[i] %in% c("GBa","GBb")) 
      {
        loc <- data.frame(lon = c(surv.Live[["GBa"]]$lon[surv.Live[["GBa"]]$year == yr],surv.Live[["GBb"]]$lon[surv.Live[["GBb"]]$year == yr]),
                          lat=c(surv.Live[["GBa"]]$lat[surv.Live[["GBa"]]$year == yr],surv.Live[["GBb"]]$lat[surv.Live[["GBb"]]$year == yr]))
        
        # The condition and meat count data.
        CF.current[["GBa"]]$year <- 2020
        CF.current[["GBb"]]$year <- 2020
                   
        loc.cf <- data.frame(lon = c(CF.current[["GBa"]]$lon[CF.current[["GBa"]]$year == yr],CF.current[["GBb"]]$lon[CF.current[["GBb"]]$year == yr]),
                             lat=c(CF.current[["GBa"]]$lat[CF.current[["GBa"]]$year == yr],CF.current[["GBb"]]$lat[CF.current[["GBb"]]$year == yr]))
        # For the growth potential related figures we also need to make a special mesh as there could be some tows with 0 individuals
        # and these may screw up the INLA'ing
        loc.gp <- data.frame(lon = c(pot.grow[["GBa"]]$slon[pot.grow[["GBa"]]$year == yr],pot.grow[["GBb"]]$slon[pot.grow[["GBb"]]$year == yr]),
                             lat=c(pot.grow[["GBa"]]$slat[pot.grow[["GBa"]]$year == yr],pot.grow[["GBb"]]$slat[pot.grow[["GBb"]]$year == yr]))
        
        loc.cf <- st_as_sf(loc.cf,coords = c('lon','lat'),crs = 4326)
        loc.cf <- st_transform(loc.cf,crs = 32619)
        loc.cf <- as(loc.cf,"Spatial")
      } # end if(banks[i] %in% c("GBa","GBb") 
      
      loc.sf <- st_as_sf(loc,coords = c('lon','lat'),crs = 4326)
      loc.sf <- st_transform(loc.sf,crs = 32619)
      loc    <- as(loc.sf,"Spatial")
      
      #bound.poly.surv.sp.buff <- spTransform(bound.poly.surv.sp.buff,CRS = st_crs(32619)[2]$proj4string)
      bound.poly.surv.sp <- spTransform(bound.poly.surv.sp, CRS(SRS_string = "EPSG:32619"))
      
    } # end if(length(spatial.maps)> 0 || plots[grep("Survey",plots)])
    
    # If we want spatial maps or seedboxes and/or have user SH.bins (for both of which we will produce all figures automatically.)
    if((length(spatial.maps > 0) || any(plots %in% "user.SH.bins"))) 
    {
      mod.res <- NULL # This will contain the model results for the spatial figures
      # Set up the resolution for the spatial plots, low is relatively fast, high is quite slow
      if(length(s.res) == 2) s.res <- s.res
      if(any(s.res == "high")) s.res <- c(250,250)
      if(any(s.res == "low")) s.res <- c(25,25)
      
      # This section only needs run if we are running the INLA models
      if(length(grep("run",INLA)) > 0)
      {
        
        # This is how the mesh and A matrix are constructed
        # Build the mesh, for our purposes I'm hopeful this should do the trick, should cover our entire survey area.
        cat("ALERT!  I'm building the mesh for",banks[i], "if this hangs here please try using a different offset for this bank.. \n")
        
        # Guidelines for meshes from Zuur, I expect the range for the process in these areas to be around 5 km (i.e. size of a bed.)
        # Using 10 km makes a mesh that seems to lead to weird behaviour so be warned!!
        range <- 5 * 1000 ; max.edge <- range/5
        #if(banks[i] == "Ger") range <- 7.5 * 1000 ; max.edge <- range/5
        
        # Will this work for them all I wonder? Max edge should be around 1/5 of the range according to Zuur
        mesh <- inla.mesh.2d(loc, boundary= inla.sp2segment(bound.poly.surv.sp), max.edge=c(1,5)*max.edge, cutoff=max.edge)
        mesh$crs <- CRS(SRS_string = "EPSG:32619")
        plot(mesh) # For testing I want to plot this to see it and ensure it isn't crazy for the moment...
        windows(11,11) ; plot(mesh) ;  plot(bound.poly.surv.sp,add=T,lwd=2)
        cat("Mesh successful, woot woot!!")
        # Now make the A matrix
        
        A <- inla.spde.make.A(mesh, loc)
        
        if(!banks[i]=="BBn") A.cf <- inla.spde.make.A(mesh, loc.cf)
        
        # We can just make the one spde object for all of these as well.
        spde <- inla.spde2.pcmatern(mesh,    
                                    prior.sigma=c(2,0.5), # The probabiliy (second number) that the marginal standard deviation (first number) is larger than the first number
                                    prior.range=c(range,0.5)) # The Meidan range and the probability that the range is less than this..

        ## All of our abundance spatial plots are counts
        family1 = "poisson"
        family1.cf <- "gaussian" # For CF, MC,MW, and SH they are more normal so go with a gaussian.
        family.clap <- "poisson" # I haven't found a good family for the clapper data, for the moment the poisson does a decent job as long
        # as we don't have very high clapper values (i.e. near 100%), it can get weird there, but I can't find a better likelihood yet...
        # I tried a beta and it wants to really smooth the surface far too much unfortunately, might work if we had higher clapper values, but when
        # low it just basically sets everything down toward 0.
        family.gp <- "lognormal" # This could use some more thought....
        
        # but I need to truncate the predicted values that are > 100 to be 100 which is BS...
        # As soon as you make a spatial model make your own intercept.  Here is
        a0 <- 1 # intercept
        # Mostly just using stock priors, again fine for our purposes for the moment.
        #pcprec <- list(prior='pc.prec', param=c(0.5, 0.5))
        # Add an index to the data
        # The spatial model, simple model with a intercept (overall bank average) with the spde spatial component
        # basically the random deviations for each piece of the mesh.
        formula3 <- y ~ 0 + a0 + f(s, model=spde)
        
        
        # if we have maps to be made and we aren't simply loading in the INLA results we need to run this bit.
        if(length(spatial.maps) > 0)
        {
          # Get the data needed....
          if(banks[i] %in% c("GBb","GBa")) 
          {
            tmp.dat <- rbind(surv.Live[["GBa"]][surv.Live[["GBa"]]$year == yr,],surv.Live[["GBb"]][surv.Live[["GBb"]]$year == yr,])
            tmp.cf <- rbind(CF.current[["GBa"]],CF.current[["GBb"]]) 
            tmp.clap <- rbind(surv.Clap[["GBa"]][surv.Clap[["GBa"]]$year == yr,],surv.Clap[["GBb"]][surv.Clap[["GBb"]]$year == yr,])
            tmp.gp <- rbind(pot.grow[["GBa"]][pot.grow[["GBa"]]$year == yr,],pot.grow[["GBb"]][pot.grow[["GBb"]]$year == yr,])
          }  # end if(banks[i] %in% c("GBb","GBa")) 
          
          if(banks[i] %in% "BBn") 
          {  
            tmp.dat <- surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,]
            tmp.clap <- surv.Clap[[banks[i]]][surv.Clap[[banks[i]]]$year == yr,]
          } # end if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","BanIce","SPB","GB")) 
          # Now loop through each spatial map we want to make.
          fitted <- NULL
          for(k in 1:length(spatial.maps))
          {
            print(spatial.maps[k])
            # In the next bunch of if statements we run the INLA model and we get the figure titles sorted out.
            if(spatial.maps[k] == "PR-spatial")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = round(tmp.dat$pre,0), link=1L),
                                effects=list(data.frame(a0=rep(1, nrow(tmp.dat))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family1, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
              
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.dat$pre)],
                                                      dat=tmp.dat$pre)
            } # end if(spatial.maps[k] == "PR-spatial")   
            
            if(spatial.maps[k] == "Rec-spatial")        
            {
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = round(tmp.dat$rec,0), link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.dat)), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family1, data = inla.stack.data(stk),#control.family= control.family1,
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.dat$rec)],
                                                      dat=tmp.dat$rec)
            } # end if(spatial.maps[k] == "Rec-spatial") 
            
            if(spatial.maps[k] == "FR-spatial") 
            {
              
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = round(tmp.dat$com,0), link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.dat)), s = 1:spde$n.spde),
                                A = list(1, A))
              #print(stk)
              # This is the INLA model itself
              mod <- inla(formula3, family=family1, data = inla.stack.data(stk),#control.family= control.family1,
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.dat$com)],
                                                      dat=tmp.dat$com)
            } # end if(spatial.maps[k] == "FR-spatial")
            
            if(spatial.maps[k] == "CF-spatial" & !banks[i] == "BBn")       
            {
              
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.cf$CF, link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.cf)), s = 1:spde$n.spde),
                                A = list(1, A.cf))
              # This is the INLA model itself
              mod <- inla(formula3, family=family1.cf, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.cf$CF)],
                                                      dat=tmp.cf$CF)
            }
            # THis seems to be making sense...
            if(spatial.maps[k] == "MC-spatial" & !banks[i] == "BBn")      
            {
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.cf$meat.count, link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.cf)), s = 1:spde$n.spde),
                                A = list(1, A.cf))
              # This is the INLA model itself
              mod <- inla(formula3, family=family1.cf, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
              
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.cf$meat.count)],
                                                      dat=tmp.cf$meat.count)
            } # end if(spatial.maps[k] == "MC-spatial") 
            
            if(spatial.maps[k] == "Clap-spatial")        
            {
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = round(tmp.clap$clap.prop,0), link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.clap)), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.clap, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
              
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.clap$clap.prop)],
                                                      dat=tmp.clap$clap.prop)
            } # end if(spatial.maps[k] == "Clap-spatial")  
            
            if(spatial.maps[k] == "MW-spatial" & !banks[i] == "BBn")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.gp$cur.mw, link=1L),
                                effects=list(data.frame(a0=rep(1, nrow(tmp.gp))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.gp, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
              
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.gp$cur.mw)],
                                                      dat= tmp.gp$cur.mw)
            } # end if(spatial.maps[k] == "PR-spatial")  
            
            if(spatial.maps[k] == "SH-spatial" & !banks[i]=="BBn")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.gp$cur.sh, link=1L),
                                effects=list(data.frame(a0=rep(1, nrow(tmp.gp))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.gp, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
              
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.gp$cur.sh)],
                                                      dat= tmp.gp$cur.sh)
            } # end if(spatial.maps[k] == "PR-spatial")  
            
            if(spatial.maps[k] == "MW.GP-spatial" & !banks[i] == "BBn")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.gp$gp.mw, link=1L),
                                effects=list(data.frame(a0=rep(1, nrow(tmp.gp))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.gp, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
              
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.gp$gp.mw)],
                                                      dat= tmp.gp$gp.mw)
            } # end if(spatial.maps[k] == "PR-spatial")  
            
            if(spatial.maps[k] == "SH.GP-spatial" & !banks[i] == "BBn")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.gp$gp.sh, link=1L),
                                effects=list(data.frame(a0=rep(1, nrow(tmp.gp))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.gp, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
              
              fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.gp$gp.sh)],
                                                      dat= tmp.gp$gp.sh)
            } # end if(spatial.maps[k] == "PR-spatial")  
            
            # for the poisson and lognormal models...
            if(any(grepl(x=mod$call, pattern="family1")) | any(grepl(x=mod$call, pattern="family.clap")) | any(grepl(x=mod$call, pattern="family.gp"))) {
              mod.res[[spatial.maps[k]]] <- exp(mod$summary.random$s$mean + mod$summary.fixed$mean)
            }
            
            # Now for the Gaussian models.
            if(any(grepl(x=mod$call, pattern="family1.cf"))) mod.res[[spatial.maps[k]]] <- 
                mod$summary.random$s$mean + mod$summary.fixed$mean
            
            # print a message if the model didn't work:
            if(max(mod.res[[spatial.maps[k]]], na.rm=T) == "Inf") stop(paste0("Inf predictions in mod.res[[spatial.maps[k]]]. Please try a different mesh for ", banks[i], " ", spatial.maps[k], ".\nRecommend changing inla.mesh.2d max.edge argument very slightly."))

            if(min(mod.res[[spatial.maps[k]]], na.rm=T) < 0) stop(paste0("Negative predictions in mod.res[[spatial.maps[k]]]. Please try a different mesh for ", banks[i], " ", spatial.maps[k], ".\nDid you deal with the distribution properly?"))
            
            # Needed to make the clapper spatial work...
            if(spatial.maps[k] == "Clap-spatial")  mod.res[[spatial.maps[k]]][mod.res[[spatial.maps[k]]] > 100] <- 100
            
              #       if(spatial.maps[k] == "Clap-spatial")  mod.res[[spatial.maps[k]]][mod.res[[spatial.maps[k]]] > 100] <- 100
          } # end for(k in 1:length(spatial.maps)) # End the loop for getting all the data needed for a bank for the spatial maps.
        } # end if(length(spatial.maps > 0))
      } # end the if(length(grep("run",INLA)) > 0)
      
      print("finished running normal models")
      
      
      
      
      ############### IF I KEEP THIS IT WILL NEED TO MIRROR THE ABOVE FINAL PRODUCT!!
      ### The user shell height bins....
      #Now we need to get the projections if we have specified the User.SH.bins plots to be produced.
      bin.names <- NULL # Name bin.names a NULL, if no user.SH.bins we still need this name to exist...
      # Here I want to retain the "run.full" INLA call as I only want to enter this loop if I've asked for USER bins, or I asked to run the full model
      # This avoids running all the models for the bins needlessly and enables INLA="run" option to produce a figure in less than 1 minute
      if(any(plots == "user.SH.bins") || INLA == "run.full")
      {
        # Get the number of these calculations we need to make..
        num.bins <- length(survey.obj[[banks[i]]]$bin.names)
        bin.names <- survey.obj[[banks[i]]]$bin.names
        user.bins <- survey.obj[[banks[i]]]$user.bins
        
        # Get the data needed....
        if(banks[i] %in% c("GBb","GBa"))
        {
          tmp.dat <- rbind(surv.Live[["GBa"]][surv.Live[["GBa"]]$year == yr,],surv.Live[["GBb"]][surv.Live[["GBb"]]$year == yr,])
        }  # end if(banks[i] %in% c("GBb","GBa"))
        
        if(banks[i] %in% "BBn")
        {
          tmp.dat <- surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,]
        } # end if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","SPB","GB"))
        
        # Only run the models if not loading them....
        if(length(grep("run",INLA)) > 0)
        {
          # Now run through each bin...
          for(k in 1:num.bins)
          {
            # In the next bunch of if statements we run the INLA model and we get the figure titles sorted out.
            # This is the stack for the INLA model
            pick <- which(names(tmp.dat) == bin.names[k])
            stk <- inla.stack(tag="est",data=list(y = round(tmp.dat[,pick],0), link=1L),
                              effects=list(a0 = rep(1, nrow(tmp.dat)), s = 1:spde$n.spde),
                              A = list(1, A))
            # This is the INLA model itself
            mod <- inla(formula3, family=family1, data = inla.stack.data(stk),
                        control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
            # Get the spatial field results that we need for pecjector later            
            mod.res[[bin.names[k]]] <- exp(mod$summary.random$s$mean + mod$summary.fixed$mean)
            
            print(k)
          } # End for(k in 1:num.bins)
        } #end if(length(grep("run",INLA)) > 0)
      }# end i if(any(plots == "user.SH.bins") || length(grep("run",INLA)) > 0)
      print("finished running user bin models")
      
      # Now here we can save the results of all INLA runs for each bank rather than having to run these everytime which can be rather slow
      # Results are only saved if the option 'run.full' is chosen
      if(INLA == 'run.full') 
      {
        save(mod.res,mesh,fitted,
             file = paste(direct,"Data/Survey_data/", yr, "/Survey_summary_output/",banks[i],"/", banks[i],"_figures_res_",s.res[1],"-",s.res[2], ".RData",sep=""))
      } # end if(save.INLA ==T) 
      
      
      #######################  FIGURES#######################  FIGURES#######################  FIGURES#######################  FIGURES ##################
      #######################  FIGURES#######################  FIGURES#######################  FIGURES#######################  FIGURES ##################
      # And if the results already exist we can instead just load them, return a warning message if the file we want doesn't exist yet and stop the function
      if(INLA == 'load') 
      {
        # If the file doesn't exist we stop...
        if(file.exists(paste(direct,"Data/Survey_data/", yr, "/Survey_summary_output/",banks[i],"/", banks[i],"_figures_res_",s.res[1],"-",s.res[2], ".RData",sep="")) ==F)
        {
          stop(paste("Hello",as.character(Sys.info()["user"]),"You wanted to load the INLA results from the file... ", 
                     paste(direct,"Data/Survey_data/", yr, "/Survey_summary_output/",banks[i],"/", banks[i],"_figures_res_",s.res[1],"-",s.res[2], ".RData",sep=""), 
                     "which sadly doesn't exist, make it exist and I'll giver a try again for you. Cheers", 
                     as.character(Sys.info()["nodename"])))
        } # end if(file.exists(paste...
        #s.maps <- spatial.maps # Make sure I don't overwrite which map I'm making....
        load(file = paste(direct,"Data/Survey_data/", yr, "/Survey_summary_output/",banks[i],"/",banks[i],"_figures_res_",s.res[1],"-",s.res[2], ".RData",sep=""))
        #spatial.maps <- s.maps
      } # end if(INLA == 'load') 
      
      ####################### Spatial Maps####################### Spatial Maps####################### Spatial Maps####################### Spatial Maps
      ####################### Spatial Maps####################### Spatial Maps####################### Spatial Maps####################### Spatial Maps
      # This plots the spatial maps requested, need this m loop so we can plot only the figures requested for spatial plots (needed to avoid plotting
      # everything when you want the seedboxes plotted).
      if(length(spatial.maps) > 0 || any(plots %in% "user.SH.bins")) # first only run this section if we want a spatial map (this excludes the seedbox spatail maps (at the end of function)
      {
        # if we have user SH bins we need to plot those...
        if(any(plots %in% "user.SH.bins") ==T) 
        {
          n.maps <- length(spatial.maps) + num.bins # This will ensure we plot all spatial maps and user SH maps.
          maps.to.make <- c(spatial.maps,bin.names) # This will ensure we plot all spatial maps and user SH maps.
        } # end if(any(plots %in% "user.SH.bins") ==T) 
        
        # If we don't have user SH bins then this...
        if(any(plots %in% "user.SH.bins") ==F) 
        {
          n.maps <- length(spatial.maps) # This will plot only the spatial maps since we didn't ask for user SH maps.
          maps.to.make <- spatial.maps
        } # end if(any(plots %in% "user.SH.bins") ==F) 
        
        
        # basemap
        # want to draw the boundary on top of inla stuff, so load it in now
        bound.survey.sf <- github_spatial_import("survey_boundaries", "survey_boundaries.zip", direct_fns = direct_fns) %>%
          filter(ID==banks[i])
        # we only surveyed the northern portion of GBa so need to deal with that here... 
        if(banks[i] == "GBa"){
          north <- st_bbox(st_as_sf(data.frame(y=c(41.6, st_bbox(bound.survey.sf)$ymax[1]),
                                               x=c(st_bbox(bound.survey.sf)$xmin[1], st_bbox(bound.survey.sf)$xmax[1])),
                                    coords = c("x","y"),crs = 4326))
          
          bound.survey.sf <- bound.survey.sf %>%
            st_crop(north)
          
          extent <- data.frame(x=c(-67.15,-65.85), y=c(41.6, 42.30), crs=4326)
          
          p <- pecjector(area = extent, plot = F,direct_fns = direct_fns, repo=direct_fns, gis.repo=paste0(direct, "Data/Maps/approved"),
                         crs = st_crs(mesh$crs), quiet=T,
                         add_layer = list(eez="eez", bathy="ScallopMap"))
        }
        
        #  GBb and BBn are the entire banks though
        if(!banks[i] == "GBa") {
          p <- pecjector(area = banks[i],plot = F,direct_fns = direct_fns, repo=direct_fns, gis.repo=paste0(direct, "Data/Maps/approved"),
                       crs = st_crs(mesh$crs), quiet=T,
                       add_layer = list(eez="eez", bathy="ScallopMap"))
        }
        
        # the legends go in different spots depending on the bank.
        if(banks[i] == "BBn") {
          p <- p +
            theme(panel.grid=element_blank(), axis.ticks=element_line(),legend.position = c(1, 0), legend.box.just = "right", legend.justification = c(1,0))
        }
        if(!banks[i] == "BBn") {
          p <- p +
            theme(panel.grid=element_blank(), axis.ticks=element_line(),legend.position = c(0, 0), legend.box.just = "left", legend.justification = c(0,0))
        }
        # manually adjust the bathy lines
        p$layers[[2]]$aes_params$colour <- "blue"
        p$layers[[2]]$aes_params$alpha <- 0.25
        
       # Initialize a counter...
        count = 0
        # Make the maps...
        for(m in 1:n.maps)
        {
          
          # This is what we want for the spatial count maps
          if(maps.to.make[m]  %in% c("PR-spatial", "Rec-spatial", "FR-spatial")) 
          {
            base.lvls=c(0,1,5,10,50,100,500,1000,2000,5000,10000,20000,50000,100000)
            cols <- c(rev(viridis::plasma(length(base.lvls[base.lvls < 2000]),alpha=0.7,begin=0.6,end=1)),
                      rev(viridis::plasma(length(base.lvls[base.lvls > 1000])-1,alpha=0.8,begin=0.1,end=0.5)))
                      
            # Now set up the figure titles, different title depending on the spatial map here.
            if(maps.to.make[m]  == "FR-spatial")
            {
              fig.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
                                      list(a=as.character(CS),year=as.character(yr),bank=banks[i]))
            } # end if(maps.to.make[m]  == "FR-spatial")
            if(maps.to.make[m]  == "Rec-spatial")
            {
              fig.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
                                      list(a=as.character(CS-1),b=as.character(RS),year=as.character(yr),bank=banks[i]))

            } # end if(maps.to.make[m]  == "Rec-spatial")
            if(maps.to.make[m]  == "PR-spatial")
            {
              
              fig.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
                                      list(b=as.character(RS),year=as.character(yr),bank=banks[i]))
              }# end if(maps.to.make[m]  == "PR-spatial")
            # And the legend title
            leg.title <- N.tow.lab
          } # end if(maps.to.make[m]  %in% c("PR-spatial", "Rec-spatial", "FR-spatial") 
          
          if(maps.to.make[m]  %in% c("CF-spatial") & !banks[i] =="BBn")   
          {
            base.lvls <- c(0,5,8,10,12,14,16,18,max(c(20,ceiling(max(mod.res[[maps.to.make[m]]])))))
            cols <- rev(viridis::inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            # Get the levels correct add extra levels to this to get a better resolution if we have levels > 18
            if(median(mod.res[[maps.to.make[m]]], na.rm=T) > 18) 
            {
              byextra <- (round(max(mod.res[[maps.to.make[m]]], na.rm=T)) - max(base.lvls[-length(base.lvls)]))/3
              extra.lvls <- c(max(base.lvls[-length(base.lvls)]) + byextra,  max(base.lvls[-length(base.lvls)]) + byextra*2,  ceiling(max(mod.res[[maps.to.make[m]]])))
              extra.cols <- viridis::inferno(length(extra.lvls) + 1 ,alpha=0.7,begin=0.35,end=0.05)[-1]
              base.lvls <- c(base.lvls[-length(base.lvls)], extra.lvls)
              cols <- c(cols, extra.cols[-length(extra.cols)])
            }
            
            fig.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
                                    list(year=as.character(yr),bank=banks[i]))
            # if(banks[i] == "GB") fig.title <- substitute(bold(paste("Condition factor (", bank,"-Spr-",year,")",sep="")),
            #                                              list(year=as.character(yr),bank=banks[i]))
            leg.title <- cf.lab
            
          } # end if(maps.to.make[m]  %in% c("CF-spatial")   
          
          # Now for the meat count
          if(maps.to.make[m]  %in% c("MC-spatial") & !banks[i] =="BBn")
          {
            # The color ramps for MC
            base.lvls <- c(seq(0,50,5),60,80,100,200)
            cols <- viridis::viridis(length(base.lvls)-1,alpha=0.7,begin=0,end=1)
            # Get the levels correct            
            
            # Now set up the figure titles
            fig.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
                                    list(m=as.character(CS),year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-Spr-",year,")",sep="")),
                                                         list(m=as.character(CS),year=as.character(yr),bank=banks[i]))
            leg.title <- mc.lab
          } # end if(maps.to.make[m]  %in% c("MC-spatial")
          
          # Finally the Clappers
          if(maps.to.make[m]  %in% c("Clap-spatial"))
          {
            # The color ramp for Clapper proportion
            base.lvls=c(0,5,10,15,20,50,100)
            cols <- rev(viridis::plasma(length(base.lvls)-1,alpha=0.7))
            # Get the levels correct            
            # And get the labels for the figures...
            fig.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-",year,")",sep="")),
                                    list(c=as.character(RS),bank=banks[i],year=as.character(yr)))
            if(banks[i] == "GB") clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-Spr-",year,")",sep="")),
                                                              list(c=as.character(RS),bank=banks[i],year=as.character(yr)))
            leg.title <- "% Dead"
          } # end if(maps.to.make[m]  %in% c("Clap-spatial")
          
          if(maps.to.make[m]  %in% c("MW-spatial") & !banks[i] =="BBn")   
          {
            base.lvls <- c(0,2,4,6,8,10,12,14,16,18,20,30,50,100)
            cols <- rev(viridis::inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            
            fig.title <- substitute(bold(paste("Meat Weight (", bank,"-",year,")",sep="")),
                                    list(year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Meat Weight (", bank,"-Spr-",year,")",sep="")),
                                                         list(year=as.character(yr),bank=banks[i]))
            leg.title <- "Meat Weight (g)"
            
          } # end if(maps.to.make[m]  %in% c("MW-spatial")  
          
          if(maps.to.make[m]  %in% c("SH-spatial"))   
          {
            base.lvls <- c(0,50,70,80,90,100,110,120,150,200)
            cols <- rev(viridis::inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            # Get the levels correct            
            fig.title <- substitute(bold(paste("Shell Height (", bank,"-",year,")",sep="")),
                                    list(year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Shell Height (", bank,"-Spr-",year,")",sep="")),
                                                         list(year=as.character(yr),bank=banks[i]))
            leg.title <- "Shell Height (mm)"
            
          } # end if(maps.to.make[m]  %in% c("SH-spatial")  
          
          
          if(maps.to.make[m]  %in% c("SH.GP-spatial") & !banks[i] =="BBn")   
          {
            base.lvls <- c(0,0.05,0.1,0.2,0.3,0.5,0.75,1,2,10)
            cols <- rev(viridis::inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            # Figure title     
            fig.title <- substitute(bold(paste("Growth Potential SH  (", bank,"-",year,")",sep="")),
                                    list(year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Growth Potential SH (", bank,"-Spr-",year,")",sep="")),
                                                         list(year=as.character(yr),bank=banks[i]))
            leg.title <- "Growth Potential (SH)"
            
          } # end if(maps.to.make[m]  %in% c("SH.GP-spatial")  
          
          if(maps.to.make[m]  %in% c("MW.GP-spatial") & !banks[i] =="BBn")   
          {
            base.lvls <- c(0,0.05,0.1,0.2,0.3,0.5,0.75,1,2,10)
            cols <- rev(viridis::inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            fig.title <- substitute(bold(paste("Growth Potential MW  (", bank,"-",year,")",sep="")),
                                    list(year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Growth Potential MW (", bank,"-Spr-",year,")",sep="")),
                                                         list(year=as.character(yr),bank=banks[i]))
            leg.title <- "Growth Potential (MW)"
            
          } # end if(maps.to.make[m]  %in% c("SH.GP-spatial")  
          
          # Now for the user specified SH bins, if they exist...
          if(maps.to.make[m]  %in% bin.names) 
          {
            base.lvls=c(0,1,5,10,50,100,500,1000,2000,5000,10000,20000,50000,1e6)
            cols <- c(rev(viridis::plasma(length(base.lvls[base.lvls < 2000]),alpha=0.7,begin=0.6,end=1)),
                      rev(viridis::plasma(length(base.lvls[base.lvls > 1000])-1,alpha=0.8,begin=0.1,end=0.5)))
            
            if(length(grep("bm",maps.to.make[m])) > 0) # if we are looking at biomass figures...
            {
              base.lvls= c(0,0.005,0.01,0.05,0.1,0.5,1,2,5,10,20,50,1000)
              cols <- c(rev(viridis::magma(length(base.lvls[base.lvls < 1]),alpha=0.7,begin=0.6,end=1)),
                        rev(viridis::magma(length(base.lvls[base.lvls > 0.5])-1,alpha=0.8,begin=0.1,end=0.5)))
            } # end if(length(grep("bm",maps.to.make[m])) > 0) 
            
            # Now set up the figure titles, different title depending on the spatial map here.
            count = count + 1
            
            if(count > num.bins/2) count = 1 # This resets the counter to start over for the Biomass plots.
            # Now I can get a nice name for the figure
            if(count == 1) fig.title.start <- paste("Scallops 0-",user.bins[count]-1," mm (",sep="")
            if(count > 1 && count < num.bins/2) fig.title.start <- paste("Scallops ", user.bins[count-1] ,"-" , user.bins[count]-1," mm (",sep="")
            if(count == num.bins/2) fig.title.start <- paste("Scallops", user.bins[count-1],"+ mm (",sep="")
            
            fig.title <- substitute(bold(paste(fts, bank,"-",year,")",sep="")),
                                    list(a=as.character(CS),year=as.character(yr),bank=banks[i],fts = fig.title.start))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste(fts, bank,"-Spr-",year,")",sep="")),
                                                         list(a=as.character(CS),year=as.character(yr),bank=banks[i],fts = fig.title.start))
            leg.title <- N.tow.lab # Add a title for the legend.
            if(length(grep("bm",maps.to.make[m])) >0) leg.title <- B.tow.lab # If it is biomass then the legend needs the biomass title.
          } #end if(maps.to.make[m]  %in% bin.names) 
          

          
          ######## Produce the figure######## Produce the figure######## Produce the figure######## Produce the figure
          ######## Produce the figure######## Produce the figure######## Produce the figure######## Produce the figure
          # Do we want to save the figure to a file or just output to the screen?  
          if(fig == "png") png(paste(plot.dir,maps.to.make[m],".png",sep=""),units="in",width = 11,height = 8.5,res=420,bg = "transparent")
          if(fig == "pdf") pdf(paste(plot.dir,maps.to.make[m],".pdf",sep=""),width = 11,height = 8.5,bg = "transparent")
          if(fig == "screen") windows(11,8.5)
          
          # Now let's start off by making our base map, if we want to make this work for inshore then we'd need to figure out how to deal with the sfa piece
          # Here we add our layer to the object above.  This is going to become a list so we can save it and modify it outside Figures.
          if(!is.null(mod.res[[maps.to.make[m]]])){
            if(!banks[i] == "GBa"){
              p2 <- pecjector(gg.obj=p, area=banks[i], legend=T,
                              add_layer=list(NULL),
                              add_inla= list(field = mod.res[[maps.to.make[m]]],mesh = mesh, dims=s.res,clip = bound.poly.surv.sp,
                                             scale = list(scale = "discrete",breaks = base.lvls, palette = cols,leg.name=leg.title, alpha=0.75))) +
                geom_sf(data=bound.survey.sf, colour="black", fill=NA)
            }
            if(banks[i] == "GBa"){
              p2 <- pecjector(gg.obj=p, area=extent, legend=T,
                              add_layer=list(NULL),
                              add_inla= list(field = mod.res[[maps.to.make[m]]],mesh = mesh, dims=s.res,clip = bound.survey.sf,
                                             scale = list(scale = "discrete",breaks = base.lvls, palette = cols,leg.name=leg.title, alpha=0.75))) +
                geom_sf(data=st_crop(bound.survey.sf, north), colour="black", fill=NA)
            }
            
            # Don't add the titles?
            if(add.title == T)  p2 <- p2 + ggtitle(fig.title) + theme(plot.title = element_text(face = "bold",size=20, hjust=0.5))
          }
          
          ################ ENd produce the figure################ ENd produce the figure################ ENd produce the figure
          ################ ENd produce the figure################ ENd produce the figure################ ENd produce the figure
          
          ############  Add the points and the legend to the figure############  Add the points and the legend to the figure
          ############  Add the points and the legend to the figure############  Add the points and the legend to the figure
          ############  Add the points and the legend to the figure############  Add the points and the legend to the figure
          # Add the regular survey tows, note this if statement is used to NOT add the following code to these plots...
          if(maps.to.make[m] %in% c("PR-spatial", "Rec-spatial", "FR-spatial",bin.names, "SH-spatial", "Clap-spatial") | 
             (maps.to.make[m] == "SH.GP-spatial" & !banks[i] =="BBn"))
          {
            surv <- st_as_sf(surv.Live[[banks[i]]],coords = c('slon','slat'),crs = 4326,remove=F) %>% 
              dplyr::filter(year == yr & state == 'live')
            
            surv <- st_transform(surv,crs = st_crs(mesh$crs))#[2]$epsg)
            surv$`Tow type`[surv$random == 3] <- paste0('repeated (n = ',length(surv$random[surv$random==3]),")")
            # Get the shapes for symbols we want, this should do what we want for all cases we've ever experienced...
            if(length(unique(surv$`Tow type`)) ==1) shp <- 16
            
            #if(banks[i] == "Ger" & length(shp) == 2) shp <- c(21,15)
            
            # Make the plot
            p3 <- p2 + geom_sf(data=surv,aes(shape=`Tow type`),size=2) + scale_shape_manual(values = shp)# +
              #scale_x_continuous(expand = c(0.02, 0.02))
          }
          
          
          if(maps.to.make[m] %in% c("MW.GP-spatial","MW-spatial","CF-spatial","MC-spatial") & !banks[i]=="BBn")
          {
            surv <- st_as_sf(CF.current[[banks[i]]],coords = c('lon','lat'),crs = 4326)
            surv <- st_transform(surv,crs = st_crs(mesh$crs))#[1]$epsg)
            surv$`Tow type` <- paste0('detailed (n = ',nrow(surv),")")
            if(length(unique(surv$`Tow type`)) ==1) shp <- 16
            p3 <- p2 + geom_sf(data=surv,aes(shape=`Tow type`),size=2) + scale_shape_manual(values = shp) 
          }
          
          ## NEXT UP FIGURE OUT THE SEEDBOXES!
          
          # Finally add seedboxes as appropriate
          if(length(sb[,1]) > 0) 
          {
            sb[,c("X", "Y")] <- apply(sb[,c("X", "Y")], 2, function(x) as.numeric(x))
            sbs <- as.PolySet(sb, projection = "LL")
            sb.sf <- st_as_sf(PolySet2SpatialPolygons(sbs))
            sb.sf <- st_transform(sb.sf,crs = st_crs(loc.sf))
            p3 <- p3 + geom_sf(data= sb.sf,fill=NA,lwd=1)
          }

          # Now print the figure
          if(!is.null(mod.res[[maps.to.make[m]]])){
            print(p3)
          }
          if(save.gg == T) save(p3,file = paste0(direct,"Data/Survey_data/",yr,"/Survey_summary_output/",banks[i],"/",maps.to.make[m],".Rdata"))
          if(fig != "screen") dev.off()
        } # end for(m in 1:n.maps)  
      }# end if(length(spatial.maps) > 0) 
      
    }   # end if(length(spatial.maps > 0) || any(plots %in% "user.SH.bins"))
    # end if(banks[i] %in% c("BBn" ,"BBs" ,"Ger", "Mid", "Sab", "GB" ,"GBb", "GBa"))
    ############  END THE INLA FIGURES ############  END THE INLA FIGURES  ############  END THE INLA FIGURES
    ############  END THE INLA FIGURES############  END THE INLA FIGURES############  END THE INLA FIGURES
    
    
    #####  Set up the survey figure #############  Set up the survey figure #############  Set up the survey figure ########
    #####  Set up the survey figure #############  Set up the survey figure #############  Set up the survey figure ########
    
    #Do we want to plot the survey?
    if(any(plots %in% "Survey"))
    {
      if(fig == "png") png(paste(plot.dir,"/survey_strata.png",sep=""),units="in",width = 11, height = 11,res=420,bg = "transparent")
      if(fig == "pdf")  pdf(paste(plot.dir,"/survey_strata.pdf",sep=""),width = 11,height = 8.5)
      if(fig == "screen") windows(11,8.5)
      
      bound.survey.sf <- github_spatial_import("survey_boundaries", "survey_boundaries.zip", direct_fns = direct_fns) %>%
        filter(ID==banks[i])
      
      p <- pecjector(area = banks[i],plot = F,direct_fns = direct_fns, quiet=T, repo=direct_fns, gis.repo=paste0(direct, "Data/Maps/approved"),
                     add_layer = list(eez = 'eez', bathy = "ScallopMap", scale.bar = scale.bar)) 
      p <- p + 
        geom_sf(data=bound.survey.sf, colour="black", fill=NA, size=1.5)
      
      # manually adjust the bathy lines
      p$layers[[2]]$aes_params$colour <- "blue"
      p$layers[[2]]$aes_params$alpha <- 0.25
      #print(p)
      
      # For the banks with detailed strata...
      #shpf <- st_read(paste0(direct,"Data/Maps/approved/GIS_layers/offshore_survey_strata/",banks[i],".shp"))
      # For the ones we just have a cut of of the survey boundaries
      shpf <- st_read(paste0(direct,"Data/Maps/approved/GIS_layers/survey_boundaries/",banks[i],".shp"))
      # Use the cut out I make for the INLA models, it looks o.k.
      shpf <- st_transform(shpf,crs = st_crs(loc.sf))
      
      # Now get the points for the figure sorted out.
      surv <- st_as_sf(surv.Live[[banks[i]]],coords = c('slon','slat'),crs = 4326,remove=F) %>% 
        dplyr::filter(year == yr & state == 'live')
      
      surv <- st_transform(surv,crs = st_crs(loc.sf))
      surv$`Tow type`[surv$random == 3] <- paste0('repeated (n = ',length(surv$random[surv$random==3]),")")
      # Get the shapes for symbols we want, this should do what we want for all cases we've ever experienced...
      if(length(unique(surv$`Tow type`)) ==1) shp <- 16
      
      # For this figure we want full bank names, this is ugly hack but does the trick.
      if(add.title == T)
      {    
        full.names <- data.frame(abrv = c("BBn","GBa","GBb"),
                                 full = c("Browns Bank North","Georges Bank 'a'","Georges Bank 'b'"))
        
        survey.title <- substitute(bold(paste("Survey (",bank," ",year,")",sep="")),
                                   list(year=as.character(yr),bank=as.character(full.names$full[full.names$abrv == banks[i]])))
        p <- p + ggtitle(survey.title)
      }
      
      # That's all we need for the areas without survey strata, pretty easy! No fill on strata
      p2 <- p + geom_sf(data=shpf,fill =NA) + geom_sf(data=surv,aes(shape=`Tow type`),size=2) + scale_shape_manual(values = shp) 
      
      # if(banks[i] %in% c("BBn" ,"BBs","Sab", "GBb", "GBa"))
      # {
      #   # Need to get the factor levels correct, always annoying...
      #   #Tows per strata is easy...
      #   if(banks[i] != "Sab" && banks[i] != "BBs") shpf$tow_num <- as.character(table(surv$Strata_ID))
      #   if(banks[i] == "Sab") shpf$tow_num <- as.character(table(surv$Strata_ID_new))
      #   # Because we never tow in that one tiny strata...
      #   if(banks[i] == "BBs") 
      #   {
      #     tmp <- as.character(table(surv$Strata_ID))
      #     if(length(tmp) == 3) tmp <- c(tmp[1],0,tmp[2:3])
      #     shpf$tow_num <- tmp
      #   } # eend if(banks[i] == "BBs") 
      #   
      #   shpf$are_km2 <- round(shpf$are_km2)
      #   shpf$`Number of Tows` <-  factor(shpf$tow_num, levels = shpf$tow_num)
      #   shpf$`Area (km^2)` <- factor(shpf$are_km2,levels = shpf$are_km2)
      #   
      #   shpf$ID <-  factor(shpf$PName, levels = shpf$PName)
      #   #shpf$Details <- paste0(shpf$PName,"                    Number of Tows = ", 
      #   #                      shpf$tow_num, "                 Area = ",   
      #   #                       shpf$are_km2,' km^2')
      #   #shpf$Details <-  factor(shpf$Details, levels = shpf$Details)
      #   cols <- unique(shpf$col)
      #   
      #   p2 <- p  + #geom_sf(data=shpf,aes(fill= Details))    +  scale_fill_manual(values = cols) + 
      #     geom_sf(data=shpf,aes(linetype = `Number of Tows`))  + 
      #     geom_sf(data=shpf,aes(colour = `Area (km^2)`))  +
      #     new_scale("fill") + geom_sf(data=shpf,aes(fill= ID))    +  
      #     geom_point(data=surv,aes(lon, lat, shape=`Tow type`),size=2) + scale_shape_manual(values = shp) + 
      #     #taking advantage of OTHER aes types and then overriding them with fill (hacky but it works):
      #     scale_fill_manual(values = cols, guide=guide_legend(override.aes = list(fill= cols)))  +
      #     scale_colour_manual(values = rep("black", length(cols)), guide=guide_legend(override.aes = list(fill= cols)))  +
      #     scale_linetype_manual(values = rep("solid", length(cols)), guide=guide_legend(override.aes = list(fill= cols)))  +
      #     theme(legend.position = 'right',legend.direction = 'vertical',
      #           legend.justification = 'left',legend.key.size = unit(.5,"line"))
      # } # end  if(banks[i] %in% c("BBn" ,"BBs","Sab", "GBb", "GBa"))
      # Finally add seedboxes as appropriate
      if(length(sb[,1]) > 0) 
      {
        sb[,c("X", "Y")] <- apply(sb[,c("X", "Y")], 2, function(x) as.numeric(x))
        sbs <- as.PolySet(sb, projection = "LL")
        sb.sf <- st_as_sf(PolySet2SpatialPolygons(sbs))
        sb.sf <- st_transform(sb.sf,crs = st_crs(loc.sf))
        p2 <- p2 + geom_sf(data= sb.sf,fill=NA,lwd=1)
      }
      # }
      if(save.gg == T) save(p3,file = paste0(direct,"Data/Survey_data/",yr,"/Survey_summary_output/",banks[i],"/Survey.Rdata"))
      print(p2)
      if(fig != "screen") dev.off()
      
    } # end if(length(plots[grep("Survey",plots)]>0))
    
    ####################################  END SURVEY PLOT ####################################  END SURVEY PLOT
    
    ####################################  END SPATIAL PLOTS ####################################  END SPATIAL PLOTS
    ####################################  END SPATIAL PLOTS ####################################  END SPATIAL PLOTS
    
    
    
    
    ####################################  MWSH and CF Time series plot #################################### 
    ####################################  MWSH and CF Time series plot ####################################       
    ####################################  MWSH and CF Time series plot #################################### 
    if(any(plots == "MW-SH") & !banks[i] == "BBn")
    {
      
      MWSH.title <- substitute(bold(paste("MW-SH Relationship (",bank,"-",year,")",sep="")),
                               list(year=as.character(yr),bank=banks[i]))
      CF.ts.title <- substitute(bold(paste("Condition factor time series (",bank,")",sep="")),
                                list(year=as.character(yr),bank=banks[i]))

      # Don't add the titles
      if(add.title == F) MWSH.title <- ""
      if(add.title == F) CF.ts.title <- ""
      
      # Because name of BanIce is longer than other banks I added this so the figure title doesn't go off screen.
      cap.size <- 1.9
      
      ############
      #Source12 Meat Height Shell weight plot on Slide 13  source("fn/shwt.plt1.r") 
      # using grid to combine ggplot with base plot
      require(grid)
      
      # prep the cf ts figure first (not like we usually do!)
      # set up for paired comparisons
      survey.obj[[banks[i]]][[1]]$plot.year <- ifelse(survey.obj[[banks[i]]][[1]]$year %in% c("2018", "2019"), survey.obj[[banks[i]]][[1]]$year, 2020)
      
      comp <- survey.obj[[banks[i]]][[1]] %>%
        pivot_longer(cols=I:CF) %>%
        subset(name %in% c("CF")) 
      
      ndetailed <- CF.current[[banks[i]]] %>%
        group_by(year=as.numeric(year)) %>%
        tally(name = "ndetailed")
      
      ndetailed$compyear <- ifelse(grepl(x = ndetailed$year, 19), 2019, 2018)
      comp$compyear <- ifelse(grepl(x = comp$year, 19), 2019, 2018)
      
      comp <- left_join(comp, select(ndetailed, -year))
      
      cf <- ggplot() +  geom_point(data=comp, aes(plot.year, value,  colour=paste0(as.factor(compyear), " (n=", ndetailed, ")"))) + 
        geom_line(data=comp, aes(plot.year, value, group=as.factor(compyear), colour=paste0(as.factor(compyear), " (n=", ndetailed, ")"))) +
        theme_bw() +
        scale_colour_manual(values=c("blue", "red"), name="Repeat year") +
        scale_x_continuous(breaks=c(2018, 2019, 2020), name = "Survey year") +
        ylab(cf.lab) +
        ylim(4,25)+
        theme(panel.grid=element_blank(), 
              strip.text = element_text(hjust=0, size=14), 
              strip.background = element_rect(colour=NA, fill=NA),
              axis.title.y = element_text(angle = 0, vjust=0.5, size=18), 
              axis.title.x = element_text(vjust=0,size=18), 
              legend.position=c(.8,.2),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              plot.title = element_text(size=22, hjust=0.5),
              axis.text = element_text(size=12)) +
        ggtitle(CF.ts.title)
      
      vp.Right <- viewport(height=unit(1, "npc"), width=unit(0.5, "npc"), 
                                 just=c("left"), 
                                 y=0.5, x=0.5)
      
      if(fig == "screen") windows(15,8)
      if(fig == "png") png(paste(plot.dir,"/MWSH_and_CF_ts.png",sep=""),
                           units="in",width = 13,height = 8.5,res=420,bg = "transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"/MWSH_and_CF_ts.pdf",sep=""),width = 13,height = 8.5)
      
      par(mfrow=c(1,2))
      shwt.plt1(SpatHtWt.fit[[banks[i]]],lw=3,ht=10,wd=12,cx=1.5,titl = MWSH.title,cex.mn = cap.size,las=1)
      
      print(cf, vp=vp.Right)
      
      dev.off()
      
      if(length(which(!is.na(survey.obj[[banks[i]]][[1]]$CF))) > 3){
        stdts.plt(survey.obj[[banks[i]]][[1]],x=c('year'),y=c('CF'),pch=16,ylab=cf.lab,las=1,col=c("blue"),
                  median.line=T,graphic='none',xlab='Year',ylim=c(4,25),titl=CF.ts.title,cex.mn=cap.size)
      }
      if(length(which(!is.na(survey.obj[[banks[i]]][[1]]$CF))) < 4){
        stdts.plt(survey.obj[[banks[i]]][[1]],x=c('year'),y=c('CF'),pch=16,ylab=cf.lab,las=1,col=c("blue"),
                  median.line=F,graphic='none',xlab='Year',ylim=c(4,25),titl=CF.ts.title,cex.mn=cap.size)
      }
      #legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
      if(fig != "screen") dev.off()
      
    } # end if(any(plots=="MW-SH"))
    ############  END THE MW SHELL HEIGHT FIGURE ###################  END THE MW SHELL HEIGHT FIGURE #######
    
    #####   THE ABUNDANCE TIME SERIES FIGURE #####   THE ABUNDANCE TIME SERIES FIGURE#####   THE ABUNDANCE TIME SERIES FIGURE
    #####   THE ABUNDANCE TIME SERIES FIGURE #####   THE ABUNDANCE TIME SERIES FIGURE#####   THE ABUNDANCE TIME SERIES FIGURE      
    if(any(plots=="abund-ts"))
    {
      
      # set up for paired comparisons
      survey.obj[[banks[i]]][[1]]$plot.year <- ifelse(survey.obj[[banks[i]]][[1]]$year %in% c("2018", "2019"), survey.obj[[banks[i]]][[1]]$year, 2020)
      
      survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
                                      list(year=as.character(yr),bank=banks[i]))
      
      if(add.title == F) survey.ts.N.title <- ""
      
      # abundance for all banks!
      comp <- survey.obj[[banks[i]]][[1]] %>%
        pivot_longer(cols=I:CV_120_plus)
      
      if(se==F){
        comp <- comp %>% subset(name %in% c("N", "NR", "NPR")) 
      }
      if(se==T){
        comp.cv <- comp %>% subset(name %in% c("N.cv", "NR.cv", "NPR.cv")) %>%
          rename(cv.value=value)
        comp.cv$name <- str_replace(comp.cv$name, pattern=".cv", replacement="")
        comp <- comp %>% subset(name %in% c("N", "NR", "NPR")) 
        comp <- full_join(comp, comp.cv) %>% select(year, n, plot.year, name, value, cv.value)
      }
      
      comp$name <- factor(comp$name, levels=c("NPR", "NR", "N"))
      levels(comp$name) <- c(paste0("Pre-recruits (<", unique(comp$RS), " mm)"),
                             paste0("Recruits (", unique(comp$RS), "-", unique(comp$CS)-1, " mm)"),
                             paste0("Fully recruited (\u2265", unique(comp$CS), " mm)"))
      comp$compyear <- ifelse(grepl(x = comp$year, 19), 2019, 2018)

      abundB <- ggplot() + geom_point(data=comp, aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), value), size=3) + 
        geom_text_repel(data=comp, aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), 
                                       value, label=as.factor(plot.year)), size=5,
                          point.padding = 0.75, min.segment.length = 0.1, nudge_x=0.15, arrow=arrow(length=unit(0.05, "inches"))) + 
        facet_wrap(~name, ncol=1, scales="free_y") + 
        theme_bw() +
        theme(panel.grid=element_blank(), 
              strip.text = element_text(hjust=0, size=14), 
              axis.title=element_text(size=14), 
              axis.text=element_text(size=12),
              axis.title.y = element_text(angle=0, vjust=0.5), 
              strip.background = element_rect(colour=NA, fill=NA),
              plot.title = element_text(size=22, hjust=0.5)) +
        xlab("Repeat year") +
        ylab(substitute(paste("",frac(N,tow),),list(N="N",tow="tow"))) +
        ggtitle(survey.ts.N.title)
      
      if(se==T) abundB <- abundB + 
        geom_errorbar(data=comp, aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), ymin=value-(value*cv.value), ymax=value+(value*cv.value)), width=0.05) 
        
      
      if(banks[i] == "GBb" & se==F) {
        source(paste0(direct_fns, "/Other_functions/scale_override.R"))
        abundB <- abundB + facet_wrap_custom(~name, ncol=1, scales="free_y", 
                                   scale_overrides = list(
                                     scale_override(which = 1, 
                                                    scale = scale_y_continuous(breaks=seq(0,300,50), 
                                                                               limits=c(0,300))),
                                     scale_override(which = 2, 
                                                    scale = scale_y_continuous(breaks=seq(0,200,50), 
                                                                               limits=c(0,200))),
                                     scale_override(which = 3, 
                                                    scale = scale_y_continuous(breaks=seq(0,600,100), 
                                                                               limits=c(0,600)))
                                   ))
      }
      
      
      if(banks[i] == "BBn" & se==F) {
        source(paste0(direct_fns, "/Other_functions/scale_override.R"))
        abundB <- abundB + facet_wrap_custom(~name, ncol=1, scales="free_y", 
                                             scale_overrides = list(
                                               scale_override(which = 1, 
                                                              scale = scale_y_continuous(breaks=seq(0,900,150), 
                                                                                         limits=c(0,900))),
                                               scale_override(which = 2, 
                                                              scale = scale_y_continuous(breaks=seq(0,30,5), 
                                                                                         limits=c(0,30))),
                                               scale_override(which = 3, 
                                                              scale = scale_y_continuous(breaks=seq(0,200,50), 
                                                                                         limits=c(0,200)))
                                             ))
      }
      
      if(banks[i] == "GBa" & se==F) {
        source(paste0(direct_fns, "/Other_functions/scale_override.R"))
        abundB <- abundB + facet_wrap_custom(~name, ncol=1, scales="free_y", 
                                             scale_overrides = list(
                                               scale_override(which = 1, 
                                                              scale = scale_y_continuous(breaks=seq(0,500,100), 
                                                                                         limits=c(0,500))),
                                               scale_override(which = 2, 
                                                              scale = scale_y_continuous(breaks=seq(0,100,20), 
                                                                                         limits=c(0,100))),
                                               scale_override(which = 3, 
                                                              scale = scale_y_continuous(breaks=seq(0,500,100), 
                                                                                         limits=c(0,500)))
                                             ))
      }
      
      if(fig == "screen") windows(8.5,11)
      if(fig == "png") png(paste(plot.dir,"/abundance_ts.png",sep=""),units="in",
                           width = 8.5, height = 11,res=300,bg="transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"/abundance_ts.pdf",sep=""),width = 8.5, height = 11)
      
      print(abundB)
      
      if(fig != "screen") dev.off()
      
    }# end if(any(plots=="abund-ts"))
    
    #####  END THE ABUNDANCE TIME SERIES FIGURE #####  END THE ABUNDANCE TIME SERIES FIGURE#####  END THE ABUNDANCE TIME SERIES FIGURE
    #####  END THE ABUNDANCE TIME SERIES FIGURE #####  END THE ABUNDANCE TIME SERIES FIGURE#####  END THE ABUNDANCE TIME SERIES FIGURE
    
    
    #####   THE BIOMASS TIME SERIES FIGURE #####   THE BIOMASS TIME SERIES FIGURE#####   THE BIOMASS TIME SERIES FIGURE
    #####   THE BIOMASS TIME SERIES FIGURE #####   THE BIOMASS TIME SERIES FIGURE#####   THE BIOMASS TIME SERIES FIGURE      
    
    if(any(plots =="biomass-ts") & !banks[i] =="BBn")
    {
      
      # set up for paired comparisons
      survey.obj[[banks[i]]][[1]]$plot.year <- ifelse(survey.obj[[banks[i]]][[1]]$year %in% c("2018", "2019"), survey.obj[[banks[i]]][[1]]$year, 2020)
      
      survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
                                       list(bank=banks[i]))
      if(banks[i] == "GB") survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,"-Spr)",sep="")),
                                                            list(year=as.character(yr),bank=banks[i]))
      if(add.title == F) survey.ts.BM.title <- ""
      
      if(fig == "screen") windows(8.5,11)
      
      if(fig == "png") png(paste(plot.dir,"/biomass_ts.png",sep=""),
                           units="in",width = 8.5, height = 11,res=420,bg="transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"/biomass_ts.pdf",sep=""),width = 8.5, height = 11)
      
      # abundance for all banks!
      comp <- survey.obj[[banks[i]]][[1]] %>%
        pivot_longer(cols=I:CV_120_plus)  
      
      if(se==F){
        comp <- comp %>% subset(name %in% c("I", "IR", "IPR")) 
      }
      if(se==T){
        comp.cv <- comp %>% subset(name %in% c("I.cv", "IR.cv", "IPR.cv")) %>%
          rename(cv.value=value)
        comp.cv$name <- str_replace(comp.cv$name, pattern=".cv", replacement="")
        comp <- comp %>% subset(name %in% c("I", "IR", "IPR")) 
        comp <- full_join(comp, comp.cv) %>% select(year, n, plot.year, name, value, cv.value)
      }

      comp$name <- factor(comp$name, levels=c("IPR", "IR", "I"))
      levels(comp$name) <- c(paste0("Pre-recruits (<", unique(comp$RS), " mm)"),
                             paste0("Recruits (", unique(comp$RS), "-", unique(comp$CS)-1, " mm)"),
                             paste0("Fully recruited (\u2265", unique(comp$CS), " mm)"))
      comp$compyear <- ifelse(grepl(x = comp$year, 19), 2019, 2018)
      
      require(ggrepel)
      biomass.ts <- ggplot() + geom_point(data=comp, aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), value/1000), size=3) + 
        geom_text_repel(data=comp, 
                        aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), value/1000, label=as.factor(plot.year)), size=5, 
                        point.padding = 0.75, min.segment.length = 0.1, nudge_x=0.15, arrow=arrow(length=unit(0.05, "inches"))) +  
        facet_wrap(~name, ncol=1, scales="free_y") +
        theme_bw() +
        theme(panel.grid=element_blank(), 
              strip.text = element_text(hjust=0, size=14), 
              axis.title=element_text(size=14), 
              axis.text=element_text(size=12),
              axis.title.y = element_text(angle=0, vjust=0.5), 
              strip.background = element_rect(colour=NA, fill=NA),
              plot.title = element_text(size=22, hjust=0.5)) +
        xlab("Repeat year") +
        ylab(substitute(paste("",frac(kg,tow),),list(N="kg",tow="tow"))) +
        ggtitle(survey.ts.BM.title) 
      
      if(se==T) biomass.ts <- biomass.ts + 
        geom_errorbar(data=comp, aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), ymin=(value-(value*cv.value))/1000, ymax=(value+(value*cv.value))/1000), width=0.05) 
      
      if(banks[i] == "GBb" & se==F) {
        source(paste0(direct_fns, "/Other_functions/scale_override.R"))
        biomass.ts <- biomass.ts + facet_wrap_custom(~name, ncol=1, scales="free_y", 
                                   scale_overrides = list(
                                     scale_override(which = 1, 
                                                    scale = scale_y_continuous(breaks=seq(0,1.5,.25), 
                                                                               limits=c(0,1.5))),
                                     scale_override(which = 2, 
                                                    scale = scale_y_continuous(breaks=seq(0,1.5,.25), 
                                                                               limits=c(0,1.5))),
                                     scale_override(which = 3, 
                                                    scale = scale_y_continuous(breaks=seq(0,10,1.5), 
                                                                               limits=c(0,10)))
                                   ))
      }
      
      if(banks[i] == "GBa" & se==F) {
        source(paste0(direct_fns, "/Other_functions/scale_override.R"))
        biomass.ts <- biomass.ts + facet_wrap_custom(~name, ncol=1, scales="free_y", 
                                                     scale_overrides = list(
                                                       scale_override(which = 1, 
                                                                      scale = scale_y_continuous(breaks=seq(0,1.2,.3), 
                                                                                                 limits=c(0,1.2))),
                                                       scale_override(which = 2, 
                                                                      scale = scale_y_continuous(breaks=seq(0,1.2,.3), 
                                                                                                 limits=c(0,1.2))),
                                                       scale_override(which = 3, 
                                                                      scale = scale_y_continuous(breaks=seq(0,12.5,3), 
                                                                                                 limits=c(0,12.5)))
                                                     ))
      }
      
      
      
      print(biomass.ts)
      
      if(fig != "screen") dev.off()
      
    } # end if(any(plots=="biomass-ts"))
    
    
    #####   END THE BIOMASS TIME SERIES FIGURE #####   END THE BIOMASS TIME SERIES FIGURE#####   END THE BIOMASS TIME SERIES FIGURE
    #####   END THE BIOMASS TIME SERIES FIGURE #####   END THE BIOMASS TIME SERIES FIGURE#####   END THE BIOMASS TIME SERIES FIGURE      
    
    
    #####   THE USER SPECIFIED BINS TIME SERIES FIGURES #####   #####   THE USER SPECIFIED BINS TIME SERIES FIGURES ##### 
    #####   THE USER SPECIFIED BINS TIME SERIES FIGURES #####   #####   THE USER SPECIFIED BINS TIME SERIES FIGURES ##### 
    
    if(any(plots == "user.SH.bins"))
    {
      survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,")",sep="")),
                                      list(year=as.character(yr),bank=banks[i]))
      if(banks[i] == "GB") survey.ts.N.title <- substitute(bold(paste("Survey abundance time series (",bank,"-Spr)",sep="")),
                                                           list(year=as.character(yr),bank=banks[i]))
      
      
      if(add.title == F) survey.ts.N.title <- ""
      if(fig == "screen") windows(8.5,11)
      if(fig == "png") png(paste(plot.dir,"/abundance_user_SH_bins_ts.png",sep=""),units="in",
                           width = 8.5, height = 11,res=420,bg="transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"/abundance_user_SH_bins_ts.pdf",sep=""),width = 8.5, height = 11)
      
      par(mfrow=c(1,1))
      if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "Ban" && banks[i] != "BanIce" && banks[i] != "GB")
      {
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F, 
                  areas=surv.info$towable_area,clr=c('blue',"blue","darkgrey"),se=T,pch=16,ys=1.3,
                  add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5,user.bins = user.bins)
      }# end if(banks[i] != "Ger" && banks[i] != "Mid")
      # For german bank
      if(banks[i] == "Ger")
      {
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F, 
                  ymin=-5,dat2=merged.survey.obj,clr=c('blue','red',"blue"),pch=c(16,17),se=T,ys=1.3,
                  add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5,user.bins = user.bins, plot.which.bins = c(1,2,3))
        legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.4,lty=c(1,2),col=c("blue","red"),bty="n")
      } # end if(banks[i] == "Ger")
      if(banks[i] == "Mid" || banks[i] == "GB"|| banks[i] == "Ban" || banks[i] == "BanIce")
      {
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F, user.bins = user.bins,
                  ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl =survey.ts.N.title,cx.mn=3,axis.cx = 1.5,ys=1.3)
      } # end if(banks[i] == "Mid")
      if(fig != "screen") dev.off()
      ##########  Now the biomass plots of the same thing  ##################
      
      survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,")",sep="")),
                                       list(bank=banks[i]))
      if(banks[i] == "GB") survey.ts.BM.title <- substitute(bold(paste("Survey biomass time series (",bank,"-Spr)",sep="")),
                                                            list(year=as.character(yr),bank=banks[i]))
      if(add.title == F) survey.ts.BM.title <- ""
      
      if(fig == "screen") windows(11,8.5)
      if(fig == "png") png(paste(plot.dir,"/biomass_user_SH_bins_ts.png",sep=""),
                           units="in",width = 8.5, height = 11,res=420,bg="transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"/biomass_user_SH_bins_ts.pdf",sep=""),width = 8.5, height = 11)
      
      if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "Ban" && banks[i] != "BanIce" && banks[i] != "GB" && banks[i] != "Sab")
      {
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,type='B', 
                  areas=surv.info$towable_area,clr=c('blue',"blue","darkgrey"),se=T,pch=16,ys=1.3,
                  add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5,user.bins=user.bins)
      } # end if(banks[i] != "Ger")
      
      if(banks[i] == "Sab")
      {
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,type='B', 
                  areas=surv.info$towable_area,clr=c('blue',"blue","darkgrey"),se=T,pch=16,ys=1.4,
                  add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5,user.bins=user.bins)
      } # end if(banks[i] = "Sab")
      
      if(banks[i] == "Ger")
      {
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F,type='B', 
                  dat2=merged.survey.obj,clr=c('blue','red',"blue"),se=T,pch=c(16,17),ys=1.3,
                  add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx=1.5,user.bins=user.bins)
        legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.4,lty=c(1,2),col=c("blue","red"),bty="n")
      } # end if(banks[i] == "Ger")
      if(banks[i] == "Mid"|| banks[i] == "GB" || banks[i] == "Ban" || banks[i] == "BanIce")
      {
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,  type='B',user.bins=user.bins,
                  ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5,ys=1.3)
      } # end if(banks[i] == "Mid")
      if(fig != "screen") dev.off()
      
    }# end if(any(plots %in% "user.SH.bins"))
    
    #####   END THE USER SPECIFIED BINS TIME SERIES FIGURES #####   #####   END THE USER SPECIFIED BINS TIME SERIES FIGURES #####   
    
    
    ##### Shell height frequency figure            ##### Shell height frequency figure ##### Shell height frequency figure
    ##### Shell height frequency figure            ##### Shell height frequency figure ##### Shell height frequency figure      
    ##### Shell height frequency figure            ##### Shell height frequency figure ##### Shell height frequency figure
    
    if(any(plots=="SHF"))
    {
      SHF.title <-  substitute(paste("Shell height frequency (",bank,")",sep=""),
                               list(bank=banks[i]))  
     
      if(add.title == F) SHF.title <- ""
    
      SHF <- as.data.frame(survey.obj[[banks[i]]][[2]]$n.yst)
      SHF$year <- SHF$years
      if(!any(SHF$year == survey.obj[[banks[i]]][[1]]$year)) {stop("years are mixed up!")}
      else
      SHF$n <- survey.obj[[banks[i]]][[1]]$n
      SHF <- SHF %>%
        mutate(compyear = ifelse(grepl(x=.$year, 19), 2019, 2018),
               plot.year = ifelse(.$year %in% c("2018", "2019"), .$year, 2020)) %>%
        pivot_longer(cols=`5`:`200`) %>%
        mutate(plot.year = paste0(plot.year, " (n=", n, ")"))
      
      ymax <- round(ceiling(max(SHF$value) * 1.05), -1)
      
      if(!banks[i]=="GBb"){
        if(add.title == F) SHF.title <- ""
        if(fig == "screen") windows(11,11)
        if(fig == "png") {
          png(paste(plot.dir,"/SHF.png",sep=""),units="in",width = 11, 
              height = 11,res=420,bg="transparent")
        }
        if(fig == "pdf") pdf(paste(plot.dir,"/SHF.pdf",sep=""),width = 11, height = 11)
        
        labs <- unique(dplyr::select(SHF[SHF$compyear==2018,], plot.year))
        P2018 <- ggplot() + geom_bar(data=SHF[SHF$compyear==2018,], aes(as.numeric(name)+2.5, value), stat="identity", fill="grey80",
                                     colour="black", width=5) + 
          facet_wrap(~plot.year, scales="free_x", nrow=2) +
          geom_text(data=labs, aes(175, ymax*0.9, label=plot.year), size=5)+
          geom_vline(data=SHF[SHF$compyear==2018,], aes(xintercept=RS))+
          geom_vline(data=SHF[SHF$compyear==2018,], aes(xintercept=CS))+
          theme_bw() +
          theme(panel.grid=element_blank(), 
                strip.text=element_blank(), 
                axis.title=element_text(size=14), 
                axis.text=element_text(size=12),
                strip.background = element_rect(colour=NA, fill=NA), 
                axis.text.x = element_text(colour=rep(c("black", NA, NA, NA), 40)), 
                axis.title.y = element_text(angle=0, vjust=0.5), 
                plot.title = element_text(hjust=0.5, size=16)) +
          scale_x_continuous(breaks=seq(5, 200, 5)) +
          scale_y_continuous(limits = c(0, ymax)) +
          xlab("\nShell height (mm)") + 
          ylab(substitute(paste("",frac(N,tow),),list(N="N",tow="tow"))) +
          ggtitle("2018 Repeats")
        
        labs <- unique(dplyr::select(SHF[SHF$compyear==2019,], plot.year))
        P2019 <- ggplot() + geom_bar(data=SHF[SHF$compyear==2019,], aes(as.numeric(name)+2.5, value), stat="identity", fill="grey80", 
                                     colour="black", width=5) + 
          geom_text(data=labs, aes(175, ymax*0.9, label=plot.year), size=5)+
          geom_vline(data=SHF[SHF$compyear==2019,], aes(xintercept=RS))+
          geom_vline(data=SHF[SHF$compyear==2019,], aes(xintercept=CS))+
          facet_wrap(~plot.year, scales="free_x", nrow=2) +
          theme_bw() +
          theme(panel.grid=element_blank(), 
                strip.text=element_blank(), 
                axis.title=element_text(size=14), 
                axis.text=element_text(size=12),
                strip.background = element_rect(colour=NA, fill=NA), 
                axis.text.x = element_text(colour=rep(c("black", NA, NA, NA), 40)), 
                axis.title.y = element_text(angle=0, vjust=0.5), 
                plot.title = element_text(hjust=0.5, size=16)) +
          scale_x_continuous(breaks=seq(5, 200, 5)) +
          scale_y_continuous(limits = c(0, ymax)) +
          xlab("Shell height (mm)") + 
          ylab(NULL)+ #substitute(paste("",frac(N,tow),),list(N="N",tow="tow")))+
          ggtitle("2019 Repeats")
        
        require(patchwork)
        print(
          P2018 + P2019 + plot_annotation(title = SHF.title, 
                                          theme = theme(plot.title=element_text(face = "bold", hjust=0.5, size=22)))
        )
      }
      
      if(banks[i] == "GBb"){
        if(add.title == F) SHF.title <- ""
        if(fig == "screen") windows(8.5,11)
        if(fig == "png") {
          png(paste(plot.dir,"/SHF.png",sep=""),units="in",width = 8.5, 
              height = 11,res=420,bg="transparent")
        }
        if(fig == "pdf") pdf(paste(plot.dir,"/SHF.pdf",sep=""),width = 8.5, height = 11)
        
        labs <- unique(dplyr::select(SHF[SHF$compyear==2019,], plot.year))
        P2019 <- ggplot() + geom_bar(data=SHF[SHF$compyear==2019,], aes(as.numeric(name)+2.5, value), stat="identity", fill="grey80", 
                                     colour="black", width=5) + 
          geom_text(data=labs, aes(175, ymax*0.9, label=plot.year))+
          geom_vline(data=SHF[SHF$compyear==2019,], aes(xintercept=RS))+
          geom_vline(data=SHF[SHF$compyear==2019,], aes(xintercept=CS))+
          facet_wrap(~plot.year, scales="free_x", nrow=2) +
          theme_bw() +
          theme(panel.grid=element_blank(), 
                strip.text=element_blank(), 
                axis.title=element_text(size=14), 
                axis.text=element_text(size=12), 
                strip.background = element_rect(colour=NA, fill=NA), 
                axis.text.x = element_text(colour=rep(c("black", NA, NA, NA), 40)), 
                axis.title.y = element_text(angle=0, vjust=0.5), 
                plot.title = element_text(hjust=0.5, size=22)) +
          scale_x_continuous(breaks=seq(5, 200, 5)) +
          scale_y_continuous(limits = c(0, ymax)) +
          xlab("Shell height (mm)") + 
          ylab(substitute(paste("",frac(N,tow),),list(N="N",tow="tow")))+
          ggtitle("2019 Repeats")
        print(P2019)
      }
      
      if(fig != "screen") dev.off()
    } # end if(any(plots=="SHF"))    
    
    ##### END Shell height frequency figure   ##### END Shell height frequency figure ##### END Shell height frequency figure
    ##### END Shell height frequency figure   ##### END Shell height frequency figure ##### END Shell height frequency figure
    
    
    ##### Shell height frequency Large Sizes      ##### Shell height frequency Large Sizes ##### Shell height frequency Large Sizes 
    ##### Shell height frequency Large Sizes      ##### Shell height frequency Large Sizes ##### Shell height frequency Large Sizes   
    
    if(any(plots=="SHF-large"))
    {
      
      SHF.title <-  substitute(bold(paste("Shell height frequency (","" >b," mm - ",bank,")",sep="")),
                               list(bank=banks[i],b=65))  
      
      if(add.title == F) SHF.title <- ""
      if(fig == "screen") windows(8.5,11)
      if(fig == "png") png(paste(plot.dir,"/SHF-large.png",sep=""),units="in",width = 8.5, height = 11,res=420,bg="transparent")
      
      if(fig == "pdf") pdf(paste(plot.dir,"/SHF-large.pdf",sep=""),width = 8.5, height = 11)
      # Grab the last 7 years of data
      
      shf.years <- min(survey.obj[[banks[i]]][[1]]$year):max(survey.obj[[banks[i]]][[1]]$year)
      s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
      shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F, select=70, rows=3,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T)	
      if(fig != "screen") dev.off()
      
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
      if(fig == "png") png(paste(plot.dir,"SHF-split.png",sep=""),units="in",width = 8.5,  height = 11,res=420,bg="transparent")
      
      if(fig == "pdf") pdf(paste(plot.dir,"SHF-split.pdf",sep=""),width = 8.5, height = 11)
      # Grab the last 7 years of data
      
      if(banks[i] != "Ger")
      {
        shf.years <- survey.obj[[banks[i]]][[1]]$year[(length(survey.obj[[banks[i]]][[1]]$year)-6):
                                                        length(survey.obj[[banks[i]]][[1]]$year)]
        s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
        shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F, split=60,
                recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T)	
        if(fig != "screen") dev.off()
      } # end  if(banks[i] != "Ger")
      
      if(banks[i]=="Ger")
      {
        shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
        s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
        shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F, split=60,
                recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T)	
        if(fig != "screen") dev.off()
      } # end if(banks[i]=="Ger")
    } # end if(any(plots=="SHF-split"))    
    
    ##### END Shell height frequency Large figure   ##### END Shell height Largefrequency figure ## END Shell height frequency Large figure
    ##### END Shell height frequency Large figure   ##### END Shell height frequency Largefigure ##### END Shell height frequency Large figure
    
    
    ##### Clapper abundance time series      ##### Clapper abundance time series##### Clapper abundance time series
    ##### Clapper abundance time series ##### Clapper abundance time series ##### Clapper abundance time series
    
    if(any(plots== "clapper-abund-ts"))
    {
      
      # set up for paired comparisons
      clap.survey.obj[[banks[i]]][[1]]$plot.year <- ifelse(clap.survey.obj[[banks[i]]][[1]]$year %in% c("2018", "2019"), clap.survey.obj[[banks[i]]][[1]]$year, 2020)
      
      clap.abund.ts.title <- substitute(bold(paste("Clapper abundance time series (",bank,")",sep="")),
                                        list(bank=banks[i]))
      if(add.title == F) clap.abund.ts.title <- ""
      if(fig == "screen") windows(8.5,11)
      if(fig == "png") png(paste(plot.dir,"Clapper_abund_ts.png",sep=""),
                           units="in",width = 8.5, height = 11,res=420,bg = "transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"Clapper_abund_ts.pdf",sep=""),width = 8.5, height = 11)
      
      # abundance for all banks!
      comp <- clap.survey.obj[[banks[i]]][[1]] %>%
        pivot_longer(cols=I:NPR.cv) %>%
        subset(name %in% c("N", "NR", "NPR")) 
      
      comp$name <- factor(comp$name, levels=c("NPR", "NR", "N"))
      levels(comp$name) <- c(paste0("Pre-recruits (<", unique(comp$RS), " mm)"),
                             paste0("Recruits (", unique(comp$RS), "-", unique(comp$CS)-1, " mm)"),
                             paste0("Fully recruited (\u2265", unique(comp$CS), " mm)"))
      comp$compyear <- ifelse(grepl(x = comp$year, 19), 2019, 2018)
      
      require(ggrepel)
      clapabund <- ggplot() + geom_point(data=comp, aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), value)) + 
        geom_text_repel(data=comp, aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), value, label=as.factor(plot.year)), point.padding = 0.1, 
                        min.segment.length = 0.1, nudge_x=0.2, segment.linetype = "dashed") + 
        facet_wrap(~name, ncol=1, scales="free_y") +
        theme_bw() +
        theme(panel.grid=element_blank(), 
              strip.text = element_text(hjust=0, size=14), 
              strip.background = element_rect(colour=NA, fill=NA),
              plot.title = element_text(hjust=0.5, 22)) +
        xlab("Repeat year") +
        ylab(substitute(paste("",frac(N,tow),),list(N="N",tow="tow"))) +
        ggtitle(clap.abund.ts.title) 
      print(clapabund)
      
      if(fig != "screen") dev.off()         
    } # end  if(any(plots== "clapper-abund-ts"))  
    
    ##### END Clapper abundance time series      ##### END Clapper abundance time series##### END Clapper abundance time series
    ##### END Clapper abundance time series ##### END Clapper abundance time series ##### END Clapper abundance time series      
    
    
    #####  Clapper % time series      #####  Clapper % time series#####  Clapper % time series
    #####  Clapper % time series #####  Clapper % time series #####  Clapper % time series      
    
    if(any(plots== "clapper-per-ts"))
    {
      browser()
      # set up for paired comparisons
      clap.survey.obj[[banks[i]]][[1]]$plot.year <- ifelse(clap.survey.obj[[banks[i]]][[1]]$year %in% c("2018", "2019"), clap.survey.obj[[banks[i]]][[1]]$year, 2020)
      
      clap.per.ts.title <- substitute(bold(paste("Clapper time series (% dead ",bank,")",sep="")),
                                      list(bank=banks[i]))
      if(add.title == F) clap.per.ts.title <- ""
      
      if(fig == "screen") windows(8.5,11)
      if(fig == "png") png(paste(plot.dir,"Clapper_per_ts.png",sep=""),units="in",width = 8.5, 
                           height = 11,res=420,bg = "transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"Clapper_per_ts.pdf",sep=""),width = 8.5, height = 11)
      
      comp <- clap.survey.obj[[banks[i]]][[1]] %>%
        pivot_longer(cols=I:NPR.cv) %>%
        subset(name %in% c("N", "NR", "NPR")) 
      
      comp2 <- survey.obj[[banks[i]]][[1]] %>%
        pivot_longer(cols=I:CV_120_plus) %>%
        subset(name %in% c("N", "NR", "NPR")) %>%
        dplyr::rename(live=value)
      
      comp <- left_join(comp, comp2) %>%
        mutate(total=value + live) %>%
        mutate(prop=value/total)
      
      comp$name <- factor(comp$name, levels=c("NPR", "NR", "N"))
      levels(comp$name) <- c(paste0("Pre-recruits (<", unique(comp$RS), " mm)"),
                             paste0("Recruits (", unique(comp$RS), "-", unique(comp$CS)-1, " mm)"),
                             paste0("Fully recruited (\u2265", unique(comp$CS), " mm)"))
      comp$compyear <- ifelse(grepl(x = comp$year, 19), 2019, 2018)
      
      require(ggrepel)
      clapperperc <- ggplot() + geom_point(data=comp, aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), prop)) + 
        geom_text_repel(data=comp, aes(paste0(as.factor(compyear), "\n(n = ", n, ")"), prop, label=as.factor(plot.year)), point.padding = 0.1, 
                        min.segment.length = 0.1, nudge_x=0.2, segment.linetype = "dashed") + 
        facet_wrap(~name, ncol=1, scales="free_y") +
        theme_bw() +
        theme(panel.grid=element_blank(), 
              strip.text = element_text(hjust=0, size=14), 
              plot.title = element_text(hjust=0.5, 22),
              strip.background = element_rect(colour=NA, fill=NA)) +
        xlab("Repeat year") +
        ylab("Percent per tow")+
        ggtitle(clap.per.ts.title) 
      print(clapperperc)
      
      if(fig != "screen") dev.off()                 
    } # end if(any(plots== "clapper-per-ts"))   
    
    ##### END Clapper % time series      ##### END Clapper % time series##### END Clapper % time series
    ##### END Clapper % time series ##### END Clapper % time series ##### END Clapper % time series      
    
    ##### Shell height, Meat weight, condition factor times series ##### Shell height, Meat weight, condition factor times series             
    ##### Shell height, Meat weight, condition factor times series ##### Shell height, Meat weight, condition factor times series
    
    if(any(plots== "SH-MW-CF-ts") & !banks[i] =="BBn")
    {
      # This only works for the banks we have thse data for...
      if(banks[i] %in% c("BBn" ,"BBs", "Sab" ,  "GBb", "GBa"))
      {
        if(fig == "screen") windows(11,8.5)
        if(fig == "png") png(paste(plot.dir,"SH_MW_CF_ts.png",sep=""),units="in",width = 8.5,height = 11,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"SH_MW_CF_ts.pdf",sep=""),width = 8.5,height = 11)
        par(mfrow=c(3,1),omi=c(0.3,0.6,0.3,0.2))
        yrs <- min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(survey.obj[[banks[i]]][[1]]$year,na.rm=T)
        # yrs2 <-min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(survey.obj[[banks[i]]][[1]]$year,na.rm=T)
        # yrs <- min(yrs,yrs2):max(yrs,yrs2)
        # This fills the missing years with NA's so the plot looks better...
        tmp <- as.data.frame(cbind(yrs,matrix(NA,nrow=length(yrs),ncol=ncol(survey.obj[[banks[i]]][[1]][,-1]))))
        tmp[tmp$yrs %in% survey.obj[[banks[i]]][[1]]$year,2:ncol(survey.obj[[banks[i]]][[1]])] <- survey.obj[[banks[i]]][[1]][,-1]
        names(tmp) <- names(survey.obj[[banks[i]]][[1]])
        
        stdts.plt(subset(tmp,year %in% yrs),y="l.bar",pch=17,lty=1,ylab="Average\n shell\n height\n (mm)",las=1,
                  median.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
        stdts.plt(subset(survey.obj[[banks[i]]][[1]],year %in% yrs),y="CF",pch=17,lty=1,las=1, ylab=cf.lab,
                  median.line=T,graphic='none',xlab='',labcx=1.2,axis.cx=1.2)
        stdts.plt(subset(tmp,year %in% yrs),y="w.bar",pch=17,lty=1,ylab="Average\n meat\n weight\n(g)",
                  median.line=T,graphic='none',xlab='',labcx=1.2,las=1,axis.cx=1.2)
        if(add.title ==T) title(paste("Shell height, Condition factor, Meat weight (",banks[i],")",sep=""), cex.main=3,outer=T)
        if(fig != "screen") dev.off()   
      } # end if(banks[i] %in% c("BBn" ,"BBs", "Sab" ,  "GBb", "GBa"))
      
    }  # end if(any(plots== "SH-MW-CF-ts"))
    
    
    ############  Breakdown figures for BBn and GB############  Breakdown figures for BBn and GB############  Breakdown figures for BBn and GB
    ############  Breakdown figures for BBn and GB############  Breakdown figures for BBn and GB############  Breakdown figures for BBn and GB  
    
    if(any(plots== "breakdown") & !banks[i] =="BBn")
    {
      # This only works for the banks we have thse data for...
      #if(banks[i] %in% c("BBn" , "GBb", "GBa","GB"))
      #{
      if(fig == "screen") windows(11,8.5)
      if(fig == "png") png(paste(plot.dir,"breakdown-",(yr),".png",sep=""),units="in",
                           width = 11,height = 8.5,res=420,bg = "transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"breakdown-",(yr),".pdf",sep=""),width = 11,height = 8.5)
      if(banks[i] %in% spat.name) mc <- subset(fish.reg, year == yr & Bank %in% unique(spat.names$bank[spat.names$label == banks[i]]))$MC_reg
      # This will make the breakdown figure for the previous year in which there was a survey (typically last year but not always...)
      # This is based on the current year being requested (which could differ from the last year in the data if you are saying using the 2018 survey results
      # but wanted to look at the 2015 data for example).
      current.year <- which(survey.obj[[banks[i]]]$model.dat$year[!is.na(survey.obj[[banks[i]]]$model.dat$n)] == yr)
      last.surv.year <- survey.obj[[banks[i]]]$model.dat$year[!is.na(survey.obj[[banks[i]]]$model.dat$n)][current.year-1]
      # To get the ymax the same between succesive years I want to do this...
      bm<-survey.obj[[banks[i]]]$shf.dat$w.yst[which(survey.obj[[banks[i]]][[1]]$year==yr),which(seq(5,200,5) >= 5)]/1000
      bm.last<-survey.obj[[banks[i]]]$shf.dat$w.yst[which(survey.obj[[banks[i]]][[1]]$year==last.surv.year),which(seq(5,200,5) >= 5)]/1000
      ymax <- max(c(max(bm,na.rm=T)*1.1),max(bm.last,na.rm=T)*1.1)
      # need to do the same thing for the meat count axis, but this requires us to calculate the meatcounts for each bar first, for both years
      ## this year:
      bmmc<-survey.obj[[banks[i]]]$shf.dat$w.yst[which(survey.obj[[banks[i]]][[1]]$year==yr),which(seq(5,200,5) >= 5)]/1000
      nummc<-survey.obj[[banks[i]]]$shf.dat$n.yst[which(survey.obj[[banks[i]]][[1]]$year==yr),which(seq(5,200,5) >= 5)]
      countmc=nummc/bmmc*0.5
      vec<-seq(0,195,5)
      y2max<-max(countmc[(min(c(RS-15,160),na.rm=T)/5):length(vec)],na.rm=T)*1.1
      ## last year
      bmmc.last<-survey.obj[[banks[i]]]$shf.dat$w.yst[which(survey.obj[[banks[i]]][[1]]$year==yr-1),which(seq(5,200,5) >= 5)]/1000
      nummc.last<-survey.obj[[banks[i]]]$shf.dat$n.yst[which(survey.obj[[banks[i]]][[1]]$year==yr-1),which(seq(5,200,5) >= 5)]
      countmc.last=nummc.last/bmmc.last*0.5
      y2max.last<-max(countmc.last[(min(c(RS-15,160),na.rm=T)/5):length(vec)],na.rm=T)*1.1
      y2max <- max(c(max(y2max,na.rm=T)*1.1),max(y2max.last,na.rm=T)*1.1)
      breakdown(survey.obj[[banks[i]]],yr=yr,mc=mc,cx.axs=1,y1max = ymax, y2max=y2max, add.title = F)
      
      if(add.title ==T) title(paste("Biomass & Meat Count by Height (",banks[i],"-",yr,")",sep=""), cex.main=2,adj=0.35)
      if(fig != "screen") dev.off()   
      
      # I also want to remake the previsou year's breakdown plot, this will go in the current years folder but will
      # be the same y-scale (there is no guarantee that the plot made last year will be, likely it won't).  It's a bit
      # clunky but basically this is the same plot as last year but re-scaled for comparative purposes...
      if(any(!is.na(bm.last))==T) {
        if(fig == "screen") windows(11,8.5)
        if(fig == "png") png(paste(plot.dir,"breakdown-",last.surv.year,".png",sep=""),units="in",
                             width = 11,height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"breakdown-",last.surv.year,".pdf",sep=""),width = 11,height = 8.5)
        
        # To get the ymax the same between succesive years I want to do this...
        breakdown(survey.obj[[banks[i]]],yr=last.surv.year,mc=mc,cx.axs=1,y1max = ymax, y2max=y2max, add.title = F)
        if(add.title ==T) title(paste("Biomass & Meat Count by Height (",banks[i],"-",last.surv.year,")",sep=""), cex.main=2,adj=0.35)
        
        if(fig != "screen") dev.off()   
      }
      
    }  # end iif(any(plots== "breakdown"))
    ############  End Breakdown figures for BBn and GBa############  End Breakdown figures for BBn and GBa############  
    ############  End Breakdown figures for BBn and GBa############  End Breakdown figures for BBn and GBa############  
    
    
    
    
    
    ############  Seedboxes###########  Seedboxes ############  Seedboxes ############  Seedboxes ############  Seedboxes ############  
    ############  Seedboxes###########  Seedboxes ############  Seedboxes ############  Seedboxes ############  Seedboxes ############  
    ############  Seedboxes###########  Seedboxes ############  Seedboxes ############  Seedboxes ############  Seedboxes ############  
    
    
    if(any(plots == "seedboxes"))    
    {
      # I'm picking November 1st of current year b/c if they close a box at this point none of our presentations
      # will have information about it.  The only reason I'm putting this closed bit in is for cases in which
      # I am making plots from previous years, so a box closed in Nov or December never would have been included in one of our
      # original presentations (maybe OSAC, but this isn't OSAC)....
      sb <- subset(seedboxes,Bank == banks[i] & Closed < paste(yr,"-11-01",sep="") & (Open >= paste(yr,"-01-01",sep="") & Active=="Yes" | is.na(Open)))
      if(banks[i] == "GB")  sb <- subset(seedboxes,Bank %in% c("GBa","GBb") & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep=""))
      if(nrow(sb) > 0) # only run the rest of this if we have data...
      {
        seed.spatial.plots <- c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial")
        bound.poly.surv <- subset(survey.bound.polys,label==banks[i]) 
        attr(bound.poly.surv,"projection")<-"LL"
        n.box <- length(seedbox.obj[[banks[i]]])
        sb[,c("X", "Y")] <- apply(sb[,c("X", "Y")], 2, function(x) as.numeric(x))
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
          
          # adjust the number of years ago for determine the "previous year" plots (esp. for breakdown plots)
          if(!(banks[i] == "Sab" && this.box$Common_name == "Starbox")) yrsago <- 1
          if(banks[i] == "Sab" && this.box$Common_name == "Starbox") yrsago <- 2
          
          # Titles for the seedbox plots....
          seedbox.bm.title <- substitute(bold(paste(box,"-Biomass time series (",bank,")",sep="")),
                                         list(year=as.character(yr),bank=banks[i],box = fig.box.name[j]))
          seedbox.abund.title <-substitute(bold(paste(box,"- Abundance time series (",bank,")",sep="")),
                                           list(year=as.character(yr),bank=banks[i],box = fig.box.name[j]))
          seedbox.SHF.title <- substitute(bold(paste(box,"- Shell Height Frequency (",bank,")",sep="")),
                                          list(year=as.character(yr),bank=banks[i],box = fig.box.name[j]))
          pre.title.seed <-substitute(bold(paste("Pre-recruit scallops (" ,""<a, " mm)",sep="")),
                                      list(a=as.character(RS),year=as.character(yr),bank=banks[i]))
          rec.title.seed <- substitute(bold(paste("Recruit scallops (",b- a, " mm)",sep="")),
                                       list(a=as.character(CS-1),b=as.character(RS),year=as.character(yr),bank=banks[i]))
          fr.title.seed <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm)",sep="")),
                                      list(a=as.character(CS),year=as.character(yr),bank=banks[i]))
          breakdown.title.seed <- substitute(bold(paste("Biomass & Meat Count by Height (",fn,"-",bank,"-",year,")",sep="")),
                                             list(fn = fig.box.name[j],year=as.character(yr),bank=banks[i]))
          last.yr.breakdown.title.seed <- substitute(bold(paste("Biomass & Meat Count by Height (",fn,"-",bank,"-",year,")",sep="")),
                                                     list(fn = fig.box.name[j],year=as.character(yr-yrsago),bank=banks[i]))
          cf.title.seed <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
                                      list(year=as.character(yr),bank=banks[i]))
          mc.title.seed <- substitute(bold(paste("Meat count (" ,"">=m, " mm " , bank,"-",year,")",sep="")),
                                      list(m=as.character(CS),year=as.character(yr),bank=banks[i]))
          clap.title.seed <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-",year,")",sep="")),
                                        list(c=as.character(RS),bank=banks[i],year=as.character(yr)))
          
          # Note that for all these time series plots I made se=F b/c when some years contained just 1 tow it
          # made things go crazy, and the SE isn't so meaningful given the variablity in the # of tows anyways.
          # Make the abundance time series for the box
          if(fig == "png") png(paste(plot.dir,box.names[j],"-abundance_ts.png",sep=""),units="in",
                               width = 8.5, height = 11,res=420,bg = "transparent") 
          if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-abundance_ts.pdf",sep=""),
                               width = 8.5, height = 11) 
          if(fig == "screen") windows(8.5,11)
          survey.ts(boxy$model.dat,min(boxy$model.dat$year,na.rm=T):yr,pdf=F,Npt=T,
                    areas=NULL,clr=c('blue',"blue","darkgrey"),se=F,pch=16,
                    add.title = add.title,titl = seedbox.abund.title,cx.mn=3,axis.cx = 1.5)
          if(fig != "screen") dev.off()
          
          # Make the biomass time series for the box
          if(fig == "png") png(paste(plot.dir,box.names[j],"-biomass_ts.png",sep=""),units="in",
                               width = 8.5, height = 11,res=420,bg = "transparent") 
          if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-biomass_ts.pdf",sep=""),
                               width = 8.5, height = 11) 
          if(fig == "screen") windows(8.5,11)
          
          survey.ts(boxy$model.dat,min(boxy$model.dat$year,na.rm=T):yr,pdf=F, Npt=T,type="B",
                    areas=NULL,clr=c('blue',"blue","darkgrey"),se=F,pch=16,
                    add.title = add.title,titl = seedbox.bm.title,cx.mn=3,axis.cx = 1.5)
          if(fig != "screen") dev.off()
          
          if(any(plots == "user.SH.bins"))
          {
            # Make the abundance time series for the box
            if(fig == "png") png(paste(plot.dir,box.names[j],"-abundance_user_SH_bins_ts.png",sep=""),units="in",
                                 width = 8.5, height = 11,res=420,bg = "transparent") 
            if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-abundance_user_SH_bins_ts.pdf",sep=""),
                                 width = 8.5, height = 11) 
            if(fig == "screen") windows(8.5,11)
            survey.ts(boxy$model.dat,min(boxy$model.dat$year,na.rm=T):yr,pdf=F, Npt=T,
                      areas=NULL,clr=c('blue',"blue","darkgrey"),se=F,pch=16,ys=1.4,
                      add.title = add.title,titl = seedbox.abund.title,cx.mn=3,axis.cx = 1.5,user.bins=user.bins)
            if(fig != "screen") dev.off()
            
            # Make the biomass time series for the box
            if(fig == "png") png(paste(plot.dir,box.names[j],"-biomass_user_SH_bins_ts.png",sep=""),units="in",
                                 width = 8.5, height = 11,res=420,bg = "transparent") 
            if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-biomass_user_SH_bins_ts.pdf",sep=""),
                                 width = 8.5, height = 11) 
            if(fig == "screen") windows(8.5,11)
            
            survey.ts(boxy$model.dat,min(boxy$model.dat$year,na.rm=T):yr,pdf=F, Npt=T,type="B",
                      areas=NULL,clr=c('blue',"blue","darkgrey"),se=F,pch=16,ys=1.4,
                      add.title = add.title,titl = seedbox.bm.title,cx.mn=3,axis.cx = 1.5,user.bins =user.bins)
            if(fig != "screen") dev.off()
          } # end if(any(plots %in% "user.SH.bins"))
          
          # A seedbox breakdown figure, this would include all tows not just proper tows...
          if(fig == "screen") windows(11,8.5)
          if(fig == "png") png(paste(plot.dir,box.names[j],"-breakdown-",(yr),".png",sep=""),units="in",
                               width = 11,height = 8.5,res=420,bg = "transparent")
          if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-breakdown-",(yr),".pdf",sep=""),
                               width = 11,height = 8.5)
          mc <- fish.reg$MC_reg[fish.reg$Bank == banks[i]]
          if(banks[i] == "GB") mc <- fish.reg$MC_reg[fish.reg$Bank == "GBa"]
          # What I'm doing is to make the axis for the current and previous year breakdown plot the same
          # as I need to compare these...
          bm<-boxy$shf.dat$w.yst[which(boxy[[1]]$year==yr),which(seq(5,200,5) >= 5)]/1000
          bm.last<-boxy$shf.dat$w.yst[which(boxy[[1]]$year==(yr-yrsago)),which(seq(5,200,5) >= 5)]/1000
          
          # need to do the same thing for the meat count axis, but this requires us to calculate the meatcounts for each bar first, for both years
          ## this year:
          bmmc<-boxy$shf.dat$w.yst[which(boxy$model.dat$year==yr),which(seq(5,200,5) >= 5)]/1000
          nummc<-boxy$shf.dat$n.yst[which(boxy$model.dat$year==yr),which(seq(5,200,5) >= 5)]
          countmc=nummc/bmmc*0.5
          vec<-seq(0,195,5)
          y2max<-max(countmc[(min(c(RS-15,160),na.rm=T)/5-1):length(vec)],na.rm=T)*1.1
          ## last year
          bmmc.last<-boxy$shf.dat$w.yst[which(boxy$model.dat$year==yr-yrsago),which(seq(5,200,5) >= 5)]/1000
          nummc.last<-boxy$shf.dat$n.yst[which(boxy$model.dat$year==yr-yrsago),which(seq(5,200,5) >= 5)]
          countmc.last=nummc.last/bmmc.last*0.5
          y2max.last<-max(countmc.last[(min(c(RS-15,160),na.rm=T)/5-1):length(vec)],na.rm=T)*1.1
          
          # If there is no data from last year then I use this years ymax and I don't make the plot for last year...
          ymax <- ifelse(length(bm.last[!is.na(bm.last)]) > 0, max(c(max(bm,na.rm=T)*1.1),max(bm.last,na.rm=T)*1.1),max(bm,na.rm=T)*1.1)
          y2max <- ifelse(length(countmc.last[!is.na(countmc.last)]) > 0, max(c(max(y2max,na.rm=T)*1.1),max(y2max.last,na.rm=T)*1.1),max(y2max,na.rm=T)*1.1)
          # Now make the breakdown plot...
          if(!ymax %in% "-Inf") {
            breakdown(boxy,yr=yr,mc=mc,cx.axs=1,add.title="F",y1max = ymax, y2max=y2max,
                      CS = survey.obj[[banks[i]]][[1]]$CS[length(survey.obj[[banks[i]]][[1]]$CS)],
                      RS = survey.obj[[banks[i]]][[1]]$RS[length(survey.obj[[banks[i]]][[1]]$RS)])
            if(add.title==T) title(breakdown.title.seed, cex.main=2,adj=0.35)
          }
          if(fig != "screen") dev.off()   
          
          # I also want to remake the previsou year's breakdown plot (if we have tows in the area), this will go in the current years folder but will
          # be the same y-scale (there is no guarantee that the plot made last year will be, likely it won't).  It's a bit
          # clunky but basically this is the same plot as last year but re-scaled for comparative purposes...
          # But only make it if we have data!
          if(length(bm.last[!is.na(bm.last)]) > 0)
          {
            if(fig == "screen") windows(11,8.5)
            if(fig == "png") png(paste(plot.dir,box.names[j],"-breakdown",(yr-yrsago),".png",sep=""),units="in",
                                 width = 11,height = 8.5,res=420,bg = "transparent")
            if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-breakdown-",(yr-yrsago),".pdf",sep=""),
                                 width = 11,height = 8.5)
            # To get the ymax the same between succesive years I want to do this...
            if(!ymax %in% "-Inf") {
              breakdown(boxy,yr=(yr-yrsago),mc=mc,cx.axs=1,y1max = ymax, y2max=y2max, add.title = F)
              if(add.title==T) title(last.yr.breakdown.title.seed, cex.main=2,adj=0.35)
            }
            if(fig != "screen") dev.off()   
          } # end if(length(bm.last[!is.na(bm.last)]) > 0)
          
          # Now the Shell height frequency plots.
          shf.years <- boxy$model.dat$year[(length(boxy$model.dat$year)-6):
                                             length(boxy$model.dat$year)]
          s.size <- boxy$model.dat$n[boxy$model.dat$year %in% shf.years]
          if(any(is.na(s.size))) s.size[which(is.na(s.size))] <- 0
          if(fig == "png") png(paste(plot.dir,box.names[j],"-SHF.png",sep=""),units="in",
                               width = 8.5, height = 11,res=420,bg = "transparent") 
          if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-SHF.pdf",sep=""),
                               width = 8.5, height = 11)
          if(fig == "screen") windows(8.5,11)
          
          shf.plt(boxy,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
                  recline=c(RS,CS),add.title = add.title,titl = seedbox.SHF.title,cex.mn=3,sample.size = T)	
          if(fig != "screen") dev.off()
          
          
          # Now zoom in on the larger size classes...
          if(fig == "png") png(paste(plot.dir,box.names[j],"-SHF-large.png",sep=""),units="in",
                               width = 8.5, height = 11,res=420,bg = "transparent") 
          if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-SHF-large.pdf",sep=""),
                               width = 8.5, height = 11) 
          if(fig == "screen") windows(8.5,11)
          shf.plt(boxy,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,select=60,
                  recline=c(RS,CS),add.title = add.title,titl = seedbox.SHF.title,cex.mn=3,sample.size = T)
          if(fig != "screen") dev.off()
          
          # Now do the SHF with the panels split classes...
          if(fig == "png") png(paste(plot.dir,box.names[j],"-SHF-split.png",sep=""),units="in",
                               width = 8.5, height = 11,res=420,bg = "transparent") 
          if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-SHF-split.pdf",sep=""),
                               width = 8.5, height = 11) 
          if(fig == "screen") windows(8.5,11)
          shf.plt(boxy,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,split=70,
                  recline=c(RS,CS),add.title = add.title,titl = seedbox.SHF.title,cex.mn=3,sample.size = T)
          if(fig != "screen") dev.off()
          
          
        } # end for(j in 1:n.box)
        #} # end !is.null(mod.res[["Clap-spatial"]]) &....    
      } # end (if nrow(sb))
    } # end the if(any(plots) %in% "seedboxes")
  } # end the i for loop 
  
  
} # end the function
