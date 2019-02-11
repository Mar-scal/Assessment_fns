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
# Nov 2018:     Created colour.bins option which allows you to specify the colour ramp on INLA spatial plots. Revisit this in June 2019 to figure out appropriate bins for spring banks.
#               Created keep.full.GB option which allows you to create INLA spatial maps for ALL of GB, not just GBa and GBb separately. 
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
  # s:  user.SH.bins      - Make plots for the selected SH bins by the you (the user), Shell height bins that you set up
  # t:  MW-spatial        - Make a plot of the average meat weight across the bank (includes all individuals)
  # u:  SH-spatial        - Make a plot of the average shell height across the bank (includes all individuals)
  # v:  MW.GP-spatial     - Make a plot of the meat weight growth potential across the bank (all individuals).
  # w:  SH.GP-spatial     - Make a plot of the shell height growth potential across the bank (all individuals).
#                           in the SurveySummary_data.r function call will be plotted.
# 2:  banks:    The banks to create figures for.  Option are "BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB" 
#               (note Banquereau is not supported yet and the GB is Georges Bank Spring
# 3:  s.res:    The spatial resolution for any spatial plots.  Default = "low" which will quickly produce the spatial plots, but
#               the detail will be low.  Options are
  # a: "low"    Quick low resolution plots, this options sets up a 50 X 50 prediction grid.
  # b: "high"   Slow high resolution plots, this option sets up a 250 X 250 prediction grid, this can take a while so only run for final figures.
  # c: c(X,Y)   Where X and Y are numbers that user inputs, (e.g. s.res = c(250,250) would give the same result as setting this to "high")
# 4: add.scale: Add a scale to the spatial figures?  T/F, default =F
# 5:  direct:   The working directory to put figures are from which to grab data.  Default = "Y:/Offshore scallop/Assessment/", 
# 6:  yr:       The survey year of interest.  Default = as.numeric(format(Sys.time(), "%Y")) which attempts to produce plots for the current year.
#               NB: If trying to reproduce figures from previous years make sure the Rdata output from SurverySummary_data.r for that year exists!
# 7:  add.title:Add titles to the figures; T/F.  Default = T
# 8:  fig:      Plot the figures to "screen", to a "pdf" or a "png" file. If fig = "pdf" or "png" figures are placed in this directory (which must exist!!) 
#               (direct/yr/Presentations/Survey_summary/bank - e.g.Y:/Offshore scallop/Assessment/2015/Presentations/Survey_summary/GBa/)
#               unless season == "testing" in which they get saved to a testing folder (see below for exact directory).
# 9:  season:   For the spring survey we need to identify that we don't have all the results in yet.  When running the scripts with only spring  
#               survey data set to "spring".  If just running GBa and GBb you can set this to "summer" if you've already created the Rdata file.
#               When summer survey is complete you can also set this to the default of "both".  I've also added the option during preliminary
#               runs or when altering the function to use a "testing" option so it loads the proper data.  You'll need to have created the 
#               testing R data when running the SurveySummary_data.r.  Additionally when set to "testing" the figures (if being saved) will be
#               saved in the folder... direct,yr,"/Presentations/Survey_summary/test_figures/",banks[i].
# 10: INLA:     What to you want to do with the spatial modelling.  Three options, the default is INLA= "run.full"  
#               1:  "run"       This will run the INLA for the spatial plot requested, 
#               2: "run.full"   THIS WILL RUN AND SAVE the models for ALL spatial figures, not just the models specified by the "plots" arguement above.
#                               This can be slow (depends on computer), but shouldn't take more than an hour for all banks based on my testing
#                               The results are saved with the Bank name and spatial resolution in the file name so that high and low runs can be saved and 
#                               recalled as necessary.  This uses the prediction stack in INLA and provides the results on a fine spatial mesh
#                               but with a proper mesh which minimizes any edge effects.
#               3: "load"  This loads in the spatial models saved and avoids having to re-run the "run.full" option whenever you want to
#                               remake figures.  These will be saved by bank and will include the spatial resolution used in model. This excludes any 
#                               special monitoring boxes (i.e. the "Starbox on Sable).  Regular seeedbox results will typically be 
#                               saved as subset of the bank.
# 11: contour:  Add a contour line around the spatial plots to help delinate them.  T/F, default = F
# 12: offsets:  The offset for the INLA figures, there is apparently a bug within INLA which results in the mesh calculations "hanging" with some
#               combinations for the mesh calculations, if the mesh for an area isn't calculating try changing this.  For 2018 the "default" offsets by bank 
#               were BBn =0.12;  BBs = 0.12;  Ger = 0.12; Mid = 0.12; Sab = 0.10, GBb = 0.15; GBa = 0.15; GB = 0.35.  This should be set to "default", if these
#               are added manually the there needs to be 1 entry for each bank you are plotting, e.g. if wanting to change from the default settings and plotting
#               just a couple of banks, let's say BBn and Mid, you would need to have this be something like offsets = c(0.15,0.13)
# 13: plt.bath  If you want to plot the bathymetry on the plots, this can be slow for the USGS data so if just trying to take a quick look at the
#               figures set this to false for quicker rendering...
# 14: sub.area Do you want to make plots of the user specfied sub areas.  T/F, default = T
# 15: colour.bins Do you want to specify some colour bins for the PR-spatial, Rec-Spatial and FR-spatial plots? Default is NULL, but if you want bins, put in a vector 
#               of 13 bins like this: c(0,5,10,50,100,200,300,500,700,1000,2000,5000,10000). Max bin must exceed max INLA estimate for any tow in plot.
# 16. keep.full.GB Set to true (T) if you want to plot all of GB on one INLA spatial map (instead of GBa and GBb separately). Default is F

###############################################################################################################

survey.figs <- function(plots = c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial","Survey","MW-SH",
                                  "abund-ts","biomass-ts","SHF","SHF-large","SHF-split",
                                  "clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown","seedboxes","user.SH.bins",
                                  "MW-spatial","SH-spatial","MW.GP-spatial","SH.GP-spatial"),
                       banks = "all" ,
                       s.res = "low",add.scale = F, 
                       direct = "Y:/Offshore scallop/Assessment/", yr = as.numeric(format(Sys.time(), "%Y"))  ,
                       add.title = T,fig="screen",season="both",INLA = "run" ,contour =F, offsets="default",
                       plt.bath = T,sub.area=T, colour.bins=NULL,
                       keep.full.GB=F, nickname=NULL)
{ 
 
   tmp.dir <- direct ; tmp.season <- season # I need this so that the directory isn't overwritten when I load the below...
  # Load the appropriate data.
  if(season == "testing") 
  {
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/testing_results.Rdata",sep=""))==T ||
       file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/testing_results_", nickname, ".Rdata",sep=""))==T) 
    {                   
      if(is.null(nickname)) load(paste(direct,"Data/Survey_data/",yr,
                                     "/Survey_summary_output/testing_results.Rdata",sep=""))
      if(!is.null(nickname)) load(paste(direct,"Data/Survey_data/",yr,
                                       "/Survey_summary_output/testing_results_", nickname, ".Rdata",sep=""))
      season <- tmp.season 
    } else stop("Please re-run Survey_Summary_script and set it so that the file 'testing_results.Rdata' gets created, Thanks eh!!") # end if/else file...
  } # end if(season == "testing") 
  
  if(season == "both") 
  {
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))==T)
    {
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
      season <- tmp.season # Needed b/c there is a season in the object I'm loading too..
    } else stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_all_results.Rdata' gets created, Thanks eh!!")
  } # end if(season == "both") 
  
  if(season == "spring") 
  {
    # If we are making the MW/SH plot & looking at GB we need to get both the spring and summer GB data
    if(any(plots %in% "MW-SH") && any(banks %in% "GB"))
    {
      # This loads last years Survey object results.
      load(paste(direct,"Data/Survey_data/",(yr-1),"/Survey_summary_output/Survey_object.Rdata",sep=""))  
      survey.obj.last <- survey.obj
    } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))==T)
    {
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
      season <- tmp.season # Needed b/c there is a season in the object I'm loading too..
    } else stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_spring_results.Rdata' gets created, Thanks eh!!") # end if/else file.
  } # end if(season == "spring") 
  if(season == "summer") 
  {
    # If we are making the MW/SH plot & looking at GB we need to get both the spring and summer GB data
    if(any(plots %in% "MW-SH") && any(banks %in% "GBa"))
    {
      # This loads the springs survey results.
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
      survey.obj.last <- survey.obj
    } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_summer_results.Rdata",sep=""))==T)
    {
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_summer_results.Rdata",sep=""))  
      season <- tmp.season # Needed b/c there is a season in the object I'm loading too..
    } 
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_summer_results.Rdata",sep=""))==F & 
       file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Selected_summer_survey_results.Rdata",sep=""))==T)
    {
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Selected_summer_survey_results.Rdata",sep=""))  
      season <- tmp.season # Needed b/c there is a season in the object I'm loading too..
    } 
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_summer_results.Rdata",sep=""))==F & 
       file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Selected_summer_survey_results.Rdata",sep=""))==F)
    {
      stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_summer_results.Rdata' gets created, Thanks eh!!") # end if/else file.else stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_summer_results.Rdata' gets created, Thanks eh!!") # end if/else file.
    } 
  }# if(season == "summer")
   
  # Now get the banks to plot set up.
  if(banks == "all") banks <- c("BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB")
  #browser()
  # Since BBs is only surveyed occasionally we need to make sure it exists, if it doesn't toss it...
  if(is.null(bank.dat$BBs) && "BBs" %in% banks) banks <- banks[-which(grepl(x=banks, "BBs"))]
  # If we are plotting the sub-areas we wanna do this...
  if(sub.area == T) {spat.name <- unique(spat.names$label); banks <- c(banks,spat.name)}
  if(sub.area == F) spat.name <- NULL
  # We may need to load in the Sable pre-stratified data for the Sable figure, but only if it is 2018 as we won't plot this after that time.
  # Load in the pre 2018 data for Sable
  if(yr == 2018 && any(banks %in% "Sab")) 
  {
    cat("Hi, just wanted to let you know that in 2018 Sable was restratified so you'll be plotting the restratified \n
         abundance and biomass time series.  Also, just an FYI that we noticed a slight mistake in how we did this in the Spring \n
         survey summary so these figures won't match what was done in the presentation. In the presentation, the correct towable 
        areas were used, but the post-restratified values from 2017 were plotted :(. This was corrected in time for pre-OSAC.  Thanks for your patience! \n")
    # Load in the data from before 2018...  # this is where survey.obj.sab comes from!
    load(paste0(direct,"Data/Survey_data/2017/Survey_summary_output/Sable_pre2018_results_forTSplot.RData"))
  }
  
  direct <- tmp.dir # I need this so that the directory isn't overwritten when I load the above
  
  # These are the functions used to within the heart of the code to make stuff happen
  source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/stdts.plt.R",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/survey.ts.r",sep=""),local=T)
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/shf.plt.r",sep=""))
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/shwt.plt1.r",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/Clap3.plt.R",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/gridPlot.r",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/meat_count_shell_height_breakdown_figure.r",sep="")) 
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
runlog <- read.csv(paste0(direct, "Assessment_fns/Survey_and_OSAC/SurveySummaryRunLog.csv"))
runlog <- runlog[, !names(runlog) %in% "X"]
rundate <- as.character(Sys.time())
runfunction <- "figures"
runassigned <- paste(as.character(deparse(match.call())), collapse="")
rundefaults <- paste(as.character(deparse(args(survey.figs))), collapse="")
runlog <- rbind(runlog, cbind(rundate, runfunction, runassigned, rundefaults))
write.csv(runlog, file = paste0(direct, "Assessment_fns/Survey_and_OSAC/SurveySummaryRunLog.csv"))

# Clean up any open plot devices.
if(!is.null(dev.list())) dev.off()
# Run through all of the banks of interest.  Note we are going to create a crap load of directories when doing sub-area plots.
for(i in 1:len)
{
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
  if(banks[i] %in% spat.name) {
    RS <- size.cats$RS[size.cats$Bank == "GBa"]
    CS <- size.cats$CS[size.cats$Bank == "GBa"]
  }
  if(banks[i] %in% "BanIce") {
    RS <- 75
    CS <- 80
  }

  # Grab any seedboxes of interest...
  # I'm picking November 1st of current year b/c if they close a box at this point none of our presentations
  # will have information about it.  The only reason I'm putting this closed bit in is for cases in which
  # I am making plots from previous years, so a box closed in Nov or December never would have been included in one of our
  # presentations (maybe OSAC, but this isn't OSAC)....
  #browser()
  sb <- subset(seedboxes,Bank == banks[i] & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep=""))
  if(banks[i] == "GB")  sb <- subset(seedboxes,Bank %in% c("GBa","GBb") & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep=""))
  
  ###  Now for the plots, first the survey data...
  # Get the  bank survey boundary polygon
  if(banks[i] == "BanIce") {
    bound.surv.poly[[banks[i]]] <- as.data.frame(bound.surv.poly[[banks[i]]])
    names(bound.surv.poly[[banks[i]]]) <- c("PID", "SID", "POS", "X", "Y", "label", "startyear")
  }
  if(banks[i] %in% c("GBa","GBb","BBn","BBs", "Ban", "BanIce", "GB", spat.name)) bound.poly.surv <- as.PolySet(bound.surv.poly[[banks[i]]],projection ="LL")
  
  if(!is.null(keep.full.GB) & keep.full.GB == T) {
    bound.poly.surv <- as.PolySet(rbind(bound.surv.poly$GBa, bound.surv.poly$GBb), projection ="LL")
  }
  # Need to treat Sable special...
  if(banks[i] %in% c("Sab")) 
  {
    if(yr < max(survey.bound.polys$startyear)) bound.poly.surv <-as.PolySet(subset(survey.bound.polys[survey.bound.polys$startyear==1900,],
                                                                 label==banks[i], 
                                                                 select=c("PID", "SID", "POS", "X", "Y", "label")), 
                                                          projection ="LL")
    if(!yr < max(survey.bound.polys$startyear)) bound.poly.surv <-as.PolySet(subset(survey.bound.polys[survey.bound.polys$startyear==2018,],
                                                                                   label==banks[i], 
                                                                                   select=c("PID", "SID", "POS", "X", "Y", "label")), 
                                                                            projection ="LL")
  } # end  if(banks[i] %in% c("Sab"))
  
  #Detailed survey polygons
  if(banks[i] %in% c("GBa","GBb","BBn","BBs",spat.name)) detail.poly.surv <- as.PolySet(detail.surv.poly[[banks[i]]],projection ="LL")
  if(banks[i] %in% c("Sab")) 
  {
    if(yr < max(survey.detail.polys$startyear)) detail.poly.surv <- as.PolySet(subset(survey.detail.polys[survey.detail.polys$startyear==1900,],
                                                                                      label==banks[i], 
                                                                                      select=c("PID", "SID", "POS", "X", "Y", "label", "Strata_ID")),
                                                                               projection = "LL")
    if(!yr < max(survey.detail.polys$startyear)) detail.poly.surv <- as.PolySet(subset(survey.detail.polys[survey.detail.polys$startyear==2018,],
                                                                                      label==banks[i], 
                                                                                      select=c("PID", "SID", "POS", "X", "Y", "label", "Strata_ID")),
                                                                               projection = "LL")
  } # end if(banks[i] %in% c("Sab")) 

  # Get the strata areas.  For most areas we use the survey.strata.table which is output from the data function
  if(banks[i] %in% c("GBa","GBb","BBn","BBs",spat.name)) strata.areas <- subset(survey.strata.table[[banks[i]]],select =c("PID","towable_area"))

  if(banks[i] %in% c("Sab") & !yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
    strata.areas <- subset(survey.info[!(survey.info$startyear==1900 & survey.info$label=="Sab"),], label==banks[i],select =c("PID","towable_area"))}
  if(banks[i] %in% c("Sab") & yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
    strata.areas <- subset(survey.info[!(survey.info$startyear==2018 & survey.info$label=="Sab"),], label==banks[i],select =c("PID","towable_area"))}
  if(banks[i] %in% c("GB", "Mid", "Ger", "Ban", "BanIce")) strata.areas <- NULL
  
  #Get all the details of the survey strata
  if(banks[i] %in% c("Sab") & !yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
    surv.info <- survey.info[survey.info$startyear==2018 & survey.info$label=="Sab",]}
  if(banks[i] %in% c("Sab") & yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
    surv.info <- survey.info[!(survey.info$startyear==2018) & survey.info$label=="Sab",]}
  
  if(!banks[i] %in% c("Sab")) surv.info <- survey.strata.table[[banks[i]]]
  
  ### If we are missing years in the data I want to add those years in as NA's so the plots see those as NA's  ####
  check.year <- min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(survey.obj[[banks[i]]][[1]]$year,na.rm=T)
  missing.year <- check.year[!is.element(check.year,survey.obj[[banks[i]]][[1]]$year)]
  
  if(length(missing.year) > 0)
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
  
  ################################# START MAKING FIGURES################################# START MAKING FIGURES################################# 

################  The non-survey spatial plots ###########################
################  Next up are the rest of the spatial plots ###########################
################  Next up are the rest of the spatial plots ###########################
 ############  Now start getting the spatial data for this year for the specific bank.
 
  # If we are making the seedbox or any of the spatial plots we need to set up our mesh...
  # We need to make a mesh for the plots from which we had data from all tows
  # and a mesh for the plots from which we only had data for select tows (i.e. condition factor)
  # Only run this for the full banks, we don't want to run this for the "sub-areas"
  if(banks[i] %in% c("BBn" ,"BBs" ,"Ger", "Mid", "Ban", "BanIce", "Sab", "GB" ,"GBb", "GBa"))
  {    
    spatial.maps <- plots[grep("spatial",plots)]
    # If we want spatial maps or seedboxes and/or have user SH.bins (for both of which we will produce all figures automatically.)
    if((length(spatial.maps > 0) || any(plots %in% c("seedboxes","user.SH.bins"))))
    {
      mod.res <- NULL # This will contain the model results for the spatial figures
      # Set up the resolution for the spatial plots, low is relatively fast, high is quite slow
      if(length(s.res) == 2) s.res <- s.res
      if(any(s.res == "high")) s.res <- c(250,250)
      if(any(s.res == "low")) s.res <- c(25,25)
      
      # The offset for the INLA mesh needs to be specified, these are tricky as some combinations of offsets can lead to the script hanging, the
      # defaults worked nicely for the 2018 survey so hopefully will work in most other years, but no guarantees! Before the default offsets were established in 
      # summer 2018 (after spring survey summary), GB was assigned 0.35 and the rest were all 0.15. If INLA plots have changed slightly since spring 2018,
      # this is likely why!
      if(offsets == "default")
      {
        if(banks[i] %in% c("BBn","BBs","Ger","Mid", "Ban")) offset = 0.12
        if(banks[i] %in% c("Sab", "BanIce")) offset = 0.1
        if(banks[i] %in% c("GBa","GBb")) offset = 0.1
        if(banks[i] %in% c("GB")) offset = 0.35
      }# end if(offsets == "default")
      if(offsets != "default") offset <- offsets[i]
      
      
      # For Middle bank Make a couple of boxes around the survey stations, these are entirely arbitrary...
      if(banks[i] == "Mid")  
      {
        bound.poly.surv <- rbind(c(1,	1,	1,	-60.78,	44.68),
                                 c(1,	1,	2,	-60.58,	44.68),
                                 c(1,	1,	3,	-60.58,	44.49),
                                 c(1,	1,	4,	-60.78,	44.49),
                                 c(1,	1,	5,	-60.78,	44.68),
                                 c(2,	1,	1,	-60.44,	44.53),
                                 c(2,	1,	2,	-60.33,	44.53),
                                 c(2,	1,	3,	-60.33,	44.42),
                                 c(2,	1,	4,	-60.44,	44.42),
                                 c(2,	1,	5,  -60.44,	44.53))
        bound.poly.surv <- as.data.frame(bound.poly.surv)
        names(bound.poly.surv) <- c("PID","SID","POS","X","Y")
        bound.poly.surv <- as.PolySet(bound.poly.surv,projection= "LL")
      } # end if(banks[i] == "Mid")  
      
      # For German bank the above polygon makes a mess of things, so we'll make our own based on the German Bank boundaries
      if(banks[i] == "Ger") 
      {
        g.bnds <- survey.bound.polys[survey.bound.polys$label==banks[i],]
        Y.range <- range(g.bnds$Y,na.rm=T)
        X.range <- range(g.bnds$X,na.rm=T)
        g.tmp <- newAreaPolys[newAreaPolys$label=="SFA26",]
        g.tmp <- g.tmp[, c("PID", "POS", "X", "Y", "label", "bank")]
        g.tmp$X[g.tmp$X < X.range[1]] <- X.range[1]
        g.tmp$X[g.tmp$X > X.range[2]] <- X.range[2]
        g.tmp$Y[g.tmp$Y > Y.range[2]] <- Y.range[2]
        g.tmp$Y[g.tmp$Y < Y.range[1]] <- Y.range[1]
        # Now I want to insert a segemnt into the boundary to run a diagonal line from around 43?9/-66.755 to 43/-66?24
        g.tmp[2,] <- c(5,2,-66.4,Y.range[1],"SFA26","Ger")
        g.tmp <- as.data.frame(rbind(g.tmp[c(1,2),],c(5,2,X.range[1],43.15,"SFA26","Ger"),g.tmp[3:nrow(g.tmp),]))
        for(k in 1:4) g.tmp[,k] <- as.numeric(g.tmp[,k]) # Silly rbind making everything characters...
        g.tmp$POS <- 1:nrow(g.tmp)
        bound.poly.surv <- as.PolySet(g.tmp,projection="LL")
      } # end if(banks[i] == "Ger") 
      # Now convert this to an object for sp, this gets our bounding area for the survey.
      
      bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)
      
      if(banks[i] %in% c("Ban", "BanIce")) 
      {
        ban.poly.clip <- read.csv(paste0(direct, "Data/Maps/approved/Other_Borders/BanqDomain_OffshorePlots_Feb2019.csv"))
        names(ban.poly.clip) <- c("POS", "PID", "X", "Y") 
        ban.poly.clip <- as.PolySet(ban.poly.clip,projection="LL")
        ban.poly.clip.sp <- PolySet2SpatialPolygons(ban.poly.clip)
      }
      
      # This section only needs run if we are running the INLA models
      if(length(grep("run",INLA)) > 0)
      {
        
        # If we are just getting the spatial maps for the seedboxes then we want all of these plots
        # We also want them all if saving the INLA results.
        seed.n.spatial.maps <- spatial.maps
        # If we are just getting the spatial maps for the seedboxes then we want all of these plots
        # We also want them all if saving the INLA results.
        if(any(plots %in% "seedboxes")) seed.n.spatial.maps <- c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial",
                                                                 "MW-spatial","SH-spatial","MW.GP-spatial","SH.GP-spatial")
        
        # Next we get the survey locations
        if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","BanIce","SPB","GB"))
        {   
          loc <- cbind(surv.Live[[banks[i]]]$lon[surv.Live[[banks[i]]]$year == yr],
                     surv.Live[[banks[i]]]$lat[surv.Live[[banks[i]]]$year == yr])
          # These are the locations for the Condition and meat count data.
          loc.cf <- cbind(CF.current[[banks[i]]]$lon[CF.current[[banks[i]]]$year == yr],
                          CF.current[[banks[i]]]$lat[CF.current[[banks[i]]]$year == yr])
          # For the growth potential related figures we also need to make a special mesh as there could be some tows with 0 individuals
          # and these may screw up the INLA'ing
          loc.gp <- cbind(pot.grow[[banks[i]]]$slon[pot.grow[[banks[i]]]$year == yr],
                          pot.grow[[banks[i]]]$slat[pot.grow[[banks[i]]]$year == yr])
        }# end if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","SPB","GB"))
        # I want 1 mesh for all of Georges bank summer survey.
        if(banks[i] %in% c("GBa","GBb")) 
        {
           loc <- cbind(c(surv.Live[["GBa"]]$lon[surv.Live[["GBa"]]$year == yr],surv.Live[["GBb"]]$lon[surv.Live[["GBb"]]$year == yr]),
                        c(surv.Live[["GBa"]]$lat[surv.Live[["GBa"]]$year == yr],surv.Live[["GBb"]]$lat[surv.Live[["GBb"]]$year == yr]))
           # The condition and meat count data.
           loc.cf <- cbind(c(CF.current[["GBa"]]$lon[CF.current[["GBa"]]$year == yr],CF.current[["GBb"]]$lon[CF.current[["GBb"]]$year == yr]),
                           c(CF.current[["GBa"]]$lat[CF.current[["GBa"]]$year == yr],CF.current[["GBb"]]$lat[CF.current[["GBb"]]$year == yr]))
           # For the growth potential related figures we also need to make a special mesh as there could be some tows with 0 individuals
           # and these may screw up the INLA'ing
           loc.gp <- cbind(c(pot.grow[["GBa"]]$slon[pot.grow[["GBa"]]$year == yr],pot.grow[["GBb"]]$slon[pot.grow[["GBb"]]$year == yr]),
                           c(pot.grow[["GBa"]]$slat[pot.grow[["GBa"]]$year == yr],pot.grow[["GBb"]]$slat[pot.grow[["GBb"]]$year == yr]))
           
        } # end if(banks[i] %in% c("GBa","GBb") 
        
        # Convert the sp boundary object to a mesh boundary for INLA.
        bound <- inla.sp2segment(bound.poly.surv.sp)
        xyl <- rbind(x=range(bound$loc[,1]), y=range(bound$loc[,2])) # get the xy ranges of our survey extent.
        
        # This is how the mesh and A matrix are constructed
        # Build the mesh, for our purposes I'm hopeful this should do the trick, the offset makes the area a bit larger so the main predictions 
        #  should cover our entire survey area.
        cat("ALERT!  I'm building the mesh for",banks[i], "if this hangs here please try using a different offset for this bank.. \n")
        if(banks[i] != "GB") mesh <- inla.mesh.2d(loc, max.edge=c(0.03,0.075), offset=offset)
        if(banks[i] == "GB") mesh <- inla.mesh.2d(loc, max.edge=c(0.04,0.075), offset=offset)
        #windows(11,11) ; plot(mesh) ; plot(bound.poly.surv.sp,add=T,lwd=2)
        cat("Mesh successful, woot woot!!")
        # Now make the A matrix
        A <- inla.spde.make.A(mesh, loc)
        A.cf <- inla.spde.make.A(mesh,loc.cf)
        
        
        # We can just make the one spde object for all of these as well.
        spde <- inla.spde2.pcmatern(mesh,    
                                    prior.sigma=c(1,0.5), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                    prior.range=c(0.1,0.5)) # The Meidan range and the probability that the range is less than this..
        # Because of the generally thin spacing on GB we need to decrease the spatial correlation distance and allow for more spatial variability in the 
        # data, so I have changed the priors...  Revised by DK August 2018, not fully incorporated into the Spring Survey summary presentation
        # previous prior.range was c(1,0.5) (July 2018 spring survey summary used this value)
        if(banks[i] == "GB") 
        {
          spde <- inla.spde2.pcmatern(mesh,    
                                    prior.sigma=c(4,0.75), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                    prior.range=c(0.1,0.5)) # The Meidan range and the probability that the range is less than this..
        }
        ## All of our abundance spatial plots are counts, so for our simple purposes a poisson is o.k.
        family1 = "poisson"
        family1.cf <- "gaussian" # For CF, MC,MW, and SH they are more normal so go with a gaussian.
        family.clap <- "poisson" # I haven't found a good family for the clapper data, for the moment the poisson does a decent job as long
        # as we don't have very high clapper values (i.e. near 100%), it can get weird there, but I can't find a better likelihood yet...
        family.gp <- "lognormal" # This could use some more thought....
        
        # but I need to truncate the predicted values that are > 100 to be 100 which is BS...
        # As soon as you make a spatial model make your own intercept.  Here is
        a0 <- 1 # intercept
        # Mostly just using stock priors, again fine for our purposes for the moment.
        pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
        # Add an index to the data
        # The spatial model, simple model with a intercept (overall bank average) with the spde spatial component
        # basically the random deviations for each piece of the mesh.
        formula3 <- y ~ 0 + a0 + f(s, model=spde)
  
        # if we have maps to be made and we aren't simply loading in the INLA results we need to run this bit.
        if(length(seed.n.spatial.maps) > 0)
        {
          # Get the data needed....
          if(banks[i] %in% c("GBb","GBa")) 
          {
            tmp.dat <- rbind(surv.Live[["GBa"]][surv.Live[["GBa"]]$year == yr,],surv.Live[["GBb"]][surv.Live[["GBb"]]$year == yr,])
            tmp.cf <- rbind(CF.current[["GBa"]],CF.current[["GBb"]]) 
            tmp.clap <- rbind(surv.Clap[["GBa"]][surv.Clap[["GBa"]]$year == yr,],surv.Clap[["GBb"]][surv.Clap[["GBb"]]$year == yr,])
            tmp.gp <- rbind(pot.grow[["GBa"]][pot.grow[["GBa"]]$year == yr,],pot.grow[["GBb"]][pot.grow[["GBb"]]$year == yr,])
          }  # end if(banks[i] %in% c("GBb","GBa")) 
  
          if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","BanIce","SPB","GB")) 
          {  
            tmp.dat <- surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,]
            tmp.cf <- CF.current[[banks[i]]]
            tmp.clap <- surv.Clap[[banks[i]]][surv.Clap[[banks[i]]]$year == yr,]
            tmp.gp <- pot.grow[[banks[i]]][pot.grow[[banks[i]]]$year == yr,]
          } # end if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","BanIce","SPB","GB")) 
          # Now loop through each spatial map we want to make.
          for(k in 1:length(seed.n.spatial.maps))
          {
            # In the next bunch of if statements we run the INLA model and we get the figure titles sorted out.
            if(seed.n.spatial.maps[k] == "PR-spatial")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.dat$pre, link=1L),
                              effects=list(data.frame(a0=rep(1, nrow(tmp.dat))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family1, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
            } # end if(seed.n.spatial.maps[k] == "PR-spatial")   
          
            if(seed.n.spatial.maps[k] == "Rec-spatial")        
            {
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.dat$rec, link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.dat)), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family1, data = inla.stack.data(stk),#control.family= control.family1,
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
            } # end if(seed.n.spatial.maps[k] == "Rec-spatial") 
            
            if(seed.n.spatial.maps[k] == "FR-spatial") 
            {
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.dat$com, link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.dat)), s = 1:spde$n.spde),
                                A = list(1, A))
              print(stk)
                          # This is the INLA model itself
              mod <- inla(formula3, family=family1, data = inla.stack.data(stk),#control.family= control.family1,
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
            } # end if(seed.n.spatial.maps[k] == "FR-spatial")
              
            if(seed.n.spatial.maps[k] == "CF-spatial")       
            {
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.cf$CF, link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.cf)), s = 1:spde$n.spde),
                                A = list(1, A.cf))
              # This is the INLA model itself
              mod <- inla(formula3, family=family1.cf, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
            }
            # THis seems to be making sense...
            if(seed.n.spatial.maps[k] == "MC-spatial")      
            {
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.cf$meat.count, link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.cf)), s = 1:spde$n.spde),
                                A = list(1, A.cf))
              # This is the INLA model itself
              mod <- inla(formula3, family=family1.cf, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
            } # end if(seed.n.spatial.maps[k] == "MC-spatial") 
            
            if(seed.n.spatial.maps[k] == "Clap-spatial")        
            {
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.clap$clap.prop, link=1L),
                                effects=list(a0 = rep(1, nrow(tmp.clap)), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.clap, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
            } # end if(seed.n.spatial.maps[k] == "Clap-spatial")  
            
            if(seed.n.spatial.maps[k] == "MW-spatial")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.gp$cur.mw, link=1L),
                                effects=list(data.frame(a0=rep(1, nrow(tmp.gp))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.gp, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
            } # end if(seed.n.spatial.maps[k] == "PR-spatial")  
            
            if(seed.n.spatial.maps[k] == "SH-spatial")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.gp$cur.sh, link=1L),
                                effects=list(data.frame(a0=rep(1, nrow(tmp.gp))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.gp, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
            } # end if(seed.n.spatial.maps[k] == "PR-spatial")  
            
            if(seed.n.spatial.maps[k] == "MW.GP-spatial")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.gp$gp.mw, link=1L),
                                effects=list(data.frame(a0=rep(1, nrow(tmp.gp))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.gp, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
            } # end if(seed.n.spatial.maps[k] == "PR-spatial")  
            
            if(seed.n.spatial.maps[k] == "SH.GP-spatial")    
            {
              # This is the stack for estimation from the INLA model
              stk <- inla.stack(tag="est",data=list(y = tmp.gp$gp.sh, link=1L),
                                effects=list(data.frame(a0=rep(1, nrow(tmp.gp))), s = 1:spde$n.spde),
                                A = list(1, A))
              # This is the INLA model itself
              mod <- inla(formula3, family=family.gp, data = inla.stack.data(stk),
                          control.predictor=list(A=inla.stack.A(stk),link=1L, compute=T))
            } # end if(seed.n.spatial.maps[k] == "PR-spatial")  
            
            # Now that this is done we need to make a prediction grid for projection onto our mesh,
            proj <- inla.mesh.projector(mesh,xlim=xyl[1, ], ylim=xyl[2,],dims = s.res) # 500 x 500 gives very fine results but is slow.        
            # Then make a matrix of the correct dimension, first for the poisson models
            
            if(seed.n.spatial.maps[k] %in% c("FR-spatial","PR-spatial","Rec-spatial","Clap-spatial","SH-spatial","SH.GP-spatial",
                                             "MW-spatial","MW.GP-spatial")) mod.res[[seed.n.spatial.maps[k]]] <- 
                                                                            inla.mesh.project(proj, exp(mod$summary.random$s$mean + mod$summary.fixed$mean))
            # Now for the Gaussian models.
            if(seed.n.spatial.maps[k] %in% c("CF-spatial","MC-spatial")) mod.res[[seed.n.spatial.maps[k]]] <- 
                                                                            inla.mesh.project(proj, mod$summary.random$s$mean + mod$summary.fixed$mean)
            # Get rid of all data outside our plotting area, necessary for the full model runs only.
            # We use this later for our visualization...
            if(banks[i] != "Sab" && banks[i] != "Mid"  && banks[i] != "Ban" && banks[i] != "BanIce") pred.in <- inout(proj$lattice$loc,bound$loc) 
            
            # Because there are holes in the survey strata on Sable things are a bit more complex...
            if(banks[i] %in% c("Sab","Mid"))
            {
              simplemesh <- inla.mesh.2d(boundary = bound,max.edge = 1e9)
              pred.in <- inla.mesh.projector(simplemesh,proj$lattice$loc)$proj$ok
            } # end if(banks[i] == "Sab")
      
            if(banks[i] %in% c("Ban", "BanIce"))
            {
              bound2 <- inla.sp2segment(ban.poly.clip.sp)
              simplemesh <- inla.mesh.2d(boundary = bound,max.edge = 1e9)
              pred.in <- inla.mesh.projector(simplemesh,proj$lattice$loc)$proj$ok
            }
            # this is the clipping step for all banks except Ban
            #if(!banks[i] %in% c("Ban", "BanIce")) 
            mod.res[[seed.n.spatial.maps[k]]][!pred.in] <- NA
            
            # for the Clapper model I need to make sure all the values are < 100...
            if(seed.n.spatial.maps[k] == "Clap-spatial")  mod.res[[seed.n.spatial.maps[k]]][mod.res[[seed.n.spatial.maps[k]]] > 100] <- 100
          } # end for(k in 1:length(seed.n.spatial.maps)) # End the loop for getting all the data needed for a bank for the spatial maps.
        } # end if(length(seed.n.spatial.maps > 0))
      } # end the if(length(grep("run",INLA)) > 0)
      print("finished running normal models")
      ### The user shell height bins....
      # Now we need to get the projections if we have specified the User.SH.bins plots to be produced.
      bin.names <- NULL # Name bin.names a NULL, if no user.SH.bins we still need this name to exist...
      # Here I want to retain the "run.full" INLA call as I only want to enter this loop if I've asked for USER bins, or I asked to run the full model
      # This avoids running all the models for the bins needlessly and enables INLA="run" option to produce a figure in less than 1 minute
      if(any(plots == "user.SH.bins") || INLA == "run.full")
      {
        # Get the number of these calculations we need to make..
        if(banks[i]  != "Ger") 
        {
          num.bins <- length(survey.obj[[banks[i]]]$bin.names)
          bin.names <- survey.obj[[banks[i]]]$bin.names
          user.bins <- survey.obj[[banks[i]]]$user.bins
        } # end if(banks[i]  != "Ger") 
        if(banks[i]  == "Ger")
        {
          num.bins <- length(spr.survey.obj$bin.names)
          bin.names <- spr.survey.obj$bin.names
          user.bins <- spr.survey.obj$user.bins
        } # end if(banks[i]  == "Ger")
        
        # Get the data needed....
        if(banks[i] %in% c("GBb","GBa")) 
        {
          tmp.dat <- rbind(surv.Live[["GBa"]][surv.Live[["GBa"]]$year == yr,],surv.Live[["GBb"]][surv.Live[["GBb"]]$year == yr,])
        }  # end if(banks[i] %in% c("GBb","GBa")) 
        
        if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","BanIce","SPB","GB")) 
        {  
          tmp.dat <- surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,]
        } # end if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","SPB","GB")) 
        
        # Only run the models if not loading them....
        if(length(grep("run",INLA)) > 0 & !(banks[i] =="BanIce" & yr<2018))
        {
          # Now run through each bin...
          for(k in 1:num.bins)
          {
            # In the next bunch of if statements we run the INLA model and we get the figure titles sorted out.
            # This is the stack for the INLA model
            pick <- which(names(tmp.dat) == bin.names[k])
            stk <- inla.stack(tag="est",data=list(y = tmp.dat[,pick], link=1L),
                              effects=list(a0 = rep(1, nrow(tmp.dat)), s = 1:spde$n.spde),
                              A = list(1, A))
            # This is the INLA model itself
            mod <- inla(formula3, family=family1, data = inla.stack.data(stk), 
                        control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
            # Now that this is done we need to make a prediction grid for projection onto our mesh,
            proj <- inla.mesh.projector(mesh,xlim=xyl[1, ], ylim=xyl[2,],dims = s.res) # 500 x 500 gives very fine results but is slow.        
            # Get rid of all data outside our plotting area, necessary for the full model runs only.
            # We use this later for our visualization...
            if(banks[i] != "Sab" && banks[i] != "Mid") pred.in <- inout(proj$lattice$loc,bound$loc) 
            # For Sable we need to do this b/c of the holes in the bank.
            if(banks[i] %in% c("Sab","Mid"))
            {
              simplemesh <- inla.mesh.2d(boundary = bound,max.edge = 1e9)
              pred.in <- inla.mesh.projector(simplemesh,proj$lattice$loc)$proj$ok
            } # end if(banks[i] == "Sab")
            # Then make a matrix of the correct dimension
            mod.res[[bin.names[k]]] <- inla.mesh.project(proj, exp(mod$summary.random$s$mean + mod$summary.fixed$mean))
            field <- exp(mod$summary.random$s$mean + mod$summary.fixed$mean)
            # Get rid of all data outside our plotting area...
            if(!banks[i] %in% c("Ban", "BanIce")) 
            mod.res[[bin.names[k]]][!pred.in] <- NA
            print(k)
          } # End for(k in 1:num.bins)
        } #end if(length(grep("run",INLA)) > 0)
      }# end i if(any(plots == "user.SH.bins") || length(grep("run",INLA)) > 0)
      print("finished running user bin models")
      # Now here we can save the results of all INLA runs for each bank rather than having to run these everytime which can be rather slow
      # Results are only saved if the option 'run.full' is chosen
      if(INLA == 'run.full') 
      {
        save(mod.res,proj,mesh,pred.in,field,
             file = paste(direct,"Data/Survey_data/", yr, "/Survey_summary_output/" ,banks[i],"_figures_res_",s.res[1],"-",s.res[2], ".RData",sep=""))
      } # end if(save.INLA ==T) 
      
      #######################  FIGURES#######################  FIGURES#######################  FIGURES#######################  FIGURES ##################
      #######################  FIGURES#######################  FIGURES#######################  FIGURES#######################  FIGURES ##################
      # And if the results already exist we can instead just load them, return a warning message if the file we want doesn't exist yet and stop the function
      if(INLA == 'load') 
      {
       # If the file doesn't exist we stop...
       if(file.exists(paste(direct,"Data/Survey_data/", yr, "/Survey_summary_output/" ,banks[i],"_figures_res_",s.res[1],"-",s.res[2], ".RData",sep="")) ==F)
       {
         stop(paste("Hello",as.character(Sys.info()["user"]),"You wanted to load the INLA results from the file... ", 
              paste(direct,"Data/Survey_data/", yr, "/Survey_summary_output/" ,banks[i],"_figures_res_",s.res[1],"-",s.res[2], ".RData",sep=""), 
              "which sadly doesn't exist, make it exist and I'll giver a try again for you. Cheers", 
              as.character(Sys.info()["nodename"])))
        } # end if(file.exists(paste...
        #s.maps <- spatial.maps # Make sure I don't overwrite which map I'm making....
        load(file = paste(direct,"Data/Survey_data/", yr, "/Survey_summary_output/" ,banks[i],"_figures_res_",s.res[1],"-",s.res[2], ".RData",sep=""))
        #spatial.maps <- s.maps
      } # end if(INLA == 'load') 
      
      ####################### Spatial Maps####################### Spatial Maps####################### Spatial Maps####################### Spatial Maps
      ####################### Spatial Maps####################### Spatial Maps####################### Spatial Maps####################### Spatial Maps
      # This plots the spatial maps requested, need this m loop so we can plot only the figures requested for spatial plots (needed to avoid plotting
      # everything when you want the seedboxes plotted).
      if(length(spatial.maps) > 0 || any(plots %in% "user.SH.bins")) # first only run this section if we want a spatial map (this excludes the seedbox spatail maps (at the end of function)
                                                                     # or the user specified SH bins.)
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
        
        count = 0
        # Make the maps...
        for(m in 1:n.maps)
        {
          # This sets up our color ramps and titles for each of the spatial plots
          if(maps.to.make[m]  %in% c("PR-spatial", "Rec-spatial", "FR-spatial")) 
          {
            base.lvls=c(0,1,5,10,50,100,500,1000,2000,5000,10000,20000,50000,1e6)
            cols <- c(rev(plasma(length(base.lvls[base.lvls < 2000]),alpha=0.7,begin=0.6,end=1)),
                      rev(plasma(length(base.lvls[base.lvls > 1000])-1,alpha=0.8,begin=0.1,end=0.5)))
            if(!is.null(colour.bins)){
              base.lvls=colour.bins
              cols <- c(rev(plasma(length(base.lvls[base.lvls < 700]),alpha=0.7,begin=0.6,end=1)),
                        rev(plasma(length(base.lvls[base.lvls > 200])-1,alpha=0.8,begin=0.1,end=0.5)))
            }
            
            max.lvl <- which(base.lvls >= max(mod.res[[maps.to.make[m]]],na.rm=T))[1]
            lvls <- base.lvls[1:max.lvl]
            cols <- cols[1:(max.lvl-1)]
            
            # Get the levels for the legend.
            ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'+',sep='')),
                                                 leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'-',lvls[max.lvl],sep='')))
            # Now set up the figure titles, different title depending on the spatial map here.
            if(maps.to.make[m]  == "FR-spatial")
            {
              fig.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-",year,")",sep="")),
                                          list(a=as.character(CS),year=as.character(yr),bank=banks[i]))
              if(banks[i] == "GB") fig.title <- substitute(bold(paste("Fully recruited scallops (" ,"">=a, " mm " , bank,"-Spr-",year,")",sep="")),
                                                               list(a=as.character(CS),year=as.character(yr),bank=banks[i]))
            } # end if(maps.to.make[m]  == "FR-spatial")
            if(maps.to.make[m]  == "Rec-spatial")
            {
              fig.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-",year,")",sep="")),
                                    list(a=as.character(CS-1),b=as.character(RS),year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Recruit scallops (",b- a, " mm " , bank,"-Spr-",year,")",sep="")),
                                                         list(a=as.character(CS-1),b=as.character(RS),year=as.character(yr),bank=banks[i]))
            } # end if(maps.to.make[m]  == "Rec-spatial")
            if(maps.to.make[m]  == "PR-spatial")
            {
              
              fig.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-",year,")",sep="")),
                                        list(b=as.character(RS),year=as.character(yr),bank=banks[i]))
              if(banks[i] == "GB") fig.title <- substitute(bold(paste("Pre-recruit scallops (" ,""<b, " mm " , bank,"-Spr-",year,")",sep="")),
                                                             list(b=as.character(RS),year=as.character(yr),bank=banks[i]))
            }# end if(maps.to.make[m]  == "PR-spatial")
            # And the legend title
            leg.title <- N.tow.lab
          } # end if(maps.to.make[m]  %in% c("PR-spatial", "Rec-spatial", "FR-spatial") 
          
          # Now for the condition factor
          if(maps.to.make[m]  %in% c("CF-spatial"))   
          {
            base.lvls <- c(0,5,8,10,12,14,16,18,50)
            cols <- rev(inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            # Get the levels correct            
            min.lvl <- max(which(base.lvls <= min(mod.res[[maps.to.make[m]]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(mod.res[[maps.to.make[m]]],na.rm=T)))
            lvls <- base.lvls[min.lvl:max.lvl]
            cols <- cols[min.lvl:(max.lvl-1)]
            ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'+',sep='')),
                                                 leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
            
            fig.title <- substitute(bold(paste("Condition factor (", bank,"-",year,")",sep="")),
                                   list(year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Condition factor (", bank,"-Spr-",year,")",sep="")),
                                                        list(year=as.character(yr),bank=banks[i]))
            leg.title <- cf.lab
            
          } # end if(maps.to.make[m]  %in% c("CF-spatial")   
          
          # Now for the meat count
          if(maps.to.make[m]  %in% c("MC-spatial"))
          {
            # The color ramps for MC
            base.lvls <- c(seq(0,50,5),1000)
            cols <- viridis(length(base.lvls)-1,alpha=0.7,begin=0,end=1)
            # Get the levels correct            
            min.lvl <- max(which(base.lvls <= min(mod.res[[maps.to.make[m]]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(mod.res[[maps.to.make[m]]],na.rm=T)))
            lvls <- base.lvls[min.lvl:max.lvl]
            cols <- cols[min.lvl:(max.lvl-1)]
            ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'+',sep='')),
                                                 leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
            
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
            cols <- rev(plasma(length(base.lvls)-1,alpha=0.7))
            # Get the levels correct            
            min.lvl <- max(which(base.lvls <= min(mod.res[[maps.to.make[m]]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(mod.res[[maps.to.make[m]]],na.rm=T)))
            lvls <- base.lvls[1:max.lvl]
            cols <- cols[min.lvl:(max.lvl-1)]
            ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'+',sep='')),
                                                 leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
            
            # And get the labels for the figures...
            fig.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-",year,")",sep="")),
                                    list(c=as.character(RS),bank=banks[i],year=as.character(yr)))
            if(banks[i] == "GB") clap.dis.title <- substitute(bold(paste("Clappers (% dead ","">=c, " mm ", bank,"-Spr-",year,")",sep="")),
                                                              list(c=as.character(RS),bank=banks[i],year=as.character(yr)))
            leg.title <- "% Dead"
          } # end if(maps.to.make[m]  %in% c("Clap-spatial")
          
          
          if(maps.to.make[m]  %in% c("MW-spatial"))   
          {
            base.lvls <- c(0,2,4,6,8,10,12,14,16,18,20,30,50,100)
            cols <- rev(inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            # Get the levels correct            
            min.lvl <- max(which(base.lvls <= min(mod.res[[maps.to.make[m]]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(mod.res[[maps.to.make[m]]],na.rm=T)))
            lvls <- base.lvls[min.lvl:max.lvl]
            cols <- cols[min.lvl:(max.lvl-1)]
            ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'+',sep='')),
                   leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                 paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
            
            fig.title <- substitute(bold(paste("Meat Weight (", bank,"-",year,")",sep="")),
                                    list(year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Meat Weight (", bank,"-Spr-",year,")",sep="")),
                                                         list(year=as.character(yr),bank=banks[i]))
            leg.title <- "Meat Weight (g)"
            
          } # end if(maps.to.make[m]  %in% c("MW-spatial")  
          
          if(maps.to.make[m]  %in% c("SH-spatial"))   
          {
            base.lvls <- c(0,50,70,80,90,100,110,120,150,200)
            cols <- rev(inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            # Get the levels correct            
            min.lvl <- max(which(base.lvls <= min(mod.res[[maps.to.make[m]]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(mod.res[[maps.to.make[m]]],na.rm=T)))
            lvls <- base.lvls[min.lvl:max.lvl]
            cols <- cols[min.lvl:(max.lvl-1)]
            ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'+',sep='')),
                   leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                 paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
            
            fig.title <- substitute(bold(paste("Shell Height (", bank,"-",year,")",sep="")),
                                    list(year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Shell Height (", bank,"-Spr-",year,")",sep="")),
                                                         list(year=as.character(yr),bank=banks[i]))
            leg.title <- "Shell Height (mm)"
            
          } # end if(maps.to.make[m]  %in% c("SH-spatial")  
          
          
          if(maps.to.make[m]  %in% c("SH.GP-spatial"))   
          {
            base.lvls <- c(0,0.05,0.1,0.2,0.3,0.5,0.75,1,2,10)
            cols <- rev(inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            # Get the levels correct            
            min.lvl <- max(which(base.lvls <= min(mod.res[[maps.to.make[m]]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(mod.res[[maps.to.make[m]]],na.rm=T)))
            lvls <- base.lvls[min.lvl:max.lvl]
            cols <- cols[min.lvl:(max.lvl-1)]
            ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'+',sep='')),
                   leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                 paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
            
            fig.title <- substitute(bold(paste("Growth Potential SH  (", bank,"-",year,")",sep="")),
                                    list(year=as.character(yr),bank=banks[i]))
            if(banks[i] == "GB") fig.title <- substitute(bold(paste("Growth Potential SH (", bank,"-Spr-",year,")",sep="")),
                                                         list(year=as.character(yr),bank=banks[i]))
            leg.title <- "Growth Potential (SH)"
            
          } # end if(maps.to.make[m]  %in% c("SH.GP-spatial")  
          if(maps.to.make[m]  %in% c("MW.GP-spatial"))   
          {
            base.lvls <- c(0,0.05,0.1,0.2,0.3,0.5,0.75,1,2,10)
            cols <- rev(inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
            # Get the levels correct            
            min.lvl <- max(which(base.lvls <= min(mod.res[[maps.to.make[m]]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(mod.res[[maps.to.make[m]]],na.rm=T)))
            lvls <- base.lvls[min.lvl:max.lvl]
            cols <- cols[min.lvl:(max.lvl-1)]
            ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'+',sep='')),
                   leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                 paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
            
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
            cols <- c(rev(plasma(length(base.lvls[base.lvls < 2000]),alpha=0.7,begin=0.6,end=1)),
                      rev(plasma(length(base.lvls[base.lvls > 1000])-1,alpha=0.8,begin=0.1,end=0.5)))
            #user-defined colour bins
            if(!is.null(colour.bins)){
              base.lvls=colour.bins
              cols <- c(rev(plasma(length(base.lvls[base.lvls < 700]),alpha=0.7,begin=0.6,end=1)),
                        rev(plasma(length(base.lvls[base.lvls > 200])-1,alpha=0.8,begin=0.1,end=0.5)))
            }
            
            if(length(grep("bm",maps.to.make[m])) > 0) # if we are looking at biomass figures...
            {
              base.lvls= c(0,0.005,0.01,0.05,0.1,0.5,1,2,5,10,20,50,1000)
              cols <- c(rev(magma(length(base.lvls[base.lvls < 1]),alpha=0.7,begin=0.6,end=1)),
                        rev(magma(length(base.lvls[base.lvls > 0.5])-1,alpha=0.8,begin=0.1,end=0.5)))
            } # end if(length(grep("bm",maps.to.make[m])) > 0) 
            
            max.lvl <- which(base.lvls >= max(mod.res[[maps.to.make[m]]],na.rm=T))[1]
            lvls <- base.lvls[1:max.lvl]
            cols <- cols[1:(max.lvl-1)]
            
            # Get the levels for the legend.
            ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                               paste(lvls[length(lvls)-1],'+',sep='')),
                   leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                 paste(lvls[length(lvls)-1],'-',lvls[max.lvl],sep='')))
            
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
          

          
          
          # Don't add the titles?
          if(add.title == F) fig.title <- ""
          # Where to get the bathymetry data, note this overwrites any call in the function above
          ifelse(banks[i] %in% c("Mid","Sab","Ban","BanIce"), bath <- "quick", bath <- "usgs")
          ifelse(banks[i] %in% c("Mid","Sab","Ban","BanIce"), iso <- c(seq(50,200,by=50)), iso <- c(seq(40,140,by=20)))
          
          ############  End prepartion of levels and figure annotations for spatial figures############  
          ############  End prepartion of levels and figure annotations for spatial figures#######
          ############  End prepartion of levels and figure annotations for spatial figures###
          
          ######## Produce the figure######## Produce the figure######## Produce the figure######## Produce the figure
          ######## Produce the figure######## Produce the figure######## Produce the figure######## Produce the figure
          # Do we want to save the figure to a file or just output to the screen?  
          if(fig == "png") png(paste(plot.dir,maps.to.make[m],".png",sep=""),units="in",width = 11,height = 8.5,res=420,bg = "transparent")
          if(fig == "pdf") pdf(paste(plot.dir,maps.to.make[m],".pdf",sep=""),width = 11,height = 8.5,bg = "transparent")
          if(fig == "screen") windows(11,8.5)
          
          par(mfrow=c(1,1))

          # This is one figure to rule all
          if(!banks[i] =="BanIce") ScallopMap(banks[i],title=fig.title,bathy.source=bath,isobath = iso,
                     plot.bathy = T,plot.boundries=T,boundries="offshore",
                     direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F,add.scale = F)
          
          if(banks[i] == "BanIce") ScallopMap("Ban",title=fig.title,bathy.source=bath,isobath = iso,
                                              plot.bathy = T,plot.boundries=T,boundries="offshore",
                                              direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F,add.scale = F)
          
          # If we have a layer to add add it...
          if(!is.null(mod.res[[maps.to.make[m]]])) 
          {
            image(list(x = proj$x, y=proj$y, z = mod.res[[maps.to.make[m]]]), axes=F,add=T,breaks = lvls,col=cols)
            if(contour == T) contour(x = proj$x, y=proj$y, z = mod.res[[maps.to.make[m]]], axes=F,add=T,levels = lvls,col="grey",drawlabels=F,lwd=1)
          } # end if(!is.null(mod.res[[maps.to.make[m]]])) 
          if(!banks[i] %in% c("Ban", "BanIce")) plot(bound.poly.surv.sp,add=T,lwd=2)
          if(banks[i] %in% c("Ban", "BanIce")) maps::map.scale(x = -59.55, y=43.97,relwidth = 0.15,ratio=F)
          ################ ENd produce the figure################ ENd produce the figure################ ENd produce the figure
          ################ ENd produce the figure################ ENd produce the figure################ ENd produce the figure

          
          ############  Add the points and the legend to the figure############  Add the points and the legend to the figure
          ############  Add the points and the legend to the figure############  Add the points and the legend to the figure
          ############  Add the points and the legend to the figure############  Add the points and the legend to the figure
          # Add the regular survey tows, note this if statement is used to NOT add the following code to these plots...
          if(maps.to.make[m] %in% c("MC-spatial", "CF-spatial","MW-spatial","MW.GP-spatial")==F)
          {
            points(lat~lon,surv.Live[[banks[i]]],subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
            # In case any of these banks has exploratory tows...
            if(banks[i] %in% c("BBn","Sab","Mid","GBb","BBs"))  
            {
  
              points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
                     & state =='live' & random %in% c(0,2,3,4,5),pch=24,bg="darkorange",cex=0.8)
              legend("topleft",legend = c(paste('exploratory (n =',
                                                 length(unique(subset(surv.Live[[banks[i]]],year==yr & random %in% c(0,2,3,4,5))$tow)),
                                                 ")",sep=""),
                                           paste('regular (n =',
                                                 length(unique(subset(surv.Live[[banks[i]]],year==yr & random==1)$tow)),
                                                 ")",sep="")),title="Tow type",
                     pt.bg = c("darkorange","black"),pch=c(24,20),inset=0.01,bg=NA,box.col=NA)
            } # end if(banks[i] %in% c("BBn","Sab","Mid","GBb","BBs"))  
            
            if(banks[i] %in% c("GBa"))  
            {
              
              points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
                     & state =='live' & random %in% c(0,2,3,4,5),pch=24,bg="darkorange",cex=0.8)
              legend("bottomright",legend = c(paste('exploratory (n =',
                                                length(unique(subset(surv.Live[[banks[i]]],year==yr & random %in% c(0,2,3,4,5))$tow)),")",sep=""),
                                              paste('regular (n =',
                                                length(unique(subset(surv.Live[[banks[i]]],year==yr & random==1)$tow)),
                                                ")",sep="")),title="Tow type",
                     pt.bg = c("darkorange","black"),pch=c(24,20),inset=0.01,bg=NA,box.col=NA)
              
              
            } # end if(banks[i] %in% c("BBn","Sab","Mid","GBb","BBs"))  
              
            # Add the legends, for German we also have to add the "matched tows"
            if(banks[i] == "Ger") 
            {
              points(lat~lon,surv.Live[[banks[i]]],subset=year==yr &
                       random==3,pch=22,bg="yellow",cex=0.8)
              points(lat~lon,surv.Live[[banks[i]]],subset=year==yr &
                       random %in% c(0,2,4,5),pch=24,bg="darkorange",cex=0.8)
              
              legend('topleft',legend=
                       c(paste('regular (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                                     random==1)$tow)),")", sep=""),
                         paste('repeated (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                                      random==3)$tow)),")", sep=""),
                         paste('exploratory (n =',
                               length(unique(subset(surv.Live[[banks[i]]],year==yr & random %in% c(0,2,4,5))$tow)),")",sep="")),
                     pch=c(20,22,24), pt.bg = c("black","yellow","darkorange"),bty='n',cex=1, inset = .02,,bg=NA,box.col=NA)
            } # end if(banks[i] == "Ger") 
            
            # For the banks without exploratory tows we add this legend.
            if(banks[i] %in% c("Ban","BanIce")) 
            {
              legend("topleft",pch=c(20), pt.bg = c("black"), title="Tow type",
                legend = paste('regular (n =',length(unique(subset(surv.Live[[banks[i]]],
                 year==yr & random==1)$tow)),")",sep=""), inset=0.01,bg=NA,box.col=NA)	
            } # end if(banks[i] == "Sab" || banks[i] == "Mid"|| banks[i] == "BBs" || banks[i] == "Ban"|| banks[i] == "GBb" || banks[i] == "GB") 
            
            if(banks[i] == "GB") 
            {
              points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
                     & state =='live' & random%in% c(2,4,5),pch=24,bg="darkorange",cex=0.8)
              points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
                     & state =='live' & random==3,pch=22,bg="yellow",cex=0.8)
              legend("bottomright",legend = c(paste('exploratory (n =',
                                                 length(unique(subset(surv.Live[[banks[i]]],year==yr & random %in% c(2,4,5))$tow)),")",sep=""),
                                           paste('repeated (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                                                        random==3)$tow)),")", sep="")),title="Tow type",
                     pt.bg = c("darkorange","yellow"),pch=c(24,22),inset=0.01,,bg=NA,box.col=NA)
            } # end if(banks[i] == "Sab" || banks[i] == "Mid"|| banks[i] == "BBs" || banks[i] == "Ban"|| banks[i] == "GBb" || banks[i] == "GB") 
     
            
            
            # For these plots the legend goes like this. Note: FK hacked in here temporarily to fix the 2018 GBa pre-recruit figure to hide the largest lvl.  
            if(maps.to.make[m] %in% c("PR-spatial", "Rec-spatial", "FR-spatial",bin.names, "SH-spatial", "SH.GP-spatial"))
            {
              legend("bottomleft",leg.lvls,fill=cols,
                   title=leg.title, title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
                   pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg=NA,box.col=NA)
            } # END if(seed.n.spatial.maps[k] %in% c("Pre-recruits", "Recruits", "Fully_Recruited","Clappers"))
            
            # For these plots the legend goes like this     
            if(maps.to.make[m] %in% c("Clap-spatial"))
            {
              legend("bottomleft",leg.lvls,fill=cols,
                     title=leg.title, title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
                     pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg=NA,box.col=NA)
            } # END if(seed.n.spatial.maps[k] %in% c("Pre-recruits", "Recruits", "Fully_Recruited","Clappers"))
          } # end if(maps.to.make[m] %in% c("MC-spatial", "CF-spatial","MW-spatial","MW.GP.spatial")==F)
          # For condition and meat count we set things up a little bit differently.
          if(maps.to.make[m] %in% c("MW.GP-spatial","MW-spatial","CF-spatial","MC-spatial"))
          {
            points(lat~lon,CF.current[[banks[i]]],pch=21,bg='grey50',cex=0.8)
            legend("topleft",pch=c(21), pt.bg = c("grey50"), title="Tow type",
                   legend = paste('Detailed Sampling (n =',length(CF.current[[banks[i]]]$tow),")",sep=""), inset=0.01,bg=NA,box.col=NA)	
            legend("bottomleft",leg.lvls,fill=cols,
                   title=leg.title, title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
                   pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg=NA,box.col=NA)
          } # end if(maps.to.make[m] %in% c("CF-spatial","MC-spatial"))
        
          
         # } # END if(seed.n.spatial.maps[k] %in% c("Pre-recruits", "Recruits", "Fully_Recruited","Clappers"))
        #} # end if(seed.n.spatial.maps[k] != "MC-spatial" && seed.n.spatial.maps[k] != "CF-spatial")
        # For condition and meat count we set things up a little bit differently.
        if(maps.to.make[m] %in% c("CF-spatial","MC-spatial"))
        {
          points(lat~lon,CF.current[[banks[i]]],pch=21,bg='grey50',cex=0.8)
          legend("topleft",pch=c(21), pt.bg = c("grey50"), title="Tow type",
                 legend = paste('Detailed Sampling (n =',length(CF.current[[banks[i]]]$tow),")",sep=""), inset=0.01,bg=NA,box.col=NA)	
          legend("bottomleft",leg.lvls,fill=cols,
                 title=leg.title, title.adj = 0.2,border="black",pch=c(rep(NA,length(lvls))),
                 pt.bg = c(rep(NA,length(lvls))),inset=0.01,bg=NA,box.col=NA)
        } # end if(seed.n.spatial.maps[k] %in% c("Condition","Meat Count"))
          # Add the survey boxes if they exist.
          if(length(sb[,1]) > 0) {
            sb[,c("X", "Y")] <- apply(sb[,c("X", "Y")], 2, function(x) as.numeric(x))
            addPolys(sb,lty=2,lwd=2)
          }
          # If saving as a png turn off the plot device
          if(fig != "screen") dev.off()
        } # end for(m in 1:n.maps)  
      }# end if(length(spatial.maps) > 0) 
  
    }   # end if(length(spatial.maps > 0) || any(plots %in% "user.SH.bins"))
  } # end if(banks[i] %in% c("BBn" ,"BBs" ,"Ger", "Mid", "Sab", "GB" ,"GBb", "GBa"))
  ############  END THE INLA FIGURES ############  END THE INLA FIGURES  ############  END THE INLA FIGURES
  ############  END THE INLA FIGURES############  END THE INLA FIGURES############  END THE INLA FIGURES
  
  
  
  #####  Set up the survey figure #############  Set up the survey figure #############  Set up the survey figure ########
  #####  Set up the survey figure #############  Set up the survey figure #############  Set up the survey figure ########
  
  #Do we want to plot the survey?
  if(any(plots %in% "Survey"))
  {
    
    # For this figure we want full bank names, this is ugly hack but does the trick.
    if(banks[i] %in% c("SPB","Ban", "BanIce", "BBn" ,"BBs" ,"Ger", "Mid", "Sab", "GB" ,"GBb", "GBa"))
    {    
    full.names <- data.frame(abrv = c("SPB","Ban","BanIce","Mid","Sab","Ger","BBs","BBn","GBa","GBb","GB"),
                             full = c("St. Pierre Bank","Banquereau (Sea Scallop)","Banquereau (Icelandic)","Middle Bank","Sable Bank","German Bank","Browns Bank South",
                                      "Browns Bank North","Georges Bank a","Georges Bank b","Georges Bank Spring"))
    
    survey.title <- substitute(bold(paste("Survey (",bank," ",year,")",sep="")),
                               list(year=as.character(yr),bank=as.character(full.names$full[full.names$abrv == banks[i]])))
    }
    
    if(banks[i] %in% spat.name) survey.title <- substitute(bold(paste("Survey (",bank," ",year,")",sep="")),list(year=as.character(yr),bank= banks[i]))
    # Now we'll need to do a few things differently if there are no survey strata on a bank, this will work for Middle and German
    
    # Save the figures?
    if(fig == "png") png(paste(plot.dir,"/survey_strata.png",sep=""),units="in",width = 11, height = 11,res=420,bg = "transparent")
    if(fig == "pdf")  pdf(paste(plot.dir,"/survey_strata.pdf",sep=""),width = 11,height = 8.5)
    if(fig == "screen") windows(11,8.5)
    
    par(mfrow=c(1,1))
    if(add.title == F) survey.title <- ""
    
    # Make the plot, this one is for cases in which we have survey strata
    if(!is.null(strata.areas))
    {
      # I need to move the scale bar for Sable and GBb...
      if(banks[i] %in% c("GBa","BBn","BBs")) ScallopMap(banks[i],poly.lst=list(as.PolySet(detail.poly.surv),surv.info),direct = direct,cex.mn=2, boundries="offshore",
                 plot.bathy=F,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
                 nafo.bord = F,nafo.lab = F,title=survey.title,dec.deg = F,add.scale = add.scale)
      # I need to move the scale bar for Sable and GBb...
      if(banks[i] %in% c("Sab","GBb")) 
      {
        ScallopMap(banks[i],poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
                   plot.bathy = plt.bath,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
                   nafo.bord = F,nafo.lab = F,title=survey.title,dec.deg = F,add.scale = F)
      } # end if(banks[i] %in% c("Sab","GBb")) 
        # For the spatial sub-areas do this
      if(banks[i] %in% spat.name) 
        {
            x.bound <- range(bound.poly.surv$X)
            y.bound <- range(bound.poly.surv$Y)
            ScallopMap(xlim=x.bound,ylim=y.bound,poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
                       plot.bathy=plt.bath,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
                       nafo.bord = F,nafo.lab = F,title=survey.title,dec.deg = F,add.scale = add.scale)
          
          if(banks[i] %in% "GBa-North"){
            # Deal with the missing strata manually
            addPolys(detail.poly.surv[detail.poly.surv$PID==3,],col=surv.info$col[surv.info$PID==3],border=NA)
            addPolys(detail.poly.surv[detail.poly.surv$PID==4,],col=surv.info$col[surv.info$PID==4],border=NA)
          }
          
        } # end if(banks[i] %in% spat.name) 
    } # end if(nrow(strata.areas) > 0)
          
    # For the banks without any strata
    
    if(is.null(strata.areas))
    {
      if(banks[i] =="BanIce") ScallopMap("Ban",direct = direct,cex.mn=2,boundries="offshore",
                 plot.bathy = plt.bath,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
                 nafo.bord = F,nafo.lab = F,title=survey.title,dec.deg = F,add.scale = add.scale)
      if(!banks[i] =="BanIce") ScallopMap(banks[i],direct = direct,cex.mn=2,boundries="offshore",
                                         plot.bathy = plt.bath,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
                                         nafo.bord = F,nafo.lab = F,title=survey.title,dec.deg = F,add.scale = add.scale)
    } # end if(nrow(strata.areas) == 0)
    # Add the regular survey tows.
    #bg.col<-tapply(GBb.surv.info$col,GBb.surv.info$PName,unique)[c(2,3,1,4,5)]
    
    # Add the regular survey tows.
    if(!banks[i] %in% spat.name) points(lat~lon,surv.Live[[banks[i]]],subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=0.8)
    if(banks[i] %in% spat.name) points(lat~lon,surv.Live[[banks[i]]],subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1.2)
    
    if(banks[i] %in% c("GBa","BBn", "Sab" , "Mid", "Ban", "BBs" ,"BanIce","GBb"))  
    {
      leg.loc <- ifelse(banks[i] %in% c("GBa","BBn","BBs"),"topleft","bottomright")    
      points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
             & state =='live' & random %in% c(0,2,3,4,5),pch=24,bg="darkorange",cex=0.8)
      legend(leg.loc,legend = c(paste('exploratory (n =',
                                      length(unique(subset(surv.Live[[banks[i]]],year==yr & random %in% c(0,2,3,4,5))$tow)),
                                      ")",sep=""),
                                paste('regular (n =',
                                      length(unique(subset(surv.Live[[banks[i]]],year==yr & random==1)$tow)),
                                      ")",sep="")),title="Tow type",
             pt.bg = c("darkorange","black"),pch=c(24,20),bg = NA,inset=0.01,box.col=NA)
    }
    if(banks[i] %in% spat.name)  
    {
      if(banks[i] %in% spat.name) leg.loc <- "bottomright"
      if(banks[i] == "GBa-West") leg.loc <- "bottomleft"
      if(banks[i] == "GBa-Central") leg.loc <- "bottom"
      if(banks[i] == "GBa-East") leg.loc <- "topright"
      if(banks[i] == "GBa-North") leg.loc <- "topright"
      points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
             & state =='live' & random %in% c(0,2,3,4,5),pch=24,bg="darkorange",cex=1.2)
      legend(leg.loc,legend = c(paste('exploratory (n =',
                                      length(unique(subset(surv.Live[[banks[i]]],year==yr & random %in% c(0,2,3,4,5))$tow)),
                                      ")",sep=""),
                                paste('regular (n =',
                                      length(unique(subset(surv.Live[[banks[i]]],year==yr & random==1)$tow)),
                                      ")",sep="")),title="Tow type",
             pt.bg = c("darkorange","black"),pch=c(24,20),bg = NA,inset=0.01,box.col=NA, cex = 1.2, pt.cex = 1.2)
      sb2 <- seedboxes[which(grepl(seedboxes$ID, pattern="C4") | grepl(seedboxes$ID, pattern="C5") | grepl(seedboxes$ID, pattern="C6") |grepl(seedboxes$ID, pattern="C7")),]
      addPolys(as.PolySet(sb2, projection = "LL"),lty=2,lwd=2)
    } # end if(banks[i] %in% c("GBa","BBn"))
    
    # Add in the bank specific survey information, for some this is easier than for others.
    if(banks[i] == "GBa") 
    {
      
      legend("bottomleft",legend=c(surv.info$PName),
             fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
             pch=c(rep(NA,length(surv.info$PName))),title = "Strata",title.adj=0.1,
             pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n',cex=0.9)
      legend(-67.15,41.82,legend = round(surv.info$area_km2),
             fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
             pch=c(rep(NA,length(surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=1.1,
             pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
      legend(-66.12,41.5,legend = as.numeric(with(subset(surv.Live[[banks[i]]],year==yr & random==1),tapply(tow,Strata_ID,length))),
             fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
             pch=c(rep(NA,length(surv.info$PName))),title = "Number of tows",title.adj=0.1,
             pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
    } # End if(banks[i] == "GBa") 
    
    if(banks[i] == "Ger") 
    {
      points(lat~lon,surv.Live[[banks[i]]],subset=year==yr &
               random==3,pch=22,bg="yellow",cex=0.8)
      points(lat~lon,surv.Live[[banks[i]]],subset=year==yr &
               random %in% c(0,2,4,5),pch=24,bg="darkorange",cex=0.8)
      
      legend('topleft',legend=
               c(paste('regular (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                             random==1)$tow)),")", sep=""),
                 paste('repeated (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                              random==3)$tow)),")", sep=""),
                  paste('exploratory (n =',
                   length(unique(subset(surv.Live[[banks[i]]],year==yr & random %in% c(0,2,4,5))$tow)),")",sep="")),
             pch=c(20,22,24), pt.bg = c("black","yellow","darkorange"),bty='n',cex=1, inset = .02,bg=NA,box.col=NA)
      
    } # end if(banks[i] == "Ger") 
    
    if(banks[i] == "BBn" ) 
    {
      legend("bottomleft",legend=surv.info$PName,fill=surv.info$col,bty='n',cex=1, title = "Strata")
      legend("bottom",legend = round(surv.info$area_km2),
             fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
             pch=c(rep(NA,length(surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.1,
             pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
      
      legend("bottomright",legend = as.numeric(with(subset(surv.Live[[banks[i]]],
                                                           year== yr & random ==1),tapply(tow,Strata_ID,length))),
             fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
             pch=c(rep(NA,length(surv.info$PName))),title = "Number of tows",title.adj=0.1,
             pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
    } # end if(banks[i] == "BBn" || banks[i] == "Sab" || banks[i] == "GBb") 
   
    
    if(banks[i] == "GB") 
    {
      points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
             & state =='live' & random%in% c(2,4,5),pch=24,bg="darkorange",cex=0.8)
      points(lat~lon,surv.Live[[banks[i]]],subset=year==yr 
             & state =='live' & random==3,pch=22,bg="yellow",cex=0.8)
      
      legend("topleft",legend = c(paste('exploratory (n =',
                                         length(unique(subset(surv.Live[[banks[i]]],year==yr & random%in% c(2,4,5))$tow)),")",sep=""),
                                   paste('repeated (n =',length(unique(subset(surv.Live[[banks[i]]],year==yr & 
                                                                                random==3)$tow)),")", sep="")),title="Tow type",
             pt.bg = c("darkorange","yellow"),pch=c(24,22),bg = NA,inset=0.01,box.col=NA)
    } # end if(banks[i] == "Sab" || banks[i] == "Mid"|| banks[i] == "BBs" || banks[i] == "Ban"|| banks[i] == "GBb" || banks[i] == "GB") 
    
    if(banks[i] == "Sab") 
    {
      legend("topleft",legend=surv.info$PName,fill=surv.info$col,bty='n',cex=1, title = "Strata")
      legend("top",legend = round(surv.info$area_km2),
             fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
             pch=c(rep(NA,length(surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.1,
             pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
      
      legend("topright",legend = as.numeric(with(subset(surv.Live[[banks[i]]],
                                                        year== yr & random ==1),
                                                 tapply(tow,Strata_ID_new,length))),
             fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
             pch=c(rep(NA,length(surv.info$PName))),title = "Number of tows",title.adj=0.1,
             pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
    } 
    
    if(banks[i] == "GBb") 
    {
      legend("topleft",legend=surv.info$PName,fill=surv.info$col,bty='n',cex=1, title = "Strata")
      legend("top",legend = round(surv.info$area_km2),
             fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
             pch=c(rep(NA,length(surv.info$PName))),title = expression(paste("Area - ",km^2,"")),title.adj=0.1,
             pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
      
      legend("topright",legend = as.numeric(with(subset(surv.Live[[banks[i]]],
                                                        year== yr & random ==1),
                                                 tapply(tow,Strata_ID,length))),
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
      tow.leg <- c(aggregate(tow~Strata_ID,subset(surv.Live[[banks[i]]],year==yr &
                                                      random==1 & state=="live"),length)$tow[1],0,
                   aggregate(tow~Strata_ID,subset(surv.Live[[banks[i]]],year==yr &
                                                      random==1 & state=="live"),length)$tow[2:3])
      legend("bottomright",legend = tow.leg,
             fill=c(surv.info$col),border=c(rep('black',length(surv.info$PName))),
             pch=c(rep(NA,length(surv.info$PName))),title = "Number of tows",title.adj=0.1,
             pt.bg = c(rep(NA,length(surv.info$PName))),col='black',bty='n')
    } # End if(banks[i] == "BBs")
    # Add the survey boxes if they exist.
    if(length(sb[,1]) > 0) {
      sb[,c("X", "Y")] <- apply(sb[,c("X", "Y")], 2, function(x) as.numeric(x))
      addPolys(as.PolySet(sb, projection = "LL"),lty=2,lwd=2)
    }
    if(fig != "screen") dev.off()
    
  } # end if(length(plots[grep("Survey",plots)]>0))
  
  ####################################  END SURVEY PLOT ####################################  END SURVEY PLOT

####################################  END SPATIAL PLOTS ####################################  END SPATIAL PLOTS
####################################  END SPATIAL PLOTS ####################################  END SPATIAL PLOTS

      
      
      
####################################  MWSH and CF Time series plot #################################### 
####################################  MWSH and CF Time series plot ####################################       
####################################  MWSH and CF Time series plot #################################### 
  if(any(plots == "MW-SH"))
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
    if(fig == "png") png(paste(plot.dir,"/MWSH_and_CF_ts.png",sep=""),
                         units="in",width = 13,height = 8.5,res=420,bg = "transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"/MWSH_and_CF_ts.pdf",sep=""),width = 13,height = 8.5)
    
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
        # Added in case R is treating year as a factor... 
        if(is.factor(survey.obj.last[["GB"]][[1]]$year)) 
        {  
          survey.obj.last[["GB"]][[1]]$year <- as.numeric(levels(survey.obj.last[["GB"]][[1]]$year))[survey.obj.last[["GB"]][[1]]$year]
        }
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
        # Added in case R is treating year as a factor... 
        if(is.factor(survey.obj.last[["GBa"]][[1]]$year)) 
        {  
          survey.obj.last[["GBa"]][[1]]$year <- as.numeric(levels(survey.obj.last[["GBa"]][[1]]$year))[survey.obj.last[["GBa"]][[1]]$year]
        } # end if(is.factor(survey.obj.last[["GBa"]][[1]]$year)) 
        points(survey.obj.last[["GBa"]][[1]]$year-0.25,survey.obj.last[["GBa"]][[1]]$CF,col="blue", lty=1, pch=16,type="o")
        abline(h=mean(survey.obj.last[["GBa"]][[1]]$CF,na.rm=T),col="blue",lty=3)
      } # end  if(season=="spring")
      legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,22),bty='n',inset=0.02,col=c("blue","red"),pt.bg=c("blue","red"))	
      
    } # end if(banks[i] == "GB")
    #legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
    if(fig != "screen") dev.off()
    
  } # end if(any(plots=="MW-SH"))
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
    if(fig == "screen") windows(8.5,11)
   
    if(fig == "png")png(paste(plot.dir,"/abundance_ts.png",sep=""),units="in",
                         width = 8.5, height = 11,res=420,bg="transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"/abundance_ts.pdf",sep=""),width = 8.5, height = 11)

    par(mfrow=c(1,1))
    
    # if log.y=T you must also specify ymin.
    if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "Ban" && banks[i] != "BanIce" && banks[i] != "GB" && banks[i] != "Sab")

    {
      survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F,
                areas=surv.info$towable_area,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
    }# end if(banks[i] != "Ger" && banks[i] != "Mid")
    # For german bank
    if(banks[i] == "Ger")
    {

      survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,
                ymin=-5,dat2=merged.survey.obj,clr=c('blue','red',"blue"),pch=c(16,17),se=T,
                add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5, yl2=c(400, 300, 300))
      legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
    } # end if(banks[i] == "Ger")
    if(banks[i] == "Mid" || banks[i] == "GB" || banks[i] == "Ban"|| banks[i] == "BanIce")
    {
      if(!banks[i] == "BanIce")survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F, 
                ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl =survey.ts.N.title,cx.mn=3,axis.cx = 1.5)

      if(banks[i] == "BanIce")  survey.ts(survey.obj[[banks[i]]][[1]],Bank=banks[i],
                                         years=min(survey.obj[[banks[i]]][[1]]$year[!is.na(survey.obj[[banks[i]]][[1]]$n)],na.rm=T):yr,pdf=F,
                                         ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl =survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
    } # end if(banks[i] == "Mid")
    
    # For sable bank (due to restratification)
    if(banks[i] == "Sab")
    {
      if(yr!=2018)  survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F, 
                              areas=surv.info$towable_area,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                              add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
      # In 2018 we restratified Sable so need to get fancy with our figure for 2018
      if(yr==2018)
      {
        # this stuff is showing what went wrong in 2018 spring survey summary presentation. 
        # freya <- survey.obj$Sab$model.dat
        # surv.info.new <- survey.info[survey.info$startyear==2018 & survey.info$label=="Sab",]
        # surv.info.old <- survey.info[!(survey.info$startyear==2018) & survey.info$label=="Sab",]
        # surv.info <- NULL
        # survey.info<-NULL
        # direct <- "C:/Documents/Offshore scallop/Assessment/"
        # load(paste0(direct, "Data/Survey_data/2017/Survey_summary_output/Survey_all_results.RData"))
        # direct <- "C:/Documents/Offshore scallop/Assessment/"
        # surv.info
        # survey.info
        # survey.obj$Sab$model.dat
        # survey.obj$Sab$model.dat$year <- as.numeric(as.character(survey.obj$Sab$model.dat$year))
        # survey.ts(freya, min(freya$year,na.rm=T):2018,pdf=F,
        #           areas=surv.info.new$towable_area,
        #           areas2=surv.info.old$towable_area,
        #           dat2=survey.obj$Sab$model.dat,
        #           clr=c('blue',"red","blue"),
        #           se=T,
        #           pch=c(16, 17),
        #           add.title = F, cx.mn=3,axis.cx = 1.5)
        # 
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F, 
                  areas=surv.info$towable_area,
                  areas2=survey.info[!(survey.info$startyear==2018) & survey.info$label=="Sab",]$towable_area,
                  dat2=survey.obj.sab,
                  clr=c('blue',"red","blue"),
                  se=T,
                  pch=c(16, 17),
                  add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
        legend("topright", inset=c(0.05, -0.9), xpd=NA, c("After restratification","Prior to restratification"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
      }

    } # end if(banks[i] == "Sab")
    
    if(fig != "screen") dev.off()
    
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
    
    if(fig == "screen") windows(8.5,11)
    
    if(fig == "png") png(paste(plot.dir,"/biomass_ts.png",sep=""),
                         units="in",width = 8.5, height = 11,res=420,bg="transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"/biomass_ts.pdf",sep=""),width = 8.5, height = 11)
    
    
    if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "Ban" && banks[i] != "BanIce" && banks[i] != "GB" && banks[i] != "Sab")
      
    {
      survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,type='B', 
                areas=surv.info$towable_area,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
    } # end if(banks[i] != "Ger")
    if(banks[i] == "Ger")
    {
      survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F,type='B', 
                dat2=merged.survey.obj,clr=c('blue','red',"blue"),se=T,pch=c(16,17),
                add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx=1.5)
      legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
    } # end if(banks[i] == "Ger")
    
    # For sable bank (due to restratification)
    if(banks[i] == "Sab")
    {
      
      if(yr!=2018)  survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,type='B', 
                              areas=surv.info$towable_area,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                              add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
      # In 2018 we restratified Sable so need to get fancy with our figure for 2018
      if(yr==2018)
      {
        survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F, type="B",
                  areas=surv.info$towable_area,
                  areas2=survey.info[!(survey.info$startyear==2018) & survey.info$label=="Sab",]$towable_area,
                  dat2=survey.obj.sab,
                  clr=c('blue',"red","blue"),se=T,pch=c(16, 17),
                  add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
        legend("topright", inset=c(0.05, -0.9), xpd=NA, c("After restratification","Prior to restratification"),
               pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
      } # end if(yr==2018)
      
    } # end if(banks[i] == "Sab")
    
    if(banks[i] == "Mid"|| banks[i] == "GB"|| banks[i] == "Ban" || banks[i] == "BanIce")
    {
      if(!banks[i] == "BanIce") survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,  type='B',
                                          ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
      
      if(banks[i] == "BanIce")  survey.ts(survey.obj[[banks[i]]][[1]],Bank=banks[i],
                                          years=min(survey.obj[[banks[i]]][[1]]$year[!is.na(survey.obj[[banks[i]]][[1]]$n)],na.rm=T):yr,pdf=F, type='B',
                                          ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl =survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
    } # end if(banks[i] == "Mid")
  
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
    if(fig == "screen") windows(11,8.5)
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
                add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5,user.bins = user.bins)
      legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
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
      legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
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
    SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,")",sep="")),
                             list(bank=banks[i]))  
    if(banks[i] == "GB")  SHF.title <-  substitute(bold(paste("Shell height frequency (",bank,"-Spr)",sep="")),
                                                   list(bank=banks[i])) 
    if(add.title == F) SHF.title <- ""
    if(fig == "screen") windows(8.5,11)
    if(fig == "png") {
      png(paste(plot.dir,"/SHF.png",sep=""),units="in",width = 8.5, 
                          height = 11,res=420,bg="transparent")
    }
    if(fig == "pdf") pdf(paste(plot.dir,"/SHF.pdf",sep=""),width = 8.5, height = 11)
    # Grab the last 7 years of data
    
    if(banks[i] != "Ger")
      {
      if(!grepl(x=banks[i], pattern="Ban")) {
        shf.years <- survey.obj[[banks[i]]][[1]]$year[(length(survey.obj[[banks[i]]][[1]]$year)-6):
                                                        length(survey.obj[[banks[i]]][[1]]$year)]
        s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
        shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
                recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T)	
        if(fig != "screen") dev.off()
      }
      
      if(grepl(x=banks[i], pattern="Ban")) {
        dev.off()
        if(fig == "screen") windows(11,8.5)
        if(fig == "png") {
          png(paste(plot.dir,"/SHF.png",sep=""),units="in",width = 11, 
              height = 8.5,res=420,bg="transparent")
        }
        if(fig == "pdf") pdf(paste(plot.dir,"/SHF.pdf",sep=""),width = 11, height = 8.5)
        shf.years <- survey.obj[[banks[i]]][[1]]$year[!is.na(survey.obj[[banks[i]]][[1]]$n) & (yr - survey.obj[[banks[i]]][[1]]$year) <10]
        s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
        shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
                recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T, rows=2)	# rows=2 allows us to 
        if(fig != "screen") dev.off()
      }
    } # end  if(banks[i] != "Ger")
    
    if(banks[i]=="Ger")
      {
        shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
        s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
        shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
                recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T)	
        if(fig != "screen") dev.off()
        # We also want to grab the matched tows figure
        s.size <- matched.survey.obj[[1]]$n
        
        if(fig == "screen") windows(8.5,11)
        if(fig == "png") png(paste(plot.dir,"/shell_height_freqency-repeated.png",sep=""),
                               units="in",width = 11, height = 8.5,res=420,bg="transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"/shell_height_freqency-repeated.pdf",sep=""),
                             width = 11, height = 8.5)
        if(add.title == F) SHFm.title <- ""
        if(add.title == T) SHFm.title <- "SHF - Repeated Tows (Ger)"
        shf.plt(matched.survey.obj,from='surv',yr=(yr-1):yr,col1='grey80',col2=1,rel=F,rows=2,
                recline=c(RS,CS),add.title = T,titl = SHFm.title,cex.mn=2, sample.size = T)	   
        if(fig != "screen") dev.off()
      } # end if(banks[i]=="Ger")
    } # end if(any(plots=="SHF"))    
                 
##### END Shell height frequency figure   ##### END Shell height frequency figure ##### END Shell height frequency figure
##### END Shell height frequency figure   ##### END Shell height frequency figure ##### END Shell height frequency figure
      
  
  
  
  ##### Shell height frequency Large Sizes      ##### Shell height frequency Large Sizes ##### Shell height frequency Large Sizes 
  ##### Shell height frequency Large Sizes      ##### Shell height frequency Large Sizes ##### Shell height frequency Large Sizes   
  
  if(any(plots=="SHF-large"))
  {
    SHF.title <-  substitute(bold(paste("Shell height frequency (","" >b," mm - ",bank,")",sep="")),
                             list(bank=banks[i],b=65))  
    if(banks[i] == "GB")  SHF.title <-  substitute(bold(paste("Shell height frequency (","" >b ," mm - ",bank,"-Spr)",sep="")),
                                                   list(bank=banks[i],b=65)) 
    if(add.title == F) SHF.title <- ""
    if(fig == "screen") windows(8.5,11)
    if(fig == "png") png(paste(plot.dir,"/SHF-large.png",sep=""),units="in",width = 8.5, height = 11,res=420,bg="transparent")
                          
    if(fig == "pdf") pdf(paste(plot.dir,"/SHF-large.pdf",sep=""),width = 8.5, height = 11)
    # Grab the last 7 years of data
    
    if(banks[i] != "Ger")
    {
      shf.years <- survey.obj[[banks[i]]][[1]]$year[(length(survey.obj[[banks[i]]][[1]]$year)-6):
                                                      length(survey.obj[[banks[i]]][[1]]$year)]
      s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
      shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F, select=70,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T)	
      if(fig != "screen") dev.off()
    } # end  if(banks[i] != "Ger")
    
    if(banks[i]=="Ger")
    {
      shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
      s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
      shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,select=70,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T)	
      if(fig != "screen") dev.off()
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

    clap.abund.ts.title <- substitute(bold(paste("Clapper abundance time series (",bank,")",sep="")),
                                      list(bank=banks[i]))
    if(banks[i] == "GB")  clap.abund.ts.title <- substitute(bold(paste("Clapper abundance time series (",bank,"-Spr)",sep="")),
                                                            list(bank=banks[i]))
    if(add.title == F) clap.abund.ts.title <- ""
    if(fig == "screen") windows(8.5,11)
    if(fig == "png") png(paste(plot.dir,"Clapper_abund_ts.png",sep=""),
                         units="in",width = 8.5, height = 11,res=420,bg = "transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"Clapper_abund_ts.pdf",sep=""),width = 8.5, height = 11)

    if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "Ban"  && banks[i] != "BanIce" && banks[i] != "GB" && banks[i] != "Sab")
    {
      yrs <- min(clap.survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(clap.survey.obj[[banks[i]]][[1]]$year,na.rm=T)
      survey.ts(clap.survey.obj[[banks[i]]][[1]], Bank=bank[i],pdf=F, years=yrs,axis.cx = 1.5,
                titl = clap.abund.ts.title,add.title=T, cx.mn=3,areas=strata.areas$towable_area,
                ht=7,wd=10,clr=c('blue',"blue","darkgrey"),se=T,pch=16, plots=c("pre",'rec','com'))
    } # if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "GB" && banks[i] != "Sab")
    
    if(banks[i] == "Sab")
    {
      yrs <- min(clap.survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(clap.survey.obj[[banks[i]]][[1]]$year,na.rm=T)
      survey.ts(clap.survey.obj[[banks[i]]][[1]], min(clap.survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,
                Bank=bank[i],pdf=F, years=yrs,axis.cx = 1.5,
                titl = clap.abund.ts.title,add.title=T, cx.mn=3,areas=surv.info$towable_area,
                ht=7,wd=10,clr=c('blue',"blue","darkgrey"),se=T,pch=16, plots=c("pre",'rec','com'))
    } # end if(banks[i] = "Sab")
    
    if(banks[i] == "Ger" || banks[i] == "Mid" || banks[i] == "Ban" || banks[i] == "BanIce" || banks[i] == "GB")
    {
      yrs <- min(surv.Clap.Rand[[banks[i]]]$year,na.rm=T):max(surv.Clap.Rand[[banks[i]]]$year,na.rm=T)
      survey.ts(clap.survey.obj[[banks[i]]][[1]],Bank=bank[i],pdf=F,axis.cx = 1.5, 
                titl = clap.abund.ts.title,add.title=T, cx.mn=3, years=yrs,
                ht=7,wd=10,clr=c('blue',"blue","darkgrey"),se=T,pch=16, plots=c("pre",'rec','com'))
    } # end if(banks[i] == "Ger" || banks[i] == "Mid" || banks[i] == "GB")
    if(fig != "screen") dev.off()                 
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
    if(fig == "png") png(paste(plot.dir,"Clapper_per_ts.png",sep=""),units="in",width = 8.5, 
                         height = 11,res=420,bg = "transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"Clapper_per_ts.pdf",sep=""),width = 8.5, height = 11)
    yrs <- min(surv.Clap.Rand[[banks[i]]]$year,na.rm=T):max(surv.Clap.Rand[[banks[i]]]$year,na.rm=T)
    Clap3.plt(surv.Clap.Rand[[banks[i]]],years=yrs,add.title = T,cex.mn = 3, mean.line=T,
              titl = clap.per.ts.title,
              CS=unique(survey.obj[[banks[i]]][[1]]$CS),RS=unique(survey.obj[[banks[i]]][[1]]$RS),
              axis.cx = 1.5)
    if(fig != "screen") dev.off()                 
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
      if(fig == "png") png(paste(plot.dir,"SH_MW_CF_ts.png",sep=""),units="in",width = 8.5,height = 11,res=420,bg = "transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"SH_MW_CF_ts.pdf",sep=""),width = 8.5,height = 11)
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
      if(fig != "screen") dev.off()   
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
    if(fig == "png") png(paste(plot.dir,"breakdown-",(yr),".png",sep=""),units="in",
                         width = 11,height = 8.5,res=420,bg = "transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"breakdown-",(yr),".pdf",sep=""),width = 11,height = 8.5)
    if(banks[i] != "GB") mc <- subset(fish.reg, year == yr & Bank %in% banks[i])$MC_reg
    if(banks[i] %in% spat.name) mc <- subset(fish.reg, year == yr & Bank %in% unique(spat.names$bank[spat.names$label == banks[i]]))$MC_reg
    if(banks[i] == "GB") mc <- fish.reg$MC_reg[fish.reg$Bank == "GBa"]
    if(banks[i] != "Ger") 
    {
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
    } # end if(banks[i] != "Ger") 

    # Using the lined surevye object for German bank...
    if(banks[i] == "Ger") 
    {
      # This will make the breakdown figure for the previous year in which there was a survey (typically last year but not always...)
      # This is based on the current year being requested (which could differ from the last year in the data if you are saying using the 2018 survey results
      # but wanted to look at the 2015 data for example).
      current.year <- which(na.omit(lined.survey.obj$model.dat)$year == yr)
      last.surv.year <- na.omit(lined.survey.obj$model.dat)$year[current.year-1]
      # Get the data...
      bm<-lined.survey.obj$shf.dat$w.yst[which(lined.survey.obj[[1]]$year==yr),which(seq(5,200,5) >= 5)]/1000
      bm.last<-lined.survey.obj$shf.dat$w.yst[which(lined.survey.obj[[1]]$year==last.surv.year),which(seq(5,200,5) >= 5)]/1000
      ymax <- max(c(max(bm,na.rm=T)*1.1),max(bm.last,na.rm=T)*1.1)
      breakdown(lined.survey.obj,yr=yr,mc=mc,cx.axs=1,y1max = ymax,add.title = F)
    }# end if(banks[i] == "Ger") 
    
    if(add.title ==T) title(paste("Biomass & Meat Count by Height (",banks[i],"-",yr,")",sep=""), cex.main=2,adj=0.35)
    if(fig != "screen") dev.off()   
    
    # I also want to remake the previsou year's breakdown plot, this will go in the current years folder but will
    # be the same y-scale (there is no guarantee that the plot made last year will be, likely it won't).  It's a bit
    # clunky but basically this is the same plot as last year but re-scaled for comparative purposes...

      if(fig == "screen") windows(11,8.5)
      if(fig == "png") png(paste(plot.dir,"breakdown-",last.surv.year,".png",sep=""),units="in",
                           width = 11,height = 8.5,res=420,bg = "transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"breakdown-",last.surv.year,".pdf",sep=""),width = 11,height = 8.5)

      if(banks[i] != "Ger")
      {
        # To get the ymax the same between succesive years I want to do this...
        breakdown(survey.obj[[banks[i]]],yr=last.surv.year,mc=mc,cx.axs=1,y1max = ymax, y2max=y2max, add.title = F)
        if(add.title ==T) title(paste("Biomass & Meat Count by Height (",banks[i],"-",last.surv.year,")",sep=""), cex.main=2,adj=0.35)
        
      } # end if(banks[i] != "Ger") 
      
      if(banks[i] == "Ger") 
      {
        breakdown(lined.survey.obj,yr=last.surv.year,mc=mc,cx.axs=1,y1max = ymax,add.title = F)
        if(add.title ==T) title(paste("Biomass & Meat Count by Height (",banks[i],"-",last.surv.year,")",sep=""), cex.main=2,adj=0.35)
      }# end if(banks[i] == "Ger") 
      
      if(fig != "screen") dev.off()   
    
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
    sb <- subset(seedboxes,Bank == banks[i] & Closed < paste(yr,"-11-01",sep="") & (Open >= paste(yr,"-01-01",sep="") | is.na(Open)))
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
                                                   list(fn = fig.box.name[j],year=as.character(yr-1),bank=banks[i]))
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
        bm.last<-boxy$shf.dat$w.yst[which(boxy[[1]]$year==(yr-1)),which(seq(5,200,5) >= 5)]/1000
        # need to do the same thing for the meat count axis, but this requires us to calculate the meatcounts for each bar first, for both years
        ## this year:
        bmmc<-boxy$shf.dat$w.yst[which(boxy$model.dat$year==yr),which(seq(5,200,5) >= 5)]/1000
        nummc<-boxy$shf.dat$n.yst[which(boxy$model.dat$year==yr),which(seq(5,200,5) >= 5)]
        countmc=nummc/bmmc*0.5
        vec<-seq(0,195,5)
        y2max<-max(countmc[(min(c(RS-15,160),na.rm=T)/5-1):length(vec)],na.rm=T)*1.1
        ## last year
        bmmc.last<-boxy$shf.dat$w.yst[which(boxy$model.dat$year==yr-1),which(seq(5,200,5) >= 5)]/1000
        nummc.last<-boxy$shf.dat$n.yst[which(boxy$model.dat$year==yr-1),which(seq(5,200,5) >= 5)]
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
        if(fig == "png") png(paste(plot.dir,box.names[j],"-breakdown",(yr-1),".png",sep=""),units="in",
                             width = 11,height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-breakdown-",(yr-1),".pdf",sep=""),
                             width = 11,height = 8.5)
        # To get the ymax the same between succesive years I want to do this...
          if(!ymax %in% "-Inf") {
            breakdown(boxy,yr=(yr-1),mc=mc,cx.axs=1,y1max = ymax, y2max=y2max, add.title = F)
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
        
        # A zoomed in view of the box in question with survey strata...
        # If we are on GB we'll grab the GBa details, if a box was ever on GBb this would need tweaked, but these are pretty minor
        # pics and there's never been a GBb box so this is fine for now....
        if(banks[i] == "GB") 
        {
          surv.info <- subset(survey.info,label== "GBa")
          detail.poly.surv <- as.PolySet(subset(survey.detail.polys[!(survey.detail.polys$startyear==1900 & survey.detail.polys$label=="Sab"),],
                                                                    label=="GBa", select=c("PID", "SID", "POS", "X", "Y", "label", "Strata_ID")),
                                                                    projection = "LL")
        } # end if(banks[i] == "GB") 
          
        if(fig == "screen") windows(11,8.5)
        if(fig == "png") png(paste(plot.dir,box.names[j],"-spatial-",(yr),".png",sep=""),units="in",
                             width = 11,height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-spatial-",(yr),".pdf",sep=""),
                             width = 11,height = 8.5)
        
        diff.x <- diff(range(this.box$X))
        diff.y <- diff(range(this.box$Y))
        x.range <- c(min(this.box$X) - 2*diff.x, max(this.box$X) + 2*diff.x)
        y.range <- c(min(this.box$Y) - 2*diff.y, max(this.box$Y) + 2*diff.y)
        ScallopMap(xlim = x.range,ylim = y.range,
                            poly.lst=list(detail.poly.surv,surv.info),direct = direct,cex.mn=2, boundries="offshore",
                   plot.bathy = plt.bath,plot.boundries = T,bathy.source="quick", xlab="",ylab="",
                   nafo.bord = F,nafo.lab = F,dec.deg = F,add.scale = F)
        if(add.scale == T) maps::map.scale(min(smap.xlim)+0.1*(max(smap.xlim)-min(smap.xlim)),
                                           min(smap.ylim)+0.1*(max(smap.ylim)-min(smap.ylim)),relwidth = 0.15,cex=1,ratio=F)
        addPolys(this.box,lwd=6,border="darkgrey")
        points(slat~slon,surv.seed,subset= year==yr & state=='live'& random==1,pch=20,bg='black',cex=1.3)
        # Add the exploratory survey tows
        points(slat~slon,surv.seed,subset=year==yr & state =='live' & random %in% c(0,2,4,5),pch=24,bg="darkorange",cex=1.3)
        if(add.title == T)  title(paste("Seedbox ",fig.box.name[j]," (",banks[i],"-",yr,")",sep=""),cex.main=2)
        if(fig != "screen") dev.off()
        
        # Next the spatial abundance figures.
        # First I will re-run the INLA if any of the seedbox tows from the most recent year are found to be outside the survey area, 
        # this is mostly here to deal with the Starbox on Sable, but could impact other seedbox if for some reason they are
        # located outside of our regular survey strata or are on bank for which we don't have strata.  For the most part this
        # will do nothing.  It's a add on of a couple hundred lines of code, really could be cleaned up to work with the initial
        # INLA calls above...
        # If any of the Strata_ID's are outside of the survey stratification scheme we will run the spatial plots as
        # local fields using the box data only.
        
        if(any(is.na(surv.seed$Strata_ID))==T)
        {
          mod.res <- NULL
          # Now set up the INLA for this seedbox...
          bound.poly.surv.sp.sb <- PolySet2SpatialPolygons(as.PolySet(this.box,projection="LL"))
          loc.sb <- cbind(surv.seed$lon[surv.seed$year == yr],
                       surv.seed$lat[surv.seed$year == yr])
          # Build the mesh, for our purposes I'm hopeful this should do the trick.
          mesh.sb <- inla.mesh.2d(loc.sb, max.edge=c(0.1), cutoff=0.001,
                               boundary = inla.sp2segment(bound.poly.surv.sp.sb))
          # plot(mesh)
          #plot(mesh.cf)
          A.sb <- inla.spde.make.A(mesh.sb, loc.sb)
          ## All of our spatial plots are counts, so for our simple purposes a poisson is o.k.
          family1 = "poisson"
          # We can just make the one spde object for all of these as well.
          spde.sb <- inla.spde2.pcmatern(mesh.sb,    
                                      prior.sigma=c(1,0.5),
                                      prior.range=c(1,0.5))
          # As soon as you make a spatial model make your own intercept.  Here is
          a0 <- 1 # intercept
          # Mostly just using stock priors, again fine for our purposes for the moment.
          pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
          # basically the random deviations for each piece of the mesh.
          formula3.sb <- y ~ 0 + a0 + f(s, model=spde.sb)
          
          for(k in 1:length(seed.n.spatial.maps))
          {
            # In the next bunch of if statements we run the INLA model and we get the figure titles sorted out.
            if(seed.n.spatial.maps[k] == "PR-spatial")    
            {
              # This is the stack for the INLA model
              stk.sb <- inla.stack(tag="est",data=list(y = surv.seed$pre[surv.seed$year == yr], link=1L),
                                effects=list(a0 = rep(1, nrow(surv.seed[surv.seed$year == yr,])), s = 1:spde.sb$n.spde),
                                A = list(1, A.sb))
              # This is the INLA model itself
              mod <- inla(formula3.sb, family=family1, data = inla.stack.data(stk.sb),
                          control.predictor=list(A=inla.stack.A(stk.sb),link=link, compute=TRUE))
              # These are the results for each spatial mesh
              mod.res.tmp<- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
              
              # This projects our mesh
              proj <- inla.mesh.projector(mesh.sb,xlim = range(this.box$X),ylim =  range(this.box$Y), dims = c(500, 500))
              # this makes a big matrix with the values in the proper locations
              mod.res[[seed.n.spatial.maps[k]]] <- inla.mesh.project(proj, mod.res.tmp)
            } # end if(seed.n.spatial.maps[k] == "PR-spatial")   
            
            if(seed.n.spatial.maps[k] == "Rec-spatial")        
            {
              # This is the stack for the INLA model
              stk.sb <- inla.stack(tag="est",data=list(y = surv.seed$rec[surv.seed$year == yr], link=1L),
                                effects=list(a0 = rep(1, nrow(surv.seed[surv.seed$year == yr,])), s = 1:spde.sb$n.spde),
                                A = list(1, A.sb))
              # This is the INLA model itself
              mod <- inla(formula3.sb, family=family1, data = inla.stack.data(stk.sb),#control.family= control.family1,
                          control.predictor=list(A=inla.stack.A(stk.sb),link=link, compute=TRUE))
              # These are the results for each spatial mesh
              mod.res.tmp<- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
              
              # This projects our mesh
              proj <- inla.mesh.projector(mesh.sb,xlim = range(this.box$X),ylim =  range(this.box$Y), dims = c(500, 500))
              # this makes a big matrix with the values in the proper locations
              mod.res[[seed.n.spatial.maps[k]]] <- inla.mesh.project(proj, mod.res.tmp)
              
            } # end if(seed.n.spatial.maps[k] == "Rec-spatial") 
            
            if(seed.n.spatial.maps[k] == "FR-spatial") 
            {
              # This is the stack for the INLA model
              stk <- inla.stack(tag="est",data=list(y = surv.seed$com[surv.seed$year == yr], link=1L),
                                effects=list(a0 = rep(1, nrow(surv.seed[surv.seed$year == yr,])), s = 1:spde.sb$n.spde),
                                A = list(1, A.sb))
              # This is the INLA model itself
              mod <- inla(formula3.sb, family=family1, data = inla.stack.data(stk.sb),#control.family= control.family1,
                          control.predictor=list(A=inla.stack.A(stk.sb),link=link, compute=TRUE))
              # These are the results for each spatial mesh
              mod.res.tmp<- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
              
              # This projects our mesh
              proj <- inla.mesh.projector(mesh.sb,xlim = range(this.box$X),ylim =  range(this.box$Y), dims = c(500, 500))
              # this makes a big matrix with the values in the proper locations
              mod.res[[seed.n.spatial.maps[k]]] <- inla.mesh.project(proj, mod.res.tmp)
            } # end if(seed.n.spatial.maps[k] == "FR-spatial")
            # End the loop for getting all the data needed for a bank for the spatial maps.
          } # end for(k in 1:length(seed.n.spatial.maps))
      }  # end if(any(is.na(surv.seed$Strata_ID))==T)
        
        
        
        if(fig == "png") png(paste(plot.dir,box.names[j],"-spatial_abundance.png",sep=""),units="in",
                             width = 11, height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-spatial_abundance.pdf",sep=""),
                             width = 11, height = 8.5)
        if(fig == "screen") windows(11,8.5)
        par(mfrow=c(2,2),omi=c(0.1,0.2,0.5,0.5),xpd=F)
        
        
        # Get the correct levels for the legend and the image plots for the spatial abundance
        if(dim(surv.seed[surv.seed$year==yr,]) >0) {
          base.lvls=c(0,5,10,50,100,500,1000,2000,5000,10000,20000,50000,1e6)
          cols <- c(rev(plasma(length(base.lvls[base.lvls < 2000]),alpha=0.7,begin=0.6,end=1)),
                    rev(plasma(length(base.lvls[base.lvls > 1000])-1,alpha=0.8,begin=0.1,end=0.5)))
          max.lvl <- which(base.lvls >= max(c(max(surv.seed$pre[surv.seed$year == yr],na.rm=T),max(surv.seed$rec[surv.seed$year == yr],na.rm=T),
                                              max(surv.seed$com[surv.seed$year == yr],na.rm=T))))[1]
          lvls <- base.lvls[1:max.lvl]
          cols <- cols[1:(max.lvl-1)]
          # Get the levels for the legend.
          ifelse(max(lvls) == max(base.lvls),  leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                             paste(lvls[length(lvls)-1],'+',sep='')),
                 leg.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                               paste(lvls[length(lvls)-1],'-',lvls[max.lvl],sep='')))
          
          # Loop through each size category
          for(b in 1:3) 
          {
            # Get the title for each panel...
            if(b ==1) fig.title <- pre.title.seed
            if(b ==2) fig.title <- rec.title.seed 
            if(b ==3) fig.title <- fr.title.seed
            # Make the map
            ScallopMap(ylim=c(min(this.box$Y),max(this.box$Y)),xlim=c(min(this.box$X),max(this.box$X)),bathy.source="usgs",
                       isobath = c(seq(40,140,by=20)),plot.bathy = T,plot.boundries = T,direct=direct,
                       title=fig.title,dec.deg = F,ylab="",xlab="",cex.mn=1.3,add.scale = F)
            if(add.scale == T) maps::map.scale(min(smap.xlim)+0.1*(max(smap.xlim)-min(smap.xlim)),
                                               min(smap.ylim)+0.1*(max(smap.ylim)-min(smap.ylim)),relwidth = 0.15,cex=1,ratio=F)
            
            # Add the contours
            #if(dim(surv.seed==0){
            if(b == 1) image(list(x = proj$x, y=proj$y, z = mod.res[["PR-spatial"]]), axes=F,add=T,breaks = lvls,col=cols)
            if(b == 2) image(list(x = proj$x, y=proj$y, z = mod.res[["Rec-spatial"]]), axes=F,add=T,breaks = lvls,col=cols)
            if(b == 3) image(list(x = proj$x, y=proj$y, z = mod.res[["FR-spatial"]]), axes=F,add=T,breaks = lvls,col=cols)
            #}
            #Add the rest of the crap to the plot.
            addPolys(this.box,lty=2,lwd=2)
            # Add the regular survey tows.
            points(slat~slon,surv.seed,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1.3)
            # Add the exploratory survey tows
            points(slat~slon,surv.seed,subset=year==yr&state =='live' & random %in% c(0,2,4,5),pch=24,bg="darkorange",cex=1.3)
            if(banks[i] == "GB") points(slat~slon,surv.seed,subset=year==yr&state =='live' & random==3,pch=22,bg="yellow",cex=1.3) # add the repeat tows.
          } # end for(b in 1:3)
          
          # Now add the legend.
          par(xpd=T)
          plot(1:10,type='n',axes=F,xlab='',ylab='',main="",cex.main=1)
          legend("left",leg.lvls,fill=cols,border="black",pch=c(rep(NA,length(lvls))),title = N.tow.lab,title.adj = 0.2,
                 pt.bg = c(rep(NA,length(lvls))),bg=NA,bty="n")
          
          if(banks[i] != "GB")
          {
            legend("topright",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",inset=0.01,
                   legend = c(paste('regular (n =',
                                    length(subset(surv.seed,year==yr & state=='live'& random==1)$ID),")",sep=""),
                              paste('exploratory (n =',
                                    length(subset(surv.seed,year==yr & state=='live'& random%in% c(0,2,4,5))$ID),")",sep="")),
                   bg=NA,box.col=NA,bty="n")
          } # end if(banks[i] != "GB")
          if(banks[i] == "GB")
          {
            legend("topright",legend = c(paste('exploratory (n =',
                                               length(unique(subset(surv.seed,year==yr & random%in% c(2,4,5))$tow)),")",sep=""),
                                         paste('repeated (n =',length(unique(subset(surv.seed,year==yr & 
                                                                                      random==3)$tow)),")", sep="")),title="Tow type",
                   pt.bg = c("darkorange","yellow"),pch=c(24,22),bg = NA,inset=0.01,box.col=NA)
          } # # end if(banks[i] == "GB")
          
          if(add.title == T) title(paste("Seedbox ",fig.box.name[j]," (",banks[i],"-",yr,")",sep=""),cex.main=2,outer=T,line=-0.5)
          if(fig != "screen") dev.off()
        } # end if(dim(surv.seed[surv.seed$year==yr,])>0)
        ########### Now add the CF, MC, and Clapper figures for inside any of the seedboxes that exist, all of these must exist for this to plot.
        if(!is.null(mod.res[["Clap-spatial"]]) & !is.null(mod.res[["CF-spatial"]]) & !is.null(mod.res[["MC-spatial"]]))
        {
          # Next the CF/MC/Clapper plots as necessary.
          if(fig == "png") png(paste(plot.dir,box.names[j],"-spatial_CF-MC-Clap.png",sep=""),units="in",
                               width = 11, height = 8.5,res=420,bg = "transparent")
          if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-spatial_CF-MC-Clap.pdf",sep=""),
                               width = 11, height = 8.5)
          if(fig == "screen") windows(11,8.5)
          
          par(mfrow=c(2,2),omi=c(0.1,0.2,0.5,0.5),xpd=F)
          
          # Loop through each size category
          for(b in 1:3) 
          {
            # Get the title for each panel...
            if(b ==1) 
            {
              base.lvls <- c(0,5,8,10,12,14,16,18,50)
              cols <- rev(inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
              # Get the levels correct            
              min.lvl <- max(which(base.lvls <= min(mod.res[["CF-spatial"]],na.rm=T)))
              max.lvl <- min(which(base.lvls >= max(mod.res[["CF-spatial"]],na.rm=T)))
              lvls <- base.lvls[min.lvl:max.lvl]
              cf.cols <- cols[min.lvl:(max.lvl-1)]
              ifelse(max(lvls) == max(base.lvls),  cf.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                                paste(lvls[length(lvls)-1],'+',sep='')),
                     cf.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                  paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
              fig.title <- cf.title.seed
            } # end if(b==1)
            
            if(b ==2) 
            {
              # The color ramps for MC
              base.lvls <- c(seq(0,50,5),1000)
              cols <- viridis(length(base.lvls)-1,alpha=0.7,begin=0,end=1)
              # Get the levels correct            
              min.lvl <- max(which(base.lvls <= min(mod.res[["MC-spatial"]],na.rm=T)))
              max.lvl <- min(which(base.lvls >= max(mod.res[["MC-spatial"]],na.rm=T)))
              lvls <- base.lvls[min.lvl:max.lvl]
              mc.cols <- cols[min.lvl:(max.lvl-1)]
              ifelse(max(lvls) == max(base.lvls),  mc.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                                paste(lvls[length(lvls)-1],'+',sep='')),
                     mc.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                  paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
              fig.title <- mc.title.seed 
            } # end if(b==2)
            
            if(b ==3)
            {
              # The color ramp for Clapper proportion
              base.lvls=c(0,5,10,15,20,50,100)
              cols <- rev(plasma(length(base.lvls)-1,alpha=0.7))
              # Get the levels correct            
              min.lvl <- max(which(base.lvls <= min(mod.res[["Clap-spatial"]],na.rm=T)))
              max.lvl <- min(which(base.lvls >= max(mod.res[["Clap-spatial"]],na.rm=T)))
              lvls <- base.lvls[1:max.lvl]
              clap.cols <- cols[min.lvl:(max.lvl-1)]
              ifelse(max(lvls) == max(base.lvls),  clap.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                                                  paste(lvls[length(lvls)-1],'+',sep='')),
                     clap.lvls <- c(paste(lvls[-length(lvls)],'-',lvls[-1],sep='')[-(length(lvls)-1):-length(lvls)],
                                    paste(lvls[length(lvls)-1],'-',lvls[length(lvls)],sep='')))
              fig.title <- clap.title.seed
            }# end if(b==3)
            
            # Make the map
            ScallopMap(ylim=c(min(this.box$Y),max(this.box$Y)),xlim=c(min(this.box$X),max(this.box$X)),bathy.source="usgs",
                       isobath = c(seq(40,140,by=20)),plot.bathy = T,plot.boundries = T,direct=direct,
                       title=fig.title,dec.deg = F,ylab="",xlab="",cex.mn=1.3,add.scale = add.scale)

            # Add the contours
            if(b == 1) image(list(x = proj$x, y=proj$y, z = mod.res[["CF-spatial"]]), axes=F,add=T,breaks = lvls,col=cf.cols)
            if(b == 2) image(list(x = proj$x, y=proj$y, z = mod.res[["MC-spatial"]]), axes=F,add=T,breaks = lvls,col=mc.cols)
            if(b == 3) image(list(x = proj$x, y=proj$y, z = mod.res[["Clap-spatial"]]), axes=F,add=T,breaks = lvls,col=clap.cols)
            
            #Add the rest of the crap to the plot.
            addPolys(this.box,lty=2,lwd=2)
            # Add the regular survey tows.
            points(slat~slon,surv.seed,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1.3)
            # Add the exploratory survey tows
            points(slat~slon,surv.seed,subset=year==yr&state =='live' & random%in% c(0,2,4,5),pch=24,bg="darkorange",cex=1.3)
            if(banks[i] == "GB") points(slat~slon,surv.seed,subset=year==yr&state =='live' & random==3,pch=22,bg="yellow",cex=1.3) # add the repeat tows.
          } # end for(b in 1:3)
          
          # Now add the legend.
          par(xpd=T)
          plot(1:10,type='n',axes=F,xlab='',ylab='',main="",cex.main=1)
          legend("topleft",cf.lvls,fill=cf.cols,border="black",pch=c(rep(NA,length(cf.lvls))),title = cf.lab,title.adj = 0.2,
                 pt.bg = c(rep(NA,length(cf.lvls))),bg=NA,bty="n")
          
          legend("top",mc.lvls,fill=mc.cols,border="black",pch=c(rep(NA,length(mc.lvls))),title = mc.lab,title.adj = 0.2,
                 pt.bg = c(rep(NA,length(mc.lvls))),bg=NA,bty="n")
          
          legend("bottomleft",clap.lvls,fill=clap.cols,border="black",pch=c(rep(NA,length(clap.lvls))),title = "% Dead",title.adj = 0.2,
                 pt.bg = c(rep(NA,length(clap.lvls))),bg=NA,bty="n")
          
          if(banks[i] != "GB")
          {
          legend("bottomright",pch=c(20,24), pt.bg = c("black","darkorange"), title="Tow type",inset=0.01,
                 legend = c(paste('regular (n =',
                                  length(subset(surv.seed,year==yr & state=='live'& random==1)$ID),")",sep=""),
                            paste('exploratory (n =',
                                  length(subset(surv.seed,year==yr & state=='live'& random %in%  c(0,2,4,5))$ID),")",sep="")),
                 bg=NA,box.col=NA,bty="n")
          
        } # end if(banks[i] != "GB")
        if(banks[i] == "GB")
        {
          legend("bottomright",legend = c(paste('exploratory (n =',
                                            length(unique(subset(surv.seed,year==yr & random %in% c(2,4,5))$tow)),")",sep=""),
                                      paste('repeated (n =',length(unique(subset(surv.seed,year==yr & 
                                                                                   random==3)$tow)),")", sep="")),title="Tow type",
                 pt.bg = c("darkorange","yellow"),pch=c(24,22),bg = NA,inset=0.01,box.col=NA)
        } # # end if(banks[i] == "GB")
          if(add.title == T) title(paste("Seedbox ",fig.box.name[j]," (",banks[i],"-",yr,")",sep=""),cex.main=2,outer=T,line=-0.5)
          if(fig != "screen") dev.off()
          
          
        } # end for(j in 1:n.box)
      } # end !is.null(mod.res[["Clap-spatial"]]) &....    
    } # end (if nrow(sb))
  } # end the if(any(plots) %in% "seedboxes")
  
  
  
  
} # end the i for loop 

} # end the function
