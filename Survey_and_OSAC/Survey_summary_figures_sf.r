#####################################  This function is used to generate all of the figures used in survey summary  #####################
#####################################  A major update occured in May of 2017, moving from contour plots to INLA spatial models ###########
#####################################  Banquereau is  not included in this function at this time ######
## Created by DK December 2015
# Update history
#Commented, checked  and revised by DK March 31, 2016
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
# Jan 2021:     Changed default behaviour of direct_fns, now points to github if not specified.
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
###               2: "run.full"   THIS WILL RUN AND SAVE the models for ALL spatial figures, not just the models specified by the "plots" arguement above.
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

#11: direct_fns The working that the functions are located in.  Default is missing which goes to github.

#12:gis.repo   The working directory to pull in the GIS layers, just needed in two spots so being lazy for now and not linking to github
#              Default is the location of the Repo on the ESS which isn't tied to Github directly and depends on it be manually updated.

#13: save.gg    Do you want to save the ggplots you made for later in life....

#14: season    For the spring survey we need to identify that we don't have all the results in yet.  When running the scripts with only spring  
###              survey data set to "spring".  If just running GBa and GBb you can set this to "summer" if you've already created the Rdata file.
###              When summer survey is complete you can also set this to the default of "both".  I've also added the option during preliminary
###              runs or when altering the function to use a "testing" option so it loads the proper data.  You'll need to have created the 
###              testing R data when running the SurveySummary_data.r.  Additionally when set to "testing" the figures (if being saved) will be
###              saved in the folder... direct,yr,"/Presentations/Survey_summary/test_figures/",banks[i].

#15: nickname  This is used if you have a specific output from Survey_summary_data call.  You want this to be the same as what you used there, e.g. "GB_special_run"

#16: sub.area  Do you want to make plots of the user specfied sub areas, currently only relevant for GBa.  T/F, default = F

#17: full.GB    Set to true (T) if you want to plot all of GB on one INLA spatial map (instead of GBa and GBb separately). Default is F


#############################################################################################################################################
##### NB: If trying to reproduce figures from previous years make sure the Rdata output from SurverySummary_data.r for that year exists! ####
#############################################################################################################################################

survey.figs <- function(plots = 'all', banks = "all" , yr = as.numeric(format(Sys.time(), "%Y"))  ,
                        fig="screen", scale.bar = NULL, bathy = 50, add.title = T, INLA = "run" , s.res = "low",
                        direct = "Y:/Offshore scallop/Assessment/", direct_fns,gis.repo = "Y:/GISData/Github_Repo/GIS_layers",
                        save.gg = F, season="both",nickname=NULL, sub.area=F, full.GB=F, layout="portrait")
{ 
  options(scipen = 999,stringsAsFactors = F)
  tmp.dir <- direct ; tmp.season <- season; tmp.yr <- yr # I need this so that the directory isn't overwritten when I load the below...
  
  if(yr==2020) {
    message("Due to the pandemic, the 2020 survey was modified and lead by Industry. Only BBn, GBa, and GBb were surveyed.\n
                       Survey design and sampling protocols were changed, so these figures cannot be created via the normal approach.")
    browser()
    source(paste0(direct_fns, "/Survey_and_OSAC/Industry2020_Survey_summary_figures_sf.r"))
    Ind2020.survey.figs(direct = direct, fig=fig,
                        yr=yr, 
                        banks = banks,
                        s.res=s.res,
                        plots = plots, 
                        sub.area=sub.area, 
                        INLA=INLA, season=season, nickname=nickname, 
                        scale.bar = c("bl", 0.25),
                        bathy = c(10, "c"),
                        se=T)
    stop("Not an error actually, you're just all done with the special 2020 survey!")
  } 
  # Load the appropriate data.
  # If you used a plot shortcut, get the correct names for the plots you
  
  if(plots == 'all') 
  {
    plots <- c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial","Survey","MW-SH",
               "abund-ts","biomass-ts","SHF","SHF-large","SHF-split",
               "clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown","seedboxes","user.SH.bins",
               "MW-spatial","SH-spatial","MW.GP-spatial","SH.GP-spatial")
  } # end if(plots == 'all')
  
  if(plots == 'spatial') 
  {
    plots <- c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial",
               "MW-spatial","SH-spatial","MW.GP-spatial","SH.GP-spatial")
  } # end if(plots == 'spatial')
  
  if(plots == 'simple')
  {
    plots <- c("MW-SH","abund-ts","biomass-ts","SHF","SHF-large","SHF-split",
               "clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown")
  }# end if(plots == 'simple'
  temp.nick <- nickname
  season <- tmp.season 
  yr <- tmp.yr
  dir.temp <- direct
  dir.fn.temp <- direct_fns
  if(season == "testing") 
  {
    # Some temporary assignments so we can load in old data without overwritting more important crap
    temp.nick <- nickname
    season <- tmp.season 
    yr <- tmp.yr
    dir.temp <- direct
    dir.fn.temp <- direct_fns
    
    # If we are making the MW/SH plot & looking at GB we need to get both the spring and summer GB data
    if(any(plots %in% "MW-SH") && any(banks %in% "GB"))
    {
      # This loads last years Survey object results.
      if(!tmp.yr==2021) load(paste(direct,"Data/Survey_data/",(tmp.yr-1),"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
      if(tmp.yr==2021) load(paste(direct,"Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
      if(dim(survey.obj$GBa$model.dat[survey.obj$GB$model.dat$year==(yr-1),])[1]==0) message("Edit line 216 to pull in last year's Survey summary object for the GB MWSH plot.")
      survey.obj.last <- survey.obj
      yr <- tmp.yr
    } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
        
    
    if(any(plots %in% "MW-SH") && any(banks %in% "GBa"))
    {
      # This loads last years Survey object results.
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/testing_results_spring2022_2.Rdata",sep=""))  
      if(dim(survey.obj$GB$model.dat[survey.obj$GB$model.dat$year==yr,])[1]==0) message("Edit line 199 to pull in the spring survey summary object for the GB MWSH plot.")
      survey.obj.last <- survey.obj
    } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
    
    nickname <- temp.nick
    direct <- dir.temp
    direct_fns <- dir.fn.temp
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
  } # end if(season == "testing") 
  nickname <- temp.nick
  direct <- dir.temp
  direct_fns <- dir.fn.temp
  season <- tmp.season 
  yr <- tmp.yr
  if(season == "both") 
  {
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))==T)
    {
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
      survey.obj.last <- survey.obj
      season <- tmp.season # Needed b/c there is a season in the object I'm loading too..
    } else stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_all_results.Rdata' gets created, Thanks eh!!")
  } # end if(season == "both") 
  season <- tmp.season 
  yr <- tmp.yr
  dir.temp <- direct
  dir.fn.temp <- direct_fns
  if(season == "spring") 
  {
    # If we are making the MW/SH plot & looking at GB we need to get both the spring and summer GB data
    if(any(plots %in% "MW-SH") && any(banks %in% "GB"))
    {
      # This loads last years Survey object results.
      if(!tmp.yr==2021) load(paste(direct,"Data/Survey_data/",(tmp.yr-1),"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
      if(tmp.yr==2021) load(paste(direct,"Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
      if(dim(survey.obj$GBa$model.dat)[1]==0) message("Edit line 273 to pull in last year's Survey summary object for the GB MWSH plot.")
      survey.obj.last <- survey.obj
    } # end if(any(plots %in% "MW-SH") & any(banks %in% "GBa"))
    season <- tmp.season 
    yr <- tmp.yr
    direct <- dir.temp
    direct_fns <- dir.fn.temp
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))==T)
    {
      load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep=""))  
      season <- tmp.season # Needed b/c there is a season in the object I'm loading too..
    } else stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_spring_results.Rdata' gets created, Thanks eh!!") # end if/else file.
  } # end if(season == "spring") 
  season <- tmp.season 
  yr <- tmp.yr
  direct <- dir.temp
  direct_fns <- dir.fn.temp
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
  nickname <- temp.nick
  direct <- dir.temp
  direct_fns <- dir.fn.temp
  season <- tmp.season 
  yr <- tmp.yr
  
  if(exists("survey.obj.last")) {
    if(max(survey.obj.last$GBa$model.dat$year)>2020 & !2020 %in% survey.obj.last$GBa$model.dat$year) {
      y2020 <- survey.obj.last$GBa$model.dat[survey.obj.last$GBa$model.dat$year==2021,]
      y2020$year <- 2020
      y2020[,2:ncol(y2020)] <- NA
      survey.obj.last$GBa$model.dat <- dplyr::arrange(dplyr::full_join(survey.obj.last$GBa$model.dat, y2020), year)
    }
  }
  
  # Now get the banks to plot set up.
  if(banks == "all") banks <- c("BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB")
  # This is useful for testing...
  if(banks == 'core') banks <- c("BBn" , "GBb", "GBa")
  
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
  } # end if(yr == 2018 && any(banks %in% "Sab")) 
  
  direct <- tmp.dir # I need this so that the directory isn't overwritten when I load the above
  
  # Bring in packages and functions
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/stdts.plt.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/survey.ts.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/shf.plt.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/shwt.plt1.R",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/Clap3.plt.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/gridPlot.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/meat_count_shell_height_breakdown_figure.r",
              "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/pectinid_projector_sf.R")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
    for(fun in funs) 
    {
      download.file(fun,destfile = basename(fun))
      source(paste0(getwd(),"/",basename(fun)))
      file.remove(paste0(getwd(),"/",basename(fun)))
    } # end for(un in funs)
  } # end if(missing(direct_fns))
  
  # These are the functions used to within the heart of the code to make stuff happen
  if(!missing(direct_fns))  
  {
    source(paste(direct_fns,"Maps/pectinid_projector_sf.R",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/stdts.plt.R",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/survey.ts.r",sep=""),local=T)
    source(paste(direct_fns,"Survey_and_OSAC/shf.plt.r",sep=""))
    source(paste(direct_fns,"Survey_and_OSAC/shwt.plt1.r",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/Clap3.plt.R",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/gridPlot.r",sep="")) 
    source(paste(direct_fns,"Survey_and_OSAC/meat_count_shell_height_breakdown_figure.r",sep="")) 
  } # end if(!missing(direct_fns))
  
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
  require(dplyr) || stop("It's 2020. We have entered the world of dplyr. ")
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
  if(!file.exists(paste0(direct, "Assessment_fns/Survey_and_OSAC/SurveySummaryRunLog.csv"))) runlog <- data.frame(X=NULL, runfunction=NULL, runassigned=NULL, rundefaults=NULL)
  if(file.exists(paste0(direct, "Assessment_fns/Survey_and_OSAC/SurveySummaryRunLog.csv"))) runlog <- read.csv(paste0(direct, "Assessment_fns/Survey_and_OSAC/SurveySummaryRunLog.csv"))
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
    sb <- subset(seedboxes,#Bank == banks[i] & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep="") | 
                 Bank == banks[i] & Active=="Yes")
    if(banks[i] == "GB")  sb <- subset(seedboxes, Bank %in% c("GBa","GBb") & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep="") | Bank %in% c("GBa","GBb") & Active=="Yes")
    
    ###  Now for the plots, first the survey data...
    # Get the  bank survey boundary polygon
    if(banks[i] == "BanIce") {
      bound.surv.poly[[banks[i]]] <- as.data.frame(bound.surv.poly[[banks[i]]])
      names(bound.surv.poly[[banks[i]]]) <- c("PID", "SID", "POS", "X", "Y", "label", "startyear")
    }
    
    if(banks[i] %in% c("GBa","GBb","BBn","BBs", "Ban", "BanIce", "GB", spat.name)) {
      if(sub.area==T) bound.poly.surv.GBsub <- lapply(bound.surv.poly[which(grepl(x=names(bound.surv.poly), "GBa-"))], function(x) as.PolySet(x, projection="LL"))
      bound.poly.surv <- as.PolySet(bound.surv.poly[[banks[i]]],projection ="LL")
    }
    if(!is.null(full.GB) & full.GB == T) {
      bound.poly.surv <- as.PolySet(rbind(bound.surv.poly$GBa, bound.surv.poly$GBb), projection ="LL")
    }
    # Need to treat Sable special...
    if(banks[i] %in% c("Sab")) 
    {
      if(yr < max(survey.bound.polys$startyear[survey.bound.polys$label=="Sab"])) bound.poly.surv <-as.PolySet(subset(survey.bound.polys[survey.bound.polys$startyear==1900,],
                                                                                                                      label==banks[i], 
                                                                                                                      select=c("PID", "SID", "POS", "X", "Y", "label")), 
                                                                                                               projection ="LL")
      if(!yr < max(survey.bound.polys$startyear[survey.bound.polys$label=="Sab"])) bound.poly.surv <-as.PolySet(subset(survey.bound.polys[survey.bound.polys$startyear==2018,],
                                                                                                                       label==banks[i], 
                                                                                                                       select=c("PID", "SID", "POS", "X", "Y", "label")), 
                                                                                                                projection ="LL")
    } # end  if(banks[i] %in% c("Sab"))
    
    #Detailed survey polygons
    if(banks[i] %in% c("GBa","GBb","BBn","BBs",spat.name)) detail.poly.surv <- as.PolySet(detail.surv.poly[[banks[i]]],projection ="LL")
    if(banks[i] %in% c("Sab")) 
    {
      if(yr < max(survey.detail.polys$startyear[survey.bound.polys$label=="Sab"])) detail.poly.surv <- as.PolySet(subset(survey.detail.polys[survey.detail.polys$startyear==1900,],
                                                                                                                         label==banks[i], 
                                                                                                                         select=c("PID", "SID", "POS", "X", "Y", "label", "Strata_ID")),
                                                                                                                  projection = "LL")
      if(!yr < max(survey.detail.polys$startyear[survey.bound.polys$label=="Sab"])) detail.poly.surv <- as.PolySet(subset(survey.detail.polys[survey.detail.polys$startyear==2018,],
                                                                                                                          label==banks[i], 
                                                                                                                          select=c("PID", "SID", "POS", "X", "Y", "label", "Strata_ID")),
                                                                                                                   projection = "LL")
    } # end if(banks[i] %in% c("Sab")) 
    
    # Get the strata areas.  For most areas we use the survey.strata.table which is output from the data function
    if(banks[i] %in% c("GBa","GBb","BBs",spat.name)) strata.areas <- subset(survey.strata.table[[banks[i]]],select =c("PID","towable_area"))
    
    if(banks[i] %in% c("Sab") & !yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
      strata.areas <- subset(survey.info[!(survey.info$startyear==1900 & survey.info$label=="Sab"),], label==banks[i],select =c("PID","towable_area"))}
    if(banks[i] %in% c("Sab") & yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
      strata.areas <- subset(survey.info[!(survey.info$startyear==2018 & survey.info$label=="Sab"),], label==banks[i],select =c("PID","towable_area"))}
    # BBn too since areas were corrected in 2021
    if(banks[i] %in% c("BBn") & !yr < max(survey.info$startyear[survey.info$label=="BBn"])) {
      strata.areas <- subset(survey.info[!(survey.info$startyear==1900 & survey.info$label=="BBn"),], label==banks[i],select =c("PID","towable_area"))}
    if(banks[i] %in% c("BBn") & yr < max(survey.info$startyear[survey.info$label=="BBn"])) {
      strata.areas <- subset(survey.info[!(survey.info$startyear==2021 & survey.info$label=="BBn"),], label==banks[i],select =c("PID","towable_area"))}
    if(banks[i] %in% c("GB", "Mid", "Ger", "Ban", "BanIce")) strata.areas <- NULL
    
    #Get all the details of the survey strata
    if(banks[i] %in% c("Sab") & !yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
      surv.info <- survey.info[survey.info$startyear==2018 & survey.info$label=="Sab",]}
    if(banks[i] %in% c("Sab") & yr < max(survey.info$startyear[survey.info$label=="Sab"])) {
      surv.info <- survey.info[!(survey.info$startyear==2018) & survey.info$label=="Sab",]}
    
    if(banks[i] %in% c("BBn") & !yr < max(survey.info$startyear[survey.info$label=="BBn"])) {
      surv.info <- survey.info[survey.info$startyear==2021 & survey.info$label=="BBn",]}
    if(banks[i] %in% c("BBn") & yr < max(survey.info$startyear[survey.info$label=="BBn"])) {
      surv.info <- survey.info[!(survey.info$startyear==2021) & survey.info$label=="BBn",]}
    
    if(!banks[i] %in% c("Sab", "BBn")) surv.info <- survey.strata.table[[banks[i]]]
    
    # for 2023 survey summary, do not use updated strata areas!
    surv.info <- surv.info[surv.info$startyear<2023,]
    
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
    
    if(banks[i] == "GBa" & sub.area==T) {
      subarea_df <- do.call(rbind,lapply(lapply(survey.obj[c("GBa-North", "GBa-West", "GBa-East", "GBa-Central", "GBa-South")], function(x) x$bankpertow),data.frame))
      subarea_cv <- do.call(rbind,lapply(lapply(survey.obj[c("GBa-North", "GBa-West", "GBa-East", "GBa-Central", "GBa-South")], function(x) x[[1]]),data.frame))
      subarea_df$subarea <- gsub(x=row.names(subarea_df),pattern = "\\..*",replacement="")
      subarea_cv$subarea <- gsub(x=row.names(subarea_cv),pattern = "\\..*",replacement="")
      subarea_df <- join(subarea_df, subarea_cv[, c("year", "subarea", "N.cv", "NPR.cv", "NR.cv", "I.cv", 'IR.cv', "IPR.cv", "CF")], type="full")
      
    }
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
      # If we are making spatial plots do this
      spatial.maps <- plots[grep("spatial",plots)]
      
      # Some stuff I need for both the Survey and spatial figures...
      if(length(spatial.maps)> 0 || grepl("Survey",plots))
      {
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
          g.tmp$PID <- as.numeric(g.tmp$PID)
          g.tmp$X <- as.numeric(g.tmp$X)
          g.tmp$Y <- as.numeric(g.tmp$Y)
          bound.poly.surv <- as.PolySet(g.tmp,projection="LL")
        } # end if(banks[i] == "Ger") 
        # Now convert this to an object for sp, this gets our bounding area for the survey.
        
        bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)
        
        # Next we get the survey locations
        if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","BanIce","SPB","GB"))
        {   
          loc <- data.frame(lon = surv.Live[[banks[i]]]$lon[surv.Live[[banks[i]]]$year == yr],
                            lat=surv.Live[[banks[i]]]$lat[surv.Live[[banks[i]]]$year == yr])
          # These are the locations for the Condition and meat count data.
          loc.cf <- data.frame(lon = CF.current[[banks[i]]]$lon[CF.current[[banks[i]]]$year == yr],
                               lat=CF.current[[banks[i]]]$lat[CF.current[[banks[i]]]$year == yr])
          # For the growth potential related figures we also need to make a special mesh as there could be some tows with 0 individuals
          # and these may screw up the INLA'ing
          loc.gp <- data.frame(lon = pot.grow[[banks[i]]]$slon[pot.grow[[banks[i]]]$year == yr],
                               lat=pot.grow[[banks[i]]]$slat[pot.grow[[banks[i]]]$year == yr])
        }# end if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","SPB","GB"))
        # I want 1 mesh for all of Georges bank summer survey.
        if(banks[i] %in% c("GBa","GBb")) 
        {
          loc <- data.frame(lon = c(surv.Live[["GBa"]]$lon[surv.Live[["GBa"]]$year == yr],surv.Live[["GBb"]]$lon[surv.Live[["GBb"]]$year == yr]),
                            lat=c(surv.Live[["GBa"]]$lat[surv.Live[["GBa"]]$year == yr],surv.Live[["GBb"]]$lat[surv.Live[["GBb"]]$year == yr]))
          
          # The condition and meat count data.
          loc.cf <- data.frame(lon = c(CF.current[["GBa"]]$lon[CF.current[["GBa"]]$year == yr],CF.current[["GBb"]]$lon[CF.current[["GBb"]]$year == yr]),
                               lat=c(CF.current[["GBa"]]$lat[CF.current[["GBa"]]$year == yr],CF.current[["GBb"]]$lat[CF.current[["GBb"]]$year == yr]))
          # For the growth potential related figures we also need to make a special mesh as there could be some tows with 0 individuals
          # and these may screw up the INLA'ing
          loc.gp <- data.frame(lon = c(pot.grow[["GBa"]]$slon[pot.grow[["GBa"]]$year == yr],pot.grow[["GBb"]]$slon[pot.grow[["GBb"]]$year == yr]),
                               lat=c(pot.grow[["GBa"]]$slat[pot.grow[["GBa"]]$year == yr],pot.grow[["GBb"]]$slat[pot.grow[["GBb"]]$year == yr]))
          
        } # end if(banks[i] %in% c("GBa","GBb") 
        
        if(banks[i] %in% c("Mid","Sab","Ban","BanIce","SPB", "BBs")) 
        {
          loc.sf <- st_as_sf(loc,coords = c('lon','lat'),crs = 4326)
          loc.sf <- st_transform(loc.sf,crs = 32620)
          loc    <- as(loc.sf,"Spatial")
          loc.cf <- st_as_sf(loc.cf,coords = c('lon','lat'),crs = 4326)
          loc.cf <- st_transform(loc.cf,crs = 32620)
          loc.cf <- as(loc.cf,"Spatial")
          #bound.poly.surv.sp.buff <- spTransform(bound.poly.surv.sp.buff,CRS = st_crs(32620)[2]$proj4string)
          bound.poly.surv.sp <- spTransform(bound.poly.surv.sp, CRSobj = st_crs(32620)[[2]])
          bound.poly.surv.sf <- st_transform(st_as_sf(bound.poly.surv.sp),crs = 32620)
        }  
        
        if(banks[i] %in% c("GBa","GBb","BBn","GB","Ger")) 
        {
          loc.sf <- st_as_sf(loc,coords = c('lon','lat'),crs = 4326)
          loc.sf <- st_transform(loc.sf,crs = 32619)
          loc    <- as(loc.sf,"Spatial")
          loc.cf <- st_as_sf(loc.cf,coords = c('lon','lat'),crs = 4326)
          loc.cf <- st_transform(loc.cf,crs = 32619)
          loc.cf <- as(loc.cf,"Spatial")
          bound.poly.surv.sp <- spTransform(bound.poly.surv.sp, CRSobj = st_crs(32619)[[2]])
          bound.poly.surv.sf <- st_transform(st_as_sf(bound.poly.surv.sp),crs = 32619)
        }

        if(!banks[i] %in% c("GBa", "GBb")) {
          if(exists("bound.poly.surv.sf") & length(unique(surv.Live[[banks[i]]]$random[surv.Live[[banks[i]]]$year==yr]))>1) {
            out <- loc.sf %>% mutate(
              intersection = as.integer(st_intersects(geometry, bound.poly.surv.sf)))
            # check for locations outside the domain
            if(any(is.na(out$intersection))) { # need to expand poly
              message("bound.poly.surv.sp expanded to accomodate stations outside domain for spatial modelling purposes.
                    These are likely due to extras. Please make sure you're ok with this!")
              
              pts_to_add <- out[is.na(out$intersection),]
              poly_to_add <- st_buffer(st_as_sfc(st_bbox(pts_to_add)), 2000)
              # plot(bound.poly.surv.sf)
              # plot(loc.sf, add=T)
              # plot(pts_to_add, add=T)
              # plot(poly_to_add, add=T)
              
              bound.poly.surv.sf <- st_union(st_make_valid(bound.poly.surv.sf), poly_to_add)
              bound.poly.surv.sp <- as_Spatial(st_geometry(bound.poly.surv.sf))
            }
          }
        }
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
          if(banks[i] == "Ban") range <- 15 * 1000 ; max.edge <- range/5
          
          # Will this work for them all I wonder? Max edge should be around 1/5 of the range according to Zuur
          mesh <- inla.mesh.2d(loc, boundary= inla.sp2segment(bound.poly.surv.sp), max.edge=c(1,5)*max.edge, cutoff=max.edge)
          #if(banks[i] %in% c("GBa","GBb","BBn","BBs","GB","Ger")) 
          mesh$crs <- raster::crs(st_crs(bound.poly.surv.sp)[[2]])
          # if(banks[i] %in% c("Mid","Sab","Ban","BanIce","SPB")) mesh$crs <- raster::crs(st_crs(bound.poly.surv.sp)[[2]])
          plot(mesh) # For testing I want to plot this to see it and ensure it isn't crazy for the moment...
          #if(!banks[i] %in% c("GB", "Ban", "BanIce", "Sab")) mesh <- inla.mesh.2d(loc, boundary= inla.sp2segment(bound.poly.surv.sp), max.edge=c(1,5)*max.edge, cutoff=max.edge/1.5)
          #if(banks[i] == "GB") mesh <- inla.mesh.2d(loc, boundary=bound.buff, max.edge=c(0.04))
          #if(banks[i] == "Sab") mesh <- inla.mesh.2d(loc, boundary=bound.buff, max.edge=c(0.05))
          #if(banks[i] %in% c("Ban", "BanIce")) mesh <- inla.mesh.2d(loc, boundary=bound.buff, max.edge=c(0.075))
          windows(11,11) ; plot(mesh) ;  plot(bound.poly.surv.sp,add=T,lwd=2)
          cat("Mesh successful, woot woot!!")
          # Now make the A matrix
          
          A <- inla.spde.make.A(mesh, loc)
          A.cf <- inla.spde.make.A(mesh,loc.cf)
          
          
          # We can just make the one spde object for all of these as well.
          spde <- inla.spde2.pcmatern(mesh,    
                                      prior.sigma=c(2,0.5), # The probabiliy (second number) that the marginal standard deviation (first number) is larger than the first number
                                      prior.range=c(range,0.5)) # The Meidan range and the probability that the range is less than this..
          # Because of the generally thin spacing on GB we need to decrease the spatial correlation distance and allow for more spatial variability in the 
          # data, so I have changed the priors...  Revised by DK August 2018, not fully incorporated into the Spring Survey summary presentation
          # previous prior.range was c(1,0.5) (July 2018 spring survey summary used this value)
          if(banks[i] %in% c("GB", "Sab"))
          {
            spde <- inla.spde2.pcmatern(mesh,    
                                        prior.sigma=c(4,0.75), # The probabiliy that the marginal standard deviation (first number) is larger than second number
                                        prior.range=c(range,0.5)) # The Meidan range and the probability that the range is less than this..
          }
          
          ## All of our abundance spatial plots are counts
          family1 = "poisson"
          family1.cf <- "gaussian" # For CF, MC,MW, and SH they are more normal so go with a gaussian.
          family.clap <- "beta" # I haven't found a good family for the clapper data, for the moment the poisson does a decent job as long
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
            
            if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","BanIce","SPB","GB")) 
            {  
              tmp.dat <- surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,]
              tmp.cf <- CF.current[[banks[i]]]
              tmp.clap <- surv.Clap[[banks[i]]][surv.Clap[[banks[i]]]$year == yr,]
              if(!banks[i] == "BanIce") tmp.gp <- pot.grow[[banks[i]]][pot.grow[[banks[i]]]$year == yr,]
            } # end if(banks[i] %in% c("Mid","Sab","Ger","BBn","BBs","Ban","BanIce","SPB","GB")) 
            # Now loop through each spatial map we want to make.
            fitted <- NULL
            for(k in 1:length(spatial.maps))
            {
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
              
              if(spatial.maps[k] == "CF-spatial")       
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
              if(spatial.maps[k] == "MC-spatial")      
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
                # Beta transform
                tmp.clap$clap.prop <- beta.transform(tmp.clap$clap.prop/100)
                # This is the stack for the INLA model
                stk <- inla.stack(tag="est",data=list(y = tmp.clap$clap.prop, link=1L),
                                  effects=list(a0 = rep(1, nrow(tmp.clap)), s = 1:spde$n.spde),
                                  A = list(1, A))
                # This is the INLA model itself
                mod <- inla(formula3, family=family.clap, data = inla.stack.data(stk),
                            control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
                
                fitted[[spatial.maps[k]]] <- data.frame(fitted = mod$summary.fitted.values$mean[1:length(tmp.clap$clap.prop)],
                                                        dat=tmp.clap$clap.prop)
              } # end if(spatial.maps[k] == "Clap-spatial")  
              
              if(spatial.maps[k] == "MW-spatial")    
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
              
              if(spatial.maps[k] == "SH-spatial")    
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
              
              if(spatial.maps[k] == "MW.GP-spatial")    
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
              
              if(spatial.maps[k] == "SH.GP-spatial")    
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
              } # end if(spatial.maps[k] == "SH.GP-spatial")  
              
              
              if(spatial.maps[k] %in% c("FR-spatial","PR-spatial","Rec-spatial","SH-spatial","SH.GP-spatial",
                                        "MW-spatial","MW.GP-spatial")) mod.res[[spatial.maps[k]]] <- 
                  exp(mod$summary.random$s$mean + mod$summary.fixed$mean)
              
              # Now for the Gaussian models.
              if(spatial.maps[k] %in% c("CF-spatial","MC-spatial")) mod.res[[spatial.maps[k]]] <- 
                  mod$summary.random$s$mean + mod$summary.fixed$mean
              
              # print a message if the model didn't work:
              if(max(mod.res[[spatial.maps[k]]], na.rm=T) == "Inf") stop(paste0("Inf predictions in mod.res[[spatial.maps[k]]]. Please try a different mesh for ", banks[i], " ", spatial.maps[k], ".\nRecommend changing inla.mesh.2d max.edge argument very slightly."))
              # Needed to make the clapper spatial work...
              
              if(spatial.maps[k] == "Clap-spatial")  mod.res[[spatial.maps[k]]] <- inv.logit(mod$summary.random$s$mean + mod$summary.fixed$mean)*100
              
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
       
          # Now let's start off by making our base map, if we want to make this work for inshore then we'd need to figure out how to deal with the sfa piece
          # encountering some issues with pecjector, so using tryCatch to determine if it's going to work, and if not, I add a 0.001 buffer.
          x <- tryCatch(print(eval(parse(text = 'pecjector(area = banks[i],
                         plot = F,
                         repo = direct_fns, 
                         c_sys = st_crs(mesh$crs)$epsg, 
                         quiet=T)'))), 
                        error = function(e) e)
          
          if("error" %in% class(x)) {
            p <- pecjector(area = banks[i], buffer=0.0001,
                           plot = F,
                           repo = direct_fns, 
                           c_sys = st_crs(mesh$crs)$epsg, 
                           quiet=T,
                           add_layer = list(eez = 'eez', 
                                            sfa = 'offshore', 
                                            bathy = bathy, 
                                            scale.bar= scale.bar)) +
              theme(panel.grid=element_blank(), 
                    axis.ticks=element_line(),
                    legend.position = 'right',
                    legend.direction = 'vertical',
                    legend.justification = 'left',
                    legend.key.size = unit(.5,"line")) #+
            #coord_sf(expand=F)
          } 
          if(!"error" %in% class(x)) {
            p <- pecjector(area = banks[i], 
                           plot = F,
                           repo = direct_fns, 
                           c_sys = st_crs(mesh$crs)$epsg, 
                           quiet=T,
                           add_layer = list(eez = 'eez', 
                                            sfa = 'offshore', 
                                            bathy = bathy, 
                                            scale.bar= scale.bar)) +
              theme(panel.grid=element_blank(), 
                    axis.ticks=element_line(),
                    legend.position = 'right',
                    legend.direction = 'vertical',
                    legend.justification = 'left',
                    legend.key.size = unit(.5,"line")) #+
            #coord_sf(expand=F)
          } 
          # # manually adjust the bathy lines
          # p$layers[[2]]$aes_params$colour <- "blue"
          # p$layers[[2]]$aes_params$alpha <- 0.25
          # 
          
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
            
            if(maps.to.make[m]  %in% c("CF-spatial"))   
            {
              base.lvls <- c(0,5,8,10,12,14,16,18,max(c(20,ceiling(max(mod.res[[maps.to.make[m]]])))))
              cols <- rev(viridis::inferno(length(base.lvls)-1,alpha=0.7,begin=0.35,end=1))
              # Get the levels correct add extra levels to this to get a better resolution if we have levels > 18
              if(median(mod.res[[maps.to.make[m]]], na.rm=T) > 18) 
              {
                byextra <- pretty(max(base.lvls[-length(base.lvls)]):round(max(mod.res[[maps.to.make[m]]], na.rm=T)), n = 3)
                extra.lvls <- byextra[-1]
                # extra.lvls <- c(max(base.lvls[-length(base.lvls)]) + byextra, 
                #                 max(base.lvls[-length(base.lvls)]) + byextra*2,  
                #                 ceiling(max(mod.res[[maps.to.make[m]]])))
                extra.cols <- viridis::inferno(length(extra.lvls) + 1 ,alpha=0.7,begin=0.35,end=0.05)[-1]
                base.lvls <- c(base.lvls[-length(base.lvls)], extra.lvls)
                cols <- c(cols, extra.cols[-length(extra.cols)])
              }
              
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
              fig.title <- substitute(bold(paste("Clappers (% dead, ", bank,"-",year,")",sep="")),
                                      list(c=as.character(RS),bank=banks[i],year=as.character(yr)))
              if(banks[i] == "GB") clap.dis.title <- substitute(bold(paste("Clappers (% dead, ", bank,"-Spr-",year,")",sep="")),
                                                                list(c=as.character(RS),bank=banks[i],year=as.character(yr)))
              leg.title <- "% Dead"
            } # end if(maps.to.make[m]  %in% c("Clap-spatial")
            
            
            if(maps.to.make[m]  %in% c("MW-spatial"))   
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
            
            
            if(maps.to.make[m]  %in% c("SH.GP-spatial"))   
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
            
            if(maps.to.make[m]  %in% c("MW.GP-spatial"))   
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
            
            
            
            # Don't add the titles?
            if(add.title == T)  p <- p + ggtitle(fig.title) + theme(plot.title = element_text(face = "bold",size=20, hjust=0.5))
            
            ######## Produce the figure######## Produce the figure######## Produce the figure######## Produce the figure
            ######## Produce the figure######## Produce the figure######## Produce the figure######## Produce the figure
            # Do we want to save the figure to a file or just output to the screen?  
            if(fig == "png") png(paste(plot.dir,maps.to.make[m],".png",sep=""),units="in",width = 11,height = 8.5,res=420,bg = "transparent")
            if(fig == "pdf") pdf(paste(plot.dir,maps.to.make[m],".pdf",sep=""),width = 11,height = 8.5,bg = "transparent")
            if(fig == "screen") windows(11,8.5)

            if(exists("poly_to_add")){
              
              if(!st_crs(bound.poly.surv.sf)==st_crs(poly_to_add)) poly_to_add <- st_transform(poly_to_add, crs = st_crs(bound.poly.surv.sf))
              
              if(maps.to.make[m] %in% c("MW.GP-spatial","MW-spatial","CF-spatial","MC-spatial")){
                bound.poly.surv.sf <- st_difference(bound.poly.surv.sf, poly_to_add)
              }
              
              if(!maps.to.make[m] %in% c("MW.GP-spatial","MW-spatial","CF-spatial","MC-spatial")){
                if(!st_geometry(bound.poly.surv.sf) == st_geometry(st_union(bound.poly.surv.sf, poly_to_add))){
                  bound.poly.surv.sf <- st_union(bound.poly.surv.sf, poly_to_add)
                }
              }
            }
           
            # Here we add our layer to the object above.  This is going to become a list so we can save it and modify it outside Figures.
            p2 <- pecjector(gg.obj = p, 
                            area = banks[i],
                            legend=T,
                            plot=F,
                            repo = direct_fns, 
                            c_sys = st_crs(mesh$crs)$epsg,
                            add_inla= list(field = mod.res[[maps.to.make[m]]],
                                           mesh = mesh, 
                                           dims = s.res,
                                           clip = bound.poly.surv.sf,
                                           scale = list(scale = "discrete",
                                                        breaks = base.lvls, 
                                                        palette = cols,
                                                        leg.name=leg.title, 
                                                        alpha=0.75))) +
              geom_sf(data=bound.poly.surv.sf, colour="black", fill=NA) + coord_sf(expand=F)
           
           #plot(mesh)
           
            ################ ENd produce the figure################ ENd produce the figure################ ENd produce the figure
            ################ ENd produce the figure################ ENd produce the figure################ ENd produce the figure
            
            
            ############  Add the points and the legend to the figure############  Add the points and the legend to the figure
            ############  Add the points and the legend to the figure############  Add the points and the legend to the figure
            ############  Add the points and the legend to the figure############  Add the points and the legend to the figure
            # Add the regular survey tows, note this if statement is used to NOT add the following code to these plots...
     
            if(maps.to.make[m] %in% c("PR-spatial", "Rec-spatial", "FR-spatial",bin.names, "SH-spatial", "SH.GP-spatial","Clap-spatial"))
            {
              surv <- st_as_sf(surv.Live[[banks[i]]],coords = c('slon','slat'),crs = 4326,remove=F) %>% 
                dplyr::filter(year == yr & state == 'live')
              surv <- st_transform(surv,crs = st_crs(mesh$crs)$epsg)
              surv$`Tow type` <- paste0('regular (n = ',length(surv$random[surv$random==1]),")")
              if(banks[i] != 'Ger') surv$`Tow type`[surv$random != 1] <- paste0('exploratory (n = ',length(surv$random[surv$random!=1]),")")
              if(banks[i] == 'Ger') surv$`Tow type`[!surv$random %in% c(1,3)] <- paste0('exploratory (n = ',length(surv$random[!surv$random %in% c(1,3)]),")")
              if(banks[i] == 'Ger') surv$`Tow type`[surv$random == 3] <- paste0('repeated (n = ',length(surv$random[surv$random==3]),")")
              # Get the shapes for symbols we want, this should do what we want for all cases we've ever experienced...
              if(length(unique(surv$`Tow type`)) ==1) shp <- 21
              if(length(unique(surv$`Tow type`)) ==2) shp <- c(17,21)
              if(length(unique(surv$`Tow type`)) ==3) shp <- c(17,21,15)
              if(banks[i] == "Ger" & length(shp) == 2) shp <- c(21,15)
              
              # Make the plot
              p3 <- p2 + geom_sf(data=surv,aes(shape=`Tow type`),size=2) + scale_shape_manual(values = shp) + coord_sf(expand=F) +
                theme(legend.key = element_rect(fill=NA))
            }
      
            if(maps.to.make[m] %in% c("MW.GP-spatial","MW-spatial","CF-spatial","MC-spatial"))
            {
              surv <- st_as_sf(CF.current[[banks[i]]],coords = c('lon','lat'),crs = 4326)
              surv <- st_transform(surv,crs = st_crs(mesh$crs)$epsg)
              surv$`Tow type` <- paste0('detailed (n = ',nrow(surv),")")
              p3 <- p2 + geom_sf(data=surv,aes(shape=`Tow type`),size=2) + scale_shape_manual(values = 21) + coord_sf(expand=F) +
                theme(legend.key = element_rect(fill=NA))
            }
            
            ## NEXT UP FIGURE OUT THE SEEDBOXES!
            #Finally add seedboxes as appropriate
            if(length(sb[,1]) > 0)
            {
              sb[,c("X", "Y")] <- apply(sb[,c("X", "Y")], 2, function(x) as.numeric(x))
              sbs <- as.PolySet(sb, projection = "LL")
              sb.sf <- st_as_sf(PolySet2SpatialPolygons(sbs))
              sb.sf <- st_transform(sb.sf,crs = st_crs(loc.sf)$epsg)
              p3 <- p3 + geom_sf(data= sb.sf,fill=NA,lwd=1) + coord_sf(expand=F)
            }

            # Now print the figure
            print(p3 + guides(fill=guide_legend(order=2), shape=guide_legend(order=1)))
       
            if(save.gg == T) save(p3,file = paste0(direct,"Data/Survey_data/",yr,"/Survey_summary_output/",banks[i],"/",maps.to.make[m],".Rdata"))
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
      if(fig == "png") png(paste(plot.dir,"/survey_strata.png",sep=""),units="in",width = 11, height = 8.5,res=420,bg = "transparent")
      if(fig == "pdf")  pdf(paste(plot.dir,"/survey_strata.pdf",sep=""),width = 11,height = 8.5)
      if(fig == "screen") windows(11,8.5)
      
      if(banks[i] %in% spat.name) {
        bbox <- st_bbox(st_as_sf(bound.poly.surv.GBsub[[banks[i]]], coords=c(X="X", Y="Y"), crs=4326))
        p <- pecjector(area = list(y = c(bbox$ymin[[1]],bbox$ymax[[1]]),x = c(bbox$xmin[[1]],bbox$xmax[[1]]),crs = 4326),
                       plot = F,repo = direct_fns,
                       add_layer = list(eez = 'eez' , sfa = 'offshore',bathy = bathy,scale.bar = scale.bar)) +
          coord_sf(expand=F)
        
      }
      
      if(!banks[i] %in% spat.name) {
        p <- pecjector(area = banks[i],plot = F,repo = direct_fns,
                     add_layer = list(eez = 'eez' , sfa = 'offshore',bathy = bathy,scale.bar = scale.bar)) +
        coord_sf(expand=F)
      }
      #print(p)
      
      # For the banks with detailed strata...
      if(banks[i] %in% c("BBn" ,"BBs" ,"Sab", "GBb", "GBa")) shpf <- st_read(paste0(gis.repo,"/offshore_survey_strata/",banks[i],".shp"))
      if(banks[i] == "Sab" & yr>2017) {
        shpf$are_km2[shpf$Strt_ID==501] <- surv.info$area_km2[surv.info$Strata_ID==501 & surv.info$label=="Sab" & surv.info$startyear==2018]
        shpf$towbl_r[shpf$Strt_ID==501] <- surv.info$towable_area[surv.info$Strata_ID==501 & surv.info$label=="Sab" & surv.info$startyear==2018]
      }
      if(banks[i] %in% spat.name) shpf <- st_read(paste0(gis.repo,"/offshore_survey_strata/GBa.shp"))
      # For the ones we just have a cut of of the survey boundaries
      if(banks[i] %in% c("Ban","SPB")) shpf <- st_read(paste0(gis.repo,"/survey_boundaries/",banks[i],".shp"))

      # Use the cut out I make for the INLA models, it looks o.k.
      if(banks[i] %in% c("Mid","Ger")) shpf <- st_as_sf(bound.poly.surv.sp)
      if(!banks[i] == "GB") shpf <- st_transform(shpf,crs = st_crs(loc.sf)$epsg)
      
      surv <- st_as_sf(surv.Live[[banks[i]]],coords = c('slon','slat'),crs = 4326,remove=F) %>% 
        dplyr::filter(year == yr & state == 'live')
      surv <- st_transform(surv,crs = st_crs(loc.sf)$epsg)
      surv$`Tow type` <- paste0('regular (n = ',length(surv$random[surv$random==1]),")")
      
      if(banks[i] != 'Ger') surv$`Tow type`[surv$random != 1] <- paste0('exploratory (n = ',length(surv$random[surv$random!=1]),")")
      if(banks[i] == 'Ger') surv$`Tow type`[!surv$random %in% c(1,3)] <- paste0('exploratory (n = ',length(surv$random[!surv$random %in% c(1,3)]),")")
      if(banks[i] == 'Ger') surv$`Tow type`[surv$random == 3] <- paste0('repeated (n = ',length(surv$random[surv$random==3]),")")
      if(banks[i] == 'GB') surv$`Tow type`[surv$random == 3] <- paste0('regular (n = ',length(surv$random[surv$random==3]),")")
      # Get the shapes for symbols we want, this should do what we want for all cases we've ever experienced...
      if(length(unique(surv$`Tow type`)) ==1) shp <- 16; ptcol <- c("black")
      if(!banks[i] == "Ger" & length(unique(surv$`Tow type`)) ==2) shp <- c(24,16); ptcol <- c("darkorange", "black")
      if(banks[i] == "Ger" & length(unique(surv$`Tow type`))==2) shp <- c(16,22); ptcol <- c("black", "yellow")
      if(length(unique(surv$`Tow type`)) ==3) shp <- c(24,16,22); ptcol <- c("darkorange", "black", "yellow")
      
      # For this figure we want full bank names, this is ugly hack but does the trick.
      if(banks[i] %in% c("SPB","Ban", "BanIce", "BBn" ,"BBs" ,"Ger", "Mid", "Sab", "GB" ,"GBb", "GBa") && add.title == T)
      {    
        full.names <- data.frame(abrv = c("SPB","Ban","BanIce","Mid","Sab","Ger","BBs","BBn","GBa","GBb","GB"),
                                 full = c("St. Pierre Bank","Banquereau (Sea Scallop)","Banquereau (Icelandic)","Middle Bank","Sable Bank","German Bank","Browns Bank South",
                                          "Browns Bank North","Georges Bank 'a'","Georges Bank 'b'","Georges Bank Spring"))
        
        survey.title <- substitute(bold(paste("Survey (",bank," ",year,")",sep="")),
                                   list(year=as.character(yr),bank=as.character(full.names$full[full.names$abrv == banks[i]])))
        p <- p + ggtitle(survey.title)
      }

      # That's all we need for the areas without survey strata, pretty easy! No fill on strata
      if(banks[i] %in% c("SPB","Ban", "BanIce","Ger", "Mid"))
      {
        p2 <- p + geom_sf(data=shpf,fill =NA) + geom_sf(data=surv,aes(shape=`Tow type`, fill=`Tow type`), stroke=1.1) + 
          scale_shape_manual(values = shp) +
          scale_fill_manual(values = ptcol) +
          theme(legend.position = 'right',legend.direction = 'vertical',
                legend.justification = 'left',legend.key.size = unit(.5,"line")) 
      }
      
      if(banks[i] == "GB")
      {
        p2 <- p + geom_sf(data=surv,aes(shape=`Tow type`, fill=`Tow type`), stroke=1.1) + 
          scale_shape_manual(values = shp) +
          scale_fill_manual(values = ptcol) +
          theme(legend.position = 'right',legend.direction = 'vertical',
                legend.justification = 'left',legend.key.size = unit(.5,"line")) 
      }
      
      if(banks[i] %in% c("BBn","BBs","Sab", "GBb", "GBa"))
      {
        # Need to get the factor levels correct, always annoying...
        #Tows per strata is easy...
        if(banks[i] != "Sab" && banks[i] != "BBs") shpf$tow_num <- as.character(table(surv$Strata_ID[surv$random %in% c(1,3)]))
        if(banks[i] == "Sab") shpf$tow_num <- as.character(table(surv$Strata_ID_new[surv$random %in% c(1,3)]))
        # Because we never tow in that one tiny strata...
        if(banks[i] == "BBs") 
        {
          tmp <- as.character(table(surv$Strata_ID[surv$random %in% c(1,3)]))
          if(length(tmp) == 3) tmp <- c(tmp[1],0,tmp[2:3])
          shpf$tow_num <- tmp
        } # eend if(banks[i] == "BBs") 
        
        #sf package update for spherical geometry results in slightly different area calcs than before, so force it NOT to use spherical geometry here.
        sf::sf_use_s2(FALSE)
        # due to the issue with BBn strata that was discovered in 2021
        if(banks[i] == "BBn") shpf$are_km2 <- as.numeric(st_area(shpf)/1000000)
        
        shpf$are_km2 <- round(shpf$are_km2)
        shpf$`Number of Tows` <-  factor(1:length(shpf$tow_num))
        shpf$`Area (km^2)` <- factor(shpf$are_km2,levels = shpf$are_km2)
        
        shpf$ID <-  factor(shpf$PName, levels = shpf$PName)
        #shpf$Details <- paste0(shpf$PName,"                    Number of Tows = ", 
        #                      shpf$tow_num, "                 Area = ",   
        #                       shpf$are_km2,' km^2')
        #shpf$Details <-  factor(shpf$Details, levels = shpf$Details)
        cols <- unique(shpf$col)
        
        p2 <- p  + #geom_sf(data=shpf,aes(fill= Details))    +  scale_fill_manual(values = cols) + 
          geom_sf(data=shpf,aes(linetype = `Number of Tows`), alpha=0)  + 
          geom_sf(data=shpf,aes(colour = `Area (km^2)`), alpha=0, linetype="blank")  +
          new_scale("fill") + geom_sf(data=shpf,aes(fill= ID), colour=NA, alpha=0.7)    +  
          geom_sf(data=surv, aes(shape=`Tow type`)) + 
          scale_shape_manual(values = shp, guide=guide_legend(order=4)) +
          #taking advantage of OTHER aes types and then overriding them with fill (hacky but it works):
          scale_fill_manual(values = cols, guide=guide_legend(override.aes = list(fill= cols, col=cols), order=1))  +
          scale_colour_manual(values = cols, guide=guide_legend(override.aes = list(fill= cols, col=cols, alpha=0.7), order=2), name=expression(paste("Area (", km^{2}, ")")))  +
          scale_linetype_manual(values = rep("blank", length(cols)), guide=guide_legend(override.aes = list(fill= cols, col=cols, alpha=0.7), order=3), 
                                labels= shpf$tow_num)  +
          theme(legend.position = 'right',legend.direction = 'vertical',
                legend.justification = 'left',legend.key.size = unit(.5,"line"),
                legend.key = element_rect(fill = NA)) + coord_sf(expand=F)
      
      } # end  if(banks[i] %in% c("BBn" ,"BBs","Sab", "GBb", "GBa"))
      # Finally add seedboxes as appropriate
      if(length(sb[,1]) > 0)
      {
        sb[,c("X", "Y")] <- apply(sb[,c("X", "Y")], 2, function(x) as.numeric(x))
        sbs <- as.PolySet(sb, projection = "LL")
        sb.sf <- st_as_sf(PolySet2SpatialPolygons(sbs))
        sb.sf <- st_transform(sb.sf,crs = st_crs(loc.sf)$epsg)
        p2 <- p2 + geom_sf(data= sb.sf,fill=NA,lwd=1)+coord_sf(expand=F)
      }
      }
      if(save.gg == T) save(p2,file = paste0(direct,"Data/Survey_data/",yr,"/Survey_summary_output/",banks[i],"/Survey.Rdata"))
      print(p2 + coord_sf(expand=F))
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
      
      # Because name of BanIce is longer than other banks I added this so the figure title doesn't go off screen.
      cap.size <- ifelse(banks[i] == "BanIce",1.9,2)
      
      ############
      
      #Source12 Meat Height Shell weight plot on Slide 13  source("fn/shwt.plt1.r") 
      if(layout=="portrait"){
        if(fig == "screen") windows(8,13)
        if(fig == "png") png(paste(plot.dir,"/MWSH_and_CF_ts.png",sep=""),
                             units="in",width = 8.5,height = 13,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"/MWSH_and_CF_ts.pdf",sep=""),width = 8.5,height = 13)
      }
      
      if(layout=="landscape") {
        if(fig == "screen") windows(13,8)
        if(fig == "png") png(paste(plot.dir,"/MWSH_and_CF_ts_wide.png",sep=""),
                             units="in",width = 13,height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"/MWSH_and_CF_ts_wide.pdf",sep=""),width = 13,height = 8.5)
      }
      
      if(layout=="portrait") par(mfrow=c(2,1), oma=c(2,2,2,2))
      if(layout=="landscape") par(mfrow=c(1,2))
      
      shwt.plt1(SpatHtWt.fit[[banks[i]]],lw=3,ht=10,wd=12,cx=1.5,titl = MWSH.title,cex.mn = cap.size,las=1)
      
      # now the condition factor figure..
      # only show the median line if there are more than 3 CF values
      if(banks[i] != "Ger" && banks[i] != "GBa" && banks[i] != "GB")
      {
        if(length(which(!is.na(survey.obj[[banks[i]]][[1]]$CF))) > 3){
          stdts.plt(survey.obj[[banks[i]]][[1]],x=c('year'),y=c('CF'),pch=16,ylab=cf.lab,las=1,col=c("blue"),
                    median.line=T,graphic='none',xlab='Year',ylim=c(4,25),titl=CF.ts.title,cex.mn=cap.size)
        }
        if(length(which(!is.na(survey.obj[[banks[i]]][[1]]$CF))) < 4){
          stdts.plt(survey.obj[[banks[i]]][[1]],x=c('year'),y=c('CF'),pch=16,ylab=cf.lab,las=1,col=c("blue"),
                    median.line=F,graphic='none',xlab='Year',ylim=c(4,25),titl=CF.ts.title,cex.mn=cap.size)
        }
      }
      
      # DK Note that German is still calculated the "old way" using the cf.data at a specific location
      # There is no model for german and we are only interested in relative CF here so this is not 
      # A problem (i.e. this is just for comparitive purposes) but when we have some time this should be fixed.
      if(banks[i] == "Ger")
      {
        stdts.plt(cf.data[[banks[i]]]$CFyrs,y=c('CF','CF2'),pch=c(23,24),col=c('blue','red'),ylab=cf.lab,
                  median.line=T,graphic='none',xlab='Year',ylim=c(4,25),las=1,titl = CF.ts.title,cex.mn=cap.size,tx.ypos=4)
        legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.4,lty=c(1,2),col=c("blue","red"),bty="n")
      }
      
      # Have to add in the CF for May into the data
      if(banks[i] == "GBa")
      {
        stdts.plt(survey.obj[[banks[i]]][[1]],x=c('year'),y=c('CF'),pch=16,ylab=cf.lab,
                  median.line=T,graphic='none',xlab='Year',ylim=c(4,25),titl = CF.ts.title,cex.mn=cap.size,las=1)
        if(season=="both" | season=="testing")
        {
          points(survey.obj.last[["GB"]][[1]]$year-0.25,survey.obj.last[["GB"]][[1]]$CF,col="red", lty=2,pch=22,type="o",bg="red")
          lines(y=rep(median(survey.obj.last[["GB"]][[1]]$CF,na.rm=T), length(survey.obj.last[["GB"]][[1]]$year)-1), 
                x = survey.obj.last[["GB"]][[1]]$year[-length(survey.obj.last[["GB"]][[1]]$year)],col="red",lty=3)
          
        } # end if(season="both")
        if(season=="summer")
        {
          # Added in case R is treating year as a factor... 
          if(is.factor(survey.obj.last[["GB"]][[1]]$year)) 
          {  
            survey.obj.last[["GB"]][[1]]$year <- as.numeric(levels(survey.obj.last[["GB"]][[1]]$year))[survey.obj.last[["GB"]][[1]]$year]
          }
          lines(survey.obj.last[["GB"]][[1]]$year-0.25,survey.obj[["GB"]][[1]]$CF,col="red", lty=2)
          lines(y=rep(median(survey.obj.last[["GB"]][[1]]$CF,na.rm=T), length(survey.obj.last[["GB"]][[1]]$year)-1), 
                x = survey.obj.last[["GB"]][[1]]$year[-length(survey.obj.last[["GB"]][[1]]$year)],col="red",lty=3)
        } # end if(season="both")
        legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,22),bty='n',inset=0.02,col=c("blue","red"),pt.bg=c("blue","red"))		
      } # end if(banks[i] == "GBa")
      # Here I'm adding in the cf for August into the spring survey data.
      
      if(banks[i] == "GB")
      {
        stdts.plt(survey.obj[[banks[i]]][[1]],x=c('year'),y=c('CF'),pch=22,ylab=cf.lab,col="red",lty=2,
                  median.line=T,graphic='none',xlab='Year',ylim=c(4,25),titl = CF.ts.title,cex.mn=cap.size,las=1)
        if(season=="both")
        {
          points(survey.obj[["GBa"]][[1]]$year+0.25,survey.obj[["GBa"]][[1]]$CF,col="blue", lty=1, pch=16,type="o")
          abline(h=mean(survey.obj[["GBa"]][[1]]$CF,na.rm=T),col="blue",lty=3)
        }
        
        if(season=="spring"|season=="testing")
        {
          # Added in case R is treating year as a factor... 
          if(is.factor(survey.obj.last[["GBa"]][[1]]$year)) 
          {  
            survey.obj.last[["GBa"]][[1]]$year <- as.numeric(levels(survey.obj.last[["GBa"]][[1]]$year))[survey.obj.last[["GBa"]][[1]]$year]
          } # end if(is.factor(survey.obj.last[["GBa"]][[1]]$year)) 
          points(survey.obj.last[["GBa"]][[1]]$year+0.25,survey.obj.last[["GBa"]][[1]]$CF,col="blue", lty=1, pch=16,type="o")
          lines(y=rep(median(survey.obj.last[["GBa"]][[1]]$CF,na.rm=T), length(survey.obj.last[["GBa"]][[1]]$year)-1), 
                x = survey.obj.last[["GBa"]][[1]]$year[-length(survey.obj.last[["GBa"]][[1]]$year)],col="blue",lty=3)
        } # end  if(season=="spring")
        legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,22),bty='n',inset=0.02,col=c("blue","red"),pt.bg=c("blue","red"))	
        
      } # end if(banks[i] == "GB")
      #legend('bottomleft',c("August","May"),lty=1:2,pch=c(16,NA),bty='n',inset=0.02,col=c("blue","red"))		
      if(fig != "screen") dev.off()
      
      if(banks[i] == "GBa" & sub.area==T) {
        
        if(fig == "screen") windows(8.5,8.5)
        if(fig == "png") png(paste(plot.dir,"/CF_ts_subarea_NS.png",sep=""),
                             units="in",width = 8.5,height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"/CF_ts_subarea_NS.pdf",sep=""),width = 8.5,height = 8.5)
        
        print(
          ggplot() + geom_point(data=subarea_df[subarea_df$subarea %in% c("GBa-North", "GBa-South"),], aes(year, CF, colour=subarea, shape=subarea), size=3) +
            geom_line(data=subarea_df[subarea_df$subarea %in% c("GBa-North", "GBa-South"),], aes(year, CF, colour=subarea), size=1) +
            #geom_smooth(data=subarea_df, aes(year, CF, colour=subarea), method="gam", se=F) +
            theme_bw() + 
            theme(panel.grid=element_blank(), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), text = element_text(size=16), legend.position = c(0.85, 0.15)) +
            ggtitle("Condition factor time series (GBa subareas)") +
            scale_colour_manual(values=c(surv.info$col[surv.info$Strata_ID==4], surv.info$col[surv.info$Strata_ID==7]), name=NULL)+
            scale_shape_discrete(name=NULL)+
            theme(axis.title.y = element_text(angle=360, vjust=0.5))+
            xlab("Year")+
            scale_y_continuous(limits=c(4, 25), breaks=seq(5,25,2.5), labels=c(5, " ", 10, " ", 15, " ", 20, " ", 25), name=cf.lab) +
            scale_x_continuous(breaks=seq(1985,2020,5), labels=c(" ", 1990, " ", 2000, " ", 2010, " ", 2020))
        )
        
        if(fig != "screen") dev.off()
        
        if(fig == "screen") windows(8.5,8.5)
        if(fig == "png") png(paste(plot.dir,"/CF_ts_subarea_gam_NS.png",sep=""),
                             units="in",width = 8.5,height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"/CF_ts_subarea_gam_NS.pdf",sep=""),width = 8.5,height = 8.5)
        
        print(
          ggplot() + geom_point(data=subarea_df[subarea_df$subarea %in% c("GBa-North", "GBa-South"),], aes(year, CF, colour=subarea, shape=subarea), size=1) +
            #geom_line(data=subarea_df, aes(year, CF, colour=subarea), size=1) +
            geom_smooth(data=subarea_df[subarea_df$subarea %in% c("GBa-North", "GBa-South"),], aes(year, CF, colour=subarea), method="gam", se=F) +
            theme_bw() + 
            theme(panel.grid=element_blank(), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), text = element_text(size=16), legend.position = c(0.85, 0.15)) +
            ggtitle("Condition factor time series (GBa subareas)") +
            scale_colour_manual(values=c(surv.info$col[surv.info$Strata_ID==4], surv.info$col[surv.info$Strata_ID==7]), name=NULL)+
            scale_shape_discrete(name=NULL)+
            theme(axis.title.y = element_text(angle=360, vjust=0.5))+
            xlab("Year")+
            scale_y_continuous(limits=c(4, 25), breaks=seq(5,25,2.5), labels=c(5, " ", 10, " ", 15, " ", 20, " ", 25), name=cf.lab) +
            scale_x_continuous(breaks=seq(1985,2020,5), labels=c(" ", 1990, " ", 2000, " ", 2010, " ", 2020)) 
        )
        if(fig != "screen") dev.off()
        
        if(fig == "screen") windows(8.5,8.5)
        if(fig == "png") png(paste(plot.dir,"/CF_ts_subarea_northareas.png",sep=""),
                             units="in",width = 8.5,height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"/CF_ts_subarea_northareas.pdf",sep=""),width = 8.5,height = 8.5)
        
        print(
          ggplot() + geom_point(data=subarea_df[subarea_df$subarea %in% c("GBa-West", "GBa-Central", "GBa-East"),], aes(year, CF, colour=subarea, shape=subarea), size=3) +
            geom_line(data=subarea_df[subarea_df$subarea %in% c("GBa-West", "GBa-Central", "GBa-East"),], aes(year, CF, colour=subarea), size=1) +
            #geom_smooth(data=subarea_df, aes(year, CF, colour=subarea), method="gam", se=F) +
            theme_bw() + 
            theme(panel.grid=element_blank(), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), text = element_text(size=16), legend.position = c(0.85, 0.15)) +
            ggtitle("Condition factor time series (GBa North sub-areas)") +
            scale_colour_manual(values=c(surv.info$col[surv.info$Strata_ID%in% c(2,3,4)]), name=NULL)+
            scale_shape_discrete(name=NULL)+
            theme(axis.title.y = element_text(angle=360, vjust=0.5))+
            xlab("Year")+
            scale_y_continuous(limits=c(4, 25), breaks=seq(5,25,2.5), labels=c(5, " ", 10, " ", 15, " ", 20, " ", 25), name=cf.lab) +
            scale_x_continuous(breaks=seq(1985,2020,5), labels=c(" ", 1990, " ", 2000, " ", 2010, " ", 2020))
        )
        
        if(fig != "screen") dev.off()
        
        if(fig == "screen") windows(8.5,8.5)
        if(fig == "png") png(paste(plot.dir,"/CF_ts_subarea_gam_northareas.png",sep=""),
                             units="in",width = 8.5,height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"/CF_ts_subarea_gam_northareas.pdf",sep=""),width = 8.5,height = 8.5)
        
        print(
          ggplot() + geom_point(data=subarea_df[subarea_df$subarea %in% c("GBa-West", "GBa-Central", "GBa-East"),], aes(year, CF, colour=subarea, shape=subarea), size=1) +
            #geom_line(data=subarea_df, aes(year, CF, colour=subarea), size=1) +
            geom_smooth(data=subarea_df[subarea_df$subarea %in% c("GBa-West", "GBa-Central", "GBa-East"),], aes(year, CF, colour=subarea), method="gam", se=F) +
            theme_bw() + 
            theme(panel.grid=element_blank(), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), text = element_text(size=16), legend.position = c(0.85, 0.15)) +
            ggtitle("Condition factor time series (GBa North sub-areas)") +
            scale_colour_manual(values=c(surv.info$col[surv.info$Strata_ID%in% c(2,3,4)]), name=NULL)+
            scale_shape_discrete(name=NULL)+
            theme(axis.title.y = element_text(angle=360, vjust=0.5))+
            xlab("Year")+
            scale_y_continuous(limits=c(4, 25), breaks=seq(5,25,2.5), labels=c(5, " ", 10, " ", 15, " ", 20, " ", 25), name=cf.lab) +
            scale_x_continuous(breaks=seq(1985,2020,5), labels=c(" ", 1990, " ", 2000, " ", 2010, " ", 2020)) 
        )
        if(fig != "screen") dev.off()
      }
      
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
                  areas=surv.info$towable_area[surv.info$startyear<2023],clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                  add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
      }# end if(banks[i] != "Ger" && banks[i] != "Mid")
      # For german bank
      if(banks[i] == "Ger")
      {
        survey.ts(data.frame(merged.survey.obj, CS=105, RS=95), min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,
                  ymin=-5,dat2=survey.obj[[banks[i]]][[1]],clr=c('red','blue', "red"),pch=c(17,16),se=T,
                  add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5, yl2=c(400, 300, 300))
        
        legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.4,lty=c(2,1),col=c("blue","red"),bty="n")
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
          legend("topright", inset=c(0.05, -0.9), xpd=NA, c("After restratification","Prior to restratification"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.4,lty=c(1,2),col=c("blue","red"),bty="n")
        }
        
      } # end if(banks[i] == "Sab")
      
      
      
      
      if(fig != "screen") dev.off()
      
      if(banks[i] == "GBa" & sub.area==T) {
        # TO RE-CREATE PREVIOUS YEAR WITH SAME Y AXIS USE:
        # subarea_bars_facet_fix.R
        
        if(fig == "screen") windows(8.5, 11)
        if(fig == "png")png(paste(plot.dir,"/abundance_bars.png",sep=""),units="in",
                            width = 8.5, height = 11,res=100,bg="transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"/abundance_bars.pdf",sep=""),width = 8.5, height = 11)
        
        subarea_LTM <- melt(subarea_df[!subarea_df$year==yr, c("subarea", "N", "NPR", "NR")], id.vars="subarea")
        subarea_LTM <- aggregate(data = subarea_LTM, value ~ subarea + variable, FUN = median)
        subarea_abund <- melt(subarea_df[subarea_df$year==yr, c("subarea", "N", "NPR", "NR")], id.vars="subarea")
        subarea_abund_cv <- melt(subarea_df[subarea_df$year==yr, c("subarea", "N.cv", "NPR.cv", "NR.cv")], id.vars="subarea", variable.name = "CV")
        subarea_abund_cv$variable <- gsub(x=subarea_abund_cv$CV, ".cv", "")
        subarea_abund_cv$CV <- subarea_abund_cv$value
        subarea_abund <- join(subarea_abund, subarea_abund_cv[,c("subarea","variable", "CV")], type="full")
        subarea_abund$variable <- factor(subarea_abund$variable, levels=c("NPR", "NR", "N"))
        levels(subarea_abund$variable) <- c("Pre-recruits", "Recruits", "Fully-recruited")
        subarea_abund$subarea <- factor(subarea_abund$subarea, levels = c("GBa-North", "GBa-South", "GBa-West", "GBa-Central", "GBa-East"))
        levels(subarea_abund$subarea) <- c("North", "South", "North-West", "North-Central", "North-East")
        subarea_LTM$variable <- factor(subarea_LTM$variable, levels=c("NPR", "NR", "N"))
        levels(subarea_LTM$variable) <- c("Pre-recruits", "Recruits", "Fully-recruited")
        subarea_LTM$subarea <- factor(subarea_LTM$subarea, levels = c("GBa-North", "GBa-South", "GBa-West", "GBa-Central", "GBa-East"))
        levels(subarea_LTM$subarea) <- c("North", "South", "North-West", "North-Central", "North-East")
        
        print(
          ggplot() + geom_point(data=subarea_abund, aes(subarea, value, colour=subarea), size=3) + 
            geom_point(data=subarea_LTM, aes(subarea, value), shape="-", size=10) + 
            geom_text(data=subarea_LTM, aes(subarea, value), label="   LTM", size=3, hjust=0) + 
            geom_errorbar(data=subarea_abund, aes(subarea, ymin=value-(value*CV), ymax=value+(value*CV),colour=subarea), width=0.1) +
            theme_bw() + theme(panel.grid=element_blank(), plot.background = element_rect(fill="transparent", colour=NA), axis.title.y = element_text(angle=360, vjust=0.5), text = element_text(size=16), plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
            ylab(expression(frac("N","tow"), "\n")) +
            xlab(NULL) +
            ggtitle(paste0(yr, " Survey Abundance by GBa Sub-area")) +
            facet_wrap(~variable, nrow=3, scales="free_y") +
            theme(strip.text = element_blank()) + 
            scale_y_continuous(expand=expansion(mult=c(0.1, 0.25), add=0)) +
            geom_text(data=subarea_abund, aes(x=0.5, y=Inf, label=variable), vjust=2, hjust=0, size=5)+
            geom_vline(xintercept = 2.5, linetype="dashed") +
            scale_colour_manual(guide="none", values=c(surv.info$col[surv.info$Strata_ID==4], surv.info$col[surv.info$Strata_ID==7], rep(surv.info$col[surv.info$Strata_ID==4], 3)))
        )
        if(fig != "screen") dev.off()
      }
      
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
                  areas=surv.info$towable_area[surv.info$startyear<2023],clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                  add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
      } # end if(banks[i] != "Ger")
      if(banks[i] == "Ger")
      {
        survey.ts(data.frame(merged.survey.obj, CS=105, RS=95), min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,type='B',
                  dat2=survey.obj[[banks[i]]][[1]],clr=c('red','blue', "red"),pch=c(17,16),se=T,
                  add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5, yl2=c(3000,3000,6000))
        
        legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.4,lty=c(2,1),col=c("blue","red"),bty="n")
        
        message("WARNING: This figure should be edited to hide 1999 estimate since the only MWSH data collected in 1999 was commercial samples in July. We do not show a CF value for 1999 in the MWSH figure because of this, so this should match that, but currently does not!")
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
                 pch=c(23,24),pt.bg = c("blue","red"),cex=1.4,lty=c(1,2),col=c("blue","red"),bty="n")
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
      
      if(banks[i] == "GBa" & sub.area==T){
        # TO RE-CREATE PREVIOUS YEAR WITH SAME Y AXIS USE:
        # subarea_bars_facet_fix.R
        
        if(fig == "screen") windows(8.5, 11)
        if(fig == "png")png(paste(plot.dir,"/biomass_bars.png",sep=""),units="in",
                            width = 8.5, height = 11,res=420,bg="transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"/biomass_bars.pdf",sep=""),width = 8.5, height = 11)
        subarea_LTM <- melt(subarea_df[!subarea_df$year==yr, c("subarea", "I", "IPR", "IR")], id.vars="subarea")
        subarea_LTM <- aggregate(data = subarea_LTM, value ~ subarea + variable, FUN = median)
        subarea_biomass <- melt(subarea_df[subarea_df$year==yr, c("subarea", "I", "IPR", "IR")], id.vars="subarea")
        subarea_biomass_cv <- melt(subarea_df[subarea_df$year==yr, c("subarea", "I.cv", "IPR.cv", "IR.cv")], id.vars="subarea", variable.name = "CV")
        subarea_biomass_cv$variable <- gsub(x=subarea_biomass_cv$CV, ".cv", "")
        subarea_biomass_cv$CV <- subarea_biomass_cv$value
        subarea_biomass <- join(subarea_biomass, subarea_biomass_cv[,c("subarea","variable", "CV")], type="full")
        subarea_biomass$variable <- factor(subarea_biomass$variable, levels=c("IPR", "IR", "I"))
        levels(subarea_biomass$variable) <- c("Pre-recruits", "Recruits", "Fully-recruited")
        subarea_biomass$subarea <- factor(subarea_biomass$subarea, levels = c("GBa-North", "GBa-South", "GBa-West", "GBa-Central", "GBa-East"))
        levels(subarea_biomass$subarea) <- c("North", "South", "North-West", "North-Central", "North-East")
        subarea_LTM$variable <- factor(subarea_LTM$variable, levels=c("IPR", "IR", "I"))
        levels(subarea_LTM$variable) <- c("Pre-recruits", "Recruits", "Fully-recruited")
        subarea_LTM$subarea <- factor(subarea_LTM$subarea, levels = c("GBa-North", "GBa-South", "GBa-West", "GBa-Central", "GBa-East"))
        levels(subarea_LTM$subarea) <- c("North", "South", "North-West", "North-Central", "North-East")
   
        print(
          ggplot() + geom_point(data=subarea_biomass, aes(subarea, value, colour=subarea), size=3) + 
            geom_point(data=subarea_LTM, aes(subarea, value), shape="-", size=10) + 
            geom_text(data=subarea_LTM, aes(subarea, value), label="   LTM", size=3, hjust=0) + 
            geom_errorbar(data=subarea_biomass, aes(subarea, ymin=value-(value*CV), ymax=value+(value*CV),colour=subarea), width=0.1) +
            theme_bw() + theme(panel.grid=element_blank(), plot.background = element_rect(fill="transparent", colour=NA), axis.title.y = element_text(angle=360, vjust=0.5), text = element_text(size=16), plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
            ylab(expression(frac("g","tow"), "\n")) +
            xlab(NULL) +
            ggtitle(paste0(yr, " Survey Biomass by GBa Sub-area")) +
            facet_wrap(~variable, nrow=3, scales="free_y") +
            theme(strip.text = element_blank()) + 
            scale_y_continuous(expand=expand_scale(mult=c(0.1, 0.25), add=0)) +
            geom_text(data=subarea_biomass, aes(x=0.5, y=Inf, label=variable), vjust=2, hjust=0, size=5)+
            geom_vline(xintercept = 2.5, linetype="dashed") +
            scale_colour_manual(guide=F, values=c(surv.info$col[surv.info$Strata_ID==4], surv.info$col[surv.info$Strata_ID==7], rep(surv.info$col[surv.info$Strata_ID==4], 3)))
          
        )
        if(fig != "screen") dev.off()
      }
      
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
          shf.years <- survey.obj[[banks[i]]][[1]]$year[!is.na(survey.obj[[banks[i]]][[1]]$n) & (yr - survey.obj[[banks[i]]][[1]]$year) <20]
          s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
          numrows <- length(s.size[!is.na(s.size)])
          shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
                  recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T, rows=numrows)	
          if(fig != "screen") dev.off()
        }
      } # end  if(banks[i] != "Ger")
      
      if(banks[i]=="Ger")
      {
        shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
        s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
        shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
                recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = T, ymax=30)	
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
        
        shf.plt(matched.survey.obj,from='surv',yr=matched.survey.obj[[1]]$year,col1='grey80',col2=1,rel=F,rows=2,
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
        survey.ts(clap.survey.obj[[banks[i]]][[1]],
                  Bank=bank[i],pdf=F, years=yrs, axis.cx = 1.5, type="N",
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
      
      if(length(unique(surv.Clap.Rand[[banks[i]]]$year)) > 3) want.ltm <- T
      if(length(unique(surv.Clap.Rand[[banks[i]]]$year)) < 4) want.ltm <- F
      
      if(!banks[[i]]=="Ger") {
        yrs <- min(surv.Clap.Rand[[banks[i]]]$year,na.rm=T):max(surv.Clap.Rand[[banks[i]]]$year,na.rm=T)
        Clap3.plt(surv.Clap.Rand[[banks[i]]],years=yrs,add.title = T,cex.mn = 3, median.line=want.ltm,
                  titl = clap.per.ts.title,
                  CS=survey.obj[[banks[i]]][[1]]$CS[survey.obj[[banks[i]]][[1]]$year==yr], RS=survey.obj[[banks[i]]][[1]]$RS[survey.obj[[banks[i]]][[1]]$year==yr],
                  axis.cx = 1.5)
      }
      if(banks[[i]]=="Ger") {
        yrs <- min(surv.Clap.Rand[[banks[i]]]$year,na.rm=T):max(surv.Clap.Rand[[banks[i]]]$year,na.rm=T)
        Clap3.plt(surv.Clap.Rand[[banks[i]]],years=yrs,add.title = T,cex.mn = 3, median.line=want.ltm,
                  titl = clap.per.ts.title,
                  CS=lined.survey.obj[[1]]$CS[lined.survey.obj[[1]]$year==yr], RS=lined.survey.obj[[1]]$RS[lined.survey.obj[[1]]$year==yr],
                  axis.cx = 1.5)
      }
      # if(banks[[i]]=="BanIce") {
      #   yrs <- 2012
      #   message("using surv.Clap instead of surv.Clap.Rand for BanIce")
      #   Clap3.plt(surv.Clap[[banks[i]]],years=yrs,add.title = T,cex.mn = 3, mean.line=T,
      #             titl = clap.per.ts.title,
      #             CS=unique(survey.obj[[banks[i]]][[1]]$CS),RS=unique(survey.obj[[banks[i]]][[1]]$RS),
      #             axis.cx = 1.5)
      #   }
      print(banks[i])
      
      if(want.ltm==T) {
        print(paste0(c("ClapPropLTMpre = ",
                       "ClapPropLTMrec = ",
                       "ClapPropLTMcom = "), round(clap.propLTMs, 3)))
      }
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
    
    if(any(plots== "breakdown"))
    {
      # This only works for the banks we have thse data for...
      #if(banks[i] %in% c("BBn" , "GBb", "GBa","GB"))
      if(fig == "screen") windows(8.5,11)
      if(fig == "png") png(paste(plot.dir,"breakdown-",(yr),".png",sep=""),units="in",
                           width = 8.5,height = 11,res=420,bg = "transparent")
      if(fig == "pdf") pdf(paste(plot.dir,"breakdown-",(yr),".pdf",sep=""),width = 8.5,height = 11)
      if(add.title ==T) title.txt <- paste("Biomass & Meat Count by Height (",banks[i],"-",yr,")",sep="")
      if(banks[i] != "GB") mc <- subset(fish.reg, year == yr & Bank %in% gsub(x=banks[i], "Ice", ""))$MC_reg
      if(banks[i] %in% spat.name) mc <- subset(fish.reg, year == yr & Bank %in% unique(spat.names$bank[spat.names$label == banks[i]]))$MC_reg
      if(banks[i] == "GB") mc <- fish.reg$MC_reg[fish.reg$Bank == "GBa"]
      if("years" %in% colnames(survey.obj[[banks[i]]]$shf.dat$w.yst)) {
        survey.obj[[banks[i]]]$shf.dat$w.yst <- survey.obj[[banks[i]]]$shf.dat$w.yst[,-which(colnames(survey.obj[[banks[i]]]$shf.dat$w.yst) == "years")]
      }
      if("years" %in% colnames(survey.obj[[banks[i]]]$shf.dat$n.yst)) {
        survey.obj[[banks[i]]]$shf.dat$n.yst <- survey.obj[[banks[i]]]$shf.dat$n.yst[,-which(colnames(survey.obj[[banks[i]]]$shf.dat$n.yst) == "years")]
      }
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
        bmmc.last<-survey.obj[[banks[i]]]$shf.dat$w.yst[which(survey.obj[[banks[i]]][[1]]$year==last.surv.year),which(seq(5,200,5) >= 5)]/1000
        nummc.last<-survey.obj[[banks[i]]]$shf.dat$n.yst[which(survey.obj[[banks[i]]][[1]]$year==last.surv.year),which(seq(5,200,5) >= 5)]
        countmc.last=nummc.last/bmmc.last*0.5
        y2max.last<-max(countmc.last[(min(c(RS-15,160),na.rm=T)/5):length(vec)],na.rm=T)*1.1
        y2max <- max(c(max(y2max,na.rm=T)*1.1),max(y2max.last,na.rm=T)*1.1)
        breakdown(survey.obj[[banks[i]]],yr=yr,mc=mc,y1max = ymax, y2max=y2max, add.title = T, title.txt=title.txt)
      } # end if(banks[i] != "Ger") 
      
      # Using the lined surevye object for German bank...
      if(banks[i] == "Ger") 
      {
        if("years" %in% colnames(lined.survey.obj$shf.dat$w.yst)) {
          lined.survey.obj$shf.dat$w.yst <- lined.survey.obj$shf.dat$w.yst[,-which(colnames(lined.survey.obj$shf.dat$w.yst) == "years")]
        }
        if("years" %in% colnames(lined.survey.obj$shf.dat$n.yst)) {
          lined.survey.obj$shf.dat$n.yst <- lined.survey.obj$shf.dat$n.yst[,-which(colnames(lined.survey.obj$shf.dat$n.yst) == "years")]
        }
        # This will make the breakdown figure for the previous year in which there was a survey (typically last year but not always...)
        # This is based on the current year being requested (which could differ from the last year in the data if you are saying using the 2018 survey results
        # but wanted to look at the 2015 data for example).
        current.year <- which(na.omit(lined.survey.obj$model.dat)$year == yr)
        last.surv.year <- na.omit(lined.survey.obj$model.dat)$year[current.year-1]
        # Get the data...
        bm<-lined.survey.obj$shf.dat$w.yst[which(lined.survey.obj[[1]]$year==yr),which(seq(5,200,5) >= 5)]/1000
        bm.last<-lined.survey.obj$shf.dat$w.yst[which(lined.survey.obj[[1]]$year==last.surv.year),which(seq(5,200,5) >= 5)]/1000
        ymax <- max(c(max(bm,na.rm=T)*1.1),max(bm.last,na.rm=T)*1.1)
        breakdown(lined.survey.obj,yr=yr,mc=mc,y1max = ymax,add.title = T, title.txt=title.txt)
      }# end if(banks[i] == "Ger") 
      
      if(fig != "screen") dev.off()   
      
      # I also want to remake the previsou year's breakdown plot, this will go in the current years folder but will
      # be the same y-scale (there is no guarantee that the plot made last year will be, likely it won't).  It's a bit
      # clunky but basically this is the same plot as last year but re-scaled for comparative purposes...
      if(any(!is.na(bm.last))==T) {
        if(fig == "screen") windows(8.5,11)
        if(fig == "png") png(paste(plot.dir,"breakdown-",last.surv.year,".png",sep=""),units="in",
                             width = 8.5,height = 11,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,"breakdown-",last.surv.year,".pdf",sep=""),width = 8.5,height = 11)
        if(add.title ==T) title.txt <- paste("Biomass & Meat Count by Height (",banks[i],"-",last.surv.year,")",sep="")
        if(banks[i] != "Ger")
        {
          # To get the ymax the same between succesive years I want to do this...
          breakdown(survey.obj[[banks[i]]],yr=last.surv.year,mc=mc,y1max = ymax, y2max=y2max, add.title = T, title.txt=title.txt)
          
        } # end if(banks[i] != "Ger") 
        
        if(banks[i] == "Ger") 
        {
          breakdown(lined.survey.obj,yr=last.surv.year,mc=mc,y1max = ymax,add.title = T, title.txt=title.txt)
        }# end if(banks[i] == "Ger") 
        
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
          if(!(banks[i] == "Sab" && this.box$Common_name == "Starbox") & !yr==2021) yrsago <- 1
          if(banks[i] == "Sab" && this.box$Common_name == "Starbox"| yr==2021) yrsago <- 2
          
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
            breakdown(boxy,yr=yr,mc=mc,y1max = ymax, y2max=y2max, add.title = T, title.txt=breakdown.title.seed)
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
              breakdown(boxy,yr=(yr-yrsago),mc=mc,y1max = ymax, y2max=y2max, add.title = T, title.txt=breakdown.title.seed)
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
