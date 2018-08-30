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
#                           in the SurveySummary_data.r function call will be plotted.
# 2:  banks:    The banks to create figures for.  Option are "BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB" 
#                (note Banquereau is not supported yet and the GB is Georges Bank Spring
# 3:  direct:   The working directory to put figures are from which to grab data.  Default = "Y:/Offshore scallop/Assessment/", 
# 4:  yr:       The survey year of interest.  Default = as.numeric(format(Sys.time(), "%Y")) which attempts to produce plots for the current year.
#               NB: If trying to reproduce figures from previous years make sure the Rdata output from SurverySummary_data.r for that year exists!
# 5:  add.title:Add titles to the figures; T/F.  Default = T
# 6:  fig:      Plot the figures to "screen", to a "pdf" or a "png" file. If fig = "pdf" or "png" figures are placed in this directory (which must exist!!) 
#               (direct/yr/Presentations/Survey_summary/bank - e.g.Y:/Offshore scallop/Assessment/2015/Presentations/Survey_summary/GBa/)
#               unless season == "testing" in which they get saved to a testing folder (see below for exact directory).
# 7:  season:   For the spring survey we need to identify that we don't have all the results in yet.  When running the scripts with only spring  
#               survey data set to "spring".  If just running GBa and GBb you can set this to "summer" if you've already created the Rdata file.
#               When summer survey is complete you can also set this to the default of "both".  I've also added the option during preliminary
#               runs or when altering the function to use a "testing" option so it loads the proper data.  You'll need to have created the 
#               testing R data when running the SurveySummary_data.r.  Additionally when set to "testing" the figures (if being saved) will be
#               saved in the folder... direct,yr,"/Presentations/Survey_summary/test_figures/",banks[i].
###############################################################################################################

survey.figs <- function(plots = c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial","Survey","MW-SH",
                                  "abund-ts","biomass-ts","SHF","SHF-large","SHF-split",
                                  "clapper-abund-ts","clapper-per-ts","SH-MW-CF-ts","breakdown","seedboxes","user.SH.bins"),
                       banks = c("BBn" ,"BBs", "Ger", "Mid", "Sab", "GBb", "GBa","GB"),
                       direct = "Y:/Offshore scallop/Assessment/", yr = as.numeric(format(Sys.time(), "%Y"))  ,
                       add.title = T,fig="screen",season="both")
{ 
  tmp.dir <- direct ; tmp.season <- season # I need this so that the directory isn't overwritten when I load the below...
  # Load the appropriate data.
  if(season == "testing") 
  {
    if(file.exists(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/testing_results.Rdata",sep=""))==T) 
    {                   
       load(paste(direct,"Data/Survey_data/",yr,
                                     "/Survey_summary_output/testing_results.Rdata",sep=""))  
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
    } else stop("Please re-run Survey_Summary_script and set it so that the file 'Survey_summer_results.Rdata' gets created, Thanks eh!!") # end if/else file.
  }# if(season == "summer")
  
  direct <- tmp.dir # I need this so that the directory isn't overwritten when I load the above
  
  # These are the functions used to within the heart of the code to make stuff happen
  source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/stdts.plt.R",sep="")) 
  source(paste(direct,"Assessment_fns/Survey_and_OSAC/survey.ts_bin_option.r",sep=""),local=T) 
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
mod.res <- NULL # This is the model response variable value at each node in the mesh.
pre.proj <- NULL # This is the projection of the values for each model onto the mesh, this is needed for plotting.
# Set up objects for plot labels.  
N.tow.lab <- expression(frac(N,tow))
B.tow.lab <- expression(frac(kg,tow))
cf.lab <-    expression(paste("CF:",bgroup("(",frac(g,dm^3)   ,")")))
mc.lab <-    expression(paste("MC:",bgroup("(",frac(N,"500 g"),")"))) 

# Clean up any open plot devices.
if(!is.null(dev.list())) dev.off()
# Run through all of the banks of interest.
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

  # Grab any seedboxes of interest...
  # I'm picking November 1st of current year b/c if they close a box at this point none of our presentations
  # will have information about it.  The only reason I'm putting this closed bit in is for cases in which
  # I am making plots from previous years, so a box closed in Nov or December never would have been included in one of our
  # presentations (maybe OSAC, but this isn't OSAC)....
  sb <- subset(seedboxes,Bank == banks[i] & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep=""))
  if(banks[i] == "GB")  sb <- subset(seedboxes,Bank %in% c("GBa","GBb") & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep=""))

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
  
  ################################# START MAKING FIGURES################################# START MAKING FIGURES################################# 
  
################  The non-survey spatial plots ###########################
################  Next up are the rest of the spatial plots ###########################
################  Next up are the rest of the spatial plots ###########################
 ############  Now start getting the spatial data for this year for the specific bank.
  
  # If we are making the seedbox or any of the spatial plots we need to set up our mesh...
  # We need to make a mesh for the plots from which we had data from all tows
  # and a mesh for the plots from which we only had data for select tows (i.e. condition factor)
  spatial.maps <- plots[grep("spatial",plots)]
  # If we want spatial maps or seedboxes and/or have user SH.bins (for both of which we will produce all figures automatically.)
  if(length(spatial.maps > 0) || any(plots %in% c("seedboxes","user.SH.bins")))
  {
    if(banks[i] != "Ger" & banks[i] != "Mid") bound.poly.surv <- as.PolySet(survey.bound.polys[survey.bound.polys$label==banks[i],],projection="LL")
    
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
      g.tmp$X[g.tmp$X < X.range[1]] <- X.range[1]
      g.tmp$X[g.tmp$X > X.range[2]] <- X.range[2]
      g.tmp$Y[g.tmp$Y > Y.range[2]] <- Y.range[2]
      g.tmp$Y[g.tmp$Y < Y.range[1]] <- Y.range[1]
      # Now I want to insert a segemnt into the boundary to run a diagonal line from around 43°9/-66.755 to 43/-66°24
      g.tmp[2,] <- c(5,2,-66.4,Y.range[1],"SFA26","Ger")
      g.tmp <- as.data.frame(rbind(g.tmp[c(1,2),],c(5,2,X.range[1],43.15,"SFA26","Ger"),g.tmp[3:nrow(g.tmp),]))
      for(k in 1:4) g.tmp[,k] <- as.numeric(g.tmp[,k]) # Silly rbind making everything characters...
      g.tmp$POS <- 1:nrow(g.tmp)
      bound.poly.surv <- as.PolySet(g.tmp,projection="LL")
    } # end if(banks[i] == "Ger") 
      # Now convert this to an object for sp and set this up and the boundary polygon for the mesh.
    bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)
    loc <- cbind(surv.Live[[banks[i]]]$lon[surv.Live[[banks[i]]]$year == yr],
                 surv.Live[[banks[i]]]$lat[surv.Live[[banks[i]]]$year == yr])
    # These are the locations for the Condition and meat count data.
    loc.cf <- cbind(CF.current[[banks[i]]]$lon[CF.current[[banks[i]]]$year == yr],
                    CF.current[[banks[i]]]$lat[CF.current[[banks[i]]]$year == yr])
    # Build the mesh, for our purposes I'm hopeful this should do the trick.
    mesh <- inla.mesh.2d(loc, max.edge=c(0.1), cutoff=0.001,
                         boundary = inla.sp2segment(bound.poly.surv.sp))
    # plot(mesh)
    #points(loc,pch=21,bg="blue")
    mesh.cf <- inla.mesh.2d(loc.cf, max.edge=c(0.1), cutoff=0.001,
                            boundary = inla.sp2segment(bound.poly.surv.sp)) # This may be rather fine but this isn't very coputationally expensive
    #plot(mesh.cf)
    A <- inla.spde.make.A(mesh, loc)
    A.cf <- inla.spde.make.A(mesh.cf, loc.cf)
    ## All of our spatial plots are counts, so for our simple purposes a poisson is o.k.
    family1 = "poisson"
    family1.cf <- "gaussian" # For CF and MC they are more normal so go with a gaussian.
    family.clap <- "poisson" # I haven't found a good family for the clapper data, for the moment the poisson does a decent job as long
    # as we don't have very high clapper values (i.e. near 100%), it can get weird there, but I can't find a better likelihood yet...
    # but I need to truncate the predicted values that are > 100 to be 100 which is BS...
    #control.family1 <- list(control.link = list(model="log"))
    # We can just make the one spde object for all of these as well.
    spde <- inla.spde2.pcmatern(mesh,    
                                prior.sigma=c(1,0.5),
                                prior.range=c(1,0.5))
    # I've increased the range for the Condition data since there is less of it...
    spde.cf <- inla.spde2.pcmatern(mesh.cf,    
                                prior.sigma=c(3,0.8),
                                prior.range=c(1,0.5))
    # As soon as you make a spatial model make your own intercept.  Here is
    a0 <- 1 # intercept
    # Mostly just using stock priors, again fine for our purposes for the moment.
    pcprec <- list(prior='pc.prec', param=c(0.5, 0.01))
    # Add an index to the data
    #surv.Live[[banks[i]]]$i[surv.Live[[banks[i]]]$year == yr] <- 1:nrow(surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,])
    #CF.current[[banks[i]]]$i[CF.current[[banks[i]]]$year == yr] <- 1:nrow(CF.current[[banks[i]]][CF.current[[banks[i]]]$year == yr,])
    # The spatial model, simple model with a intercept (overall bank average) with the spde spatial component
    # basically the random deviations for each piece of the mesh.
    formula3 <- y ~ 0 + a0 + f(s, model=spde)
    formula3.cf <- y ~ 0 + a0 + f(s, model=spde.cf)
  
    # If we are just getting the spatial maps for the seedboxes then we want all of these plots
    if(any(plots %in% "seedboxes")) seed.n.spatial.maps <- c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial")
    # If we just wanted select spatial maps, we'll make everything b/c we still need them all for the seedboxes
    if(length(spatial.maps) > 0 && any(plots %in% "seedboxes")) seed.n.spatial.maps <-  c("PR-spatial","Rec-spatial","FR-spatial",
                                                                                         "CF-spatial","MC-spatial","Clap-spatial")
    # If we don't want the seedboxes then we just do this for the plots we are interested in.
    if(length(grep("seedboxes",plots)) == 0) seed.n.spatial.maps <- spatial.maps
    # if we have some maps to make make the maps, if not move on with your life.
    if(length(seed.n.spatial.maps > 0))
    {
      for(k in 1:length(seed.n.spatial.maps))
      {
        # In the next bunch of if statements we run the INLA model and we get the figure titles sorted out.
        if(seed.n.spatial.maps[k] == "PR-spatial")    
        {
          # This is the stack for the INLA model
          stk <- inla.stack(tag="est",data=list(y = surv.Live[[banks[i]]]$pre[surv.Live[[banks[i]]]$year == yr], link=1L),
                          effects=list(a0 = rep(1, nrow(surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,])), s = 1:spde$n.spde),
                            A = list(1, A))
          # This is the INLA model itself
          mod <- inla(formula3, family=family1, data = inla.stack.data(stk),
                      control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
          # These are the results for each spatial mesh
          mod.res[[seed.n.spatial.maps[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
          
          # This projects our mesh
          proj = inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
          # this makes a big matrix with the values in the proper locations
          pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
        } # end if(seed.n.spatial.maps[k] == "PR-spatial")   
      
        if(seed.n.spatial.maps[k] == "Rec-spatial")        
        {
          # This is the stack for the INLA model
          stk <- inla.stack(tag="est",data=list(y = surv.Live[[banks[i]]]$rec[surv.Live[[banks[i]]]$year == yr], link=1L),
                            effects=list(a0 = rep(1, nrow(surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,])), s = 1:spde$n.spde),
                            A = list(1, A))
          # This is the INLA model itself
          mod <- inla(formula3, family=family1, data = inla.stack.data(stk),#control.family= control.family1,
                      control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
          # These are the results for each spatial mesh
          mod.res[[seed.n.spatial.maps[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
          
          # This projects our mesh
          proj = inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
          # this makes a big matrix with the values in the proper locations
          pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
  
        } # end if(seed.n.spatial.maps[k] == "Rec-spatial") 
        
        if(seed.n.spatial.maps[k] == "FR-spatial") 
        {
          # This is the stack for the INLA model
          stk <- inla.stack(tag="est",data=list(y = surv.Live[[banks[i]]]$com[surv.Live[[banks[i]]]$year == yr], link=1L),
                            effects=list(a0 = rep(1, nrow(surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,])), s = 1:spde$n.spde),
                            A = list(1, A))
          # This is the INLA model itself
          mod <- inla(formula3, family=family1, data = inla.stack.data(stk),#control.family= control.family1,
                      control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
          # These are the results for each spatial mesh
          mod.res[[seed.n.spatial.maps[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
          
          # This projects our mesh
          proj <- inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
          # this makes a big matrix with the values in the proper locations
          pre.proj[[seed.n.spatial.maps[k]]] <- inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
        } # end if(seed.n.spatial.maps[k] == "FR-spatial")
          
        
        if(seed.n.spatial.maps[k] == "CF-spatial")       
        {
          # This is the stack for the INLA model
          stk <- inla.stack(tag="est",data=list(y = CF.current[[banks[i]]]$CF, link=1L),
                            effects=list(a0 = rep(1, nrow(CF.current[[banks[i]]])), s = 1:spde.cf$n.spde),
                            A = list(1, A.cf))
          # This is the INLA model itself
          mod <- inla(formula3.cf, family=family1.cf, data = inla.stack.data(stk),
                      control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
          # These are the results for each spatial mesh
          mod.res[[seed.n.spatial.maps[k]]] <- mod$summary.random$s$mean+mod$summary.fixed$mean
          
          # This projects our mesh
          proj = inla.mesh.projector(mesh.cf,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
          # this makes a big matrix with the values in the proper locations
          pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
        }
        # THis seems to be making sense...
        if(seed.n.spatial.maps[k] == "MC-spatial")      
        {
          # This is the stack for the INLA model
          stk <- inla.stack(tag="est",data=list(y = CF.current[[banks[i]]]$meat.count, link=1L),
                            effects=list(a0 = rep(1, nrow(CF.current[[banks[i]]])), s = 1:spde.cf$n.spde),
                            A = list(1, A.cf))
          # This is the INLA model itself
          mod <- inla(formula3.cf, family=family1.cf, data = inla.stack.data(stk),
                      control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
          # These are the results for each spatial mesh
          mod.res[[seed.n.spatial.maps[k]]] <- mod$summary.random$s$mean+mod$summary.fixed$mean
          
          # This projects our mesh
          proj = inla.mesh.projector(mesh.cf,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
          # this makes a big matrix with the values in the proper locations
          pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
        } # end if(seed.n.spatial.maps[k] == "MC-spatial") 
        
        if(seed.n.spatial.maps[k] == "Clap-spatial")        
        {
          # This is the stack for the INLA model
          stk <- inla.stack(tag="est",data=list(y = surv.Clap[[banks[i]]]$clap.prop[surv.Clap[[banks[i]]]$year == yr], link=1L),
                            effects=list(a0 = rep(1, nrow(surv.Clap[[banks[i]]][surv.Clap[[banks[i]]]$year == yr,])), s = 1:spde$n.spde),
                            A = list(1, A))
          # This is the INLA model itself
          mod <- inla(formula3, family=family.clap, data = inla.stack.data(stk),
                      control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
          # These are the results for each spatial mesh
          mod.res[[seed.n.spatial.maps[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
          # This is a total hack for the moment, but b/c we're using a poisson model here it is possible to get values
          # > 100 which of course is BS.  Problem is I can't get any other likelihoods to give decent results.
          # For the rare case in which the poisson is predicting > 100 this seems to do the trick and give
          # resonalbe results for all banks.  So far I've only had this problem on BBs....
          mod.res[[seed.n.spatial.maps[k]]][mod.res[[seed.n.spatial.maps[k]]] > 100] <- 100
          # This projects our mesh
          proj = inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
          # this makes a big matrix with the values in the proper locations
          pre.proj[[seed.n.spatial.maps[k]]] = inla.mesh.project(proj, mod.res[[seed.n.spatial.maps[k]]])
        } # end if(seed.n.spatial.maps[k] == "Clap-spatial")  
      
      # End the loop for getting all the data needed for a bank for the spatial maps.
      } # end for(k in 1:length(seed.n.spatial.maps))
    } # end if(length(seed.n.spatial.maps > 0))
    # Now we need to get the projections if we have specified the User.SH.bins plots to be produced.
    bin.names <- NULL # Name bin.names a NULL, if no user.SH.bins we still need this name to exist...
    if(any(plots == "user.SH.bins"))
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
      
      #ranges.bm <- data.frame(bin = rep(NA,num.bins/2),min = rep(NA,num.bins/2),max=rep(NA,num.bins/2)) # I need this to make nice figures for the biomasses...
      #count = 0
      for(k in 1:num.bins)
      {
        # In the next bunch of if statements we run the INLA model and we get the figure titles sorted out.
        # This is the stack for the INLA model
        pick <- which(names(surv.Live[[banks[i]]]) == bin.names[k])
        stk <- inla.stack(tag="est",data=list(y = surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,pick], link=1L),
                          effects=list(a0 = rep(1, nrow(surv.Live[[banks[i]]][surv.Live[[banks[i]]]$year == yr,])), s = 1:spde$n.spde),
                          A = list(1, A))
        # This is the INLA model itself
        mod <- inla(formula3, family=family1, data = inla.stack.data(stk),
                    control.predictor=list(A=inla.stack.A(stk),link=link, compute=TRUE))
        # These are the results for each spatial mesh
        mod.res[[bin.names[k]]] <- exp(mod$summary.random$s$mean+mod$summary.fixed$mean)
        
        # This projects our mesh
        proj = inla.mesh.projector(mesh,xlim = range(bound.poly.surv$X),ylim =  range(bound.poly.surv$Y), dims = c(500, 500))
        # this makes a big matrix with the values in the proper locations
        pre.proj[[bin.names[k]]] = inla.mesh.project(proj, mod.res[[bin.names[k]]])
        # This gets us the ranges of biomass values, I just did this once to get a good sense of good levels.
        #if(length(grep("bm",bin.names[k])) >0) 
        #{
        #  count <- count + 1
        #  ranges.bm[count,] <- c(bin.names[k],range(pre.proj[[bin.names[k]]],na.rm=T)) 
        #} # end if(length(grep("bm",bin.names[k])) >0) 
      } # End for(k in 1:num.bins)
    }# end if(any(plots %in% "user.SH.bins"))
    
    
    
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
          base.lvls=c(0,5,10,50,100,500,1000,2000,5000,10000,20000,50000,1e6)
          cols <- c(rev(magma(length(base.lvls[base.lvls < 2000]),alpha=0.7,begin=0.6,end=1)),
                    rev(magma(length(base.lvls[base.lvls > 1000])-1,alpha=0.8,begin=0.1,end=0.5)))
          max.lvl <- which(base.lvls >= max(pre.proj[[maps.to.make[m]]],na.rm=T))[1]
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
          min.lvl <- max(which(base.lvls <= min(pre.proj[[maps.to.make[m]]],na.rm=T)))
          max.lvl <- min(which(base.lvls >= max(pre.proj[[maps.to.make[m]]],na.rm=T)))
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
          min.lvl <- max(which(base.lvls <= min(pre.proj[[maps.to.make[m]]],na.rm=T)))
          max.lvl <- min(which(base.lvls >= max(pre.proj[[maps.to.make[m]]],na.rm=T)))
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
          min.lvl <- max(which(base.lvls <= min(pre.proj[[maps.to.make[m]]],na.rm=T)))
          max.lvl <- min(which(base.lvls >= max(pre.proj[[maps.to.make[m]]],na.rm=T)))
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
        
        # Now for the user specified SH bins, if they exist...
        if(maps.to.make[m]  %in% bin.names) 
        {
          base.lvls=c(0,5,10,50,100,500,1000,2000,5000,10000,20000,50000,1e6)
          cols <- c(rev(magma(length(base.lvls[base.lvls < 2000]),alpha=0.7,begin=0.6,end=1)),
                    rev(magma(length(base.lvls[base.lvls > 1000])-1,alpha=0.8,begin=0.1,end=0.5)))
          if(length(grep("bm",maps.to.make[m])) > 0) # if we are looking at biomass figures...
          {
            base.lvls= c(0,0.005,0.01,0.05,0.1,0.5,1,2,5,10,20,50,1000)
            cols <- c(rev(plasma(length(base.lvls[base.lvls < 1]),alpha=0.7,begin=0.6,end=1)),
                      rev(plasma(length(base.lvls[base.lvls > 0.5])-1,alpha=0.8,begin=0.1,end=0.5)))
          } # end if(length(grep("bm",maps.to.make[m])) > 0) 
          
          max.lvl <- which(base.lvls >= max(pre.proj[[maps.to.make[m]]],na.rm=T))[1]
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
  
        ### Now we move to the figure....  
        # Do we want to save the figure to a file or just output to the screen?  
        if(fig == "png") png(paste(plot.dir,maps.to.make[m],".png",sep=""),units="in",width = 11,height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,maps.to.make[m],".pdf",sep=""),width = 11,height = 8.5,bg = "transparent")
        if(fig == "screen") windows(11,8.5)
        par(mfrow=c(1,1))
          
        # Where to get the bathymetry data, note this overwrites any call in the function above
        ifelse(banks[i] %in% c("Mid","Sab","Ban"), bath <- "quick", bath <- "usgs")
        ifelse(banks[i] %in% c("Mid","Sab","Ban"), iso <- c(seq(50,200,by=50)), iso <- c(seq(40,140,by=20)))
        # This is one figure to rule all the contours!
        ScallopMap(banks[i],title=fig.title,bathy.source=bath,isobath = iso,plot.bathy=T,plot.boundries=T,boundries="offshore",
                   direct=direct,cex.mn=2,xlab="",ylab="",dec.deg = F)
        image(list(x = proj$x, y=proj$y, z = pre.proj[[maps.to.make[m]]]), axes=F,add=T,breaks = lvls,col=cols)
        plot(bound.poly.surv.sp,add=T,lwd=2)
        #plot(mesh.cf,add=T)
      
  
        # Add the regular survey tows.
        if(maps.to.make[m] != "MC-spatial" && maps.to.make[m] != "CF-spatial")
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
          if( banks[i] == "Ban") 
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
   
          
          
          # For these plots the legend goes like this     
          if(maps.to.make[m] %in% c("PR-spatial", "Rec-spatial", "FR-spatial",bin.names))
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
        } # end if(seed.n.spatial.maps[k] != "MC-spatial" && seed.n.spatial.maps[k] != "CF-spatial")
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
        if(length(sb[,1]) > 0) addPolys(sb,lty=2,lwd=2)
        # If saving as a png turn off the plot device
        if(fig != "screen") dev.off()
      } # end for(m in 1:length(spatial.maps))    
    }# end if(length(spatial.maps) > 0) 

  }   # end if(length(spatial.maps > 0) || any(plots %in% "seedboxes"))
  
  
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
    
    # Save the figures?
    if(fig == "png") png(paste(plot.dir,"/survey_strata.png",sep=""),units="in",width = 11, height = 11,res=420,bg = "transparent")
    if(fig == "pdf")  pdf(paste(plot.dir,"/survey_strata.pdf",sep=""),width = 11,height = 8.5)
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
    
    if(banks[i] %in% c("GBa","BBn", "Sab" , "Mid","BBs" ,"Ban","GBb"))  
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
      legend(-66.12,41.5,legend = as.numeric(with(subset(surv.Live[[banks[i]]],year==yr),tapply(tow,Strata_ID,length))),
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
             pch=c(20,22,24), pt.bg = c("black","yellow","darkorange"),bty='n',cex=1, inset = .02,,bg=NA,box.col=NA)
      
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
    
    
    
    if(banks[i] == "Sab" ||  banks[i] == "GBb") 
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
    if(length(sb[,1]) > 0) addPolys(sb,lty=2,lwd=2)
    
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
    if(fig != "screen") dev.off()
    
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
    if(fig == "png") png(paste(plot.dir,"/abundance_ts.png",sep=""),units="in",
                         width = 8.5, height = 11,res=420,bg="transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"/abundance_ts.pdf",sep=""),width = 8.5, height = 11)
    
    par(mfrow=c(1,1))
    if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "GB")
    {
      survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F, 
                areas=surv.info$towable_area,clr=c('blue',"blue","darkgrey"),se=T,pch=16,
                add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
    }# end if(banks[i] != "Ger" && banks[i] != "Mid")
    # For german bank
    if(banks[i] == "Ger")
    {
      survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F, 
                ymin=-5,dat2=merged.survey.obj,clr=c('blue','red',"blue"),pch=c(16,17),se=T,yl2=400,
                add.title = T,titl = survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
      legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
    } # end if(banks[i] == "Ger")
    if(banks[i] == "Mid" || banks[i] == "GB")
    {
      survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F, 
                ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl =survey.ts.N.title,cx.mn=3,axis.cx = 1.5)
    } # end if(banks[i] == "Mid")
    
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
    
    if(fig == "screen") windows(11,8.5)
    if(fig == "png") png(paste(plot.dir,"/biomass_ts.png",sep=""),
                         units="in",width = 8.5, height = 11,res=420,bg="transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"/biomass_ts.pdf",sep=""),width = 8.5, height = 11)
    
    if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "GB")
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
    if(banks[i] == "Mid"|| banks[i] == "GB")
    {
      survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,Bank=banks[i],pdf=F,  type='B',
               ht=6.5,wd=10,clr=c('blue',"blue","darkgrey"),se=F,pch=16,add.title=T,titl = survey.ts.BM.title,cx.mn=3,axis.cx = 1.5)
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
    if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "GB")
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
    if(banks[i] == "Mid" || banks[i] == "GB")
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
    
    if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "GB" && banks[i] != "Sab")
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
    } # end if(banks[i] != "Ger")
    
    if(banks[i] == "Ger")
    {
      survey.ts(survey.obj[[banks[i]]][[1]],min(survey.obj[[banks[i]]][[1]]$year,na.rm=T):yr,pdf=F,type='B', 
                dat2=merged.survey.obj,clr=c('blue','red',"blue"),se=T,pch=c(16,17),ys=1.3,
                add.title = T,titl = survey.ts.BM.title,cx.mn=3,axis.cx=1.5,user.bins=user.bins)
      legend("topright",c("unlined","lined"),pch=c(23,24),pt.bg = c("blue","red"),cex=1.5,lty=c(1,2),col=c("blue","red"),bty="n")
    } # end if(banks[i] == "Ger")
    if(banks[i] == "Mid"|| banks[i] == "GB")
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
    if(fig == "png") png(paste(plot.dir,"/SHF.png",sep=""),units="in",width = 8.5, 
                          height = 11,res=420,bg="transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"/SHF.pdf",sep=""),width = 8.5, height = 11)
    # Grab the last 7 years of data
    
    if(banks[i] != "Ger")
      {
        shf.years <- survey.obj[[banks[i]]][[1]]$year[(length(survey.obj[[banks[i]]][[1]]$year)-6):
                                                        length(survey.obj[[banks[i]]][[1]]$year)]
        s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
        shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
            recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
        if(fig != "screen") dev.off()
      } # end  if(banks[i] != "Ger")
    
    if(banks[i]=="Ger")
      {
        shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
        s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
        shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,
                recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
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
                recline=c(RS,CS),add.title = T,titl = SHFm.title,cex.mn=2, sample.size = s.size)	   
        if(fig != "screen") dev.off()
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
    if(fig == "png") png(paste(plot.dir,"/SHF-large.png",sep=""),units="in",width = 8.5, height = 11,res=420,bg="transparent")
                          
    if(fig == "pdf") pdf(paste(plot.dir,"/SHF-large.pdf",sep=""),width = 8.5, height = 11)
    # Grab the last 7 years of data
    
    if(banks[i] != "Ger")
    {
      shf.years <- survey.obj[[banks[i]]][[1]]$year[(length(survey.obj[[banks[i]]][[1]]$year)-6):
                                                      length(survey.obj[[banks[i]]][[1]]$year)]
      s.size <- survey.obj[[banks[i]]][[1]]$n[survey.obj[[banks[i]]][[1]]$year %in% shf.years]
      shf.plt(survey.obj[[banks[i]]],from='surv',yr=shf.years, col1='grey80',col2=1,rel=F, select=70,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
      if(fig != "screen") dev.off()
    } # end  if(banks[i] != "Ger")
    
    if(banks[i]=="Ger")
    {
      shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
      s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
      shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,select=70,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
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
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
      if(fig != "screen") dev.off()
    } # end  if(banks[i] != "Ger")
    
    if(banks[i]=="Ger")
    {
      shf.years <-  lined.survey.obj[[1]]$year[(length(lined.survey.obj[[1]]$year)-6):length(lined.survey.obj[[1]]$year)]
      s.size <- lined.survey.obj[[1]]$n[lined.survey.obj[[1]]$year %in% shf.years]
      shf.plt(lined.survey.obj,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F, split=60,
              recline=c(RS,CS),add.title = T,titl = SHF.title,cex.mn=3,sample.size = s.size)	
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
    if(banks[i] != "Ger" && banks[i] != "Mid" && banks[i] != "GB")
    {
      yrs <- min(clap.survey.obj[[banks[i]]][[1]]$year,na.rm=T):max(clap.survey.obj[[banks[i]]][[1]]$year,na.rm=T)
      survey.ts(clap.survey.obj[[banks[i]]][[1]], Bank=bank[i],pdf=F, years=yrs,axis.cx = 1.5,
                titl = clap.abund.ts.title,add.title=T, cx.mn=3,areas=strata.areas$towable_area,
                ht=7,wd=10,clr=c('blue',"blue","darkgrey"),se=T,pch=16, plots=c("pre",'rec','com'))
    } # if(banks[i] != "Ger" && banks[i] != "Mid")
    
    if(banks[i] == "Ger" || banks[i] == "Mid" || banks[i] == "GB")
    {
      
      yrs <- min(surv.Clap.Rand[[banks[i]]]$year,na.rm=T):max(surv.Clap.Rand[[banks[i]]]$year,na.rm=T)
      survey.ts(clap.survey.obj[[banks[i]]][[1]],Bank=bank[i],pdf=F,axis.cx = 1.5, 
                titl = clap.abund.ts.title,add.title=T, cx.mn=3, years=yrs,
                ht=7,wd=10,clr=c('blue',"blue","darkgrey"),se=T,pch=16, plots=c("pre",'rec','com'))
    } # end if(banks[i] == "Ger" || banks[i] == "Mid")
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
              titl = clap.per.ts.title ,CS=CS,RS=RS,axis.cx = 1.5)
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
    if(banks[i] == "GB") mc <- fish.reg$MC_reg[fish.reg$Bank == "GBa"]
    
    if(banks[i] != "Ger") 
    {
      # To get the ymax the same between succesive years I want to do this...
      bm<-survey.obj[[banks[i]]]$shf.dat$w.yst[which(survey.obj[[banks[i]]][[1]]$year==yr),which(seq(5,200,5) >= 5)]/1000
      bm.last<-survey.obj[[banks[i]]]$shf.dat$w.yst[which(survey.obj[[banks[i]]][[1]]$year==(yr-1)),which(seq(5,200,5) >= 5)]/1000
      ymax <- max(c(max(bm,na.rm=T)*1.1),max(bm.last,na.rm=T)*1.1)
      breakdown(survey.obj[[banks[i]]],yr=yr,mc=mc,cx.axs=1,y1max = ymax,add.title = F)
    } # end if(banks[i] != "Ger") 

    # Using the lined surevye object for German bank...
    if(banks[i] == "Ger") 
    {
      bm<-lined.survey.obj$shf.dat$w.yst[which(lined.survey.obj[[1]]$year==yr),which(seq(5,200,5) >= 5)]/1000
      bm.last<-lined.survey.obj$shf.dat$w.yst[which(lined.survey.obj[[1]]$year==(yr-1)),which(seq(5,200,5) >= 5)]/1000
      ymax <- max(c(max(bm,na.rm=T)*1.1),max(bm.last,na.rm=T)*1.1)
      breakdown(lined.survey.obj,yr=yr,mc=mc,cx.axs=1,y1max = ymax,add.title = F)
    }# end if(banks[i] == "Ger") 
    
    if(add.title ==T) title(paste("Biomass & Meat Count by Height (",banks[i],"-",yr,")",sep=""), cex.main=2,adj=0.35)
    if(fig != "screen") dev.off()   
    
    # I also want to remake the previsou year's breakdown plot, this will go in the current years folder but will
    # be the same y-scale (there is no guarantee that the plot made last year will be, likely it won't).  It's a bit
    # clunky but basically this is the same plot as last year but re-scaled for comparative purposes...
    if(fig == "screen") windows(11,8.5)
    if(fig == "png") png(paste(plot.dir,"breakdown-",(yr-1),".png",sep=""),units="in",
                         width = 11,height = 8.5,res=420,bg = "transparent")
    if(fig == "pdf") pdf(paste(plot.dir,"breakdown-",(yr-1),".pdf",sep=""),width = 11,height = 8.5)
    if(banks[i] != "Ger") 
    {
      # To get the ymax the same between succesive years I want to do this...
      breakdown(survey.obj[[banks[i]]],yr=(yr-1),mc=mc,cx.axs=1,y1max = ymax,add.title = F)
    } # end if(banks[i] != "Ger") 
    
    # Using the lined surevye object for German bank...
    if(banks[i] == "Ger") 
    {
      breakdown(lined.survey.obj,yr=(yr-1),mc=mc,cx.axs=1,y1max = ymax,add.title = F)
    }# end if(banks[i] == "Ger") 
    
    if(add.title ==T) title(paste("Biomass & Meat Count by Height (",banks[i],"-",(yr-1),")",sep=""), cex.main=2,adj=0.35)
    if(fig != "screen") dev.off()   
    
    #} # end if(banks[i] %in% c("BBn" , "GBb", "GBa"))
    
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
    sb <- subset(seedboxes,Bank == banks[i] & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep=""))
    if(banks[i] == "GB")  sb <- subset(seedboxes,Bank %in% c("GBa","GBb") & Closed < paste(yr,"-11-01",sep="") & Open >= paste(yr,"-01-01",sep=""))
    if(nrow(sb) > 0) # only run the rest of this if we have data...
    {
      seed.spatial.plots <- c("PR-spatial","Rec-spatial","FR-spatial","CF-spatial","MC-spatial","Clap-spatial")
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
        # If there is no data from last year then I use this years ymax and I don't make the plot for last year...
        ymax <- ifelse(length(bm.last[!is.na(bm.last)]) > 0, max(c(max(bm,na.rm=T)*1.1),max(bm.last,na.rm=T)*1.1),max(bm,na.rm=T)*1.1)
        # Now make the breakdown plot...
        breakdown(boxy,yr=yr,mc=mc,cx.axs=1,add.title="F",y1max = ymax,
                  CS = survey.obj[[banks[i]]][[1]]$CS[length(survey.obj[[banks[i]]][[1]]$CS)],
                  RS = survey.obj[[banks[i]]][[1]]$RS[length(survey.obj[[banks[i]]][[1]]$RS)])
        if(add.title==T) title(breakdown.title.seed, cex.main=2,adj=0.35)
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
        breakdown(boxy,yr=(yr-1),mc=mc,cx.axs=1,y1max = ymax,add.title = F)
        if(add.title==T) title(last.yr.breakdown.title.seed, cex.main=2,adj=0.35)
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
                recline=c(RS,CS),add.title = add.title,titl = seedbox.SHF.title,cex.mn=3,sample.size = s.size)	
        if(fig != "screen") dev.off()
        
        
        # Now zoom in on the larger size classes...
        if(fig == "png") png(paste(plot.dir,box.names[j],"-SHF-large.png",sep=""),units="in",
                             width = 8.5, height = 11,res=420,bg = "transparent") 
        if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-SHF-large.pdf",sep=""),
                             width = 8.5, height = 11) 
        if(fig == "screen") windows(8.5,11)
        shf.plt(boxy,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,select=60,
                recline=c(RS,CS),add.title = add.title,titl = seedbox.SHF.title,cex.mn=3,sample.size = s.size)
        if(fig != "screen") dev.off()
        
        # Now do the SHF with the panels split classes...
        if(fig == "png") png(paste(plot.dir,box.names[j],"-SHF-split.png",sep=""),units="in",
                             width = 8.5, height = 11,res=420,bg = "transparent") 
        if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-SHF-split.pdf",sep=""),
                             width = 8.5, height = 11) 
        if(fig == "screen") windows(8.5,11)
        shf.plt(boxy,from='surv',yr=shf.years, col1='grey80',col2=1,rel=F,split=70,
                recline=c(RS,CS),add.title = add.title,titl = seedbox.SHF.title,cex.mn=3,sample.size = s.size)
        if(fig != "screen") dev.off()
        
        
        # Next the spatial abundance figure.  
        if(fig == "png") png(paste(plot.dir,box.names[j],"-spatial_abundance.png",sep=""),units="in",
                             width = 11, height = 8.5,res=420,bg = "transparent")
        if(fig == "pdf") pdf(paste(plot.dir,box.names[j],"-spatial_abundance.pdf",sep=""),
                             width = 11, height = 8.5)
        if(fig == "screen") windows(11,8.5)
        par(mfrow=c(2,2),omi=c(0.1,0.2,0.5,0.5),xpd=F)
        
        
        # Get the correct levels for the legend and the image plots for the spatial abundance
        base.lvls=c(0,5,10,50,100,500,1000,2000,5000,10000,20000,50000,1e6)
        cols <- c(rev(magma(length(base.lvls[base.lvls < 2000]),alpha=0.7,begin=0.6,end=1)),
                  rev(magma(length(base.lvls[base.lvls > 1000])-1,alpha=0.8,begin=0.1,end=0.5)))
        max.lvl <- which(base.lvls >= max(c(max(pre.proj[["PR-spatial"]],na.rm=T),max(pre.proj[["FR-spatial"]],na.rm=T),
                                            max(pre.proj[["Rec-spatial"]],na.rm=T))))[1]
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
                     title=fig.title,dec.deg = F,ylab="",xlab="",cex.mn=1.3)
          # Add the contours
          if(b == 1) image(list(x = proj$x, y=proj$y, z = pre.proj[["PR-spatial"]]), axes=F,add=T,breaks = lvls,col=cols)
          if(b == 2) image(list(x = proj$x, y=proj$y, z = pre.proj[["Rec-spatial"]]), axes=F,add=T,breaks = lvls,col=cols)
          if(b == 3) image(list(x = proj$x, y=proj$y, z = pre.proj[["FR-spatial"]]), axes=F,add=T,breaks = lvls,col=cols)
          
          #Add the rest of the crap to the plot.
          addPolys(this.box,lty=2,lwd=2)
          # Add the regular survey tows.
          points(slat~slon,surv.seed,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1.3)
          # Add the exploratory survey tows
          points(slat~slon,surv.seed,subset=year==yr&state =='live' & random==5,pch=24,bg="darkorange",cex=1.3)
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
                                length(subset(surv.seed,year==yr & state=='live'& random==5)$ID),")",sep="")),
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
        
        
        ########### Now add the CF, MC, and Clapper figures for inside any of the seedboxes that exist.
        # Next the spatial abundance figure.  
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
            min.lvl <- max(which(base.lvls <= min(pre.proj[["CF-spatial"]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(pre.proj[["CF-spatial"]],na.rm=T)))
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
            min.lvl <- max(which(base.lvls <= min(pre.proj[["MC-spatial"]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(pre.proj[["MC-spatial"]],na.rm=T)))
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
            min.lvl <- max(which(base.lvls <= min(pre.proj[["Clap-spatial"]],na.rm=T)))
            max.lvl <- min(which(base.lvls >= max(pre.proj[["Clap-spatial"]],na.rm=T)))
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
                     title=fig.title,dec.deg = F,ylab="",xlab="",cex.mn=1.3)
          # Add the contours
          if(b == 1) image(list(x = proj$x, y=proj$y, z = pre.proj[["CF-spatial"]]), axes=F,add=T,breaks = lvls,col=cf.cols)
          if(b == 2) image(list(x = proj$x, y=proj$y, z = pre.proj[["MC-spatial"]]), axes=F,add=T,breaks = lvls,col=mc.cols)
          if(b == 3) image(list(x = proj$x, y=proj$y, z = pre.proj[["Clap-spatial"]]), axes=F,add=T,breaks = lvls,col=clap.cols)
          
          #Add the rest of the crap to the plot.
          addPolys(this.box,lty=2,lwd=2)
          # Add the regular survey tows.
          points(slat~slon,surv.seed,subset=year==yr & state=='live'& random==1,pch=20,bg='black',cex=1.3)
          # Add the exploratory survey tows
          points(slat~slon,surv.seed,subset=year==yr&state =='live' & random==5,pch=24,bg="darkorange",cex=1.3)
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
                                length(subset(surv.seed,year==yr & state=='live'& random==5)$ID),")",sep="")),
               bg=NA,box.col=NA,bty="n")
        
      } # end if(banks[i] != "GB")
      if(banks[i] == "GB")
      {
        legend("bottomright",legend = c(paste('exploratory (n =',
                                          length(unique(subset(surv.seed,year==yr & random%in% c(2,4,5))$tow)),")",sep=""),
                                    paste('repeated (n =',length(unique(subset(surv.seed,year==yr & 
                                                                                 random==3)$tow)),")", sep="")),title="Tow type",
               pt.bg = c("darkorange","yellow"),pch=c(24,22),bg = NA,inset=0.01,box.col=NA)
      } # # end if(banks[i] == "GB")
        if(add.title == T) title(paste("Seedbox ",fig.box.name[j]," (",banks[i],"-",yr,")",sep=""),cex.main=2,outer=T,line=-0.5)
        if(fig != "screen") dev.off()
        
        
      } # end for(j in 1:n.box)
    } # end (if nrow(sb))
  } # end the if(any(plots) %in% "seedboxes")
  
  
  
  
} # end the i for loop 

} # end the function
