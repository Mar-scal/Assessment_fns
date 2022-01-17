# An attempt by DK to make the survey design scripts fit within the current structure, we are currently using 
# a variety of files for this which contain the same information as what we use for the survey/modelling and 
# if we leave it like this the duplication will make us look like chumps eventually...
#####  DK August 26th, 2016
# Update history
# Commented, checked  and revised by DK August 2016
# March 2017 : Added option for text points or not on GBa, and changed Ger to have 20 extra random stations so Ginette and Tech can "randomly" 
# remove 20 of them...also fixed "zoomed in" GBa figures so the Northeast and Northwest labels were correct...
# June 2017:  Added an option for adding in extra stations to the figures.  These would need to be in the file extra.stations.csv, see below.
# July 2017:  Revised so that the seed chosen (either by user or by R) is plotted on the figures.  Tidied up the zoom figures on GBa so that the 
#             dot colors reflect the strata, allowed for the points on all figures to be the EID numbers (not the final survey cruise nubmers so use 
#             with caution), and changed the name of the csv with the survey locations to preliminary to differentiate it from the final csv
#             which is created after the cruise track has been determined for the survey.
# Jan 2018:   DK revised so that the minimum distance between stations on Sable is 2 km, it had historically been 3 km, but with the new
#             Haddock Box and loss of area on Sable it was basically impossible to pull off a survey of 100 stations...
# Feb 2018:   DK revised the German allocation so that something other that 80 tows worked...
# May 2018:  Revised the GBa sampling design to allocate more stations than we would if randomly assigning by area on the northern portion of GBa and tidied up the seedbox dates, now 
#            using our friend lubridate...
# Jan 2019:  DK revised to incorporate Banquereau stations, at this time we are proposing Ban stations to be fixed based on the 2012 stations.  Also had
#            to clean up for the revised Sable strataification due to WEBCA.  DK also made some other tweaks to the function, specifically how the
#            text points are dealt with, changed so the filenames easily incorporate the text point option and cut down on if() statements. We also added the ger.rep argument to allow backup repeat stations
# Jun 2020:  Updated to work with pectinid_sf and interactive plotly 
# Jan 2021: DK revised direct_fns behaviour to point to github by default.

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  None, this is a top level file whose output is either csv files or figures.
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 1: source(paste(direct_fns,"Survey_design/alloc.poly_new.r",sep=""))
# 2: source(paste(direct_fns,"Survey_design/Relief.plots.r",sep=""))
# 3: source(paste(direct_fns,"Survey_design/genran.r",sep=""))
# 4: source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))
###############################################################################################################

###############################################################################################################
# Arguments
# yr:            The year of interest for making survey design.  Default is the current year as.numeric(format(Sys.time(), "%Y"))
#
# direct:        The directory to load/save all the data, Default is the network Offshore "Y:/Offshore/Assessment/"
#
# direct_fns:    This is missing by default, will point to github when missing.  
#
# export:        Do you want to export the survey design locations.  T/F Default is F
#
# seed:          If you want to reproduce results you can specify the random seed used for allocating these.  Default = NULL which will use R's 
#####                internal random number generators.  
#
# plot:          Do you want to plot the results.  T/F Default is T
#
# fig:           Where do you want to plot the figures.  FOUR options, includes the default print to your "screen" optionally can do "pdf" or "png". As of                  2020, you can now also make an interactive leaflet plot. 
#
# legend:        Add a legend to the figure.  T/F Default  is T
#
# zoom:          Do you want to produce magnified maps for GBa tow locations.  T/F default is T
#
# banks:         What banks do you want to run this on.  Default banks are "BBs","BBn","GBa","GBb","Sab","Mid","GB","Ger" (note no option for Ban yet)
#
# relief.plots:  For German bank do you want to make the "relief plots", note these take a long time to make!!.  T/F and default is F
#
# digits:        For the relief plots this controls the smoothing of the surface.  Basically this says how many digits to retain in the X and Y locations
#####                Default is 4 (which is very detailed, using 3 makes a very smooth surface.)
#
# point.style:   Do you want to have the points in the zoomed in GBa plots text of the numbers or just cute little cexy circles?  
#####                Three options, Default = "points" which plots filled circles, "stn_num" puts in station numbers, and "both" does both
#####                "both" uses the x.adj and y.adj proportion to place the ID next to the point.
#
# x.adj:         adjustment of ID placement relative to the full x-range (e.g. x.adj=0.02 will place ID 2% away from the point in the x direction.)
#
# y.adj:         adjustment of ID placement relative to the full y-range (e.g. y.adj=0.02 will place ID 2% away from the point in the y direction.)
#
# ger.new:       Number of new stations to generate on German bank, default is 60 (this must be <= 80, generally we only use 60 or 80), change the
#####                alloc.poly number of stations below if you need > 80 new tows
# ger.rep:       Number of German repeat stations to assign. Default is 20 stations, but you can add more to be used as backup repeats
#
# add.extras:    Do we want to add the extra stations to the figures, the coordinates of these extra stations 
#####                would need to be in the file Data/Survey_data/Extra_stations.csv.  T/F with a default of F.
#
# pt.txt.sz:     Control the size of the points or the point text. To keep it simple you adjust one you adjust the other.  Default = 1
#
# repo:          Where are the functions you need for this.  Default = 'github' which points to the github repo and latest stable versions of the functions
#####                 Alternative is to specify the directory the function exists, something like "D:/Github/Offshore/Assessment_fns/DK/" to get to the folders with this files in them
##### SURVEY DESIGN

Survey.design <- function(yr = as.numeric(format(Sys.time(), "%Y")) ,direct, export = F,seed = NULL, point.style = "points",
                          plot=T,fig="screen",legend=T, zoom = T,banks = c("BBs","BBn","GBa","GBb","Sab","Mid","GB","Ger"),
                          add.extras = F,relief.plots = F,digits=4,ger.new = 60, x.adj=0.002, y.adj=0.002,ger.rep=20, cables=F,
                          pt.txt.sz = 1,repo = 'github')
{
  # Make sure data imported doesn't become a factor
  options(stringsAsFactors=F)
  # load required packages
  require(PBSmapping) || stop("Install PBSmapping Package bub")
  require(lubridate) || stop("Install the lubridate Package before it's too late!")
  require(RCurl)|| stop("Install the RCurl Package please")
  require(sf) || stop("Install the sf package before you get lost")
  require(tidyverse) || stop("Install the tidyverse package so you can do everything")
  require(maptools)|| stop("Install maptools you fools")
  require(dplyr) || stop("Install dplyr, it's the best")
  if(fig == "leaflet") require(leaflet) || stop("Please install the leaflet package")
  # load in the functions we need to do the survey design
  # Note I put the survey design functions in a "Survey_Design" folder with the other functions, and putting the figures in the "Survey_Design" folder 
  # Load functions right from Github repo...
  if(repo == 'github')
  {
    funs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_design/alloc.poly.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_design/genran.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_design/Relief.plots.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/Convert_PBSmapping_into_GIS_shapefiles.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/github_spatial_import.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/combo_shp.R")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
    for(fun in funs) 
    {
      download.file(fun,destfile = basename(fun))
      source(paste0(getwd(),"/",basename(fun)))
      file.remove(paste0(getwd(),"/",basename(fun)))
    } # end for(un in funs)
  } # end if(repo == 'github')
  
  if(repo !='github')
  {
    source(paste0(repo,"Survey_design/alloc.poly.r"))
    source(paste0(repo,"Survey_design/Relief.plots.r"))
    source(paste0(repo,"Survey_design/genran.r"))
    source(paste0(repo,"Survey_and_OSAC/convert.dd.dddd.r"))
    source(paste0(repo,"Maps/Convert_PBSmapping_into_GIS_shapefiles.R"))
    source(paste0(repo,"Maps/pectinid_projector_sf.R"))
    source(paste0(repo,"Maps/github_spatial_import.R"))
    source(paste0(repo,"Maps/combo_shp.R"))
  } # end if(repo !='github')
  
  
  # This needs fixed!!
  #sc <- getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",ssl.verifypeer = FALSE)
  #eval(parse(text = sc))  
  
  if(missing(direct)) direct <- "Y:/Offshore/Assessment/"
  
  # Bring in flat files we need for this to work, they are survey polyset, survey information, extra staions and the seedboxes.
  if(repo =='github')
  {
    #surv.polyset <- read.csv(paste(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv",sep=""),stringsAsFactors = F) #Read1
    surv.polyset <- github_spatial_import(subfolder="offshore_survey_strata", "offshore_survey_strata.zip", direct_fns=direct_fns, quiet=T)
    #areas <- read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""),stringsAsFactors = F) #Read1
    areas <- github_spatial_import(subfolder="offshore", "offshore.zip", direct_fns=direct_fns, quiet=T)
  }
  if(!repo =='github')
  {
    surv.polyset <- combo.shp(paste0(direct, "Data/Maps/approved/GIS_layers/offshore_survey_strata"),make.sf=T,make.polys=F, quiet=T)
    areas <- combo.shp(paste0(direct, "Data/Maps/approved/GIS_layers/offshore"),make.sf=T,make.polys=F, quiet=T)
  }
  # these have to come in as CSV
  surv.polydata <- read.csv(paste(direct,"Data/Survey_data/survey_information.csv",sep=""),stringsAsFactors = F)#Read2
  seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),
                       stringsAsFactors = F,header=T) # Read3
  seedboxes$Open <- dmy(seedboxes$Open)
  # DK created this file by extracting the extra tows from the survey results in years past
  #Read4 The idea would be to add new extra stations to this over time so they are all maintained in one location
  if(add.extras == T) extra.tows <- read.csv(paste(direct,"Data/Survey_data/Extra_stations.csv",sep=""),stringsAsFactors = F)
  # Note the folder structure is using the "Survey_Data" directory, there are two essentially static files stored in the top level directory
  # the file with information about fixed station location are in the top level folder (fixed_station_banks_towlst.csv)
  # the extra stations for all the banks are also stored in the same location (Extra_stations.csv), my hope is this is the file in which
  # all of the new extra stations are stored so that we have the details regarding the extra station locations for all banks in all years
  # in one location.
  
  # Define some variables
  surv.poly <- NULL
  polydata <- NULL
  towlst <- NULL
  
  # Close all open plot devices
  #if(plot == T && !is.null(dev.list())) dev.off(dev.list())
  # Get the index for the loop.
  num.banks <- length(banks)
  #browser()
  for(i in 1:num.banks)
  {
    # Grab the bank
    bnk <- banks[i]
    # Grab any extra tows selected for that bank and make them a PBS mapping object
    if(add.extras ==F) extras <- data.frame(bill = NULL) # Did it this way to minimize the amount of code I need to change...
    if(add.extras == T) 
    {
      extras <- extra.tows %>% dplyr::filter(year == yr, bank == bnk) %>% dplyr::select(tow,lon,lat,bank)
      names(extras) <- c("EID","X","Y","bank")
      extras$EID <- as.numeric(extras$EID)
      extras <- st_as_sf(extras, remove=F, coords=c("X", "Y"), crs=4326)
      # if(bnk != "Mid") key <-findPolys(extras, subset(areas,bank == bnk))
      # if(bnk == "Mid") key <-findPolys(extras, subset(areas,bank == "Sab")) # This will put extras with Middle if any on Sable, but they won't show up on figure so ok.
      # extras <- extras[extras$EID %in% key$EID,]
      # attr(extras,"projection") <- "LL"
      extras$lon.deg.min <- round(convert.dd.dddd(x = extras$X, format = "deg.min"), 4)
      extras$lat.deg.min <- round(convert.dd.dddd(x = extras$Y, format = "deg.min"), 4)
      extras <- extras[,c("EID", "X", "Y", "lon.deg.min", "lat.deg.min", "bank")]
    } # end if(add.extras == T)
    
    # Grab any seedboxes that were closed during the current year.
    sb <- subset(seedboxes,Bank == bnk & Active=="Yes") #Open >= ymd(paste(yr,"-01-01",sep="")))
    if(bnk == "GB") sb <- subset(seedboxes,Bank == "GBa" & Active=="Yes") # >= ymd(paste(yr,"-01-01",sep="")))
    
    if(nrow(sb > 0 ))
    {
      sb.sp <- PolySet2SpatialPolygons(as.PolySet(sb, projection = "LL"))
      sb <- st_as_sf(sb.sp, coords = c("X","Y"),crs=4326)
      
    }
    
    # Now for the banks that have survey strata we get the allocation.
    if(bnk %in% c("BBs","BBn","GBa","GBb","Sab"))
    {
      # Get the correct survey polygons
      surv.poly[[i]] <- subset(surv.polyset,label==bnk)
      sf_use_s2(FALSE) # do this for historical consistency
      shp_strata <- subset(surv.poly[[i]], startyr == max(surv.poly[[i]]$startyr)) %>%
        dplyr::rename(startyear = 'startyr') %>%
        dplyr::rename(Strata_ID = 'Strt_ID') %>%
        dplyr::rename(towable_area = 'towbl_r')
        
      #attr(surv.poly[[i]],"projection")<-"LL"
      
      #polydata[[i]] <- subset(surv.polydata,label==bnk)
      # For areas in which we have mutliple survey strata information... e.g. Sable which was changed due to WEBCA.
      #polydata[[i]] <- polydata[[i]][polydata[[i]]$startyear == max(polydata[[i]]$startyear,na.rm=T),]
      # For GBa we actually want a different allocation scheme, I've set the number of tows in each strata to be what we had in 2016, this is similar to what we've
      # observed since 2010, but every year has varied slightly (in the north).
      # This scheme is based on a vague comment in the 2013 GBa Assessment about preferentially placing some tows in the northern portion of the bank.  Note that for the 2017 survey we 
      # had simply stratified by area based on disscusions with Ginette and a collective lack of knowledge about the details around the 2013 comment.
      if(bnk == "GBa") shp_strata$allocation <- c(51,37,32,26,35,11,8) 
      
      #shp_strata <- pbs.2.gis(dat = surv.poly[[i]], env.object=T)
      #shp_strata <- left_join(surv.poly[[i]],polydata[[i]]) # Get the polydata into this the strata shape file for fun later...
      # Give them their correct colors and order
      shp_strata$Strata <- factor(shp_strata$Strata_ID,labels = shp_strata$PName)
      
      # if the seed hasn't been set, set it...
      if(is.null(seed)) seed <- sample(1:2^15, 1)
      # Now allocate the tows, each bank has it's own settings for this allocation.
      #DK Note, I question the use of mindist of 3 on Sable, I'm sure there is good reason for it, but how much does this limit the 
      #actual randomization of the tow locations?
      # DK revised Sable to be a minimum distance of 2 km given that the Haddock box has removed a percentage of the bank...
      
      #table(towlst[[i]]$Strata_ID)
      
      if(bnk == "BBn") towlst[[i]]<-alloc.poly(strata = shp_strata, 
                                               ntows=100,pool.size=3,mindist=1,seed=seed, repo=repo)
      if(bnk == "BBs") towlst[[i]]<-alloc.poly(strata=shp_strata,
                                               ntows=25,seed=seed, repo=repo)
      if(bnk == "Sab") towlst[[i]]<-alloc.poly(strata = shp_strata,
                                               ntows=100,pool.size=3,mindist=2,seed=seed, repo=repo)
      if(bnk == "GBb") towlst[[i]]<-alloc.poly(strata=shp_strata,
                                               ntows=30,pool.size=5,seed=seed, repo=repo)
      if(bnk == "GBa") towlst[[i]]<-alloc.poly(strata=shp_strata,
                                               ntows=200,pool.size=5,mindist=1,seed=seed, repo=repo)
      
      if(bnk == "GBa" & yr==2019) {
        # manually shift 3 stations in 2019:
        towlst[[i]]$Tows[towlst[[i]]$Tows$EID==15, c("X", "Y")] <- c(-66.445, 42.101)
        towlst[[i]]$Tows[towlst[[i]]$Tows$EID==64, c("X", "Y")] <- c(-66.668, 42.148)
        towlst[[i]]$Tows[towlst[[i]]$Tows$EID==84, c("X", "Y")] <- c(-67.048, 42.056)
        towlst[[i]]$Tows[towlst[[i]]$Tows$EID==190, c("X", "Y")] <- c(-66.416, 41.407)
      }
      
      if(bnk=="GBa" & yr ==2021) {
        tows2021edited <- read.csv(paste0(direct, "/Data/Survey_data/2021/Summer/GBa/Preliminary_Survey_design_Tow_locations_GBa_cruisetrackJL22.csv"))
        towlst[[i]]$Tows$X <- tows2021edited$X[!tows2021edited$STRATA %in% "extra"]
        towlst[[i]]$Tows$Y <- tows2021edited$Y[!tows2021edited$STRATA %in% "extra"]
      }
      
      if(bnk=="GBb" & yr ==2021) {
        tows2021edited <- read.csv(paste0(direct, "/Data/Survey_data/2021/Summer/GBb/Preliminary_Survey_design_Tow_locations_GBb_cruisetracksJL20.csv"))
        towlst[[i]]$Tows$X <- tows2021edited$X[!tows2021edited$STRATA %in% "extra"]
        towlst[[i]]$Tows$Y <- tows2021edited$Y[!tows2021edited$STRATA %in% "extra"]
      }
      
      # In 2019, we noticed that the strata created during the 2018 restratification of of Sable were slightly wrong. However, the stations had already been made for the 2019 survey and presented to the SWG.
      # Instead of creating an brand new survey design for Sable in 2019, we opted to simply move station 27 from it's original location outside of the SFZ (inside WEBCA).
      # This was done manually using the CSV and script below.
      # To see the changes made in 2019 to the Sable strata, see: Y:/Offshore/Assessment/2018/Misc/Sable_re_stratification/Restratification_of_SB_pkg_sp_Updated2019.R
      # And emails in Y:/Offshore/Assessment/2018/Misc/Sable_re_stratification
      if(bnk=="Sab" & yr ==2019) {
        tows2019edited <- read.csv(paste0(direct, "/Data/Survey_data/2019/Spring/Sab/Preliminary_Survey_design_Tow_locations_Sab_edited.csv"))
        towlst[[i]]$Tows$X <- tows2019edited$X[!tows2019edited$STRATA %in% "extra"]
        towlst[[i]]$Tows$Y <- tows2019edited$Y[!tows2019edited$STRATA %in% "extra"]
      }
      
      if(bnk == "Sab" & yr==2020) {
        # manually shift 3 stations in 2019:
        towlst[[i]]$Tows[towlst[[i]]$Tows$EID==23, "Y"] <- 43.365
        towlst[[i]]$Tows[towlst[[i]]$Tows$EID==63, "Y"] <- 43.675
      }
      
      if(bnk == "BBs" & yr==2020) {
        # manually shift 3 stations in 2019:
        towlst[[i]]$Tows[towlst[[i]]$Tows$EID==24, c("X", "Y")] <- c(-65.835, 42.516)
      }
      
      #get the deg dec minutes coordinates too
      if(!bnk == "Ger")  {
        writetows <- towlst[[i]]$Tows
        if(add.extras==T) {
          extras$Poly.ID <- "extra"
          writetows <- rbind(towlst[[i]]$Tows, extras[,c("EID", "X", "Y", "Poly.ID")])
        }
        writetows$`Longitude (DDMM.mm)` <- round(convert.dd.dddd(x = writetows$X, format = "deg.min"), 4)
        writetows$`Latitude (DDMM.mm)` <- round(convert.dd.dddd(x = writetows$Y, format = "deg.min"), 4)
        st_geometry(writetows) <- NULL
        writestrata <- dplyr::rename(towlst[[i]]$Strata, Poly.ID="PID")
        st_geometry(writestrata) <- NULL
        if(is.character(writetows$Poly.ID)) writestrata$Poly.ID <- as.character(writestrata$Poly.ID)
        writetows <- left_join(writetows, writestrata[, c("Poly.ID", "Strata")], by="Poly.ID")
        writetows <- writetows[, c("EID", "Longitude (DDMM.mm)", "Latitude (DDMM.mm)", "X", "Y", "Poly.ID", "Strata")]
      }
      
      # if you want to save the tow lists you can export them to csv's.
      if(export == T && bnk %in% c("BBn","BBs","GB","Mid","Sab", "Ban")) 
      {
        if(!seed == yr-2000) seedlab <- seed
        if(!seed == yr-2000) {
          dir.create(path = paste0(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/", seedlab, "/"))
          write.csv(writetows,paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/", seedlab, "/Preliminary_Survey_design_Tow_locations_", bnk, ".csv",sep=""),row.names=F)
        } #Write1
        if(seed == yr-2000) {write.csv(writetows,paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Preliminary_Survey_design_Tow_locations_", bnk, ".csv",sep=""),row.names=F)} #Write1
      } # end if(export == T && bnk %in% c("BBn","BBs","GB","Ger","Mid","Sab","Ban")) 
      if(export == T && bnk %in% c("GBa","GBb")) 
      {  
        if(!seed == yr-2000) seedlab <- seed
        if(!seed == yr-2000) {
          dir.create(path = paste0(direct,"Data/Survey_Data/",yr,"/Summer/",bnk,"/", seedlab, "/"))
          write.csv(writetows,paste(direct,"Data/Survey_Data/",yr,"/Summer/",bnk,"/", seedlab, "/Preliminary_Survey_design_Tow_locations_", bnk, ".csv",sep=""),row.names=F)
        } #Write1
        if(seed == yr-2000) {write.csv(writetows,paste(direct,"Data/Survey_Data/",yr,"/Summer/",bnk,"/Preliminary_Survey_design_Tow_locations_", bnk, ".csv",sep=""),row.names=F)} #Write1
      } # end if(export == T && bnk %in% c("GBa,GBb")) 
      
      # Now if you want to make the plots do all of this.
      if(plot == T)
      {
        if(!seed == yr-2000) seedlab <- seed
        
        # Where do yo want the plot to go?
        if(fig=="screen") windows(11,8.5)
        
        if(seed == yr-2000){
          if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", res=420,
                                height = 8.5,bg = "transparent")
          if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation_",bnk,"_",point.style,".pdf"),width = 11, 
                                height = 8.5,bg = "transparent")
        }
        if(!seed == yr-2000){
          dir.create(path = paste0(direct,yr,"/Survey_Design/",bnk,"/", seedlab, "/"))
          if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/", seedlab, "/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", res=420,
                                height = 8.5,bg = "transparent")
        }
        
        if(fig == "leaflet"){
          
          require(leaflet)
          print(leaflet() %>%
                  #setView(-62, 45, 5)%>%
                  addProviderTiles(provider = providers$Esri.OceanBasemap) %>%
                  addPolygons(data=shp_strata, color="black", weight=0.1, fillColor = shp_strata$col, fillOpacity=0.5) %>% # doesn't work :(
                  addCircles(lng = towlst[[i]]$Tows$X, 
                             lat = towlst[[i]]$Tows$Y, 
                             label= paste0(towlst[[i]]$Tows$EID, "_", towlst[[i]]$Tows$Poly.ID), 
                             popup =  paste0(round(towlst[[i]]$Tows$X, 4), ",", round(towlst[[i]]$Tows$Y, 4))))
          
        }
        
        if(!fig == "leaflet") 
        {
          # Now to get the points and the colors all tidy here...
          tmp <- towlst[[i]]$Tows
          p.dat <- towlst[[i]]$Strata %>% dplyr::select(Strata_ID,col,area,PName)
          tmp.sf <- st_join(tmp,p.dat, by = c("STRATA" = "PName"))
          
          shp_strata <- towlst[[i]]$Strata
          shp_strata$area2 <- paste0(round(shp_strata$area,0), expression(km^2))
          
          # Make the base plot...
          bp<- pecjector(area = bnk,c_sys = 4326, add_layer = list(sfa='offshore',eez='eez',bathy = c(50,'c')),plot=F, quiet=T)# + 
          
          bp <- bp + geom_sf(data=shp_strata, aes(fill=Strata), colour=NA, alpha=0.75) + 
            scale_fill_manual(values = shp_strata$col, 
                              labels=paste0(shp_strata$Strata, "\n", 
                                            shp_strata$allocation, " tows"))
          
          #+ geom_sf(data=final.strata) + scale_fill_manual(values = col.codes$col)
          #plotly::ggplotly(bp)
          
          # So what do we want to do with the points, first plots the station numbers
          if(point.style == "stn_num") bp2 <- bp + geom_sf_text(data=tmp.sf,aes(label = EID),size=pt.txt.sz) #text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
          # This just plots the points
          if(point.style == "points")  bp2 <- bp + geom_sf(data=tmp.sf, shape=1,size=pt.txt.sz) + geom_sf(data=tmp.sf, shape=1,size=pt.txt.sz)  #
          # Note regarding point colours. Sometimes points fall on the border between strata so it appears that they are mis-coloured. To check this,
          # run above line WITHOUT bg part to look at where the points fell and to make sure thay they are coloured correctly. It's not 
          # a coding issue, but if it looks like it will be impossible for the tow to occur within a tiny piece of strata, re-run the plots with a diff seed.
          #browser()
          # This does both, if it doesn't look pretty change the x.adj and y.adj options
          if(point.style == "both" ) bp2 <- bp + geom_sf_text(data=tmp.sf,aes(label = EID),nudge_x = x.adj,nudge_y = y.adj,size=pt.txt.sz) + geom_sf(data=tmp.sf,shape=1,size=pt.txt.sz) #+ geom_sf(data=tmp.sf, shape=21,size=pt.txt.sz)
          #print(bp2)
          if(nrow(extras) > 0)
          {
            if(point.style == "points") bp2 <- bp2 + geom_sf(data = extras, shape =24,fill = "darkorange",size=pt.txt.sz )
            if(point.style == "both" )  bp2 <- bp2 + geom_sf_text(data=extras,aes(label = EID),nudge_x = x.adj,nudge_y = y.adj,size=pt.txt.sz) + 
                geom_sf(data=extras, shape =24,fill = "darkorange" ,size=pt.txt.sz)
          }
          if(cables==T)
          {
            cables <- st_read("Z:/Maps/Undersea_cables/AllKnownCables2015.shp") %>%
              st_transform(4326) %>%
              filter(Name_EN == st_intersection(.,shp_strata)$Name_EN)
            bp2 <- bp2 + geom_sf(data=cables, colour="red") +
              xlim(ggplot_build(bp)$layout$panel_scales_x[[1]]$range$range) +
              ylim(ggplot_build(bp)$layout$panel_scales_y[[1]]$range$range)
          }
          # # And if there are any seedboxes
          if(nrow(sb) > 0) bp2 <- bp2 + geom_sf(data=sb,fill=NA)
          
          pf <- bp2 + labs(title= paste("Survey (",bnk,"-",yr,")",sep=""),
                           subtitle = paste("Note: The random seed was set to ",seed,sep=""),
                           caption = paste("Survey stations (n = ",length(towlst[[i]]$Tows$Y),")"," \n Extra stations (n = ",
                                           nrow(extras),")",sep="",collapse =" ")) +
            theme(legend.position=c(1.01,0.25), legend.justification=c(0,0), plot.margin = margin(1,9,1,0,"cm")) +
            coord_sf(expand=F)
          if(fig != 'dashboard') print(pf)
          # Turn the device off if necessary.  
          if(!fig %in% c("screen", "leaflet",'dashboard')) dev.off()
        }}}
    
    if(bnk %in% c("Mid","GB","Ban")) 
    {
      #Read5
      towlst[[i]] <-  subset(read.csv(paste(direct,"Data/Survey_data/fixed_station_banks_towlst.csv",sep="")),Bank == bnk)
      
      #get the deg dec minutes coordinates too
      towlst[[i]]$lon.deg.min <- round(convert.dd.dddd(x = towlst[[i]]$X, format = "deg.min"), 4)
      towlst[[i]]$lat.deg.min <- round(convert.dd.dddd(x = towlst[[i]]$Y, format = "deg.min"), 4)
      towlst[[i]] <- towlst[[i]][, c("EID", "X", "Y", "lon.deg.min", "lat.deg.min")]
      
      writetows <- towlst[[i]]
      
      if(add.extras==T) writetows <- rbind(writetows, extras[,!names(extras) %in% "bank"])
      
      writetows$Bank <- bnk
      writetows$Survey <- "spring"
      
      if(export == T) {
        write.csv(writetows[,c("EID", "X", "Y", "lon.deg.min", "lat.deg.min", "Bank", "Survey")],paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Preliminary_Survey_design_Tow_locations_", bnk, ".csv",sep=""),row.names=F) #Write1
      }
      
      # attr(towlst[[i]],"projection") <- "LL"
      if(plot == T)
      {
        if(fig=="screen") windows(11,8.5)
        if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", res=420,
                              height = 8.5,bg = "transparent")
        if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".pdf"),width = 11, 
                              height = 8.5,bg = "transparent")
        
        if(fig == "leaflet"){
          require(leaflet)
          
          print(leaflet() %>%
                  setView(-62, 45, 5)%>%
                  addProviderTiles(provider = providers$Esri.OceanBasemap) %>%
                  addCircles(lng = towlst[[i]]$X, 
                             lat = towlst[[i]]$Y, 
                             label= paste0(towlst[[i]]$EID), 
                             popup =  paste0(round(towlst[[i]]$X, 4), ",", round(towlst[[i]]$Y, 4))))
        }
        
        if(!fig == "leaflet") 
        {
          #browser()
          # Now to get the points and the colors all tidy here...
          tmp <- towlst[[i]]
          tmp.sf <- st_as_sf(tmp,crs= 4326,coords = c("X","Y"))
          
          # Make the base plot...
          if(!bnk=="Mid") bp <- pecjector(area = bnk,repo = 'github',c_sys = 4326, add_layer = list(bathy = c(50,'c'), sfa = 'offshore',survey=c('offshore','outline')),plot=F, quiet=T)# + 
          if(bnk=="Mid") bp <- pecjector(area = bnk,repo = 'github',c_sys = 4326, add_layer = list(bathy = c(50,'c'), sfa = 'offshore'),plot=F, quiet=T)# + 
          
          # So what do we want to do with the points, first plots the station numbers
          if(point.style == "stn_num") bp2 <- bp + geom_sf_text(data=tmp.sf,aes(label = EID),size=pt.txt.sz) #text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
          # This just plots the points
          if(point.style == "points")  bp2 <- bp + geom_sf(data=tmp.sf,shape=1,size=pt.txt.sz) 
          # Note regarding point colours. Sometimes points fall on the border between strata so it appears that they are mis-coloured. To check this,
          # run above line WITHOUT bg part to look at where the points fell and to make sure thay they are coloured correctly. It's not 
          # a coding issue, but if it looks like it will be impossible for the tow to occur within a tiny piece of strata, re-run the plots with a diff seed.
          
          # This does both, if it doesn't look pretty change the x.adj and y.adj options
          if(point.style == "both" ) bp2 <- bp + geom_sf_text(data=tmp.sf,aes(label = EID),nudge_x = x.adj,nudge_y = y.adj,size=pt.txt.sz) + geom_sf(data=tmp.sf, shape=1,size=pt.txt.sz)
          if(nrow(extras) > 0) 
          {
            if(point.style == "points") bp2 <- bp2 + geom_sf(data = extras, shape =24,fill = "darkorange" ,size=pt.txt.sz)
            if(point.style == "both" )  bp2 <- bp2 + geom_sf_text(data=extras,aes(label = EID),nudge_x = x.adj,nudge_y = y.adj,size=pt.txt.sz) + geom_sf(data=extras, shape =24,fill = "darkorange" ,size=pt.txt.sz)
          }
          
          if(cables==T)
          {
            cables <- st_read("Z:/Maps/Undersea_cables/AllKnownCables2015.shp") %>%
              st_transform(4326) %>%
              filter(Name_EN == st_intersection(.,shp_strata)$Name_EN)
            bp2 <- bp2 + geom_sf(data=cables, colour="red") +
              xlim(ggplot_build(bp)$layout$panel_scales_x[[1]]$range$range) +
              ylim(ggplot_build(bp)$layout$panel_scales_y[[1]]$range$range)
          }
          # # And if there are any seedboxes
          
          # And if there are any seedboxes
          if(nrow(sb) > 0) bp2 <- bp2 + geom_sf(data=sb,fill=NA)                                                                                                                  
          cap <- paste("Survey stations (n = ",length(tmp.sf$EID),")")
          if(bnk != "Ban") cap <- paste("Fixed stations (n = ",length(tmp.sf$EID),")",sep="")
          if(nrow(extras >0 )) cap <- paste(cap," \n Extra stations (n = ",
                                               nrow(extras),")",sep="",collapse =" ")
          sub.title <- paste("Note: The random seed was set to ",seed,sep="")
          if(bnk != "Ban") sub.title <- ''
          pf <- bp2 + labs(title= paste("Survey (",bnk,"-",yr,")",sep=""),
                           subtitle = sub.title,
                           caption = cap)  + coord_sf(expand=F)
          if(fig != 'dashboard') print(pf)
          # Turn the device off if necessary.  
          #if(!fig %in% c("screen")) dev.off()
          
        }
        if(!fig %in% c("screen", "leaflet","dashboard")) dev.off()
      }# end if(plot==T)
    } # end if(bnk %in% c("Mid","GB", "Ban")) 
    
    
    if(bnk == "Ger")
    {
      #Read6 Be careful here, I'm assuming that when you run this you are looking at the Ger data from "last year" and looking to identify the
      # tows that you want to repeat for this year, if trying to reproduce something specific for past years you'd need to change yr
      # to the year that you are interested in (if going back before 2012 you may need to create this csv file)
      survey.dat <- read.csv(paste(direct,"Data/Survey_data/",(yr-1),"/Spring/Ger/Survey1985-",(yr-1),".csv",sep=""))
      lastyearstows <- subset(survey.dat,state=='live'& year==(yr-1) & random==1,c('tow','slon','slat','stratum'))
      lastyearstows$stratum<-1
      #Read7 This contains the boundary polygon from the file "GerSurvPoly1.csv".
      Ger.sf <- st_read(paste0(direct, "Data/Maps/approved/Survey/German_WGS_84/WGS_84_German.shp"), quiet=T) %>%
        st_transform(4326)
      
      # This gets us the tows for German bank, note we have ger.new new tows and 20 repeats when we call it this way.
      Ger.tow.lst<-alloc.poly(strata=list(Ger.sf, data.frame(PID=1,PName="Ger",border=NA,col=rgb(0,0,0,0.2),repeats=ger.rep)),
                              ntows=ger.new+20,
                              pool.size=3,mindist=1,
                              repeated.tows=lastyearstows,
                              seed=seed,
                              repo=repo)
      # FK modified the last piece of alloc.poly and added in a the sample command afterwards, this was
      # done so that we can re-create the sample stations for TPD (i.e. our survey technician).
      #ntows=ger.new,pool.size=3,mindist=1,repeated.tows=lastyearstows,seed=seed)  
      ger.tows <- sample(Ger.tow.lst$Tows$new.tows$EID,size=ger.new,replace=F)    
    
      Ger.tow.lst$Tows$new.tows <- Ger.tow.lst$Tows$new.tows[Ger.tow.lst$Tows$new.tows$EID %in% ger.tows,]
      Ger.tow.lst$Tows$new.tows$EID <- 1:nrow(Ger.tow.lst$Tows$new.tows)
      
      # Rename and tidy up the data
      Ger.tow.lst$Tows$new.tows$STRATA="new"
      Ger.tow.lst$Tows$repeated.tows$STRATA="repeated"
      Ger.tow.lst$Tows$repeated.tows$EIDlastyear <- Ger.tow.lst$Tows$repeated.tows$EID - 1000
      # any repeated tows above 20 get flagged as repeated-backup
      # pull all OTHER repeats as backups and plot/list separately
      names(lastyearstows) <- c("tow", "X", "Y", "stratum")
      Ger.repeat.backups <- plyr::join(lastyearstows, as.data.frame(Ger.tow.lst$Tows$repeated.tows), type="full")
      Ger.repeat.backups <- dplyr::select(Ger.repeat.backups[is.na(Ger.repeat.backups$EID),], c("tow", "X", "Y"))
      #if(length(Ger.tow.lst$Tows$repeated.tows$STRATA) > 20) Ger.tow.lst$Tows$repeated.tows$STRATA[21:ger.rep] <- "repeated-backup"
      Ger.tow.lst$Tows$new.tows$Poly.ID=1
      Ger.tow.lst$Tows$repeated.tows$Poly.ID=24
      Ger.repeat.backups$Poly.ID<-24
      Ger.repeat.backups$STRATA = "repeated-backup"
      names(Ger.repeat.backups)[which(names(Ger.repeat.backups)=="tow")] <- "EID"
      Ger.repeat.backups$EID <- Ger.repeat.backups$EID + 2000
      Ger.tow.lst$Tows$backup.repeats <- Ger.repeat.backups
      Ger.tow.lst$Tows$backup.repeats <- st_as_sf(Ger.tow.lst$Tows$backup.repeats, coords=c("X", "Y"), remove=F, crs=4326)
      Ger.tow.lst$Tows$backup.repeats$EIDlastyear <- Ger.tow.lst$Tows$backup.repeats$EID - 2000
      #if(length(Ger.tow.lst$Tows$repeated.tows$STRATA) > 20) Ger.tow.lst$Tows$repeated.tows$Poly.ID[Ger.tow.lst$Tows$repeated.tows$STRATA=="repeated-backup"] <- 3
      Ger.tow.dat<- rbind(Ger.tow.lst$Tows$new.tows, 
                          Ger.tow.lst$Tows$repeated.tows[,c("EID", "X", "Y", "Poly.ID", "STRATA")],
                          Ger.tow.lst$Tows$backup.repeats[,c("EID", "X", "Y", "Poly.ID", "STRATA")])
      Ger.tow.dat.rep<- rbind(dplyr::select(Ger.tow.lst$Tows$repeated.tows, -"nndist"),
                              Ger.tow.lst$Tows$backup.repeats)
      
      # Get degree decimal minutes
      Ger.tow.dat$lon.deg.min <- round(convert.dd.dddd(x = Ger.tow.dat$X, format = "deg.min"), 4)
      Ger.tow.dat$lat.deg.min <- round(convert.dd.dddd(x = Ger.tow.dat$Y, format = "deg.min"), 4)
      Ger.tow.dat <- Ger.tow.dat[,c("EID", "X", "Y", "lon.deg.min", "lat.deg.min", "Poly.ID", "STRATA")]
      
      # Get degree decimal minutes (repeat backups)
      Ger.tow.dat.rep$lon.deg.min <- round(convert.dd.dddd(x = Ger.tow.dat.rep$X, format = "deg.min"), 4)
      Ger.tow.dat.rep$lat.deg.min <- round(convert.dd.dddd(x = Ger.tow.dat.rep$Y, format = "deg.min"), 4)
      Ger.tow.dat.rep <- Ger.tow.dat.rep[,c("EID", "X", "Y", "lon.deg.min", "lat.deg.min", "Poly.ID", "STRATA", "EIDlastyear")]
      
      writetows <- Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]
      st_geometry(writetows) <- NULL
      writerepeats <- Ger.tow.dat.rep[Ger.tow.dat.rep$STRATA %in% c("repeated", "repeated-backup"),]
      st_geometry(writerepeats) <- NULL
      
      #Write3 If you want to save the data here's where it will go
      if(export == T)  {
        
        if(!seed == yr-2000) seedlab <- seed
        if(!seed == yr-2000) {
          dir.create(path = paste0(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/", seedlab, "/"))
          write.csv(writetows,paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/", seedlab, "/Preliminary_Survey_design_Tow_locations_", bnk, ".csv",sep=""),row.names=F)
          write.csv(writerepeats,paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/", seedlab, "/Preliminary_Survey_design_Tow_locations_",bnk,"_repbackups.csv",sep=""),row.names=F)
        } #Write1
        if(seed == yr-2000) {
          write.csv(writetows,paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Preliminary_Survey_design_Tow_locations_",bnk,".csv",sep=""),row.names=F)
          write.csv(writerepeats,paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Preliminary_Survey_design_Tow_locations_",bnk,"_repbackups.csv",sep=""),row.names=F)
        }
      }
      # Plot this bad boy up if you want to do such things
      if(plot==T)
      {
        if(!seed == yr-2000) seedlab <- seed
        
        # Where do yo want the plot to go?
        if(fig=="screen") windows(11,8.5)
        
        if(seed == yr-2000){
          if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", res=420,
                                height = 8.5,bg = "transparent")
          if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation_",bnk,"_",point.style,".pdf"),width = 11, 
                                height = 8.5,bg = "transparent")
        }
        
        if(!seed == yr-2000){
          dir.create(path = paste0(direct,yr,"/Survey_Design/",bnk,"/", seedlab, "/"))
          if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/", seedlab, "/Survey_allocation-",bnk,"_",point.style,".png"),width = 11, units="in", res=420,
                                height = 8.5,bg = "transparent")
          if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/", seedlab, "/Survey_allocation_",bnk,"_",point.style,".pdf"),width = 11, 
                                height = 8.5,bg = "transparent")
        }
        
        if(fig == "leaflet"){
          require(leaflet)
          print(leaflet() %>%
                  setView(-62, 45, 5)%>%
                  addProviderTiles(provider = providers$Esri.OceanBasemap) %>%
                  addCircles(lng = Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$X, 
                             lat = Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$Y, 
                             label= paste0(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$EID, "_", Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$STRATA), 
                             popup =  paste0(round(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$X, 4), ",", round(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("new", "repeated"),]$Y, 4))))
          
        }
        
        if(!fig == "leaflet") 
        {
          
          # Now to get the points and the colors all tidy here...
          tmp <- Ger.tow.dat
          tmp.sf <- st_as_sf(tmp,crs= 4326,coords = c("X","Y"))
          tmp.sf.reg <- tmp.sf %>% dplyr::filter(STRATA %in% c("new","repeated"))
          tmp.sf.reg$`Tow type` <- tmp.sf.reg$STRATA
          # Make the base plot...
          bp <- pecjector(area = bnk,repo = 'github',c_sys = 4326, add_layer = list(bathy = c(50,'c'), sfa = 'offshore',survey=c('offshore','outline')),plot=F, quiet=T)# + 
          
          bp <- bp + geom_sf(data=Ger.sf, fill="lightgrey", colour="NA", alpha=0.75)
          
          # So what do we want to do with the points, first plots the station numbers
          if(point.style == "stn_num") bp2 <- bp + geom_sf_text(data=tmp.sf.reg,aes(label = EID),size=pt.txt.sz) #text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
          # This just plots the points
          if(point.style == "points")  bp2 <- bp + geom_sf(data=tmp.sf.reg,aes(shape=`Tow type`),size=pt.txt.sz) + scale_shape_manual(values = c(1,0))
          # Note regarding point colours. Sometimes points fall on the border between strata so it appears that they are mis-coloured. To check this,
          # run above line WITHOUT bg part to look at where the points fell and to make sure thay they are coloured correctly. It's not 
          # a coding issue, but if it looks like it will be impossible for the tow to occur within a tiny piece of strata, re-run the plots with a diff seed.
          # Add points, station numbers, or both.
          if(point.style == "both") bp2 <- bp + geom_sf_text(data=tmp.sf.reg,aes(label = EID),size=pt.txt.sz,nudge_x = x.adj,nudge_y = y.adj) + 
            geom_sf(data=tmp.sf.reg,aes(shape=`Tow type`),size=pt.txt.sz) + scale_shape_manual(values = c(1,0))
          
          if(nrow(extras) > 0) 
          {
            if(point.style == "points") bp2 <- bp2 + geom_sf(data = extras, shape =24,fill = "darkorange" ,size=pt.txt.sz)
            if(point.style == "stn_num") bp2 <- bp + geom_sf_text(data=extras,aes(label = EID),size=pt.txt.sz) #text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
            if(point.style == "both" )  bp2 <- bp2 + geom_sf_text(data=extras,aes(label = EID),nudge_x = x.adj,nudge_y = y.adj,size=pt.txt.sz) +
                geom_sf(data=extras, shape =24,fill = "darkorange" ,size=pt.txt.sz)
          }
          
          if(cables==T)
          {
            cables <- rgdal::readOGR("Z:/Maps/Undersea_cables/AllKnownCables2015.shp")
            cables.sf <- st_transform(st_as_sf(cables),crs=4326)
            bp2 <- bp2 + geom_sf(data=cables.sf)
          }
          
          # And if there are any seedboxes
          if(nrow(sb) > 0) bp2 <- bp2 + geom_sf(data=sb,fill=NA)     
          
          cap <- paste("New stations (n = ",nrow(tmp.sf.reg %>% dplyr::filter(`Tow type`=="new")),")"," \n Repeat stations (n = ",
                       nrow(tmp.sf.reg %>% dplyr::filter(`Tow type`=="repeated")),")",sep="",collapse =" ")
          if(nrow(extras >0 )) cap <- paste(cap," \n Extra stations (n = ",
                                               nrow(extras),")",sep="",collapse =" ")
          sub.title <- paste("Note: The random seed was set to ",seed,sep="")
          pf <- bp2 + labs(title= paste("Survey (",bnk,"-",yr,")",sep=""),
                           subtitle = sub.title,
                           caption = cap) + coord_sf(expand=F)
          if(fig != 'dashboard') print(pf)
         
        } # # end if(!fig == "leaflet") 
        
        # Turn off the plot device if not plotting to screen
        if(!fig %in% c("screen", "leaflet",'dashboard')) dev.off()
        
        ### PLOTS BACKUP REPEATS
        if(fig=="screen") windows(11,8.5)
        
        if(seed == yr-2000){
          if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_repeat.backups.png"),width = 11, units="in", res=420,
                                height = 8.5,bg = "transparent")
          if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"_repeat.backups.pdf"),width = 11, 
                                height = 8.5,bg = "transparent")
        }
        if(!seed == yr-2000){
          if(fig =="png")   png(paste0(direct,yr,"/Survey_Design/",bnk,"/", seedlab, "/Survey_allocation-",bnk,"_repeat.backups.png"),width = 11, units="in", res=420,
                                height = 8.5,bg = "transparent")
          if(fig =="pdf")   pdf(paste0(direct,yr,"/Survey_Design/",bnk,"/", seedlab, "/Survey_allocation-",bnk,"_repeat.backups.pdf"),width = 11, 
                                height = 8.5,bg = "transparent")
        }
        
        if(fig == "leaflet"){
          require(leaflet)
          print(leaflet() %>%
                  setView(-62, 45, 5)%>%
                  addProviderTiles(provider = providers$Esri.OceanBasemap) %>%
                  addCircles(lng = Ger.tow.dat[Ger.tow.dat$STRATA %in% c("repeated", "repeated-backup"),]$X, 
                             lat = Ger.tow.dat[Ger.tow.dat$STRATA %in% c("repeated", "repeated-backup"),]$Y, 
                             label= paste0(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("repeated", "repeated-backup"),]$EID, "_", Ger.tow.dat[Ger.tow.dat$STRATA %in% c("repeated", "repeated-backup"),]$STRATA), 
                             popup =  paste0(round(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("repeated", "repeated-backup"),]$X, 4), ",", round(Ger.tow.dat[Ger.tow.dat$STRATA %in% c("repeated", "repeated-backup"),]$Y, 4))))
          
        } 
        
        if(!fig == "leaflet") {
          #browser()
          # Now to get the points and the colors all tidy here...
          tmp.sf.rpt <- tmp.sf %>% dplyr::filter(STRATA %in% c("repeated","repeated-backup"))
          tmp.sf.rpt$`Tow type` <- tmp.sf.rpt$STRATA
          # We can se the previous baseplot...
          # So what do we want to do with the points, first plots the station numbers
          if(point.style == "stn_num") bp2 <- bp + geom_sf_text(data=tmp.sf.rpt,aes(label = EID),size=pt.txt.sz) #text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
          # This just plots the points
          if(point.style == "points")  bp2 <- bp + geom_sf(data=tmp.sf.rpt,aes(shape=`Tow type`)) + scale_shape_manual(values = c(0,3))
          # Note regarding point colours. Sometimes points fall on the border between strata so it appears that they are mis-coloured. To check this,
          # run above line WITHOUT bg part to look at where the points fell and to make sure thay they are coloured correctly. It's not 
          # a coding issue, but if it looks like it will be impossible for the tow to occur within a tiny piece of strata, re-run the plots with a diff seed.
          # Add points, station numbers, or both.
          if(point.style == "both") bp2 <- bp + geom_sf_text(data=tmp.sf.rpt,aes(label = EID),size=pt.txt.sz,nudge_x = x.adj,nudge_y = y.adj) + 
            geom_sf(data=tmp.sf.rpt,aes(shape=`Tow type`)) + scale_shape_manual(values = c(0,3))
          
          if(nrow(extras) > 0) 
          {
            if(point.style == "points") bp2 <- bp2 + geom_sf(data = extras, shape =24,fill = "darkorange" )
            if(point.style == "stn_num") bp2 <- bp + geom_sf_text(data=extras,aes(label = EID),size=pt.txt.sz) #text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
            if(point.style == "both" )  bp2 <- bp2 + geom_sf_text(data=extras,aes(label = EID),nudge_x = x.adj,nudge_y = y.adj,size=pt.txt.sz) + geom_sf(data=extras, shape =24,fill = "darkorange" )
          }
          
          if(cables==T)
          {
            cables <- rgdal::readOGR("Z:/Maps/Undersea_cables/AllKnownCables2015.shp")
            cables.sf <- st_transform(st_as_sf(cables),crs=4326)
            bp2 <- bp2 + geom_sf(data=cables.sf)
          }
          #browser()
          # And if there are any seedboxes
          if(nrow(sb) > 0) bp2 <- bp2 + geom_sf(data=sb,fill=NA)     
          
          cap <- paste("Repeated stations (n = ",nrow(tmp.sf.rpt %>% dplyr::filter(`Tow type`=="repeated")),")"," \n Repeat backup stations (n = ",
                       nrow(tmp.sf.rpt %>% dplyr::filter(`Tow type`=="repeated-backup")),")",sep="",collapse =" ")
          if(nrow(extras >0 )) cap <- paste(cap," \n Extra stations (n = ",
                                               nrow(extras),")",sep="",collapse =" ")
          sub.title <- paste("Note: The random seed was set to ",seed,sep="")
          pf2 <- bp2 + labs(title= paste("Survey (",bnk,"-",yr,")",sep=""),
                            subtitle = sub.title,
                            caption = cap) + coord_sf(expand=F)
          if(fig != 'dashboard') print(pf2)
          
        }
        if(!fig %in% c("screen", "leaflet",'dashboard')) dev.off()
      } # end if(plot==T)
      
      # Now if you want to make these new fangled relief plots... source(paste(direct_fns,"Survey_design/Relief.plots.r",sep=""))
      if(relief.plots == T)  Relief.plots(Ger.tow.dat,fig = fig,digits=digits)
      
      # finally, append Ger.tow.dat into towlst
      towlst[[i]] <- Ger.tow.dat
    }# end if(bnk== "Ger")
    
  } # end for(i in 1:num.banks)
  if(fig == 'dashboard') plotly::ggplotly(pf)
  return(pf =pf) # Return the last object and the last primary figure 
} # end function