# This is a hack of a function the INLA lasses/lads use to plot their INLA model results
# Stand-alone function created by DK in Nov 2018
# Major revision to work with sf package undertaken in 2020, DK joined the party in March 2020, attempt to make this more user friendly 
# removed option add_land (we always want land...)

# Note that we need a couple of cute little functions for this to work.  Be sure to all call "addalpha" , "convert_coords", and "all.layers"
# Before running this function, but I haven't settled on a directory for them quite yet...


# Arguements:

## The general mapping inputs if you are just wanting to produce a spatial map without an INLA surface
#1:   area      The area you want to plot, this can be a custom field (see function convert_coords.R for options) or a dataframe with
#               The coordinates and the projection of those coordindates specified.  Default provides Maritime Region boundaries
#               in lat/long coordinates and a WGS84 projection.
#2:   repo      The repository from which you will pull the data.  The default is "github" (which has all the main data) 
#               option is to specify the directory you want to pull data from, likely you want to use "Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers"
#3:   c_sys     What coordinate system are you using, options are "ll"  which is lat/lon and WGS84 or "utm_zone" which is utm, you put in the zone yourself
#               for example if you want utm_20 you can enter "+init=epsg:32620" which will use utm zone 20 which is best for BoF and SS 
#               for utm_19  use +init=epsg:32619, this is best for GB, SPA3 and 6, if you have are using something else go nuts!
#4:   add_layer Do you have a layer you'd like to add to the plot.  default = and empty list which will just return a map of the area with land on it.  To add layers
#               they need to be added as a list with specific options broken out here. A complete example is
#               list(eez = 'eez' , bathy = 50, nafo = 'main',sfas = 'offshore',strata = "offshore", local.obj = NULL, ext.obj = "directory")
###  a: eez     Do you want to add the eez  Simply put eez = 'eez' in the list and it will be included (putting in anything in quotes wil work, looking for eez object in add_layer)
###  b: bathy   Do you want to add in the bathymetry, if so if you put in a number you will get a contour plot with intervals between contours of the nubmer you enter
#######         along with a continuous smooth surface. e.g. bathy = 50 gives 50 meter contour grids + underlying bathy colours.  
#               If you enter any character value (say 's') you will get just a continuous surface.
#              
#######         This now relies on NOAA bathymetry (so you need internet connection!), the finer the scale bathy you want the slower this runs.
###  c: nafo    Do you want to add nafo areas. two options, nafo = 'main' will plot the main nafo boundaries, while nafo = 'sub' will plot the subareas. not specifying nafo will plot nothing.
###  d: sfa     Do you want to add the sfa boundariesto the figure, options are sfa = "inshore", sfa="offshore", or sfa="all".  If sourcing locally point repo to correct location.
###  e:strata   Do you want to add the strata boundariesto the figure, options are strata = "inshore", strata="offshore", or strata="all".  If sourcing locally point repo to correct location.
###  f:custom   Do you have a specific object you'd like to add, this can be a csv or shapefile, you specify exactly where the
#               custom layer/PBS massing object is. custom = "Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/seedboxes"
#               If using PBSmapping this assumes that you have the data formatted properly in a csv or xlsx file and 
#               that the proection for the data is WGS84.  Default = NULL
#5: buffer      Add a buffer to the area plotted.  Default = 0 which just plots to the extent of the coordinates entered  Entering 0.05 will give approx a 5% buffer based on the
#               size of your area
#6:  direct_fns The directory that our local functions reside in, defaults to the ESS directory structure so it pulls in the stable master version of the function
#               "Y:/Offshore/Assessment/Assesment_fns/"

## The INLA related inputs, if field and mesh are not supplied these won't do anything.  You do need field and mesh if plotting an INLA model result
#8:   field    The values from the random field generated using INLA.  Default = NULL which just plots a nice map of the "area" chosen
#               Note that the field and the mesh need to be in the same projection as c_sys.
#9:   mesh     The INLA mesh that corresponds to the values of field, which gets projected properly in this function .
#               Default = NULL which results in a plot of a nice map of the 'area' chosen
#               Note that the field and the mesh need to be in the same projection as c_sys.
#10:   zlim     The range of values for the field plotted, default is  c(0,1) and this seems to be overwritten if you supply levels outside the 0-1 range.
#11:   dims     The number of X and Y values for the INLA surface.  Higher is better resolution, too high is silly. Default = c(50, 50) which is pretty low res
#12:   trans    Do you want to transform the spacing of the levels "none" (default), and "exp" (e.g. for a model with log link) is supported, but you can add more that!
#13:   clip     Do you want to clip the area plotted to some polygon?  Default = NULL which is no clipping
#14:   lvls     The number of levels you want to set up.  Default is seq(0,1,by=0.01) which is a lot of levels!
#15:  colors    What colours would you like to use for the colour ramp.  Default = c("blue","white","yellow","darkred"), will get split into more colours based on lvls
#16:  alpha     Do you want the colours to have some level of transparency.  0 = translucent, 1 = opaque, Default = 0.8

pecjector = function(area = data.frame(y = c(40,46),x = c(-68,-55),proj_sys = 4326),repo = "github",c_sys = "ll", 
                     add_layer = list(), buffer = 0,
                     direct_fns = "Y:/Offshore/Assessment/Assesment_fns/",
                     # The below control the INLA surface added to the figure.
                     field = NULL, mesh=NULL, 
                     zlim = c(0,1), dims = c(50, 50), trans= "none", clip= NULL,
                     lvls = seq(0,1,by=0.01),colors = c("blue","white","yellow","darkred"),alpha = 0.8, ...
) 
{ 
  # require(splancs) || stop("You need le package splancs, thanks!")
  # require(PBSmapping) || stop("You need PBSmapping, thanks!")
  # require(sp) || stop("You need sp, thanks!")
  # require(rgdal)  || stop("You need rgdal, thanks!")
  # require(rgeos) || stop("You need rgeos, thanks!")
  # require(raster) || stop("You need raster, thanks!")
  # require(maptools) || stop("You need maptools, thanks!")
  #require(maps) || stop("You need maps, thanks!")
  #require(mapdata)|| stop("You need mapdata, thanks!")
  # require(viridis) || stop("You need the viridis package, thanks!")
  require(marmap) || stop("You need the marmap function to get the bathymetry")
  require(sf) || stop("It's 2020. We have entered the world of sf. ")
  require(ggplot2) || stop("Install ggplot2 or else.")
  require(stars) || stop("Install stars or else.")
  require(tmaptools) || stop("Install this new maptools package, for working with sf objects")
  require(rnaturalearth) || stop("Install rnaturalearth package, this replaces maps and mapdata packages")
  require(rnaturalearthdata)|| stop("Install rnaturalearthdata package, this replaces maps and mapdata packages")
  require(rnaturalearthhires) || stop("You need rnaturalearthhires run this to install devtools::install_github('ropensci/rnaturalearthhires') ")
  require(raster)|| stop("You need raster, well you might not, depends really what you are doing... ")
  require(rgdal)|| stop("You need rgdal pal")
  require(RStoolbox) || stop ("You need RStoolbox to rasterize and reproject your bathymetry")
  require(pals) || stop("Pals is your one stop shop of colour pallettes in R, install it!")
  require(ggnewscale)
  # If you are using github then we can call in the below 3 functions needed below from github, they aren't in production currently so should work fine
  # if(repo == "github")
  # {
  #   require(RCurl)|| stop("You need RCurl or this will all unfurl!")
  #   eval(parse(text = getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R", ssl.verifypeer = FALSE)))
  #   eval(parse(text = getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R", ssl.verifypeer = FALSE)))
  #   eval(parse(text = getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/combine_shapefile_layers.R", ssl.verifypeer = FALSE)))
  # } # end if(repo == "github")
  # 
  # # If getting the data from a local source...
  # if(repo == "local")
  # {
  ## always pull from local... these should be in the same location as pectinid_projector, right?  
  source(paste(direct_fns,"Maps/convert_coords.R",sep="")) 
  source(paste(direct_fns,"Maps/add_alpha_function.R",sep="")) 
  source(paste(direct_fns,"Maps/combine_shapefile_layers.R",sep="")) 
  # } # end if(repo == "local")
  # Don't do this if the field and mesh lengths differ.
  if(!is.null(field)) stopifnot(length(field) == mesh$n) 
  
  
  # Now if you set the c_sys to "ll" that means "ll" and WGS84, so explicitly set this now.
  if(c_sys == "ll") c_sys <- 4326 # 32620 is UTM 20, just FYI 
  
  # Now we need to get our ylim and xlim using the convert.coords function
  # Get our coordinates in the units we need them, need to do some stick handling if we've entered specific coords above
  # This the case in which we enter numbers as our coordinate system
  if(is.data.frame(area)) coords <- convert.coords(plot.extent = area[,c("x","y")],in.csys = area$proj_sys[1],out.csys = c_sys,bbox.buf = buffer,make.sf=T)
  # This is the case when we put a name in and let convert.coords sort it out.
  if(!is.data.frame(area)) coords <- convert.coords(plot.extent = area,out.csys = c_sys,bbox.buf = buffer, make.sf=T)
  # All I need from the coords call above is the bounding box.
  b.box <- coords$b.box
  # Get the limits of the bounding box
  xlim <- as.numeric(c(st_bbox(b.box)$xmin,st_bbox(b.box)$xmax))
  ylim <- as.numeric(c(st_bbox(b.box)$ymin,st_bbox(b.box)$ymax))
  
  # Get the spatial coordinates correct for the boxes, likely they are already in the Lat/Long WGS84 format, but might not be...
  # Note that this requires the boxes are already spatial polygons and have a coordinate reference system.
  #if(!is.null(add_obj)) add_obj <- st_transform(add_obj,crs = c_sys)
  #browser()
  # ID what layers we are looking for.
  layers <- names(add_layer)
  # If we are going to add the EEZ do this...
  if(any(layers == 'eez'))
  {
    # if we already have the full eez in the global environment we don't need to reload it, we do need to sub-set it and project it though
    if(repo == 'github')
    {
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to the temp directory you created above
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/EEZ/EEZ.zip", temp)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # Now read in the shapefile
      eez.all <- st_read(paste0(temp2, "/EEZ.shp"))
    } else { # end if(repo == 'github' )
      loc <- paste0(repo,"Data/Maps/approved/GIS_layers/EEZ")
      eez.all <- st_read(loc)
    } # end the else

    # We then need to transform these coordinates to the coordinates of the eez data
    eez.bbox <- b.box %>% st_transform(st_crs(eez.all))
    # Then intersect the coordiates so we only plot the part of the eez we want
    eez <- st_intersection(eez.all,eez.bbox)
    # and now transform this eez subset to the proper coordinate system. If there is an eez in the picture....
    if(!is.null(eez)) eez <- eez %>% st_transform(c_sys)
  } # end if(!is.null(add_EEZ)) 

  # Now grab the land, note we're using the rnaturalearth package now
  # We also want to get the land
  land.all <- ne_countries(scale = "large", returnclass = "sf",continent = "North America")

  # f we are lat/lon and WGS84 we don't need to bother worrying about clipping the land (plotting it all is fine)
  if(c_sys == "+init=epsg:4326") land.sf <- st_intersection(land.all, b.box)
  # If we need to reproject do it...
  if(c_sys != "+init=epsg:4326") 
  {
    t.bbox <- st_transform(b.box,crs = st_crs(land.all))
    land.sf <- st_intersection(land.all,t.bbox)
    land.sf <- st_transform(land.sf,crs=c_sys)
  } # end if(c_sys != "+init=epsg:4326") 
  
  
  # The bathymetry comes in now as a raster.  Thanks to sf wonderfulness we can handle this now
  if(any(layers == 'bathy')) # This would need done everytime as the boundng box could change for each call
  {
    # Set the maximum depth that you show in the bathymetry.
    if(is.na(add_layer$bathy[2])) add_layer$bathy[2] <- -500 # If you didn't set a maximum layer depth then it defaults to -500
    # I need coordinates for the bathy
    bath.box <- st_bbox(b.box)
    if(st_crs(b.box)[1]$epsg != "4326") bath.box <- b.box %>% st_transform(crs=4326) %>% st_bbox() 
      
    # The bathymetry data is given in NOAA as Lat/Lon WGS84 according to NOAA's website.  https://www.ngdc.noaa.gov/mgg/global/
    # Based on a figure in their paper seems the contours are meters https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0073051
    # This is a little slow when doing the whole of the Maritimes (about 15 seconds)
    bathy.org <- getNOAA.bathy(lon1 = bath.box$xmin ,bath.box$xmax,lat1 = bath.box$ymin,lat2=bath.box$ymax,resolution =1)
   
    bathy <- marmap::as.raster(bathy.org)
    #Now if we want smooth contours we do this...
    # For the continuous colours everything deeper than specificed (default = 500m) will be the same colour, just tidies up the plots.
    bathy.s <- bathy.org
    bathy.s[which(bathy.s < -abs(add_layer$bathy[2]))] <- -abs(add_layer$bathy[2])
    bathy.s[which(bathy.s > 0)] <- 0
    bathy.s <- marmap::as.raster(bathy.s)
    # Now if the epsg isn't 4326 I need to reproject my raster, which is a wicked pain (really we only need to do this for UTMs, but whatevs.)
    if(c_sys != 4326)
    {
      bathy.st <- st_as_stars(bathy.s)
      b.new <- b.box %>% st_transform(c_sys) %>% st_bbox() %>% st_as_stars() 
      # Now warp the existing raster to the new projection grid.
      bathy.smooth <- bathy.st %>% st_warp(b.new)
    } else { bathy.smooth <- st_as_stars(bathy.s)} # If we don't reproject it's easy peasy...
    
    # Are we adding in the bathy contour lines, if so do all this fun.
    if(is.numeric(add_layer$bathy[1]))
    {
      # Now if the epsg isn't 4326 I need to reproject my raster, which is a wicked pain (really we only need to do this for UTMs, but whatevs.)
      if(c_sys != 4326)
      {
        # Now I may need to make a new grid to project to, base it on the size of the bounding box
        b.new <- as(b.box,"Spatial")
        # Then make this a pretty hi resolution raster
        ras <- raster(ncol=500,nrow=500,crs = crs(b.new))
        # Give it the correct extent
        extent(ras) <- extent(b.new)
        # And make it a raster
        b.ras <- rasterize(b.new,ras)
        # I need a new grid that is a raster object
        re.proj.bathy <- projectRaster(bathy,b.ras)
        re.proj.bathy <- projectRaster(bathy,b.ras) # For some reason this doesn't always work the first time you call it during an R session, but works when you do a second time?
        # Now I need to try and fortify this raster, I need to have the RStoolbox to fortify the raster 
        bathy.gg <- fortify(re.proj.bathy)
      } else { bathy.gg <- fortify(bathy)}
      # define the contour breaks, only plot contours between 0 and everything deeper than specificed (default = 500m) .
      bathy.breaks <- seq(0, -abs(add_layer$bathy[2]), -abs(add_layer$bathy[1]))
  
      }
      
     
  } # end if(any(layers == 'bathy'))
  
  # If you want to add the NAFO division, the autoritaive versions are on the web so when we say "repo = 'github'", for NAFO this is actually going
  # to NAFO's website to get them.  We have a version of these saved locally as well which is accessed when "repo = 'local'"
  #browser()
  if(any(layers == 'nafo'))
  {
    # If they don't already exist and our repo is github and we just want the main nafo division go get them from online
    if(repo == 'github' && add_layer$nafo == "main")
    {
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to there
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/NAFO/Divisions.zip", temp)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # This pulls in all the layers from the above location
      nafo.divs <- all.layers(temp2,make.sf=T)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      nafo.divs <- st_transform(nafo.divs,c_sys)
      #trim to bbox
      nafo.divs <- st_intersection(nafo.divs, b.box)
    } # if(repo == 'github' && !exists("nafo.divs"))
    
    # if we want the main divisions from local
    if(repo != 'github' && add_layer$nafo == "main")
    {
      loc <- paste0(repo,"/NAFO/Divisions")
      nafo.divs <- all.layers(loc,make.sf=T)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      nafo.divs <- st_transform(nafo.divs,c_sys)
      #trim to bbox
      nafo.divs <- st_intersection(nafo.divs, b.box)
    } # end if(repo == 'local' && !exists("nafo.divs") && add_nafo = "main")
    
    # Now if we want the nafo sub-areas we do this, for the locals
    if(repo == 'github' && !exists("nafo.subs") && add_layer$nafo == "sub")
    {
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to the temp directory you created above
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/NAFO/Subareas.zip", temp)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # This pulls in all the layers from the above location
      nafo.subs <- all.layers(temp2,make.sf=T)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      nafo.subs <- st_transform(nafo.subs,c_sys)
      #trim to bbox
      nafo.subs <- st_intersection(nafo.subs, b.box)
    } # end if(add_sfas != "offshore")
    
    # Now if we want the nafo sub-areas we do this, for the locals
    if(repo != 'github' && add_layer$nafo == "sub")
    {
      # Now if we want the nafo sub-areas we do this...
      loc <- paste0(repo,"/NAFO/Subareas")
      nafo.subs <- all.layers(loc,make.sf=T)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      nafo.subs <- st_transform(nafo.subs,c_sys)
      #trim to bbox
      nafo.subs <- st_intersection(nafo.subs, b.box)
    } # end if(repo == 'local' && !exists("nafo.subs") && add_nafo = "main")
    
  } # end if(add_nafo != "no")
  
  
  # Now do we want to add in the SPA and SFA's for the region, this is using the approve Inshore polygons that Leslie Nasmith developed in 2014, these are
  # all NAD83 lat/lon's.  There are a bunch of shapefiles for inshore this this isn't always speedy
  if(any(layers == 'sfa')) 
  {
    if(repo == 'github')
    {
      if(add_layer$sfa != "offshore")
      {
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore/inshore.zip", temp)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        
        # This pulls in all the layers from the above location
        inshore.spa <- all.layers(temp2,make.sf=T)
        
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        inshore.spa  <- st_transform(inshore.spa,c_sys)
        #trim to bbox
        inshore.spa <- st_intersection(inshore.spa, b.box)
        
      } # end if(add_sfas != "offshore")
      
      if(add_layer$sfa != "inshore" && !exists("offshore.spa"))
      {
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore/offshore.zip", temp)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        
        # This pulls in all the layers from the above location
        offshore.spa <- all.layers(temp2,make.sf=T)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        offshore.spa  <- st_transform(offshore.spa,c_sys)
        #trim to bbox
        # This is a hack to clean up maps objects in R, if you ever see the error
        #"TopologyException: Input geom 1 is invalid: Self-intersection at or near point"
        # It is due to cleans up issues with the polygons coming from maps in this case, but any "bad" polygons get taken care of.
        offshore.spa <- st_simplify(offshore.spa,dTolerance = 0.00001)
        # This really should be done on projected data not Lat/Lon data, but this does the trick for our purposes.  If using this for something
        # more than simply trying to draw some land we might want to do something more complex.
        offshore.spa <- st_buffer(offshore.spa,dist = 0)
        offshore.spa <- st_intersection(offshore.spa, b.box)
      } # end if(add_sfas != "offshore")  
      
    }# end if(repo = 'github')
    
    # Now if you aren't using github do this...
    if(repo != 'github')
    {
      if(add_layer$sfa != "offshore")
      {
        loc <- paste0(repo,"inshore")
        inshore.spa <- all.layers(loc,make.sf=T)
        
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        inshore.spa  <- st_transform(inshore.spa,c_sys)
        #trim to bbox
        inshore.spa <- st_intersection(inshore.spa, b.box)
      } # end if(detailed != "offshore")
      if(add_layer$sfa != "inshore" )
      {
        loc <- paste0(repo,"offshore")
        # This pulls in all the layers from the above location
        offshore.spa <- all.layers(loc,make.sf=T)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        offshore.spa  <- st_transform(offshore.spa,c_sys)
        #trim to bbox
        offshore.spa <- st_intersection(offshore.spa, b.box)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
    } # end if(repo == 'local')
  } # end if(!is.null(add_sfas)) 
  #browser()
  # Now we do the same thing for the strata
  if(any(layers == 'strata')) 
  {
    if(repo == 'github')
    {
      if(add_layer$strata != "offshore")
      {
      
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_survey_strata/inshore_survey_strata.zip", temp)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        #browser()
        # This pulls in all the layers from the above location, and puts some metadata in there matching offshore structure
        inshore.strata <- all.layers(temp2,make.sf=T,make.polys=F)
        inshore.strata$Strt_ID <- as.character(900:(length(inshore.strata$ID)+899))
        inshore.strata$col <- cividis(nrow(inshore.strata))
        inshore.strata$ID <- inshore.strata$ET_ID
        inshore.strata <- inshore.strata %>% dplyr::select(Strt_ID,ID,col)
        
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        inshore.strata  <- st_transform(inshore.strata,c_sys)
        #trim to bbox
        #inshore.strata <- st_intersection(inshore.strata, b.box)
      } # end if(add_strata != "offshore")
      
      if(add_layer$strata  != "inshore")
      {
        # Note we only do this if there is no offshore.strata object already loaded, this will really speed up using this function multiple times as you only will load these data once.
        # The only problem with this would be if offshore strata was loaded as an object but it wasn't the offshore strata we wanted!

        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore_survey_strata/offshore_survey_strata.zip", temp)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        #browser()
        # This pulls in all the layers from the above location
        offshore.strata <- all.layers(temp2,make.sf=T,make.polys=F)
        #browser()
        # Now transform all the layers in the object to the correct coordinate system
        offshore.strata  <- st_transform(offshore.strata,c_sys) 
        offshore.strata <- offshore.strata %>% dplyr::select(Strt_ID,ID,col)
        
        # Trimming these to the bbox always trips up so just skip it
        #offshore.strata <- st_intersection(offshore.strata, b.box)
        
      } # end if(add_strata != "offshore")  
      
    }# end if(repo = 'github')
    #browser()
    # Now if you aren't using github do this...
    if(repo != 'github')
    {
      if(add_layer$strata  != "offshore")
      {
        loc <- paste0(repo,"inshore_survey_strata")
        # This pulls in all the layers from the above location, and puts some metadata in there matching offshore structure
        inshore.strata <- all.layers(temp2,make.sf=T,make.polys=F)
        inshore.strata$Strt_ID <- as.character(900:(length(inshore.strata$ID)+899))
        inshore.strata$col <- cividis(nrow(inshore.strata))
        inshore.strata$ID <- inshore.strata$ET_ID
        inshore.strata <- inshore.strata %>% dplyr::select(Strt_ID,ID,col)
      } # end if(detailed != "offshore")
      if(add_layer$strata  != "inshore")
      {
        loc <- paste0(repo,"offshore_survey_strata")
        # This pulls in all the layers from the above location
        offshore.strata <- all.layers(loc,make.sf=T,make.polys=F)
  
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        offshore.strata  <- st_transform(offshore.strata,c_sys) # Convert back
        offshore.strata <- offshore.strata %>% dplyr::select(Strt_ID,ID,col)
        #trim to bbox
        #offshore.strata <- st_intersection(offshore.strata, b.box)
        
      } # end if(detailed != "offshore")
    } # end if(repo == 'local')
    if(add_layer$strata == 'offshore') final.strata <- offshore.strata
    if(add_layer$strata == 'inshore') final.strata <- inshore.strata
    if(add_layer$strata == 'all') final.strata <- rbind(offshore.strata,inshore.strata)
    # I need to re-order the strata so the colour render correctly...
    col.codes <- final.strata[order(final.strata$Strt_ID),]
  } # end if(!is.null(add_strata)) 
  
  # Here you can add a custom PBSmapping object or shapefile here
  if(any(layers == 'custom'))
  {
    # If it is an xls or a csv we assume we have a PBSmapping object
    if(grepl(".xls",add_layer$custom) || grepl(".csv",add_layer$custom))
    {
      custom <- NULL
      if(grepl(".csv",add_custom)) temp <- read.csv(add_custom)
      if(grepl(".xls",add_custom)) temp <- read_excel(add_custom,sheet=1) # This will only pull in the first sheet, don't get fancy here
      temp <- as.PolySet(temp,projection = "LL") # I am assuming you provide Lat/Lon data and WGS84
      temp <- PolySet2SpatialLines(temp) # Spatial lines is a bit more general (don't need to have boxes closed)
      custom <- st_as_sf(temp)
    } else { custom <- all.layers(add_layer$custom,make.sf=T)}# If it doesn't then we assume we have a shapefile, if anything else this won't work.
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      custom  <- st_transform(custom,c_sys)
      #trim to bbox
      custom <- st_intersection(custom, b.box)
  } # end  if(!is.null(add_custom))
  #browser()
  
  # If we have a field to plot, i.e. an INLA object and mesh
  if(!is.null(field))
  {
    # Project the values appropriately for the data
    projec = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
    field.projec = inla.mesh.project(projec, field)
    # If you want to clip the data to some coordinates/shape this is where that happens.
    if(!is.null(clip)) 
    {
      pred.in <- inout(projec$lattice$loc,clip) 
      field.projec[!pred.in] <- NA
    } # end if(!is.null(clip)) 
    
    #windows(11,11)
    # Transform the axis?
    if(trans== "exp") arg.list <- list(at=lvls,labels=round(exp(lvls)))
    if(trans == "none") arg.list <- list(at=lvls,labels=lvls)
    #browser()

  } # end if(!is.null(field))
  #browser()
    pect_plot <- ggplot() + 
      geom_sf(data=b.box, fill=NA) +
      theme_minimal() + xlab("") + ylab("")+
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) 
    #browser()
    
    if(exists("bathy.smooth")) pect_plot <- pect_plot + geom_stars(data=bathy.smooth) + scale_fill_gradientn(colours = rev(brewer.blues(100)),guide = 'colourbar')  +  theme(legend.position = "none")
    if(exists("bathy.gg")) pect_plot <- pect_plot + geom_contour(data=bathy.gg, aes(x=x, y=y, z=layer), breaks=bathy.breaks)  +  theme(legend.position = "none")
    if(exists("nafo.divs")) pect_plot <- pect_plot + geom_sf(data=nafo.divs, fill=NA)
    if(exists("nafo.subs")) pect_plot <- pect_plot + geom_sf(data=nafo.subs, fill=NA)
    if(exists("inshore.spa")) pect_plot <- pect_plot + geom_sf(data=inshore.spa, fill=NA)
    if(exists("offshore.spa")) pect_plot <- pect_plot + geom_sf(data=offshore.spa, fill=NA)
    if(exists("final.strata")) pect_plot <- pect_plot + new_scale("fill")  + geom_sf(data=final.strata,aes(fill= Strt_ID)) + scale_fill_manual(values = col.codes$col)+ theme(legend.position = "none")
    if(exists("eez")) pect_plot <- pect_plot + geom_sf(data=eez, colour="firebrick")
    if(exists("land.sf")) pect_plot <- pect_plot + geom_sf(data=land.sf, fill="grey")   
    
    # Some finishing touches...
    pect_plot <- pect_plot + coord_sf(xlim = xlim,ylim=ylim)
    print(pect_plot)
    return(pect_plot = pect_plot)
 } # end function
