# This is a hack of a function the INLA lasses/lads use to plot their INLA model results
# Stand-alone function created by DK in Nov 2018
# Major revision to work with sf package undertaken in 2020, DK joined the party in March 2020, attempt to make this more user friendly 
# removed option add_land (we always want land...)

# Note that we need a couple of cute little functions for this to work.  Be sure to all call "addalpha" , "convert_coords", and "all.layers"
# Before running this function, but I haven't settled on a directory for them quite yet...

# Arguements:

## The general mapping inputs if you are just wanting to produce a spatial map without an INLA surface

#1: gg.obj     If you have an existing gg.object that you want to load in as a base map you can pull that in here and just add additional components to that
###              using the pectinid calls you want to use.

#2: area       The area you want to plot, this can be a custom field (see function convert_coords.R for options) or a list with
###               the coordinates and the projection of those coordindates specified.  Default provides Maritime Region boundaries
###               in lat/long coordinates and a WGS84 projection.

#3: plot       Do you want to display the plot.  Default = T.  if plot= F you will just get a ggplot object useful if just making it a baselayer.

#4: repo       The repository from which you will pull the data.  The default is "github" (which has all the main data) 
###               option is to specify the directory you want to pull data from, likely you want to use "Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers"

#5: c_sys      What coordinate system are you using, options are "ll"  which is lat/lon and WGS84 or "utm_zone" which is utm, you put in the zone yourself
###               for example if you want utm_20 you can enter "+init=epsg:32620" which will use utm zone 20 which is best for BoF and SS 
###               for utm_19  use +init=epsg:32619, this is best for GB, SPA3 and 6, if you have are using something else go nuts!

#6: buffer      Add a buffer to the area plotted.  Default = 0 which just plots to the extent of the coordinates entered  Entering 0.05 will give approx a 5% buffer based on the
###               size of your area

#7: direct_fns  The directory that our local functions reside in, defaults to the ESS directory structure so it pulls in the stable master version of the function
###               "Y:/Offshore/Assessment/Assesment_fns/"

#################################### LAYER OPTIONS#################################### LAYER OPTIONS#################################### LAYER OPTIONS

#6: add_layer   Do you have a layer you'd like to add to the plot.  default = and empty list which will just return a map of the area with land on it.  To add layers
###               they need to be added as a list with specific options broken out here. A complete example is
###               list(eez = 'eez' , bathy = 50, nafo = 'main',sfas = 'offshore',survey = "offshore", s.labels = 'offshore')
####  a: eez        Do you want to add the eez  Simply put eez = 'eez' in the list and it will be included (putting in anything in quotes wil work, looking for eez object in add_layer)

####  b: bathy      Do you want to add in the bathymetry, this can be a fairly complex call as it has 3 options you want to specify
#######              The first is a number giving the depth contours you want.  50 tends to look good. If you only specify this 
#######              you will get both the smooth surface and the contours with a maximum depht of 500 m
#######              The second option is optional, it one of 'both' which plots smooth surface and contours, 's' which plots a smooth bathy surface, 
#######              or 'c' which only plots the depth contour lines
#######              The final is the maximum depth you want for the contours, you can leave this out, defaut is 500 meters which looks good.
#######              bathy = c(50,'both',500) or bathy = 50 will plot smooth surface + contour lines at a 50 meter intervals and 500 is the maximum depth
#######              bathy = c(50,'s') or c(50,'s',500) will print the smooth only with max deth of 500 meters. 
#######              This now relies on NOAA bathymetry (so you need internet connection!), the finer the scale bathy you want the slower this runs.

####  c: nafo       Do you want to add nafo areas. two options, nafo = 'main' will plot the main nafo boundaries, 
#######              while nafo = 'sub' will plot the subareas. not specifying nafo will plot nothing.

####  d: sfa        Do you want to add the sfa boundariesto the figure, options are sfa = "inshore", sfa="offshore", or sfa="all".  If sourcing locally point repo to correct location

####  e: survey     Do you want to add the strata boundariesto the figure, requires 2 arguments, first is the area you want to plot options are 'inshore', 'offshore', or 'all'.
#######              Second argument is whether you want the full strata plotted (with colours) or just an outline of the strata, so either 'detailed', or 'outline'.  
#######              So survey = c("all", "detailed") will plot all survey extents and every strata boundary there is. survey = c("inshore","outline") will just plot the outline of the inshore surveys.
#######               If sourcing locally point repo to correct location.

####  f: s.labels  Add labels to the figures?  Several options here I need to lay out.  
#######              s.labels = 'offshore' - Puts basic labels for offshore areas - Good for broad overview of offshore
#######              s.labels = 'inshore' - Puts basic labels for inshore areas - Good for broad overview of inshore
#######              s.labels = 'all' - Puts the above two sets of labels - Good for broad overview of everywhere
#######              the next two options only work if you are looking at a zoomed in version of inshore, way too much detail for a figure covering everywhere.
#######              s.labels = 'ID' - Puts in detailed inshore labels for the areas
#######              s.labels = "IDS" - Puts in detailed inshore survey strata labesl for all the inshore areas

####  g: custom     Do you have a specific object you'd like to add, this can be a csv or shapefile, you specify exactly where the
#######             custom layer/PBS massing object is. custom = "Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/seedboxes"
#######             If using PBSmapping this assumes that you have the data formatted properly in a csv or xlsx file and 
#######             that the proection for the data is WGS84.  Default = NULL

#### h:  scale.bar  Do you want to add a scale bar to the figure, it also pops in a fancy north arrow.  
#######             To add it you specify what corner you want it in and optionally it's size as a second option.
#######             scale.bar = 'bl' will put it in bottom left (options are bl,bc,br,tl,tc,tr) 
#######             scale.bar = c('bl',0.5) will put in a scale bar that is half the length of the figure in the bottom left corner.

#################################### INLA OPTIONS#################################### INLA OPTIONS#################################### INLA OPTIONS
## The INLA related inputs, if field and mesh are not supplied these won't do anything.  You do need field and mesh if plotting an INLA model result
#7: add_inla   Similar structure to the add_layer, this allows an INLA field to be added to the figure.  To do this you need a INLA random field and mesh
#                at a minimum.  If either field or mesh is NULL this won't plot anything. Other options should enable a plot to be produced based on default settings.

###  a: field   The output random field (or whatever INLA field you want to plot).  This will be converted to a shapefile with correct projection attributes

###  b: mesh    The INLA mesh that corresponds to the values of field, which gets projected properly in this function .
######          Note that the field is assumed to be in the same projection as the mesh.

###  c: range   The range of values for the field plotted, default uses the full range of the data.

###  d: clip    Do you want to clip the area plotted to something.  Default is no clipping, there are several options you have here...
######            clip = "D/Location/of/a/shape/file.shp" will read in a shapefile as an sf object.
######            clip = sf_or_sp_object will take in your sf or sp object and clip the field to this
######            clip = list(y = c(40,46),x = c(-68,-55),crs = 4326) will grab your coordinates
######            clip = "D/Location/of/a/shape/file.shp" will read in a shapefile as an sf object.

###  e: dims    The number of X and Y values for the INLA surface.  Higher is better resolution, higher = slow. Default dims = c(50, 50) which is pretty low res but quick
###  f: scale   Do you want to use a continuous scale, or a manual scale with categories.  The nature of that scale is controled by the other options in this list
######            What colours would you like to use for the colour ramp.  Default = NULL which will plot a viridis based ramp using geom_gradientn() and pecjector defaults
######            If not NULL it is this...list(scale = 'c',palette = viridis::viridis(100,begin=0,direction=1,option='D'), limits = c(0,1), breaks =seq(0,1,by=0.05),alpha=0.8)
######            Each of these fields is NULL able (i.e. you only need to specify what you want and let pecjector handle the rest)
######            $scale = list(scale = 'discrete',...) # if you want a discrete scale add scale = 'discrete', if not specified we get a continuous scale.
######            $scale = list(palette = viridis(100),...) Here is where you specify your colour palette and number of colours.  Number of colours should be > the number of breaks!
######            $scale = list(breaks = seq(0,1,by=0.05),...) Where do you want to put breaks, this is really for your legend mainly as the colour scheme is much more flexible.
######            $scale = list(limits = c(0,1),...) What are upper and lower bounds of data covered by your your colour palette, 
######            $scale = list(alpha = 0.8,...)     Do you want the colours to have some level of transparency.  0 = translucent, 1 = opaque.
######            $scale =  list(leg.name = "Bill",...) What do you want the name of your legend to be.  

# A working full example of a call to this function that should work without any modification as long
# as you are connected to the NAS drive...
#pecjector(gg.obj = NULL, area = "BBn",plot = T, 
#          repo = 'local',c_sys = 32619, buffer =1000,direct_fns = "Y:/Offshore/Assessment/Assesment_fns/",
#          add_layer = list(eez = 'eez', bathy = 50, nafo = 'main',sfa = 'offshore',survey = c('offshore','detailed'),s.labels = 'offshore',scale.bar = 'bl',scale.bar = c('bl',0.5)))

########## If you had an INLA layer, a full call to that would be to add this to the above..
#         add_inla(field = inla.field.obj, mesh = mesh.inla.obj,range = c(0,1),clip = sf.obj,dims = c(50,50),
#         scale= list(scale = 'discrete', palette = 'viridis::viridis(100)', breaks = seq(0,1, by = 0.05), limits = c(0,1), alpha = 0.8,leg.name = "Ted"))

pecjector = function(gg.obj = NULL,area = list(y = c(40,46),x = c(-68,-55),crs = 4326), plot = T,
                     repo = "github",c_sys = "ll",  buffer = 0, direct_fns = "Y:/Offshore/Assessment/Assesment_fns/",
                     # Controls what layers to add to the figure (eez, nafo, sfa's, labels, )
                     add_layer = list(),
                     # The below control the INLA surface added to the figure, the col subgroup controls what the field looks like
                     add_inla = list(), # list(scale = 'discrete',palette = viridis::viridis(100,begin=0,direction=1,option='D'), limits = c(0,1), breaks =seq(0,1,by=0.05),alpha=0.8)
                     ...) 
{ 
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
  require(pals) || stop("Pals package is needed, it is your one stop shop of colour pallettes in R, install it!")
  require(ggnewscale)  || stop ("Please install ggnewscale...If you want multiple colour ramps on one ggplot, you want ggnewscale :-)")
  require(ggspatial) ||stop ("Please install ggspatial which is needed to include the scale bar")
  #require(rmapshaper) || stop ("Please install rmapshaper, has a nice means of cleaning up that ugly German survey figure...")
  #require(scales) || stop ("Please install scales, needed to make legend in discrete ggplot not be in scientific notation")
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
  # This is needed to spin the field projection to be oriented how GIS wants them, unclear why it is weird like this!
  rotate <- function(x) t(apply(x, 2, rev)) 
  options(scipen=999)# Avoid scientific notation

  # Don't do this if the field and mesh lengths differ.
  if(!is.null(add_inla$field)) stopifnot(length(add_inla$field) == add_inla$mesh$n) 
  
  # If we set area up as a ggplot we don't need to do any of this fun.
  # } # end if(repo == "local")
  # Now if you set the c_sys to "ll" that means "ll" and WGS84, so explicitly set this now.
  if(c_sys == "ll") c_sys <- 4326 # 32620 is UTM 20, just FYI 
  
  # Now we need to get our ylim and xlim using the convert.coords function
  # Get our coordinates in the units we need them, need to do some stick handling if we've entered specific coords above
  # This the case in which we enter numbers as our coordinate system
  if(is.data.frame(area)) coords <- convert.coords(plot.extent = area[,c("x","y")],in.csys = area$crs,out.csys = c_sys,bbox.buf = buffer,make.sf=T)

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
      loc <- paste0(repo,"/EEZ")
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
  if(c_sys == "4326") land.sf <- st_intersection(land.all, b.box)
  # If we need to reproject do it...
  if(c_sys != "4326") 
  {
    t.bbox <- st_transform(b.box,crs = st_crs(land.all))
    land.sf <- st_intersection(land.all,t.bbox)
    land.sf <- st_transform(land.sf,crs=c_sys)
  } # end if(c_sys != "+init=epsg:4326") 
  
  if(!is.null(add_layer$bathy))
  {
  # The bathymetry comes in now as a raster.  Thanks to sf wonderfulness we can handle this now
  if(any(layers == 'bathy')) # This would need done everytime as the boundng box could change for each call
  {
    # If you have not specified teh type of surface you want then you get both
    if(is.na(add_layer$bathy[2])) add_layer$bathy[2] <- 'both' 
    #If you didn't set a maximum layer depth then it defaults to -500
    if(is.na(add_layer$bathy[3])) add_layer$bathy[3] <- -500 
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
    if(add_layer$bathy[2] == 'both' || add_layer$bathy[2] == 's' )
    {
      bathy.s <- bathy.org
      bathy.s[which(bathy.s < -abs(as.numeric(add_layer$bathy[3])))] <- -abs(as.numeric(add_layer$bathy[3]))
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
    } # end if(add_layer$bathy[2] == 'both' || add_layer$bathy[2] == 's' )
    
    # Are we adding in the bathy contour lines, if so do all this fun.
    if(add_layer$bathy[2] == 'both' || add_layer$bathy[2] == 'c' )
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
      bathy.breaks <- seq(0, -abs(as.numeric(add_layer$bathy[3])), -abs(as.numeric(add_layer$bathy[1])))
      }

  } # end if(any(layers == 'bathy'))
  } # end the is.null()
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
        # This is a hack to clean up maps objects in R, if you ever see the error
        #"TopologyException: Input geom 1 is invalid: Self-intersection at or near point"
        # It is due to cleans up issues with the polygons coming from maps in this case, but any "bad" polygons get taken care of.
        offshore.spa <- st_simplify(offshore.spa,dTolerance = 0.00001)
        # This really should be done on projected data not Lat/Lon data, but this does the trick for our purposes.  If using this for something
        # more than simply trying to draw some land we might want to do something more complex.
        offshore.spa <- st_buffer(offshore.spa,dist = 0)
        #trim to bbox
        offshore.spa <- st_intersection(offshore.spa, b.box)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
    } # end if(repo == 'local')
  } # end if(!is.null(add_sfas)) 
  #browser()
  # Now we do the same thing for the strata
  if(any(layers == 'survey')) 
  {
    if(repo == 'github')
    {
      if(add_layer$survey[1] != "offshore")
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
        inshore.strata <- st_buffer(inshore.strata,dist=0)
        #trim to bbox
        inshore.strata <- st_intersection(inshore.strata, b.box)
      } # end if(add_strata != "offshore")
      
      if(add_layer$survey[1]  != "inshore")
      {
        # Note we only do this if there is no offshore.strata object already loaded, this will really speed up using this function multiple times as you only will load these data once.
        # The only problem with this would be if offshore strata was loaded as an object but it wasn't the offshore strata we wanted!
        
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        if(add_layer$survey[2] == 'detailed') download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore_survey_strata/offshore_survey_strata.zip", temp)
        if(add_layer$survey[2] == 'outline') download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/survey_boundaries/survey_boundaries.zip", temp)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        #browser()
        # This pulls in all the layers from the above location
        offshore.strata <- all.layers(temp2,make.sf=T,make.polys=F)
        #browser()
        # Need to add a couple of layers if we are just pulling in the survey_boundaries polygons
        if(add_layer$survey[2] == 'outline') 
        {
          offshore.strata$Strt_ID <- as.character(1:nrow(offshore.strata))
          offshore.strata$col <- NA
        }
        # Now transform all the layers in the object to the correct coordinate system
        offshore.strata  <- st_transform(offshore.strata,c_sys) 
        offshore.strata <- offshore.strata %>% dplyr::select(Strt_ID,ID,col)
        offshore.strata <- st_buffer(offshore.strata,dist=0)
        # Trimming these to the bbox always trips up so just skip it
        offshore.strata <- st_intersection(offshore.strata, b.box)
        
      } # end if(add_strata != "offshore")  
      
    }# end if(repo = 'github')
    #browser()
    # Now if you aren't using github do this...
    if(repo != 'github')
    {
      if(add_layer$survey[1]  != "offshore")
      {
        loc <- paste0(repo,"inshore_survey_strata")
        # This pulls in all the layers from the above location, and puts some metadata in there matching offshore structure
        inshore.strata <- all.layers(loc,make.sf=T,make.polys=F)
        inshore.strata$Strt_ID <- as.character(900:(length(inshore.strata$ID)+899))
        inshore.strata$col <- cividis(nrow(inshore.strata))
        inshore.strata$ID <- inshore.strata$ET_ID
        inshore.strata <- inshore.strata %>% dplyr::select(Strt_ID,ID,col)
        inshore.strata <- st_buffer(inshore.strata,dist=0)
        inshore.strata <- st_intersection(inshore.strata, b.box)
      } # end if(detailed != "offshore")
      if(add_layer$survey[1]  != "inshore")
      {
        if(add_layer$survey[2] == 'detailed') loc <- paste0(repo,"offshore_survey_strata")
        if(add_layer$survey[2] == 'outline') loc <- paste0(repo,"survey_boundaries")
        
        # This pulls in all the layers from the above location
        offshore.strata <- all.layers(loc,make.sf=T,make.polys=F)
 
        # Need to add a couple of layers if we are just pulling in the survey_boundaries polygons
        if(add_layer$survey[2] == 'outline') 
        {
          offshore.strata$Strt_ID <- as.character(1:nrow(offshore.strata))
          offshore.strata$col <- NA
        }
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        offshore.strata  <- st_transform(offshore.strata,c_sys) # Convert back
        offshore.strata <- offshore.strata %>% dplyr::select(Strt_ID,ID,col)
        offshore.strata <- st_buffer(offshore.strata,dist=0)
        #trim to bbox
        offshore.strata <- st_intersection(offshore.strata, b.box)
        
      } # end if(detailed != "offshore")
    } # end if(repo == 'local')
    if(add_layer$survey[1] == 'offshore') final.strata <- offshore.strata
    if(add_layer$survey[1] == 'inshore') final.strata <- inshore.strata
    if(add_layer$survey[1] == 'all') final.strata <- rbind(offshore.strata,inshore.strata)
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
  
  # Do we want to add the labels to the figure he's what ya gotta do...
  if(any(layers == 's.labels')) 
  {
    if(repo == 'github')
    {
      temp <- tempfile()
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/labels/labels.zip", temp)
      # Download this to the temp directory you created above
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      s.labels <- all.layers(temp2,make.sf=T,make.polys=F)
      s.labels <- st_transform(s.labels,c_sys)

      if(add_layer$s.labels == "offshore") s.labels <- s.labels %>% dplyr::filter(region == 'offshore')
      if(add_layer$s.labels == "inshore") s.labels <- s.labels %>% dplyr::filter(region == 'inshore')
      if(add_layer$s.labels == "ID") s.labels <- s.labels %>% dplyr::filter(region == 'inshore_detailed')
      if(add_layer$s.labels == "IDS") s.labels <- s.labels %>% dplyr::filter(region == 'inshore_detailed_survey')
      if(add_layer$s.labels == "all") s.labels <- s.labels %>% dplyr::filter(region %in% c('offshore','inshore'))
      s.labels <- st_intersection(s.labels, b.box)
    }

    # If not going through Github it's easy!
    if(repo != 'github')
    {
      loc <- paste0(repo,"other_boundaries/labels")
      s.labels <-   st_read(loc)
      if(add_layer$s.labels == "offshore") s.labels <- s.labels %>% dplyr::filter(region == 'offshore')
      if(add_layer$s.labels == "inshore") s.labels <- s.labels %>% dplyr::filter(region == 'inshore')
      if(add_layer$s.labels == "ID") s.labels <- s.labels %>% dplyr::filter(region == 'inshore_detailed')
      if(add_layer$s.labels == "IDS") s.labels <- s.labels %>% dplyr::filter(region == 'inshore_detailed_survey')
      if(add_layer$s.labels == "all") s.labels <- s.labels %>% dplyr::filter(region %in% c('offshore','inshore'))
      s.labels <- st_intersection(s.labels, b.box)
    }
  }

  # Finally add a scale bar if so desired.

  if(any(layers == 'scale.bar') && !is.null(add_layer$scale.bar))
  {
    scal.loc <- add_layer$scale.bar[1]
    # If we wanted to set the scale bar width ourselves
    if(length(add_layer$scale.bar) ==2) {scale.width <- as.numeric(add_layer$scale.bar[2])} else {scale.width = 0.25}
  }
  
  #browser()
  # If we have a field to plot, i.e. an INLA object and mesh, in here we convert it from a raster to an spatial DF in SF and then we plots it.
  if(!is.null(add_inla$field) && !is.null(add_inla$mesh))
  {
    #browser()
    # The mesh needs to have a CRS identified
    if(is.null(add_inla$mesh$crs)) 
    {
      # If we have a value < 0 or that the maximum value of the mesh is < 180 I assume we are in lat/lon WGS84, which should be close at least for
      # anything in the maritime region.
      if(min(add_inla$mesh$loc[,1]) < 0 || max(add_inla$mesh$loc[,1] < 180)) add_inla$mesh$crs <- mesh.csys <-  crs(st_crs(4326)[2]$proj4string)
      if(max(add_inla$mesh$loc[,1]) > 20000) add_inla$mesh$crs <- mesh.csys <- crs(st_crs(32620)[2]$proj4string)
      # if I was successful in making a mesh warn the user if we automatically added a CRS to the mesh
      if(exists("mesh.csys")) cat(paste0("Hello, local Programmer, You did not specify the mesh CRS, I used the coordinates in the mesh to take 
                                                    a guess that the coordinate system is ", st_crs(st_crs(mesh.csys)[2]$proj4string)[1]$epsg , " please confirm!!"))
      # If not successful in making a mesh we shut er down.
      if(!exists("mesh.csys")) cat(paste0("Hello, local Programmer, You did not specify the mesh CRS, I used the coordinates in the mesh to take 
                                                    a guess that the coordinate system is ",st_crs(st_crs(mesh.csys)[2]$proj4string)[1]$epsg, " please confirm!!"))
    } # end  if(is.null(add_inla$mesh$crs)) 
    # Add in the dims option if that isn't there, low resolution to start.
    if(is.null(add_inla$dims)) add_inla$dims <- c(50,50)
    
    # Project the values appropriately for the data, the xlim/ylim will come from the mesh itself.
    projec = inla.mesh.projector(add_inla$mesh, xlim = range(add_inla$mesh$loc[,1],na.rm=T) , ylim = range(add_inla$mesh$loc[,2],na.rm=T), dims=add_inla$dims)
    inla.field = inla.mesh.project(projec, add_inla$field)
    raster <- raster(rotate(rotate(rotate(inla.field))))
    extent(raster) <- c(range(projec$x),range(projec$y))
    # To convert a raster to a spatial polygon.is easy..
    sp.field <- as(raster, "SpatialPolygonsDataFrame") 
    proj4string(sp.field) <- add_inla$mesh$crs # For SP need that gross full crs code, so this...
    # Make it an sf object
    spd <- st_as_sf(sp.field,as_points=F,merge=F)
    # Now we need to convert to the coordinate system you want
    spd <- st_transform(spd,crs = c_sys)
    # If you want to clip the data to some coordinates/shape this is where that happens.
    #browser()
    if(!is.null(add_inla$clip))
    {
      # The clip has several options, you can use a local shapefile, or you can bring in a shapefile from a local directory
      # I'm assuming this is the local directory
      if(!any(class(add_inla$clip) %in% c('sf','sfc'))) 
      {
        # If a character string I assume it is looking for a shapefile 
        if(is.character(add_inla$clip)) clip <- st_read(add_inla$clip) 
        # If it is a dataframe I assume it is a properly set up data.frame
        if(is.data.frame(add_inla$clip))   clip <- st_as_sf(add_inla$clip,coords = c('x','y'),crs = add_inla$clip$crs[1] )
        # As always sp objects are bizarre, if the clip area is an sp object this won't return a null, everything else will so... weird but works...
        if(!is.null(attr(class(add_inla$clip),'package'))) clip <- st_as_sf(add_inla$clip)
      } # end if(class(add_line$clip)[1] != 'sf') 
      if(any(class(add_inla$clip) %in% c('sf','sfc'))) clip <- add_inla$clip
      # Now we want to transform to the correct coordinate system, I'm assuming the above will all have a CRS.
      clip <- st_transform(clip,crs = c_sys)
      # And clip the spd to the region you want.
      spd <- st_intersection(spd,clip)
    } # end  if(!is.null(add_inla$clip))
    # Now to make the colour ramps...
    # First I'll make a couple of generic colour ramps 
    #I'll set one up using 100 colours and a maximium of 10 breaks, break locations based on the data.
    #browser()

      if(!is.null(add_inla$scale$alpha))   {alph <- add_inla$scale$alpha}                   else alph <- 1
      if(!is.null(add_inla$scale$palette)) {col <- addalpha(add_inla$scale$palette,alph)}   else col <- addalpha(pals::viridis(100),1)
      if(!is.null(add_inla$scale$limits))  {lims <- add_inla$scale$limits}                  else lims <- c(min(spd$layer,na.rm=T),max(spd$layer,na.rm=T))
      if(!is.null(add_inla$scale$breaks))  {brk <- add_inla$scale$breaks}                   else brk <- pretty(spd$layer,n=10)
      if(!is.null(add_inla$scale$leg.name))  {leg <- add_inla$scale$leg.name}               else leg <- "Legend"
    # And now make the colour object needed. 
    if(is.null(add_inla$scale$scale)) # If not specified it's a continuous ramp
    {
    #scc <- scale_colour_gradientn(colours = col, limits= lims,breaks=brk)
    sfc <- scale_fill_gradientn(colours = col, limits=lims,breaks=brk,name=leg)
    }
    #browser()
    if(!is.null(add_inla$scale$scale)) # If you put anything in there it is a discrete ramp.
       {
      # Cut it up into the bins you want and force it to spit out numbers not scientific notation for <= 10 digits.
         spd <- spd %>% mutate(brk = cut(layer, breaks = brk,dig.lab=10)) 
         n.breaks <- length(unique(spd$brk))
         #scd <- scale_colour_manual(values = col[1:n.breaks])
         sfd <- scale_fill_manual(values = col[1:n.breaks],name=leg)
    }
    
  } # end  if(!is.null(add_inla$field) && !is.null(add_inla$mesh))
  
  # If you have an existing gg.object to use as a base plot pull that in here
  if(!is.null(gg.obj)) pect_plot <- gg.obj
  
  #If not set up a base plot.
  if(is.null(gg.obj))
  {
    pect_plot <- ggplot() + 
      geom_sf(data=b.box, fill=NA) +
      theme_minimal() + xlab("") + ylab("") +
      scale_x_continuous(expand = c(0,0)) + # Not sure either of these scale additions is needed...
      scale_y_continuous(expand = c(0,0)) 
  } # end if(!is.null(gg.obj))
  
    #browser()
    
    if(exists("bathy.smooth")) pect_plot <- pect_plot + geom_stars(data=bathy.smooth) + scale_fill_gradientn(colours = rev(brewer.blues(100)),guide = FALSE)  
    if(exists("bathy.gg")) pect_plot <- pect_plot + geom_contour(data=bathy.gg, aes(x=x, y=y, z=layer), breaks=bathy.breaks)  
    if(exists("nafo.divs")) pect_plot <- pect_plot + geom_sf(data=nafo.divs, fill=NA)
    if(exists("nafo.subs")) pect_plot <- pect_plot + geom_sf(data=nafo.subs, fill=NA)
    if(exists("inshore.spa")) pect_plot <- pect_plot + geom_sf(data=inshore.spa, fill=NA)
    if(exists("offshore.spa")) pect_plot <- pect_plot + geom_sf(data=offshore.spa, fill=NA)
    if(exists("final.strata"))
    {
      if(add_layer$survey[2] == "detailed") pect_plot <- pect_plot + new_scale("fill")  + geom_sf(data=final.strata,aes(fill= Strt_ID)) + scale_fill_manual(values = col.codes$col)
      if(add_layer$survey[2] == "outline") pect_plot <- pect_plot + new_scale("fill") + geom_sf(data=final.strata,fill = 'gray95') 
    }
    if(exists("sfc")) pect_plot <- pect_plot + new_scale("fill") + geom_sf(data=spd, aes(fill=layer), colour = NA) + sfc 
    if(exists("sfd")) pect_plot <- pect_plot + new_scale("fill") + geom_sf(data=spd, aes(fill=brk), colour = NA)  + sfd  
    if(exists("custom")) pect_plot <- pect_plot + geom_sf(data=custom, fill=NA)
    if(exists("eez")) pect_plot <- pect_plot + geom_sf(data=eez, colour="firebrick",size=1.25)
    if(exists("land.sf")) pect_plot <- pect_plot + geom_sf(data=land.sf, fill="grey")   
    if(exists("s.labels")) 
    {
      if(s.labels$region == 'offshore' || s.labels$region == 'all') pect_plot <- pect_plot + geom_sf_text(data=s.labels, aes(label = lab_short))   
      if(s.labels$region != 'offshore' && s.labels$region != 'all') pect_plot <- pect_plot + geom_sf_text(data=s.labels, aes(label = lab_short),angle=35) # rotate it@!
    }
    if(exists('scal.loc')) pect_plot <- pect_plot + annotation_scale(location = scal.loc, width_hint = scale.width) + 
                                                    annotation_north_arrow(location = scal.loc, which_north = "true", height = unit(1,"cm"), width = unit(1,'cm'),
                                                    pad_x = unit(0, "cm"), pad_y = unit(0.75, "cm"),style = north_arrow_fancy_orienteering)
    
    # Some finishing touches...I don't know that the xlim and ylim are actually necessary, think it is now redundant
    pect_plot <- pect_plot + coord_sf(xlim = xlim,ylim=ylim)
    
    if(plot == T) print(pect_plot) # If you want to immediately display the plot
      return(pect_plot = pect_plot)
 } # end function
