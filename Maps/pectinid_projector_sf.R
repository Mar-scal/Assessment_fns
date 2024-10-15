# This is a hack of a function the INLA lasses/lads use to plot their INLA model results
# Stand-alone function created by DK in Nov 2018
# Major revision to work with sf package undertaken in 2020, DK joined the party in March 2020, attempt to make this more user friendly 
# removed option add_land (we always want land...)

# Arguments:

## The general mapping inputs if you are just wanting to produce a spatial map without an INLA surface

#1: gg.obj        If you have an existing plot object that you want to load in as a base map you can pull that in here and just add additional components to that
###              using the pectinid calls you want to use.  Currently only supports a ggplot object, not plotly

#1b: plot_as   What type of plot do you want to make?  Currently supports plot_as = "ggplot" (default), and plot_as = 'plotly' uses the 
#              native plotly code to make the figure.  Also, "ggplotly" is an option which runs the ggplot code and sticks a plotly wrapper around it

###               the coordinates and the projection of those coordinates specified.  Default provides Maritime Region boundaries
###               in lat/long coordinates and a WGS84 projection.

#3: plot       Do you want to display the plot.  Default = T.  if plot= F you will just get a ggplot object useful if just making it a base layer.

#4: gis.repo       The repository from which you will pull the GIS related data.  The default is "github" (which has all the main data) 
###               option is to specify the directory you want to pull data from, likely you want to use "Y:/GISData/Github_Repo/GIS_layers/"

#5: c_sys      What coordinate system are you using, options are "ll" which is lat/lon and WGS84 (uses EPSG 4326) or "utm_zone" which is utm, you put in the zone yourself
###               for utm_19  use "32619", this is best for GB, SPA3 and 6, if you  are using something else go nuts!

#6: buffer      Add a buffer to the area plotted.  Default = 0 which just plots to the extent of the coordinates entered  Entering 0.05 will give approx a 5% buffer based on the
###               size of your area


#7: repo      The directory that our local functions reside in, this now defaults to 'github so it pulls in the stable master version of the function on github
###               If you set to repo = 'Y:/Offshore/Assessment/Assesment_fns' it will grab the functions from the version on the ESS
#8: legend    If you added a custom or INLA layer you can print the legend if you like.  Default = F which doesn't plot legend.
#9: txt.size  If you want to change the size of the text in the figure (legend and axis).  Default txt.size = 18.
#9a:axes      If you want to show the axis labels as either Degree Minutes or Degree Minutes Seconds instead of default (NULL). NULL = decimal degress or UTM, axes = "DM" will show Degree minutes, 
#                  axes = "DMS" will show Degree minute seconds.  
#################################### LAYER OPTIONS#################################### LAYER OPTIONS#################################### LAYER OPTIONS

#10: add_layer   Do you have a layer you'd like to add to the plot.  default = and empty list which will just return a map of the area with land on it.  To add layers
###               they need to be added as a list with specific options broken out here. A complete example is

###               list(land = 'grey',eez = 'eez' , bathy = 50, nafo = 'main',sfas = 'offshore',survey = "offshore", s.labels = 'offshore')

####  a: land       Do you want to add the land?  This is the only layer that plots by default with land = 'grey'. To plot land
#######             but with no fill set land = NA, exclude from add_layer list if you don't want land plotted.
#######             land = 'world' will pull in a lower resolution world map, useful if using function to plot somewhere other than NW Atl.
#######             We use a land layer developed by Brittany W. in Dec 2020 as of Dec 2020 includes NA landmass from Long Island to Labrador
####  b: eez        Do you want to add the eez  Simply put eez = 'eez' in the list and it will be included (putting in anything in quotes will work, looking for eez object in add_layer)

####  c: bathy      Do you want to add in the bathymetry, this can be a fairly complex call as it has 3 options you want to specify
#######              The first is a number giving the depth contours you want.  50 tends to look good. If you only specify this 
#######              you will get both the smooth surface and the contours with a maximum depth of 500 m
#######              The second option is optional, it one of 'both' which plots smooth surface and contours, 's' which plots a smooth bathy surface, 
#######              or 'c' which only plots the depth contour lines
#######              The final is the maximum depth you want for the contours, you can leave this out, default is 500 meters which looks good.
#######              bathy = c(50,'both',500) or bathy = 50 will plot smooth surface + contour lines at a 50 meter intervals and 500 is the maximum depth
#######              bathy = c(50,'s') or c(50,'s',500) will print the smooth only with max depth of 500 meters. 
#######              This now relies on NOAA bathymetry (so you need internet connection!), the finer the scale bathy you want the slower this runs.
#######             You can also specify bathy="ScallopMap" to use the usgs bathymetry object used in the inshore and (some) offshore ScallopMap figures.

####  d: nafo       Do you want to add nafo areas. two options, nafo = 'main' will plot the main nafo boundaries, 
#######              while nafo = 'sub' will plot the subareas. not specifying nafo will plot nothing.

####  e: sfa        Do you want to add the sfa boundaries to the figure, options are sfa = "inshore", sfa="offshore", or sfa="all".  If sourcing locally point gis.repo to correct location

####  f: survey     Do you want to add the strata boundaries the figure, requires 2 arguments, first is the area you want to plot options are 'inshore', 'offshore', or 'all'.

#######              Second argument is whether you want the full strata plotted (with colours) or just an outline of the strata, so either 'detailed', or 'outline'.  
#######              So survey = c("all", "detailed") will plot all survey extents and every strata boundary there is. survey = c("inshore","outline") will just plot the outline of the inshore surveys.
#######               If sourcing locally point gis.repo to correct location.

####  g: s.labels  Add labels to the figures?  Several options here I need to lay out.  
#######              s.labels = 'offshore' - Puts basic labels for offshore areas - Good for broad overview of offshore
#######              s.labels = "offshore_detailed" - Puts in more detailed labels for offshore, makes the figure we post in our update documents.
#######              s.labels = 'inshore' - Puts basic labels for inshore areas - Good for broad overview of inshore
#######              s.labels = 'all' - Puts the above two sets of labels - Good for broad overview of everywhere
#######              the next two options only work if you are looking at a zoomed in version of inshore, way too much detail for a figure covering everywhere.
#######              s.labels = 'ID' - Puts in detailed inshore labels for the areas
#######              s.labels = "IDS" - Puts in detailed inshore survey strata labesl for all the inshore areas


#### h:  scale.bar  Do you want to add a scale bar to the figure, it also pops in a fancy north arrow.  Also allowing for 'padding' of the location in x and y direction to allow you
#######             ability to mess about with the location a bit. Note you have to specify both an x and y shift for this to work.
#######             To add it you specify what corner you want it in and optionally it's size as a second option.
#######             scale.bar = 'bl' will put it in bottom left (options are bl,bc,br,tl,tc,tr) 
#######             scale.bar = c('bl',0.5) will put in a scale bar that is half the length of the figure in the bottom left corner.
#######             scale.bar = c('bl',0.5,1,2) will put in a scale bar that is half the length of the figure 1 cm horizontally shifted into the figure from corner and 2 cm in the vertical

#################################### INLA OPTIONS#################################### INLA OPTIONS#################################### INLA OPTIONS
## The INLA related inputs, if field and mesh are not supplied these won't do anything.  You do need field and mesh if plotting an INLA model result
#11: add_inla   Similar structure to the add_layer, this allows an INLA field to be added to the figure.  To do this you need a INLA random field and mesh
###                at a minimum.  If either field or mesh is NULL this won't plot anything. Other options should enable a plot to be produced based on default settings.

###  a: field   The output random field (or whatever INLA field you want to plot).  This will be converted to a shapefile with correct projection attributes

###  b: mesh    The INLA mesh that corresponds to the values of field, which gets projected properly in this function .
######          Note that the field is assumed to be in the same projection as the mesh.

###  c: range   The range of values for the field plotted, default uses the full range of the data.

###  d: clip    Do you want to clip the area plotted to something.  Default is no clipping, there are several options you have here...
######            clip = "D/Location/of/a/shape/file.shp" will read in a shapefile as an sf object.
######            clip = sf_or_sp_object will take in your sf or sp object and clip the field to this
######            clip = list(y = c(40,46),x = c(-68,-55),crs = 4326) will grab your coordinates

###  e: dims    The number of X and Y values for the INLA surface.  Higher is better resolution, higher = slow. Default dims = c(50, 50) which is pretty low res but quick

###  f: scale   Do you want to use a continuous scale, or a manual scale with categories.  The nature of that scale is controled by the other options in this list
######            What colours would you like to use for the colour ramp.  Default = NULL which will plot a viridis based ramp using geom_gradientn() and pecjector defaults
######            scale = 'c' has same behaviour as NULL.  scale = 'd' (really anything but NULL or "c" how it's coded)
######            then you get a discrete with these options list(scale = 'd',palette = viridis::viridis(100,begin=0,direction=1,option='D'), limits = c(0,1), breaks =seq(0,1,by=0.05),alpha=0.8)
######            Each of these fields is NULL able (i.e. you only need to specify what you want and let pecjector handle the rest)
######            $scale = list(scale = 'c',...) # if you want a discrete scale add scale = 'd', if not specified or set to 'c' we get a continuous scale.
######            $scale = list(palette = viridis(100),...) Here is where you specify your colour palette and number of colours.  Number of colours should be > the number of breaks!
######            $scale = list(breaks = seq(0,1,by=0.05),...) Where do you want to put breaks, this is really for your legend mainly as the colour scheme is much more flexible.
######            $scale = list(limits = c(0,1),...) What are upper and lower bounds of data covered by your your colour palette, 
######            $scale = list(alpha = 0.8,...)     Do you want the colours to have some level of transparency.  0 = translucent, 1 = opaque.
######            $scale =  list(leg.name = "Bill",...) What do you want the name of your legend to be.  

#################################### CUSTOM OPTIONS#################################### CUSTOM OPTIONS#################################### INLA OPTIONS
# 12: add_custom     This is really similar to the add_inla, this allows you to add your own custom object and pretty it up
###                 however your heart desires
###  a: obj         Do you have a specific object you'd like to add, this can be a csv, shapefile, sp or sf object
#######             if you just want a quick and dirty shapefile made you can simply go custom = list(obj = foo), 
#######             This works for sp, sf, csv/xls(assumes PBSmapping) or actual shapefiles.
#######             If using PBSmapping this assumes that you have the data formatted properly in a csv or xlsx file and 
#######             that the projection for the data is WGS84.  Default = NULL
########## The next 3 options only work if you haven't specified any of the scale options, if scale is specified this is ignored
###  b: size        The size of the lines, defaults to 1 if not specified

###  c: fill        The fill of the object, defaults to NA so transparent

###  d: color       The color of the lines, defaults to 'grey'
#### e: facet       For the custom object you can facet wrap based on any of the variables in the custom object.  Defaults to missing
##########          The full call for the simple custom plots would be custom = list(obj = foo,size = 1, fill = 'pink',color = 'yellow')
##########          Or if not an object in R custom = list(obj = "Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/seedboxes",size =1 , colour = 'yellow',fill = 'pink')

###  f: scale   Do you want to use a continuous scale, or a manual scale with categories.  The nature of that scale is controled by the other options in this list
######            What colours would you like to use for the colour ramp.  Default = NULL which will plot a viridis based ramp using geom_gradientn() and pecjector defaults
######            scale = 'c' has same behaviour as NULL.  scale = 'discrete' (really anything but NULL or "continuous" how it's coded)
######            then you get a discrete with these options list(scale = 'discrete',palette = viridis::viridis(100,begin=0,direction=1,option='D'), limits = c(0,1), breaks =seq(0,1,by=0.05),alpha=0.8)
######            Each of these fields is NULL able (i.e. you only need to specify what you want and let pecjector handle the rest)
######            $scale = list(scale = 'd',...) # if you want a discrete scale add scale = 'd', if not specified or set to 'c' we get a continuous scale.###            $scale = list(palette = viridis(100),...) Here is where you specify your colour palette and number of colours.  Number of colours should be > the number of breaks!
######            $scale = list(palette = viridis(100),...) Here is where you specify your colour palette and number of colours.  Number of colours should be > the number of breaks!
######            $scale = list(breaks = seq(0,1,by=0.05),...) Where do you want to put breaks, this is really for your legend mainly as the colour scheme is much more flexible.
######            $scale = list(limits = c(0,1),...) What are upper and lower bounds of data covered by your your colour palette, 
######            $scale = list(alpha = 0.8,...)     Do you want the colours to have some level of transparency.  0 = translucent, 1 = opaque.
######            $scale =  list(leg.name = "Bill",...) What do you want the name of your legend to be.  




# A working almost full example (no custom) of a call to this function that should work without any modification as long
# as you are connected to the NAS drive...
#pecjector(obj = NULL, area = "BBn",plot = T, 
#          add_layer = list(land = 'grey',eez = 'eez', bathy = 50, nafo = 'main',sfa = 'offshore',survey = c('offshore','detailed'),s.labels = 'offshore',scale.bar = 'bl',scale.bar = c('bl',0.5)))

########## If you had an INLA layer, a full call to that would be to add this to the above..
#         add_inla(field = inla.field.obj, mesh = mesh.inla.obj,range = c(0,1),clip = sf.obj,dims = c(50,50),
#         scale= list(scale = 'discrete', palette = viridis::viridis(100), breaks = seq(0,1, by = 0.05), limits = c(0,1), alpha = 0.8,leg.name = "Ted"))

########## If you had an custom layer, a full call to that would be to add this to the above, scale behaves same as INLA scale
#         add_custom(obj = foo, size = 1, fill = NA, color = 'grey', facet = 'column name from foo',
#           scale= list(scale = 'discrete', palette = viridis::viridis(100), breaks = seq(0,1, by = 0.05), limits = c(0,1), alpha = 0.8,leg.name = "Ted"))


pecjector = function(gg.obj = NULL,plot_as = "ggplot" ,area = list(y = c(40,46),x = c(-68,-55),crs = 4326), plot = T, txt.size = 18,
                     gis.repo = "github",c_sys = "ll",  buffer = 0, repo = "github", legend = F, axes = NULL, quiet=F,
                     # Controls what layers to add to the figure (land,eez, nafo, sfa's, labels, )
                     add_layer = list(land = 'grey'),
                     
                     # The below control the INLA surface added to the figure, the col subgroup controls what the field looks like
                     add_inla = list(), # list(scale = 'discrete',palette = viridis::viridis(100,begin=0,direction=1,option='D'), limits = c(0,1), breaks =seq(0,1,by=0.05),alpha=0.8)
                     add_custom = list(), # list(obj = foo, size = 1, fill = NA, color = 'grey',  facet = 'column name from foo',
                     #                                                scale= list(scale = 'discrete', palette = viridis::viridis(100), breaks = seq(0,1, by = 0.05), limits = c(0,1), alpha = 0.8,leg.name = "Ted"))
                     ...) 
{ 
  
  
  require(marmap) || stop("You need the marmap function to get the bathymetry")
  require(sf) || stop("It's 2020. We have entered the world of sf. ")
  require(ggplot2) || stop("Install ggplot2 or else.")
  require(stars) || stop("Install stars or else.")
  require(tmaptools) || stop("Install this new tmaptools package, for working with sf objects")
  #require(maptools) || stop("Install this old maptools package, for the Polyset2SpatialLines function")
  if(is.null(add_layer$land)) add_layer$land <- "grey"
  if(add_layer$land == 'world')
  {
  require(rnaturalearth) || stop("Install rnaturalearth package, this replaces maps and mapdata packages")
  require(rnaturalearthdata)|| stop("Install rnaturalearthdata package, this replaces maps and mapdata packages")
  require(rnaturalearthhires) || stop("You need rnaturalearthhires run this to install devtools::install_github('ropensci/rnaturalearthhires') ")
  }
  require(raster)|| stop("You need raster, well you might not, depends really what you are doing... ")
  #require(rgdal)|| stop("You need rgdal pal")
  #require(RStoolbox) || stop ("You need RStoolbox to rasterize and reproject your bathymetry")
  require(pals) || stop("Pals package is needed, it is your one stop shop of colour pallettes in R, install it!")
  require(ggnewscale)  || stop ("Please install ggnewscale...If you want multiple colour ramps on one ggplot, you want ggnewscale :-)")
  require(ggspatial) ||stop ("Please install ggspatial which is needed to include the scale bar")
  require(RCurl) || stop ("Please install RCurl so yo you can pull functions from github")
  require(readr) || stop ("Please install RCurl so yo you can pull csv from github")
  require(s2) || stop ("Please install s2 so you can tidy up any crappy geography. Note, you may also need to update sf")
  require(purrr) || stop ("Please install purrr so you can make cool axes tick marks")
  require(tidyverse) ||stop ("Please install some tidyverse packaage that has mutate so you can make cool axes tick marks")
  if(length(add_inla) > 0) require(INLA) || stop ("If you want to run INLA model output, might help to install the INLA packages!!")
  if(plot_as != 'ggplot') require(ggthemes) ||stop ("Please install ggspatial which is needed for the map theme for plotly")
  if(plot_as != 'ggplot') require(plotly) || stop ("Please install plotly if you want an interactive plot")
  
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
  if(repo != 'github')
  {
    source(paste(repo,"/Maps/convert_coords.R",sep="")) 
    source(paste(repo,"/Maps/add_alpha_function.R",sep="")) 
    source(paste(repo,"/Maps/combo_shp.R",sep="")) 
    source(paste(repo,"/Survey_and_OSAC/convert.dd.dddd.R",sep="")) 
  }
  
  if(repo == 'github')
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/convert_coords.R",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/combo_shp.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
    for(fun in funs) 
    {
      download.file(fun,destfile = basename(fun), quiet=quiet)
      source(paste0(getwd(),"/",basename(fun)))
      file.remove(paste0(getwd(),"/",basename(fun)))
    } # end for(un in funs)
  } # end if(repo == 'github')
  
  # Because it was causing all sorts of funny little issues we decided to globally turn off the s2 functionality
  sf_use_s2(FALSE)
  
  # This is needed to spin the field projection to be oriented how GIS wants them, unclear why it is weird like this!
  rotate <- function(x) t(apply(x, 2, rev)) 
  options(scipen=999)# Avoid scientific notation
  loc <- area # Need to change the name as something in the environment is getting confused when using area which is also an internal function in the 'terra' package.
  # Don't do this if the field and mesh lengths differ.
  #if(!is.null(add_inla$field)) stopifnot(length(add_inla$field) == add_inla$mesh$n) 
  
  # If we set area up as a ggplot we don't need to do any of this fun.
  # } # end if(repo == "local")
  # Now if you set the c_sys to "ll" that means "ll" and WGS84, so explicitly set this now.
  if(c_sys == "ll") c_sys <- 4326 # 32620 is UTM 20, just FYI 
  # Now we need to get our ylim and xlim using the convert.coords function
  # Get our coordinates in the units we need them, need to do some stick handling if we've entered specific coords above
  # This the case in which we enter numbers as our coordinate system  
  if(any(class(loc) == 'list')) coords <- convert.coords(plot.extent = list(y=loc$y,x=loc$x),in.csys = unique(loc$crs),out.csys = c_sys,bbox.buf = buffer,make.sf=T)
  if(any(class(loc)=="data.frame")) coords <- convert.coords(plot.extent = list(y=loc$y,x=loc$x),in.csys = unique(loc$crs),out.csys = c_sys,bbox.buf = buffer,make.sf=T)
  # This is the case when we put a name in and let convert.coords sort it out.
  if(any(class(loc) == 'character')) coords <- convert.coords(plot.extent = loc,out.csys = c_sys,bbox.buf = buffer, make.sf=T)
  if(any(class(loc) %in% c("sp"))) loc <- st_as_sf(loc) # Convert to sf cause I already have that ready to roll below
  # and finally if the object is an sf or sp object we just pull the bounding box from that object to use that.
  if(any(class(loc) %in% c("sf",'sfc','sfg')))
  {
    sf.box <- st_bbox(loc)
    coords <- convert.coords(plot.extent = list(y=c(sf.box$ymin,sf.box$ymax),x=c(sf.box$xmin,sf.box$xmax)),in.csys = st_crs(loc),out.csys = c_sys,bbox.buf = buffer,make.sf=T)
  }
  #browser()
  # All I need from the coords call above is the bounding box.
  b.box <- st_make_valid(coords$b.box)
  # Get the limits of the bounding box
  xlim <- as.numeric(c(st_bbox(b.box)$xmin,st_bbox(b.box)$xmax))
  ylim <- as.numeric(c(st_bbox(b.box)$ymin,st_bbox(b.box)$ymax))
  # Get the spatial coordinates correct for the boxes, likely they are already in the Lat/Long WGS84 format, but might not be...
  # Note that this requires the boxes are already spatial polygons and have a coordinate reference system.
  #if(!is.null(add_obj)) add_obj <- st_transform(add_obj,crs = c_sys)
  #
  # ID what layers we are looking for.
  layers <- names(add_layer)

  # If we are going to add the EEZ do this...
  if(any(layers == 'eez'))
  {
    # if we already have the full eez in the global environment we don't need to reload it, we do need to sub-set it and project it though
    if(gis.repo == 'github')
    {
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to the temp directory you created above
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/EEZ/EEZ.zip", temp, quiet=quiet)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # Now read in the shapefile
      eez.all <- st_read(paste0(temp2, "/EEZ.shp"), quiet=quiet)
    } else { # end if(gis.repo == 'github' )
      loc <- paste0(gis.repo,"/EEZ")
      eez.all <- st_read(loc, quiet=quiet)
    } # end the else
    
    # We then need to transform these coordinates to the coordinates of the eez data
    eez.bbox <- b.box %>% st_transform(st_crs(eez.all))
    # Then intersect the coordiates so we only plot the part of the eez we want
    # sf::sf_use_s2(FALSE)
    # eez.all <- st_make_valid(eez.all)
    eez <- st_intersection(eez.all,eez.bbox)
    # Make the eez a big mutlilinestring, it makes plotly happier...
    #eez <- st_cast(eez, to = "MULTILINESTRING")
    if(nrow(eez) == 0) rm(eez) # Get rid of the eez as it causes greif for plotly if it remains but is empty...
    
    # and now transform this eez subset to the proper coordinate system. If there is an eez in the picture....
    if(exists('eez')) eez <- eez %>% st_transform(c_sys)
    
    
  } # end if(!is.null(add_EEZ)) 
  # Now grab the land, note we're using the rnaturalearth package now
  # We also want to get the land
  if(any(layers == 'land'))
  {
    lnd <- add_layer$land
    # If using the world map we go here
    if(lnd == 'world')
    {
    land.all <- ne_countries(scale = "large", returnclass = "sf",continent = "North America")
    land.col <- 'grey' # Forceing land to be grey if you are using the world map
    }
    if(lnd != 'world' & gis.repo == 'github')
    {
      temp <- tempfile()
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/other_boundaries.zip", temp,quiet=quiet)
      # Download this to the temp directory you created above
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      land.all <- st_read(dsn = paste0(temp2,"/Atl_region_land.shp"))
      land.col <- lnd
      stpierre <- st_read(dsn = paste0(temp2,"/SPM_adm0.shp"))
      land.all <- st_union(land.all, stpierre)
    }
    # If you want to sorce it locally.
    if(lnd != 'world' & gis.repo != 'github') 
    {
      land.all <- st_read(paste0(gis.repo,"/other_boundaries/Atl_region_land.shp"))
      land.col <- lnd
      stpierre <- st_read(paste0(gis.repo,"/other_boundaries/SPM_adm0.shp"))
      land.all <- st_union(land.all, stpierre)
    }
    
    # f we are lat/lon and WGS84 we don't need to bother worrying about clipping the land (plotting it all is fine)
    if(c_sys == "4326") 
    {
      land.all <- st_make_valid(land.all) # The geography has an issue, so we need to fix that issue with st_make_valid
      land.sf <- st_intersection(land.all, b.box)
      land.sf <- land.sf  %>% st_make_valid() %>% st_simplify() %>% st_cast("MULTIPOLYGON") 
      if(nrow(land.sf) == 0) rm(land.sf) # Get rid of the land object as it causes greif for plotly if it remains but is empty...
    }
    # If we need to reproject do it...
    if(c_sys != "4326") 
    {
      land.all <- st_make_valid(land.all) # The geography has an issue, so we need to fix that issue with st_make_valid
      t.bbox <- st_transform(b.box,crs = st_crs(land.all))
      land.sf <- st_intersection(land.all,t.bbox)
      land.sf <- st_cast(st_simplify(st_make_valid(land.sf)),"MULTIPOLYGON")
      land.sf <- st_transform(land.sf,crs=c_sys)
      if(nrow(land.sf) == 0) rm(land.sf) # Get rid of the land object as it causes greif for plotly if it remains but is empty...
    } # end if(c_sys != "+init=epsg:4326") 
  }
 
  
  if(!is.null(add_layer$bathy)) # Is this redundant with below any(layers == 'bathy'))?
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
      # Add a small buffer to the area.  This might not always work if you are looking at a very small area
      # But it does seem to work for all of our pre-defined areas, if you get an error in Marmap try and make your area larger 
      # until the error goes away, then you can just use coord_sf from the ggspatial packages to trim the area back to what you want.
      
      # This covers NAD83 and WGS84 Lat/Lon
      if(c_sys %in% c("4269","4326")) 
      {
        bath.box <- st_bbox(st_as_sf(data.frame(x = c(floor(st_bbox(b.box)$xmin),ceiling(st_bbox(b.box)$xmax)), y = c(floor(st_bbox(b.box)$ymin),ceiling(st_bbox(b.box)$ymax))),coords = c('x','y'),crs = c_sys))
      }
      # Assuming other c_sys to be UTM, could be wrong!
      if(!c_sys %in% c("4269","4326")) 
      {
        bath.box <- b.box %>% st_transform(crs=4326)  # Needs to be in Lat/Lon for getNOAA.bathy, so we go for 4326
        bath.box <- st_bbox(st_as_sf(data.frame(x = c(floor(st_bbox(bath.box)$xmin),ceiling(st_bbox(bath.box)$xmax)), y = c(floor(st_bbox(bath.box)$ymin),ceiling(st_bbox(bath.box)$ymax))),coords = c('x','y'),crs = c_sys))
      }
      # The bathymetry data is given in NOAA as Lat/Lon WGS84 according to NOAA's website.  https://www.ngdc.noaa.gov/mgg/global/
      # Based on a figure in their paper seems the contours are meters https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0073051
      # This is a little slow when doing the whole of the Maritimes (about 15 seconds)
      # This is a lat/lon WGS84 object...
      
      # If you want to make it look like ScallopMap USGS bathy...
      if(add_layer$bathy[1] == "ScallopMap") {
        # if we already have the full eez in the global environment we don't need to reload it, we do need to sub-set it and project it though
        if(gis.repo == 'github')
        {
          # Figure out where your tempfiles are stored
          temp <- tempfile()
          # Download this to the temp directory you created above
          download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/bathymetry/bathymetry.zip", temp)
          # Figure out what this file was saved as
          temp2 <- tempfile()
          # Unzip it
          unzip(zipfile=temp, exdir=temp2)
          # Now read in the shapefile
          bathy.scallopmap <- st_read(paste0(temp2, "/bathymetry_15m.shp"), quiet=quiet)
        } else { # end if(gis.repo == 'github' )
          loc <- paste0(gis.repo,"/bathymetry")
          bathy.scallopmap <- st_read(loc, quiet=quiet)
        } # end the else
        
        bathy.scallopmap <- bathy.scallopmap %>%
          st_transform(st_crs(bath.box))%>%
          st_crop(st_bbox(b.box))
      }
      
      if(!add_layer$bathy[1] == "ScallopMap"){
        #bathy.org <- getNOAA.bathy(lon1 = bath.box$xmin ,bath.box$xmax,lat1 = bath.box$ymin,lat2=bath.box$ymax,resolution =1)
        # bathy <- marmap::as.raster(bathy.org)
        # Read in our version of the bathymetry from the GIS layers repo.
        githubURL <- ("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/bathymetry/NW_Atl_bathy_raster_from_NOAA_marmap.Rds")
        download.file(githubURL,"NW_Atl_bathy_raster_from_NOAA_marmap.Rds", method="curl")
        bathy <- readRDS("NW_Atl_bathy_raster_from_NOAA_marmap.Rds")
        
        # Now clip this to the bounding area, note that the bathy is basically a EPSG:4326 so we need to crop it accordingly and transform our b.box to this...
        bathy <- crop(bathy,as_Spatial(st_transform(b.box,crs = 4326)))
        #Now if we want smooth contours we do this...
        # For the continuous colours everything deeper than specificed (default = 500m) will be the same colour, just tidies up the plots.
        if(add_layer$bathy[2] == 'both' || add_layer$bathy[2] == 's' )
        {
          bathy.org <- marmap::as.bathy(bathy)
          bathy.s <- bathy.org
          bathy.s[which(bathy.s < -abs(as.numeric(add_layer$bathy[3])))] <- -abs(as.numeric(add_layer$bathy[3]))
          bathy.s[which(bathy.s > 0)] <- 0
          bathy.s <- marmap::as.raster(bathy.s)
          bathy.s <- crop(bathy.s,as_Spatial(st_transform(b.box,crs = 4326)))
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
            bathy.gg <- as.data.frame(re.proj.bathy, xy=T) #fortify(re.proj.bathy)
          } else { bathy.gg <- as.data.frame(bathy, xy=T)}
          # define the contour breaks, only plot contours between 0 and everything deeper than specificed (default = 500m) .
          bathy.breaks <- seq(0, -abs(as.numeric(add_layer$bathy[3])), -abs(as.numeric(add_layer$bathy[1])))
        }
      }
    } # end if(any(layers == 'bathy'))
  } # end the is.null()
  # If you want to add the NAFO division, the autoritaive versions are on the web so when we say "gis.repo = 'github'", for NAFO this is actually going
  # to NAFO's website to get them.  We have a version of these saved locally as well which is accessed when "gis.repo = 'local'"
  
  if(any(layers == 'nafo'))
  {
    # If they don't already exist and our gis.repo is github and we just want the main nafo division go get them from online
    if(gis.repo == 'github' && add_layer$nafo == "main")
    {
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to there
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/NAFO/Divisions.zip", temp, quiet=quiet)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # This pulls in all the layers from the above location
      nafo.divs <- combo.shp(temp2,make.sf=T, quiet=quiet)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      nafo.divs <- st_transform(nafo.divs,c_sys)
      #trim to bbox
      nafo.divs <- st_intersection(nafo.divs, b.box)
      nafo.divs <- st_cast(nafo.divs,to= "MULTILINESTRING")
      if(nrow(nafo.divs) == 0) rm(nafo.divs) # Get rid of the nafo.divs object as it causes greif for plotly if it remains but is empty...
      
    } # if(gis.repo == 'github' && !exists("nafo.divs"))
    
    # if we want the main divisions from local
    if(gis.repo != 'github' && add_layer$nafo == "main")
    {
      loc <- paste0(gis.repo,"/NAFO/Divisions")
      nafo.divs <- combo.shp(loc,make.sf=T, quiet=quiet)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      nafo.divs <- st_transform(nafo.divs,c_sys)
      #trim to bbox
      nafo.divs <- st_intersection(nafo.divs, b.box)
      nafo.divs <- st_cast(nafo.divs,to= "MULTILINESTRING")
      if(nrow(nafo.divs) == 0) rm(nafo.divs) # Get rid of the nafo.divs object as it causes greif for plotly if it remains but is empty...
    } # end if(gis.repo == 'local' && !exists("nafo.divs") && add_nafo = "main")
    
    # Now if we want the nafo sub-areas we do this, for the locals
    if(gis.repo == 'github' && !exists("nafo.sub") && add_layer$nafo == "sub")
    {
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to the temp directory you created above
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/NAFO/Subareas.zip", temp, quiet=quiet)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # This pulls in all the layers from the above location
      nafo.sub <- combo.shp(temp2,make.sf=T, quiet=quiet)
      nafo.sub <- st_make_valid(nafo.sub)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      nafo.sub <- st_transform(nafo.sub,c_sys)
      #trim to bbox
      nafo.sub <- st_intersection(nafo.sub, b.box)
      nafo.sub <- st_cast(nafo.sub,to= "MULTILINESTRING")
      if(nrow(nafo.sub) == 0) rm(nafo.sub) # Get rid of the nafo.sub object as it causes greif for plotly if it remains but is empty...
    } # end if(add_sfas != "offshore")
    
    # Now if we want the nafo sub-areas we do this, for the locals
    if(gis.repo != 'github' && add_layer$nafo == "sub")
    {
      # Now if we want the nafo sub-areas we do this...
      loc <- paste0(gis.repo,"/NAFO/Subareas")
      nafo.sub <- combo.shp(loc,make.sf=T, quiet=quiet)
      nafo.sub <- st_make_valid(nafo.sub)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      nafo.sub <- st_transform(nafo.sub,c_sys)
      #trim to bbox
      nafo.sub <- st_intersection(nafo.sub, b.box)
      nafo.sub <- st_cast(nafo.sub,to= "MULTILINESTRING")
      if(nrow(nafo.sub) == 0) rm(nafo.sub) # Get rid of the nafo.sub object as it causes greif for plotly if it remains but is empty...
      
    } # end if(gis.repo == 'local' && !exists("nafo.sub") && add_nafo = "main")
    
  } # end if(add_nafo != "no")
  

  # Now do we want to add in the SPA and SFA's for the region, this is using the approve Inshore polygons that Leslie Nasmith developed in 2014, these are
  # all NAD83 lat/lon's.  There are a bunch of shapefiles for inshore this this isn't always speedy
  if(any(layers == 'sfa')) 
  {
    if(gis.repo == 'github')
    {
      if(add_layer$sfa != "offshore")
      {
        #
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_boundaries.zip", temp, quiet=quiet)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        # This pulls in all the layers from the above location
        inshore.spa <- combo.shp(temp2,make.sf=T, quiet=quiet)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        inshore.spa <- st_make_valid(inshore.spa)
        inshore.spa  <- st_transform(inshore.spa,c_sys)
        #trim to bbox
        inshore.spa <- st_intersection(inshore.spa, b.box) # This swtiches the object to a geometry type
        inshore.spa <- st_cast(inshore.spa,to= "MULTILINESTRING")
        if(nrow(inshore.spa) == 0) rm(inshore.spa) # Get rid of the inshore.spa object as it causes greif for plotly if it remains but is empty...
        
      } # end if(add_sfas != "offshore")
      
      if(add_layer$sfa != "inshore" && !exists("offshore.spa"))
      {
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore/offshore.zip", temp, quiet=quiet)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        
        # This pulls in all the layers from the above location
        offshore.spa <- combo.shp(temp2,make.sf=T, quiet=quiet)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        # Because of issues with the polygons immediately needed to turn it into a multilinestring to avoid bad polygons, works a charm after that...
        if(any(st_is_empty(offshore.spa))) message(paste0("removed ", offshore.spa[st_is_empty(offshore.spa),]$ID, " because they were empty"))
        offshore.spa <- offshore.spa[!st_is_empty(offshore.spa),]
        offshore.spa <- st_cast(offshore.spa,to= "MULTILINESTRING")
        offshore.spa  <- st_transform(offshore.spa,c_sys)
        offshore.spa <- st_make_valid(offshore.spa)
        offshore.spa <- st_intersection(offshore.spa, b.box)
        offshore.spa <- st_cast(offshore.spa,to= "MULTILINESTRING")
        if(nrow(offshore.spa) == 0) rm(offshore.spa) # Get rid of the offshore.spa object as it causes greif for plotly if it remains but is empty...
        
      } # end if(add_sfas != "offshore")  
      
    }# end if(gis.repo = 'github')
    
    # Now if you aren't using github do this...
    if(gis.repo != 'github')
    {
      if(add_layer$sfa != "offshore")
      {
        loc <- paste0(gis.repo,"/inshore_boundaries")
        
        inshore.spa <- combo.shp(loc,make.sf=T, quiet=quiet)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        inshore.spa <- st_make_valid(inshore.spa)
        
        inshore.spa  <- st_transform(inshore.spa,c_sys)
        #trim to bbox
        inshore.spa <- st_intersection(inshore.spa, b.box)
        inshore.spa <- st_cast(inshore.spa,to= "MULTILINESTRING")
        if(nrow(inshore.spa) == 0) rm(inshore.spa) # Get rid of the offshore.spa object as it causes greif for plotly if it remains but is empty...
        
        
      } # end if(detailed != "offshore")
      if(add_layer$sfa != "inshore" )
      {
        loc <- paste0(gis.repo,"/offshore")
        # This pulls in all the layers from the above location
        offshore.spa <- combo.shp(loc,make.sf=T, quiet=quiet)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        # Because of issues with the polygons immediately needed to turn it into a multilinestring to avoid bad polygons, works a charm after that...
        offshore.spa <- st_cast(offshore.spa,to= "MULTILINESTRING")
        offshore.spa  <- st_transform(offshore.spa,c_sys)
        offshore.spa <- st_make_valid(offshore.spa)
        offshore.spa <- st_intersection(offshore.spa, b.box)
        offshore.spa <- st_cast(offshore.spa,to= "MULTILINESTRING")
        if(nrow(offshore.spa) == 0) rm(offshore.spa) # Get rid of the offshore.spa object as it causes greif for plotly if it remains but is empty...
        
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
    } # end if(gis.repo == 'local')
  } # end if(!is.null(add_sfas)) 
  
  # Now we do the same thing for the strata
  if(any(layers == 'survey')) 
  {
    #
    if(gis.repo == 'github')
    {
      if(add_layer$survey[1] != "offshore")
      {
        
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_boundaries/inshore_survey_strata/inshore_survey_strata.zip", temp, quiet=quiet)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        #
        # This pulls in all the layers from the above location, and puts some metadata in there matching offshore structure
        inshore.strata <- combo.shp(temp2,make.sf=T,make.polys=F, quiet=quiet)
        #nshore.strata$Strt_ID <- as.character(900:(length(inshore.strata$ID)+899))
        inshore.strata$Strt_ID <- as.character(900:(length(inshore.strata$STRATA_ID)+899))
        inshore.strata$col <- cividis(nrow(inshore.strata))
        inshore.strata$ID <- inshore.strata$STRATA_ID
        inshore.strata <- inshore.strata %>% dplyr::select(Strt_ID,ID,col)
        
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        inshore.strata  <- st_transform(inshore.strata,c_sys)
        inshore.strata <- st_buffer(inshore.strata,dist=0)
        #trim to bbox
        inshore.strata <- st_intersection(inshore.strata, b.box)
        inshore.strata <- st_buffer(inshore.strata,dist=0)
        
        inshore.strata <- st_cast(inshore.strata,to = "MULTIPOLYGON")
        if(nrow(inshore.strata) == 0) rm(inshore.strata) # Get rid of the offshore.spa object as it causes greif for plotly if it remains but is empty...
      } # end if(add_strata != "offshore")
      
      if(add_layer$survey[1]  != "inshore")
      {

        # Note we only do this if there is no offshore.strata object already loaded, this will really speed up using this function multiple times as you only will load these data once.
        # The only problem with this would be if offshore strata was loaded as an object but it wasn't the offshore strata we wanted!
        
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        
        # Download this to the temp directory you created above
        if(add_layer$survey[2] == 'detailed') download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore_survey_strata/offshore_survey_strata.zip", temp, quiet=quiet)
        if(add_layer$survey[2] == 'outline') download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/survey_boundaries/survey_boundaries.zip", temp, quiet=quiet)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)

        # We need to tidy up the GBa strata as it causes problems with spherical geometry, the hack for the moment is just to turn that off for the detailed strata.
        #sf::sf_use_s2(FALSE)

        # This pulls in all the layers from the above location
        offshore.strata <- combo.shp(temp2,make.sf=T,make.polys=F, quiet=quiet)
        # Need to tidy up the object for s2 world... I'm not sure if the MaxLength (in meters given our shape files are lat/lon) is appropriate or not
        #offshore.strata <- st_segmentize(offshore.strata,dfMaxLength = 100)
        # These two strata are the problem, figure it out Monday, switching to a line string solves problem, but then we can't 'color' them in anymore so 
        # that's not a solution either as I can't get back to a polygon...
        offshore.strata <- st_make_valid(offshore.strata)
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
        offshore.strata <- st_cast(offshore.strata,to = "MULTIPOLYGON")
        if(nrow(offshore.strata) == 0) rm(offshore.strata) # Get rid of the offshore.spa object as it causes greif for plotly if it remains but is empty...
        #sf::sf_use_s2(TRUE)
      } # end if(add_strata != "offshore")  
      
    }# end if(gis.repo = 'github')
    #
    # Now if you aren't using github do this...
    if(gis.repo != 'github')
    {
      if(add_layer$survey[1]  != "offshore")
      {
        #browser()
        loc <- paste0(gis.repo,"/inshore_boundaries/inshore_survey_strata")
        # This pulls in all the layers from the above location, and puts some metadata in there matching offshore structure
        inshore.strata <- combo.shp(loc,make.sf=T,make.polys=F, quiet=quiet)
        inshore.strata$Strt_ID <- as.character(900:(length(inshore.strata$STRATA_ID)+899))
        inshore.strata$col <- cividis(nrow(inshore.strata))
        inshore.strata$ID <- inshore.strata$STRATA_ID
        inshore.strata <- inshore.strata %>% dplyr::select(Strt_ID,ID,col)
        inshore.strata <- st_buffer(inshore.strata,dist=0)
        inshore.strata <- st_intersection(inshore.strata, b.box)
        inshore.strata <- st_buffer(inshore.strata,dist=0)
        inshore.strata <- st_cast(inshore.strata,to = "MULTIPOLYGON")
        if(nrow(inshore.strata) == 0) rm(inshore.strata) # Get rid of the offshore.spa object as it causes greif for plotly if it remains but is empty...
        
      } # end if(detailed != "offshore")
      if(add_layer$survey[1]  != "inshore")
      {
        if(add_layer$survey[2] == 'detailed') loc <- paste0(gis.repo,"/offshore_survey_strata")
        if(add_layer$survey[2] == 'outline') loc <- paste0(gis.repo,"/survey_boundaries")
 
        # This pulls in all the layers from the above location
        #sf::sf_use_s2(FALSE)
        offshore.strata <- combo.shp(loc,make.sf=T,make.polys=F, quiet=quiet)
        offshore.strata <- st_make_valid(offshore.strata)
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
        offshore.strata <- st_cast(offshore.strata,to = "MULTIPOLYGON")
        if(nrow(offshore.strata) == 0) rm(offshore.strata) # Get rid of the offshore.spa object as it causes greif for plotly if it remains but is empty...
        #sf::sf_use_s2(TRUE)
      } # end if(detailed != "offshore")
    } # end if(gis.repo == 'local')
    if(exists("offshore.strata")) final.strata <- offshore.strata
    if(exists("inshore.strata")) final.strata <- inshore.strata
    if(exists("inshore.strata") & exists("offshore.strata")) final.strata <- rbind(offshore.strata,inshore.strata)
    
    # I need to re-order the strata so the colour render correctly...
    if(exists("final.strata")) {
      col.codes <- final.strata[order(final.strata$Strt_ID),]
      if(is.factor(col.codes$col)) col.codes$col <- as.character(col.codes$col)
    }
  } # end if(any(layers == 'survey')) 
  #sf::st_use_s2(FALSE)
  # Here you can add a custom sp, sf, PBSmapping object or shapefile here
  
  if(length(add_custom) != 0)
  {
    
    # If it's an sf object it's easy peasy
    if(grepl('^sf',class(add_custom$obj)[1]))  custom <- add_custom$obj

    # If it's an sp object this should work.
    if(grepl("Spatial",class(add_custom$obj)[1])) custom <- st_as_sf(add_custom$obj)
    # If the object is a character string we have 3 options, it's a csv or xls file or it is pointing to shapefile
    if(class(add_custom$obj)[1] == "character")
    {
      # If it is an xls or a csv we assume we have a PBSmapping object
      if(grepl(".xls",add_custom$obj) || grepl(".csv",add_custom$obj))
      {
        if(grepl(".csv",add_custom$obj)) temp <- read.csv(add_custom$obj)
        if(grepl(".xls",add_custom$obj)) temp <- read_excel(add_custom$obj,sheet=1) # This will only pull in the first sheet, don't get fancy here
        temp <- as.PolySet(temp,projection = "LL") # I am assuming you provide Lat/Lon data and WGS84
        browser()
        temp <- PolySet2SpatialLines(temp) # Spatial lines is a bit more general (don't need to have boxes closed)
        custom <- st_as_sf(temp)
      } else { custom <- combo.shp(add_custom$obj,make.sf=T, quiet=quiet)}# If it doesn't then we assume we have a shapefile, if anything else this won't work.
    } # end if(class(add_custom$obj)[1] == "character")
    # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
    custom  <- st_transform(custom,c_sys)
    #trim to bbox
    custom <- st_intersection(custom, b.box)
    
    # If we specify the size, fill or color to be a unique value we set these up here, if they are blank we go to some defaults.
    if(!is.null(add_custom$size))   {size <- add_custom$size}      else size <- 1
    if(!is.null(add_custom$color))   {colr <- add_custom$color}    else colr <- "grey"
    if(!is.null(add_custom$fill))   {phil <- add_custom$fill}      else phil <- NA
    
    # So that's the object itself, with simple fill parameters 
    # If we have something there for the scale that tells us we want a fill scale
    if(!is.null(add_custom$scale)) 
    {
      # So only get this if we have a numeric layer, if we have a factor we go below...
      if(is.numeric(custom$layer))
      {
      # Now if we want to get fancy and we want to be able to plot this custom object with a fill that represents some data 'layer'
      # We set this up very much like what we do with the inla objects below, attempting to make the code mirror the inla code here
      # Might be able to make this more efficient, but for now I duplicate.
      if(!is.null(add_custom$scale$limits))  {lims <- add_custom$scale$limits}                  else lims <- c(min(custom$layer,na.rm=T),max(custom$layer,na.rm=T))
      if(!is.null(add_custom$scale$breaks))  {brk <- add_custom$scale$breaks}                   else brk <- pretty(custom$layer,n=100)
      # And now make the colour object needed. 
      } # end if(is.numeric(custom$layer))
      if(!is.null(add_custom$scale$alpha))   {alph <- add_custom$scale$alpha}                   else alph <- 1
      if(!is.null(add_custom$scale$palette)) {col <- addalpha(add_custom$scale$palette,alph)}   else col <- addalpha(pals::viridis(100),alph)
      if(!is.null(add_custom$scale$leg.name))  {leg <- add_custom$scale$leg.name}               else leg <- "Legend"
      
      
      if(is.null(add_custom$scale$scale) || add_custom$scale$scale == 'c') # Default gives us a continuous ramp
      {
        #scc <- scale_colour_gradientn(colours = col, limits= lims,breaks=brk) cfc = custom fill continuous
        if(plot_as != "plotly")  cfc <- scale_fill_gradientn(colours = col, limits=lims,breaks=brk,name=leg)
        # For plotly the continuous is overwritten and we just can have discrete ramps.
        if(plot_as == "plotly")  
        {       
          custom <- custom %>% mutate(brk = cut(layer, breaks = brk,dig.lab=10)) 
          n.breaks <- length(unique(custom$brk))
          combo <- data.frame(brk = levels(custom$brk),col = col)
          custom.cols <- left_join(custom,combo,by = "brk")
        }
      } else {

                # Cut it up into the bins you want and force it to spit out numbers not scientific notation for <= 10 digits.
                if(is.numeric(custom$layer)) custom <- custom %>% mutate(brk = cut(layer, breaks = brk,dig.lab=10)) 
                # If your layer is a not numeric then treat it as a factor.
                if(!is.numeric(custom$layer)) {custom$brk <- as.factor(custom$layer) ; brk <- unique(custom$brk) }
                n.breaks <- length(unique(custom$brk))
                #scd <- scale_colour_manual(values = col[1:n.breaks]) cfd = custom fill discrete
                if(plot_as != "plotly") cfd <- scale_fill_manual(values = col[1:n.breaks],name=leg)
                if(plot_as == "plotly") 
                {
                  combo <- data.frame(brk = levels(custom$brk),col = col)
                  custom.cols <- left_join(custom,combo,by = "brk")
                } # end if(plot_as == "plotly") 
              } # end if(length(add_custom$scale)>0)

    } # endif(!is.null(add_inla$custom$scale)) 
    # Just to make logic easier later, if either cfd or cfc exists we make this fancy.custom object which will tell the plot
    # not to make the simple custom plot
    if(exists('cfc') || exists('cfd')) fancy.custom <- "I'm fancy!"
    # If we want to facet_wrap this we do it..
    if(!is.null(add_custom$facet)) fac.et <- "It's a wrap"
    #
  } # end  if(!is.null(add_custom))
  
  # Do we want to add the labels to the figure he's what ya gotta do...
  if(any(layers == 's.labels')) 
  {
    if(gis.repo == 'github')
    {
      temp <- tempfile()
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/other_boundaries/labels/labels.zip", temp, quiet=quiet)
      # Download this to the temp directory you created above
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      s.labels <- combo.shp(temp2,make.sf=T,make.polys=F, quiet=T)
      s.labels <- st_transform(s.labels,c_sys)
   
      if(add_layer$s.labels == "offshore") s.labels <- s.labels %>% dplyr::filter(region == 'offshore')
      if(add_layer$s.labels == "inshore") s.labels <- s.labels %>% dplyr::filter(region == 'inshore')
      if(add_layer$s.labels == "ID") s.labels <- s.labels %>% dplyr::filter(region == 'inshore_detailed')
      if(add_layer$s.labels == "IDS") s.labels <- s.labels %>% dplyr::filter(region == 'inshore_detailed_survey')
      if(add_layer$s.labels == "all") s.labels <- s.labels %>% dplyr::filter(region %in% c('offshore','inshore'))
      if(add_layer$s.labels == "offshore_detailed") s.labels <- s.labels[grepl('offshore_detailed',s.labels$region),]
      s.labels <- st_intersection(s.labels, b.box)
      #Needed to be a little funky for offshore detailed because we may have to plot some of the offshore ones on an angle...
      # if(any(grepl("angle",s.labels$region)))
      # {
      #   s.labels.angle <- s.labels %>% dplyr::filter(region == "offshore_detailed_angle")
      #   s.labels <- s.labels %>% dplyr::filter(region != "offshore_detailed_angle")
      # }
    }
    
    # If not going through Github it's easy!
    if(gis.repo != 'github')
    {
      loc <- paste0(gis.repo,"/other_boundaries/labels")
      s.labels <-   st_read(loc, quiet=quiet)
      if(add_layer$s.labels == "offshore") s.labels <- s.labels %>% dplyr::filter(region == 'offshore')
      if(add_layer$s.labels == "inshore") s.labels <- s.labels %>% dplyr::filter(region == 'inshore')
      if(add_layer$s.labels == "ID") s.labels <- s.labels %>% dplyr::filter(region == 'inshore_detailed')
      if(add_layer$s.labels == "IDS") s.labels <- s.labels %>% dplyr::filter(region == 'inshore_detailed_survey')
      if(add_layer$s.labels == "all") s.labels <- s.labels %>% dplyr::filter(region %in% c('offshore','inshore'))
      if(add_layer$s.labels == "offshore_detailed") s.labels <- s.labels[grepl('offshore_detailed',s.labels$region),]
      #if(add_layer$s.labels == "offshore_detailed_angle") s.labels <- s.labels[grepl('offshore_detailed_angle',s.labels$region),]
      
      s.labels <- st_intersection(s.labels, b.box)

      #Needed to be a little funky for offshore detailed because we may have to plot some of the offshore ones on an angle...
      # if(any(grepl("angle",s.labels$region)))
      # {
      #   s.labels.angle <- s.labels %>% dplyr::filter(region == "offshore_detailed_angle")
      #   s.labels <- s.labels %>% dplyr::filter(region != "offshore_detailed_angle")
      # }
    }
  }
  
  # Finally add a scale bar if so desired.
  
  if(any(layers == 'scale.bar') && !is.null(add_layer$scale.bar))
  {
    scal.loc <- add_layer$scale.bar[1]
    scale.width = 0.25
    xpad <- 0
    ypad <- 0
    # If we wanted to set the scale bar width ourselves
    if(length(add_layer$scale.bar) ==2) scale.width <- as.numeric(add_layer$scale.bar[2])
    # And if we wanted to play with the position....
    if(length(add_layer$scale.bar) ==4) 
    {
      scale.width <- as.numeric(add_layer$scale.bar[2])
      xpad <- as.numeric(add_layer$scale.bar[3])
      ypad <- as.numeric(add_layer$scale.bar[4])
    } 
  }
  
  # If we have a field to plot, i.e. an INLA object and mesh, in here we convert it from a raster to an spatial DF in SF and then we plots it.
  if(!is.null(add_inla$field) && !is.null(add_inla$mesh))
  {
    # The mesh needs to have a CRS identified
    if(is.null(add_inla$mesh$crs)) 
    {
      # If we have a value < 0 or that the maximum value of the mesh is < 180 I assume we are in lat/lon WGS84, which should be close at least for
      # anything in the maritime region.
      if(min(add_inla$mesh$loc[,1]) < 0 || max(add_inla$mesh$loc[,1] < 180)) add_inla$mesh$crs <- mesh.csys <-  crs("+init=epsg:4326")
      # If you have really big numbers I'm going for it being UTM, assuming you are doing something in the Maritimes this is fine...
      if(max(add_inla$mesh$loc[,1]) > 20000) add_inla$mesh$crs <- mesh.csys <- crs("+init=epsg:32620")
      # if I was successful in making a mesh warn the user if we automatically added a CRS to the mesh
      if(exists("mesh.csys")) cat(paste0("Hello, local Programmer, You did not specify the mesh CRS, I used the coordinates in the mesh to take 
                                                    a guess that the coordinate system is ", mesh.csys , " please confirm!!"))
      # If not successful in making a mesh we shut er down.
      if(!exists("mesh.csys")) cat(paste0("Hello, local Programmer, You did not specify the mesh CRS and I was unable to figure out what it should be, you'll
                                           probably get an error before finishing reading this message, or maybe your plot will look dumb... 
                                           please fix and have a nice day!"))
    } # end  if(is.null(add_inla$mesh$crs)) 
    # Add in the dims option if that isn't there, low resolution to start.
    if(is.null(add_inla$dims)) add_inla$dims <- c(50,50)
    
    # Project the values appropriately for the data, the xlim/ylim will come from the mesh itself.
    projec = inla.mesh.projector(add_inla$mesh, xlim = range(add_inla$mesh$loc[,1],na.rm=T) , ylim = range(add_inla$mesh$loc[,2],na.rm=T), dims=add_inla$dims)
    if(add_inla$mesh$n == length(add_inla$field)) {
      inla.field = inla.mesh.project(projec, add_inla$field)
    }
    # If the above step has already happened (this is mostly for backwards compatibility with old code)....
    if(add_inla$mesh$n != length(add_inla$field)) inla.field <- add_inla$field
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
    
    if(!is.null(add_inla$clip))
    {
      # The clip has several options, you can use a local shapefile, or you can bring in a shapefile from a local directory
      # I'm assuming this is the local directory
      if(!any(class(add_inla$clip) %in% c('sf','sfc'))) 
      {
        # If a character string I assume it is looking for a shapefile 
        if(is.character(add_inla$clip)) clip <- st_read(add_inla$clip, quiet=quiet) 
        # If it is a dataframe I assume it is a properly set up data.frame
        if(is.data.frame(add_inla$clip))   clip <- st_as_sf(add_inla$clip,coords = c('x','y'),crs = add_inla$clip$crs[1] )
        # As always sp objects are bizarre, if the clip area is an sp object this won't return a null, everything else will so... weird but works...
        if(!is.null(attr(class(add_inla$clip),'package'))) clip <- st_as_sf(add_inla$clip)
      } # end if(class(add_line$clip)[1] != 'sf') 
      if(any(class(add_inla$clip) %in% c('sf','sfc'))) clip <- add_inla$clip
      # Now we want to transform to the correct coordinate system, I'm assuming the above will all have a CRS.
      clip <- st_transform(clip,crs = c_sys)
      # And clip the spd to the region you want.
      spd <- st_intersection(spd,st_make_valid(clip))
    } # end  if(!is.null(add_inla$clip))
    # Now to make the colour ramps...
    # First I'll make a couple of generic colour ramps 
    #I'll set one up using 100 colours and a maximium of 10 breaks, break locations based on the data.
    
    if(!is.null(add_inla$scale$alpha))   {alph <- add_inla$scale$alpha}                   else alph <- 1
    if(!is.null(add_inla$scale$palette)) {col <- addalpha(add_inla$scale$palette,alph)}   else col <- addalpha(pals::viridis(100),alph)
    if(!is.null(add_inla$scale$limits))  {lims <- add_inla$scale$limits}                  else lims <- c(min(spd$layer,na.rm=T),max(spd$layer,na.rm=T))
    if(!is.null(add_inla$scale$breaks))  {brk <- add_inla$scale$breaks}                   else brk <- pretty(spd$layer,n=100)
    if(!is.null(add_inla$scale$leg.name))  {leg <- add_inla$scale$leg.name}               else leg <- "Legend"
    # And now make the colour object needed. 
    if(is.null(add_inla$scale$scale) || add_inla$scale$scale == 'c') # If not specified or you specify 'continuous' it's a continuous ramp
    {
      #scc <- scale_colour_gradientn(colours = col, limits= lims,breaks=brk)
      if(plot_as != "plotly") sfc <- scale_fill_gradientn(colours = col, limits=lims,breaks=brk,name=leg)
      # Note that plotly only works with a discrete ramp so even if we ask for continuous you are getting a discrete ramp.
      if(plot_as == "plotly")  
      {
        spd <- spd %>% mutate(brk = cut(layer, breaks = brk,dig.lab=10)) 
        n.breaks <- length(unique(spd$brk))
        combo <- data.frame(brk = levels(spd$brk),col = col)
        spd <- left_join(spd,combo,by = "brk")
      }
    } else { # if not continuous then it's discrete...
      # Cut it up into the bins you want and force it to spit out numbers not scientific notation for <= 10 digits.
      spd <- spd %>% mutate(brk = cut(layer, breaks = brk,dig.lab=10))
      n.breaks <- length(unique(spd$brk))
      #scd <- scale_colour_manual(values = col[1:n.breaks])
      if(plot_as != "plotly") sfd <- scale_fill_manual(values = col[1:n.breaks],name=leg)
      if(plot_as == "plotly") 
      {
        combo <- data.frame(brk = levels(spd$brk),col = col)
        spd <- left_join(spd,combo,by = "brk")
      } # end if(plot_as == "plotly") 
    } # end else/ end if(is.null(add_inla$scale$scale)) 
    
  } # end  if(!is.null(add_inla$field) && !is.null(add_inla$mesh))
  
  # Development of native plotly figure.
  if(plot_as != "plotly")
  {
    # If you have an existing object to use as a base plot pull that in here
    if(!is.null(gg.obj)) pect_plot <- gg.obj
    
    #If not set up a base plot.
    if(is.null(gg.obj))
    {
      #browser()
      pect_plot <- ggplot(data=b.box) + 
        geom_sf(fill=NA) + coord_sf(expand=F)
      #pect_plot
        #theme_minimal() + xlab("") + ylab("") #+

    } # end if(!is.null(gg.obj))
    
    if(exists("bathy.smooth")) pect_plot <- pect_plot + geom_stars(data=bathy.smooth) + scale_fill_gradientn(colours = rev(brewer.blues(100)),guide = FALSE)  
    if(exists("final.strata")){
      
      if(add_layer$survey[2] == "detailed") pect_plot <- pect_plot + new_scale("fill")  + geom_sf(data=final.strata,aes(fill= Strt_ID)) + scale_fill_manual(values = col.codes$col)
      
    }
    if(exists("bathy.gg") & !exists("bathy.smooth")) pect_plot <- pect_plot + geom_contour(data=bathy.gg, aes(x=x, y=y, z=layer), colour="lightblue", breaks=bathy.breaks)  
    if(exists("bathy.gg") & exists("bathy.smooth")) pect_plot <- pect_plot + geom_contour(data=bathy.gg, aes(x=x, y=y, z=layer), breaks=bathy.breaks)  
    if(exists("bathy.scallopmap")) pect_plot <- pect_plot + geom_sf(data=bathy.scallopmap, colour="lightblue")  
    if(exists("sfc")) pect_plot <- pect_plot + new_scale("fill") + geom_sf(data=spd, aes(fill=layer), colour = NA) + sfc 
    if(exists("sfd")) pect_plot <- pect_plot + new_scale("fill") + geom_sf(data=spd, aes(fill=brk), colour = NA)  + sfd  
    # If we have custom fancy plots we add these here
    if(exists("cfc")) pect_plot <- pect_plot + new_scale("fill") + geom_sf(data=custom, aes(fill=layer), colour = NA) + cfc 
    if(exists("cfd")) pect_plot <- pect_plot + new_scale("fill") + geom_sf(data=custom, aes(fill=brk), colour = NA)  + cfd  
    # If we just have a simple custom plot we add here...
    if(exists("custom") & !exists("fancy.custom")) pect_plot <- pect_plot + geom_sf(data=custom, fill=phil, color = colr, size= size)
    if(exists("custom") & exists("fac.et")) pect_plot <- pect_plot + facet_wrap(c(add_custom$facet))
    if(exists("final.strata") && add_layer$survey[2] == "outline") pect_plot <- pect_plot + geom_sf(data=final.strata, fill=NA) 
    if(exists("nafo.divs")) pect_plot <- pect_plot + geom_sf(data=nafo.divs, fill=NA)
    if(exists("nafo.sub")) pect_plot <- pect_plot + geom_sf(data=nafo.sub, fill=NA)
    if(exists("inshore.spa")) pect_plot <- pect_plot + geom_sf(data=inshore.spa, fill=NA)
    if(exists("offshore.spa")) pect_plot <- pect_plot + geom_sf(data=offshore.spa, fill=NA)
    
    if(exists("eez")) pect_plot <- pect_plot + geom_sf(data=eez, colour="firebrick",size=1.25)
    if(exists("land.sf")) pect_plot <- pect_plot + geom_sf(data=land.sf, fill=land.col)   
    if(exists("s.labels")) 
    {
      if(any(s.labels$region == 'offshore')) pect_plot <- pect_plot + geom_sf_text(data=s.labels[grepl('offshore',s.labels$region),], aes(label = lab_short),size=3)   
      if(any(s.labels$region =='offshore_detailed')) pect_plot <- pect_plot + geom_sf_text(data=s.labels[s.labels$region=='offshore_detailed',], aes(label = lab_short),size=3)   
      if(any(s.labels$region =='offshore_detailed_angle')) pect_plot <- pect_plot + geom_sf_text(data = s.labels[s.labels$region=='offshore_detailed_angle',], aes(label = lab_short),size = 3,angle =-45)
      if(any(grepl('inshore',s.labels$region))) pect_plot <- pect_plot + geom_sf_text(data=s.labels[grepl('inshore',s.labels$region),], aes(label = lab_short),angle=35,size=3) # rotate it@!
      
    } # end if(exists("s.labels")) 
    if(exists('scal.loc')) pect_plot <- pect_plot + annotation_scale(location = scal.loc, width_hint = scale.width,pad_x = unit(xpad + 1.5, "cm"), pad_y = unit(ypad + 1.5, "cm")) + 
                                                    annotation_north_arrow(location = scal.loc, which_north = "true", height = unit(1.25,"cm"), width = unit(1,'cm'),
                                                                           pad_x = unit(xpad + 1.5, "cm"), pad_y = unit(ypad+1.9, "cm"),style = north_arrow_fancy_orienteering)
    
    # Some finishing touches...I don't know that the xlim and ylim are actually necessary, think it is now redundant
    if(legend == F) {
      pect_plot <- pect_plot + 
        coord_sf(xlim = xlim,ylim=ylim) + 
        theme(legend.position = "none",text = element_text(size=txt.size), panel.background=element_blank())  +
        xlab(NULL) + 
        ylab(NULL)#+ theme_minimal()
    }
    if(legend == T) 
    {
      if(exists("brk")){
        if(length(brk) <= 6) hgt <- unit(0.5,'cm')
        if(length(brk) > 6 & length(brk) <= 12) hgt <- unit(0.75,'cm')
        if(length(brk) > 12) hgt <- unit(1,'cm')
      }
      if(!exists("brk")) hgt <- unit(0.5,'cm')
      
      pect_plot <- pect_plot + 
        coord_sf(xlim = xlim,ylim=ylim)+
        theme(legend.key.height =hgt,text = element_text(size=txt.size), panel.background=element_blank()) +
        xlab(NULL) +
        ylab(NULL)
    }

  # Now add in option to show axis labels in Deg-Min and Deg-Min-Sec if you want.
    if(!is.null(axes))
    {
      # Need to shrink it here so I get the boundaries right
      pect_plot <- pect_plot + coord_sf(expand=F)
      
      ylim <- layer_scales(pect_plot)$y$range$range
      xlim <- layer_scales(pect_plot)$x$range$range
      
      locs <- st_as_sf(data.frame(X = xlim,Y = ylim),coords = c("X","Y"),crs = c_sys,remove =F)
      locs <- st_transform(locs,crs=4326)
      
      tmp <- locs %>%  mutate(lon = unlist(map(locs$geometry,1)),
                              lat = unlist(map(locs$geometry,2)))
      
      # Pretty doesn't work because we want these to be the same length
      lat.loc <- pretty(tmp$lat,n=5)
      n.lat <- length(lat.loc)
      lon.loc <- pretty(tmp$lon,n=5)
      n.lon <- length(lon.loc)
      
      if(n.lat != n.lon)
      {
        if(n.lat < n.lon) lat.loc <- c(lat.loc,rep(max(lat.loc),(n.lon-n.lat)))
        if(n.lat > n.lon) lon.loc <- c(lon.loc,rep(max(lon.loc),(n.lat-n.lon)))
      }
      # To do the conversion to Dec Deg I'll need to combine these and do a conversion...
      
      #n.ticks <- nrow(tmp)
      lat.loc <- lat.loc[2:(n.lat-1)]
      lat.tmp <- convert.dd.dddd(lat.loc,'deg.min.sec')
      lon.loc <- lon.loc[2:(n.lon-1)]
      lon.tmp <- convert.dd.dddd(lon.loc,'deg.min.sec')
      # Determine where you want to put your axis tick marks.  Make sure they all are within the plotting domain of tst or you'll get an unequal length vector error when you make next plot
      #lon.loc <- c(66.167,66,65.833333,65.6667,65.5)
      # Convert to deg-min-sec
      # Repeat
      #lat.loc <- c(43.416667,43.5,43.583333,43.6667,43.75)
      # Now make some labels based on the above, if you can figure out how to get a minute symbol in here you are better than me!
      if(axes == "DMS")
      {
        lon.disp <- paste0(substr(lon.tmp$Degree_Min$Degree_Minutes,2,3),expression("*{degree}*"), substr(lon.tmp$Degree_Min$Degree_Minutes,5,6),expression("*{minute}*"), substr(lon.tmp$Degree_Min_Sec$Degree_Minute_Seconds,8,9),expression("*{second}*W"))
        lat.disp <- paste0(substr(lat.tmp$Degree_Min$Degree_Minutes,1,2),expression("*{degree}*"), substr(lat.tmp$Degree_Min$Degree_Minutes,4,5),expression("*{minute}*"), substr(lat.tmp$Degree_Min_Sec$Degree_Minute_Seconds,7,8),expression("*{second}*N"))
      }
      if(axes == "DM")
      {
        lon.disp <- paste0(substr(lon.tmp$Degree_Min$Degree_Minutes,2,3),expression("*{degree}*"), substr(lon.tmp$Degree_Min$Degree_Minutes,5,6),expression("*{minute}*W"))
        lat.disp <- paste0(substr(lat.tmp$Degree_Min$Degree_Minutes,1,2),expression("*{degree}*"), substr(lat.tmp$Degree_Min$Degree_Minutes,4,5),expression("*{minute}*N"))
      }
      # And then replot the figure
      pect_plot <- pect_plot + scale_x_continuous(breaks =lon.loc,labels=parse(text = lon.disp)) +
        scale_y_continuous(breaks = lat.loc,labels=parse(text = lat.disp)) 
    } # end if(!is.null(axes))
  } # end if(plot_as != 'plotly')

  # Not implemented, strangely it seems native plotly is unable to handle the variaty of inputs we have here.
  if(plot_as == "plotly")
  {
    # First we want to stitch all the line objects into one object, makes plotly more efficient...
    multi.lines.base <- st_as_sf(data.frame(ID = "box",geometry = data.frame(st_geometry(b.box)))) %>% st_cast(to="MULTILINESTRING")
    multi.lines <- multi.lines.base[-1,]
    #st_geometry(multi.lines) <- 'x'
    # If you have an existing gg.object to use as a base plot pull that in here
    if(!is.null(gg.obj)) pect_plot <- ggplotly(gg.obj)
    # If need be set up the basemap
    #if(is.null(gg.obj)) pect_plot <- plot_ly(multi.lines.base,color = I('grey'))
    #
    

    # Now add in the lines...
    if(exists("nafo.divs")) 
    {
      nafo.divt <- nafo.divs %>% dplyr::select(id,geometry) 
      names(nafo.divt) <- c("ID","geometry")
      multi.lines <- rbind(multi.lines,st_geometry(nafo.divt))
      
    }
    if(exists("nafo.sub")) 
    {
      nafo.subt <- nafo.sub %>% dplyr::select(id,geometry) 
      names(nafo.subt) <- c("ID","geometry")
      multi.lines <- rbind(multi.lines,nafo.subt)
    }

    if(exists("inshore.spa")) 
    {
      # I need to clip the lines that go over the land
      if(exists("land.sf"))
      {
        inshore.spa <- rgeos::gDifference(as_Spatial(inshore.spa),as_Spatial(land.sf))
        inshore.spa <- st_as_sf(inshore.spa)
        # For some reason st_difference doesn't work...
        #inshore.spa <- st_difference(inshore.spa,land.sf)
        inshore.spa$ID <- "Inshore"
      }
      multi.lines <- rbind(multi.lines,inshore.spa)
    }
    if(exists("offshore.spa")) multi.lines <- rbind(multi.lines,offshore.spa)
    # if(exists("eez")) 
    # {
    #   eezt <- eez %>% dplyr::select(UUID,geometry)
    #   names(eezt) <- c("ID","geometry")
    #   multi.lines <- rbind(multi.lines,eezt)
    # }
    
    # Or add the lines to the existing plot
    if(is.null(gg.obj))  if(!exists("pect_plot")) 
    {
      multi.lines <- st_cast(multi.lines,"MULTILINESTRING")
      pect_plot <- multi.lines %>% plot_ly() %>% add_sf(color=I("white"))
    }
    
    if(exists("bathy.gg")) 
    {
      bathy1 <- st_as_sf(rasterToContour(bathy,levels = c(-5000,-500,seq(-450,0,50))))
      # Clip to the land
      if(exists("land.sf")) bathy1 <- st_as_sf(rgeos::gDifference(as_Spatial(bathy1),as_Spatial(land.sf)))
      pect_plot <- pect_plot %>%  add_sf(data=bathy1,color=I('light blue')) %>% hide_legend()
      
    } # end  if(exists("bathy.gg"))
    
    if(exists("eez")) pect_plot <- pect_plot %>% add_sf(data = eez, color=I("firebrick"))  %>% hide_legend()
    
    # Stick any management boundaries in here if we have them... Note how to change the line properties!
    if(nrow(multi.lines) > 0) pect_plot <-  pect_plot %>% add_sf(data = multi.lines,color=I("grey30"),line = list(width =0.5))  %>% hide_legend()
    
    if(exists("land.sf")) pect_plot <- pect_plot %>% add_sf(data=land.sf,color=I("grey40"))  %>% hide_legend()
    if(exists("final.strata"))
    {
      if(add_layer$survey[2] == "detailed") 
      {
        final.strata$strat_ID <-paste (substr(final.strata$ID,1,3),final.strata$Strt_ID,sep="-")
        if(is.factor(final.strata$col)) final.strata$col <- as.character(final.strata$col)
        #final.strata$col3 <- 1:nrow(final.strata)
        pect_plot <- pect_plot  %>%
          add_sf(data=final.strata %>% group_by(strat_ID), split = ~ strat_ID, text = ~paste("Strata is:", strat_ID), #color = ~strat_ID,
                 line = list(width=0,color='black'), 
                 #line=~col, # For some reason this was causing problems in the summer of 2020 so removed and used above hack instead
                 fillcolor = ~col,
                 hoveron = "fills",
                 hoverinfo = "text") %>% 
          hide_legend()
        
      } # end if(exists("final.strata"))
      if(add_layer$survey[2] == "outline") pect_plot <- pect_plot %>% add_sf(data=final.strata,color = I('gray75'))  %>% hide_legend()
    } # end if(exists("final.strata"))
    
    if(exists("spd"))
    {
      #
      pect_plot <- pect_plot %>% add_sf(data=spd,split = ~ brk,  text = ~paste("Values:", brk),
                                        line = list(width=0), fillcolor = ~col,
                                        hoveron = "fills",
                                        hoverinfo = "text")  
    } # end if(exists("spd"))
    
    if(exists("custom.cols"))
    {
      #
      pect_plot <- pect_plot %>% add_sf(data=custom.cols,split = ~ brk,  text = ~paste("Values:", brk),
                                        line = list(width=0), fillcolor = ~col,
                                        hoveron = "fills",
                                        hoverinfo = "text")  
    } # end if(exists("custom.cols"))
    
    if(exists('custom') & !exists("custom.cols"))
    {
      pect_plot <- pect_plot %>% add_sf(data=custom,color = colr)  
    }
    # If we don't want the legend
    if(legend == F) pect_plot <- pect_plot %>% hide_legend()
    
  } # end if(plot_as == "plotly")
  
  # At the end we want to 'unexpand' the figure so we don't have an annoying buffer!
  #browser()
  if(plot_as != "plotly")
  {
    pect_plot <- pect_plot + 
      # scale_x_continuous(limits=xlim) + # keep this so you can extract coords outside pecjector
      # scale_y_continuous(limits=ylim) + # keep this so you can extract coords outside pecjector
      coord_sf(expand=F, xlim=xlim, ylim=ylim) + xlab("") + ylab("") + 
                             theme(panel.grid=element_blank(),panel.background = element_rect(fill = 'white'))
  } # end if(plot_as != "plotly")
  
  if(plot_as == 'ggplotly') 
  {
    pect_plot <- pect_plot + theme_map() 
    if(legend == F) pect_plot <- ggplotly(pect_plot) %>% hide_legend()
    if(legend == T) pect_plot <- ggplotly(pect_plot) 
  } # end if(plot_as == 'ggplotly') 
  

  if(plot == T) print(pect_plot) # If you want to immediately display the plot
  #browser()
  return(pect_plot = pect_plot)
} # end function
