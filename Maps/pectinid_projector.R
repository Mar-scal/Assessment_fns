# This is a hack of a function the INLA lasses/lads use to plot their INLA model results
# Stand-alone function created by DK in Nov 2018

# Note that we need a couple of cute little functions for this to work.  Be sure to all call "addalpha" , "convert_coords", and "all.layers"
# Before running this function, but I haven't settled on a directory for them quite yet...

# Arguements:

## The general mapping inputs if you are just wanting to produce a spatial map without an INLA surface
#1:   area      The area you want to plot, this can be a custom field (see function convert_coords.R for options) or a dataframe with
#               The coordinates and the projection of those coordindates specified.  Default provides Maritime Region boundaries
#               in lat/long coordinates and a WGS84 projection.
#2:   repo      The repository from which you will pull the data.  The default is "github" (which has all the main data) 
#               option is to look for a 'local' directory based upon the "direct specification used 
#3:   c_sys     What coordinate system are you using, options are "ll"  which is lat/lon and WGS84 or "utm_zone" which is utm, you put in the zone yourself
#               for example if you want utm_20 you can enter "+init=epsg:32620" which will use utm zone 20 which is best for BoF and SS 
#               for utm_19  use +init=epsg:32619, this is best for GB, SPA3 and 6, if you have are using something else go nuts!
#4:   add_EEZ   Do you want to add in the EEZ, default = NULL which does nothing.  If you want the EEZ this pulls it from either
#               our github respository (if repo == "github") and add_EEZ is not NULL.  If repo == "local" which looks for a GIS repository
#               for the EEZ based on your  directory if add_EEZ is NOT NULL.  Basically add_EEZ = "anything except null" will results in it being plotted
#5:  add_bathy  Do you want to add in the bathymetry, default = NULL which does nothing.  If you want to add the bathy you simply need to supply the 
#               resolution that you want.  The smaller the resolution the slower this runs, it relies on data provided by NOAA, so if the server
#               is down this won't plot the bathymetry.  add_bathy = 10 plots the 10 meter bathymetry for the region selected by your xlim and ylim
#6:  add_land   Do you want to add land to the figure, T/F, default = T which plots the the world in HiRes from the mapdata pacakge
#7:  add_nafo   Do you want to add the NAFO divisions to the figure, Three options for NAFO, deault is "no" which doesn't add nafo areas.  Options include
#               "main" which only plots the main areas, and "sub" which will plot the sub-areas.  The sub-areas are only available locally as
#               the NAFO site does not have a GIS layer which includes the sub-areas.
#8:  add_sfas   Do you want to add the sfa boundariesto the figure.  NULL doesn't add anything.
#               If you are using the github repository the options are "inshore", "offshore", or "all"
#               If you are sourcing these locally you'll need to supply the directory for the shapefile
#9:  add_strata Do you want to add the survey strata to the figure, NuLL = default.  NULL doesn't add anything.
#               If you are using the github repository the options are "inshore", "offshore", or "all"
#               If you are sourcing these locally you'll need to supply the directory for the shapefile
#10: add_obj    Do you have some spatial objects to add?  Default = NULL which doesn't add anything. 
#               While this was designed for seedboxes any sp class objects will work.  This can either be an SP object in R
#               Or will plot the seedboxes that were closed in the year you specify (e.g. add_obj = 2018 will pull in all seedboxes)
#               that were closed in 2018
#11: add_custom Do you have a specific object you'd like to add, this can be a csv or shapefile, you specify exactly where the
#               custom layer/PBS massing object is.
#               If using PBSmapping this assumes that you have the data formatted properly in a csv or xlsx file and 
#               that the proection for the data is WGS84.  Default = NULL
#12:  direct    If you aren't using the GitHub respoistory the directory that your shapefiles reside.  The default is the
#               location of the files on the ESS directory.  If repo = "github" this is ignored.  This is limited to look for shapefiles 
#               that are found in a similar repository structure to GitHub, this is just pointing to where those are.  If you want to add
#               shapefiles that are in a different folder structure/name either use the add_custom option or just add these
#               after running this function (you'd likely need to overplot the land after if you were doing that)

## The INLA related inputs, if field and mesh are not supplied these won't do anything.  You do need field and mesh if plotting an INLA model result
#13:   field    The values from the random field generated using INLA.  Default = NULL which just plots a nice map of the "area" chosen
#               Note that the field and the mesh need to be in the same projection as c_sys.
#14:   mesh     The INLA mesh that corresponds to the values of field, which gets projected properly in this function .
#               Default = NULL which results in a plot of a nice map of the 'area' chosen
#               Note that the field and the mesh need to be in the same projection as c_sys.
#15:   zlim     The range of values for the field plotted, default is  c(0,1) and this seems to be overwritten if you supply levels outside the 0-1 range.
#16:   dims     The number of X and Y values for the INLA surface.  Higher is better resolution, too high is silly. Default = c(50, 50) which is pretty low res
#17:   trans    Do you want to transform the spacing of the levels "none" (default), and "exp" (e.g. for a model with log link) is supported, but you can add more that!
#18:   clip     Do you want to clip the area plotted to some polygon?  Default = NULL which is no clipping
#19:   lvls     The number of levels you want to set up.  Default is seq(0,1,by=0.01) which is a lot of levels!
#20:  colors    What colours would you like to use for the colour ramp.  Default = c("blue","white","yellow","darkred"), will get split into more colours based on lvls
#21:  alpha     Do you want the colours to have some level of transparency.  0 = translucent, 1 = opaque, Default = 0.8
#22:  plot_package  NULL for base R, "ggplot2" for ggplot2
pecjector = function(area = data.frame(y = c(40,46),x = c(-68,-55),proj_sys = "+init=epsg:4326"),repo = "github",c_sys = "ll", 
                     add_EEZ = NULL, add_bathy = NULL,add_land = F,add_nafo="no",add_sfas = NULL, 
                     add_strata = NULL, add_obj = NULL,add_custom = NULL,
                     direct = "Y:/Offshore scallop/Assessment",
                     # The below control the INLA surface added to the figure.
                     field = NULL, mesh=NULL, 
                     zlim = c(0,1), dims = c(50, 50), trans= "none", clip= NULL,
                     lvls = seq(0,1,by=0.01),colors = c("blue","white","yellow","darkred"),alpha = 0.8,
                     plot_package = NULL, ...
) 
{ 
  require(splancs) || stop("You need le package splancs, thanks!")
  require(PBSmapping) || stop("You need PBSmapping, thanks!")
  require(sp) || stop("You need sp, thanks!")
  require(rgdal)  || stop("You need rgdal, thanks!")
  require(rgeos) || stop("You need rgeos, thanks!")
  require(raster) || stop("You need raster, thanks!")
  require(maptools) || stop("You need maptools, thanks!")
  require(maps) || stop("You need maps, thanks!")
  require(mapdata)|| stop("You need mapdata, thanks!")
  require(viridis) || stop("You need the viridis package, thanks!")
  require(marmap) || stop("You need the marmap function to get the bathymetry")
  
  # If you are using github then we can call in the below 3 functions needed below from github, they aren't in production currently so should work fine
  if(repo == "github")
  {
    require(RCurl)|| stop("You need RCurl or this will all unfurl!")
    eval(parse(text = getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R", ssl.verifypeer = FALSE)))
    eval(parse(text = getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/add_alpha_function.R", ssl.verifypeer = FALSE)))
    eval(parse(text = getURL("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/combine_shapefile_layers.R", ssl.verifypeer = FALSE)))
  } # end if(repo == "github")
  
  # If getting the data from a local source...
  if(repo == "local")
  {
    source(paste(direct,"Assessment_fns/Maps/convert_coords.R",sep="")) #logs_and_fish is function call
    source(paste(direct,"Assessment_fns/Maps/add_alpha_function.R",sep="")) # The new scallopMap
    source(paste(direct,"Assessment_fns/Maps/combine_shapefile_layers.R",sep="")) # The new scallopMap
  } # end if(repo == "local")
  
  
  if(!exists("addalpha")) stop("You need to source the add_alpha_function.R for this to work, it should be in the same folder location as this function")
  if(!exists("convert.coords")) stop("You need to source the convert_coords.R for this to work, it should be in the same folder location as this function")
  if(!exists("all.layers")) stop("You need to source all.layers.R for this to work, it should be in the same folder location as this function")
  # Don't do this if the field and mesh lengths differ.
  if(!is.null(field)) stopifnot(length(field) == mesh$n) 
  
  
  # Now if you set the c_sys to "ll" that means "ll" and WGS84, so explicitly set this now.
  if(c_sys == "ll") c_sys <- "+init=epsg:4326"
  
  # Now we need to get our ylim and xlim using the convert.coords function
  if(is.data.frame(area))
  {
    # Get our coordinates in the units we need them, need to do some stick handling if we've entered specific coords above
    coords <- convert.coords(plot.extent = area[,c("x","y")],c_sys = c_sys,initial.proj = area$proj_sys[1])
    xlim <- coords@bbox[rownames(coords@bbox) == 'x']
    ylim <- coords@bbox[rownames(coords@bbox) == 'y']
  } # end   if(is.data.frame(area))
  
  if(!is.data.frame(area)) 
  {
    # 
    coords <- convert.coords(plot.extent = area,c_sys = c_sys)
    xlim <- coords@bbox[rownames(coords@bbox) == 'x']
    ylim <- coords@bbox[rownames(coords@bbox) == 'y']
  } # end if(!is.data.frame(area)) 
  
  # Get the spatial coordinates correct for the boxes, likely they are already in the Lat/Long WGS84 format, but might not be...
  # Note that this requires the boxes are already spatial polygons and have a coordinate reference system.
  if(!is.null(add_obj)) add_obj <- spTransform(add_obj,CRS(c_sys))
  
  # We will want to bound the data to the area of interest (this is especially important if we are using something other than lat/lon)
  # This assumes that we have entered the xlim and ylim in the coordinate reference system we have chosen
  # Now I actually want the bounding box to be about 5% larger than the xlim and ylim values just so nothing gets clipped 
  # incorrectly, so I need to do a little sloppy stick handling...
  
  if(xlim[1] < xlim[2])
  {
    if(xlim[1] < 0) x1 <- xlim[1] + 0.05*xlim[1]
    if(xlim[1] > 0) x1 <- xlim[1] - 0.05*xlim[1]
    # Now we could have xlim[2] being positive when xlim 1 is negative so this needs it's own if's
    if(xlim[2] < 0) x2 <- xlim[2] - 0.05*xlim[2]
    if(xlim[2] > 0) x2 <- xlim[2] + 0.05*xlim[2]
    
  } else{
    if(xlim[1] < 0) y1 <- xlim[1] - 0.05*xlim[1]
    if(xlim[1] > 0) y1 <- xlim[1] + 0.05*xlim[1]
    # Now we could have xlim[2] being positive when xlim 1 is negative so this needs it's own if's
    if(xlim[2] < 0) y2 <- xlim[2] + 0.05*xlim[2]
    if(xlim[2] > 0) y2 <- xlim[2] - 0.05*xlim[2]
  } # end else and the if tied to xlim
  # Now do the same for y's
  if(ylim[1] < ylim[2])
  {
    if(ylim[1] < 0) y1 <- ylim[1] + 0.05*ylim[1]
    if(ylim[1] > 0) y1 <- ylim[1] - 0.05*ylim[1]
    # Now we could have ylim[2] being positive when ylim 1 is negative so this needs it's own if's
    if(ylim[2] < 0) y2 <- ylim[2] - 0.05*ylim[2]
    if(ylim[2] > 0) y2 <- ylim[2] + 0.05*ylim[2]
    
  } else{
    if(ylim[1] < 0) y1 <- ylim[1] - 0.05*ylim[1]
    if(ylim[1] > 0) y1 <- ylim[1] + 0.05*ylim[1]
    # Now we could have ylim[2] being positive when ylim 1 is negative so this needs it's own if's
    if(ylim[2] < 0) y2 <- ylim[2] + 0.05*ylim[2]
    if(ylim[2] > 0) y2 <- ylim[2] - 0.05*ylim[2]
  } # end else and the if tied to ylim
  
  # i.e. these can't be lat/lon while the c_sys is UTM
  b.box <- as(raster::extent(c(x1,x2,y1,y2)), "SpatialPolygons")
  proj4string(b.box) = c_sys
  
  # If we are going to add the EEZ do this...
  if(!is.null(add_EEZ)) 
  {
    # if we already have the full eez in the global environment we don't need to reload it, we do need to sub-set it and project it though
    if(!exists("eez.all"))
    {
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
        eez.all <- readOGR(paste0(temp2, "/EEZ.shp"))
      } else { # end if(repo == 'github' )
                loc <- paste0(direct,"Data/Maps/approved/GIS_layers/EEZ")
                eez.all <- readOGR(loc)
              } # end the else
    } # end if(!exists("eez.all"))
    # We then need to transform these coordinates to the coordinates of the eez data
    eez.bbox <- spTransform(b.box,proj4string(eez.all))
    # Then intersect the coordiates so we only plot the part of the eez we want
    eez <- gIntersection(eez.all,eez.bbox)
    # and now transform this eez subset to the proper coordinate system. If there is an eez in the picture....
    if(!is.null(eez)) eez <- spTransform(eez,c_sys)
  } # end if(!is.null(add_EEZ)) 
  # browser()
  # For the moment the bathymetry can only be added to a figure if it is in lat-lon coordinates as we don't have a bathymetric shapefile
  if(!is.null(add_bathy) && c_sys == "+init=epsg:4326") # This would need done everytime as the boundng box could change for each call
  {
    # The bathymetry needs to be in lat-lon, this is Lat/lon and WGS 84, I'm assuming that's what we want!
    proj4string(b.box) = c_sys
    # If c.sys isn't in lat/lon we need to tranform it to lat-lon
    # The bathymetry data is given in NOAA as Lat/Lon WGS84 according to NOAA's website.  https://www.ngdc.noaa.gov/mgg/global/
    bathy <- getNOAA.bathy(lon1 = b.box@bbox[1] ,lon2=b.box@bbox[3],lat1 = b.box@bbox[2],lat2=b.box@bbox[4],resolution = add_bathy)
    # Now I need to convert this to a sp object, note that this data is lat/lon and WGS 84
    bathy.sp <- as.SpatialGridDataFrame(bathy)
    # Because we have the bathymetry on a grid rather than as a vector we can't easily reproject, the lazy solution is that we
    # won't plot bathymetry data unless c_sys = "ll", there is a solution but it is slow
  } # end if(!is.null(add_bathy))
  
  if(add_land == T && !exists("land.all"))
  {
    # We also want to get the land
    land <- maps::map(database = "worldHires", c("USA","Canada"),fill=TRUE,col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(land$names, ":"), function(x) x[1])
    land.all <- map2SpatialPolygons(land, IDs = IDs,proj4string =  CRS("+init=epsg:4326"))
    # This is a hack to clean up maps objects in R, if you ever see the error
    #"TopologyException: Input geom 1 is invalid: Self-intersection at or near point"
    # It is due to cleans up issues with the polygons coming from maps in this case, but any "bad" polygons get taken care of.
    land.all <- gSimplify(land.all,tol=0.00001)
    # This really should be done on projected data not Lat/Lon data, but this does the trick for our purposes.  If using this for something
    # more than simply trying to draw some land we might want to do something more complex.
    land.all <- gBuffer(land.all,byid=T,width=0)
  } # end if(add_land == T && !exists("land.all"))
  # f we are lat/lon and WGS84 we don't need to bother worrying about clipping the land (plotting it all is fine)
  if(c_sys == "+init=epsg:4326" && add_land ==T) land.sp <- land.all
  
  # Now if we need to transform the coordinates we will clip the land object so that we aren't trying to project like a sucker.
  if(c_sys != "+init=epsg:4326" && add_land == T) 
  {
    # Now we get the land into the coordinate system of the land
    land.b.box <- spTransform(b.box,"+init=epsg:4326")
    # Now clip the land to this bounding box
    land.sp <- gIntersection(land.all,land.b.box)
    # And now we can transform the land, if there is any to be plotted!
    if(!is.null(land.sp)) land.sp <- spTransform(land.sp,c_sys)
  } # end if(c_sys != +init=epsg:4326) 
  
  
  # If you want to add the NAFO division, the autoritaive versions are on the web so when we say "repo = 'github'", for NAFO this is actually going
  # to NAFO's website to get them.  We have a version of these saved locally as well which is accessed when "repo = 'local'"
  #browser()
  if(add_nafo != "no")
  {
    # If they don't already exist and our repo is github and we just want the main nafo division go get them from online
    if(repo == 'github' && !exists("nafo.divs") && add_nafo == "main")
    {
      # Figure out where your tempfiles are stored
      temp <- tempfile()
      # Download this to there
      download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/NAFO/Divisions/Divisions.zip", temp)
      # Figure out what this file was saved as
      temp2 <- tempfile()
      # Unzip it
      unzip(zipfile=temp, exdir=temp2)
      # Now read in the shapefile
      nafo.divs <- all.layers(temp2)
      # Now transform the data to the coordinates we are using.
      for(i in 1:length(nafo.divs)) nafo.divs <- spTransform(nafo.divs[[i]],c_sys)

      
    } # if(repo == 'github' && !exists("nafo.divs"))
    ###########PICK IT UP HERE PECTINID PROJECTOR WITH NAFO DIVISIONS IS BROKEN IN HERE SOMEWHERE
    # if we want the main divisions and 
    if(repo == 'local' && !exists("nafo.divs") && add_nafo == "main")
    {
      loc <- paste0(direct,"Data/Maps/approved/GIS_layers/NAFO/Divisions")
      nafo.divs <- all.layers(loc)
      for(i in 1:length(nafo.divs)) nafo.divs[[i]] <- spTransform(nafo.divs[[i]],c_sys)
    } # end if(repo == 'local' && !exists("nafo.divs") && add_nafo = "main")

    # Now if we want the nafo sub-areas we do this, for the locals
    if(repo == 'github' && !exists("nafo.subs") && add_nafo == "sub")
    {
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/NAFO/Subareas/Subareas.zip", temp)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        # This pulls in all the layers from the above location
        nafo.subs <- all.layers(temp2)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(nafo.subs)) nafo.subs[[i]] <- spTransform(nafo.subs[[i]],c_sys)
      } # end if(add_sfas != "offshore")
    
    # Now if we want the nafo sub-areas we do this, for the locals
    if(repo == 'local' && !exists("nafo.subs") && add_nafo == "sub")
    {
      # Now if we want the nafo sub-areas we do this...
      loc <- paste0(direct,"Data/Maps/approved/GIS_layers/NAFO/Subareas")
      nafo.subs <- all.layers(loc)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      for(i in 1:length(nafo.subs)) nafo.subs[[i]] <- spTransform(nafo.subs[[i]],c_sys)
    } # end if(repo == 'local' && !exists("nafo.subs") && add_nafo = "main")
    
  } # end if(add_nafo != "no")
  #browser()

  
  
  # Now do we want to add in the SPA and SFA's for the region, this is using the approve Inshore polygons that Leslie Nasmith developed in 2014, these are
  # all NAD83 lat/lon's.  There are a bunch of shapefiles for inshore this this isn't always speedy
  if(!is.null(add_sfas)) 
  {
    if(repo == 'github')
    {
      if(add_sfas != "offshore" && !exists("inshore.spa"))
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
        inshore.spa <- all.layers(temp2)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(inshore.spa)) inshore.spa[[i]] <- spTransform(inshore.spa[[i]],c_sys)
      } # end if(add_sfas != "offshore")
      
      if(add_sfas != "inshore" && !exists("offshore.spa"))
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
        offshore.spa <- all.layers(temp2)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(offshore.spa)) offshore.spa[[i]] <- spTransform(offshore.spa[[i]],c_sys)
      } # end if(add_sfas != "offshore")  
      
    }# end if(repo = 'github')
    
    # Now if you aren't using github do this...
    if(repo == 'local')
    {
      if(add_sfas != "offshore" && !exists("inshore.spa"))
      {
        loc <- paste0(direct,"Data/Maps/approved/GIS_layers/inshore")
        inshore.spa <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(inshore.spa)) inshore.spa[[i]] <- spTransform(inshore.spa[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
      if(add_sfas != "inshore" && !exists("offshore.spa"))
      {
        loc <- paste0(direct,"Data/Maps/approved/GIS_layers/offshore")
        # This pulls in all the layers from the above location
        offshore.spa <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(offshore.spa)) offshore.spa[[i]] <- spTransform(offshore.spa[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
    } # end if(repo == 'local')
  } # end if(!is.null(add_sfas)) 
  #browser()
  # Now we do the same thing for the strata
  if(!is.null(add_strata)) 
  {
    if(repo == 'github')
    {
      if(add_strata != "offshore"  && !exists("inshore.strata"))
      {
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_survey_strata/inshore_survey_strata.zip", temp)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        
        # This pulls in all the layers from the above location
        inshore.strata <- all.layers(temp2)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(inshore.strata)) inshore.strata[[i]] <- spTransform(inshore.strata[[i]],c_sys)
      } # end if(add_strata != "offshore")
      
      if(add_strata != "inshore" && !exists("offshore.strata"))
      {
        # Notewe only do this if there is no offshore.strata object already loaded, this will really speed up using this function multiple times as you only will load these data once.
        # The only problem with this would be if offshore strata was loaded as an object but it wasn't the offshore strata we wanted!
        
        # Figure out where your tempfiles are stored
        temp <- tempfile()
        # Download this to the temp directory you created above
        download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore_survey_strata/offshore_survey_strata.zip", temp)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        
        # This pulls in all the layers from the above location
        offshore.strata <- all.layers(temp2)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(offshore.strata)) offshore.strata[[i]] <- spTransform(offshore.strata[[i]],c_sys)
      } # end if(add_strata != "offshore")  
      
    }# end if(repo = 'github')
    
    # Now if you aren't using github do this...
    if(repo == 'local')
    {
      if(add_strata != "offshore" && !exists("inshore.strata"))
      {
        loc <- paste0(direct,"Data/Maps/approved/GIS_layers/inshore_survey_strata")
        inshore.strata <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(inshore.strata)) inshore.strata[[i]] <- spTransform(inshore.strata[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
      if(add_strata != "inshore" && !exists("offshore.strata"))
      {
        loc <- paste0(direct,"Data/Maps/approved/GIS_layers/offshore_survey_strata")
        # This pulls in all the layers from the above location
        offshore.strata <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(offshore.strata)) offshore.strata[[i]] <- spTransform(offshore.strata[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
    } # end if(repo == 'local')
  } # end if(!is.null(add_strata)) 
  
  
  # Here you can add a custom PBSmapping object or shapefile here
  if(!is.null(add_custom))
  {
    # If it is an xls or a csv we assume we have a PBSmapping object
    if(grepl(".xls",add_custom) || grepl(".csv",add_custom))
    {
      custom <- NULL
      if(grepl(".csv",add_custom)) temp <- read.csv(add_custom)
      if(grepl(".xls",add_custom)) temp <- read_excel(add_custom,sheet=1) # This will only pull in the first sheet, don't get fancy here
      temp <- as.PolySet(temp,projection = "LL") # I am assuming you provide Lat/Lon data and WGS84
      temp <- PolySet2SpatialLines(temp) # Spatial lines is a bit more general (don't need to have boxes closed)
      temp <- spTransform(temp,c_sys)
      custom$custom <- temp # Just want to make it a list object so it works like the shapefile works.
    } else { # If it doesn't then we assum we have a shapefile, if anything else this won't work.
      # 
      custom <- all.layers(add_custom)
      # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
      for(i in 1:length(custom)) custom[[i]] <- spTransform(custom[[i]],c_sys)
    } # end the else statement
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
    if(plot_package=="base" | is.null(plot_package)){
      par(mar=c(4,4,1,1))
      image.plot(list(x = projec$x, y=projec$y, z = field.projec), xlim = xlim, ylim = ylim, zlim=zlim, 
                 axes=F,las=1,add=F, breaks=lvls, axis.args= arg.list,
                 col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=alpha))
    }
  } # end if(!is.null(field))
  
  # If we don't specify the field let's just make a blank plot of the correct type..
  if(plot_package=="base" | is.null(plot_package)){
    if(is.null(field)) plot(coords,col='white')
    # 
    if(!is.null(add_obj)) plot(add_obj,angle = 45,density=8,col="black",lwd=.5,add=T)
    # Add the eez in a funky colour and fairly thick line if it will show up on the figure.
    if(!is.null(add_EEZ)) 
    {
      if(!is.null(eez)) plot(eez,lwd=3,col=magma(1,alpha=0.3,begin=0),add=T) # EEZ b/t Can and US
    } # end if(!is.null(add_EEZ)) 
    # Add the bathymetry
    if(!is.null(add_bathy) && c_sys == "+init=epsg:4326") plot(bathy,add=T,col = "blue") # bathymetry, only works if plotting in lat/lon coordinates and WGS84...
    # Add the NAFO regions, make them dotted lines and thin
    #browser()
    if(add_nafo =="main") for(i in 1:length(nafo.divs))plot(nafo.divs[[i]],add=T,lwd= 0.5) # NAFO divisions
    if(add_nafo =="sub") for(i in 1:length(nafo.subs)) plot(nafo.subs[[i]],add=T,lwd=0.5) # NAFO sub areas
    # now add in the spa boundaries, these are set up as a number of layers in an sp object
    if(!is.null(add_sfas))  
    {
      # Here's all we need for inshore.
      if(add_sfas != "offshore") for(i in 1:length(inshore.spa)) plot(inshore.spa[[i]],add=T)   
      if(add_sfas != "inshore") for(i in 1:length(offshore.spa)) plot(offshore.spa[[i]],add=T,border="blue")   
    } # end if(add_sfas == T) 
    
    # now add in the strata boundaries, these are set up as a number of layers in an sp object
    if(!is.null(add_strata))  
    {
      # Here's all we need for inshore.
      if(add_strata != "offshore") for(i in 1:length(inshore.strata)) plot(inshore.strata[[i]],add=T)   
      if(add_strata != "inshore") for(i in 1:length(offshore.strata)) plot(offshore.strata[[i]],add=T,border="blue")   
    } # end if(add_sfas == T) 
    # We always want to add the land at the end so it overplots pieces we don't care about...
    if(!is.null(add_custom)) for(i in 1:length(custom)) plot(custom[[i]],add=T)   
    
    if(add_land == T) 
    {
      if(!is.null(land.sp)) plot(land.sp,add=T,col = "lightgrey")
    } # end if(add_land == T) 
  }
  #browser()
  # Now if these shapefiles aren't already in the global environment but exist within the function put them into the global environment for later use
  if(!exists("eez.all",where=1) && exists("eez.all"))                   assign('eez.all',eez.all,pos=1)
  if(!exists("nafo.divs",where=1) && exists("nafo.divs"))               assign('nafo.divs',nafo.divs,pos=1)
  if(!exists("nafo.subs",where=1) && exists("nafo.subs"))               assign('nafo.subs',nafo.subs,pos=1)
  if(!exists("land.all",where=1) && exists("land.all"))                 assign('land.all',land.all,pos=1)
  if(!exists("offshore.spa",where=1) && exists("offshore.spa"))         assign('offshore.spa',offshore.spa,pos=1)
  if(!exists("inshore.spa",where=1) && exists("inshore.spa"))           assign('inshore.spa',inshore.spa,pos=1)
  if(!exists("offshore.strata",where=1) && exists("offshore.strata"))   assign('offshore.strata',offshore.strata,pos=1)
  if(!exists("inshore.strata",where=1) && exists("inshore.strata"))     assign('inshore.strata',inshore.strata,pos=1)
  
  # make a ggplot2 obj
  if(plot_package == "ggplot2" && is.null(field) && is.null(add_obj) && !is.null(add_EEZ) &&
      is.null(add_strata) &&
      is.null(add_custom)){
    require(ggplot2)
    
    # here's the ggplot template (pect_ggplot). We'll add geom layers onto this based on the arguments provided
    pect_ggplot <- ggplot() +
      coord_map() + 
      theme_bw() + 
      theme(panel.grid=element_blank()) +
      scale_x_continuous(expand = c(0,0), limits = c(x1, x2)) +
      scale_y_continuous(expand = c(0,0), limits = c(y1, y2)) 
    
    if(!is.null(add_bathy) & c_sys == "+init=epsg:4326"){
      if(!is.null(add_bathy)) {
        input_list <- as.list(substitute(list(...)))
        # Now I need to convert this to a sp object, note that this data is lat/lon and WGS 84
        bathy_f <- fortify.bathy(bathy)
        if(length(input_list$bathy_breaks)>0) pect_ggplot <- pect_ggplot + geom_contour(data = bathy_f, aes(x=x, y=y, z=z), breaks=input_list$bathy_breaks, colour="grey")
        if(is.null(input_list$bathy_breaks)) pect_ggplot <- pect_ggplot + geom_contour(data = bathy_f, aes(x=x, y=y, z=z), binwidth=100, colour="grey")
      }
    }   
    
    if(!is.null(add_sfas)) {
      if((add_sfas == "inshore" | add_sfas == "all")) {
        inshore.spa_f <- NULL
        for(i in 1:length(inshore.spa)){
          ext <- as(extent(x1, x2, y1, y2), "SpatialPolygons")
          crs(ext) <- crs(inshore.spa[[i]])
          inshore.spa_ext <- gIntersection(inshore.spa[[i]], ext, byid=T)
          if(!is.null(inshore.spa_ext) & class(inshore.spa_ext) == "SpatialLinesDataFrame"){
            inshore.spa_f <- fortify(inshore.spa_ext, region="ID")
            pect_ggplot <- pect_ggplot + geom_path(data=inshore.spa_f, aes(x=long, y=lat, group=group), fill=NA, colour="blue")
          }
        }
      }
      
      if(add_sfas == "offshore" | add_sfas == "all") {
        offshore.spa_f <- NULL
        #offshore.spa <- offshore.spa[which(names(offshore.spa) %in% c("GBa", "GBb"))]
        offshore.spa <- offshore.spa[which(grepl(x = names(offshore.spa), pattern="GBa-")==FALSE)]
        for(i in 1:length(offshore.spa)){
          ext <- as(extent(x1, x2, y1, y2), "SpatialPolygons")
          crs(ext) <- crs(offshore.spa[[i]])
          
          if(!i==8){
            offshore.spa_ext <- gIntersection(offshore.spa[[i]], ext, byid=T)
          }
          if(i==8){
            offshore.spa_ext <- NULL
          }
          if(!is.null(offshore.spa_ext)){
            offshore.spa_f <- fortify(model = offshore.spa_ext, region = "ID")
            pect_ggplot <- pect_ggplot + geom_polygon(data=offshore.spa_f, aes(x=long, y=lat, group=group), fill=NA, colour="blue")
          }
        }
      }
    }
    
    if(!is.null(add_EEZ)){
      if(add_EEZ == T) {
        if(!is.null(eez)){
          ext <- as(extent(x1, x2, y1, y2), "SpatialPolygons")
          crs(ext) <- crs(eez)
          eez_ext <- gIntersection(eez, ext, byid=T)
          eez_f<- SpatialLinesDataFrame(eez_ext, match.ID = F, data = data.frame(ID = 1))
          if(!is.null(eez_f)){
            eez_f <- fortify(eez_f, region = "ID")
            pect_ggplot <- pect_ggplot + geom_path(data=eez_f, aes(x=long, y=lat, group=group), colour="red", lwd=2)
          }
        }
      }
    }
    
    if(!is.null(add_nafo)){
      if(add_nafo == "main") {
        ext <- as(extent(x1, x2, y1, y2), "SpatialPolygons")
        crs(ext) <- crs(nafo.divs$Divisions)
        nafo.divs_ext <- gIntersection(nafo.divs$Divisions, ext, byid=T, drop_lower_td = T)
        if(!is.null(nafo.divs_ext) & !is.null(nafo.divs_sub5zejm)){
          nafo.divs_f <- fortify(nafo.divs_ext, region = "ID")
          nafo.divs_sub5zejm_f <- fortify(nafo.divs_sub5zejm, region="ID")
          pect_ggplot <- pect_ggplot + geom_path(data=nafo.divs_f, aes(x=long, y=lat, group=group), colour="black")
        }
      }
      if(add_nafo == "sub") {
        for(i in 1:length(nafo.subs)){
         # if(i==3){
            ext <- as(extent(x1, x2, y1, y2), "SpatialPolygons")
            crs(ext) <- crs(nafo.subs[[i]])
            nafo.subs_int <- gIntersection(nafo.subs[[i]], ext, byid=T, drop_lower_td = T)
            if(!is.null(nafo.subs_int)){ 
              nafo_s <- fortify(nafo.subs_int)
              # nafo_s$name <- names(nafo.subs)[i]
              if(is.data.frame(nafo_s)) pect_ggplot <- pect_ggplot + geom_path(data=nafo_s, aes(x=long, y=lat, group=group), colour="black")
            }
          #}
        }
      }
    }
    
    if(!is.null(add_strata)) {
      if(add_strata == "inshore") {
        #inshore.strata[[i]]
      }
      if(add_strata == "offshore") {
        #offshore.strata[[i]]
      }
    }
    
    if(!is.null(add_land)){
      if(add_land == T) {
        ext <- as(extent(x1, x2, y1, y2), "SpatialPolygons")
        crs(ext) <- crs(land.sp)
        land.sp.int <- gIntersection(land.sp, ext, byid=T)
        if(!is.null(land.sp.int)){ 
          land_f <- fortify(land.sp.int)
          if(dim(land_f)[1]>0) {
            land_f$order <- 1:nrow(land_f)
            pect_ggplot <- pect_ggplot + geom_polygon(data=land_f, aes(x=long, y=lat, group=group), fill="darkgrey", colour="black")
          }
        }
      }
    }
    
    pect_ggplot <<- pect_ggplot +
      xlab("Longitude") + ylab("Latitude")
#  }
  }
} # end function
