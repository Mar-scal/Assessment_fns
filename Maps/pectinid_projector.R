# This is a hack of a function the INLA lasses/lads use to plot their INLA model results
# Stand-alone function created by DK in Nov 2018

# Note that we need a couple of cute little functions for this to work.  Be sure to all call "addalpha" , "convert_coords", and "all.layers"
# Before running this function, but I haven't settled on a directory for them quite yet...

# Arguements:

## The general mapping inputs if you are just wanting to produce a spatial map without an INLA surface
#1:   area      The area you want to plot, this can be a custom field (see function convert_coords.R for options) or a dataframe with
#               The coordinates and the projection of those coordindates specified.  Default provides Maritime Region boundaries
#               in lat/long coordinates and a WGS84 projection.
#2:   repo      The repository from which you will pull the data.  This is either Github (which has all the main data)
#3:   c_sys     What coordinate system are you using, options are "ll"  which is lat/lon and WGS84 or "utm_zone" which is utm, you put in the zone yourself
#               for example if you want utm_20 you can enter "+init=epsg:32620" which will use utm zone 20 which is best for BoF and SS 
#               for utm_19  use +init=epsg:32619, this is best for GB, SPA3 and 6, if you have are using something else go nuts!
#4:   add_EEZ   Do you want to add in the EEZ, default = NULL which does nothing.  If you want the EEZ this pulls it from either
#               our github respository (if github = "repo") or a location of your chosing (this would have to be the full directory)
#               add_EEZ = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/world_eez_boundary"
#5:  add_bathy  Do you want to add in the bathymetry, default = NULL which does nothing.  If you want to add the bathy you simply need to supply the 
#               resolution that you want.  The smaller the resolution the slower this runs, it relies on data provided by NOAA, so if the server
#               is down this won't plot the bathymetry.  add_bathy = 10 plots the 10 meter bathymetry for the region selected by your xlim and ylim
#6:  add_land   Do you want to add land to the figure, T/F, default = T which plots the the world in HiRes from the mapdata pacakge
#7:  add_nafo   Do you want to add the NAFO divisions to the figure, T/F, default = T which grabs the NAFO divisions from online repository.
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

pecjector = function(area = data.frame(y = c(40,46),x = c(-68,-55),proj_sys = "+init=epsg:4326"),repo = "github",c_sys = "ll", 
                     add_EEZ = NULL, add_bathy = NULL,add_land = F,add_nafo=F,add_sfas = NULL, 
                     add_strata = NULL, add_obj = NULL,add_custom = NULL,
                     direct = "Y:/Offshore scallop/Assessment/Data/Maps/approved/",
                     # The below control the INLA surface added to the figure.
                     field = NULL, mesh=NULL, 
                     zlim = c(0,1), dims = c(50, 50), trans= "none", clip= NULL,
                     lvls = seq(0,1,by=0.01),colors = c("blue","white","yellow","darkred"),alpha = 0.8
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
    eval(parse(text = getURL("https://raw.githubusercontent.com/Dave-Keith/Assessment_fns/master/Maps/convert_coords.R", ssl.verifypeer = FALSE)))
    eval(parse(text = getURL("https://raw.githubusercontent.com/Dave-Keith/Assessment_fns/master/Maps/add_alpha_function.R", ssl.verifypeer = FALSE)))
    eval(parse(text = getURL("https://raw.githubusercontent.com/Dave-Keith/Assessment_fns/master/Maps/combine_shapefile_layers.R", ssl.verifypeer = FALSE)))
  } # end if(repo == "github")
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
        download.file("https://raw.githubusercontent.com/Dave-Keith/GIS_layers/master/EEZ/EEZ.zip", temp)
        # Figure out what this file was saved as
        temp2 <- tempfile()
        # Unzip it
        unzip(zipfile=temp, exdir=temp2)
        # Now read in the shapefile
        eez.all <- readOGR(paste0(temp2, "/EEZ.shp"))
      } else{ # end if(repo = 'github')
        eez.all <- read.OGR(add_EEZ)} # If the repo is anything other than github we are loading the file using the add_EEZ.
    } # end if(!exists("eez.all"))
      # We then need to transform these coordinates to the coordinates of the eez data
    eez.bbox <- spTransform(b.box,proj4string(eez.all))
    # Then intersect the coordiates so we only plot the part of the eez we want
    eez <- gIntersection(eez.all,eez.bbox)
    # and now transform this eez subset to the proper coordinate system. If there is an eez in the picture....
    if(!is.null(eez)) eez <- spTransform(eez,c_sys)
  } # end if(!is.null(add_EEZ)) 

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

  
  # If you want to add the NAFO division we get this from the interweb so we know we have the right ones, adds about 20 seconds to the run to download.
  if(add_nafo == T && !exists("nafo.divs"))
  {
    # Figure out where your tempfiles are stored
    temp <- tempfile()
    # Download this to there
    download.file("https://www.nafo.int/Portals/0/GIS/Divisions.zip", temp)
    # Figure out what this file was saved as
    temp2 <- tempfile()
    # Unzip it
    unzip(zipfile=temp, exdir=temp2)
    # Now read in the shapefile
    nafo.divs <- readOGR(paste0(temp2, "/Divisions/Divisions.shp"))
    # Now transform the data to the coordinates we are using.
    nafo.divs <- spTransform(nafo.divs,c_sys)
  } # end if(add_nafo == T)
  
  
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
        download.file("https://raw.githubusercontent.com/Dave-Keith/GIS_layers/master/inshore/inshore.zip", temp)
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
        download.file("https://raw.githubusercontent.com/Dave-Keith/GIS_layers/master/offshore/offshore.zip", temp)
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
    if(repo != 'github')
    {
      if(add_sfas != "offshore" && !exists("inshore.spa"))
      {
        loc <- paste0(direct,"inshore")
        inshore.spa <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(inshore.spa)) inshore.spa[[i]] <- spTransform(inshore.spa[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
      if(add_sfas != "inshore" && !exists("offshore.spa"))
      {
        loc <- paste0(direct,"offshore")
        # This pulls in all the layers from the above location
        offshore.spa <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(offshore.spa)) offshore.spa[[i]] <- spTransform(offshore.spa[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
    } # end if(repo != 'github')
  } # end if(!is.null(add_sfas)) 
  
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
        download.file("https://raw.githubusercontent.com/Dave-Keith/GIS_layers/master/inshore_survey_strata/inshore_survey_strata.zip", temp)
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
        download.file("https://raw.githubusercontent.com/Dave-Keith/GIS_layers/master/offshore_survey_strata/offshore_survey_strata.zip", temp)
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
    if(repo != 'github')
    {
      if(add_strata != "offshore" && !exists("inshore.strata"))
      {
        loc <- paste0(direct,"inshore_survey_strata")
        inshore.strata <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(inshore.strata)) inshore.strata[[i]] <- spTransform(inshore.strata[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
      if(add_strata != "inshore" && !exists("offshore.strata"))
      {
        loc <- paste0(direct,"offshore_survey_strata")
        # This pulls in all the layers from the above location
        offshore.strata <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(offshore.strata)) offshore.strata[[i]] <- spTransform(offshore.strata[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
    } # end if(repo != 'github')
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
  par(mar=c(4,4,1,1))
  image.plot(list(x = projec$x, y=projec$y, z = field.projec), xlim = xlim, ylim = ylim, zlim=zlim, 
             axes=F,las=1,add=F, breaks=lvls, axis.args= arg.list,
             col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=alpha))
  } # end if(!is.null(field))
  #browser()
  # If we don't specify the field let's just make a blank plot of the correct type..
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
  if(add_nafo ==T) plot(nafo.divs,add=T,lty=2) # NAFO divisions
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
  #browser()
  # Now if these shapefiles aren't already in the global environment but exist within the function put them into the global environment for later use
  if(!exists("eez.all",where=1) && exists("eez.all"))                   assign('eez.all',eez.all,pos=1)
  if(!exists("nafo.divs",where=1) && exists("nafo.divs"))               assign('nafo.divs',nafo.divs,pos=1)
  if(!exists("land.all",where=1) && exists("land.all"))                 assign('land.all',land.all,pos=1)
  if(!exists("offshore.spa",where=1) && exists("offshore.spa"))         assign('offshore.spa',offshore.spa,pos=1)
  if(!exists("inshore.spa",where=1) && exists("inshore.spa"))           assign('inshore.spa',inshore.spa,pos=1)
  if(!exists("offshore.strata",where=1) && exists("offshore.strata"))   assign('offshore.strata',offshore.strata,pos=1)
  if(!exists("inshore.strata",where=1) && exists("inshore.strata"))     assign('inshore.strata',inshore.strata,pos=1)
} # end function