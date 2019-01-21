# This is a hack of a function the INLA lasses/lads use to plot their INLA model results
# Stand-alone function created by DK in Nov 2018

# Note that we need a couple of cute little functions for this to work.  Be sure to all call "addalpha"  and "all.layers 
# Before running this function, but I haven't settled on a directory for them quite yet...

# Arguements:

#1:   field     The values from the random field generated using INLA.  Default = NULL which just plots a nice map of the "area" chosen
#2:   mesh      The INLA mesh that corresponds to the values of field, which gets projected properly in this function .
#               Default = NULL which results in a plot of a nice map of the 'area' chosen
#3:   area      The area you want to plot, this can be a custom field (see function convert_coords.R for options) or a dataframe with
#               The coordinates and the projection of those coordindates specified.  Default provides Maritime Region boundaries
#               in lat/long coordinates and a WGS84 projection.
#5:   zlim      The range of values for the field plotted, default is  c(0,1) and this seems to be overwritten if you supply levels outside the 0-1 range.
#6:   dims      The number of X and Y values for the INLA surface.  Higher is better resolution, too high is silly. Default = c(50, 50) which is pretty low res
#7:   trans     Do you want to transform the spacing of the levels "none" (default), and "exp" (e.g. for a model with log link) is supported, but you can add more that!
#8:   clip      Do you want to clip the area plotted to some polygon?  Default = NULL which is no clipping
#9:   lvls      The number of levels you want to set up.  Default is seq(0,1,by=0.01) which is a lot of levels!
#10:  repo      The repository from which you will pull the data.  This is either Github (which has all the main data)
#11:  colors    What colours would you like to use for the colour ramp.  Default = c("blue","white","yellow","darkred"), will get slipt into more colours based on lvls
#12:  alpha     Do you want the colours to have some level of transparency.  0 = translucent, 1 = opaque, Default = 0.8
#13:  c_sys     What coordinate system are you using, options are "ll"  which is lat/lon and WGS84 or "utm_zone" which is utm, you put in the zone yourself
#               for example if you want utm_20 you can enter "+init=epsg:32620" which will use utm zone 20 which is best for BoF and SS 
#               for utm_19  use +init=epsg:32619, this is best for GB, SPA3 and 6, if you have are using something else go nuts!
#14:  add_EEZ   Do you want to add in the EEZ, default = NULL which does nothing.  If you want the EEZ this pulls it from either
#               our github respository (if github = "repo") or a location of your chosing (this would have to be the full directory)
#               add_EEZ = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/world_eez_boundary"
#15:  add_bathy Do you want to add in the bathymetry, default = NULL which does nothing.  If you want to add the bathy you simply need to supply the 
#               resolution that you want.  The smaller the resolution the slower this runs, it relies on data provided by NOAA, so if the server
#               is down this won't plot the bathymetry.  add_bathy = 10 plots the 10 meter bathymetry for the region selected by your xlim and ylim
#16: add_land   Do you want to add land to the figure, T/F, default = T which plots the the world in HiRes from the mapdata pacakge
#17: add_nafo   Do you want to add the NAFO divisions to the figure, T/F, default = T which grabs the NAFO divisions from online repository.
#18: add_sfas   Do you want to add the sfa boundariesto the figure.  NULL doesn't add anything.
#               If you are using the github repository the options are "inshore", "offshore", or "all"
#               If you are sourcing these locally you'll need to supply the directory for the shapefile
#19: add_strata Do you want to add the survey strata to the figure, NuLL = default.  NULL doesn't add anything.
#               If you are using the github repository the options are "inshore", "offshore", or "all"
#               If you are sourcing these locally you'll need to supply the directory for the shapefile
#10: add_obj    Do you have some spatial objects to add?  Default = NULL which doesn't add anything. 
#               While this was designed for seedboxes any sp class objects will work.  This can either be an SP object in R
#               Or will plot the seedboxes that were closed in the year you specify (e.g. add_obj = 2018 will pull in all seedboxes)
#               that were closed in 2018
#21: add_custom Do you have a specific object you'd like to add, this can be a csv or shapefile, you specify exactly where the
#               custom layer/PBS massing object is.
#               If using PBSmapping this assumes that you have the data formatted properly in a csv or xlsx file and 
#               that the proection for the data is WGS84.  Default = NULL
#22:  direct    If you aren't using the GitHub respoistory the directory that your shapefiles reside.  The default is the
#               location of the files on the ESS directory.  If repo = "github" this is ignored.  This is limited to look for shapefiles 
#               that are found in a similar repository structure to GitHub, this is just pointing to where those are.  If you want to add
#               shapefiles that are in a different folder structure/name either use the add_custom option or just add these
#               after running this function (you'd likely need to overplot the land after if you were doing that)

pecjector = function(field = NULL, mesh=NULL, area = data.frame(y = c(40,46),x = c(-68,-55),proj_sys = "+init=epsg:4326"),
                     zlim = c(0,1), dims = c(50, 50), trans= "none", repo = "github",
                     clip= NULL,lvls = seq(0,1,by=0.01),colors = c("blue","white","yellow","darkred"),alpha = 0.8,
                     c_sys = "ll", 
                     add_EEZ = NULL,add_bathy = NULL,add_land = F,add_nafo=F,add_sfas = NULL, 
                     add_strata = NULL, add_obj = NULL,add_custom = NULL,
                     direct = "Y:/Offshore scallop/Assessment/Data/Maps/approved/") 
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
  if(!exists("addalpha")) stop("You need to source the add_alpha_function.R for this to work, it should be in the same folder location as this function")
  if(!exists("convert.coords")) stop("You need to source the convert_coords.R for this to work, it should be in the same folder location as this function")
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
  #browser()
  
  # Get the spatial coordinates correct for the boxes, likely they are already in the Lat/Long WGS84 format, but might not be...
  if(!is.null(add_obj) && c_sys == "+init=epsg:4326") add_obj <- spTransform(add_obj,CRS(c_sys))

  if(c_sys != "+init=epsg:4326")
  {
    # We will want to clip the data to the area of interest so that the transformation works (especially if going for utm)
    b.box <- as(raster::extent(c(xlim,ylim)), "SpatialPolygons")
    proj4string(b.box) = c_sys
    # Note that this requires the boxes are already spatial polygons and have a coordinate reference system.
    # If we aren't in lat/lon both our boxes and our EEZ will need to be converted...
  } # end if(c_sys != "+init=epsg:4326")

  # If we are going to add the EEZ do this...
  if(!is.null(add_EEZ)) 
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
      eez <- readOGR(paste0(temp2, "/EEZ.shp"))
    } else{ # end if(repo = 'github')
      eez <- read.OGR(add_EEZ)} # If the repo is anything other than github we are loading the file using the add_EEZ.
    # We then need to transform these coordinates to the coordinates of the eez data
    eez.bbox <- spTransform(b.box,proj4string(eez))
    # Then intersect the coordiates so we only plot the part of the eez we want
    eez <- gIntersection(eez,eez.bbox)
    # and now transform this eez subset to the proper coordinate system. If there is an eez in the picture....
    if(!is.null(eez)) eez <- spTransform(eez,c_sys)
  } # end if(!is.null(add_EEZ)) 

  # For the moment the bathymetry can only be added to a figure if it is in lat-lon coordinates as we don't have a bathymetric shapefile
  if(!is.null(add_bathy) && c_sys == "+init=epsg:4326")
  {
    # The bathymetry needs to be in lat-lon, this is Lat/lon and WGS 84, I'm assuming that's what we want!
    proj4string(bbox) = c_sys
    # If c.sys isn't in lat/lon we need to tranform it to lat-lon
    # The bathymetry data is given in NOAA as Lat/Lon WGS84 according to NOAA's website.  https://www.ngdc.noaa.gov/mgg/global/
    bathy <- getNOAA.bathy(lon1 = b.box@bbox[1] ,lon2=b.box@bbox[3],lat1 = b.box@bbox[2],lat2=b.box@bbox[4],resolution = 10)
    # Now I need to convert this to a sp object, note that this data is lat/lon and WGS 84
    bathy.sp <- as.SpatialGridDataFrame(bathy)
    # Because we have the bathymetry on a grid rather than as a vector we can't easily reproject, the lazy solution is that we
    # won't plot bathymetry data unless c_sys = "ll", there is a solution but it is slow
  } # end if(!is.null(add_bathy))
  #browser()
  
  if(add_land == T)
  {
  # We also want to get the land
  land <- maps::map(database = "worldHires", c("USA","Canada"),fill=TRUE,col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(land$names, ":"), function(x) x[1])
  land.sp <- map2SpatialPolygons(land, IDs = IDs,proj4string =  CRS(c_sys))
  # This is a hack to clean up maps objects in R, if you ever see the error
  #"TopologyException: Input geom 1 is invalid: Self-intersection at or near point"
  # It is due to cleans up issues with the polygons coming from maps in this case, but any "bad" polygons get taken care of.
  land.sp <- gSimplify(land.sp,tol=0.00001)
  # This really should be done on projected data not Lat/Lon data, but this does the trick for our purposes.  If using this for something
  # more than simply trying to draw some land we might want to do something more complex.
  land.sp <- gBuffer(land.sp,byid=T,width=0)

  # Now if we need to transform the coordinates we will clip the land object so that we aren't trying to project like a sucker.
    if(c_sys != "+init=epsg:4326") 
    {
      # Now we get the land into 
      land.b.box <- spTransform(b.box,c_sys)
      # Now clip the land to this bounding box
      land.sp <- gIntersection(land.sp,land.b.box)
      # And now we can transform the land, if there is any to be plotted!
      if(!is.null(land.sp)) land.sp <- spTransform(land.sp,c_sys)
    } # end if(c_sys != +init=epsg:4326) 
  } # end if(plot.land == T)
  
  # If you want to add the NAFO division we get this from the interweb so we know we have the right ones, adds about 20 seconds to the run to download.
  if(add_nafo == T)
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
      if(add_sfas != "offshore")
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
    
      if(add_sfas != "inshore")
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
      if(add_sfas != "offshore")
      {
        loc <- paste0(direct,"inshore")
        inshore.spa <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(inshore.spa)) inshore.spa[[i]] <- spTransform(inshore.spa[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
      if(add_sfas != "inshore")
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
      if(add_strata != "offshore")
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
      
      if(add_strata != "inshore")
      {
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
      if(add_strata != "offshore")
      {
        loc <- paste0(direct,"inshore_survey_strata")
        inshore.strata <- all.layers(loc)
        # Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
        for(i in 1:length(inshore.strata)) inshore.strata[[i]] <- spTransform(inshore.strata[[i]],c_sys)
        # Now we don't have these nice shape files for SFA 29 sadly... I'll take these ones
      } # end if(detailed != "offshore")
      if(add_strata != "inshore")
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
  proj = inla.mesh.projector(mesh, xlim = xlim, ylim = ylim, dims=dims)
  field.proj = inla.mesh.project(proj, field)
  # If you want to clip the data to some coordinates/shape this is where that happens.
  if(!is.null(clip)) 
  {
    pred.in <- inout(proj$lattice$loc,clip) 
    field.proj[!pred.in] <- NA
  } # end if(!is.null(clip)) 

  #windows(11,11)
  # Transform the axis?
  if(trans== "exp") arg.list <- list(at=lvls,labels=round(exp(lvls)))
  if(trans == "none") arg.list <- list(at=lvls,labels=lvls)
  #browser()
  par(mar=c(4,4,1,1))
  image.plot(list(x = proj$x, y=proj$y, z = field.proj), xlim = xlim, ylim = ylim, zlim=zlim, 
             axes=F,las=1,add=F, breaks=lvls, axis.args= arg.list,
             col = addalpha(colorRampPalette(colors,interpolate = "spline",alpha=T)(length(lvls)-1),alpha=alpha))
  }
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
  if(!is.null(add_bathy) && c_sys == "+init=epsg:4326") plot(bathy.sp,add=T,col = "blue") # bathymetry, only works if plotting in lat/lon coordinates and WGS84...
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
  
} # end function
