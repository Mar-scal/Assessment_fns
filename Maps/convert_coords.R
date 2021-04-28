# Here is a little function you can use to convert between coordinate systems it should take whatever coordinate
# system you like and convert between them.  There are also a number of "custom" y/x coordinate boxes 
# that can be used to generate the boundaries for plot boxes (which was the original intent of this function.
# DK created December 2018

# Arguements
#1  plot.extent:  The coordinates you want in a dataframe with y and x coordinates specfied, or you can use one of the names below 
#2  in.csys:     The cooridanate system of the data you entered in th plot.extent.  Default is Lat/Lon with WGS84
#3  out.csys:    The cooridanate system you want to convert your y/x coordinates to. Default is "WGS84"
#4  bbox.buf:    Do you want to add a buffer to the bounding box, set as a percentage of the distance between your longitudes.  Setting this to 0.05 would set a buffer of
#                 5% of this distance.  Default is 0 which no buffer.
#  make.sf:       Do you want the output to be an sf object, default = F to ensure backwards compatibility to other

convert.coords <- function(plot.extent= list(y = c(40,46),x = c(-68,-55)),in.csys = 4326, out.csys= 4326,bbox.buf = 0,make.sf=F)
{
  # You'll need the sp library for this to work
  require(sp) || stop("You need sp, thanks!")
  require(sf) || stop("Get with it cuz, you need sf package")
  require(tmaptools) ||  stop("Yo cuz, you need tmaptools package")
  
  # Custom plot.extent used if you want to enter your own y and x 
  if(is.list(plot.extent))	{ y=plot.extent$y; 			x=plot.extent$x}
  
  ## These are the predfined lists used to define y and x for the plot created below
  if(!is.list(plot.extent))
  {
    #offshore
    # This includes southern Newfoundland, this is a nice map of everywhere 
    if(plot.extent %in% c('NL','nl',"Newfoundland","NEWFOUNDLAND","newfoundland",'nf',"NF","NFLD")){y=c(40.00,48.00);x=c(-68.00,-54.00)} 
    # This is the Martime offshore, includes most of the inshore too just due to nature of area
    if(plot.extent%in% c('offshore',"OFFSHORE","Offshore"))                                        {y=c(40.00,46.00);x=c(-68.00,-55.00)}
    if(plot.extent%in% c('SS','ss','Scotian Shelf','scotian shelf',"SCOTIAN SHELF"))		           {y=c(40.50,47.00);x=c(-68.00,-57.00)}
    if(plot.extent%in% c('WOB','wob',"WOb",'Western Offshore Banks'))		                           {y=c(40.50,44.00);x=c(-68.00,-64.00)} 
    if(plot.extent%in% c('ESS','ess','Ess',"Eastern SS","EASTERN SS",'eastern ss'))                {y=c(43.00,45.40);x=c(-62.50,-57.40)}
    if(plot.extent%in% c('WSS','wss','Wss',"Western SS","WESTERN SS",'western ss'))		             {y=c(41.00,44.00);x=c(-67.00,-64.00)}
    if(plot.extent%in% c('BBn','bbn','BBN','Browns N','browns n','BROWNS N',
                         'Browns n','26A',"SFA26A","26N","SFA26N"))                                {y=c(42.40,43.00);x=c(-66.60,-65.60)}
    if(plot.extent%in% c('BBs','bbs','BBS','Browns S','browns s','BROWNS S',
                         'Browns s','26B',"SFA26B","26S","SFA26S"))                                {y=c(42.25,42.75);x=c(-66.00,-65.25)}
    if(plot.extent%in% c('BB','bb','Bb',"Browns","browns",'BROWNS'))                               {y=c(42.25,43.00);x=c(-66.50,-65.25)}
    if(plot.extent%in% c("GB","gb","Georges","georges","GEORGES",
                         "27B","27","SFA27B","SFA27"))		                                         {y=c(41.10,42.30);x=c(-67.30,-65.60)}
    if(plot.extent%in% c('GBa',"GBa","gba","Georges A","georges a",
                         "GEORGES A","Georges a","27A","SFA27A"))                                  {y=c(41.20,42.30);x=c(-67.15,-65.85)}
    if(plot.extent%in% c('GBb',"GBB","gbb","Georges B","georges b","GEORGES B","Georges b"))		   {y=c(41.60,42.30);x=c(-66.70,-65.60)}
    if(plot.extent%in% c('Ger',"GER","ger","German","GERMAN","german","SFA26C","26C"))             {y=c(42.80,43.80);x=c(-67.00,-65.60)}
    if(plot.extent%in% c('Sab',"SABLE","sab","sable","SAB","Sable"))		                           {y=c(43.00,44.35);x=c(-62.50,-60.50)}
    if(plot.extent%in% c('SPB','spb','Spb',"St Pierre", "Saint Pierre"))		                       {y=c(44.50,47.50);x=c(-58.00,-55.00)}
    if(plot.extent%in% c('SPB-banks', 'spb-banks',"SPB-BANKS","SPB BANKS", "spb banks",
                         "SFA10","SFA11","SFA12","10","11","12"))                                   {y=c(45.25,46.25);x=c(-57.25,-55.50)}
    # These are from offshore ScallopMap.r and I believe we'll need them
    if(plot.extent%in% c('West',"WEST","west","Western","WESTERN","western"))                      {y=c(43.00,44.10);x=c(-62.20,-60.40)}
    # We need to be slight careful that we don't get mixed up between middle bank and mid bay
    if(plot.extent%in% c('Mid',"mid","MID","Middle","middle","MIDDLE"))		                         {y=c(44.20,44.90);x=c(-61.30,-60.10)}
    if(plot.extent%in% c('Ban-Nar',"BAN-Nar","ban-nar"))                                           {y=c(43.90,44.80);x=c(-60.25,-58.50)}
    if(plot.extent%in% c('Sab-West','SAB-WEST',"SAB WEST","sab west","sab-west"))                  {y=c(42.80,44.50);x=c(-62.50,-58.80)}
    if(plot.extent%in% c('Ban-Wide','BAN-WIDE','BAN WIDE',"ban wide","ban-wide","Ban",
                         "Banquereau","BANQUEREAU",'banquereau',"BAN",'ban'))                      {y=c(43.70,45.20);x=c(-60.50,-57.00)}
    if(plot.extent%in% c('GOM','gom','Gulf of Maine',"gulf of maine","GULF OF MAINE"))             {y=c(40.00,45.00);x=c(-70.60,-65.80)}
    
    
    #inshore
    if(plot.extent %in% c("SFA29",'sfa29','29W','29w','SFA29W','sfa29w','29'))	                   {y=c(43.10,43.80);x=c(-66.50,-65.45)}
    if(plot.extent %in% c('gm',"GM","Grand Mannan"))		                                           {y=c(44.40,45.20);x=c(-67.20,-66.30)}
    if(plot.extent %in% c('inshore',"Inshore","INSHORE"))	                                         {y=c(43.10,45.80);x=c(-67.50,-64.30)}
    if(plot.extent%in% c('bof','BOF',"BoF","Bay","Bay of FUndy"))		                               {y=c(44.25,45.80);x=c(-66.50,-64.30)}
    if(plot.extent%in% c('upper',"UPPER","UB","Upper","Upper Bay","upper bay"))	                   {y=c(45.00,46.00);x=c(-65.20,-64.30)}
    # Need to be slightly careful here since we have middle bank 
    if(plot.extent%in% c('Mid Bay',"MB","mb",'mid bay'))		                                       {y=c(44.30,45.50);x=c(-66.60,-64.70)} 
    if(plot.extent%in% c('spa3',"SPA3","3"))	                                                     {y=c(43.62,44.60);x=c(-66.82,-65.80)}
    if(plot.extent%in% c('spa4',"SPA4","4"))	                                                     {y=c(44.48,44.96);x=c(-66.20,-65.51)}
    if(plot.extent%in% c('spa1',"SPA1","1"))	                                                     {y=c(44.50,45.80);x=c(-66.40,-64.30)}
    if(plot.extent%in% c('spa6',"SPA6","6"))	                                                     {y=c(44.30,45.25);x=c(-67.40,-65.90)}
    if(plot.extent%in% c('spa1A',"SPA1A",'spa1a','SPA1a','spa1A',"1A","1a"))                       {y=c(44.37,45.30);x=c(-66.40,-64.80)}
    if(plot.extent%in% c('spa1B',"SPA1B",'spa1b','SPA1b','spa1B',"1B","1b"))	                     {y=c(44.80,45.70);x=c(-66.20,-64.30)}
    if(plot.extent%in% c('spa5','SPA5',"5"))	                                                     {y=c(44.56,44.78);x=c(-65.82,-65.51)}
  } # end if(!is.data.frame(plot.extent))
  # Now transform the data to the coordinate system you are using.
  coords <- st_as_sf(data.frame(y=y,x=x),coords = c("x","y"),crs = in.csys)
  # and now transform it to the projection you want
  coords <- st_transform(coords,out.csys)
  
  # Next we build a bounding box polygon
  b.box <- st_bbox(coords)
  b.box <- bb_poly(b.box) # this inherents the coordinate system of coords, so all good
  
  # If we want to build in a buffer 
  if(bbox.buf > 0)
  {
    #st_buffer can't be in Lat/Lon so we convert out b.box to the best UTM system in the region, it's just a bounding box so this isn't a biggy!
    box.utm <- st_transform(b.box,crs = 32620) # Need this to get buffer, using UTM 20, which is good enough for setting a reasonable buffer in the NW Atlantic
    dis <- st_area(box.utm)^.5 * bbox.buf # Calculate the area of the bouding box, assuming it is roughly square we take square root and that is a reasonable bounding distance.
    box.buf <- st_buffer(box.utm,dist=dis) # Put a buffer around the area
    b.box <- st_transform(box.buf,crs = out.csys) # And transform back to the system we want
  } # end if(bbox.buf > 0)
  return(list(coords = coords, b.box = b.box))
  
}