## RUNME pecjector, these are examples of what you can do with pecjector


##### FOR NOAA SERVER ERROR, TRY UPDATING MARMAP PACKAGE AS FOLLOWS:
#install.packages("marmap")


# Pull in pecjector from Github
  funs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R")
  # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
} # end for(un in funs)

# Alternatively go find your local version....
source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/pectinid_projector_sf.R")
source("D:/Github/Assessment_fns/Maps/pectinid_projector_sf.R")
  
# Simple blank map of the region
# You can see the scallopy options inside the function 'convert_coords.R'
bp <- pecjector(area = "NL")

# Maybe you want to project the map, I'll used EPSG 32620 which is decent enough
bp.p1 <- pecjector(area="NL",c_sys = 32620)

# Maybe you want to plot some custom area, this is lat/lon and WGS84...
# Note the use of a list() for the area now, this is something we have built into the pecjector options in a few spots.
bp.p2 <- pecjector(area = list(y = c(35,55),x = c(-82,-36),crs = 4326))

# You could also project this if you like, 
bp.p3 <- pecjector(area = list(y = c(35,55),x = c(-82,-36),crs = 4326),c_sys = 32620)

# Now we can start to get a bit more fancy, much of the add ons here rely on the GIS github repo
# that we've been working on for the last 2 years.  But this is all done silently for you
# also not the use of the original bp object now serving as a basemap for the additional layers.
# It doesn't do much here, but showing the concept
bp.p4 <- pecjector(gg.obj = bp, area = "NL",add_layer = list(eez = 'eez'))
# Let's now add the bathymetry
bp.p5 <- pecjector(area = "NL", add_layer = list(bathy = 50))
# We could also just add the bathy contours
bp.p6 <- pecjector(area = "NL", add_layer = list(bathy = c(50,"c")))
# Or if we had the basemap with eez we could add the bathy to that too (note how the bathy overplots on the eez in this scenario as the basemap has 'old' info)
bp.p7 <- pecjector(gg.obj = bp.p4, area = "NL", add_layer = list(bathy = c(50,'c')))
# If I had used the smooth bathymetry surface the eez would be hidden underneath
bp.p8 <- pecjector(gg.obj = bp.p4, area = "NL", add_layer = list(bathy = 50))
# So instead we can just do it all in one go as well, here the code has been written so the bathy is put on first and everything goes on top of this.
# Also note that the order these are put in the list doesn't matter a lick. Notice I've added in the nafo areas as well
bp.p9 <- pecjector(area = "NL", add_layer = list(bathy = 50,eez = 'eez',nafo = 'main'))
# We can now start to add in interesting scallop features and also notice I've changed the nafo to plot the smaller subareas that we are often interested in
bp.p10 <- pecjector(area = "NL", add_layer = list(bathy = 50,eez = 'eez',nafo = 'sub',sfa = 'all'))
# Even more fancy is we can add in the survey strata for all areas (or just some of them....)
bp.p11 <- pecjector(area = "NL", add_layer = list(bathy = 50,eez = 'eez',nafo = 'sub',sfa = 'all',survey = c('all','outline')))
# And we can put some 'nice' labels on the areas too
bp.p12 <- pecjector(area = "NL", add_layer = list(bathy = 50,eez = 'eez',nafo = 'sub',s.labels = 'all'),gis.repo = 'D:/Github/GIS_layers')
# You can see this doesn't look great for the whole area as it is too busy, so lets zoom in on the WSS but keeping everythign we did above
# You notice how much quicker that was as all the layers were already loaded to the basemap 
# Looks like 13-15 aren't working as they used to, not zooming in on the subarea, if you want this functionality contact DK/FK...
bp.p13 <- pecjector(gg.obj = bp.p12, area = "WSS")
# we can also change things up to include more detailed survey strata information.  See how the legend is a bit busted here tho...
bp.p14 <- pecjector(gg.obj = bp.p12, area = "WSS", add_layer = list(survey = c("all","detailed")))
# Lets just zoom in on BBn, see how the strata remain kinda shit though...
bp.p15 <- pecjector(gg.obj = bp.p14, area = "BBn")
bp.p15

# So instead we'd probably really want to do this
bp.p16 <- pecjector(area = "BBn",add_layer = list(bathy = 50,eez = 'eez',nafo = 'sub',sfa = 'offshore',survey = c('offshore','detailed')))
# Moving to inshore we can do this too
bp.p17 <- pecjector(area = "Inshore",add_layer = list(bathy = 50,eez = 'eez',nafo = 'sub',sfa = 'inshore'))
# And quickly add in the survey strata and lets toss in a scale bar in bottom left across 50% of the plot
bp.p18 <- pecjector(gg.obj = bp.p17,area = "Inshore",add_layer = list(survey = c('inshore','detailed'),s.labels = 'inshore',scale.bar = c('bl',0.5)))
bp.p18

# Here's a new pretty basemap for offshore...
bp.off <- pecjector(area = "NL",add_layer = list(sfa = 'offshore', s.labels = 'offshore_detailed'))
windows(15,10); bp.off #Maximize image on your screen and the text should look good.
# Now in behind all this is we have some defaults you don't really need to worry about, this is the same thing
pecjector(gg.obj = bp.p17,area = "Inshore",add_layer = list(survey = c('inshore','detailed'),s.labels = 'inshore',scale.bar = c('bl',0.5)),
          repo = 'D:/Github/Assessment_fns', gis.repo = 'github',plot = T,buffer =0, c_sys = 4326)

# If you have the github repos locally this would work...
pecjector(gg.obj = bp.p17,area = "Inshore",add_layer = list(survey = c('inshore','outline'),s.labels = 'inshore',scale.bar = c('bl',0.5)),
          repo = 'D:/Github/Assessment_fns', gis.repo = 'D:/Github/GIS_layers',plot = T,buffer =0, c_sys = 4326,plot_as = "ggplot")

# Now I can also blow your mind...
bp.int1 <- pecjector(area = "NL", add_layer = list(bathy = c(50,'c'),eez = 'eez',nafo = 'sub',sfa = 'all',survey = c('all','detailed'),s.labels = 'all'),
                     plot_as = 'ggplotly')
# Roughly the same, though subtly different is...
bp.int2 <- pecjector(area = "NL",add_layer = list(bathy = c(50,'c'),eez = 'eez',nafo = 'sub',sfa = 'all',s.labels = 'all'),
                     plot_as = 'plotly')

# OK, so s.labels = 'all' needs fixed

bp.hmm <- pecjector(area = list(y = c(35,55),x = c(-82,-36),crs = 4326),c_sys = 4326,
                   add_layer = list(bathy = 50,eez = 'eez',nafo = 'sub',s.labels = 'all'))

