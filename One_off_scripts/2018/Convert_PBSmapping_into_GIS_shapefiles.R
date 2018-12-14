# this is a little script to convert the offshore PBSmapping format data into shapefiles...

require(splancs) || stop("You need le package splancs, thanks!")
require(PBSmapping) || stop("You need PBSmapping, thanks!")
require(sp) || stop("You need sp, thanks!")
require(rgdal)  || stop("You need rgdal, thanks!")
require(rgeos) || stop("You need rgeos, thanks!")
require(raster) || stop("You need raster, thanks!")
require(maptools) || stop("You need maptools, thanks!")
require(maps) || stop("You need maps, thanks!")
require(mapdata)|| stop("You need mapdata, thanks!")