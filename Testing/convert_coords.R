# Here is a little function you can use to convert between coordinate systems it should take whatever coordinate
# system you like and convert between them.  There are also a number of "custom" y/x coordinate boxes 
# that can be used to generate the boundaries for plot boxes (which was the original intent of this function.
# DK created December 2018

# Arguements
#1  plot.extent:  The coordinates you want in a dataframe with y and x coordinates specfied, or you can use one of the names below 
#2  c_sys:        The coordinate system you want to convert your y/x coordinates to. Default is "WGS84"
#3  initial.proj: The projection of the data you entered in th plot.extent.  Default is Lat/Lon with WGS84

convert.coords <- function(plot.extent= data.frame(y = c(40,46),x = c(-68,-55)),c_sys = "+init=epsg:4326", initial.proj= "+init=epsg:4326")
{
# You'll need the sp library for this to work
require(sp) || stop("You need sp, thanks!")
  
# Custom plot.extent used if you want to enter your own y and x 
if(is.data.frame(plot.extent))	{ y=plot.extent$y; 			x=plot.extent$x}

## These are the predfined lists used to define y and x for the plot created below
if(!is.data.frame(plot.extent))
{
#offshore
if(plot.extent =='NL')      { y=c(40,48);         x=c(-68,-54) } # DK added August 7, 2015, option to include Southern NewFoundland
if(plot.extent=='offshore') { y=c(40,46);         x=c(-68,-55)}
if(plot.extent=='SS')		   { y=c(40.5,47); 		  x=c(-68,-57)		}
if(plot.extent=='WOB')		   { y=c(40.5,44); 		  x=c(-68,-64)		} # Western offshore Banks, DK added
if(plot.extent=='ESS')		   { y=c(43,45.4); 	    x=c(-62.5,-57.4)	}
if(plot.extent=='WSS')		   { y=c(41,44); 		    x=c(-67,-64)		}
if(plot.extent=='BBn')		   { y=c(42.4,43); 		  x=c(-66.6,-65.6)	}
if(plot.extent=='BBs')		   { y=c(42.25,42.75); 	x=c(-66,-65.25)	}
if(plot.extent=='BB')		   { y=c(42.25,43);      x=c(-66.5,-65.25)	}
if(plot.extent=='GB')		   { y=c(41.1,42.3); 	  x=c(-67.3,-65.6)	}
if(plot.extent=='GBa')      {y=c(41.2,42.3);      x=c(-67.15,-65.85)}
if(plot.extent=='GBb')		   { y=c(41.6,42.3); 	  x=c(-66.7,-65.6)	}
if(plot.extent=='Ger')		   { y=c(42.8,43.8); 	  x=c(-67,-65.6)		}
if(plot.extent=='Sab')		   { y=c(43,44.35); 	    x=c(-62.5,-60.5)	}
if(plot.extent=='SPB')		   { y=c(44.5,47.5);	    x=c(-58,-55)		}
if(plot.extent=='SPB-banks'){ y=c(45.25,46.25);	  x=c(-57.25,-55.5)		}
# These are from offshore ScallopMap.r and I believe we'll need them
if(plot.extent=='West')	   {  y=c(43,44.1); 		x=c(-62.2,-60.4)	}
if(plot.extent=='Mid')		   { y=c(44.2,44.9);	x=c(-61.3,-60.1) }
if(plot.extent=='Ban')		   { y=c(43.9,44.8); 	x=c(-60.25,-58.5)	}
if(plot.extent=='Sab-West') { y=c(42.8,44.5); 		x=c(-62.5,-58.8)	}
if(plot.extent=='Ban-Wide') { y=c(43.7,45.2); 	x=c(-60.5,-57)	}


#inshore
if(plot.extent=='sfa29')	{ y=c(43.1,43.8);	x=c(-66.5,-65.45) }
if(plot.extent=='gm')		{ y=c(44.4,45.2);	x=c(-67.2,-66.3) }
if(plot.extent=='inshore')	{ y=c(43.1,45.8);	x=c(-67.5,-64.3) }
if(plot.extent=='bof')		{ y=c(44.25,45.8);	x=c(-66.5,-64.3) }
if(plot.extent=='upper')	{ y=c(45,46);		x=c(-65.2,-64.3) }
if(plot.extent=='mid')		{ y=c(44.3,45.5);	x=c(-66.6,-64.7) }
if(plot.extent=='spa3')	{ y=c(43.62,44.6);	x=c(-66.82,-65.8) }
if(plot.extent=='spa4')	{ y=c(44.48,44.96);	x=c(-66.2,-65.51) }
if(plot.extent=='spa1')	{ y=c(44.5,45.8);	x=c(-66.4,-64.3) }
if(plot.extent=='spa6')	{ y=c(44.3,45.25);	x=c(-67.4,-65.9) }
if(plot.extent=='spa1A')	{ y=c(44.5,45.3);	x=c(-66.4,-64.8) }
if(plot.extent=='spa1B')	{ y=c(44.8,45.7);	x=c(-66.2,-64.3) }
if(plot.extent=='spa5')	{ y=c(44.56,44.78);	x=c(-65.82,-65.51) }
} # end if(!is.data.frame(plot.extent))
  
# Now transform the data to the coordinate system you are using.
coords <- data.frame(y=y,x=x)

# Now make this into a Spatial Points object
coordinates(coords) <- ~ x+y
# Give it the projection you started with
proj4string(coords) <- CRS(initial.proj)
# ANd now transform it to the projection you want
coords <- spTransform(coords,c_sys)

return(coords)
    
}