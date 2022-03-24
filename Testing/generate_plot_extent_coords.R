# Here is a little function you can use to get coordinates for a plot, this has a bunch of default coordinates in it
# 



plot.loc <- function(plot.extent){
  
  # Custom area used if you want to create your own ylim and xlim for the plot created below
  if(length(area) > 1)	{ylim=ylim; xlim=xlim}
  
  ## These are the predfined lists used to define ylim and xlim for the plot created below
  
  #offshore
  if(area =='NL')      { ylim=c(40,48);         xlim=c(-68,-54) } # DK added August 7, 2015, option to include Southern NewFoundland
  if(area=='offshore') { ylim=c(40,46);         xlim=c(-68,-55)}
  if(area=='SS')		   { ylim=c(40.5,47); 		  xlim=c(-68,-57)		}
  if(area=='WOB')		   { ylim=c(40.5,44); 		  xlim=c(-68,-64)		} # Western offshore Banks, DK added
  if(area=='ESS')		   { ylim=c(43,45.4); 	    xlim=c(-62.5,-57.4)	}
  if(area=='WSS')		   { ylim=c(41,44); 		    xlim=c(-67,-64)		}
  if(area=='BBn')		   { ylim=c(42.4,43); 		  xlim=c(-66.6,-65.6)	}
  if(area=='BBs')		   { ylim=c(42.25,42.75); 	xlim=c(-66,-65.25)	}
  if(area=='BB')		   { ylim=c(42.25,43);      xlim=c(-66.5,-65.25)	}
  if(area=='GB')		   { ylim=c(41.1,42.3); 	  xlim=c(-67.3,-65.6)	}
  if(area=='GBa')      {ylim=c(41.2,42.3);      xlim=c(-67.15,-65.85)}
  if(area=='GBb')		   { ylim=c(41.6,42.3); 	  xlim=c(-66.7,-65.6)	}
  if(area=='Ger')		   { ylim=c(42.8,43.8); 	  xlim=c(-67,-65.6)		}
  if(area=='Sab')		   { ylim=c(43,44.35); 	    xlim=c(-62.5,-60.5)	}
  if(area=='SPB')		   { ylim=c(44.5,47.5);	    xlim=c(-58,-55)		}
  if(area=='SPB-banks'){ ylim=c(45.25,46.25);	  xlim=c(-57.25,-55.5)		}
  # These are from offshore ScallopMap.r and I believe we'll need them
  if(area=='West')	   {  ylim=c(43,44.1); 		xlim=c(-62.2,-60.4)	}
  if(area=='Mid')		   { ylim=c(44.2,44.9);	xlim=c(-61.3,-60.1) }
  if(area=='Ban')		   { ylim=c(43.9,44.8); 	xlim=c(-60.25,-58.5)	}
  if(area=='Sab-West') { ylim=c(42.8,44.5); 		xlim=c(-62.5,-58.8)	}
  if(area=='Ban-Wide') { ylim=c(43.7,45.2); 	xlim=c(-60.5,-57)	}
  
  
  #inshore
  if(area=='sfa29')	{ ylim=c(43.1,43.8);	xlim=c(-66.5,-65.45) }
  if(area=='gm')		{ ylim=c(44.4,45.2);	xlim=c(-67.2,-66.3) }
  if(area=='inshore')	{ ylim=c(43.1,45.8);	xlim=c(-67.5,-64.3) }
  if(area=='bof')		{ ylim=c(44.25,45.8);	xlim=c(-66.5,-64.3) }
  if(area=='upper')	{ ylim=c(45,46);		xlim=c(-65.2,-64.3) }
  if(area=='mid')		{ ylim=c(44.3,45.5);	xlim=c(-66.6,-64.7) }
  if(area=='spa3')	{ ylim=c(43.62,44.6);	xlim=c(-66.82,-65.8) }
  if(area=='spa4')	{ ylim=c(44.48,44.96);	xlim=c(-66.2,-65.51) }
  if(area=='spa1')	{ ylim=c(44.5,45.8);	xlim=c(-66.4,-64.3) }
  if(area=='spa6')	{ ylim=c(44.3,45.25);	xlim=c(-67.4,-65.9) }
  if(area=='spa1A')	{ ylim=c(44.5,45.3);	xlim=c(-66.4,-64.8) }
  if(area=='spa1B')	{ ylim=c(44.8,45.7);	xlim=c(-66.2,-64.3) }
  if(area=='spa5')	{ ylim=c(44.56,44.78);	xlim=c(-65.82,-65.51) }
}