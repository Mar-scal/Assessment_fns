source("D:/R/Assessment_fns/Survey_and_OSAC/Plotting_tow_tracks.r")

# For the spring run this, note that this doesn't run BBs.
res <- tow.track.plots(bk = 'spring', extras = T, export=T,fig="png",compare=T,labels=T,year=2018)

# For the summer this should do the trick
res <- tow.track.plots(bk = "summer", extras = T, export=T,fig="png",compare=T,labels=T,year=2018)

