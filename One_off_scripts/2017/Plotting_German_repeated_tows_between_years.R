## Here's a cute little scipt to plot/compare tow tracks between different years, this spcific example runs some German tow tracks...


# TOW TRACK DATA GERMAN BANK
# Need to input the number of tows, file location, weighting scheme, seconds between readings
#Source10 source("fn/getdis.r") 
direct="d:/r/"
direct <- "Y:/Offshore scallop/Assessment/"
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_and_OSAC/getdis.r",sep="")) 

yr <- 2017
load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_spring_results.Rdata",sep="")) 
repeats <- new.ger.tows[new.ger.tows$stratum ==2,]

This_years_tow <-dist.coef(repeats$tow,path="Y:/Alan/LE05/logfiles/German/", w=c(1:10,9:1),rule=8,smooth=T,plt=F,direct=direct)
                                           
Last_years_tow <-dist.coef(repeats$EID,path="Y:/Alan/LE03/logfiles/German/", w=c(1:10,9:1),rule=8,smooth=T,plt=F,direct=direct)
                           


windows(11,11)
ScallopMap(ylim=c(43.4,43.6),xlim=c(-66.7,-66.5))
addLines(This_years_tow[[2]],col="blue")
addLines(Last_years_tow[[2]],col="darkgreen")
text(repeats$lon,repeats$lat,labels =repeats$tow,cex=0.6)

windows(11,11)
ScallopMap(ylim=c(43.1,43.4),xlim=c(-66.5,-66.3))
addLines(This_years_tow[[2]],col="blue")
addLines(Last_years_tow[[2]],col="darkgreen")
text(repeats$lon,repeats$lat,labels =repeats$tow,cex=0.6)

windows(11,11)
ScallopMap(ylim=c(43.05,43.2),xlim=c(-66.3,-66.05))
addLines(This_years_tow[[2]],col="blue")
addLines(Last_years_tow[[2]],col="darkgreen")
text(repeats$lon,repeats$lat,labels =repeats$tow,cex=0.6)

windows(11,11)
ScallopMap(ylim=c(43.0,43.2),xlim=c(-65.95,-65.78))
addLines(This_years_tow[[2]],col="blue")
addLines(Last_years_tow[[2]],col="darkgreen")
text(repeats$lon,repeats$lat,labels =repeats$tow,cex=0.6)

