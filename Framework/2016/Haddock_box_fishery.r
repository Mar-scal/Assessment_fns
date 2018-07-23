## Here I just take a quick look at the fishing effort in a couple of different years on the proposed Haddock box


direct <- "D:/r/"
library(PBSmapping)
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))#source the ScallopMap script (function containing maps and parameters)
source(paste(direct,"Assessment_fns/Survey_and_OSAC/OSAC_fishery_figures.r",sep="")) #Source1
source(paste(direct,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Assessment_fns/Fishery/fishery.dat.r",sep=""))

# First the Haddock box outline (at least the Eastern half of it that Ginette outlines)
# A few of these numbers are guesses back on the document Ginette made up (HistoryscallopCATHoddockbox-Par2.docx)
Lats <- c(c(44 + 2/60),c(44 + 2/60),c(43+20/60),c(43+4/60),c(43+4/60),c(43+40/60),c(44+2/60))
Lons <- c(c(-61 - 42/60),c(-61 - 15/60),c(-61 - 15/60),c(-62),c(-62-10/60),c(-62-10/60),c(-61-42/60))
PID <- rep(1,7)
POS <- seq(1,length(PID))

Haddock.box <- as.PolySet(data.frame(PID = PID, POS=POS, X = Lons,Y=Lats),projection="LL")

# Now bring in the fishery data

logs_and_fish(loc="offshore",year = years,un=un,pw=pwd,db.con=db.con,direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

#fish.dat <- subset(fish.dat, date < "1999-01-01")

bank.spatial <- fishery_figures(fish.dat=fish.dat,bnk="Sab",dirct=direct,poly.brd="black",years = 2016,
                                save.fig=F,add.titles = T)

# Set the date for the most recent data.
#yr <- format(max.date, "%Y")
# We create the spatial exploitation map if we have the most recent years data
bnk.fish.dat <- subset(fish.dat, bank== bnk[i] & date >= as.Date(paste(yr,"-01-01",sep="")),
                       select = c('ID','lon','lat','pro.repwt'))
names(bnk.fish.dat)[1:4]<-c("EID","X","Y","Z")
addPolys(Haddock.box)
