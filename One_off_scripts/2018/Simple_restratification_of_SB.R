direct <- "d:/r/"
library(PBSmapping)
#Read4 Get the survey boundary polygons for all banks, I want to use the orginal ones here 
survey.bound.polys<-read.csv(paste(direct,"data/Maps/archive/2018/survey_boundary_polygons.csv",sep=""),
                             header=T,stringsAsFactors = F)
#Read5 Get the detailed survey polygons for all banks
# Need to use the original strata for this, it is located here...
survey.detail.polys <- read.csv(paste(direct,"data/Maps/archive/2018/survey_detail_polygons.csv",sep=""),
                                header=T,stringsAsFactors = F)

#Read6 Get the survey information for each bank
survey.info <- read.csv(paste(direct,"data/Survey_data/survey_information.csv",sep=""),
                        header=T,stringsAsFactors = F)

newAreaPolys<-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep="")
                       ,stringsAsFactors = F,header=T)
unique(newAreaPolys$label)

source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep="")) 
source(paste(direct,"Assessment_fns/Survey_design/survey_design.r",sep="")) 


webca <- newAreaPolys[newAreaPolys$label == "WEBCA",]
webca <- as.PolySet(webca,projection = "LL")
sfz <- newAreaPolys[newAreaPolys$label == "SFZ",]
sfz <- as.PolySet(sfz,projection = "LL")
big.sfz <- data.frame(PID = rep(1,7),POS = seq(1:7),
                      X = c(sfz$X[8],sfz$X[7],webca$X[4],-60.5,-60.5,sfz$X[8],sfz$X[8]),
                      Y=c(sfz$Y[8],sfz$Y[7],webca$Y[4],webca$Y[4],44.2,44.2,sfz$Y[8]),
                      label=rep("Sab",7))

sab.bound <- survey.bound.polys[survey.bound.polys$label=="Sab",]
sab.bound <- as.PolySet(sab.bound,projection ="LL")
sab <- survey.detail.polys[survey.detail.polys$label=="Sab",]
sab <- as.PolySet(sab,projection = "LL")

sab.bound.new <- joinPolys(sab.bound,big.sfz,"INT")
# Export this as the new sable boundaries
write.csv(sab.bound.new,paste0(direct,"2018/Misc/Sable_re_stratification/Sable_new_boundary.csv"))
# The detailed survey strata split is this....
sab.new <- joinPolys(sab,big.sfz,"INT")
sab.new$Strata <- sab.new$PID+500
sab.new <- as.data.frame(sab.new)
# Export this as our new strata...
write.csv(sab.new,paste0(direct,"/2018/Misc/Sable_re_stratification/Sable_new_strata.csv"))

# Here is the new size for the Sable area.
org <- calcArea(sab,rollup=1)
areas <- calcArea(sab.new,rollup =1)

sab.info <- survey.info[survey.info$label=="Sab",]
#sab.info$col[c(1:2,4:5)] <- NA
# Plot of the overlap
windows(11,11)
ScallopMap("Sab",direct=direct,poly.lst=list(sab.new,sab.info))
addPolys(sab.new[sab.new$PID==4,],col=sab.info$col[sab.info$PID==4],border=NA)
addPolys(sab.new[sab.new$PID==5,],col=sab.info$col[sab.info$PID==5],border=NA)

addPolys(big.sfz)
addPolys(sab.bound.new)

### Now that this is all updated we can run the survey design on sable...

write.csv(survey.info,paste(direct,"data/Survey_data/survey_information.csv",sep=""))

# Now the plot isn't perfect yet as the join has done something weird with strata 3 (it has removed the holes from this strata)
# and I haven't figured out how to fill them yet...
Survey.design(yr = 2018,direct = direct,banks =c("Sab"),export=T,relief.plots = F,fig="screen",seed=20)

surv.dat <- read.csv(paste0(direct,"Data/Survey_data/2018/Spring/Sab/Preliminary_Survey_design_Tow_locations.csv"))

for(i in 1:5)
{
  surv.dat$col[surv.dat$Poly.ID==sab.info$PID[i]] <- sab.info$col[sab.info$PID[i]]
}
windows(11,11)
ScallopMap("Sab",direct=direct,poly.lst=list(sab.new,sab.info))
addPolys(sab.new[sab.new$PID==4,],col=sab.info$col[sab.info$PID==4],border=NA)
addPolys(sab.new[sab.new$PID==5,],col=sab.info$col[sab.info$PID==5],border=NA)
addPolys(sfz)
addPolys(webca)
points(surv.dat$X,surv.dat$Y,bg=surv.dat$col,pch=21)
text(-62,44.2,"Note: The random seed was set to 20")
title("Survey (Sab-2018)",cex.main=2)
