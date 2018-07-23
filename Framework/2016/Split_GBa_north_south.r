################################################################################################################
##### This is the script Grabs Georges Bank A and splits the data into north and south components
####  IMPORTANT NOTE:  The re-stratification is VERY rough for the low strata (which is common between north and south!)
####  DK Jan 20, 2016
################################################################################################################
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  None, this is a top level file whose output is either csv files or figures.
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files"), 
# see "Source_relation_SurveySummary.docx" for complete details
#   1:  import.survey.dat.r
#   2:  springsurveydata.r (2011)
#   6:  get.offshore.survey.r 
#   7:  import.hyd.data.r
#   8:  import.fishery.data.r
#   9:  ScallopMap.r
#   10: getdis.r
#   11: shwt.lme.r
#   13: confac.r
#   14: surv.by.tow.r
#   15: simple.surv.r
#   16: stds.plt.r
#   17: survey.ts.r
#   18: shf.plt.r
#   19: contour.gen.r
#   20: restratwp.r
#   21: survey.dat.r
#   22: shwt.plt.r
#   23: sprSurv.r
#   25: clap3.plt.r
#   29: gridPlot.r
##
###############################################################################################################


##############################################################################################################
################################### SECTION 1 SECTION 1 SECTION 1 ############################################
#################################### LOAD PACKAGES, LOAD FLAT FILES (where possible) #########################
################################### PRE-PROCESS DATA (where possible)               #########################
################################### SECTION 1 SECTION 1 SECTION 1 ############################################
##############################################################################################################
#


################################### Start Load Packages and Functions #########################################
# Step 1:  DK August 17, 2015.  We could make it so that we load all required packages and functions immediately
#
##########################################################################################################
# clean up the workspace
rm(list=ls())
# re-load rprofile if you have one...
source(".Rprofile")
# First load required packages
require(PBSmapping)
require(RColorBrewer)
############################# GENERAL DATA ########################################################
############################# GENERAL DATA ########################################################
# Enter here standard data which is used throughout this script.
# The length of the loop to run
atow<-800*2.4384/10^6 # area of standard tow in km2
direct <- "d:/r/"
yr = 2015
############################# END GENERAL DATA ########################################################

############################# LOAD data ########################################################
# Just to make sure we have everything correct in here...
    
    dirc <- "d:/r/"
    load(paste(direct,"Data/Survey_data/",yr,"/Survey_summary_output/Survey_all_results.Rdata",sep=""))  
    direct <- dirc
    # These are the functions used to within the heart of the code to make stuff happen
    source(paste(direct,"Assessment_fns/getdis.r",sep="")) #SOurce10
    source(paste(direct,"Assessment_fns/shwt.lme.r",sep="")) #Source11
    source(paste(direct,"Assessment_fns/condFac.r",sep="")) #Source13
    source(paste(direct,"Assessment_fns/surv.by.tow.r",sep="")) #Source14 Revised by DK September 2015
    source(paste(direct,"Assessment_fns/simple.surv.r",sep="")) #Source15 Revised by DK September 2015
    source(paste(direct,"Assessment_fns/stdts.plt.R",sep="")) #Source16
    source(paste(direct,"Assessment_fns/survey.ts.r",sep=""),local=T) #Source17 Revised by DK September 2015
    source(paste(direct,"Assessment_fns/shf.plt.r",sep="")) #Source18
    source(paste(direct,"Assessment_fns/Contour/contour.gen.r",sep="")) #Source19 Revised by DK September 2015
    source(paste(direct,"Assessment_fns/restratwp.r",sep=""),local=T) #Source20
    source(paste(direct,"Assessment_fns/survey.dat.r",sep="")) #Source21 Revised by DK September 2015
    source(paste(direct,"Assessment_fns/shwt.plt.r",sep="")) #Source22
    source(paste(direct,"Assessment_fns/shwt.plt1.r",sep="")) #Source22
    source(paste(direct,"Assessment_fns/sprSurv.r",sep="")) #Source23
    source(paste(direct,"Assessment_fns/Clap3.plt.R",sep="")) #Source25
    source(paste(direct,"Assessment_fns/gridPlot.r",sep="")) #Source29
    source(paste(direct,"Assessment_fns/logs_and_fishery_data.r",sep=""))
    source(paste(direct,"Assessment_fns/fishery.dat.r",sep=""))
    

    # These two variables need split into north/south manually, so do that here
    GBa.split <- NULL
    surv.split <- NULL
    GBa.split[["GBa.south"]] <- subset(bank.dat[["GBa"]],lat < 41.83333)
    GBa.split[["GBa.south"]] <- subset(bank.dat[["GBa"]],lat >= 41.83333)
    surv.split[["GBa.north"]] <- subset(surv.dat[["GBa"]],lat >= 41.83333)[,1:62]
    surv.split[["GBa.south"]] <- subset(surv.dat[["GBa"]],lat < 41.83333)[,1:62]
    # Set up our shell height data here as well
    SH.dat <- data.frame(year = 1980:2030,CS = c(rep(75,6),rep(85,10),rep(95,2030-1995)),RS = c(rep(60,6),rep(75,10),rep(85,2030-1995)))
    
            
    # Here we need to estimate the area of the low strata in the north and south respectively
    # This is not a rigorous exercise, but should get a good estimate of the area
    # if we went down this road we would need to fix up these strata!
    # First if we look at the random tows in the north and south, given they are allocated randomly
    # in this strata this should get a good estimate of % of each area covered by each strata
    # This suggests that around 70% of the tows that have happened in strata 1 have been in the south, in 
    # agreement with the above area calcs, it ain't perfect but it's good enough to move forward!!
    nrow(subset(GBa.split$GBa.south,stratum ==1)) / 
      (nrow(subset(GBa.split$GBa.south,stratum ==1)) + nrow(subset(GBa.split$GBa.north,stratum ==1)))
    # Looking more recently it is around 67%, seems very consistent that in the neighbourhood of 70% is our split
    nrow(subset(GBa.split$GBa.south,stratum ==1 & year > 2006)) / 
      (nrow(subset(GBa.split$GBa.south,stratum ==1 & year > 2006)) + nrow(subset(GBa.split$GBa.north,stratum ==1 & year > 2006)))
    
    # Just look at the low strata...
    low <- subset(detail.poly.surv,PID==1)
    # I'm sure this isn't entirely correct since I haven't closed off the polygons that
    # cross over between areas, but this also suggests the split is close to 70-30 split
    # Which is in agreement with our survey tow allocations above, and passes the eye test so I will go with this number to split
    south <- sum(calcArea(subset(low, Y < 41.83333))$area) / 
            (sum(calcArea(subset(low, Y < 41.83333))$area) + sum(calcArea(subset(low, Y >= 41.83333))$area))
    
    # Based on this let's get the strata reassigned in our strata defs table.
    strata.areas.full <- subset(survey.info,label=="GBa")
    #a 5-7 are all "southern GBa", 2-4 are all northern
    # strata 1 is split between both regions.  Grab this and make the correction to the area of strata 1
    strata.areas <- NULL
    strata.areas[["GBa.south"]] <- subset(strata.areas.full,PID %in% c(1,5:7))
    strata.areas[["GBa.south"]]$PID <- 1:4
    # Need to renumber 5-7 to 2-4
    strata.areas[["GBa.south"]][1,5:6] <- strata.areas[["GBa.south"]][1,5:6]*south
    strata.areas[["GBa.south"]] <- strata.areas[["GBa.south"]][,c(1,5)]
    # And now the north
    strata.areas[["GBa.north"]] <- subset(strata.areas.full,PID %in% c(1:4))
    strata.areas[["GBa.north"]][1,5:6] <- strata.areas[["GBa.north"]][1,5:6]*(1-south)
    strata.areas[["GBa.north"]] <- strata.areas[["GBa.north"]][,c(1,5)]
    
    #### Now we need to get the strata correct in the data as well, the new.strata is now different but only for the south, 
    surv.split[["GBa.south"]]$new.stratum[surv.split[["GBa.south"]]$new.stratum==5] <- 2
    surv.split[["GBa.south"]]$new.stratum[surv.split[["GBa.south"]]$new.stratum==6] <- 3
    surv.split[["GBa.south"]]$new.stratum[surv.split[["GBa.south"]]$new.stratum==7] <- 4
  
  # Now we can loop through and get the rest of the data necessary to run the model with a north south split
  # first we need to initial some variables
  surv.Clap <- NULL
  surv.Live <- NULL
  surv.Rand <- NULL
  survey.obj<- NULL
  clap.survey.obj <- NULL
  SS.summary <- NULL
  SHF.summary <- NULL
  bnk <- c("GBa.north","GBa.south")   
  years <- 1981:2015
  # with years defined we can set up the CS/RS vectors
  CS <- SH.dat$CS[SH.dat$year %in% years]
  RS <- SH.dat$RS[SH.dat$year %in% years]
  
  for(i in 1:length(bnk))
    {
    
		# Calculate the biomass of the Pre-recruits, Recruits and the Commerical Scallops in each tow on the bank
		#Source14 source("fn/surv.by.tow.r") surv.by.tow calculates number or biomass of pre, rec and com size scallops in each tow
		surv.split[[bnk[i]]] <- surv.by.tow(surv.split[[bnk[i]]], years, pre.ht=RS, rec.ht=CS)
		surv.split[[bnk[i]]] <- surv.by.tow(surv.split[[bnk[i]]], years, pre.ht=RS, rec.ht=CS,type="B",mw.par="CF")
		# Subset the data into the clappers (dead) and live scallops.  Use only random survey tows for Clappers...
		# For GB spring the survey of interest are the comparative tows...
		surv.Clap[[bnk[i]]]<-subset(surv.split[[bnk[i]]],state=='dead'& random==1)
		surv.Live[[bnk[i]]]<-subset(surv.split[[bnk[i]]],state=='live')
		surv.Rand[[bnk[i]]]<-subset(surv.split[[bnk[i]]],state=='live' & random==1)		
	
	
		
		# Clappers the banks for each size class
		
		surv.Clap[[bnk[i]]]$clap.prop<-surv.Clap[[bnk[i]]]$tot/(surv.Rand[[bnk[i]]]$tot+surv.Clap[[bnk[i]]]$tot)*100
		surv.Clap[[bnk[i]]]$clap.prop[is.na(surv.Clap[[bnk[i]]]$clap.prop)]<-0
		surv.Clap[[bnk[i]]]$clap.propCom<-surv.Clap[[bnk[i]]]$com/(surv.Rand[[bnk[i]]]$com+surv.Clap[[bnk[i]]]$com)*100
		surv.Clap[[bnk[i]]]$clap.propCom[is.na(surv.Clap[[bnk[i]]]$clap.propCom)]<-0
		surv.Clap[[bnk[i]]]$clap.propRec<-surv.Clap[[bnk[i]]]$rec/(surv.Rand[[bnk[i]]]$rec+surv.Clap[[bnk[i]]]$rec)*100
		surv.Clap[[bnk[i]]]$clap.propRec[is.na(surv.Clap[[bnk[i]]]$clap.propRec)]<-0
		surv.Clap[[bnk[i]]]$clap.propPre<-surv.Clap[[bnk[i]]]$pre/(surv.Rand[[bnk[i]]]$pre+surv.Clap[[bnk[i]]]$pre)*100
		surv.Clap[[bnk[i]]]$clap.propPre[is.na(surv.Clap[[bnk[i]]]$clap.propPre)]<-0
		surv.Clap[[bnk[i]]]$clap.prop[is.na(surv.Clap[[bnk[i]]]$clap.prop)]<-0
		
		
		
		  
		    survey.obj[[bnk[i]]] <- survey.dat(surv.Rand[[bnk[i]]], RS=RS, CS=CS, 
		                                bk="GBa", areas=strata.areas[[bnk[i]]], mw.par="CF")	
		    clap.survey.obj[[bnk[i]]] <- survey.dat(surv.Clap[[bnk[i]]],SpatHtWt.fit[["GBa"]], RS=RS, CS= CS, 
		                                  bk=bnk[i], areas=strata.areas[[bnk[i]]], mw.par="CF")		
	
		    survey.obj[[bnk[i]]][[1]]$CF <- na.omit(sapply(1:length(years),
		                                    function(x){with(subset(surv.Rand[[bnk[i]]],year == years[x]),
		                                                     weighted.mean(CF,com.bm,na.rm=T))}))
		    survey.obj[[bnk[i]]][[1]]$clappers<-clap.survey.obj[[bnk[i]]][[1]]$N
		    survey.obj[[bnk[i]]][[1]]$clappersR<-clap.survey.obj[[bnk[i]]][[1]]$NR
		  
		  # Mostly due to GB, but I want to have the CS and RS for each year of the calculations here...
		  survey.obj[[bnk[i]]][[1]]$CS <- CS
		  survey.obj[[bnk[i]]][[1]]$RS <- RS
	    # Now get the rest of the Survey summary and SHF summaries for the banks, later we'll export as csv's.
		      SS.summary[[bnk[i]]] <- survey.obj[[bnk[i]]][[1]]
		      SS.summary[[bnk[i]]]$bank <- bnk[i]
		      # Same for the SHF data.
		      SHF.summary[[bnk[i]]] <- as.data.frame(cbind(survey.obj[[bnk[i]]][[1]]$year,survey.obj[[bnk[i]]][[2]]$n.yst))
		      SHF.summary[[bnk[i]]]$bank <- bnk[i]
		  # Give the SS.summary headers nice names and output the results to the appropriate folder
		  names(SS.summary[[bnk[i]]]) <- c("year","n","FR.BM","CV.FR.BM","R.BM","CV.R.BM","Pre.BM","CV.Pre.BM",
		                                   "FR_N", "CV.FR.N",  "R.N","CV.R.N","Pre.N", "CV.Pre.N","bank")
		  
		  

	
  } #end for i in 1:length(bnk)

###############  Now I need to get the fishery data and the von B parameters   ##################################
  
  logs_and_fish(loc="offshore",year = 1981:yr,un=un.ID,pw=pwd.ID,db.con="ptran",direct.off=direct)
  # If you get any NA's related warnings it may be something is being treated as a Factor in one of the two files.  
  # This should combine without any warnings so don't ignore warnings here.
  fish.dat<-merge(new.log.dat,old.log.dat,all=T)
  fish.dat$ID<-1:nrow(fish.data)
  
  # O.K. we can populate the NAFO region properly easily enough here...
  fish.dat$nafo[fish.dat$bank=="GBa" & fish.dat$lat < 41.83333] <- "5ZEM"
  fish.dat$nafo[fish.dat$bank=="GBa" & fish.dat$lat >= 41.83333] <- "5ZEJ"
  
  # Bring in the VonB model parameters
  vonB <- read.csv(paste(direct,"Data/Ageing/Von_B_growth_parameters.csv",sep=""))
  
  # We have 2 banks which are modeled so 
  bank <- c("GBa.south","GBa.north")
  nafo <- c("5ZEM","5ZEJ")
  mod.dat <- NULL
  cpue.dat <- NULL
  proj.dat <- NULL
  
  # Now we need to calculate the growth for the models and we also extract the fishery data for the survey year here.  First up GBa.
  for(i in 1:length(bank))
  {
    years <- min(survey.obj[[bank[i]]][[1]]$year):max(survey.obj[[bank[i]]][[1]]$year)
    # Bring in the vonB parameters..
    vonB.par <-vonB[vonB$Bank =="GBa",]
    # Calculate the fishery data, note that this is on survey year and will differ from the OSAC fishery data...
    cpue.dat[[bank[i]]] <- fishery.dat(fish.dat,bk="GBa",nafo.div=nafo[i],yr=years,method='jackknife',direct=direct,period = "survyr") 	
    # Combine the survey and Fishery data here.
    mod.dat[[bank[i]]] <- merge(survey.obj[[bank[i]]][[1]],cpue.dat[[bank[i]]],by ="year")
    # Get the CV for the CPUE...
    mod.dat[[bank[i]]]$U.cv <- mod.dat[[bank[i]]]$cpue.se/mod.dat[[bank[i]]]$cpue
    # now get the catch data from end of survey until end of the year for the projection
    proj.sub <- subset(fish.dat,year %in% years & months(as.Date(fish.dat$date)) %in% c("September","October","November","December"))
    # Now calculate the fishery statistics for the projection period
    proj.dat[[bank[i]]] <- fishery.dat(proj.sub,bk="GBa",nafo.div=nafo[i],yr=years,method='jackknife',direct=direct,period = "survyr") 	
    # So first up, this condition is the weighted mean condition, this uses the GAM predicted scallop condition factor for each tow
    # and the biomass from each tow to come up with an overall bank average condition factor.
    # This is weight in this year, which becomes t-1 
    waa.tm1 <- mod.dat[[bank[i]]]$CF*(mod.dat[[bank[i]]]$l.bar/100)^3
    # Using this years weight we can find the exptected shell height for the scallops in the next year
    laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K)) + exp(-vonB.par$K) * mod.dat[[bank[i]]]$l.bar
    # Now we can get an expected wight in the next year# Linf * (1-exp(-K)) + exp(-K) * height
    waa.t <- c(mod.dat[[bank[i]]]$CF[-1],mod.dat[[bank[i]]]$CF[nrow(mod.dat[[bank[i]]])])*(laa.t/100)^3
    # Here we use the actual data to calculate the weight, of course for the final year we don't have this
    # so we just use the expectation
    waa.t2 <- mod.dat[[bank[i]]]$CF*(laa.t/100)^3
    # Now the growth, expected and realized.
    mod.dat[[bank[i]]]$g <- waa.t/waa.tm1
    mod.dat[[bank[i]]]$g2 <- waa.t2/waa.tm1
    
    # for recruits
    waa.tm1 <- mod.dat[[bank[i]]]$CF*(mod.dat[[bank[i]]]$l.k/100)^3
    laa.t <- vonB.par$Linf*(1-exp(-vonB.par$K))+exp(-vonB.par$K)*mod.dat[[bank[i]]]$l.k
    waa.t <- c(mod.dat[[bank[i]]]$CF[-1],mod.dat[[bank[i]]]$CF[nrow(mod.dat[[bank[i]]])])*(laa.t/100)^3
    waa.t2 <- mod.dat[[bank[i]]]$CF*(laa.t/100)^3
    mod.dat[[bank[i]]]$gR <- waa.t/waa.tm1
    mod.dat[[bank[i]]]$gR2 <- waa.t2/waa.tm1# setwd("C:/Assessment/2014/r")
  } # end for(i in 1:length(bank))  
  # The catch has been cross-checked and is almost identical to the full bank catch reported elsewhere other than in the
  # first half of the 80's where we are missing information on where the catch was... most excellent!
  #mod.dat$GBa.south$catch + mod.dat$GBa.north$catch
  
# Now there are a few bits of NA's that will give us trouble later so lets clean those us...  
# For the these missing terms I will just replace with the bank average growth
mod.dat$GBa.north$g[is.na(mod.dat$GBa.north$g)] <- median(mod.dat$GBa.north$g,na.rm=T)
mod.dat$GBa.north$g2[is.na(mod.dat$GBa.north$g2)] <- median(mod.dat$GBa.north$g2,na.rm=T)
mod.dat$GBa.north$gR[is.na(mod.dat$GBa.north$gR)] <- median(mod.dat$GBa.north$gR,na.rm=T)
mod.dat$GBa.north$gR2[is.na(mod.dat$GBa.north$gR2)] <- median(mod.dat$GBa.north$gR2,na.rm=T)
mod.dat$GBa.north$l.bar[is.na(mod.dat$GBa.north$l.bar)] <- median(mod.dat$GBa.north$l.bar,na.rm=T)
mod.dat$GBa.north$w.bar[is.na(mod.dat$GBa.north$w.bar)] <- median(mod.dat$GBa.north$w.bar,na.rm=T)
mod.dat$GBa.north$w.k[is.na(mod.dat$GBa.north$w.k)] <- median(mod.dat$GBa.north$w.k,na.rm=T)
mod.dat$GBa.north$l.k[is.na(mod.dat$GBa.north$l.k)] <- median(mod.dat$GBa.north$l.k,na.rm=T)

# In the south we have funny businessin 2007-2008 as there was only one trip during the 2008 survey year down here..
mod.dat$GBa.south$cpue.var[is.na(mod.dat$GBa.south$cpue.var)] <- median(mod.dat$GBa.south$cpue.var,na.rm=T) 
mod.dat$GBa.south$cpue.se[mod.dat$GBa.south$year ==2008]] <- sqrt(mod.dat$GBa.south$cpue.var[mod.dat$GBa.south$year ==2008])
mod.dat$GBa.south$U.cv <- mod.dat$GBa.south$cpue.se/mod.dat$GBa.south$cpue

  
##########################################################################################################################
# Now that we've done everything we can save all the results or we could skip ahead and just load them all if already run.
rm("un.ID","pwd.ID")
# If we have all the data save it as this
save(list = ls(all.names = TRUE), 
                          file = paste(direct,"Data/Model/",(yr+1),"/Framework/GBa_north_south_results.Rdata",sep=""))


	