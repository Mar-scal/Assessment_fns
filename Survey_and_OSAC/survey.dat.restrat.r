# survey.dat.restrat.r
################################################################################################################
####  Created by FK in May 2018.  This function uses Domain.estimates to calculate stratified estimates and CVs for Sable Bank 
#### for years prior to 2018, and PEDstrata to get the stratified mean and variance (CV) from the post-2018 survey data. 
#### 
####
# Update history
# May 2018: FK created this based on survey.dat, but with domain estimation. When more banks need restratifying, you must edit
#           Survey.Summary_data.r to assign strata correctly (see line ~418 assign.strata() if statement) and 
#           bring that bank to this script instead of using survey.dat (see Sable example on line 734 of SurveySummary_data.r). 
#           This method also requires that all new strata coordinates and areas be appended to the following files,
#           with the startyear column filled in for the year of the change:
#           paste(direct,"data/Survey_data/survey_information.csv",sep="")
#           paste(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv")
#           paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv")
#           DO NOT DELETE OLD STRATA INFORMATION from any of these files as this is used to restratify historical tow data.
#           *** Note: this method will require revisions if a bank is restratified more than once ***
#           Documentation is in direct/Assessment_fns/Survey_and_OSAC/README_restratification.md
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary_data.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################

###############################################################################################################
# source(".../Assessment_fns/Survey/survey.dat.restrat.r") ok

# Arguments
#shf:          Shell height frequency data
#htwt.fit:     Shell height Meat Weight model fit
#years:        Years of interest, not required.
#RS:           Size a scallop becomes a recruit:  Defalut = 80
#CS:           Size a scallop becomes commercial:  Default = 100
#bk:           The bank of interest.  Default = "Sab".  Currently this should only run for Sab because it's the only restratified bank.
#areas:        An object with the strata numbers/names and the area covered by each stratum.          
#mw.par:       How is meat weight to be calculated.  Default = 'annual', options are ('fixed' or 'annual') alternatively
#              some variant of "CF" is used if the meat weight is being calculated from condition factor.  Need to have
#              mw.par = column name that includes CF data for this option to work properly.
# user.bins:   Calculate the biomass of specified user bins, these bins will have had to already been calculated with the surv.by.tow function
#              for this to work!!  Default is NULL which doesn't do any calculations.  If not null user.bins should look something like 
#              user.bins <- c(50,70,90,110)
###############################################################################################################

survey.dat.restrat <- function(shf, htwt.fit, years, RS=80, CS=100, bk="Sab", areas,  mw.par='annual',user.bins = NULL) {

  if(!bk=="Sab") print("You wound up in survey.dat.restrat even though your bank shouldn't be restratified. How did you get here? 
                        Please return to SurveySummary_data.r")
  if(bk=="Sab"){
    # load the PEDstrata package, note this is a locally developed package not available from R repositories
    require(PEDstrata)  || stop("PEDstrata package required please obtain a copy and install this locally developed package")
    require(survey)     || stop("survey package required please install package and try again")
    require(splancs)    || stop("splancs package required please install package and try again")
    # This is silly, but for the below code to work we need to increase the RS/CS by 5
    RS <- RS + 5
    CS <- CS + 5

    # If years is not supplied than obtain from the data
    if(missing(years)==T) years<-sort(unique(shf$year))

    # Sable was restratified prior to 2018 survey due to the creation of the Western Emerald Bank Conservation Area. 
    # It was decided at February 2018 Survey Working Group meeting to remove WEBCA from Sable strata, therefore, a domain estimator
    # is required to convert pre-2018 survey data to the new strata. For this reason, we must break this script into a pre and post 
    # re-stratification sections.
    # 1900 has been set as the start year for all offshore strata up to 2018, since these are the strata to be used for all data < 2018.
    
    # Create strata object for PEDstrata package, includes Strata and number of towable units in that strata.
    # I know Strata and strata.id are the same, but Domainestimates.R uses both columns, and I'm rolling with the punches here.
    HSIstrata.obj <- data.frame(Strata=areas[,1], strata.id=areas[,1], NH=areas[,2], startyear=areas[,3])[order(areas[,1]),]
    if(length(unique(HSIstrata.obj$startyear))==2){
      strata.obj <- HSIstrata.obj[HSIstrata.obj$startyear == min(unique(HSIstrata.obj$startyear)),]
      domain.obj <- HSIstrata.obj[HSIstrata.obj$startyear == max(unique(HSIstrata.obj$startyear)),]

      # also need to make the strata names different from each other. So I'm just making up a new convention for the new strata.
      domain.obj$Strata <- paste0(domain.obj$Strata, "_2.0")
      domain.obj$strata.id <- paste0(domain.obj$strata.id, "_2.0")
      
      # and if you haven't already, you gotta do this in your actual tow data too. Everything has to match.
      if(nchar(x = unique(shf$Strata_ID_new)) == nchar(x = unique(shf$Strata_ID_old))) shf$Strata_ID_new  <- paste0(shf$Strata_ID_new, "_2.0")
    }
    
    # need to change the names of strata.obj and domain.obj
    names(strata.obj)[1] <- "STRATA.ID.OLD"
    names(domain.obj)[1] <- "STRATA.ID.NEW"
    
    if(!length(unique(HSIstrata.obj$startyear))==2){
      print("Houston you have a problem. You either don't need to restratify this bank at all, and therefore shouldn't be in the survey.dat.restrat function,
  or you have to re-write this to accommmodate more than 1 restratification. Sorry that you have to go through that, cuz even doing this much was hard.
  Take a walk to clear your mind and prepare yourself for some coding!")
    }
    
    # Output the object to screen and determine the number of towable units for this bank.
    print(strata.obj)
    print(domain.obj)
    N.tu.old <- strata.obj$NH
    N.tu <- domain.obj$NH
    
    # for easier indexing of shell height bins in shf
    bin <- as.numeric(substr(names(shf),2,nchar(names(shf))))
    
    # And make some more objects if we have user specified bins
    if(!is.null(user.bins)) 
    {
      # Get the names for the user bins and good names for the final results...
      bnames <- paste0("bin_lt_",user.bins[1])
      mean.names <- paste0("mean_lt_",user.bins[1])
      CV.names <- paste0("CV_lt_",user.bins[1])
      for(p in 1:length(user.bins)+1)
      {
        if(p > 1 && p < length(user.bins)+1)
        {
          bnames[p] <- paste0("bin_",user.bins[p-1],"-",user.bins[p])
          mean.names[p] <- paste0("mean_",user.bins[p-1],"-",user.bins[p])
          CV.names[p] <- paste0("CV_",user.bins[p-1],"-",user.bins[p])
        } # end if(p > 1 && p < length(user.bins)+1)
        
        if(p == length(user.bins)+1)
        {
          bnames[p] <- paste0("bin_",user.bins[p-1],"_plus")
          mean.names[p] <- paste0("mean_",user.bins[p-1],"_plus")
          CV.names[p] <- paste0("CV_",user.bins[p-1],"_plus")
        } # end if(p == length(user.bins)+1)
      } # End for(p in 1:length(user.bins)+1)
      
      bnames <- c(bnames,paste0(bnames,"_bm"))
      mean.names <- c(mean.names,paste0(mean.names,"_bm"))
      CV.names <- c(CV.names,paste0(CV.names,"_bm"))
      tmp <- as.data.frame(matrix(NA,ncol = (length(CV.names) + length(mean.names)+1) ,nrow=length(years)))
      names(tmp) <- c(mean.names,CV.names,"year")
      tmp$year <- years
    } # end if(!is.null(user.bins))
    
    # intialize objects for upcoming for loop.
    w.yst <- matrix(NA, length(years), 40)
    n.yst <- w.yst
    n.stratmeans <-list(NULL)
    w.stratmeans <-list(NULL)
    avgsizepertow <- list(NULL)
    strat.res <- data.frame(year=years)
    Strata.obj <- NULL
    Domain.obj <- NULL
    mw <- NULL
    bankpertow <- NULL
    
    #objects for domain estimation
    scall.est.w.IPR <- NULL
    scall.est.w.IR <- NULL
    scall.est.w.I <- NULL
    scall.est.n.IPR <- NULL
    scall.est.n.IR <- NULL
    scall.est.n.I <- NULL
    scall.dom.w.bins<- NULL
    scall.dom.n.bins<- NULL
    out.domain <- data.frame(YEAR=years,BANK=bk,
                             yst.w.IPR=rep(NA,length(years)),
                             var.yst.w.IPR=rep(NA,length(years)),
                             yst.w.IR=rep(NA,length(years)),
                             var.yst.w.IR=rep(NA,length(years)),
                             yst.w.I=rep(NA,length(years)),
                             var.yst.w.I=rep(NA,length(years)),
                             yst.n.IPR=rep(NA,length(years)),
                             var.yst.n.IPR=rep(NA,length(years)),
                             yst.n.IR=rep(NA,length(years)),
                             var.yst.n.IR=rep(NA,length(years)),
                             yst.n.I=rep(NA,length(years)),
                             var.yst.n.I=rep(NA,length(years)),
                             descrip=rep('domain',length(years))) 
    
    
    # If CS and RS are just one value turn them into a vector the same length as the number of years of data.
    if(length(CS) == 1)	CS <- rep(CS, length(years))
    if(length(RS) == 1)	RS <- rep(RS, length(years))
    
    # For loop to do the calculations of meat weight for non-restratified banks. Domain estimation happens in here too!
    for(i in 1:length(years))
    {
      # Set the bins
      mw.bin<-seq(5,200,5)
      # select the current years data.
      ann.dat<-subset(shf,year==years[i])
      # Use the MW-SH model fit to calculate the meat weight, assumes that year was a random effect in the model
      # Remember mw is in grams here.
      # FK had to specify htwt.fit <- SpatHtWt.fit[[bk]]??
      if(mw.par=='annual') mw[[i]] <- matrix(exp(log(seq(2.5,200,5))*htwt.fit$b[i]+log(htwt.fit$a[i])),
                                             nrow(ann.dat),40,byrow=T,dimnames=list(ann.dat$tow,mw.bin))
      # Use the MW-SH model fit to calculate the meat weight, assumes that year was not included in the model
      # Remember mw is in grams here.
      
      if(mw.par=='fixed') mw[[i]]<-matrix(exp(log(seq(2.5,200,5))*htwt.fit$B+htwt.fit$A),nrow(ann.dat),40,
                                          byrow=T,dimnames=list(ann.dat$tow,mw.bin))
      # DK Note:  So as this was it would overwright the calculations from mw.par=="annual" but this
      # would actually cause an error if ever this was specified as annual
      # Use some other data to estimate Meat Weight, Condition factor generally used for this option.
      # Remember mw is in grams here.
      
      if(mw.par !='annual' && mw.par !='fixed') mw[[i]]<-sweep(matrix((seq(2.5,200,5)/100)^3,nrow(ann.dat),
                                                                      40,byrow=T,dimnames=list(ann.dat$tow,mw.bin)),1,FUN='*',ann.dat[,mw.par])
      print("Careful, you didn't specify the location for prediction of CF so I have picked mean depth, lat, and lon between 2005 and 2014 be sure this is how this has been done in the past!")
      
      num <- data.frame(subset(shf, year==years[i], which(bin==5):which(bin==200)), 
                        STRATA.ID.NEW=shf$Strata_ID_new[shf$year==years[i]], 
                        STRATA.ID.OLD=shf$Strata_ID_old[shf$year==years[i]])
      
      # Remove rows with strata ID's which are NA's
      num<-na.omit(num)
      
      # Add up the numbers of Scallops in each size category.
      num$pre <- rowSums(num[, which(mw.bin==5):(which(mw.bin==RS[i])-1)],na.rm=T)
      num$rec <- rowSums(num[, which(mw.bin==RS[i]):(which(mw.bin==CS[i])-1)],na.rm=T)
      num$com <- rowSums(num[, which(mw.bin==CS[i]):which(mw.bin==200)],na.rm=T)
      
      # Make a dataframe with the biomasses for each bin and tow, add the strata ID's as well
      # This is in grams per tow
      w <- data.frame(subset(shf, year==years[i], which(bin==5):which(bin==200))*mw[[i]], 
                      STRATA.ID.NEW=shf$Strata_ID_new[shf$year==years[i]],
                      STRATA.ID.OLD=shf$Strata_ID_old[shf$year==years[i]])
      
      # Remove any rows in which the strata is NA
      w<-na.omit(w)
      # Add up the biomass of Scallops in each size category, again this is in grams per tow
      w$pre <- rowSums(w[, which(mw.bin==5):(which(mw.bin==RS[i])-1)],na.rm=T)
      w$rec <- rowSums(w[, which(mw.bin==RS[i]):(which(mw.bin==CS[i])-1)],na.rm=T)
      w$com <- rowSums(w[, which(mw.bin==CS[i]):which(mw.bin==200)],na.rm=T)
      
      # The proportion of towable area in each strata.
      # run HSIstrata.obj to remind yourself of the years for each if you want
      pstrat_new <- as.numeric(N.tu/sum(N.tu))
      pstrat_old <- as.numeric(N.tu.old/sum(N.tu.old))
      
      pstrat_new <- data.frame(prop=c(pstrat_new, NA), strata_id=c(paste0(501:505, "_2.0"), "NA_2.0"))
      
      # restratification occurs for all years < 2018
      if(years[i]<max(unique(HSIstrata.obj$startyear))) {
        # get domaine estimator for biomasses in each size category - Domain.estimates(data, Strata, Domain, strata.obj, domain.obj, Nd = NULL
        #source("Y:/Offshore scallop/Assessment/Assessment_fns/Survey_and_OSAC/Domainestimates.R")
        # step 1: get a domain estimation object for each size category using BIOSurvey2::Domain.est function
        scall.dom.IPR <- BIOSurvey2::Domain.est(x = w, Strata="STRATA.ID.OLD",
                                                Domain="STRATA.ID.NEW", strata.obj=strata.obj,
                                                domain.obj=domain.obj,
                                                Species = "pre")
        scall.dom.IR <- BIOSurvey2::Domain.est(x = w, Strata="STRATA.ID.OLD",
                                               Domain="STRATA.ID.NEW", strata.obj=strata.obj,
                                               domain.obj=domain.obj,
                                               Species = "rec")
        scall.dom.I <- BIOSurvey2::Domain.est(x = w, Strata="STRATA.ID.OLD",
                                              Domain="STRATA.ID.NEW", strata.obj=strata.obj,
                                              domain.obj=domain.obj,
                                              Species = "com")
        scall.dom.NPR <- BIOSurvey2::Domain.est(x = num, Strata="STRATA.ID.OLD",
                                                Domain="STRATA.ID.NEW", strata.obj=strata.obj,
                                                domain.obj=domain.obj,
                                                Species = "pre")
        scall.dom.NR <- BIOSurvey2::Domain.est(x = num, Strata="STRATA.ID.OLD",
                                               Domain="STRATA.ID.NEW", strata.obj=strata.obj,
                                               domain.obj=domain.obj,
                                               Species = "rec")
        scall.dom.N <- BIOSurvey2::Domain.est(x = num, Strata="STRATA.ID.OLD",
                                              Domain="STRATA.ID.NEW", strata.obj=strata.obj,
                                              domain.obj=domain.obj,
                                              Species = "com")
        
        # step 2: use BIOSurvey2::summary.domain.est to get summary of stratified design. This returns a number of useful survey design results and optimization summaries.
        IPR.tmp <- BIOSurvey2::summary.domain.est(scall.dom.IPR)
        IR.tmp <- BIOSurvey2::summary.domain.est(scall.dom.IR)
        I.tmp <- BIOSurvey2::summary.domain.est(scall.dom.I)
        NPR.tmp <- BIOSurvey2::summary.domain.est(scall.dom.NPR)
        NR.tmp <- BIOSurvey2::summary.domain.est(scall.dom.NR)
        N.tmp <- BIOSurvey2::summary.domain.est(scall.dom.N)
        
        # step 3: grab the "yst" (stratified estimates) for each size category. Remember you're still in a year loop, so this is only one year at a time.
        out.domain[i, seq(3, 13, 2)] <- as.numeric(c(IPR.tmp[[2]][2],
                                                     IR.tmp[[2]][2],
                                                     I.tmp[[2]][2],
                                                     NPR.tmp[[2]][2],
                                                     NR.tmp[[2]][2],
                                                     N.tmp[[2]][2]))
        # step 4: grab the "var.yst" (stratified estimate variances) for each size category too. Refer to Y:\INSHORE SCALLOP\BoF\Assessment_fns\CV calculations for Models.docx
        #         if you want more info on variance/CVs/error for stratified estimates. It's not as easy as you'd hope, but this is why we have the wonderful PEDStrata and BIOsurvey2 packages!
        out.domain[i, seq(4, 14, 2)] <- as.numeric(c(IPR.tmp[[2]][3],
                                                     IR.tmp[[2]][3],
                                                     I.tmp[[2]][3],
                                                     NPR.tmp[[2]][3],
                                                     NR.tmp[[2]][3],
                                                     N.tmp[[2]][3]))
        
        # out.domain CONTAINS STRATIFIED ESTIMATES & VARIANCES (not CVs)
       
        # Step 5: Put the appropriate biomass and count values into respective Strata.obj list objects
        Domain.obj$I[[i]] <- out.domain$yst.w.I[i]
        Domain.obj$IR[[i]] <- out.domain$yst.w.IR[i]
        Domain.obj$IPR[[i]] <- out.domain$yst.w.IPR[i]
        Domain.obj$N[[i]] <- out.domain$yst.n.I[i]
        Domain.obj$NR[[i]] <- out.domain$yst.n.IR[i]
        Domain.obj$NPR[[i]] <- out.domain$yst.n.IPR[i]
        
        # Step 6: Put the total number of tows into the strat.res object
        strat.res$n[i] <- sum(scall.dom.I$nh)
        
        # Step 7: Convert to Biomass (tonnes) and abundance (in millions) estimates for the bank
        strat.res$I[i] <- I.tmp[[2]]$yst * sum(N.tu) / 10^6			#g to t
        strat.res$IR[i] <- IR.tmp[[2]]$yst * sum(N.tu) / 10^6			#g to t
        strat.res$IPR[i] <- IPR.tmp[[2]]$yst * sum(N.tu) / 10^6			#g to t
        strat.res$N[i] <- N.tmp[[2]]$yst * sum(N.tu) / 10^6			#in millions
        strat.res$NR[i] <- NR.tmp[[2]]$yst * sum(N.tu) / 10^6			#in millions
        strat.res$NPR[i] <- NPR.tmp[[2]]$yst * sum(N.tu) / 10^6			#in millions
        
        # Step 8: Calculate the CVs based on Y:\INSHORE SCALLOP\BoF\Assessment_fns\CV calculations for Models.docx
        strat.res$I.cv[i] <- sqrt(I.tmp[[2]]$var.yst) / I.tmp[[2]]$yst
        strat.res$IR.cv[i] <- sqrt(IR.tmp[[2]]$var.yst) / IR.tmp[[2]]$yst
        strat.res$IPR.cv[i] <- sqrt(IPR.tmp[[2]]$var.yst) / IPR.tmp[[2]]$yst
        strat.res$N.cv[i] <- sqrt(N.tmp[[2]]$var.yst) / N.tmp[[2]]$yst
        strat.res$NR.cv[i] <- sqrt(NR.tmp[[2]]$var.yst) / NR.tmp[[2]]$yst
        strat.res$NPR.cv[i] <- sqrt(NPR.tmp[[2]]$var.yst) / NPR.tmp[[2]]$yst
        
        # Save the bank-wide per tow mean estimates
        bankpertow <- rbind(bankpertow, data.frame(year=years[i], N = N.tmp[[2]]$yst, NR = NR.tmp[[2]]$yst, NPR = NPR.tmp[[2]]$yst, 
                                                   I=I.tmp[[2]]$yst, IR=IR.tmp[[2]]$yst, IPR=IPR.tmp[[2]]$yst))
       
        # We do not have Strata.obj information for these years since PEDstrata is no longer used
        Strata.obj$I[[i]] <- list(NA)
        Strata.obj$IR[[i]] <- list(NA)
        Strata.obj$IPR[[i]] <- list(NA)
        Strata.obj$N[[i]] <- list(NA)
        Strata.obj$NR[[i]] <- list(NA)
        Strata.obj$NPR[[i]] <- list(NA)
        
      }# end if(years[i] < year of restratification )
      
      # if the year is after 2018, then we can just run PEDstrata as normal, since the survey was conducted using the current strata.
      if(years[i] == max(unique(HSIstrata.obj$startyear)) | years[i] > max(unique(HSIstrata.obj$startyear))) {
        
        # Step 1: Calculate stratified estimates of biomass and abundances for 3 size categories. Save the summary objects, which contain a number of useful survey design results and optimization summaries.
        # Step 1a: PEDstrata requires that the column name be "Strata" in your strata.group object. So do this, and then calculate.
        names(domain.obj)[1] <- "Strata"
        Strata.obj$I[[i]] <- PEDstrata(w, domain.obj, 'STRATA.ID.NEW', w$com)
        Strata.obj$IR[[i]] <- PEDstrata(w, domain.obj, 'STRATA.ID.NEW', w$rec)
        Strata.obj$IPR[[i]] <- PEDstrata(w, domain.obj, 'STRATA.ID.NEW', w$pre)
        Strata.obj$N[[i]] <- PEDstrata(num, domain.obj, 'STRATA.ID.NEW', num$com)
        Strata.obj$NR[[i]] <- PEDstrata(num, domain.obj, 'STRATA.ID.NEW', num$rec)
        Strata.obj$NPR[[i]] <- PEDstrata(num, domain.obj, 'STRATA.ID.NEW', num$pre)
        
        I.tmp <- summary(Strata.obj$I[[i]], effic = T)
        IR.tmp <- summary(Strata.obj$IR[[i]], effic = T)
        IPR.tmp <- summary(Strata.obj$IPR[[i]], effic = T)
        N.tmp <- summary(Strata.obj$N[[i]], effic = T)
        NR.tmp <- summary(Strata.obj$NR[[i]], effic = T)
        NPR.tmp <- summary(Strata.obj$NPR[[i]], effic = T)
        
        # Step 2: Store the stratified estimates in Strata.obj. Note that the Strata.obj for Sable will ONLY contain the yst
        # values now, and will therefore not match the Strata.obj for other banks.
        Domain.obj$I[[i]] <- I.tmp$yst
        Domain.obj$IR[[i]] <- IR.tmp$yst
        Domain.obj$IPR[[i]] <- IPR.tmp$yst
        Domain.obj$N[[i]] <- N.tmp$yst
        Domain.obj$NR[[i]] <- NR.tmp$yst
        Domain.obj$NPR[[i]] <- NPR.tmp$yst
        
        # Step 3: Put the total number of tows in strat.res object
        strat.res$n[i] <- sum(Strata.obj$I[[i]]$nh)
        
        # Step 4: Calculate Biomass (tonnes) and abundance (millions) estimates for the bank
        strat.res$I[i] <- I.tmp$yst * sum(N.tu) / 10^6			#g to t
        strat.res$IR[i] <- IR.tmp$yst * sum(N.tu) / 10^6			#g to t
        strat.res$IPR[i] <- IPR.tmp$yst * sum(N.tu) / 10^6			#g to t
        strat.res$N[i] <- N.tmp$yst * sum(N.tu) / 10^6			#in millions
        strat.res$NR[i] <- NR.tmp$yst * sum(N.tu) / 10^6			#in millions
        strat.res$NPR[i] <- NPR.tmp$yst * sum(N.tu) / 10^6			#in millions
        
        # Step 5: Calculate the CVs, based on Y:\INSHORE SCALLOP\BoF\Assessment_fns\CV calculations for Models.docx
        strat.res$I.cv[i] <- I.tmp$se.yst / I.tmp$yst
        strat.res$IR.cv[i] <- IR.tmp$se.yst / IR.tmp$yst
        strat.res$IPR.cv[i] <- IPR.tmp$se.yst / IPR.tmp$yst
        strat.res$N.cv[i] <- N.tmp$se.yst / N.tmp$yst
        strat.res$NR.cv[i] <- NR.tmp$se.yst / NR.tmp$yst
        strat.res$NPR.cv[i] <- NPR.tmp$se.yst / NPR.tmp$yst
        
        # Save the bank-wide per tow estimates
        bankpertow <- rbind(bankpertow, data.frame(year=years[i], N = N.tmp$yst, NR = NR.tmp$yst, NPR = NPR.tmp$yst, 
                                                   I=I.tmp$yst, IR=IR.tmp$yst, IPR=IPR.tmp$yst))
        
        
      } # end if(years[i] = or > year of restratification )
      
      # By this point, we should have matching data whether it's pre re-stratification or post, so we can go back to treating them the same way from here on.
      
      # Calculate the mean abundance and mean biomass (grams) per tow (this is based on new strata). 
      # Note, this is the SAME as calculating a stratified estimate with PEDstrata package, but the format is nicer and it ignores the variance calculation
      n.stratmeans[[i]] <- with(num, sapply(1:40, function(x){tapply(num[,x],STRATA.ID.NEW,mean)}))
      w.stratmeans[[i]] <- with(w, sapply(1:40, function(x){tapply(w[,x],STRATA.ID.NEW,mean)}))
      #Multiply the mean abundance(biomass) in each shell height category in a strata by the proportion of towable area
      #in that strata.  Sum this product for each strata resulting in an estimate of total abundance (biomass) for each
      #shell height category in a given year. (ybar_st)
      if(is.null(nrow(n.stratmeans[[i]]))) n.yst[i,] <- n.stratmeans[[i]]
      if(!is.null(nrow(n.stratmeans[[i]]))) n.yst[i,] <- apply(X=sapply(1:nrow(n.stratmeans[[i]]), function(x){n.stratmeans[[i]][x,] * pstrat_new$prop[pstrat_new$strata_id %in% row.names(n.stratmeans[[i]])][x]}),MARGIN = 1, FUN = function(X) sum(X, na.rm=T))
      #  Now multiply by the total bank area to determine the survey estimated abundance(biomass).
      # The abundance is actual numbers
      n.Yst <- n.yst[i,] * sum(N.tu)
      if(is.null(nrow(w.stratmeans[[i]])))  w.yst[i,] <- w.stratmeans[[i]]
      if(!is.null(nrow(w.stratmeans[[i]]))) w.yst[i,] <- apply(X=sapply(1:nrow(w.stratmeans[[i]]), function(x){w.stratmeans[[i]][x,] * pstrat_new$prop[pstrat_new$strata_id %in% row.names(n.stratmeans[[i]])][x]}),MARGIN=1, FUN = function(X) sum(X, na.rm=T))
      w.Yst <- w.yst[i,] * sum(N.tu)
      
      # Average weight of fully recruited scallop by year
      strat.res$w.bar[i] <- sum(w.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)]) /
        sum(n.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])
      
      # Average shell height of fully recruited scallop by year
      strat.res$l.bar[i] <- sum((n.yst[i,]*seq(2.5,200,5))[which(mw.bin==CS[i]):which(mw.bin==200)]) / 
        sum(n.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])
      strat.res$l.k[i] <- sum((n.yst[i,]*seq(2.5,200,5))[which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)]) / 
        sum(n.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)])
      
      # Weight at size of recruitment by year
      strat.res$w.k[i] <- sum(w.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)]) /
        sum(n.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)])		
      
      strat.res[i,] <- cbind(years[i], strat.res$n[i], strat.res$I[i],
                                strat.res$I.cv[i], strat.res$IR[i], strat.res$IR.cv[i],
                                  strat.res$IPR[i],  strat.res$IPR.cv[i],
                                  strat.res$N[i], strat.res$N.cv[i],
                                strat.res$NR[i], strat.res$NR.cv[i],
                                  strat.res$NPR[i], strat.res$NPR.cv[i], 
                                  strat.res$w.bar[i], strat.res$l.bar[i],
                                  strat.res$l.k[i], strat.res$w.k[i])
      
      strat.res[,2:dim(strat.res)[2]] <- apply(strat.res[,2:dim(strat.res)[2]], 2, function(x) as.numeric(as.character(x)))

      # Average size per tow
      ## total caught in tow
      num$tot <- rowSums(num[,1:40])
      avgsizepertow[[i]] <- rowSums(t(apply(num[,1:40], 1, function(x) mw.bin*x)),na.rm=T)/num$tot
      
      # So I need to get the results for the user specified SH bins if they are requested.  
      if(!is.null(user.bins))
      {
        # Now get the annual estimate for each of our user bins.
        user.bin.res <- NULL
        for(k in 1:(length(user.bins)+1))
        {
          # Here's the results by bin, non-stratified at the moment!
          if(k == 1)
          {
            # The abundance and biomass of the smallest user specified size bin
            user.bin.res[[bnames[k]]]  <- rowSums(num[, which(mw.bin==5):which(mw.bin==user.bins[k])],na.rm=T)
            user.bin.res[[bnames[k+length(user.bins)+1]]] <- rowSums(w[, which(mw.bin==5):which(mw.bin==user.bins[k])],na.rm=T)
          } #end if(k == 1)
          # For the middle size categories
          if(k > 1 && k <= length(user.bins))
          {
            # The abundance and biomass of the smallest user specified size bin
            user.bin.res[[bnames[k]]]  <- rowSums(num[, which(mw.bin==user.bins[k-1]+5):which(mw.bin==user.bins[k])],na.rm=T)
            user.bin.res[[bnames[k+length(user.bins)+1]]] <- rowSums(w[, which(mw.bin==user.bins[k-1]+5):which(mw.bin==user.bins[k])],na.rm=T)
          } # end if(k > 1 && k <= length(user.bins))
          # And finally the largest size categories
          if(k == (length(user.bins)+1))
          {
            # The abundance and biomass of the smallest user specified size bin
            user.bin.res[[bnames[k]]]  <-  rowSums(num[, which(mw.bin==user.bins[k-1]+5):which(mw.bin==200)],na.rm=T)
            user.bin.res[[bnames[k+length(user.bins)+1]]] <- rowSums(w[, which(mw.bin==user.bins[k-1]+5):which(mw.bin==200)],na.rm=T)
          } # end if(k == (length(user.bins)+1))
          
          # need to have the strata assigned here too.
          user.bin.res$STRATA.ID.OLD <- w$STRATA.ID.OLD
          user.bin.res$STRATA.ID.NEW <- w$STRATA.ID.NEW
          
          user.bin.res <- as.data.frame(user.bin.res)
        }# end for(k in 1:length(user.bins)+1))

        # Now we can get the stratified results... 
        for(f in 1:length(mean.names))
        {
          
          if(years[i]<max(unique(HSIstrata.obj$startyear))){
            # get domaine estimator for biomasses in each size category - Domain.est(data, Strata, Domain, strata.obj, domain.obj, Nd = NULL
            #source("Y:/Offshore scallop/Assessment/Assessment_fns/Survey_and_OSAC/Domainestimates.R")
            scall.dom.w.userbin <- BIOSurvey2::Domain.est(x = user.bin.res, "STRATA.ID.OLD", "STRATA.ID.NEW", strata.obj, domain.obj, bnames[f])
            
            # summary of stratified design, returns a number of useful survey design results and optimization summaries.
            res.tmp <- BIOSurvey2::summary.domain.est(scall.dom.w.userbin)[[2]]
            res.tmp$n[i] <- sum(scall.dom.w.userbin$nh, na.rm=T)
            
            tmp[i,mean.names[f]] <-  res.tmp$yst* sum(N.tu)/10^6			# in millions or tonnes...
            # Strata calculations for biomass for pre-recruit sized Scallops
            tmp[i,CV.names[f]] <- sqrt(res.tmp$var.yst) /  res.tmp$yst
          }
          
          if(years[i]>max(unique(HSIstrata.obj$startyear)) | years[i]==max(unique(HSIstrata.obj$startyear))) {
            # The stratified calculation/object
            res.tmp <- summary(PEDstrata(w, domain.obj, 'STRATA.ID.NEW', user.bin.res[[bnames[f]]]),effic=T)
            
            tmp[i,mean.names[f]] <-  res.tmp$yst* sum(N.tu)/10^6			# in millions or tonnes...
            # Strata calculations for biomass for pre-recruit sized Scallops
            tmp[i,CV.names[f]] <- res.tmp$se.yst /  res.tmp$yst
          }
        } # end for(m in 1:length(mean.names))
      } # end if(!is.null(user.bins))
    }# end for(i in 1:length(years))
    
    names(strat.res) <- c("year", "n", "I", "I.cv",
                          "IR", "IR.cv", "IPR", "IPR.cv",
                          "N", "N.cv", "NR", "NR.cv", "NPR", "NPR.cv",
                          "w.bar", "l.bar", "l.k", "w.k")
    
    # Data for the delay-difference stock assessment model and survey summary
    if(!is.null(user.bins)) model.dat <- merge(strat.res,tmp)
    
    if(is.null(user.bins))  model.dat <-  strat.res
    
    # Data for shf plots used in the survey summary
    shf.dat <- list(n.yst=n.yst,w.yst=w.yst,n.stratmeans=n.stratmeans,w.stratmeans=w.stratmeans,avgsizepertow=avgsizepertow)
    # Return the data to function calling it.

    model.dat$year <- as.numeric(as.character(model.dat$year))
    
    if(is.null(user.bins)) return(list(model.dat=model.dat,shf.dat=shf.dat,Strata.obj=Strata.obj, Domain.obj=Domain.obj, bankpertow=bankpertow))
    if(!is.null(user.bins)) return(list(model.dat=model.dat,shf.dat=shf.dat,Strata.obj=Strata.obj, Domain.obj=Domain.obj, bin.names = bnames,user.bins = user.bins, bankpertow=bankpertow))

    model.dat
    
  }# end if(bk=="Sab")
  
  # Write a file with strata tow assignments just for fun:
  
  # write.csv(paste(direct, ))
} # end survey.dat.restrat()
