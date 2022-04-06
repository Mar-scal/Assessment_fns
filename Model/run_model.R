# Run the Offshore scallop JAGS model
# can run multiple banks at once

run_model <- function(banks, yr, export.tables, direct, direct_fns, direct_out, nickname, run.model = T, model.dat = NULL,
                      strt.mod.yr=1986, nchains = 8,niter = 175000, nburn = 100000, nthin = 20,final.run = F,parallel = T,
                      make.diag.figs=T, make.update.figs=T, fig="screen", language="en",
                      jags.model = "Assessment_fns/Model/DDwSE3_jags.bug",seed = 123,parameters = NULL){
  
  require(R2jags) || stop("You need the R2jags package installed or this ain't gonna work")
  require(ggrepel)|| stop("You need the ggrepel package installed or this ain't gonna work")
  
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/projections.r",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/decision.r",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Model/post.plt.R",
              "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Model/exploit.plt.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/fit.plt.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/diag.plt.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Model/biomass.plt.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Fishery/fishery.dat.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Fishery/logs_and_fishery_data.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R",
              "https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/github_spatial_import.R"
    )
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
    for(fun in funs) 
    {
      download.file(fun,destfile = basename(fun))
      source(paste0(getwd(),"/",basename(fun)))
      file.remove(paste0(getwd(),"/",basename(fun)))
    }
  } else { 
    source(paste(direct_fns,"Model/projections.r",sep=""))
    source(paste(direct_fns,"Model/decision.r",sep=""))
    source(paste(direct_fns,"Model/post.plt.R",sep=""))
    source(paste(direct_fns,"Model/exploit.plt.r",sep=""))
    source(paste(direct_fns,"Model/fit.plt.R",sep=""))
    source(paste(direct_fns,"Model/diag.plt.R",sep=""))
    source(paste(direct_fns,"Model/biomass.plt.r",sep=""))
    source(paste(direct_fns,"Maps/pectinid_projector_sf.r",sep=""))
    source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep=""))
    source(paste(direct_fns,"Fishery/fishery.dat.R",sep=""))
    source(paste(direct_fns,"Maps/github_spatial_import.r",sep=""))
  }
  
  #Initialize variables...
  DD.out <- NULL
  DD.lst <- NULL
  DDpriors <- NULL
  D.tab <- NULL
  yrs <- NULL
  proj.catch <- NULL
  URP <- NULL
  LRP <- NULL
  DD.dat <- NULL
  proj <- NULL
  D_low <- NULL
  D_high <- NULL
  TACi <- NULL
  mod.out <- NULL
  
  # Get the number of banks we are running across.
  
  num.banks <- length(banks)
  
  for(j in 1:num.banks)
  {
    # pick the bank
    bnk = banks[j]
    
    if(is.null(nickname)) load(file=paste(direct,"Data/Model/",(yr+1),"/", bnk, "/Model_input.RData",sep=""))
    if(!is.null(nickname)) load(file=paste(direct,"Data/Model/",(yr+1),"/", bnk, "/Model_input_", nickname, ".RData",sep=""))
    
    # If we are running a sub-area we need to make sure we have the correct bank to pull the data from.
    master.bank <-ifelse(grepl("GBa",bnk[j])==T , "GBa","BBn")
    # We need to create the working directories needed to save the results...
    # Get the plot directory
    if(is.null(nickname)) plotsGo <- paste(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/",sep="")
    if(!is.null(nickname)) plotsGo <- paste(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/", nickname, "/",sep="")
    # If the above directory does not exist then we need to create it and we will also need to create a similar data directory
    # Based on the assumption that directory doesn't exist either.
    if(dir.exists(plotsGo)==F)
    {
      # This enables us to create the base specified directory on up...
      if(dir.exists(direct) ==F) dir.create(direct)
      if(dir.exists(paste0(direct,(yr+1))) ==F) dir.create(paste0(direct,(yr+1)))
      if(dir.exists(paste0(direct,(yr+1),"/Updates")) ==F) dir.create(paste0(direct,(yr+1),"/Updates"))
      if(dir.exists(paste0(direct,(yr+1),"/Updates/",bnk)) ==F) dir.create(paste0(direct,(yr+1),"/Updates/",bnk))
      #dir.create(paste0(direct,(yr+1),"/Updates/Figures_and_tables/"))
      if(dir.exists(paste0(direct,(yr+1),"/Updates/",bnk, "/Figures_and_tables")) ==F) dir.create(paste0(direct,(yr+1),"/Updates/",bnk,"/Figures_and_tables/"))
      if(!plotsGo==paste0(direct,(yr+1),"/Updates/",bnk, "/Figures_and_tables")) dir.create(plotsGo)
      # Similarly I need to make sure we have the data directories
      if(dir.exists(paste0(direct,"Data")) ==F) dir.create(paste0(direct,"Data"))
      if(dir.exists(paste0(direct,"Data/Model")) ==F) dir.create(paste0(direct,"Data/Model"))
      if(dir.exists(paste0(direct,"Data/Model/",(yr+1))) ==F) dir.create(paste0(direct,"Data/Model/",(yr+1)))
      if(dir.exists(paste0(direct,"Data/Model/",(yr+1),"/",bnk)) ==F) dir.create(paste0(direct,"Data/Model/",(yr+1),"/",bnk))
      if(dir.exists(paste0(direct,"Data/Model/",(yr+1),"/",bnk, "/Results")) ==F) dir.create(paste0(direct,"Data/Model/",(yr+1),"/",bnk,"/Results"))
      if(!is.null(nickname) & dir.exists(paste0(direct,"Data/Model/",(yr+1),"/",bnk, "/Results/", nickname))==F) dir.create(paste0(direct,"Data/Model/",(yr+1),"/",bnk,"/Results/", nickname))
    } # end if(dir.exists(plot.dir)==F)
    
    
    #Read2 Get the managment data, note this file needs updated annually! 
    cat(paste("*NOTE #1* Please ensure the file Ref_pts_and_tac.csv is updated with the interim TAC (from the Interim Fishing Plan sent after OSAC) or you won't have information for the", 
              yr+1,"prediction, wouldn't you feel silly without that.\n",sep = " "))
    manage.dat <- read.csv(file=paste(direct,"Data/Model/Ref_pts_and_tac.csv",sep=""))
    # If you need to run the model giver.
    # There is a possiblitly that some of the sub-area values will be 0's, for those cases I'm going to use the value from the 
    # following year as the value and spit out a warning
    if(any(mod.dat[[bnk]] == 0))
    {
      # Here are the columns that have 0's in them, there must be a better way to do this, but we also want to
      # go from most recent to oldest just in case we have back to back years with 0's...
      rows.of.interest.survey <- unique(unlist(unique(apply(mod.dat[[bnk]],2,FUN = function(x) which(x == 0)))))
      rows.of.interest.catch <- unique(unlist(unique(apply(mod.dat[[bnk]],2,FUN = function(x) which(is.na(x))))))
      rows.of.interest <- unique(rev(sort(c(rows.of.interest.catch,rows.of.interest.survey))))
      for(p in 1:length(rows.of.interest)) 
      {
        tmp <- mod.dat[[bnk]][rows.of.interest[p],]
        colms <- c(which(tmp==0),which(is.na(tmp)))
        mod.dat[[bnk]][rows.of.interest[p],colms] <- mod.dat[[bnk]][(rows.of.interest[p]+1),colms]
        # Also can have trouble if only 1 fishing trip...
      } # end for(p in 1:length(rows.of.interst)) 
      cat(paste("WHOA, check this out, for",bnk," we had to replace some years that had 0's in them with non-zero values, check your data and the script
            for details if this is news to you!! \n"))    
    } # end if(any(mod.dat[[bnk]] == 0)
    
    if(run.model==T){
      
      # Grab the data, start model at either 1986 (note that BBn data starts in 1991 so anything earlier will default to 1991)
      DD.dat <- subset(mod.dat[[bnk]],year %in% strt.mod.yr:max(mod.dat[[bnk]]$year),
                       select = c("year","n.x","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                                  "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS",  "RS","catch","effort","n.y","cpue",
                                  "cpue.var","cpue.se","LCI","UCI","U.cv", "g","g2","gR","gR2"))
      
      names(DD.dat) <- c( "year","n","I","I.cv","IR",  "IR.cv", "IPR", "IPR.cv","N","N.cv","NR","NR.cv", "NPR", "NPR.cv",
                          "w.bar","l.bar", "l.k", "w.k","CF","clappers","clappersR","CS",  "RS","C","E","n.trips","U",
                          "U.var","U.se","LCI","UCI","U.cv", "g","g2","gR","gR2") 
      # Organize the data and set up the model priors/initialization data, then run the model.
      yrs[[bnk]]<-min(DD.dat$year):max(DD.dat$year)
      NY<- length(yrs[[bnk]])
      DD.lst[[bnk]]<-as.list(subset(DD.dat,year %in% yrs[[bnk]],c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers",
                                                                  "clappersR","g2","gR2")))
      # DK NOTE: Downweight the CV for the CPUE data. This is done to be consistent with CV used
      # Previously in the model assessments. This has been flagged as an action item to investigate 
      # and resolve in the next framework.
      cat(paste0("*NOTE # 2* See code for message about the CPUE CV being downweighted artificially, this needs revised in next Framework as it ain't legit.\n"))
      ifelse(names(DD.lst[[bnk]])[9] == "U.se", names(DD.lst[[bnk]])[9] <- "U.cv", DD.lst[[bnk]]$U.cv <- DD.lst[[bnk]]$U.cv*50)
      # Also, if doing this we need to change the original data to represent what the model is seeing..
      # So if we used the SE let's replace the U.cv data with the U.se data, if we are doing the
      # 50x to match what we've done before than we need to change those data as well.
      ifelse(names(DD.lst[[bnk]])[9] == "U.se", DD.dat$U.cv <- DD.dat$U.se, DD.dat$U.cv <- DD.dat$U.cv*50)
      
      # Add a couple items to the DD.lst list...
      DD.lst[[bnk]]$NY<- length(DD.lst[[bnk]]$C)
      DD.lst[[bnk]]$year<-min(DD.dat$year):max(DD.dat$year)
      # Set up Priors.  This first bit is getting our variance correct for the CV's for Biomass, Recruit biomass, and catch rates.
      # This is then added to our list of priors to get them correct.
      # Biomass CV
      uI=log(DD.lst[[bnk]]$I.cv^2+1) # See Smith and Hubley 2014 for details, this is variance of the log of a CV
      # DK Note:  Smith/Hubley suggest this should be 3, so why we setting it to 2???
      Ip.a=2+(uI/uI)^2 # This is the alpha prior term for the prior (an inverse-gamma, i.e. gamma using 1/var); a rather funky way of setting alpha =2
      Ip.b=1/(uI*((uI/uI)^2+1)) # This is the beta term for the prior, again a strangly complex way of saying 1/(2*(uI))
      # Recruit biomass CV, see above comments for details.
      uIR=log(DD.lst[[bnk]]$IR.cv^2+1)
      IRp.a=2+(uIR/uIR)^2
      IRp.b=1/(uIR*((uIR/uIR)^2+1))
      # Catch Rate CV, see above comments for details.
      uU=log(DD.lst[[bnk]]$U.cv^2+1)
      Up.a=2+(uU/uU)^2
      Up.b=1/(uU*((uU/uU)^2+1))
      
      DDpriors[[bnk]]=list(
        logK=			    list(a=7,		  b=7,		d="dnorm",	l=1		),		# scaler to total biomass, a= mean  b = sd, this gives a huge range of starting values
        r=				    list(a=0, 		b=1,		d="dlnorm",	l=NY	),		# scaled recruit biomass, a= meanlog  b = sdlog
        m=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality fully recruited a= meanlog  b = sdlog
        mR=				    list(a=-2,		b=2,		d="dlnorm",	l=NY	),		# natural mortality  recruits a= meanlog  b = sdlog
        S=				    list(a=8, 		b=11,		d="dbeta",  l=1		),		# clapper dissolution rate a= shape1, b=shape2, 8 & 11 gives ~ normal mean of .45ish
        q=				    list(a=20, 		b=40,		d="dbeta",	l=1		),		# survey catchability fully recruited a= shape1, b=shape2
        qU=				    list(a=0,		  b=1,	  d="dunif",	l=1		),		# fishery catchability CPUE a= min, b = max
        sigma=			  list(a=0, 		b=5,		d="dunif",	l=1		),		# process error (SD) a = min, b = max
        ikappa.tau2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error FR clappers  a = shape, b = scale (1/rate)
        ikappa.rho2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	# measurement error recruit clappers a = shape, b = scale (1/rate)
        I.precision=	list(a=Ip.a,	b=Ip.b,	d="dgamma",	l=NY	),		# measurement error variance survey FR a = shape, b = scale (1/rate)
        IR.precision=	list(a=IRp.a,	b=IRp.b,d="dgamma",	l=NY	),		# measurement error variance survey recruits a = shape, b = scale (1/rate)
        U.precision=	list(a=Up.a,	b=Up.b,	d="dgamma",	l=NY	)		  # measurement error variance CPUE  a = shape, b = scale
      )
      
      #Prepare priors for JAGS
      for(h in 1:length(DDpriors[[bnk]]))
      {
        # Get the variances for log-normal and normal converted to precisions, note that in BUGS language the precision is
        # the inverse of the squared standard deviation (which is what you specify in R).  The standard deviation is what
        # was specified in the Prior list (as it is more intuitive)
        if(DDpriors[[bnk]][[h]]$d%in%c("dlnorm","dnorm")) DDpriors[[bnk]][[h]]$b <- 1/DDpriors[[bnk]][[h]]$b^2
        # For a Gamma to convert to precision the precision term is  the inverse of the 'Scale" term in a typical 
        # gamma distribution parameterization, aka this is now knonwn as the rate.
        # Happily this is the same as the parameterization in R dgamma(x,shape,rate) so our b parameter is correct for posterior plots.
        if(DDpriors[[bnk]][[h]]$d=="dgamma")DDpriors[[bnk]][[h]]$b<-1/DDpriors[[bnk]][[h]]$b
      } # end for(h in 1:length(DDpriors[[bnk]]))
      # Made a data.frame of the priors, unwrap the list and combine by row.
      prior.dat<- data.frame(par=names(DDpriors[[bnk]]),do.call("rbind",lapply(DDpriors[[bnk]],rbind)))
      prior.lst<-list()
      # Now turn this into a list
      for(k in seq(1,nrow(prior.dat)*2,2))
      {
        prior.lst[[k]]<-prior.dat$a[[ceiling(k/2)]]
        prior.lst[[k+1]]<-prior.dat$b[[ceiling(k/2)]]
      } # end for(k in seq(1,nrow(prior.dat)*2,2))
      # And give the list names
      names(prior.lst)<-paste(rep(prior.dat$par,2)[order(rep(1:nrow(prior.dat),2))],rep(c('a','b'),nrow(prior.dat)),sep='.')
      
      # Now if they haven't already been selected grab the parameters you want for the model.
      ifelse(is.null(parameters) == T, parameters <- c(names(DDpriors[[bnk]]),'K','P','B','R','mu','Imed','Ipred','Irep', 'IRmed','IRpred','IRrep',
                                                       "Cmed","Crep","CRmed","CRrep",'sIresid','sIRresid','sPresid','Iresid',
                                                       'IRresid','Presid',"Cresid","CRresid","sCresid","sCRresid"),parameters)
      # Run the model and see how long it takes.
      # n = 400,000 and burn = 100,000, thin = 20 with 2 chains do not decrease these as retaining this much
      # data is needed to stabilize the projections, it does lengthen the run time to 10-20 minutes in serial
      # Running in parallel stick with that burn in but we can get away with n=200,000, burn = 100,000, thin = 20, and 6 chains
      # they are longer chains than really are needed for the model to converge, but this is really being done just for the projections.
      # Run the model now.
      start<-Sys.time()
      ## Call to JAGS, do you want to run in parallel?
      
      if(parallel==F)
      {
        out <- jags(data =  c(prior.lst,DD.lst[[bnk]]), inits = NULL,parameters.to.save = parameters,  
                    model.file = paste(direct,jags.model,sep=""),n.chains = nchains, n.iter = niter, n.burnin = nburn, 
                    n.thin = nthin)
      }
      
      if(parallel==T)
      {
        out <- jags.parallel(data =  c(prior.lst,DD.lst[[bnk]]), inits = NULL,parameters.to.save = parameters,  
                             model.file = paste(direct,jags.model,sep=""),n.chains = nchains, n.iter = niter, n.burnin = nburn, 
                             n.thin = nthin,jags.seed = seed)
      }
      # How long did that take?
      print(Sys.time()-start)
      
      # Rename the output so I retain the results 
      DD.out[[bnk]] <- list(data=c(prior.lst,DD.lst[[bnk]],yrs[[bnk]]), sims.list=out$BUGSoutput$sims.list,median=out$BUGSoutput$median,
                            mean=out$BUGSoutput$mean,summary=out$BUGSoutput$summary,priors = prior.lst,parameters=parameters)
      
      # I will also retain the MCMC object produced in case I want it for something.
      mod.out[[bnk]] <- out
      
      #source("fn/projections.r")
      # The catch since the survey for the most recent year is this, if there was no catch set this to 0.
      proj.catch[[bnk]] <- max(proj.dat[[bnk]]$catch[proj.dat[[bnk]]$year == max(DD.dat$year)],0)
      # Get the low and upper boundaries for the decision table (this might be a silly way to do this...)
      if(bnk %in% c("GBa","BBn"))
      {
        D_low[[bnk]] <- subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$D_tab_low
        D_high[[bnk]] <-  subset(manage.dat,year==(max(DD.dat$year)+1) & bank == bnk)$D_tab_high
      } # end if(bnk %in% c("GBa","BBn"))
      # If we are looking at one of the sub-areas we will go for 1/3 of the mean biomass estimate for the current year...
      if(!bnk %in% c("GBa","BBn")) {D_low[[bnk]] <- 0; D_high[[bnk]] <- out$BUGSoutput$mean$B[length(out$BUGSoutput$mean$B)]/3}
      # The increment size for the decision table.  500 for GBa and 50 for BBn
      step <- ifelse(bnk == "GBa", 500,50)
      # The URP and LRP for the bank, for the moment only GBa has been accepted so it's the only one used.
      # For more info on GBa reference points: see Y:\Offshore\Assessment\Non-Github archive and documentation\Help and Documentation\GBa Reference Points Literature Review.docx 
      if(bnk %in% c("GBa","BBn"))
      {
        refyears <- which(DD.out[[bnk]]$dat$year %in% 1986:2009)
        LRP[[bnk]] <- mean(DD.out[[bnk]]$median$B[refyears]) * 0.3 
        URP[[bnk]] <- mean(DD.out[[bnk]]$median$B[refyears]) * 0.8 
      } # end if(bnk %in% c("GBa","BBn"))
      
      # For the sub-areas just make these NA.
      if(!bnk %in% c("GBa","BBn")) {URP[[bnk]] <- NA; LRP[[bnk]]<- NA}
      
      # Get the projection scenarios of interest
      if(length(proj.catch[[bnk]]) > 0) proj[[bnk]] <- seq(D_low[[bnk]],D_high[[bnk]],step) + proj.catch[[bnk]]
      # If we don't have projected catch data yet (i.e. I'm running the model before the logs have data in them..)
      if(length(proj.catch[[bnk]]) == 0) 
      {
        # Set projected catch to 0
        proj.catch[[bnk]] <- 0
        proj[[bnk]] <- seq(D_low[[bnk]],D_high[[bnk]],step) + proj.catch[[bnk]]
        writeLines("YO YO LOOK HERE!!  The projected catch used in this model is 0, this should only happen in preliminary runs!!")
      }
      # The interim TAC is known for GBa and BBn,
      if(bnk %in% c("GBa","BBn")) TACi[[bnk]] <- subset(manage.dat,year== (max(DD.dat$year)+1) & bank == bnk)$TAC
      # For the sub-areas let's just make this last years catch from the area, not perfect but should be reasonable
      if(!bnk %in% c("GBa","BBn")) TACi[[bnk]] <- DD.lst[[bnk]]$C[DD.lst[[bnk]]$NY]
      
      # Now do the projections
      DD.out[[bnk]]<- projections(DD.out[[bnk]],C.p=proj[[bnk]]) # C.p = potential catches in decision table
      
      ### Generate Decision Table ###
      ### Note that from the 2015 SSR we have these definitely set at...
      #Georges Bank 'a' reference points are based on 30% and 80% of the mean biomass from 1986 to 2009. 
      #The Lower Reference Point (LRP) is 7,137 t and the Upper Stock Reference (USR) is 13,284 t.
      
      if (bnk == "GBa") 
      {
        D.tab[[bnk]]<-decision(DD.out[[bnk]],bnk, mu=0.15,refs=c(URP[[bnk]],LRP[[bnk]]),post.survey.C=proj.catch[[bnk]], yr=yr)
        if (export.tables == T) write.csv(D.tab[[bnk]],paste0(plotsGo,"Decision_GBa.csv",sep=""),row.names=F) #Write1
        
      } # END if(bnk == "GBa")
      
      # Now Browns North or the sub areas
      if (bnk != "GBa") 
      {
        D.tab[[bnk]]<-decision(DD.out[[bnk]],bnk, mu=0.15,post.survey.C=proj.catch[[bnk]], yr=yr)
        if (export.tables == T) write.csv(D.tab[[bnk]],paste0(plotsGo,"Decision_",bnk,".csv",sep=""),row.names=F) #Write2
      } # END if(bnk == "BBn")
      
      # For i = 1 this will just get the first bank, unfortunately if i =2 then this will pull in results for both 
      #  (if running this as a loop) which is silly, but work arounds are dumber than this solution
      # If you are happy and want to keep these results 
      
      if(final.run == T) 
      {
        save(DD.lst, DDpriors,DD.out,DD.dat,mod.out,mod.dat,cpue.dat,proj.dat,yr,D.tab,manage.dat,proj.catch,
             URP,LRP,proj,bnk,TACi,yrs,j,
             file=paste(direct_out,"Data/Model/",(yr+1),"/",bnk,"/Results/Final_model_results.RData",sep=""))
      } # end if(final.run == T) 
      # If you are still testing results the model will save here, 
      if(final.run == F && is.null(nickname))
      {
        save(DD.lst, DDpriors,DD.out,DD.dat,mod.out,mod.dat,cpue.dat,proj.dat,yr,D.tab,manage.dat,proj.catch,
             URP,LRP,proj,bnk,TACi,yrs,j,
             file=paste(direct_out,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_testing_results.RData",sep=""))
      } # if(final.run == F) 
      if(final.run == F && !is.null(nickname))
      {
        save(DD.lst, DDpriors,DD.out,DD.dat,mod.out,mod.dat,cpue.dat,proj.dat,yr,D.tab,manage.dat,proj.catch,
             URP,LRP,proj,bnk,TACi,yrs,j,
             file=paste(direct_out,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_testing_results_", nickname, ".RData",sep=""))
      } # if(final.run == F) 
      
      print("done running model. Results saved in Data/Model/year/bank/Results/")
    }
    
    if(run.model==F) {
      load(model.dat)
    }
        
    ##################################################################################
    ################### run model diagnostics
    ##################################################################################
    if(make.diag.figs == T){
      # Initialize necessary variables
      mort <- NULL
      TACI <- NULL
      BM.proj.1yr <- NULL
      B.quantiles <- NULL
      percent.B.change <- NULL
      prob.below.USR <- NULL
      FR.bm <- NULL
      FR.ltm <- NULL
      rec.bm <- NULL
      rec.ltm <- NULL
      neff <- NULL
      rhat <- NULL
      
      # Some model outputs needed for the Update.  First the mortality
      mort[[bnk]] <- 1- exp(-DD.out[[bnk]]$mean$m[length(DD.out[[bnk]]$mean$m)])
      # This lines up the column headers with the projected catch...
      TACI[[bnk]]<- which(DD.out[[bnk]]$data$C.p==(TACi[[bnk]]+proj.catch[[bnk]]))
      # This get us the predicted biomass for next year based on the projected catch
      BM.proj.1yr[[bnk]] <- DD.out[[bnk]]$median$B.p[TACI[[bnk]]]
      # This is only useful for GBa at the moment since BBn doesn't have reference points accepted yet...
      if(bnk == "GBa")
      {
        # Get the quantiles, this likely would need changed, but which quantile is > our URP (13,284 as of 2015)
        B.quantiles[[bnk]] <- quantile(DD.out[[bnk]]$sims.list$B[,length(DD.out[[bnk]]$sims.list$B[1,])],probs=seq(0,1,0.01))
        # This is the probability (well percentage) that Biomass is below the USR
        prob.below.USR[[bnk]] <- names((which(B.quantiles[[bnk]] > URP[[bnk]])[1]))
      } # end if(bnk=="GBa")
      
      # Here we can grab the Fully recruited and recruit biomass for the last 2 years and the median of the time series.
      FR.bm[[bnk]] <- DD.out[[bnk]]$median$B[(length(DD.out[[bnk]]$mean$B)-1):length(DD.out[[bnk]]$median$B)]
      # We exclude the current year from the median estimate
      FR.ltm[[bnk]] <- median(DD.out[[bnk]]$median$B[-length(DD.out[[bnk]]$median$B)])
      # Recruit biomass
      rec.bm[[bnk]] <- DD.out[[bnk]]$median$R[(length(DD.out[[bnk]]$median$R)-1):length(DD.out[[bnk]]$median$R)]
      # We exclude the current year from the median estimate
      rec.ltm[[bnk]] <- median(DD.out[[bnk]]$median$R[-length(DD.out[[bnk]]$median$R)])
      
      # Get the percent biomass change from the projection. 0 means unchanged, + means % increase, - means % decline
      percent.B.change[[bnk]] <- (BM.proj.1yr[[bnk]] / DD.out[[bnk]]$median$B[length(DD.out[[bnk]]$median$B)]) -1
      
      ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS 
      ##### Now we can run some model diagnostics.
      # Some quick diagnoistics, the maximum should be < 1.05
      rhat[[bnk]] <- summary(DD.out[[bnk]]$summary[,8])
      
      # Effective number of observations.  
      #Not sure what our minimum should be here, but using the Rhat + looking at the chains should indicate where there are problems...
      neff[[bnk]] <- range(DD.out[[bnk]]$summary[,9])
      
      
      if(is.null(nickname)) save(mort,TACI,BM.proj.1yr,B.quantiles,percent.B.change,prob.below.USR,FR.bm,FR.ltm,rec.bm,rec.ltm,neff,rhat,
                                 file=paste(direct_out,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_results_and_diagnostics.RData",sep=""))
      if(!is.null(nickname)) save(mort,TACI,BM.proj.1yr,B.quantiles,percent.B.change,prob.below.USR,FR.bm,FR.ltm,rec.bm,rec.ltm,neff,rhat,
                                  file=paste(direct_out,"Data/Model/",(yr+1),"/",bnk,"/Results/Model_results_and_diagnostics_", nickname, ".RData",sep=""))
      
      
      print("done running diagnostics")
      
      ##################### END Section 3 Model Diagnostics #####################  END Section 3 Model Diagnostics #################  
      ##################### END Section 3 Model Diagnostics #####################  END Section 3 Model Diagnostics #################  
      
      
      #################  SECTION 4 Figures #################  SECTION 4 Figures #################  SECTION 4 Figures #################  
      #################  SECTION 4 Figures #################  SECTION 4 Figures #################  SECTION 4 Figures #################  
      # Now if I want to make the figures both from the model output and for the update document do these...
      
      #####Plot model diagnostics############## 
      # These plots include the posterior fits, exploitation estimate, Biomass fit to survey and CPUE, residual plot
      # and the model convergence plot (which is a 700+ page pdf of the convergence of each parameter + it's ACF.)
      # posterior densities for model parameters
      post.plt(DD.out[[bnk]],DDpriors[[bnk]],years=yrs[[bnk]], graphic=fig,multi=T,path=plotsGo)
      #dev.off()
      #exploitaiton time series
      exploit.plt(DD.out[[bnk]], years=yrs[[bnk]], plt=c('f','m','mR'),graphic=fig,path=plotsGo)
      #dev.off()
      
      # for 2020, we have to insert NAs because of COVID non-survey
      DD.plt <- DD.out[[bnk]]
      DD.plt$median$B[which(yrs[[bnk]]==2020)] <- NA
      DD.plt$sims.list$B[,which(yrs[[bnk]]==2020)] <- NA
      DD.plt$median$R[which(yrs[[bnk]]==2020)] <- NA
      DD.plt$sims.list$R[,which(yrs[[bnk]]==2020)] <- NA
      DD.plt$data$I[which(yrs[[bnk]]==2020)] <- NA
      DD.plt$data$IR[which(yrs[[bnk]]==2020)] <- NA
      
      # model biomass fit to survey
      fit.plt(DD.plt, years = yrs[[bnk]], CI=T,graphic=fig,path=plotsGo,CV=T, language=language)
      # diagnostic plot
      diag.plt(DD.out[[bnk]], years = yrs[[bnk]],graphic=fig,path=plotsGo)
      
      # Here we pull together all of the chains and look to see that they are both well mixed and that
      # there is no correlation.   This is a complete crap load of plots!!
      # Function to plot all the chains.
      # Get the bank pulled out and figure out how many parameters we have
      num.param <- length(names(DD.out[[bnk]]$sims.list))
      param.names <- names(DD.out[[bnk]]$sims.list)
      # Make the pdf, given the number of parameters in the model you don't get an option for making this plot print to screen
      # if you run diagnostics you get this pdf
      
      # Set the number of rows
      nr <- ceiling(sqrt(nchains))
      # Set the number of columns, the funky little command is used to add one to the nc if nchains is a perfect square
      # As we are printing nchains + 1
      ifelse(sqrt(nchains)%%1==0,  nc <- ceiling(sqrt(nchains))+1, nc <- ceiling(sqrt(nchains)))
      # I always force this to make a pdf because it is a bazillion pages long...
      pdf(file=paste(plotsGo,"Model_convergence.pdf",sep=""),onefile=T)
      # Set up the plotting device.
      par(mfrow = c(nr,nc),mar=c(2,2,3,1))
      for(i in 1:num.param)
      {
        # This pulls out all the plot for parameters with only one value
        if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])==T)
        {
          # Since our all of these parameters are hyperparameter (there isn't one for every year) this works, 
          #if this was a matrix we'd get an error here.
          len <- length(DD.out[[bnk]]$sims.list[[param.names[i]]])
          # This sets us up to pull out the right "chains" from the data using the k loop below.
          bins <- seq(1,len,by = len/nchains)
          # Get the ylimits for the plot.
          ylims <- range(DD.out[[bnk]]$sims.list[[param.names[i]]])
          colr <- rainbow(nchains) # set up a color ramp
          count <- 0 # Set up a counter
          # I need to run this loop twice, once for the first figure so we get all the lines added and a second time to 
          # add in the ACF's.  Probably a nicer way to do this, but it'll do...
          # Now run the loop and make the figure showing the mixing of the chains
          for(k in bins)
          {
            count <- count+1 # used for the color ramp
            # Get the data
            dat <-DD.out[[bnk]]$sims.list[[param.names[i]]][k:(k+len/nchains-1)] 
            if(k ==1) plot(dat,type="l",col=colr[count], main = paste(param.names[i], "Chain"),xlab="",ylab="",ylim=ylims)
            if(k > 1) lines(dat,col=colr[count])
          } # end for(k in bins)
          
          # Now to make the ACF figures
          count  <- 0 # Reset the counter
          for(k in bins)
          {
            count <- count+1 # used to ID the chain
            # Pick it up from here.
            dat <-DD.out[[bnk]]$sims.list[[param.names[i]]][k:(k+len/nchains-1)] 
            # And look for any signs of autocorrelation in the chains...
            acf(dat,lag.max = 10,main = paste("ACF chain",count),xlab="",ylab="",ylim=c(0,0.3))
          }# end for(k in bins)
          
        } # end if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[1]]])==T)
        
        # This pulls out all the plots for parameters with multiple values (i.e. annual estimates)
        if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])==F)
        {
          num.reps <- ncol(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])
          rep.names <- paste(names(DD.out[[bnk]]$sims.list)[i],1:num.reps,sep="_")
          # Run this loop for each chain for these parameters.
          for(p in 1:num.reps)
          {
            # Get the length again (this could probably be tidied up as this number has to be the same for all parameters.)
            len <- length(DD.out[[bnk]]$sims.list[[param.names[i]]][,p])
            # Get the bins for the loop
            bins <- seq(1,len,by = len/nchains)
            # Get the ylimits for the plot.
            ylims <- range(DD.out[[bnk]]$sims.list[[param.names[i]]][,p])
            colr <- rainbow(nchains) # set up a color ramp
            count <- 0 # Set up a counter
            # Set up the plotting device.
            #par(mfrow = c(nr,nc),mar=c(2,2,3,1))
            # I need to run this loop twice, once for the first figure so we get all the lines added and a second time to 
            # add in the ACF's.  Probably a nicer way to do this, but it'll do...
            # Now run the loop and make the figure showing the mixing of the chains
            for(k in bins)
            {
              count <- count+1 # used for the color ramp
              # Get the data
              dat <-DD.out[[bnk]]$sims.list[[param.names[i]]][k:(k+len/nchains-1),p]
              if(k ==1)  plot(dat,type="l",col=colr[count], main = paste(rep.names[p], "Chain"),xlab="",ylab="",ylim=ylims)
              if(k > 1) lines(dat,col=colr[count])
            } # end for(k in bins)
            count <- 0 # Reset the counter
            # And now for the ACF figures
            for(k in bins)
            {
              count <- count+1 # used for the chain index
              # Get the data
              dat <-DD.out[[bnk]]$sims.list[[param.names[i]]][k:(k+len/nchains-1),p]
              # And look for any signs of autocorrelation in the chains...
              acf(dat,lag.max = 10,main = paste("ACF chain ",count),xlab="",ylab="",ylim=c(0,0.3))
            } # end for(k in bins)
          }# end for(p in 1:num.reps)
        } # end if(is.vector(DD.out[[bnk]]$sims.list[[names(DD.out[[bnk]]$sims.list)[i]]])==F)
      }  # end for(i in 1:num.param)
      dev.off()
    }
    
    ##################################################################################
    ################## run update figures
    ##################################################################################
    if(make.update.figs==T){
      
      if(bnk == "GBa") bm.max <- NULL
      if(bnk == "BBn") bm.max <- 25000
      
      # # for 2020, we have to insert NAs because of COVID non-survey
      DD.plt <- DD.out[[bnk]]
      # DD.plt$median$B[which(yrs[[bnk]]==2020)] <- NA
      # DD.plt$sims.list$B[,which(yrs[[bnk]]==2020)] <- NA
      # DD.plt$median$R[which(yrs[[bnk]]==2020)] <- NA
      # DD.plt$sims.list$R[,which(yrs[[bnk]]==2020)] <- NA
      # DD.plt$data$I[which(yrs[[bnk]]==2020)] <- NA
      # DD.plt$data$IR[which(yrs[[bnk]]==2020)] <- NA
      
      # Now make the biomass plots for the areas as necessary
      if(bnk != "GBa")
      {
        # If it's BBn, we have a y-axis maximum that we want to use (bm.max)
        if(bnk=="BBn") biomass.plt(DD.plt,years=yrs[[bnk]], graphic=fig,TAC=TACi[[bnk]]+proj.catch[[bnk]],path=plotsGo,refs=NULL,pred=1,
                                   URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median,Bymax=bm.max, language=language)
        # If it's a GBa subarea (i.e. not BBn and not GBa), we rely on biomass.plt to assign the y-axis maximum based on the upper credible limit,
        # we also don't have a TAC for the subareas
        if(bnk!= "BBn") biomass.plt(DD.plt,years=yrs[[bnk]], graphic=fig,TAC=NULL,path=plotsGo,refs=NULL,pred=1,
                                    URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median, Bymax=NULL, language=language)
      } # end if(bnk == "BBn")
      
      if(bnk == "GBa")
      {
        biomass.plt(DD.plt,years=yrs[[bnk]], graphic=fig,TAC=TACi[[bnk]]+proj.catch[[bnk]],path=plotsGo,refs = c("LRP","URP","zones"),pred=1,
                    URP =URP[[bnk]], LRP=LRP[[bnk]],avg.line=median,Bymax=bm.max, language=language)
      } # end if(bnk == "GBa")
      
      # Only make these figures for GBa or BBn
      if(bnk %in% c("GBa","BBn"))
      {
        #  Now we transition to produce the figures used in the Update document that are not dependent on model output.
        # First up we need the fishery data and TAC here, we don't actually have the calendar year fishery data
        # anywhere at this point so we grab that
        if(!missing(direct_fns)) logs_and_fish(loc="offshore",year = 1998:max(mod.dat[[bnk]]$year),un=un,pw=pw,db.con=db.con,direct=direct, direct_fns=direct_fns)
        if(missing(direct_fns)) logs_and_fish(loc="offshore",year = 1998:max(mod.dat[[bnk]]$year),un=un,pw=pw,db.con=db.con,direct=direct)
        # If you get any NA's related warnings it may be something is being treated as a Factor in one of the two files.
        # This should combine without any warnings so don't ignore warnings here.
        fish.dat<-merge(new.log.dat,old.log.dat,all=T)
        fish.dat$ID<-1:nrow(fish.dat)
        # Being lazy we get the data for each bank We are just looking for the annual values here so nothing fancy needed...
        if(!missing(direct_fns))
        {
          dat <- fishery.dat(fish.dat,bk=bnk,yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct,direct_fns=direct_fns, period = "calyr")
          if(bnk=="GBa")dat1<-fishery.dat(fish.dat,bk="GBb",yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct,direct_fns=direct_fns, period = "calyr")
        }
        
        if(missing(direct_fns))
        {
          dat <- fishery.dat(fish.dat,bk=bnk,yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct, period = "calyr")
          if(bnk=="GBa")dat1<-fishery.dat(fish.dat,bk="GBb",yr=1998:max(mod.dat[[bnk]]$year),method='jackknife',direct=direct, period = "calyr")
        }
        
        if(fig== "screen") windows(8.5,8.5)
        if(fig == "pdf") pdf(paste(plotsGo,"TAC_landings.pdf",sep=""),width=8.5,height=8.5)
        if(fig == "png") png(paste(plotsGo,"TAC_landings.png",sep=""),width=8.5,height=8.5,res=920,units="in")
        # Here are the time series on Georges Bank
        if(bnk == "GBa") par(mfrow=c(2,1),cex=1.2,mar=c(2,5,1,1))
        if(bnk != "GBa") par(mfrow=c(1,1),cex=1.2,mar=c(2,5,1,1))
        plot(dat$catch~dat$year,type="n",ylab="",xlab="",las=1,xaxt="n",bty="n",ylim=c(0,max(dat$catch*1.1,na.rm=T)))
        axis(1,pos=0)
        abline(h=0)
        points(dat$catch~dat$year,  type='h',pch=15,lwd=16,lend=3,col="grey50")
        lines(subset(manage.dat,year %in% dat$year & bank ==bnk)$TAC~dat$year,lwd=2,col="blue")
        if(bnk == "GBa" & language=="en") legend("topright","TAC",title="Georges Bank A",bty="n",col="blue",lwd=2)
        if(bnk == "GBa" & language=="fr") legend("topright","TAC",title="Georges \u00ABA\u00BB",bty="n",col="blue",lwd=2)
        if(bnk == "BBn" & language=="en")
        {
          legend("topright","TAC",title="Browns Bank North",bty="n",col="blue",lwd=2)
          mtext(side=2,"Landings (meat, t)",line=3.3,cex=1.5)
        }
        if(bnk == "BBn" & language=="fr")
        {
          legend("topright","TAC",title="Nord du banc de Brown",bty="n",col="blue",lwd=2)
          mtext(side=2,"D\u{E9}barquements (tonnes de chair)",line=3.3,cex=1.5)
        }
        
        
        # Now GBb
        if(bnk=="GBa" & language=="en")
        {
          plot(dat1$catch~dat1$year,type="n",ylab="",xlab="",las=1,xaxt="n",bty="n",ylim=c(0,2000))
          axis(1,pos=0)
          abline(h=0)
          points(dat1$catch~dat1$year,  type='h',pch=15,lwd=16,lend=3,col="grey50")
          lines(subset(manage.dat,year %in% dat1$year & bank =="GBb")$TAC~dat1$year,lwd=2,col="blue")
          legend("topright","TAC",title="Georges Bank B",bty="n",col="blue",lwd=2)
          mtext(side=2,"Landings (meat, t)",line=3.3,adj=2,cex=1.5)
        } # end if(bnk=")
        # Now GBb
        if(bnk=="GBa" & language=="fr")
        {
          plot(dat1$catch~dat1$year,type="n",ylab="",xlab="",las=1,xaxt="n",bty="n",ylim=c(0,2000))
          axis(1,pos=0)
          abline(h=0)
          points(dat1$catch~dat1$year,  type='h',pch=15,lwd=16,lend=3,col="grey50")
          lines(subset(manage.dat,year %in% dat1$year & bank =="GBb")$TAC~dat1$year,lwd=2,col="blue")
          legend("topright","TAC",title="Georges \u00ABB\u00BB",bty="n",col="blue",lwd=2)
          mtext(side=2, "D\u{E9}barquements (tonnes de chair)",line=3.3,adj=-9,cex=1.5)
        }
        # Turn off the plot device if making a pdf.
        if(fig!="screen") dev.off()
        
        #############  FINALLY I WANT TO MAKE AN OVERALL PLOT OF THE BANKS AND THAT WILL BE THAT...
        # Also make the overall plot of the banks...
        # set up the labels first
        
        labels <- github_spatial_import(subfolder = "other_boundaries/labels", zipname = "labels.zip")
        offshore <- github_spatial_import(subfolder = "offshore", zipname = "offshore.zip")
        
        labels <- labels[grepl('offshore_detailed',labels$region),]
        
        sfa.labels <- labels[grep(x=labels$lab_short, "SFA"),]
        
        sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="A ", replacement="A", fixed=T)
        sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern=" (", replacement="\n", fixed=T)
        sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern=")", replacement="", fixed=T)
        sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern=" Bank", replacement="", fixed=T)
        sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="-BAN", replacement="", fixed=T)
        sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="north", replacement="North", fixed=T)
        sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="south", replacement="South", fixed=T)
        sfa.labels$lab_short[sfa.labels$lab_short=="SFA27A"] <- "SFA27A\nGeorges 'a'"
        sfa.labels$lab_short[sfa.labels$lab_short=="SFA27B"] <- "SFA27B\nGeorges 'b'"
        sfa.labels <- sfa.labels[-grep(pattern = "Includes", x=sfa.labels$lab_short),]
        sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="26\nG", replacement="26C\nG", fixed=T)
        sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="26\nBrowns North", replacement="26A\nBrowns North", fixed=T)
        sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="26\nBrowns South", replacement="26B\nBrowns South", fixed=T)
        sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="25\nBa", replacement="25B\nBa", fixed=T)
        sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="25\nEa", replacement="25A\nEa", fixed=T)
        
        sfa.labels <- sfa.labels %>%
          tidyr::separate(lab_short, into=c("SFA", "bank"), sep="\n", remove=F)
        
        sf_use_s2(FALSE)
        joined <- st_join(offshore, sfa.labels)
        
        joined <- st_difference(joined[!(joined$bank=="Banquereau" & joined$ID.x=="Sab"),])
        joined$fr <- joined$lab_short
        joined$fr <- gsub(x=joined$fr, pattern="Browns South", replacement ="Sud de Brown")
        joined$fr <- gsub(x=joined$fr, pattern="Browns North", replacement ="Nord de Brown")
        joined$fr <- gsub(x=joined$fr, pattern="Georges 'a'", replacement ="Georges \u00ABa\u00BB")
        joined$fr <- gsub(x=joined$fr, pattern="Georges 'b'", replacement ="Georges \u00ABb\u00BB")
        joined$fr <- gsub(x=joined$fr, pattern="Eastern Scotian Shelf", replacement ="Est du plateau n\u00E9o-\u00E9cossais")
        
        
        nonGB <- joined[!joined$SFA %in% c("SFA27A", "SFA27B"),]
        GBalab <- joined[joined$SFA %in% c("SFA27A"),]
        GBblab <- joined[joined$SFA %in% c("SFA27B"),]
        
        p <-  pecjector(area = "NL", add_layer = list(land = 'grey',
                                                      eez = 'eez',
                                                      sfa='offshore'),c_sys = 4326, quiet=T)

        if(fig== "screen") windows(11,8.5)
        if(fig == "pdf") pdf(paste(plotsGo,"Offshore_banks.pdf",sep=""),width=13,height=11)
        if(fig == "png") png(paste(plotsGo,"Offshore_banks.png",sep=""),width=11,height=8,res=920,units="in")

        if(language=="en") {
          p <-  p + geom_sf_text(data = nonGB[!(nonGB$SFA%in% c("SFA11", "SFA26B")),], 
                                 aes(label = lab_short), 
                                 fun.geometry = sf::st_centroid) +
            geom_sf_text(data = nonGB[nonGB$SFA=="SFA11",], 
                         aes(label = lab_short), 
                         fun.geometry = sf::st_centroid, 
                         nudge_x=-0.5, nudge_y=-0.5) +
            geom_sf_text(data = nonGB[nonGB$SFA=="SFA26B",], 
                         aes(label = lab_short), 
                         fun.geometry = sf::st_centroid, 
                         nudge_y=0.5) +
            geom_text_repel(data = GBalab, 
                            aes(x=as.data.frame(st_coordinates(st_centroid(GBalab)))$X,
                                y=as.data.frame(st_coordinates(st_centroid(GBalab)))$Y,
                                label = lab_short), 
                            nudge_x=-1, nudge_y=-0.5, direction = "x", min.segment.length=0, box.padding = 0) +
            geom_text_repel(data = GBblab, 
                            aes(x=as.data.frame(st_coordinates(st_centroid(GBblab)))$X,
                                y=as.data.frame(st_coordinates(st_centroid(GBblab)))$Y,
                                label = lab_short), 
                            nudge_x=1.25, nudge_y=-0.5, direction = "x", min.segment.length=0, box.padding = 0) +
            coord_sf(expand=F) +
            scale_x_continuous(limits=c(-68, -54)) +
            #scale_y_continuous(limits=c(40, 48)) + 
            theme_bw() +
            xlab(NULL) + ylab(NULL)
        }
                
        if(language=="fr") {
          p <- p + geom_sf_text(data = nonGB[!(nonGB$SFA%in% c("SFA11", "SFA26B")),], 
                                aes(label = fr), 
                                fun.geometry = sf::st_centroid) +
            geom_sf_text(data = nonGB[nonGB$SFA=="SFA11",], 
                         aes(label = fr), 
                         fun.geometry = sf::st_centroid, 
                         nudge_x=-0.5, nudge_y=-0.5) +
            geom_sf_text(data = nonGB[nonGB$SFA=="SFA26B",], 
                         aes(label = fr), 
                         fun.geometry = sf::st_centroid, 
                         nudge_y=0.5) +
            geom_text_repel(data = GBalab, 
                            aes(x=as.data.frame(st_coordinates(st_centroid(GBalab)))$X,
                                y=as.data.frame(st_coordinates(st_centroid(GBalab)))$Y,
                                label = fr), 
                            nudge_x=-1, nudge_y=-0.5, direction = "x", min.segment.length=0, box.padding = 0) +
            geom_text_repel(data = GBblab, 
                            aes(x=as.data.frame(st_coordinates(st_centroid(GBblab)))$X,
                                y=as.data.frame(st_coordinates(st_centroid(GBblab)))$Y,
                                label = fr), 
                            nudge_x=1.25, nudge_y=-0.5, direction = "x", min.segment.length=0, box.padding = 0) +
            coord_sf(expand=F) +
            scale_x_continuous(limits=c(-68, -54)) +
            #scale_y_continuous(limits=c(40, 48)) + 
            theme_bw() +
            xlab(NULL) + ylab(NULL)
        }
        
        print(p)
        # Turn off the plot device if making a pdf.
        if(fig != "screen") dev.off()
      } # end if(bnk %in% c("GBa","BBn"))
      print("done making document figures")
    }
    
  }
}