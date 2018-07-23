####################################################################
## Delay difference model.  Necessary inputs are survey estimates for  biomass, abundance, and growth for recruits and commerical sized 
# individuals and the total catch for each year.
###################################################################
# Update history
# January 2016 - Revised by DK 
# April 2016, "Update" script has been overhauled and converted to a function called "Update_function_JAGS"
# May 16, 2016, updated to include options to allow different databases/usernames in the call to the fishery data.
# Sept 20, 2016, updated to allow for different loader files to be called in from the output of Survey_Summary_data.r. Removed spatial figures
# March 14, 2017 - Revision to the Scallop update function, using somewhat simplier DD model...
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
# logs_and_fishery_data.r
# fishery.dat.r
# projections.r
# decision.r
# post.plt.R
# exploit.plt.r
# fit.plt.R
# diag.plt.R
# peR_jags.r
# biomass.plt.r
# ScallopMap.r
# contour.gen.r 
##
###############################################################################################################


###############################################################################################################
# Arguments
# 1:  direct:                   The root directory to work from  Default = "d:/r/", 
# 2:  yr:                       The year of the survey used to generate update. Default = as.numeric(format(Sys.time(), "%Y"))-1 
# 3:  strt.mod.yr:              Start year for the models.  Set to 1986 but note that BBn doesn't have data back that far so starts in 1991.
# 4:  dat.type                  The type of data you will load, 1 of 3 options, point it to a "csv" flat file or an "RData" file 
#                               or load the data locally if you set to "workspace"  Default = "RData"
# 5:  dat                       The data to load, can be  a "csv" or "RData" file.  See previous, default is a "RData" file
#                               located here....paste(direct,"Data/Model_input.RData",sep="")
# 6:  fig:                      Print to 'pdf' or to screen.  default="screen":
# 7:  plot.loc                  If you are saving plots as pdf's where do you want to put them...

###########  Results options.  These options set up how the results from the model will be processed and influence our predictions

# 8:  proj.catch:                    The expected Total allowable catch next year, Biomass in Tonnes.  Default = 3000
# 9:  D_low:                         For the decsion table the lowest removal scemarop to use, in tonnes.  Default = 0
#10:  D_high:                        For the decsion table the lowest removal scenario to use, in tonnes.  Default = 0
#11:  URP:                           The URP for the bank, Default = 13284 tonnes (URP for GBa)
#12:  LRP:                           The LRP for the bank, Default = 7137 (LRP for GBa)
#13:  step:                          Increment size for the decision table   Default = 500 tonnes.

#14:  projection.paras:              If we want to supply our own parameters for the projection.  Default = NULL
#                                    which will simply use last years values.
#                                    You need to specify them in a data frame it should look something like 
#                                    projection.paras = data.frame(m = 0.2,mR =0.3, g= 1.2, gR = 1.5, r = 300)
#                                    Note that you only need to include the parameters you want to specify 
#                                    Note that m is the instaneous mortality, to convert to proportion  1-exp(-.m)
#                                    projection.paras = data.frame(g=1.2) would set the the commercial sized growth parameter to 1.2, 
#                                    all others would be based on the last year of data from the model run...
#                                    Mortality, growth, and recruit abundance are the only parameters you can alter.
#15: proj.uncert:                    Do you want your user specified projections to include uncertainty.
#                                    Options are 
#                                     a:  "NULL" (default) If you want your user specified parameters to be fixed values leave this as NULL. 
#                                     b:  "posterior" If you want your the uncertainty to be based on the model results.
#                                         This will grab the standard deviation of the last year of data for each parameter.
#                                     c:  "prior" If you'd like to use the uncertainty based "roughly" on the priors for the parameters 
#                                     d:  user specified uncertainty.  This should be a dataframe the same size as "proj.para.  Two examples
#                                         proj.uncert = data.frame(m = 0.05,mR =0.05, g= 0.1, gR = 0.1, r = 0.2)
#                                         proj.uncert = data.frame(g = 0.1)    
########################################################################################################################

###########  Model options.  By and large these options will only be used if run.mod=T
#16:  CPUE.mod                  If you have CPUE data run the CPUE model.
#17:  down.weight.CPUE          If you need to downweight the CPUE how much do you want to downweight it (Scallop model uses 50).  
#                               Default = NULL which is no downweighting
#18:  nchains:                  Number of chains to run.  Default = 8.  When running in parallel each chain gets it's own thread.
#                               The best way to get more saved replicates if the model has converged is to increase the number of chains run.
#19:  niter:                    Number of iterations to run.  Default = 175000
#20:  nburn:                    Number of initial iterations to ignore.  Default = 50000
#21:  nthin:                    Thinning rate of iterations.  Default = 20
#22:  para:                     Run in parallel using jags.parallel? T/F, default =T. Number of processors = nchains
#23:  jags.model:               Get the model to use (same model used in prediction evaluations).  By deFault it looks the the folder
#                               "direct"/Assessment_fns/Model/DDwSE3_jags.bug where direct was specified above.
#24:  parallel:                 Do you want to run JAGS in parallel.  (T/F), F will run JAGS but just using one core.
#25:  seed:                     If running JAGS in parallel you can set a "seed" so that your model results are reproducable.  Default = 123
#26:  parameters:               Model parameters to output.  Default = NULL which will produce all of the priors + the following parameters
#                               'K','P','B','R','mu','Imed','Ipred','Irep', 'IRmed','IRpred',
#                               'IRrep','sIresid','sIRresid','sPresid','Iresid','IRresid','Presid'
#27  export.tables:             Export the Decision tables, should only be done when satisified with results.  T/F, default = F
#28  convergence.check:         Do you want to run the convergence check. T/F, default = T.  The convergence check produces a pdf
#                               which shows the mixing and ACF for each of the parameters/chains in the model.
# These are parameters that you can vary to see how they influence our results.
#29:  M.priors:                 We can specify our priors for mortality of commercial and recruit "fish".
#                               default = data.frame(M.a=2,M.b=6,mR.a=2,MR.b=6)
#30:  q.prior:                  The catchability prior, this is modeled as a Beta distribution.  Default is NULL which 
#                               uses a prior of dbeta(20,40), which is a relatively tight prior centered at 0.33 (this is for the scallop model.)
#                               q.prior = data.frame(a=10,b=50) would give median q around 0.16, see scripts with "A Beta distribution moment" 
#                               to understand how to best parameterize these beta distribution priors.
#31 growth.var:                 Do you want to add growth variability into the model.  Default = NULL which is no variability.
#                               Suggested that this is around 0.1, but you can do whatever you'd like.  Just enter this as 1 number (i.e. growth.var = 0.9)
#32 growth.rate:                Do you want to change the growth rate used in the model  Default = NULL which used the default von B parameters.
#                               Simple multiplier so 0.9 would decrease growth by 10%, 1.1 would increase by 10%.  (i.e. growth.rate = 0.9)
#                               you cold also specify this to be a vector of the length of number of years and change this for each year.
#33 surv.var:                   Do you want to change the variability in the survey variance (specify both FR and Recruits). Default = NULL
#                               which used the survey variability. Simple multiplier so 0.9 would decrease growth by 10%, 1.1 would increase by 10%
#                               you will need to specify this for both FR and Recs so enter this as something like surv.var = c(1.1,1.1)
#######################################################################################################################################

###############################################################################################################

update_JAGS <- function(mod.dat = mod.dat, yr = as.numeric(format(Sys.time(), "%Y"))-1 , fig="screen",
                    # Where you want to put/look for your data, location is a combo of direct and the *.loc name
                    direct = "d:/r/", plot.loc = "2017/Framework/Process_error/Results/",
                    fun.loc = "Assessment_fns/Framework/2017/Process_error/",
                    dat.loc = "Data/Framework/2017/Process_error/",
                    jags.model = "models/DD_no_clappers.bug", # This needs to be located in a subfolder of fun.loc folder 
                    # Here is some info you'll need to specify by hand.
                    D_low = 0, D_high = 10000, step = 500, URP =13284, LRP =7137, 
                    # Options for projections and priors.
                    proj.catch = 3000,projection.paras = NULL ,proj.uncert= NULL,
                    q.prior = NULL,
                   # The output options
                   export.tables = F, 
                   # The main model options, these are only used if run.mod = T. (Tho the options starting with "n" could be sent to 
                   # be used with the prediction evaluation model runs if the "pe." options are not specified and run.pre.eval.model=T)
                   nchains = 8,niter = 175000, nburn = 100000, nthin = 20,parallel = T,strt.mod.yr = 1986,CPUE.mod =T,down.weight.CPUE =NULL,
                   seed = 123,parameters = NULL,convergence.check = T,
                   # Process error senssistivity analysis variables
                   m.priors = data.frame(M.a=2,M.b=6,MR.a=2,MR.b=6,year="all"),growth.var = NULL, growth.rate = NULL,surv.var = NULL)
{
  
  # The location of all the data/output
  plot.loc <- paste(direct,plot.loc,sep="") # This is figures/tables.
  fun.loc <- paste(direct,fun.loc,sep="")
  dat.loc <- paste(direct,dat.loc,sep="")
  jags.model = paste(fun.loc,jags.model,sep="")
  
# Load in the functions needed for this function to run.
source(paste(fun.loc,"functions/projections.r",sep=""))
source(paste(fun.loc,"functions/decision.r",sep=""))
source(paste(fun.loc,"functions/post.plt.R",sep=""))
source(paste(fun.loc,"functions/exploit.plt.r",sep=""))
source(paste(fun.loc,"functions/fit.plt.R",sep=""))
source(paste(fun.loc,"functions/diag.plt.R",sep=""))
source(paste(fun.loc,"functions/biomass.plt.r",sep=""))
# The necesary library
library("R2jags")


#############  Section 1 Model#############  Section 1 Model#############  Section 1 Model#############  Section 1 Model ###########
#############  Section 1 Model#############  Section 1 Model#############  Section 1 Model#############  Section 1 Model ###########
#############  Section 1 Model#############  Section 1 Model#############  Section 1 Model#############  Section 1 Model ###########
# Load the data 


    # Grab the data, start model at either 1986 (note that BBn data starts in 1991 so anything earlier will default to 1991)
    if(CPUE.mod == T) # If you have CPUE data...
    {
      DD.dat <- subset(mod.dat,year %in% strt.mod.yr:yr,select = 
                       c("I","I.cv","IR","IR.cv","N","N.cv","NR","NR.cv","clappers","clappersR","g","gR","catch","cpue","U.cv","g.se","gR.se","year"))
      names(DD.dat) <- c("I","I.cv","IR","IR.cv","N","N.cv","NR","NR.cv","clappers","clappersR","G","GR","C","U","U.cv","G.se","GR.se","year") # just making catch turn into "C".
      if(!is.null(down.weight.CPUE))  DD.dat$U.cv <- DD.dat$U.cv*down.weight.CPUE # For scallop fishery we need to downweight the CV
      # 
    } # end if(CPUE.mod == F)
    if(CPUE.mod == F) # If you do not have CPUE data...
    {
      DD.dat <- subset(mod.dat,year %in% strt.mod.yr:yr,select = 
                       c("I","I.cv","IR","IR.cv","N","N.cv","NR","NR.cv","clappers","clappersR","g","gR","catch","g.se","gR.se","year"))
      names(DD.dat) <- c("I","I.cv","IR","IR.cv","N","N.cv","NR","NR.cv","clappers","clappersR","G","GR","C","G.se","GR.se","year") # just making catch turn into "C".
    } # end if(CPUE.mod == F)
    
    # What happens if we turn down Growth by a certain percentage.  Growth rate can be 1 number which turns growth down equally across all years
    # But also could be a vector which could be used for finer control 
    if(!is.null(growth.rate)) 
    {
      DD.dat$G <- growth.rate * DD.dat$G
      DD.dat$GR <- growth.rate * DD.dat$GR
    } # end if(!is.null(growth.rate)) 
  
    # If you want to add in some growth variability to the model.
    if(!is.null(growth.var)) 
    {
      if(growth.var == 0) growth.var <- 1e-6 # If you set this to 0 in JAGS it breaks, so set the variability to be essentially 0.
      DD.dat$G.se <- rlnorm(length(DD.dat$G.se),log(growth.var),0.15)
      DD.dat$GR.se <- rlnorm(length(DD.dat$G.se),log(growth.var),0.15)
    } # end if(!is.null(growth.var)) 
  
    # If you want to change the level of uncertainty in our survey time series
    if(!is.null(surv.var)) 
    {
      DD.dat$I.cv <- DD.dat$I.cv * surv.var[1]
      DD.dat$IR.cv <- DD.dat$IR.cv  * surv.var[2]
    } # end  if(!is.null(surv.var))
  
    # Organize the data and set up the model priors/initialization data, then run the model.
    yrs<-min(DD.dat$year):max(DD.dat$year)
    DD.lst<-as.list(DD.dat)
    DD.lst$NY<- length(yrs)
    
    
    # Set up Priors.  First make sure our m priors are lined up properly.
    if(nrow(m.priors) > 1) m.priors <- m.priors[m.priors$year %in% yrs,] # Subset to the correct number of years if you have multile years
    if(nrow(m.priors) == 1) m.priors <- data.frame(M.a=rep(m.priors$M.a,DD.lst$NY),M.b=rep(m.priors$M.b,DD.lst$NY),
                                                   MR.a=rep(m.priors$MR.a,DD.lst$NY),MR.b=rep(m.priors$MR.b,DD.lst$NY)) # If just one year make it a string...
    # Now our catchability priors
    if(is.null(q.prior)) q.pr <- data.frame(a=20,b=40)# If we don't specify a q prior we make it a fairly specific prior centered at 0.33
    if(!is.null(q.prior)) q.pr <- q.prior # if we do specify a q prior just rename it.
    
    #This first bit is getting our variance correct for the CV's for Biomass and Recruit biomass
    # This is then added to our list of priors to get them correct.
    # Biomass CV
    uI=log(DD.lst$I.cv^2+1) # See Smith and Hubley 2014 for details, this is variance of the log of a CV
    # DK Note:  Smith/Hubley suggest this should be 3, so why we setting it to 2???
    Ip.a=2+(uI/uI)^2 # This is the alpha prior term for the prior (an inverse-gamma, i.e. gamma using 1/var); a rather funky way of setting alpha =2
    Ip.b=1/(uI*((uI/uI)^2+1)) # This is the beta term for the prior, again a strangly complex way of saying 1/(2*(uI))
    # Recruit biomass CV, see above comments for details.
    uIR=log(DD.lst$IR.cv^2+1)
    IRp.a=2+(uIR/uIR)^2
    IRp.b=1/(uIR*((uIR/uIR)^2+1))
    # Do the same for the catch rates if they are included in the model.
    if(CPUE.mod ==T)
    {
      # Catch Rate CV, see above comments for details.
      uU=log(DD.lst$U.cv^2+1)
      Up.a=2+(uU/uU)^2
      Up.b=1/(uU*((uU/uU)^2+1))
    }

    if(CPUE.mod == F) 
    {
      DDpriors=list(
        # scaler to total biomass, a= mean  b = sd, this gives a huge range of starting values
        logK=			    list(a=7,		           b=7,		             d="dnorm",	  l=1),		
        # survey catchability fully recruited a= shape1, b=shape2
        q=				    list(a=q.pr$a, 	       b=q.pr$b,		       d="dlnorm",	  l=1),		
        # process error (SD) a = min, b = max
        sigma=			  list(a=0, 		         b=5,		             d="dunif",	  l=1),		
        # measurement error variance survey FR a = shape, b = scale (1/rate)
        I.precision=	list(a=Ip.a,	         b=Ip.b,	           d="dgamma",	l=DD.lst$NY),		
        # measurement error variance survey recruits a = shape, b = scale (1/rate)
        IR.precision=	list(a=IRp.a,	         b=IRp.b,            d="dgamma",	l=DD.lst$NY),	
        # FR natural mortality, default mostly b/t 0.1-0.4
        M=				    list(a=m.priors$M.a,   b=m.priors$M.b,		 d="dbeta",	  l=DD.lst$NY),	
        # recruit natural mortality, default mostly b/t 0.1-0.4
        MR=				    list(a=m.priors$MR.a,  b=m.priors$MR.b,		 d="dbeta",	  l=DD.lst$NY),	
        # scaled recruit biomass, a= meanlog  b = sdlog
        r=				    list(a=0, 		         b=1,		             d="dlnorm",	l=DD.lst$NY),
        # Growth of fully recruited individuals
        G =           list(a=log(DD.lst$G),  b= DD.lst$G.se,     d="dlnorm",  l=DD.lst$NY),    
        # Growth of recruits
        GR =          list(a=log(DD.lst$GR), b= DD.lst$GR.se,    d="dlnorm",  l=DD.lst$NY)   
  
      )
    } # end if(CPUE.mod == F) 
    
    if(CPUE.mod == T) 
    {
      DDpriors=list(
        # scaler to total biomass, a= mean  b = sd, this gives a huge range of starting values
        logK=			    list(a=7,		           b=7,		             d="dnorm",	  l=1),		
        # survey catchability fully recruited a= shape1, b=shape2
        q=				    list(a=q.pr$a, 	       b=q.pr$b,		       d="dbeta",	  l=1),		
        # process error (SD) a = min, b = max
        sigma=			  list(a=0, 		         b=5,		             d="dunif",	  l=1),		
        # measurement error variance survey FR a = shape, b = scale (1/rate)
        I.precision=	list(a=Ip.a,	         b=Ip.b,	           d="dgamma",	l=DD.lst$NY),		
        # measurement error variance survey recruits a = shape, b = scale (1/rate)
        IR.precision=	list(a=IRp.a,	         b=IRp.b,            d="dgamma",	l=DD.lst$NY),	
        # FR natural mortality, default mostly b/t 0.1-0.4
        M=				    list(a=m.priors$M.a,   b=m.priors$M.b,		 d="dbeta",	  l=DD.lst$NY),	
        # recruit natural mortality, default mostly b/t 0.1-0.4
        MR=				    list(a=m.priors$MR.a,  b=m.priors$MR.b,		 d="dbeta",	  l=DD.lst$NY),		
        # Growth of fully recruited individuals
        G =           list(a=log(DD.lst$G),  b= DD.lst$G.se,     d="dlnorm",  l=DD.lst$NY),    
        # Growth of recruits
        GR =          list(a=log(DD.lst$GR), b= DD.lst$GR.se,    d="dlnorm",  l=DD.lst$NY),    
        # scaled recruit biomass, a= meanlog  b = sdlog
        r=				    list(a=0, 		         b=1,		             d="dlnorm",	l=DD.lst$NY),
        # fishery catchability CPUE a= min, b = max
        qU=				    list(a=0,		           b=1,	               d="dunif",	  l=1),		
        # measurement error variance CPUE  a = shape, b = scale
        U.precision=	list(a=Up.a,	         b=Up.b,	           d="dgamma",	l=DD.lst$NY)		  
      )
      # If we are running the clapper + beta for mortality priors model we need to set up these priors
      if(jags.model == paste(fun.loc,"models/DD_jags_m_beta_and_clappers.bug",sep=""))
      {
        DDpriors=list(
          # scaler to total biomass, a= mean  b = sd, this gives a huge range of starting values
          logK=			    list(a=7,		           b=7,		             d="dnorm",	  l=1),		
          # survey catchability fully recruited a= shape1, b=shape2
          q=				    list(a=q.pr$a, 	       b=q.pr$b,		       d="dbeta",	  l=1),		
          # process error (SD) a = min, b = max
          sigma=			  list(a=0, 		         b=5,		             d="dunif",	  l=1),		
          # measurement error variance survey FR a = shape, b = scale (1/rate)
          I.precision=	list(a=Ip.a,	         b=Ip.b,	           d="dgamma",	l=DD.lst$NY),		
          # measurement error variance survey recruits a = shape, b = scale (1/rate)
          IR.precision=	list(a=IRp.a,	         b=IRp.b,            d="dgamma",	l=DD.lst$NY),	
          # FR natural mortality, default mostly b/t 0.1-0.4
          M=				    list(a=m.priors$M.a,   b=m.priors$M.b,		 d="dbeta",	  l=DD.lst$NY),	
          # recruit natural mortality, default mostly b/t 0.1-0.4
          MR=				    list(a=m.priors$MR.a,  b=m.priors$MR.b,		 d="dbeta",	  l=DD.lst$NY),		
          # Growth of fully recruited individuals
          G =           list(a=log(DD.lst$G),  b= DD.lst$G.se,     d="dlnorm",  l=DD.lst$NY),    
          # Growth of recruits
          GR =          list(a=log(DD.lst$GR), b= DD.lst$GR.se,    d="dlnorm",  l=DD.lst$NY),    
          # scaled recruit biomass, a= meanlog  b = sdlog
          r=				    list(a=0, 		         b=1,		             d="dlnorm",	l=DD.lst$NY),
          # fishery catchability CPUE a= min, b = max
          qU=				    list(a=0,		           b=1,	               d="dunif",	  l=1),		
          # measurement error variance CPUE  a = shape, b = scale
          U.precision=	list(a=Up.a,	         b=Up.b,	           d="dgamma",	l=DD.lst$NY),
          # clapper dissolution rate a= shape1, b=shape2, 8 & 11 gives ~ normal mean of .45ish
          S=				    list(a=8, 		b=11,		d="dbeta",  l=1		),		
          # measurement error FR clappers  a = shape, b = scale (1/rate)
          ikappa.tau2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		),	
          # measurement error recruit clappers a = shape, b = scale (1/rate))
          ikappa.rho2=	list(a=3, 		b=2.2407,	d="dgamma",	l=1		))	
      } # if if(jags.model == paste(direct,fun.loc,"models/DD_jags_m_beta_and_clappers.bug"))

    } # end if(CPUE.mod == T) 
  
    
    #Prepare priors for JAGS
      for(h in 1:length(DDpriors))
      {
        # Get the variances for log-normal and normal converted to precisions, note that in BUGS language the precision is
        # the inverse of the squared standard deviation (which is what you specify in R).  The standard deviation is what
        # was specified in the Prior list (as it is more intuitive)
        if(DDpriors[[h]]$d%in%c("dlnorm","dnorm")) DDpriors[[h]]$b <- 1/DDpriors[[h]]$b^2
        # For a Gamma to convert to precision the precision term is  the inverse of the 'Scale" term in a typical 
        # gamma distribution parameterization, aka this is now knonwn as the rate.
        # Happily this is the same as the parameterization in R dgamma(x,shape,rate) so our b parameter is correct for posterior plots.
        if(DDpriors[[h]]$d=="dgamma")DDpriors[[h]]$b<-1/DDpriors[[h]]$b
      } # end for(h in 1:length(DDpriors))
      # Made a data.frame of the priors, unwrap the list and combine by row.
      prior.dat<- data.frame(par=names(DDpriors),do.call("rbind",lapply(DDpriors,rbind)))
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
      ifelse(is.null(parameters) == T, parameters <- c(names(DDpriors),'K','P','B','R','mu','Imed','Ipred','Irep', 'IRmed','IRpred','IRrep',
                                                                              'sIresid','sIRresid','sPresid','Iresid','m','mR',
                                                                              'IRresid','Presid',"G","GR"),parameters)
      # Run the model
      start<-Sys.time()
      ## Call to JAGS, do you want to run in parallel?
      
      if(parallel==F)
      {
        out <- jags(data =  c(prior.lst,DD.lst), inits = NULL,parameters.to.save = parameters,  
                    model.file = jags.model,n.chains = nchains, n.iter = niter, n.burnin = nburn, 
                    n.thin = nthin)
      }
      
      if(parallel==T)
      {
        out <- jags.parallel(data =  c(prior.lst,DD.lst), inits = NULL,parameters.to.save = parameters,  
                             model.file = jags.model,n.chains = nchains, n.iter = niter, n.burnin = nburn, 
                             n.thin = nthin,jags.seed = seed)
      }
      # How long did that take?
      print(Sys.time()-start)
      
      # Rename the output so I retain the results 
      DD.out <- list(data=c(prior.lst,DD.lst,yrs), sims.list=out$BUGSoutput$sims.list,median=out$BUGSoutput$median,
                            mean=out$BUGSoutput$mean,summary=out$BUGSoutput$summary,priors = prior.lst,parameters=parameters)
                       
      # I will also retain the MCMC object produced in case I want it for something.
      mod.out <- out
      
      
      # If we don't specify these variables set them automatically from within the data, trying to make
      # a nice decision table based on the data itself, good for a first pass if you don't know what's coming...
      if(is.null(D_high)) D_high <- max(c(signif(ceiling(max(mod.dat$catch)),digits=1),proj.catch))
      # This is ugly way to get the step sizes for the Decsion table, but at least I don't need to think about it...
      if(is.null(step)) 
        {
        if(D_high > 10000) step <- 1000
        if(D_high < 10000) step <- 100
        if(D_high < 5000) step <-  50
        if(D_high < 1000) step <-  25
        if(D_high < 500) step  <-  5
        }
      
      if(is.null(URP)) URP <- 0.8*(mean(mod.out$BUGSoutput$mean$B))
      if(is.null(LRP)) LRP <- 0.3*(mean(mod.out$BUGSoutput$mean$B))
      # Now do the projections for next year
      ### DK, pick it up here, something is going screwy here, when I hard code it to be 0, 10000, and 500 it works
      # but when I use D_low, D_high, and step it breaks, not sure what' up... Still some issues below in the plots
      # but getting pretty close I think...
      DD.out<- projections(DD.out,C.p=seq(D_low,D_high,by=step),# C.p = potential catches in decision table
                           proj.para = projection.paras ,proj.uncert = proj.uncert) 
 
      ### Generate Decision Table ###
        D.tab<-decision(DD.out, "GBa",mu=0.15,refs=c(URP,LRP),post.survey.C=0,vers=2)
        if (export.tables == T) write.csv(D.tab,paste0(plot.loc,"Decision.csv",sep=""),row.names=F) #Write1

      # Save the model results
        save(DD.lst, DDpriors,DD.out,mod.dat,yr,D.tab,URP,LRP,DD.dat,proj.catch,yrs,
           file=paste(dat.loc,"Ma_",m.priors[1,1],"_Mb_",m.priors[1,2],"_MRa_",m.priors[1,3],"_MRb_",m.priors[1,4],
                      "_Growth_var_",growth.var[1],"_Growth_rate_",growth.rate[1],"_Survey_var_",surv.var[1],"_Model_results.RData",sep=""))


############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  
############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  
############# END Section 2 Model#############  END Section 2 Model#############  END Section 2 Model#############  




############# Section 3 Some model result summaries and figures############# ############# Section 3 Some model result summaries and figures############# 
############# Section 3 Some model result summaries and figures############# ############# Section 3 Some model result summaries and figures############# 
# If we want the diagnostics  this is the section.  

        
    # Some model outputs needed for the Update.  First the mortality
    mort <- 1- exp(-DD.out$mean$m[length(DD.out$mean$m)])
    mort.R <- 1- exp(-DD.out$mean$mR[length(DD.out$mean$mR)])
    # This lines up the column headers with the projected catch...
    TACI <- which(DD.out$data$C.p==(proj.catch))
    # This get us the predicted biomass for next year based on the projected catch
    BM.proj.1yr <- DD.out$median$B.p[TACI]
    # This is only useful for GBa at the moment since BBn doesn't have reference points accepted yet...

      # Get the quantiles, this likely would need changed, but which quantile is > our URP (13,284 as of 2015)
      B.quantiles <- quantile(DD.out$sims.list$B[,length(DD.out$sims.list$B[1,])],probs=seq(0,1,0.01))
      # This is the probability (well percentage) that Biomass is below the USR
      prob.below.USR <- names((which(B.quantiles > URP)[1]))

    
    # Here we can grab the Fully recruited and recruit biomass for the last 2 years and the median of the time series.
    FR.bm <- DD.out$median$B[(length(DD.out$mean$B)-1):length(DD.out$median$B)]
    # We exclude the current year from the median estimate
    FR.ltm <- median(DD.out$median$B[-length(DD.out$median$B)])
    # Recruit biomass
    rec.bm <- DD.out$median$R[(length(DD.out$median$R)-1):length(DD.out$median$R)]
    # We exclude the current year from the median estimate
    rec.ltm <- median(DD.out$median$R[-length(DD.out$median$R)])
      
    # Get the percent biomass change from the projection. 0 means unchanged, + means % increase, - means % decline
    percent.B.change <- (BM.proj.1yr / DD.out$median$B[length(DD.out$median$B)]) -1

    ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS ####################  MODEL DIAGNOSITCS 
    ##### Now we can run some model diagnostics.
    # Some quick diagnoistics, the maximum should be < 1.05
    rhat <- summary(DD.out$summary[,8])
  
    # Effective number of observations.  
    #Not sure what our minimum should be here, but using the Rhat + looking at the chains should indicate where there are problems...
    neff <- range(DD.out$summary[,9])


    save(mort,mort.R,TACI,BM.proj.1yr,B.quantiles,percent.B.change,prob.below.USR,FR.bm,FR.ltm,rec.bm,rec.ltm,neff,rhat,
         file=paste(dat.loc,"Ma_",m.priors[1,1],"_Mb_",m.priors[1,2],"_MRa_",m.priors[1,3],"_MRb_",m.priors[1,4],
                    "_Growth_var_",growth.var[1],"_Growth_rate_",growth.rate[1],"_FR_Survey_var_",surv.var[1],"_Rec_Survey_var_",surv.var[2],
                    "Model_results_and_diagnostics.RData",sep=""))

   

    #####Plot model diagnostics############## 
    # These plots include the posterior fits, exploitation estimate, Biomass fit to survey and CPUE, residual plot
    # and the model coinvergence plot (which is a 700+ page pdf of the convergence of each parameter + it's ACF.)
    # posterior densities for model parameters
    post.plt(DD.out,DDpriors,years=yrs, graphic=fig,multi=T,path=plot.loc)
    #dev.off()
    ##exploitaiton time series
    exploit.plt(DD.out, years=yrs, plt=c('f','m','mR'),graphic=fig,path=plot.loc)
    #dev.off()
    # model biomass fit to survey
    fit.plt(DD.out, years = yrs, CI=T,graphic=fig,path=plot.loc,CV=T)
    # diagnostic plot
    diag.plt(DD.out, years = yrs,graphic=fig,path=plot.loc)
    # The biomass plot for fully recruited and recruits along with the projection
    biomass.plt(DD.out,years=yrs, graphic=fig,TAC=proj.catch,path=plot.loc,refs = c("LRP","URP","zones"),pred=1,
                URP =URP, LRP=LRP,avg.line=median)
      
      
      
    # Here we pull together all of the chains and look to see that they are both well mixed and that
    # there is no correlation.   This is a complete crap load of plots!!
    # Scripts to plot all the chains.
    if(convergence.check == T)
    {
      # Get the bank pulled out and figure out how many parameters we have
      num.param <- length(names(DD.out$sims.list))
      param.names <- names(DD.out$sims.list)
      # Make the pdf, given the number of parameters in the model you don't get an option for making this plot print to screen
      # if you run diagnostics you get this pdf
      
      # First we can set the dimensions for the plotting device.
      # Set the number of rows
      nr <- ceiling(sqrt(nchains))
      # Set the number of columns, the funky little command is used to add one to the nc if nchains is a perfect square
      # As we are printing nchains + 1
      ifelse(sqrt(nchains)%%3==0,  nc <- ceiling(sqrt(nchains))+1, nc <- ceiling(sqrt(nchains)))
      pdf(file=paste(plot.loc,"Model_convergence.pdf",sep=""),onefile=T)
      # Set up the plotting device.  This should work for everything but the single chain scenario, which you shouldn't be running anyways!
      plot.mat <- matrix(1:ceiling(sqrt(nchains))^2,ceiling(sqrt(nchains)),byrow=T)
      plot.mat[plot.mat > (nchains+1)] <- 0
      if(nchains ==1) plot.mat <- matrix(1:2,1,2) # If you're silly and only running 1 chain...
      layout(plot.mat)  # 1 column, 2 rows
      
        
      par(mar=c(2,2,3,1))
      for(i in 1:num.param)
      {
        # This pulls out all the plot for parameters with only one value
        if(is.vector(DD.out$sims.list[[names(DD.out$sims.list)[i]]])==T)
        {
          # Since our all of these parameters are hyperparameter (there isn't one for every year) this works, 
          #if this was a matrix we'd get an error here.
          len <- length(DD.out$sims.list[[param.names[i]]])
          # This sets us up to pull out the right "chains" from the data using the k loop below.
          bins <- seq(1,len,by = len/nchains)
          # Get the ylimits for the plot.
          ylims <- range(DD.out$sims.list[[param.names[i]]],na.rm=T)
          colr <- rainbow(nchains) # set up a color ramp
          count <- 0 # Set up a counter
          # I need to run this loop twice, once for the first figure so we get all the lines added and a second time to 
          # add in the ACF's.  Probably a nicer way to do this, but it'll do...
          # Now run the loop and make the figure showing the mixing of the chains
          for(k in bins)
          {
            count <- count+1 # used for the color ramp
            # Get the data
            dat <-DD.out$sims.list[[param.names[i]]][k:(k+len/nchains-1)] 
            if(k ==1) plot(dat,type="l",col=colr[count], main = paste(param.names[i], "Chain"),xlab="",ylab="",ylim=ylims)
            if(k > 1) lines(dat,col=colr[count])
          } # end for(k in bins)
          
          # Now to make the ACF figures
          count  <- 0 # Reset the counter
          for(k in bins)
          {
            count <- count+1 # used to ID the chain
            # Pick it up from here.
            dat <-DD.out$sims.list[[param.names[i]]][k:(k+len/nchains-1)] 
            # And look for any signs of autocorrelation in the chains...
            acf(dat,lag.max = 10,main = paste("ACF chain",count),xlab="",ylab="",ylim=c(0,0.3))
          }# end for(k in bins)
          
        } # end if(is.vector(DD.out$sims.list[[names(DD.out$sims.list)[1]]])==T)

        # This pulls out all the plots for parameters with multiple values (i.e. annual estimates)
        if(is.vector(DD.out$sims.list[[names(DD.out$sims.list)[i]]])==F)
        {
          num.reps <- ncol(DD.out$sims.list[[names(DD.out$sims.list)[i]]])
          rep.names <- paste(names(DD.out$sims.list)[i],1:num.reps,sep="_")
          # Run this loop for each chain for these parameters.
          for(j in 1:num.reps)
          {
            # Get the length again (this could probably be tidied up as this number has to be the same for all parameters.)
            len <- length(DD.out$sims.list[[param.names[i]]][,j])
            # Get the bins for the loop
            bins <- seq(1,len,by = len/nchains)
            # Get the ylimits for the plot.
            ylims <- range(DD.out$sims.list[[param.names[i]]][,j],na.rm=T)
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
              dat <-DD.out$sims.list[[param.names[i]]][k:(k+len/nchains-1),j]
              if(k ==1)  plot(dat,type="l",col=colr[count], main = paste(rep.names[j], "Chain"),xlab="",ylab="",ylim=ylims)
              if(k > 1) lines(dat,col=colr[count])
            } # end for(k in bins)
            count <- 0 # Reset the counter
            # And now for the ACF figures
            for(k in bins)
            {
              count <- count+1 # used for the chain index
              # Get the data
              dat <-DD.out$sims.list[[param.names[i]]][k:(k+len/nchains-1),j]
              # And look for any signs of autocorrelation in the chains...
              acf(dat,lag.max = 10,main = paste("ACF chain ",count),xlab="",ylab="",ylim=c(0,0.3),na.action = na.pass)
            } # end for(k in bins)
          }# end for(j in 1:num.reps)
        } # end if(is.vector(DD.out$sims.list[[names(DD.out$sims.list)[i]]])==F)
      }  # end for(i in 1:num.param)
      dev.off()
    }# end if(convergence.check == T)
      
  ############# End Section 3 result summaries and figures############# ############# End Section 3 result summaries and figures############# 
  ############# End Section 3 result summaries and figures############# ############# End Section 3 result summaries and figures############# 
      
} # end function
