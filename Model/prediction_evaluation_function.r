###################################################################################################
# This function is used to get the prediction evaluation results to be used for the prediction evaluation figure or anything else you so desire!
###################################################################################
# Update history
# Feb 2016 DK made some minor edits
# April 2015 DK overhauled this function to work with JAGS and to produce box-plots removed some of code as no evidence it is ever used.
# Jan 2018:  DK split the function into 2 functions, one to run the model and one to pull in these results and make the figures...
# April 2018:  Dk tidying up some loose ends from the January revision
# Oct 2018:  More DK tidying to make it work for multiple years (had something screwy in priors...)
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##
##  1:  Update_function_JAGS.r
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
##
###############################################################################################################


###############################################################################################################
# Arguments
#
# 1:  input:      The input used for the model run.  Default is missing, should be the DD.dat object used for the main model
# 2:  priors:     The priors used for the model run.  Default is missing, generally you'll specify DD.out[[bnk]]$priors
# 3:  parameters: The parameters from the original model output.  Default is NULL, generally you specify DD.out[[bnk]]$parameters
# 4:  pe.years:   The years for the prediction evaluation to run.  For example putting 2017 would set this up to run the prediction evaluation for 2017 only.
#                  default is null which runs the prediction evaluation for the most recent year.
# 4:  model:      The JAGS model.  Default is missing, should be the same model as you used for the main model run. 
# 6:  niter:      Number of iterations to run.  Default = 60000
# 7:  nburn:      The burnin for the model.  Default = 40000 
# 8:  nthin       Thinning rate for the model output.  Deafult = 20
# 9:  nchains     The number of chains Default = 8. If running in parallel increasing this will return more replicates with no computing time cost.
# 9:  graphic:    What to do if making a figure.  Print to "screen" by default, optionally save as a "pdf".
#10:  run:        Do you need to run the model?  (T/F), Default = T, if you have run this once the results are saved and you can set to F
#12:  plot:       The plot to produce the various potential figures.  There are 3 options here, 
#                 1: default = NULL, no plot is produced
#                 2: "box", this produces the nice box plot of the predicted vs modeled results for each year of interest
#                 3:  "ts", this produces a point summary of the time series of the differences. 
#                 4:  "ts_all", produces a very busy plot of the predictive vs. modelled estimates.
#13:  lab:        If you want to add a specific label to the output figure (only useful if graphics = "pdf")
#14:  g2:         If specified the growth term for FR's used in the prediction evals is the current year condition * expected SH next year
#15:  gR2:        If specified the growth term for recruits used in the prediction evals is the current year condition * expected SH next year
#16:  path:       The path to put the pdf, will default to whatever is specified (direct is not used here)
#17:  direct:       The path to put the output results (not including the figures).  Default = Y:/Offshore scallop/Assessment
#18:  bank:       The bank to run the prediction evaluations on.  Default = "GBa" ("BBn" is only other option that should work currently)
#19:  parallel:   If running the predictin evaluations do you want to run in parallel?  Number of clusters used set by nchains.
#20:  j.seed:     If running in parallel you can specify a seed (for initial values) so the results are reproducable.  Default = 123
#21: save.res:    Where do you want to save the results, "default" saves them along with all the other figures.  If not default you
#                 must specify the working directory you want to save these too.
#
###############################################################################################################


pred.eval <- function(input, priors, parameters, pe.years= NULL,model = "Assessment_fns/Model/DDwSE3_jags.bug",  growth = "both",
                      niter = NULL, nburn = NULL, nthin = NULL,nchains=NULL,
                      direct,direct_fns,j.seed=123,parallel=T,bank=NULL,save.res = "default")
{
  
# The functions to load
  if(missing(direct_fns))
  {
    funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Model/projections.r")
    # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
    for(fun in funs) 
    {
      download.file(fun,destfile = basename(fun))
      source(paste0(getwd(),"/",basename(fun)))
      file.remove(paste0(getwd(),"/",basename(fun)))
    }
   } else { source(paste0(direct_fns, "Model/projections.r")) } #end else statement
  
library(R2jags)
if(is.null(bank)) bank <- names(input) # Identify the bank if not specified
# The input data is the years and all the data excluding the number of years
input.dat <- data.frame(input[names(input)!="NY"])
# Set the years to be just the most recent year if pe.years is set to null.
if(is.null(pe.years)) pe.years <- max(input.dat$year)
num.years <- length(pe.years)	
	
if(growth[1] == "both") growth <- c("modelled","realized")
growth <- sort(growth) # the below needs this in alphabetical order...

# Make a temp priors object we can use to reset the priors for the second k loop
priors.t <- priors

# Now we loop this through each growth scenario
for(k in 1:length(growth))
{ 
  priors <- priors.t
  # Now if we are using the realized growth we need to revise these so the model is pulling in the correct growth term
  if(growth[k]=="realized") 
  {
    input.dat$g <- input.dat$g2
    input.dat$gR <- input.dat$gR2
  } # if(growth[k]=="realized") 
  # Define an out object
  out<-NULL
	# Start the performance evalulations for each year requested
	for(i in 1:num.years)
	{
	  # Run the model for the years identified 
	  # Modify the input list to the proper number of years and remove the year column
	  if(i == 1) input.lst <- as.list(subset(input.dat,year %in% min(input.dat$year):pe.years[i]))
	  if(i > 1) input.lst <- as.list(subset(input.dat,year %in% min(input.dat$year):pe.years[i]),-i)
	  # Get the Number of years correct.
	  input.lst$NY <- length(input.lst$C)
	  #browser()
	  
	  # Need to do a little stick handling to get the right priors for the ones specified each year
	  # We need to do this for the predictions in previous years
	   priors[which(lapply(priors,length)>1)] <- as.list(data.frame(priors[which(lapply(priors,length)>1)])[1:input.lst$NY,]) 	
	  # Run model either in parallel...
	   start<-Sys.time()
	   if(parallel == T)
	   {
	     outp <- jags.parallel(data =  c(priors,input.lst), inits = NULL,parameters.to.save = parameters,  
	                         model.file = paste(direct,model,sep=""),n.chains = nchains, n.iter = niter, n.burnin = nburn, 
	                          n.thin = nthin,jags.seed = j.seed)
	   }# end if(para = T)
	    
	   # ...or not
	   if(parallel == F)
	   {
	     outp <- jags(data =  c(priors,input.lst), inits = NULL,parameters.to.save = parameters,  
	                         model.file = paste(direct,model,sep=""),n.chains = nchains, n.iter = niter, n.burnin = nburn, 
	                         n.thin = nthin)
	   } # end if(para = F)
	   
	   print(Sys.time()-start)
	   DD <- list(data=c(priors,input.lst), sims.list=outp$BUGSoutput$sims.list,median=outp$BUGSoutput$median,
	                     mean=outp$BUGSoutput$mean,summary=outp$BUGSoutput$summary,priors = priors)
 		 # Run projections from 0 up to catch in the final year
		 DD <- projections(DD,C.p = c(0,input.lst$C[input.lst$NY]))
 		 # Save medians and the projected biomass, this uses the actual catch in the given year for the projection
		 out[[as.character(pe.years[i])]]<-list(median = DD$median, proj.bm.sims = DD$sims.list$B.p[,2],
		                                       bm.sims = DD$sims.list$B,year = pe.years[i])
	} # end for(i in 1:length(pe))
	
  # Now save the output so we don't have to run this every time.
  if(growth[k] == "modelled") 
  {
    if(save.res == "default")  save(out,file= paste(direct,"Data/Model/",(max(pe.years)+1),"/",bank,"/Results/Projection_evaluation_modelled_growth.RData",sep=''))
    if(save.res != "default")  save(out,file= paste(save.res,"Projection_evaluation_modelled_growth.RData",sep=''))
    out.modelled <- out
  } # end if(growth[k] == "modelled") 

  if(growth[k] == "realized")
  {  
    if(save.res == "default")  save(out,file= paste(direct,"Data/Model/",(max(pe.years)+1),"/",bank,"/Results/Projection_evaluation_realized_growth.RData",sep=''))
    if(save.res != "default")  save(out,file= paste(save.res,"Projection_evaluation_realized_growth.RData",sep=''))
    
    out.realized <- out
  } # end if(growth[k] == "realized")
  
} # end for(k in 1:length(growth))

if(length(growth) == 2) return(list(PE.modelled = out.modelled,PE.realized = out.realized))
if(length(growth) == 1 && growth == "realized") return(list(PE.realized = out.realized))
if(length(growth) == 1 && growth == "modelled") return(list(PE.modelled = out.modelled))
} #end function