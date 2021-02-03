###################################################################################################
# This function produces the prediction evaluation plots.   If no input data is selected it finds the results from the most recent year and plots that.
###################################################################################
# Update history
# Feb 2016 DK made some minor edits
# April 2015 DK overhauled this function to work with JAGS and to produce box-plots removed some of code as no evidence it is ever used.
# January 2018:  DK took the old peR_jags function and split it into two functions, this is the plotting half of the function and is slightly
#                more flexible now as you can input the data directly from the newly created prediction_evaluation_function
# May 2018"  Need to add a couple of if stateents to make sure R wasn't being a jerk about handling the names for the mod.bm and proj.bm objects.
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
# 1:  input:      The data to input, default = NULL which looks for the most recent data and plots that.
# 2:  growth:     The growth plot to make, default = "both" which produces the "modelled" growth and "realized" growth figures.
#                 both only works if you have NOT specified the input data.  Options are "realized", "modelled", and "both"
# 3:  years:      The years for the model runs.  Default is missing, should be the same as you used to run the main model 
# 4:  graphic:    What to do if making a figure.  Print to "screen" by default, you can now save as any valid image type (jpg, png, pdf, etc).
# 5:  plot:       The plot to produce the various potential figures.  There are 3 options here, 
#                 1 "box", the default, this produces the nice box plot of the predicted vs modeled results for each year of interest
#                 2:  "ts", this produces a point summary of the time series of the differences. 
#                 3:  "retrospective", produces a very busy plot of all the predictive vs. modelled estimates.
# 6:  path:       The path to put the figure, Will default to the working directory
# 7:  bank:       The bank to run the prediction evaluations on.  Default = "GBa" ("BBn" is only other option that should work currently)
# 8:  wd:         The width of the figure.  Default =11
# 9:  ht:         The height of the figure.  Default = 8.5
# 10: direct:.....The directory to look for the data if you haven't specified the input, won't do anything if you have something in input.
# 11: title:      Add a title to the figure.  T/F, default = F
#12:  txt_size:   The size of the text in the figure, default = 18
#
###############################################################################################################


pe.fig <- function(input = NULL, growth = "both", years, graphic="screen",plot= "box",path=NULL,bank="GBa",
                   wd=11,ht=8.5,direct, title = F,txt_size = 18)
{
  
  require(tidyverse) || stop("Make sure you have tidyverse loaded to run this")
  require(reshape2) || stop("Make sure you have reshape2 loaded to run this")
  options(stringsAsFactors = F)
  # If you haven't specified the path set it to the working directory...
  if(is.null(path)) path <- getwd()
  # If going to plot both growth terms...
  if(growth == "both") growth <- c("modelled", "realized")
  
  # If years is supplied get the max from that
  if(!missing(years)) yr <- max(years)
  # If you don't supply the years you need to supply the input
  if(missing(years) && is.null(input)) stop("Hallo again modelling friend, for the Pred-Eval Figures you need to specify the year 
                                          or provide the input data")
  if(missing(years)) yr <- max(as.numeric(names(out)))
  # If you have supplied the input data directly this is run
  if(!is.null(input))
  {
    out.tmp <- input
    if(growth == "realized") out.realized <- input
    if(growth == "modelled") out.modelled <- input
    if(length(growth) > 1) stop("Hey, heads up, if you are inputting the Pred-Eval results directly you need to specify if it is 'modelled' growth (i.e. g and gR) 
                                or 'realized' growth (g2 and gR2) input data, growth cannot = 'both' in this case")
  } # end if(!is.null(input))
  # If you haven't supplied the data we pull out all of the historic projections we've run
  if(is.null(input))
  {
    # First load in the data required to make the plot...
    
    # If the files don't exist yet print a warning saying you have to run the models first
    if(file.exists(paste(direct,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_modelled_growth.RData",sep=''))==F &&
       file.exists(paste(direct,"Data/Model/",(yr+1),"/",bank,"/Results/Projection_evaluation_realized_growth.RData",sep=''))==F &&  
       max(years) >=2017) 
    {
      print("Yo dude, you gotta run the prediction evaluation models before you can plot them...rookie...")
    } # end if if(file.exists(...) = F)
    
    # If g2 is specified then we plot the g2 model results
    if(any(growth %in% c("modelled")))
    {
      # This loads in all the model runs from 2000-2016 without us having to continually re-run them...
      if(file.exists(paste(direct,"Data/Model/",2017,"/",bank,"/Results/Projection_evaluation_results_mod_growth.RData",sep='')))
      {
        load(paste(direct,"Data/Model/",2017,"/",bank,"/Results/Projection_evaluation_results_mod_growth.RData",sep=''))
      } # end ugly if 
      
      # Or load this one...
      if(file.exists(paste(direct,"Data/Model/",2017,"/",bank,"/Results/Projection_evaluation_modelled_growth.RData",sep='')))
      {
        load(paste(direct,"Data/Model/",2017,"/",bank,"/Results/Projection_evaluation_modelled_growth.RData",sep=''))
      }# end ugly if # 2
      
      if(exists("out")==F) print(paste0("missing 2017 Projection_evaluation_modelled_growth.RData for ", bank, ". Your figures are doomed to fail unless you go back and run the model."))
      if(exists("out")==T) out.tmp <- out; rm(out) # pass "out" object to out.tmp, then remove it from the environment
      
      # For every year after this we'll grab the results and stick them into the same object..
      if(max(years) >=2017)
      {
        # Now I need to grab the data from 2017 up to the current year...
        post.years <- 2017:years
        for(i in 1:length(post.years))
        {
          if(file.exists(paste0(direct,"Data/Model/",post.years[i]+1,"/",bank,"/Results/Projection_evaluation_modelled_growth.RData")))
          {load(paste(direct,"Data/Model/",post.years[i]+1,"/",bank,"/Results/Projection_evaluation_modelled_growth.RData",sep=''))}
          if(exists("out")==T) out.tmp[[as.character(post.years[i])]] <- out[[as.character(post.years[i])]]
          if(exists("out")==F) print(paste0("missing ", post.years[i]+1, " Projection_evaluation_modelled_growth.RData for ", bank, ". Your figures are doomed to fail unless you go back and run this model."))
        } # end for(i in 1:length(years[years > 2016])) 
      } # end if(max(years) >=2017)
      # Get the data in a sensisble order...
      
      out.modelled <- out.tmp[order(names(out.tmp))]
    } # end iif(any(growth %in% c("modelled")))
    
    # If we want to plot the realized growth results do this....
    if(any(growth %in% c("realized")))
    {
      # This loads in all the model runs from 2000-2016 without us having to continually re-run them...
      if(file.exists(paste(direct,"Data/Model/",2017,"/",bank,"/Results/Projection_evaluation_results_g2_growth.RData",sep='')))
      {
        load(paste(direct,"Data/Model/",2017,"/",bank,"/Results/Projection_evaluation_results_g2_growth.RData",sep=''))
      } # end ugly if
      
      # Or load this one...
      if(file.exists(paste(direct,"Data/Model/",2017,"/",bank,"/Results/Projection_evaluation_realized_growth.RData",sep='')))
      {
        load(paste(direct,"Data/Model/",2017,"/",bank,"/Results/Projection_evaluation_realized_growth.RData",sep=''))
      } # end ugly if # 2
      
      
      out.tmp <- out
      if(max(years) >=2017)
      {
        # For every year after this we'll grab the results and stick them into the same object...
        # Now I need to grab the data from 2017 up to the current year...
        post.years <- 2017:years
        for(i in 1:length(post.years))
        {
          load(paste(direct,"Data/Model/",post.years[i]+1,"/",bank,"/Results/Projection_evaluation_realized_growth.RData",sep=''))
          out.tmp[[as.character(post.years[i])]] <- out[[as.character(post.years[i])]]
        } # end for(i in 1:length(years[years > 2016])) 
      }
      # Get the names in a reasonable order...
      out.realized <- out.tmp[order(names(out.tmp))]
      
    } # end iif(any(growth %in% c("realized")))
  } # end if(is.null(input))	
  
  # The predicted biomass was this (the 2 represents the actual catch values from the projection)
  # But remember threse are the predictions from a given year (so are actually the following year values)
  # that's dealt with below...
  if(any(growth %in% c("modelled")))
  {
    # Get the number of years we are plotting
    yrs <- as.numeric(names(out.modelled))
    preds.m <- sapply(names(out.modelled), function(i){out.modelled[[i]]$median$B.p[2]})
    ests.m <- sapply(names(out.modelled), function(i){rev(out.modelled[[i]]$median$B)[1]})
    
    # Now convert these data into something nice for GGplot
    modelled.dat <- data.frame(year = as.numeric(names(ests.m)),projected = preds.m,actual = ests.m)
    modelled.dat <- melt(modelled.dat,id.vars = "year",value.name = "biomass",variable.name = "type")
    modelled.dat$year[modelled.dat$type == "projected"] <- modelled.dat$year[modelled.dat$type == "projected"] + 1
    # Because we don't have data for both the projection and model in the 1st and final year lets toss those...
    modelled.dat <- modelled.dat[modelled.dat$year != min(modelled.dat$year) & modelled.dat$year != max(modelled.dat$year),]
    
  } # end 	  if(any(growth %in% c("modelled")))
  
  if(any(growth %in% c("realized")))
  {
    # Get the number of years we are plotting
    yrs <- as.numeric(names(out.realized))
    # This gets the median predicted and estimated median biomasses for each year.
    preds.r <- sapply(names(out.realized), function(i){out.realized[[i]]$median$B.p[2]})
    ests.r <-  sapply(names(out.realized), function(i){rev(out.realized[[i]]$median$B)[1]})
    
    # Now convert these data into something nice for GGplot
    realized.dat <- data.frame(year = as.numeric(names(ests.r)),projected = preds.r,actual = ests.r)
    realized.dat <- melt(realized.dat,id.vars = "year",value.name = "biomass",variable.name = "type")
    realized.dat$year[realized.dat$type == "projected"] <- realized.dat$year[realized.dat$type == "projected"] + 1
    # Because we don't have data for both the projection and model in the 1st and final year lets toss those...
    realized.dat <- realized.dat[realized.dat$year != min(realized.dat$year) & realized.dat$year != max(realized.dat$year),]
  } # end if(any(growth %in% c("realized")))
  # This plot is the median biomass trend from:
  # a: the model predictions (e.g. the biomass is generated for 2012 from the model that ran from 1986:2011)
  # b: from the actual model estimates (e.g. the biomass is generated for 2011 from the model that runs from 1986:2011)
  # and finally from the from the model using all data (e.g. the 2011 biomass is extracted from the model than ran from 1986:CurrentYear)
  if("ts" %in% plot)
  {
    if(any(growth %in% c("modelled")))
    {
      # Now make the plot for the modelled growth data
      p <- ggplot(modelled.dat) + 
        geom_point(aes(year,biomass/1000,colour=type)) + 
        geom_line(aes(year,biomass/1000,group=type,colour=type)) + 
        ylab("Fully recruited biomass (Kt)") + 
        xlab("") + 
        theme_bw(base_size=txt_size) + 
        theme(panel.grid=element_blank(),legend.title = element_blank()) + 
        scale_colour_discrete(name=NULL) + 
        scale_x_continuous(breaks = seq(min(yrs),max(yrs),by=3))
      if(title == T) p <- p + ggtitle("Biomass based on modelled growth rate")
      if(graphic != 'screen') ggsave(paste0("pred_eval_point_estimates_",max(years),"_modelled_growth.",graphic),path = path,width = wd, height = ht) 
      if(graphic=="screen")  windows(wd,ht) ; plot(p)
      #if(graphic != "screen")dev.off()
    } # end 	  if(any(growth %in% c("modelled")))
    
    if(any(growth %in% c("realized")))
    {
      # Now make the plot for the realized growth data
      p <- ggplot(realized.dat) + geom_point(aes(year,biomass/1000,colour=type)) + geom_line(aes(year,biomass/1000,group=type,colour=type)) + 
        ylab("Fully recruited biomass (Kt)") + xlab("") + theme_bw(base_size=txt_size) + theme(panel.grid=element_blank(),legend.title = element_blank()) + 
        scale_colour_discrete(name=NULL)+ scale_x_continuous(breaks = seq(min(yrs),max(yrs),by=3))
      if(title == T) p <- p + ggtitle("Biomass based on realized growth rate")
      if(graphic != 'screen') ggsave(paste0("pred_eval_point_estimates_",max(years),"_realized_growth.",graphic),path = path,width = wd, height = ht) 
      if(graphic=="screen")  windows(wd,ht) ; plot(p)
      
    } # end if(any(growth %in% c("realized")))
  } # end if(ts %in% plot)
  
  # Now the second plot which plots the time series median biomass along with the predicted for every model (year)
  # This could get really messy really quickly!!
  if("retrospective" %in% plot)
  {
    if(any(growth %in% c("modelled")))
    {
      biomass.m <- NULL
      
      # This gets the biomass time series from each model run for any years that we have the data for from that model.
      for(i in 1:length(names(out.modelled)))
      {
        # Get the biomass, the year associated with that biomass, and the model "year" the biomass estimate was made...
        biomass.m[[names(out.modelled)[i]]]$biomass <- out.modelled[[i]]$median$B[(length(out.modelled[[1]]$median$B)-length(years)+1):length(out.modelled[[i]]$median$B)]
        biomass.m[[names(out.modelled)[i]]]$year <-  yrs[1:length(biomass.m[[names(out.modelled)[i]]]$biomass)]
        biomass.m[[names(out.modelled)[i]]]$dat.year <- as.numeric(names(out.modelled)[i])
        biomass.m[[names(out.modelled)[i]]] <- as.data.frame(biomass.m[[names(out.modelled)[i]]])
      } # end for(i in 1:length(names(out.realized)))
      # Now unwrap the data
      bm.ts.modelled <- do.call("rbind",biomass.m)
      
      # and now plot the data
      
      p <- ggplot(bm.ts.modelled) + geom_point(aes(year,biomass/1000,colour=as.factor(dat.year))) + 
        geom_line(aes(year,biomass/1000,group=as.factor(dat.year),colour=as.factor(dat.year))) + 
        ylab("Fully recruited biomass (Kt)") + xlab("") + theme_bw(base_size=txt_size) + theme(panel.grid=element_blank(),legend.title = element_blank()) + 
        scale_colour_discrete(name=NULL) + scale_x_continuous(breaks = seq(min(yrs),max(yrs),by=3))
      if(title == T) p <- p + ggtitle("Historic Biomass trends using modelled growth rate")
      if(graphic != 'screen') ggsave(paste0("pred_eval_all_models_",max(years),"_modelled_growth.",graphic),path = path,width = wd, height = ht) 
      if(graphic=="screen")  windows(wd,ht) ; plot(p)
    } # end if(any(growth %in% c("modelled")))
    
    
    if(any(growth %in% c("realized")))
    {
      biomass.r <- NULL
      # This gets the biomass time series from each model run for any years that we have the data for from that model.
      for(i in 1:length(names(out.realized)))
      {
        # Get the biomass, the year associated with that biomass, and the model "year" the biomass estimate was made...
        biomass.r[[names(out.realized)[i]]]$biomass <- out.realized[[i]]$median$B[(length(out.realized[[1]]$median$B)-length(years)+1):length(out.realized[[i]]$median$B)]
        biomass.r[[names(out.realized)[i]]]$year <- yrs[1:length(biomass.r[[names(out.realized)[i]]]$biomass)]
        biomass.r[[names(out.realized)[i]]]$dat.year <- as.numeric(names(out.realized)[i])
        biomass.r[[names(out.realized)[i]]] <- as.data.frame(biomass.r[[names(out.realized)[i]]])
      } # end for(i in 1:length(names(out.realized)))
      # Now unwrap the data
      bm.ts.realized <- do.call("rbind",biomass.r)
      
      # and now plot the data
      ggplot(bm.ts.realized) + geom_point(aes(year,biomass/1000,colour=as.factor(dat.year))) + 
        geom_line(aes(year,biomass/1000,group=as.factor(dat.year),colour=as.factor(dat.year))) + 
        ylab("Fully recruited biomass (Kt)") + xlab("") + theme_bw(base_size=txt_size) + theme(panel.grid=element_blank(),legend.title = element_blank()) + 
        scale_colour_discrete(name=NULL)+ scale_x_continuous(breaks = seq(min(yrs),max(yrs),by=3))
      if(title == T) p <- p + ggtitle("Historic Biomass trends using realized growth rate")
      if(graphic != 'screen') ggsave(paste0("pred_eval_all_models_",max(years),"_realized_growth.",graphic),path = path,width = wd, height = ht) 
      if(graphic=="screen")  windows(wd,ht) ; plot(p)
    } # end if(any(growth %in% c("realized")))
    
  } # if(ts_all %in% plot)
  
  # This final plot is the boxplot of the data along with the prediction, DK modified to make this
  # the boxplot of both the estimate and the prediction for each model.
  if("box" %in% plot)
  {
    if(any(growth %in% c("modelled")))
    {
      # Got it working, woot woot!!
      num.years <- length(out.modelled)
      proj.bm <- NULL
      mod.bm <- NULL
      for(i in 1:num.years)
      {
        proj.bm[[names(out.realized)[i]]] <- out.realized[[i]]$proj.bm.sims[out.realized[[i]]$proj.bm.sims > quantile(out.realized[[i]]$proj.bm.sims,0.1) &
                                                                              out.realized[[i]]$proj.bm.sims < quantile(out.realized[[i]]$proj.bm.sims,0.9)]
        tmp <- out.realized[[i]]$bm.sims[,ncol(out.realized[[i]]$bm.sims)]
        mod.bm[[names(out.realized)[i]]] <- tmp[tmp > quantile(tmp,0.1) & tmp < quantile(tmp,0.9)]
      } # for (i in 1:num.years)
      proj.bm <- melt(proj.bm,value.name = "bm")
      names(proj.bm) <- c("bm","year")
      mod.bm <- melt(mod.bm,value.name = "bm")
      names(mod.bm) <- c("bm","year")
      proj.bm <- data.frame(biomass = proj.bm$bm,year = as.numeric(proj.bm$year)+1,type = "projected")
      mod.bm <- data.frame(biomass = mod.bm$bm,year = as.numeric(mod.bm$year),type = "actual")
      bm.dat <- rbind(proj.bm,mod.bm)
      # Because we don't have data for both the projection and model in the 1st and final year lets toss those...
      bm.dat <- bm.dat[bm.dat$year != min(bm.dat$year) & bm.dat$year != max(bm.dat$year),]
      
      # Now make the figure
      p <- ggplot(bm.dat,aes(x=as.factor(year),y=biomass/1000,fill=type)) + geom_boxplot(position="dodge") + ylab("Fully recruited biomass (Kt)") + xlab("")+ 
        theme_bw(base_size=txt_size) + theme(panel.grid=element_blank(),legend.title = element_blank()) + scale_colour_discrete(name=NULL) + scale_x_discrete(breaks = seq(min(yrs),max(yrs),by=1))
      if(title == T) p <- p + ggtitle("Biomass based on modelled growth rate")
      if(graphic != 'screen') ggsave(paste0("pred_eval_boxplot_",max(years),"_",bank,"_modelled_growth.",graphic),path = path,width = wd, height = ht) 
      if(graphic=="screen")  windows(wd,ht) ; plot(p)
    } # end if(any(growth %in% c("modelled")))
    
    if(any(growth %in% c("realized")))
    {
      # Got it working, woot woot!!
      num.years <- length(out.realized)
      proj.bm <- NULL
      mod.bm <- NULL
      for(i in 1:num.years)
      {
        proj.bm[[names(out.modelled)[i]]] <- out.modelled[[i]]$proj.bm.sims[out.modelled[[i]]$proj.bm.sims > quantile(out.modelled[[i]]$proj.bm.sims,0.1) &
                                                                              out.modelled[[i]]$proj.bm.sims < quantile(out.modelled[[i]]$proj.bm.sims,0.9)]
        tmp <- out.modelled[[i]]$bm.sims[,ncol(out.modelled[[i]]$bm.sims)]
        mod.bm[[names(out.modelled)[i]]] <- tmp[tmp > quantile(tmp,0.1) & tmp < quantile(tmp,0.9)]
      } # for (i in 1:num.years)
      proj.bm <- melt(proj.bm,value.name = "bm")
      names(proj.bm) <- c("bm","year")
      mod.bm <- melt(mod.bm,value.name = "bm")
      names(mod.bm) <- c("bm","year")
      proj.bm <- data.frame(biomass = proj.bm$bm,year = as.numeric(proj.bm$year)+1,type = "projected")
      mod.bm <- data.frame(biomass = mod.bm$bm,year = as.numeric(mod.bm$year),type = "actual")
      bm.dat <- rbind(proj.bm,mod.bm)
      # Because we don't have data for both the projection and model in the 1st and final year lets toss those...
      bm.dat <- bm.dat[bm.dat$year != min(bm.dat$year) & bm.dat$year != max(bm.dat$year),]
      
      
      # Now make the figure
      p <- ggplot(bm.dat,aes(x=as.factor(year),y=biomass/1000,fill=type)) + geom_boxplot(position="dodge") + ylab("Fully recruited biomass (Kt)") + xlab("")+ 
        theme_bw(base_size=txt_size) + theme(panel.grid=element_blank(),legend.title = element_blank()) + scale_colour_discrete(name=NULL) + scale_x_discrete(breaks = seq(min(yrs),max(yrs),by=1))
      if(title == T) p <- p + ggtitle("Biomass based on realized growth rate")
      
      if(graphic != 'screen') ggsave(paste0("pred_eval_boxplot_",max(years),"_",bank,"_realized_growth..",graphic),path = path,width = wd, height = ht) 
      if(graphic=="screen")  windows(wd,ht) ; plot(p)
    } # end if(any(growth %in% c("modelled")))
    
    
  } # end if("box" %in% plot)
  
  # Grab either the modelled or realized data for this little comparison, if we have the modelled data we use that as it is 
  # really the more interesting question here
  
  if(any(growth %in% "modelled")) out2 <- modelled.dat
  else out2 <- realized.dat
  # Here is some information that is of interest I think...
  # Get the model estimates, this sorts it by first year to last year
  ests <- out2$biomass[out2$type== "actual"][order(out2$year[out2$type == "actual"])]
  preds <- out2$biomass[out2$type == "projected"][order(out2$year[out2$type == "actual"])]
  # Get the difference between the predictions and the model estiamtes
  resids <- preds-ests
  # Same thing but the ratio.
  prec<-preds/ests
  # When we over project how bad is it?
  print(paste("Mean over-projection",round(mean(prec[prec>1]),2),"times estimate"))
  # When we under project how bad is it?
  print(paste("Mean under-projection",round(mean(prec[prec<1]),2),"times estimate"))
  # Where is the typical projection landing
  print(paste("Mean projection",round(mean(prec),2),"times estimate"))
  # How far from the actual biomass are our projections on average (in terms of percetage differences.)
  print(paste("Mean residual =",round(mean(abs(resids))/mean(ests)*100,2),"%"))
  # Make a table of these results.
  tab1<-data.frame(Year=sort(out2$year[out2$type == "actual"]), Residual=resids, Proportion=prec)
  return(list(pred.eval.summary = tab1)) # Return the above information as a table.
} # end function