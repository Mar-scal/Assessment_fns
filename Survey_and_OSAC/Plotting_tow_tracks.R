# Here is an excting new function to plot the tow tracks, the official data is at the time of creation stored in the "Alan" directory on the ESS, the files
# used here are copied from that directory into the "Data/Tow_tracks" directory used for assessments so that each year can be easily plotted.
# This function should work back until 2014, we don't have data stored properly currently for data before this time.

# DK created August 2018
# DK Jan 2021 updated direct_fns

##########################
# ARGUMENTS

#bk:          The bank(s) of interest, options include bk = "summer", bk = "spring", with the default being all banks bk = 'all".  Note these options exclude BBs.
#             To run for a specific bank your options are any combination of  bk = c("Mid","Sab","Ger","BBn","BBs","GB","GBa","GBb") 
#year:        What year do you want to run.  Default is the current year 
#extras:      Do you want to run the extra tow locations?  These are saved to a different file and are stand alone figures as well. T/F, Default  = T,.
#export:      Do you want to export the tow bearing/distance coefficient data.  T/F, Default = F, 
#fig:         Do you want to produce a figure.  If so set to one of "png", "pdf", or "screen".  Default is NULL which doesn't make a figure,
#file.loc:    Do you have a specific folder location you are looking for for regular survey tows.  Default = NULL which selects files in the ...Tow_tracks/Bank folder as required.
#extras.loc:  Do you have a specific folder you are looking for extra industry tows in.  Default = NULL which will look in the file.loc folder for any extras if extras = T
#compare:     Do you want to get the survey tow tracks for last year.  These will be overlain with current year tows if producing a figure. T/F, default = T
#labels:      Do you want to add the tow numbers to the figure (if it is being produced).  T/F, default = T
#direct:      The directory to work out of. default = "Y:/Offshore scallop/Assessment/"
#direct_fns:  Where you are sourcing the functions from.  Default is missing which will point to github.
###########################



tow.track.plots <- function(bk = c("Mid","Sab","Ger","BBn","BBs","GB","GBa","GBb"), year = format(Sys.Date(),"%Y"),extras = T, export = F, fig=NULL, 
                        file.loc = NULL,extras.loc = NULL,compare =F,labels = F,direct, direct_fns)
{

if(bk == 'spring' || bk == 'all') cat("Hallow wonderful human!  Note that bk='spring' and bk = 'all' doesn't include BBs.\n If there was a survey on BBs this year please run the function with bk = 'BBs'.  Thanks for being you! \n")
if(bk == 'all') bk <- c("Mid","Sab","Ger","BBn","GB")
if(bk == 'spring') bk <- c("Mid","Sab","Ger","BBn","GB")
if(bk == 'summer') bk <- c("GBa","GBb")

# Load functions 
if(missing(direct_fns))
{
  funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/getdis.r",
            "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r",
            "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/ScallopMap.r")
  # Now run through a quick loop to load each one, just be sure that your working directory is read/write!
dir <- tempdir()
for(fun in funs) 
{
  temp <- dir
  download.file(fun,destfile = paste0(dir, "\\", basename(fun)))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}# end for(fun in funs)
}# end if(missing(direct_fns))  

if(!missing(direct_fns))
{
source(paste0(direct_fns,"Survey_and_OSAC/getdis.r"))
source(paste0(direct_fns,"Survey_and_OSAC/convert.dd.dddd.r"))
source(paste0(direct_fns,"Maps/ScallopMap.r"))
} # end if(!missing(direct_fns))

library(reshape2)
library(PBSmapping)


num.banks <- length(bk)
dist.calcs <- NULL
ly.dist.calcs <- NULL
surv.names <- NULL


# Now loop through each bank of interested
for(i in 1:num.banks)
{
  # The file locations need a special name to be consistent with the folder locations used in the survey.
  if(bk[i] == "Mid") bank <- "Middle"
  if(bk[i] == "Sab") bank <- "Sable"
  if(bk[i] == "Ger") bank <- "German"
  if(bk[i] == "GB")  bank <- "GBmon"
  if(bk[i] == "GBa") bank <- "GBa"
  if(bk[i] == "GBb") bank <- "GBb"
  if(bk[i] == "BBs") bank <- "BBS"
  if(bk[i] == "BBn") bank <- "BBN"
  # The original files were stored in Y:/Alan/.. in the survey specific folders.  
  # Copies have been place in "Y:/Offshore scallop/Assessment/Data/Tow_tracks" with the hope we could start storing these here, or somewhere
  # other than the Alan folder on the ESS.
  if(is.null(file.loc)) loc.tows <- paste0(direct,"Data/Tow_tracks/",year,"/",bank,"/")
  if(!is.null(file.loc)) loc.tows <- file.loc
  
  # The files to chose from...
  files <- list.files(loc.tows)
  # This picks out everything that is a number and doesn't start with a 9 (which are for extra stations), it converts to a number and drops anything that isn't numeric
  tow.nums <- as.numeric(substr(files[which(as.numeric(substr(files,1,1)) < 9)],1,3))
  # Do the calcs.
  dist.calcs[[bank]] <- dist.coef(tow.nums,path=loc.tows,w=c(1:10,9:1),rule=8,smooth=T,plt=F, meh=1000)

  # Do you want to compare tows between this year and the last survey year?
  if(compare == T) 
  {
    # Now go back in time until you find a previous folder with the data you want...
    for(k in 2014:(year-1)) surv.names[[as.character(k)]] <- list.files(paste0(direct,"Data/Tow_tracks/",k))
    # This is gross but the best my brain to do after a long weekend...
    tmp <- as.data.frame(do.call("rbind",surv.names))
    tmp$year <- row.names(tmp)
    tmp <- melt(tmp, "year")
    last.year <- max(tmp$year[tmp$value == bank])
    ly.tows <- paste0(direct,"Data/Tow_tracks/",last.year,"/",bank,"/")
    
    # The files to chose from...
    ly.files <- list.files(ly.tows)
    # This picks out everything that is a number and doesn't start with a 9 (which are for extra stations), it converts to a number and drops anything that isn't numeric
    ly.tow.nums <- as.numeric(substr(ly.files[which(as.numeric(substr(ly.files,1,1)) < 9)],1,3))
    # Now do the calcs for the last year of survey data.
    ly.dist.calcs[[bank]] <- dist.coef(ly.tow.nums,path=ly.tows,w=c(1:10,9:1),rule=8,smooth=T,plt=F, meh=1000)
  } # end if(compare == T) 
  
  # If you want extra tows and you haven't specified the location of those tows then we pull out any/all extras that are found
  # in the same location as the regular tows for a bank.
  if(extras == T && is.null(extras.loc))
  {
    # List the files in the folder
    all.files <- list.files(loc.tows)
    # Now grab any extra tows on a bank, this assumes that the extras start with the number 9, i.e. they are 900 series tows.
    extra.tows <- as.numeric(substr(all.files[grep("^9",all.files)],1,3))
    # If there are extra tows in the folder do em up....
    
    if(length(extra.tows) >0)
    {
      # Now get the data for the extras and save to a csv if requested
      dist.calcs[[paste0(bank,"_extras")]] <- dist.coef(extra.tows,path=loc.tows,w=c(1:10,9:1),rule=8,smooth=T,plt=F, meh=1000)
      if(export == T) write.csv(file = paste0(loc.tows,bank,".dis.",year,".extras.csv"),data.frame(bank = bank, dist.calcs[[paste0(bank,"_extras")]][[1]]))
    } # end if(length(extra.tows) >0)
    if(length(extra.tows) == 0) cat("Hallow fellow surveyist! Just a note that in",year, bank, "doesn't have any extra tows.\n")
  } # end if(extras == T && is.null(extras.loc))
  
  if(export == T) write.csv(file = paste0(loc.tows,bank,".dis.",year,".csv"),data.frame(bank = bank, dist.calcs[[bank]][[1]]))
} # end for(i in 1:num.banks)
                         

if(extras == T && !is.null(extras.loc))
{
        # List the files in the folder
      all.files <- list.files(extras.loc)
      # Now grab any extra tows on a bank, this assumes that the extras start with the number 9, i.e. they are 900 series tows.
      extra.tows <- as.numeric(substr(all.files[grep("^9",all.files)],1,3))
      if(length(extra.tows) >0)
      {
        # Now get the data for the extras and save to a csv if requested
        dist.calcs[["extras"]] <- dist.coef(extra.tows,path=extras.loc,w=c(1:10,9:1),rule=8,smooth=T,plt=F, meh=1000)
        if(export == T) write.csv(file = paste0(extras.loc,"extras.dis.",year,".csv"),data.frame(bank = "extras", dist.calcs[["extras"]][[1]]))
      } # end if(length(extra.tows) >0)
      # output a mesage saying you asked for extra tows but the function couln't find any.
      if(length(extra.tows) == 0) 
      {
        cat("Hallow again fellow surveyist! Just a note that you have requested the extra tows found in the folder: \n", extras.loc, "\n Sadly I can't find any extras in this folder.\n  Please ensure the extras start with the number 9 as I am expecting them to be 900 series numbers, as they have been in the past!\n Note: Do not mistake temptation for opportunity :-D \n")      
      } # end if(length(extra.tows) == 0) 
} # end if(extras == T && !is.null(extras.loc))

# If we want to make a figure, let's make a figure....
if(!is.null(fig))
{
 
  num.figs <- length(dist.calcs)
  name.figs <- names(dist.calcs)
  for(j in 1:num.figs)
  {
    loc <- name.figs[j]
    
    # If we are looking at extras then just plot them via their extents.
    y_range <- c(min(dist.calcs[[loc]][[2]]$Y)*.997,max(dist.calcs[[loc]][[2]]$Y))*1.002
    x_range <- c(min(dist.calcs[[loc]][[2]]$X)*1.001,max(dist.calcs[[loc]][[2]]$X))*0.999
    # Now make the figure, automaticaly saving this is slightly awkward as I need to make this line up with naming conventions in our
    # file structure, so here's some ugly if statements...
    if(fig != "screen")
    {
      if(loc == "extras")
      {
        fig = "screen"
        cat("Hope you don't mind another message fellow surveyist! You manually selected the location of the Extra Stations to plot, \n because of this I don't really know where to save the figures so I decided to just printed them to your screen \n you will have to manually save them from there.\n Note: There is no mistake so great as that of being always right :-D \n")
      } # end if(loc == "extras")
    }
      
    if(fig == "screen") windows(11,11)
    if(fig == "pdf") pdf(paste0(direct,year,"/Presentations/Survey_summary/Exploratory_figures/",bk,"/",loc,"_Tow_tracks.pdf"),width=11,height=11)
    if(fig == "png") png(paste0(direct,year,"/Presentations/Survey_summary/Exploratory_figures/",bk,"/",loc,"_Tow_tracks.png"),width=11,height=11,units="in",res = 400)     
    ScallopMap(ylim = y_range,xlim=x_range,title = paste("Survey Stations",loc,year))
    addLines(dist.calcs[[loc]][[2]],col="tan",lwd=4)       
    # Add last years tows if we want, but don't ever do that for the extra stations.
    if(compare == T && length(grep("extras",loc)) == 0) addLines(ly.dist.calcs[[loc]][[2]],col="firebrick",lwd=1.5)
    # Add the tow numbers if someone wants those
    if(labels== T) text(convert.dd.dddd(dist.calcs[[loc]][[1]]$slon),convert.dd.dddd(dist.calcs[[loc]][[1]]$elat),
                            dist.calcs[[loc]][[1]]$tow,cex=0.5)
    if(fig != "screen") dev.off()
                                 
  } # end for(j in 1:num.figs
} # End if(!is.null(fig))

# Spit out the data in case someone wants to use it.
return(list(dist.calcs,ly.dist.calcs))
} # end function
