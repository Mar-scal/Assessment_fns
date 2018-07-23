####  Commented by DK starting on July 28, 2015.  PEDStrata function is the workhorse of the PEDStrata package
####  It is actually relatively straightforward once the data are pre-processed.  Basically this returns
####  The commerical sized survey catch (yhi), strata ID's, number of towable units(Nh), proportion of towable units in each strata (Wh)
####  and the number of tows in each strata (nh), see ??PEDstrata for package details.
####
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "survey.dat.r"
##  2:  Many others I am sure
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################

# Arguments

#data.obj:  The raw data containing strata ID's and possibly catch information for each tow.  
#            One column in this data should have same name as 'strata.name' argument
#strata.group: The Strata ID + towable area of the strata.
#strata.name: The colunn name in 'data.obj' from which to extract the strata
#catch: The total commerical catch for each tow in the data.obj.
#Subset: Subset the data.  If so define how (.e.g. something like data.obj$year == 2012)



function (data.obj, strata.group, strata.name, catch, Subset) 
{
  # Subset the data
  if (!missing(Subset)) 
    {
      data.obj <- data.obj[Subset, ]
      catch <- catch[Subset]
    } # end if (!missing(Subset)) 
  # Pull out the Strata numbers from the data (same as data.obj$STRATA.ID if strata.name ="STRATA_ID")
  obj.Strata <- data.obj[[strata.name]]
  # Only look at strata that are in both our data and our strata definition.
  s.group <- is.element(strata.group$Strata, unique(obj.Strata))
  # subset our strata based on above
  s.group.Strata <- strata.group$Strata[s.group]
  # Extract the number of towable units in each strata
  s.group.NH <- strata.group$NH[s.group]
  # Again from our data only look at rows that are in both our data and our strata definition
  s.obj <- is.element(obj.Strata, s.group.Strata)
  # Subset the catch and data based on the above.
  catch <- catch[s.obj]
  data.obj <- data.obj[s.obj, ]
  # turn the strata into factors
  obj.Strata <- factor(obj.Strata[s.obj], levels = s.group.Strata)
  # split the catch into a list with catch in each strata level being a different level in the list.
  yhi <- split(catch, obj.Strata)
  # Determine how many tows were in each strata.
  nh <- as.vector(sapply(yhi, length))
  # Return the results, yhi is the catch in each strata, Strata is the strata levels, Nh is number of towable units in a strata
  # Wh is proportion of towable area in each strata and nh is the number of tows in each strata.
  res <- list(yhi = yhi, Strata = s.group.Strata, Nh = s.group.NH, 
              Wh = s.group.NH/sum(s.group.NH), nh = nh)
  class(res) <- "PEDstrata"
  return(res)
} # end function