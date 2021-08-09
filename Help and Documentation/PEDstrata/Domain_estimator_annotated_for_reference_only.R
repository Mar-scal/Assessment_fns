####  Commented by DK starting in July of 2021.  This function is found in the BIOSurvey2 package
####  This code is made to deal with changes in the survey domain, origins of this are from
#### Sarndal, C. E., Swenson, B., and Wretman, J. 1992. Model assisted survey sampling. Springer, New York.
#### But Stephen has also documented this here 
#    Y:\References\Survey_design\Smith 2006 - Incorporating bottom type information into survey estimates of sea scallop -ICES CM 2006-I-01.pdf
#### 
################################################################################################################

# Arguments

# x
# Strata
# Domain
# strata.obj
# domain.obj
# Species


# You can play around with this using Sable using this code.

# Find survey results from 20189
#survey.info <- read.csv("Y:/Offshore/Assessment/data/Survey_data/survey_information.csv", header=T,stringsAsFactors = F)
# strata.areas <- survey.info %>% dplyr::filter(label == "Sab") %>% dplyr::select(Strata_ID,towable_area,startyear)
# orig.old <- strata.areas$Strata_ID[strata.areas$startyear == 1900]
# orig.new <- strata.areas$Strata_ID[strata.areas$startyear == 2018]
# # If you want you can remove some strata areas to play around with it.
# strata.areas <- strata.areas %>% dplyr::filter(!(Strata_ID %in% c(501) & startyear %in% c(1900)))
# unique.old <- strata.areas$Strata_ID[strata.areas$startyear == 1900]
# unique.new <- strata.areas$Strata_ID[strata.areas$startyear == 2018]
# # This is totally not what the data is, but just making up a functional toy example.
# num <- data.frame(pre = rlnorm(100,3,1), 
#                   STRATA.ID.NEW=round(runif(100,min(unique.new),max(unique.new))), 
#                   STRATA.ID.OLD=round(runif(100,min(orig.old),max(orig.old)))) # You can change this to have more/less/or same number of strata...
# num$STRATA.ID.NEW  <- paste0(num$STRATA.ID.NEW, "_2.0")
# 
# HSIstrata.obj <- data.frame(Strata=strata.areas[,1], strata.id=strata.areas[,1], NH=strata.areas[,2], startyear=strata.areas[,3])[order(strata.areas[,1]),]
# strata.obj <- HSIstrata.obj[HSIstrata.obj$startyear == min(unique(HSIstrata.obj$startyear)),]
# domain.obj <- HSIstrata.obj[HSIstrata.obj$startyear == max(unique(HSIstrata.obj$startyear)),]
# 
# # also need to make the strata names different from each other. So I'm just making up a new convention for the new strata.
# domain.obj$Strata <- paste0(domain.obj$Strata, "_2.0")
# domain.obj$strata.id <- paste0(domain.obj$strata.id, "_2.0")
# 
# # need to change the names of strata.obj and domain.obj
# names(strata.obj)[1] <- "STRATA.ID.OLD"
# names(domain.obj)[1] <- "STRATA.ID.NEW"
# 
# 
# Domain.est(x = num, Strata="STRATA.ID.OLD",
#                        Domain="STRATA.ID.NEW", strata.obj=strata.obj,
#                        domain.obj=domain.obj,
#                        Species = "pre")
# x <- num
# Strata="STRATA.ID.OLD"
# Domain="STRATA.ID.NEW" 
# strata.obj=strata.obj
# domain.obj=domain.obj
# Species = "pre"

Domain.est <- function (x, Strata, Domain, strata.obj, domain.obj, Species) 
{
  #browser()
  # The number of strata in your original design
  H <- length(strata.obj[, Strata])
  # The number of strata in your new design
  D <- length(domain.obj[, Domain])
  # The unique strata in your new design
  unique.domain <- unique(domain.obj[, Domain])
  # The strata ID's for each tow in your original design
  STrata <- x[, Strata]
  # The starta ID's for each tow in your new design
  DOmain <- x[, Domain]
  #
  SPecies <- x[, Species]
  # If there are any tows in the original strata that aren't in the new domain delete them
  if (any(is.na(STrata))) {
    x <- x[!is.na(STrata)]
    DOmain <- x[, Domain][!is.na(STrata)]
  }
  # If there are any tows in the new domain that aren't in the original strata delete them
  if (any(is.na(DOmain))) {
    x <- x[!is.na(DOmain)]
    STrata <- STrata[!is.na(DOmain)]
    DOmain <- DOmain[!is.na(DOmain)]
  }
  # Inital some variables
  nsdh <- matrix(0, H, D)
  ysdh <- matrix(0, H, D)
  ss.ysdh <- matrix(0, H, D)
  # Loop through all of the original strata
  for (h in 1:H) {
    temp.strata <- STrata == strata.obj[, Strata][h]
    temp.domain <- DOmain[temp.strata]
    # if temp.domain are factors remove the factor levels...
    if (is.factor(temp.domain)) 
      temp.domain <- temp.domain[, drop = TRUE]
    # Make a list of the tows in strata h' with the list name being the new domain these tows are located in.
    yk <- split(SPecies[temp.strata], temp.domain)
    # This makes sure the list from the previous step includes only the list of new domains
    yk <- yk[is.element(names(yk), unique.domain)]
    # Logical call to identify which of the group of domain names are in these h tows.
    names.domain <- is.element(unique.domain, names(yk))
    # If there are tows within at least one of the domains then we do soem calculations.
    if (sum(names.domain) > 0) {
      # Make a sum of squares term, this is the sum of squares that forms the first part of the numerator of equation 2 in Smith 2006 ICES CM paper.
      # This is calculted for each of the strata areas we have data for.
      ss.ysdh[h, match(names(yk), unique.domain)] <- sapply(yk, 
                                                            function(x) {
                                                              sum((x - mean(x))^2)
                                                            })
      # This is simply the sum of the yk's (biomass/numbers/etc) for tows that are in both domains, I believe to be part of numerator in equation 1 of Smith ICES CM paper
      ysdh[h, match(names(yk), unique.domain)] <- sapply(yk, 
                                                         sum)
      # This is the number of tows in the new domain that is linked to the above
      nsdh[h, match(names(yk), unique.domain)] <- sapply(yk, 
                                                         length)
    }
  }
  # THis is the total number of tows in each of the new survey domains, this is nh I believe
  nhI <- apply(nsdh, 1, sum)
  # This gives us a matrix of our y values in the new survey domain and identifies what survey strata they were originally tied to.
  dimnames(ysdh) <- dimnames(nsdh) <- list(strata.obj[, Strata], 
                                           unique.domain)
  # This would remove any y's if the nh for that row was 0.
  ysdh <- ysdh[nhI != 0, ]
  ss.ysdh <- ss.ysdh[nhI != 0, ]
  nsdh <- nsdh[nhI != 0, ]
  # This gives us an index matrix, it's a 1 if there is a value in nsdh, and a 0 otherwise.
  nsdhI <- 1 * (nsdh != 0)
  # This is the same as nhI when there are tows everywhere, based on above I suspect it drops 0 cases.
  nh <- apply(nsdh, 1, sum)
  # This uses the above nsdhI, it retuns nsdh in denominator, unless nsdhI is a 0, then it replaces the value with a 0.
  # Then we calculate y for each of the combos of the new domain and original survey strata. This is y_bar_dh in Equation 1 of Smith 2006 I believe
  ybsdh <- ysdh/(nsdh + (1 - nsdhI))
  # This gets the towable area for each of the original strata and drops any in which nh is 0.
  Nh <- strata.obj$NH[nhI != 0]
  # This gets the towable area in the new domain
  Nd <- domain.obj$NH
  # This is division of fh in Equation 2 of Smith 2006. It is the proportion of the towable area that was actually towed
  fh <- nh/Nh 
  # This is the ndh/nh term in/after equation 2 in Smith 2006. Represents the proportion of the tows in the new domain that occurred in each of the old survey strata.
  pdh <- sweep(nsdh, 1, nh, "/")
  # This is Nhatd of equation 2 in the denominator, als think this is the denominator of equation 1.
  Nhatd <- apply(sweep(pdh, 1, Nh, "*"), 2, sum)
  # I think this is the mean estimate for each domain.
  ybd <- apply(sweep(ysdh, 1, 1/fh, "*"), 2, sum)/Nhatd
  # Front part of the variance equation.
  var.1 <- sweep(ss.ysdh, 1, (Nh^2 * (1 - fh))/(nh * (nh - 1)), "*")
  # Back half of numerator in Equation 2 of Smith 2006
  var.2 <- nsdhI * (nsdh * (1 - pdh) * (sweep(ybsdh, 2, ybd, 
                                              "-"))^2)
  # The rest of the necessary calculation for equation 2 where we get the bit you need to multiply by.
  var.2 <- sweep(var.2, 1, (Nh^2 * (1 - fh))/(nh * (nh - 1)), "*")
  # Finally we put var.1 and var.2 together to get the variance estimates for mean estimates for each domain, 
  # but note this is using Nd^2 not Nhatd^2 like it looks like you'd want to do in equation 2
  var.ybd <- (apply(var.1, 2, sum, na.rm = T) + (var.2 <- apply(var.2, 
                                                                2, sum, na.rm = T)))/Nd^2
  names(ybd) <- names(var.2) <- names(var.ybd) <- unique.domain
  # I am entirely unclear what the var.diffdomain term is in the below out object.
  out <- list(Strata = Strata, Domain = Domain, ybd = ybd, 
              var.ybd = var.ybd, var.diffdomain = var.2/Nhatd^2, se.ybd = sqrt(var.ybd), 
              Nd = Nd, nsdh = nsdh, nh = nh, pdh = pdh, Nh = Nh, domain.obj = domain.obj)
  class(out) <- "domain.est"
  out
}
