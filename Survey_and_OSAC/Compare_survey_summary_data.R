### method to compare survey summary Rdata files
### Use this template to compare two different sets of Survey Summary data.
### You may have to change year, or comment out certain sections depending on what you're comparing. This was a bit quirky because I was running it for Banquereau flat files at one point.

################### Checking 2018 Survey summary run in v3.0.1 beta against results from last year (v2.1)

load("C:/Documents/Offshore scallop/Assessment/Data/Survey_data/2019/Survey_summary_output/testing_results_BetaTesting_LE09.Rdata")
#load("C:/Documents/Offshore scallop/Assessment/Data/Survey_data/2012/Survey_summary_output/testing_results_Banspring3.Rdata")
real_surv.dat1 <- surv.dat
real_all.surv.dat1 <- all.surv.dat
real_mw.dat.all1 <- mw.dat.all
real_cf.data <- cf.data
real_bank.dat1 <- bank.dat

#load("C:/Documents/Offshore scallop/Assessment/Data/Survey_data/2018/Survey_summary_output/testing_results_BetaTesting_GB20182.Rdata")
load("C:/Documents/Offshore scallop/Assessment/Data/Survey_data/2019/Survey_summary_output/testing_results_SCALOFF_LE09.RData")
new_surv.dat1 <- surv.dat
new_all.surv.dat1 <- all.surv.dat
new_mw.dat.all1 <- mw.dat.all
new_cf.data <- cf.data
new_bank.dat1 <- bank.dat

## do some joins etc to make sure these match. also dig in to the list of issues in notebook. 
require(compareDF)
towdat <- list()
towdat_raw <- list()
banktowdat_raw <- list()
hfdat <- list()
hfdat_raw <- list()
bankhfdat_raw <- list()
mwdat <- list()
mwdat_raw <- list()
stratdat <- list()
# new_surv.dat1[["Ban"]] <- NULL
# new_surv.dat1[["BanIce"]] <- NULL
### adjust the sections and years in the loop to accommodate the comparison that you want to do!
for(i in 1:length(names(new_surv.dat1))) {
  print(names(new_surv.dat1)[i])
  new_surv.dat[[i]] <- new_surv.dat1[[i]][new_surv.dat1[[i]]$year==2019,]
  real_surv.dat[[i]] <- real_surv.dat1[[names(new_surv.dat1)[i]]][real_surv.dat1[[names(new_surv.dat1)[i]]]$year==2019,]
  if("CFse.fit" %in% names(new_surv.dat[[i]])) new_surv.dat[[i]] <- select(new_surv.dat[[i]], -CFse.fit)
  new_surv.dat[[i]]$row <- paste0(new_surv.dat[[i]]$ID, ".", new_surv.dat[[i]]$state)
  real_surv.dat[[i]]$row <- paste0(real_surv.dat[[i]]$ID, ".", real_surv.dat[[i]]$state)
  
  # new_bank.dat <- new_bank.dat1[[i]][new_bank.dat1[[i]]$year==2018 & new_bank.dat1[[i]]$bank == names(new_surv.dat1)[i],]
  # real_bank.dat <- real_bank.dat1[[names(new_surv.dat1)[i]]][real_bank.dat1[[names(new_surv.dat1)[i]]]$year==2018 & real_bank.dat1[[names(new_surv.dat1)[i]]]$bank == names(new_surv.dat1)[i],]
  # new_bank.dat$row <- paste0(new_bank.dat$year, ".", new_bank.dat$cruise, ".", new_bank.dat$tow, ".", new_bank.dat$state)
  # real_bank.dat$row <- paste0(real_bank.dat$year, ".", real_bank.dat$cruise, ".", real_bank.dat$tow, ".", real_bank.dat$state)
  
  new_all.surv.dat <- new_all.surv.dat1[new_all.surv.dat1$year==2019 & new_all.surv.dat1$bank == names(new_surv.dat1)[i],]
  real_all.surv.dat <- real_all.surv.dat1[real_all.surv.dat1$year==2019 & real_all.surv.dat1$bank == names(new_surv.dat1)[i],]
  new_all.surv.dat$row <- paste0(new_all.surv.dat$year, ".", new_all.surv.dat$cruise, ".", new_all.surv.dat$tow, ".", new_all.surv.dat$state)
  real_all.surv.dat$row <- paste0(real_all.surv.dat$year, ".", real_all.surv.dat$cruise, ".", real_all.surv.dat$tow, ".", real_all.surv.dat$state)

  if(!names(new_surv.dat1)[i] == "BanIce") {
    new_mw.dat.all <- new_mw.dat.all1[[names(new_surv.dat1)[i]]][new_mw.dat.all1[[names(new_surv.dat1)[i]]]$year==2019,]
    real_mw.dat.all <- real_mw.dat.all1[[names(new_surv.dat1)[i]]][real_mw.dat.all1[[names(new_surv.dat1)[i]]]$year==2019,]
    new_mw.dat.all$row <- paste0(new_mw.dat.all$ID, ".", new_mw.dat.all$sh)
    real_mw.dat.all$row <- paste0(real_mw.dat.all$ID, ".", real_mw.dat.all$sh)
  }
  
  towdat[[i]] <- compare_df(df_new = arrange(new_surv.dat[[i]][,
                                                       c("ID", "tow", "year", "cruise", "bank", "slat", "slon", "elat", "elon", 
                                                         "depth", "state", "random",  "lon", "lat", "surv.bank", "row")], row), 
                            df_old = arrange(real_surv.dat[[i]][,
                                                        c("ID", "tow", "year", "cruise", "bank", "slat", "slon", "elat", "elon", 
                                                          "depth", "state", "random","lon", "lat", "surv.bank", "row")], row), 
                            "row", stop_on_error = F)
  
  # banktowdat_raw[[i]] <- compare_df(df_new = new_bank.dat[,
  #                                                         c("tow", "year", "cruise", "bank", "slat", "slon", "elat", "elon",
  #                                                           "depth", "state", "random", "lon", "lat", "surv.bank", "row")],
  #                               df_old = real_bank.dat[,
  #                                                          c("tow", "year", "cruise", "bank", "slat", "slon", "elat", "elon",
  #                                                            "depth", "state", "random", "lon", "lat", "surv.bank", "row")],
  #                               "row", stop_on_error = F)

  towdat_raw[[i]] <- compare_df(df_new = new_all.surv.dat[,
                                                       c("tow", "year", "cruise", "bank", "date", "slat", "slon", "elat", "elon",
                                                         "depth", "state", "random", "month","survey", "lon", "lat", "surv.bank", "row")],
                            df_old = real_all.surv.dat[,
                                                        c("tow", "year", "cruise", "bank", "date", "slat", "slon", "elat", "elon",
                                                          "depth", "state", "random", "month","survey", "lon", "lat", "surv.bank", "row")],
                            "row", stop_on_error = F)

  hfdat[[i]] <- compare_df(df_new = arrange(new_surv.dat[[i]][,
                                                      c(paste0("h", seq(5,200,5)), "row")], row), 
                           df_old = arrange(real_surv.dat[[i]][,
                                                       c(paste0("h", seq(5,200,5)), "row")], row), "row", stop_on_error = F)
  
  # bankhfdat_raw[[i]] <- compare_df(df_new = new_bank.dat[,
  #                                                        c(paste0("h", seq(5,200,5)), "row")],
  #                              df_old = real_bank.dat[,
  #                                                         c(paste0("h", seq(5,200,5)), "row")], "row", stop_on_error = F)

  hfdat_raw[[i]] <- compare_df(df_new = new_all.surv.dat[,
                                                         c(paste0("h", seq(5,200,5)), "row")],
                               df_old = real_all.surv.dat[,
                                                          c(paste0("h", seq(5,200,5)), "row")], "row", stop_on_error = F)

  mwdat[[i]] <- compare_df(df_new = arrange(new_surv.dat[[i]][,
                                                      c("CF", "CFh", "l.bar", "w.bar", "meat.count", "row")], row), 
                           df_old = arrange(real_surv.dat[[i]][,
                                                       c("CF", "CFh", "l.bar", "w.bar", "meat.count", "row")], row), "row", stop_on_error = F)
  
  if(!names(new_surv.dat1)[i] == "BanIce") {
    mwdat_raw[[i]] <- compare_df(df_new = new_mw.dat.all[,
                                                         c("sh", "wmw", "row")],
                                 df_old = real_mw.dat.all[,
                                                          c("sh", "wmw", "row")], "row", stop_on_error = F)
  }
  
  stratdat[[i]] <- compare_df(df_new = arrange(new_surv.dat[[i]][,
                                                         c("pre", "rec", "com", "tot", "pre.bm", "rec.bm", "com.bm", "tot.bm", "row")], row), 
                              df_old = arrange(real_surv.dat[[i]][,
                                                          c("pre", "rec", "com", "tot", "pre.bm", "rec.bm", "com.bm", "tot.bm", "row")], row), "row", stop_on_error = F)
}


## Ban
names(which(apply(towdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # month
names(which(apply(towdat_raw[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # month
names(which(apply(hfdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(hfdat_raw[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(mwdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # CF, CFh, w.bar, meat.count
#head(mwdat[[1]]$comparison_df) # CF, CFh, w.bar, meat.count
names(which(apply(mwdat_raw[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
names(which(apply(stratdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm

# ## BBs
# names(which(apply(towdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
# names(which(apply(towdat_raw[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
# names(which(apply(hfdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
# names(which(apply(hfdat_raw[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
# names(which(apply(mwdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
# head(mwdat[[2]]$comparison_df) # CF, CFh, w.bar, meat.count
# names(which(apply(mwdat_raw[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
# names(which(apply(stratdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 

## BanIce
names(which(apply(towdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(towdat_raw[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(hfdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(hfdat_raw[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(mwdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # CF, CFh, w.bar, meat.count
head(mwdat[[2]]$comparison_df) # CF, CFh, w.bar, meat.count
names(which(apply(mwdat_raw[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
names(which(apply(stratdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm

## BBn
names(which(apply(towdat[[3]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
names(which(apply(towdat_raw[[3]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(hfdat[[3]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(hfdat_raw[[3]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(mwdat[[3]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # CF, CFh, w.bar, meat.count
head(mwdat[[3]]$comparison_df) # CF, CFh, w.bar, meat.count
names(which(apply(mwdat_raw[[3]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
names(which(apply(stratdat[[3]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # com.bm, tot.bm

## Ger
names(which(apply(towdat[[4]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(towdat_raw[[4]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(hfdat[[4]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(hfdat_raw[[4]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(mwdat[[4]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # CF, CFh, w.bar, meat.count
head(mwdat[[4]]$comparison_df) # CF, CFh, w.bar, meat.count
names(which(apply(mwdat_raw[[4]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
names(which(apply(stratdat[[4]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm

## Mid
names(which(apply(towdat[[5]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(towdat_raw[[5]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(hfdat[[5]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(hfdat_raw[[5]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(mwdat[[5]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # CF, CFh, w.bar, meat.count
head(mwdat[[5]]$comparison_df) # CF, CFh, w.bar, meat.count
names(which(apply(mwdat_raw[[5]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
names(which(apply(stratdat[[5]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm

## Sab
names(which(apply(towdat[[6]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(towdat_raw[[6]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(hfdat[[6]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(hfdat_raw[[6]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(mwdat[[6]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # CF, CFh, w.bar, meat.count
head(mwdat[[6]]$comparison_df) # CF, CFh, w.bar, meat.count
names(which(apply(mwdat_raw[[6]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
names(which(apply(stratdat[[6]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm

## GB
names(which(apply(towdat[[7]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(towdat_raw[[7]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
names(which(apply(hfdat[[7]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(hfdat_raw[[7]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
names(which(apply(mwdat[[7]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # CF, CFh, w.bar, meat.count
head(mwdat[[7]]$comparison_df) # CF, CFh, w.bar, meat.count
names(which(apply(mwdat_raw[[7]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
names(which(apply(stratdat[[7]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm




# ## GBb
# names(which(apply(towdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
# names(which(apply(towdat_raw[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
# names(which(apply(hfdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
# names(which(apply(hfdat_raw[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
# names(which(apply(mwdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # CF, CFh, w.bar, meat.count
# head(mwdat[[1]]$comparison_df) # CF, CFh, w.bar, meat.count
# names(which(apply(mwdat_raw[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
# names(which(apply(stratdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm
# 
# ## GBa
# names(which(apply(towdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
# names(which(apply(towdat_raw[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
# names(which(apply(hfdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
# names(which(apply(hfdat_raw[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # all good
# names(which(apply(mwdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # CF, CFh, w.bar, meat.count
# head(mwdat[[2]]$comparison_df) # CF, CFh, w.bar, meat.count
# names(which(apply(mwdat_raw[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-")))))
# names(which(apply(stratdat[[2]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm

##############
## Ban
names(which(apply(towdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
head(towdat[[1]]$comparison_df) # "chng_type" "date" "slon" "random" "lon"
towdat[[1]]$comparison_df[which(apply(towdat[[1]]$comparison_table_diff[, c("tow", "slon")], 1, function(r) any(r %in% c("+", "-")))),]
towdat[[1]]$comparison_df[which(apply(towdat[[1]]$comparison_table_diff[, c("tow", "date")], 1, function(r) any(r %in% c("+", "-")))),]

names(which(apply(banktowdat_raw[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 

names(which(apply(hfdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # "h60""h65""h70""h75""h120""h135""h160""h165""h170""h175" 
hfdat[[1]]$comparison_df[which(apply(hfdat[[1]]$comparison_table_diff[, c("row", "h120")], 1, function(r) any(r %in% c("+", "-")))),]

names(which(apply(bankhfdat_raw[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # "h60""h65""h70""h75""h120""h135""h160""h165""h170""h175" 
# same as above

names(which(apply(mwdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # "CF"         "CFh"        "l.bar"      "w.bar"      "meat.count"
head(mwdat[[1]]$comparison_df) # CF, CFh, w.bar, meat.count
mwdat[[1]]$comparison_df[which(apply(mwdat[[1]]$comparison_table_diff[, c("row", "CF")], 1, function(r) any(r %in% c("+", "-")))),]
mwdat[[1]]$comparison_df[which(apply(mwdat[[1]]$comparison_table_diff[, c("row", "CFh")], 1, function(r) any(r %in% c("+", "-")))),]

names(which(apply(stratdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm


#######################
## BanIce
names(which(apply(towdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) 
head(towdat[[1]]$comparison_df) # "chng_type" "date" "slon" "random" "lon"
towdat[[1]]$comparison_df[which(apply(towdat[[1]]$comparison_table_diff[, c("tow", "lon")], 1, function(r) any(r %in% c("+", "-")))),]

names(which(apply(hfdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # "h60""h65""h70""h75""h120""h135""h160""h165""h170""h175" 
hfdat[[1]]$comparison_df[which(apply(hfdat[[1]]$comparison_table_diff[, c("row", "h120")], 1, function(r) any(r %in% c("+", "-")))),]

names(which(apply(mwdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # "CF"         "CFh"        "l.bar"      "w.bar"      "meat.count"
head(mwdat[[1]]$comparison_df) # CF, CFh, w.bar, meat.count
mwdat[[1]]$comparison_df[which(apply(mwdat[[1]]$comparison_table_diff[, c("row", "CF")], 1, function(r) any(r %in% c("+", "-")))),]
mwdat[[1]]$comparison_df[which(apply(mwdat[[1]]$comparison_table_diff[, c("row", "CFh")], 1, function(r) any(r %in% c("+", "-")))),]

names(which(apply(stratdat[[1]]$comparison_table_diff, 2, function(r) any(r %in% c("+", "-"))))) # pre.bm, rec.bm, com.bm, tot.bm
stratdat[[1]]$comparison_df[which(apply(stratdat[[1]]$comparison_table_diff[, c("row", "pre.bm")], 1, function(r) any(r %in% c("+", "-")))),]







