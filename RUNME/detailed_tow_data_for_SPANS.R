# For Detailed Tow Data (for SPANS, usually right after Industry report)
year<- 2022
DR <- "DR2022_10"
direct <- "Y:/Offshore/Assessment/"
#load(paste0(direct, "/Data/Survey_data/", year, "/Survey_summary_output/testing_results_SCALOFF_LE10.Rdata"))

#banks <- names(survey.obj)
banks <- c("GB", "BBn", "Sab", "Mid", "Ger")
banks <- c("GBa", "GBb")

cruises <- c("GB", "BB", "SAB", "MID", "GER")
cruises <- c("GBa", "GBb")
#cruises <- c("GBa", "GBb", "BB")

# read in this function
detailed.tow.data <- function(year=2022, DR="DR2022_10", banks=banks, cruises=cruises, un.ID=un.ID, pwd.ID=pwd.ID, db.con="ptran"){

  require(ROracle)
  
  chan <-dbConnect(dbDriver("Oracle"),username=un.ID, password=pwd.ID,db.con)
  
  sh.live <- dbGetQuery(chan, paste0("SELECT CRUISE, YEAR, SURVEY_NAME, MGT_AREA_CD, SPECIES_ID, TOW_NO, TOW_TYPE_ID, DEPTH_F, TOW_DATE, START_LAT, START_LON, END_LAT, END_LON, 
                                   round(bin_0, 2) bin_0,round(bin_5, 2) bin_5,round(bin_10, 2) bin_10,round(bin_15, 2) bin_15,round(bin_20, 2) bin_20,round(bin_25, 2) bin_25,round(bin_30, 2) bin_30,round(bin_35, 2) bin_35,round(bin_40, 2) bin_40,
                                   round(bin_45, 2) bin_45,round(bin_50, 2) bin_50,round(bin_55, 2) bin_55,round(bin_60, 2) bin_60,round(bin_65, 2) bin_65,round(bin_70, 2) bin_70,round(bin_75, 2) bin_75,round(bin_80, 2) bin_80,round(bin_85, 2) bin_85,
                                   round(bin_90, 2) bin_90,round(bin_95, 2) bin_95,round(bin_100, 2) bin_100,round(bin_105, 2) bin_105,round(bin_110, 2) bin_110,round(bin_115, 2) bin_115,round(bin_120, 2) bin_120,round(bin_125, 2) bin_125,round(bin_130, 2) bin_130,
                                   round(bin_135, 2) bin_135,round(bin_140, 2) bin_140,round(bin_145, 2) bin_145,round(bin_150, 2) bin_150,round(bin_155, 2) bin_155,round(bin_160, 2) bin_160,round(bin_165, 2) bin_165,round(bin_170, 2) bin_170,round(bin_175, 2) bin_175,
                                   round(bin_180, 2) bin_180,round(bin_185, 2) bin_185,round(bin_190, 2) bin_190,round(bin_195,2) bin_195, 'live' state
                                   FROM SCALOFF.OSLIVERES_SS_VW  
                                   WHERE YEAR = ", year))
  
  sh.dead <- dbGetQuery(chan, paste0("SELECT CRUISE, YEAR, SURVEY_NAME, MGT_AREA_CD, SPECIES_ID, TOW_NO, TOW_TYPE_ID, DEPTH_F, TOW_DATE, START_LAT, START_LON, END_LAT, END_LON, 
                                   round(bin_0, 2) bin_0,round(bin_5, 2) bin_5,round(bin_10, 2) bin_10,round(bin_15, 2) bin_15,round(bin_20, 2) bin_20,round(bin_25, 2) bin_25,round(bin_30, 2) bin_30,round(bin_35, 2) bin_35,round(bin_40, 2) bin_40,
                                   round(bin_45, 2) bin_45,round(bin_50, 2) bin_50,round(bin_55, 2) bin_55,round(bin_60, 2) bin_60,round(bin_65, 2) bin_65,round(bin_70, 2) bin_70,round(bin_75, 2) bin_75,round(bin_80, 2) bin_80,round(bin_85, 2) bin_85,
                                   round(bin_90, 2) bin_90,round(bin_95, 2) bin_95,round(bin_100, 2) bin_100,round(bin_105, 2) bin_105,round(bin_110, 2) bin_110,round(bin_115, 2) bin_115,round(bin_120, 2) bin_120,round(bin_125, 2) bin_125,round(bin_130, 2) bin_130,
                                   round(bin_135, 2) bin_135,round(bin_140, 2) bin_140,round(bin_145, 2) bin_145,round(bin_150, 2) bin_150,round(bin_155, 2) bin_155,round(bin_160, 2) bin_160,round(bin_165, 2) bin_165,round(bin_170, 2) bin_170,round(bin_175, 2) bin_175,
                                   round(bin_180, 2) bin_180,round(bin_185, 2) bin_185,round(bin_190, 2) bin_190,round(bin_195,2) bin_195, 'dead' state
                                   FROM SCALOFF.OSDEADRES_SS_VW  
                                   WHERE YEAR = ", year))
  
  mw.sql <- dbGetQuery(chan, paste0("SELECT CRUISE, YEAR, SURVEY_NAME, MGT_AREA_CD, SPECIES_ID, TOW_NO, DEPTH_F, TOW_DATE, START_LAT, START_LON, END_LAT, END_LON, SCALLOP_NUM, SHELL_HEIGHT, WET_MEAT_WGT
                              FROM SCALOFF.OSSAMPLES_SS_VW
                              WHERE YEAR = ", year))
  
  if("BanIce" %in% banks){
    sh.live.ice <- dbGetQuery(chan, paste0("SELECT CRUISE, YEAR, SURVEY_NAME, MGT_AREA_CD, SPECIES_ID, TOW_NO, TOW_TYPE_ID, DEPTH_F, TOW_DATE, START_LAT, START_LON, END_LAT, END_LON, 
                                   round(bin_0, 2) bin_0,round(bin_5, 2) bin_5,round(bin_10, 2) bin_10,round(bin_15, 2) bin_15,round(bin_20, 2) bin_20,round(bin_25, 2) bin_25,round(bin_30, 2) bin_30,round(bin_35, 2) bin_35,round(bin_40, 2) bin_40,
                                   round(bin_45, 2) bin_45,round(bin_50, 2) bin_50,round(bin_55, 2) bin_55,round(bin_60, 2) bin_60,round(bin_65, 2) bin_65,round(bin_70, 2) bin_70,round(bin_75, 2) bin_75,round(bin_80, 2) bin_80,round(bin_85, 2) bin_85,
                                   round(bin_90, 2) bin_90,round(bin_95, 2) bin_95,round(bin_100, 2) bin_100,round(bin_105, 2) bin_105,round(bin_110, 2) bin_110,round(bin_115, 2) bin_115,round(bin_120, 2) bin_120,round(bin_125, 2) bin_125,round(bin_130, 2) bin_130,
                                   round(bin_135, 2) bin_135,round(bin_140, 2) bin_140,round(bin_145, 2) bin_145,round(bin_150, 2) bin_150,round(bin_155, 2) bin_155,round(bin_160, 2) bin_160,round(bin_165, 2) bin_165,round(bin_170, 2) bin_170,round(bin_175, 2) bin_175,
                                   round(bin_180, 2) bin_180,round(bin_185, 2) bin_185,round(bin_190, 2) bin_190,round(bin_195,2) bin_195, 'live' state
                                   FROM SCALOFF.OSLIVERES_ICE_VW  
                                   WHERE YEAR = ", year))
    sh.live <-rbind(sh.live, sh.live.ice)
    sh.dead.ice <- dbGetQuery(chan, paste0("SELECT CRUISE, YEAR, SURVEY_NAME, MGT_AREA_CD,  SPECIES_ID, TOW_NO, TOW_TYPE_ID, DEPTH_F, TOW_DATE, START_LAT, START_LON, END_LAT, END_LON, 
                                   round(bin_0, 2) bin_0,round(bin_5, 2) bin_5,round(bin_10, 2) bin_10,round(bin_15, 2) bin_15,round(bin_20, 2) bin_20,round(bin_25, 2) bin_25,round(bin_30, 2) bin_30,round(bin_35, 2) bin_35,round(bin_40, 2) bin_40,
                                   round(bin_45, 2) bin_45,round(bin_50, 2) bin_50,round(bin_55, 2) bin_55,round(bin_60, 2) bin_60,round(bin_65, 2) bin_65,round(bin_70, 2) bin_70,round(bin_75, 2) bin_75,round(bin_80, 2) bin_80,round(bin_85, 2) bin_85,
                                   round(bin_90, 2) bin_90,round(bin_95, 2) bin_95,round(bin_100, 2) bin_100,round(bin_105, 2) bin_105,round(bin_110, 2) bin_110,round(bin_115, 2) bin_115,round(bin_120, 2) bin_120,round(bin_125, 2) bin_125,round(bin_130, 2) bin_130,
                                   round(bin_135, 2) bin_135,round(bin_140, 2) bin_140,round(bin_145, 2) bin_145,round(bin_150, 2) bin_150,round(bin_155, 2) bin_155,round(bin_160, 2) bin_160,round(bin_165, 2) bin_165,round(bin_170, 2) bin_170,round(bin_175, 2) bin_175,
                                   round(bin_180, 2) bin_180,round(bin_185, 2) bin_185,round(bin_190, 2) bin_190,round(bin_195,2) bin_195, 'dead' state
                                   FROM SCALOFF.OSDEADRES_ICE_VW  
                                   WHERE YEAR = ", year))
    sh.dead <-rbind(sh.dead, sh.dead.ice)
    mw.sql.ice <- dbGetQuery(chan, paste0("SELECT CRUISE, YEAR, SURVEY_NAME, MGT_AREA_CD,  SPECIES_ID, TOW_NO, DEPTH_F, TOW_DATE, START_LAT, START_LON, END_LAT, END_LON, SCALLOP_NUM, SHELL_HEIGHT, WET_MEAT_WGT
                              FROM SCALOFF.OSSAMPLES_ICE_VW
                              WHERE YEAR = ", year))
    mw.sql <-rbind(mw.sql, mw.sql.ice)
  }
  
  require(lubridate)
  sh.live$TOW_DATE <- ceiling_date(sh.live$TOW_DATE, unit = "day")
  sh.dead$TOW_DATE <- ceiling_date(sh.dead$TOW_DATE, unit = "day")
  mw.sql$TOW_DATE <- ceiling_date(mw.sql$TOW_DATE, unit = "day")
  
  sh.live$SPECIES_ID[sh.live$SPECIES_ID==1] <- "SEA SCALLOP"
  sh.live$SPECIES_ID[sh.live$SPECIES_ID==2] <- "ICELANDIC SCALLOP"
  sh.dead$SPECIES_ID[sh.dead$SPECIES_ID==1] <- "SEA SCALLOP"
  sh.dead$SPECIES_ID[sh.dead$SPECIES_ID==2] <- "ICELANDIC SCALLOP"
  mw.sql$SPECIES_ID[mw.sql$SPECIES_ID==1] <- "SEA SCALLOP"
  mw.sql$SPECIES_ID[mw.sql$SPECIES_ID==2] <- "ICELANDIC SCALLOP"
  
  for (i in 1:length(banks)){
    for (j in 1:length(cruises)){
      if(grepl(tolower(cruises[j]), tolower(banks[i]))) cruise <- cruises[j]
    }
    
    cruise <- paste0(cruise, year)
    
    if(banks[i] == "GB") cruise <- paste0(cruise, ".1")
    if(banks[i] %in% c("GBa", "GBb")) cruise <- paste0("GB", year, ".2")
    
    if(!banks[i] %in% c("GB", "BanIce")) {
      
      sh.live.sub <- sh.live[sh.live$SURVEY_NAME == cruise & sh.live$MGT_AREA_CD == banks[i] & sh.live$SPECIES_ID=="SEA SCALLOP",]
      sh.dead.sub <- sh.dead[sh.dead$SURVEY_NAME == cruise & sh.dead$MGT_AREA_CD == banks[i] & sh.dead$SPECIES_ID=="SEA SCALLOP",]
      mw.sub <- mw.sql[mw.sql$SURVEY_NAME == cruise & mw.sql$MGT_AREA_CD == banks[i] & mw.sql$SPECIES_ID=="SEA SCALLOP",]
      
      sh <- dplyr::arrange(rbind(sh.live.sub, sh.dead.sub), TOW_NO)
      mw.sub <- dplyr::arrange(mw.sub, TOW_NO, SCALLOP_NUM)
      
      write.csv(sh, file = paste0("Y:/Offshore/Data requests/", year, "/", DR, "/", toupper(banks[i]), ".", year, ".std.hf.csv"), row.names = FALSE)
      write.csv(mw.sub, file = paste0("Y:/Offshore/Data requests/", year, "/", DR, "/", toupper(banks[i]), ".", year, ".mwsh.csv"), row.names = FALSE)
    }
    
    if(banks[i] == "GB") {
      sh.live.sub <- sh.live[sh.live$SURVEY_NAME == cruise & sh.live$MGT_AREA_CD %in% c("GBa", "GBb") & sh.live$SPECIES_ID=="SEA SCALLOP",]
      sh.dead.sub <- sh.dead[sh.dead$SURVEY_NAME == cruise & sh.dead$MGT_AREA_CD %in% c("GBa", "GBb") & sh.dead$SPECIES_ID=="SEA SCALLOP",]
      mw.sub <- mw.sql[mw.sql$SURVEY_NAME == cruise & mw.sql$MGT_AREA_CD %in% c("GBa", "GBb") & mw.sql$SPECIES_ID=="SEA SCALLOP",]
      
      sh <- dplyr::arrange(rbind(sh.live.sub, sh.dead.sub), TOW_NO)
      mw.sub <- dplyr::arrange(mw.sub, TOW_NO, SCALLOP_NUM)
      
      write.csv(sh, file = paste0("Y:/Offshore/Data requests/", year, "/", DR, "/", toupper(banks[i]), "spring.", year, ".std.hf.csv"), row.names = FALSE)
      write.csv(mw.sub, file = paste0("Y:/Offshore/Data requests/", year, "/", DR, "/", toupper(banks[i]), "spring.", year, ".mwsh.csv"), row.names= FALSE)
    }
    
    if(banks[i]== "BanIce"){
      sh.live.sub <- sh.live[sh.live$SURVEY_NAME == cruise & sh.live$MGT_AREA_CD == gsub(x=banks[i], "Ice", "") & sh.live$SPECIES_ID=="ICELANDIC SCALLOP",]
      sh.dead.sub <- sh.dead[sh.dead$SURVEY_NAME == cruise & sh.dead$MGT_AREA_CD == gsub(x=banks[i], "Ice", "") & sh.dead$SPECIES_ID=="ICELANDIC SCALLOP",]
      mw.sub <- mw.sql[mw.sql$SURVEY_NAME == cruise & mw.sql$MGT_AREA_CD == gsub(x=banks[i], "Ice", "") & mw.sql$SPECIES_ID=="ICELANDIC SCALLOP",]
      
      sh <- dplyr::arrange(rbind(sh.live.sub, sh.dead.sub), TOW_NO)
      mw.sub <- dplyr::arrange(mw.sub, TOW_NO, SCALLOP_NUM)
      
      write.csv(sh, file = paste0("Y:/Offshore/Data requests/", year, "/", DR, "/", toupper(banks[i]), ".", year, ".std.hf.csv"), row.names = FALSE)
      write.csv(mw.sub, file = paste0("Y:/Offshore/Data requests/", year, "/", DR, "/", toupper(banks[i]), ".", year, ".mwsh.csv"), row.names = FALSE)
    }
    
    print(banks[i])
    print(dim(sh))
    print(dim(mw.sub))
  }
}

## RUN THIS:
detailed.tow.data(year=year, DR=DR, banks=banks, cruises=cruises, un.ID=un.ID, pwd.ID=pwd.ID)

# compare to live and dead views in SQL

# GBAshf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBA.", year, ".std.hf.csv"))
# GBBshf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBB.", year, ".std.hf.csv"))
# GBAmwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBA.", year, ".mwsh.csv"))
# GBBmwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBB.", year, ".mwsh.csv"))
GBshf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBspring.", year, ".std.hf.csv"))
GBmwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBspring.", year, ".mwsh.csv"))
BBNshf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/BBN.", year, ".std.hf.csv"))
BBNmwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/BBN.", year, ".mwsh.csv"))
# BBSshf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/BBS.", year, ".std.hf.csv"))
# BBSmwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/BBS.", year, ".mwsh.csv"))
SABshf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/SAB.", year, ".std.hf.csv"))
SABmwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/SAB.", year, ".mwsh.csv"))
MIDshf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/MID.", year, ".std.hf.csv"))
MIDmwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/MID.", year, ".mwsh.csv"))
GERshf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GER.", year, ".std.hf.csv"))
GERmwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GER.", year, ".mwsh.csv"))

GBashf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBa.", year, ".std.hf.csv"))
GBamwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBa.", year, ".mwsh.csv"))
GBbshf <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBb.", year, ".std.hf.csv"))
GBbmwsh <- read.csv(paste0("Y:/Offshore/Data requests/", year, "/", DR, "/GBb.", year, ".mwsh.csv"))


require(tidyverse)
source(paste0("C:/Users/keyserf/Documents/Github/Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r"))

plot <- function(melted) {
  melted$lat <- convert.dd.dddd(melted$START_LAT)
  melted$lon <- convert.dd.dddd(melted$START_LON)
  
  melted <- melted[melted$value > 0,]
  
  ggplot() + geom_point(data=melted, aes(lon, lat, size=value)) +
    facet_wrap(~as.numeric(as.character(name))) + 
    coord_sf()
}

melted <- pivot_longer(GBashf[GBashf$STATE=="live",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(GBashf[GBashf$STATE=="dead",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(GBbshf[GBbshf$STATE=="live",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(GBbshf[GBbshf$STATE=="dead",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(GBshf[GBshf$STATE=="live",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(GBshf[GBshf$STATE=="dead",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(BBNshf[BBNshf$STATE=="live",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(BBNshf[BBNshf$STATE=="dead",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(BBSshf[BBSshf$STATE=="live",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(BBSshf[BBSshf$STATE=="dead",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(SABshf[SABshf$STATE=="live",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(SABshf[SABshf$STATE=="dead",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(MIDshf[MIDshf$STATE=="live",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(MIDshf[MIDshf$STATE=="dead",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(GERshf[GERshf$STATE=="live",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

melted <- pivot_longer(GERshf[GERshf$STATE=="dead",], cols=starts_with("BIN_"), names_prefix = "BIN_")
plot(melted)

# MWSH checks
ggplot() + geom_smooth(data=GBamwsh, aes(SHELL_HEIGHT, WET_MEAT_WGT, group=TOW_NO)) + facet_wrap(~TOW_NO)
ggplot() + geom_smooth(data=GBbmwsh, aes(SHELL_HEIGHT, WET_MEAT_WGT, group=TOW_NO)) + facet_wrap(~TOW_NO)
ggplot() + geom_smooth(data=GBmwsh, aes(SHELL_HEIGHT, WET_MEAT_WGT, group=TOW_NO)) + facet_wrap(~TOW_NO)
ggplot() + geom_smooth(data=BBNmwsh, aes(SHELL_HEIGHT, WET_MEAT_WGT, group=TOW_NO)) + facet_wrap(~TOW_NO)
ggplot() + geom_smooth(data=BBSmwsh, aes(SHELL_HEIGHT, WET_MEAT_WGT, group=TOW_NO)) + facet_wrap(~TOW_NO)
ggplot() + geom_smooth(data=SABmwsh, aes(SHELL_HEIGHT, WET_MEAT_WGT, group=TOW_NO)) + facet_wrap(~TOW_NO)
ggplot() + geom_smooth(data=MIDmwsh, aes(SHELL_HEIGHT, WET_MEAT_WGT, group=TOW_NO)) + facet_wrap(~TOW_NO)
ggplot() + geom_smooth(data=GERmwsh, aes(SHELL_HEIGHT, WET_MEAT_WGT, group=TOW_NO)) + facet_wrap(~TOW_NO)

