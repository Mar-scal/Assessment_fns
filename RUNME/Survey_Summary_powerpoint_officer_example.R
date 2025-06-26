## standardized Survey Summary PPT presentation maker

require(officer)
require(tidyverse)

direct <- "Y:/Offshore/Assessment/"
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"

load("Y:/Offshore/Assessment/Data/Survey_data/2024/Survey_summary_output/Survey_all_results.Rdata")
names(survey.obj) <- gsub(names(survey.obj), pattern="-", replacement="")
names(surv.Live) <- gsub(names(surv.Live), pattern="-", replacement="")

reportyear <- 2024

source(paste0(direct_fns, "Survey_and_OSAC/Survey_Summary_Word.R"))
Survey_Summary_Word(year=reportyear, reportseason="both",
                    data=paste0("Y:/Offshore/Assessment/Data/Survey_data/", reportyear, "/Survey_summary_output/Survey_all_results.Rdata"),
                    direct="Y:/Offshore/Assessment/",
                    direct_fns = direct_fns)
# objects: "bankcheck" df, ntows" df and "highlights" df

year <- 2024


table <- highlights[#highlights$bank==params$bank & 
  highlights$variable %in% c("NPR", "NR", "N", "IPR", "IR", "I"), ]

table$word[as.numeric(table$thisyearraw) < as.numeric(table$lastyearraw)] <- "decreased"
table$word[as.numeric(table$thisyearraw) > as.numeric(table$lastyearraw)] <- "increased"

table$perc[table$word=="increased"] <- 
  (as.numeric(table$thisyearraw[table$word=="increased"]) - as.numeric(table$lastyearraw[table$word=="increased"]))/
  as.numeric(table$lastyearraw[table$word=="increased"]) *100

table$perc[table$word=="decreased"] <-   
  (as.numeric(table$lastyearraw[table$word=="decreased"]) - as.numeric(table$thisyearraw[table$word=="decreased"]))/
  as.numeric(table$lastyearraw[table$word=="decreased"]) *100

table$perclab <- ScallopRound(table$perc, 2)

table$perclab[table$perc>0 & table$perc < 0.01] <- "<0.01"
# table$perclab[table$perc>99] <- ">99"

table$state <- paste0(table$word, " by ", table$perclab, "% since")

table$state[is.na(table$perc)] <- "was similar to"

table$state <- gsub(x= table$state, pattern="increased by ", replacement = "+")
table$state <- gsub(x= table$state, pattern="decreased by ", replacement = "-")

highlights$lastyear[highlights$variable %in% c("N", "NR", "NPR", "I", "IR", "IPR", "Nclap", "NRclap", "NPRclap", "PRpercentclap", "Rpercentclap", "Cpercentclap") & !is.na(highlights$lastyearraw) & (highlights$lastyearraw>0 & highlights$lastyearraw<0.01)] <- "<0.01"
highlights$thisyear[highlights$variable %in% c("N", "NR", "NPR", "I", "IR", "IPR", "Nclap", "NRclap", "NPRclap", "PRpercentclap", "Rpercentclap", "Cpercentclap") & !is.na(highlights$thisyearraw) & (highlights$thisyearraw>0 & highlights$thisyearraw<0.01)] <- "<0.01"
highlights$LTM[highlights$variable %in% c("N", "NR", "NPR", "I", "IR", "IPR", "Nclap", "NRclap", "NPRclap", "PRpercentclap", "Rpercentclap", "Cpercentclap") & !is.na(highlights$LTMraw) & (highlights$LTMraw>0 & highlights$LTMraw<0.01)] <- "<0.01"


highlights$word[highlights$variable=="CF" & highlights$word=="was similar"] <- "was similar to"
highlights$word[highlights$variable=="CF" & highlights$word=="increased"] <- "increased since"
highlights$word[highlights$variable=="CF" & highlights$word=="decreased"] <- "decreased since"

# create an annotated base ppt with all layouts
#annotate_base(path = "Y:/Offshore/Assessment/2021/Presentations/Survey_summary/template.pptx", output_file = "Y:/Offshore/Assessment/2021/Presentations/Survey_summary/annotated_layout.pptx")

# build a new ppt from the template
newpres <- read_pptx("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/template.pptx")

# details on the layouts available from the template. You can edit these by opening the template and then View > Slide Master and edit accordingly.
layout_summary(newpres)
layout_properties ( x = newpres, layout = "Title Slide", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop MWSH-CF Plot", master = "Default Design")
layout_properties ( x = newpres, layout = "Title and Content", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop map", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop INLA map 2", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop summary layout", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop table", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop Time Series Plot", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop SHF Plot", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop INLA map plot", master = "Default Design")
# and so on to get the properties for each layout.

# create the overview table for slide 2
overviewtable <- data.frame(Bank=c("Middle", "Banquereau", "Sable", "German", "Browns South", "Browns North", 
                                   "Georges Bank (monitoring stations)", "Georges Bank 'a'", "Georges Bank 'b'"), 
                            `Last Survey`=c(2023, 2019, 2023, 2023, 2023, 2023, 2023, 2023, 2023), 
                            `Surveyed this year`=c("YES", "NO", "YES", "YES", "NO", "YES", "YES", "YES", "YES"))
names(overviewtable) <- gsub(x=names(overviewtable), pattern=".", replacement=" ", fixed=T)

# create the first few slides
newpres <- newpres %>%
  
  # title slide
  add_slide(layout="Title Slide", master="Default Design") %>%
  ph_with(location = ph_location_label(ph_label = "Title 1"), value="Survey Summary 2024", index=1) %>%
  ph_with(location = ph_location_label(ph_label = "Subtitle 2"), value="Seafood Producers Association of Nova Scotia", index=1) %>%
  
  # slide 1
  add_slide(layout="Scallop table", master="Default Design") %>%
  ph_with(location = ph_location_label(ph_label = "Title 1"), value="Overview", index=1) %>%
  ph_with(value = overviewtable, location = ph_location_label(ph_label = "Content Placeholder 2"), index=1)
  
banks <- c("Mid", "Sab", "Ger", "BBn", "GB", "GBa", "GBb") #BBs

# now do the rest with a big loop

for (i in 1:length(banks)) {
  print(banks[i])
  if(banks[i] == "GBa") corner <- "Georges 'a'"
  if(banks[i] == "GBb") corner <- "Georges 'b'"
  if(banks[i] == "Mid") corner <- "Middle Bank"
  if(banks[i] == "Sab") corner <- "Sable Bank"
  if(banks[i] == "Ger") corner <- "German Bank"
  if(banks[i] == "BBs") corner <- "Browns South"
  if(banks[i] == "BBn") corner <- "Browns North"
  if(banks[i] == "GB") corner <- "Georges Bank (monitoring stations)"
  
  newpres <- newpres %>%
    
    # slide 2
    add_slide(layout="Scallop map", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/survey_strata.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 2"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 3
    add_slide(layout="Scallop Time Series Plot", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/abundance_ts.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 2"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 4
    add_slide(layout="Scallop Time Series Plot", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/biomass_ts.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 2"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 5
    add_slide(layout="Scallop SHF Plot", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/SHF.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 3"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # Add repeat tow slide for German
    # slide 9
    add_slide(layout="Scallop MWSH-CF Plot", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/MWSH_and_CF_ts_wide.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 10"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%
    
    # slide 6
    add_slide(layout="Scallop INLA map plot", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/PR-spatial.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 3"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 7
    add_slide(layout="Scallop INLA map plot", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/Rec-spatial.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 3"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 8
    add_slide(layout="Scallop INLA map plot", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/FR-spatial.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 3"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 10
    add_slide(layout="Scallop INLA map plot", master="Default Design") %>%
    #ph_with_img(paste0("Y:/Offshore/Assessment/2017/Presentations/Survey_summary/", banks[i],"/CF-spatial.png"), type="pic", index=1) %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/CF-spatial.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 3"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 11
    add_slide(layout="Scallop INLA map plot", master="Default Design") %>%
    #ph_with_img(paste0("Y:/Offshore/Assessment/2017/Presentations/Survey_summary/", banks[i],"/MC-spatial.png"), type="pic", index=1) %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i],"/MC-spatial.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 3"), index=2) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 12
    add_slide(layout="Scallop Time Series Plot", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i], "/Clapper_abund_ts.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 2"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 13
    add_slide(layout="Scallop Time Series Plot", master="Default Design") %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i], "/Clapper_per_ts.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 2"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%

    # slide 14
    add_slide(layout="Scallop Time Series Plot", master="Default Design") %>%
    #ph_with_img(paste0("Y:/Offshore/Assessment/2018/Presentations/Survey_summary/", banks[i], "/breakdown-2018.png"), type="pic", index=1) %>%
    ph_with(external_img(paste0("Y:/Offshore/Assessment/2024/Presentations/Survey_summary/", banks[i], "/breakdown-2024.png")),
            location = ph_location_label(ph_label = "Picture Placeholder 2"), index=2) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1) %>%
    
    # slide 15
    add_slide(layout="Scallop summary layout", master="Default Design") %>%
    ph_with(location = ph_location_label(ph_label = "Text Placeholder 3"), value = "Summary", index=1) %>%
    ph_with(value = unordered_list(
      level_list = c(1, rep(2, 3), 1, rep(2, 3), 1, 2), #Bullet point levels
      str_list = c("Abundance",
                   paste0("Pre-recruit: ", table$state[table$bank==banks[i] & table$variable == "NPR"], " ", yeartable$lastyear[yeartable$bank == banks[i]], ", ", table$nearLTM[table$bank==banks[i] & table$variable == "NPR"], " LTM"),
                   paste0("Recruit: ", table$state[table$bank==banks[i] & table$variable == "NR"], " ", yeartable$lastyear[yeartable$bank == banks[i]], ", ", table$nearLTM[table$bank==banks[i] & table$variable == "NR"], " LTM"),
                   paste0("Fully-recruited: ", table$state[table$bank==banks[i] & table$variable == "N"], " ", yeartable$lastyear[yeartable$bank == banks[i]], ", ", table$nearLTM[table$bank==banks[i] & table$variable == "N"], " LTM"),
                   "Biomass",
                   paste0("Pre-recruit: ", table$state[table$bank==banks[i] & table$variable == "IPR"], " ", yeartable$lastyear[yeartable$bank == banks[i]], ", ", table$nearLTM[table$bank==banks[i] & table$variable == "IPR"], " LTM"),
                   paste0("Recruit: ", table$state[table$bank==banks[i] & table$variable == "IR"], " ", yeartable$lastyear[yeartable$bank == banks[i]], ", ", table$nearLTM[table$bank==banks[i] & table$variable == "IR"], " LTM"),
                   paste0("Fully-recruited: ", table$state[table$bank==banks[i] & table$variable == "I"], " ", yeartable$lastyear[yeartable$bank == banks[i]], ", ", table$nearLTM[table$bank==banks[i] & table$variable == "I"], " LTM"),
                   "Condition factor",
                   paste0(highlights$word[highlights$variable=="CF" & highlights$bank==banks[i]], " ", yeartable$lastyear[yeartable$bank == banks[i]], ", ", strsplit(highlights$nearLTM[highlights$variable=="CF" & highlights$bank==banks[i]], " (", fixed=T)[[1]][1], " LTM")
      )),
      location = ph_location_label(ph_label = "Text Placeholder 2"), index=1) %>%
    ph_with(location = ph_location_label(ph_label = "Content Placeholder 19"), value=corner, index=1)
}

print(newpres, target = "Y:/Offshore/Assessment/2024/Presentations/Survey_summary/officer_output.pptx")   
###################################################################
###################################################################
##################### BASE POWERPOINT DONE ########################
###################################################################
###################################################################




###################################################################
########################### EDITING ###############################
###################################################################
# Say I want to add a weird slide that's new and we don't have a layout for it yet.
### Open the powerpoint in powerpoint, and create the new layout in Slide Master.
### Save and close.
### Then read it in again.

editedpres <- read_pptx("Y:/Offshore scallop/Assessment/2019/Presentations/Survey_summary/officer_example3.pptx")

# important to know how many slides we have currently
max(pptx_summary(editedpres)$slide_id)

# and what's on each slide
pptx_summary(editedpres)


# add the slide at the end of the ppt then move it into the right spot
editedpres <- editedpres %>%
  add_slide(layout="Scallop Seedbox SHF detail", master="Default Design") %>%
  ph_with_img(type="pic", index=2, src = "Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/GBa/Experimental-spatial-2018.png") %>%
  ph_with_img(type="pic", index=1, src="Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/GBa/Experimental-SHF.png") %>%
  ph_with_text(type="body", value=corner) %>%
  move_slide(index=31, to=16)

# delete an unnecessary slide (e.g. slide clapper abundance for GBb)
## figure out which slide it's on
pptx_summary(editedpres)[pptx_summary(editedpres)$slide_id==28,]

slide_summary(editedpres, index=28)

editedpres <- editedpres %>%
  remove_slide(index=28)
  

############################################################### 
######## Update all slide numbers and print - Done! ###########
############################################################### 
slidenums <- unique(pptx_summary(editedpres)$slide_id)

for(i in 2:max(slidenums)) {
  editedpres <- editedpres %>%
    on_slide(index=slidenums[i]) %>%
    ph_with_text(type="sldNum", value=paste0(slidenums[i] - 1))
}


# then print it again to save your changes
print(editedpres, target = "Y:/Offshore scallop/Assessment/2019/Presentations/Survey_summary/officer_example3.pptx")   




