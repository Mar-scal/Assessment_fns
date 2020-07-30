## standardized Survey Summary PPT presentation maker

require(officer)
require(tidyverse)

load("Y:/Offshore scallop/Assessment/Data/Survey_data/2018/Survey_summary_output/Survey_all_results.Rdata")
names(survey.obj) <- gsub(names(survey.obj), pattern="-", replacement="")
names(surv.Live) <- gsub(names(surv.Live), pattern="-", replacement="")

source("C:/Documents/Offshore scallop/Assessment/Assessment_fns/RUNME_Survey_OSAC_Model/Survey_Summary_Word.R")
Survey_Summary_Word(year=2018, reportseason="summer", data="Y:/Offshore scallop/Assessment/Data/Survey_data/2018/Survey_summary_output/Survey_all_results.Rdata")
# objects: "bankcheck" df, ntows" df and "highlights" df


# create an annotated base ppt with all layouts
annotate_base(path = "Y:/Offshore scallop/Assessment/2019/Presentations/Survey_summary/template.pptx", output_file = "Y:/Offshore scallop/Assessment/2019/Presentations/Survey_summary/annotated_layout.pptx")

# build a new ppt from the template
newpres <- read_pptx("Y:/Offshore scallop/Assessment/2019/Presentations/Survey_summary/template.pptx")

# details on the layouts available from the template. You can edit these by opening the template and then View > Slide Master and edit accordingly.
layout_summary(newpres)
layout_properties ( x = newpres, layout = "Title Slide", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop MWSH-CF Plot", master = "Default Design")
layout_properties ( x = newpres, layout = "Title and Content", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop map", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop INLA map 2", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop summary layout", master = "Default Design")
layout_properties ( x = newpres, layout = "Scallop table", master = "Default Design")
# and so on to get the properties for each layout.

# create the overview table for slide 2
overviewtable <- data.frame(Bank=c("Georges Bank 'a'", "Georges Bank 'b'"), `Last Survey`=2018, `Surveyed this year`="YES")
names(overviewtable) <- gsub(x=names(overviewtable), pattern=".", replacement=" ", fixed=T)

# create the first few slides
newpres <- newpres %>%
  
  # title slide
  add_slide(layout="Title Slide", master="Default Design") %>%
  ph_with_text(type="ctrTitle", str="Survey Summary Summer 2019", index=1) %>%
  ph_with_text(type="subTitle", str="Seafood Producers Association of Nova Scotia", index=1) %>%
  
  # slide 1
  add_slide(layout="Scallop table", master="Default Design") %>%
  ph_with_text(type="title", str="Overview", index=1) %>%
  ph_with_table(overviewtable, type="body", index=2)
  
banks <- c("GBa", "GBb")

# now do the rest with a big loop

for (i in 1:length(banks)) {
  
  if(banks[i] == "GBa") corner <- "Georges 'a'"
  if(banks[i] == "GBb") corner <- "Georges 'b'"
  
  newpres <- newpres %>%
    
    # slide 2
    add_slide(layout="Scallop map", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/survey_strata.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 3
    add_slide(layout="Scallop Time Series Plot", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/abundance_ts.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 4
    add_slide(layout="Scallop Time Series Plot", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/biomass_ts.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 5
    add_slide(layout="Scallop SHF Plot", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/SHF.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 6
    add_slide(layout="Scallop INLA map plot", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/PR-spatial.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 7
    add_slide(layout="Scallop INLA map plot", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/Rec-spatial.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 8
    add_slide(layout="Scallop INLA map plot", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/FR-spatial.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 9
    add_slide(layout="Scallop MWSH-CF Plot", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/MWSH_and_CF_ts.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 10
    add_slide(layout="Scallop INLA map 2", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2017/Presentations/Survey_summary/", banks[i],"/CF-spatial.png"), type="pic", index=1) %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/CF-spatial.png"), type="pic", index=2) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 11
    add_slide(layout="Scallop INLA map 2", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2017/Presentations/Survey_summary/", banks[i],"/MC-spatial.png"), type="pic", index=1) %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i],"/MC-spatial.png"), type="pic", index=2) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 12
    add_slide(layout="Scallop Time Series Plot", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i], "/Clapper_abund_ts.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 13
    add_slide(layout="Scallop Time Series Plot", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i], "/Clapper_per_ts.png"), type="pic", index=1) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 14
    add_slide(layout="Scallop Breakdown 2", master="Default Design") %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i], "/breakdown-2018.png"), type="pic", index=1) %>%
    ph_with_img(paste0("Y:/Offshore scallop/Assessment/2018/Presentations/Survey_summary/", banks[i], "/breakdown-2017.png"), type="pic", index=2) %>%
    ph_with_text(type="body", str=corner, index=1) %>%
    
    # slide 15
    add_slide(layout="Scallop summary layout", master="Default Design") %>%
    ph_with_text(type="body", str = "Summary", index=1) %>%
    ph_with_text(type="body", str = "We caught a whole bunch of scallops", index=2) %>%
    ph_add_par(level=1, type="body", id_chr = 3) %>%
    ph_add_text(str = "And then we counted them", id_chr = 3) %>%
    ph_add_par(level=2, type="body",id_chr = 3) %>%
    ph_add_text(str = "And by we, I mean Tricia", id_chr = 3) %>%
    ph_add_par(level=1,type="body", id_chr = 3) %>%
    ph_add_text(str = "Then we did some number magic", id_chr = 3) %>%
    ph_add_par(level=1,type="body", id_chr = 3) %>%
    ph_add_text(str = paste0("And voila, we estimated the scallop biomass for ", banks[i], "!"), id_chr = 3) %>%
    ph_add_par(level=1,type="body", id_chr = 3) %>%
    ph_add_text(str = paste0("Condition on ", banks[i], " was ",  highlights$thisyear[highlights$variable=="CF" & highlights$bank==banks[i]]), id_chr = 3) %>%
    ph_with_text(type="body", str=corner, index=3)
  
}

print(newpres, target = "Y:/Offshore scallop/Assessment/2019/Presentations/Survey_summary/officer_example3.pptx")   
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
  ph_with_text(type="body", str=corner) %>%
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
    ph_with_text(type="sldNum", str=paste0(slidenums[i] - 1))
}


# then print it again to save your changes
print(editedpres, target = "Y:/Offshore scallop/Assessment/2019/Presentations/Survey_summary/officer_example3.pptx")   




