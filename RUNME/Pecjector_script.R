## RUNME pecjector

direct_fns <- "Y:/Github/Offshore/Assessment_fns/FK/"

source(paste0(direct_fns, "Maps/pectinid_projector_sf.R"))

pecjector(area = "BBn", 
          repo="local", 
          add_EEZ = T, 
          add_bathy = 100, # specifying a depth with m will plot a single isobath. Specifying a number will set breaks of that size, resulting in multiple isobaths
          add_land = T, 
          add_nafo = "main", 
          add_sfas = "offshore", 
          add_strata = "offshore", 
          direct_fns=direct_fns,
          plot_package = "ggplot2")

