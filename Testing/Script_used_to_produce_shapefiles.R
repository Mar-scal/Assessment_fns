# Let's get converted....

source("D:/R/Assessment_fns/Testing/Convert_PBSmapping_into_GIS_shapefiles.R")


# Lets convert the seedboxes to GIS coordinates, here I'd assume WGS84 because this is what industry uses
# Note to be careful with the names of the layers, they need to have nice names for windows saving (e.g. symbols may break this).
pbs.2.gis(dat = "Y:/Offshore scallop/Assessment/Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",
          proj = "LL",c_sys = "WGS84",type = "lines",layer.names = "label",
          save.loc = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/offshore")

# Lets convert the seedboxes to GIS coordinates, here I'd assume WGS84 because this is what industry uses
# Note to be careful with the names of the layers, they need to have nice names for windows saving (e.g. symbols may break this).
pbs.2.gis(dat = "Y:/Offshore scallop/Assessment/Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",
          proj = "LL",c_sys = "WGS84",type = "lines",layer.names = "ID",
          save.loc = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/seeboxes")
# Now lets convert the survey boundaries into shapefiles, Note that I had to hack the Survey boundary file briefly for this to work
# I briefly renamed the labels for the Old Sable strata to "Sab_old" so that we didn't have Sable in here twice.
pbs.2.gis(dat = "Y:/Offshore scallop/Assessment/Data/Maps/approved/Survey/survey_boundary_polygons.csv",
          proj = "LL",c_sys = "WGS84",type = "lines",layer.names = "label",
          save.loc = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/offshore_boundaries")

# Lets convert the offshore survey strata into shapefiles, Note that I had to hack the Survey boundary file briefly for this to work
# I briefly renamed the labels for the Old Sable strata to "Sab_old" so that we didn't have Sable in here twice.
pbs.2.gis(dat = "Y:/Offshore scallop/Assessment/Data/Maps/approved/Survey/survey_detail_polygons.csv",
          proj = "LL",c_sys = "WGS84",type = "lines",layer.names = "label",
          save.loc = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/offshore_survey_strata")

# Lets convert the inshore survey strata into shapefiles, Here is the SPA6 inside
# I briefly renamed the labels for the Old Sable strata to "Sab_old" so that we didn't have Sable in here twice.
pbs.2.gis(dat = "Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_IN_R_final_MOD.csv",
          proj = "LL",c_sys = "WGS84",type = "lines",layer.names = "label",
          save.loc = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/inshore_survey_strata")
# Lets convert the inshore survey strata into shapefiles, Here is the SPA6 outside
# I briefly renamed the labels for the Old Sable strata to "Sab_old" so that we didn't have Sable in here twice.
pbs.2.gis(dat = "Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6_VMS_OUT_R_final_MOD.csv",
          proj = "LL",c_sys = "WGS84",type = "lines",layer.names = "label",
          save.loc = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/inshore_survey_strata")

# Lets convert the inshore survey strata into shapefiles, Here is the SPA6 boundary
# I briefly renamed the labels for the Old Sable strata to "Sab_old" so that we didn't have Sable in here twice.
pbs.2.gis(dat = "Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SPA6wgs84_outterline.csv",
          proj = "LL",c_sys = "WGS84",type = "lines",layer.names = "label",
          save.loc = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/inshore_survey_strata")

# Lets convert the inshore survey strata into shapefiles, Here is SPA4 
# I briefly renamed the labels for the Old Sable strata to "Sab_old" so that we didn't have Sable in here twice.
pbs.2.gis(dat = "Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/spa4stratadefs.csv",
          proj = "LL",c_sys = "WGS84",type = "lines",layer.names = "label",
          save.loc = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/inshore_survey_strata")

# Lets convert the inshore survey strata into shapefiles, Here is SPA3 
# I briefly renamed the labels for the Old Sable strata to "Sab_old" so that we didn't have Sable in here twice.
pbs.2.gis(dat = "Y:/Offshore scallop/Assessment/Data/Maps/approved/Other_Borders/SCSTRATADEFS.csv",
          proj = "LL",c_sys = "WGS84",type = "lines",layer.names = "label",
          save.loc = "Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/inshore_survey_strata")
