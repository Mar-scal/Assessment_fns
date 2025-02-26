source("C:/Users/keyserf/Github/Assessment_fns/Survey_and_OSAC/Extract_survey_temperatures_function_offshore.r")

survey.bottom.temps(direct = "Y:/Offshore/Assessment/Data/Survey_data/2024/Database loading/", cruise = "LE20", 
                    towfile = "Y:/Offshore/Assessment/Data/Survey_data/2024/Industry Reports/GBa_olex_tracks_2024_temp.csv",
                    num.temps=2, survey_time = "both", tow_duration=10, fig="pdf", export=T)


