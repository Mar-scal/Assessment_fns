August 2016
D. Keith

The files in this folder are simple scripts used to call in other functions to perform the major functions that we do in Scallop Unit.  While this automation makes it easy (I hope) to run
get the results it's important you understand the underlying functions and check to make sure the results make sense.  Hopefully these functions/scripts make that easier as there should be no need
to tweak code in these files if there are no "special circumstances" in a given year.  So here is a quick run down of what these functions do, detailed help files shouldn't be necessary for these files.
The order of the files here is also the process flow for the offshore, 1: Design survey, 2: Analyze survey results, 3:  Analyze fishery data, 4:  Run model.

1:  Survey_design_script.R		This script will produce the survey design for the upcoming year.  The results are now reproducable by using the "seed" in the function call, the survey design will remain 
                                randomized this just allows R to "remember" the randomization scheme you used.  Critical functions are found in the "Survey_design" folder.

2:  Survey_summary_script.R		This script calls two functions.  The first is SurveySummary_data.r which will run the survey analysis and compiles all the data needed for Survey Summary meetings and also 
								gets the survey data ready for Modeling.  The second function is Survey_summary_figures.r, this function produces the figures used in the Survey Summary meetings, once SurveySummary_data.r
								has been run the analyses will be saved and you can then make whatever figures you'd like using the Survey_summary_figures.r function. Primary functions this script relies on are in the
								Survey_and_OSAC folder.

3:  Survey_tow_by_tow.R			This script is different (and needs cleaned up at some point) from the others in this folder.  This is used to obtain a series of different figures that look at tow by tow
								breakdown of what we saw in the survey, it produces very detailed view of what was found in the survey.  
								Once I get around to turning this into a function it will likely be removed from this folder and run as part of the Survey_summary_script.  For now it remains here.
								
4:	OSAC_summary_script.R		This script is used to a: calculate the fishery data summaries and b: produce the figures which are needed for OSAC meetings.  Primary functions this script relies on are in the
								Survey_and_OSAC folder.

5:  Update_script.R 			This script is used to run the offshore model.  All necessary options for running the model are here.  Primary functions this script relies on are in the Model folder.


								 