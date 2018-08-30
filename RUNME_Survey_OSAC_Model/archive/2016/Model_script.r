####################################################################
## GEORGES A AND BROWNS BANK NORTH.  This is a simple script to call the model functions and run them for offshore.
############## CURRENTLY (APRIL 2016) THIS SCRIPT DOES NOTHING AS Update.r needs to be converted to a function       ########################
############## SO FOR THE MOMENT RUN Update.r IF RECREATING OSAC PRESENTATION RESULTS                          ########################
#######################################################################################################################################  
## Created by DK April 2016
## Update history

direct = "d:/r/"
#direct = "g:/r/"
source(paste(direct,"Assessment_fns/Model/Update_function.r",sep=""))

update(direct = direct,run.mod=T,preprocessed=T,make.figs=T,make.diag.figs = F,make.pred.eval.figs = T,make.update.figs=F,bank="BBn")


