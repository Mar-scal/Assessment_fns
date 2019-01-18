# This function allows us to run SQL queries using RODBC or Roracle depending on what the user requires/prefers
# To use this function in your script, add the following:
# source(Y:/Offshore scallop/Assessment/Assessment_fns/Other_functions/ScallopQuery.R)
# then replace your query code to:
# results <- ScallopQuery(
#                         package ="RODBC" or "ROracle", 
#                         SQLtext = "YOUR QUERY TEXT HERE" or a vector containing your query,
#                         un = yourusernamehere,
#                         pw = yourpwdhere,
#                         db.con = yourdbconhere)
# This will open and close your connection the the database automatically in one line, and output your results. 

ScallopQuery <- function(package="RODBC", un=un.ID, pw=pwd.ID, db.con=db.con, SQLtext="SQLtext") {

  # Open the channel if using ROracle:
  if(package %in% "ROracle") {
    require(ROracle)
    # Open the channel to the database
    chan <- dbConnect(dbDriver("Oracle"), un, pw, db.con, believeNRows=FALSE)
  }
  
  # Open the channel if using RODBC:
  if(package %in% "RODBC") {
    require(RODBC)
    # Open the channel to the database
    chan <- odbcConnect(uid=un, pwd = pw, dsn=db.con)
  }  
  
  # Run query if using ROracle
  if(package %in% "ROracle") {
    # Run the query and add data to the disc.lst object
    queryresults <- dbGetQuery(chan, SQLtext)
    # close the odbc connection
    dbDisconnect(chan)
  }
  
  # Run query if using ROracle
  if(package %in% "RODBC") {
    # Run the query and add data to the disc.lst object
    queryresults <- sqlQuery(chan, SQLtext)
    # close the odbc connection
    odbcClose(chan)
  }
  
  # return the results as an object
  return(queryresults)
}
