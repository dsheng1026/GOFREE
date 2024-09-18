
#' ReadGCAM
#'
#' @param filetype specify the input is a database (folder) or a project (.dat)
#' @param input_path specify the full path for the input file
#' @param db_name specify the database name
#' @param query use the default query file in the package "my_batch.xml"
#' @param scen_name specify the scenario name
#' @param prj_name specify the project (.dat) name either to name it or load the existed one
#'
#' @return a list to be called in other functions
#' @export
#'
#' @examples
#' prj <- ReadGCAM(filetype = 'db',
#' input_path = "path",
#' db_name = "DB",
#' query = system.file("extdata", "my_batch.xml", package = "GCAMUSAJobs"),,
#' scen_name = "Scenario_from_DB",
#' prj_name = "A_Project_Name.dat")

ReadGCAM <- function(filetype,
                     input_path, # full path of the input file (db or .dat)
                     db_name, # name of the db folder
                     query = system.file("extdata", "my_batch.xml", package = "GCAMUSAJobs"),
                     scen_name, # name of the scenario of interest
                     prj_name){ # name of a prj

  if(!filetype %in% c("db", "prj")){
    print("Need a GCAM output databse: 'db' or an existing prj: 'dat'; Current input is not a valid input type")
  } else{
    if(filetype == "db"){ # if read from a GCAM output db
      if(is.null(input_path)){
        print("miss the path of GCAM output DB")} else{
          if(is.null(db_name)){
            print("miss db name")} else{
              if(is.null(scen_name)){
                print("miss the scenario name")} else{
                  if(is.null(prj_name)) {
                    print("miss the prj name")} else{
                      # Load the rgcam project:
                      conn <- rgcam::localDBConn(input_path, db_name, migabble = TRUE)
                      prj <- rgcam::addScenario(conn,prj_name,scen_name,query)}
                }
            }
        }
    } else{ # read from an existing prj
      if(is.null(prj_name)){
        print("missing a prj name")
      } else{
        prj <- rgcam::loadProject(paste0(input_path,'/',prj_name))}
    }
  }
  return(prj)
}
