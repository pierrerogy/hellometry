#' Get measurement table
#'
#' Gets dataframe with equation used in estimations
#'
#' @param database Should data from the bwg database be used to supplement the measurements (TRUE/FALSE)
#' @return Dataframe with equation used in estimations
#' @export
get_measurements <- function(database){
  if(database == TRUE)
    dats <- 
      read.csv(system.file("extdata", "measurement_table_withdb.csv", package = "hellometry")) else
        dats <- 
          read.csv(system.file("extdata", "measurement_table_nodb.csv", package = "hellometry"))
      
  ## Return data
  return(dats)       
  
}
