#' Get database data
#'
#' Gets BWG data without biomass estimates
#'
#' @return Latest version of BWG data on file
#' @export
database_data <- function(){
  ## Get data
  ret <-
    read_extdata("bwg_database.csv")
   
  ## Return data
  return(ret)       
  
}




