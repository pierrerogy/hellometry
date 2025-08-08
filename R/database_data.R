#' Get database data
#'
#' Gets BWG data without biomass estimates
#'
#' @param none
#' @return Latest version of BWG data on file
#' @export
database_data <- function(){
  ## Get data
  ret <- 
    read.csv(system.file("extdata", "bwg_database.csv", 
                         package = "hellometry"))
   
  ## Return data
  return(ret)       
  
}




