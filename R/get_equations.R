#' Get equation table
#'
#' Gets dataframe with equation used in estimations
#'
#' @param 
#' @return Dataframe with equation used in estimations
#' @export
get_equations <- function(){
  dats <- 
    read.csv(system.file("extdata", "equation_table.csv", package = "hellometry"))
  ## Return data
  return(dats)       
  
}
