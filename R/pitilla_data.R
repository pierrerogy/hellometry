#' Get practice data
#'
#' Gets a toy dataset, collected in Pitilla (Guanacaste, Costa Rica) by Diane Srivastava in 2002
#'
#' @param 
#' @return The Pitilla data 
#' @export
pitilla_data <- function(){
  pitilla <- 
    read.csv(system.file("extdata", "pitilla.csv", package = "hellometry")) %>%
    ## Remove superfluous columns
    dplyr::select(bwg_name, size_mm, abundance)
  ## Return data
  return(pitilla)       
  
}




