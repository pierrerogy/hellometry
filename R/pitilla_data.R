#' Get practice data
#'
#' Gets a toy dataset, collected in Pitilla (Guanacaste, Costa Rica) by Diane Srivastava in 2002.
#' Some mock rows added by Pierre Rogy to demonstrate some aspects of the package
#'
#' @param taxonomy Do you want all taxonomic columns to be included? (TRUE (default)/FALSE)
#' @return The Pitilla data 
#' @export
pitilla_data <- function(taxonomy = TRUE){
  pitilla <-
    read_extdata("pitilla.csv")
  
  ## If the taxonomy is not wanted  
  if(!(taxonomy))  
    pitilla <-
      pitilla %>% 
      dplyr::select(bwg_name, size_col, stage, abundance)
  
  ## Return data
  return(pitilla)       
  
}




