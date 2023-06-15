#' Add BWG-formatted taxonomy to data
#'
#' Add all taxonomy columns needed by the package
#' 
#' @param data_table The input data table, the column 'bwg_name' has to be present 
#' @return An updated data frame with taxonomy added
#' @export
add_taxonomy <- function(data_table){
  
  # Function %notin%
  '%notin%' <- 
    Negate('%in%')
  
  # This one column needs to be here
  if("bwg_name" %notin% colnames(data_table))
    stop("Please call column with bwg species names values 'bwg_name'")
  
  # Check columns in common
  cols_in_common <- 
    intersect(colnames(data_table), colnames(get_bwgnames()))
  
  # Add taxonomy to data table
  data_table <- 
    data_table %>% 
    dplyr::left_join(get_bwgnames(),
                     by = cols_in_common) %>% 
    dplyr::relocate(domain:species, 
                    .after = bwg_name)
  # Return updated data
  return(data_table)
}




