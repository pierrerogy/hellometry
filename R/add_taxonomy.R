#' Add BWG-formatted taxonomy to data
#'
#' Add all taxonomy columns needed by the package
#' 
#' @param data_table The input data table, the column 'bwg_name' has to be present 
#' @return An updated measurement table that will be used to estimate sizes and gather taxonomy
#' @export
add_taxonomy <- function(data_table){
  
  # Function %notin%
  '%notin%' <- 
    Negate('%in%')
  
  # This one column needs to be here
  if("bwg_name" %notin% colnames(data_table))
    stop("Please call column with bwg species names values 'bwg_name'")
  
  # Add taxonomy to data table
  data_table <- 
    data_table %>% 
    dplyr::left_join(get_bwgnames(),
                     by = "bwg_name") %>% 
    dplyr::relocate(domain:species, 
                    .after = bwg_name)
  # Return updated data
  return(data_table)
}




