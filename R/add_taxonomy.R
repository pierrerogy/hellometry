#' Add BWG-formatted taxonomy to data
#'
#' Add all taxonomy columns needed by the package
#' 
#' @param data_table The input data table
#' @return An updated measurement table that will be used to estimate sizes and gather taxonomy
#' @export
add_taxonomy <- function(data_table){
  # Add taxonomy to data table
  data_table <- 
    test %>% 
    dplyr::left_join(get_bwgnames(),
                     by = "bwg_name") %>% 
    dplyr::relocate(domain:species, 
                    .after = bwg_name)
  # Return updated data
  return(data_table)
}




