#' Append measurement table with new measurements
#'
#' Append measurement table with new measurements, because this table
#' also compiles measurements for all species
#' 
#' @param measurement_table Table containing all measurements
#' @param dats Dataframe to be used for estimations
#' @param level_list List of taxonomic levels to be used in the measurement table
#' @return An updated measurement table that will be used to estimate sizes and gather taxonomy
#' @export
append_measurements <- function(measurement_table, dats, level_list){
  # Select columns based on possible range of taxonomy
  # We should not expect to have all of them
  # so use any_of
  suppressWarnings(
    data_stub <- 
      dats %>% 
      ## Only keep numerical measurements
      ## Suppress warnings too
      dplyr::mutate(size_mm = as.numeric(size_mm)) %>% 
      dplyr::filter(!is.na(size_mm)) %>% 
      dplyr::select(tidyselect::any_of(c(level_list,
                    "stage", "abundance", "size_mm", 
                    "biomass_mg", "biomass_type"))))
  
  # Bind new species to measurement table
  measurement_table <-
    measurement_table %>% 
    dplyr::bind_rows(data_stub)
  
  # Return
  return(measurement_table)
  
}




