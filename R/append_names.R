#' Append measurement table with new species names
#'
#' Append measuremement table with new species names, because this table
#' also compiles taxonomy for all species
#' 
#' @param measurement_table Table containing all measurements
#' @param dats Dataframe to be used for estimations
#' @param level_list List of taxonomic levels to be used in the measurement table
#' @return An updated measurement table that will be used to estimate sizes and 
#' gather taxonomy
#' @export
append_names <- function(measurement_table, dats, level_list){
  
  # If there are rows in measurement table, use it
  if(nrow(measurement_table > 0)){
  # Make stub of measurement_table 
  measurement_stub <- 
    measurement_table %>% 
    dplyr::select(tidyselect::any_of(c(level_list,
                                       "stage", "abundance", "size_mm", 
                                       "biomass_mg", "biomass_type"))) %>% 
    unique()
  
  # Check if any species not present in the database
  data_stub <- 
    dats %>% 
    dplyr::select(tidyselect::any_of(c(level_list,
                                       "stage", "abundance", "size_mm", 
                                       "biomass_mg", "biomass_type"))) %>% 
    unique() %>% 
    ## Remove all species present in the database already 
    ## Of course remove traits if in level_list
    dplyr::anti_join(measurement_stub,
                     by = level_list[level_list %notin% "traits"]) %>% 
    ## Convert all sizes and biomasses to NA
    dplyr::mutate(size_mm = NA,
                  biomass_mg = NA)
  
  # Bind new species to measurement table
  measurement_table <-
    measurement_table %>% 
    dplyr::bind_rows(data_stub)} else

  # If not, just use dats as measurement table
    measurement_table <- 
      dats
  
  # Return
  return(measurement_table)
  
}




