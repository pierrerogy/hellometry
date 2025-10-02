#' Append measurement table with new species names
#'
#' Append measuremement table with new species names, because this table
#' also compiles taxonomy for all species
#' 
#' @param measurement_table Table containing all measurements
#' @param dats Dataframe to be used for estimations
#' @param level_vec Vector of taxonomic levels to be used in the measurement table
#' @return An updated measurement table that will be used to estimate sizes and 
#' gather taxonomy
#' @export
append_names <- function(measurement_table, dats, level_vec, no_BWG_data){
  
  # If there are rows in measurement table, use it
  if(!no_BWG_data){
  # Make stub of measurement_table 
  measurement_stub <- 
    measurement_table %>% 
    dplyr::select(tidyselect::any_of(c(level_vec,
                                       "stage", "abundance", "size_col", 
                                       "biomass_col", "biomass_type"))) %>% 
    unique()
  
  # Check if any species not present in the database
  data_stub <- 
    dats %>% 
    dplyr::select(tidyselect::any_of(c(level_vec,
                                       "stage", "abundance", "size_col", 
                                       "biomass_col", "biomass_type"))) %>% 
    unique() %>% 
    ## Remove all species present in the database already 
    ## Of course remove traits if in level_vec
    dplyr::anti_join(measurement_stub,
                     by = level_vec[level_vec %notin% "traits"]) %>% 
    ## Convert all sizes and biomasses to NA
    dplyr::mutate(size_col = NA,
                  biomass_col = NA) %>% 
    ## Just make sure all taxonomic levels are characters
    dplyr::mutate(dplyr::across(tidyselect::any_of(level_vec), 
                                as.character))

  # Bind new species to measurement table
  ret <-
    measurement_table %>% 
    dplyr::bind_rows(data_stub)} else

  # If not, just use dats as measurement table
    ret<- 
        measurement_table
  
  # Return
  return(ret)
  
}




