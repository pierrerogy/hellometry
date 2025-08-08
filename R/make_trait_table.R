#' Modify measurement table so that estimations are done with traits
#'
#' Remove columns with taxonomy, and replace them with trait-based groupings
#'
#' @param mmeasurement_table A table with the numerical measurements and biomass 
#' used to compute allometric lms 
#' @param level_list list of taxonomic levels in measurement_table
#' @param trait_columns List of traits to match


#' @return Tibble of four columns: level = "traits", group_focus_species, stage of focus species, 
#' bwg_name of matched species
#' @export
#' 
#' 
make_trait_table <- function(measurement_table, level_list, 
                             trait_columns) {
  
  # Remove taxonomy columns from measurement_table
  ret <- 
    measurement_table %>% 
    dplyr::select(-dplyr::all_of(level_list))
  
  # Get long format data of matching traits
  matched_traits <- 
    matcher_of_traits(measurement_table = measurement_table,
                      trait_columns = trait_columns)
  

  
  # Return
  return(ret)
  
}


