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
  
  # Get long format data of matching traits
  matched_traits <- 
    matcher_of_traits(measurement_table = measurement_table,
                      trait_columns = trait_columns)
  

  # Combine the two together
  ret <- 
    matched_traits %>% 
    ## Remove species without match
    dplyr::group_by(level, group, focus_species, focus_species_stage) %>% 
    dplyr::filter(n() > 1) %>% 
    dplyr::ungroup() %>% 
    ## Split into set of lists
    dplyr::group_split(group,
                       .keep = F) %>% 
    ## Add measurements for all species
    purrr::map(.,
               ~ .x %>% 
                 ## 
                 dplyr::left_join(measurement_table[1:500,] %>% 
                                    ## Keep important columns only
                                    dplyr::select(bwg_name, size_mm, biomass_mg),
                                  by = "bwg_name",
                                  ## Just because one species can have several measurements
                                  relationship = "many-to-many"))
  
  
  FEED TO FULL ESTTABLE?
  CHANGE NAME?
  

  
  # Return
  return(ret)
  
}


