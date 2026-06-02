#' Prepare data to do estimations with traits in `full_estimation_table`
#'
#' Remove columns with taxonomy, and replace them with trait-based groupings
#'
#' @param measurement_table A table with the numerical measurements and biomass 
#' used to compute allometric lms 
#' @param trait_columns List of traits to match, should be column names in measurement_table


#' @return Tibble of four columns: level = "traits", group_focus_species, stage of focus species, 
#' bwg_name of matched species
#' @export
#' 
#' 
make_trait_table <- function(measurement_table,
                             trait_columns) {
  
  # Get long format data of matching traits
  matched_traits <- 
    matcher_of_traits(measurement_table = measurement_table,
                      trait_columns = trait_columns)
  
  
  # Print message to say that we are cleaning the data now
  print("Cleaning trait groupings..")

  # Combine the two together
  ret <- 
    matched_traits %>% 
    ## Remove species without match
    dplyr::group_by(level, name, stage) %>% 
    dplyr::filter(n() > 1) %>% 
    dplyr::ungroup() %>% 
    ## Split into set of lists
    dplyr::group_split(name,
                       .keep = T) %>% 
    ## Add measurements for all species
    purrr::map(.,
               ~ .x %>% 
                 ## Add measurements
                 dplyr::left_join(measurement_table %>% 
                                    ## Keep important columns only
                                    dplyr::select(bwg_name, size_col, biomass_col),
                                  by = "bwg_name",
                                  ## Just because one species can have several measurements
                                  relationship = "many-to-many") %>% 
                 ## Remove all NAs in sizes
                 dplyr::filter(!is.na(size_col))) %>% 
    ## Filter out those groups with less than three unique measurements
    purrr::map(.,
               ~ .x %>%
                 ## Group by level, name and stage
                 dplyr::group_by(level, name, stage) %>%
                 ## Filter out those with less than three unique size_col
                 dplyr::filter(dplyr::n_distinct(size_col) >= 3) %>%
                 ## Ungroup to avoid future issues
                 dplyr::ungroup()) %>% 
    ## Remove empty levels
    purrr::keep(function(x) nrow(x) > 0)

  # Return
  return(ret)

}


