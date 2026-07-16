#' Prepare data to do estimation with traits in `full_estimation_table`
#'
#' Remove columns with taxonomy, and replace them with trait-based groupings
#'
#' @param measurement_table A table with the numerical measurements and biomass
#' used to compute allometric lms
#' @param trait_columns List of traits to match, should be column names in measurement_table
#' @param id_col Name of the column holding a unique identifier per species/taxon.
#' Default "species".


#' @return List of tibbles, one per trait grouping, each carrying the
#' measurements of the species sharing that grouping's traits
#' @export
#'
#'
make_trait_table <- function(measurement_table,
                             trait_columns, id_col = "species") {

  # Get long format data of matching traits
  matched_traits <-
    matcher_of_traits(measurement_table = measurement_table,
                      trait_columns = trait_columns,
                      id_col = id_col)


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
                 ## Add measurements, joining matched ids to the id column
                 dplyr::left_join(measurement_table %>%
                                    ## Keep important columns only
                                    dplyr::select(dplyr::all_of(id_col),
                                                  size_col, biomass_col),
                                  by = stats::setNames(id_col, "id"),
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
