#' Build measurement table to be used for biomass estimations
#'
#' Compiles the table of numerical measurements used to estimate sizes and fit
#' allometric models. The table is built entirely from the data you supply:
#' every row with a numerical `size_col` is kept (expanded by `abundance` so
#' each row represents one individual), carrying its taxonomy along.
#'
#' @param dats Dataframe to be used for estimation.
#' @param level_vec Vector of taxonomic levels to be used in the measurement table.
#' @return A table with measurements and taxonomy, ready to be used for
#'  size and biomass estimation.
#' @examples
#' # Reference measurements for one family, renamed to the columns the package expects
#' measurements <-
#'   bromeliad_inverts_measurements() %>%
#'   dplyr::filter(family == "Culicidae") %>%
#'   dplyr::rename(size_col = body_size_mm,
#'                 biomass_col = body_mass_mg,
#'                 biomass_type = mass_type) %>%
#'   dplyr::mutate(size_col = as.character(size_col),
#'                 biomass_col = as.numeric(biomass_col))
#'
#' # Taxa to estimate, with no measurement of their own
#' communities <-
#'   trini_communities() %>%
#'   dplyr::filter(family == "Culicidae") %>%
#'   dplyr::rename(abundance = n) %>%
#'   dplyr::mutate(size_col = "unknown",
#'                 biomass_col = NA,
#'                 biomass_type = "dry")
#'
#' # The two together are what the package works on
#' dats <-
#'   dplyr::bind_rows(communities, measurements)
#' level_vec <-
#'   c("species", "genus", "family")
#'
#' # One row per measured individual, taxonomy carried along
#' make_measurement_table(dats = dats, level_vec = level_vec)
#' @export
make_measurement_table <- function(dats, level_vec){

  # Keep only rows with a numerical size measurement.
  ## as.numeric() warns on non-numeric ("unknown", "small", ...) values, which
  ## are exactly the rows we drop, so silence the warning.
  suppressWarnings(
    ret <-
      dats %>%
      dplyr::mutate(size_col = as.numeric(size_col)) %>%
      dplyr::filter(!is.na(size_col)) %>%
      ## We should not expect every taxonomic level to be present, so use any_of
      dplyr::select(tidyselect::any_of(c(level_vec,
                                         "stage", "abundance", "size_col",
                                         "biomass_col", "biomass_type"))))

  # Have each row represent an individual
  ret <-
    ret %>%
    ## First make sure all 0s/NAs are 1s so that no row is removed
    dplyr::mutate(abundance = ifelse(abundance == 0 | is.na(abundance),
                                     1, abundance)) %>%
    ## Then use tidyr::uncount() to make each row a unique measurement
    tidyr::uncount(abundance)

  # Return measurement table
  return(ret)

}
