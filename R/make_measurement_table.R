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
