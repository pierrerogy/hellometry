#' Combine data with reference measurements
#'
#' Combine data and reference measurements into one table, to build the
#' measurement table from both. Missing abundances in reference measurements
#' are set to 1.
#'
#' @param dats Dataframe to be used for estimation.
#' @param measurements Dataframe of reference measurements, or NULL.
#' @return `dats` combined with reference measurements, or `dats` if
#' `measurements` is NULL.
#' @examples
#' dats <-
#'   data.frame(species = c("sp_a", "sp_b"),
#'              stage = "larva",
#'              abundance = c(2, 5),
#'              size_col = "unknown",
#'              biomass_col = NA,
#'              biomass_type = "dry")
#' measurements <-
#'   data.frame(species = "sp_a",
#'              stage = "larva",
#'              size_col = c(3.1, 2.8),
#'              biomass_col = c(0.4, 0.3),
#'              biomass_type = "dry")
#'
#' combine_measurements(dats, measurements)
#' @export
combine_measurements <- function(dats, measurements){

  # Without reference measurements, use data as is
  if (is.null(measurements)) {
    ret <- dats
  } else {
    # Important columns have proper names
    if("size_col" %notin% colnames(measurements))
      stop("Please call column with measurement values 'size_col' in measurements")
    if("biomass_col" %notin% colnames(measurements))
      stop("Please call column with biomass values 'biomass_col' in measurements")
    if("stage" %notin% colnames(measurements))
      stop("Please call column with life stage (larva/pupa/adult) 'stage' in measurements")
    if("biomass_type" %notin% colnames(measurements))
      stop("Please call column with biomass type (dry/wet) 'biomass_type' in measurements")

    # Each reference row is one individual if no abundance given
    if("abundance" %notin% colnames(measurements))
      measurements <-
        measurements %>%
        dplyr::mutate(abundance = 1)

    # Combine, size_col holds numbers and categories so must be character
    ret <-
      dplyr::bind_rows(
        dats %>%
          dplyr::mutate(size_col = as.character(size_col),
                        biomass_col = as.numeric(biomass_col)),
        measurements %>%
          dplyr::mutate(size_col = as.character(size_col),
                        biomass_col = as.numeric(biomass_col)))
  }

  return(ret)

}
