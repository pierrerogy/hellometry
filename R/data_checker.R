#' Set of checks to perform on the data to make sure it can be handled by `hellometry()`
#'
#' Will fail if any condition not met
#'
#' @param dats Dataframe to be used for estimation
#' @param biomass_type "dry"/"wet". Whether the biomass used in inference should
#' be dry (default) or wet. See `dry_wet()` for details.
#' @param model "lm"/"poisson". What kind of allometric model should be computed.
#' See `full_estimation_table()` for details.
#' @export
data_checker <- function(dats, biomass_type, model){

  # Important columns have proper names
  if("abundance" %notin% colnames(dats))
    stop("Please call column with abundance values 'abundance'")
  if("size_col" %notin% colnames(dats))
    stop("Please call column with specimen measurement values 'size_col'")
  if("stage" %notin% colnames(dats))
    stop("Please call column with life stage (larva/pupa/adult) 'stage'")
  if("biomass_type" %notin% colnames(dats))
    stop("Please call column with biomass type (dry/wet) 'biomass_type'")

  # Biomass kind needs to be dry or wet
  if(biomass_type %notin% c("dry", "wet"))
    stop("Biomass kind has to be 'dry' or 'wet'")

  # Model kind needs to be one we can compute
  if(model %notin% c("lm", "poisson"))
    stop("Model kind has to be 'lm' or 'poisson'")

  # Check for NA values in abundance before processing
  if (any(is.na(dats$abundance))) {
    stop("Abundance contains NA values") }

}
