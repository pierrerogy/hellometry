#' Error checking for data frame
#'
#' Throws custom error message if data not in accepted format
#'
#' @param dats The data used to estimate biomass
#' @param biomass_kind Should data used in inference be "dry" or "wet", "most_common" or "both"
#' @return Error messages only
#' @export
error_check <- function(dats, mass_kind){
  
  # Some error catching 
  ## Important columns have proper names
  if("abundance" %notin% colnames(dats))
    stop("Please call column with abundance values 'abundance'")
  if("bwg_name" %notin% colnames(dats))
    stop("Please call column with bwg species names values 'bwg_name'")
  if("size_mm" %notin% colnames(dats))
    stop("Please call column with specimen measurement values 'size_mm'")
  if("stage" %notin% colnames(dats))
    stop("Please call column with life stage (larva/pupa/adult) 'stage'")
  if("mass_mg" %notin% colnames(dats))
    stop("Please call column with biomass or body mass 'mass_mg'")
  ## Biomass kind needs to be dry, wet or both
  if(mass_kind %notin% c("dry", "wet", "most_numerous", "both"))
    stop("Mass kind has to be 'dry', 'wet', 'most_numerous' or 'both'")

}