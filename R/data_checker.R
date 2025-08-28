#' Set of checks to perform on the data to make sure it can be handled by `hellometry()`
#'
#' Will fail if any condition not met
#'
#' @param dats Dataframe to be used for estimations
#' @export
data_checker <- function(dats, biomass_type, use_BWG_db, no_BWG_data){

  # Important columns have proper names
  if("abundance" %notin% colnames(dats))
    stop("Please call column with abundance values 'abundance'")
  if("size_mm" %notin% colnames(dats))
    stop("Please call column with specimen measurement values 'size_mm'")
  if("stage" %notin% colnames(dats))
    stop("Please call column with life stage (larva/pupa/adult) 'stage'")
  if("biomass_type" %notin% colnames(dats))
    stop("Please call column with biomass type (dry/wet) 'biomass_type'")
  
  # Database is actually a true false
  if(use_BWG_db %notin% c(TRUE, FALSE))
    stop("use_BWG_db has to be TRUE/FALSE")
  
  # no_BWG_data is actually a true false
  if(no_BWG_data %notin% c(TRUE, FALSE))
    stop("no_BWG_data has to be TRUE/FALSE")
  
  # Biomass kind needs to be dry or both
  if(biomass_type %notin% c("dry", "wet"))
    stop("Biomass kind has to be 'dry' or 'wet'")
  
  # Check for NA values in abundance before processing
  if (any(is.na(dats$abundance))) {
    stop("Abundance contains NA values") }     
  
}
