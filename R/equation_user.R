#' Computes biomass with provided equation
#'
#' Called when biomass computation happens, uses linear modelling
#' and predict function
#'
#' @param size_mm Input size to be used in equation
#' @param eq Allometric equation to be used
#' @param taxo Taxonomy information of the species
#' @return Biomass estimation and confidence intervals
#' @export
equation_user <- function(size_mm, eq, taxo){
  
  # Get biomass
  per_cap_biomass <- 
    ## Ugly way of getting the model
    predict.lm(eq, 
               newdata = data.frame(size_mm),
               interval = "confidence",
               level = 0.95)
  
  # Transform to normal units
  per_cap_biomass <-
    10^per_cap_biomass
  
  ## Exceptions
  ### With equation of Acari, weight is in micrograms, need to convert to milligrams
  suppressWarnings(
    if(!is.na(taxo$subclass) & taxo$subclass == "Acari")
      per_cap_biomass <-
      per_cap_biomass/1000)
  
  ## Return value  
  return(per_cap_biomass)
  
}
