#' Determines if allometric equation is ln or log for firfighters
#'
#' Called when biomass computation happens because some equations are in log10 format 
#' (log10(biomass) = slope x log10(size) + intercept), others in ln format 
#' (ln(biomass) = slope x ln(size) + ln(intercept)). Has an exception argument, so far used for #' Acari, because equation used generates weight in micrograms, not milligrams (argument
#' controls the conversion)
#'
#' @param size_input Input size to be used in equation
#' @param row Row of the equation data frame (i.e. one equation)
#' @param taxo Taxonomy information of the species
#' @return Correct use of equation for one individual of given size
#' @export
firehose <- function(size_input, row, taxo){
  ## Determine which type of equation to use
  type <- 
    ifelse(!is.na(row$intercept), 
           "log10",
           ifelse(!is.na(row$ln_intercept),
                  "ln", NA))
  
  ## If function is log10
  if(type == "log10")
    per_cap_biomass <- 
      10^(row$slope * log10(size_input) + row$intercept)
  
  ## If function is natural logarithm
  if(type == "ln")
    per_cap_biomass <-
      exp(row$slope * log(size_input) + row$ln_intercept)
  
  ## Exceptions
  ### With equation of Acari, weight is in micrograms, need to convert to milligrams
  if(!is.na(taxo$subclass) & taxo$subclass == "Acari")
    per_cap_biomass <-
      per_cap_biomass/1000
  
  ## Return value  
  return(per_cap_biomass)
  
}