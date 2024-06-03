#' Get equations for allometry
#'
#' Computes a linear model that will be used as allometric equation
#'
#' @param dats A table with the numerical measurements and biomass used to compute allometric models
#' @param equation_type Which package should be used to compute the models. LIST TBD
#' @param biomass_kind Should data used in inference be "dry" or "wet", "most_common" or "both"

#' @return Dataframe wiht computed models at which level
#' @export
get_equations <- function(dats, equation_type){
  # Packages to get equations
  ## lm already in base r
  require(lme4) ## when I will add mixed effects
  require(brms) ## when I will add mixed effects
  require(lmodel2) ## OLS 
  
  # Make data frame to return
  ret <- 
    lm(log10(mass_mg) ~ log10(size_mm), 
       data = dats)
  
  # Return table
  return(ret)
  
}
