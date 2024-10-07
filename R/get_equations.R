#' Get equations for allometry
#'
#' Computes a linear model that will be used as allometric equation
#'
#' @param dats A table with the numerical measurements and biomass used to compute allometric models
#' @param equation_type Which package should be used to compute the models. "lm" is default, "ols"
#' is for lmodel2 (options used are "interval" for both x and y, and 100 permutations)

#' @return Dataframe with computed models at which level, *please double check models*
#' @export
get_equations <- function(dats, equation_type = "lm"){
  # Packages to get equations
  ## lm already in base r
  require(lmodel2) ## OLS 
  
  # Make model to return, depending on which package is asked
  if(equation_type == "lm")
  ret <- 
    lm(log10(mass_mg) ~ log10(size_mm), 
       data = dats)
  else if(equation_type == "ols")
    ret <- 
      lmodel2::lmodel2(log10(mass_mg) ~ log10(size_mm), 
                       range.x = "interval", 
                       range.y = "interval", 
                       nperm = 100,
                       data = dats)
  
  # Return table
  return(ret)
  
}
