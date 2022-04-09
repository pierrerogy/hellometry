#' When everything else fails, call the firefighters to bring some literature equations
#'
#' Estimates biomass with equations from literature. Handles a number of 
#' equation for each taxonomic class.
#'
#' @param size_input Input size to be used in equation
#' @param abundance Input abundance to be used in equation
#' @param taxo Taxonomy information of the species
#' @return Computed biomass, confidence intervals as strings, and path
#' @export
firetruck <- function(size_input, abundance, taxo){
  # Load data
  extra_eq <- 
    read.csv(system.file("extdata", "extra_equations.csv", 
                         package = "hellometry"))
  
  ## Only keep equations of interest
  extra_eq <- 
    extra_eq %>% 
    dplyr::filter(class == taxo$class)
  
  ## Number of equations
  if(nrow(extra_eq) == 0) 
    equations <- "none"  else 
      if(nrow(extra_eq) == 1) 
        equations <- "one" else
          if(nrow(extra_eq) > 1) 
            equations <- "multiple"
  
  # Case 1: one equation for the level
  if(equations == "one")
    ## simply compute the biomass using correct equation
    c(biomass <- firehose(size_input, extra_eq, taxo) * abundance,
      path <- paste0(path,"-AE:external_eq"))
  
  
  # Case 2: more than one equation at the level
  if(equations == "multiple") 
    c(## Unite values in type count for path
      ## Set biomass to zero
      biomass <- 0,
      ## Sum biomass obtained from the different equations
      for(j in 1:nrow(extra_eq)){
        eq <- extra_eq[j,]
        biomass <- biomass + firehose(size_input, eq, taxo) * abundance
      },
      ## Average the result
      biomass <- biomass/nrow(extra_eq))
  
  # Case 3: no equations at the level
  ## Simply return NA
  if(equations == "none")
    biomass <- NA
  
  # Return computed biomass
  return(c(as.character(biomass), rep("external_eq", 2)))
}
