#' Estimates biomass (mg)
#'
#' Looks if raw biomass value exists for given measurement, use it if #' available. 
#' Looks for at least three length measurement to compute allometric equation in a given 
#' grouping in the following order: species to family, traits, 
#' suborder to class. Once threshold of at least three length measurements 
#' equation is met calls the get_equation function to compute equation
#'
#' @param specname  BWG name of the species
#' @param level Taxonomic level of estimation
#' @param size_mm Size in mm
#' @param abundance Abundance of individuals of the species/size combination
#' @param stage Developmental stage of the invertebrate (larva/pupa/adult)
#' @param path Path taken trough function
#' @param taxo Taxonomic information about the species
#' @param equation_table Table containing all allometric equation
#' @param measurement_table Table containing all measurements
#' @param biomass_kind Should data used in inference be "dry" for just dry biomass, or "both" (default) for both dry and wet biomass.
#' If both (the default) is chosen, then the function will determine which dry or wet equations or raw weight is present, and choose
#' the most numerous. If there is the same number of dry and wet equations, dry equations are always favoured. Dry and wet
#' measurements are never mixed.
#' @return new column with estimate biomass value 
#' @export

get_biomass <- function(specname, level, size_input, abundance, stage, path, taxo, measurement_table, biomass_kind){
  #browser()
  # Check if species is length_raw, and if size is present
  raw_meas <- 
    measurement_table %>% 
    dplyr::filter(provenance == "length.raw") %>% 
    dplyr::filter(bwg_name == specname,
           size_mm == size_input,
           stage == stage) %>% 
    unique()
  
  # Switch for wet and dry measurements
  type_count <- 
    dry_wet(raw_meas,
            biomass_kind)
  raw_meas <- 
    type_count[[1]]
  type_count <- 
    type_count[[2]]
  
  ## If both are present, then we do not need to compute
  ## or at least just take the mean of the directly measured biomasses
  if(nrow(raw_meas) > 0)
    c(compute <- FALSE,
      biomass <- c(mean(raw_meas$biomass_mg), NA, NA),
      path <- paste0(path, "-raw_", unique(raw_meas$biomass_type)),
      return(c(biomass, as.character(path)))) else
      (compute <- TRUE)
  
  # Make bifurcation depending on number of equations
  if(compute)
    c(# Get equation and suppress warnings from function
    suppressWarnings(
    eq <- 
      get_equation(measurement_table, specname,
                   taxo, level, biomass_kind)),
    ## Check if presence of equation, input can be model so suppress warnings
    suppressWarnings(
    if(is.na(eq[[1]])) 
      equations <- "none"  else 
        if(!is.na(eq[[1]])) 
          equations <- "one"))
      
  # Case 1: one equation for the level
  if(equations == "one")
    ## simply compute the biomass using correct equation
    c(biomass <- equation_user(size_input, eq[[1]], taxo) * abundance,
      path <- paste0(path,"-AE:", level, "_", eq[[2]]))
      
  # Case 3: no equations at the level
  ## Simply return NA
  if(equations == "none")
      biomass <- rep(NA, 3)
    
  # Return computed biomass
  return(c(biomass, 
           as.character(path)))
}
