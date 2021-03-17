#' Estimates biomass (mg)
#'
#' Look if raw biomass value exists for given measurement, use it if #' available. Look for at least one allometric equation in a given #' grouping in the following order: species to family, traits, 
#' suborder to phylum. Once threshold of at least one allometric 
#' equation is met: if there is only one equation, compute value of #' biomass; if there is more than one allometric equation, average #' the computed values of biomass.
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

get_biomass <- function(specname, level, size_mm, abundance, stage, path, taxo, equation_table, measurement_table, biomass_kind){
  #browser()
  # Check if species is length_raw, and if size is present
  raw_meas <- 
    measurement_table %>% 
    dplyr::filter(provenance == "length.raw") %>% 
    dplyr::filter(bwg_name == specname,
           size_mm == size_mm) %>% 
    unique()
  
  # Switch for wet and dry measurements
  ## Only keep dry measurements
  if(biomass_kind == "dry")
    (raw_meas <- 
      raw_meas %>% 
       dplyr::filter(biomass_type == "dry")) else
  ## Just keep the most numerous
        c(type_count <- 
          raw_meas %>% 
          dplyr::count(biomass_type) %>% 
          ### Keep only the most common kind of measurement
            suppressWarnings(dplyr::filter(n == max(n))),
          ## If we have the same number of measurements in both dry and wet, prioritise dry
          ifelse(nrow(type_count) == 2,
                 (type_count <- 
                   type_count %>% 
                    dplyr::filter(biomass_type == "dry")),
                 type_count <- 
                   type_count),
          raw_meas <- 
            raw_meas %>% 
            dplyr::filter(biomass_type == type_count$biomass_type))
        
  
  ## If both are present, then we do not need to compute
  ## or at least just take the mean of the directly measured biomasses
  if(nrow(raw_meas) > 0)
    c(compute <- FALSE,
      biomass <- mean(raw_meas$biomass_mg),
      path <- paste0(path, "-raw_", unique(raw_meas$biomass_type)),
      return(data.frame(biomass, as.character(path)))) else
      compute <- TRUE
    
  # Get allometry information of species 
  ## If using taxonomy, filter taxonomy of species of interest 
  if(level != "traits")
    ### Create blank dataframe if no trophic level (if NA)
    if(do.call(paste, list(taxo[,level])) == "NA")
      allometry <- data.frame() else
         allometry <-
            equation_table %>% 
          dplyr::filter(equation_table[,level] == do.call(paste, list(taxo[,level]))) %>% 
          dplyr::filter(!is.na(intercept) | !is.na(ln_intercept)) %>% 
            dplyr::select(biomass_type, intercept, slope, ln_intercept) %>% 
            unique()
  ## If using trait, get custom list of species with other function
  if(level == "traits")
    c(spec_list <- 
        matcher_of_traits(specname, measurement_table),
      allometry <-
        equation_table %>% 
        dplyr::filter(equation_table$bwg_name %in% spec_list) %>% 
        dplyr::filter(!is.na(intercept) | !is.na(ln_intercept)) %>% 
        dplyr::select(biomass_type, intercept, slope, ln_intercept) %>% 
        unique())
      
  # Switch for wet and dry measurements    
  if(nrow(allometry > 0))
    ## Only keep dry measurements
    c(if(biomass_kind == "dry")
      c(allometry <- 
          allometry %>% 
          dplyr::filter(biomass_type == "dry"),
        type_count <- 
          allometry %>% 
          dplyr::count(biomass_type)) else 
            ## Count instance of equations based on dry and wet weight
            c(type_count <- 
                allometry %>% 
                dplyr::count(biomass_type) %>% 
                ### Keep only the most common kind of equation
                suppressWarnings(dplyr::filter(n == max(n))),
              ## If we have the same number of equations in both dry and wet, prioritise dry
              ifelse(nrow(type_count) == 2,
                     type_count <- 
                       type_count %>% 
                       dplyr::filter(biomass_type == "dry"),
                     type_count <- 
                       type_count),
              ## Extract the selected kind of equation
              allometry <- 
                allometry %>% 
                dplyr::filter(biomass_type == type_count$biomass_type)))
  
  # Get number of equations
   equation_number <- 
     nrow(allometry)
  
  # Make bifurcation depending on number of equations
  if(compute)
    ## Number of equations
    if(equation_number == 0) 
      equations <- "none"  else 
        if(equation_number == 1) 
          equations <- "one" else
            if(equation_number > 1) 
              equations <- "multiple"
      
  # Case 1: one equation for the level
  if(equations == "one")
    ## simply compute the biomass using correct equation
    c(biomass <- equation_finder(size_mm, allometry, taxo) * abundance,
      path <- paste0(path,"-AE:", level, "_",equation_number, "_", allometry$biomass_type[1]))
      
    
  # Case 2: more than one equation at the level
  if(equations == "multiple") 
    c(## Unite values in type count for path
      ## Set biomass to zero
      biomass <- 0,
    ## Sum biomass obtained from the different equations
    for(j in 1:equation_number){
      eq <- allometry[j,]
      biomass <- biomass + equation_finder(size_mm, eq, taxo) * abundance
    },
    ## Average the result
    biomass <- biomass/equation_number,
    ## Add equation information to path
    path <- paste0(path,"-AE:", level, "_",equation_number, "_", type_count$biomass_type[1]))
    
  # Case 3: no equations at the level
  ## Simply return NA
  if(equations == "none")
      biomass <- NA
    
  # Return computed biomass
  return(data.frame(biomass, as.character(path)))
}
