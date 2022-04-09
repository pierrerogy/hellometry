#' Get equation for allometry
#'
#' Once a threshold of three length measurements is reached,
#' computes a linear model that will be used as allometric equation
#'
#' @param measurement_table A table with the numerical measurements and biomass used to compute allometric lms 
#' @param specname  BWG name of the species
#' @param taxo Taxonomic information about the species
#' @param level Taxonomic level of estimation
#' @param biomass_kind Should data used in inference be "dry" for just dry biomass, or "both" (default) for both dry and wet biomass.
#' @return List with allometric equations used in estimations, and biomass kind
#' @export
get_equation <- function(measurement_table, specname,
                         taxo, level, biomass_kind){
  # Switch for wet and dry measurements
  type_count <- 
    dry_wet(measurement_table,
            biomass_kind)
  measurement_table <- 
    type_count[[1]]
  type_count <- 
    type_count[[2]]
   
 
  # Prepare data depending on level given
  if(level != "traits")
    c(if(do.call(paste, list(taxo[,level])) == "NA")
      dats <- data.frame() else 
        dats <- 
          measurement_table %>% 
          ## Filter by correct level
          dplyr::filter(measurement_table[,level] == do.call(paste, list(taxo[,level])) &
                          measurement_table$stage == taxo$stage)) else
        c(spec_list <- 
            matcher_of_traits(specname, measurement_table),
          dats <-
            measurement_table %>% 
            dplyr::filter(measurement_table$bwg_name %in% spec_list))

  # Remove useless rows those with NA values
  if(nrow(dats > 0))
    c(dats <- 
        dats %>% 
        dplyr::mutate(size_mm = as.numeric(size_mm)) %>% 
        dplyr::filter(!is.na(size_mm)) %>% 
        dplyr::filter(!is.na(biomass_mg)),
      # Make each row a unique measurement
      dats <- 
        multiply_rows(dats))

  # Make lm for each species, 
  # only make lms if more than three measurements
  if(nrow(dats) > 2)
    model <- 
      lm(log10(biomass_mg) ~ log10(size_mm),
         data = dats) else
           (model <- 
             NA)
    
  # Return table
  return(list(model, type_count))
  
}
