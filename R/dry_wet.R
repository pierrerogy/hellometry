#' Parsing dry and wet measurements
#'
#' Filter dataframe based on required biomass type (dry or wet), 
#' or most common one (both)
#'
#' @param dats Input data
#' @param biomass_kind If biomass should be wet or dry
#' @return Dataframe with filtered biomass kind, and chosen one (dry or wet)
#' @export
dry_wet <- function(dats, biomass_kind){
  # If biomass is dry
  if(biomass_kind == "dry")
    c(dats <- 
        dats %>% 
        dplyr::filter(biomass_type == "dry"),
      type_count <- 
        dats %>% 
        dplyr::count(biomass_type)) else 
          ## Count instance of equations based on dry and wet weight
          c(type_count <- 
              dats %>% 
              dplyr::count(biomass_type) %>% 
              ## Remove those rows with NA
              dplyr::filter(!is.na(biomass_type)) %>% 
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
            dats <- 
              dats %>% 
              dplyr::filter(biomass_type == type_count$biomass_type))
  
  # Return data
  return(list(dats, type_count$biomass_type))       
      
}
