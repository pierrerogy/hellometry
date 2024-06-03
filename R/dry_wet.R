#' Parsing dry and wet measurements
#'
#' Filter dataframe based on required biomass type (dry or wet), 
#' or most common one (both)
#'
#' @param dats Input data
#' @param mass_kind If biomass should be wet, dry, most numerous, or both
#' @return List with filtered dataframe and mass kind
#' @export
dry_wet <- function(dats, mass_kind){
  # If biomass is dry or wet
  if(mass_kind %in% c("dry", "wet"))
    {
    ## Filter by value
    dats <- 
        dats %>% 
        dplyr::filter(mass_type == mass_kind)
      
      } 
  else if(mass_kind == "most_numerous") 
      {
        ## First count how many of each
        temp <- 
          dats %>% 
          dplyr::count(mass_type) %>% 
          ### Remove those rows with NA
          dplyr::filter(!is.na(mass_type)) %>% 
          ### Keep only the most common kind of equation
          dplyr::filter(n == max(n))
        
        ## If we have the same number of equations in both dry and wet, prioritise dry
        if(nrow(temp) == 2){
              ### Filter dry
               temp <- 
                 temp %>% 
                 dplyr::filter(mass_type == "dry")
               }
              
        ## Record which one is the best
        mass_kind <- 
          temp$mass_type
        
        ## Extract the selected kind of equation
        dats <- 
          dats %>% 
          dplyr::filter(mass_type == mass_kind)
               }
  
  
  # Return data
  return(list(dats, mass_kind))       
      
}
