#' Finds which species have matching traits
#'
#' Called if grouping in sizest or get_biomass is “traits”, i.e. if threshold of at 
#' least three distinct measurements or one allometric equation not met at family level. Match 
#' traits of maximum body size, body shape, locomotion and morphological defense across all species #' in the database. Those species with +/- 1 match are added to a species pool to get allometric 
#' equations or size estimates (instead of a taxonomic grouping). Does not proceed if any NA is 
#' found in the trait values of the species
#'
#' @param species_name BWG name of the species
#' @param measurement_table Table containing all measurements
#' @return List of species with matching traits
#' @export
matcher_of_traits <- function(specname, measurement_table){
  # Extract traits of given species
  traits <- 
    measurement_table %>% 
    dplyr::filter(bwg_name == specname) %>% 
    dplyr::select(BS1:BF4)
  
  # NA catcher 
  if(ncol(traits) == ncol(traits[!is.na(colSums(traits))]))
    proceed <- TRUE else
      proceed <- FALSE
    
  # If no NAS in traits get all species that have the same traits +/- 1 and make it a vector
  if(proceed)
    suppressWarnings(spec_list <- 
      measurement_table %>% 
     ## Kept in that format to make explicit changes and allow flexibility in trait matching
      dplyr::filter(BS1 %in% c((traits$BS1 - 1):(traits$BS1 + 1)) &
             BS2 %in% c((traits$BS2 - 1):(traits$BS2 + 1)) &
             BS3 %in% c((traits$BS3 - 1):(traits$BS3 + 1)) &
             BS4 %in% c((traits$BS4 - 1):(traits$BS4 + 1)) &
             LO1 %in% c((traits$LO1 - 1):(traits$LO1 + 1)) &
             LO2 %in% c((traits$LO2 - 1):(traits$LO2 + 1)) &
             LO3 %in% c((traits$LO3 - 1):(traits$LO3 + 1)) &
             LO4 %in% c((traits$LO4 - 1):(traits$LO4 + 1)) &  
             LO5 %in% c((traits$LO5 - 1):(traits$LO5 + 1)) &
             LO6 %in% c((traits$LO6 - 1):(traits$LO6 + 1)) &
             LO7 %in% c((traits$LO7 - 1):(traits$LO7 + 1)) &
             MD1 %in% c((traits$MD1 - 1):(traits$MD1 + 1)) &
             MD2 %in% c((traits$MD2 - 1):(traits$MD2 + 1)) &
             MD3 %in% c((traits$MD3 - 1):(traits$MD3 + 1)) &
             MD4 %in% c((traits$MD4 - 1):(traits$MD4 + 1)) &  
             MD5 %in% c((traits$MD5 - 1):(traits$MD5 + 1)) &
             MD6 %in% c((traits$MD6 - 1):(traits$MD6 + 1)) &
             MD7 %in% c((traits$MD7 - 1):(traits$MD7 + 1)) &
             MD8 %in% c((traits$MD8 - 1):(traits$MD8 + 1)) &
             BF1 %in% c((traits$BF1 - 1):(traits$BF1 + 1)) &
             BF2 %in% c((traits$BF2 - 1):(traits$BF2 + 1)) &
             BF3 %in% c((traits$BF3 - 1):(traits$BF3 + 1)) &
             BF4 %in% c((traits$BF4 - 1):(traits$BF4 + 1))) %>% 
      dplyr::select(bwg_name) %>% 
      unique() %>% 
      dplyr::pull())
  
  # If we have NAs 
  if(!proceed)
    spec_list <- 
      c(specname)

  # Return list
  return(spec_list)
  
}
