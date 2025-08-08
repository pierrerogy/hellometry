#' Build measurement table to be used for biomass estimations
#'
#' Gets dataframe with equation used in estimations
#'
#' @param dats Dataframe to be used for estimations
#' @param level_list List of taxonomic levels to be used in the measurement table
#' @param database Logical. Should data from the BWG database be used to supplement 
#' a set of BWG-specific measurements, not present in the database 
#' (TRUE(default)/FALSE). This function uses numerical measurements both from the 
#' BWG database and from the data you provide. Only put FALSE if you are doing 
#' estimations from data already present in the BWG database (estimation for 
#' these data available with `database_data(TRUE)`).
#' @param nothing Logical. If TRUE, no BWG-specific data will be used for estimations, 
#' only the data you provide. This is useful if you have your own measurement 
#' database or work on a different system (default FALSE).
#' @return A table with measurements and species names, ready to be used for 
#'  size and biomass estimations
#' @export
make_measurement_table <- function(dats, level_list, database, nothing){
  
  # Get numeric values to add to measurement table
  ## Will lead warnings for non-numerical measurements, so wrap in suppressWarnings
  suppressWarnings(
    numeric_taxa <- 
      dats %>%
      dplyr::mutate(size_mm = as.numeric(size_mm)) %>% 
      dplyr::filter(!is.na(size_mm)))
  ## Little catch here if we are using the BWG database
  if("provenance" %in% colnames(numeric_taxa)){
    numeric_taxa <- 
      numeric_taxa %>% 
      dplyr::filter(provenance == "length.raw")}
  
  # Get measurement table
  ## If database is TRUE, get the BWG database measurements
  ## If nothing is TRUE, return empty tibble
  ret <- 
    get_measurements(level_list = level_list,
                     database = database, 
                     nothing = nothing)
  
  # Add all species name to measurement table
  ret <- 
    append_names(measurement_table = ret, 
                 dats = dats, 
                 level_list = level_list,
                 nothing = nothing)
  
  # Add all numerical measurements to measurement table
  ## A species can have a name but no measurement so done in two steps 
  ## (append_name then append_measurements)
  ret <- 
    append_measurements(measurement_table = ret,
                        dats = numeric_taxa, 
                        level_list = level_list) %>% 
    ## Have each row representing an individual
    ## First make sure all 0s are 1s so that no row is removed
    dplyr::mutate(abundance = ifelse(abundance == 0 | is.na(abundance), 
                                     1, abundance)) %>% 
    ## Then use tidyr::uncount() to make each row a unique measurement
    tidyr::uncount(abundance)
  
  # Limoniidae and Tipulidae essentially the same thing so can be considered together
  # If family exists in column names
  if("family" %in% colnames(ret)){
    measurement_table <- 
      tipulimo(ret)}  
  
  # Return measurement table
  return(ret)
  
}
