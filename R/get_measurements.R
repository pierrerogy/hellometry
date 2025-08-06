#' Get measurement table
#'
#' Gets dataframe with equation used in estimations
#'
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
#' @return Dataframe with equation used in estimations
#' @export
get_measurements <- function(level_list, database, nothing = FALSE){
  if(database == TRUE)
    # Do we want the database data?
    {dats <- 
      read.csv(system.file("extdata", 
                           "measurement_table_withdb.csv", 
                           package = "hellometry"))} else
    # Or just extra measurements  
         {dats <- 
           read.csv(system.file("extdata", 
                               "measurement_table_nodb.csv", 
                               package = "hellometry"))}
  
    
                           
  # Or even nothing
  if(nothing == TRUE)
    {dats <- 
      tibble::tibble()} else
        # If we use the database, keep columns that we want
        ## With a catch for traits not in species names
        if(nothing == FALSE)
        {dats <- 
          dats %>% 
          dplyr::select(species_id, dplyr::all_of(level_list[level_list %notin% "traits"]),
                        functional_group:abundance)}
                          
  # Return data
  return(dats)       
  
}
