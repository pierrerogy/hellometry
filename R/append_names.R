#' Append measurement table with new species names
#'
#' Append measuremement table with new species and measurements, because this table
#' also compiles taxonomy for all species
#' 
#' @param measurement_table Table containing all measurements
#' @param data_table The input data table
#' @return An updated measurement table that will be used to estimate sizes and gather taxonomy
#' @export
append_names <- function(measurement_table, data_table){
  # Make stub of measurement_table 
  measurement_stub <- 
    measurement_table %>% 
    dplyr::select(species_id:stage) %>% 
    unique()
  
  # Check if any species not present in the database
  data_stub <- 
    data_table %>% 
    dplyr::select(bwg_name, domain:species, stage) %>% 
    unique() %>% 
    ## Remove all species present in the database already
    dplyr::anti_join(measurement_stub,
                     by = c("bwg_name", "domain", "kingdom", "phylum", 
                            "subphylum", "class", "subclass", "ord", 
                            "subord", "family", "subfamily", "tribe", 
                            "genus", "species", "stage"))
  
  # Bind new species to measurement table
  measurement_table <-
    measurement_table %>% 
    dplyr::bind_rows(data_stub)
  
  return(measurement_table)
  
}




