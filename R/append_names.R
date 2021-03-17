#' Append measurement table with new species names
#'
#' Append measuremement table with new species, because this table
#' also compiles taxonomy for all species
#' 
#' @param measurement_table Table containing all measurements
#' @param data_table The input data table
#' @return An updated measurement table that will be used to estimate sizes and gather taxonomy
#' @export
append_names <- function(measurement_table, data_table){
  # Add rows to measurement table
  measurement_table <- 
    measurement_table %>%
    dplyr::bind_rows(data_table %>% 
                       ## Only select taxonomy rows
                       dplyr::select(bwg_name:species) %>% 
                       unique() %>% 
                       ## Use anti join to only keep those not present in the measurement table
                       dplyr::anti_join(measurement_table %>% 
                                          dplyr::select(bwg_name:species) %>% 
                                          unique,
                                        by = c("bwg_name", "domain", "kingdom", 
                                               "phylum", "subphylum", "class", "subclass", 
                                               "ord", "subord", "family", "subfamily", "tribe", 
                                               "genus", "species")))
  
  # Return updated data
  return(measurement_table)
  
}




