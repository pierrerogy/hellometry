#' Append measurement table with new measurements
#'
#' Append measuremement table with new measurements, because this table
#' also compiles measurements for all species
#' 
#' @param measurement_table Table containing all measurements
#' @param data_table The input data table
#' @return An updated measurement table that will be used to estimate sizes and gather taxonomy
#' @export
append_measurements <- function(measurement_table, data_table){
  # Vector of taxonomy columns 
  taxo_vec <- 
    c("bwg_name", "domain", "kingdom", "phylum", 
      "subphylum", "class", "subclass", "ord", 
      "subord", "family", "subfamily", "tribe", 
      "genus", "species")
  
  # Select columns based on possible range of taxonomy
  # We should not expect to have all of them
  # so use any_of
  suppressWarnings(
    data_stub <- 
      data_table %>% 
      ## Only keep numerical measurements
      ## Suppress warnings too
      dplyr::mutate(size_mm = as.numeric(size_mm)) %>% 
      dplyr::filter(!is.na(size_mm)) %>% 
      dplyr::select(dplyr::any_of(taxo_vec),
                    stage, abundance, size_mm, 
                    biomass_mg, biomass_type))
  
  # Bind new species to measurement table
  measurement_table <-
    measurement_table %>% 
    dplyr::bind_rows(data_stub)
  
  return(measurement_table)
  
}




