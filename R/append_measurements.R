#' Append measurement table
#'
#' Append measurmement table with size in mm, to be used for size estimation
#'
#' @param measurement_table, data_table
#' @return An updated measurement table that will be used to estimate sizes
#' @export
append_measurements <- function(measurement_table, data_table){
  # Keep only numerical measurements of data table
  # NEED TO ADD IN CASE HAS DRY/WET ALSO
  data_table_num <- 
    data_table %>% 
    dplyr::select(bwg_name, size_mm, abundance) %>% 
    dplyr::mutate(size_mm = as.numeric(size_mm)) %>% 
    dplyr::filter(!is.na(size_mm),
           abundance > 0) %>% 
    dplyr::group_by(bwg_name, size_mm) %>% 
    dplyr::summarise_all(sum)
    
  # Make stub of measurement_table 
  measurement_stub <- 
    measurement_table %>% 
    dplyr::select(species_id:BF4) %>% 
    unique()
  
  # Join to numerical measurements
  data_table_num <- 
    data_table_num %>% 
    dplyr::left_join(measurement_stub)
  
  # Bind to measurement table
  measurement_table <- 
    measurement_table %>% 
    dplyr::bind_rows(data_table_num)
  
  # Return updated data
  return(measurement_table)
  
}




