# Update measurement table --------------------------------------------------
measurement_supplement <- function(measurement_table, data_table){
  # Keep only numerical measurements of data table
  data_table_num <- 
    data_table %>% 
    dplyr::select(bwg_name, size_mm) %>% 
    filter(!is.na(as.numeric(size_mm))) %>% 
    unique %>% 
    rename(length_mm = size_mm) %>% 
    mutate(length_mm = as.numeric(length_mm))
  
  # Make stub of measurement_table 
  measurement_stub <- 
    measurement_table %>% 
    dplyr::select(species_id:BF4) %>% 
    unique()
  
  # Join to numerical measurements
  data_table_num <- 
    data_table_num %>% 
    left_join(measurement_stub)
  
  # Bind to measurement table
  measurement_table <- 
    measurement_table %>% 
    bind_rows(data_table_num)
  
  # Return updated data
  return(measurement_table)
  
}




