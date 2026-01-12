#' Get BWG names
#'
#' Gets dataframe with equation used in estimations
#'
#' @param none Directly loads BWG data to get list of BWG names
#' @return Dataframe with BWG names and corresponding taxonomy
#' @export
get_bwgnames <- function(){
  dats <- 
    read.csv(system.file("extdata", "measurement_table_withdb.csv", 
                         package = "hellometry")) %>% 
    dplyr::select(bwg_name:species, stage) %>% 
    unique() %>% 
    dplyr::filter(!is.na(bwg_name))
  
  ## Return data
  return(dats)       
      
}
