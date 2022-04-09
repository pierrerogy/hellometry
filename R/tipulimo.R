#' Small function to control for Tipulidae/Limoniidae name change
#'
#' Gets dataframe with equation used in estimations
#'
#' @param data Dataframe with families as separate entries
#' @return Dataframe with families put together
#' @export
tipulimo <- function(dats){
  ## Change name
  dats <- 
    dats %>% 
    dplyr::mutate(family = ifelse(family %in% c("Tipulidae", "Limoniidae"),
                                  "Tipulidae_Limoniidae", family))
  ## Return data
  return(dats)       
      
}
