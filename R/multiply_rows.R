#' Multiply rows based on abundance
#'
#' In order to have one row per measurement (e.g. if several
#' individuals had the same size)
#'
#' @param dats The data, needs to have a column called abundance
#' @return Original data frame with rows mutiplied based on abundance
#' @export
multiply_rows <- function(dats){
  # Filter data based on abundance
  dats_abundance <- 
    dats %>% 
    dplyr::filter(abundance > 1)
  
  # If we have more than one
  if(nrow(dats_abundance) > 0){
    c(## Small new data frame to store extra rows
    dats_bind <- 
      data.frame(),
    ## Loop to multiply row and add to dats_bind
    for(k in 1:nrow(dats_abundance)){
      ### Get row
      row <- 
        dats_abundance[k,]
      ### Bind by how many abundance - 1, because original row is preserved
      for(l in 1:(row$abundance - 1)){
        dats_bind <- 
          dats_bind %>% 
          dplyr::bind_rows(row)
      }
      
    },
    ### Add to original data
    dats <- 
      dats %>% 
      dplyr::bind_rows(dats_bind)
    
    )
    
  } 
  
  # If we don't have any just return original data

  # Return data
  return(dats %>% 
           dplyr::select(-abundance))       
      
}
