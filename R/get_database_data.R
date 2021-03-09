#' Append measurement table
#'
#' Gets BWG data with or without biomass estimates
#'
#' @param estimated Either data without estimation (FALSE) or with estimations (TRUE)
#' @return The same BWG data, either without or with biomass estimated (i.e. calculated with hello_metry()) 
#' @export
get_database_data <- function(estimated){
  ## Get data
  if(estimated == TRUE)
    (dats <- 
      read.csv(system.file("extdata", "bwg_database_estimated.csv", package = "hellometry"))) else
       if(estimated == FALSE) 
         (dats <- 
           read.csv(system.file("extdata", "bwg_database.csv", package = "hellometry"))) else
            ## estimated has to be TRUE/FALSE
            stop("estimated has to be TRUE/FALSE")
            
   
  ## Return data
  return(dats)       
  
}




