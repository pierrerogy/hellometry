#' Parsing dry and wet measurements
#'
#' Filter dataframe based on required biomass type (dry or wet).
#'
#' @param dats Input data
#' @param biomass_type If biomass should be wet or dry, or most common
#' @return Dataframe with filtered biomass type, and chosen one (dry or wet)
#' @export
dry_wet <- function(dats, biomass_type){

  # Simply filter based on chosen category
  dats <-
    dats %>%
    ## Filter based on input
    ## .data$ is the column, .env$ is the function argument
    ## (without these, both sides resolve to the column and nothing is filtered)
    dplyr::filter(.data$biomass_type == .env$biomass_type)
  
  # Return data
  return(dats)
}