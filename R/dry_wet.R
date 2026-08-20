#' Parsing dry and wet measurements
#'
#' Filter dataframe based on required biomass type (dry or wet).
#'
#' @param dats Input data
#' @param biomass_type If biomass should be wet or dry, or most common
#' @return Dataframe with filtered biomass type, and chosen one (dry or wet)
#' @examples
#' # Data holding both kinds of biomass
#' dats <-
#'   data.frame(species = c("sp_a", "sp_b"),
#'              stage = "larva",
#'              size_col = c(3.1, 2.2),
#'              biomass_col = c(0.4, 0.2),
#'              biomass_type = c("dry", "wet"))
#'
#' # Keep the dry measurements only
#' dry_wet(dats, biomass_type = "dry")
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