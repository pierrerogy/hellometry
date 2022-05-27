#' Filling in biomass blanks
#'
#' Add biomass estimates to a dataframe that has some biomass 
#' estimations but not everywhere. Also gives you the opportunity
#' to use the algorithm on non-BWG species.
#' 
#' Please name column with number of specimen "abundance", column with biomass "biomass_mg",
#' column with BWG name "bwg_name",  and column with measurement in mm "size_mm".
#' If you do not have a numerical measurement for a given specimen, the algorithm can
#' do size estimations if you input "small", "medium", "large" or "unknown". In this
#' case, the algorithm will use existing size measurements for the species, and use 
#' the size distribution to estimate "small", "medium" and "large" inputs, or a weighted
#' average of all measurements if the input is "unknown".
#' 
#' @param data_table The input data table
#' @return Your initial data table with three extra columns: value (in mm)
#' of size estimation (NA if not done), total biomass for given row (in mg), 
#' as well as path taken through the function. The nomenclature of this path column is as follows.
#' ## Special cases
#'- "null_biomass": abundance was 0
#'- "_size_estimaton_failed": there were not enough close relatives or species with matching traits to compute size
#'- "_biomass_estimation_failed": there were no allometric equation at to estimate biomass
#'- "_raw_dry/wet": if there was a raw (direct) measurement for that particular species-size combination, and whether that measurement is dry or wet biomass
#'- "_external_dry_equations": data provided was not sufficient so equations from the literature were used
#' ## Regular cases
#'- if the species goes through estimation of size (sizest()): 
#'  estimation kind:level_number of measurements.
#'  For example,  WA:genus_5 (size estimation using weighted average on 5 measurements from the species' genus), BIN:subfamily_3 (size estimation using size bins (S, M, L) on three measurement a the subfamily level)
#'- if the allometric equations are used (get_biomass()): 
#'  biomass estimation:taxonomic level of inference_number of equations_dry/wet.
#'  For example, -AE:bwg_name_wet (allometric equation from wet biomass at the species level), -AE:subclass_dry (allometric equation from dry biomass at the subclass level)
#' # If a species went through both size estimation and biomass estimations, the path will be composite, e.g. WA:genus_5-AE:bwg_name_wet

#' @export
fill_blanks <- function(data_table){
  
  # Separate rows with and without biomass
  ## With
  data_table_with <- 
    data_table %>% 
    dplyr::filter(!is.na(biomass_mg))
  
  ## Without
  data_table_without <- 
    data_table %>% 
    dplyr::filter(is.na(biomass_mg))
  
  # do hellometry on part without
  
  # make the columns of the two match
  
  # add together with olumn that size raw or estimated
  
  
  
  
}

data_table <- 
  readr::read_csv(here::here("bichos_biomass_est.csv")) %>% 
  dplyr::rename(biomass_mg = dry_mass_mg)
