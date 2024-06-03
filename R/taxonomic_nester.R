#' Split data to get estimation of size and allometric equations for all levels
#'
#' Once a custom threshold measurements is reached,
#' computes a linear model that will be used as allometric equation
#'
#' @param dats A table with the numerical measurements and biomass used to compute allometric models
#' @param equation_type Which package should be used to compute the models. LIST TBD
#' @param mass_kind Should data used in inference be "dry" or "wet", "most_common" or "both"
#' @param traits Custom pathway for traits TBD
#' @param cutoff Custom cutoff of the number of measurements for a given level above which the model will be computed
#' @param taxa list of taxa at which to compute the equations, must match column names of your data

#' @return Dataframe wiht computed models at which level
#' @export
taxonomic_nester <- function(dats, equation_type, mass_kind = "most_numerous", traits = F, cutoff = 3,
                             size = TRUE, equations = TRUE, 
                          taxa =
                            c("domain", "kingdom", "phylum", "subphylum", "class", "subclass", 
                              "order", "suborder", "family", "subfamily", "tribe", "genus", "species", "bwg_name")){

  # Add error checks
  error_check(dats, 
              mass_kind)
  
  # Switch for wet and dry measurements
  type_count <- 
    dry_wet(dats,
            mass_kind)
  measurement_table <- 
    type_count[[1]]
  type_count <- 
    type_count[[2]]
  
  # TRAITS
  
  
  # Make data frame to return
  ret <- 
    tibble::tibble()
  
  
  # For each level in each column get the equation
  for(level in taxa){
    ## Select columns and filter rows
    temp <- 
      dats %>% 
      dplyr::select(dplyr::all_of(level), stage, size_mm, mass_mg, abundance) %>% 
      ### Convert size and biomass column to numeric
      dplyr::mutate(size_mm = as.numeric(size_mm),
                    mass_mg = as.numeric(mass_mg)) %>%
      tidyr::drop_na() %>% 
      ### Multiply rows by abundance
      multiply_rows() %>% 
      ### Group by taxon and stage
      dplyr::group_by(.[1], stage) %>% 
      ### Filter at cutoff
      dplyr::add_count(.[1], stage) %>%
      filter(n > cutoff) %>%
      dplyr::select(-n) %>%
      ### Nest
      tidyr::nest()
    
    ## Estimate size or equation, depending on which one is asked
    if(size)
      temp <- 
        temp %>% 
        dplyr::mutate(size_estimated = purrr::map(data, 
                                                  ~ sizest(.x)))
    if(equations)
      temp <- 
        temp %>% 
        dplyr::mutate(model = purrr::map(data, 
                                         ~ get_equations(.x, 
                                                        equation_type)))
    ## Tidying
    temp <- 
      temp %>% 
      ### Add some cleaning
      dplyr::mutate(level = level,
                    biomass_type = type_count) %>% 
      dplyr::rename(taxon = 1)
    
    # Add to ret
    ret <- 
      ret %>% 
      dplyr::bind_rows(temp)
    
  }
  
  
  # # Prepare data depending on level given
  # if(level != "traits")
  #   c(if(do.call(paste, list(taxo[,level])) == "NA")
  #     dats <- data.frame() else 
  #       dats <- 
  #         measurement_table %>% 
  #         ## Filter by correct level
  #         dplyr::filter(measurement_table[,level] == do.call(paste, list(taxo[,level]))) %>% 
  #         dplyr::filter(stage == taxo$stage)) else
  #       c(spec_list <- 
  #           matcher_of_traits(specname, measurement_table),
  #         dats <-
  #           measurement_table %>% 
  #           dplyr::filter(measurement_table$bwg_name %in% spec_list))
  
  # Do some cleaning, just because I am picky
  ret <- 
    ret %>% 
    dplyr::select(level, type, taxon, stage, level, model, size_estimated)
  
  # Return table
  return(ret)
  
}
