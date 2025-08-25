#' Finds which species have matching traits
#'
#' Evaluates all unique trait groupings and returns species that have the same 
#' traits +/- 1
#'
#' @param measurement_table A table with the numerical measurements and biomass 
#' used to compute allometric lms 
#' @param trait_columns List of traits to match, should be column names in measurement_table


#' @return Tibble of four columns: level = "traits", group_focus_species, stage of focus species, 
#' bwg_name of matched species
#' @export
#' 
  matcher_of_traits <- function(measurement_table, trait_columns) {
    
    # First filter dataset with unique groupings of traits
    measurement_table <- 
      measurement_table %>% 
      dplyr::select(bwg_name, dplyr::all_of(trait_columns), stage) %>% 
      unique()
    
    # Print a little message saying that we are grouping species by traits
    print("Grouping species by trait similarities..")
    
    # Set progress bar with number of rows
    pb <- 
      progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", 
                                 total = nrow(dats))
    ## Initiate it
    pb$tick(0)
    
    # For each row of the filtered dataframe
    ret <- 
      purrr::map_dfr(seq_len(nrow(measurement_table)), function(i) {
      
      ## Extract name of focus species
      focus_species <- 
        measurement_table$bwg_name[i]
      ## Extract stage of focus species
      stage <- 
        measurement_table$stage
      ## Get traits, make numeric so that it goes a lot faster
      focus_traits <-
        as.numeric(measurement_table[i, trait_columns]) 
      
      ## Make a small dataframe with species with similar traits
      similar <- 
        dats %>%
        ## Look row by row
        dplyr::rowwise() %>%
        ## Check trait similarities based on absolute values
        dplyr::filter(all(abs(dplyr::c_across(dplyr::all_of(trait_columns)) - focus_traits) <= 1)) %>%
        ## Ungroup
        ungroup()
      
      ## Add tick to progress bar
      pb$tick()
      
      ## Make a little catch in case we do not find species with similar traits
        if (nrow(similar) == 0) 
         { ## Returning an empty tibble
          return(tibble::tibble(level = character(), 
                                name = character(), 
                                stage = character(),
                                bwg_name = character()))} else
      
      ## But if we get some species with similar traits
        {## Make data long format
          return(similar %>%
                   dplyr::transmute(level = "traits",
                                    name = focus_species,
                                    stage = stage,
                                    bwg_name = bwg_name))}
                  
      })
    

    # Return
    return(ret)
  
}

  
