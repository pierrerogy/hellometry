#' Returns size and biomass estimations for every taxon in measurement table
#'
#'
#' @param level_list Vector of taxonomic levels over which to iterate estimations
#' @param measurement_table A table with the numerical measurements and biomass 
#' used to compute allometric lms 
#' @param what Should size (what = "size_mm") or biomass (what = "biomass_mg") be 
#' estimated? 
#' @param model What kind of model should be computed, so far only lm
#' @param traits Should the table be computed by traits or not? Default FALSE
#' @param trait_columns List of traits to match, should be column names in measurement_table
#' @return Tibble with size estimations or allometric models used in estimations. 
#' Note that only models with p < 0.05 and R^2 < 0.95 are kept.
#' @export
#' 
full_estimation_table <- function(level_list, measurement_table,
                                  what, model = "lm", traits = FALSE,
                                  trait_columns = c()){ 
    
  # If we want to get estimations with traits
  if(traits == T) {
    ## Use trait functions to make groupings of similar trait species
    ret <- 
      make_trait_table(measurement_table = measurement_table,
                       trait_columns = trait_columns)
    
  }
  
  # If we do not want traits
  if(traits == F) {
  # Get all possible combinations of data
    ret <- 
      ## Split into list of tibbles, which each containing level and size_mm
      purrr::map(
        level_list[level_list != "traits"],
        ~ measurement_table %>%
          ## Select only the level and size_mm
          dplyr::select(name = dplyr::all_of(.x), 
                        stage, size_mm, biomass_mg) %>%
          ## Remove NAs, name being the taxonomic level
          dplyr::filter(!is.na(name) & !is.na(size_mm)) %>% 
          ## Add column with level name
          dplyr::mutate(level = .x)) %>% 
      ## Remove empty levels 
      purrr::keep(function(x) nrow(x) > 0) %>% 
      ## Filter out those groups with less than three unique measurements
      purrr::map(
        .,
        ~ .x %>%
            ## Group by level, name and stage
            dplyr::group_by(level, name, stage) %>%
            ## Filter out those with less than three unique size_mm
            dplyr::filter(dplyr::n_distinct(size_mm) >= 3) %>%
            ## Ungroup to avoid future issues
            dplyr::ungroup()) %>% 
      ## Remove empty levels 
      purrr::keep(function(x) nrow(x) > 0)}
    

  # If size we need the weighted average and size bins
    if(what == "size_mm"){
      ret <- 
        ret %>% 
        purrr::map(.,
                   ~ .x %>%
                     ## Group by name
                     dplyr::group_by(level, name, stage) %>%
                     ## Make a column with terciles of sizes
                     dplyr::mutate(size_group = ntile(size_mm, 3)) %>%
                     ## Group by size group and get mean of each
                     dplyr::group_by(level, name, stage, size_group) %>%
                     dplyr::summarise(
                       size_mm = mean(size_mm),
                       .groups = "drop") %>%
                     ## Create a new column with size category
                     dplyr::mutate(size_category = dplyr::case_when(
                       size_group == 1 ~ "small",
                       size_group == 2 ~ "medium",
                       size_group == 3 ~ "large")) %>%
                     ## Keep relevant columns
                     dplyr::select(level, name, stage, size_category, size_mm) %>% 
                     ## Add overall mean
                     dplyr::bind_rows(
                       .x %>%
                         # Get average grouping by level and name
                         group_by(level, name, stage) %>%
                         summarise(size_mm = mean(size_mm), 
                                   .groups = "drop") %>%
                         ### Make size category column (to fit this average with unknown)
                         dplyr::mutate(size_category = "unknown"))) %>% 
        ## Flatten
        purrr::list_c()}
                   
                   
    # If biomass we need allometric equations               
    if(what == "biomass_mg") {
      ## Prepare data in common for models
      ret <- 
        ret %>% 
        ### Since some organisms can have known size but unknown biomass
        ### We also need to filter here!
        purrr::map(.,
                   ~ .x %>%
                     ### First remove all NAs in biomass
                     dplyr::filter(!is.na(biomass_mg)) %>% 
                     ### Group by level, name and stage
                     dplyr::group_by(level, name, stage) %>%
                     ### Filter out those with less than three unique size_mm
                     dplyr::filter(dplyr::n_distinct(biomass_mg) >= 3)) %>% 
        ## Remove empty levels 
        purrr::keep(function(x) nrow(x) > 0)
      
      ## If we want basic lms
      if(model == "lm"){
        ret <- 
          ret %>%
          purrr::map(.,
                     ### Make a model column
                     ~ .x %>% 
                       dplyr::summarise(
                         model = list(lm(log10(biomass_mg) ~ log10(size_mm), 
                                    data = dplyr::cur_data())),
                         .groups = "keep") %>% 
                       ### Filter out models with near-perfect fit
                       dplyr::filter(summary(model[[1]])$r.squared < 0.95 &
                                       ### and based on p value
                                       summary(model[[1]])$coefficients[2, 4] < 0.05))}
      ## If we want other models
      if(model == "brms"){
        ret <- 
          ret
      }
      
      
      
      ## Convert data to format we want
      ret <- 
        ret %>% 
        ## Flatten
        purrr::list_c() %>% 
        ## Ungroup
        dplyr::ungroup()
    }
      
  # Return
  return(ret)
}
