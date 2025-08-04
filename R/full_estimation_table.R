#' Returns size and biomass estimations for every taxon in measurement table
#'
#'
#' @param level_list Vector of taxonomic levels over which to iterate estimations
#' @param measurement_table A table with the numerical measurements and biomass 
#' used to compute allometric lms 
#' @param what Should size (what = "size_mm") or biomass (what = "biomass_mg") be 
#' estimated?
#' @return Tibble with size estimations or allometric models used in estimations
#' @export
#' 
full_estimation_table <- function(level_list, measurement_table,
                                  what) {
    # Get all possible combinations of data
    dats <- 
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
          dplyr::mutate(level = .x)) 
    
    
  # If size we need the weighted average and size bins
    if(what == "size_mm"){
      dats <- 
        dats %>% 
        ## Each name value should have at least three measurements
        purrr::map(
          .,
          ~ .x %>%
            group_by(level, name, stage,) %>%
            filter(n() > 3) %>%
            ungroup()) %>% 
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
      dats <- 
        dats %>% 
        ## First remove all NAs in biomass
        purrr::map(
          .,
          ~ .x %>%
            dplyr::filter(!is.na(biomass_mg)) %>% 
            ## Group now by level, name and stage
            dplyr::group_by(level, name, stage) %>%
            ## Remove those with less than three measurements
            dplyr::filter(n() > 3) %>%
            ## Make a model column
            dplyr::summarise(
              model = list(lm(log10(biomass_mg) ~ log10(size_mm), 
                         data = dplyr::cur_data())),
              .groups = "keep") %>% 
            ## Filter out models with near-perfect fit
            dplyr::filter(summary(model[[1]])$r.squared < 0.95) %>% 
            ## Filter models based on p value
            dplyr::filter(summary(model[[1]])$coefficients[2, 4] < 0.05)) %>% 
        ## Flatten
        purrr::list_c() %>% 
        ## Ungroup
        dplyr::ungroup()
    }
      
  # Return
  return(dats)
}
