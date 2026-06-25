#' Returns size and biomass estimations for every taxon in measurement table
#'
#'
#' @param level_vec Vector of taxonomic levels over which to iterate estimations
#' @param measurement_table A table with the numerical measurements and biomass 
#' used to compute allometric lms 
#' @param what Should size (what = "size_col") or biomass (what = "biomass_col") be 
#' estimated? 
#' @param model What kind of model should be computed, so far only lm
#' @param traits Should the table be computed by traits or not? Default FALSE
#' @param trait_columns List of traits to match, should be column names in measurement_table
#' @param id_col Name of the column holding a unique identifier per species/taxon,
#' used only when `traits = TRUE`. Default "species".
#' @param r_square_cutoff_upper Upper cutoff for R2 in allometric models, models with values above it will not be used un estimations. Default is 0.95 to avoid overfit models
#' @param r_square_cutoff_lower Lower cutoff for R2 in allometric models, models with values below it will not be used un estimations. Default is 0.
#' @param p_val_cutoff Upper cutoff for p-value of allometric models, models with p_value above it will not be used in estimations. Default is 0.05.
#' @return Tibble with size estimations or allometric models used in estimations. 
#' @export
#' 
full_estimation_table <- function(level_vec, measurement_table,
                                  what, model = "lm", traits = FALSE,
                                  trait_columns = c(), id_col = "species",
                                  r_square_cutoff_upper = 0.95,
                                  r_square_cutoff_lower = 0,
                                  p_val_cutoff = 0.05){

  # If we want to get estimations with traits
  if (traits) {
    ## Use trait functions to make groupings of similar trait species
    ret <-
      make_trait_table(measurement_table = measurement_table,
                       trait_columns = trait_columns,
                       id_col = id_col)

  } else {
  # If we do not want traits
  # Get all possible combinations of data
    ret <-
      ## Split into list of tibbles, which each containing level and size_col
      purrr::map(
        level_vec[level_vec != "traits"],
        ~ measurement_table %>%
          ## Select only the level and size_col
          dplyr::select(name = dplyr::all_of(.x), 
                        stage, size_col, biomass_col) %>%
          ## Drop rows with a missing taxon name (NA or "") so the estimation
          ## falls back to the level above, and rows without a numeric size
          dplyr::filter(!is.na(name) & name != "" & !is.na(size_col)) %>%
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
            ## Filter out those with less than three unique size_col
            dplyr::filter(dplyr::n_distinct(size_col) >= 3) %>%
            ## Ungroup to avoid future issues
            dplyr::ungroup()) %>% 
      ## Remove empty levels 
      purrr::keep(function(x) nrow(x) > 0)}
    

  # If size we need the weighted average and size bins
    if(what == "size_col"){
      ret <- 
        ret %>% 
        purrr::map(.,
                   ~ .x %>%
                     ## Group by name
                     dplyr::group_by(level, name, stage) %>%
                     ## Make a column with terciles of sizes
                     dplyr::mutate(size_group = ntile(size_col, 3)) %>%
                     ## Group by size group and get mean of each
                     dplyr::group_by(level, name, stage, size_group) %>%
                     dplyr::summarise(
                       size_col = mean(size_col),
                       .groups = "drop") %>%
                     ## Create a new column with size category
                     dplyr::mutate(size_category = dplyr::case_when(
                       size_group == 1 ~ "small",
                       size_group == 2 ~ "medium",
                       size_group == 3 ~ "large")) %>%
                     ## Keep relevant columns
                     dplyr::select(level, name, stage, size_category, size_col) %>% 
                     ## Add overall mean
                     dplyr::bind_rows(
                       .x %>%
                         # Get average grouping by level and name
                         group_by(level, name, stage) %>%
                         summarise(size_col = mean(size_col), 
                                   .groups = "drop") %>%
                         ### Make size category column (to fit this average with unknown)
                         dplyr::mutate(size_category = "unknown"))) %>% 
        ## Flatten
        purrr::list_c()}
                   
                   
    # If biomass we need allometric equations               
    if(what == "biomass_col") {
      ## Prepare data in common for models
      ret <- 
        ret %>% 
        ### Since some organisms can have known size but unknown biomass
        ### We also need to filter here!
        purrr::map(.,
                   ~ .x %>%
                     ### First remove all NAs in biomass
                     dplyr::filter(!is.na(biomass_col)) %>% 
                     ### Group by level, name and stage
                     dplyr::group_by(level, name, stage) %>%
                     ### Filter out those with less than three unique size_col
                     dplyr::filter(dplyr::n_distinct(biomass_col) >= 3)) %>% 
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
                         model = list(lm(log10(biomass_col) ~ log10(size_col),
                                    data = dplyr::pick(size_col, biomass_col))),
                         .groups = "drop") %>%
                       ### Summarise each model once, then derive R2 and p-value
                       dplyr::mutate(
                         .smry = purrr::map(model, summary),
                         .r2   = purrr::map_dbl(.smry, "r.squared"),
                         .pval = purrr::map_dbl(.smry, ~ .x$coefficients[2, 4])) %>%
                       ### Filter out models based on R2 cutoffs and p value
                       dplyr::filter(.r2 < r_square_cutoff_upper,
                                     .r2 > r_square_cutoff_lower,
                                     .pval < p_val_cutoff) %>%
                       ### Drop helper columns
                       dplyr::select(-.smry, -.r2, -.pval))}
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
