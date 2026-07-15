#' Wrapper function to estimate size (mm) and biomass (mg) from an input dataset
#'
#' Wrapper function, input your data and get size and measurement. 
#' Please name column with number of specimen "abundance", column with measurement 
#' in mm "size_col", column with life stage (e.g. larva/adult) "stage", column 
#' with biomass type (dry/wet) "biomass_type", and the column with biomass in mg 
#' "biomass_col".
#' If you do not have a numerical measurement for a given specimen, the function
#' can do size estimations if you input "small", "medium", "large" or "unknown".
#' In this case, the algorithm will use existing size measurements for the species,
#' and use the size distribution to estimate "small", "medium" and "large" inputs,
#' or a weighted average of all measurements if the input is "unknown". For biomass
#' measurements, just leave NA for those cells you want estimates in.
#'
#' Please make sure that the level_vec argument has levels in increasing order
#' of resolution, e.g. from species to order.
#'
#'
#' @param dats The input data table, please include columns "abundance",
#' "size_col", "biomass_col", "stage" (larva/pupa/adult).
#' @param level_vec A character vector indicating the taxonomic levels present
#' in your data, in increasing order of taxonomic resolution.
#' @param biomass_type "dry"/"wet". Should data used in inference be "dry" for
#' just dry biomass (default), or "wet" for just wet biomass. See `dry_wet()`
#' for more information on how this works.
#' @param r_square_cutoff_upper Upper cutoff for R2 in allometric models, models with values above it will not be used in estimation. Default is 0.95 to avoid overfit models
#' @param r_square_cutoff_lower Lower cutoff for R2 in allometric models, models with values below it will not be used in estimation. Default is 0.
#' @param p_val_cutoff Upper cutoff for p-value of allometric models, models with p_value above it will not be used in estimation. Default is 0.05.
#' @return A list with three tibbles:
#' - data: the input data with added size and biomass estimates, and new columns 
#'         with the taxonomic level and name of the taxon at which the estimate 
#'         was made.
#' - size_estimates: tibble with size estimates that were used, 
#'                     with columns for taxonomic level, name and size category
#'                     of the estimation.
#' - model_estimates: tibble with biomass models that were used, with columns
#'                      for taxonomic level and name.
#' See `full_estimation_table()` to get all possible size estimates and models
#' for your data
#' @export
hellometry <- function(dats,
                       level_vec,
                       biomass_type = "dry",
                       r_square_cutoff_upper = 0.95,
                       r_square_cutoff_lower = 0,
                       p_val_cutoff = 0.05) {

  # Columns must be properly named, biomass_type must have a valid value
  data_checker(dats = dats,
               biomass_type = biomass_type)

  # Make copy of dataset to then reuse
  ret <-
    dats %>%
    ## Add row numbers, to join back to original data at the end
    dplyr::mutate(row = row_number())

  # Make measurement table
  ## Print message
  print("Making measurement table...")
  ## Make table
  measurement_table <-
    make_measurement_table(dats = dats,
                           level_vec = level_vec)

  # Getting size estimates
  ## Print message
  print("Getting size estimates...")
  ## Make table with all estimates
  full_estimation_table_size <- 
    full_estimation_table(level_vec = level_vec, 
                          measurement_table = measurement_table,
                          what = "size_col",
                          r_square_cutoff_upper = r_square_cutoff_upper,
                          r_square_cutoff_lower = r_square_cutoff_lower,
                          p_val_cutoff = p_val_cutoff) 
  
  # Join size estimates to the original data
  size_estimates <- 
    ret %>% 
    ## Filter numerical measurements
    dplyr::filter(size_col %in% c("small", "medium", "large", "unknown")) %>% 
    ## Select columns we need
    ## Only keep columns we want
    dplyr::select(row, rev(dplyr::any_of(level_vec)), 
                  stage, size_col) %>% 
    ## Rename size category to join to estimation result
    dplyr::rename(size_category = size_col) %>% 
    ## Loop around taxonomic levels and add to main data
    purrr::reduce(
      ### Levels to loop around
      level_vec[level_vec != "traits"],
      ### Data to be used as base (non_numeric_taxa)
      .init = .,
      ### Join data based on series of filter
      .f = ~ .x %>%
        dplyr::left_join(full_estimation_table_size %>%
                           ### Filter rows based on current trophic level
                           dplyr::filter(level == .y) %>%
                           ### Rename taxon_name to match the corresponding level column (e.g., "genus")
                           ### Rename size to a level-specific name (e.g., "genus_size") to avoid overwriting
                           dplyr::select(!!sym(.y) := name, ### !!sym() because we are pasting character string that needs to be interpreted as column name 
                                         !!paste0(.y, "_size_col") := size_col, 
                                         stage, size_category),
                         ### Join by column name, stage and size category to get a new column for that trophic level
                         by = c(.y, "stage", "size_category"))) %>%
    ## Now that all estimates at all levels have been joined, pick the one at the smallest taxonomic level
    dplyr::mutate(
      ### Coalesce all size_col columns to get the first non-NA value
      ### Follows the order of the given vector!
      size_col = dplyr::coalesce(!!!syms(paste0(level_vec[level_vec != "traits"], 
                                               "_size_col"))),
      ### Make a new column that indicates the taxonomic level at which the estimation was kept
      ### case_when chooses the first TRUE condition and assigns its corresponding value (the taxonomic level name)
      size_level = dplyr::case_when(!!!purrr::imap(paste0(level_vec[level_vec != "traits"], 
                                                          "_size_col"), 
                                                   ### Check which one of the values comes first as non-NA
                                                   ~ expr(!is.na(!!sym(.x)) ~ ### !!sym() because we are pasting character string that needs to be interpreted as column name
                                                            !!level_vec[level_vec != "traits"][.y]))),
      ### Finally, create a new column that contains the taxon name at the level of estimation
      size_taxon_name = dplyr::case_when(
        !!!imap(level_vec[level_vec != "traits"], ~ 
                  expr(size_level == !!.x ~ !!sym(.x))))) %>%
    ## Only keep columns we want
    dplyr::select(row, stage,
                  size_col, size_category,
                  size_level, size_taxon_name) 
  
  # Getting biomass models
  ## Print message
  print("Getting biomass models...")
  ## Make table
  ### Some models will be bad but filtered out,
  ### Wrap in suppressWarnings to avoid cluttering
  suppressWarnings(
    full_estimation_table_biomass <- 
      full_estimation_table(level_vec = level_vec, 
                            measurement_table = dry_wet(measurement_table,
                                                        biomass_type = "dry"),
                            what = "biomass_col",
                            r_square_cutoff_upper = r_square_cutoff_upper,
                            r_square_cutoff_lower = r_square_cutoff_lower,
                            p_val_cutoff = p_val_cutoff))
  
  # Join models to the original data
  model_estimates <- 
    ret %>% 
    ## Get NA biomasses
    dplyr::filter(is.na(biomass_col)) %>% 
    ## Select columns we need
    dplyr::select(row, dplyr::any_of(level_vec), 
                  stage, biomass_col, biomass_type) %>%
    ## Loop around taxonomic levels and add to main data
    purrr::reduce(
      ### Levels to loop around
      level_vec[level_vec != "traits"],
      ### Data to be used as base (non_numeric_taxa)
      .init = .,
      ### Join data based on series of filter
      .f = ~ .x %>%
        dplyr::left_join(full_estimation_table_biomass %>%
                           ### Filter rows based on current trophic level
                           dplyr::filter(level == .y) %>%
                           ### Rename taxon_name to match the corresponding level column (e.g., "genus")
                           ### Rename size to a level-specific name (e.g., "genus_size") to avoid overwriting
                           dplyr::select(!!sym(.y) := name, ### !!sym() because we are pasting character string that needs to be interpreted as column name 
                                         stage, !!paste0(.y, "_model") := model),
                         ### Join by column name, stage and size category to get a new column for that trophic level
                         by = c(.y, "stage"))) %>%
    ## Now that all models at all levels have been joined, pick the one at the smallest taxonomic level
    ## Things are a bit trickier because the column consists of a list of models, not numerical values
    dplyr::mutate(
      ## Add a new column that contains the first non-NA model
      model = purrr::pmap(dplyr::select(., 
                                        dplyr::all_of(paste0(level_vec[level_vec != "traits"], 
                                                             "_model"))), 
                          function(...) {
                            ### Convert input to list
                            models <- 
                              list(...)
                            ### Find the first non-null model
                            first_non_null <- 
                              purrr::keep(models, 
                                          ~ !is.null(.x))
                            ### Return the first non-null model or NA if none found
                            if (length(first_non_null) > 0) 
                              first_non_null[[1]] else NA}),
      ## Get the corresponding level for that model
      model_level = purrr::pmap_chr(dplyr::select(., 
                                                  dplyr::all_of(paste0(level_vec[level_vec != "traits"], 
                                                                       "_model"))), 
                                    function(...) {
                                      ### Convert input to list
                                      models <- 
                                        list(...)
                                      ### Get the index of the first non-NULL model
                                      level_idx <- 
                                        which(purrr::map_lgl(models, 
                                                             ~ !is.null(.x)))
                                      ### Return the corresponding level or NA if none found
                                      if (length(level_idx) > 0) 
                                        level_vec[level_vec != "traits"][[level_idx[1]]] else NA_character_}),
      ## Get the corresponding taxon name from which the model came
      model_taxon_name = dplyr::case_when(
        !!!imap(level_vec[level_vec != "traits"], 
                ~ expr(model_level == !!.x ~ !!sym(.x))))) %>% 
    ## Only keep columns we want
    dplyr::select(row, stage,
                  model,  model_level, 
                  model_taxon_name) 
  
  # Stick back to original data
  ## Print message
  print("Combining estimates and models with data...")
  ## Stick back to the data
  ### Suppress warnings when mutated to numeric
  suppressWarnings(
    ret <- 
      ret %>% 
      ## Join size estimates by row number 
      dplyr::left_join(size_estimates %>% 
                         dplyr::select(-stage),
                       by = "row") %>% 
      ## Combine by new and old sizes
      dplyr::mutate(size_col.x = as.numeric(size_col.x),
                    size_col = dplyr::coalesce(size_col.x, size_col.y)) %>%
      ## Join models by row number
      dplyr::select(-size_col.x, -size_col.y) %>% 
      dplyr::left_join(model_estimates %>% 
                         dplyr::select(-stage),
                       by = "row") %>%
      ## Now get estimates of biomass for a single individual
      dplyr::mutate(
        ### Iterate over the list column
        biomass = purrr::map2(
          model, size_col, ~ {
            ### If the cell is NULL (biomass was inputted),make NA
            if (is.null(.x)) {
              NA_real_}
            ### If the cell is NA (no model could be computed), make NA
            else if (length(.x) == 0 || any(is.na(.x))) {
              NA_real_}
            else {
              ### if we have all the data we need, use predict to get estimate
              predict(.x, 
                      newdata = data.frame(size_col = .y),
                      interval = "prediction")}})) %>% 
      ## Do rowwise operations
      dplyr::rowwise() %>% 
      ## Convert to normal units
      dplyr::mutate(biomass_lower = 10^biomass[2],
                    biomass_upper = 10^biomass[3],
                    biomass = 10^biomass[1]*abundance) %>% 
      ## Get new prediction error interval related to abundace
      ## First get individual SE, EM = 1.96 * SEindividual
      ## Multiply by squareroot of abundance to get SEtotal
      ## Multiply by 1.96 to get back to total EM  dplyr::mutate(prediction_interval = (((biomass_upper - biomass_lower)/2)/1.96)*sqrt(abundance)*1.96) %>% 
      dplyr::mutate(prediction_interval = (((biomass_upper - biomass_lower)/2)/1.96)*sqrt(abundance)*1.96) %>% 
      dplyr::mutate(biomass_lower = biomass - prediction_interval,
                    biomass_upper = biomass + prediction_interval) %>% 
      ## Remove predicioon interval
      dplyr::select(-prediction_interval) %>% 
      ## Ungroup
      dplyr::ungroup() %>% 
      ## Coalesce the two biomass columns
      dplyr::mutate(biomass_col = dplyr::coalesce(biomass, biomass_col)) %>%
      ## Remove biomass column
      dplyr::select(-biomass) %>%
      ## If we have a value in size_col but no size estimate, make it "raw_data"
      dplyr::mutate(dplyr::across(c(size_category, size_level, size_taxon_name),
                                  ~ ifelse(is.na(.),
                                           "raw_data", .))) %>% 
      ## If we have no biomass estimate say that estimation failed
      dplyr::mutate(dplyr::across(c(model_level, model_taxon_name),
                                  ~ ifelse(is.na(biomass_col),
                                           "estimation_failed", .))) %>% 
      ## Finally if we have NAs left in model_level and model_taxon_name, make them "raw_data"
      dplyr::mutate(dplyr::across(c(model_level, model_taxon_name),
                                  ~ ifelse(is.na(.),
                                           "raw_data", .))) %>% 
      ## I am a bit of a maniac so have size_col before biomass_col, and intevals after
      dplyr::relocate(size_col, 
                      .before = "biomass_col") %>% 
    dplyr::relocate(biomass_lower, biomass_upper, 
                    .after = "biomass_col"))
  
  
  
  # Return a list of three dataframes
  ## The original data with estimates, the used size estimates, and the used models
  return(list(
    ## Original data with estimates
    data = 
      ret %>% 
      ### Remove row number
      dplyr::select(-row),
    ## Size estimation
    size_estimates = 
      size_estimates %>% 
      ### Remove NAs
      dplyr::filter(!is.na(size_col)) %>%
      ### Make longer format 
      dplyr::select(level = size_level, 
                    name = size_taxon_name, 
                    stage, size_category, size_col) %>% 
      ### Keep unique rows
      unique(),
    ## Model estimation
    model_estimates = 
      model_estimates %>% 
      ### Remove NAs
      dplyr::filter(!is.na(model)) %>%
      ### Make longer format 
      dplyr::select(level = model_level, 
                    name = model_taxon_name, 
                    stage, model) %>% 
      ### Keep unique rows
      unique()))
  
}
