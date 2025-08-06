#' Wrapper function to estimate size (mm) and biomass (mg) from an input dataset
#'
#' Wrapper function, input your data and get size and measurement . 
#' Please name column with number of specimen "abundance", column with measurement 
#' in mm "size_mm", column with life stage (larva/pupa/adult) "stage", column 
#' with biomass type (dry/wet) "biomass_type", and the column with biomass in mg 
#' "biomass_mg".
#' If you do not have a numerical measurement for a given specimen, the function 
#' can do size estimations if you input "small", "medium", "large" or "unknown". 
#' In this case, the algorithm will use existing size measurements for the species, 
#' and use the size distribution to estimate "small", "medium" and "large" inputs, 
#' or a weighted average of all measurements if the input is "unknown". For biomass 
#' measurements, just leave NA for those cells you want estimations in.
#' If you are using BWG data, please make sure that the column with BWG names is 
#' called "bwg_name".
#' 
#' Please make sure that the level_list parameter has levels in increasing order 
#' of resolution, e.g. from species to order.
#' 
#'
#' @param dats The input data table, please include columns columns "abundance", 
#' "size_mm", "biomass_mg", "stage" (larva/pupa/adult, please only put 'adult' for 
#' adult insects)
#' @param level_list A character vector indicating the taxonomic levels present 
#' in your data, in indecreasing order of taxonomic resolution.
#' @param biomass_type "dry"/"wet". Should data used in inference be "dry" for 
#' just dry biomass (default), or "wet" for just wet biomass. See `dry_wet()`
#' for more information on how this works.
#' @param database Logical. Should data from the BWG database be used to supplement 
#' a set of BWG-specific measurements, not present in the database 
#' (TRUE(default)/FALSE). This function uses numerical measurements both from the 
#' BWG database and from the data you provide. Only put FALSE if you are doing 
#' estimations from data already present in the BWG database (estimation for 
#' these data available with `database_data(TRUE)`).
#' @param nothing Logical. If TRUE, no BWG-specific data will be used for estimations, 
#' only the data you provide. This is useful if you have your own measurement 
#' database or work on a different system (default FALSE).
#' 
#' @return A list with three dataframes:
#' - data: the input data with added size and biomass estimations, and new columns 
#'         with the taxonomic level and name of the taxon at which the estimation 
#'         was made.
#' - size_estimations: dataframe with size estimations that were used, 
#'                     with columns for taxonomic level, name and size category
#'                     of the estimation.
#' - model_estimations: dataframe with biomass models that were used, with columns
#'                      for taxonomic level and name.
#' See `full_estimation_table()` to get all possible size estimations and models
#' for your data
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' @export
hellometry <- function(dats, level_list, biomass_type = "dry", database = TRUE, nothing = FALSE) {
  
  # Get new species names, with catch if no bwg_name in level_list
  if("bwg_name" %in% level_list & any(is.na(dats$bwg_name))) {
    dats <- 
      name_herder(dats = dats, 
                  level_list = level_list) }
  
  # Columns, biomass_type must have specific values, database and nothing must be logical
  data_checker(dats = dats, 
               biomass_type = biomass_type,
               database = database, 
               nothing = nothing)
  
  # Make copy of dataset to then reuse
  ret <- 
    dats %>% 
    ## Add row numbers, to join back to original data at the end
    dplyr::mutate(row = row_number())
  
  # Limoniidae and Tipulidae essentially the same thing so can be considered together
  # If family exists in column names
  if("family" %in% colnames(ret)){
    ret <- 
      tipulimo(ret)}  
  
  # Make measurement table
  ## Print message
  print("Making measurement table...")
  ## Make table
  measurement_table <- 
    make_measurement_table(dats = dats,
                           level_list = level_list,
                           database = database,
                           nothing = nothing)

  # Getting size estimations
  ## Print message
  print("Getting size estimations...")
  ## Make table with all estimations
  full_estimation_table_size <- 
    full_estimation_table(level_list = level_list, 
                          measurement_table = measurement_table,
                          what = "size_mm") 
  
  # Join size estimations to the original data
  size_estimations <- 
    ret %>% 
    ## Filter numerical measurements
    dplyr::filter(size_mm %in% c("small", "medium", "large", "unknown")) %>% 
    ## Select columns we need
    ## Only keep columns we want
    dplyr::select(row, rev(dplyr::any_of(level_list)), 
                  stage, size_mm) %>% 
    ## Rename size category to join to estimation result
    dplyr::rename(size_category = size_mm) %>% 
    ## Loop around taxonomic levels and add to main data
    purrr::reduce(
      ### Levels to loop around
      level_list[level_list != "traits"],
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
                                         !!paste0(.y, "_size_mm") := size_mm, 
                                         stage, size_category),
                         ### Join by column name, stage and size category to get a new column for that trophic level
                         by = c(.y, "stage", "size_category"))) %>%
    ## Now that all estimations at all levels have been joined, pick the one at the smallest taxonomic level
    dplyr::mutate(
      ### Coalesce all size_mm columns to get the first non-NA value
      ### Follows the order of the given vector!
      size_mm = dplyr::coalesce(!!!syms(paste0(level_list[level_list != "traits"], 
                                               "_size_mm"))),
      ### Make a new column that indicates the taxonomic level at which the estimation was kept
      ### case_when chooses the first TRUE condition and assigns its corresponding value (the taxonomic level name)
      size_level = dplyr::case_when(!!!purrr::imap(paste0(level_list[level_list != "traits"], 
                                                          "_size_mm"), 
                                                   ### Check which one of the values comes first as non-NA
                                                   ~ expr(!is.na(!!sym(.x)) ~ ### !!sym() because we are pasting character string that needs to be interpreted as column name
                                                            !!level_list[level_list != "traits"][.y]))),
      ### Finally, create a new column that contains the taxon name at the level of estimation
      size_taxon_name = dplyr::case_when(
        !!!imap(level_list[level_list != "traits"], ~ 
                  expr(size_level == !!.x ~ !!sym(.x))))) %>%
    ## Only keep columns we want
    dplyr::select(row, stage,
                  size_mm, size_category,
                  size_level, size_taxon_name) 
  
  # Getting biomass models
  ## Print message
  print("Getting biomass models...")
  ## Make table
  ### Some models will be bad but filtered out,
  ### Wrap in suppressWarnings to avoid cluttering
  suppressWarnings(
    full_estimation_table_biomass <- 
      full_estimation_table(level_list = level_list, 
                            measurement_table = dry_wet(measurement_table,
                                                        biomass_type = "dry"),
                            what = "biomass_mg"))
  
  # Join models to the original data
  model_estimations <- 
    ret %>% 
    ## Get NA biomasses
    dplyr::filter(is.na(biomass_mg)) %>% 
    ## Select columns we need
    dplyr::select(row, dplyr::any_of(level_list), 
                  stage, biomass_mg, biomass_type) %>%
    ## Loop around taxonomic levels and add to main data
    purrr::reduce(
      ### Levels to loop around
      level_list[level_list != "traits"],
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
                                        dplyr::all_of(paste0(level_list[level_list != "traits"], 
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
                                                  dplyr::all_of(paste0(level_list[level_list != "traits"], 
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
                                        level_list[level_list != "traits"][[level_idx[1]]] else NA_character_}),
      ## Get the corresponding taxon name from which the model came
      model_taxon_name = dplyr::case_when(
        !!!imap(level_list[level_list != "traits"], 
                ~ expr(model_level == !!.x ~ !!sym(.x))))) %>% 
    ## Only keep columns we want
    dplyr::select(row, stage,
                  model,  model_level, 
                  model_taxon_name) 
  
  # Stick back to original data
  ## Print message
  print("Combining estimations and models with data...")
  ## Stick back to the data
  ### Suppress warnings when mutated to numeric
  suppressWarnings(
    ret <- 
      ret %>% 
      ## Join size estimations by row number 
      dplyr::left_join(size_estimations %>% 
                         dplyr::select(-stage),
                       by = "row") %>% 
      ## Combine by new and old sizes
      dplyr::mutate(size_mm.x = as.numeric(size_mm.x),
                    size_mm = dplyr::coalesce(size_mm.x, size_mm.y)) %>%
      ## Join models by row number
      dplyr::select(-size_mm.x, -size_mm.y) %>% 
      dplyr::left_join(model_estimations %>% 
                         dplyr::select(-stage),
                       by = "row") %>%
      ## Now get estimations of biomass for a single individual
      dplyr::mutate(
        ### Iterate over the list column
        biomass = purrr::map2_dbl(
          model, size_mm, ~ {
            ### If the cell is NULL (biomass was inputted),make NA
            if (is.null(.x)) {
              NA_real_}
            ### If the cell is NA (no model could be computed), make NA
            else if (length(.x) == 0 || any(is.na(.x))) {
              NA_real_}
            else {
              ### if we have all the data we need, use predict to get estimation
              predict(.x, 
                      newdata = data.frame(size_mm = .y))}})) %>% 
      ## Convert back to normal unit (model uses log10) and multiply by abundance
      dplyr::mutate(biomass = (10^biomass)*abundance) %>% 
      ## Coalesce the two biomass columns
      dplyr::mutate(biomass_mg = dplyr::coalesce(biomass, biomass_mg)) %>%
      ## Remove biomass column
      dplyr::select(-biomass) %>%
      ## If we have a value in size_mm but no size estimation, make it "raw_data"
      dplyr::mutate(dplyr::across(c(size_category, size_level, size_taxon_name),
                                  ~ ifelse(is.na(.),
                                           "raw_data", .))) %>% 
      ## If we have no biomass estimation say that estimation failed
      dplyr::mutate(dplyr::across(c(model_level, model_taxon_name),
                                  ~ ifelse(is.na(biomass_mg),
                                           "estimation_failed", .))) %>% 
      ## Finally if we have NAs left in model_level and model_taxon_name, make them "raw_data"
      dplyr::mutate(dplyr::across(c(model_level, model_taxon_name),
                                  ~ ifelse(is.na(.),
                                           "raw_data", .))) %>% 
      ## I am a bit of a maniac so have size_mm before biomass_mg
      dplyr::relocate(size_mm, 
                      .before = "biomass_mg"))
  
  
  # Return a list of three dataframes
  ## The original data with estimations, the used size estimations, and the used models
  return(list(
    ## Original data with estimations
    data = 
      ret %>% 
      ### Remove row number
      dplyr::select(-row),
    ## Size estimations
    size_estimations = 
      size_estimations %>% 
      ### Remove NAs
      dplyr::filter(!is.na(size_mm)) %>%
      ### Make longer format 
      dplyr::select(level = size_level, 
                    name = size_taxon_name, 
                    stage, size_category, size_mm) %>% 
      ### Keep unique rows
      unique(),
    ## Model estimations
    model_estimations = 
      model_estimations %>% 
      ### Remove NAs
      dplyr::filter(!is.na(model)) %>%
      ### Make longer format 
      dplyr::select(level = model_level, 
                    name = model_taxon_name, 
                    stage, model) %>% 
      ### Keep unique rows
      unique()))
  
}
