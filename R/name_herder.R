#' Assign species name if BWG name is missing
#' Gets or generates missing BWG names
#' 
#' Looks into measurement table, if finds a single matching taxonomy,
#' gives this name to the species, otherwise gives a new name. Please 
#' Make sure that each species has a unique taxonomy. If there are two 
#' unidentified species at the same level (e.g. two <i>Stibasoma</i>, then put 
#' any arbitrary string in the 'species' column to contrast between the two)
#'
#' @param dats The input data table
#' @return The input data table with missing BWG names filled
#' @export
name_herder <- function(dats, level_list){
  
  # Remove traits from level_list
  level_list <- 
    level_list[level_list %notin% "traits"]
  
  # First get species without bwg name
  data_noname <- 
    dats %>% 
    dplyr::filter(is.na(bwg_name)) %>% 
    dplyr::select(dplyr::all_of(level_list)) %>% 
    unique()

  # Count how many "species" do not have bwg_name
  data_noname_chr <-
    data_noname %>%
    ## Just make sure everything is a character
    dplyr::mutate(dplyr::across(dplyr::everything(), 
                                as.character)) %>%
    ## Count with row number
    dplyr::mutate(row_id = row_number())
  
  # Get all bwg names from the database
  bwgnames_df <- 
    get_bwgnames() %>%
    ## Filter the taxonomic level we want, and make sure they are characters
    dplyr::mutate(dplyr::across(level_list, 
                                as.character))
  
  # Join data with bwg names, and get unique combinations
  match_counts <- 
    bwgnames_df %>%
    ## Join
    dplyr::inner_join(data_noname_chr, 
                      by = level_list) %>%
    ## Group by row_id
    dplyr::group_by(row_id) %>%
    ## Count matches and keep first one if more than one
    dplyr::summarise(n = n(), 
                     bwg_name = first(bwg_name), 
                     .groups = "drop")
  
  # Merge match counts back to original data
  result <-
    data_noname_chr %>%
    ## Just keep row_id
    dplyr::select(row_id) %>%
    ## Left join with match counts
    dplyr::left_join(match_counts, 
                     by = "row_id") %>%
    ## Create new name based on match counts
    dplyr::mutate(new_name = dplyr::case_when(
      is.na(n) ~ paste0("new_", row_id),        # no match
      n == 1   ~ bwg_name,                      # one match
      n > 1    ~ paste0("new_", row_id)         # multiple matches
    ))
  
  # Add species names to data_noname
  data_noname <- 
    cbind(bwg_name2 = result$new_name,
          data_noname %>% 
            dplyr::select(-bwg_name))
  
  # Now bring back to dataframe
  dats <- 
    dats %>% 
    ## Merge the two together to align columns
    dplyr::left_join(data_noname, 
                     by = level_list[which(level_list != "bwg_name")]) %>% 

    ## Replace NAs in original bwg_name columns by the new names
    dplyr::mutate(bwg_name = dplyr::coalesce(bwg_name, bwg_name2)) %>% 
    ## Remove bwg_name2 column
    dplyr::select(-bwg_name2)
  
  # Return data 
  return(dats)
  
}




