#' Assign species name if BWG name is missing
#' Gets or generates mising BWG names
#' 
#' Looks into measurement table, if finds a single matching taxonomy,
#' gives this name to the species, otherwise gives a new name. Please 
#' make sure that each species has a unique taxonomy. If there are two 
#' unidentified species at the same level (e.g. two <i>Stibasoma</i>, then put 
#' any arbitrary string in the 'species' column to contrast between the two)
#'
#' @param data_table The input data table
#' @return The input data table with missing BWG names filled
#' @export
name_herder <- function(data_table){
  
  # First get species without bwg name
  data_noname <- 
    data_table %>% 
    dplyr::filter(is.na(bwg_name)) %>% 
    dplyr::select(domain:species) %>% 
    unique()
  
  # Record hwo many species were missing a BWG name
  n_noname <- 
    nrow(data_noname)
  
  # Make empty vector to store new species name 
  new_names <- 
    vector()
  
  # Loop to examine if species is already in BWG database
  for(i in 1:n_noname)
    c(## Get row of species as character,
      row <- 
        data_noname[i,] %>%
        dplyr::mutate(across(everything(), as.character)),
      ## Use inner join to see if species is present in database or not
      bwgnames <- 
        get_bwgnames() %>% 
        dplyr::inner_join(row,
                          by = c("domain", "kingdom", "phylum", "subphylum", "class", "subclass", 
                                 "ord", "subord", "family", "subfamily", "tribe", "genus", "species")),
      ## If there is no row, it means that there was no match
      ## So give a custom name and add it to vector of names
      if(nrow(bwgnames) == 0)
        new_names <- 
            rbind(new_names,
                  paste0("new_", i)),
      ## If there is one row, then there is a match!
      if(nrow(bwgnames) == 1)
        new_names <- 
            rbind(new_names,
                  bwgnames$bwg_name),
      ## If there is more than one match, also give a new name
      if(nrow(bwgnames) > 1)
        new_names <- 
        rbind(new_names,
              paste0("new_", i)))
  
  # Add species names to data_noname
  data_noname <- 
    cbind(new_names,
          data_noname) %>% 
    dplyr::rename(bwg_name = new_names)
  
  # Now bring back to dataframe
  data_table <- 
    data_table %>% 
    ## Merge the two together to align columns
    merge(data_noname, 
          by = c("domain", "kingdom", "phylum", "subphylum", "class", 
                 "subclass", "ord", "subord", "family", "subfamily", 
                 "tribe", "genus", "species"),
          all = T) %>% 
    ## Replace NAs in original bwg_name columns by the new names
    dplyr::mutate(bwg_name.x = ifelse(is.na(bwg_name.x),
                                      bwg_name.y, bwg_name.x)) %>% 
    ## Remove the column with computed names only
    dplyr::select(-bwg_name.y) %>% 
    ## Change back name to proper
    dplyr::rename(bwg_name = bwg_name.x) %>% 
    ## Put it back where I like it
    dplyr::relocate(bwg_name, .before = domain)
  
  # Return data 
  return(data_table)
  
}




