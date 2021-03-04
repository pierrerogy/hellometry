#' Returns size estimates based on taxonomic approach or trait approach
#'
#' Collect abundance/size distribution from species in both measurement and data tables. Look for #' at least three distinct numeric measurements in a given grouping in the following order: species #' to family, traits, suborder to phylum. Once threshold of at least three distinct numeric 
#' measurements is met: if size category “unknown”, return weighted average of all available 
#' measurements, if size category one of S, M, L, divide measurements into three bins of unique 
#' measurement values, if bins not equal, larger bins for S and L. Return average of     
#' measurements within given bin
#'
#' @param specname, size_mm, level, stage, path, taxo, equation_table, measurement_table,  
#' data_table
#' @return extra column with size estimate
#' @export
sizest <- function(specname, size_mm, level, stage, path, taxo, equation_table, measurement_table, data_table){

  ## Decide where to go depending on value of size
  if(size_mm == "unknown")
    category <-  "weighted_average" else
      if(size_mm %in% c("small", "medium", "large"))
        category <-  "size_cat" else
          category <-  "naught"
  
  ## Initiate new_size
  new_size <- 
    NA
  
  ### If using trait, get custom list of species with matcher_of_traits()
  if(level == "traits")
    c(spec_list <- 
        matcher_of_traits(specname, measurement_table),
      other_meas <- 
        data_table %>% 
        filter(bwg_name %in% spec_list)  %>% 
        #### Select relevant columns
        dplyr::select(size_mm, abundance) %>% 
        #### Add measurements from allometry table
        bind_rows(measurement_table %>% 
                    filter(bwg_name %in% spec_list) %>%
                    dplyr::select(length_mm, abundance) %>% 
                    rename(size_mm = length_mm) %>% 
                    mutate(size_mm = as.character(size_mm))) %>% 
        #### Group by size and sum
        group_by(size_mm) %>% 
        summarise_all(sum)  %>% 
        mutate(size_mm = as.numeric(size_mm)) %>% 
        filter(!is.na(size_mm)) %>% 
        filter(!is.na(abundance)))
  
  ### If using taxonomy, filter taxonomy of species of interest 
  if(level != "traits")
    #### Create blank dataframe if no trophic level (if NA)
    if(do.call(paste, list(taxo[,level])) == "NA")
      other_meas <- data.frame() else
        #### First extract actual name of trophic level
        c(level_name <- 
            taxo %>%
            ## using all_of(), see <https://tidyselect.r-lib.org/reference/faq-external-vector.html>
            dplyr::select(all_of(level)) %>% 
            unique() %>% 
            pull(), #### pull transforms it from a column to a vector
          ### Get a vector of all species from that specific level
          spec_list <- 
            measurement_table %>% 
            filter(measurement_table[,level] == level_name) %>% 
            dplyr::select(bwg_name) %>% 
            unique() %>% 
            pull(),
          other_meas <- 
            data_table %>% 
            filter(bwg_name %in% spec_list)  %>% 
            #### Select relevant columns
            dplyr::select(size_mm, abundance) %>% 
            #### Add measurements from allometry table
            bind_rows(measurement_table %>% 
                        filter(bwg_name %in% spec_list) %>%
                        dplyr::select(length_mm, abundance) %>% 
                        rename(size_mm = length_mm) %>% 
                        mutate(size_mm = as.character(size_mm))) %>% 
            #### Group by size and sum
            group_by(size_mm) %>% 
            summarise_all(sum) %>% 
            mutate(size_mm = as.numeric(size_mm)) %>% 
            filter(!is.na(size_mm)) %>% 
            filter(!is.na(abundance)))
    
  ## Get number of other numerical measurements
  meas_number <- 
    nrow(other_meas)
  ## And make it a condition, cut-off set at three other numerical measurements
  if(meas_number >= 3) 
    others <- TRUE else
      others <- FALSE
  ## If we have more than three numerical measurements at a given level
  ## Either compute weighted average or size categories
  if(others)
    c(if(category == "weighted_average")
    ### Compute weighted average
    c(new_size <- 
        weighted.mean(other_meas$size_mm, other_meas$abundance),
      path <- 
        paste0(path, "WA:", level,"_", meas_number)),
    ### Compute size categories
    if(category == "size_cat")
      c(size_vec <- 
          other_meas$size_mm,
        #### Sample elements to get 3 vectors of regular sizes
        temp_list <- 
          size_vec %>% 
          split(1:3),
        #### Make empty vector
        vec_size <- 
          c(),
        #### Get length of each sample vector
        for(i in 1:3){
          vec_size <- 
            c(vec_size, length(temp_list[[i]]))
        },
        #### Now that I have the size of the bins, we can compute the sizes!
        #### The each new_size will be the mean for all values in that bin
        #### S and L bins smaller than M
        if(size_mm == "small")
          new_size <- 
            mean(size_vec[1:min(vec_size)]) else
              if(size_mm == "large")
                new_size <- 
                  mean(size_vec[(length(size_vec) - min(vec_size) + 1):length(size_vec)]) else
                    if(size_mm == "medium")
                      new_size <- 
                        mean(size_vec[(1 + min(vec_size)):length(size_vec) - min(vec_size) + 1]),
        path <- paste0(path, "BIN:", level, "_", meas_number)))
  
  ## If fails, just return initial value        
  if(!others)
    new_size <- 
      size_mm

  ## If there was an expected value return special path
  if(category == "naught")
    c(new_size  <- 
        "size_cat_unknown",
      path <- 
        paste0(path, "size_cat_unknown"))
  
  ## Return data
  return(data.frame(new_size, as.character(path)))
}

