#' Wrapper function going row-by-row to estimate size (mm) and biomass (mg)
#'
#' Wrapper function, returns initial data table with three extra columns: total 
#' biomass from given species, size and abundance, as well as path taken through 
#' the function
#'
#' @param equation_table, measurement_table, data_table, print, biomass_kind
#' @return The original data frame, 
#' @export
hello_metry <- function(equation_table, measurement_table, data_table, print, biomass_kind){
  # browser()
  # Function %notin%
  '%notin%' <- 
    Negate('%in%')
  
    # Make copy of data table
  data_return <- 
    data_table %>% 
    ## And add new columns to fill
    mutate(size_used = NA,
           biomass = NA,
           path = NA)
  
  # Some error catching 
  ## Important columns have proper names
  if("abundance" %notin% colnames(data_table))
    stop("Please call column with abundance values 'abundance'")
  if("bwg_name" %notin% colnames(data_table))
    stop("Please call column with bwg species names values 'bwg_name'")
  if("size_mm" %notin% colnames(data_table))
    stop("Please call column with specimen measurement values 'size_mm'")
  ## Print is actually a true false
  if(print %notin% c(TRUE, FALSE))
    stop("Print has to be TRUE/FALSE")
  ## Biomass kind needs to be dry or both
  if(biomass_kind %notin% c("dry", "both"))
    stop("Biomass kind has to be 'dry' or 'both'")
  
  # Make list of taxonomic groups/traits to gro through
  # Note that after family we look at traits, and then back to higher trophic levels (the broad ones)
  level_list <- 
    c("bwg_name",
      "genus",
      "subfamily",
      "family",
      "traits",
      "subord",
      "ord",
      "subclass",
      "class",
      "phylum")
  
  ## Limoniidae and Tipulidae essentially the same thing so can be considered together
  measurement_table <- 
    measurement_table %>% 
    mutate(family = ifelse(family %in% c("Tipulidae", "Limoniidae"),
                           "Tipulidae_Limoniidae", family))
  equation_table <- 
    equation_table %>% 
    mutate(family = ifelse(family %in% c("Tipulidae", "Limoniidae"),
                           "Tipulidae_Limoniidae", family))
  
 # Loop to fill row by row
  for(i in 1:nrow(data_return)){
    ## If people want to print row names to track progress
    if(print == TRUE)
      print(i)
    
    ## Initialise function parameters
    row <- data_return[i,]
    specname <- row$bwg_name
    size_mm <- row$size_mm
    abundance <- row$abundance
    stage <- row$stage
    
    ## Initialise path to record what happens in this function
    path <- ""
    
    ## Some error catching
    ### Make sure abundance is not NA
    if(is.na(abundance))
      stop(paste0("Abundance row ", i," is NA"))
    
   ## Let's not waste time if abundance is zero
    if(abundance == 0) 
      c(biomass <- 0, 
       path <- "null_biomass") else
          biomass <- 
            NA
  
   ## Check if species is in the database
   if(abundance > 0)
   ### Make small frame to get taxonomy
     c(taxo <- 
        measurement_table %>% 
        filter(bwg_name == specname) %>% 
        dplyr::select(species_id:species) %>% 
        unique(),
     ### If not in database, give special biomass value
      if(nrow(taxo) == 0)
        c(biomass <-
        "notindatabase",
        path <- paste0(path, "not_in_database")),
      ### If in database 
      if(nrow(taxo) > 0)
          #### Check if we have a numeric size
          if(!is.na(as.numeric(size_mm))) 
            size_mm <- as.numeric(size_mm) else
              #### If we don't, do a size estimation
              #### As long as size is not numeric
              while(!is.numeric(size_mm)){
                ##### Go through my list of group
                for(level in level_list){
                  est <- sizest(specname, size_mm, level, stage, path, taxo, equation_table, measurement_table, data_table)
                  size_mm <- est[,1]
                  path <- est[,2]
                  #### Break loop if done
                  if(is.numeric(size_mm))
                    break
                  ###### If we got to the end and still nothing, give up
                  if(!is.numeric(size_mm) & level == "phylum")
                    c(path <- 
                        paste0(path, "size_estimation_failed"),
                      biomass <- 
                        "cannot_estimate",
                      size_mm <- 
                        666)
                }})
          
    ### Call the allometric equations
    #### As long as biomass is NA
    while(is.na(biomass)){
      ##### Go through my list of group
      for(level in level_list){
        est <- get_biomass(specname, level, size_mm, abundance, stage, path, taxo, equation_table, measurement_table, biomass_kind)
        biomass <- est[,1]
        path <- est[,2]
        #### Break loop if done
        if(!is.na(biomass))
          break
        ###### If we got to the end and still nothing, give up
        if(is.na(biomass) & level == "phylum")
          c(biomass <-
              "cannot_estimate",
            path <- 
              paste0(path, "_no_equations"))
      }}
  
    ### Add biomass and path value to data frame to return
    data_return[i,(ncol(data_return)-2)] <- size_mm
    data_return[i,(ncol(data_return)-1)] <- biomass
    data_return[i,ncol(data_return)] <- as.character(path)
    
  } 
  ## Return new data frame
  return(data_return %>% 
           rename(biomass_mg = biomass,
                  size_original = size_mm) %>% 
           left_join(measurement_table %>% 
                       dplyr::select(bwg_name:species) %>% 
                       unique()) %>% 
           relocate(size_original:path, .after = species) %>% 
           ### to make things easy only put NA where size was not computed
           mutate(size_used = ifelse(abundance == 0, NA, size_used)))
        
}