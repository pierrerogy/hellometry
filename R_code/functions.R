# Functions
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Function %notin%
'%notin%' <- 
  Negate('%in%')

# Estimate biomass --------------------------------------------------------
get_allometric_equations <- function(specname, level, size_mm, abundance, path, taxo, equation_table, measurement_table){

  # Check if species is length_raw, and if size is present
  raw_meas <- 
    measurement_table %>% 
    filter(provenance == "length.raw") %>% 
    filter(bwg_name == specname,
           length_mm == size_mm)
  ## If both are present, then we do not need to compute
  ## or at least just take the mean of the directly measured biomasses
  if(nrow(raw_meas) > 0)
    c(compute <- FALSE,
      biomass <- mean(raw_meas$biomass_mg),
      path <- paste0(path, "_raw_", unique(raw_meas$biomass_type)),
      return(data.frame(biomass, as.character(path)))) else
      compute <- TRUE
    
  # Get allometry information of species 
  ## If using taxonomy, filter taxonomy of species of interest 
  if(level != "traits")
    ### Create blank dataframe if no trophic level (if NA)
    if(do.call(paste, list(taxo[,level])) == "NA")
      allometry <- data.frame() else
         allometry <-
            equation_table %>% 
            filter(equation_table[,level] == do.call(paste, list(taxo[,level]))) %>% 
            filter(!is.na(intercept) | !is.na(ln_intercept)) %>% 
            dplyr::select(biomass_type, intercept, slope, ln_intercept) %>% 
            unique()
  ## If using trait, get custom list of species with other function
  if(level == "traits")
    c(spec_list <- 
        matcher_of_traits(specname, measurement_table),
      allometry <-
        equation_table %>% 
        filter(equation_table$bwg_name %in% spec_list) %>% 
        filter(!is.na(intercept) | !is.na(ln_intercept)) %>% 
        dplyr::select(biomass_type, intercept, slope, ln_intercept) %>% 
        unique())
      
  # Get number of equations
   equation_number <- 
     nrow(allometry)
  
  # Make bifurcation depending on number of equations
  if(compute)
    ## Number of equations
    if(equation_number == 0) 
      equations <- "none"  else 
        if(equation_number == 1) 
          equations <- "one" else
            if(equation_number > 1) 
              equations <- "multiple"
      
  # Case 1: one equation for the level
  if(equations == "one")
    ## simply compute the biomass using correct equation
    c(biomass <- equation_finder(size_mm, allometry, taxo) * abundance,
      path <- paste0(path,"_BM:", level, "_1", allometry$biomass_type[1]))
      
    
  # Case 2: more than one equation at the level
  if(equations == "multiple") 
    c(## Count instance of equations based on dry and wet weight
      type_count <- 
        allometry %>% 
        count(biomass_type) %>% 
        ### Keep only the most common kind of equation
        filter(n == max(n)),
      ## If we have the same number of equations in both dry and wet, prioritise dry
      ifelse(nrow(type_count) == 2,
             type_count <- 
               type_count %>% 
               filter(biomass_type == "dry"),
             type_count <- 
               type_count),
      ## Extract the selected kind of equation
      allometry <- 
        allometry %>% 
        filter(biomass_type == type_count$biomass_type),
      ## Update number of equations
      equation_number <- 
        nrow(allometry),
      ## Unite values in type count for path
      type_count <- 
        type_count %>% 
        unite(count, n, biomass_type),
      ## Set biomass to zero
      biomass <- 0,
    ## Sum biomass obtained from the different equations
    for(j in 1:equation_number){
      eq <- allometry[j,]
      biomass <- biomass + equation_finder(size_mm, eq, taxo) * abundance
    },
    ## Average the result
    biomass <- biomass/equation_number,
    ## Add equation information to path
    path <- paste0(path,"_BM:", level, "_", type_count))
    
  # Case 3: no equations at the level
  ## Simply return NA
  if(equations == "none")
      biomass <- NA
    
  # Return computed biomass
  return(data.frame(biomass, as.character(path)))
}

# Find right kind of allometric equation to use ---------------------------
equation_finder <- function(size_mm, row, taxo){
  ## Determine which type of equation to use
  type <- 
    ifelse(!is.na(row$intercept), 
           "log10",
           ifelse(!is.na(row$ln_intercept),
                  "ln", NA))
  
  ## Possible error wit equations table
  if(is.na(type))
    stop("Error with equation format, please double check equation table")
  
  ## If function is log10
  if(type == "log10")
    per_cap_biomass <- 
      10^(row$slope * log10(size_mm) + row$intercept)
  
  ## If function is natural logarithm
  if(type == "ln")
    per_cap_biomass <-
      exp(row$slope * log(size_mm) + row$ln_intercept)
  
  ## Exceptions
  ### With equation of Acari, weight is in micrograms, need to convert to milligrams
  if(!is.na(taxo$subclass) & taxo$subclass == "Acari")
    per_cap_biomass <-
      per_cap_biomass/1000
  
  ## Return value  
  return(per_cap_biomass)
  
}

# Estimate size  -----------------------------------------------
sizest <- function(specname, size_mm, level, path, taxo, equation_table, measurement_table, data_table){

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
# Trait-matching ----------------------------------------------------------
matcher_of_traits <- function(specname, measurement_table){
  # Extract traits of given species
  traits <- 
    measurement_table %>% 
    filter(bwg_name == specname) %>% 
    dplyr::select(BS1:BF4)
  
  # NA catcher 
  if(ncol(traits) == ncol(traits[!is.na(colSums(traits))]))
    proceed <- TRUE else
      proceed <- FALSE
    
  # If no NAS in traits get all species that have the same traits +/- 1 and make it a vector
  if(proceed)
    spec_list <- 
      measurement_table %>% 
     ## Kept in that format to make explicit changes and allow flexibility in trait matching
      filter(BS1 %in% c((traits$BS1 - 1):(traits$BS1 + 1)) &
             BS2 %in% c((traits$BS2 - 1):(traits$BS2 + 1)) &
             BS3 %in% c((traits$BS3 - 1):(traits$BS3 + 1)) &
             BS4 %in% c((traits$BS4 - 1):(traits$BS4 + 1)) &
             LO1 %in% c((traits$LO1 - 1):(traits$LO1 + 1)) &
             LO2 %in% c((traits$LO2 - 1):(traits$LO2 + 1)) &
             LO3 %in% c((traits$LO3 - 1):(traits$LO3 + 1)) &
             LO4 %in% c((traits$LO4 - 1):(traits$LO4 + 1)) &  
             LO5 %in% c((traits$LO5 - 1):(traits$LO5 + 1)) &
             LO6 %in% c((traits$LO6 - 1):(traits$LO6 + 1)) &
             LO7 %in% c((traits$LO7 - 1):(traits$LO7 + 1)) &
             MD1 %in% c((traits$MD1 - 1):(traits$MD1 + 1)) &
             MD2 %in% c((traits$MD2 - 1):(traits$MD2 + 1)) &
             MD3 %in% c((traits$MD3 - 1):(traits$MD3 + 1)) &
             MD4 %in% c((traits$MD4 - 1):(traits$MD4 + 1)) &  
             MD5 %in% c((traits$MD5 - 1):(traits$MD5 + 1)) &
             MD6 %in% c((traits$MD6 - 1):(traits$MD6 + 1)) &
             MD7 %in% c((traits$MD7 - 1):(traits$MD7 + 1)) &
             MD8 %in% c((traits$MD8 - 1):(traits$MD8 + 1)) &
             BF1 %in% c((traits$BF1 - 1):(traits$BF1 + 1)) &
             BF2 %in% c((traits$BF2 - 1):(traits$BF2 + 1)) &
             BF3 %in% c((traits$BF3 - 1):(traits$BF3 + 1)) &
             BF4 %in% c((traits$BF4 - 1):(traits$BF4 + 1))) %>% 
      dplyr::select(bwg_name) %>% 
      unique() %>% 
      pull()
  
  # If we have NAs 
  if(!proceed)
    spec_list <- 
      c(specname)

  # Return list
  return(spec_list)
  
}

# Wrapper function going row-by-row ----------------------------
hello_metry <- function(equation_table, measurement_table, data_table, print){
  # browser()
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
                  est <- sizest(specname, size_mm, level, path, taxo, equation_table, measurement_table, data_table)
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
        est <- get_allometric_equations(specname, level, size_mm, abundance, path, taxo, equation_table, measurement_table)
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
                       dplyr::select(domain:species) %>% 
                       unique()) %>% 
           relocate(size_original:path, .after = species) %>% 
           ### to make things easy only put NA where size was not computed
           mutate(size_used = ifelse(abundance == 0, NA, size_used)))
        
}



# Update measurement table --------------------------------------------------
measurement_supplement <- function(measurement_table, data_table){
  # Keep only numerical measurements of data table
  data_table_num <- 
    data_table %>% 
    dplyr::select(bwg_name, size_mm) %>% 
    filter(!is.na(as.numeric(size_mm))) %>% 
    unique %>% 
    rename(length_mm = size_mm) %>% 
    mutate(length_mm = as.numeric(length_mm))
  
  # Make stub of measurement_table 
  measurement_stub <- 
    measurement_table %>% 
    dplyr::select(species_id:BF4) %>% 
    unique()
  
  # Join to numerical measurements
  data_table_num <- 
    data_table_num %>% 
    left_join(measurement_stub)
  
  # Bind to measurement table
  measurement_table <- 
    measurement_table %>% 
    bind_rows(data_table_num)
  
  # Return updated data
  return(measurement_table)
    
}




