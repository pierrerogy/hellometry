#' Wrapper function going row-by-row to estimate size (mm) and biomass (mg)
#'
#' Wrapper function, input your data and get size and measurement . Please name column with number of specimen "abundance", 
#' column with BWG name "bwg_name",  and column with measurement in mm "size_mm".
#' If you do not have a numerical measurement for a given specimen, the algorithm can
#' do size estimations if you input "small", "medium", "large" or "unknown". In this
#' case, the algorithm will use existing size measurements for the species, and use 
#' the size distribution to estimate "small", "medium" and "large" inputs, or a weighted
#' average of all measurements if the input is "unknown". If you feed in a dataframe that already
#' has some biomass measurements but not everywhere, please name this column "biomass_mg", and include
#' another column "biomass_kind" that says if the biomass is "dry" or "wet". This is important as this
#' information is used in the estimations.
#'
#' @param data_table The input data table, please include columns columns "abundance", "bwg_name", "size_mm", "stage" (larva/pupa/adult, please only put 'adult' for adult insects) are present
#' @param biomass_kind Should data used in inference be "dry" for just dry biomass, or "both" (default) for both dry and wet biomass.
#' If both (the default) is chosen, then the function will determine which dry or wet equations or raw weight is present, and choose
#' the most numerous. If there is the same number of dry and wet equations, dry equations are always favoured. Dry and wet
#' measurements are never mixed. 
#' @param database Should data from the bwg database be used to supplement the measurements (TRUE(default)/FALSE). This function uses numerical
#' measurements both from the BWG database and from the data you provide. Only put FALSE if you are doing estimations from 
#' data already present in the BWG database (estimation for these data available with database_data(TRUE)).
#' 
#' @return Your initial data table with three extra columns: value (in mm)
#' of size estimation (NA if not done), total biomass for given row (in mg), 
#' as well as path taken through the function. The nomenclature of this path column is as follows.
#' ## Special cases
#'- "null_biomass": abundance was 0
#'- "_size_estimaton_failed": there were not enough close relatives or species with matching traits to compute size
#'- "_biomass_estimation_failed": there were no allometric equation at to estimate biomass
#'- "_raw_dry/wet": if there was a raw (direct) measurement for that particular species-size combination, and whether that measurement is dry or wet biomass
#'- "_external_dry_equations": data provided was not sufficient so equations from the literature were used
#' ## Regular cases
#'- if the species goes through estimation of size (sizest()): 
#'  estimation kind:level_number of measurements.
#'  For example,  WA:genus_5 (size estimation using weighted average on 5 measurements from the species' genus), BIN:subfamily_3 (size estimation using size bins (S, M, L) on three measurement a the subfamily level)
#'- if the allometric equations are used (get_biomass()): 
#'  biomass estimation:taxonomic level of inference_number of equations_dry/wet.
#'  For example, -AE:bwg_name_wet (allometric equation from wet biomass at the species level), -AE:subclass_dry (allometric equation from dry biomass at the subclass level)
#' # If a species went through both size estimation and biomass estimations, the path will be composite, e.g. WA:genus_5-AE:bwg_name_wet


#' @export
hello_metry <- function(data_table, biomass_kind = "both", database = TRUE){
  #browser()
  # Function %notin%
  '%notin%' <- 
    Negate('%in%')
  
  # Some error catching 
  ## Important columns have proper names
  if("abundance" %notin% colnames(data_table))
    stop("Please call column with abundance values 'abundance'")
  if("bwg_name" %notin% colnames(data_table))
    stop("Please call column with bwg species names values 'bwg_name'")
  if("size_mm" %notin% colnames(data_table))
    stop("Please call column with specimen measurement values 'size_mm'")
  if("stage" %notin% colnames(data_table))
    stop("Please call column with life stage (larva/pupa/adult) 'stage'")
  ## Database is actually a true false
  if(database %notin% c(TRUE, FALSE))
    stop("Database has to be TRUE/FALSE")
  ## Biomass kind needs to be dry or both
  if(biomass_kind %notin% c("dry", "both"))
    stop("Biomass kind has to be 'dry' or 'both'")
  
  
  # Load measurement table
  # If user wants to use the database measurement or not
  measurement_table <- 
    get_measurements(database = database)
  # Check if any BWG name missing, and if yes add names and append table
  if(sum(is.na(data_table$bwg_name)) > 0)
    c(## Get new species names
      data_table <- 
        name_herder(data_table),
      ## Append measurement table
      measurement_table <- 
        append_names(measurement_table,
                            data_table))
  
  # Add if any measurement table
  measurement_table <- 
    append_measurements(measurement_table,
                        data_table)
  
  # Make copy of data table
  # Make it so that it incorporates measurements present
  # If biomass column is present make a fork here
  if("biomass_mg" %notin% colnames(data_table))
    {data_return <- 
      data_table %>% 
      ## And add new columns to fill
      dplyr::mutate(size_used_mm = NA,
                    biomass = NA,
                    ci_lwr = NA,
                    ci_upr = NA,
                    path = NA)} else
                      {data_return <- 
                        data_table %>% 
                        ## And add new columns to fill
                        dplyr::mutate(size_used_mm = NA,
                                     biomass = biomass_mg,
                                     ci_lwr = NA,
                                     ci_upr = NA,
                                      path = NA) %>% 
                        dplyr::select(-biomass_mg)}

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
      "class")
  
  ## Limoniidae and Tipulidae essentially the same thing so can be considered together
  measurement_table <- 
    tipulimo(measurement_table)
  
  # Initiate progress bar
  pb <- 
    progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", 
                               total = nrow(data_return))
  pb$tick(0)

  # Loop to fill row by row
  for(i in 1:nrow(data_return)){
    
    ## Initialise function parameters
    row <- data_return[i,]
    biomass <- row$biomass
    ## If we already have a biomass value go to the next iteration
    if(!is.na(biomass)) next
    specname <- row$bwg_name
    size_mm <- row$size_mm
    abundance <- row$abundance
    stage0 <- row$stage
    
    ## Initialise path to record what happens in this function
    path <- ""
    
    ## Some error catching
    ### Make sure abundance is not NA
    if(is.na(abundance))
      stop(paste0("Abundance row ", i," is NA"))
    
   ## Let's not waste time if abundance is zero
    if(abundance == 0) 
      c(biomass <- rep(0,3) , 
       path <- "null_biomass") else
          biomass <- 
            rep(NA, 3)
  
   ## Check if species is in the database
   if(abundance > 0)
   ### Make small frame to get taxonomy
     c(taxo <- 
        measurement_table %>% 
        dplyr::filter(bwg_name == specname & stage == stage0) %>% 
        dplyr::select(bwg_name:species, stage) %>% 
        unique(),
     ### If not in database, give special biomass value
      if(nrow(taxo) == 0)
        c(taxo <- 
            row %>% 
            dplyr::select(domain:species, stage, bwg_name)),
      ### If in database 
      #### Check if we have a numeric size
        suppressWarnings(if(!is.na(as.numeric(size_mm))) 
            c(size_mm <- as.numeric(size_mm),
              path <- paste0(path, "raw_size")) else
              #### If we don't, do a size estimation
              #### As long as size is not numeric
              while(!is.numeric(size_mm)){
                ##### Go through my list of group
                for(level in level_list){
                  est <- sizest(specname, size_mm, level, stage0, path, taxo, equation_table, measurement_table, data_table)
                  size_mm <- est[,1]
                  path <- est[,2]
                  #### Break loop if done
                  if(is.numeric(size_mm))
                    break
                  ###### If we got to the end and still nothing, give up
                  if(!is.numeric(size_mm) & level == "class")
                    c(path <- 
                        paste0(path, "size_estimation_failed"),
                      biomass <- 
                        "cannot_estimate",
                      size_mm <- 
                        666)
                }}))
    
    ### Change 666 to proper message
         if(size_mm == 666)
           size_mm <- 
            "cannot_estimate"
      
    ### Call the allometric equations if we have a numerical measurement
    if(is.numeric(size_mm)){
    #### As long as biomass is NA
    #### Vector of three elements, so using first one
      ##### Go through my list of group
      for(level in level_list){
        c(est <- 
            get_biomass(specname, level, size_mm, abundance, stage0, path, taxo, measurement_table, biomass_kind),
        path <- 
          est[4],
        biomass <- 
          as.numeric(est[1:3]),
        #### Round number and break loop if done
        if(!is.na(biomass[1]))
          c(biomass <- 
              signif(biomass, 
                     digits = 4),
          break),
        ###### If we got to the end and still nothing,call the firefighterss
        if(is.na(biomass[1]) & level == "class")
          c(biomass <-
              firetruck(size_mm, abundance, taxo),
              ifelse(is.na(biomass[1]),
                     c(biomass <- rep("cannot_estimate", 3),
                       path <- paste0(path, "_cannot_estimate")),
                     c(biomass <- c(signif(as.numeric(biomass[1]), 
                                           digits = 4),
                                    rep("cannot_estimate", 2)),
                     path <- paste0(path, "-external_dry_equations")))))
        

      }}
  
    ### Add biomass and path value to data frame to return
    data_return[i,(ncol(data_return)-4)] <- 
      as.character(size_mm)
    ### Need to reverse the order of elements in the vector
    data_return[i,(ncol(data_return)-1):(ncol(data_return)-3)] <- 
      matrix(as.character(rev(biomass)), nrow = 1)
    data_return[i,ncol(data_return)] <- 
      as.character(path)
    
    ### Add tick
    pb$tick()
    
  } 
  ## Return new data frame
  return(data_return %>% 
           dplyr::rename(biomass_mg = biomass,
                  size_original = size_mm) %>% 
           ### to make things easy only put NA where size was not computed
           dplyr::mutate(size_used_mm = ifelse(abundance == 0, NA, size_used_mm)))
        
}
