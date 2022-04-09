#' Summarise path column
#'
#' Summarise estimations using path column
#'
#' @param data_table Output data from hello_metry()
#' @return A list with three elements. Element 1 returns how many times
#' a raw numerical measurement or biomass was used to estimate (or get 
#' a biomass value). Element 2 returns how many unique estimations 
#' were performed for size categories (weighted average (WA) or size
#' bins (BIN)) and allometric equations (AE)). Element 3 returns the 
#' level, number of unique estimations, and mean number of values (numerical size measurement or
#' number of allometric equation) for each of these.
#'
#' @export
chart_path <- function(data_table){
  # Notin functiom
  '%notin%' <- 
    Negate('%in%')
  
  ## Save data inside function
  data_table <- 
    data_table
  
  ## Is the data correct
  if("path" %notin% colnames(data_table))
    stop("Column 'path' missing. Did your data go through hello_metry()?")
 
  ## Extract path column
  path <- 
    data_table %>% 
    dplyr::select(path)
  
  ## Do a little fix of string to make the life easier
  path$path <- 
    stringr::str_replace_all(path$path,
                             "bwg_name",
                             "bwgname")
  
  ## Extract null biomass, count and add grouping column
  null_biomass <- 
    path %>%
    dplyr::filter(path == "null_biomass") %>% 
    dplyr::group_by(path) %>% 
    dplyr::count() %>% 
    dplyr::rename(what = path) %>% 
    dplyr::ungroup()
  ### If there are none
  if(nrow(null_biomass) == 0)
    null_biomass <- 
    data.frame(what = "null_biomass",
               n = 0)
    
  
  ## Make frame for failed estimations
  est_failed <- 
    path %>%
    ### Filter rows containing 'failed'
    dplyr::filter(stringr::str_detect(path, 'failed')) %>% 
    dplyr::group_by(path) %>%
    dplyr::count() %>% 
    dplyr::rename(what = path) %>% 
    ### Just keep the biomass estimation failed part of the string
    dplyr::mutate(what = ifelse(stringr::str_detect(what, "biomass_estimation_failed"), 
                                "biomass_estimation_failed",
                                 what)) %>% 
    dplyr::ungroup()
  ### If there are none
  if(nrow(est_failed) == 0)
    est_failed <- 
    data.frame(what = "size_estimation_failed",
               n = 0)
  
  ## Split path between different levels of estimations
  size_biomass <- 
    path %>% 
    ## First remove paths already tallied
    dplyr::filter(!stringr::str_detect(path, "size_estimation_failed"),
                  path != "null_biomass") %>% 
    tidyr::separate(path,
                    into = c("size", "biomass"), 
                    sep = "-")
    
  ## Make frame for raw biomass and size measurements
  raw_meas <- 
    size_biomass %>%
    dplyr::select(-biomass) %>% 
    ### Tick two columns on top of each other
    rbind(size_biomass %>% 
            dplyr::select(-size) %>% 
            dplyr::rename(size = biomass)) %>% 
    ### Only keep rows including 'raw' and sum
    dplyr::filter(stringr::str_detect(size, 'raw')) %>% 
    dplyr::group_by(size) %>% 
    dplyr::count() %>% 
    dplyr::rename(what = size,
                  n_instances = n) %>% 
    dplyr::ungroup()
  ### If there are none
  if(nrow(raw_meas) == 0)
    raw_meas <- 
    data.frame(what = "raw",
               n = 0)
  
  ## Remove rows already fully tallied to make count of estimations easier
  ## i.e. both size and biomass were raw, or size raw and biomass estimation failed
  size_biomass <- 
    size_biomass %>% 
    unique() %>% 
    dplyr::filter(!(stringr::str_detect(size, 'raw') & 
                      stringr::str_detect(biomass, 'raw')),
                  !(stringr::str_detect(size, 'raw') & 
                      biomass == "biomass_estimation_failed"))
  
      
  ## Create two dataframes with mean number of measurements/equations for each level of estimation
  ### Suppressing warnings
  suppressWarnings(
  for(i in c("size", "biomass")){
    ### Make temporary dataframe with cleaned measurements
    temp <- 
      size_biomass %>% 
      ### Select appropriate column
      dplyr::select(i) %>% 
      ### Split between what kind of inference, and level_number
      tidyr::separate(i,
                      into = c("what", "how"), 
                      sep = ":") %>%
      ### Remove rows where they were raw or no estimations
      dplyr::filter(what != "" &
                      !(stringr::str_detect(what, 'raw')))
    
    ### The amount of information differs between size and biomass
    ### Need to harmonise beforehand
    if(i == "size")
      #### Split between level and how many
        temp <- 
          temp %>% 
          tidyr::separate(how,
                          into = c("level", "n"),
                          sep = "_") %>% 
          ### Make howmany numeric (default is character)
          dplyr::mutate(n = as.numeric(n))
    if(i == "biomass")
      ### Split between level and how many
      temp <- 
        temp %>% 
        ### Add how many external
        dplyr::mutate(how = ifelse(stringr::str_detect(what, "external"),
                                   "external_dry_equations",
                                   how)) %>% 
        dplyr::rename(level = how)
          
        
    
    ## Calculate sum for all levels
    ### Assign can be used to create a dataframe with "paste"
    assign(paste0(i, "_summary"),
           temp %>% 
             dplyr::select(what, level) %>%
             dplyr::group_by(what, level) %>% 
             dplyr::count() %>% 
             dplyr::rename(unique_estimations = n) %>% 
             dplyr::left_join(temp %>% 
                                dplyr::group_by(what, level) %>%
                                dplyr::summarise_all(mean),
                              by = c("what", "level")) %>% 
             dplyr::ungroup())
    ## Calculate overall sum
    assign(paste0(i, "_overall"),
           temp %>% 
             dplyr::select(what, level) %>%
             dplyr::group_by(what) %>% 
             dplyr::count() %>% 
             dplyr::rename(unique_estimations = n) %>% 
             dplyr::ungroup())
})
  
  ## Combine summary frames
  summary <- 
    size_summary %>% 
    dplyr::bind_rows(biomass_summary) %>% 
    dplyr::rename(mean_nvalues = n) %>% 
    dplyr::ungroup()
  
  ## Combine overall frames
  overall <- 
    size_overall %>% 
    dplyr::bind_rows(biomass_overall) %>% 
    dplyr::ungroup()
  
  ## Combine in a list
  dats <- 
    list(raw = raw_meas,
         overall_est = overall,
         summary_est = summary)
  
  ## Print general info in a nice way
  cat(paste("Hello, thanks for using the package!", 
            paste0("There were ", nrow(data_table), " rows in your data."), 
            paste0("Of these ", sum(est_failed$n), " failed, and ",
            null_biomass$n, " had 0 abundance."),
            paste0("I performed ", nrow(size_biomass), " unique estimations."),
            sep = "\n"))
  cat("\n")
  
  ## Return list
  return(dats)

}
