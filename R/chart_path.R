#' Summarise path column
#'
#' Summarise estimations using path column
#'
#' @param data_table Output data from hello_metry()
#' @return A list with two elements. Element 1 returns how many unique
#' estimations were performed for size categories (weighted average (WA) or size
#' bins (BIN)) and allometric equations (AE)), and how many biomass values had raw wet or dry
#' equivalence in the database. Element 2 returns the level, number of
#' unique estimations, and mean number of values (numerical size measurement or
#' number of allometric equation) for each of these.
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
    dplyr::rename(what = path)
  ### If there are none
  if(nrow(null_biomass) == 0)
    null_biomass <- 
    data.frame(what = "null_biomass",
               n = 0)
    
  
  ## Extract estimation failures, count and add grouping column
  est_failed <- 
    path %>%
    dplyr::filter(path %in% c("size_estimation_failed",
                               "biomass_estimation_failed")) %>% 
    dplyr::group_by(path) %>%
    dplyr::count() %>% 
    dplyr::rename(what = path)
  ### If there are none
  if(nrow(est_failed) == 0)
    est_failed <- 
    data.frame(what = "est_failed",
               n = 0)
    
  ## Extract raw measurements
  raw_meas <- 
    path %>%
    dplyr::filter(path %in% c("-raw_dry", "-raw_wet")) %>% 
    dplyr::group_by(path) %>% 
    dplyr::count() %>% 
    dplyr::rename(what = path,
                  unique_estimations = n) 
  ### Remove the -
  raw_meas$what <- 
    stringr::str_remove_all(raw_meas$what,
                            "-")
  
  ## Split path between different levels of estimations
  size_biomass <- 
    path %>% 
    unique() %>% 
    dplyr::filter(path %notin% c("null_biomass", "size_estimation_failed",
                                 "biomass_estimation_failed", "-raw_dry", "-raw_wet")) %>% 
    tidyr::separate(path,
                    into = c("size", "biomass"), 
                    sep = "-")
  
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
      ### Remove rows where they were no estimation
      dplyr::filter(what != "")
    
    ### The amount of information differs between size and biomass
    ### Need to harmonise beforehand
    if(i == "size")
      c(temp <- 
          temp %>% 
          ### Split between level and how many
          tidyr::separate(how,
                          into = c("level", "n"),
                          sep = "_") %>% 
          ### Make howmany numeric (default is character)
          dplyr::mutate(n = as.numeric(n)))
    if(i == "biomass")
      c(#### Cases where no equations were used
        temp2 <- 
          temp %>% 
          dplyr::filter(what != "AE") %>% 
          dplyr::select(what) %>% 
          dplyr::group_by(what) %>% 
          dplyr::count() %>% 
          dplyr::rename(unique_estimations = n),
        #### Update raw measurements
        raw_meas <- 
          raw_meas %>% 
          dplyr::bind_rows(temp2) %>% 
          dplyr::group_by(what) %>% 
          dplyr::summarise_all(sum),
        #### Cases where equations were used
        temp <- 
          temp %>% 
          dplyr::filter(what == "AE") %>% 
          ### Split between level and how many
          tidyr::separate(how,
                          into = c("level", "n", "type"),
                          sep = "_") %>% 
          ### Put back together level and type
          tidyr::unite(c("level", "type"),
                       col = "level",
                       sep = "_") %>% 
          ### Make howmany numeric (default is character)
          dplyr::mutate(n = as.numeric(n)))
    
    
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
                              by = c("what", "level")))
    ## Calculate overall sum
    assign(paste0(i, "_overall"),
           temp %>% 
             dplyr::select(what, level) %>%
             dplyr::group_by(what) %>% 
             dplyr::count() %>% 
             dplyr::rename(unique_estimations = n))
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
    dplyr::bind_rows(biomass_overall,
                     raw_meas) %>% 
    dplyr::ungroup()
    
  ## Combine in a list
  dats <- 
    list(overall = overall,
         summary = summary)
  
  ## Print general info
  cat(paste("Hello, thanks for using the package!", 
            paste0("There were ", nrow(data_table), " rows in your data."), 
            paste0("Of these ", sum(est_failed$n), " failed, and ",
            null_biomass$n, " had 0 abundance."),
            paste0("You thus have ", nrow(data_table) -sum(est_failed$n) - null_biomass$n,
                   " estimations."),
            sep = "\n"))
  
  ## Return list
  return(dats)

}
