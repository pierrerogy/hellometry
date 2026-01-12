#' Add BWG-formatted taxonomy to data
#'
#' Add all taxonomy columns needed by the package
#' 
#' @param data_table The input data table, the column 'bwg_name' has to be present 
#' @return An updated data frame with taxonomy added
#' @export
add_taxonomy <- function(data_table){
  
  # This one column needs to be here
  if("bwg_name" %notin% colnames(data_table))
    stop("Please call column with bwg species names values 'bwg_name'")
  
  # Taxon list
  taxa <- 
    c("domain", "kingdom", "phylum", "subphylum", "class", "subclass", 
      "ord", "subord", "family", "subfamily", "tribe", "genus", "species",
      "stage")
  
  # Add taxonomy to data table
  ret <- 
    data_table %>% 
    dplyr::left_join(get_bwgnames()) %>% 
    dplyr::relocate(all_of(taxa), 
                    .after = bwg_name)
  # Return updated data
  return(ret)
}




