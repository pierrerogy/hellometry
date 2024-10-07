#' Get all size estimates
#'
#' Computes weigthed average and small, medium, large size categories for input data
#'
#'
#' @param dats Input data
#' @return A vector with weighted average of size, and estimations for small, medium and large sizes
#' @export
sizest <- function(dats){

  # Make all size estimations
  ret <- 
    c(
      # First compute weighted average
      weighted.mean(dats$size_mm),
      # Second compute size categories
      dats$size_mm %>%
        ## Order vector
        sort(decreasing = TRUE) %>% 
        ## Split into three chunks along order values
        split(cut(seq_along(.), 
                  3, 
                  labels = FALSE)) %>% 
        ## Get mean of each chunk
        purrr::map(~mean(.)) %>% 
        ## Flatten back to vector
        purrr::flatten_dbl())
    
  # Return
  return(ret)
}

