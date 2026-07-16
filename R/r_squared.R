#' Get the R2 of a fitted allometric model
#'
#' Wraps `performance::r2()`, which returns the R2 suited to the model it is
#' given, so that any kind of model can be filtered the same way in
#' `full_estimation_table()`
#'
#' @param model A fitted allometric model
#' @return A single R2 value, NA if none could be computed
#' @export
r_squared <- function(model) {

  # Get the R2, which performance::r2() returns as a list
  ## Degenerate models can fail here, so catch that and return nothing
  ret <-
    tryCatch(suppressWarnings(performance::r2(model)),
             error = function(e) NULL)

  # If no R2 came back, return NA so that the model gets filtered out
  if (is.null(ret) || length(ret) == 0)
    return(NA)

  # Keep the first value, i.e. the R2 itself
  ret <-
    as.numeric(ret[[1]])

  # Return
  return(ret)

}