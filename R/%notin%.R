#' Define the %notin% operator directly
#' @export
`%notin%` <- function(x, table) {
  !x %in% table
}