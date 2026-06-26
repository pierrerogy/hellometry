#' Not In Operator
#'
#' Returns logical vector indicating if elements are **not** in a given set.
#'
#' @param x vector or NULL: the values to be matched.
#' @param table vector or NULL: the values to be matched against.
#' @return A logical vector of the same length as `x`.
#' @export
`%notin%` <- function(x, table) {
  !(x %in% table)
}