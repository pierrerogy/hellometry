#' Negate %in%
#'
#' Just a lot more practical
#'
#' @param x regular %in% parameter
#' @param y regular %in% parameter


#' @return Input without supplied parameters
#' @export

'%notin%' <- function(x,y)
  {Negate('%in%'(x,y))}
