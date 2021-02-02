#' Temporary R file to push the R folder to GitHub
#'
#' @description This function doesn't do anything useful, and I just created it 
#' so that the R folder would be pushed to GitHub as empty folders don't get 
#' committed. Will delete once we add real functions.
#'
#' @param x a numeric value
#' @param y a numeric value
#' 
#' @return Returns the sum of \code{x} and \code{y}
#'
#' @examples
#'
#' temp(1, 10)
#' temp(2, NA)
#'
#' @export

temp <- function(x, y) {
  sumxy <- sum(x, y, na.rm = TRUE)
  return(sumxy)
}