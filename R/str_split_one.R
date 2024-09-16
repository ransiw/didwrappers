#' Split a string
#'
#' @param string A character vector with, at most, one element.
#' @param pattern A regex pattern
#' @param n number of splits to make
#'
#' @return A character vector.
# @export
#'
str_split_one <- function(string, pattern, n = Inf) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = pattern, n = n)[[1]]
  } else {
    character()
  }
}
