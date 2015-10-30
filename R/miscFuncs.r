#' A Set of Miscellaneous Functions for Working with Data.
#'
#' @param x  A string to convert to title case.
#' @return A string in title case
#' @examples
#' .simpleCap('a lower case string')
#' @export


.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
