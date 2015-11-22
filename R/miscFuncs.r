
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


#' Look up information on a station by number
#'
#' @param station the station number to look up (as a string)
#' @param field use loc_code (default) or station?
#' @examples
#' stLook('2000')
#' stLook('P0012', 'station')
#' @export

stLook <- function(station, field='loc_code') {
  x <- stationInfo[stationInfo[, field]==station, ]
  x

}


gmean <- function(x){
  exp(mean(log(x)))
}
