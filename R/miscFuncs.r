
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
#' stLook(c('2000', '0012'))
#' stLook('P0012', 'station')
#' @export

stLook <- function(station, field='loc_code') {
  x <- stationInfo[stationInfo[[field]] %in% station, ]
  x

}


#' @export
loadPMdat <- function(dtype) {
  if (dtype=='hab') load('../pmtoolsFiles/raw_data/hab14.rda', .GlobalEnv)
  if (dtype=='wq') load('../pmtoolsFiles/raw_data/wq14.rda', .GlobalEnv)
  if (dtype=='bio') load('../pmtoolsFiles/raw_data/bio14.rda', .GlobalEnv)
}

#' @export
gmean <- function(x){
  exp(mean(log(x)))
}

#' Calculate summary statistic by station
#' @param dfm data frame containing the data
#' @param stat statistical function to calculate.  Mean is the default.
#' @param station station field name, supplied as a string
#' @param vbl field name numeric variable to summarize, supplied as a string
#' @return a data frame with summary statistic by station
#' @import plyr
#' @export

statByStation <- function(dfm, stat = 'mean', station = 'loc_code',
                          vbl = 'result') {
  statByStat_1 <- function(x) {
    stat = do.call(stat, list(x[['result']]))
    cns = ifelse(any(x[['cens']]=='='), 'Detected', 'None\nDetected')
    pct_det = mean(x$cens == '=')
    data.frame(stat, cns, pct_det)
  }

  ddply(dfm, station, statByStat_1)
}

