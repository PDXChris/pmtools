
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

stLook <- function(station, field='station') {
  x <- stationInfo[stationInfo[[field]] %in% station, ]
  x

}


# Load PAWMAP data
#' @export
loadPMdat <- function(dtype) {
  if (dtype=='hab') load('../pmtoolsFiles/raw_data/hab14.rda', .GlobalEnv)
  if (dtype=='wq') load('../pmtoolsFiles/raw_data/wq14.rda', .GlobalEnv)
  if (dtype=='bio') load('../pmtoolsFiles/raw_data/bio14.rda', .GlobalEnv)
}

#' Calculate the geometric mean
#'
#' @param x a vector of numbers
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
    stat = do.call(stat, list(x[[vbl]]))
    cns = ifelse(any(x[['cens']]=='='), 'Detected', 'None\nDetected')
    pct_det = mean(x$cens == '=')
    data.frame(stat, cns, pct_det)
  }

  ddply(dfm, station, statByStat_1)
}

#' Create text summarizing regression fit
#' @param fit fit object of class lm
#' @return text with adj. r^2, p, slope and intercept summaries
#' @export

fitStats <- function(fit) {
  fitTxt <- paste0("Adj R^2 = ", signif(summary(fit)$adj.r.squared, 2),
                   "; p = ", signif(summary(fit)$coef[2,4], 2), "\n",
                   "Slope = ", signif(fit$coef[[2]], 2),
                   "; Intercept = ", signif(fit$coef[[1]],2)
  )
  return(fitTxt)
}


