
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
#' @param station A string or vector of station numbers to look up
#' @param field use station (default) or loc_code?
#' @examples
#' stLook('P2000')
#' stLook(c('P2000', 'P0012'))
#' stLook('0012', 'loc_code')
#' @export

stLook <- function(station, field = 'station') {

  # ID stations not in list
  if (any(!station %in% stationInfo[[field]])){
    miss_stats <- station[!station %in% stationInfo[[field]]]
    warning(paste0('The following stations are not in the PAWMAP table: ',
                   toString(miss_stats)))
    station <- station[station %in% stationInfo[[field]]]
  }

  x <- stationInfo[stationInfo[[field]] %in% station, ]
  x

}


# Load PAWMAP data
#' @export
loadPMdat <- function(dtype) {

  filepath <- switch(dtype,
                     hab = '../pmtoolsFiles/raw_data/hab14.rda',
                     wq  = '../pmtoolsFiles/raw_data/wq14.rda',
                     bio = '../pmtoolsFiles/raw_data/bio14.rda'
  )

  load(filepath, .GlobalEnv)
}

#' Calculate the geometric mean
#'
#' @param x a vector of numbers
#' @export

gmean <- function(x){
  exp(mean(log(x)))
}

#' Calculate summary statistic by station
#' @param df data frame containing the data
#' @param stat statistical function to calculate.  Mean is the default.
#' @param station station field name, supplied as a string
#' @param vbl field name numeric variable to summarize, supplied as a string
#' @return a data frame with summary statistic by station
#' @importFrom plyr ddply
#' @export

statByStation <- function(df, stat = 'mean', station = 'loc_code',
                          vbl = 'result') {
  statByStat_1 <- function(x) {
    stat = do.call(stat, list(x[[vbl]]))
    cns = ifelse(any(x[['cens']]=='='), 'Detected', 'None\nDetected')
    pct_det = mean(x$cens == '=')
    data.frame(stat, cns, pct_det)
  }

  ddply(df, station, statByStat_1)
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

#' Drop Amphibians from a data frame of fish names
#'
#' @param df The data frame containing the fish data
#' @param vbl  A field within df containing the fish names
#' @return A A data frame without amphibians
#' @export

dropAmphibs <- function(df, vbl) {

  ## Remove amphibians and invertebrates
  drops <- c("dicamptodon.*", ".*frog.*",
             "cra.fish", ".*salamander.*", ".*newt.*")
  df <- df[!grepl(paste(drops, collapse="|"), df[[vbl]]), ]
}
