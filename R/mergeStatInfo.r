#' Merge PAWMAP station information into a data frame.
#'
#' @param df  A data frame with a station field
#' @param fields  Fields to add from the station table
#' @param ... Provides the ability to add by (if using loc_code) or by.x and
#' by.y if the station field is not named "station"
#' @return The original data frame merged with station info fields.  The merge
#' returns all rows of df even if there is no match, to indicate that there
#' are stations in df not present in the station table.
#' @examples
#' df <- data.frame(station=unique(stationInfo$station),
#'                  result=rnorm(length(stationInfo$station)))
#' mergeStatInfo(df)
#' @export


mergeStatInfo <- function(df, fields=c('station', 'watershed', 'subwat',
                                       'loc.lbl', 'panel', 'duration'), ...) {

  args1 <- list(...)

  if ('by' %in% names(args1)) {
    x_fields <- unique(c(fields, args1[['by']]))
  } else if ('by.x' %in% names(args1)) {
    x_fields <- unique(c(fields, args1[['by.x']]))
  } else x_fields <- fields

  merge(pmtools::stationInfo[, x_fields], df,
        all.y=TRUE, ...)
}
