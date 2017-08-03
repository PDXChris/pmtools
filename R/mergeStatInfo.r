#' Merge PAWMAP station information into a data frame.
#'
#' @param df  A data frame with a station field
#' @param by.x  The station field within the stationInfo table
#' @param by.y  The station field within the data frame df
#' @param fields  Fields to add from the station table
#' @return The original data frame merged with station fields.  The merge
#' returns all rows of df even if there is no match, to indicate that there
#' are stations in df not present in the station table.
#' @examples
#' df <- data.frame(loc_code=unique(stationInfo$station),
#'                  result=rnorm(length(stationInfo$station)))
#' mergeStatInfo(df)
#' @export


mergeStatInfo <- function(df, by.x='station', by.y='station',
                          fields=c('watershed', 'subwat',
                                   'loc.lbl', 'panel', 'duration')) {
  merge(stationInfo[, c(by.x, fields)],
        df[, !colnames(df) %in% fields],
        by.x=by.x, by.y=by.y, all.y=TRUE)
}
