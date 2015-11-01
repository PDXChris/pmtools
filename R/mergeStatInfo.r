#' Merge PAWMAP station information into a data frame.
#'
#' @param df  A data frame with a station field
#' @param stat  The station field within df
#' @param fields  Fields to add from the station table
#' @return The original data frame merged with station fields.  The merge
#' returns all rows of df even if there is no match to indicate there
#' are stations in df not present in the station table.
#' @examples
#' df <- data.frame(loc_code=unique(stationInfo$loc_code),
#'                  result=rnorm(length(stationInfo$loc_code)))
#' mergeStatInfo(df)
#' @export


mergeStatInfo <- function(df, stat='loc_code',
                          fields=c('watershed', 'subwat',
                                   'loc.lbl', 'panel', 'duration')) {
  merge(stationInfo[,c(stat, fields)],
        df[, !colnames(df) %in% fields],
        by = stat, all.y=TRUE)
}
