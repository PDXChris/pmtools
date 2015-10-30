#' Merge PAWMAP station information into a data frame.
#'
#' @param dat  A data frame with station field.
#' @param stat  The station field within dat
#' @param fields  Fields to add from the station table
#' @return The original data frame merged with station fields
#' @examples
#' mergeStatInfo(wq14[wq14$metric_code == 'cu', ])
#' @export


mergeStatInfo <- function(dat, stat='loc_code',
                          fields=c('watershed', 'subwat',
                                   'loc.lbl', 'panel', 'duration')) {
  merge(stationInfo[,c(stat, fields)],
        dat[, !colnames(dat) %in% fields],
        by = stat, all.y=TRUE)
}
