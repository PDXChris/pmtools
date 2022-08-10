#' Merge PAWMAP station information into a data frame.
#'
#' @param df  A data frame with a station field
#' @param by.y Name of station field, to be matched with site_identifier
#' @param filterNA should stations w/o info be filtered? (A warning listing the
#' unmatched stations is provided.)
#' @param fields  Fields to add from the station table
#' @param renameStat Should the site_identifier field be renamed to station?
#' (to match historical output)
#' @return The original data frame merged with station info fields.  The merge
#' returns all rows of df even if there is no match, to indicate that there
#' are stations in df not present in the station table.
#' @examples
#' df <- data.frame(site_identifier=unique(stationInfo$site_identifier),
#'                  result=rnorm(length(stationInfo$site_identifier)))
#' mergeStatInfo(df)
#' @export


mergeStatInfo <- function(df, by.y='site_identifier', filterNA=TRUE,
                          fields=c('site_identifier', 'watershed', 'subwat',
                                       'loc.lbl', 'panel', 'duration'),
                          renameStat=NULL) {

  # ensure that key field "station' is included
  x_fields <- unique(c('site_identifier', fields))

  df_wStats <- merge(pmtools::stationInfo[, x_fields], df,
        all.y=TRUE, by.x='site_identifier', by.y=by.y)

  # remove stations w/o metadata if filterNA=T
  if (filterNA){
    if (any(is.na(df_wStats[['watershed']]))) {
      unmatchedStats = unique(df_wStats$site_identifier[is.na(df_wStats$watershed)])
      warning('There are stations which do not have metadata.
              The following stations will be removed: ', toString(unmatchedStats))
      df_wStats <- df_wStats[!is.na(df_wStats[['watershed']]), ]
    }
  }

  if (renameStat){
    names(df_wStats)[names(df_wStats)=="site_identifier"] <- renameStat
  }

  df_wStats
}
