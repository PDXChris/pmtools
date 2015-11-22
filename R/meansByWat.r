#' Summarize PAWMAP data by watershed
#'
#' @param dfm  The data frame containing the variable
#' @param vbl The name of the field storing the value to be summarized
#' @param select Select the proper variable from long format
#' @param selVar the field containing the metric code
#' @param storm Calculate separate means for storm and seasonal data?
#' @param sort The statistic to sort the data on
#' @return A table with various means by watershed
#' @export

meansByWat <- function(dfm, vbl, select = NULL, selVar = 'metric_code',
                       storm = FALSE, sort='geomean') {

  if (!is.null(select)) {
    dfm <- dfm[dfm[[selVar]]==select, ]
  }

  dfm <- mergeStatInfo(dfm, fields = 'watershed')

  if(storm){
    grp <- c('watershed', 'storm')
  } else {
    grp <- 'watershed'
  }

  dfm <- plyr::ddply(dfm, grp,
                     function(x){
                       c(   mean = mean(x[[vbl]], na.rm = TRUE),
                         geomean = pmtools:::gmean(x[, vbl]),
                          median = median(x[[vbl]], na.rm = TRUE))
                     }
  )

  dfm <- dfm[order(dfm[[sort]], decreasing = TRUE), ]

  dfm
}
