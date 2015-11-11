#' Plot PAWMAP Macroinvertebrate Data within a selected watershed.
#'
#' @param wat the watershed to plot
#' @param dfm  The data frame containing the variable
#' @return A ggplot dot plot of the Macroinvertebrate Observed/Expected scores
#' within a watershed
#' @examples
#' library(ggplot2)
#' stations <- unique(stationInfo$loc_code[stationInfo$duration=='P'])
#' d <- data.frame(loc_code=unique(stations), metric_name='oep5',
#'                  watershed='Tryon Creek', result=rnorm(length(stations)))
#' d <- mergeStatInfo(d)
#' p <- plotOep5_InWat('Johnson Creek', d)
#' p + ggtitle('Macroinvertebrate Observed/Expected - Generated Data for Example\n')
#' @export

plotOep5_InWat <- function(wat, dfm=oep5) {

  tmp <- mergeStatInfo(dfm)
  tmp <- tmp[tmp$watershed == wat, ]

  # Sort data by mean of seasonal data
  tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, result) )


  p <- ggplot(aes(loc.lbl, result), data = tmp) +
    geom_point(size=4) +
    geom_hline(yintercept=.85, colour='red', size=1.5) +
    geom_hline(yintercept=.91, colour='darkgreen', size=1.5) +
        xlab('') +  ylab("\nObserved/Expected") + coord_flip() + theme_bw() +
    theme(axis.text = element_text(size=14),
          title = element_text(size = rel(1.3)))

  return(p)

}
