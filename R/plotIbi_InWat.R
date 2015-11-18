#' Plot PAWMAP Fish Data within a selected watershed.
#'
#' @param wat the watershed to plot
#' @param dfm  The data frame containing the variable
#' @return A ggplot dot plot of Fish Index of Biotic Integrity scores
#' within a watershed
#' @examples
#' library(ggplot2)
#' stations <- unique(stationInfo$loc_code[stationInfo$duration=='P'])
#' d <- data.frame(loc_code=unique(stations), metric_name='oep5',
#'                  watershed='Tryon Creek', result=rnorm(length(stations)))
#' d <- mergeStatInfo(d)
#' p <- plotOep5_InWat('Johnson Creek', d)
#' p + ggtitle('Macroinvertebrate Observed/Expected - Generated Data for Example\n')
#' @import ggplot2
#' @export

plotIbi_InWat <- function(wat, dfm=runFishIbi(), onlySummer=FALSE) {

  tmp <- mergeStatInfo(dfm)
  tmp <- tmp[tmp$watershed == wat, ]
  tmp$season <- factor(lubridate::quarter(tmp$ACTLDATE))
  levels(tmp$season) = c('Winter', 'Spring', 'Summer', 'Fall')
  if (onlySummer==TRUE) tmp <- tmp[tmp$season=='Summer', ]

  # Order watershed levels by their mean result
  tmp$loc.lbl <- factor(tmp$loc.lbl,
                          levels=levels(reorder(tmp$loc.lbl, tmp[['fish.ibi']], mean)))



  # Sort data by mean of seasonal data
  # tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, result) )


  p <- ggplot(aes(loc.lbl, fish.ibi), data = tmp) +
    geom_point(aes(shape=season), size=5, position = position_jitter(width = .2, height=0)) +
    # geom_hline(yintercept=.85, colour='red', size=1.5) +
    # geom_hline(yintercept=.91, colour='darkgreen', size=1.5) +
        xlab('') +  ylab("\nFish Index of Biotic Integrity") + coord_flip() +
    theme_bw() + theme(axis.text = element_text(size=14),
          title = element_text(size = rel(1.3)))

  return(p)

}
