#' Plot PAWMAP Fish Data within a selected watershed.
#'
#' @param wat the watershed to plot
#' @param dfm  The data frame containing the variable
#' @return A ggplot dot plot of Fish Index of Biotic Integrity scores
#' within a watershed
#' @examples
#' library(ggplot2)
#' stations <- unique(stationInfo$loc_code[stationInfo$duration=='P'])
#' random.dates <- sample(seq(as.Date('2010/07/01'), as.Date('2020/07/01'), by="day"), length(stations))
#' d <- data.frame(loc_code=unique(stations), metric_name='oep5',
#'                  watershed='Johnson Creek', sample_end_time=random.dates,
#'                  fish.ibi=rnorm(length(stations)))
#' d <- mergeStatInfo(d)
#' p <- plotIbi_InWat(d, 'Johnson Creek')
#' p + ggtitle('Fish Index of Biotic Integrity - Generated Data for Example\n')
#' @import ggplot2
#' @export

plotIbi_InWat <- function(dfm, wat, ibiField='fish.ibi',
                          dateField= 'sample_end_time', onlySummer=FALSE) {

  tmp <- mergeStatInfo(dfm)
  tmp <- tmp[tmp$watershed == wat, ]
  tmp$season <- factor(quarters(tmp[[dateField]]))
  levels(tmp$season) = c('Winter', 'Spring', 'Summer', 'Fall')
  if (onlySummer==TRUE) tmp <- tmp[tmp$season=='Summer', ]

  # Order watershed levels by their mean result
  tmp$loc.lbl <- factor(tmp$loc.lbl,
                          levels=levels(reorder(tmp$loc.lbl, tmp[[ibiField]], mean)))


  # Sort data by mean of seasonal data
  # tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, result) )


  p <- ggplot(aes(loc.lbl, fish.ibi), data = tmp) +
    geom_point(aes(shape=season, color=season),
               size=5, position = position_jitter(width = .2, height=0)) +
    # geom_hline(yintercept=.85, colour='red', size=1.5) +
    # geom_hline(yintercept=.91, colour='darkgreen', size=1.5) +
        xlab('') +  ylab("\nFish Index of Biotic Integrity") + coord_flip() +
    theme_bw() + theme(axis.text = element_text(size=14),
          title = element_text(size = rel(1.3))) +
    scale_shape('Season') +
    scale_color_manual('Season', values = c('black', 'black', 'deepskyblue2', 'black'))

  return(p)

}
