#' Plot PAWMAP Fish Data within a selected watershed.
#'
#' @param dfm  The data frame containing the variable
#' @param wat the watershed to plot
#' @param ibiField string supplying the name of the IBI field
#' @param dateField string supplying the name of the date field
#' @param onlySummer Should the IBI be based only on summer surveys (as per protocol),
#' or use all surveys?
#' @return A ggplot dot plot of Fish Index of Biotic Integrity scores
#' within a watershed
#' @examples
#' library(ggplot2)
#' stations <- unique(stationInfo$site_identifier[stationInfo$duration=='P'])
#' random.dates <- sample(seq(as.Date('2010/07/01'),
#'                        as.Date('2020/07/01'), by="day"), length(stations))
#' d <- data.frame(site_identifier=unique(stations), metric_name='oep5',
#'                  collection_start_date=random.dates,
#'                  fish.ibi=rnorm(length(stations)))
#' d <- mergeStatInfo(d)
#' p <- plotIbi_InWat(d, 'Johnson Creek')
#' p + ggtitle('Fish Index of Biotic Integrity - Generated Data for Example\n')
#' @import ggplot2
#' @export

plotIbi_InWat <- function(dfm, wat, ibiField='fish.ibi',
                          dateField='collection_start_date', onlySummer=FALSE) {

  dfm <- dfm[dfm$watershed == wat, ]
  dfm$season <- factor(quarters(dfm[[dateField]]))
  levels(dfm$season) = c('Winter', 'Spring', 'Summer', 'Fall')
  if (onlySummer==TRUE) dfm <- dfm[dfm$season=='Summer', ]

  # Order watershed levels by their mean result
  dfm$loc.lbl <- factor(dfm$loc.lbl,
                          levels=levels(reorder(dfm$loc.lbl, dfm[[ibiField]], mean)))


  # Sort data by mean of seasonal data
  # dfm <- transform(dfm, loc.lbl=reorder(loc.lbl, result) )


  p <- ggplot(aes(loc.lbl, fish.ibi), data = dfm) +
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
