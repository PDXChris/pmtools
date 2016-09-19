#' Plot PAWMAP Habitat Data within a selected watershed.
#'
#' @param vbl  the name of the variable to plot
#' @param wat the watershed to plot
#' @param dfm  The data frame conatining the variable
#' @return A ggplot dot plot of the variable within a watershed
#' @examples
#' library(ggplot2)
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_code='xcl',
#'                  watershed='Willamette Streams', result=rnorm(length(stationInfo$loc_code)))
#' d <- mergeStatInfo(d)
#' p <- plotHab_InWat('xcl', 'Willamette Streams', d)
#' p + ggtitle('Riparian Canopy - Generated Data for Example\n')
#' @export


plotHab_InWat <- function(vbl, wat, dfm=hab14) {
  tmp <- dfm[dfm[, 'metric_code'] == vbl, ]
  tmp <- mergeStatInfo(tmp)
  tmp <- tmp[tmp$watershed==wat, ]

  # Create labels
  poll.lab <- met.cod$label[match(vbl, met.cod$metric_code)]

  # Sort data
  tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, result) )

  if (vbl =='xcl') dfm[['result']] <- dfm[['result']] * 100

  p <- ggplot(aes(result, loc.lbl), data = tmp) +
    geom_point(size=4) +
    ylab('') + theme_bw() + # scale_x_log10(breaks = breaks, labels = breaks) +
    xlab(paste0('\n', poll.lab)) +
    theme(
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 18, face='bold'),
      legend.text =element_text(size = 14),
      legend.key.size = unit(1.3, "cm"),
      legend.title = element_text(size=16, face = "bold", hjust=0))

  if (vbl=="v1tm100") {
    p <- p + geom_vline(x=20, color='red', lwd=1.2) +
      geom_vline(x=30, , color='darkgreen', lwd=1.2)
  }

  p

}
