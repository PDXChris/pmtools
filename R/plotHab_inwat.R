#' Plot PAWMAP Habitat Data within a selected watershed.
#'
#' @param vbl  the field containing the variable to plot
#' @param wat the watershed to plot
#' @param dfm  The data frame containing the variable
#' @param metric_code The function assumes the field name is the metric code.  If not,
#' supply the metric code to allow conversions and benchmarks
#' @return A ggplot dot plot of the variable within a watershed
#' @examples
#' library(ggplot2)
#' ## NEEDS TO BE UPDATED
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_code='xcl',
#'                  watershed='Willamette Streams', result=rnorm(length(stationInfo$loc_code)))
#' d <- mergeStatInfo(d)
#' p <- plotHab_InWat('xcl', 'Willamette Streams', d)
#' p + ggtitle('Riparian Canopy - Generated Data for Example\n')
#' @export


plotHab_InWat <- function(vbl, wat, dfm, metric_code=NULL) {
  tmp <- mergeStatInfo(tmp)
  tmp <- tmp[tmp$watershed==wat, ]
  if (is.null(metric_code)) metric_code <- vbl

  # Create labels
  poll.lab <- as.character(met.cod$label[match(vbl, met.cod$metric_code)])

  # Sort data
  tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, get(vbl)) )

  if (metric_code == 'xcl') dfm[[vbl]] <- dfm[[vbl]] * 100

  p <- ggplot(aes_string(vbl, 'loc.lbl'), data = tmp) +
    geom_point(size=4) +
    ylab('') + theme_bw() +
    xlab(paste0('\n', poll.lab)) +
    theme(
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 18, face='bold'),
      legend.text =element_text(size = 14),
      legend.key.size = unit(1.3, "cm"),
      legend.title = element_text(size=16, face = "bold", hjust=0))

  if (metric_code == "v1tm100") {
    p <- p + geom_vline(xintercept = 20, color='red', lwd=1.2) +
      geom_vline(xintercept = 30, , color='darkgreen', lwd=1.2)
  }

  p

}
