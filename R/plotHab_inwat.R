#' Plot PAWMAP Habitat Data within a selected watershed.
#'
#' @param dfm  The data frame containing the habitat data
#' @param vbl  The field containing the numeric habitat values to plot
#' @param wat The watershed to plot
#' @param metric_code The function assumes the field name is the metric code.
#' If not, (e.g., habitat data are in a field named "result") supply the
#' metric code to allow conversions and benchmarks
#' @return A ggplot dot plot of the variable within a watershed
#' @examples
#' library(ggplot2)
#' d <- data.frame(site_identifier=unique(stationInfo$site_identifier), metric_code='xcl',
#'                  result=rnorm(length(stationInfo$site_identifier)))
#' d <- mergeStatInfo(d)
#' p <- plotHab_InWat(d, 'result', 'Willamette Streams')
#' p + ggtitle('Riparian Canopy - Generated Data for Example\n')
#' @export


plotHab_InWat <- function(dfm, vbl, wat, metric_code=NULL) {
  dfm <- dfm[dfm$watershed==wat, ]
  if (is.null(metric_code)) metric_code <- vbl

  # Create labels
  poll.lab <- as.character(met.cod$label[match(vbl, met.cod$metric_code)])

  # Sort data
  dfm <- transform(dfm, loc.lbl=reorder(loc.lbl, get(vbl)) )

  if (metric_code == 'xcl') dfm[[vbl]] <- dfm[[vbl]] * 100

  p <- ggplot(aes_string(vbl, 'loc.lbl'), data = dfm) +
    geom_point(size=4) +
    ylab('') + theme_bw() +
    xlab(paste0('\n', poll.lab)) +
    theme(
      axis.text.y = element_text(size = 14),
      axis.title  = element_text(size = 18, face='bold'),
      legend.text = element_text(size = 14),
      legend.key.size = unit(1.3, "cm"),
      legend.title = element_text(size=16, face = "bold", hjust=0))

  if (metric_code == "v1tm100") {
    p <- p + geom_vline(xintercept = 20, color='red', lwd=1.2) +
      geom_vline(xintercept = 30, color='darkgreen', lwd=1.2)
  }

  p

}
