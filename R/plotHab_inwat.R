#' Plot PAWMAP Habitat Data within a selected watershed.
#'
#' @param vbl  the name of the variable to plot
#' @param wat the watershed to plot
#' @param dfm  The data frame conatining the variable
#' @return A ggplot dot plot of the variable within a watershed
#' @examples
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_name='xcl',
#' result=rnorm(length(stationInfo$loc_code))
#' d <- mergeStatInfo(d)
#' p <- plotHab_inwat('xcl', d)
#' p + ggtitle('Riparian Canopy - Generated Data for Example\n')
#' @export


plotHab_inwat <- function(vbl, wat, dfm=hab14) {
  tmp <- dfm[dfm[, 'metric_code'] == vbl, ]
  tmp <- mergeStatInfo(tmp)
  tmp <- tmp[tmp$watershed==wat, ]

  tmp <- merge(tmp, met.cod, by='metric_code')

  # Create labels
  hbm.lab <- .simpleCap(vbl)

  # Sort data
  tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, result) )

  p <- ggplot(aes(result, loc.lbl), data = tmp) +
    geom_point(size=4) +
    ylab('') + theme_bw() + # scale_x_log10(breaks = breaks, labels = breaks) +
    xlab(paste0('\n', unique(tmp$label))) +
    theme(
      # plot.margin = unit(c(0, .75, 0, 0), "lines"),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 18, face='bold'),
          legend.text =element_text(size = 14),
          legend.key.size = unit(1.3, "cm"),
          legend.title = element_text(size=16, face = "bold", hjust=0))

  if (vbl=="v1tm100") {
    p <- p + geom_vline(x=20, color='red', lwd=1.2) + geom_vline(x=30, , color='darkgreen', lwd=1.2)
  }

  return(p)

}
