#' Plot PAWMAP habitat data by watershed.
#'
#' @param vbl  the field containing the variable to plot
#' @param dfm  The data frame containing the variable
#' @param add.wat Should a field designating watershed be added? (Based on station #)
#' @return A ggplot box plot of the variable by watershed
#' @examples
#' library(ggplot2)
#' ## NEED TO UPDATE
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_code='xcl',
#'                  result=rnorm(length(stationInfo$loc_code)))
#' p <- plotHab_ByWat(d, 'result')
#' p + ggtitle('Large Riparian Canopy - Generated Data for Example\n')
#' @export

plotHab_ByWat <- function(dfm, vbl, add.wat = TRUE) {

  # Add watershed if add.wat=T
  if (add.wat) dfm <- mergeStatInfo(dfm)

  # Create labels
  lbls <- met.cod[match(vbl, met.cod$metric_code), ]

  dfm$watershed <- gsub(' ', '\n', dfm$watershed)
  dfm$watershed <- factor(dfm$watershed,
                          levels=c("Fanno\nCreek", "Tualatin\nStreams",
                                   "Tryon\nCreek", "Willamette\nStreams",
                                   "Johnson\nCreek", "Columbia\nSlough"))

  ttl <- paste0(lbls$label, ' in Portland Streams\n')

  p <- ggplot(aes_string('watershed', vbl), data = dfm) +
    geom_boxplot(fill = 'darkseagreen') +
    scale_y_continuous(expand = c(0.1, 0)) +
    geom_vline(xintercept = c(2.5, 4.5), size = 1) +
    theme_bw() + xlab('') + ylab(paste0('\n', lbls$units,'\n')) + ggtitle(ttl) +
    theme(plot.title   = element_text(size = 16, face = 'bold', hjust = 0.5),
          axis.text.x  = element_text(size = 12))

  if (vbl=="v1tm100") {
    p <- p + geom_hline(yintercept = 30, color = 'darkgreen', size = 1.2) +
      geom_hline(yintercept = 20, color = 'red', size = 1.2)

  }

  lbl <- data.frame(x=c(1.5, 3.5, 5.5),
                    y=ifelse(vbl %in% c("bankhard", "xcl", 'xcembed'),
                             rep(1.2 * max(dfm[[vbl]]), 3),
                             ifelse(vbl %in% c('v1tm100', 'x_hall'),
                                    rep(0.93 * max(dfm[[vbl]]), 3),
                                    rep(max(dfm[[vbl]]), 3))),
                    label=c('West Slope of\nWest Hills',
                            'East Slope of\nWest Hills', 'Eastside\nStreams'))

  p <- p + geom_text(data = lbl, aes(x = x, y = y, label = label))

  return(p)
}
