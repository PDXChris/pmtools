#' Plot PAWMAP habitat data by watershed.
#'
#' @param vbl  the name of the water quality variable to plot
#' @param dfm  The data frame containing the variable
#' @return A ggplot box plot of the variable by watershed
#' @examples
#' library(ggplot2)
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_code='xcl',
#'                  result=rnorm(length(stationInfo$loc_code)))
#' p <- plotHabByWshd('xcl', d)
#' p + ggtitle('Large Riparian Canopy - Generated Data for Example\n')
#' @export

plotHabByWshd <- function(vbl, dfm=hab14) {

  # Subset data to single variable, format
  dfm <- dfm[dfm[, 'metric_code'] == vbl, ]

  # Add more descriptive variable info - location, labels, title
  tmp <- mergeStatInfo(dfm)
  tmp <- merge(tmp, met.cod, by='metric_code')
  tmp$watershed <- gsub(' ', '\n', tmp$watershed)
  tmp$watershed <- factor(tmp$watershed,
                          levels=c("Fanno\nCreek", "Tualatin\nStreams",
                                   "Tryon\nCreek", "Willamette\nStreams",
                                   "Johnson\nCreek", "Columbia\nSlough"))

  ttl <- paste0(unique(tmp$short), ' in Portland Streams\n')

  p <- ggplot(aes(watershed, result), data=tmp) + geom_boxplot(fill='darkseagreen') + theme_bw() +
    xlab('') + ylab(paste0('\n', unique(tmp$label),'\n')) +
    scale_fill_discrete('Type', labels=c('Seasonal', 'Storm')) +
    ggtitle(ttl) + theme(plot.title = element_text(size=16, face='bold'),
                         axis.text.x  = element_text(size=12)) +
    geom_vline(x=c(2.5, 4.5), size=1) + scale_y_continuous(expand=c(0.1, 0))

  if (vbl=="v1tm100") {
    p <- p + geom_hline(y=30, color='darkgreen', size=1.2) +
      geom_hline(y=20, color='red', size=1.2)

  }

  lbl <- data.frame(x=c(1.5, 3.5, 5.5),
                    y=ifelse(vbl%in% c("bankhard", "xcl", 'xcembed'),
                             rep(1.2*max(tmp$result), 3),
                             ifelse(vbl %in% c('v1tm100', 'xcb_hall'),
                                    rep(0.95*max(tmp$result), 3),
                                    rep(max(tmp$result), 3))),
                    label=c('West Slope of\nWest Hills',
                            'East Slope of\nWest Hills', 'Eastside\nStreams'))

  p <- p + geom_text(aes(x=x, y=y, label=label, fill=NULL, face='bold'), data=lbl)

  return(p)
}
