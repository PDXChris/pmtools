#' Plot Empirical Cumulative Distribution Plots of PAWMAP data by watershed.
#'
#' @param vbl  the name of the water quality variable to plot
#' @param dfm  The data frame containing the variable
#' @param season Should the seasonal or storm data be plotted?
#' @return A ggplot ECDF plot of the variable by watershed
#' @export

plotCDF_byWat <- function(vbl, dfm, season='seasonal', scale='linear') {

    # Choose variable & set labels
  ttl <- met.cod$label[match(vbl, met.cod$metric_code)]

  dfm <- data.frame(dfm)
  dfm <- mergeStatInfo(dfm[dfm[, 'metric_code'] == vbl, ])

  # Select appropriate data; complete title
  if (season=='seasonal') {
    dfm <- dfm[dfm[, 'storm'] == 'seasonal', ]
    sbttl <- 'During Seasonal Sampling\n'
  }

  if (season=='storm') {
    dfm <- dfm[dfm[, 'storm'] == 'storm', ]
    sbttl <- 'During Storm Sampling\n'
  }


  p <- ggplot(data=dfm,
         aes(result, color=watershed)) + stat_ecdf(size=2) + coord_flip() + theme_bw() +
    # xlab(xlb) +
    ggtitle(paste0(ttl, ' in Portland Watersheds\n', sbttl))

  if (scale=='log') p <- p + scale_x_log10()

  return(p)

}
