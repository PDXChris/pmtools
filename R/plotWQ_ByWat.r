#' Plot PAWMAP water quality data by watershed.
#'
#' @param dfm  The data frame containing the variable
#' @param result  the name of the water quality variable to plot
#' @param analyte_field the field containing the name of the analyte
#' @param analyte_units the field containing the analyte measurement units
#' @param stationInfo should station information be added to the data?  If not, a field named 'watershed' is required.
#' @return A ggplot box plot of the variable by watershed
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' d <- data.frame(station=unique(stationInfo$station), metric_name='copper',
#'                 result=rlnorm(length(stationInfo$station)), storm='Seasonal',
#'                 analyte_units = 'ug/L')
#' d <- rbind(d, data.frame(station=unique(stationInfo$station), metric_name='copper',
#'                       result=2*rlnorm(length(stationInfo$station)), storm='Seasonal',
#'                 analyte_units = 'ug/L'))
#' p <- plotWQ_ByWat(d, analyte_field = 'metric_name')
#' p + ggtitle('Copper - Generated Data for Example\n')
#' @export


plotWQ_ByWat <- function(dfm, result = 'numeric_result', analyte_field='janus_analyte_name',
                         analyte_units='analyte_units', stationInfo=TRUE) {

  # merge w/ station info; add storm field
  if (stationInfo) dfm <- mergeStatInfo(dfm)

  # Format and order watershed factors for axis
  dfm$watershed <- gsub(' ', '\n', dfm$watershed)
  dfm$watershed <- factor(dfm$watershed,
                          levels=c("Fanno\nCreek", "Tualatin\nStreams",
                                   "Tryon\nCreek", "Willamette\nStreams",
                                   "Johnson\nCreek", "Columbia\nSlough"))

  # create labels and configure y-axis
  breaks <- as.vector(c(1, 2, 5) %o% 10^(-5:5))

  if (length(unique(dfm[[analyte_field]])) > 1) {
    stop("Multiple analytes are present; reconfigure data")
  }
  vbl <- unique(dfm[[analyte_field]])
  vlbl <- as.character(met.cod$label[match(vbl, met.cod[, 'metric_name'])])
  if (length(unique(dfm[[analyte_units]])) > 1) {
    stop("Multiple analyte units are present; reconfigure data")
  }
  ylb <- paste0(vlbl,' (', trimws(unique(dfm[[analyte_units]])), ')\n')


  p <- ggplot(data=dfm, aes_string('watershed', result)) +
    geom_boxplot(aes(fill=storm)) +
    xlab('') + theme_bw() + ylab(ylb) +
    scale_y_log10(breaks=breaks, expand=c(0, 0.1)) +
    scale_fill_manual(name='Sample\nType', labels=c('Seasonal', 'Storm'),
                      values=c("darkseagreen", "#0090b2")) +
    theme(text = element_text(size=14),
          axis.text.x = element_text(size=15)) +
    geom_vline(xintercept = c(2.5, 4.5), size=1)

  lbl <- data.frame(x=c(1.5, 3.5, 5.5), y=rep(1.4*max(dfm[[result]]), 3),
                    label=c('West Slope of\nWest Hills',
                            'East Slope of\nWest Hills', 'Eastside\nStreams'))
  p <- p + geom_text(data=lbl, aes(x=x, y=y, label=label),
                     size=(5), vjust=1)

  # provide standard lines where available
  m.tmp <- as.character(met.cod$metric_code[match(vbl, met.cod[['metric_name']])])
  if (m.tmp %in% std.lns$metric_code) {
    r.lin <- std.lns[match(m.tmp, std.lns$metric_code), ]$red.line
    if (m.tmp == 'do' | m.tmp == 'ecoli') {
      r.dash <- std.lns[match(m.tmp, std.lns$metric_code), ]$grn.line
      p <- p + geom_hline(yintercept=r.lin, linetype='solid', color='red', size=1.5) +
        geom_hline(yintercept=r.dash, linetype='dashed', color='red', size=1.5)
    } else {
      p <- p + geom_hline(yintercept=r.lin, color='red', size=1.5)
    }}

  return(p)
}
