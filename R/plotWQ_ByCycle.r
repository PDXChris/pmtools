#' Plot PAWMAP water quality data by watershed.
#'
#' @param dfm  The data frame containing the variable
#' @param analyte_field the field containing the name of the water quality analyte
#' @param result  the field containing the numeric value
#' @param analyte_units the field containing the analyte measurement units
#' @param storm the field indicating seasonal or storm samples
#' @param stationInfo should station information be added to the data?  If not, a field named 'watershed' is required.
#' @return A ggplot box plot of the variable plotted by sampling cycle
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' stations <- unique(stationInfo$station)
#' num_stations <- length(stations)
#' d <- data.frame(station=stations, janus_analyte_name='copper',
#'                 numeric_result=rlnorm(num_stations), storm='Seasonal',
#'                 analyte_units = 'ug/L',
#'                 cycle=replicate(num_stations, sample(c(1,2), 1)))
#' d <- rbind(d, data.frame(station=stations, janus_analyte_name='copper',
#'               numeric_result=2*rlnorm(num_stations), storm='Storm',
#'               analyte_units = 'ug/L',
#'               cycle=replicate(num_stations, sample(c(1,2), 1))))
#'
#' p <- plotWQ_byCycle(d)
#' p + ggtitle('Copper - Generated Data for Example\n')
#' @export


plotWQ_byCycle <- function(dfm, result = 'numeric_result', analyte_field='janus_analyte_name',
                         analyte_units='analyte_units', storm = 'storm',
                         stationInfo=TRUE) {

  # merge w/ station info; add storm field
  if (stationInfo) dfm <- mergeStatInfo(dfm)

  # Format and order watershed factors for axis
  dfm$watershed <- gsub(' ', '\n', dfm$watershed)
  dfm$watershed <- factor(dfm$watershed,
                          levels=c("Fanno\nCreek", "Tualatin\nStreams",
                                   "Tryon\nCreek", "Willamette\nStreams",
                                   "Johnson\nCreek", "Columbia\nSlough"))
  if(any(is.na(unique(dfm[['watershed']])))) {
    warning(paste0('Stations without watershed matches in dataset.  The following
            stations are filtered:',
                   paste(unique(dfm[["station"]][is.na(dfm[['watershed']])]),
                         sep=', ')))
    dfm <- dfm[!is.na(dfm[['watershed']]), ]
  }

  dfm[[storm]] <- factor(dfm[[storm]])
  dfm[['cycle']] <- factor(dfm[['cycle']])

  # create labels and configure y-axis
  breaks <- as.vector(c(1, 2, 5) %o% 10^(-5:5))

  if (length(unique(dfm[[analyte_field]])) > 1) {
    stop("Multiple analytes are present.  Reconfigure data.")
  }
  vbl <- unique(dfm[[analyte_field]])
  vlbl <- as.character(met.cod$label[match(vbl, met.cod[, 'metric_name'])])
  if (length(unique(dfm[[analyte_units]])) > 1) {
    warning("Multiple analyte units are present. Check data.")
  }
  ylb <- paste0('\n', vlbl,' (', trimws(unique(dfm[[analyte_units]])), ')\n')

  # order watershed levels by median of seasonal samples
  tmp <- dfm[dfm[[storm]]=='Seasonal', ]
  tmp <- aggregate(tmp[[result]], list(tmp[['watershed']]),
                       FUN=median, na.rm=TRUE)
  watSort <- tmp$x
  names(watSort) <- tmp$Group.1
  watSort <- sort(watSort)
  # browser()
  dfm[['watershed']] <- factor(dfm[['watershed']], levels = names(watSort))


  p <- ggplot(dfm, aes_string('watershed', result)) +
    geom_boxplot(aes(fill=cycle)) + coord_flip() +
    facet_grid(reformulate('.', storm)) +
    xlab('') + ylab(ylb)  +
    scale_y_log10(breaks=breaks, expand=c(0, 0.1))


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
