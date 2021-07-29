#' Plot PAWMAP Water Quality Data for each station within a selected watershed.
#'
#' @param indf  The data frame containing the variable to be plotted
#' @param wat The watershed for which station data will be plotted
#' @param analyte_field  The field containing the name of the analyte
#' @param result  The field storing the numeric value
#' @param analyte_units The field containing the measurement units
#' @param storm The field indicating whether it is a storm or seasonal sample
#' @param storm.value  The value indicating it is a storm sample
#' @param geo.mean  Should the geometric mean be used instead of the arithmetic?
#' @return A ggplot dot plot of the variable at each station within a watershed
#' @examples
#' library(ggplot2)
#' stations <- unique(stationInfo$station)
#' num_stations <- length(stations)
#' d <- data.frame(station=stations, janus_analyte_name='copper', watershed='Johnson Creek',
#'                 numeric_result=rlnorm(num_stations), storm_affected='No')
#' d <- rbind(d, data.frame(station=stations, janus_analyte_name='copper', watershed='Johnson Creek',
#'                  numeric_result=2*rlnorm(num_stations), storm_affected='Yes'))
#' d <- mergeStatInfo(d)
#' p <- plotWQ_byStat(d, 'Johnson Creek')
#' p + ggtitle('Copper - Generated Data for Example\n')
#' @importFrom plyr ddply
#' @import ggplot2
#' @export

plotWQ_byStat <- function(indf, wat, analyte_field='janus_analyte_name',
                          result='numeric_result', analyte_units='analyte_units',
                          storm='storm_affected', storm.value='Yes', geo.mean=FALSE) {

  # Subset data to single analyte & watershed, using either metric_code or metric_name
  indf <- indf[indf[['watershed']] == wat, ]
  analyte <- unique(indf[[analyte_field]])
  if (length(analyte) > 1) {
    stop(stop("Multiple analytes are present in data frame.  Reconfigure data."))
  }

  # get metric code for variable
  m.tmp <- as.character(met.cod$metric_code[match(analyte, met.cod[['metric_name']])])


  # Create labels
  poll.lab <- met.cod$label[match(analyte, met.cod[['metric_name']])]
  units <- as.character(unique(indf[[analyte_units]]))
  breaks <- as.vector(c(1, 2, 5) %o% 10^(-5:5))   #make scale non-scientific format

  # Summarize seasonal data.  Reshape data; add station info; format
  seas.df <- indf[indf[[storm]] != storm.value, ]

  # Get mean and range by station.  If E. coli, use geometric mean.
  if (geo.mean) {
    seas.sum <- ddply(seas.df, .(loc.lbl, cycle), function(x) {
      data.frame(smean=exp(mean(log(x[, result]), na.rm=T)),
                 smin=min(x[, result], na.rm=T),
                 smax=max(x[, result], na.rm=T))
    })
    leg.lbl <- c('Seasonal\nRange', 'Seasonal\nGeometric\nMean', 'Storm\nSample')
  } else {
    seas.sum <- ddply(seas.df, .(loc.lbl, cycle), function(x){
      data.frame(smean=mean(x[, result], na.rm=T),
                 smin=min(x[, result], na.rm=T),
                 smax=max(x[, result], na.rm=T))
    })
    leg.lbl <- c('Seasonal\nRange', 'Seasonal\nMean', 'Storm\nSample')
  }

  # Join back storm data
  storm.df <- indf[indf[[storm]]==storm.value, c('loc.lbl', 'cycle', result)]
  names(storm.df)[names(storm.df) == result] <- 'storm'
  outdf <- merge(seas.sum, storm.df)
  outdf$cycle <- factor(outdf$cycle)


  # Sort data by mean of seasonal data
  outdf <- transform(outdf, loc.lbl=reorder(loc.lbl, smean) )

  # Set up plot
  p <- suppressWarnings(ggplot(data = outdf, aes(smean, loc.lbl, color=cycle)) + #coord_flip() +
    geom_errorbarh(aes(y=loc.lbl, xmin=smin, xmax=smax,
                       shape='range', linetype='range'), size=1.2,
                   height = 0, position=position_dodgev(height=0.7)) +
    geom_point(aes(x=smean, shape='smean', linetype='smean'), size=4,
               position=position_dodgev(height=0.7)) +
    geom_point(aes(x=storm, shape='storm', linetype='storm'), size=4,
               position=position_dodgev(height=0.7)) +
    ylab('') + xlab(paste('\n', poll.lab, ' (', units, ')\n', sep="")) + theme_bw() +
    theme(plot.margin = unit(c(.75, 0, 0, 0), "lines"),
          axis.text.y = element_text(size = 14),
          axis.title  = element_text(size = 18, face='bold'),
          legend.text = element_text(size = 14),
          legend.key.size = unit(1.3, "cm"),
          legend.title = element_text(size=16, face = "bold", hjust=0)) +
    guides(shape = guide_legend(override.aes = list(shape = c(NA, 19, 17)))) +
    scale_linetype_manual(name = "Results", labels=leg.lbl, values=c(1,0,0)) +
    scale_shape_manual(name = "Results", labels=leg.lbl, values=c(19, 19, 17)))

  # log transform for all but DO
  if (analyte != 'dissolved oxygen') {
    p <- p + scale_x_log10(breaks = breaks, labels = breaks)
  }

  # Add standard lines where available
  if (m.tmp %in% std.lns$metric_code) {
    r.lin <- std.lns[match(m.tmp, std.lns$metric_code), ]$red.line
    if (m.tmp == 'do') {
      p <- p + geom_vline(xintercept=8, linetype='solid', color='red', size=1.5) +
        geom_vline(xintercept=11, linetype='dashed', color='red', size=1.5)
    } else {
      p <- p + geom_vline(xintercept=r.lin, color='red', size=1.5)
    }}

  if (m.tmp == 'ecoli') {
    p <- p + geom_vline(xintercept=126, color='red', size=1.5, linetype=2)
  }

  return(p)
}
