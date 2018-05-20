#' Plot PAWMAP Water Quality Data within a selected watershed.
#'
#' @param analyte  the name of the analyte to plot
#' @param wat the watershed to plot
#' @param dfm  The data frame containing the variable
#' @param analyte_field the field containing the name of the analyte
#' @param result  the field storing the numeric value
#' @param analyte_units the field containing the measurement units
#' @return A ggplot dot plot of the variable within a watershed
#' @examples
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_name='copper',
#'                 result=rlnorm(length(stationInfo$loc_code)), season='S')
#' d <- rbind(d, data.frame(loc_code=unique(stationInfo$loc_code), metric_name='copper',
#'                  result=2*rlnorm(length(stationInfo$loc_code)), season='T'))
#' d <- mergeStatInfo(d)
#' p <- plotWQ_InWat('copper', 'Johnson Creek', d)
#' p + ggtitle('Copper - Generated Data for Example\n')
#' @import plyr
#' @import ggplot2
#' @export

plotWQ_InWat <- function(analyte, wat, dfm, analyte_field='janus_analyte_name',
                         result='numeric_result', analyte_units='analyte_units') {
  # source('R/dodge_v.r')

  # Subset data to single analyte & watershed, using either metric_code or metric_name
  dfm <- dfm[dfm[[analyte_field]] == analyte, ]

  tmp <- mergeStatInfo(dfm)
  tmp <- tmp[tmp$watershed==wat, ]

  # get metric code for variable, whether code or name is used
  m.tmp <- as.character(met.cod$metric_code[match(analyte, met.cod[['metric_name']])])


  # Create labels
  poll.lab <- met.cod$label[match(analyte, met.cod[['metric_name']])]
  if (length(unique(dfm[[analyte_field]])) > 1) {
    stop("Multiple analytes are present; reconfigure data")
  }

  if (length(unique(dfm[[analyte_units]])) > 1) {
    stop("Multiple analyte units are present; reconfigure data")
  }
  units <- as.character(unique(tmp[[analyte_units]]))
  breaks <- as.vector(c(1, 2, 5) %o% 10^(-5:5))   #make scale non-scientific format

  # Summarize seasonal data.  Reshape data; add station info; format
  tmp <- tmp[tmp[['storm']] == 'Seasonal', ]

  # Get mean and range by station.  If E. coli, use geometric mean.
  if (m.tmp == 'ecoli') {
    tmp <- ddply(tmp, .(loc_code, loc.lbl, cycle), here(summarise), smean=exp(mean(log(get(result)))),
                       smin=min(get(result)), smax=max(get(result)))
  } else {
      tmp <- ddply(tmp, .(loc_code, loc.lbl, cycle), here(summarise), smean=mean(get(result)),
                       smin=min(get(result)), smax=max(get(result)))
  }

  # Join back storm data
  tmp3 <- dfm[dfm[['storm']]=='Storm', c('loc_code', 'cycle', result)]
  names(tmp3)[names(tmp3) == result] <- 'storm'
  tmp <- merge(tmp, tmp3)
  tmp$cycle <- factor(tmp$cycle)
  # tmp$storm <- factor(tmp$storm)


  # Sort data by mean of seasonal data
  tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, smean) )

  # Set up plot
  p <- ggplot(data = tmp, aes(smean, loc.lbl, color=cycle)) + #coord_flip() +
    geom_errorbarh(aes(y=loc.lbl, xmin=smin, xmax=smax,
                     shape='range', linetype='range'), size=1.2,
                   height = 0, position=position_dodgev(height=0.7)) +
    geom_point(aes(x=smean, shape='smean', linetype='smean'), size=4,
               position=position_dodgev(height=0.7)) +
    geom_point(aes(x=storm, shape='storm', linetype='storm'), size=4,
               position=position_dodgev(height=0.7)) +
    ylab('') + theme_bw() + scale_x_log10(breaks = breaks, labels = breaks) +
    xlab(paste('\n', poll.lab, ' (', units, ')\n', sep="")) +
    theme(plot.margin = unit(c(.75, 0, 0, 0), "lines"),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 18, face='bold'),
          legend.text =element_text(size = 14),
          legend.key.size = unit(1.3, "cm"),
          legend.title = element_text(size=16, face = "bold", hjust=0)) +
    guides(shape = guide_legend(override.aes = list(shape = c(NA, 19, 17)))) +
    scale_linetype_manual(name = "Results",
                          labels=c('Seasonal\nRange', 'Seasonal\nMean', 'Storm\nSample'),
                          values=c(1,0,0)) +
    scale_shape_manual(name = "Results",
                       labels=c('Seasonal\nRange', 'Seasonal\nMean', 'Storm\nSample'),
                       values=c(19, 19, 17))
  # Add standard lines where available
  if (m.tmp %in% std.lns$met.cod) {
    r.lin <- std.lns[match(m.tmp, std.lns$met.cod), ]$red.line
    if (m.tmp == 'do') {
      m.tmp <- data.frame(x=c(8,11), l=c('solid', 'dashed'))
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
