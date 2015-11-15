#' Plot PAWMAP Water Quality Data within a selected watershed.
#'
#' @param vbl  the name of the variable to plot
#' @param wat the watershed to plot
#' @param dfm  The data frame containing the variable
#' @param vName Name of the field storing vbl
#' @return A ggplot dot plot of the variable within a watershed
#' @examples
#' library(ggplot2)
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

plotWQ_InWat <- function(vbl, wat, dfm=wq14, vName='metric_name') {

  # Subset data to single analyte & watershed, using either metric_code or metric_name
  dfm <- dfm[dfm[, vName] == vbl, ]

  tmp <- mergeStatInfo(dfm)
  tmp <- tmp[tmp$watershed==wat, ]
  # tmp <- nrsa:::allCharToFac(tmp)

  # Create labels
  poll.lab <- met.cod$label[match(vbl, met.cod[, vName])]
  unt.lab <- as.character(unique(tmp$units))
  breaks <- as.vector(c(1, 2, 5) %o% 10^(-5:5))   #make scale non-scientific format

  # Summarize seasonal data.  Reshape data; add station info; format
  tmp <- tmp[tmp$season!='T', ]

  # Get mean and range by station.  If E. coli, use geometric mean.
  if (vbl == 'ecoli') {
    tmp <- ddply(tmp, .(loc_code, loc.lbl), summarise, smean=exp(mean(log(result))),
                       smin=min(result), smax=max(result))
  } else {
      tmp <- ddply(tmp, .(loc_code, loc.lbl), summarise, smean=mean(result),
                       smin=min(result), smax=max(result))
  }

  # Join back storm data
  tmp3 <- dfm[dfm[ , 'season']=='T', c('loc_code', 'result')]
  names(tmp3)[names(tmp3) == 'result'] <- 'storm'
  tmp <- merge(tmp, tmp3)

  # Sort data by mean of seasonal data
  tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, smean) )

  # Set up plot
  p <- ggplot(aes(smean, loc.lbl), data = tmp) +
    geom_segment(aes(y=loc.lbl, x=smin, yend=loc.lbl, xend=smax,
                     shape='range', linetype='range'), size=1.2) +
    geom_point(aes(x=smean, shape='smean', linetype='smean'), size=4) +
    geom_point(aes(x=storm, shape='storm', linetype='storm'), size=4) +
    ylab('') + theme_bw() + scale_x_log10(breaks = breaks, labels = breaks) +
    xlab(paste('\n', poll.lab, ' (', unt.lab, ')\n', sep="")) +
    theme(plot.margin = unit(c(.75, 0, 0, 0), "lines"),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 18, face='bold'),
          legend.text =element_text(size = 14),
          legend.key.size = unit(1.3, "cm"),
          legend.title = element_text(size=16, face = "bold", hjust=0)) +
    guides(shape = guide_legend(override.aes = list(shape = c(NA, 19, 17)))) +
    scale_linetype_manual(name = "Results", labels=c('Seasonal\nRange',
                                                     'Seasonal\nMean', 'Storm\nSample'), values=c(1,0,0)) +
    scale_shape_manual(name = "Results", labels=c('Seasonal\nRange',
                                                  'Seasonal\nMean', 'Storm\nSample'), values=c(19, 19, 17))
  # Add standard lines where available
  m.tmp <- as.character(met.cod$metric_code[match(vbl, met.cod[, vName])])
  if (m.tmp %in% std.lns$metric_code) {
    r.lin <- std.lns[match(m.tmp, std.lns$metric_code), ]$red.line
    if (m.tmp == 'do') {
      m.tmp <- data.frame(x=c(8,11), l=c('solid', 'dashed'))
      p <- p + geom_vline(xintercept=8, linetype='solid', color='red', size=1.5) +
        geom_vline(xintercept=11, linetype='dashed', color='red', size=1.5)
    } else {
      p <- p + geom_vline(xintercept=r.lin, color='red', size=1.5)
    }}

  if (m.tmp=='ecoli') {
    p <- p + geom_vline(xintercept=126, color='red', size=1.5, linetype=2)
  }

  return(p)
}
