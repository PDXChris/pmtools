#' Plot PAWMAP water quality data by watershed.
#'
#' @param vbl  the name of the water quality variable to plot
#' @param dfm  The data frame containing the variable
#' @param vName Name of the field storing vbl
#' @return A ggplot box plot of the variable by watershed
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_name='copper',
#'                 result=rlnorm(length(stationInfo$loc_code)), season='S')
#' d <- rbind(d, data.frame(loc_code=unique(stationInfo$loc_code), metric_name='copper',
#'                       result=2*rlnorm(length(stationInfo$loc_code)), season='T'))
#' d <- mergeStatInfo(d)
#' p <- plotWQ_ByWat('copper', d)
#' p + ggtitle('Copper - Generated Data for Example\n')
#' @export


plotWQ_ByWat <- function(vbl, dfm=wq14, vName='metric_name') {

  # Subset data to single analyte & watershed, using either metric_code or metric_name
  dfm <- dfm[dfm[, vName] == vbl, ]

  # merge w/ station info; add storm field
  dfm <- mergeStatInfo(dfm)
  dfm$storm <- ifelse(dfm$season=='T', TRUE, FALSE)

  # Format and order watershed factors for axis
  dfm$watershed <- gsub(' ', '\n', dfm$watershed)
  dfm$watershed <- factor(dfm$watershed,
                          levels=c("Fanno\nCreek", "Tualatin\nStreams",
                                   "Tryon\nCreek", "Willamette\nStreams",
                                   "Johnson\nCreek", "Columbia\nSlough"))
  breaks <- as.vector(c(1, 2, 5) %o% 10^(-5:5))

  vlbl <- met.cod$label[match(vbl, met.cod[, vName])]
  trim.trailing <- function (x) sub("\\s+$", "", x)
  ylb <- paste0(vlbl,' (', trim.trailing(unique(dfm$units)), ')\n')


  p <- ggplot(aes(watershed, result, fill=storm), data=dfm) + geom_boxplot() +
    xlab('') + theme_bw() + ylab(ylb) +
    scale_y_log10(breaks=breaks, expand=c(0, 0.1)) +
    scale_fill_manual(name='Type', labels=c('Seasonal', 'Storm'),
                      values=c("darkseagreen", "#0090b2")) +
    theme(plot.title = element_text(size=16, face='bold'),
                          axis.text.x  = element_text(size=15)) +
    geom_vline(xintercept = c(2.5, 4.5), size=1)

  lbl <- data.frame(x=c(1.5, 3.5, 5.5), y=rep(1.4*max(dfm$result), 3),
                    label=c('West Slope of\nWest Hills',
                            'East Slope of\nWest Hills', 'Eastside\nStreams'))
  p <- p + geom_text(aes(x=x, y=y, label=label, fill=NULL, face='bold'),
                     size=(6), vjust=1, data=lbl)

  # provide standard lines where available
  m.tmp <- as.character(met.cod$metric_code[match(vbl, met.cod[, vName])])
  if (m.tmp %in% std.lns$metric_code) {
    r.lin <- std.lns[match(m.tmp, std.lns$metric_code), ]$red.line
    if (m.tmp == 'do') {
      m.tmp <- data.frame(y=c(8,11), l=c('solid', 'dashed'))
      p <- p + geom_hline(yintercept=8, linetype='solid', color='red', size=1.5) +
        geom_hline(yintercept=11, linetype='dashed', color='red', size=1.5)
    } else {
      p <- p + geom_hline(yintercept=r.lin, color='red', size=1.5)
    }}

  if (m.tmp=='ecoli') {
    p <- p + geom_hline(yintercept=126, color='red', size=1.5, linetype=2)
  }

  return(p)
}
