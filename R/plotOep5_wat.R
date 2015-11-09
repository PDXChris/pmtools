#' Plot PAWMAP Macroinvertberate Scores by watershed.
#'
#' @param vbl  the name of the macroinvertebrate variable to plot
#' @param dfm  The data frame conatining the variable
#' @return A ggplot box plot of the variable by watershed
#' @examples
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_name='oe',
#' result=rnorm(length(stationInfo$loc_code))
#' d <- mergeStatInfo(d)
#' p <- plotOep5_wat('oe', d)
#' p + ggtitle('Macroinvertebrate Observed/Expected Ratio - Generated Data for Example\n')
#' @export

plotOep5_wat <- function(oe, dfm=oep5) {
  tmp <- dfm[dfm[, 'metric_code'] == oe, ]
  tmp <- mergeStatInfo(tmp)

  tmp$watershed <- factor(tmp$watershed,
                          levels=levels(with(tmp, reorder(watershed,result, mean))))
  tmp.mn <- aggregate(tmp$result, list(tmp$watershed), mean)

  p <- ggplot( ) + geom_jitter(aes(y=result, x=watershed), data = tmp, size=7, alpha=0.5,
                               position = position_jitter(width = .1, height=0)) +
    coord_flip() + theme_bw() + xlab('') + scale_y_continuous(labels=function(x)x*100) +
    geom_point(aes(Group.1, x, colour='red'),
               data=tmp.mn, size=15, shape='+') +
    scale_colour_manual(name='Mean', values='red', labels='') +
    ylab(paste('\nObserved/Expected')) +
    ggtitle('Macroinvertebrate Observed/Expected Ratio\nin Portland Streams\n') +
    geom_text(aes(Group.1, x, label=round(x,2)),
              data=tmp.mn,
              vjust=-1.5, colour='red', size=6) +
    theme(plot.title = element_text(size=16, face='bold'),
          axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
          axis.title=element_text(size=16))

  return(p)
}
