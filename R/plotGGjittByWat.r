#' Plot A GGplot Jitter plot by Watershed
#'
#' @param dfm  A data frame with a variable to plot
#' @param x  The variable to plot by watershed
#' @param label  A string to label the variable axis
#' @return A ggplot graph with the variable on the horizontal axis and the
#' watersheds along the vertical axis sorted by mean of the variable
#' @examples
#' dfm <- data.frame(loc_code=stationInfo$loc_code,
#'                    vari=rnorm(length(stationInfo$loc_code)))
#' p <- plotGGjittByWat(dfm, vari, "\nA generated variable")
#' p
#' #  The graph object can be modified
#' p + ggtitle('An Example Graph\n')
#' @export


plotGGjittByWat <- function(dfm, x, label){

  library(ggplot2)
  library(pmtools)
  library(plyr)

  dfm <- mergeStatInfo(dfm, fields='watershed')
  z <- substitute(x)

  # Order watershed levels by their mean result
  dfm$watershed <- factor(dfm$watershed,
                    levels=levels(with(dfm, reorder(watershed, eval(z), mean))))
  # Create data frame for plotting mean symbols
  tmp  <- with(dfm, aggregate(eval(z), list(watershed), mean))

  p <- ggplot( ) + coord_flip() + theme_bw() + xlab('') +
    geom_jitter(data = dfm, aes_string(y=deparse(substitute(x)), x='watershed'), size=7, alpha=0.5,
                position = position_jitter(width = .1, height=0)) +
    geom_point(data=tmp,
               aes(Group.1, x, colour='red'), size=15, shape='+') +
    scale_colour_manual(name='Mean', values='red', labels='') +
    ylab(label) +
    geom_text(aes(Group.1, x, label=round(x,2)),
              data=tmp,
              vjust=-1.5, colour='red', size=6) +
    theme(plot.title = element_text(size=16, face='bold'),
          axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
          axis.title=element_text(size=16))

  return(p)
}