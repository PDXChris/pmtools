#' Plot A GGplot Jitter plot by Watershed
#'
#' @param dfm  A data frame with a variable to plot
#' @param vbl  The variable to plot by watershed
#' @param watershed the variable identifying the watershed
#' @param fill supply fill aesthetic as string for geom_jitter
#' @return A ggplot graph with the variable on the horizontal axis and the
#' watersheds along the vertical axis sorted by mean of the variable
#' @import ggplot2
#' @export


plotGGjitt_ByWat <- function(dfm, vbl, watershed='watershed', fill=NULL){

    # Order watershed levels by their mean result
  dfm[[watershed]] <- factor(dfm[[watershed]],
                    levels=levels(reorder(dfm[[watershed]], dfm[[vbl]], mean)))

  # Create data frame for plotting mean symbols
  tmp  <- aggregate(dfm[[vbl]], list(dfm[[watershed]]), mean)
  if (max(dfm[[vbl]], na.rm = TRUE) > 1) {
    tmp$x <- round(tmp$x, 1)
  } else {
    tmp$x <- as.numeric(formatC(tmp$x, 2))
  }

  p <- ggplot( ) + coord_flip() + theme_bw() + xlab('')

  # Jittered data points - allow extra aes arguments
  if(is.null(fill)) {
    p <- p + geom_jitter(data = dfm, aes_string(y=vbl, x=watershed),
                         size=7, alpha=0.5,
                         position = position_jitter(width = .1, height=0))
  } else {
    p <- p + geom_jitter(data = dfm, aes_string(y=vbl, x=watershed, fill=fill),
                         size=7, alpha=0.5, shape=21,
                         position = position_jitter(width = .1, height=0))

    }

  # Add mean pont & label; themes & legend
  p <- p + geom_point(data=tmp,
               aes(Group.1, x, colour='red'), size=15, shape='+') +
    geom_text(aes(Group.1, x, label=x),data=tmp,
              vjust=-1.5, colour='red', size=6) +
    scale_colour_manual(name='Mean', values='red', labels='') +
    theme(plot.title = element_text(size=16, face='bold'),
          axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
          axis.title=element_text(size=16))

  return(p)
}
