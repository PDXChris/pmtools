#' Plot A GGplot jitter plot of a biological metric by watershed
#'
#' @param df  A data frame with a variable to plot
#' @param col  The column containing the values to plot by watershed
#' @param metric The biological metric to plot: either 'macro.oe', 'bird.bii', or 'fish.ibi'
#' @param title Should a title be added to the graph?  Default is true.
#' @return A ggplot graph with the variable on the horizontal axis and the
#' watersheds along the vertical axis sorted by mean of the variable
#' @import ggplot2
#' @export

plotBio_ByWat <- function(df, col,
                          metric = c('macro.oe', 'bird.bii', 'fish.ibi'),
                          title = TRUE, ...) {

  metric <- match.arg(metric)

  # Create a generic jitterplot by watershed
  p <- plotGGjitt_ByWat(df, col, ...)

  # Add appropriate labels
  ## For Macroinvertebrate Observed/Expected
  if (metric == 'macro.oe') {
    p <- p + geom_hline(yintercept = 0.91, color = 'darkgreen', size = 1.2) +
      geom_hline(yintercept = 0.85, color = 'red', size = 1.2) +
      ylab('\nObserved/Expected')
  }

  ## For Bird Integrity Index
  if (metric == 'bird.bii') {
    p <- p + ylab('\nBird Integrity Index')
  }

  if (metric == 'fish.ibi') {
    p <- p + geom_hline(yintercept = 75, color = 'darkgreen', size = 1.2) +
      geom_hline(yintercept = 50, color = 'red', size = 1.2) +
      ylab('\nFish Index of Biotic Integrity')
  }

  if(title){
    ttl <- switch(metric,
                  macro.oe = "Macroinvertebrate Observed/Expected Scores\nin Portland Watersheds\n",
                  bird.bii = 'Bird Integrity Index in Portland Watersheds\n',
                  fish.ibi = "Fish Index of Biotic Integrity\nin Portland Watersheds\n")

    p <- p + ggtitle(ttl)
  }

  return(p)
}
