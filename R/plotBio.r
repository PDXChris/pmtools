#' Plot A GGplot Jitter plot of a Biological Metric by Watershed
#'
#' @param dfm  A data frame with a variable to plot
#' @param vbl  The variable to plot by watershed
#' @return A ggplot graph with the variable on the horizontal axis and the
#' watersheds along the vertical axis sorted by mean of the variable
#' @import ggplot2
#' @export

plotBio <- function(dfm, vbl) {

  # Create a generic jitterplot by watershed
  p <- plotGGjitt_ByWat(dfm, vbl)

  # Add appropriate labels
  ## For Macroinvertebrate Observed/Expected
  if (deparse(substitute(dfm))=='oep5') {
    p <- p + geom_hline(y=0.91, color='darkgreen', size=1.2) +
      geom_hline(y=0.85, color='red', size=1.2) +
      ylab('\nObserved/Expected') +
      ggtitle("Macroinvertebrate Observed/Expected Scores\nin Portland Watersheds\n")
  }

  ## For Bird Integrity Index
  if (deparse(substitute(dfm))=='bii') {
    p <- p + ggtitle('Bird Integrity Index in Portland Watersheds\n') +
      ylab('\nBird Integrity Index Score')
  }
  return(p)
}
