#' Plot PAWMAP biological metrics within a selected watershed.
#'
#' @param dfm  The data frame containing the variable
#' @param vbl the field containing the metric to plot
#' @param wat the watershed to plot
#' @return A ggplot dot plot of the Macroinvertebrate Observed/Expected scores
#' within a watershed
#' @examples
#' library(ggplot2)
#' stations <- unique(stationInfo$loc_code[stationInfo$duration=='P'])
#' d <- data.frame(loc_code=unique(stations), metric_name='oep5',
#'                  watershed='Tryon Creek', result=rnorm(length(stations)))
#' d <- mergeStatInfo(d)
#' p <- plotBio_InWat(d, 'result', 'Johnson Creek')
#' p + ggtitle('Macroinvertebrate Observed/Expected - Generated Data for Example\n')
#' @import ggplot2
#' @export

plotBio_InWat <- function(dfm, vbl, wat, bio) {

  # tmp <- mergeStatInfo(dfm)
  tmp <- tmp[tmp$watershed == wat, ]

  # Sort data by mean of seasonal data
  tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, tmp[, vbl]))

  ## Set generic plot
  p <- ggplot(data = tmp, aes_string('loc.lbl', vbl)) + xlab('') +
    geom_point(size=4) + coord_flip() + theme_bw() +
    theme(axis.text = element_text(size=14),
          title = element_text(size = rel(1.3)))

  ## For Macroinvertebrate Observed/Expected
  if (bio=='bugs') {
    ttl <- paste0("Macroinvertebrate Observed/Expected Scores\nin ", wat, "\n")
    p <- p + geom_hline(y=0.91, color='darkgreen', size=1.2) +
      geom_hline(y=0.85, color='red', size=1.2) +
      ylab('\nObserved/Expected') +
      ggtitle(ttl)
  }

  ## For Bird Integrity Index
  if (bio=='birds') {
    ttl <- paste0('Bird Integrity Index in ', wat, '\n')
    p <- p + ggtitle(ttl) +
      ylab('\nBird Integrity Index Score')
  }

  ## For Fish Integrity Index
    if (bio=='fish') {
      ttl <- paste0('Fish Index of Biotic Integrity in ', wat, '\n')
      p <- p + geom_hline(y=75, color='darkgreen', size=1.2) +
      geom_hline(y=50, color='red', size=1.2) +
      ylab('\nFish Index of Biotic Integrity') +
      ggtitle(ttl)
  }


  return(p)

}
