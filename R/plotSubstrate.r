#' Plot PAWMAP substrate data
#'
#' @param substrate plot "fines" or "gravel"?
#' @param dfm  The data frame containing the variable
#' @param load should the data be loaded from a file?
#' @param path path for data if load=TRUE
#' @return A ggplot dot plot of the substrate data plotted vs. percent riffles,
#' with points above relevant criteria highlighted and labeled
#' @examples
#' num <- length(stationInfo$loc_code)
#' dfm <- data.frame(loc_code=stationInfo$loc_code,
#'                    pct_ri=runif(num, 0, 100),
#'                    pct_fn=runif(num, 0, 60),
#'                    pct_grav=runif(num, 0, 60))
#' dfm <- mergeStatInfo(dfm)
#' plotSubstrate('fines', dfm)
#' plotSubstrate('fines', dfm)
#' @import ggplot2
#' @export


plotSubstrate <- function(substrate, dfm=NULL, load=TRUE, path=NULL) {
  ## SHOULD ADD ABILITY TO SPECIFY FIELDS W/ DEFAULTS

  if (is.null(dfm)) dfm <- formSubstrate(dfm = dfm, load = load, path = path)

  if (substrate == 'fines'){
    colPts <- dfm[dfm[['pct_ri']]>=50 & dfm[['pct_fn']] > 25, ]
    y <- 'pct_fn'

  } else {
    colPts <- dfm[dfm[['pct_ri']]>=50 & dfm[['pct_grav']] < 35, ]
    colPtsGr <- dfm[dfm[['pct_ri']]>=50 & dfm[['pct_grav']] < 15, ]
    y <- 'pct_grav'}

  p <- ggplot(aes_string(x='pct_ri', y=y), data=dfm) +
    geom_point(size=5) + theme_bw() +
    xlab('\nPercent Riffle Habitat')

  if (substrate == 'fines'){
    # Plot riffles vs. fines to look for sediment issues
    p <- p + ylab('Percent Fine Substrate') +
      geom_hline(yintercept=25, color='red', lwd=1.2) +
      geom_vline(xintercept=50, , color='red', lwd=1.2)

    p <- p +
      geom_point(aes(color=watershed), size=5, data=colPts) +
      geom_text(aes(label=paste0(subwat,'  '), hjust=1, color=watershed),
                data=colPts)
  }

  if (substrate == 'gravel'){
  # Plot gravel vs. riffle to look for gravel supply/retention issues
  p <- p + ylab('Percent Gravel Substrate\n') +
    geom_hline(yintercept=15, color='red', lwd=1.2) +
    geom_vline(xintercept=50, , color='red', lwd=1.2) +
    geom_hline(yintercept=35, color='darkgreen', lwd=1.2)

  p <- p +
    geom_point(aes(color=watershed), size=5, data=colPts) +
    geom_text(aes(label=paste0(subwat,'  '), hjust=1, color=watershed),
              data=colPtsGr)
  }


  p
}
