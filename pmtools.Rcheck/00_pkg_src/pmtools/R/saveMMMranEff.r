#' Save and Plot the Random Effects from a Mixed Model.
#'
#' @param mods  A list of lme objects from a mixed model.
#' @param plot  Should the random effects be ranked and plotted?
#' @return A data frame of random effects, or a list with data frame and plots
#' @examples
#' mods <- runMetMixMod(wq14, 'metric_name')
#' saveMMMranEff(mods)  # a data frame
#' saveMMMranEff(mods, plot = TRUE)  # a list with data frame and plots
#' @import ggplot2
#' @export


saveMMMranEff <- function(mods, plot=FALSE) {

  # create data frames and add station info
  for (i in names(mods)) {

    # save data from list into data frame; add station info
    lmeDF <- data.frame(loc_code=row.names(nlme::ranef(mods[[i]])),
                              raneff=nlme::ranef(mods[[i]])[,1])
    lmeDF <- merge(lmeDF, stationInfo[, c('loc_code', 'watershed', 'subwat')])

    # create plot if set TRUE
    if (plot) {
      lmeDF$loc_code_srt <- with(lmeDF, paste(subwat, loc_code, sep='-'))
      lmeDF$loc_code_srt <- factor(lmeDF$loc_code_srt,
                                         levels = as.vector(lmeDF[order(lmeDF$raneff),]$loc_code_srt))
      levels(lmeDF$loc_code_srt) <- lmeDF[order(lmeDF$raneff),]$loc_code_srt

      # create title for plot
      ttl <- paste0('Urban Metal Index for ', pmtools:::.simpleCap(i), ' in Portland Streams')

      g <- ggplot(aes(loc_code_srt, raneff), data=lmeDF) +
        geom_bar(stat = "identity") + coord_flip() +
        theme_bw() + xlab('') + ylab('Urban Metal Index') +
        ggtitle(ttl)

      # save to list
      if (i == names(mods)[1]){
        raneffs <- list()
        raneffs$plots <- list()
      }
      raneffs$plots[[i]] <- g

    }

    # rename to add metal to raneff
    re <- paste0(i, '_raneff')
    lmeDF <- reshape::rename(lmeDF, c(raneff=re))

    if (i == names(mods)[1]){
      tmp.mrg <- lmeDF
    } else {
      tmp.mrg <- merge(tmp.mrg, lmeDF)
    }
  }
  if (plot) {
    raneffs$df <- tmp.mrg
    return(raneffs)
  } else {return(tmp.mrg)}
}
