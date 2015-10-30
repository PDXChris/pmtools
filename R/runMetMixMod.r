#' Run a Mixed Model for Metals and TSS in Streams.
#'
#' @param dat  A data frame with metals and TSS data.
#' @param vrbl  A string of the name of the field storing variable names
#' @return A list of lme objects for metals
#' @examples
#' runMetMixMod(wq14, 'metric_name')
#' @export


runMetMixMod <- function(dat, vrbl) {
  # test if vrbl is a valid variable
  if (!vrbl %in% names(dat)) stop(print(paste0('The variable ', vrbl, ' is not in the data frame')))

  for (i in c('copper', 'lead', 'mercury', 'zinc')) {

    # query metal & TSS, rename and merge, format and add station info
    tmp <- dat[dat[, vrbl] == i, ]
    tmp2 <- dat[dat[, 'metric_name'] %in% met.cod$metric_name[met.cod$metric_code=='tss'], ]
    tmp2 <- reshape::rename(tmp2, c(result='tss', cens='tss.cens'))
    tmp2 <- tmp2[, c('loc_code',  'tss', 'tss.cens', 'coll_date', 'season')]
    tmp <- tmp[, c('loc_code', 'metric_name', 'cens', 'result', 'coll_date', 'season')]
    tmp <- merge(tmp2, tmp, by=c("loc_code", "coll_date", "season" ))

    # Add station info
    tmp$loc_code <- sprintf("%04d", as.numeric(tmp$loc_code))
    tmp <- merge(stationInfo[, c('loc_code', 'watershed', 'subwat',
                                 'duration', 'panel')], tmp)

    tmp$metric_name <- factor(tmp$metric_name)
    tmp$panel <- factor(tmp$panel)
    tmp$season <- factor(tmp$season, levels=c('U', 'F', 'T', 'W', 'P'))
    tmp$storm <- factor(ifelse(tmp$season=='T', 'storm', 'seasonal'))

    # Run mixed model; save results to df
    if (i=='copper') mods <- list()
    mods[[i]] <- nlme::lme(log(result) ~ storm + log(tss), data=tmp, random=~1|loc_code)

  }
  #return a list w/ models of the four metals
  return(mods)
}
