#' Load and format PAWMAP substrate data
#'
#' @param dfm  The data frame containing the substrate data
#' @param load should the data be loaded from a file?
#' @param path path for data if load=TRUE
#' @return A data frame with substrate data in wide format
#' @export


formSubstrate <- function(dfm, load=FALSE, path=NULL) {

  # NEED TO FIX DFM UNDER load=F

  # Load and format data if load=TRUE
  if (load) {
    if (is.null(path)) path <- '../pmtoolsFiles/raw_data/priority2014.csv'
    dfm <- read.csv(path)
    dfm$loc_code <- substr(dfm$uid, 2,5)
    dfm <- reshape2::melt(dfm, id=c('uid','loc_code', 'year'), variable.name = 'metric_code')

    # Format data for substrate into wide format
    dfm <- dfm[dfm$metric_code %in% c('pct_ri', 'pct_fn', 'pct_gr', 'pct_gc',
                                      'pct_gf', 'pct_grav'), ]

  }

    dfm <- mergeStatInfo(dfm)
    dfm <- reshape2::dcast(dfm, loc_code + watershed + subwat + duration + panel +
                             loc.lbl ~ metric_code, value='value')


  # Eliminate the Slough station which denied permissions
  dfm <- dfm[dfm[, 'loc_code']!='1089', ]


  #  Create a combined gravel category
  if (!'pct_grav' %in% names(dfm)) {

    dfm$pct_grav <- dfm$pct_gc + dfm$pct_gf
    dfm$pct_grav <- ifelse(is.na(dfm$pct_grav), dfm$pct_gr, dfm$pct_grav)
  }
  dfm
}
