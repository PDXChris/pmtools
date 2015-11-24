#' Compile total exceedances of substrate criteria by watershed
#'
#' @param dfm  The data frame containing the variable
#' @return A table of number of exceedances of each substrate criterion
#' @export

substCritStat <- function(dfm = NULL){

  if(is.null(dfm)) {dfm <- formSubstrate(, T)}

  tmp <- plyr::ddply(dfm, 'watershed', function(x){
    c(N = nrow(x))
  }
  )

  dfm <- dfm[dfm[['pct_ri']] >= 50, ]

  tmp2 <- plyr::ddply(dfm, 'watershed', function(x){
    c(gt50ri      = sum(x[['pct_ri']] >= 50),
      fnGt25      = sum(x[['pct_fn']] >= 25),
      fnGt12_Lt25 = sum(x[['pct_fn']] >= 12 & x[['pct_fn']] < 25),
      grLt15      = sum(x[['pct_grav']] <= 15),
      grLt35_Gt15 = sum(x[['pct_grav']] > 15 & x[['pct_grav']] < 35))
  }
  )
  tmp <- merge(tmp, tmp2)
  tmp
}


#' List stations exceeding substrate criteria
#'
#' @param dfm  The data frame containing the variable
#' @return A list of stations exceeding each substrate criterion
#' @export

substCritList <- function(dfm = NULL, wat=NULL){

  if(is.null(dfm)) {dfm <- formSubstrate(, T)}

  if(!is.null(wat)){
    numStations <- nrow(dfm[dfm[['watershed']]==wat, ])
  }

    # Subset to sites with > 50% riffles; drop unneeded variables
    dfm <- dfm[dfm[['pct_ri']] >= 50,
             !names(dfm) %in% c('subwat', 'loc_code','pct_gr')]
    dfm[, grepl('pct', names(dfm))] <- round(dfm[, grepl('pct', names(dfm))], 1)
    dfm <- dfm[c('watershed', 'duration', 'panel', 'loc.lbl', 'pct_ri',
                 'pct_fn', 'pct_grav', 'pct_gf', 'pct_gc')]

  # List sites exceeding each criterion
    lst <- list(numStations           = numStations,
                over_50pct_riffles    = dfm[dfm[['pct_ri']] >= 50, ],
                fines_over25          = dfm[dfm[['pct_fn']] >= 25, ],
                fines_over12_under25  = dfm[dfm[['pct_fn']] >= 12 & dfm[['pct_fn']] < 25, ],
                gravel_under15        = dfm[dfm[['pct_grav']] <= 15, ],
                gravel_under35_over15 = dfm[dfm[['pct_grav']] > 15 & dfm[['pct_grav']] < 35, ]
    )
  # names(lst)[1] <- 'test_this'

  # Subset to watershed if watershed specified
  if(!is.null(wat)){
    lst[2:length(lst)] <- lapply(lst[2:length(lst)], function(x) x[-1][x[['watershed']]==wat, ])
  }

  lst
}
