#' Get Fish Index of Biotic Integrity scores
#'
#' @return A data frame with fish ibi scores and subscores
#' @export


runFishIbi <- function(onlySummer=FALSE){

  load('../pmtoolsFiles/raw_data/bio14.rda')

  # Add boatable data
  tmp <- rbind(fish13, data.frame(fish13.bw, Order_100K=2))
  # remove salamanders
  tmp <- tmp[!grepl('dicampt', tmp$comm_name), ]

  # Limit to complete surveys
  tmp <- merge(tmp, fsh13.srv[, c('BATCHNO', 'NOTFISH', 'FISHED', 'FISH_COM')],
               by='BATCHNO', all.x=T)
  tmp <- subset(tmp, grepl('10', tmp$FISHED))
  # tmp0 = list of full surveys
  tmp0 <- unique(tmp[,c('BATCHNO', 'loc_code', 'ACTLDATE')])

  # calculate IBI
  tmp <- with(tmp, nrsa::calculateFishIBI(BATCHNO, Order_100K, comm_name, LENGTH/10,
                                          ANOMALY==1, return.subindices = T))
  tmp <- tmp[complete.cases(tmp), ]
  tmp <- merge(tmp0, tmp, by.x = 'BATCHNO', by.y = 'site', all.y=T)

  ## ADD ZEROS FOR NO FISH
  tmp.z <- fsh13.srv[fsh13.srv$NOTFISH %in% c('NO FISH', 'OTHER') &
                       grepl('10', fsh13.srv$FISHED),
                     c('loc_code', 'ACTLDATE')]
  tmp.z$fish.ibi <- 0
  tmp.z$ACTLDATE <- as.Date(tmp.z$ACTLDATE)
  # Add to IBIs
  tmp <- plyr::rbind.fill(tmp, tmp.z)

  ## These stations got Dicamptodon only - no fish.  Add zeros for them.
  tmp <- plyr::rbind.fill(tmp, data.frame(BATCHNO=c(137, 150, 431),
                                          loc_code=c('0526', '0633', '2185'),
                                          ACTLDATE=as.Date(c('2011-07-07',
                                                    '2011-08-11', '2013-07-09')),
                                          fish.ibi=c(0,0,0)))
  # Limit to summer months
  tmp$season <- factor(lubridate::quarter(tmp$ACTLDATE))
  levels(tmp$season) = c('Winter', 'Spring', 'Summer', 'Fall')
  if (onlySummer==TRUE) tmp <- tmp[tmp$season=='Summer', ]

  return(tmp)
}
