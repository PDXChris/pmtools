#' Tally PAWMAP Fish Species Composition Data.
#'
#' @param dfm The data frame containing fish counts
#' @param speciesIn The field in dfm that identifies species
#' @param sppLook Look up species traits by: "Common_Name" (default)
#' or "Species_Name"
#' @param by Are the data formatted one row per individual ('row'), or counts per
#' species ('counts')?
#' @param countFields Only used if by='count'. A vector of length 2 indicating
#' the fields for total number of each species captured first, and the number of
#' surveys on which they were captured second.
#' @param station The field identifying the sampling station
#' @param dateField The field identifying the sampling date
#' @return A table of fish species presence (number of surveys on which
#' species was captured), and abundance (total number of individuals captured)
#' @export

tallyFishSpp <- function(dfm, speciesIn='comm_name', sppLook='Common_Name',
                         by='row', countFields=c('totNum', 'numSurv'),
                         station='site_identifier', dateField='collection_start_date') {

  dfm[[speciesIn]] <- tolower(dfm[[speciesIn]])

  # Exclude unwanted species
  dfm <- dfm[!dfm[[speciesIn]] %in% c('dicamptodon\n', 'cyprinidae juvenile'), ]

  ##  Format data for plotting
  if (by=='row') {
    # dfm1 = total count for each spp.
    dfm1 <- plyr::ddply(dfm, speciesIn, function(x) num=nrow(x))
    dfm1 <- plyr::rename(dfm1, c(V1='num'))

    # dfm2 = number of surveys w/ detects
    dfm2 <- plyr::ddply(dfm, speciesIn,
                        function(x) nrow(unique(x[, c(station, dateField)])))

    dfm <- merge(dfm1, dfm2)
    dfm <- plyr::rename(dfm, c(V1='freq'))
  }
  # Warn about unmatched species
  if(any(!unique(dfm[[speciesIn]]) %in% fish.habits3[[sppLook]])) {
    warning(paste('The following species are not in the trait database:\n',
                  toString(setdiff(dfm[[speciesIn]], fish.habits3[[sppLook]]))))
  }

  if (by=='count') {
    names(dfm)[names(dfm)==countFields[1]] <- 'num'
    names(dfm)[names(dfm)==countFields[2]] <- 'freq'
  }


  # Merge species information
  dfm <- merge(dfm, fish.habits3[, c('Family', 'Species_Name', 'Common_Name', 'Origin')],
               by.x=speciesIn, by.y=sppLook, all.x=TRUE)

  # create variable for legend and bar coloring
  dfm$bar[dfm$Origin=='A'] <- 'Non-Native'
  dfm$bar[dfm$Origin=='N' & dfm$Family!='Salmonidae'] <- 'Native'
  dfm$bar[dfm$Origin=='N' & dfm$Family=='Salmonidae'] <- 'Salmonid'
  dfm$bar[dfm[[speciesIn]]=='starry flounder'] <- 'Native'
  dfm$bar[is.na(dfm$bar)] <- 'None Captured'
  dfm$bar <- factor(dfm$bar, levels=c('Native', 'Salmonid', 'Non-Native', 'None Captured'))

  # add number of fishless surveys
  dfm$num[is.na(dfm[[speciesIn]])] <- 0

  # format common name for axis labeling
  dfm[[speciesIn]] <- sapply(dfm[[speciesIn]],
                             function(x) paste(toupper(substr(x, 1, 1)),
                                               substr(x, 2, nchar(as.character(x))), sep=""))

  # Order for plotting most frequent at top
  dfm <- dfm[order(dfm$freq, dfm$num),]

  # order factor by freq
  dfm[[speciesIn]] <- factor(dfm[[speciesIn]], dfm[[speciesIn]][order(dfm$freq)])

  return(dfm)
}
