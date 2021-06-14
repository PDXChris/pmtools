#' Get Fish Index of Biotic Integrity scores
#' @param dfm data frame of fish survey results
#' @param onlySummer Only use summer surveys (per protocol) or all seasons?
#'
#' @return A data frame with fish ibi scores and subscores
#' @export


runFishIbi <- function(dfm, onlySummer=FALSE){

  # calculate IBI  ##BATCHNO NEEDS TO BE REPLACED
  tmp <- with(dfm, nrsa::calculateFishIBI(BATCHNO, Order_100K, comm_name, LENGTH/10,
                                          ANOMALY==1, return.subindices = T))
  tmp <- tmp[complete.cases(tmp), ]

  ## ADD ZEROS FOR NO FISH
  tmp.z <- dfm[dfm$fished_none_collected == 'Yes',]
  tmp.z$fish.ibi <- 0
  tmp.z$ACTLDATE <- as.Date(tmp.z$ACTLDATE)
  # Add to IBIs
  tmp <- plyr::rbind.fill(tmp, tmp.z)

  # Limit to summer months
  tmp$season <- factor(lubridate::quarter(tmp$ACTLDATE))
  levels(tmp$season) = c('Winter', 'Spring', 'Summer', 'Fall')
  if (onlySummer==TRUE) tmp <- tmp[tmp$season=='Summer', ]

  return(tmp)
}

#' Remove fish surveys that were not attempted
#'
#' @param df data frame of fish surveys
#' @param field name of field indicating whether survey was attempted
#'
#' @return A data frame with non-surveys removed
#' @export

removeNotFished <- function(df, field = 'fished'){
  df <- df[!grepl('not fished', df[['fished']], ignore.case = TRUE), ]
  df
}


#' Choose the fields to keep in fish survey data
#'
#' @param df data frame from fish database query
#' @param fields a vector of field names to select
#'
#' @import dplyr
#' @export

selectFishFields <-
  function(df, fields = c('site_identifier', 'collection_start_date',
                          'fished', 'fished_none_collected', 'common_name',
                          'fish_length_mm')){

  df %>% select(any_of(fields)) %>%
    mutate(common_name = tolower(common_name),
           sampleDate = as.Date(collection_start_date))
}
