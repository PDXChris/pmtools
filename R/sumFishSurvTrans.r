#' Query number of PAWMAP fish surveys and transects.
#'
#' @export

queryFullFishEvent <- function(){
  con <- BESdata:::dbConnect("WATERSHED")
  event <- RODBC::sqlQuery(con, "select * from fish_event;")
  sites <- RODBC::sqlQuery(con, "select * from site;")
  fullFishEvent <- merge(sites, event, by = 'site_id')
  fullFishEvent$sampleDate <- as.Date(fullFishEvent$collection_end)
  BESdata:::dbDisconnect(con)
  fullFishEvent
}

#' Summarize total fish surveys and transects
#' @export

sumFishSurvTrans <- function(){

  # tally transects per date; plot counts
  numTrans <- queryFullFishEvent() %>% group_by(site_identifier, collection_end) %>%
    summarise(num_trans= sum(c_across(subreach_ab_fished:subreach_jk_fished), na.rm=T))

  h <- hist(numTrans$num_trans, plot=F)

  plot(h, ylim = c(0, max(h$counts)*1.2), xlab = '# Transects Sampled per Survey',
       main = 'PAWMAP Fish Surveys', labels=T)

  numSurv <- numTrans %>% summarise(num_survs = n(), totNumTrans = sum(num_trans))

  fishSurvTrans <- list(numSurv=numSurv, numTrans=numTrans, plot=h)
  fishSurvTrans
}
