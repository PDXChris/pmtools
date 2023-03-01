#' Query number of PAWMAP fish surveys and transects at each station.
#' @export

queryFishEvents <- function(){
  con <- BESdata:::dbConnect("WATERSHED")
  FishEvents <- RODBC::sqlQuery(con, "select * from V_RPT_FISH_EVENT;")
  FishEvents$sampleDate <- as.Date(FishEvents$collection_end)
  BESdata:::dbDisconnect(con)
  FishEvents
}

#' Summarize total fish surveys and transects
#' @import dplyr
#' @export

sumFishSurvTrans <- function(){

  # tally transects per date
  numTrans <- queryFishEvents() %>%
    group_by(site_identifier, collection_end, fished_name,
             fished_none_collected_name, comments) %>%
    summarise(num_trans = sum(c_across(subreach_ab_fished:subreach_jk_fished), na.rm=T))

  # sum # surveys at each site (excludes not fished) and total # transects
  numSurveys <- numTrans %>% ungroup() %>%
    group_by(site_identifier) %>%
    summarise(totNumTrans = sum(num_trans),
              num.survs = length(num_trans[num_trans > 0]))

  # plot & save histogram of # transects per survey
  h <- hist(numTrans$num_trans, plot=F)
  plot(h, ylim = c(0, max(h$counts)*1.2),
       xlab = '# Transects Sampled per Survey',
       main = 'PAWMAP Fish Surveys', labels=T)

  # save objects as list and return
  fishSurvTrans <- list(numSurveys=numSurveys, numTrans=numTrans, plot=h)
  fishSurvTrans
}
