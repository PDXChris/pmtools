compRespPred <- function(dat, vari, rsp, plg='', wide=FALSE) {
#   detach(package:reshape)
  library(reshape2)
  
  if (wide==TRUE) {
    dat <- melt(dat, id=c('loc_code'), variable.name='metric_code', value.name='result')
  }
# browser()
  tv <- deparse(substitute(vari))
  tmp <- dat[dat$metric_code==tv,]
  print(substitute(vari))
  if(tv %in% c('cu', 'cu_d', 'hg', 'pb', 'pb_d', 'tss', 'zn', 'zn_d')) {
    tmp <- ddply(tmp, .(loc_code, metric_code), here(summarize), vari = exp(mean(log(result))))
  } else {tmp <- ddply(tmp, .(loc_code, metric_code), summarize, vari=mean(result))}
  
  tmp <- rename(tmp, c(vari=tv))
  
  if(deparse(substitute(rsp))=='ibi') {
    tmp2 <- ddply(rsp, .(loc_code), summarise, ibi=mean(ibi))
    tmp <- merge(tmp, tmp2)
  } 
  
  if (deparse(substitute(rsp))=='oep5') {
    tmp <- merge(tmp, oep5[, c('loc_code', 'result')], by = 'loc_code')
    tmp <- rename(tmp, c(result='oep5'))
  }
  
  if (deparse(substitute(rsp))=='bii') {
    tmp <- merge(tmp, bii[, c('loc_code', 'bii_adjusted_scores')], by = 'loc_code')
    tmp <- rename(tmp, c(bii_adjusted_scores='bii'))
  }
#   browser()
  frm <- formula(paste(substitute(rsp), substitute(vari), sep="~"))
  if (plg == 'x' & min(tmp[, c(tv)])==0) {
    tmp[, c(tv)] <- tmp[, c(tv)] + .5*min(tmp[, c(tv)][tmp[, c(tv)]>0])
  }
  plot(frm, tmp, log=plg)
#   browser()
  if (plg == 'x') {
    frm <- formula(paste(substitute(rsp), ' ~ log(', substitute(vari), ')', sep=""))
  }
#   browser()
  print(summary(lm(frm, tmp)))
  tmp
#   assign('tmpo', tmp, ".GlobalEnv")
}
