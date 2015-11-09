compRespPred <- function(dat, var, rsp) {

tmp <- get(dat)[get(dat)$metric_code==var,]
# tmp <- melt(tmp, id=c('loc_code', 'watershed', 'subwat', 'season'), measure.vars=c('result'))
if(var %in% c('cu', 'cu_d', 'hg', 'pb', 'pb_d', 'tss', 'zn', 'zn_d')) {
  tmp <- ddply(tmp, .(loc_code, metric_code), summarize, get(var)=mean(log(result)))
} else {ddply(tmp, .(loc_code, metric_code), summarize, get(var)=mean(result))}

if(rsp=='ibi') {
  tmp2 <- ddply(get(rsp), .(loc_code), summarise, ibi=mean(ibi))
  tmp <- merge(tmp, tmp2)
} 

if (rsp=='oep5') {
  tmp <- merge(tmp, oep5[, c('loc_code', 'result')])
  tmp <- rename(tmp, c(result='oep5'))
}

if (rsp=='bii') {
  tmp <- merge(tmp, bii[, c('loc_code', 'bii_adjusted_scores')])
  tmp <- rename(tmp, c(bii_adjusted_scores='bii'))
}

plot(get(rsp) ~ get(var), tmp)
summary(lm(get(rsp) ~ get(var), tmp))
}
