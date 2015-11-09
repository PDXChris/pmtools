compRespPred <- function(dat, var, rsp) {

tmp <- dat[dat$metric_code==substitute(var),]
# t <- substitute(var)
print(substitute(var))
if(deparse(substitute(var)) %in% c('cu', 'cu_d', 'hg', 'pb', 'pb_d', 'tss', 'zn', 'zn_d')) {
  tmp <- ddply(tmp, .(loc_code, metric_code), here(summarize), var = exp(mean(log(result))))
} else {ddply(tmp, .(loc_code, metric_code), summarize, var=mean(result))}

tmp <- rename(tmp, c(var=deparse(substitute(var))))

if(deparse(substitute(rsp))=='ibi') {
  tmp2 <- ddply(rsp, .(loc_code), summarise, ibi=mean(ibi))
  tmp <- merge(tmp, tmp2)
} 

if (deparse(substitute(rsp))=='oep5') {
  tmp <- merge(tmp, oep5[, c('loc_code', 'result')])
  tmp <- rename(tmp, c(result='oep5'))
}

if (deparse(substitute(rsp))=='bii') {
  tmp <- merge(tmp, bii[, c('loc_code', 'bii_adjusted_scores')])
  tmp <- rename(tmp, c(bii_adjusted_scores='bii'))
}

plot(substitute(rsp) ~ substitute(var), tmp)
summary(lm(rsp ~ var, tmp))
}
