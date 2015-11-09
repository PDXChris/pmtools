importHabData <- function() {
  # Import and format data
  tmp <- read.csv('raw_data/priority2014.csv')
  tmp$loc_code <- substr(tmp$uid, 2,5)

  tmp <- melt(tmp, id=c('uid','loc_code'))
  tmp <- subset(tmp, variable %in% c("v1tm100", "xcl", "xcembed", "xcb_hall", "bankhard",
                                       'pct_fn', 'pct_gc', 'pct_gf', 'pct_gr', 'pct_fast',
                                       'x_hall', 'pct_ri', 'xslope'))

  # tmp <- mergeStatInfo(tmp)

  # tmp <- hab13
  tmp <- dcast(tmp, loc_code  ~ variable, value.var='value', mean)
  tmp$pct_grav <- tmp$pct_gc + tmp$pct_gf
  tmp$pct_grav <- ifelse(is.na(tmp$pct_grav), tmp$pct_gr, tmp$pct_grav)
  tmp <- tmp[ , !names(tmp) %in% c("pct_gc","pct_gf", "pct_gr")]
  tmp <- melt(tmp, id=c('loc_code'))


  tmp1 <- rbind(read.csv('./raw_data/ripveg.csv'), read.csv('./raw_data/x_hall.csv'))

  tmp1 <- tmp1 %>% select(-(X)) %>%
    filter(metric %in% c('xcl', 'x_hall') & uid != 'P1089')

  tmp1 <- dcast(tmp1, uid ~ metric, value.var='result')
  tmp1 <- tmp1 %>% mutate(loc_code = substr(uid, 2,5)) %>% select(-uid)
  tmp1 <- melt(tmp1, id=c('loc_code'))
  rbind(tmp, tmp1) %>% rename(metric_code = variable, result=value) %>% filter(loc_code != '1089')
}
