library(reshape2)
library(dplyr)
# habitat data

# Import and format data
tmp1 <- read.csv('raw_data/priority2014.csv')
tmp1$loc_code <- substr(tmp1$uid, 2,5)

tmp1 <- melt(tmp1, id=c('uid','loc_code'))
tmp1 <- subset(tmp1, variable %in% c("v1tm100", "xcl", "xcembed", "xcb_hall", "bankhard",
                                     'pct_fn', 'pct_gc', 'pct_gf', 'pct_gr', 'pct_fast',
                                     'x_hall', 'pct_ri', 'xslope'))


tmp1 <- dcast(tmp1, loc_code  ~ variable, value.var='value', mean)
tmp1$pct_grav <- tmp1$pct_gc + tmp1$pct_gf
tmp1$pct_grav <- ifelse(is.na(tmp1$pct_grav), tmp1$pct_gr, tmp1$pct_grav)
tmp1 <- tmp1[ , !names(tmp1) %in% c("pct_gc","pct_gf", "pct_gr")]

tmp <- tmp1

tmp1 <- rbind(read.csv('./raw_data/ripveg.csv'), read.csv('./raw_data/x_hall.csv'))

tmp1 <- tmp1 %>% select(-(X)) %>%
  dplyr::rename(metric_code=metric, station=uid) %>%
  filter(metric_code %in% c('xcl', 'x_hall') & station != 'P1089')

tmp1 <- dcast(tmp1, station ~ metric_code, value.var='result')
tmp1 <- tmp1 %>% mutate(loc_code = substr(station, 2,5)) %>% select(-station)

tmp <- full_join(tmp, tmp1)
