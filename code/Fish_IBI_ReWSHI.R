library(ggplot2)
# library(reshape)
library(plyr)
library(nrsa)
library(lubridate)
library(pmtools)
library(dplyr)

load('./raw_data/bio14.rda')

tmp <- rbind(fish13, data.frame(fish13.bw, Order_100K=2))
tmp <- merge(tmp, fsh13.srv[, c('BATCHNO', 'NOTFISH', 'FISHED', 'FISH_COM')], by='BATCHNO', all.x=T)
tmp <- subset(tmp, grepl('10', tmp$FISHED))

tmp0 <- unique(tmp[,c('BATCHNO', 'loc_code', 'ACTLDATE')])

tmp <- with(tmp, calculateFishIBI(BATCHNO, Order_100K, comm_name, LENGTH/10,
                                  ANOMALY==1, return.subindices = T))
tmp <- tmp[complete.cases(tmp), ]
tmp <- merge(tmp0, tmp, by.x = 'BATCHNO', by.y = 'site', all.y=T)

# tmp <- mergeStatInfo(tmp)

## ZEROS FOR NO FISH
tmp.z <- fsh13.srv[fsh13.srv$NOTFISH %in% c('NO FISH', 'OTHER') & grepl('10', fsh13.srv$FISHED),
          c('loc_code', 'ACTLDATE')]
# ibi.z <- ibi.z[ibi.z$ACTLDATE < '2012-07-01',]
tmp.z$fish.ibi <- 0
tmp.z$ACTLDATE <- as.Date(tmp.z$ACTLDATE)

tmp <- rbind.fill(tmp, tmp.z)

# Limit to summer months
# tmp <- subset(tmp, (as.POSIXlt(ACTLDATE, format='%Y-%m-%d')$mon + 1) %in% 7:9)

tmp <- rbind.fill(tmp, data.frame(loc_code=c('0526', '0633'),
                             ACTLDATE=as.Date(c('2011-07-07', '2011-08-11')), fish.ibi=c(0,0)))

tmp <- mergeStatInfo(tmp)
tmp$season <- factor(quarter(tmp$ACTLDATE))
levels(tmp$season) = c('Winter', 'Spring', 'Summer', 'Fall')


ddply(tmp, .(watershed, season), summarize, mnibi=mean(fish.ibi), num=length(fish.ibi))

tmp$watershed <- factor(tmp$watershed,
                        levels=levels(with(tmp, reorder(watershed, fish.ibi, mean))))

# write.csv(ddply(tmp, .(watershed), summarize, mnibi=mean(fish.ibi), num=length(fish.ibi)),
          # './WSHI/fish_ibi_WSHI_2015_03.csv', row.names=FALSE)

p <- ggplot( ) + geom_jitter(aes(y=fish.ibi, x=watershed), data = tmp, size=7, alpha=0.5,
                             position = position_jitter(width = .2)) +
  coord_flip() + theme_bw() + xlab('') +
  geom_point(aes(Group.1, x, colour='red'),
             data=aggregate(tmp$fish.ibi, list(tmp$watershed), mean), size=15, shape='+') +
  scale_colour_manual(name='Mean', values='red', labels='') +
  ylab(paste('\nFish Index of Biological Integrity')) +
  geom_text(aes(Group.1, x, label=round(x,0)),
            data=aggregate(tmp$fish.ibi, list(tmp$watershed), mean),
            vjust=-1.5, colour='red', size=6) +
  theme(axis.text=element_text(size=14), axis.title.x=element_text(size=16))

print(p)

# png('./Graphs/FishIBI_ggdot_X_Wtshd_pnl14.jpg', width=860, height=544)
# print(p)
# dev.off()

boxplot(fish.ibi ~ season, tmp)
boxplot(fish.ibi ~ panel, tmp)

tb <- tmp %>% distinct(watershed, loc_code, ACTLDATE) %>%
  select(watershed, loc_code, panel, season, ACTLDATE)
table(tb$panel, tb$season)
