plotIBI_inwat <- function(wat) {
library(reshape)
library(ggplot2)
library(scales)

library(ggplot2)
library(reshape)

## WADEABLE SURVEYS
# Limit to summer months, or 
pm.ibi <- subset(fish13, (as.POSIXlt(fish13$ACTLDATE, format='%m/%d/%Y')$mon + 1) %in% 7:9)
# pm.ibi <- fish13

# Limit to complete surveys
pm.ibi <- merge(pm.ibi, fish.surv[, c('loc_code', 'BATCHNO', 'NOTFISH', 'FISHED', 'FISH_COM')])
pm.ibi <- subset(pm.ibi, grepl('10', pm.ibi$FISHED))
# pm.ibi <-pm.ibi[pm.ibi$ACTLDATE < '2012-07-01',]

# stream.order <- read.csv('PM_1_4_StrmOrder.csv')
# stream.order <- rename(stream.order, c(id='loc_code'))
# stream.order$loc_code <- sprintf("%04d", stream.order$loc_code)
pm.ibi <- merge(pm.ibi, stream.order)
pm.ibi <- rename(pm.ibi, c(comm_name='Common_Name'))

ibi <- ddply(pm.ibi, .(loc_code, ACTLDATE), calcFishIBI2)
ibi <-rename(ibi, c(V1='ibi'))

#### BOATABLE SURVEYS
# Limit to summer months, or 
pm.ibi.b <- subset(fish13.bw, (as.POSIXlt(fish13.bw$ACTLDATE, format='%m/%d/%Y')$mon + 1) %in% 7:9)
# pm.ibi.b <- fish13

# Limit to complete surveys
pm.ibi.b <- merge(pm.ibi.b, fish.surv[, c('loc_code', 'BATCHNO', 'NOTFISH', 'FISHED', 'FISH_COM')])
pm.ibi.b <- subset(pm.ibi.b, grepl('10', pm.ibi.b$FISHED))
# pm.ibi.b <- pm.ibi.b[pm.ibi.b$ACTLDATE < '2012-07-01',]

pm.ibi.b <- merge(pm.ibi.b, stream.order)
pm.ibi.b <- rename(pm.ibi.b, c(comm_name='Common_Name'))

ibi.b <- ddply(pm.ibi.b, .(loc_code, ACTLDATE), calcFishIBI2)
ibi.b <- rename(ibi.b, c(V1='ibi'))

## ZEROS FOR NO FISH
ibi.z <- fish.surv[fish.surv$NOTFISH %in% c('NO FISH', 'OTHER') & grepl('10', fish.surv$FISHED),
                   c('loc_code', 'ACTLDATE')]
ibi.z <- subset(ibi.z, (as.POSIXlt(ibi.z$ACTLDATE, format='%m/%d/%Y')$mon + 1) %in% 7:9)
# ibi.z <- ibi.z[ibi.z$ACTLDATE < '2012-07-01',]
ibi.z$ibi <- 0
ibi <- rbind(ibi, ibi.b, ibi.z)

tmp <- merge(ibi, stat.info3[,c('loc_code', 'watershed', 'subwat', 'panel', 'duration')])

tmp <- ibi
tmp <- mergeStatInfo(tmp)
tmp <- subset(tmp, watershed == wat)

tmp$fmon <- as.POSIXlt(tmp$ACTLDATE)$mon
tmp$fmon <- ifelse(tmp$fmon < 7, tmp$fmon + 12, tmp$fmon)
tmp$loc.lbl <- factor(tmp$loc.lbl)
tmp$loc.lbl <- factor(tmp$loc.lbl, 
                      levels=levels(with(tmp, reorder(loc.lbl, ibi, median))))


ann_text <- data.frame(mon=rep(7,3), x = c(0.3, 1, 0.3), y=c(73, 57, 48), 
                       text = c('Acceptable', 'Marginally\nImpaired', 'Severely Impaired'),
                       angle = c(90, 0, 90))

p <- ggplot( ) + geom_point(aes(y=ibi, x=loc.lbl), data = tmp, size=7) + 
  coord_flip() + theme_bw() + xlab('') + 
  geom_hline(yintercept=75, colour='darkgreen', size=1.5) +
  geom_hline(yintercept=50, colour='red', size=1.5) + 
  geom_text(data=ann_text, aes(x=x, y=y, label=text, angle = angle), hjust=0, fontface='bold') + 
  ylab(paste('\nFish Index of Biological Integrity')) + 
  theme(axis.text=element_text(size=14), axis.title.x=element_text(size=16))

# p <- ggplot() + geom_point(data=tmp, aes(fmon, ibi), size=4)  + theme_bw() +
#   geom_hline(yintercept=75, colour='darkgreen', size=1.5) +
#   geom_hline(yintercept=50, colour='red', size=1.5) + 
#   geom_text(data=ann_text, aes(x=mon, y=y, label=text), hjust=0, fontface='bold') + 
#   facet_wrap(~ loc.lbl) + 
#   scale_x_continuous(limits=c(7, 18), breaks=seq(7, 18, 3), 
#                      labels=c("Jul", "Oct", "Jan", "Apr")) + 
#   ylim(0, 100) + 
#   xlab('') + ylab('Fish Index of Biotic Integrity\n') + 
#   theme(strip.text.x = element_text(size=12, face='bold'))

print(p)
}