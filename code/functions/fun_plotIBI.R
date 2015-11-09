function() {
library(ggplot2)
library(reshape)

## WADEABLE SURVEYS
# Limit to summer months, or 
pm.ibi <- subset(fish13, (as.POSIXlt(fish13$ACTLDATE, format='%m/%d/%Y')$mon + 1) %in% 7:9)
# pm.ibi <- fish13

# Limit to complete surveys
pm.ibi <- merge(pm.ibi, fish.surv[, c('BATCHNO', 'NOTFISH', 'FISHED', 'FISH_COM')], by='BATCHNO')
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
pm.ibi.b <- merge(pm.ibi.b, fish.surv[, c('BATCHNO', 'NOTFISH', 'FISHED', 'FISH_COM')], by='BATCHNO')
pm.ibi.b <- subset(pm.ibi.b, grepl('10', pm.ibi.b$FISHED))
# pm.ibi.b <- pm.ibi.b[pm.ibi.b$ACTLDATE < '2012-07-01',]

pm.ibi.b <- merge(pm.ibi.b, stream.order)
pm.ibi.b <- rename(pm.ibi.b, c(comm_name='Common_Name'))

ibi.b <- ddply(pm.ibi.b, .(loc_code, ACTLDATE), calcFishIBI2)
ibi.b <- rename(ibi.b, c(V1='ibi'))

## ZEROS FOR NO FISH
ibi.z <- fish.surv[fish.surv$NOTFISH %in% c('NO FISH', 'OTHER') & grepl('10', fish.surv$FISHED),
                   c('BATCHNO', 'ACTLDATE')]
ibi.z <- merge(ibi.z, site.uid3, all.x=T)
ibi.z <- subset(ibi.z, (as.POSIXlt(ibi.z$ACTLDATE, format='%m/%d/%Y')$mon + 1) %in% 7:9)
ibi.z <- ibi.z[ibi.z$ACTLDATE < '2012-07-01',]
ibi.z <- subset(ibi.z, select=-c(SITE_ID, BATCHNO))
ibi.z$ibi <- 0
ibi <- rbind(ibi, ibi.b, ibi.z)

tmp <- mergeStatInfo(ibi)

tmp$watershed <- factor(tmp$watershed, 
                        levels=levels(with(tmp, reorder(watershed, ibi, median))))

p <- ggplot( ) + geom_jitter(aes(y=ibi, x=watershed), data = tmp, size=7, alpha=0.5, 
                             position = position_jitter(width = .2)) + 
  coord_flip() + theme_bw() + xlab('') + 
  geom_point(aes(Group.1, x, colour='red'), 
             data=aggregate(tmp$ibi, list(tmp$watershed), median), size=15, shape='+') + 
  scale_colour_manual(name='Median', values='red', labels='') + 
  ylab(paste('\nFish Index of Biological Integrity')) + 
  geom_text(aes(Group.1, x, label=round(x,0)), 
            data=aggregate(tmp$ibi, list(tmp$watershed), median), 
            vjust=-1.5, colour='red', size=6) + 
  theme(axis.text=element_text(size=14), axis.title.x=element_text(size=16)) 

print(p)

# jpeg('FishIBI_ggdot_X_Wtshd_pnl12.jpg', quality=100,
#      width=860, height=544)
# print(p)
# dev.off()
}
