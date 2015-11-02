library(plyr)
library(pmtools)
library(nrsa)

poll <- 'tss'

# Subset data to single analyte, single panel, only needed fields
tmp <- wq14[wq14$metric_code == poll, c('loc_code', 'coll_date', 'metric_code', 'result',
                       'season', 'cens', 'units')]
tmp <- mergeStatInfo(tmp)
tmp <- nrsa:::allCharToFac(tmp)
tmp$area <- tmp$watershed
tmp$area[tmp$watershed=='Tryon Creek'] <- 'Willamette Streams'
tmp$area[tmp$watershed=='Fanno Creek'] <- 'Tualatin River'
tmp$area <- factor(tmp$area)


# Maxes and mins
range(tmp$result)
tmp[tmp$result==max(tmp$result), ]
tmp[tmp$result==min(tmp$result), ]

# Compare seasons
boxplot(result~season, data=tmp, log='y', main=.simpleCap(poll))
ddply(tmp, c('season'), summarise, med=median(result))
kruskal.test(result~as.factor(season), data=tmp)


# Compare Intermittent - Fall, Storm Winter season only
ddply(tmp[tmp$season %in% c('F', 'T', 'W'),], 'duration', summarise, med=median(result))
boxplot(result~duration, data=tmp[tmp$season %in% c('F', 'T', 'W'),], log='x',
        main='Intermittent vs Perennial\nFall, Storm Winter Samples Only',
        horizontal=T, xlab=poll)
wilcox.test(result~duration, data=tmp[tmp$season %in% c('F', 'T', 'W'),])


# Which season did max occur in at each station
temp <- merge(tmp[, colnames(tmp) %in% c("watershed", "loc.lbl", "result",
                                         "cens", "season")],
              ddply(tmp[tmp$cens!='<',], c('watershed', 'loc.lbl', 'cens'),
                    summarise, max=max(result)),
              by.x=c('loc.lbl', 'result', 'cens'),
              by.y=c('loc.lbl', 'max', 'cens'))
temp
table(temp$season)

# Watershed patterns
boxplot(result~area, data=tmp, log='y')
par(mar=c(5,8.5,2,2))
boxplot(result~watershed, data=tmp, horizontal=T, log='x', las=1,
        at=rev(1:nlevels(tmp$watershed)), xlab=poll)
dev.off()

#compare medians
temp <- ddply(tmp, 'watershed', summarise, med=median(result), num=length(result))
temp[order(temp$med),]

#compare geometric means
temp <- ddply(tmp, 'watershed', summarise, gm=exp(mean(log(result))), num=length(result))
temp[order(temp$gm),]

kruskal.test(result~area, data=tmp)

# Sort locations
tmp[order(tmp$result),colnames(tmp) %in% c('result', 'loc.label', 'duration',
                                           'watershed', 'season')]
#sort by gm or med
temp <- ddply(tmp, c('loc.label', 'watershed', 'duration'), summarise,
              med=median(result), gm=exp(mean(log(result))))
temp[order(temp$gm),]

