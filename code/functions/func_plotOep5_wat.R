plotOep5_wat <- function() {

library(reshape)
library(ggplot2)
# tmp <- read.csv('./Bugs/panel3_rivpacs_results.csv')
# tmp$X <- sprintf("%04d", as.numeric(tmp$X))
# tmp <- rename(tmp, c(X='loc_code', OE='result'))
# tmp$metric_code <- 'oep5'
# tmp <- subset(tmp, select=c(loc_code, metric_code, result))
# tmp <- merge(tmp, stat.info3[,c('loc_code', 'watershed', 'subwat', 'loc.lbl',
#                                         'duration', 'panel')])
# tmp2 <- subset(alldf12, metric_code=='oep5')
# tmp2 <- subset(tmp2, select=-c(coll_date, panel, season, cens, units, mdl))
# tmp2 <- merge(tmp2, stat.info3[,c('loc_code', 'watershed', 'subwat', 'loc.lbl',
#                                 'duration', 'panel')])
# tmp <- rbind(tmp2, tmp)

tmp <- mergeStatInfo(oep5)

tmp$watershed <- factor(tmp$watershed,
                        levels=levels(with(tmp, reorder(watershed,result, mean))))


p <- ggplot( ) + geom_jitter(aes(y=result, x=watershed), data = tmp, size=7, alpha=0.5,
                             position = position_jitter(width = .1, height=0)) +

  coord_flip() + theme_bw() + xlab('') + scale_y_continuous(labels=function(x)x*100) +
  geom_point(aes(Group.1, x, colour='red'),
             data=aggregate(tmp$result, list(tmp$watershed), mean), size=15, shape='+') +
  scale_colour_manual(name='Median', values='red', labels='') +
  ylab(paste('\nObserved/Expected')) +
  ggtitle('Macroinvertebrate Observed/Expected Ratio\nin Portland Streams\n') +
  geom_text(aes(Group.1, x, label=round(x,2)),
            data=aggregate(tmp$result, list(tmp$watershed), mean),
            vjust=-1.5, colour='red', size=6) +
  theme(plot.title = element_text(size=16, face='bold'), axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title=element_text(size=16))

print(p)
}
