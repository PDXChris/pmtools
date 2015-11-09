plotOep5_inwat <- function(wat) {

library(reshape)
library(ggplot2)

# tmp <- read.csv('./Bugs/panel3_rivpacs_results.csv')
# tmp$X <- sprintf("%04d", as.numeric(tmp$X))
# tmp <- rename(tmp, c(X='loc_code', OE='result'))
# tmp$metric_code <- 'oep5'
# tmp <- subset(tmp, select=c(loc_code, metric_code, result))
# tmp <- merge(tmp, stat.info3[,c('loc_code', 'watershed', 'subwat', 'loc.lbl',
#                                         'panel')])
# tmp2 <- subset(alldf12, metric_code=='oep5')
# tmp2 <- subset(tmp2, select=-c(coll_date, panel, season, cens, units, mdl))
# tmp2 <- merge(tmp2, stat.info3[,c('loc_code', 'watershed', 'subwat', 'loc.lbl',
#                                 'panel')])
# tmp <- rbind(tmp2, tmp)

tmp <- mergeStatInfo(oep5)
tmp <- tmp[tmp$watershed == wat, ]

# Sort data by mean of seasonal data
tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, result) )


p <- ggplot(aes(loc.lbl, result), data = tmp) + 
  geom_point(size=4) + 
  geom_hline(yintercept=.85, colour='red', size=1.5) + 
  xlab('') +  ylab("\nObserved/Expected") + coord_flip() + theme_bw() + 
#   ggtitle('Macroinvertebrate Observed/Expected Ratio\nin Northwest Streams\n') + 
  theme(axis.text = element_text(size=14), 
        title = element_text(size = rel(1.3)))


print(p)

# png(paste0(getwd(), '/Graphs/NW_Streams/NW_Streams_oep5_ggseg_pnl13.png'), width=800, height=400)
# print(p)
# dev.off()
}