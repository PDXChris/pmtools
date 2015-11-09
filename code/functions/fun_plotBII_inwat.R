plotBII_inwat <- function(wat) {

library(ggplot2)
library(reshape)

# format data, merge w/ station info
tmp <- merge(stat.info3[,c('loc_code', 'watershed', 'loc.lbl', 'duration', 'panel')], bii)
tmp <- subset(tmp, watershed == wat)
tmp <- rename(tmp, c(bii_adjusted_scores = 'bii'))

# order locations by BII
tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, bii) )



p <- ggplot( ) + geom_point(aes(y=bii, x=loc.lbl), data = tmp, size=6) + 
  coord_flip() + theme_bw() + xlab('') + 
  ylab(paste('\nBird Integrity Index')) + 
  theme(axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), 
        axis.title.x=element_text(size=16)) 

print(p)

# png('./Graphs/NW_Streams/Biology/bii_ggdot_1-3.png', width=800, height=520)
# print(p)
# dev.off()
}