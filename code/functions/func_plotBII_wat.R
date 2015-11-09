plotBII_wat <- function() {
library(ggplot2)
library(reshape)

# read in data, format, merge w/ station info
# bii <- read.csv('./raw_data/PM_BII_1_3.csv')
# bii$loc_code <- sprintf("%04d", bii$loc_code)
tmp <- merge(stat.info3[,c('loc_code', 'watershed', 'loc.lbl', 'duration', 'panel')], bii)
tmp <- rename(tmp, c(bii_adjusted_scores = 'bii'))

# order watersheds by median BII
tmp$watershed <- factor(tmp$watershed, 
                        levels=levels(with(tmp, reorder(watershed, bii, median))))


p <- ggplot( ) + geom_boxplot(aes(y=bii, x=watershed), data = tmp) + 
  coord_flip() + theme_bw() + xlab('') + 
  ylab(paste('\nBird Integrity Index')) + 
  geom_text(aes(Group.1, x, label=round(x,0)), 
            data=aggregate(tmp$bii, list(tmp$watershed), median), 
            hjust=1.3, colour='red', size=6) + 
  theme(axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), 
        axis.title.x=element_text(size=16)) 

print(p)
}