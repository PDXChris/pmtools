#' Plot PAWMAP Bird Integrity Index by watershed.
#'
#' @param vbl  the name of the water quality variable to plot
#' @param dfm  The data frame containing the variable
#' @return A ggplot box plot of the index by watershed
#' @examples
#' library(ggplot2)
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_code='bii',
#'                 result=rnorm(length(stationInfo$loc_code)))
#' d <- mergeStatInfo(d)
#' p <- plotBII_ByWat('result', d)
#' p + ggtitle('Bird Integrity Index - Generated Data for Example\n')
#' @export


plotBII_ByWat <- function(vbl, dfm=bii) {

# add station info, renames
tmp <- mergeStatInfo(dfm)
names(tmp)[names(tmp)==vbl] <- 'bii'

# order watersheds by median BII
tmp$watershed <- factor(tmp$watershed,
                        levels=levels(with(tmp, reorder(watershed, bii, median))))
tmp.mdn <- aggregate(tmp$bii, list(tmp$watershed), median)

p <- ggplot( ) + geom_boxplot(aes(y=bii, x=watershed), data = tmp) +
  coord_flip() + theme_bw() + xlab('') + ylab(paste('\nBird Integrity Index')) +
  geom_text(aes(Group.1, x, label=round(x,0)),
            data=tmp.mdn,
            hjust=1.3, colour='red', size=6) +
  theme(axis.text.y=element_text(size=14), axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=16))

return(p)
}
