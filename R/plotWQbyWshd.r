#' Plot PAWMAP water quality data by watershed.
#'
#' @param vbl  the name of the water quality variable to plot
#' @param dfm  The data frame conatining the variable
#' @return A ggplot box plot of the variable by watershed
#' @examples
#' d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_name='copper',
#' result=rlnorm(length(stationInfo$loc_code)), season='S')
#' d <- rbind(d, data.frame(loc_code=unique(stationInfo$loc_code), metric_name='copper',
#'                          result=2*rlnorm(length(stationInfo$loc_code)), season='T'))
#' d <- mergeStatInfo(d)
#' p <- plotWQbyWshd('copper', d)
#' p + ggtitle('Copper - Generated Data for Example\n')
#' @export


plotWQbyWshd <- function(vbl, dfm=wq14) {
  # Subset data to single variable, add info
  dfm <- dfm[dfm[, 'metric_code'] == vbl, ]
  tmp <- mergeStatInfo(dfm)

  tmp$storm <- ifelse(tmp$season=='T', TRUE, FALSE)

  # Format and order watershed factors for axis
  tmp$watershed <- gsub(' ', '\n', tmp$watershed)
  tmp$watershed <- factor(tmp$watershed,
                          levels=c("Fanno\nCreek", "Tualatin\nStreams",
                                   "Tryon\nCreek", "Willamette\nStreams",
                                   "Johnson\nCreek", "Columbia\nSlough"))
  breaks <- as.vector(c(1, 2, 5) %o% 10^(-5:5))

  vlbl <- met.cod$label[(match(vbl, met.cod$metric_code))]
  titl <- paste0(vlbl,' in Portland Watersheds\n')

  trim.trailing <- function (x) sub("\\s+$", "", x)


  p <- ggplot(aes(watershed, result, fill=storm), data=tmp) + geom_boxplot() +
    scale_y_log10(breaks=breaks, expand=c(0, 0.1)) + xlab('') + theme_bw() +
    ylab(paste0(vlbl,' (', trim.trailing(unique(tmp$units)), ')\n')) +
    scale_fill_manual(name='Type', labels=c('Seasonal', 'Storm'), values=c("darkseagreen", "#0090b2")) +
    ggtitle(titl) + theme(plot.title = element_text(size=16, face='bold'),
                          axis.text.x  = element_text(size=15)) +
    geom_vline(x=c(2.5, 4.5), size=1)

  lbl <- data.frame(x=c(1.5, 3.5, 5.5), y=rep(1.4*max(tmp$result), 3),
                    label=c('West Slope of\nWest Hills', 'East Slope of\nWest Hills', 'Eastside\nStreams'))
  p <- p + geom_text(aes(x=x, y=y, label=label, fill=NULL, face='bold'), vjust=1, data=lbl)

  return(p)
}
