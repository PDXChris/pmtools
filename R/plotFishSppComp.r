#' Plot PAWMAP Fish Species Composition Data.
#'
#' @param dfm_plt The data frame containing fish counts
#' @param speciesIn The field in dfm_plt that identifies species
#' @param sppLook Look up species traits by: "Common_Name" (default)
#' or "Species_Name"
#' @param by Are the data formatted one row per individual ('row'), or counts per
#' species ('counts')?
#' @param countFields Only used if by='count'. A vector of length 2 indicating
#' the fields for total number of each species captured first, and the number of
#' surveys on which they were captured second.
#' @return A tornado plot of fish species presence (number of surveys on which
#' species was captured), and abundance (total number of individuals captured)
#' @import ggplot2
#' @import grid
#' @export
plotFishSpp <- function(dfm, speciesIn='comm_name', sppLook='Common_Name',
                        by='row', countFields=c('totNum', 'numSurv')) {

  dfm_plt <- tallyFishSpp(dfm, speciesIn, sppLook, by, countFields)

  # exclude rare spp if many spp
  if (nrow(dfm_plt) > 25) dfm_plt <- dfm_plt[dfm_plt$num >= 2,]

  # Colors for labeling bars
  lst <- c(Native='palegreen4', Salmonid='steelblue2', `Non-Native`='firebrick2')
  barCol <- lst[unique(dfm_plt[['bar']])]


  p <- ggplot(data = dfm_plt, (aes_string(speciesIn, 'freq', fill='bar'))) +
    geom_bar(stat='identity') + coord_flip() + theme_bw() +
    scale_fill_manual(name='Species Type',
                      values = barCol) +
    ylab('\nPresence:\nNumber of surveys w/ detects')  + xlab('') +
    theme(legend.position = "none") +
    scale_x_discrete(breaks=levels(dfm_plt[[speciesIn]]),
                     labels=rep('', length(dfm_plt[[speciesIn]]))) +
    scale_y_reverse()

  q <- ggplot(aes_string(speciesIn, 'num', fill='bar'), data = dfm_plt) +
    geom_bar(stat='identity') + coord_flip() + theme_bw() +
    scale_fill_manual(name='Species Type',
                      values = barCol) +
    ylab('\nAbundance:   \nTotal number of individuals detected') + xlab('')

  q <- q + theme(legend.position = c(0.8, 0.2),
                 legend.background = element_rect(fill = 'white'),
                 axis.text.y = element_text(hjust=0),
                 axis.title.x = element_text(size = 12, hjust=.7))

  # adjust font size for species labels if only a few species
  if(nrow(dfm_plt) < 15){
    q <- q + theme(axis.text.y = element_text(size = 14))
  }

#   print(q)

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2, widths = unit(c(.43,.57), 'npc'))))

  vplayout <- function(y)
    viewport(layout.pos.row=1, layout.pos.col = y)
  print(p, vp = vplayout(1))
  print(q, vp = vplayout(2))
}
