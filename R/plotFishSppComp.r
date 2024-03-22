#' Plot PAWMAP Fish Species Composition Data.
#'
#' @param dfm The data frame containing fish counts
#' @param speciesIn The field in dfm that identifies species
#' @param sppLook Look up species traits by: "Common_Name" (default)
#' or "Species_Name"
#' @param by Are the data formatted one row per individual ('row'), or counts per
#' species ('counts')?
#' @param countFields Only used if by='count'. A vector of length 2 indicating
#' the fields for total number of each species captured first, and the number of
#' surveys on which they were captured second.
#' @param limitLength Should the number of species plotted be limited to the top 25?
#' (to prevent an overcrowded plot)
#' @param ... Additional arguments passed to tallyFishSpp
#' @return A tornado plot of fish species presence (number of surveys on which
#' species was captured), and abundance (total number of individuals captured)
#' @import ggplot2
#' @import gridExtra
#' @import ggpubr
#' @export

plotFishSpp <- function(dfm, speciesIn='common_name', sppLook='Common_Name',
                        by='row', countFields=c('totNum', 'numSurv'),
                        limitLength=TRUE, ...) {

  dfm <- tallyFishSpp(dfm, speciesIn, sppLook, by, countFields, ...)


  # exclude rare spp if many spp
  if (nrow(dfm) > 25 & limitLength) {
    excludedSpp <- unique(dfm[1:(nrow(dfm) - 25), ][[speciesIn]])
    warning("Too many species to plot.  The following rarer species were excluded from the plot: ",
            toString(excludedSpp))
    dfm <- dfm[(nrow(dfm) - 24):nrow(dfm), ]
  }

  # Colors for labeling bars
  lst <- c(Native='palegreen4', Salmonid='steelblue2',
           `Non-Native`='firebrick2', `None Captured` = 'grey')
  barCol <- lst[unique(dfm[['bar']])]


  p.pres<- ggplot(data = dfm, (aes_string(speciesIn, 'freq', fill='bar'))) +
    geom_bar(stat='identity') + coord_flip() + theme_bw() +
    scale_fill_manual(name='Species Type',
                      values = barCol) +
    ylab('\nPresence:\nNumber of surveys w/ captures')  + xlab('') +
    theme(legend.position = "none", axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 12, hjust=.7)) +
    scale_y_reverse()

  p.abun <- ggplot(aes_string(speciesIn, 'num', fill='bar'), data = dfm) +
    geom_bar(stat='identity') + coord_flip() + theme_bw() +
    scale_fill_manual(name='Species Type',
                      breaks = names(barCol[names(barCol) != 'None Captured']),
                      values = barCol) +
    ylab('\nAbundance:   \nTotal number of individuals captured') + xlab('')

  p.abun <- p.abun + theme(legend.position = c(0.8, 0.2),
                 legend.background = element_rect(fill = 'white'),
                 axis.text.y = element_text(hjust=0),
                 axis.title.x = element_text(size = 12, hjust=.7))

  # adjust font size for species labels if only a few species
  if(nrow(dfm) < 15){
    p.abun <- p.abun + theme(axis.text.y = element_text(size = 13))
  }

  p.all <- arrangeGrob(p.pres, p.abun, ncol = 2, widths = unit(c(.43,.57), 'npc'))
  as_ggplot(p.all)
}
