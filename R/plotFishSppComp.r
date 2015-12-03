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
#' @return A tornado plot of Fish species presence (number of surveys on which
#' species was captured), and abundance (total number of individuals captured)
#' @import ggplot2
#' @import grid
#' @export
plotFishSpp <- function(dfm, speciesIn='comm_name', sppLook='Common_Name',
                        by='row', countFields=c('totNum', 'numSurv')) {

  # Exclude unwanted species
  dfm <- dfm[!dfm[[speciesIn]] %in% c('dicamptodon\n', 'cyprinidae juvenile'), ]

  ##  Format data for plotting
  if (by=='row') {
    # dfm1 = total count for each spp.
    dfm1 <- plyr::ddply(dfm, speciesIn, function(x) num=nrow(x))
    dfm1 <- plyr::rename(dfm1, c(V1='num'))

    # dfm2 = number of surveys w/ detects
    dfm2 <- plyr::ddply(dfm, speciesIn,
                        function(x) nrow(unique(x[, c('loc_code', 'ACTLDATE')])))

    dfm <- merge(dfm1, dfm2)
    dfm <- plyr::rename(dfm, c(V1='freq'))
  }
  # Warn about unmatched species
  if(any(!unique(dfm[[speciesIn]]) %in% fish.habits3[[sppLook]])) {
    warning(paste('The following species are not in the trait database:\n',
                  toString(setdiff(dfm[[speciesIn]], fish.habits3[[sppLook]]))))
  }

  if (by=='count') {
    names(dfm)[names(dfm)==countFields[1]] <- 'num'
    names(dfm)[names(dfm)==countFields[2]] <- 'freq'
  }


  # Merge species information
  dfm <- merge(dfm, fish.habits3[, c('Family', 'Species_Name', 'Common_Name', 'Origin')],
               by.x=speciesIn, by.y=sppLook, all.x=TRUE)

  # create variable for legend and bar coloring
  dfm$bar[dfm$Origin=='A'] <- 'Non-Native'
  dfm$bar[dfm$Origin=='N' & dfm$Family!='Salmonidae'] <- 'Native'
  dfm$bar[dfm$Origin=='N' & dfm$Family=='Salmonidae'] <- 'Salmonid'
  dfm$bar[dfm[[speciesIn]]=='starry flounder'] <- 'Native'
  dfm$bar <- factor(dfm$bar, levels=c('Native', 'Salmonid', 'Non-Native'))

  # format common name for axis labeling
  dfm[[speciesIn]] <- sapply(dfm[[speciesIn]],
                          function(x) paste(toupper(substr(x, 1, 1)),
                                            substr(x, 2, nchar(as.character(x))), sep=""))

  # exclude rare spp if many spp
  if (nrow(dfm)) dfm <- dfm[dfm$num > 2,]

  # Order for plotting most frequent at top
  dfm <- dfm[order(dfm$freq, dfm$num),]

  # order factor by freq
  dfm[[speciesIn]] <- factor(dfm[[speciesIn]], dfm[[speciesIn]][order(dfm$freq)])

  # Colors for labeling bars
  lst <- c(Native='palegreen4', Salmonid='steelblue2', `Non-Native`='firebrick2')
  barCol <- lst[unique(dfm[['bar']])]


  p <- ggplot(data = dfm, (aes_string(speciesIn, 'freq', fill='bar'))) +
    geom_bar(stat='identity') + coord_flip() + theme_bw() +
    scale_fill_manual(name='Species Type',
                      values = barCol) +
    ylab('\nPresence:\nNumber of surveys w/ detects')  + xlab('') +
    theme(legend.position = "none") +
    scale_x_discrete(breaks=levels(dfm[[speciesIn]]),
                     labels=rep('', length(dfm[[speciesIn]]))) +
    scale_y_reverse()

  q <- ggplot(aes_string(speciesIn, 'num', fill='bar'), data = dfm) +
    geom_bar(stat='identity') + coord_flip() + theme_bw() +
    scale_fill_manual(name='Species Type',
                      values = barCol) +
    ylab('\nAbundance:   \nTotal number of individuals detected') + xlab('')

  q <- q + theme(legend.position = c(0.8, 0.2),
                 legend.background = element_rect(fill = 'white'),
                 axis.text.y = element_text(hjust=0),
                 axis.title.x = element_text(size = 12, hjust=.7))

  # adjust font size for species labels if only a few species
  if(nrow(dfm) < 15){
    q <- q + theme(axis.text.y = element_text(size = 14))
  }

  print(q)

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1,2, widths = unit(c(.43,.57), 'npc'))))

  vplayout <- function(y)
    viewport(layout.pos.row=1, layout.pos.col = y)
  print(p, vp = vplayout(1))
  print(q, vp = vplayout(2))
}
