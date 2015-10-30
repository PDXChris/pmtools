#' Plot Correlations of Predictors vs. a Response
#' @description Takes a response variable (e.g., a macroinvertebrate or fish IBI) and
#' plots it against a number of predictor variables.
#' @usage plotRespVsPred(dat, resp, pred, id='loc_code')
#' @param dat the dataset containing the predictor and response variables
#' @param resp the name of the response variable within the data frame, as a string
#' @param pred a vector of the column numbers of the predictors or the names as strings
#' @param id the ID variable - typically the station number/name
#' @details Creates faceted scatterplots with each predictor as a facet.
#' @return Faceted scatterplots of each predictor vs. the response
#' @export


plotRespVsPred <- function(dat, resp, pred, id='loc_code') {

  # Create a list storing results of correlation for each variable
  a <- lapply(tmp[, pred], function(x) cor.test(tmp[, resp, drop=TRUE], x))

  #Filter to significant correlations; select significant fields
  a <- Filter(function(x) x$p.value < 0.05, a)
  tmp <- tmp[, c(id, resp, names(a))]

  # Format and provide meaningful labels
  tmp <- reshape::melt.data.frame(tmp, id=c(id, resp))

  #THIS NEEDS TO BE GENERALIZED
  lbl <- read.csv('lbl.csv')
  tmp$variable <- lbl$label[match(tmp$variable, lbl$code)]

  # Create data frame to store correlation results
  tmp.r <- plyr::ldply(a, function(x) c(x[['estimate']],
                                  'p.value'=x[['p.value']]), .id='variable')
  tmp.r$variable <- lbl$label[match(tmp.r$variable, lbl$code)]

  tmp$variable <- factor(tmp$variable,
                         levels=tmp.r[order(-abs(tmp.r$cor)), ]$variable)


  # Plot
  p <- ggplot(tmp, aes(value, oep5)) + geom_point() +
    facet_wrap(~variable, scales='free_x') + geom_smooth(method='lm') +
    geom_text(data=tmp.r, aes(label=paste0('r = ', round(cor, 2))),
                                         x=Inf, y=Inf, hjust=1.3, vjust=1) +
    ylab('Oberserved/Expected Ratio\n') + xlab('') +
    theme(strip.text = element_text(face='bold'))
  # + stat_smooth(color='red', se=FALSE)
  #
  # png('../Year_2/Graphs/OEvsKeyCorr.png', width=800, height=500)
  # print(p)
  # dev.off()
  return(p)
}
