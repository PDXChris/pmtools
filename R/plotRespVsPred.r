#' Plot Correlations of Predictors vs. a Response
#' @description Takes a response variable (e.g., a macroinvertebrate or fish IBI) and
#' plots it against a number of predictor variables.
#' @usage plotRespVsPred(dat, resp, pred)
#' @param df the dataset containing the predictor and response variables
#' @param resp the name of the response variable within the data frame, as a string
#' @param pred a vector of the column numbers of the predictors or the names as strings
#' @details Creates faceted scatterplots with each predictor as a facet, the
#' correlation coefficient plotted in the corner, and the facets sorted in
#' decreasing correlation strength.
#' @return Faceted scatterplots of each predictor vs. the response
#' @examples plotRespVsPred(mtcars, 'mpg', c('hp', 'wt', 'disp'))
#' @export


plotRespVsPred <- function(df, resp, pred) {

  # Create a list storing results of correlation for each variable
  a <- lapply(df[, pred], function(x) cor.test(df[, resp, drop=TRUE], x))

  # Filter to significant correlations; select significant fields
  a <- Filter(function(x) x$p.value < 0.05, a)
  df <- df[, c(resp, names(a))]

  # Format and provide meaningful labels
  df <- reshape::melt.data.frame(df, id=c(resp))

  # Create data frame to store correlation results
  df.r <- plyr::ldply(a, function(x) c(x[['estimate']],
                                  'p.value'=x[['p.value']]), .id='variable')
  # Sort factors (and thus facets) by correl. coeff.
  df$variable <- factor(df$variable,
                         levels=df.r[order(-abs(df.r$cor)), ]$variable)

  # Plot
  p <- ggplot(df, aes_string('value', resp)) + geom_point() +
    facet_wrap(~variable, scales='free_x') + geom_smooth(method='lm') +
    geom_text(data=df.r, aes(label=paste0('r = ', round(cor, 2))),
                                         x=Inf, y=Inf, hjust=1.3, vjust=1) +
    ylab(paste0(resp, '\n')) + xlab('') +
    theme(strip.text = element_text(face='bold'))
  # + stat_smooth(color='red', se=FALSE)

  return(p)
}
