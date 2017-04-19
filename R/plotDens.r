#' Plot Density of a Variable
#'
#' @description Plot a density and strip plot to illustrate the distribution of a variable
#'
#' @param df  The data frame containing the variable
#' @param vbl The variable to plot.  A character string.
#' @param xtrn Data transformation
#' @param title Title for the plot
#' @param print Should the graph be saved to a file?
#' @return A density plot
#' @examples
#' df <- data.frame(x=rlnorm(100))
#' plotDens(df, 'x')
#' plotDens(df, 'x', xtrn = 'log10')
#' @export
#' @import grid
#' @import ggplot2

plotDens <- function (df, vbl, xtrn=NULL, title=NULL, print=FALSE) {

  # create a title for the plot
  if (is.null(title)) {
    title <- paste0('Densityplot -- data: ', deparse(substitute(df)),
                    '\nvariable: ', vbl, '\n')
  }

  # 1st plot - Density.  2nd - dotplot
  p <- ggplot(data=df, aes_string(x=vbl)) + geom_density(fill='red', alpha=0.5) +
    theme_bw() + theme(axis.title.x=element_text(vjust=0)) + ggtitle(title)
  q <- ggplot(data=df, aes_string(x=vbl, y='1')) + geom_jitter(size=3, alpha=0.5) +
    theme_bw() + theme(axis.text.y = element_blank()) +
    xlab('') + ylab('')

  # option to transform data
  if (!is.null(xtrn)) {
    p <- p + scale_x_continuous(trans=xtrn)
    q <- q + scale_x_continuous(trans=xtrn)
  }

  gp1<- ggplot_gtable(ggplot_build(p))
  gp2<- ggplot_gtable(ggplot_build(q))
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth

  grph <- gridExtra::grid.arrange(gp1, gp2, ncol=1, heights=c(5,1))

  # option to save plot to pdf
  if (print==TRUE) {
    ##  NOTE this will break in general use
    pdf(paste0('./Graphs/', vbl, 'ggDens', xtrn, '.pdf'), width=10.5, height=8)
    print(grph)
    dev.off()
  }
  return(grph)
}
