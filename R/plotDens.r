#' Plot Density of a Variable
#'
#' @description Plot a density and strip plot to illustrate the distribution of a variable
#'
#' @param dat  The data frame containing the variable
#' @param vari The variable to plot.  A character string.
#' @param xtrn Data transformation
#' @return A density plot
#' @examples
#' df <- data.frame(x=rlnorm(100))
#' plotDens(df, 'x')
#' plotDens(df, 'x', xtrn = 'log10')
#' @export
#' @import grid

plotDens <- function (dat, vari, xtrn='', print=FALSE) {

  # create a ttile for the plot
  ttl <- paste0('Densityplot -- data: ', deparse(substitute(dat)),
                '\nvariable: ', vari, '\n')

  # 1st plot - Density.  2nd - dotplot
  p <- ggplot(data=dat, aes_string(x=vari)) + geom_density(fill='red', alpha=0.5) +
    theme_bw() + theme(axis.title.x=element_text(vjust=0)) + ggtitle(ttl)
  q <- ggplot(data=dat, aes_string(x=vari, y='1')) + geom_jitter(size=3, alpha=0.5) +
    theme_bw() + theme(axis.text.y = element_blank()) +
    xlab('') + ylab('')

  # option to transform data
  if (xtrn!='') {
    p <- p + scale_x_continuous(trans=xtrn)
    q <- q + scale_x_continuous(trans=xtrn)
  }

  gp1<- ggplot_gtable(ggplot_build(p))
  gp2<- ggplot_gtable(ggplot_build(q))
  maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
  gp1$widths[2:3] <- maxWidth
  gp2$widths[2:3] <- maxWidth

  # option to save plot to pdf
  if (print==TRUE) {
    pdf(paste0('./Graphs/', vari, 'ggDens', xtrn, '.pdf'), width=10.5, height=8)
    gridExtra::grid.arrange(gp1, gp2, ncol=1, heights=c(5,1))
    dev.off()
  }

  gridExtra::grid.arrange(gp1, gp2, ncol=1, heights=c(5,1))


}
