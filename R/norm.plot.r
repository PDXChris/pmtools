#' @title Plot a variable and test for normality
#'
#' @description Plot a variable in a qqnorm plot and return the results of a Shapiro-Wilk normality test.
#' @param x a vector of numbers
#' @return the results of a Shapiro-Wilk normality test
#' @examples
#' x <- rnorm(100)
#' norm.plot(x)

#' @export

norm.plot <- function(x) {
  vbl<-deparse(substitute(x))
  lab<-paste("Normal Q-Q Plot: ", vbl)
  qqnorm(x, main=lab)
  qqline(x)
  shapiro.test(x)
}

#' @title A Panel Function for Pairs to Add Correlation Coefficients
#'
#' @description Panel functions to add correlation coefficients to half a pairs plot
#' @param x, y a vector of numbers
#' @return correlation coefficients for pairs
#' @examples
#' pairs(USJudgeRatings, lower.panel = panel.smooth, upper.panel = panel.cor)
#' @export

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
