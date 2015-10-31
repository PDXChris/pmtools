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

