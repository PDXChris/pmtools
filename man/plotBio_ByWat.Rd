% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotBio_ByWat.r
\name{plotBio_ByWat}
\alias{plotBio_ByWat}
\title{Plot A GGplot jitter plot of a biological metric by watershed}
\usage{
plotBio_ByWat(
  df,
  col,
  metric = c("macro.oe", "bird.bii", "fish.ibi"),
  title = TRUE,
  ...
)
}
\arguments{
\item{df}{A data frame with a variable to plot}

\item{col}{The column containing the values to plot by watershed}

\item{metric}{The biological metric to plot: either 'macro.oe', 'bird.bii', or 'fish.ibi'}

\item{title}{Should a title be added to the graph?  Default is true.}
}
\value{
A ggplot graph with the variable on the horizontal axis and the
watersheds along the vertical axis sorted by mean of the variable
}
\description{
Plot A GGplot jitter plot of a biological metric by watershed
}
