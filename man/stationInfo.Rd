\name{stationInfo}
\alias{stationInfo}
\docType{data}
\title{Information on PAWMAP Monitoring Stations.}
\description{
%%  ~~ Information on PAWMAP monitoring stations. Includes information on location (watershed and subwatershed), stream type (perennial vs. intermittent), and rotational panel ~~
}
\usage{data("stationInfo")}
\format{
  A data frame with 128 observations on the following 9 variables.
  \describe{
    \item{\code{site_identifier}}{the station number}
    \item{\code{watershed}}{the watershed in which the station is located}
    \item{\code{subwat}}{the subwatershed in which the station is located}
    \item{\code{loc.lbl}}{a label containing the subwatershed and loc_code}
    \item{\code{duration}}{stream type (perennial vs. intermittent); a factor with levels \code{I} \code{P}}
    \item{\code{panel}}{the rotational sampling panel}
    \item{\code{loc_descr}}{a description of the sampling location}
    \item{\code{stream}}{stream in which station is located}
  }
}
\references{
%%  ~~ The Portland Area Watershed Monitoring and Assessment Program (PAWMAP), \url{https://www.portlandoregon.gov/bes/article/489038} ~~
}
\examples{
data(stationInfo)
str(stationInfo)
}
\keyword{datasets}
