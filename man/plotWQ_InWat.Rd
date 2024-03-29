% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotWQ_InWat.r
\name{plotWQ_InWat}
\alias{plotWQ_InWat}
\title{Plot PAWMAP Water Quality Data for each station within a selected watershed.}
\usage{
plotWQ_InWat(
  indf,
  wat,
  analyte_field = "janus_analyte_name",
  result = "numeric_result",
  analyte_units = "analyte_units",
  storm = "storm_affected",
  storm.value = "Yes",
  geo.mean = FALSE
)
}
\arguments{
\item{indf}{The data frame containing the variable to be plotted}

\item{wat}{The watershed for which station data will be plotted}

\item{analyte_field}{The field containing the name of the analyte}

\item{result}{The field storing the numeric value}

\item{analyte_units}{The field containing the measurement units}

\item{storm}{The field indicating whether it is a storm or seasonal sample}

\item{storm.value}{The value indicating it is a storm sample}

\item{geo.mean}{Should the geometric mean be used instead of the arithmetic?}
}
\value{
A ggplot dot plot of the variable at each station within a watershed
}
\description{
Plot PAWMAP Water Quality Data for each station within a selected watershed.
}
\examples{
library(ggplot2)
stations <- unique(stationInfo$site_identifier)
num_stations <- length(stations)
d <- data.frame(site_identifier=stations, janus_analyte_name='copper',
                numeric_result=rlnorm(num_stations), storm_affected='No',
                cycle=replicate(num_stations, sample(c(1,2), 1)))
d <- rbind(d, data.frame(site_identifier=stations, janus_analyte_name='copper',
                 numeric_result=2*rlnorm(num_stations), storm_affected='Yes',
                 cycle=replicate(num_stations, sample(c(1,2), 1))))
d <- mergeStatInfo(d)
p <- plotWQ_InWat(d, 'Johnson Creek')
p + ggtitle('Copper - Generated Data for Example\n')
}
