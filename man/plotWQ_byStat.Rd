% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotWQ_byStat.r
\name{plotWQ_byStat}
\alias{plotWQ_byStat}
\title{Plot PAWMAP Water Quality Data within a selected watershed.}
\usage{
plotWQ_byStat(
  indf,
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

\item{analyte_field}{The field containing the name of the analyte}

\item{result}{The field storing the numeric value}

\item{analyte_units}{The field containing the measurement units}

\item{storm}{The field indicating whether it is a storm or seasonal sample}

\item{storm.value}{The value indicating it is a storm sample}

\item{geo.mean}{Should the geometric mean be used instead of the arithmetic?}
}
\value{
A ggplot dot plot of the variable within a watershed
}
\description{
Plot PAWMAP Water Quality Data within a selected watershed.
}
\examples{
d <- data.frame(loc_code=unique(stationInfo$loc_code), metric_name='copper',
                result=rlnorm(length(stationInfo$loc_code)), season='S')
d <- rbind(d, data.frame(loc_code=unique(stationInfo$loc_code), metric_name='copper',
                 result=2*rlnorm(length(stationInfo$loc_code)), season='T'))
d <- mergeStatInfo(d)
p <- plotWQ_InWat('copper', 'Johnson Creek', d)
p + ggtitle('Copper - Generated Data for Example\n')
}
