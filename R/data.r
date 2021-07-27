#' Fish traits table
#'
#' A table of fish traits used in biotic indices.
#'
#' @format A data frame with 67 rows and 10 variables:
#' \describe{
#'   \item{Family}{Taxonomic family}
#'   \item{Species_Name}{Scientific name}
#'   \item{Origin}{Native (N) or Alien (A) species?}
#'
#'   ...
#' }
"fish.traits"

#' Standard values for plotting
#'
#' A table of habitat, water quality and biological standrads and benchmarks for plotting.
#'
#' @format A data frame with 7 rows and 3 variables:
#' \describe{
#'   \item{metric_code}{code from met.cod table}
#'   \item{red.line}{Higher (more lenient) standard}
#'   \item{grn.line}{Lower (more restrictive) standard}
#'
#'   ...
#' }
"std.lns"
