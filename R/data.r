#' Fish traits table
#'
#' A table of fish traits used in biotic indices.
#'
#' @format A data frame with 67 rows and 10 variables:
#' \describe{
#'   \item{Family}{Taxonomic family}
#'   \item{Species_Name}{Scientific name}
#'   \item{Common_Name}{The common name of the species}
#'   \item{Origin}{Native (N) or Alien (A) species?}
#'   \item{Habitat}{Benthic or water column}
#'   \item{Hider}{is the species found in substrate interstices or among macrophytes or
#'    organic debris?}
#'   \item{Tolerance}{Indicates the species is tolerant to disturbance or poor habitat}
#'   \item{Foraging}{Foraging behavior: F/S, filterer/specialist; S/S, scraper/specialist;
#'   O, omnivore; I, invertivore; T, top carnivore}
#'   \item{Reproduction}{Reprodcutive behavior: NLN, nonguarding lithophil
#'   (gravelcobble) nester; LN, lithophil nester; L, lithophil; V, vegetation;
#'   P, psammophil (sand - fine gravel); CN, cavity nester; LB, livebearer;
#'   VN, vegetation nester; PN, psammophil nester}
#'   \item{Notes}{Notes on traits}
#'   ...
#' }
"fish.traits"

#' Standard values for plotting
#'
#' A table of habitat, water quality and biological standards and benchmarks for plotting.
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
