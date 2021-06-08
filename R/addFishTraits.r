#' Add fish traits to a data frame of fish counts
#'
#' @param df The data frame containing fish counts
#' @param speciesIn The field in dfm that identifies species
#' @param sppLook Look up species traits by: "Common_Name" (default)
#' or "Species_Name"
#' @param traits Which trait fields should be included?
#' @param filterNA Should species not in the traits table be dropped? (A warning
#' listing the unmatched species is provided.)
#' @return A table of fish species presence (number of surveys on which
#' species was captured), and abundance (total number of individuals captured)
#' @export


addFishTraits <- function(df, speciesIn = 'common_name', sppLook = 'Common_Name',
                          traits = c('Family', 'Species_Name', 'Common_Name', 'Origin'),
                          filterNA = TRUE){
  # Warn about unmatched species
  not.in.traits <- setdiff(df[[speciesIn]], fish.traits[[sppLook]])
  if (length(not.in.traits) > 0) {
    warning("There are species that do not match species names in the trait file:",
            toString(not.in.traits), ". These species will be removed if filterNA = TRUE")
  }

  df <- merge(df, fish.traits[, traits],
              by.x=speciesIn, by.y=sppLook, all.x=!filterNA)
  df
}
