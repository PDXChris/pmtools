#' Standardize fish names and correct common misspellings
#'
#' @param vct  A vector containing the fish names
#' @return A vector with corrected fish names
#' @export
#' @import stringi

fixFishNames <- function(vct) {

  ## Lookup vector for proper names
                   # Real name                   # regex to capture mistake
  kNamesMap <- c("three-spined stickleback" = "three\\s?spine stickleback",
                 "largescale sucker"        = "large scale sucker",
                 "mosquito fish"            = "mosquitofish",
                 "pumpkinseed"              = "pumpkin seed",
                 "bluegill"                 = "blue gill",
                 "redside shiner"           = "red sided shiner",
                 "smallmouth bass"          = "small mouth.*",
                 "rainbow/steelhead"        = ".*steelhead.*",
                 "rainbow/steelhead"        = ".*rainbow.*",
                 "goldfish"                 = "gold fish",
                 "coho salmon"              = "^coho$",
                 "speckled dace"            = "speckled da.*",
                 "largemouth bass"          = "large mouth bass",
                 "longnose dace"            = "longnose dare",
                 "bass spp."                = "^bass$",
                 "western brook lamprey"    = "^brook lamprey$",
                 "northern"                 = "nothern",
                 "ammocoete"                = "ammocete",
                 "american shad"            = "^shad$",
                 "reticulate sculpin"       = "^reticultate sculpin$",
                 "pacific giant salamander" = "dicamptodon.*",
                 "cutthroat trout"          = "cutthroat.*",
                 "peamouth"                 = "peamouth.*")

  ret <- stri_replace_all_regex(vct, pattern = unname(kNamesMap),
                                replacement = names(kNamesMap),
                                vectorize_all=FALSE)
  ret
}

#' Drop Amphibians from a data frame of fish names
#'
#' @param dfm The data frame containing the fish data
#' @param vbl  A field within dfm containing the fish names
#' @return A A data frame without amphibians
#' @export

dropAmphibs <- function(dfm, vbl) {

  ## Remove amphibians and invertebrates
  drops <- c("dicamptodon.*", ".*frog.*",
             "cra.fish", ".*salamander.*")
  dfm <- vct[!grepl(paste(drops, collapse="|"), dfm[[vbl]])]
}
