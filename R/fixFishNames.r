#' Standardize fish names and correct common misspellings
#'
#' @param vct  A vector containing the fish names
#' @return A vector with corrected fish names
#' @export
#' @import stringi

fixFishNames <- function(vct) {

  ## Lookup vector for proper names
                   # Real name                   # regex to capture mistake
  kNamesMap <- c("threespine stickleback" = "three[\\s, -]?spined? stickle\\s?back",
                 "largescale sucker"        = "large scale sucker",
                 "western mosquitofish"     = ".*mosquito.*",
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
                 "longnose dace"            = "longnose da.*",
                 "bass spp."                = "^bass$",
                 "western brook lamprey"    = "^brook lamprey$",
                 "northern"                 = "nothern",
                 "ammocoete"                = "ammocete",
                 "american shad"            = "^shad$",
                 "reticulate sculpin"       = "^reticultate sculpin$",
                 "pacific giant salamander" = "dicamptodon.*",
                 "cutthroat trout"          = "cutthroat.*",
                 "peamouth"                 = "peamouth.*",
                 "sand roller"              = "sandroller",
                 "unidentified centrarchid" = "unidentified sunfish/bass",
                 "unidentified bass"        = "black bass.*",
                 "oriental weatherfish"     = ".*weather.*",
                 "unidentified sculpin"     = "freshwater sculpins")

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
