#' Standardize fish names and correct common misspellings
#'
#' @param vct  A vector containing the fish names
#' @return A A vector with corrected fish names
#' @export

fixFishNames <- function(vct) {

  ## Remove amphibians and invertebrates
  drops <- c("dicamptodon.*", ".*frog.*",
             "cra.fish", ".*salamander.*")
  vct <- vct[!grepl(paste(drops, collapse="|"), vct)]

  ## Replace alternate names
  vct <- gsub("three\\s?spine stickleback",
              'three-spined stickleback', x=vct)
  vct <- gsub('large scale sucker', 'largescale sucker', x=vct)
  vct <- gsub('mosquito fish', 'mosquitofish', x=vct)
  vct <- gsub('pumpkin seed', 'pumpkinseed', x=vct)
  vct <- gsub('blue gill', 'bluegill', x=vct)
  vct <- gsub("red sided shiner", "redside shiner", x=vct)
  vct <- gsub("small mouth bass", "smallmouth bass", x=vct)
  vct <- gsub("^small mouth$", "smallmouth bass", x=vct)
  vct <- gsub("steelhead trout", "rainbow/steelhead", x=vct)
  vct <- gsub("^steelhead$", "rainbow/steelhead", x=vct)
  vct <- gsub("gold fish", "goldfish", x=vct)
  vct <- gsub("rainbow trout", "rainbow/steelhead", x=vct)
  vct <- gsub("^coho$", "coho salmon", x=vct)
  vct <- gsub("speckled da.*", "speckled dace", x=vct)
  vct <- gsub("large mouth bass", "largemouth bass", x=vct)
  vct <- gsub("longnose dare", "longnose dace", x=vct)
  vct <- gsub("^bass$", "bass spp.", x=vct)
  vct <- gsub("^brook lamprey$", "western brook lamprey", x=vct)
  vct <- gsub("nothern", "northern", x=vct)
  vct <- gsub("ammocete", "ammocoete", x=vct)
  vct <- gsub("^shad$", "american shad", x=vct)
  vct <- gsub("^reticultate sculpin$", "reticulate sculpin", x=vct)
  vct
}
