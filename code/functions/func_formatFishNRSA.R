formatFishNRSA <- function(data) {
  library(reshape)
  # Read in data; remove unnecessary columns; format key columns
  tmp <- data
  tmp <- subset(tmp, select=-c(PAGE, COLUMN, LINE))
  tmp <- rename(tmp, c(SITE_ID='loc_code', NAME_COM='comm_name'))
  tmp$loc_code <- sprintf("%04d", tmp$loc_code)
  tmp$ACTLDATE <- as.Date(tmp$ACTLDATE, format='%m/%d/%Y %H:%M:%S')
  tmp$comm_name <- tolower(tmp$comm_name)
  
  tmp <- subset(tmp, !comm_name %in% c("dicamptodon", "bullfrog tadpole", 
                                       "nw salamander", "nothern pacific tree frog", "crawfish", 
                                       "pacific giant salamander", "salamander spp."))
  
  tmp$comm_name <- gsub('three spine stickleback', 'three-spined stickleback', x=tmp$comm_name)
  tmp$comm_name <- gsub('threespine stickleback', 'three-spined stickleback', x=tmp$comm_name)
  tmp$comm_name <- gsub('^stickleback', 'three-spined stickleback', x=tmp$comm_name)
  tmp$comm_name <- gsub('large scale sucker', 'largescale sucker', x=tmp$comm_name)
  tmp$comm_name <- gsub('mosquito fish', 'mosquitofish', x=tmp$comm_name)
  tmp$comm_name <- gsub('pumpkin seed', 'pumpkinseed', x=tmp$comm_name)
  tmp$comm_name <- gsub('blue gill', 'bluegill', x=tmp$comm_name)
  tmp$comm_name <- gsub("red sided shiner", "redside shiner", x=tmp$comm_name)
  tmp$comm_name <- gsub("small mouth bass", "smallmouth bass", x=tmp$comm_name)
  tmp$comm_name <- gsub("^small mouth$", "smallmouth bass", x=tmp$comm_name)
  tmp$comm_name <- gsub("steelhead trout", "rainbow/steelhead", x=tmp$comm_name)
  tmp$comm_name <- gsub("^steelhead$", "rainbow/steelhead", x=tmp$comm_name)
  tmp$comm_name <- gsub("gold fish", "goldfish", x=tmp$comm_name)
  tmp$comm_name <- gsub("rainbow trout", "rainbow/steelhead", x=tmp$comm_name)
  tmp$comm_name <- gsub("^coho$", "coho salmon", x=tmp$comm_name)
  tmp$comm_name <- gsub("speckled dace\n", "speckled dace", x=tmp$comm_name)
  tmp$comm_name <- gsub("speckled dare", "speckled dace", x=tmp$comm_name)
  tmp$comm_name <- gsub("large mouth bass", "largemouth bass", x=tmp$comm_name)
  tmp$comm_name <- gsub("longnose dare", "longnose dace", x=tmp$comm_name)
  tmp$comm_name <- gsub("^bass$", "bass spp.", x=tmp$comm_name)
  tmp$comm_name <- gsub("^brook lamprey$", "western brook lamprey", x=tmp$comm_name)
  tmp$comm_name <- gsub("nothern", "northern", x=tmp$comm_name)
  tmp$comm_name <- gsub("ammocete", "ammocoete", x=tmp$comm_name)
  tmp$comm_name <- gsub("^shad$", "american shad", x=tmp$comm_name)
  tmp$comm_name <- gsub("prickly sculpin", "american shad", x=tmp$comm_name)
  
  return(tmp)
}