fishTraits <- function(){
  trait.list <-
    list(family = c("Catostomidae", "Catostomidae", "Catostomidae", "Centrarchidae", "Centrarchidae",
                    "Centrarchidae", "Centrarchidae","Centrarchidae", "Centrarchidae", "Centrarchidae",
                    "Centrarchidae", "Centrarchidae", "Centrarchidae", "Centrarchidae", "Centrarchidae",
                    "Centrarchidae", "Clupeidae", "Cobitidae", "Cottidae", "Cottidae", "Cottidae",
                    "Cottidae", "Cottidae", "Cottidae", "Cyprinidae", "Cyprinidae", "Cyprinidae",
                    "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae",
                    "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", "Cyprinidae", "Fundulidae",
                    "Gasterosteidae", "Ictaluridae", "Ictaluridae", "Ictaluridae", "Percidae", "Percopsidae",
                    "Petromyzontidae", "Petromyzontidae", "Petromyzontidae", "Petromyzontidae", "Poeciliidae",
                    "Salmonidae", "Salmonidae", "Salmonidae", "Salmonidae", "Salmonidae", "Salmonidae",
                    "Petromyzontidae"),
         species = c("Catostomus macrocheilus", "Catostomus spp.", "Catostomus platyrhynchus", "Lepomis cyanellus",
                     "Lepomis gibbosus", "Lepomis gulosus", "Lepomis macrochirus", "Lepomis spp.",
                     "Micropterus dolomieu", "Micropterus salmoides", "Micropterus spp.", "Pomoxis annularis",
                     "Pomoxis nigromaculatus", "Pomoxis spp.", "Lepomis auritus", "Lepomis microlophus",
                     "Alosa sapidissima", "Misgurnus anguillicaudatus", "Cottus asper", "Cottus gulosus",
                     "Cottus perplexus", "Cottus rhotheus", "Cottus spp.", "Cottus beldingi",
                     "Acrocheilus alutaceus", "Carassius auratus", "Cyprinus carpio",
                     "Mylocheilus caurinus", "Pimephales promelas", "Ptychocheilus oregonensis",
                     "Rhinichthys cataractae", "Rhinichthys osculus", "Rhinichthys spp.",
                     "Richardsonius balteatus", "Ctenopharyngodon idella", "Oregonichthys crameri",
                     "Rhinichthys falcatus", "Gila bicolor", "Fundulus diaphanus",
                     "Gasterosteus aculeatus", "Ameiurus melas", "Ameiurus natalis",
                     "Ameiurus nebulosus", "Perca flavescens", "Percopsis transmontana",
                     "Lampetra richardsoni", "Lampetra spp.", "Lampetra tridentata",
                     "Lampetra spp.", "Gambusia affinis", "Oncorhynchus clarki", "Oncorhynchus kisutch",
                     "Oncorhynchus mykiss", "Oncorhynchus spp.", "Oncorhynchus tshawytscha",
                     "Prosopium williamsoni", "Petromyzontidae"),
         common_name = c("largescale sucker", "unidentified sucker", "mountain sucker",
                         "green sunfish", "pumpkinseed", "warmouth", "bluegill", "sunfish",
                         "smallmouth bass", "largemouth bass", "bass spp.", "white crappie",
                         "black crappie", "crappie", "redbreast sunfish", "redear sunfish",
                         "american shad", "weather loach", "prickly sculpin", "riffle sculpin",
                         "reticulate sculpin", "torrent sculpin", "unidentified sculpin",
                         "paiute sculpin", "chiselmouth", "goldfish", "common carp", "peamouth",
                         "fathead minnow", "northern pikeminnow", "longnose dace", "speckled dace",
                         "dace spp.", "redside shiner", "grass carp", "oregon chub", "leopard dace",
                         "tui chub", "banded killifish", "three-spined stickleback", "black bullhead",
                         "yellow bullhead", "brown bullhead", "yellow perch", "sand roller",
                         "western brook lamprey", "unidentified lamprey", "pacific lamprey",
                         "lamprey ammocoete", "mosquitofish", "cutthroat trout", "coho salmon",
                         "rainbow/steelhead", "unidentified salmonid", "chinook salmon",
                         "mountain whitefish", "unidentified lamprey"),
         origin = c("N", "N", "N", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
                    "A", "A", "A", "A", "N", "N", "N", "N", "N", "N", "N", "A", "A",
                    "N", "A", "N", "N", "N", "N", "N", "A", "N", "N", "A", "A", "N",
                    "A", "A", "A", "A", "N", "N", "N", "N", "N", "A", "N", "N", "N",
                    "N", "N", "N", "N"),
         habitat = c("B", "B", "B", "W", "W", "W", "W", "W", "W", "W", "W", "W",
                     "W", "W", "W", "W", "W", "B", "B", "B", "B", "B", "B", "B", "B",
                     "B", "B", "W", "B", "W", "B", "B", "B", "W", "B", "W", "B", "W",
                     "W", "W", "B", "B", "B", "W", "B", "B", "B", "B", "B", "W", "W",
                     "W", "W", "W", "W", "B", "B"),
         hider = c("", "", "", "", "", "", "", "", "", "", "", "",
                   "", "", "", "", "", "", "", "H", "H", "H", "", "H", "", "",
                   "", "", "", "", "H", "H", "H", "", "", "H", "H", "", "",
                   "H", "H", "H", "H", "", "H", "H", "H", "H", "H", "H", "H",
                   "", "H", "", "", "", "H"),
         tolerance = c("I", "", "S", "T", "I", "T", "T", "", "I", "T", "", "T", "T",
                       "T", "T", "T", "I", "T", "I", "I", "I", "S", "", "S", "I", "T",
                       "T", "I", "T", "I", "I", "I", "I", "I", "T", "S", "I", "I", "T",
                       "I", "T", "T", "T", "I", "I", "S", "S", "S", "S", "T", "S", "S",
                       "S", "S", "S", "S", "S"),
         foraging = c("O", "", "S/S", "T", "I", "T", "I", "", "T", "T", "T", "T",
                      "T", "T", "I", "I", "I", "O", "I", "I", "I", "T", "", "I", "S/S",
                      "O", "O", "I", "O", "T", "I", "I", "I", "I", "H", "I", "I", "O",
                      "O", "I", "O", "O", "O", "T", "I", "F/S", "F/S", "F/S", "F/S",
                      "O", "T", "T", "T", "T", "T", "I", "F/S"),
         reproduction = c("L", "L", "L", "PN", "PN", "PN", "PN", "PN", "LN", "PN", "",
                          "VN", "PN", "", "PN", "PN", "", "V", "CN", "CN", "CN", "CN",
                          "CN", "CN", "L", "V", "V", "L", "P/CN", "L", "L", "LN", "", "LV",
                          "V", "V", "L", "", "V", "VN", "P/CN", "P/CN", "P/CN", "V", "L",
                          "NLN", "NLN", "NLN", "NLN", "LB", "NLN", "NLN", "NLN", "NLN",
                          "NLN", "NLN", "NLN"))
  ret <- structure(trait.list,
                   .Names = c("family", "species","common_name", "origin", "habitat",
                              "hider", "tolerance", "foraging", "reproduction"),
                   row.names = 1L:57L,
                   class = "data.frame")

  lunkers <- c("Cottus asper"              = 10,
               "Cottus rhotheus"           = 10,
               "Oncorhynchus clarki"       = 25,
               "Oncorhynchus mykiss"       = 30,
               "Acrocheilus alutaceus"     = 30,
               "Ptychocheilus oregonensis" = 30,
               "Catostomus macrocheilus"   = 30)

  ret$native      <- ret$origin  == 'N'
  ret$native.ben  <- ret$habitat == 'B' & ret$native
  ret$native.wc   <- ret$habitat == 'W' & ret$native
  ret$hider       <- ret$hider   == 'H'
  ret$sensitive   <- ret$tolerance    == 'S'
  ret$tolerant    <- ret$tolerance    == 'T'
  ret$nlns        <- ret$reproduction == 'NLN'
  ret$filter.feed <- ret$foraging == 'F/S'
  ret$top.carniv  <- ret$foraging == 'T' & ret$native
  ret$omnivore    <- ret$foraging == 'O'

  ret$lunker      <- lunkers[ret$species]
  ret$target.lunker <- ret$species %in% names(lunkers)

  return(ret)
}
