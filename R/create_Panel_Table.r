#' Create table of PAWMAP sampling years, panels and cycles
#' @param cycles The number of four year rotational panels to include in the table
#' @return A table of PAWMAP sampling years, panels and cycles
#' @export


create_Panel_Table <- function(cycles = 5){

  panel_years <- data.frame(PAWMAP_Year = 1:(n*4), TempYear = 2010:(2010 + n*4 - 1),
                            panel = rep(c(1:4), n), cycle = rep(1:n, each = 4))

  panel_years$FiscalYear <- paste0(panel_years$tempYear, '-', panel_years$tempYear + 1)

  panel_years <- panel_years[, c(1, 5, 2:4)]
  panel_years
}
