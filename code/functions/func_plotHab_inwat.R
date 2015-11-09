plotHab_inwat <- function(var, wat) {

library(reshape)
library(ggplot2)
library(grid)

tmp <- subset(hab13, metric_code %in% c("v1tm100", "xcl", "xcembed", "xcb_hall", "bankhard"))
tmp <- merge(tmp, met.cod, by='metric_code')
# tmp$metric_code <- factor(tmp$metric_code)
# levels(tmp$units) <- trim.trailing(levels(tmp$units))

tmp <- merge(tmp[, !colnames(tmp) %in% c('watershed', 'loc.lbl', 'panel', 'duration')], 
             stat.info3[,c('loc_code', 'watershed', 'loc.lbl', 'panel', 'duration')])
  
  # Subset data to single variable, format
  tmp <- subset(tmp, metric_code == var & watershed == wat)
#   tmp$metric_code <- factor(tmp$metric_code)
  # tmp$units <- factor(tmp$units)
  
  
  
  # Create labels
  #for capitalizing xlab and possibly title text
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  hbm.lab <- .simpleCap(var)
  
  # Sort data 
  tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, result) )
  tmp$panel <- factor(tmp$panel)
  
  
  p <- ggplot(aes(result, loc.lbl), data = tmp) + 
    geom_point(size=4) + 
    ylab('') + theme_bw() + # scale_x_log10(breaks = breaks, labels = breaks) +  
    xlab(paste0('\n', unique(tmp$label))) + 
    theme(plot.margin = unit(c(.75, 0, 0, 0), "lines"),
          axis.text.y = element_text(size = 14), 
          axis.title = element_text(size = 18, face='bold'),
          legend.text =element_text(size = 14), 
          legend.key.size = unit(1.3, "cm"),
          legend.title = element_text(size=16, face = "bold", hjust=0))
  
  if (var=="v1tm100") {
    p <- p + geom_vline(x=20, color='red', lwd=1.2) + geom_vline(x=30, , color='darkgreen', lwd=1.2) 
  }
  
  print(p)
  
}