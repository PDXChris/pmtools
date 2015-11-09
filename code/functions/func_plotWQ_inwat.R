plotWQ_inwat <- function(var, wat) {
  
library(reshape)
library(ggplot2)
library(grid)

# Subset data to single analyte, single panel, non-storm, only needed fields, format
tmp <- subset(wq13, metric_name == var & watershed == wat, 
                select=c('loc_code', 'coll_date', 'panel', 'metric_name', 'result', 
                         'season', 'cens', 'units'))
  tmp$metric_name <- factor(tmp$metric_name)
  tmp$units <- factor(tmp$units)
  tmp <- merge(tmp, stat.info3[,c('loc_code', 'watershed', 'loc.lbl', 'duration')])
  
  # Create labels
  #for capitalizing xlab and possibly title text
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  poll.lab <- .simpleCap(var)
  unt.lab <- levels(factor(tmp$units))
  breaks <- as.vector(c(1, 2, 5) %o% 10^(-5:5))   #make scale non-scientific format
  
  # Summarize seasonal data.  Reshape data; add station info; format
  tmp <- subset(tmp, season!='T')
  
  if (var == 'e.coli') {
  tmp <- ddply(tmp, .(loc_code, panel), summarise, smean=exp(mean(log(result))), smin=min(result), smax=max(result))
  } else {
    tmp <- ddply(tmp, .(loc_code, panel), summarise, smean=mean(result), smin=min(result), smax=max(result))
    
  }
  tmp <- merge(tmp[,!colnames(tmp) %in% c('watershed', 'loc.lbl', 'duration')], 
               stat.info3[,c('loc_code', 'watershed', 'loc.lbl', 'duration')])
#   tmp$watershed <- gsub(' ', '\n', tmp$watershed)
#   tmp$duration <- factor(tmp$duration, levels=c("P", "I"))
#   names(tmp)[names(tmp) == poll] <- 'result'
  
  # Join back storm data
  tmp3 <- subset(wq13, metric_name == var & season=='T' & watershed == wat, 
                 select=c('loc_code', 'result'))
  names(tmp3)[names(tmp3) == 'result'] <- 'storm'
  tmp <- merge(tmp, tmp3)
  rm(tmp3)
  
  # Sort data by mean of seasonal data
  tmp <- transform(tmp, loc.lbl=reorder(loc.lbl, smean) )
  tmp$panel <- factor(tmp$panel)
  
  
  p <- ggplot(aes(smean, loc.lbl), data = tmp) + 
    geom_segment(aes(y=loc.lbl, x=smin, yend=loc.lbl, xend=smax, 
                     shape='range', linetype='range'), size=1.2) + 
    geom_point(aes(x=smean, shape='smean', linetype='smean'), size=4) +
    geom_point(aes(x=storm, shape='storm', linetype='storm'), size=4) + 
    ylab('') + theme_bw() + scale_x_log10(breaks = breaks, labels = breaks) +  
    xlab(paste('\n', poll.lab, ' (', unt.lab, ')\n', sep="")) + 
    theme(plot.margin = unit(c(.75, 0, 0, 0), "lines"),
          axis.text.y = element_text(size = 14), 
          axis.title = element_text(size = 18, face='bold'),
          legend.text =element_text(size = 14), 
          legend.key.size = unit(1.3, "cm"),
          legend.title = element_text(size=16, face = "bold", hjust=0)) + 
    guides(shape = guide_legend(override.aes = list(shape = c(NA, 19, 17)))) + 
        scale_linetype_manual(name = "Results", labels=c('Seasonal\nRange', 
                                                     'Seasonal\nMean', 'Storm\nSample'), values=c(1,0,0)) + 
    scale_shape_manual(name = "Results", labels=c('Seasonal\nRange', 
                                                  'Seasonal\nMean', 'Storm\nSample'), values=c(19, 19, 17))
  
  m.tmp <- met.cod[match(var, met.cod$metric_name), ]$metric_code
  if (m.tmp %in% std.lns$met.cod) {
    r.lin <- std.lns[match(m.tmp, std.lns$met.cod), ]$red.line
    if (var == 'dissolved oxygen') {
      m.tmp <- data.frame(x=c(8,11), l=c('solid', 'dashed'))
      p <- p + geom_vline(xintercept=8, linetype='solid', color='red', size=1.5) + 
        geom_vline(xintercept=11, linetype='dashed', color='red', size=1.5)
    } else {
    p <- p + geom_vline(aes(xintercept=r.lin), color='red', size=1.5)
  }}
  
  if (var=='e. coli') {
    p <- p + geom_vline(aes(xintercept=126), color='red', size=1.5, linetype=2)
  }
  
  print(p)
}