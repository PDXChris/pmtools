ggBiplotDot <- function(PC, DF, dot, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- cbind(DF, PC$x)
  plt <- ggplot(data, aes_string(x=x, y=y)) + 
    geom_point(alpha=.5, aes_string(size=dot)) + 
    scale_size_continuous(range = c(2, 12))
  
  plt <- plt + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plt <- plt + coord_equal() + geom_text(data=datapc, aes(x=v1*1.1, y=v2*1.1, label=varnames), 
                                           size = 5, vjust=1, color="red")
  plt <- plt + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), 
                              arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  plt
}