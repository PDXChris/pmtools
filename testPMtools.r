library(devtools)
install_github('PDXChris/pmtools')

library(pmtools)
mods <- pmtools::runMetMixMod(wq14, 'metric_name')
saveMMMranEff(mods, plot = T)
plotDens(wq14[wq14$metric_code == 'zn', ], 'result', xtrn='log10')

