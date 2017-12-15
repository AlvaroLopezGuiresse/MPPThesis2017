# Intialisation
rm(list = ls())
pkgs <- c('dplyr', 'magrittr', 'methods', 'rvest', 'stringi', 'rio',
  'plm', 'knitr', 'rmarkdown', 'stargazer', 'rgdal', 'ggplot2', 'ggmap', 'viridis',
  'scales', 'tidyr', 'broom', 'lmtest', 'GGally')

for (p in pkgs) {
  load <- require(p, character.only = T, quietly = T)
}
rm(pkgs)
