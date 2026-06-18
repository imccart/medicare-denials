# _main.R — master analysis driver (VRDC seat)
# Run from project root: source("code/analysis/vrdc/_main.R")
# No renv on the seat; packages load from the installed library.

pacman::p_load(tidyverse, data.table, modelsummary, kableExtra, fixest)

source("code/analysis/vrdc/1-build-panel.R")   # merge SAS exports + uploaded crosswalk -> analysis_panel
source("code/analysis/vrdc/2-event-study.R")   # Sun-Abraham event study + heterogeneity
