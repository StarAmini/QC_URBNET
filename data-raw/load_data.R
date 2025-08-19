# Run in the package main directory to import example data and metadata

library(devtools)

Bern <- readRDS("data-raw/Bern2024.RDS")

use_data(Bern, overwrite = TRUE)
