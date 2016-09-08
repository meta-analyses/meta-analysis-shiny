pkgs <- c(
  "DT",
  "devtools",
  "shinyjs",
  "ggplot2",
  "rCharts"
)
# Which packages do we require?
# lapply(pkgs, library, character.only = T)
reqs <- as.numeric(lapply(pkgs, require, character.only = TRUE))

# Load data
source("data-processing.R")