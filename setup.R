pkgs <- c(
  "DT",
  "devtools",
  "shinyBS",
  "shinyjs",
  "dplyr",  
  "plyr",  
  "ggplot2",
  "stringr",
  "Hmisc",
  "rCharts"
)
# Which packages do we require?
# lapply(pkgs, library, character.only = T)
reqs <- as.numeric(lapply(pkgs, require, character.only = TRUE))

# Committing all the installation packages
# # Install packages we require
# if(sum(!reqs) > 0) install.packages(pkgs[!reqs])
# 
# if(!require(rCharts)) devtools::install_github("rCharts", "ramnathv")
# 
# if(!require(shiny)) devtools::install_github("shiny", "rstudio")

source("data-processing.R")
# Functions
#source("functions.R")