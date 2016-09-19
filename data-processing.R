rm (list = ls())
# Read the data from the meta-analysis repo
data <- read.csv("../meta-analysis/data/09.06_COMBINED DATASET REDUCED.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

data$tot_personyrs <- as.numeric(data$tot_personyrs)
data[(is.na(data$tot_personyrs)),]$tot_personyrs <- 
  data[(is.na(data$tot_personyrs)),]$mean_followup * data[(is.na(data$tot_personyrs)),]$n_baseline
data[(is.na(data$mean_followup)),]$mean_followup <- 
  data[(is.na(data$mean_followup)),]$tot_personyrs / data[(is.na(data$mean_followup)),]$n_baseline

data$outcome <- trimws(data$outcome)

# Replace 'Heart failure' with 'heart failure'
data[data$outcome == 'Heart failure',]$outcome <- 'heart failure'

# Replace "Alzheimer's Disease" with "Alzheimer's disease"
data[data$outcome == "Alzheimer's Disease",]$outcome <- "Alzheimer's disease"

# Read the functions from the meta-analysis repo
source("../meta-analysis/all-functions.R")

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(data$outcome)))
uoutcome$outcome <- as.character(uoutcome$outcome)