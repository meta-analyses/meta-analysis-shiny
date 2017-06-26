rm (list = ls())
# Read the data from the meta-analysis repo
raw_data <- read.csv("../meta-analysis/data/09.06_COMBINED DATASET REDUCED.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

raw_data$tot_personyrs <- as.numeric(raw_data$tot_personyrs)
raw_data[(is.na(raw_data$tot_personyrs)),]$tot_personyrs <- 
  raw_data[(is.na(raw_data$tot_personyrs)),]$mean_followup * raw_data[(is.na(raw_data$tot_personyrs)),]$n_baseline
raw_data[(is.na(raw_data$mean_followup)),]$mean_followup <- 
  raw_data[(is.na(raw_data$mean_followup)),]$tot_personyrs / raw_data[(is.na(raw_data$mean_followup)),]$n_baseline

raw_data$outcome <- trimws(raw_data$outcome)

# Replace 'Heart failure' with 'heart failure'
raw_data[raw_data$outcome == 'Heart failure',]$outcome <- 'heart failure'

# Replace "Alzheimer's Disease" with "Alzheimer's disease"
raw_data[raw_data$outcome == "Alzheimer's Disease",]$outcome <- "Alzheimer's disease"

# Replace "CVD" with "Cardiovascular Disease"
raw_data[raw_data$outcome == "CVD",]$outcome <- "Cardiovascular Disease"

# Replace "CHD" with "Coronary Heart Disease"
raw_data[raw_data$outcome == "CHD",]$outcome <- "Coronary Heart Disease"

raw_data <- plyr::arrange(raw_data, outcome)

#data$outcome <- stringi::stri_trans_totitle(data$outcome)

# Read the functions from the meta-analysis repo
source("../meta-analysis/all-functions.R")

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(raw_data$outcome)))
uoutcome$outcome <- as.character(uoutcome$outcome)