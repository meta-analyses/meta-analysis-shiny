rm (list = ls())
# Read the data from the meta-analysis repo

# raw_data <- read.csv("../meta-analysis/data/20170623_MASTER_PA_Dose_Metananalysis_Data_Extraction.csv", header = T, stringsAsFactors = F, skipNul = TRUE)

raw_data <- NULL

if(grepl('^/var/shiny/meta-analysis-shiny', getwd()) || grepl('/srv/shiny-server/meta-analysis-shiny', getwd())){
  # Set encoding as found at: https://stackoverflow.com/a/14363274
  # only for server
  raw_data <- read.csv("../meta-analysis/data/20170905_MASTER_PA_Dose_Metananalysis_Data_Extraction.csv", fileEncoding="latin1", 
                       header = T, stringsAsFactors = F, skipNul = TRUE)
}else{
  raw_data <- read.csv("../meta-analysis/data/20170905_MASTER_PA_Dose_Metananalysis_Data_Extraction.csv", header = T, 
                       stringsAsFactors = F, skipNul = TRUE)
}

raw_data$tot_personyrs <- as.numeric(raw_data$tot_personyrs)
#raw_data[is.na(raw_data$tot_personyrs),]$tot_personyrs <- 0
raw_data$mean_followup <- as.numeric(raw_data$mean_followup)
raw_data$n_baseline <- as.numeric(raw_data$n_baseline)
#raw_data[is.na(raw_data$n_baseline),]$n_baseline <- 0

raw_data[(is.na(raw_data$tot_personyrs)),]$tot_personyrs <- 
  raw_data[(is.na(raw_data$tot_personyrs)),]$mean_followup * raw_data[(is.na(raw_data$tot_personyrs)),]$n_baseline
raw_data[(is.na(raw_data$mean_followup)),]$mean_followup <- 
  raw_data[(is.na(raw_data$mean_followup)),]$tot_personyrs / raw_data[(is.na(raw_data$mean_followup)),]$n_baseline

raw_data$outcome <- trimws(raw_data$outcome)

raw_data$outcome <- trimws(raw_data$outcome)
raw_data$pa_domain_subgroup <- trimws(raw_data$pa_domain_subgroup)
raw_data$overall <- trimws(raw_data$overall)
raw_data$sex_subgroups <- trimws(raw_data$sex_subgroups)

raw_data$totalpersons <- as.numeric(raw_data$totalpersons)
raw_data$personyrs <- as.numeric(raw_data$personyrs)

raw_data[raw_data$effect_measure == "RR",]$effect_measure <- "rr"
raw_data[raw_data$effect_measure == "HR",]$effect_measure <- "hr"

raw_data$type <- ""

raw_data[raw_data$effect_measure == "or",]$type <- "ir"
raw_data[raw_data$effect_measure == "rr",]$type <- "ir"
raw_data[raw_data$effect_measure == "hr",]$type <- "ci"
raw_data$type <- as.character(raw_data$type)


## RENAME columns

raw_data$cases <- as.numeric(raw_data$cases)

raw_data$dose <- raw_data$Final.Harmonised.exposure..MMET.hrs.wk.
raw_data$Final.Harmonised.exposure..MMET.hrs.wk. <- NULL
raw_data$rr <- raw_data$effect

raw_data <- subset(raw_data, select = c(ref_number, Author, outcome, outcome_type, pa_domain_subgroup, overall, sex_subgroups, effect_measure, type, n_baseline, totalpersons, tot_personyrs, personyrs,
                                        mean_followup, dose, rr, effect, uci_effect, lci_effect, cases))


## Populate missing totalpersons and personyrs

for (i in unique(raw_data$ref_number)){
  
  raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$totalpersons <-
    round(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases /
            sum(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases)  *
            raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$n_baseline)
  
  
  raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$personyrs <-
    round(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases /
            sum(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases)  *
            raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$tot_personyrs)
}


# Replace 'Heart failure' with 'heart failure'
raw_data[raw_data$outcome == 'Heart failure',]$outcome <- 'heart failure'

# Replace "CVD" with "Cardiovascular Disease"
raw_data[raw_data$outcome == "CVD",]$outcome <- "Cardiovascular Disease"

# Replace "CHD" with "Coronary Heart Disease"
raw_data[raw_data$outcome == "CHD",]$outcome <- "Coronary Heart Disease"

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(raw_data$outcome)))
uoutcome$outcome <- as.character(uoutcome$outcome)
# Sort
uoutcome$outcome <- sort(uoutcome$outcome)
# Remove the blank outcome
uoutcome <- dplyr::filter(uoutcome, outcome != "")


raw_data_tp_ltpa <- subset(raw_data, pa_domain_subgroup == "LTPA")
for (i in 1:nrow(uoutcome)){
  dat <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i])
  for (j in 1:length(unique(dat$ref_number))){
    dat1 <- subset(dat, ref_number == (unique(dat$ref_number))[j]) 
    uoverall <- unique(dat1$overall)
    usexgroups <- unique(dat1$sex_subgroups)
    if (length(usexgroups) > 2 && length(uoverall) > 1){
      # Remove gender specific from total_population
      raw_data_tp_ltpa <- subset(raw_data_tp_ltpa, (ref_number == unique(dat1$ref_number) & sex_subgroups != c(1,2)) | (ref_number != unique(dat1$ref_number))) 
      #cat(uoutcome$outcome[i], " - ", unique(dat1$ref_number), " - ", unique(dat1$overall), " - ", unique(dat1$sex_subgroups), "\n")
    }
    
  }
}

raw_data_gsp_ltpa <- subset(raw_data, pa_domain_subgroup == "LTPA" & (sex_subgroups %in% c(1,2)))

for (i in 1:nrow(uoutcome)){
  if (!i %in% c(2,6)){
    dat <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i])
    for (j in 1:length(unique(dat$ref_number))){
      dat1 <- subset(dat, ref_number == (unique(dat$ref_number))[j]) 
      usexgroups <- unique(dat1$sex_subgroups)
      if (length(usexgroups) == 1){
        # Remove single gender specific studies
        raw_data_gsp_ltpa <- subset(raw_data_gsp_ltpa, 
                                    (ref_number == unique(dat1$ref_number) & sex_subgroups == usexgroups) | 
                                      (ref_number != unique(dat1$ref_number))) 
        #cat(uoutcome$outcome[i], " - ", unique(dat1$ref_number), " - ", unique(dat1$overall), " - ", unique(dat1$sex_subgroups), "\n")
      }
    }
  }
}


## Create ref_number for men and women subgroups
## for total population
raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

raw_data_tp_ltpa <- plyr::arrange(raw_data_tp_ltpa, outcome)


raw_data_gsp_ltpa[raw_data_gsp_ltpa$overall != 1 & raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number <- paste(raw_data_gsp_ltpa[raw_data_gsp_ltpa$overall != 1 & raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
raw_data_gsp_ltpa[raw_data_gsp_ltpa$overall != 1 & raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number <- paste(raw_data_gsp_ltpa[raw_data_gsp_ltpa$overall != 1 & raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

raw_data_gsp_ltpa <- plyr::arrange(raw_data_gsp_ltpa, outcome)

#data$outcome <- stringi::stri_trans_totitle(data$outcome)

# Read the functions from the meta-analysis repo
source("../meta-analysis/all-functions.R")
