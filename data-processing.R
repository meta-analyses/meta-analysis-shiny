rm (list = ls())
library(tidyverse, warn.conflicts = FALSE)
library(plotly)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)


raw_data <- NULL

# Read the data from the meta-analysis repo
if(grepl('^/var/shiny/meta-analysis-shiny', getwd()) || grepl('/srv/shiny-server/meta-analyses-physical-activity', getwd())){
  # Set encoding as found at: https://stackoverflow.com/a/14363274
  # only for server
  raw_data <- read.csv("../meta-analysis/data/R_variables_March23.csv", fileEncoding="latin1", 
                       header = T, stringsAsFactors = F, skipNul = TRUE)
}else{
  raw_data <- read.csv("../meta-analysis/data/R_variables_March23.csv", header = T, 
                       stringsAsFactors = F, skipNul = TRUE)
}

local_pa_domain_subgroup <- "LTPA" 

local_last_knot <- 0.75
ALT <- FALSE
NO_BMI_EFFECT <- FALSE


raw_data$mean_followup <- as.numeric(raw_data$mean_followup)
raw_data$n_baseline <- as.numeric(raw_data$n_baseline)
#raw_data[is.na(raw_data$n_baseline),]$n_baseline <- 0

raw_data[(is.na(raw_data$tot_personyrs)),]$tot_personyrs <- 
  raw_data[(is.na(raw_data$tot_personyrs)),]$mean_followup * raw_data[(is.na(raw_data$tot_personyrs)),]$n_baseline
raw_data[(is.na(raw_data$mean_followup)),]$mean_followup <- 
  raw_data[(is.na(raw_data$mean_followup)),]$tot_personyrs / raw_data[(is.na(raw_data$mean_followup)),]$n_baseline

raw_data$outcome <- trimws(raw_data$outcome)
raw_data$pa_domain_subgroup <- trimws(raw_data$pa_domain_subgroup) 
raw_data$overall <- trimws(raw_data$overall) 
raw_data$sex_subgroups <- trimws(raw_data$sex_subgroups)
raw_data$outcome_type <- trimws(raw_data$outcome_type)

raw_data$totalpersons <- as.numeric(raw_data$n_per_category)
raw_data$personyrs <- as.numeric(raw_data$person_years_per_category)

raw_data[raw_data$effect_measure == "RR",]$effect_measure <- "rr"
raw_data[raw_data$effect_measure == "HR",]$effect_measure <- "hr"
raw_data[raw_data$effect_measure == "OR",]$effect_measure <- "or"


raw_data$type <- ""

raw_data[raw_data$effect_measure == "or",]$type <- "ir" 
raw_data[raw_data$effect_measure == "rr",]$type <- "ir"
raw_data[raw_data$effect_measure == "hr",]$type <- "ci"
raw_data$type <- as.character(raw_data$type)

raw_data[raw_data$outcome_type == "Both",]$outcome_type <- "Fatal-and-non-fatal"
raw_data$outcome_type <- as.character(raw_data$outcome_type)

## RENAME columns

raw_data <- raw_data %>% rename(effect = most_adj_effect, cases = cases_per_category,
                                uci_effect = most_adj_uci, lci_effect = most_adj_lci) %>% 
  mutate(cases = as.numeric(cases), uci_effect = as.numeric(uci_effect), lci_effect = as.numeric(lci_effect))



# ## FOR SENSITIVITY ANALYSIS
#raw_data$dose <- raw_data$Final.Harmonised.exposure..MMET.hrs.wk...FOR.SENSITIVITY.ANALYSIS

# MAIN <- TRUE

## FOR THE CURRENT ASSUMPTIONS
# raw_data$dose <- round(ifelse(ALT, raw_data$m_met_h_wk_alt, raw_data$m_met_h_wk), 2)
if (ALT){
  raw_data$dose <- round(raw_data$m_met_h_wk_alt, 2)
}else{
  raw_data$dose <- round(raw_data$m_met_h_wk, 2)
}

if (NO_BMI_EFFECT){
  raw_data$effect <- as.numeric(raw_data$no_bmi_effect)
  raw_data$lci_effect <- as.numeric(raw_data$no_bmi_lci)
  raw_data$uci_effect <- as.numeric(raw_data$no_bmi_uci)
}

#raw_data$Final.Harmonised.exposure..MMET.hrs.wk. <- NULL
raw_data$RR <- raw_data$effect

raw_data <- subset(raw_data, select = c(ref_number, first_author, outcome, outcome_type, sex_subgroups, type, n_baseline, totalpersons, tot_personyrs, personyrs,
                                        mean_followup, dose, RR, effect, effect_measure, lci_effect, uci_effect, cases, overall, pa_domain_subgroup))


## Populate missing totalpersons and personyrs

for (i in unique(raw_data$ref_number)){
  raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & !(is.na(raw_data$personyrs)) & !(is.na(raw_data$tot_personyrs)),]$totalpersons <-
    round(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & !(is.na(raw_data$personyrs)) & !(is.na(raw_data$tot_personyrs)),]$personyrs /
            sum(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & !(is.na(raw_data$personyrs)) & !(is.na(raw_data$tot_personyrs)),]$tot_personyrs)  *
            raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (is.na(raw_data$totalpersons)) & !(is.na(raw_data$personyrs)) & !(is.na(raw_data$tot_personyrs)),]$n_baseline)
  
  
  raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (!is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$personyrs <-
    round(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (!is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases /
            sum(raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (!is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$cases)  *
            raw_data[!is.na(raw_data$n_baseline) & raw_data$ref_number == i & (!is.na(raw_data$totalpersons)) & (is.na(raw_data$personyrs)) & !(is.na(raw_data$cases)),]$tot_personyrs)
}

raw_data$outcome <- stringi::stri_trans_totitle(raw_data$outcome, opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))

raw_data[raw_data$outcome == 'All-cause cvd',]$outcome <- 'All-cause CVD'

# Identify unique outcomes
uoutcome <- data.frame(outcome = as.character(unique(raw_data$outcome)))
uoutcome$outcome <- as.character(uoutcome$outcome)

# Sort
uoutcome$outcome <- sort(uoutcome$outcome)

# Remove the blank outcome
uoutcome <- dplyr::filter(uoutcome, outcome != "")

# All-cause mortality
# Cardiovascular diseases
# Coronary heart disease
# Stroke
# Breast cancer
# Colon cancer
# Endometrial cancer
# Lung cancer
# Total cancer
#uoutcome$outcome <- uoutcome[c(1, 3, 5, 9, 2, 4, 6, 8, 10, 7),]

overall_pop_tbles <- read_csv("../meta-analysis/data/csv/MA-DR/combined_tables.csv")

gender_pop_tbles <- read_csv("../meta-analysis/data/csv/MA-DR/combined_tables_by_gender.csv")


pa_exposure <- "LTPA"


#data$outcome <- stringi::stri_trans_totitle(data$outcome)

# Read the functions from the meta-analysis repo
source("../meta-analysis/script/all-functions.R")

# Filter by overall 1 for total population
raw_data_tp_ltpa <- subset(raw_data, pa_domain_subgroup == "LTPA" & overall == 1)

# Filter by sex subgroups for both men and female
raw_data_gsp_ltpa <- subset(raw_data, pa_domain_subgroup == "LTPA" & (sex_subgroups %in% c(1,2)))

for (i in 1:nrow(uoutcome)){
  if (!uoutcome$outcome[i] %in% c('Breast cancer','Endometrial cancer')){
    dat <- subset(raw_data_gsp_ltpa, outcome == uoutcome$outcome[i])
    uid <- unique(dat$ref_number)
    for (j in 1:length(uid)){
      dat1 <- subset(dat, ref_number == uid[j]) 
      usexgroups <- unique(dat1$sex_subgroups)
      if (length(usexgroups) == 1){
        # Remove single gender specific studies
        raw_data_gsp_ltpa <- subset(raw_data_gsp_ltpa, (ref_number != uid[j])) 
      }
    }
  }
}


## Create ref_number for men and women subgroups
## for total population
if (nrow(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]) > 0)
  raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
if (nrow(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]) > 0)
  raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number <- paste(raw_data_tp_ltpa[raw_data_tp_ltpa$overall != 1 & raw_data_tp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

# Create ID column
raw_data_tp_ltpa$id <- as.integer(as.factor(raw_data_tp_ltpa$ref_number))

raw_data_tp_ltpa <- plyr::arrange(raw_data_tp_ltpa, outcome)


raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number <- paste0(raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 1,]$ref_number, "-1")
raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number <- paste0(raw_data_gsp_ltpa[raw_data_gsp_ltpa$sex_subgroups == 2,]$ref_number, "-2")

# Create ID column
raw_data_gsp_ltpa$id <- as.integer(as.factor(raw_data_gsp_ltpa$ref_number))

raw_data_gsp_ltpa <- plyr::arrange(raw_data_gsp_ltpa, outcome)