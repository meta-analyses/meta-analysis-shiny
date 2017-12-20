source("setup.R")

total_population <- T
sub_population <- F
local_dpi <- 600
g <- 1
local_last_knot <- 0.75
fpath <- "output/main/"

low_quantile <- F

if (low_quantile){
  local_last_knot <- 0.85
  fpath <- "output/85/"
}

conservative <- F

if (conservative){
  
  ## CHANGE variable name in data-processing.R
  fpath <- "output/conservative/"
}

cat(fpath, "\n")

## FUNCTIONS

get_overall_data <- function (PA_exposure, outcome_disease, outcome_types){
  
  acmfdata <- data.frame()
  
  if (!is.na(outcome_disease) && outcome_types != 'all'){
    acmfdata <- subset(raw_data_tp_ltpa, 
                       outcome == outcome_disease & 
                         pa_domain_subgroup == PA_exposure & 
                         outcome_type == outcome_types)
  }else{
    acmfdata <- subset(raw_data_tp_ltpa, 
                       outcome == outcome_disease & 
                         pa_domain_subgroup == PA_exposure)
    
    
  }
  
  if (nrow(acmfdata) > 0){
    acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
    # Filter studies by study size
    acmfdata <- subset(acmfdata, n_baseline >= 10000)
    
  }
  
  # td <<- acmfdata
  acmfdata
  
}


get_subpopulation_data <- function (PA_exposure, outcome_disease, outcome_types, gender){
  
  acmfdata <- data.frame()
  if (outcome_types != 'all'){
    acmfdata <- subset(raw_data_gsp_ltpa, 
                       outcome == outcome_disease & 
                         pa_domain_subgroup == PA_exposure & 
                         sex_subgroups == gender &
                         outcome_type == outcome_types)
  }else{
    acmfdata <- subset(raw_data_gsp_ltpa, 
                       outcome == outcome_disease & 
                         pa_domain_subgroup == PA_exposure & 
                         sex_subgroups == gender)#input$in_sub_population)
    
  }
  
  # Remove where both dose and response are null
  acmfdata <- subset(acmfdata, !is.na(RR) & !is.na(dose))
  
  if (nrow(acmfdata) > 0){
    acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
  }
  
  acmfdata
}



get_ma_table <- function(plot_data, colname = "RR"){
  
  c(round(plot_data[[colname]][which.min(abs(plot_data$dose - 4.375))], 2),
    round(plot_data[[colname]][which.min(abs(plot_data$dose - 8.75))], 2),
    round(plot_data[[colname]][which.min(abs(plot_data$dose - 17.5))], 2))#,
}

get_pif_values <- function(dataset, outcome, last_knot, dose_value){
  # last_df <<- dataset
  # last_outcome <<- outcome
  # last_lastknot <<- last_knot
  # last_dvalue <<- dose_value
  # 
  # 
  # dataset <- last_df
  # outcome <- last_outcome
  # last_knot <- last_lastknot 
  # dose_value <- last_dvalue
  
  
  cat( "outcome ", outcome,  " and dose value " , dose_value, "\n")
  if (nrow(dataset) > 0){
    if (max(dataset$dose) < dose_value)
      return(0)
  }
  
  if (nrow(dataset) > 0){
    
    local_cov_method <- F
    
    if (outcome == "Coronary heart disease" || outcome == "Cardiovascular disease" || outcome == "stroke")
      local_cov_method <- T
    
    plot_data <- data.frame(metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = local_cov_method, maxQuantile = last_knot, lout = 1000))
    # browser()
    colnames(plot_data) <- c("dose","RR", "lb", "ub")
    
    removeNA <- F
    
    # Update RR from the lookup table
    for (i in 1:nrow(dataset)){
      val_rr <- plot_data[["RR"]][which.min(abs(plot_data$dose - dataset$dose[i]))]
      val <- subset(plot_data, RR == val_rr)
      if (nrow(val) > 0){
        dataset$RR[i] <- val$RR[1]
        if (removeNA){
          if (!is.na(dataset$lci[i]))
            dataset$lci[i] <- val$lb[1]
          if (!is.na(dataset$lci[i]))
            dataset$uci[i] <- val$ub[1]
        }else{
          dataset$lci[i] <- val$lb[1]
          dataset$uci[i] <- val$ub[1]
        }
      }
    }
    
    sum_tp <- sum(dataset$totalpersons * dataset$RR, na.rm = T)
    
    dataset_ls <- dataset
    
    #Replace lower dose with 8.75
    dataset_ls[dataset_ls$dose < dose_value,]$dose <- dose_value
    
    local_var <- dataset_ls
    
    #val <- subset(plot_data, round(dose, 1) <= (dose_value + 0.05) & round(dose, 1) >= (dose_value - 0.05))
    val_rr <- plot_data[["RR"]][which.min(abs(plot_data$dose - dose_value))]
    val <- subset(plot_data, RR == val_rr)
    
    if (nrow(val) > 0)
      dataset_ls[dataset_ls$dose == dose_value,]$RR <- val$RR[1]
    
    sum_ls_tp <- sum(dataset$totalpersons * dataset_ls$RR, na.rm = T)
    
    pert_ls <- ((sum_tp - sum_ls_tp) / sum_tp) * 100
    
    dataset_ls <- local_var
    
    if (nrow(val) > 0){
      
      if (removeNA){
        dataset_ls[dataset_ls$dose == dose_value & !is.na(dataset_ls$uci),]$uci <- val$ub[1]
      }else{
        dataset_ls[dataset_ls$dose == dose_value,]$uci <- val$ub[1]
      }
      
    }
    
    sum_ls_lower_tp <- sum(dataset$totalpersons * dataset_ls$uci, na.rm = T)
    
    sum_tp <- sum(dataset$totalpersons * dataset$uci, na.rm = T)
    
    pert_ls_lower <- ((sum_tp - sum_ls_lower_tp) / sum_tp) * 100
    
    dataset_ls <- local_var
    
    if (nrow(val) > 0){
      if (removeNA){
        dataset_ls[dataset_ls$dose == dose_value & !is.na(dataset_ls$uci),]$lci <- val$lb[1]
      }else{
        dataset_ls[dataset_ls$dose == dose_value,]$lci <- val$lb[1]
      }
    }
    
    sum(dataset_ls$lci, na.rm = T)
    
    sum(dataset$lci, na.rm = T)
    
    sum_ls_upper_tp <- sum(dataset$totalpersons * dataset_ls$lci, na.rm = T)
    
    sum_tp <- sum(dataset$totalpersons * dataset$lci, na.rm = T)
    
    pert_ls_upper <- ((sum_tp - sum_ls_upper_tp) / sum_tp) * 100
    
    lower_guideline_value <- paste0(round(pert_ls, 1) , "% (", round(pert_ls_lower, 1), " - ",  round(pert_ls_upper, 1), ")" )
    
  }
  
}
# mortality figures

tab_data_mortality <- data.frame()
tab_data_incidence <- data.frame()

if (total_population){
  for (i in 1:nrow(uoutcome)){ 
    if (uoutcome$outcome[i] %in% c('All-cause mortality','Cardiovascular disease', 'Total cancer', 'Coronary heart disease', 'Stroke')){
      
      local_loop_last_knot <- local_last_knot
      if (uoutcome$outcome[i] == "Coronary heart disease")
        local_loop_last_knot <- 0.77
      
      cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
      acmfdata <- get_overall_data(PA_exposure = "LTPA", outcome_disease = uoutcome$outcome[i], 
                                   outcome_types = "mortality")
      local_cov_method <- F
      if (uoutcome$outcome[i] == "Coronary heart disease" || uoutcome$outcome[i] == "Cardiovascular disease" || uoutcome$outcome[i] == "Stroke")
        local_cov_method <- T
      
      if (nrow(acmfdata) > 0){
        acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
        last_knot <- get_last_knot(acmfdata, dose_pert = local_loop_last_knot , personyrs_pert = local_loop_last_knot)
        last_knot <- last_knot[2]
        if (nrow(acmfdata) > 0){
          dataset <- acmfdata
          q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
          if (!is.null(dataset)){
            dataset$personyrs <- round(dataset$personyrs)
            group_by(dataset, id) %>% select(dose, se) %>%
              summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
            #plot_data <- data.frame(metaAnalysis(overall_data, ptitle = "", returnval = T, covMethed = local_cov_method, maxQuantile = last_knot))
            dataset2 <- data.frame(metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = local_cov_method, minQuantile = 0, maxQuantile = last_knot, lout = 1000))
            colnames(dataset2) <- c("dose","RR", "lb", "ub")
            
            plotTitle <- paste0( uoutcome$outcome[i] ,  " - Total Population")
            if (uoutcome$outcome[i] != 'All-cause mortality')
              plotTitle <- paste0( uoutcome$outcome[i] ,  " - Mortality - Total Population")
            
            plotTitle <-  paste0(simpleCap(plotTitle), ' \nNumber of entries: ',
                                 length(unique(acmfdata$id)),
                                 ' \nNumber of people: ' , formatC(round(sum(acmfdata$totalpersons, na.rm = T)), 
                                                                   format = "f", big.mark = ",", drop0trailing = TRUE))
            
            cat("highest val ", ifelse(i == 1, min(dataset2$ub), max(dataset$dose)), "\n")
            
            p <- ggplot() +
              geom_line(data = dataset, aes(dose, RR, col = factor(ref_number), label = personyrs)) +
              geom_point(data = dataset, aes(dose, RR, col = factor(ref_number)), size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
              geom_line(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, y = RR), size = 0.8) +
              geom_line(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, y = RR), size = 0.8, linetype = "dashed") +
              geom_ribbon(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
              geom_ribbon(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.10) +
              geom_vline(xintercept= q, linetype="dotted", alpha = 0.6) +
              scale_x_continuous(expand = c(0, 0),
                                 breaks = seq(from = 0, to = 80, by = 10) ,
                                 limits = c(0, max(dataset$dose) + 10)) + 
              scale_y_continuous(expand = c(0, 0),
                                 breaks = seq(from = 0, to = ifelse(i == 1, max(dataset2$ub), max(dataset$RR)) + 0.2, by = 0.2)) + #,
              #limits = c(0, NA)) +
              coord_cartesian(ylim = c(0, ifelse(uoutcome$outcome[i] == 'Cardiovascular disease', max(dataset2$ub), max(dataset$RR)) + 0.2), expand = T) +
              # xlim = c(0, max(dataset$dose) + 3)) + 
              theme_classic() + theme(
                legend.position="none",
                plot.title = element_text(hjust = 0.5)) +
              xlab("\nMarginal MET hours per week\n") +
              ylab("\nRelative Risk\n") +
              labs(title = paste(plotTitle))
            print(p)
            ggsave(paste0(fpath, uoutcome$outcome[i], "-mortality", ".png"), height=5, width=10, units='in', dpi = local_dpi, scale = 1)
            
            if (nrow(tab_data_mortality) == 0){
              tab_data_mortality <- data.frame(MMET = c(4.375, 8.75, 17.5), RR = paste(get_ma_table(dataset2, "RR"), " (", get_ma_table(dataset2, "lb"),
                                                                                       " - ", get_ma_table(dataset2, "ub"), ")", sep = ""),
                                               PIF = c(get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 4.375),
                                                       get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 8.75),
                                                       get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 17.5))
                                               
              )
              colnames(tab_data_mortality) <- c("MMETh",paste("RR", uoutcome$outcome[i]), paste("PIF", uoutcome$outcome[i]))
              
            }else{
              dat <- data.frame(RR = paste(get_ma_table(dataset2, "RR"), " \n (", get_ma_table(dataset2, "lb"),
                                           " - ", get_ma_table(dataset2, "ub"), ")", sep = ""),
                                PIF = c(get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 4.375),
                                        get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 8.75),
                                        get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 17.5))
                                
              )
              
              colnames(dat) <- c(paste("RR", uoutcome$outcome[i]), paste("PIF", uoutcome$outcome[i]))
              
              tab_data_mortality <- cbind(tab_data_mortality, dat)
            }
            
          }
          
          
        }
      }
    }
  }
}

# Incidence :
# 1)	CVD
# 2)	Coronary Heart Disease
# 3)	Stroke
# 
# 4)	All  Cancers 
# 5)	Each Individual Cancer (conditional on accrual of sufficient studies)

# incidence figures

if (total_population){
  for (i in c(2, 3, 4, 9, 5, 7, 6, 8)){#:nrow(uoutcome)){
    cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- get_overall_data(PA_exposure = "LTPA", outcome_disease = uoutcome$outcome[i], 
                                 outcome_types = "incidence")
    local_cov_method <- F
    if (uoutcome$outcome[i] == "Coronary heart disease" || uoutcome$outcome[i] == "Cardiovascular disease" || uoutcome$outcome[i] == "Stroke")
      local_cov_method <- T
    
    local_loop_last_knot <- local_last_knot
    if (conservative && uoutcome$outcome[i] == "Colon cancer")
      local_loop_last_knot <- 0.82
    
    if (nrow(acmfdata) > 0){
      acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
      last_knot <- get_last_knot(acmfdata, dose_pert = local_loop_last_knot , personyrs_pert = local_loop_last_knot)
      last_knot <- last_knot[2]
      if (nrow(acmfdata) > 0){
        dataset <- acmfdata
        q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
        if (!is.null(dataset)){
          dataset$personyrs <- round(dataset$personyrs)
          group_by(dataset, id) %>% select(dose, se) %>%
            summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
          dataset2 <- data.frame(metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = local_cov_method, minQuantile = 0, maxQuantile = last_knot, lout = 1000))
          colnames(dataset2) <- c("dose","RR", "lb", "ub")

          plotTitle <- paste0( uoutcome$outcome[i] ,  " - Total Population")
          if (uoutcome$outcome[i] != "All-cause mortality")
            plotTitle <- paste0( uoutcome$outcome[i] ,  " - Incidence - Total Population")

          plotTitle <-  paste0(simpleCap(plotTitle), ' \nNumber of entries: ',
                               length(unique(acmfdata$id)),
                               ' \nNumber of people: ' , formatC(round(sum(acmfdata$totalpersons, na.rm = T)),
                                                                 format = "f", big.mark = ",", drop0trailing = TRUE))

          p <- ggplot() +
            geom_line(data = dataset, aes(dose, RR, col = factor(ref_number), label = personyrs)) +
            geom_point(data = dataset, aes(dose, RR, col = factor(ref_number)), size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
            geom_line(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, y = RR), size = 0.8) +
            geom_line(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, y = RR), size = 0.8, linetype = "dashed") +
            geom_ribbon(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
            geom_ribbon(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.10) +
            geom_vline(xintercept= q, linetype="dotted", alpha = 0.6) +
            scale_x_continuous(expand = c(0, 0),
                               breaks = seq(from = 0, to = 80, by = 10) ,
                               limits = c(0, max(dataset$dose) + 10)) +
            scale_y_continuous(expand = c(0, 0),
                               breaks = seq(from = 0, to = max(dataset$RR) + 0.2, by = 0.2)) +
            coord_cartesian(ylim = c(0, max(dataset$RR) + 0.2), expand = T) +
            theme_classic() + theme(
              legend.position="none",
              plot.title = element_text(hjust = 0.5)) +
            xlab("\nMarginal MET hours per week\n") +
            ylab("\nRelative Risk\n") +
            labs(title = paste(plotTitle)) 
            # + annotate("segment", x = 3, xend = 5 , y= 0.5, yend = 0.8, arrow = arrow(), label = "Some text")
          print(p)
          ggsave(paste0(fpath, uoutcome$outcome[i], "-incidence", ".png"), height=5, width=10, units='in', dpi = local_dpi, scale = 1)
          
          if (nrow(tab_data_incidence) == 0){
            tab_data_incidence <- data.frame(MMET = c(4.375, 8.75, 17.5), RR = paste(get_ma_table(dataset2, "RR"), " (", get_ma_table(dataset2, "lb"),
                                                                           " - ", get_ma_table(dataset2, "ub"), ")", sep = ""),
                                   PIF = c(get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 4.375),
                                           get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 8.75),
                                           get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 17.5))
                                   
            )
            colnames(tab_data_incidence) <- c("MMETh",paste("RR", uoutcome$outcome[i]), paste("PIF", uoutcome$outcome[i]))
            
          }else{
            dat <- data.frame(RR = paste(get_ma_table(dataset2, "RR"), " (", get_ma_table(dataset2, "lb"),
                                         " - ", get_ma_table(dataset2, "ub"), ")", sep = ""),
                              PIF = c(get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 4.375),
                                      get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 8.75),
                                      get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 17.5))
                              
            )
            
            colnames(dat) <- c(paste("RR", uoutcome$outcome[i]), paste("PIF", uoutcome$outcome[i]))
            
            tab_data_incidence <- cbind(tab_data_incidence, dat)
          }
        }
      }
    }
  }
}

if (!sub_population){
  write.csv(tab_data_incidence, file = paste(fpath, "tab_data_incidence.csv"), row.names = F)
  write.csv(tab_data_mortality, file = paste(fpath, "tab_data_mortality.csv"), row.names = F)
}

# Mortality: all-cause and CVD.
# Incidence: colon cancer.

if (sub_population){
  for (i in 1:nrow(uoutcome)){
    if (uoutcome$outcome[i] %in% c('All-cause mortality','Cardiovascular disease', 'Colon cancer')){
      pop <- "Male"
      if (g == 2)
        pop <- "Female"
      cat(pop, " Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
      local_outcome_type <- "mortality"
      if (uoutcome$outcome[i] == "Colon cancer")
        local_outcome_type <- "incidence"
      
      local_loop_last_knot <- local_last_knot
      if (uoutcome$outcome[i] == "Colon cancer" && g == 2)
        local_loop_last_knot <- 0.78
      acmfdata <- get_subpopulation_data(PA_exposure = "LTPA", outcome_disease = uoutcome$outcome[i],
                                         outcome_types = local_outcome_type, gender = g)
      local_cov_method <- F
      if (uoutcome$outcome[i] == "Coronary heart disease" || uoutcome$outcome[i] == "Cardiovascular disease" || uoutcome$outcome[i] == "Stroke")
        local_cov_method <- T

      if (nrow(acmfdata) > 0){
        acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
        last_knot <- get_last_knot(acmfdata, dose_pert = local_loop_last_knot , personyrs_pert = local_loop_last_knot)
        last_knot <- last_knot[2]
        if (nrow(acmfdata) > 0){
          dataset <- acmfdata
          q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
          if (!is.null(dataset)){
            dataset$personyrs <- round(dataset$personyrs)
            group_by(dataset, id) %>% select(dose, se) %>%
              summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
            dataset2 <- data.frame(metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = local_cov_method, minQuantile = 0, maxQuantile = last_knot, lout = 1000))
            colnames(dataset2) <- c("dose","RR", "lb", "ub")

            plotTitle <- paste0( uoutcome$outcome[i] ,  " - ", pop , " Population")
            if (!uoutcome$outcome[i] == 'All-cause mortality')
              plotTitle <- paste0( uoutcome$outcome[i] ,  " - ", tools::toTitleCase(local_outcome_type), " - ", pop ," Population")

            plotTitle <-  paste0(simpleCap(plotTitle), ' \nNumber of entries: ',
                                 length(unique(acmfdata$id)),
                                 ' \nNumber of people: ' , formatC(round(sum(acmfdata$totalpersons, na.rm = T)),
                                                                   format = "f", big.mark = ",", drop0trailing = TRUE))

            cat("highest val ", ifelse(i == 1, min(dataset2$ub), max(dataset$dose)), "\n")

            p <- ggplot() +
              geom_line(data = dataset, aes(dose, RR, col = factor(ref_number), label = personyrs)) +
              geom_point(data = dataset, aes(dose, RR, col = factor(ref_number)), size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
              geom_line(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, y = RR), size = 0.8) +
              geom_line(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, y = RR), size = 0.8, linetype = "dashed") +
              geom_ribbon(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
              geom_ribbon(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.10) +
              geom_vline(xintercept= q, linetype="dotted", alpha = 0.6) +
              scale_x_continuous(expand = c(0, 0),
                                 breaks = seq(from = 0, to = 80, by = 10) ,
                                 limits = c(0, max(dataset$dose) + 10)) +
              scale_y_continuous(expand = c(0, 0),
                                 breaks = seq(from = 0, to = ifelse(i == 1, max(dataset2$ub), max(dataset$RR)) + 0.2, by = 0.2)) + #,
              #limits = c(0, NA)) +
              coord_cartesian(ylim = c(0, ifelse(i == 1, max(dataset2$ub), max(dataset$RR)) + 0.2), expand = T) +
              theme_classic() + 
              theme(
                legend.position="none",
                plot.title = element_text(hjust = 0.5)) +
              xlab("\nMarginal MET hours per week\n") +
              ylab("\nRelative Risk\n") +
              labs(title = paste(plotTitle))
            #p
            print(p)
            ggsave(paste0(fpath, pop, "-", uoutcome$outcome[i], "-", local_outcome_type, ".png"), height=5, width=10, units='in', dpi = local_dpi, scale = 1)

            if (nrow(tab_data_mortality) == 0){
              tab_data_mortality <- data.frame(MMET = c(4.375, 8.75, 17.5), RR = paste(get_ma_table(dataset2, "RR"), " (", get_ma_table(dataset2, "lb"),
                                                                                       " - ", get_ma_table(dataset2, "ub"), ")", sep = ""),
                                               PIF = c(get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 4.375),
                                                       get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 8.75),
                                                       get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 17.5))

              )
              colnames(tab_data_mortality) <- c("MMETh",paste("RR", uoutcome$outcome[i]), paste("PIF", uoutcome$outcome[i]))

            }else{
              dat <- data.frame(RR = paste(get_ma_table(dataset2, "RR"), " \n (", get_ma_table(dataset2, "lb"),
                                           " - ", get_ma_table(dataset2, "ub"), ")", sep = ""),
                                PIF = c(get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 4.375),
                                        get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 8.75),
                                        get_pif_values(dataset = acmfdata, uoutcome$outcome[i], last_knot = last_knot , dose_value = 17.5))

              )

              colnames(dat) <- c(paste("RR", uoutcome$outcome[i]), paste("PIF", uoutcome$outcome[i]))

              tab_data_mortality <- cbind(tab_data_mortality, dat)
            }



          }
        }
      }
    }
  }
}

if (sub_population){
  write.csv(tab_data_mortality, file = paste(fpath, g, "tab_data_mortality.csv"), row.names = F)
}



## CREATE DOSE TABLE

if (total_population){
  
  dat <- subset(raw_data_tp_ltpa, outcome != "Heart failure")
}

