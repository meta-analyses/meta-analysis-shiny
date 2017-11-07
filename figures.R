source("setup.R")

total_population <- T
local_last_knot <- 0.75

if (total_population){
  for (i in c(1,3,10)){#:nrow(uoutcome)){
    # i <- 1
    cat("Total Population - Outcome: ", uoutcome$outcome[i], " and i ", i, "\n")
    acmfdata <- subset(raw_data_tp_ltpa, outcome == uoutcome$outcome[i] & pa_domain_subgroup == "LTPA" & outcome_type == "mortality")
    acmfdata <- subset(acmfdata, n_baseline >= 10000)
    local_cov_method <- F
    if (i == 3 || i == 4) local_cov_method <- T
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      acmfdata <- subset(acmfdata, select = c(id, ref_number, Author, effect_measure, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
      last_knot <- get_last_knot(acmfdata, dose_pert = local_last_knot , personyrs_pert = local_last_knot)
      # cat("dose_pert and person_years_pert ", last_knot, "\n")
      last_knot <- last_knot[2]
      if (nrow(acmfdata) > 0){
        dataset <- acmfdata
        q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
        if (!is.null(dataset)){
          dataset$personyrs <- round(dataset$personyrs)
          group_by(dataset, id) %>% select(dose, se) %>%
            summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
          # pa <- acmfdata
          
          dataset2 <- data.frame(metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = local_cov_method, minQuantile = 0, maxQuantile = last_knot, lout = 1000))
          colnames(dataset2) <- c("dose","RR", "lb", "ub")
          
          #obj <- metaAnalysis(acmfdata, returnval = T, ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
          #dataset2 <- data.frame(dose = obj[1], RR = as.data.frame(obj[2])[1], lb = as.data.frame(obj[2])[2], ub = as.data.frame(obj[2])[3])
          # colnames(dataset2) <- c("dose","RR", "lb", "ub")
          plotTitle <- paste0( uoutcome$outcome[i] ,  " - Total Population")
          if (i != 1)
            plotTitle <- paste0( uoutcome$outcome[i] ,  " - Mortality - Total Population")
          
          plotTitle <-  paste0(simpleCap(plotTitle), ' \nNumber of entries: ',
                               length(unique(acmfdata$id)),
                               ' \nNumber of people: ' , round(sum(acmfdata$totalpersons)))#, " ", local_last_knot)
          
          cat(min(dataset$RR), " - ", max(dataset$RR), "\n")
          
          p <- ggplot() +
            geom_line(data = dataset, aes(dose, RR, col = factor(ref_number), label = personyrs)) +
            geom_point(data = dataset, aes(dose, RR, col = factor(ref_number)), size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
            geom_line(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, y = RR), size = 0.8) +
            geom_line(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, y = RR), size = 0.8, linetype = "dashed") +
            geom_ribbon(data = subset(dataset2, dose < as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
            geom_ribbon(data = subset(dataset2, dose >= as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.10) +
            geom_vline(xintercept= q, linetype="dotted", alpha = 0.6) +
            scale_x_continuous(expand = c(0, 0),
                               breaks = seq(from = 0, to = 80, by = 10)) + 
            scale_y_continuous(expand = c(0, 0),
                               breaks = seq(from = 0, to = max(dataset$RR) + 0.2, by = 0.2)) + #,
                               #limits = c(0, NA)) +
            coord_cartesian(ylim = c(0, max(dataset$RR) + 0.2)) +
                             # xlim = c(0, max(dataset$dose) + 3)) + 
            theme(legend.position="none",
                  plot.title = element_text(hjust = 0.5)) +
            xlab("\nMarginal MET hours per week\n") +
            ylab("\nRelative Risk\n") +
            labs(title = paste(plotTitle))
          print(p)
          ggsave(paste0(uoutcome$outcome[i], "-mortality", ".png"), height=5, width=10, units='in', dpi=600, scale = 1)
        }
      }
    }
  }
}