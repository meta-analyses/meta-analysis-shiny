options(shiny.sanitize.errors = F)
lower_guideline_value <- NA
upper_guideline_value <- NA

to_download <- NULL
shinyServer(function(input, output, session){
  
  # To set initialize to_download
  observe({
    to_download$top_plot_data <<- NULL
    to_download$bottom_plot_data <<- NULL
  })
  
  get_overall_data <- function (PA_exposure, outcome_disease, outcome_types){
    
    acmfdata <- data.frame()
    
    if (!is.na(outcome_disease)){
      
      # Subset according to outcome, domain and outcome type
      acmfdata <- subset(raw_data_tp_ltpa, 
                         outcome == outcome_disease & 
                           pa_domain_subgroup == PA_exposure & 
                           outcome_type == outcome_types)
      
      # Add additional "fatal" studies that had no "both" types
      if (outcome_types == "Both") {
        # Subset fatal types
        add_fdata <- subset(raw_data_tp_ltpa, outcome == outcome_disease & 
                              pa_domain_subgroup == local_pa_domain_subgroup & 
                              outcome_type == "Fatal")
        # ONLY add those studies that have no "both" studies
        add_fdata <- subset(add_fdata, !id %in% acmfdata$id)
        # Add additional rows
        if (nrow(add_fdata) > 0) {
          # if (nrow(acmfdata) == 0)
          #  next()
          acmfdata <- rbind(acmfdata, add_fdata)
        }
        
        # Subset Non-fatal types
        add_nfdata <- subset(raw_data_tp_ltpa, outcome == outcome_disease & 
                               pa_domain_subgroup == local_pa_domain_subgroup & 
                               outcome_type == "Non-fatal")
        
        # ONLY add those studies that have no "both" studies
        add_nfdata <- subset(add_nfdata, !id %in% acmfdata$id)
        # Add additional rows
        if (nrow(add_nfdata) > 0) {
          acmfdata <- rbind(acmfdata, add_nfdata)
        }
      }
      
      acmfdata <- remove_with_missing_data(acmfdata)
    }
    
    acmfdata
    
    # if (nrow(acmfdata) > 0){
    #   acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
    #   # Remove when totalperson is not available for hr, and personsyears for rr/or
    #   acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
    #                                    (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    #   
    #   # Filter studies by study size
    #   acmfdata <- subset(acmfdata, n_baseline >= 10000)
    #   
    # }
    # 
    # acmfdata
    
  }
  
  get_subpopulation_data <- function (PA_exposure, outcome_disease, outcome_types, gender){
    
    acmfdata <- data.frame()
    
    if (!is.na(outcome_disease)){
      
      # if (outcome_types != 'all'){
      #   acmfdata <- subset(raw_data_gsp_ltpa, 
      #                      outcome == outcome_disease & 
      #                        pa_domain_subgroup == PA_exposure & 
      #                        sex_subgroups == gender &
      #                        outcome_type == outcome_types)
      # }else{
      #   acmfdata <- subset(raw_data_gsp_ltpa, 
      #                      outcome == outcome_disease & 
      #                        pa_domain_subgroup == PA_exposure & 
      #                        sex_subgroups == gender)#input$in_sub_population)
      #   
      # }
      
      # Subset according to outcome, domain and outcome type
      acmfdata <- subset(raw_data_gsp_ltpa, outcome == outcome_disease & 
                           sex_subgroups == gender &
                           pa_domain_subgroup == PA_exposure & 
                           outcome_type == outcome_types)
      
      # Add additional "fatal" studies that had no "both" types
      if (outcome_types == "Both") {
        # Subset fatal types
        add_fdata <- subset(raw_data_gsp_ltpa, outcome == outcome_disease & 
                              sex_subgroups == gender &
                              pa_domain_subgroup == local_pa_domain_subgroup & 
                              outcome_type == "Fatal")
        # ONLY add those studies that have no "both" studies
        add_fdata <- subset(add_fdata, !id %in% acmfdata$id)
        # Add additional rows
        if (nrow(add_fdata) > 0) {
          # if (nrow(acmfdata) == 0)
          #  next()
          acmfdata <- rbind(acmfdata, add_fdata)
        }
        
        # Subset Non-fatal types
        add_nfdata <- subset(raw_data_gsp_ltpa, outcome == outcome_disease & 
                               sex_subgroups == gender &
                               pa_domain_subgroup == local_pa_domain_subgroup & 
                               outcome_type == "Non-fatal")
        
        # ONLY add those studies that have no "both" studies
        add_nfdata <- subset(add_nfdata, !id %in% acmfdata$id)
        # Add additional rows
        if (nrow(add_nfdata) > 0) {
          acmfdata <- rbind(acmfdata, add_nfdata)
        }
      }
      
    }
    
    #acmfdata <- data.frame()
    
    # # Remove where both dose and response are null
    # acmfdata <- subset(acmfdata, !is.na(RR) & !is.na(dose))
    # 
    # if (nrow(acmfdata) > 0){
    #   acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
    #   # Remove when totalperson is not available for hr, and personsyears for rr/or
    #   acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
    #                                    (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    #   
    # }
    
    acmfdata <- remove_with_missing_data(acmfdata)
    
    acmfdata
  }
  
  remove_with_missing_data <- function(acmfdata){
    
    # Fill missing values by inferring to useful columns
    acmfdata <- getMissingVariables(acmfdata, infertotalpersons = TRUE, kcases = FALSE)
    
    if (!is.null(acmfdata) && nrow(acmfdata) > 0){
      
      # Before removing any lines with n requirement less than 10k
      missing_cases <- acmfdata
      
      # Keep only those studies with cases present
      acmfdata <- subset(acmfdata, !is.na(cases))
      
      # Keep only those studies with n_baseline greater than 10k
      missing_cases <- setdiff(missing_cases, acmfdata)
      if (nrow(missing_cases) > 0) {
        missing_cases$reason <- "missing cases"
        missing_cases$NO_BMI_EFFECT <- NO_BMI_EFFECT
        missing_cases[["is_alt_analysis"]] <- ALT
      }
      
      # Before removing any lines with n requirement less than 10k
      n_subset <- acmfdata
      
      # Remove all studies with missing RRs
      missing_RR_ids <- subset(acmfdata, is.na(RR)) %>% select(id) %>% pull()
      
      if (length(missing_RR_ids) > 0) {
        temp <- subset(acmfdata, id %in% missing_RR_ids)
        temp$reason <- "missing RRs"
        temp$NO_BMI_EFFECT <- NO_BMI_EFFECT
        temp[["is_alt_analysis"]] <- ALT
        acmfdata <- subset(acmfdata, !id %in% missing_RR_ids)
      }
      
      # Remove all studies with negative standard error (SE)
      negative_SE_ids <- subset(acmfdata, se < 0) %>% select(id) %>% pull()
      if (length(negative_SE_ids) > 0) {
        temp <- subset(acmfdata, id %in% negative_SE_ids)
        temp$reason <- "negative error"
        temp$NO_BMI_EFFECT <- NO_BMI_EFFECT
        temp[["is_alt_analysis"]] <- ALT
        acmfdata <- subset(acmfdata, !id %in% negative_SE_ids)
      }
      
      # Before removing any lines with n requirement less than 10k
      n_missing <- acmfdata
      
      # Remove all studies with mandatory info
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0)) |
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0)))))
      n_missing <- setdiff(n_missing, acmfdata)
      if (nrow(n_missing) > 0) {
        n_missing$reason <- "missing either person years or total persons"
      }
      
      # NOTE TO MATT/LEANDRO
      # This removes all studies with repeating rows such as studies with both sex and ethnicity entries
      # Won'TRUE need it if we remove all such rows from the dataset
      # Identify all studies with repeating IDs
      local_filter <- acmfdata %>%
        group_by(id) %>%
        summarise(c = sum(is.na(se))) %>%
        filter(c > 1) %>%
        dplyr::select(id) %>% pull()
      
      # Remove all such studies altogether - which is a temp fix
      if (length(local_filter) > 0) {
        temp <- subset(acmfdata, id %in% local_filter)
        temp$reason <- "multiple stratification"
        temp$NO_BMI_EFFECT <- NO_BMI_EFFECT
        temp[["is_alt_analysis"]] <- ALT
        acmfdata <- acmfdata %>% filter(!id %in% local_filter)
      }
      
      orig_col_names <- colnames(acmfdata)
      
      # Select subset of columns
      acmfdata <- subset(acmfdata, select = c(id, ref_number, first_author, effect_measure, outcome_type, type, totalpersons, personyrs, dose, RR, logrr, cases, uci_effect, lci_effect, se))
      
    }
    
    return(acmfdata)
    
  }
  
  
  
  output$top_plot <- renderPlotly({
    input$in_outcome
    input$in_outcome_type
    input$total_sub_population
    
    # browser()
    
    pop_title <- "Total Population"
    
    if (input$total_sub_population == "1"){
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      
    }
    else{# Sub-population
      acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 1)
      pop_title <- "Male Population"
    }
    
    if (input$total_sub_population == "2" && input$plot_options == "2"){
      
      if (nrow(acmfdata) > 0){
        
        last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        
        last_knot <- last_knot[2]
        
        q <- quantile(acmfdata$dose, c(0, last_knot / 2, last_knot))
        
        p_title <- get_title(dataset = acmfdata, pop_type = "female")
        
        to_download$top_plot_data <<- acmfdata
        
        get_dose_plot(acmfdata, q, plot_title = p_title)
      }else{
        
        get_dose_plot(NULL, 0, "")
        
      }
      
    }
    
    else{
      
      
      outcome_type <- ""
      
      if (input$in_outcome_type != "all"){
        outcome_type <- paste(stringi::stri_trans_totitle(input$in_outcome_type), "- ")
        
      }
      
      
      if (nrow(acmfdata) > 0){
        
        last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
        
        if (input$total_sub_population == "2")
          last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        
        last_knot <- last_knot[2]
        
        
        
        local_cov_method <- F
        
        if (input$in_outcome == "Coronary heart disease" || input$in_outcome == "Cardiovascular disease" || input$in_outcome == "Stroke")
          local_cov_method <- T
        
        # browser()
        
        res <- get_convergent_ma(data = acmfdata, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
        
        # acmfdata <- read_csv('temp.csv')
        
        if (!is.null(res)){
          
          # browser()
          
          # Save results as data frame
          plot_data <- data.frame(cbind(res[[1]], res[[2]]))
          colnames(plot_data) <- c("dose","RR", "lb", "ub")
          
          to_download$top_plot_data <<- plot_data
          
          fig_title <- input$in_outcome
          if (fig_title != toupper(fig_title))
            fig_title <- stringi::stri_trans_totitle(fig_title)
          
          fig_title <- paste0(pop_title, " - ", outcome_type,  fig_title, "\n Number of entries: ",  length(unique(acmfdata$id)) , 
                              " & Person-years: ", format(round(sum(acmfdata$personyrs, na.rm = TRUE)), scientific = FALSE, big.mark = ','))
          q <- quantile(acmfdata$dose, c(0, last_knot / 2, last_knot))
          
          getPlot(dataset = plot_data, q = q, plotTitle = fig_title, pop_title, input$in_outcome, outcome_type)
        }
        
      }else{
        
        getPlot(dataset = NULL, q = NULL, plotTitle =  "", pop_title, input$in_outcome, outcome_type)
      }
      
    }
    
  })
  
  
  output$bottom_plot <- renderPlotly({
    
    input$in_outcome
    input$in_outcome_type
    input$total_sub_population
    
    if (input$total_sub_population == "1"){
      
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      
      if (nrow(acmfdata) > 0){
        
        p_title <- get_title(dataset = acmfdata, pop_type = "total")
        
        last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
        
        last_knot <- last_knot[2]
        
        q <- quantile(acmfdata$dose, c(0, last_knot / 2, last_knot))
        
        to_download$bottom_plot_data <<- acmfdata
        
        get_dose_plot(acmfdata, q, plot_title = p_title)
      }else{
        
        get_dose_plot(NULL, 0, "")
      }
      
    }
    
    else{
      
      sub_pop_data <- get_subpopulation_data(PA_exposure = pa_exposure, outcome = input$in_outcome, outcome_types = input$in_outcome_type, gender = 2)
      
      if (input$plot_options == "1"){
        
        if (nrow(sub_pop_data) > 0){
          
          local_cov_method <- F
          
          if (input$in_outcome == "Coronary heart disease" || input$in_outcome == "Cardiovascular disease" || input$in_outcome == "Stroke")
            local_cov_method <- T
          
          last_knot <- get_last_knot(sub_pop_data, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
          
          last_knot <- last_knot[2]
          
          get_convergent_ma
          
          res <- get_convergent_ma(data = sub_pop_data, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
          
          # Save results as data frame
          plot_data <- data.frame(cbind(res[[1]], res[[2]]))
          colnames(plot_data) <- c("dose","RR", "lb", "ub")
          
          to_download$bottom_plot_data <<- plot_data
          
          q <- quantile(sub_pop_data$dose, c(0, last_knot / 2, last_knot))
          
          getPlot(dataset = plot_data, q = q, plotTitle = get_title(dataset = sub_pop_data, pop_type = "female"), "female population", input$in_outcome, input$in_outcome_type)
          
        }else{
          
          gt <- "Female Population"
          
          fig_title <- input$in_outcome
          if (fig_title != toupper(fig_title))
            fig_title <- stringi::stri_trans_totitle(fig_title)
          
          fig_title <- paste0(gt, " - ", fig_title)
          
          getPlot(dataset = NULL, q = NULL, plotTitle =  get_title(dataset = NULL, pop_type = "female"), "female population", input$in_outcome, input$in_outcome_type)
          
        }
        
      }else{
        
        if (nrow(sub_pop_data) > 0){
          
          last_knot <- get_last_knot(sub_pop_data, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
          
          last_knot <- last_knot[2]
          
          q <- quantile(sub_pop_data$dose, c(0, last_knot / 2, last_knot))
          
          p_title <- get_title(dataset = sub_pop_data, pop_type = "female")
          
          to_download$bottom_plot_data <<- sub_pop_data
          
          get_dose_plot(sub_pop_data, q, plot_title = p_title)
        }
        
        else{
          get_dose_plot(NULL, 0, "")
        }
        
        
      }
      
    }
    
  })
  
  get_title <- function(dataset, pop_type ){
    fig_title <- ""
    
    if (nrow(dataset) > 0){
      outcome_type <- ""
      
      if (input$in_outcome_type != "all"){
        outcome_type <- paste(stringi::stri_trans_totitle(input$in_outcome_type), "- ")
        
      }
      
      gt <- "Total Population"
      if (pop_type == "female")
        gt <- "Female Population"
      else if (pop_type == "male")
        gt <- "Male Population"
      
      fig_title <- input$in_outcome
      if (fig_title != toupper(fig_title))
        fig_title <- stringi::stri_trans_totitle(fig_title)
      
      fig_title <- paste0(gt, " - ", outcome_type, fig_title, "\n Number of entries: ",  
                          length(unique(dataset$id)) , 
                          " & Person-years: ", format(round(sum(dataset$personyrs, na.rm = TRUE)), scientific = FALSE, big.mark = ','))
      
    }
    
    fig_title
    
    
  }
  
  
  get_dose_plot <- function (dataset, q, plot_title){
    
    if (!is.null(dataset)){
      dataset$personyrs <- round(dataset$personyrs)
      group_by(dataset, id) %>% select(dose, se) %>%
        summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
      gg <- ggplotly(
        ggplot(dataset, aes(dose, RR, col = ref_number, label = personyrs)) + geom_point(size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
          geom_line() +
          scale_x_continuous(expand = c(0, 0),
                             breaks = seq(from = 0, to = max(dataset$dose, na.rm = T), by = 5)) +
          scale_y_continuous(expand = c(0, 0),
                             breaks = seq(from = 0, to = (max(dataset$RR, na.rm = T) + 0.4), by = 0.2),
                             limits = c(0, NA)) +
          # annotate("text", label=paste(names(qg)[2], "q"), x=round(q[2],1), y = 0.3, parse=T, size=3) +
          # annotate("text", label=paste(names(qg)[3], "q"), x=round(q[3],1) , y = 0.3, parse=T, size=3) +
          # annotate("text", label = paste(100 * (input$in_main_quantile[2] - input$in_main_quantile[1]) / 2, "% (pyrs)"), x=round(q[2],1) - 2.5, y = 0.15, parse=T, size=3) +
          # annotate("text", label = paste(100 * input$in_main_quantile[2], "% (pyrs)"), x=round(q[3],1) - 2.5 , y = 0.15, parse=T, size=3) +
          # annotate("segment", label="1-beta", x = 5, xend = 7, y = 0.3, yend = 0.5, arrow = arrow()) +
          #scale_y_continuous(trans = "log", breaks = c(.1, .25, .5, .75, 1, 1.25)) +
          #scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
          # coord_cartesian(xlim = c(0, max(dataset$dose))) +
          geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
          theme_classic() + guides(col = FALSE) + 
          xlab("\nMarginal MET hours per week\n") +
          ylab("\nRelative Risk\n") +
          labs(title = paste(plot_title)) +
          theme(legend.position="none")
      )
      
    }else{
      gg <- ggplot(data.frame()) + geom_point() + xlim(0, 100) + ylim(0, 1) + 
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 12, colour = "red", vjust = 7),
          plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="red"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 1.05)) +
        labs (title = "Sorry no data is available")
    }
    
    p <- ggplotly(gg)
    p
    
  }
  
  
  getPlot <- function (dataset, q, plotTitle, pop_type , outcome, outcome_type ){
    
    if (!is.null(dataset)){
      
      # Round to three decimal points
      dataset$RR <- round(dataset$RR, 3)
      dataset$dose <- round(dataset$dose, 3)
      
      # Round confidence interval as well
      dataset$ub <- round(dataset$ub, 3)
      dataset$lb <- round(dataset$lb, 3)
      
      gg <- ggplot() + 
        geom_line(data = subset(dataset, dose < as.numeric(q[3])), aes(x = dose, y = RR)) +
        geom_line(data = subset(dataset, dose >= as.numeric(q[3])), aes(x = dose, y = RR), linetype = "dashed") +
        geom_ribbon(data = subset(dataset, dose < as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
        geom_ribbon(data = subset(dataset, dose >= as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.10) +
        scale_x_continuous(expand = c(0, 0),
                           breaks = seq(from = 0, to = 35, by = 5)) + 
        scale_y_continuous(expand = c(0, 0),
                           breaks = seq(from = 0, to = max(dataset$ub), by = 0.2),
                           limits = c(0, NA)) +
        coord_cartesian(xlim = c(0, 35)) +
        xlab(paste("\n", "Marginal MET hours per week", "\n")) +
        ylab("\nRelative Risk\n") +
        geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
        
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 12, colour = "black", vjust = 7),
          plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 1.05)) + 
        labs(title = paste(plotTitle)) #+ labs(fill = "") 
      
      # figure1 <- last_plot()
      # ggsave(paste0(paste(pop_type , outcome, sep = "-"), outcome_type, ".png"), height=5, width=10, units='in', dpi=600)
      
    }else{
      gg <- ggplot(data.frame()) + geom_point() + xlim(0, 100) + ylim(0, 1) + 
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 12, colour = "red", vjust = 7),
          plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="red"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 1.05)) +
        labs (title = "Sorry no data is available")
    }
    
    p <- ggplotly(gg)
    
    p
  }
  
  output$lowest_guideline <- renderUI({
    HTML("")
    
    if (input$total_sub_population == "1"){
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
      last_knot <- last_knot[2]
      HTML("PIF for meeting the half the lower WHO guideline (MMETh >= 4.375 per week): ", get_pif_values(dataset = acmfdata, last_knot = last_knot , dose_value = 4.375), "\n")
      
    }
    else{# Sub-population
      m_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 1)
      if (nrow(m_acmfdata) > 0){
        m_last_knot <- get_last_knot(m_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        m_last_knot <- m_last_knot[2]
      }
      
      w_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 2)
      if (nrow(w_acmfdata) > 0){
        w_last_knot <- get_last_knot(w_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        w_last_knot <- w_last_knot[2]
      }
      
      if (nrow(m_acmfdata) > 0 && nrow(w_acmfdata) > 0){
        HTML("PIF for meeting the half the lower WHO guideline (MMETh >= 4.375 per week) <br/>",
             "<b>","Male:","</b>", get_pif_values(dataset = m_acmfdata, last_knot = m_last_knot , dose_value = 4.375), "<br/>",
             "<b>","Female:","</b>", get_pif_values(dataset = w_acmfdata, last_knot = w_last_knot , dose_value = 4.375))
      }
    }
  })
  
  
  output$lower_guideline <- renderUI({
    
    HTML("")
    
    if (input$total_sub_population == "1"){
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      if (nrow(acmfdata) > 0){
        last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
        last_knot <- last_knot[2]
        HTML("PIF for meeting the lower WHO guideline (MMETh >= 8.75 per week): ", get_pif_values(dataset = acmfdata, last_knot = last_knot , dose_value = 8.75), "\n")
      }
      
    }
    else{# Sub-population
      
      m_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 1)
      if (nrow(m_acmfdata) > 0){
        m_last_knot <- get_last_knot(m_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        m_last_knot <- m_last_knot[2]
      }
      
      w_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 2)
      if (nrow(w_acmfdata) > 0){
        w_last_knot <- get_last_knot(w_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        w_last_knot <- w_last_knot[2]
      }
      
      if (nrow(m_acmfdata) > 0 && nrow(w_acmfdata) > 0){
        HTML("PIF for meeting the lower WHO guideline (MMETh >= 8.75 per week) <br/>",
             "<b>","Male:","</b>", get_pif_values(dataset = m_acmfdata, last_knot = m_last_knot , dose_value = 8.75), "<br/>",
             "<b>","Female:","</b>", get_pif_values(dataset = w_acmfdata, last_knot = w_last_knot , dose_value = 8.75))
      }
    }
  })
  
  
  output$upper_guideline <- renderUI({
    HTML("")
    
    if (input$total_sub_population == "1"){
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
      last_knot <- last_knot[2]
      HTML("PIF for meeting the upper WHO guideline (MMETh >= 17.5 per week): ", get_pif_values(dataset = acmfdata, last_knot = last_knot , dose_value = 17.5), "\n")
    }
    else{# Sub-population
      m_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 1)
      if (nrow(m_acmfdata) > 0){
        m_last_knot <- get_last_knot(m_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        m_last_knot <- m_last_knot[2]
      }
      
      w_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 2)
      if (nrow(w_acmfdata) > 0){
        w_last_knot <- get_last_knot(w_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        w_last_knot <- w_last_knot[2]
      }
      
      if (nrow(m_acmfdata) > 0 && nrow(w_acmfdata) > 0){
        HTML("PIF for meeting the upper WHO guideline (MMETh >= 17.5 per week): <br/>",
             "<b>","Male:","</b>", get_pif_values(dataset = m_acmfdata, last_knot = m_last_knot , dose_value = 17.5), "<br/>",
             "<b>","Female:","</b>", get_pif_values(dataset = w_acmfdata, last_knot = w_last_knot , dose_value = 17.5))
      }
    }
    
  })
  
  output$overall_datatable <- DT::renderDataTable({
    
    overall_data <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
    
    if(nrow(overall_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$overall_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    
    overall_data <- subset(overall_data, select = c(ref_number, first_author, effect_measure, 
                                                    totalpersons, personyrs, dose, RR, cases, lci_effect, uci_effect))
    
    # browser()
    # Remove gender specific suffix from ref_number
    overall_data$ref_number <- sapply(strsplit(as.character(overall_data$ref_number),"-"), `[`, 1)
    
    fname <- "total_population"
    # Round relevant columns
    overall_data$totalpersons <- round(overall_data$totalpersons)
    overall_data$personyrs <- round(overall_data$personyrs)
    
    # Empty the warning message - as some lines have been selected by the user
    output$overall_warning_message <- renderUI("")
    DT::datatable(overall_data, 
                  extensions = 'Buttons',
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('colvis', list(
                                     extend = 'collection',
                                     buttons = list(list(extend='csv',
                                                         filename = fname),
                                                    list(extend='excel',
                                                         filename = fname)),
                                     text = 'Download'
                                   )),
                                 scrollX = TRUE,
                                 pageLength = nrow(overall_data),
                                 order=list(list(2,'desc'))))
  })
  
  output$male_population_datatable <- DT::renderDataTable({
    
    sub_population_data <- get_subpopulation_data(PA_exposure = pa_exposure, outcome = input$in_outcome, outcome_types = input$in_outcome_type, gender = 1)
    if(nrow(sub_population_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$male_sub_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    
    # Subset by columns
    sub_population_data <- subset(sub_population_data, select = c(ref_number, first_author, effect_measure, totalpersons, personyrs, dose, RR, cases, lci_effect, uci_effect))
    
    # Remove gender specific suffix from ref_number
    sub_population_data$ref_number <- sapply(strsplit(sub_population_data$ref_number,"-"), `[`, 1)
    
    fname <- "male_population"
    
    # Empty the warning message - as some lines have been selected by the user
    output$male_sub_warning_message <- renderUI("")
    DT::datatable(sub_population_data, 
                  extensions = 'Buttons',
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('colvis', list(
                                     extend = 'collection',
                                     buttons = list(list(extend='csv',
                                                         filename = fname),
                                                    list(extend='excel',
                                                         filename = fname)),
                                     text = 'Download'
                                   )),
                                 scrollX = TRUE,
                                 pageLength = nrow(sub_population_data),
                                 order=list(list(2,'desc'))))
  })
  
  
  output$female_population_datatable <- DT::renderDataTable({
    
    sub_population_data <- get_subpopulation_data(PA_exposure = pa_exposure, outcome = input$in_outcome, outcome_types = input$in_outcome_type, gender = 2)
    if(nrow(sub_population_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$female_sub_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    
    # Subset by columns
    sub_population_data <- subset(sub_population_data, select = c(ref_number, first_author, effect_measure, totalpersons, personyrs, dose, RR, cases, lci_effect, uci_effect))
    
    # Round relevant columns
    sub_population_data$totalpersons <- round(sub_population_data$totalpersons)
    sub_population_data$personyrs <- round(sub_population_data$personyrs)
    
    fname <- "female_population"
    
    # Remove gender specific suffix from ref_number
    sub_population_data$ref_number <- sapply(strsplit(sub_population_data$ref_number,"-"), `[`, 1)
    
    # Empty the warning message - as some lines have been selected by the user
    output$female_sub_warning_message <- renderUI("")
    DT::datatable(sub_population_data, 
                  extensions = 'Buttons',
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(dom = 'Bfrtip',
                                 buttons = 
                                   list('colvis', list(
                                     extend = 'collection',
                                     buttons = list(list(extend='csv',
                                                         filename = fname),
                                                    list(extend='excel',
                                                         filename = fname)),
                                     text = 'Download'
                                   )),
                                 scrollX = TRUE,
                                 pageLength = nrow(sub_population_data),
                                 order=list(list(2,'desc'))))
  })
  
  
  
  output$dose_range <- DT::renderDataTable({
    
    dat <- data.frame()
    
    if (input$total_sub_population == "1"){
      overall_data <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      dat <- data.frame()
      
      if (nrow(overall_data) > 0){
        
        local_cov_method <- F
        
        if (input$in_outcome == "Coronary heart disease" || input$in_outcome == "Cardiovascular disease" || input$in_outcome == "Stroke")
          local_cov_method <- T
        
        last_knot <- get_last_knot(overall_data, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
        
        last_knot <- last_knot[2]
        
        res <- get_convergent_ma(data = overall_data, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)
        
        # Save results as data frame
        plot_data <- data.frame(cbind(res[[1]], res[[2]]))
        
        colnames(plot_data) <- c("dose","RR", "lb", "ub")
        #MMET = c(4.375, 8.75, 17.5),  
        dat <- data.frame(MMETh = c(4.375, 8.75, 17.5), RR = paste(get_ma_table(plot_data, "RR"), " (", get_ma_table(plot_data, "lb"),
                                                                   " - ", get_ma_table(plot_data, "ub"), ")", sep = ""))
        
      }
    }else{# Sub-population
      
      local_cov_method <- F
      
      if (input$in_outcome == "Coronary heart disease" || input$in_outcome == "Cardiovascular disease" || input$in_outcome == "Stroke"
          || input$in_outcome == "Colon cancer")
        local_cov_method <- T
      
      m_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 1)
      if(nrow(m_acmfdata) > 0){
        m_last_knot <- get_last_knot(m_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        m_last_knot <- m_last_knot[2]
        
        res <- get_convergent_ma(data = m_acmfdata, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = m_last_knot, lout = 1000)
        # Save results as data frame
        m_plot_data <- data.frame(cbind(res[[1]], res[[2]]))
        
        colnames(m_plot_data) <- c("dose","RR", "lb", "ub")
        
      }
      
      w_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 2)
      td <<- w_acmfdata
      if(nrow(m_acmfdata) > 0){
        w_last_knot <- get_last_knot(w_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        w_last_knot <- w_last_knot[2]
        res <- get_convergent_ma(data = m_acmfdata, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = w_last_knot, lout = 1000)        
        # Save results as data frame
        w_plot_data <- data.frame(cbind(res[[1]], res[[2]]))
        colnames(w_plot_data) <- c("dose","RR", "lb", "ub")
      }
      # MMET = c(4.375, 8.75, 17.5),  
      if (nrow(m_acmfdata) > 0 && nrow(w_acmfdata) > 0){
        dat <- data.frame(MMETh = c(4.375, 8.75, 17.5), 'Male RR' = paste(get_ma_table(m_plot_data, "RR"), " (", get_ma_table(m_plot_data, "lb"),
                                                                          " - ", get_ma_table(m_plot_data, "ub"), ")", sep = ""),
                          'Female RR' = paste(get_ma_table(w_plot_data, "RR"), " (", get_ma_table(w_plot_data, "lb"),
                                              " - ", get_ma_table(w_plot_data, "ub"), ")", sep = ""), check.names = FALSE)
        
      }
    }
    DT::datatable(dat, options = list(paging = F, dom = 't'), rownames = FALSE) #%>%
  })
  
  
  get_ma_table <- function(plot_data, colname = "RR"){
    
    c(round(plot_data[[colname]][which.min(abs(plot_data$dose - 4.375))], 2),
      round(plot_data[[colname]][which.min(abs(plot_data$dose - 8.75))], 2),
      round(plot_data[[colname]][which.min(abs(plot_data$dose - 17.5))], 2))#,
  }
  
  get_pif_values <- function(dataset, last_knot, dose_value){
    
    if (nrow(dataset) > 0){
      if (max(dataset$dose) < dose_value)
        return(0)
    }
    
    if (nrow(dataset) > 0){
      
      local_cov_method <- F
      
      res <- get_convergent_ma(data = dataset, ptitle = "", returnval = TRUE, covMethed = TRUE, minQuantile = 0, maxQuantile = last_knot, lout = 1000)        
      
      # Save results as data frame
      plot_data <- data.frame(cbind(res[[1]], res[[2]]))
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
      
      lower_guideline_value <- paste0(round(pert_ls, 1) , "% (95% CI: ", round(pert_ls_lower, 1), " - ",  round(pert_ls_upper, 1), ")" )
      
    }
    
  }
  
  
  output$download_top_data <- downloadHandler(
    filename = function() {
      paste(input$in_outcome, "-", input$in_outcome_type, ".csv", sep="")
    },
    content = function(file) {
      # cat(summary(to_download$top_plot_data), "\n")
      write.csv(to_download$top_plot_data, file)
    }
  )
  
  
  output$download_bottom_data <- downloadHandler(
    filename = function() {
      paste(input$in_outcome, "-", input$in_outcome_type, ".csv", sep="")
    },
    content = function(file) {
      write.csv(to_download$bottom_plot_data, file)
    }
  )
  
})
