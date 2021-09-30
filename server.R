options(shiny.sanitize.errors = F)
lower_guideline_value <- NA
upper_guideline_value <- NA

to_download <- NULL
shinyServer(function(input, output, session){
  
  observe( {
    input$total_sub_population
    if(input$total_sub_population == "1"){
      hideTab(inputId = "main_panel", target = "Male Population Data")
      hideTab(inputId = "main_panel", target = "Female Population Data")
      showTab(inputId = "main_panel", target = "Total Population Data")
    }else if(input$total_sub_population != "1"){
      showTab(inputId = "main_panel", target = "Male Population Data")
      showTab(inputId = "main_panel", target = "Female Population Data")
      hideTab(inputId = "main_panel", target = "Total Population Data")
    }
  })
  
  overall_pop_dose_res_data <- reactive({
    
    snake_case_outcome <- gsub(x = input$in_outcome, pattern = " ", replacement = "-") %>% tolower()
    snake_case_outcome_type <- gsub(x = input$in_outcome_type, pattern = " ", replacement = "-") %>% 
      tolower()
    ma_filename <- paste0(snake_case_outcome, "-", snake_case_outcome_type)
    
   overall_pop_tbles %>% filter(filename == ma_filename & quantile == input$in_main_quantile %>% as.numeric()) %>% dplyr::select(-c(filename, quantile))
    
  }) %>% bindCache(input$in_outcome,
                         input$in_outcome_type,
                         input$in_main_quantile)
  
  male_pop_dose_res_data <- reactive({
    
    snake_case_outcome <- gsub(x = input$in_outcome, pattern = " ", replacement = "-") %>% tolower()
    snake_case_outcome_type <- gsub(x = input$in_outcome_type, pattern = " ", replacement = "-") %>% tolower()
    ma_filename <- paste0("male-", snake_case_outcome, "-", snake_case_outcome_type)
    
    gender_pop_tbles %>% filter(filename == ma_filename & quantile == input$in_main_quantile %>% as.numeric()) %>% dplyr::select(-c(filename, quantile))
    
  }) %>% bindCache(input$in_outcome,
                  input$in_outcome_type,
                  input$in_main_quantile)
  
  female_pop_dose_res_data <- reactive({
    
    snake_case_outcome <- gsub(x = input$in_outcome, pattern = " ", replacement = "-") %>% tolower()
    snake_case_outcome_type <- gsub(x = input$in_outcome_type, pattern = " ", replacement = "-") %>% tolower()
    ma_filename <- paste0("female-", snake_case_outcome, "-", snake_case_outcome_type)
    
    gender_pop_tbles %>% filter(filename == ma_filename & quantile == input$in_main_quantile %>% as.numeric()) %>% dplyr::select(-c(filename, quantile))
    
  }) %>% bindCache(input$in_outcome,
                   input$in_outcome_type,
                   input$in_main_quantile)
  
  
  get_overall_data <- reactive({
    
    if (!is.na(input$in_outcome)){
      
      local_outcome <- isolate({input$in_outcome})
      local_outcome_type <- isolate({input$in_outcome_type}) 
      local_outcome_type <- gsub(x = local_outcome_type, pattern = " ", replacement = "-") 
      
      # Subset according to outcome, domain and outcome type
      acmfdata <- subset(raw_data_tp_ltpa, 
                         outcome == local_outcome & 
                           pa_domain_subgroup == local_pa_domain_subgroup & 
                           outcome_type == local_outcome_type)
      
      # Add additional "fatal" studies that had no "both" types
      if (local_outcome_type == "Fatal-and-non-fatal") {
        # Subset fatal types
        add_fdata <- subset(raw_data_tp_ltpa, outcome == local_outcome & 
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
        add_nfdata <- subset(raw_data_tp_ltpa, outcome == local_outcome & 
                               pa_domain_subgroup == local_pa_domain_subgroup & 
                               outcome_type == "Non-fatal")
        
        # ONLY add those studies that have no "both" studies
        add_nfdata <- subset(add_nfdata, !id %in% acmfdata$id)
        # Add additional rows
        if (nrow(add_nfdata) > 0) {
          acmfdata <- rbind(acmfdata, add_nfdata)
        }
      }
      
      remove_with_missing_data(acmfdata)
      
    }else
      data.frame()
    
  }) %>% bindCache(input$in_outcome,
                input$in_outcome_type)
  
  get_male_subpopulation_data <- reactive(get_subpopulation_data(outcome_disease = input$in_outcome,
                                                                 outcome_types = input$in_outcome_type,
                                                                 gender = 1)) %>% bindCache(input$in_outcome,
                                                                                            input$in_outcome_type)
  
  get_female_subpopulation_data <- reactive(get_subpopulation_data(outcome_disease = input$in_outcome,
                                                                 outcome_types = input$in_outcome_type,
                                                                 gender = 2)) %>% bindCache(input$in_outcome,
                                                                                            input$in_outcome_type)
  
  get_subpopulation_data <- function(outcome_disease, outcome_types, gender){
    
    if (!is.na(outcome_disease)){
      
      local_outcome_type <- gsub(x = outcome_types, pattern = " ", replacement = "-")
      
      # Subset according to outcome, domain and outcome type
      acmfdata <- subset(raw_data_gsp_ltpa, outcome == outcome_disease & 
                           sex_subgroups == gender &
                           pa_domain_subgroup == local_pa_domain_subgroup & 
                           outcome_type == local_outcome_type)
      
      # Add additional "fatal" studies that had no "both" types
      if (local_outcome_type == "Fatal-and-non-fatal") {
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
      
      remove_with_missing_data(acmfdata)
      
    }else{
      data.frame()
    }
    
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
    input$in_main_quantile
    input$plot_options
    
    isolate({
      in_outcome <- input$in_outcome
      in_outcome_type <- input$in_outcome_type
      total_sub_population <- input$total_sub_population
      in_main_quantile <- input$in_main_quantile
      plot_options <- input$plot_options
    })
    
    pop_title <- "Total Population"
    
    if (total_sub_population == "1"){
      acmfdata <- get_overall_data()
      
    }
    else{# Sub-population
      acmfdata <- get_male_subpopulation_data()
      pop_title <- "Male Population"
    }
    
    if (total_sub_population == "2" && plot_options == "2"){
      
      if (!is.null(acmfdata) && nrow(acmfdata) > 0){
        
        last_knot <- get_last_knot(acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        
        last_knot <- round(last_knot[2], 2)
        
        q <- quantile(acmfdata$dose, c(0, last_knot / 2, last_knot))
        
        p_title <- get_title(dataset = acmfdata, pop_type = "female")
        
        to_download$top_plot_data <<- acmfdata
        
        get_ind_plot(acmfdata, q, plot_title = p_title)
      }else{
        
        get_ind_plot(NULL, 0, "")
        
      }
      
    }
    
    else{
      
      
      outcome_type <- ""
      
      if (in_outcome_type != "all"){
        outcome_type <- paste(in_outcome_type, "- ")
        
      }
      
      
      if (!is.null(acmfdata) && nrow(acmfdata) > 0){
        
        if (total_sub_population == "1")
          plot_data <-   (overall_pop_dose_res_data() %>% as.data.frame())
        else
          plot_data <- (male_pop_dose_res_data() %>% as.data.frame())
        
        if (!is.null(plot_data) && nrow(plot_data) > 0){

          to_download$top_plot_data <<- plot_data
          
          fig_title <- in_outcome
          
          last_knot <- get_last_knot(acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
          
          last_knot <- round(last_knot[2], 2)
          
          q <- quantile(acmfdata$dose, c(0, last_knot / 2, last_knot))
          
          fig_title <- paste0(pop_title, " - ", outcome_type,  fig_title, "\nNumber of entries: ",  length(unique(acmfdata$id)) , 
                              " & Person-years: ", format(round(sum(acmfdata$personyrs, na.rm = TRUE)), scientific = FALSE, big.mark = ','))
          
          get_DR_plot(dataset = plot_data, q = q, plotTitle = fig_title, pop_title, in_outcome, outcome_type)
        }else
          get_DR_plot(dataset = NULL, q = NULL, plotTitle =  "", pop_title, in_outcome, outcome_type)
        
      }else{
        
        get_DR_plot(dataset = NULL, q = NULL, plotTitle =  "", pop_title, in_outcome, outcome_type)
      }
      
    }
    
  }) %>% bindCache(input$in_outcome,
                   input$in_outcome_type,
                   input$total_sub_population,
                   input$in_main_quantile,
                   input$plot_options)
  
  
  output$bottom_plot <- renderPlotly({
    
    input$in_outcome
    input$in_outcome_type
    input$total_sub_population
    input$in_main_quantile
    input$plot_options
    
    isolate({
      in_outcome <- input$in_outcome
      in_outcome_type <- input$in_outcome_type
      total_sub_population <- input$total_sub_population
      in_main_quantile <- input$in_main_quantile
      plot_options <- input$plot_options
    })
    
    if (total_sub_population == "1"){
      
      acmfdata <- get_overall_data()
      
      if (!is.null(acmfdata) && nrow(acmfdata) > 0){
        
        p_title <- get_title(dataset = acmfdata, pop_type = "total")
        
        last_knot <- get_last_knot(acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        
        last_knot <- round(last_knot[2], 2)
        
        q <- quantile(acmfdata$dose, c(0, last_knot / 2, last_knot))
        
        to_download$bottom_plot_data <<- acmfdata
        
        get_ind_plot(acmfdata, q, plot_title = p_title)
      }else{
        
        get_ind_plot(NULL, 0, "")
      }
      
    }
    
    else{
      
      sub_pop_data <- get_female_subpopulation_data()
      
      if (plot_options == "1"){
        
        if (!is.null(sub_pop_data) && nrow(sub_pop_data) > 0){

          plot_data <- female_pop_dose_res_data()
          
          last_knot <- get_last_knot(sub_pop_data, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())

          last_knot <- round(last_knot[2], 2)
          
          to_download$bottom_plot_data <<- plot_data
          
          q <- quantile(sub_pop_data$dose, c(0, last_knot / 2, last_knot))
          
          get_DR_plot(dataset = plot_data, q = q, plotTitle = get_title(dataset = sub_pop_data, pop_type = "female"), "female population", in_outcome, in_outcome_type)
          
        }else{
          
          gt <- "Female Population"
          
          fig_title <- in_outcome
          
          fig_title <- paste0(gt, " - ", fig_title)
          
          get_DR_plot(dataset = NULL, q = NULL, plotTitle =  get_title(dataset = NULL, pop_type = "female"), "female population", in_outcome, in_outcome_type)
          
        }
        
      }else{
        
        if (!is.null(sub_pop_data) && nrow(sub_pop_data) > 0){
          
          last_knot <- get_last_knot(sub_pop_data, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
          last_knot <- round(last_knot[2], 2)
          
          q <- quantile(sub_pop_data$dose, c(0, last_knot / 2, last_knot))
          
          p_title <- get_title(dataset = sub_pop_data, pop_type = "female")
          
          to_download$bottom_plot_data <<- sub_pop_data
          
          get_ind_plot(sub_pop_data, q, plot_title = p_title)
        }
        
        else{
          get_ind_plot(NULL, 0, "")
        }
      }
    }
    
  }) %>% bindCache(input$in_outcome,
                   input$in_outcome_type,
                   input$total_sub_population,
                   input$in_main_quantile,
                   input$plot_options)
  
  get_title <- function(dataset, pop_type ){
    fig_title <- ""
    
    if (nrow(dataset) > 0){
      outcome_type <- ""
      
      if (input$in_outcome_type != "all"){
        outcome_type <- paste(input$in_outcome_type, "- ")
        
      }
      
      gt <- "Total Population"
      if (pop_type == "female")
        gt <- "Female Population"
      else if (pop_type == "male")
        gt <- "Male Population"
      
      fig_title <- input$in_outcome
      
      fig_title <- paste0(gt, " - ", outcome_type, fig_title, "\nNumber of entries: ",  
                          length(unique(dataset$id)) , 
                          " & Person-years: ", format(round(sum(dataset$personyrs, na.rm = TRUE)), scientific = FALSE, big.mark = ','))
      
    }
    fig_title
  }
  
  
  get_ind_plot <- function (dataset, q, plot_title){
    
    if (!is.null(dataset) && nrow(dataset) > 0){
      
      dataset$personyrs <- round(dataset$personyrs)
      group_by(dataset, id) %>% select(dose, se) %>%
        summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
      
      dataset$ref_number <- as.factor(dataset$ref_number)
      
      dataset <- dataset %>% filter(dose <= 35)
      
      ymax <- dataset %>% filter(dose <= 35) %>% dplyr::select(dose) %>% max(na.rm = T) %>% as.numeric()
      
      ymin <- dataset %>% filter(dose <= 35) %>% dplyr::select(dose) %>% min(na.rm = T) %>% as.numeric()
      
      # Create plot
      gg <- ggplot() +
        geom_line(data = dataset, aes(dose, RR, col = ref_number, group = ref_number)) +
        geom_point(data = dataset, aes(dose, RR, col = ref_number, label = first_author, group = personyrs), size = 4 * (dataset$personyrs - min(dataset$personyrs)) / diff(range(dataset$personyrs))) +
        geom_vline(xintercept = q, linetype = "dotted", alpha = 0.6) +
        scale_x_continuous(expand = c(0, 0),
                           breaks = seq(from = 0, to = 35, by = 5)) + 
        scale_y_continuous(expand = c(0, 0),
                           breaks = seq(from = ifelse(ymin > 0, 0, round(ymin, 1) + 0.2), to = ymax, by = 0.2)) +
        
        #annotate("text", label = paste0(round((stringr::str_remove(names(q), "%")[3] %>% as.numeric()) / 2, 2), "% (person-yrs)"), x = round(q[2],1) + 5, y = 0.15, size = 2) +
        #annotate("text", label = paste0(round(stringr::str_remove(names(q), "%")[3] %>% as.numeric(), 2), "% (person-yrs)"), x = round(q[3],1) + 5, y = 0.15, size = 2) +
        
        coord_cartesian(xlim = c(0, 35)) + #, ylim = c(ymin, ymax)) +
        geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 12, colour = "black", vjust = 7),
          plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
          legend.direction = "horizontal",
          legend.position = "none") +
        xlab("\nMarginal MET hours per week\n") +
        ylab("\nRelative Risk\n") +
        labs(title = paste(plot_title))
      
      
      # qdf <- q %>% as.data.frame()
      # names(qdf) <- 'val'
      # 
      # p <- ggplotly(gg) %>% add_annotations(x = qdf$val,
      #                                       y = 0.8,
      #                                       text = paste0("Person Years (", names(q), ")"),
      #                                       xref = "x",
      #                                       yref = "y",
      #                                       showarrow = TRUE,
      #                                       arrowhead = 4,
      #                                       arrowsize = .5,
      #                                       ax = 20,
      #                                       ay = -40,
      #                                       font=list(size = 7))
      
      p <- ggplotly(gg)
      

    }else{
      gg <- ggplot(data.frame()) + geom_point() + xlim(0, 100) + ylim(0, 1) + 
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 12, colour = "red", vjust = 7),
          plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="red"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 1.05)) +
        labs (title = "Sorry no data is available")
      
      p <- ggplotly(gg)
    }
    
    p
    
  }
  
  
  get_DR_plot <- function (dataset, q, plotTitle, pop_type , outcome, outcome_type ){
    
    if (!is.null(dataset) && nrow(dataset) > 0){
      
      # Round to three decimal points
      dataset$RR <- round(dataset$RR, 3)
      dataset$dose <- round(dataset$dose, 3)
      
      # Round confidence interval as well
      dataset$ub <- round(dataset$ub, 3)
      dataset$lb <- round(dataset$lb, 3)
      
      dataset <- dataset %>% filter(dose <= 35)
      
      ymax <- dataset %>% filter(dose <= 35) %>% dplyr::select(ub) %>% max(na.rm = T) %>% as.numeric() # filter(dose <= 35) %>% 
      
      ymin <- dataset %>% filter(dose <= 35) %>% dplyr::select(lb) %>% min(na.rm = T) %>% as.numeric()
      
      # geom_label(aes(label = Dose),
      #data = DF %>% filter(Time == max(Time)),
      #nudge_x = 0.35,
      #size = 4)
      
      gg <- ggplot() + 
        geom_line(data = subset(dataset, dose < as.numeric(q[3]) && dose <= 35), aes(x = dose, y = RR)) +
        geom_ribbon(data = subset(dataset, dose < as.numeric(q[3]) && dose <= 35), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.25) +
        scale_x_continuous(expand = c(0, 0),
                           breaks = seq(from = 0, to = 35, by = 5)) + 
        scale_y_continuous(expand = c(0, 0),
                           breaks = seq(from = ifelse(ymin > 0, 0, round(ymin, 1) + 0.2), to = ymax, by = 0.2)) +
        coord_cartesian(xlim = c(0, 35)) + #, ylim = c(ymin, ymax)) +
        xlab(paste("\n", "Marginal MET hours per week", "\n")) +
        ylab("\nRelative Risk\n") +
        geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
        #geom_label(aes(label = "test"), data = q %>% as.data.frame(), nudge_x = 0.35, size = 4) +
        
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 12, colour = "black", vjust = 7),
          plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 1.05)) + 
        labs(title = paste(plotTitle)) #+ labs(fill = "") 
      
      if (max(dataset$dose) >= as.numeric(q[3])){
        gg <- gg + 
          geom_line(data = subset(dataset, dose >= as.numeric(q[3])), aes(x = dose, y = RR), linetype = "dashed") +
          geom_ribbon(data = subset(dataset, dose >= as.numeric(q[3])), aes(x = dose, ymin=`lb`,ymax=`ub`), alpha = 0.10)
      }
      
      qdf <- q %>% as.data.frame()
      names(qdf) <- 'val'
      
      p <- ggplotly(gg) %>% add_annotations(x = qdf$val,
                                            y = ymin,
                                            text = paste0("Person Years (", names(q), ")"),
                                            xref = "x",
                                            yref = "y",
                                            showarrow = TRUE,
                                            arrowhead = 4,
                                            arrowsize = .5,
                                            ax = 20,
                                            ay = -40,
                                            font=list(size = 7))
      
    }else{
      gg <- ggplot(data.frame()) + geom_point() + xlim(0, 100) + ylim(0, 1) + 
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 12, colour = "red", vjust = 7),
          plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="red"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 1.05)) +
        labs (title = "Sorry no data is available")
      
      p <- ggplotly(gg)
    }
    
    p
  }
  
  output$lowest_guideline <- renderUI({
    
    input$in_outcome
    input$in_outcome_type
    input$total_sub_population
    input$in_main_quantile
    
    isolate({
      in_outcome <- input$in_outcome
      in_outcome_type <- input$in_outcome_type
      total_sub_population <- input$total_sub_population
      in_main_quantile <- input$in_main_quantile
    })
    
    HTML("")
    
    if (total_sub_population == "1"){
      acmfdata <- get_overall_data()
      if (!is.null(acmfdata) && nrow(acmfdata) > 0 && nrow(overall_pop_dose_res_data()) > 0){
        last_knot <- get_last_knot(acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        last_knot <- round(last_knot[2], 2)
        
        HTML("<b>Potential Impact Fraction (PIF)</b> <br/>",
             get_pif_values(dataset = acmfdata, plot_data = overall_pop_dose_res_data(), last_knot = last_knot , dose_value = 4.375), 
             " of all cases could be prevented if all people met half the WHO recommended levels of physical activity (4.375 marginal MET hours per week).")
      }
      
    }
    else{# Sub-population
      m_acmfdata <- get_male_subpopulation_data()
      if (!is.null(m_acmfdata) && nrow(m_acmfdata) > 0){
        m_last_knot <- get_last_knot(m_acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        m_last_knot <- round(m_last_knot[2], 2)
      }
      
      w_acmfdata <- get_female_subpopulation_data()
      if (!is.null(w_acmfdata) && nrow(w_acmfdata) > 0){
        w_last_knot <- get_last_knot(w_acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        w_last_knot <- round(w_last_knot[2], 2)
      }
      
      if (!is.null(m_acmfdata) && !is.null(w_acmfdata) && nrow(m_acmfdata) > 0 && nrow(w_acmfdata) > 0 &&
          nrow(male_pop_dose_res_data()) > 0 && nrow(female_pop_dose_res_data() > 0)){
        
        HTML("<b>Potential Impact Fraction (PIF)</b> <br/>",
             get_pif_values(dataset = m_acmfdata, plot_data = male_pop_dose_res_data(), last_knot = m_last_knot , dose_value = 4.375), 
             " of all cases in <u>men</u></b> and ",
             get_pif_values(dataset = w_acmfdata, plot_data = female_pop_dose_res_data(), last_knot = w_last_knot , dose_value = 4.375),  
             "of all cases in <u>women</u> could be prevented if all people met half the WHO recommended levels of physical activity (4.375 marginal MET hours per week).")
      }

    }
  }) %>% bindCache(input$in_outcome,
                   input$in_outcome_type,
                   input$total_sub_population,
                   input$in_main_quantile)
  
  
  output$lower_guideline <- renderUI({
    
    input$in_outcome
    input$in_outcome_type
    input$total_sub_population
    input$in_main_quantile
    
    isolate({
      in_outcome <- input$in_outcome
      in_outcome_type <- input$in_outcome_type
      total_sub_population <- input$total_sub_population
      in_main_quantile <- input$in_main_quantile
    })
    
    HTML("")
    
    if (total_sub_population == "1"){
      acmfdata <- get_overall_data()
      if (!is.null(acmfdata) && nrow(acmfdata) > 0 && nrow(overall_pop_dose_res_data()) > 0){
        last_knot <- get_last_knot(acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        last_knot <- round(last_knot[2], 2)
        HTML(get_pif_values(dataset = acmfdata, plot_data = overall_pop_dose_res_data(), last_knot = last_knot , dose_value = 8.75), 
             " of all cases could be prevented if all people met the WHO recommended levels of physical activity (8.75 marginal MET hours per week).")
      }
      
    }
    else{# Sub-population
      
      m_acmfdata <- get_male_subpopulation_data()
      if (!is.null(m_acmfdata) && nrow(m_acmfdata) > 0){
        m_last_knot <- get_last_knot(m_acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        m_last_knot <- round(m_last_knot[2], 2)
      }
      
      w_acmfdata <- get_female_subpopulation_data()
      if (!is.null(w_acmfdata) && nrow(w_acmfdata) > 0){
        w_last_knot <- get_last_knot(w_acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        w_last_knot <- round(w_last_knot[2], 2)
      }
      
      if (!is.null(m_acmfdata) && !is.null(w_acmfdata) && nrow(m_acmfdata) > 0 && nrow(w_acmfdata) > 0 &&
          nrow(male_pop_dose_res_data()) > 0 && nrow(female_pop_dose_res_data() > 0)){
        HTML(get_pif_values(dataset = m_acmfdata, plot_data = male_pop_dose_res_data(), last_knot = m_last_knot , dose_value = 8.75), 
             " of all cases in <u>men</u></b> and ",
             get_pif_values(dataset = w_acmfdata, plot_data = female_pop_dose_res_data(), last_knot = w_last_knot , dose_value = 8.75),  
             "of all cases in <u>women</u> could be prevented if all people met the WHO recommended levels of physical activity (8.75 marginal MET hours per week).")
      }
    }
  }) %>% bindCache(input$in_outcome,
                   input$in_outcome_type,
                   input$total_sub_population,
                   input$in_main_quantile)
  
  
  output$upper_guideline <- renderUI({
    
    input$in_outcome
    input$in_outcome_type
    input$total_sub_population
    input$in_main_quantile
    
    isolate({
      in_outcome <- input$in_outcome
      in_outcome_type <- input$in_outcome_type
      total_sub_population <- input$total_sub_population
      in_main_quantile <- input$in_main_quantile
    })
    
    HTML("")
    
    if (total_sub_population == "1"){
      acmfdata <- get_overall_data()
      if (!is.null(acmfdata) && nrow(acmfdata) > 0 && nrow(overall_pop_dose_res_data()) > 0){
        last_knot <- get_last_knot(acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        last_knot <- round(last_knot[2], 2)
        HTML(get_pif_values(dataset = acmfdata, plot_data = overall_pop_dose_res_data(), last_knot = last_knot , dose_value = 17.5), 
             " of all cases could be prevented if all people met twice the WHO recommended levels of physical activity (17.5 marginal MET hours per week).")
      }
    }
    else{# Sub-population
      m_acmfdata <- get_male_subpopulation_data()
      if (!is.null(m_acmfdata) && nrow(m_acmfdata) > 0){
        m_last_knot <- get_last_knot(m_acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        m_last_knot <- round(m_last_knot[2], 2)
      }
      
      w_acmfdata <- get_female_subpopulation_data()
      if (!is.null(w_acmfdata) && nrow(w_acmfdata) > 0){
        w_last_knot <- get_last_knot(w_acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        w_last_knot <- round(w_last_knot[2], 2)
      }
      
      if (!is.null(m_acmfdata) && !is.null(w_acmfdata) && nrow(m_acmfdata) > 0 && nrow(w_acmfdata) > 0  &&
          nrow(male_pop_dose_res_data()) > 0 && nrow(female_pop_dose_res_data() > 0)){
        HTML(get_pif_values(dataset = m_acmfdata, plot_data = male_pop_dose_res_data(), last_knot = m_last_knot , dose_value = 17.5), 
             " of all cases in <u>men</u></b> and ",
             get_pif_values(dataset = w_acmfdata, plot_data = female_pop_dose_res_data(), last_knot = w_last_knot , dose_value = 17.5),  
             " of all cases in <u>women</u> could be prevented if all people met twice the WHO recommended levels of physical activity (17.5 marginal MET hours per week).")
      }
    }
    
  }) %>% bindCache(input$in_outcome,
                   input$in_outcome_type,
                   input$total_sub_population,
                   input$in_main_quantile)
  
  output$overall_datatable <- DT::renderDataTable({
    
    overall_data <- get_overall_data()
    
    if(is.null(overall_data) || nrow(overall_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$overall_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    
    overall_data <- subset(overall_data, select = c(ref_number, first_author, effect_measure, 
                                                    totalpersons, personyrs, cases, dose, RR, lci_effect, uci_effect))
    
    # Remove gender specific suffix from ref_number
    overall_data$ref_number <- sapply(strsplit(as.character(overall_data$ref_number),"-"), `[`, 1)
    
    fname <- "total_population"
    # Round relevant columns
    overall_data$totalpersons <- round(overall_data$totalpersons)
    overall_data$personyrs <- round(overall_data$personyrs)
    
    overall_data <- overall_data %>% dplyr::rename("ref_id" = "ref_number", 
                                                   "persons" = "totalpersons",
                                                   "person_years" = "personyrs",
                                                   "risk_estimate" = "RR")
    
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
    
    sub_population_data <- get_male_subpopulation_data()
    if(is.null(sub_population_data) || nrow(sub_population_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$male_sub_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    
    # Subset by columns
    sub_population_data <- subset(sub_population_data, select = c(ref_number, first_author, effect_measure, totalpersons, personyrs, cases, dose, RR, lci_effect, uci_effect))
    
    # Remove gender specific suffix from ref_number
    sub_population_data$ref_number <- sapply(strsplit(sub_population_data$ref_number,"-"), `[`, 1)
    
    fname <- "male_population"
    
    sub_population_data <- sub_population_data %>% dplyr::rename("ref_id" = "ref_number", 
                                                   "persons" = "totalpersons",
                                                   "person_years" = "personyrs",
                                                   "risk_estimate" = "RR")
    
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
    
    sub_population_data <- get_female_subpopulation_data()
    if(is.null(sub_population_data) || nrow(sub_population_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$female_sub_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    
    # Subset by columns
    sub_population_data <- subset(sub_population_data, select = c(ref_number, first_author, effect_measure, totalpersons, personyrs, dose, cases, RR, lci_effect, uci_effect))
    
    # Round relevant columns
    sub_population_data$totalpersons <- round(sub_population_data$totalpersons)
    sub_population_data$personyrs <- round(sub_population_data$personyrs)
    
    fname <- "female_population"
    
    # Remove gender specific suffix from ref_number
    sub_population_data$ref_number <- sapply(strsplit(sub_population_data$ref_number,"-"), `[`, 1)
    
    sub_population_data <- sub_population_data %>% dplyr::rename("ref_id" = "ref_number", 
                                                                 "persons" = "totalpersons",
                                                                 "person_years" = "personyrs",
                                                                 "risk_estimate" = "RR")
    
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
    
    input$in_outcome
    input$in_outcome_type
    input$total_sub_population
    input$in_main_quantile
    input$plot_options
    
    isolate({
      in_outcome <- input$in_outcome
      in_outcome_type <- input$in_outcome_type
      total_sub_population <- input$total_sub_population
      in_main_quantile <- input$in_main_quantile
      plot_options <- input$plot_options
    })
    
    dat <- data.frame()
    
    sketch = htmltools::withTags(table(
      class = 'display'))
    
    if (total_sub_population == "1"){
      overall_data <- get_overall_data()
      dat <- data.frame()
      
      if (!is.null(overall_data) && nrow(overall_data) > 0){
        
        sketch = htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 1, 'Marginal MET hours per week'),
              th(colspan = 1, 'Relative risk and 95% confidence interval')
            )
          )
        ))
        
        last_knot <- get_last_knot(overall_data, personyrs_pert = in_main_quantile %>% as.numeric() %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric() %>% as.numeric())
        
        last_knot <- round(last_knot[2], 2)
        
        snake_case_outcome <- gsub(x = in_outcome, pattern = " ", replacement = "-") %>% tolower()
        snake_case_outcome_type <- gsub(x = in_outcome_type, pattern = " ", replacement = "-") %>% tolower()
        ma_filename <- paste0(snake_case_outcome, "-", snake_case_outcome_type)
        
        plot_data <- overall_pop_tbles %>% filter(filename == ma_filename & quantile == in_main_quantile %>% as.numeric()) %>% dplyr::select(-c(filename, quantile))
        
        if (nrow(plot_data) > 0){
          colnames(plot_data) <- c("dose","RR", "lb", "ub")
          dat <- data.frame("Marginal MET hours per week" = c(4.375, 8.75, 17.5), "Relative risk and 95% confidence interval" = paste(get_ma_table(plot_data, "RR"), " (", get_ma_table(plot_data, "lb"),
                                                                     " - ", get_ma_table(plot_data, "ub"), ")", sep = ""), check.names = F)
        }
        
      }
    }else{# Sub-population
      
      m_acmfdata <- get_male_subpopulation_data()
      if(!is.null(m_acmfdata) && nrow(m_acmfdata) > 0){
        m_last_knot <- get_last_knot(m_acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        m_last_knot <- round(m_last_knot[2], 2)
        
        snake_case_outcome <- gsub(x = in_outcome, pattern = " ", replacement = "-") %>% tolower()
        snake_case_outcome_type <- gsub(x = in_outcome_type, pattern = " ", replacement = "-") %>% tolower()
        ma_filename <- paste0("male-", snake_case_outcome, "-", snake_case_outcome_type)
        
        m_plot_data <- gender_pop_tbles %>% filter(filename == ma_filename & quantile == in_main_quantile %>% as.numeric()) %>% dplyr::select(-c(filename, quantile))
        
        colnames(m_plot_data) <- c("dose","RR", "lb", "ub")
        
      }
      
      w_acmfdata <- get_female_subpopulation_data()
      td <<- w_acmfdata
      if(!is.null(w_acmfdata) && nrow(w_acmfdata) > 0){
        w_last_knot <- get_last_knot(w_acmfdata, personyrs_pert = in_main_quantile %>% as.numeric(), dose_pert = in_main_quantile %>% as.numeric())
        w_last_knot <- round(w_last_knot[2], 2)
        
        snake_case_outcome <- gsub(x = in_outcome, pattern = " ", replacement = "-") %>% tolower()
        snake_case_outcome_type <- gsub(x = in_outcome_type, pattern = " ", replacement = "-") %>% tolower()
        ma_filename <- paste0("female-", snake_case_outcome, "-", snake_case_outcome_type)
        
        w_plot_data <- gender_pop_tbles %>% filter(filename == ma_filename & quantile == in_main_quantile %>% as.numeric()) %>% dplyr::select(-c(filename, quantile))
        
        colnames(w_plot_data) <- c("dose","RR", "lb", "ub")
      }
      # MMET = c(4.375, 8.75, 17.5),  
      if (!is.null(m_acmfdata) && !is.null(w_acmfdata) && nrow(m_acmfdata) > 0 && nrow(w_acmfdata) > 0 && nrow(m_plot_data) > 0 && nrow(w_plot_data) > 0){
        dat <- data.frame("Marginal MET hours per week" = c(4.375, 8.75, 17.5), 'Male RR' = paste(get_ma_table(m_plot_data, "RR"), " (", get_ma_table(m_plot_data, "lb"),
                                                                          " - ", get_ma_table(m_plot_data, "ub"), ")", sep = ""),
                          'Female RR' = paste(get_ma_table(w_plot_data, "RR"), " (", get_ma_table(w_plot_data, "lb"),
                                              " - ", get_ma_table(w_plot_data, "ub"), ")", sep = ""), check.names = FALSE)
        
        sketch = htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 2, 'Marginal MET hours per week'),
              th(colspan = 2, 'Relative risk and 95% confidence interval')
            ),
            tr(
              lapply(rep(c('Men', 'Women'), 1), th)
            )
          )
        ))
        
        
      }
    }
    DT::datatable(dat, container = sketch, options = list(paging = F, dom = 't'), rownames = FALSE) #%>%
  }) 
  # %>% bindCache(input$in_outcome,
  #                  input$in_outcome_type,
  #                  input$total_sub_population,
  #                  input$in_sub_quantile,
  #                  input$plot_options)
  
  get_ma_table <- function(plot_data, colname = "RR"){
    
    c(round(plot_data[[colname]][which.min(abs(plot_data$dose - 4.375))], 2),
      round(plot_data[[colname]][which.min(abs(plot_data$dose - 8.75))], 2),
      round(plot_data[[colname]][which.min(abs(plot_data$dose - 17.5))], 2))#,
  }
  
  get_pif_values <- function(dataset, plot_data, last_knot, dose_value){
    
    if (nrow(dataset) > 0){
      if (max(dataset$dose) < dose_value)
        return(0)
    }
    
    if (nrow(dataset) > 0){
      
      local_cov_method <- F
      
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
      paste(ifelse(input$total_sub_population == "1", "total-population-", "male-population-"), 
            gsub(x = input$in_outcome, pattern = " ", replacement = "-") %>% tolower(), "-", 
            gsub(x = input$in_outcome_type, pattern = " ", replacement = "-") %>% tolower(), "-q-", input$in_main_quantile,  ".csv", sep="")
    },
    content = function(file) {
      write.csv(to_download$top_plot_data, file)
    }
  )
  
  
  output$download_bottom_data <- downloadHandler(
    filename = function() {
      paste(ifelse(input$total_sub_population == "1", "total-population-", "female-population-"), 
            gsub(x = input$in_outcome, pattern = " ", replacement = "-") %>% tolower(), "-", 
            gsub(x = input$in_outcome_type, pattern = " ", replacement = "-") %>% tolower(), "-q-", input$in_main_quantile,  ".csv", sep="")
    },
    content = function(file) {
      write.csv(to_download$bottom_plot_data, file)
    }
  )
  
  # Only requred to run if the region changes (as that affects purpose) or the purpose changes (as that affects geographies)
  observe({
    
    #isolate({
    update_outcomes(input_outcome_cat())
    #})
  })
  
  input_outcome_cat <- reactive({
    if(is.null(input$in_outcome_cat)) {
      "All-cause mortality"
    } else {
      input$in_outcome_cat
    }
  })
  
  update_outcomes <- function(outcome){
    
    # Identify locally available purposes and update list accordingly
    # if(!is.null(outcome)){
      
      # c("All-cause mortality" = 1, 
      #   "Cardiovascular diseases" = 2, 
      #   "Cancers" = 3,
      #   "Neurological disorders" = 4,
      #   "Others" = 5)
      
      local_outcome_choices <- selected_outcome <- "All-cause mortality"
      
      if (outcome == "2"){
        
        local_outcome_choices <- (uoutcome %>% filter(outcome %in% c("All-cause cvd",
                                                                    "Coronary heart disease",
                                                                    "Heart failure",
                                                                    "Stroke")))$outcome
        
      }else if (outcome == "3"){
        
        local_outcome_choices <- (uoutcome %>% filter(str_detect(outcome, "cancer") | outcome %in% c("Myeloid leukemia", "Myeloma")))$outcome
        
      }else if (outcome == "4"){
        
        local_outcome_choices <- (uoutcome %>% filter(outcome %in% c("Depression", 
                                                                     "Depressive symptoms", 
                                                                     "Major depression"
                                                                     )))$outcome
        
      }else if (outcome == "5"){
        
        local_outcome_choices <- (uoutcome %>% filter(outcome %in% c("All-cause dementia", 
                                                                    "Alzheimer's disease",
                                                                    "Parkinson's disease", 
                                                                    "Vascular dementia")))$outcome
        
      }
      
      updateSelectInput(session, "in_outcome", 
                        choices = local_outcome_choices, 
                        selected = local_outcome_choices[1])
      
    # }
  }
  
})
