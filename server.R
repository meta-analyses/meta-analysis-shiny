options(shiny.sanitize.errors = F)
lower_guideline_value <- NA
upper_guideline_value <- NA
shinyServer(function(input, output, session){
  
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
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
    }
    
    acmfdata
  }
  
  
  
  output$top_plot <- renderPlotly({
    input$in_outcome
    input$in_outcome_type
    input$total_sub_population
    
    pop_title <- "Total Population"
    
    if (input$total_sub_population == 1)
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
    else{# Sub-population
      acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 1)
      pop_title <- "Male Population"
    }
    
    if (input$total_sub_population == 2 && input$plot_options == 2){
      
        if (nrow(acmfdata) > 0){
          
          #dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, 
                                             # ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = input$in_sub_quantile[2]))
          
          #colnames(dataset) <- c("dose","RR", "lb", "ub")
        
          last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
          
          last_knot <- last_knot[2]
          
          q <- quantile(acmfdata$dose, c(0, last_knot / 2, last_knot))
          
          p_title <- get_title(dataset = acmfdata, pop_type = "female")
          
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
        
        if (input$total_sub_population == 2)
          last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
          
        last_knot <- last_knot[2]
        
        
        
        local_cov_method <- F
        
        if (input$in_outcome == "Coronary Heart Disease" || input$in_outcome == "Cardiovascular Disease" || input$in_outcome == "stroke")
          local_cov_method <- T
        
        plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", returnval = T, covMethed = local_cov_method, minQuantile = 0, maxQuantile = last_knot))
        colnames(plot_data) <- c("dose","RR", "lb", "ub")
        
        
        # cat("top plot \n")
        # print(data.frame(MMET = c(4.375, 8.75, 17.5, 35),  RR = get_ma_table(plot_data, "RR"), LB = get_ma_table(plot_data, "lb"),
        #                  UB = get_ma_table(plot_data, "ub")), quote = F, row.names = FALSE)
        
        
        
        
        
        fig_title <- input$in_outcome
        if (fig_title != toupper(fig_title))
          fig_title <- stringi::stri_trans_totitle(fig_title)
        
        fig_title <- paste0(pop_title, " - ", outcome_type,  fig_title, "\n Number of entries: ",  length(unique(acmfdata$id)) , 
                                                                      " & Number of people: " , formatC(round(sum(acmfdata$totalpersons, na.rm = T)), 
                                                                                                        format = "f", big.mark = ",", drop0trailing = TRUE))
        q <- quantile(acmfdata$dose, c(0, last_knot / 2, last_knot))
        
        getPlot(dataset = plot_data, q = q, plotTitle = fig_title, pop_title, input$in_outcome, outcome_type)
        
      }else{
        
        getPlot(dataset = NULL, q = NULL, plotTitle =  "", pop_title, input$in_outcome, outcome_type)
      }
    
    }
    
  })
  
  
  output$bottom_plot <- renderPlotly({
    
    input$in_outcome
    input$in_outcome_type
    input$total_sub_population
    
    if (input$total_sub_population == 1){
      
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      
      if (nrow(acmfdata) > 0){
      
        p_title <- get_title(dataset = acmfdata, pop_type = "total")
        
        # dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, 
        #                                    ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = input$in_sub_quantile[2]))
        # 
        # colnames(dataset) <- c("dose","RR", "lb", "ub")
        # 
        last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
        
        last_knot <- last_knot[2]
        
        q <- quantile(acmfdata$dose, c(0, last_knot / 2, last_knot))
      
        get_dose_plot(acmfdata, q, plot_title = p_title)
      }else{
        
        get_dose_plot(NULL, 0, "")
      }
      
    }
    
    else{
      
      sub_pop_data <- get_subpopulation_data(PA_exposure = pa_exposure, outcome = input$in_outcome, outcome_types = input$in_outcome_type, gender = 2)
      
      if (input$plot_options == 1){
        
        if (nrow(sub_pop_data) > 0){
          
          local_cov_method <- F
          
          if (isolate(input$in_outcome) == "Coronary Heart Disease" || isolate(input$in_outcome) == "Cardiovascular Disease" 
              || isolate(input$in_outcome) == "stroke")
            local_cov_method <- T
          
          last_knot <- get_last_knot(sub_pop_data, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
          
          last_knot <- last_knot[2]
          
          # cat("bottom plot \n")
          plot_data <- data.frame(metaAnalysis(sub_pop_data, ptitle = "", covMethed = local_cov_method, returnval = T, minQuantile = 0, maxQuantile = last_knot))
          colnames(plot_data) <- c("dose","RR", "lb", "ub")
          
          q <- quantile(sub_pop_data$dose, c(0, last_knot / 2, last_knot))
          
          
          getPlot(dataset = plot_data, q = q, plotTitle = get_title(dataset = sub_pop_data, pop_type = "female"), "female population", input$in_outcome, input$in_outcome_type)
          
        }else{
          
          gt <- "Female Population"
          #if (input$in_sub_population == 2)
          #  gt <- "Female Population"
          
          fig_title <- input$in_outcome
          if (fig_title != toupper(fig_title))
            fig_title <- stringi::stri_trans_totitle(fig_title)
          
          fig_title <- paste0(gt, " - ", fig_title)
          
          getPlot(dataset = NULL, q = NULL, plotTitle =  get_title(dataset = NULL, pop_type = "female"), "female population", input$in_outcome, input$in_outcome_type)
          
        }
        
      }else{
        
        if (nrow(sub_pop_data) > 0){
          
          
          # dataset <- data.frame(metaAnalysis(sub_pop_data, returnval = T, 
          #                                    ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = input$in_sub_quantile[2]))
          # 
          # colnames(dataset) <- c("dose","RR", "lb", "ub")
          # 
          last_knot <- get_last_knot(sub_pop_data, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
          
          last_knot <- last_knot[2]
          
          q <- quantile(sub_pop_data$dose, c(0, last_knot / 2, last_knot))
          
          p_title <- get_title(dataset = sub_pop_data, pop_type = "female")
          
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
                          " & Number of people: " , formatC(round(sum(dataset$totalpersons, na.rm = T)), 
                                                            format = "f", big.mark = ",", drop0trailing = TRUE))
      
      }
    
    fig_title
    
    
  }
  
  
  get_dose_plot <- function (dataset, q, plot_title){
    
    
    if (!is.null(dataset)){
      dataset$personyrs <- round(dataset$personyrs)
      
      group_by(dataset, id) %>% select(dose, se) %>%
        summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
      gg <- ggplotly(
        ggplot(dataset, aes(dose, rr, col = ref_number, label = personyrs)) + geom_point(size = 4 * (dataset$personyrs - min(dataset$personyrs))/diff(range(dataset$personyrs))) +
          geom_line() +
          #scale_y_continuous(trans = "log", breaks = c(.1, .25, .5, .75, 1, 1.25)) +
          scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
          coord_cartesian(xlim = c(0, max(dataset$dose))) +
          geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
          theme_classic() + guides(col = FALSE) + 
          xlab("\nMarginal MET hours per week\n") +
          ylab("\nRelative Risk\n") +
          labs(title = paste(plot_title)) +
          theme(
            plot.margin = unit(c(2, 1, 1, 1), "cm"), 
            plot.title = element_text(size = 12, colour = "black", vjust = 7),
            plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
            legend.direction = "horizontal",
            legend.position = c(0.1, 1.05))
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
      gg <- ggplot(dataset, aes(dose, RR)) + 
        geom_line(data = dataset) + 
        geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
        #scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
        scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
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
    
    if (input$total_sub_population == 1){
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
      last_knot <- last_knot[2]
      HTML("PIF for meeting the half the lower WHO guideline (MMET >= 4.375 per week): ", get_pif_values(dataset = acmfdata, last_knot = last_knot , dose_value = 4.375), "\n")
      
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
        HTML("<b>","Male","</b> <br/>", 
             "PIF for meeting the half the lower WHO guideline (MMET >= 4.375 per week): ", 
             get_pif_values(dataset = m_acmfdata, last_knot = m_last_knot , dose_value = 4.375), "<br/>",
             "<b>","Female","</b> <br/>", 
             "PIF for meeting the half the lower WHO guideline (MMET >= 4.375 per week)", 
             get_pif_values(dataset = w_acmfdata, last_knot = w_last_knot , dose_value = 4.375), "\n")
      }
    }
  })
  
  
  output$lower_guideline <- renderUI({
    
    HTML("")
    
    if (input$total_sub_population == 1){
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      if (nrow(acmfdata) > 0){
        last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
        last_knot <- last_knot[2]
        HTML("PIF for meeting the lower WHO guideline (MMET >= 8.75 per week): ", get_pif_values(dataset = acmfdata, last_knot = last_knot , dose_value = 8.75), "\n")
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
        HTML("<b>","Male","</b> <br/>", 
             "PIF for meeting the lower WHO guideline (MMET >= 8.75 per week): ", 
             get_pif_values(dataset = m_acmfdata, last_knot = m_last_knot , dose_value = 8.75), "\n",
             "<b>","Female","</b> <br/>", 
             "PIF for meeting the lower WHO guideline (MMET >= 8.75 per week): ", 
             get_pif_values(dataset = w_acmfdata, last_knot = w_last_knot , dose_value = 8.75), "\n")
      }
    }
  })
  
  
  output$upper_guideline <- renderUI({
    HTML("")
    
    if (input$total_sub_population == 1){
      acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
      last_knot <- last_knot[2]
      HTML("PIF for meeting the upper WHO guideline (MMET >= 17.5 per week): ", get_pif_values(dataset = acmfdata, last_knot = last_knot , dose_value = 17.5), "\n")
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
        
        HTML("<b>","Male","</b> <br/>", 
             "PIF for meeting the upper WHO guideline (MMET >= 17.5 per week): ", 
             get_pif_values(dataset = m_acmfdata, last_knot = m_last_knot , dose_value = 17.5), "\n",
            "<b>","Female","</b> <br/>", 
             "\n PIF for meeting the upper WHO guideline (MMET >= 17.5 per week): ", 
            get_pif_values(dataset = w_acmfdata, last_knot = w_last_knot , dose_value = 17.5), "\n")
      }
    }
    
  })
  
  
  # output$upper_guideline <- renderUI({
  #   set_pif_values()
  #   HTML("PIF for meeting the upper WHO guideline (MMET >= 17.5 per week): ", upper_guideline_value , "\n")
  # })
  
  output$overall_datatable <- DT::renderDataTable({
    
    overall_data <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
    
    if(nrow(overall_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$overall_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    

    overall_data <- subset(overall_data, select = c(id, ref_number, Author, effect_measure, totalpersons, personyrs, dose, rr, cases, lci_effect, uci_effect))
    # Remove gender specific suffix from ref_number
    overall_data$ref_number <- sapply(strsplit(overall_data$ref_number," "), `[`, 1)
    
    
    # Round relevant columns
    overall_data$totalpersons <- round(overall_data$totalpersons)
    overall_data$personyrs <- round(overall_data$personyrs)
    
    
    # Convert id into factor and then back to numeric for an ordered id
    overall_data$id <- as.numeric(as.factor(overall_data$id))
    
    # Empty the warning message - as some lines have been selected by the user
    output$overall_warning_message <- renderUI("")
    DT::datatable(overall_data, options = list(pageLength = 20)) #%>%
    #formatRound(columns = names(numeric_line_col_names), digits=2)
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
    sub_population_data <- subset(sub_population_data, select = c(id, ref_number, Author, effect_measure, totalpersons, personyrs, dose, rr, cases, lci_effect, uci_effect))
    
    # Convert id into factor and then back to numeric for an ordered id
    sub_population_data$id <- as.numeric(as.factor(sub_population_data$id))
    # Remove gender specific suffix from ref_number
    sub_population_data$ref_number <- sapply(strsplit(sub_population_data$ref_number," "), `[`, 1)
    
    # Empty the warning message - as some lines have been selected by the user
    output$male_sub_warning_message <- renderUI("")
    DT::datatable(sub_population_data, options = list(pageLength = 20)) #%>%
    #formatRound(columns = names(numeric_line_col_names), digits=2)
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
    sub_population_data <- subset(sub_population_data, select = c(id, ref_number, Author, effect_measure, totalpersons, personyrs, dose, rr, cases, lci_effect, uci_effect))
    
    # Round relevant columns
    sub_population_data$totalpersons <- round(sub_population_data$totalpersons)
    sub_population_data$personyrs <- round(sub_population_data$personyrs)
    
    # Convert id into factor and then back to numeric for an ordered id
    sub_population_data$id <- as.numeric(as.factor(sub_population_data$id))
    # Remove gender specific suffix from ref_number
    sub_population_data$ref_number <- sapply(strsplit(sub_population_data$ref_number," "), `[`, 1)
    
    # Empty the warning message - as some lines have been selected by the user
    output$female_sub_warning_message <- renderUI("")
    DT::datatable(sub_population_data, options = list(pageLength = 20)) #%>%
    #formatRound(columns = names(numeric_line_col_names), digits=2)
  })
  
  
  
  output$dose_range <- DT::renderDataTable({
    
    dat <- data.frame()
    
    if (input$total_sub_population == 1){
    
      overall_data <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)
      
      dat <- data.frame()
      
      if (nrow(overall_data) > 0){
        
        local_cov_method <- F
        
        if (isolate(input$in_outcome) == "Coronary Heart Disease" || isolate(input$in_outcome) == "Cardiovascular Disease" || isolate(input$in_outcome) == "stroke")
          local_cov_method <- T
        
        last_knot <- get_last_knot(overall_data, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
        
        last_knot <- last_knot[2]
        
        plot_data <- data.frame(metaAnalysis(overall_data, ptitle = "", returnval = T, covMethed = local_cov_method, maxQuantile = last_knot))
        
        colnames(plot_data) <- c("dose","RR", "lb", "ub")
        #MMET = c(4.375, 8.75, 17.5),  
        dat <- data.frame(MMET = c(4.375, 8.75, 17.5), RR = paste(get_ma_table(plot_data, "RR"), " (", get_ma_table(plot_data, "lb"),
                          " - ", get_ma_table(plot_data, "ub"), ")", sep = ""))
  
      }
    }else{# Sub-population
      
      local_cov_method <- F
      
      if (isolate(input$in_outcome) == "Coronary Heart Disease" || isolate(input$in_outcome) == "Cardiovascular Disease" || isolate(input$in_outcome) == "stroke")
        local_cov_method <- T
      
      m_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 1)
      if(nrow(m_acmfdata) > 0){
        m_last_knot <- get_last_knot(m_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        m_last_knot <- m_last_knot[2]
        m_plot_data <- data.frame(metaAnalysis(m_acmfdata, ptitle = "", returnval = T, covMethed = local_cov_method, maxQuantile = m_last_knot))
        colnames(m_plot_data) <- c("dose","RR", "lb", "ub")
        
      }
      
        
      w_acmfdata <- get_subpopulation_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type, gender = 2)
      if(nrow(m_acmfdata) > 0){
        w_last_knot <- get_last_knot(w_acmfdata, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        w_last_knot <- w_last_knot[2]
        w_plot_data <- data.frame(metaAnalysis(w_acmfdata, ptitle = "", returnval = T, covMethed = local_cov_method, maxQuantile = w_last_knot))
        colnames(w_plot_data) <- c("dose","RR", "lb", "ub")
      }
      
      
      # MMET = c(4.375, 8.75, 17.5),  
      if (nrow(m_acmfdata) > 0 && nrow(w_acmfdata) > 0){
        dat <- data.frame(MMET = c(4.375, 8.75, 17.5), 'Male RR' = paste(get_ma_table(m_plot_data, "RR"), " (", get_ma_table(m_plot_data, "lb"),
                                     " - ", get_ma_table(m_plot_data, "ub"), ")", sep = ""),
                          'Female RR' = paste(get_ma_table(w_plot_data, "RR"), " (", get_ma_table(w_plot_data, "lb"),
                                      " - ", get_ma_table(w_plot_data, "ub"), ")", sep = ""), check.names = FALSE)
      
      }
        
      
    }
    
    DT::datatable(dat, options = list(paging = F, dom = 't'), rownames= FALSE) #%>%
  })
  
  
  get_ma_table <- function(plot_data, colname = "RR"){
    
    c(round(plot_data[[colname]][which.min(abs(plot_data$dose - 4.375))], 2),
      round(plot_data[[colname]][which.min(abs(plot_data$dose - 8.75))], 2),
      round(plot_data[[colname]][which.min(abs(plot_data$dose - 17.5))], 2))#,
      #round(plot_data[[colname]][which.min(abs(plot_data$dose - 35))], 2))
  }
  
  
  
  get_pif_values <- function(dataset, last_knot, dose_value){
    
    if (nrow(dataset) > 0){
      if (max(dataset$dose) < dose_value)
        return(0)
    }
    
    if (nrow(dataset) > 0){
      
      local_cov_method <- F
      
      if (input$in_outcome == "Coronary Heart Disease" || input$in_outcome == "Cardiovascular Disease" || input$in_outcome == "stroke")
        local_cov_method <- T
      
      plot_data <- data.frame(metaAnalysis(dataset, ptitle = "", returnval = T, covMethed = local_cov_method, maxQuantile = last_knot, lout = 1000))
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      removeNA <- F
      
      # Update RR from the lookup table
      for (i in 1:nrow(dataset)){
        val <- subset(plot_data, round(dose, 1) <= (dataset$dose[i] + 0.05) & round(dose, 1) >= (dataset$dose[i] - 0.05))
        if (nrow(val) > 0){
          dataset$rr[i] <- val$RR[1]
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
      
      sum_tp <- sum(dataset$totalpersons * dataset$rr, na.rm = T)
      
      dataset_ls <- dataset
      
      #Replace lower dose with 8.75
      dataset_ls[dataset_ls$dose < dose_value,]$dose <- dose_value
      
      local_var <- dataset_ls
      
      val <- subset(plot_data, round(dose, 1) <= (dose_value + 0.05) & round(dose, 1) >= (dose_value - 0.05))
      
      if (nrow(val) > 0)
        dataset_ls[dataset_ls$dose == dose_value,]$rr <- val$RR[1]
      
      sum_ls_tp <- sum(dataset$totalpersons * dataset_ls$rr, na.rm = T)
      
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
      
      lower_guideline_value <- paste0(round(pert_ls, 2) , "% (95% CI: ", round(pert_ls_lower, 2), " - ",  round(pert_ls_upper, 2), ")" )
      
    }
    
  }
  
  
})
