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
      # if (input$in_outcome == "all-cause mortality")
      #   acmfdata <- subset(acmfdata, n_baseline >= 40000)
      # else
      #   acmfdata <- subset(acmfdata, n_baseline >= 10000)
      
      # if (isolate(input$in_outcome) == "Cardiovascular Disease"){
      #   #td <<- acmfdata
      #   acmfdata <- subset(acmfdata, !is.na(rr))
      # }
      #   
        
      
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
          
          dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, 
                                             ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = input$in_sub_quantile[2]))
          
          colnames(dataset) <- c("dose","RR", "lb", "ub")
        
          last_knot <- get_last_knot(dataset, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
          
          last_knot <- last_knot[2]
          
          q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
          
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
        
        if (isolate(input$in_outcome) == "Coronary Heart Disease" || isolate(input$in_outcome) == "Cardiovascular Disease" || isolate(input$in_outcome) == "stroke")
          local_cov_method <- T
        
        
        plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", returnval = T, covMethed = local_cov_method, minQuantile = 0, maxQuantile = last_knot))
        colnames(plot_data) <- c("dose","RR", "lb", "ub")
        
        fig_title <- input$in_outcome
        if (fig_title != toupper(fig_title))
          fig_title <- stringi::stri_trans_totitle(fig_title)
        
        fig_title <- paste0(pop_title, " - ", outcome_type,  fig_title, "\n Number of samples: ",  length(unique(acmfdata$id)) , 
                                                                      " & Number of people: " , formatC(round(sum(acmfdata$totalpersons, na.rm = T)), 
                                                                                                        format = "f", big.mark = ",", drop0trailing = TRUE))
        getPlot(dataset = plot_data, last_knot = last_knot, plotTitle = fig_title)
        
      }else{
        
        getPlot(dataset = NULL, last_knot = NULL, plotTitle =  "")
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
        
        dataset <- data.frame(metaAnalysis(acmfdata, returnval = T, 
                                           ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = input$in_sub_quantile[2]))
        
        colnames(dataset) <- c("dose","RR", "lb", "ub")
        
        last_knot <- get_last_knot(dataset, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
        
        last_knot <- last_knot[2]
        
        q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
      
        get_dose_plot(acmfdata, q, plot_title = p_title)
      }else{
        
        get_dose_plot(NULL, 0, "")
      }
      
    }
    
    else{
      
      sub_pop_data <- get_subpopulation_data(PA_exposure = pa_exposure, outcome = input$in_outcome, outcome_types = input$in_outcome_type, gender = 2)
      
      if (input$plot_options == 1){
        
        if (nrow(sub_pop_data) > 0){
          
          td <<- sub_pop_data
          
          local_cov_method <- F
          
          if (isolate(input$in_outcome) == "Coronary Heart Disease" || isolate(input$in_outcome) == "Cardiovascular Disease" 
              || isolate(input$in_outcome) == "stroke")
            local_cov_method <- T
          
          last_knot <- get_last_knot(sub_pop_data, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
          
          last_knot <- last_knot[2]
          
          plot_data <- data.frame(metaAnalysis(sub_pop_data, ptitle = "", covMethed = local_cov_method, returnval = T, minQuantile = 0, maxQuantile = last_knot))
          colnames(plot_data) <- c("dose","RR", "lb", "ub")
          
          getPlot(dataset = plot_data, last_knot = last_knot, plotTitle = get_title(dataset = sub_pop_data, pop_type = "female"))
          
        }else{
          
          gt <- "Female Population"
          #if (input$in_sub_population == 2)
          #  gt <- "Female Population"
          
          fig_title <- input$in_outcome
          if (fig_title != toupper(fig_title))
            fig_title <- stringi::stri_trans_totitle(fig_title)
          
          fig_title <- paste0(gt, " - ", fig_title)
          
          getPlot(dataset = NULL, last_knot = NULL, plotTitle =  get_title(dataset = NULL, pop_type = "female"))
          
        }
        
      }else{
        
        if (nrow(sub_pop_data) > 0){
          
          
          dataset <- data.frame(metaAnalysis(sub_pop_data, returnval = T, 
                                             ptitle = "", covMethed = T, minQuantile = 0, maxQuantile = input$in_sub_quantile[2]))
          
          colnames(dataset) <- c("dose","RR", "lb", "ub")
          
          last_knot <- get_last_knot(dataset, personyrs_pert = input$in_sub_quantile[2], dose_pert = input$in_sub_quantile[2])
          
          last_knot <- last_knot[2]
          
          q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
          
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
      
      fig_title <- paste0(gt, " - ", outcome_type, fig_title, "\n Number of samples: ",  
                          length(unique(dataset$id)) , 
                          " & Number of people: " , formatC(round(sum(dataset$totalpersons, na.rm = T)), 
                                                            format = "f", big.mark = ",", drop0trailing = TRUE))
      
      }
    
    fig_title
    
    
  }
  
  
  get_dose_plot <- function (dataset, q, plot_title){
    
    
    if (!is.null(dataset)){
      
      group_by(dataset, id) %>% select(dose, se) %>%
        summarise(min = min(dose), max = max(dose), ref = dose[is.na(se)])
      gg <- ggplotly(
        ggplot(dataset, aes(dose, rr, col = ref_number, label = personyrs)) + geom_point() +
          geom_line() +
          #scale_y_continuous(trans = "log", breaks = c(.1, .25, .5, .75, 1, 1.25)) +
          scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
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
  
  
  getPlot <- function (dataset, last_knot, plotTitle, xlab = "Marginal MET hours per week" ){
    
    
    # outfile <- tempfile(fileext='.png')
    # 
    # # Generate the PNG
    # png(outfile, width=400, height=300)
    
    if (!is.null(dataset)){
      
      q <- quantile(dataset$dose, c(0, last_knot / 2, last_knot))
      
      gg <- ggplot(dataset, aes(dose, RR)) + 
        geom_line(data = dataset) + 
        geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
        scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
        xlab(paste("\n", xlab, "\n")) +
        ylab("\nRelative Risk\n") +
        geom_vline(xintercept= q, linetype="dotted", alpha=0.4) + 
        
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 12, colour = "black", vjust = 7),
          plot.subtitle = element_text(size = 10, hjust=0.5, face="italic", color="black"),
          legend.direction = "horizontal",
          legend.position = c(0.1, 1.05)) + 
        labs(title = paste(plotTitle)) #+ labs(fill = "") 
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
  
  set_pif_values <- reactive({
    
    acmfdata <- get_overall_data(PA_exposure = pa_exposure, outcome_disease = input$in_outcome, outcome_types = input$in_outcome_type)

    if (nrow(acmfdata) > 0){
      
      local_cov_method <- F
      
      if (isolate(input$in_outcome) == "Coronary Heart Disease")
        local_cov_method <- T
      
      last_knot <- get_last_knot(acmfdata, personyrs_pert = input$in_main_quantile[2], dose_pert = input$in_main_quantile[2])
      
      last_knot <- last_knot[2]

      plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", returnval = T, covMethed = local_cov_method, maxQuantile = last_knot, lout = 1000))
      colnames(plot_data) <- c("dose","RR", "lb", "ub")

      removeNA <- F

      # Update RR from the lookup table
      for (i in 1:nrow(acmfdata)){
        val <- subset(plot_data, round(dose, 1) <= (acmfdata$dose[i] + 0.05) & round(dose, 1) >= (acmfdata$dose[i] - 0.05))
        if (nrow(val) > 0){
          acmfdata$rr[i] <- val$RR[1]
          if (removeNA){
            if (!is.na(acmfdata$lci[i]))
              acmfdata$lci[i] <- val$lb[1]
            if (!is.na(acmfdata$lci[i]))
              acmfdata$uci[i] <- val$ub[1]
          }else{
            acmfdata$lci[i] <- val$lb[1]
            acmfdata$uci[i] <- val$ub[1]
          }
        }
      }

      sum_tp <- sum(acmfdata$totalpersons * acmfdata$rr, na.rm = T)

      acmfdata_ls <- acmfdata

      #Replace lower dose with 8.75
      acmfdata_ls[acmfdata_ls$dose < 8.75,]$dose <- 8.75

      local_var <- acmfdata_ls

      val <- subset(plot_data, round(dose, 1) <= (8.75 + 0.05) & round(dose, 1) >= (8.75 - 0.05))

      if (nrow(val) > 0)
        acmfdata_ls[acmfdata_ls$dose == 8.75,]$rr <- val$RR[1]

      sum_ls_tp <- sum(acmfdata$totalpersons * acmfdata_ls$rr, na.rm = T)

      pert_ls <- ((sum_tp - sum_ls_tp) / sum_tp) * 100

      acmfdata_ls <- local_var

      if (nrow(val) > 0){

        if (removeNA){
          acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$uci),]$uci <- val$ub[1]
        }else{
          acmfdata_ls[acmfdata_ls$dose == 8.75,]$uci <- val$ub[1]
        }

      }

      sum_ls_lower_tp <- sum(acmfdata$totalpersons * acmfdata_ls$uci, na.rm = T)

      sum_tp <- sum(acmfdata$totalpersons * acmfdata$uci, na.rm = T)

      pert_ls_lower <- ((sum_tp - sum_ls_lower_tp) / sum_tp) * 100

      acmfdata_ls <- local_var

      if (nrow(val) > 0){
        if (removeNA){
          acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$uci),]$lci <- val$lb[1]
        }else{
          acmfdata_ls[acmfdata_ls$dose == 8.75,]$lci <- val$lb[1]
        }
      }

      sum(acmfdata_ls$lci, na.rm = T)

      sum(acmfdata$lci, na.rm = T)

      sum_ls_upper_tp <- sum(acmfdata$totalpersons * acmfdata_ls$lci, na.rm = T)

      sum_tp <- sum(acmfdata$totalpersons * acmfdata$lci, na.rm = T)

      pert_ls_upper <- ((sum_tp - sum_ls_upper_tp) / sum_tp) * 100

      lower_guideline_value <<- paste0(round(pert_ls, 2) , "% (95% CI: ", round(pert_ls_lower, 2), " - ",  round(pert_ls_upper, 2), ")" )

      acmfdata_hs <- acmfdata

      #Replace higher dose with 17.5
      acmfdata_hs[acmfdata_hs$dose < 17.5,]$dose <- 17.5

      local_var <- acmfdata_hs

      val <- subset(plot_data, round(dose, 1) <= (17.5 + 0.05) & round(dose, 1) >= (17.5 - 0.05))
      if (nrow(val) == 0)
        val <- subset(plot_data, round(dose, 1) <= (17.5 + 0.1) & round(dose, 1) >= (17.5 - 0.1))

      if (nrow(val) > 0){
        acmfdata_hs[acmfdata_hs$dose == 17.5,]$rr <- val$RR[1]
      }

      sum_hs_tp <- sum(acmfdata$totalpersons * acmfdata_hs$rr, na.rm = T)

      sum_tp <- sum(acmfdata$totalpersons * acmfdata$rr, na.rm = T)

      pert_hs <- ((sum_tp - sum_hs_tp) / sum_tp) * 100

      acmfdata_hs <- local_var

      if (nrow(val) > 0){
        if (removeNA){
          acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$uci),]$uci <- val$ub[1]
        }else{
          acmfdata_hs[acmfdata_hs$dose == 17.5,]$uci <- val$ub[1]
        }
      }

      sum_hs_lower_tp <- sum(acmfdata$totalpersons * acmfdata_hs$uci, na.rm = T)

      sum_tp <- sum(acmfdata$totalpersons * acmfdata$uci, na.rm = T)

      pert_hs_lower <- ((sum_tp - sum_hs_lower_tp) / sum_tp) * 100

      acmfdata_hs <- local_var

      if (nrow(val) > 0){
        if (removeNA){
          acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$uci),]$lci <- val$lb[1]
        }else{
          acmfdata_hs[acmfdata_hs$dose == 17.5,]$lci <- val$lb[1]
        }
      }

      sum_hs_upper_tp <- sum(acmfdata$totalpersons * acmfdata_hs$lci, na.rm = T)

      sum_tp <- sum(acmfdata$totalpersons * acmfdata$lci, na.rm = T)

      pert_hs_upper <- ((sum_tp - sum_hs_upper_tp) / sum_tp) * 100

      upper_guideline_value <<- paste0(round(pert_hs, 2) , "% (95% CI: ", round(pert_hs_lower, 2), " - ",  round(pert_hs_upper, 2), ")" )
    }
    
    
  })
  
  output$lower_guideline <- renderUI({
    set_pif_values()
    HTML("PIF for meeting the lower WHO guideline (MMET >= 8.75 per week): ", lower_guideline_value, "\n")
  })
  
  output$upper_guideline <- renderUI({
    set_pif_values()
    HTML("PIF for meeting the upper WHO guideline (MMET >= 17.5 per week): ", upper_guideline_value , "\n")
  })
  
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
    
    # Convert id into factor and then back to numeric for an ordered id
    sub_population_data$id <- as.numeric(as.factor(sub_population_data$id))
    # Remove gender specific suffix from ref_number
    sub_population_data$ref_number <- sapply(strsplit(sub_population_data$ref_number," "), `[`, 1)
    
    # Empty the warning message - as some lines have been selected by the user
    output$female_sub_warning_message <- renderUI("")
    DT::datatable(sub_population_data, options = list(pageLength = 20)) #%>%
    #formatRound(columns = names(numeric_line_col_names), digits=2)
  })
})
