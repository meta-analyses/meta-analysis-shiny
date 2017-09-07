options(shiny.sanitize.errors = F)
lower_guideline_value <- NA
upper_guideline_value <- NA
shinyServer(function(input, output, session){
  
  get_overall_data <- reactive({
    input$in_outcome 
    input$in_outcome_type
    input$in_PA_exposure
    input$in_sub_population
    
    acmfdata <- data.frame()
    
    if (!is.na(input$in_outcome_type) & input$in_outcome_type != 'all'){
      acmfdata <- subset(raw_data_tp_ltpa, 
                         outcome == input$in_outcome & 
                           pa_domain_subgroup == input$in_PA_exposure & 
                           outcome_type == input$in_outcome_type)
    }else{
      acmfdata <- subset(raw_data_tp_ltpa, 
                         outcome == input$in_outcome & 
                           pa_domain_subgroup == input$in_PA_exposure)
                           
      
    }
    
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                       (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Filter studies by study size
      if (input$in_outcome == "all-cause mortality")
        acmfdata <- subset(acmfdata, n_baseline >= 40000)
      else
        acmfdata <- subset(acmfdata, n_baseline >= 10000)
      
      if (isolate(input$in_outcome) == "Cardiovascular Disease"){
        #td <<- acmfdata
        acmfdata <- subset(acmfdata, !is.na(rr))
      }
        
        
      
    }
    acmfdata
    
  })
  
  get_subpopulation_data <- reactive({
    input$in_outcome 
    input$in_outcome_type
    input$in_PA_exposure
    input$in_sub_population
    
    acmfdata <- data.frame()
    if (is.na(input$in_outcome_type))
      return()
    
    if (input$in_outcome_type != 'all'){
      acmfdata <- subset(raw_data_gsp_ltpa, 
                       outcome == input$in_outcome & 
                         pa_domain_subgroup == input$in_PA_exposure & 
                         sex_subgroups == input$in_sub_population & 
                         outcome_type == input$in_outcome_type)
    }else{
      acmfdata <- subset(raw_data_gsp_ltpa, 
                         outcome == input$in_outcome & 
                           pa_domain_subgroup == input$in_PA_exposure & 
                           sex_subgroups == input$in_sub_population)
      
    }
    
    # Remove where both dose and response are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    
    if (nrow(acmfdata) > 0){
      acmfdata <- getMissingVariables(acmfdata, infertotalpersons = T, kcases = T)
      # Remove when totalperson is not available for hr, and personsyears for rr/or
      acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyrs) | personyrs == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
      
      # Filter studies by study size greater than 10k
      acmfdata <- subset(acmfdata, n_baseline > 10000)
      
    }
    
    acmfdata
    
    
  })
  
  
  
  output$plot_overall_analysis <- renderPlotly({
    
    acmfdata <- get_overall_data()
    
    if (nrow(acmfdata) > 0){
      
      local_cov_method <- F
      
      if (isolate(input$in_outcome) == "Coronary Heart Disease")
        local_cov_method <- T
      
      plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", returnval = T, covMethed = local_cov_method, minQuantile = input$in_main_quantile[1], maxQuantile = input$in_main_quantile[2]))
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      fig_title <- "all-cause mortality"
      if (fig_title != toupper(fig_title))
        fig_title <- stringi::stri_trans_totitle(fig_title)
      
      fig_title <- paste0("Overall Population - ", fig_title, "\n Number of samples: ",  length(unique(acmfdata$id)) , 
                                                                    " & Number of people: " , formatC(round(sum(acmfdata$totalpersons, na.rm = T)), 
                                                                                                      format = "f", big.mark = ",", drop0trailing = TRUE))
      getPlot(plot_data, fig_title)
      
    }else{
      
      getPlot(NULL, "")
    }
    
  })
  
  
  output$plot_subpopulation_analysis <- renderPlotly({
    
    
    acmfdata <- get_subpopulation_data()
    
    if (nrow(acmfdata) > 0){
      
      plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = input$in_sub_quantile[1], maxQuantile = input$in_sub_quantile[2]))
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      gt <- "Male Population"
      if (input$in_sub_population == 2)
        gt <- "Female Population"
      
      fig_title <- input$in_outcome
      if (fig_title != toupper(fig_title))
        fig_title <- stringi::stri_trans_totitle(fig_title)
      
      fig_title <- paste0(gt, " - ", fig_title, "\n Number of samples: ",  
                          length(unique(acmfdata$id)) , 
                          " & Number of people: " , formatC(round(sum(acmfdata$totalpersons, na.rm = T)), 
                                                            format = "f", big.mark = ",", drop0trailing = TRUE))
      getPlot(plot_data, fig_title)
      
    }else{
      
      gt <- "Male Population"
      if (input$in_sub_population == 2)
        gt <- "Female Population"
      
      fig_title <- input$in_outcome
      if (fig_title != toupper(fig_title))
        fig_title <- stringi::stri_trans_totitle(fig_title)
      
      fig_title <- paste0(gt, " - ", fig_title)
      
      getPlot(NULL, fig_title)
  
  }
    
      
    
  })
  
  
  getPlot <- function (dataset, plotTitle, xlab = "Marginal MET hours per week" ){
    
    
    # outfile <- tempfile(fileext='.png')
    # 
    # # Generate the PNG
    # png(outfile, width=400, height=300)
    
    if (!is.null(dataset)){
    
      q <- quantile(dataset$dose, c(input$in_main_quantile[1], (input$in_main_quantile[1] + input$in_main_quantile[2]) / 2, input$in_main_quantile[2]))
      
      gg <- ggplot(dataset, aes(dose, RR)) + 
        geom_line(data = dataset) + 
        geom_ribbon(data = dataset, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
        #coord_cartesian(ylim = c(0, 1), xlim = c(0, xMax)) +
        scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
        # coord_cartesian(xlim = c(0, 70)) +
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
      gg <- ggplot()
    }
    
    p <- ggplotly(gg)
    # dev.off()
    p
  }
  
  set_pif_values <- reactive({
    
    acmfdata <- get_overall_data()

    if (nrow(acmfdata) > 0){
      
      local_cov_method <- F
      
      if (isolate(input$in_outcome) == "Coronary Heart Disease")
        local_cov_method <- T

      plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", returnval = T, covMethed = local_cov_method, minQuantile = input$in_main_quantile[1], maxQuantile = input$in_main_quantile[2], lout = 1000))
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
    
    overall_data <- get_overall_data()
    # Select only a subset of columns
    #     [1] "id"             "ref_number"     "study"          "authors"       
    #     [5] "outcome"        "effect_measure" "type"           "follow_up"     
    #     [9] "sex_subgroups"  "overall"        "totalpersons"   "personyears"   
    #     [13] "dose"           "rr"             "cases"          "lci"           
    #     [17] "uci"            "logrr"          "se"
    
    overall_data <- subset(overall_data, select = c(id, ref_number, Author, effect_measure, totalpersons, personyrs, dose, rr, cases, lci_effect, uci_effect))
    
    # Convert id into factor and then back to numeric for an ordered id
    overall_data$id <- as.numeric(as.factor(overall_data$id))
    
    if(nrow(overall_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$overall_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    # Empty the warning message - as some lines have been selected by the user
    output$overall_warning_message <- renderUI("")
    DT::datatable(overall_data, options = list(pageLength = 20)) #%>%
    #formatRound(columns = names(numeric_line_col_names), digits=2)
  })
  
  output$subpopulation_datatable <- DT::renderDataTable({
    
    sub_population_data <- get_subpopulation_data()
    # Subset by columns
    sub_population_data <- subset(sub_population_data, select = c(id, ref_number, Author, effect_measure, totalpersons, personyrs, dose, rr, cases, lci_effect, uci_effect))
    
    # Convert id into factor and then back to numeric for an ordered id
    sub_population_data$id <- as.numeric(as.factor(sub_population_data$id))
    
    if(nrow(sub_population_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$sub_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    # Empty the warning message - as some lines have been selected by the user
    output$sub_warning_message <- renderUI("")
    DT::datatable(sub_population_data, options = list(pageLength = 20)) #%>%
    #formatRound(columns = names(numeric_line_col_names), digits=2)
  })
})
