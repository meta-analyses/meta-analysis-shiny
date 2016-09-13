shinyServer(function(input, output, session){
  
  get_overall_data <- reactive({
    input$in_outcome 
    input$in_outcome_type
    input$in_PA_exposure
    input$in_sub_population
    
    acmdata <- getDiseaseSpecificData(data, 
                                      outcome1 = input$in_outcome, 
                                      paexposure = input$in_PA_exposure, 
                                      out_type = input$in_outcome_type,
                                      overall1 = 1)
    acmfdata <- data.frame()
    # if (nrow(acmdata) > 0){
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    acmfdata
    # }else
    # data.frame()
  })
  
  get_subpopulation_data <- reactive({
    input$in_outcome 
    input$in_outcome_type
    input$in_PA_exposure
    input$in_sub_population
    
    acmdata <- getDiseaseSpecificData(data, input$in_outcome, input$in_PA_exposure, gender =  input$in_sub_population, out_type = input$in_outcome_type)
    
    acmfdata <- data.frame()
    acmfdata <- formatData(acmdata, kcases = T, infertotalpersons = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    acmfdata
    
  })
  
  observe({
    input$in_outcome 
    input$in_outcome_type
    input$in_PA_exposure
    input$in_sub_population
    # outcome_type
    outcome_type <- c("all", 
                      "mortality",
                      "incidence")
    index <- c()    
    for (i in 1:length(outcome_type)){
      acmdata <- getDiseaseSpecificData(data, input$in_outcome, input$in_PA_exposure, overall1 = 1, out_type = outcome_type[i])
      if (nrow(acmdata) > 0){
        index <- append(index, i)
      }
    }
    
  })
  
  output$plot_overall_analysis <- renderChart({
    
    acmfdata <- get_overall_data()
    
    if (nrow(acmfdata) > 0){
      plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = input$in_main_quantile[1], maxQuantile = input$in_main_quantile[2]))
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      h1 <- Highcharts$new()
      h1$series(
        data = toJSONArray2(plot_data[,c('dose', 'RR')], names = F, json = F),
        zIndex = 1,
        name = "Relative Risk",
        type = "line"
      )
      h1$series(
        data = toJSONArray2(plot_data[,c('dose', 'lb', 'ub')], names = F, json = F),
        type = 'arearange',
        fillOpacity = 0.3,
        lineWidth = 0,
        color = 'grey',
        zIndex = 0,
        name = "Confidence Interval"
      )
      
      fig_title <- input$in_outcome
      if (fig_title != toupper(fig_title))
        fig_title <- stringi::stri_trans_totitle(fig_title)
      
      fig_title <- paste0("Overall Population - ", fig_title)
      
      h1$title(text= fig_title)
      
      h1$yAxis(title = list(text = 'Relative Risk'), min = 0, max = max(plot_data[,'ub']) + 0.1)
      h1$xAxis(title = list(text = 'Marginal MET Hours'), min = 0)
      
      h1$tooltip(formatter = "#! function() {return 'RR: <b>' + Math.round(this.y * 100.0) / 100.0 + '<br/>' + 'Dose : <b>' + Math.round(this.x * 100.0) / 100.0; } !#") #  
      h1$set(dom = "plot_overall_analysis")
      h1$exporting(enabled = T)
      return (h1)
    }else
      return(NULL)
    
  })
  
  output$plot_subpopulation_analysis <- renderChart({
    
    h <- Highcharts$new()
    acmfdata <- get_subpopulation_data()
    
    if (nrow(acmfdata) > 0){
      plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = "", covMethed = T, returnval = T, minQuantile = input$in_sub_quantile[1], maxQuantile = input$in_sub_quantile[2]))
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      h1 <- Highcharts$new()
      h1$series(
        data = toJSONArray2(plot_data[,c('dose', 'RR')], names = F, json = F),
        zIndex = 1,
        name = "Relative Risk",
        type = "line"
      )
      h1$series(
        data = toJSONArray2(plot_data[,c('dose', 'lb', 'ub')], names = F, json = F),
        type = 'arearange',
        fillOpacity = 0.3,
        lineWidth = 0,
        color = 'grey',
        zIndex = 0,
        name = "Confidence Interval"
      )
      
      gt <- "Male Population"
      if (input$in_sub_population == 2)
        gt <- "Female Population"
      
      fig_title <- input$in_outcome
      if (fig_title != toupper(fig_title))
        fig_title <- stringi::stri_trans_totitle(fig_title)
      
      fig_title <- paste0(gt, " - ", fig_title)
      
      h1$title(text= fig_title)
      
      h1$yAxis(title = list(text = 'Relative Risk'), min = 0, max = max(plot_data[,'ub']) + 0.1)
      h1$xAxis(title = list(text = 'Marginal MET Hours'), min = 0)
      
      h1$tooltip(formatter = "#! function() {return 'RR: <b>' + Math.round(this.y * 100.0) / 100.0 + '<br/>' + 'Dose : <b>' + Math.round(this.x * 100.0) / 100.0; } !#") #  
      h1$set(dom = "plot_subpopulation_analysis")
      h1$exporting(enabled = T)
      return (h1)
    }else
      return(NULL)
  })
  
  output$overall_datatable <- DT::renderDataTable({
    
    overall_data <- get_overall_data()
    # Select only a subset of columns
    #     [1] "id"             "ref_number"     "study"          "authors"       
    #     [5] "outcome"        "effect_measure" "type"           "follow_up"     
    #     [9] "sex_subgroups"  "overall"        "totalpersons"   "personyears"   
    #     [13] "dose"           "rr"             "cases"          "lci"           
    #     [17] "uci"            "logrr"          "se"
    
    overall_data <- subset(overall_data, select = c(ref_number, authors, effect_measure, totalpersons, personyears, dose, rr, cases, lci, uci))
    
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
    sub_population_data <- subset(sub_population_data, select = c(ref_number, authors, effect_measure, totalpersons, personyears, dose, rr, cases, lci, uci))
    
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
