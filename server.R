shinyServer(function(input, output, session){
  
  get_overall_data <- reactive({
    input$in_outcome 
    input$in_PA_exposure
    input$in_sub_population
    
    acmdata <- getDiseaseSpecificData(data, input$in_outcome, input$in_PA_exposure, overall1 = 1)
    # acmdata <- acmdata[!duplicated(acmdata$ref_number),]
    acmfdata <- formatData(acmdata, kcases = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
    acmfdata

  })
  
  get_subpopulation_data <- reactive({
    input$in_outcome 
    input$in_PA_exposure
    input$in_sub_population
    
    acmdata <- getDiseaseSpecificData(data, input$in_outcome, input$in_PA_exposure, gender =  input$in_sub_population)
    acmfdata <- formatData(acmdata, kcases = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
    acmfdata
    
  })
  
  output$plot_overall_analysis <- renderPlotly({
    
    acmfdata <- get_overall_data()
    if (nrow(acmfdata) > 0){
      plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = paste( input$in_PA_exposure, " LTPA - Female Population"), covMethed = T, returnval = T))
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      outfile <- tempfile(fileext='.png')
      
      # Generate the PNG
      png(outfile, width=400, height=300)
      
      gg <- 
        ggplot(plot_data, aes(dose,  RR)) + 
        geom_line(data = plot_data) + 
        geom_ribbon(data = plot_data, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
        #coord_cartesian(ylim = c(min(plot_data$lb) + 0.2, max(plot_data$ub) + 0.2), xlim = c(0, max(plot_data$dose) + 10)) +
        scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
        xlab("\n Dose \n") +
        ylab("\nRelative Risk\n") + 
        
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 7), 
          legend.direction = "horizontal",
          legend.position = c(0.1, 1.05)) + 
        ggtitle("Overall Population") +
        labs(fill = "") 
      p <- ggplotly(gg)
      dev.off()
      p
    }
  })
  
  output$plot_subpopulation_analysis <- renderPlotly({
    
    
    acmfdata <- get_subpopulation_data()
    
    if (nrow(acmfdata) > 0){
      plot_data <- data.frame(metaAnalysis(acmfdata, ptitle = paste( input$in_PA_exposure, " LTPA - Female Population"), covMethed = T, returnval = T))
      colnames(plot_data) <- c("dose","RR", "lb", "ub")
      
      outfile <- tempfile(fileext='.png')
      
      # Generate the PNG
      png(outfile, width=400, height=300)
      
      gt <- "Male Population"
      if (input$in_sub_population == 2)
        gt <- "Female Population"
      
      gg <- 
        ggplot(plot_data, aes(dose,  RR)) + 
        geom_line(data = plot_data) + 
        geom_ribbon(data = plot_data, aes(ymin=`lb`,ymax=`ub`),alpha=0.4) +
        #coord_cartesian(ylim = c(min(plot_data$lb) + 0.2, max(plot_data$ub) + 0.2), xlim = c(0, max(plot_data$dose) + 10)) +
        scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + 
        xlab("\n Dose \n") +
        ylab("\nRelative Risk\n") + 
        
        theme(
          plot.margin = unit(c(2, 1, 1, 1), "cm"), 
          plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 7), 
          legend.direction = "horizontal",
          legend.position = c(0.1, 1.05)) + 
        ggtitle(gt) +
        labs(fill = "") 
      p <- ggplotly(gg)
      dev.off()
      p
    }
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
    
    # cat("draw nrow: ", nrow(overall_data), "\n")
    if(nrow(overall_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$overall_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    # Empty the warning message - as some lines have been selected by the user
    output$overall_warning_message <- renderUI("")
    DT::datatable(overall_data, options = list(pageLength = 10)) #%>%
      #formatRound(columns = names(numeric_line_col_names), digits=2)
  })
  
  output$subpopulation_datatable <- DT::renderDataTable({
    
    sub_population_data <- get_subpopulation_data()
    # Subset by columns
    sub_population_data <- subset(sub_population_data, select = c(ref_number, authors, effect_measure, totalpersons, personyears, dose, rr, cases, lci, uci))
    
    # cat("draw nrow: ", nrow(overall_data), "\n")
    if(nrow(sub_population_data) <= 0){
      # Set the warning message that no lines have been selected by the user
      output$sub_warning_message <- renderUI(HTML("<strong>No data available </strong>"))
      # Return an empty data.frame
      return(data.frame(File=character()))
    }
    # Empty the warning message - as some lines have been selected by the user
    output$sub_warning_message <- renderUI("")
    DT::datatable(sub_population_data, options = list(pageLength = 10)) #%>%
    #formatRound(columns = names(numeric_line_col_names), digits=2)
  })
  
  
  
})
