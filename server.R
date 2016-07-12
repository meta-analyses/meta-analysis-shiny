shinyServer(function(input, output, session){
  
  output$plot_overall_analysis <- renderPlotly({
    
    acmdata <- getDiseaseSpecificData(data, input$in_outcome, input$in_PA_exposure, overall1 = 1)
    # acmdata <- acmdata[!duplicated(acmdata$ref_number),]
    acmfdata <- formatData(acmdata, kcases = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
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
    
    acmdata <- getDiseaseSpecificData(data, input$in_outcome, input$in_PA_exposure, gender =  input$in_sub_population)
    # acmdata <- acmdata[!duplicated(acmdata$ref_number),]
    acmfdata <- formatData(acmdata, kcases = T)
    # Remove all cases where both rr and dose are null
    acmfdata <- subset(acmfdata, !is.na(rr) & !is.na(dose))
    # Remove when totalperson is not available for hr, and personsyears for rr/or
    acmfdata <- subset(acmfdata, !((effect_measure == "hr" & (is.na(personyears) | personyears == 0) ) | 
                                     (effect_measure != "hr" & (is.na(totalpersons | totalpersons == 0) ) ) ))
    
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
  
  
  
  
})
