source("setup.R")

shinyUI(fluidPage(
  list(tags$title(HTML('Meta-Analysis'))),
  useShinyjs(),
  width="100%", height="100%",
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels == 1",
                     selectInput(inputId = "inOutcome", label = "Select Outcome:", choices =  uoutcome$outcome)
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Mode Share", value = 1,
               showOutput("plotAnalysis", "highcharts")
      ),
      
      id = "conditionedPanels"
    )
  )
))