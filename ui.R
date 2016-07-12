source("setup.R")

pa_exposure <- c("LTPA", "PTA")
sub_population <- c("Overall Population",
                    "Male Population",
                    "Female Population")

shinyUI(fluidPage(
  list(tags$title(HTML('Meta-Analysis'))),
  useShinyjs(),
  width="100%", height="100%",
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels == 1",
                     selectInput(inputId = "in_outcome", label = "Select Outcome:", choices =  uoutcome$outcome),
                     selectInput(inputId = "in_PA_exposure", label = "Select Physical Activity Exposure:", choices =  pa_exposure),
                     radioButtons("in_sub_population", "Population: ", sub_population, inline = TRUE)
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Analysis", value = 1,
               showOutput("plotAnalysis", "highcharts")
      ),
      
      id = "conditionedPanels"
    )
  )
))