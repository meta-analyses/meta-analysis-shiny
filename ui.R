source("setup.R")

pa_exposure <- c("LTPA", "TPA")
# "Overall Population"
sub_population <- c("Male Population" = 1,
                    "Female Population" = 2)

shinyUI(fluidPage(
  list(tags$title(HTML('Meta-Analysis'))),
  useShinyjs(),
  width="100%", height="100%",
  sidebarPanel(
    selectInput(inputId = "in_outcome", label = "Select Outcome:", choices =  uoutcome$outcome),
    radioButtons(inputId = "in_PA_exposure", label = "Select Physical Activity Exposure:", choices =  pa_exposure),
    HTML("<hr>"),
    radioButtons("in_sub_population", "Population: ", sub_population, inline = TRUE)
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Analysis",
               plotlyOutput("plot_overall_analysis"),
               plotlyOutput("plot_subpopulation_analysis")
      ),
      tabPanel("Overall Data",
               uiOutput("overall_warning_message"),
               DT::dataTableOutput("overall_datatable")
      ),
      tabPanel("Sub-population Data",
               uiOutput("sub_warning_message"),
               DT::dataTableOutput("subpopulation_datatable")
      ),
      
      id = "conditionedPanels"
    )
  )
))