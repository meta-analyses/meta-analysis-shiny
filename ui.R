source("setup.R")

# domain
pa_exposure <- c("LTPA", "TPA")

# outcome_type
# outcome_type <- c("All" = "all", 
#                   "Mortality" = "mortality",
#                   "Incidence" = "incidence")


# outcome_type
outcome_type <- c("all",
                  "incidence",
                  "mortality")

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
    radioButtons(inputId = "in_outcome_type", label = "Select Outcome type:", choices =  outcome_type),
    HTML("<hr>"),
    radioButtons("in_sub_population", "Population: ", sub_population, inline = TRUE),
    sliderInput(inputId = "in_quantile", label = "Quantiles", min = 0, max = 1, value = c(0.25, 0.75), step = 0.05)
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Analysis",
               plotOutput('plot_overall_analysis'),
               plotOutput('plot_subpopulation_analysis')
#                plotlyOutput("plot_overall_analysis"),
#                plotlyOutput("plot_subpopulation_analysis")
      ),
      tabPanel("Outcome-specific Data",
               uiOutput("overall_warning_message"),
               DT::dataTableOutput("overall_datatable")
      ),
      tabPanel("Outcome-specific Sub-population Data",
               uiOutput("sub_warning_message"),
               DT::dataTableOutput("subpopulation_datatable")
      ),
      
      id = "conditionedPanels"
    )
  )
))