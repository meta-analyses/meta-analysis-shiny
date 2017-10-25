# Load required packages/libraries
source("setup.R")

# domain
#pa_exposure <- c("LTPA", "TPA")

# outcome_type
outcome_type <- c("all",
                  "incidence",
                  "mortality")

# "Overall Population"
sub_population <- c("Male Population" = 1,
                    "Female Population" = 2)


total_sub_population <- c("Total Population" = 1,
                          "Sub-population" = 2)

plot_options <- c("Meta-Analysis" = 1,
                  "Dose range" = 2)


shinyUI(fluidPage(
  list(tags$title(HTML('Meta-Analysis'))),
  width="100%", height="100%",
  sidebarPanel(
    selectInput(inputId = "in_outcome", label = "Select Outcome:", choices =  uoutcome$outcome),
    # radioButtons(inputId = "in_PA_exposure", label = "Select Physical Activity Exposure:", choices =  pa_exposure),
    radioButtons(inputId = "in_outcome_type", label = "Select Outcome type:", choices =  outcome_type),
    HTML("<hr>"),
    radioButtons("total_sub_population", "Population: ", total_sub_population, inline = TRUE),
    HTML("<hr>"),
    conditionalPanel(
      condition = "input.total_sub_population != 1",
      radioButtons("plot_options", "Plot options: ", plot_options, inline = TRUE),
      HTML("<hr>")
    ),
    sliderInput(inputId = "in_main_quantile", label = "Main outcome quantiles", min = 0, max = 1, value = c(0, 0.75), step = 0.05),
    HTML("<hr>"),
    sliderInput(inputId = "in_sub_quantile", label = "Sub-population quantiles", min = 0, max = 1, value = c(0, 0.75), step = 0.05),
    HTML("<hr>"),
    uiOutput("lower_guideline"),
    HTML("<hr>"),
    uiOutput("upper_guideline"),
    conditionalPanel(
      condition = "input.total_sub_population == 1",
      HTML("<hr>"),
      DT::dataTableOutput("dose_range")
    )
    
  ),
  
  mainPanel(

    tabsetPanel(
      tabPanel("Analysis",
               plotlyOutput("top_plot"),
               plotlyOutput("bottom_plot")
               # conditionalPanel(
               #   condition = "input.total_sub_population != 1",
               #   plotlyOutput("bottom_plot")
               # )
      ),
      tabPanel("Total Population Data",
               uiOutput("overall_warning_message"),
               DT::dataTableOutput("overall_datatable")
      ),
      tabPanel("Male Population Data",
               uiOutput("male_sub_warning_message"),
               DT::dataTableOutput("male_population_datatable")
      ),
      tabPanel("Female Population Data",
               uiOutput("female_sub_warning_message"),
               DT::dataTableOutput("female_population_datatable")
      ),

      id = "conditionedPanels"
    )
  )
))