# Load required packages/libraries
source("setup.R")

# domain
#pa_exposure <- c("LTPA", "TPA")

# outcome_type
outcome_type <- c("Fatal and non-fatal",
                  "Fatal",
                  "Non-fatal")

# "Overall Population"
sub_population <- c("Male Population" = 1,
                    "Female Population" = 2)


total_sub_population <- c("Total Population" = 1,
                          "Sub-population" = 2)

plot_options <- c("Meta-Analysis" = 1,
                  "Dose range" = 2)
shinyUI(fluidPage(
  titlePanel(fluidRow(
    column(4, tags$a(img(src="mrc-cam.png", style = "height:50px"), href="http://www.mrc-epid.cam.ac.uk", target="_blank", align="left")),
    #column(2, tags$a(img(src="cam.png", style = "height:50px"), href="http://www.cam.ac.uk", target="_blank"), align = 'left'),
    column(2, offset = 6, div(tags$a(img(src="cedar.png", style = "height:50px"), href="http://www.cedar.iph.cam.ac.uk/", target="_blank")), align="right")
    
    )
    , "Meta-Analyses Physical Activity"),
  width="100%", height="100%",
  sidebarPanel(
    
    # ChoiceGroup.shinyInput("choice", value = "B", options = options),
    # textOutput("groupValue"),
    
    selectInput(inputId = "in_outcome", label = "Select Outcome:", choices =  uoutcome$outcome),
    # radioButtons(inputId = "in_PA_exposure", label = "Select Physical Activity Exposure:", choices =  pa_exposure),
    radioButtons(inputId = "in_outcome_type", label = "Select Outcome type:", choices =  outcome_type),
    HTML("<hr>"),
    radioButtons("total_sub_population", "Population: ", total_sub_population, inline = TRUE),
    conditionalPanel(
      condition = "input.total_sub_population != 1",
      HTML("<hr>"),
      radioButtons("plot_options", "Plot options: ", plot_options, inline = TRUE)
    ),
    HTML("<hr>"),
    conditionalPanel(
      condition = "input.total_sub_population == 1",
      #sliderInput(inputId = "in_main_quantile", label = "Main outcome quantiles", min = 0.75, max = 0.95, value = 0.75, step = 1, ticks = F),
      radioButtons(inputId = "in_main_quantile", label = "Last knot (quantiles)",
                   c("0.75",
                     "0.85",
                     "0.95")),
      HTML("<hr>")
    ),
    conditionalPanel(
      condition = "input.total_sub_population != 1",
      radioButtons(inputId = "in_sub_quantile", label = "Sub-population quantiles",
                   c("0.75",
                     "0.85",
                     "0.95")),
      HTML("<hr>")
    ),
    uiOutput("lowest_guideline"),
    HTML("<hr>"),
    uiOutput("lower_guideline"),
    HTML("<hr>"),
    uiOutput("upper_guideline"),
    HTML("<hr>"),
    DT::dataTableOutput("dose_range")
    
  ),
  
  mainPanel(

    tabsetPanel(
      tabPanel("Analysis",
               plotlyOutput("top_plot"),
               downloadLink("download_top_data", "Download Data"),
               plotlyOutput("bottom_plot"),
               downloadLink("download_bottom_data", "Download Data")
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