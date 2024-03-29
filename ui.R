# Load required packages/libraries
source("setup.R")

# outcome_type
outcome_type <- c("Fatal and non-fatal",
                  "Fatal",
                  "Non-fatal")

# "Overall Population"
sub_population <- c("Male Population" = 1,
                    "Female Population" = 2)


total_sub_population <- c("Total population" = 1,
                          "Sex-stratified" = 2)

plot_options <- c("Meta-Analysis" = 1,
                  "Dose range" = 2)

broad_outcomes <- c("All-cause mortality" = 1, 
                    "Cardiovascular diseases" = 2, 
                    "Cancers" = 3,
                    "Depression" = 4,
                    "Neurological disorders" = 5)

shinyUI(fluidPage(
  titlePanel(fluidRow(
    column(4, tags$a(img(src="MRCEpid_core_logo2021_RGB.png", style = "height:50px"), href="https://www.mrc-epid.cam.ac.uk", target="_blank", align="left")),
    column(4, offset = 4, tags$a(img(src="LOGO_ERC-FLAG_EU_cropped.jpg", style = "height:50px"), href="https://www.mrc-epid.cam.ac.uk/research/studies/glasst/", target="_blank"), align="right")
  )
  , "Meta-Analyses Physical Activity"),
  width="100%", height="100%",
  sidebarPanel(
    radioButtons(inputId = "in_outcome_cat", label = "Outcome category:", choices =  broad_outcomes, inline = TRUE),
    selectInput(inputId = "in_outcome", label = "Outcome:", choices =  uoutcome$outcome,
                selected = uoutcome$outcome[[which(uoutcome$outcome == "All-cause mortality")]]),
    fluidRow(
      column(4, radioButtons(inputId = "in_outcome_type", label = "Outcome type:", choices =  outcome_type, selected = outcome_type[2])),
      column(4, radioButtons("total_sub_population", "Population: ", total_sub_population)),
      column(4, with_tippy(radioButtons(inputId = "in_main_quantile", label = "Knots (person years quantiles)",
                 c("0-37.5-75th" = "0.75",
                   "0-42.5-85th" = "0.85",
                   "0-47.5-95th" = "0.95")),
             "Knots are where we allow shape changes. Using person years (%), it is at three locations (0th, 37.5th and 75th, as an example) ", 
             placement = "top"))
      ),
    
    fluidRow(column(4, checkboxInput(inputId = "y_axis_log10", label = "logarithmic y-axis (log10)", value = TRUE))),
    conditionalPanel(
      condition = "input.total_sub_population != 1",
      radioButtons("plot_options", "Plot options: ", plot_options, inline = TRUE)
    ),
    shinyBS::bsCollapse(shinyBS::bsCollapsePanel("Potential Impact Fraction (PIF)", 
                                                 DT::dataTableOutput("PIF"),
                                                 DT::dataTableOutput("dose_range")
    ),
    shinyBS::bsCollapsePanel("Dose distribution", "This shows distribution of Marginal MET hours per week",
                             uiOutput("generic_warning_message"), 
                             DT::dataTableOutput("dose_distr"),
                             plotlyOutput("dose_distr_plot"))
    )
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Analysis",
               plotlyOutput("top_plot"),
               downloadButton("download_top_data", "Download data", icon = shiny::icon("file-download")),
               plotlyOutput("bottom_plot"),
               downloadButton("download_bottom_data", "Download data", icon = shiny::icon("file-download"))
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
      id = "main_panel"
    )
  )
))