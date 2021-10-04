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
    column(4, tags$a(img(src="mrc-cam.png", style = "height:50px"), href="http://www.mrc-epid.cam.ac.uk", target="_blank", align="left")),
    column(2, offset = 6, div(tags$a(img(src="cedar.png", style = "height:50px"), href="http://www.cedar.iph.cam.ac.uk/", target="_blank")), align="right")
  )
  , "Meta-Analyses Physical Activity"),
  width="100%", height="100%",
  sidebarPanel(
    div(
    radioButtons(inputId = "in_outcome_cat", label = "Outcome category:", choices =  broad_outcomes, inline = TRUE),
    selectInput(inputId = "in_outcome", label = "Outcome:", choices =  uoutcome$outcome,
                selected = uoutcome$outcome[[which(uoutcome$outcome == "All-cause mortality")]]),
    fluidRow(
      column(4, radioButtons(inputId = "in_outcome_type", label = "Outcome type:", choices =  outcome_type, selected = outcome_type[2])),
      column(4, radioButtons("total_sub_population", "Population: ", total_sub_population)),
      column(4, with_tippy(radioButtons(inputId = "in_main_quantile", label = "Last knot (person years quantiles)",
                 c("0.75",
                   "0.85",
                   "0.95")),
             "Knots are where we allow shape changes. With person years (%), it is at three locations (0, last_knot / 2 and last_knot)", 
             placement = "top"))
      ),
    conditionalPanel(
      condition = "input.total_sub_population != 1",
      HTML("<hr>"),
      radioButtons("plot_options", "Plot options: ", plot_options, inline = TRUE)
    ),
    HTML("<hr>"),
    DT::dataTableOutput("PIF"),
    HTML("<hr>"),
    DT::dataTableOutput("dose_range")
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