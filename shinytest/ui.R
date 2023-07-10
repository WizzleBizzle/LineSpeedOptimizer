library(shiny)
library(plotly)
function(req) {
  fluidPage(titlePanel(" Schedule Analyzer"),
            sidebarLayout(
              sidebarPanel(
                "Please remember this tool is not perfect. Go through each item and make adjustments if something seems off",
                fileInput(
                  "file",
                  "Choose Master DOM to schedule",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                  )
                ),
                fileInput(
                  "oldfile",
                  "Input yesterday's unpainted parts csv",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                  )
                ),
                tags$hr(),
                sliderInput("slider", "shift length", 4, 10, 8, 1),
                sliderInput("shiftAmt", "# of shifts", 1, 2, 2, 1, ticks = F),
                sliderInput("pessimism", "Adjust paint time multiplier", 0.5, 2, 1, 0.1, ticks = T),
                checkboxInput(
                  'curTime',
                  'Use Current Time of Day?',
                  value = T,
                  width = NULL
                ),
                actionButton("analyze", "Analyze"),
                width = 2
              ),
              mainPanel(
                tabsetPanel(
                  type = 'tabs',
                  tabPanel("Dashboard", tableOutput('dashboard')),
                  tabPanel(
                    "Scheduled Parts",
                    DT::dataTableOutput('exp_name_table'),
                    downloadButton("downloadData", "Download")
                  ),
                  tabPanel(
                    "Parts to do later",
                    DT::dataTableOutput("TODO"),
                    downloadButton("downloadTODO", "Download")
                  ),
                  tabPanel("Graph", plotlyOutput("plot"))
                )
              )
            ))
}