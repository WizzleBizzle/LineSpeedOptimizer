#require these packages
library(shiny)
library(plotly)


#create UI
function(req) {
  #using fluid page type
  fluidPage(titlePanel(" Schedule Analyzer"),
            #sidebar contains sliders, warning, file input, analyze button
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
                ),fileInput(
                  "oldSchedule",
                  "import yesterday's schedule for cross referencing",
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
                  value = F,
                  width = NULL
                ),
                checkboxInput(
                  'tmrw',
                  'Are you planning for tomorrow?',
                  value = T,
                  width = NULL
                ),
                actionButton("analyze", "Analyze"),
                width = 2
              ),
              #main panel has tabs for dashboard, schedule, parts not done today, and graph
              mainPanel(
                tabsetPanel(
                  type = 'tabs',
                  tabPanel("Dashboard", tableOutput('dashboard')),
                  tabPanel(
                    "Scheduled Parts",
                    downloadButton("downloadTable", "Download"),
                    DT::dataTableOutput('exp_name_table')
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