library(shiny)
library(readr)
library(lubridate)
library(R6)

# Create a process-level (i.e. in global scope) reactive-value manager.
ValueManager <- R6::R6Class(
  classname = "ValueManager",
  public = list(
    initialize = function() {
      private$reactive_inner_counter <- reactiveValues(
        # data = data.frame(`...1` = 1,model = '80FF', Type = 'QD', line_Speed=28, Due_Date = Sys.Date(),
        #                    qty = 1, start_time = Sys.Date(),
        #                    end_time = Sys.Date(), duration_Seconds = 1, isPerf = T, Finish = 'B12')
        data = data.frame()
      )
    },
    set_value = function(value) {
      private$reactive_inner_counter$data = value
    },
    get_value = function() {
      return(private$reactive_inner_counter$data)
    },
    observe = function(){
      #return(private$reactive_inner_counter)
    }
  ),
  private = list(
    reactive_inner_counter = NULL
  )
)

# Instantiate the manager outside the server function..
value_manager <- ValueManager$new()

ui <- fluidPage(tabsetPanel(type = 'tabs',
                            tabPanel('Display',fluidRow(wellPanel(h1(textOutput("currentTime"), style = 'font-size:50px;'),align = 'center')),
                                h1(strong(textOutput("CurrentPart")), style = 'font-size:60px;', align = 'center'),
                                h1(textOutput("DDate"), style = 'font-size:60px;', align = 'center'),
                                h1(htmlOutput("NextPart"), style = 'font-size:30', align = 'center')
                            
                            ),tabPanel('Input', fileInput(
                              "file",
                              "Choose Schedule to display",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                              )
                            ) #, actionButton("analyze", "Analyze")
                            )
                            
                            
))

server <- function(input, output, session) {
  observeEvent(input$file, {
    csv <- read_csv(input$file$datapath, trim_ws = FALSE)
    
    value_manager$set_value(csv)
  })
  observe({
    output$currentTime <- renderText({
      invalidateLater(1000, session)
      format(with_tz(Sys.time(),'America/New_York'), "%H:%M:%S")
    })
  })
  
  
  observe({
    if(nrow(value_manager$get_value())>0){
    cur_index <- reactiveVal(1)
    
    csv <- value_manager$get_value()
    cur_part <- csv[force_tz(csv$start_time,'America/New_York')<=with_tz(Sys.time(),'America/New_York')
                    & force_tz(csv$end_time,'America/New_York')>=with_tz(Sys.time(),'America/New_York'),]
    cur_index(cur_part$...1)
    
    
    
    output$CurrentPart <- renderText({
      invalidateLater(1000, session)
      cur_part <- csv[force_tz(csv$start_time,'America/New_York')<=with_tz(Sys.time(),'America/New_York')
                      & force_tz(csv$end_time,'America/New_York')>=with_tz(Sys.time(),'America/New_York'),]
      cur_index(cur_part$...1)
      print(paste0('Now Hanging: ', cur_part$model," ", cur_part$Type))
    })
    
    output$NextPart <- renderUI({
      invalidateLater(1000, session)
      text3 = ''
      if(cur_index()<nrow(csv)){
        for(i in (cur_index()+1):min(cur_index()+5,nrow(csv))){
          text1 <- paste('Coming Up: ', csv$model[i]," ", csv$Type[i])
          text2 <- paste('Due: ', format(as.Date(csv$`Due_Date`[i], format ='%m/%d/%Y'), '%B %d'))
          text3 <- paste(text3, text1, text2, " ", sep = '<br/>')
        }
      }
      HTML(text3)
    })
    output$DDate <- renderText({
      invalidateLater(1000, session)
      print(paste0('Due Date: ', format(as.Date(csv$`Due_Date`[cur_index()], format ='%m/%d/%Y'), '%B %d') ) )
    })
    
    
  }})
}

shinyApp(ui, server)
