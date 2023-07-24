library(shiny)
library(readr)
library(lubridate)
library(R6)

# Create a process-level (i.e. in global scope) reactive-value manager.
#this will allow it to sync between instances
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

CurrentPos <- R6::R6Class(
  classname = 'CurrentPos',
  public = list(
    initialize = function(){
      private$count <- reactiveValues(
        data=1
      )
    },
    set_value = function(value){
      private$count$data
    },
    get_value = function(){
      return(private$count$data)
    }
  ),
  private = list(
    count = NULL
  )
)





# Instantiate the manager outside the server function..
value_manager <- ValueManager$new()
current_count <- CurrentPos$new()

#begin server function
function(input, output, session) {
  
  #read csv input
  observeEvent(input$file, {
    csv <- read_csv(input$file$datapath, trim_ws = FALSE)
    
    #assign csv to R6 object
    value_manager$set_value(csv)
  })
  
  #have clock continually tick
  observe({
    output$currentTime <- renderText({
      invalidateLater(1000, session)
      format(with_tz(Sys.time(),'America/New_York'), "%H:%M:%S")
    })
  })
  
  #this will update parts as they finish over time
  observe({
    
    if(nrow(value_manager$get_value())>0){
    
      #assigns current index to be 'reactive'
      cur_index <- reactiveVal(1)
    
      #retrieves csv from value manager object
      
      csv <- value_manager$get_value()
      
            #fixes time zone and finds part that should be hung
      cur_part <- csv[force_tz(csv$start_time,'America/New_York')<=with_tz(Sys.time(),'America/New_York')
                    & force_tz(csv$end_time,'America/New_York')>=with_tz(Sys.time(),'America/New_York'),]
      
      #sets current index to be the current parts' index
      cur_index(cur_part$...1)
    
    
    
    output$CurrentPart <- renderText({
      #repeats every second
      invalidateLater(1000, session)
      
      #fixes time zone and finds part that should be hung
      cur_part <- csv[force_tz(csv$start_time,'America/New_York')<=with_tz(Sys.time(),'America/New_York')
                      & force_tz(csv$end_time,'America/New_York')>=with_tz(Sys.time(),'America/New_York'),]
      
      #updates index
      cur_index(cur_part$...1)
      
      #prints current part
      print(paste0('Now Hanging: ', cur_part$model," ", cur_part$Type))
    })
    
    #html outputs upcoming parts
    #may be slightly glitchy due to it's dependency on cur index, but works well enough
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
    
    #also prints Due Date for current part, updates every second
    output$DDate <- renderText({
      invalidateLater(1000, session)
      print(paste0('Due Date: ', format(as.Date(csv$`Due_Date`[cur_index()], format ='%m/%d/%Y'), '%B %d') ) )
    })
    
    
  }})
}
