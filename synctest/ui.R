library(shiny)
library(readr)
library(lubridate)
library(R6)

fluidPage(tabsetPanel(type = 'tabs',
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
