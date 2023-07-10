library(shiny)
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(DT)
library(yaml)
library(plotly)
function(input, output) {
  CalcTimeChange <- function(density, speed) {
    prod_time = (2.4 * (1 / density) / speed) * 60
    return(prod_time)
  }
  
  # Read the CSV file
  #troubleshooting manual testing code
  #csv <- read_csv("6_1929.csv", trim_ws = FALSE, skip = 9); csv <- csvFormat(csv)
  
  csvFormat <- function(csv) {
    csv <- csv[-grep("@@", csv$`Order!Count`),]
    csv <- csv[-grep("##", csv$`Order!Count`),]
    csv <- csv[-grep("Order!Count", csv$`Order!Count`),]
    
      
    
    if (length(grep("DSR-ATL-EXT", csv$`Group`)) > 0) {
      csv <- csv[-grep("DSR-ATL-EXT", csv$`Group`),]
    }
    if (length(grep("GRD ATL-EXT", csv$`Group`)) > 0) {
      csv <- csv[-grep("GRD ATL-EXT", csv$`Group`),]
    }
    if (length(grep("GRD ATL-SMD", csv$`Group`)) > 0) {
      csv <- csv[-grep("GRD ATL-SMD", csv$`Group`),]
    }
    if (length(grep("GRD-", csv$`Model`)) > 0) {
      csv <- csv[-grep("GRD-", csv$`Model`),]
    }
    if (length(grep("VCR", csv$`Model`)) > 0) {
      csv <- csv[-grep("VCR", csv$`Model`),]
    }
    if (length(grep("SB-", csv$`Model`)) > 0) {
      csv <- csv[-grep("SB-", csv$`Model`),]
    }
    if (length(grep("MILL", csv$Finish)) > 0) {
      csv <- csv[-grep("MILL", csv$Finish),]
    }

    if (length(grep("Final Assembly", csv$`Station`)) > 0) {
      csv <- csv[-grep("Final Assembly", csv$`Station`),]
    }
    if (length(grep("Paint Line", csv$`Station`)) > 0) {
      csv <- csv[-grep("Paint Line", csv$`Station`),]
    }
    if (length(grep("Hold", csv$`Status`)) > 0) {
      csv <- csv[-grep("Hold", csv$`Status`),]
    }
    
    if (length(grep("TUP", csv$`Model`)) > 0) {
      csv <- csv[-grep("TUP", csv$`Model`),]
    }

    csv <- cbind(csv, FC = grepl("FC", csv$Description))
    csv <- cbind(csv, Speed = 1)
    csv <- cbind(csv, Shift = 0)
    csv <- cbind(csv, Density = 1)
    csv[grep("PERF", csv$`Group`), 'Speed'] <- 28
    csv[grep("PIB", csv$`Model`), 'Speed'] <- 28
    
    csv[grep('MS', csv$Model), 'Speed'] <- 14
    csv[grep('MS', csv$Model), 'Shift'] <- 14
    
    csv[grep("SEC", csv$`Group`), 'Speed'] <- 14
    csv[grep("SEC", csv$`Group`), 'Shift'] <- 2
    csv[grep("80", csv$`Group`), 'Speed'] <- 28
    csv[grep("80", csv$`Group`), 'Density'] <- 2
    csv[grep("SMD", csv$`Group`), 'Speed'] <- 22
    csv[grep("AMD", csv$`Group`), 'Speed'] <- 24
    csv[grep("8", csv$`Model`), 'Speed'] <- 28
    csv[grep("8", csv$`Model`), 'Density'] <- 2
    csv[grep("9", csv$`Model`), 'Speed'] <- 18
    csv[grep("9", csv$`Model`), 'Density'] <- 1.5
    csv[grep("510", csv$`Model`), 'Speed'] <- 18
    csv[grep("520", csv$`Model`), 'Speed'] <- 14
    csv[grep("530", csv$`Model`), 'Speed'] <- 18
    csv[grep("535", csv$`Model`), 'Speed'] <- 18
    
    csv[grep("510", csv$`Model`), 'Density'] <- 2.5
    csv[grep("520", csv$`Model`), 'Density'] <- 2.5
    csv[grep("530", csv$`Model`), 'Density'] <- 2.5
    csv[grep("535", csv$`Model`), 'Density'] <- 2.5
    
    csv[grep("530FF", csv$`Model`), 'Speed'] <- 18
    csv[grep("535FF", csv$`Model`), 'Speed'] <- 18
    
    csv[grep("530FF", csv$`Model`), 'Density'] <- 1
    csv[grep("535FF", csv$`Model`), 'Density'] <- 1
    
    csv[grep("610", csv$`Model`), 'Speed'] <- 24
    csv[grep("620", csv$`Model`), 'Speed'] <- 18
    csv[grep("630", csv$`Model`), 'Speed'] <- 24
    csv[grep("635", csv$`Model`), 'Speed'] <- 24
    
    csv[grep("610", csv$`Model`), 'Density'] <- 2.5
    csv[grep("620", csv$`Model`), 'Density'] <- 2.5
    csv[grep("630", csv$`Model`), 'Density'] <- 2.5
    csv[grep("635", csv$`Model`), 'Density'] <- 2.5
    
    csv[grep("630FF", csv$`Model`), 'Speed'] <- 25
    csv[grep("635FF", csv$`Model`), 'Speed'] <- 25
    
    csv[grep("630FF", csv$`Model`), 'Density'] <- 1
    csv[grep("635FF", csv$`Model`), 'Density'] <- 1
    
    csv[grep("ACVD", csv$`Model`), 'Speed'] <- 22
    csv[grep("ACVD", csv$`Model`), 'Density'] <- 2
    
    csv[grep("AMCD", csv$`Model`), 'Speed'] <- 28
    csv[grep("AMCD", csv$`Model`), 'Density'] <- 2
    
    csv[grep("AMD", csv$`Model`), 'Speed'] <- 24
    csv[grep("AMD", csv$`Model`), 'Density'] <- 2
    
    csv[grep("LCMD", csv$`Model`), 'Speed'] <- 22
    
    csv[grep("AMDA", csv$`Model`), 'Speed'] <- 24
    csv[grep("AMDA", csv$`Model`), 'Density'] <- 2
    
    csv[grep("APF", csv$`Model`), 'Speed'] <- 24
    csv[grep("APF", csv$`Model`), 'Density'] <- 2
    
    csv[grep("SCD-", csv$`Model`), 'Speed'] <- 24
    csv[grep("SCDA-", csv$`Model`), 'Speed'] <- 18
    
    csv[grep("FC", csv$`Description`), 'Speed'] <- 12
    csv[grep("FC", csv$`Description`), 'Density'] <- 2
    
    for (i in grep("ASCD-", csv$`Model`)) {
      if (csv$FC[i]) {
        csv[i, 'Speed'] <- 14
      }
    }
    csv[grep("ASPD", csv$`Model`), 'Speed'] <- 28
    
    csv[grep("LBP", csv$`Model`), 'Speed'] <- 28
    csv[grep("LBP", csv$`Model`), 'Density'] <- 2
    
    csv[grep("PDC", csv$`Model`), 'Speed'] <- 24
    
    csv[grep("SCVD", csv$`Model`), 'Speed'] <- 22
    csv[grep("SCVD", csv$`Model`), 'Density'] <- 2
    
    csv[grep("SDS", csv$`Model`), 'Speed'] <- 28
    csv[grep("SDS", csv$`Model`), 'Density'] <- 2
    
    csv[grep("SMD", csv$`Model`), 'Speed'] <- 22
    csv[grep("SMD", csv$`Model`), 'Density'] <- 2
    
    csv[grep("SPD", csv$`Model`), 'Speed'] <- 28
    
    csv[grep("SPF", csv$`Model`), 'Speed'] <- 28
    csv[grep("SPF", csv$`Model`), 'Density'] <- 2
    
    csv[grep("AMF", csv$`Model`), 'Speed'] <- 28
    csv[grep("AMF", csv$`Model`), 'Density'] <- 2
    
    csv[grep("PDDR", csv$`Model`), 'Speed'] <- 28
    csv[grep("PDF", csv$`Model`), 'Speed'] <- 28
    csv[grep("PDMC", csv$`Model`), 'Speed'] <- 28
    csv[grep("PDSP", csv$`Model`), 'Speed'] <- 28
    
    csv[grep("PFRF", csv$`Model`), 'Speed'] <- 28
    csv[grep("PFRF", csv$`Model`), 'Density'] <- 2
    
    csv[grep("10A", csv$`Model`), 'Speed'] <- 28
    csv[grep("10FF", csv$`Model`), 'Speed'] <- 28
    
    csv[grep("10A", csv$`Model`), 'Density'] <- 2
    csv[grep("10FF", csv$`Model`), 'Density'] <- 2
    
    csv[grep("SDR", csv$`Model`), 'Speed'] <- 28
    csv[grep("SDR", csv$`Model`), 'Density'] <- 2
    
    try(csv[-which(csv$Finish == "B12"), 'Speed'] <- 5)
    csv <- cbind(csv, ttd = 0)
    csv <- cbind(csv, prod_time = 0)
    
    for (i in 1:nrow(csv)) {
      csv[i, "ttd"] <-
        as.numeric(as.Date(csv$`Due!Date`[i], format = "%m/%d/%y")) - as.numeric(Sys.Date())
      csv[i, "prod_time"] <- 60 / (csv[i, 'Speed'] / 2.4)
    }
    csv[, "Qty"] <- as.numeric(csv[, "Qty"])
    
    #perf id
    csv <- cbind(csv, isPerf = F)
    csv[grep('SCD', csv$'Group'), 'isPerf'] <- T
    csv[grep('PERF', csv$'Group'), 'isPerf'] <- T
    csv[grep("SCD", csv$`Model`), 'isPerf'] <- T
    csv[grep("SPD", csv$`Model`), 'isPerf'] <- T
    csv[grep("PDC", csv$`Model`), 'isPerf'] <- T
    csv[grep("PDF", csv$`Model`), 'isPerf'] <- T
    csv[grep("PDDR", csv$`Model`), 'isPerf'] <- T
    csv[grep("PFRF", csv$`Model`), 'isPerf'] <- T
    csv[grep("PDSP", csv$`Model`), 'isPerf'] <- T
    csv[grep("PDMC", csv$`Model`), 'isPerf'] <- T
    
    csv$Model <- gsub('-.*','',csv$Model)
    csv[grep(TRUE, csv$FC), 'Model'] <-
      paste0(csv[grep(TRUE, csv$FC), 'Model'], "/FC")
    try(csv[-which(csv$Finish == "B12"), 'Model'] <-
          paste0(csv[-which(csv$Finish == "B12"), 'Model'], "/", csv[-which(csv$Finish ==
                                                                              "B12"), 'Finish']))
    
    # csv <- na.omit(csv[,c('ttd' , 'Due!Date','Ack.!Date' , 'Model' , 'Speed' , 'prod_time' , 'isPerf' ,
    #                       'Type' , 'Shift' ,'Finish' , 'Density', 'Qty')])
    csv <-
      aggregate(
        Qty ~ `ttd` + `Due!Date`+`Ack.!Date` + `Model` + `Speed` + `prod_time` + `isPerf` +
          `Type` + `Shift` + `Finish` + `Density`,
        data = csv,
        FUN = sum
      )
    
    csv[,'ttd'] <- csv[,'ttd'] - min(csv[,'ttd'])
    for(i in grep('Q',csv$Type)){
      csv$ttd[i]= (as.numeric(as.Date(csv$`Ack.!Date`[i], format = "%m/%d/%y"))-as.numeric(Sys.Date()))
      if(csv$ttd[i]>0){
        csv$ttd[i] <- csv$ttd[i]*1.5
      }
        
    }
    
    
    # csv<- csv[-grep("B12",csv$Finish),]
    return(csv)
  }
  
  
  TimeChangePenalty <- function(curspeed, model, Line_Speeds) {
    return(abs(curspeed - Line_Speeds[model]))
  }
  
  
  CritValueBase <-
    function(qty,
             speed,
             density,
             TimeInDay,
             Current_Time,
             ttd
             ) {
      TimeTilDueAdj = ((TimeInDay - Current_Time)) + (ttd*TimeInDay)
      prod_time = CalcTimeChange(density,speed)*qty
      return(TimeTilDueAdj / (prod_time))
      
    }
  
  # TimeInDay = 57600;
  # shiftAmt = 2;
  # Current_Time = 900;
  # default_time = as.POSIXct("2023-06-16 06:15:00 EDT");
  # pessimism = 1;
  
  main <-
    function(csv,
             TimeInDay = 57600,
             shiftAmt = 2,
             Current_Time = 900,
             default_time = as.POSIXct("2023-06-16 06:15:00 EDT"),
             pessimism = 1) {
      
      Shift = 1
      start_time = Current_Time
      Current_Part = "na"
      Current_Speed = 0
      current_Paint = "B12"
      min_ttd = min(csv$ttd)
      downtime = Current_Time %% 7200
      
      
      date(default_time) <- Sys.Date()
      
      Schedule = data.frame(
        model = c("model"),
        Type = c(0),
        line_Speed = c(0),
        Due_Date = c(0),
        qty = c(0),
        start_time = default_time,
        end_time = default_time,
        duration_Seconds = c(0),
        isPerf = F,
        Finish = 'B12'
      )
      
      while (Current_Time < TimeInDay && sum(csv$Qty) > 0) {
        Crit = 9999999999999
        delay = 0
        
        bestmodel = -1
        
        for (i in 1:nrow(csv)) {
          
          if (csv$Qty[i] > 0) {
            
            newCrit = CritValueBase(
              qty = csv$Qty[i],
              speed = csv$Speed[i],
              csv$Density[i],
              TimeInDay = TimeInDay,
              Current_Time = Current_Time,
              ttd = csv$ttd[i]
            )
            
            if(grepl('Q',csv$`Type`[i])&&csv$ttd[i]<=0){
              newCrit = newCrit/2
            }
            
            if (newCrit < Crit) {
              bestmodel = i
              
              Crit = newCrit
              
            }
        }
          
        }
        
        if (Current_Time > (TimeInDay / 2)) {
          Shift = 2
          
        }
        timeAdj = csv$Qty[bestmodel] * CalcTimeChange(csv[bestmodel, 'Density'], csv[bestmodel, "Speed"])*pessimism
        
        if (current_Paint != csv$Finish[bestmodel]) {
          current_Paint = csv$Finish[bestmodel]
          timeAdj = timeAdj + 5 * 60
        }
        
        
        Current_Part = csv$Model[bestmodel]
        Current_Speed = csv$Speed[bestmodel]
        current_Paint = csv$Finish[bestmodel]
        downtime = downtime + timeAdj
        if (downtime > 7200) {
          timeAdj = timeAdj + 10 * 60
          #delay = delay + 5*60
          downtime = Current_Time %% 7200
          
        }
        
        check1 = c(csv$`Model`[bestmodel], csv$`Type`[bestmodel], csv$`Due!Date`[bestmodel])
        check2 = c(Schedule[nrow(Schedule), 1], Schedule[nrow(Schedule), "Type"], Schedule[nrow(Schedule), "Due_Date"])
        
        if (all(check1 %in% check2)) {
          Schedule[nrow(Schedule), 5] = Schedule[nrow(Schedule), 5] + 1
          
          Schedule[nrow(Schedule), 'end_time'] = Schedule[nrow(Schedule), 'end_time'] +
            timeAdj
          
          Schedule[nrow(Schedule), 'duration_Seconds'] = Schedule[nrow(Schedule), 'duration_Seconds'] +
            (timeAdj)
        } else {
          new = data.frame(
            model = csv$`Model`[bestmodel],
            Type = csv$Type[bestmodel],
            line_Speed = csv$Speed[bestmodel],
            Due_Date = csv$`Due!Date`[bestmodel],
            qty = csv$Qty[bestmodel],
            start_time = (Schedule[nrow(Schedule), 'end_time']) +
              delay,
            end_time = (Schedule[nrow(Schedule), 'end_time']) + timeAdj + delay,
            duration_Seconds = timeAdj,
            isPerf = csv$isPerf[bestmodel],
            Finish = csv$Finish[bestmodel]
          )
          Schedule[nrow(Schedule) + 1, ] <- new
        }
        csv$Qty[bestmodel] <- 0
        Current_Time = Current_Time + timeAdj + delay
        
        
      }
      
      
      Schedule[, 'duration_Seconds'] <- round(Schedule[, 'duration_Seconds'])
      Schedule <- Schedule [-1,]
      rownames(Schedule) <- 1:nrow(Schedule)
      Sorted_Schedule = data.frame(
        model = c("model"),
        Type = c(0),
        line_Speed = c(0),
        Due_Date = c(0),
        qty = c(0),
        start_time = default_time,
        end_time = default_time,
        duration_Seconds = c(0),
        isPerf = F,
        Finish = 'B12'
      )
      Schedule_temp <- Schedule
      #sort function here
      sort_time = start_time
      print(paste0('start time: ', sort_time))
      Finish='B12'
      speed = 1
      model = 'na'
      for (i in 1:nrow(Schedule)){
        best_index = -1
        best_val = -1



        for(i in 1:nrow(Schedule_temp)){
          if((Schedule_temp$isPerf[i]==(cospi((sort_time / 7200)-0.5)>0))||length(grep(T,Schedule_temp$isPerf))==0||length(grep(F,Schedule_temp$isPerf))==0){}
          cur_val = (Schedule_temp$line_Speed[i])
          
          if(grepl('Q',Schedule_temp$Type[i])){
            cur_val = cur_val*6
          }
          
          if(model==Schedule_temp$model[i]){
            cur_val <- cur_val*2
          }
          cur_val = cur_val -(as.numeric(as.Date(Schedule_temp$`Due_Date`[i], format = "%m/%d/%y"))- as.numeric(Sys.Date()))/10
          
          cur_val = cur_val/(1+TimeChangePenalty(speed,i,Schedule_temp$line_Speed)/10)
          
          if (Schedule_temp$isPerf[i]) {cur_val <- cur_val * (((cospi((sort_time / 7200) - 0.5) / 2) + 1)^4)}
          if(Schedule_temp$Finish[i]!='B12'){
            
            cur_val <- cur_val /((25200/sort_time)^4)
          }
          if(Finish != 'B12'){
            if(Finish != Schedule_temp$Finish[i]){
              cur_val <- cur_val/10
            }
            if(Schedule_temp$Finish[i]== 'B12'){cur_val = cur_val/10}
          }
          
          if(best_val<cur_val){
            best_val <- cur_val
            best_index = i
          }
          }
        #}
        model = Schedule_temp$model[best_index]
        Finish = Schedule_temp$Finish[best_index]
        speed = Schedule_temp$line_Speed[best_index]
        sort_time = sort_time + Schedule_temp$duration_Seconds[best_index]
        Sorted_Schedule <- rbind(Sorted_Schedule,Schedule_temp[best_index,])
        Schedule_temp <- Schedule_temp[-best_index,]
        
      }
      
      
      
      
      Sorted_Schedule <- Sorted_Schedule[-1,]
      Sorted_Schedule <- pivot(Sorted_Schedule)
      rownames(Sorted_Schedule) <- 1:nrow(Sorted_Schedule)
      csv <- csv[-which(csv$Qty == 0), ]
      
      
      
      return(list(Sorted_Schedule, csv))
    }
  pivot <- function(csv) {
    csv$start_time <- min(csv$start_time)
    
    for (i in 1:(nrow(csv)) - 1) {
      csv$end_time[i] <- csv$start_time[i] + csv$duration_Seconds[i]
      csv$start_time[i + 1] <- csv$end_time[i]
    }
    csv$end_time[nrow(csv)] <-
      csv$start_time[nrow(csv)] + csv$duration_Seconds[nrow(csv)]
    
    return(csv)
  }
  
  
  observeEvent(input$analyze, {
    req(input$file)
    
    csv <- read_csv(input$file$datapath, trim_ws = FALSE, skip = 9)
    csv <- csvFormat(csv)
    
    if (!is.null(input$oldfile)) {
      olddata <- read_csv(input$oldfile$datapath)
      olddata <- olddata[, -1]
      if (ncol(olddata) != 11) {
        olddata <-
          read_csv(input$oldfile$datapath,
                   trim_ws = FALSE,
                   skip = 9)
        olddata <- csvFormat(olddata)
      }
      
      csv <- rbind(csv, olddata)
    }
    
    # Run your R script on the data
    # Replace 'your_function' with the function that processes the data and creates the schedule
    default_time = as.POSIXct("2023-06-16 06:00:00 EDT")
    date(default_time) <- Sys.Date()
    
    cur_time <-
      (as.numeric(difftime(
        with_tz(Sys.time(), 'America/New_York'), default_time
      ), units = "secs"))
    
    # Display the schedule in the main panel
    TimeInDay = (input$slider + 0.5) * 3600 * input$shiftAmt
    if (input$curTime) {
      data <-
        main(
          csv,
          TimeInDay = TimeInDay,
          shiftAmt = input$shiftAmt,
          Current_Time = cur_time,
          default_time = with_tz(Sys.time(), 'America/New_York'),
          pessimism = input$pessimism
        )
    } else{
      data <- main(csv,
                   TimeInDay = TimeInDay,
                   shiftAmt = input$shiftAmt,
                   pessimism = input$pessimism)
    }
    
    table <- data[[1]]
    todo <- data[[2]]
    
    stuff <- table
    stuff$start_time <- format(table$start_time, "%H:%M:%S")
    stuff$end_time <- format(table$end_time, "%H:%M:%S")
    
    exp_name_table <- reactive({
      stuff
    })
    
    output$exp_name_table <- DT::renderDataTable({
      exp_name_table()
    }, selection = 'none', editable = F, extensions = 'RowReorder', options =
      list(
        pageLength = nrow(table),
        dom = 'ltp',
        rowReorder = T,
        order = list(c(0, 'asc'))
      ), callback = JS(
        "table.on('row-reorder', function(e, details, changes) {
       Shiny.onInputChange('exp_name_table_row_reorder', JSON.stringify(details));
    });"
      ))
    file_order <- reactiveVal(value = seq(1, nrow(table)))
    observeEvent(input$exp_name_table_row_reorder, {
      info <- input$exp_name_table_row_reorder
      if (is.null(info) | class(info) != 'character') {
        return()
      }
      
      info <- read_yaml(text = info)
      if (length(info) == 0) {
        return()
      }
      
      .order <- file_order()
      .new_order <- .order
      
      for (i in 1:length(info)) {
        j <- info[[i]]
        .new_order[(j$newPosition + 1)] <- .order[(j$oldPosition + 1)]
      }
      
      # Replace the data object of a table output and avoid regenerating the full table,
      #.exp_name_table <- isolate(exp_name_table())
      # don't need DT::coerceValue like they use in example -- this will always be a string
      #.exp_name_table <- .exp_name_table[order(.new_order),]
      #DT::replaceData(exp_name_table_proxy2, .exp_name_table, resetPaging = FALSE, rownames = FALSE)
      
      file_order(.new_order)
      
      table <- table[.new_order, ]
      rownames(table) <- seq(1, nrow(table))
      table <- pivot(table)
      
      stuff <- table
      stuff$start_time <- format(table$start_time, "%H:%M:%S")
      stuff$end_time <- format(table$end_time, "%H:%M:%S")
      
      exp_name_table <- reactive({
        stuff
      })
      output$exp_name_table <- DT::renderDataTable({
        exp_name_table()
      }, selection = 'none', editable = F, extensions = 'RowReorder', options =
        list(
          pageLength = nrow(table),
          dom = 'ltp',
          rowReorder = T
        ), callback = JS(
          "table.on('row-reorder', function(e, details, changes) {
       Shiny.onInputChange('exp_name_table_row_reorder', JSON.stringify(details));
    });"
        ))
      output$plot <- renderPlotly({
        g <-
          ggplot(
            table,
            aes(
              x = start_time,
              y = line_Speed,
              group = model,
              label = qty,
              text = Due_Date
            )
          ) + geom_segment(
            linewidth = 1.5,
            aes(
              x = start_time,
              y = line_Speed,
              xend = end_time,
              yend = line_Speed,
              color = model
            )
          ) +
          ggtitle("Recommended Schedule") + xlab("Hours") + ylab("Line Speed") +
          theme(legend.position = "none") + scale_x_datetime(
            breaks = pretty(table$start_time, n = input$slider * input$shiftAmt),
            date_labels = "%H:%M"
          ) + ylim(0, 30)
        ggplotly(g, tooltip = c('x', 'y', 'group', 'label', 'text'))
      })
      
    })
    
    
    
    output$TODO <-
      DT::renderDataTable(
        data.frame(
          `Model` = todo$Model,
          `Type` = todo$Type,
          `Finish` = todo$Finish,
          `Due_Date` = todo$`Due!Date`,
          `Quantity` = todo$Qty
        )
        ,
        selection = 'none',
        editable = F,
        options = list(pageLength = nrow(todo), dom = 'ltp')
      )
    output$plot <- renderPlotly({
      g <-
        ggplot(table,
               aes(
                 x = start_time,
                 y = line_Speed,
                 group = model,
                 label = qty,
                 text = Due_Date
               )) + geom_segment(
                 linewidth = 1.5,
                 aes(
                   x = start_time,
                   y = line_Speed,
                   xend = end_time,
                   yend = line_Speed,
                   color = model
                 )
               ) +
        ggtitle("Recommended Schedule") + xlab("Hours") + ylab("Line Speed") +
        theme(legend.position = "none") + scale_x_datetime(
          breaks = pretty(table$start_time, n = input$slider * input$shiftAmt),
          date_labels = "%H:%M"
        ) + ylim(0, 30)
      ggplotly(g, tooltip = c('x', 'y', 'group', 'label', 'text'))
    })
    
    pphtime <-
      (as.numeric(difftime(
        max(data[[1]]$end_time), min(data[[1]]$start_time)
      ), units = "hours"))
    dashboard <-
      data.frame(
        Parts_per_Hour = sum(table$qty) / (pphtime),
        Parts_to_Produce = sum(table$qty),
        Parts_to_do_Tomorrow = sum(todo$Qty)
      )
    colnames(dashboard) <-
      c("Parts Per Hour", 'Parts to Produce', 'Parts to do Tomorrow')
    output$dashboard <- renderTable(dashboard)
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("schedule-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(table, file)
      }
    )
    output$downloadTODO <- downloadHandler(
      filename = function() {
        paste("unpainted_parts_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data[[2]], file)
      }
    )
    
    
  })
  
  
  
}
