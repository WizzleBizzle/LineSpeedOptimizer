
#define 3rd party libraries this program needs to run
library(shiny)
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(DT)
library(yaml)
library(plotly)
library(R6)

#define entire file as shiny function for deployment
function(input, output) {
  
  #CalcTimeChange is the function that implements Will's density function
  CalcTimeChange <- function(density, speed) {
    prod_time = (2.4 * (1 / density) / speed) * 60
    return(prod_time)
  }
  
  #this function reads a Raw DOM and formats it for optimization
  #I couldn't find a more efficient way to do it, so everything is hard coded
  csvFormat <- function(csv,tmrw) {
    date = Sys.Date()
    if(as.POSIXlt(Sys.Date())$wday==5&&tmrw){
      date=date+3
    }else if(tmrw){
      date = date+1
    }
    #Remove rows that contain no useful data
    #the grep function will be used a lot in this function, so make sure you
    #understand it!
    csv <- csv[-grep("@@", csv$`Order!Count`),]
    csv <- csv[-grep("##", csv$`Order!Count`),]
    csv <- csv[-grep("Order!Count", csv$`Order!Count`),]
    
      
    #remove parts that don't need to be painted based on group
    #note the if statement, this is to ensure the line of code within it
    #doesn't run if there are no lines to remove
    #if it were to run, the csv would be corrupted
    if (length(grep("DSR-ATL-EXT", csv$`Group`)) > 0) {
      csv <- csv[-grep("DSR-ATL-EXT", csv$`Group`),]
    }
    if (length(grep("GRD ATL-EXT", csv$`Group`)) > 0) {
      csv <- csv[-grep("GRD ATL-EXT", csv$`Group`),]
    }
    if (length(grep("GRD ATL-SMD", csv$`Group`)) > 0) {
      csv <- csv[-grep("GRD ATL-SMD", csv$`Group`),]
    }
    
    #remove parts that don't need to be painted by model
    #note the if statement, included for the same reason as above
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

    #remove parts that have already been painted and scanned into system
    if (length(grep("Final Assembly", csv$`Station`)) > 0) {
      csv <- csv[-grep("Final Assembly", csv$`Station`),]
    }
    if (length(grep("Paint Line", csv$`Station`)) > 0) {
      csv <- csv[-grep("Paint Line", csv$`Station`),]
    }
    
    #remove parts that are on hold
    if (length(grep("Hold", csv$`Status`)) > 0) {
      csv <- csv[-grep("Hold", csv$`Status`),]
    }
    
    #touch up painting is done manually and thus don't need to be rehung
    if (length(grep("TUP", csv$`Model`)) > 0) {
      csv <- csv[-grep("TUP", csv$`Model`),]
    }
    
    #Remove colors and block out time for them instead
      csv <- csv[which(csv$Finish == "B12"),]
    

    #add new columns that will assist with optimization: Fixed cone, speed, shift #,
    #and part density
    csv <- cbind(csv, FC = grepl("FC", csv$Description))
    csv <- cbind(csv, Speed = 1)
    csv <- cbind(csv, Shift = 0)
    csv <- cbind(csv, Density = 1)
    
    #This is the ugliest part of the code: manually assign speeds densities to every part
    #If you're editing this, ensure that some of the more generic catches don't overwrite more specific ones
    #e.g do '530' before '530ff', as the other order will remove the 530ff's information 
    
    #no if statment needed as this is additive, not subtractive
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
    
    #for loop to enter in ascd's information
    for (i in grep("ASCD-", csv$`Model`)) {
      if (csv$FC[i]) {
        csv[i, 'Speed'] <- 14
      }
    }
    
    
    #more hard coded speeds and density
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
    
    
    
    
    #assign time til due and prod time columns
    csv <- cbind(csv, ttd = 0)
    csv <- cbind(csv, prod_time = 0)
    
    #calculate time til due and prod time (not adjusted for density yet)
    for (i in 1:nrow(csv)) {
      csv[i, "ttd"] <-
        as.numeric(as.Date(csv$`Due!Date`[i], format = "%m/%d/%y")) - as.numeric(date)
      csv[i, "prod_time"] <- 60 / (csv[i, 'Speed'] / 2.4)
    }
    #convert qty to numeric vs character
    csv[, "Qty"] <- as.numeric(csv[, "Qty"])
    
    #Define parts considered 'Perf' as such
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
    
    #remove model and rev number from parts
    csv$Model <- gsub('-.*','',csv$Model)
    
    #add /FC to fixed cone parts
    csv[grep(TRUE, csv$FC), 'Model'] <-
      paste0(csv[grep(TRUE, csv$FC), 'Model'], "/FC")
    
    #add paint type to parts that aren't b12, 'tried' in case there are none
    try(csv[-which(csv$Finish == "B12"), 'Model'] <-
          paste0(csv[-which(csv$Finish == "B12"), 'Model'], "/", csv[-which(csv$Finish ==
                                                                              "B12"), 'Finish']))
    
    # csv <- na.omit(csv[,c('ttd' , 'Due!Date','Ack.!Date' , 'Model' , 'Speed' , 'prod_time' , 'isPerf' ,
    #                       'Type' , 'Shift' ,'Finish' , 'Density', 'Qty')])
    
    #aggregate csv based on important values. this is crucial to decrease processing time
    csv <-
      aggregate(
        Qty ~ `ttd` + `Due!Date`+`Ack.!Date` + `Model` + `Speed` + `prod_time` + `isPerf` +
          `Type` + `Shift` + `Finish` + `Density`,
        data = csv,
        FUN = sum
      )
    
    #set min ttd to 0. must do this to make critical ratio work properly
    csv[,'ttd'] <- csv[,'ttd'] - min(csv[,'ttd'])
    
    #adjust ttd for quick ship parts such that they have a high priority if due the same day, much less if not
    for(i in grep('Q',csv$Type)){
      csv$ttd[i]= (as.numeric(as.Date(csv$`Ack.!Date`[i], format = "%m/%d/%y"))-as.numeric(date))
      if(csv$ttd[i]>0){
        csv$ttd[i] <- csv$ttd[i]*1.5
      }
        
    }
    
    
    # csv<- csv[-grep("B12",csv$Finish),]
    
    #output the csv
    return(csv)
  }
  
  
  #cross referencing function: this uses SQL to cross reference the completed schedule and the
  #imported DOM file. it subtracts completed parts from the DOM
  #This is not a great solution, but will hopefully work well enough until scanning is added to Perf
  csvMerge <- function(csv,complete){
    complete <- complete[grep(TRUE,complete$isPerf),]
    
    test <- full_join(csv,complete, by = c('Model'='model','Type'='Type','Due!Date'="Due_Date",'Ack.!Date'="Ack_Date", 'Finish' = 'Finish', 'isPerf' = 'isPerf')) %>% mutate(Qty = replace_na(Qty, 0) - replace_na(qty, 0))
    
    test <-
      aggregate(
        Qty ~ `ttd` + `Due!Date`+`Ack.!Date` + `Model` + `Speed` + `prod_time` + `isPerf` +
          `Type` + `Shift` + `Finish` + `Density`,
        data = test,
        FUN = sum
      )
    return(test)
    
  }
  
  #function assigns small penalty for line speed changes
  TimeChangePenalty <- function(curspeed, model, Line_Speeds) {
    return(abs(curspeed - Line_Speeds[model]))
  }
  
  #base critical value function, generates default prioritization of parts based on 
  # ttd and time to complete
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
  
  #main function. very convoluted, but this function is where the hard math is done
  main <-
    function(csv,
             TimeInDay = 57600,
             shiftAmt = 2,
             Current_Time = 900,
             default_time = as.POSIXct("2023-06-16 06:15:00 EDT"),
             pessimism = 1,
             tmrw = F) {
      
      #define variables that will help with math later. first shift, start time,
      # current part, current speed, current paint type, minimum time til due, and downtime
      Shift = 1
      start_time = Current_Time
      Current_Part = "na"
      Current_Speed = 0
      current_Paint = "B12"
      min_ttd = min(csv$ttd)
      downtime = Current_Time %% 7200
      
      #define date (not time) of default time to be today
      date(default_time) <- Sys.Date()
      
      if(as.POSIXlt(Sys.Date())$wday==5&&tmrw){
        date(default_time) <- Sys.Date()+3
      }else if(tmrw){
        date(default_time) <- Sys.Date()+1
      }
      
      
      print(default_time)
      
      #initialize schedule
      Schedule = data.frame(
        model = c("model"),
        Type = c(0),
        line_Speed = c(0),
        Due_Date = c(0),
        Ack_Date = c(0),
        qty = c(0),
        start_time = default_time,
        end_time = default_time,
        duration_Seconds = c(0),
        isPerf = F,
        Finish = 'B12'
      )
      
      #first loop to solve optimization problem: this finds the most important parts
      #that must be done today and gathers them all up
      #this does NOT generate the best order to hang them in
      while (Current_Time < TimeInDay && sum(csv$Qty) > 0) {
        
        #make impossible values that will be overwritten each iteration
        Crit = 9999999999999
        delay = 0
        bestmodel = -1
        
        #for loop that checks every possible part in csv to see if it should be prioritized
        #uses base critical value, and weights heavily towards quick ships due the day of being
        #selected. quick ships due the day of should probably be hard coded to be selected,
        # and I'd put that as a reasonably high priority improvement to make, as some could theoretically slip
        #through the cracks
        for (i in 1:nrow(csv)) {
          
          if (csv$Qty[i] > 0) {
            #find crit of part that for loop is currently on
            newCrit = CritValueBase(
              qty = csv$Qty[i],
              speed = csv$Speed[i],
              csv$Density[i],
              TimeInDay = TimeInDay,
              Current_Time = Current_Time,
              ttd = csv$ttd[i]
            )
            #slants schedule to choose both perf and non perf parts using sine wave
            if (csv$isPerf[i]) {newCrit <- newCrit * (((cospi((Current_Time / 7200) - 0.5) / 2) + 1)^10)}
            
            #weight it if it's a quick ship due today
            if(grepl('Q',csv$`Type`[i])&&csv$ttd[i]<=0){
              newCrit = -1
            }
            
            #if the current crit val is better than the previous best crit val, the current
            #possible selection becomes the best part
            if (newCrit < Crit) {
              bestmodel = i
              
              Crit = newCrit
              
            }
        }
          
        }
        #for loop ends with best part selected
        
        #update shift as needed
        if (Current_Time > (TimeInDay / 2)) {
          Shift = 2
          
        }
        #adjust time in day based on Will's density function and pessimism/optimism
        timeAdj = csv$Qty[bestmodel] * CalcTimeChange(csv[bestmodel, 'Density'], csv[bestmodel, "Speed"])*pessimism
        
        #downtime added if color swap happens
        if (current_Paint != csv$Finish[bestmodel]) {
          current_Paint = csv$Finish[bestmodel]
          timeAdj = timeAdj + 5 * 60
        }
        
        #set current part, speed, and paint to new model type
        Current_Part = csv$Model[bestmodel]
        Current_Speed = csv$Speed[bestmodel]
        current_Paint = csv$Finish[bestmodel]
        downtime = downtime + timeAdj
        
        #add scheduled downtime as normal
        if (downtime > 7200) {
          timeAdj = timeAdj + 10 * 60
          #delay = delay + 5*60
          downtime = Current_Time %% 7200
          
        }
        
        #this is a convoluted code section, but in short it creates a new csv with only the parts to do for the day.
        #it's a holdover from a time when the code only used one while loop, and it works so
        #I don't care enough to change it
        
          new = data.frame(
            model = csv$`Model`[bestmodel],
            Type = csv$Type[bestmodel],
            line_Speed = csv$Speed[bestmodel],
            Due_Date = csv$`Due!Date`[bestmodel],
            Ack_Date = csv$`Ack.!Date`[bestmodel],
            qty = csv$Qty[bestmodel],
            start_time = (Schedule[nrow(Schedule), 'end_time']) +
              delay,
            end_time = (Schedule[nrow(Schedule), 'end_time']) + timeAdj + delay,
            duration_Seconds = timeAdj,
            isPerf = csv$isPerf[bestmodel],
            Finish = csv$Finish[bestmodel]
          )
          Schedule[nrow(Schedule) + 1, ] <- new
        #remove part from option once it's been finished by setting qty to 0
        csv$Qty[bestmodel] <- 0
        
        #advance time by delay and time adj (production time)
        Current_Time = Current_Time + timeAdj + delay
        
        
      }
      #end while loop
      
      #as requested by Will, adding in a block of time each day for colors
      new = data.frame(
        model = "Colors",
        Type = "N/A",
        line_Speed = 5,
        Due_Date = csv$`Due!Date`[bestmodel],
        Ack_Date = csv$`Ack.!Date`[bestmodel],
        qty = 1,
        start_time = (Schedule[nrow(Schedule), 'end_time']),
        end_time = (Schedule[nrow(Schedule), 'end_time']),
        duration_Seconds = 10800,
        isPerf = F,
        Finish = "Color"
      )
      Schedule[nrow(Schedule) + 1, ] <- new
      
      
      #now things get weird
      #this is another while loop that sorts the data gathered above to ensure that 
      #parts are done in a better order each day
      
      #schedule is edited for use, rounding seconds and removing errant row always created
      #during initialization
      Schedule[, 'duration_Seconds'] <- round(Schedule[, 'duration_Seconds'])
      Schedule <- Schedule [-1,]
      rownames(Schedule) <- 1:nrow(Schedule)
      
      #sorted schedule is initialized, and will be the eventual result of the loop
      Sorted_Schedule = data.frame(
        model = c("model"),
        Type = c(0),
        line_Speed = c(0),
        Due_Date = c(0),
        Ack_Date = c(0),
        qty = c(0),
        start_time = default_time,
        end_time = default_time,
        duration_Seconds = c(0),
        isPerf = F,
        Finish = 'B12'
      )
      
      #temp schedule is made to not destroy original 'schedule' csv
      Schedule_temp <- Schedule
      
      #initializing a bunch of needed variables for sort function
      sort_time = start_time
      print(paste0('start time: ', sort_time))
      Finish='B12'
      speed = 1
      model = 'na'
      
      #sort loop begins
      for (i in 1:nrow(Schedule)){
        
        #same idea as while loop, but only uses line speed instead of critical value
        
        #initialize important values with impossible numbers
        best_index = -1
        best_val = -1



        for(i in 1:nrow(Schedule_temp)){
          
          
          
          #this if isn't used, but was an option I made for sorting. leaving it in as you may get some use out of it
          #I don't entirely remember what it was for, only that the way I currently have it was better
          #if((Schedule_temp$isPerf[i]==(cospi((sort_time / 7200)-0.5)>0))||length(grep(T,Schedule_temp$isPerf))==0||length(grep(F,Schedule_temp$isPerf))==0){}
          
          #cur val is just line speed for part
          cur_val = (Schedule_temp$line_Speed[i])
          
          #make Q's happen asap
          if(grepl('Q',Schedule_temp$Type[i])){
            cur_val = cur_val*6
          }
          
          #if a part is being hung, weight towards hanging all of that type
          if(model==Schedule_temp$model[i]){
            cur_val <- cur_val*2
          }
          
          #fudge value to make parts due first done first, but not great enough change to effect much except tie breaking
          cur_val = cur_val -(as.numeric(as.Date(Schedule_temp$`Due_Date`[i], format = "%m/%d/%y"))- as.numeric(Sys.Date()))/10
          
          #fudge value to make schedule change line speed less dramatically
          cur_val = cur_val/(1+TimeChangePenalty(speed,i,Schedule_temp$line_Speed)/10)
          
          #the cospi math represents the desire to swap between 'Perf' and not 'perf every two ish hours
          #whenever you see it, remember that it's just the perf weight
          if (Schedule_temp$isPerf[i]) {cur_val <- cur_val * (((cospi((sort_time / 7200) - 0.5) / 2) + 1)^8)}
          
          if(sort_time<25200&&Schedule_temp$model[i]=="Colors"){
            cur_val=-1
          }else if(sort_time>25200&&Schedule_temp$model[i]=="Colors"){
            cur_val=9999999999999
          }
        
          
          #at the end of all that, it sees if the current value is greater than the 
          #best value (note the change from before, higher is better now)
          if(best_val<cur_val){
            best_val <- cur_val
            best_index = i
          }
          }
        #}
        
        #then record all the data and delete it from the temp schedule
        model = Schedule_temp$model[best_index]
        Finish = Schedule_temp$Finish[best_index]
        speed = Schedule_temp$line_Speed[best_index]
        sort_time = sort_time + Schedule_temp$duration_Seconds[best_index]
        Sorted_Schedule <- rbind(Sorted_Schedule,Schedule_temp[best_index,])
        Schedule_temp <- Schedule_temp[-best_index,]
        
      }
      
      
      
      #remove the initialization data from the sorted schedule, run the 'pivot' function
     # to recalculate times, and reorder the names
      Sorted_Schedule <- Sorted_Schedule[-1,]
      Sorted_Schedule <- pivot(Sorted_Schedule)
      rownames(Sorted_Schedule) <- 1:nrow(Sorted_Schedule)
     
      #remove 0 qty rows from csv, for use in 'parts to do' table
       csv <- csv[-which(csv$Qty <= 0), ]
      
       #return sorted schedule and 'parts still to do'
      return(list(Sorted_Schedule, csv))
    }
  
  #pivoting functionality,  recalculates start and end time of parts
  #used during the drag and drop operation
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
  
  #shiny is weird, but all this is to actually generate an interactive GUI
  #I will explain it as best as I can
  observeEvent(input$analyze, {
    #only do anything if a file has been input into gui
    req(input$file)
    
    #read csv, skipping faulty info found in DOMS
    csv <- read_csv(input$file$datapath, trim_ws = FALSE, skip = 9)
    
    #format csv
    csv <- csvFormat(csv,tmrw = input$tmrw)
    
    #legacy data, can safely ignore
    if (!is.null(input$oldSchedule)) {
      olddata <- read_csv(input$oldSchedule$datapath)
      csv <- csvMerge(csv,olddata)
      
    }
    #end ignore
    
    # begin to run your R script on the data
    #set default time to today at 6:00 AM
    default_time = as.POSIXct("2023-06-16 06:00:00 EDT")
    date(default_time) <- Sys.Date()
    
    #set time zone
    cur_time <-
      (as.numeric(difftime(
        with_tz(Sys.time(), 'America/New_York'), default_time
      ), units = "secs"))
    #convert input sliders to time in day in seconds
    TimeInDay = (input$slider + 0.5) * 3600 * input$shiftAmt
    
    #two ways to run main, either using current time or not
    #after this runs, the schedule has been made
    if (input$curTime) {
      data <-
        main(
          csv,
          TimeInDay = TimeInDay,
          shiftAmt = input$shiftAmt,
          Current_Time = cur_time,
          default_time = with_tz(Sys.time(), 'America/New_York'),
          pessimism = input$pessimism,
          tmrw = input$tmrw
        )
    } else{
      data <- main(csv,
                   TimeInDay = TimeInDay,
                   shiftAmt = input$shiftAmt,
                   pessimism = input$pessimism,
                   tmrw = input$tmrw)
    }
    
    #get schedule and todo from main's data output
    table <- data[[1]]
    todo <- data[[2]]
    
    #very very confusing as shiny is weird, but this will make the start and end time
    #show up properly
    #we're making a new variable with it as another section of code needs them
    #unformatted
    stuff <- table
    stuff$start_time <- format(table$start_time, "%H:%M:%S")
    stuff$end_time <- format(table$end_time, "%H:%M:%S")
    
    #define stuff(the formatted schedule) as 'reactive' under the new name
    #exp_name_table. this will be useful for drag and drop later
    exp_name_table <- reactive({
      stuff
    })
    
    #render the formatted schedule.
    #the row reorder and callback JS is to allow for drag and drop functionality
    #it's not going to get any less simple, apologies
    #I barely understand how I managed to get this to work
    #note that this function is called again under the row reordering event below
    output$exp_name_table <- DT::renderDataTable({
      exp_name_table()
    },selection = 'single', editable = T, extensions = c('RowReorder','Buttons'), options =
      list(
        pageLength = nrow(table),
        dom = 'ltBp',buttons = c(
          "csv"
        ),
        rowReorder = T
      ), callback = JS(
        "table.on('row-reorder', function(e, details, changes) {
       Shiny.onInputChange('exp_name_table_row_reorder', JSON.stringify(details));
    });"
      ))
    
    #define the order of the sorted table as a reactive value
    
    file_order <- reactiveVal(value = seq(1, nrow(table)))
    
    #on drag and drop event happening, do all this stuff 
    #I wish I could better explain, but much of this code was copied over
    #and shoehorned into working
    #in short, it reorders the table and gui, and then completely regenerates them,
    #running the pivot function before finally re displaying them
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
      },selection = 'single', editable = T, extensions = c('RowReorder','Buttons'), options =
        list(
          pageLength = nrow(table),
          dom = 'ltBp',buttons = c(
            "csv"
          ),
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
      
      output$downloadTable <- downloadHandler(
        filename = function() {
          paste("Schedule_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(table, file)
        }
      )
      
    })
    
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste("Schedule_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(table, file)
      }
    )
    
    
    #generate todo table
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
    #generate plot
    #note that this function is called under the row reordering event above
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
    
    #calculate parts per hour
    pphtime <-
      (as.numeric(difftime(
        max(data[[1]]$end_time), min(data[[1]]$start_time)
      ), units = "hours"))
    
    #generate dashboard info
    dashboard <-
      data.frame(
        Parts_per_Hour = sum(table$qty) / (pphtime),
        Parts_to_Produce = sum(table$qty),
        Parts_to_do_Tomorrow = sum(todo$Qty)
      )
    colnames(dashboard) <-
      c("Parts Per Hour", 'Parts to Produce', 'Parts to do Tomorrow')
    
    #render dashboard
    output$dashboard <- renderTable(dashboard)
    
    #assign functionality to download button for stuff to do tomorrow
    
    
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
