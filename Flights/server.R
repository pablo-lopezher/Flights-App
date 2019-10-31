#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(stringi)
library(dplyr)
library(nycflights13)
library(ggthemes)
library(tools)

source("ann.R")


airports <- read.csv("Airports.csv",sep=";", encoding = "UTF-8")

data_Flights <- flights %>% 
    mutate(date = make_datetime(year, month,day)) %>% 
    dplyr::select(flight, origin, dest, date) %>% 
    group_by(dest,date) %>% 
    summarize(count = n()) %>% 
    inner_join(airports, by = c("dest" = "Airport")) 

Day <- c("All",sort(unique(day(data_Flights$date))))
Week <- c("All",sort(unique(week(data_Flights$date))))
Month <- c("All",sort(unique(month(data_Flights$date))))
Year <- c(unique(year(data_Flights$date)))
Day_Week <- c("All",sort(unique(wday(data_Flights$date))))



server <- function(input, output,session) {
    
    
    dataSelected <- reactive({
        
        minDate <- input$dateRange[1]
        maxDate <- input$dateRange[2]
        
        destSelected <- input$flightSelect
        
        f <- data_Flights  %>% 
            filter(
                date >= minDate,
                date <= maxDate
            ) %>%  
            group_by(dest, Lat, Long) %>% 
            summarize(count = sum(count))
        
        if (!("All" %in% destSelected)) {
            f <- filter(f, dest %in% destSelected)
        }
        
        f <- as.data.frame(f)
        f
        
    })
    
    
    dataSelected_byDate <- reactive({
        
        minDate <- input$dateRange[1]
        maxDate <- input$dateRange[2]
        
        destSelected <- input$flightSelect
        
        f <- data_Flights
        
        if (!("All" %in% destSelected)) {
            f <- filter(f, dest %in% destSelected)
        }
        
        f <- filter(f,
                    date >= minDate,
                    date <= maxDate
        ) %>%  
            group_by(date) %>% 
            summarize(count = sum(count))
        
        f <- as.data.frame(f)
        f
        
    })
    
    dataTable <- reactive({
        
        period <- tolower(input$period)
        ifelse(period=="day_week",period<-"wday",period)
        f <- data_Flights
        f$Period = flatten_dbl(invoke_map(period,data_Flights$date))
        
        if(input$dest != "All"){
            f <- f[f$dest == input$dest,]
        }
        
        if(input$level != "All"){
            f <- f[f$Period == as.numeric(input$level),]
        }
        
        if(input$gp_by == "No"){
            f <- f %>% group_by(Period,dest, Lat, Long) %>% 
                summarize(Flights = sum(count))
            names(f)[2] <- "Destination"
            
        } else {
            f <- f %>% group_by(Period) %>% 
                summarize(Flights = sum(count))
        }
        
        names(f)[1] <- toTitleCase(period)
        f
        
    })
    
    
    observe({
        
        updateSelectInput(session,"level",
                          choices = get(input$period),
                          label = paste(input$period,":")              
        )
        
    })
    
    
    output$flights <- renderLeaflet({
        
        colfunc<-colorRampPalette(c("dodgerblue2","khaki","blue")) 
        
        map <- leaflet(data = dataSelected()) %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png"
            ) %>%  
            addCircleMarkers(
                dataSelected()$Long,
                dataSelected()$Lat,
                radius = dataSelected()$count/max(dataSelected()$count)*25,
                fillColor = colfunc(nrow(dataSelected())),
                stroke=FALSE, fillOpacity=0.6
            )
    })
    
    
    output$trendPlot <- renderPlotly({
        
        ggideal_point <- ggplot(dataSelected_byDate()) +
            geom_line(aes(x = date, y = count),color="royalblue") +
            labs(x = NULL, y = "Flights") +
            theme(legend.direction = "horizontal", legend.position = "bottom")
        
        gg <- plotly_build(ggideal_point)
        gg$layout$annotations <- NULL
        gg$layout$annotations <- list()
        gg
        
    })
    
    
    output$barChart <- renderPlotly ({
        
        p <- plot_ly(y = reorder(dataSelected()$dest, dataSelected()$count), x = dataSelected()$count, type = 'bar', orientation = 'h',
                     marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                   line = list(color = 'rgba(50, 171, 96, 1.0)', width = 0.5)))
        p
        
    }) 
    
    
    output$flightsTable <- DT::renderDataTable(DT::datatable({
        dataTable()
    }))
    
    
    
    data_fc <- eventReactive(input$action_fc, {
        withProgress(message = 'Making plot', value=0.5, {  
            neural_nk(data_Flights,input$dest_fc)  
        })
    })
    
    
    output$plot_fc <- renderPlotly({
        
        ggideal_point <- ggplot(data_fc()) +
            geom_line(aes(x = date, y = flights_date,color="Actual flights")) +
            geom_line(aes(x = date, y = pred_flights,color="Predicted flights")) +
            scale_color_manual(values =  c("Actual flights" = "royalblue", "Predicted flights"="red")) +
            labs(y="Flights", x="Date", color = 'Flights')
        
        gg <- plotly_build(ggideal_point)
        gg
        
        
    })
    
    
    output$mse <- renderValueBox({
        valueBox(
            tags$p(paste0("MSE : ", round(mean((data_fc()$flights_date-data_fc()$pred_flights)^2),1)), style="font-size:50%;"),subtitle = NULL, icon = NULL, color = "light-blue")
        
    })
    
    
}



