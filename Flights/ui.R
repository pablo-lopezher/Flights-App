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
library(leaflet)
library(lubridate)
library(plotly)


dest1 <- sort(unique(data_Flights$dest))
dest <- c("All", dest1)


header <- dashboardHeader(
    title = "NYC Flights"
)

body <- dashboardBody(
    
    tabItems(
        
        tabItem("dashboard",
                
                fluidRow(
                    
                    column(width = 9,
                           box(width = NULL, solidHeader = TRUE,
                               leafletOutput("flights", height = 500))
                           ,
                           box( width = NULL, solidHeader = TRUE,
                                plotlyOutput("trendPlot", height = 250) )
                    ),
                    column(width = 3,
                           
                           box(width = NULL, status = "warning",
                               selectInput('flightSelect', 'Destinations',dest, multiple=TRUE, selectize=TRUE, selected = dest[1])),
                           
                           box(width = NULL, status = "warning",
                               dateRangeInput('dateRange',
                                              label = 'Date Range',
                                              start = ymd("2013-01-01"), end = ymd("2013-12-31"))
                           ),
                           box(width = NULL, solidHeader = TRUE,
                               plotlyOutput("barChart", height = 550))
                    )
                )
        ),
        
        tabItem("forecast",
                
                column( width = 12, offset = 0.5, titlePanel("Flights forecast"),
                        
                        fluidRow(column(12,div(style = "height:5px;background-color:transparent"))),
                        fluidRow(
                            column(4,
                                   selectInput("dest_fc", "Destination:",  dest1)),
                            column(3, offset = 5, 
                                   valueBoxOutput("mse", width = 8 ))),
                        fluidRow(
                            column(3,
                                   actionButton("action_fc",label = "Forecast", style ="color:white",class="btn btn-primary btn-md") 
                            )),
                        fluidRow(column(12,div(style = "height:30px;background-color:transparent"))),
                        fluidRow(column(12,
                                        box( width = NULL, solidHeader = TRUE,
                                             plotlyOutput("plot_fc", height = 400) )
                        ))           
                        
                )),
        
        tabItem("table",
                column( width = 10, offset = 0.5, titlePanel("NYC Flights data"),
                        fluidRow(
                            column(3,
                                   selectInput("dest", "Destination:",  dest) 
                            ),
                            column(3,
                                   selectInput("period", "Period:", c("Day", "Week", "Month", "Year","Day_Week") )),
                            column(3,
                                   selectInput("level","Level:",c())),
                            column(3,
                                   selectInput("gp_by","Group by period:",c("No","Yes")))
                        ),
                        box( width = NULL, solidHeader = T, 
                             DT::dataTableOutput("flightsTable"))
                )))
)

sidebar <- dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Forecast", tabName = "forecast", icon = icon("chart-line")),
        menuItem("Table", tabName = "table", icon = icon("table"))
    )
)    

dashboardPage(
    header,
    sidebar,
    body
)
















