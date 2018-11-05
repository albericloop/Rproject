library(shiny) 
library(shinydashboard) 
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(magrittr)

data<-read.csv("datasets/logs.csv",sep = ";")
str(data)
data$Time = ymd_hms(data$Time)
don<-xts(x = data$Latitude, order.by = data$Time)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    menuItem("Single users", tabName = "singleUsers"),
    menuItem("all users", tabName = "allUsers"),
    selectInput("var", 
    label = "Choose a type",
    choices = c("Friend", "Behaviour","Cheated","On time","Auto skipped","Skipped"),
    selected = "Friend") 
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "allUsers", 
              fluidRow(
                box( 
                    dygraph(don) %>%
                    dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
                    dyRangeSelector() %>%
                    dyCrosshair(direction = "vertical") %>%
                    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
                    dyRoller(rollPeriod = 1)
                  )
              )),
      tabItem(tabName = "singleUsers",
              h2("single users "))
    )
    
    
    
  ))

server <- function(input, output) {

} 

shinyApp(ui = ui, server = server)