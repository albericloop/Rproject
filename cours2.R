library(shiny) 
library(shinydashboard) 
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(magrittr)


preprocessing<-function(datalogs,datausers){
  
  newdate <- seq(as.Date(min(datalogs$Time)), as.Date(max(datalogs$Time)), by="days")
  
  daylist<-data.frame(date=newdate)
  daylist["Friend"]<-0
  daylist["Behaviour"]<-0
  daylist["Cheated"]<-0
  daylist["Ontime"]<-0
  daylist["Autoskipped"]<-0
  daylist["Skipped"]<-0
  daylist["Snoozed"]<-0
  daylist["NbPers"]<-0
  
  # for(i in 1:dim(daylist)[1]) {
  #   daylist$NbPers[i]<-count_if(le(daylist$date[i]), datausers$Started)
  # }
  
  for(i in 1:dim(datalogs)[1]) {
    date = datalogs$Time[i]
    type = datalogs$Type[i]
    type<-str_replace_all(string=type, pattern=" ", repl="")
    
    num<-which(daylist$date==date,arr.ind=TRUE)
    switch(type, 
           Behaviour={
             daylist$Behaviour[num]<-daylist$Behaviour[num]+1
           },
           Friend={
             daylist$Friend[num]<-daylist$Friend[num]+1  
           },
           Skipped={
             daylist$Skipped[num]<-daylist$Skipped[num]+1
           },
           Autoskipped={
             daylist$Autoskipped[num]<-daylist$Autoskipped[num]+1   
           },
           Ontime={
             daylist$Ontime[num]<-daylist$Ontime[num]+1
           },
           Cheated={
             daylist$Cheated[num]<-daylist$Cheated[num]+1   
           },
           Snoozed={
             daylist$Snoozed[num]<-daylist$Snoozed[num]+1   
           },
           {
           }
    )
  }
  return(daylist)
}


datalogs<-read.csv("datasets/logs.csv",sep = ";")
datausers<-read.xls("datasets/surveydataece.xlsx")

datalogs$Time <- strptime(as.character(datalogs$Time), "%d/%m/%Y")
datausers$Started <- strptime(as.character(datausers$Started),format = "%d-%b")
year(datausers$Started)<-2017
daylist<-preprocessing(datalogs,datausers)

don<-xts(x = daylist$Friend, order.by = daylist$date)

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