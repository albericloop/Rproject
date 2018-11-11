library(shiny) 
library(shinydashboard) 
library(dygraphs)
library(xts)        
library(tidyverse)
library(lubridate)
library(magrittr)
library(stringr)
library(gdata)
library(expss)
library(rworldmap)


preprocessing<-function(datalogs,datausers){
  
  #newdate <- seq(as.Date(min(datalogs$Time)), as.Date(max(datalogs$Time)), by="days")
  
  daylist<-data.frame(date=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
  daylist["Friend"]<-0
  daylist["Behaviour"]<-0
  daylist["Cheated"]<-0
  daylist["Ontime"]<-0
  daylist["Autoskipped"]<-0
  daylist["Skipped"]<-0
  daylist["Snoozed"]<-0
  daylist["Total"]<-0
  for(i in 1:dim(daylist)[1]) {
    daylist$Total[i]<-count_if(daylist$date[i], datalogs$Time)
  }
  
  friend <- subset(datalogs, Type == "Friend", select=c(Time))
  for(i in 1:dim(daylist)[1]) {
    daylist$Friend[i]<-count_if(daylist$date[i], friend$Time)
  }
  
  behaviour <- subset(datalogs, Type == "Behaviour", select=c(Time))
  for(i in 1:dim(daylist)[1]) {
    daylist$Behaviour[i]<-count_if(daylist$date[i], behaviour$Time)
  }
  
  cheated <- subset(datalogs, Type == "Cheated", select=c(Time))
  for(i in 1:dim(daylist)[1]) {
    daylist$Cheated[i]<-count_if(daylist$date[i], cheated$Time)
  }
  
  ontime <- subset(datalogs, Type == "On time", select=c(Time))
  for(i in 1:dim(daylist)[1]) {
    daylist$Ontime[i]<-count_if(daylist$date[i], ontime$Time)
  }
  
  autoskipped <- subset(datalogs, Type == "Auto skipped", select=c(Time))
  for(i in 1:dim(daylist)[1]) {
    daylist$Autoskipped[i]<-count_if(daylist$date[i], autoskipped$Time)
  }
  
  skipped <- subset(datalogs, Type == "Skipped", select=c(Time))
  for(i in 1:dim(daylist)[1]) {
    daylist$Skipped[i]<-count_if(daylist$date[i], skipped$Time)
  }
  
  snoozed <- subset(datalogs, Type == "Snoozed", select=c(Time))
  for(i in 1:dim(daylist)[1]) {
    daylist$Snoozed[i]<-count_if(daylist$date[i], snoozed$Time)
  }

  return(daylist)
}

pickedColors = colors()[c(30,35,40,45,50,12,60)]
newmap <- getMap(resolution = "low")
dataCountries <- data.frame(Country=c('Russia','Cyprus', 'Belize', 'Austria' ,'Virgin Islands', 
                                      'Italy','United States' ,'United Kingdom', 'Germany', 'France' ,'Poland' ,'Switzerland'),
                                      Value=c(-0.310,-0.206,-0.300,-0.179,-0.196,-0.174,-0.105,-0.142,-0.082,-0.097,-0.027,0.052))
pdf1 <- joinCountryData2Map(dataCountries, joinCode="NAME", nameJoinColumn="Country")
# get the coordinates for each country
country_coord<-data.frame(coordinates(pdf1),stringsAsFactors=F)

datalogs<-read.csv("datasets/logs.csv",sep = ";")
datausers<-read.xls("datasets/surveydataece.xlsx")
datalogs$Time <- strptime(as.character(datalogs$Time), "%d/%m/%Y %H:%M")
datalogs$Hour <- hour(datalogs$Time)
datalogs$Day <- weekdays(as.Date(datalogs$Time))

#datausers$Started <- strptime(as.character(datausers$Started),format = "%d-%b")
#year(datausers$Started)<-2017
#daylist<-preprocessing(datalogs,datausers)
# str(daylist$Friend)
# daylist$Friend<-as.integer(daylist$Friend)
# str(daylist$Friend)

varUser<- ""
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    menuItem("Single user", tabName = "singleUser"),
    menuItem("all users", tabName = "allUsers")
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "allUsers", 
              h2("all users: "),
              fluidRow(
                box(selectInput("varType", 
                                label = "Choose a type",
                                choices = c("Friend","Cheated","On time","Auto skipped","Skipped","Snoozed"),
                                selected = "Friend"),
                    selectInput("varTimeType", 
                                label = "Choose a time",
                                choices = c("Hour","Day"),
                                selected = "Hour")
                  ),
                box( 
                )
              ),
              fluidRow(
                box(plotOutput("countByTime"))
              ),
              fluidRow(
                
              )
              ),
      tabItem(tabName = "singleUser",
              h2("single user: "),
              h3(varUser),
              fluidRow(
                box(selectInput("varUser", 
                                label = "Choose a user",
                                choices = unique(datalogs$User),
                                selected = "Friend") 
                )),
              fluidRow(
              box(plotOutput("countBy")),
              box(plotOutput("pieType"))
              ),
              fluidRow(
                h3("smoking localization"),
                box(plotOutput("userMap"))
              ))
    )
  ))

server <- function(input, output) {
  
  #print(table(subset(datalogs, User == input$varUser)$Type))
  
  output$countByTime <- renderPlot({

    if( input$varTimeType=="Hour"){
      data<-subset(datalogs, Type == input$varType)$Hour
      text<-"Hour of the day"
    }else{
      DailyData<-subset(datalogs, Type == input$varType)
      data <- factor(DailyData$Day,labels = c("Monday","Thursday","Wednesday","Tuesday","Friday","Saturday","Sunday"))
      text<-"Day of the week"
    }
    barplot(table(data),ylab="number of smoking occurences",main=text, col=pickedColors)
    })
  
  output$countByDay <- renderPlot({
    data<-subset(datalogs, Type == input$varType)
    fdata <- factor(data$Day,labels = c("Monday","Thursday","Wednesday","Tuesday","Friday","Saturday","Sunday"))
    barplot(table(fdata),ylab="number of smoking occurences",main="Hour of the day", col=pickedColors)
  })
  
  output$countBy <- renderPlot({
    barplot(table(subset(datalogs, User == input$varUser)$Type),ylab="number of smoking occurences",main="occurence of smoking by type of smoking", col=pickedColors)
  })
  output$pieType <- renderPlot({
    # Calculate the percentage for each day, rounded to one decimal place
    slices_labels <- round(table(subset(datalogs, User == input$varUser)$Type)/sum(table(subset(datalogs, User == input$varUser)$Type)) * 100, 1)
    
    # Concatenate a '%' char after each value
    slices_labels <- paste(slices_labels, "%", sep="")
    pie(table(subset(datalogs, User == input$varUser)$Type),labels = slices_labels, main="proportion of smoking types",col=pickedColors)
    
  })
  output$userMap <- renderPlot({
    plot(newmap, xlim = c(35, 36), ylim = c(32, 35), asp = 1)
    points(subset(datalogs, User == input$varUser)$Longitude,subset(datalogs, User == input$varUser)$Latitude, col = "red", cex = .6)
    text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord))
    
  })
  output$userTime <- renderPlot({
    plot(daylist$date,daylist$Friend)
  })
  
} 

shinyApp(ui = ui, server = server)