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
library(readxl)

makeTabByUser<-function()
{
  users <- unique(datalogs$User)
  nbUser <- length(users)
  userlist <- data.frame(1:nbUser)
  userlist["Name"] <- 0
  userlist["BehaviourNumber"] <- 0
  userlist["SmokedNumber"] <- 0
  userlist["DaysNumber"] <- 0
  userlist["savedCigarettes"] <- 0
  for(i in 1:length(users)){
    userlist$Name[i] = toString(users[i])
    data <- subset(datalogs, User == users[i])
    smokedBehaviour <- count_if("Behaviour", data$Type)
    
    smokedCheated <- count_if("Cheated", data$Type)
    smokedOntime <- count_if("On time", data$Type)
    
    smokedCigarettes <- (smokedCheated+smokedOntime)
    
    datecount <- difftime(as.Date(max(data$Date)) ,as.Date(min(data$Date)) , units = c("days"))
    #datecount <- as.Date(max(data$Date)) - as.Date(min(data$Date))
    userlist$BehaviourNumber[i] <- smokedBehaviour
    userlist$SmokedNumber[i] <- smokedCigarettes
    userlist$DaysNumber[i] <- datecount-7
    if(datecount > 7){
      userlist$savedCigarettes[i] <- (smokedBehaviour/7)*(datecount-7) - (smokedCigarettes)
    }else{
      userlist$savedCigarettes[i] <- 0
    }
  }
  return(userlist)
}


pickedColors = colors()[c(30,35,40,45,50,12,60)]
newmap <- getMap(resolution = "low")
dataCountries <- data.frame(Country=c('Russia','Cyprus', 'Belize', 'Austria' ,'Virgin Islands', 
                                      'Italy','United States' ,'United Kingdom', 'Germany', 'France' ,'Poland' ,'Switzerland'),
                                      Value=c(-0.310,-0.206,-0.300,-0.179,-0.196,-0.174,-0.105,-0.142,-0.082,-0.097,-0.027,0.052))
pdf1 <- joinCountryData2Map(dataCountries, joinCode="NAME", nameJoinColumn="Country")
# get the coordinates for each country
country_coord<-data.frame(coordinates(pdf1),stringsAsFactors=F)

datalogs<-read.csv("datasets/logs.csv", header=TRUE, sep = ";", encoding = "MacRoman")
#datausers<-read.xls("datasets/surveydataece.xlsx")
datalogs$Date <- strptime(as.character(datalogs$Time), "%d/%m/%Y")

datalogs$Time <- strptime(as.character(datalogs$Time), "%d/%m/%Y %H:%M")

datalogs$Hour <- hour(datalogs$Time)
datalogs$Day <- weekdays(as.Date(datalogs$Time))

tabByUser <- makeTabByUser()
print(tabByUser)
cigPrice = 1

varUser<- ""

dataSurvey = read_excel("datasets/surveydataece.xlsx")


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(
    menuItem("Single user", tabName = "singleUser"),
    menuItem("All users", tabName = "allUsers")
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "allUsers", 
              #h2("All users: "),
             tabBox(
                  # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "100%", width = "100%",
                tabPanel("Information", "",
                         fluidRow(
                           box(  verbatimTextOutput("totalCigSaved")),
                           box(  verbatimTextOutput("avgCigSaved")),
                           box(  verbatimTextOutput("totalMoneySaved")),
                           box(  verbatimTextOutput("avgMoneySaved"))
                         )    
                ),
                tabPanel("Classic", "",
                    box(plotOutput("countByTime")),
                    #box(plotOutput("countAllUsers")),
                    fluidRow(
                      box(selectInput("varType",
                                      label = "Choose a type",
                                      choices = c("All","Friend","Cheated","On time","Auto skipped","Skipped","Snoozed"),
                                      selected = "Friend"),
                          selectInput("varTimeType",
                                      label = "Choose a time",
                                      choices = c("Hour","Day"),
                                      selected = "Day")
                      )
                    )
                ),
                tabPanel("Engagement", "",
                 fluidRow(
                   box(plotOutput("userEngagement")),
                   box(plotOutput("userEngagementHour"))
                 )    
                 )
              )
      ),
      tabItem(tabName = "singleUser",
              fluidRow(
              box(selectInput("varUser2",
                             label = "Choose a user",
                              choices = unique(dataSurvey$Name),
                              selected = "Friend")
              )),
              fluidRow(
                tabBox(
                  title = "single user",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1",
                  tabPanel("information",
                           h2("age"),
                           textOutput("ageCategory")
                           ),
                  tabPanel("old info",
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
                    ),
                    fluidRow(
                      h3("Progression"),
                      p("The progression is a ratio computed according to intital frequence of smoking (behavior) and giving bonuses to manual skips and maluses to cheat"),
                      box(selectInput("varProgPeriod", 
                                      label = "Choose a period type",
                                      choices = c("weeks","days"),
                                      selected = "weeks") 
                      ),
                      box(plotOutput("prog"))
                    ))
                )
              )
     )
    )
  ))


server <- function(input, output) {
  
  output$countByTime <- renderPlot({
    print("1")
    if( input$varTimeType=="Hour"){
      if(input$varType == "All"){
        data <- datalogs$Hour
      }else{
        data<-subset(datalogs, Type == input$varType)$Hour
      }
      text<-"Hour of the day"
    }else{
      print("2")
      if(input$varType == "All"){
        DailyData<-datalogs
      }else{
        DailyData<-subset(datalogs, Type == input$varType)
      }
      data <- factor(DailyData$Day,labels = c("Monday","Thursday","Wednesday","Tuesday","Friday","Saturday","Sunday"))
      text<-"Day of the week"
    }
    print("3")
    barplot(table(data),ylab="number of smoking occurences",main=text, col=pickedColors)
  })
  
  output$countByDay <- renderPlot({
    data<-subset(datalogs, Type == input$varType)
    fdata <- factor(data$Day,labels = c("Monday","Thursday","Wednesday","Tuesday","Friday","Saturday","Sunday"))
    barplot(table(fdata),ylab="number of smoking occurences",main="Hour of the day", col=pickedColors)
  })
  output$countAllUsers <- renderPlot({
    barplot(table(datalogs$Type),ylab="number of smoking occurences",main="occurence of smoking by type of smoking", col=pickedColors)
  })
  output$countBy <- renderPlot({
    barplot(table(subset(datalogs, User == input$varUser)$Type),ylab="number of smoking occurences",main="occurence of smoking by type of smoking", col=pickedColors)
  })
  output$prog <- renderPlot({
    
    sub <- subset(datalogs, User == input$varUser)
    sub <- sub[,c("Type","Time")]
    
    
    regularWeekCount = as.numeric(table(sub$Type)["Behaviour"])
    
    sub$Type = as.numeric(sub$Type)
    
    sub$Type[sub$Type == 1] = 1
    sub$Type[sub$Type == 2] = -1
    sub$Type[sub$Type == 3] = -2
    sub$Type[sub$Type == 4] = 0
    sub$Type[sub$Type == 5] = -1
    sub$Type[sub$Type == 6] = 1
    sub$Type[sub$Type == 7] = 0
    
    
    
    sub$Date <- strftime(sub$Time,format="%d/%m/%Y %H:%M")
    sub$Day <- strftime(sub$Time,format="%d/%m/%Y")
    # week
    sub$Week <- strftime(sub$Time,format="%W")
  
    #progDay
    if(input$varProgPeriod == "days"){
      progDay <- aggregate(x=sub$Type, by=list(date = sub$Day), FUN=sum)
      progDay$x <- 1 - (as.numeric(progDay$x))/(-regularWeekCount/7)
      barplot(progDay$x,names.arg = factor(progDay$date))
    }
    
    if(input$varProgPeriod == "weeks"){
      #progWeek
      progWeek <- aggregate(x=sub$Type, by=list(date = sub$Week), FUN=sum)
      progWeek$x <- 1 - (as.numeric(progWeek$x))/(-regularWeekCount)
      barplot(progWeek$x,names.arg = factor(progWeek$date))
    }
  })
  
  output$ageCategory <- renderText({ 
    dataSurvey[dataSurvey$Name == "Renaud Courbis","Age"]
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
  
  output$userEngagement <- renderPlot({
    print("11")
    users<-unique(datalogs$User)
    daylist<-data.frame(date=c(1:200))
    daylist["Score"]<-15
    daylist["nbUser"]<-0
    print("22")
    for(i in 1:length(users)){
      data <- subset(datalogs, User == users[i])
      newdate <- seq(as.Date(min(data$Date)), as.Date(max(data$Date)), by="days")
      cpt <- 0
      for(j in 1:length(newdate)){
        subless <- subset(data, Date == newdate[j])
        cntless<-count_if("Auto skipped", subless$Type)
        cnt<- nrow(subless)
        if(j>7){
          res <-cntless
          cpt <- cpt+1
          daylist$Score[cpt] <- daylist$Score[cpt]-res
          if(cnt!=0){
            daylist$nbUser[cpt] <- daylist$nbUser[cpt]+1
          }
        }
      }
    }
    plot(x=daylist$date, y=daylist$Score/daylist$nbUser, xlim=c(1,100),ylim=c(-15,0),
         col='black', type='l',
         main='Engagement following the number of days of testing', xlab='number of days', ylab='engagement')
  })
  
  output$userEngagementHour <- renderPlot({
    users<-unique(datalogs$User)
    daylist<-data.frame(date=c(0:23))
    daylist["Score"]<-0
    daylist["nbUser"]<-0
    for(i in 1:length(users)){
      data <- subset(datalogs, User == users[i])
      cpt <- 0
      for(j in 0:23){
        subless <- subset(data, Hour == j)
        cntless<-count_if("Auto skipped", subless$Type)
        cnt<- nrow(subless)
          res <-cntless
          cpt <- cpt+1
          daylist$Score[j] <- daylist$Score[cpt]+res
          if(cnt!=0){
            daylist$nbUser[j] <- daylist$nbUser[cpt]+1
          }
      }
    
    }
    plot(x=daylist$date, y=daylist$Score/daylist$nbUser, xlim=c(0,23),ylim=c(0,15),
         col='black', type='l',
         main='Engagement following the number of hours of the day of testing', xlab='Hour', ylab='engagement')
  })
  output$totalCigSaved <-  renderText({
    total = as.integer(sum(tabByUser$savedCigarettes))
    totalString = toString(total)
    lastString = paste(totalString,"cigarettes saved ")
  })
  output$avgCigSaved <-  renderText({
    totalCig = as.integer(sum(tabByUser$savedCigarettes))
    totalUsers = nrow(tabByUser)
    totalString = toString(as.integer(totalCig/totalUsers))
    lastString = paste(totalString,"cigarettes saved per user")
  })
  output$totalMoneySaved <-  renderText({
    total = as.integer(sum(tabByUser$savedCigarettes)*cigPrice)
    totalString = toString(total)
    lastString = paste(totalString,"$ saved")
  })
  output$avgMoneySaved <-  renderText({
    totalMoney = as.integer(sum(tabByUser$savedCigarettes)*cigPrice)
    totalUsers = nrow(tabByUser)
    totalString = toString(as.integer(totalMoney/totalUsers))
    lastString = paste(totalString,"€ saved per user")
  })
} 

shinyApp(ui = ui, server = server)