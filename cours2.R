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
library(dplyr)
library(schoolmath)

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

UnaccentNames <- function(text){
  text <- gsub("[\x8e]","e",text)
  text <- gsub("[\x91]","e",text)
  text <- gsub("[\x8f]","e",text)
  
  text <- str_replace(text,"\x83","E")
  text <- str_replace(text,"\x91","a")
  text <- str_replace(text,"ƒ","E")
  text <- str_replace(text,"Ž","e")
  text <- str_replace(text,"Ž","e")
  text <- str_replace(text,"‘","e")
  text <- str_replace(text,"\u008f","e")
  
  text <- str_replace(text,"é","e")
  text <- str_replace(text,"è","e")
  text <- str_replace(text,"ë","e")
  text <- str_replace(text,"é","e")
  text <- str_replace(text,"É","E")
  return(text)
}
#replace with correct accents
datalogs$User = UnaccentNames(datalogs$User)

datalogs$Date <- strptime(as.character(datalogs$Time), "%d/%m/%Y")

datalogs$Time <- strptime(as.character(datalogs$Time), "%d/%m/%Y %H:%M")

datalogs$Hour <- hour(datalogs$Time)

datalogs$Day <- weekdays(as.Date(datalogs$Time))

behav <- datalogs
other <- behav[behav$Type == "Behaviour", c("User","Time")]
other <- other[!duplicated(other[,"User"]),]

behav <- merge(x=behav, y=other, by="User", all = TRUE)

behav$nbWeek <- time_length(interval(start = behav$Time.y, end = behav$Time.x), unit = "weeks") 
behav$nbWeek <- floor(behav$nbWeek)


behav <- plyr::rename(behav,c("Time.x"="Time"))
behav <- select(behav,"User","Time","nbWeek")

datalogs <- merge(x=datalogs,y=behav, by=c("User","Time"))

datalogsSmoked <- subset(datalogs, Type == "Behaviour" |Type == "On time" | Type == "Cheated")

tabByUser <- makeTabByUser()

cigPrice = 1

varUser<- ""

dataSurvey = read_excel("datasets/surveydataece.xlsx")
dataSurvey$Name <- UnaccentNames(dataSurvey$Name)

NameList = unique(c(unique(dataSurvey$Name),unique(datalogs$User)))

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
                    fluidRow(
                      box(plotOutput("countByTime")),
                      box(plotOutput("allUserCigConsumption"))
                    )
                ),
                tabPanel("Engagement", "",
                 fluidRow(
                   box(plotOutput("userEngagement"))
                 )
                 )
              )
      ),
      tabItem(tabName = "singleUser",
                    fluidRow(
                    box(selectInput("varUser",
                                   label = "Choose a user",
                                    choices = unique(NameList),
                                    selected = "Friend")
                    )),
                    fluidRow(
                      tabBox(
                          title = "single user",
                          height = "100%", width = "100%",
                          # The id lets us use input$tabset1 on the server to find the current tab
                          id = "tabset2",
                          tabPanel("information",
                                   fluidRow(
                                     valueBoxOutput("meanConsumedWeekdays"),
                                     valueBoxOutput("meanConsumedWeekenddays"),
                                     valueBoxOutput("singleUserOverallEngagement"),
                                     valueBoxOutput("singleUserTotalCigSavedRender"),
                                     valueBoxOutput("singleUserTotalMoneySavedRender"),
                                     valueBoxOutput("ageCategory"),
                                     valueBoxOutput("age"),
                                     valueBoxOutput("meanConsumed")
                                   )
                          ),
                          tabPanel("Classic",
                                  h3(varUser),
                                  fluidRow(
                                    box(plotOutput("countBy")),
                                    box(plotOutput("pieType"))
                                  ),
                                  fluidRow(
                                    h3("Progression"),
                                    p("The progression is a ratio computed according to intital frequence of smoking (behavior) and giving bonuses to manual skips and maluses to cheat"),
                                    box(selectInput("varProgPeriod", 
                                        label = "Choose a period type",
                                        choices = c("weeks","days"),
                                        selected = "weeks"),
                                    plotOutput("prog")
                                    ),
                                    box(
                                      h3("Cigarettes consumption in last seven days"),
                                      plotOutput("lastSeven")
                                    )
                                ),
                                fluidRow(
                                  h3("smoking localization"),
                                  box(plotOutput("userMap"))
                                )
                          ),
                          tabPanel("Week",
                                   fluidRow(
                                     box(
                                       h2("weeks comparison"),
                                       plotOutput("comparisonWeeks")
                                     ),
                                     box(
                                       selectInput("modes",
                                                   label = "Choose a mode",
                                                   choices = unique(datalogs$Type),
                                                   selected = "On Time"),
                                       plotOutput("modesPlot")
                                     )
                                   )
                          ),
                          tabPanel("Engagement",
                                   fluidRow(
                                     box(plotOutput("singleUserEngagement"))
                                   )
                          ),
                          tabPanel("All days",
                                   fluidRow(
                                     box(
                                       h2("Cigarettes consumption over all period"),
                                       plotOutput("daysCigarettesConsumption")
                                      
                                     ),
                                     box(
                                       selectInput("mode2",
                                                   label = "Choose a mode",
                                                   choices = unique(datalogs$Type),
                                                   selected = "On Time"),
                                       plotOutput("daysCigarettesConsumptionModes")
                                     )
                                   )
                          )
                      )
              )
     )
    )
  ))


server <- function(input, output) {
  
  singleUserTotalCigSaved<-function(){
    data <- subset(tabByUser, Name == input$varUser)$savedCigarettes
    totalString = toString(as.integer(data))
  }
  
  output$allUserCigConsumption <- renderPlot({
    cigConsumption <- datalogsSmoked[c("nbWeek","Day","Type")]
    cigConsumption <- data.frame(table(cigConsumption))
    cigConsumption <- cigConsumption[cigConsumption$Freq!=0,c("nbWeek","Day","Freq")]
    cigConsumption$Day <- factor(cigConsumption$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    cigConsumption <- cigConsumption[order(cigConsumption$Day), ]
    ggplot(cigConsumption, aes(x=Day, y=Freq, fill=Day)) + geom_boxplot() + ggtitle("Mean and std of cigarette consumption per weekday ")
  })
    
    
  output$lastSeven <- renderPlot({
    
    sub <- subset(datalogsSmoked, User == input$varUser)
    sub <- subset(sub, Date >= tail(sub,1)$Date - as.difftime(7, unit="days"))
    
    sub$Date<-as.POSIXct(sub$Date)
    
    smokedDays <- sub %>%
      select(Type, Date) %>%
      count(Date)
    barplot(smokedDays$n,names.arg = smokedDays$Date)
  })
  
  output$daysCigarettesConsumption <- renderPlot({
    
    sub <- subset(datalogsSmoked, User == input$varUser)
    
    sub$Date<-as.POSIXct(sub$Date)
    
    smokedDays <- sub %>%
      select(Type, Date) %>%
      count(Date)
    
    plot(smokedDays$Date,smokedDays$n, type = "l")
  })
  
  output$daysCigarettesConsumptionModes <- renderPlot({
    sub <- subset(datalogsSmoked, User == input$varUser)
    sub <- subset(sub,Type == input$mode2)
    
    sub$Date<-as.POSIXct(sub$Date)
    smokedDays <- sub %>%
      select(Type, Date) %>%
      count(Date)
    plot(smokedDays$Date,smokedDays$n, type = "l")
  })
  
  output$modesPlot <- renderPlot({
    
    sub <- subset(datalogsSmoked, User == input$varUser)
    sub <- subset(sub, Type == input$modes)
    sub$Week <- strftime(sub$Time, format = "%W")
    
    smokedWeeks <- sub %>%
      select(Type, Week) %>%
      count(Week)
    
    smokedWeeks$n <- smokedWeeks$n / 7
    barplot(smokedWeeks$n,names.arg = smokedWeeks$Week)
    
  })
  
  output$comparisonWeeks <- renderPlot({
    sub <- subset(datalogsSmoked, User == input$varUser)
    sub$Week <- strftime(sub$Time, format = "%W")
    smokedWeeks <- sub %>%
      select(Type, Week) %>%
      count(Week)
    smokedWeeks$n <- smokedWeeks$n/7
    barplot(smokedWeeks$n,names.arg = smokedWeeks$Week)
  })
  
  output$singleUserTotalCigSavedRender <- renderValueBox({
    totalString = singleUserTotalCigSaved()
    lastString = paste(totalString,"cigarettes saved ")
    valueBox(
      paste0(lastString),
      paste("Cigarettes saved")
    )
  })
  
  output$singleUserTotalMoneySavedRender <- renderValueBox({
    totalString = singleUserTotalCigSaved()
    lastString = paste(totalString,"$ saved ")
    valueBox(
      paste0(lastString),
      paste("Money saved")
    )
  })

  output$countByTime <- renderPlot({
    cigCompsuption <- datalogsSmoked[c("Day","Type","Hour")]
    timeslots <- c(0,2,4,6,8,10,12,14,16,18,20,22,24)
    days = c("Monday","Thursday","Wednesday","Tuesday","Friday","Saturday","Sunday")
    cigCompsuption$Hour <- cut(as.numeric(cigCompsuption$Hour), breaks = timeslots, right = FALSE)
    cigCompsuption <- data.frame(table(cigCompsuption))
    cigCompsuption <- aggregate(list(Freq=cigCompsuption$Freq),by = list(Hour=cigCompsuption$Hour,Day=cigCompsuption$Day), sum)
    cigCompsuption$Day <- factor(cigCompsuption$Day,labels = days)
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    ggplot(data = cigCompsuption, title = "test", aes( x = Day, y = Freq , fill=Hour))+geom_bar( stat = 'identity',position = 'dodge')+ scale_fill_manual(values=cbbPalette)+ ggtitle("Cigarettes per weekday per time slots")
  })
  
  output$meanConsumedWeekdays <- renderValueBox({
    sub1 <- subset(datalogsSmoked, Day %in% c("Monday","Thursday","Wednesday","Tuesday","Friday"))
    sub2 <- subset(sub1, User == input$varUser)
    nb = nrow(sub2)
    if(nb != 0){
      nbWeekDays = sum(!weekdays(seq(min(sub2$Date), max(sub2$Date), "days")) %in% c("Saturday", "Sunday"))
      avg = nb/nbWeekDays
      avg = lapply(avg, round, 2)
    }else{
      avg = "not enough data"
    }
    valueBox(
      paste0(avg),
      paste("Mean of consumed cigarettes in weekdays")
    )
  })
  
  output$meanConsumedWeekenddays <- renderValueBox({
    
    sub1 <- subset(datalogsSmoked, Day %in% c("Saturday","Sunday"))
    sub2 <- subset(sub1, User == input$varUser)
    nb = nrow(sub2)
    if(nb != 0){
      nbWeekendDays = sum(weekdays(seq(min(sub2$Date), max(sub2$Date), "days")) %in% c("Saturday", "Sunday"))
      avg = nb/nbWeekendDays
      avg = lapply(avg, round, 2)
    }else{
      avg = "not enough data"
    }
    valueBox(
      paste0(avg),
      paste("Mean of consumed cigarettes in week ends days")
    )
  })
  
  output$meanConsumed <- renderValueBox({
    sub2 <- subset(datalogsSmoked, User == input$varUser)
    nb = nrow(sub2)
    if(nb != 0){
      nbDays = length(seq(min(sub2$Date), max(sub2$Date), "days"))
      avg = nb/nbDays
      avg = lapply(avg, round, 2)
    }else{
      avg = "not enough data"
    }
    valueBox(
      paste0(avg),
      paste("Mean of consumed cigarettes")
    )
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
  
  userAge <- reactive(
    if( length(dataSurvey[dataSurvey$Name == input$varUser,"Age"][[1]]) >0){
      dataSurvey[dataSurvey$Name == input$varUser,"Age"][[1]]
    }else{
      "undefined"
    }
  )
  
  userAgeCategory <- reactive( if (userAge()<=30) "young" else if (userAge()<=50) "adult" else "old" )
  
  output$ageCategory <- renderValueBox({ 
    if(userAge() == "undefined"){
      val = "undefined"
    }else if (userAge()<=30){
      val = "young" 
    }else if (userAge()<=50){
      val = "adult" 
    }else{
      val = "old" 
    }
    valueBox(
      paste0(val),
      paste("Age category")
    )
  })
  
  output$age <- renderValueBox({ 
    val = userAge()
    valueBox(
      paste0(val),
      paste("Age")
    )
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
  
  output$cigConsumption <- renderPlot({
    
    data<-subset(datalogs, Type == input$varType)
    fdata <- factor(data$Day,labels = c("Monday","Thursday","Wednesday","Tuesday","Friday","Saturday","Sunday"))
    
    barplot(table(fdata),ylab="number of smoking occurences",main="Hour of the day", col=pickedColors)
    
    users<-unique(datalogs$User)
    daylist<-data.frame(date=c(1:200))
    daylist["Score"]<-15
    daylist["nbUser"]<-0
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

  output$userEngagement <- renderPlot({
    users<-unique(datalogs$User)
    daylist<-data.frame(date=c(1:200))
    daylist["Score"]<-15
    daylist["nbUser"]<-0
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
  
  OverallEngagement <- function(){
    data = subset(datalogs, User == input$varUser)
    if(nrow(data) > 0){
      datelist <- seq(as.Date(min(data$Date)), as.Date(max(data$Date)), by="days")
      lengthdate = length(seq(as.Date(min(data$Date)), as.Date(max(data$Date)), by="days"))
      if(lengthdate >7){
        engagementList<-data.frame(date=c(-6:lengthdate-7))
        engagementList["Engagement"]<-0
        engagementList["AutoSkip"]<-0
        engagementList["Smoked"]<-0
        for(j in 1:length(datelist)){
          subless <- subset(data, Date == datelist[j])
          cntAutoSkip = count_if("Auto skipped", subless$Type)
          if(cntAutoSkip != 0){
            smoked = count_if("Skipped", subless$Type) + count_if("Snoozed", subless$Type) + count_if("On time", subless$Type)
            engagement = 1 - (cntAutoSkip/(cntAutoSkip + smoked))
            engagementList$Engagement[j] = engagement
            engagementList$AutoSkip[j] = cntAutoSkip
            engagementList$Smoked[j] = smoked
          }else{
            engagementList$Engagement[j] = 0
            engagementList$AutoSkip[j] = 0
            engagementList$Smoked[j] = 0
          }
        }
        #engagementList
        Overall = sum(engagementList$Engagement)/lengthdate
      }else{
         Overall = -1
      }
    }else{
           Overall = -1
    }
  }
  
  output$singleUserOverallEngagement <- renderValueBox({
    val = OverallEngagement()
    if(val != -1){
      if(is.decimal(val)){
        val = lapply(val, round, 2)
      }
    }else{
      val = "not enough data"
    }
    valueBox(
      paste0(val),
      paste("Overall Engagement")
    )
  })
  
  
  output$singleUserEngagement <- renderPlot({
    data = subset(datalogs, User == input$varUser)
    datelist <- seq(as.Date(min(data$Date)), as.Date(max(data$Date)), by="days")
    lengthdate = length(seq(as.Date(min(data$Date)), as.Date(max(data$Date)), by="days"))
    if(lengthdate >7){
      engagementList<-data.frame(date=c(-6:lengthdate-7))
      engagementList["Engagement"]<-0
      engagementList["AutoSkip"]<-0
      engagementList["Smoked"]<-0
      for(j in 1:length(datelist)){
        subless <- subset(data, Date == datelist[j])
        cntAutoSkip = count_if("Auto skipped", subless$Type)
        if(cntAutoSkip != 0){
          smoked = count_if("Skipped", subless$Type) + count_if("Snoozed", subless$Type) + count_if("On time", subless$Type)
          engagement = 1 - (cntAutoSkip/(cntAutoSkip + smoked))
          engagementList$Engagement[j] = engagement
          engagementList$AutoSkip[j] = cntAutoSkip
          engagementList$Smoked[j] = smoked
        }else{
          engagementList$Engagement[j] = 0
          engagementList$AutoSkip[j] = 0
          engagementList$Smoked[j] = 0
        }
      }
      plot(x=engagementList$date, y=engagementList$Engagement,  xlim=c(0,lengthdate-7), ylim=c(0,1),
           col='black', type='l',
           main='Engagement following the number of days of testing', xlab='number of days', ylab='engagement per day')
    }else{
      plot(x=c(0:1), y=c(0:1),  xlim=c(0,lengthdate-7), ylim=c(0,1),
           col='black', type='l',
           main='Engagement following the number of days of testing (no data)', xlab='number of days', ylab='engagement per day')
    }
  })
  
  output$totalCigSaved <- renderText({
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
    totalString = toString(as.integer(total))
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