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

pickedColors = colors()[c(30,35,40,45,50,12,60)]
newmap <- getMap(resolution = "low")
dataCountries <- data.frame(Country=c('Russia','Cyprus', 'Belize', 'Austria' ,'Virgin Islands', 
                                      'Italy','United States' ,'United Kingdom', 'Germany', 'France' ,'Poland' ,'Switzerland'),
                                      Value=c(-0.310,-0.206,-0.300,-0.179,-0.196,-0.174,-0.105,-0.142,-0.082,-0.097,-0.027,0.052))
pdf1 <- joinCountryData2Map(dataCountries, joinCode="NAME", nameJoinColumn="Country")
# get the coordinates for each country
country_coord<-data.frame(coordinates(pdf1),stringsAsFactors=F)

datalogs<-read.csv("datasets/logs.csv",sep = ";")
#datausers<-read.xls("datasets/surveydataece.xlsx", perl = perl)

datalogs$Time <- strptime(as.character(datalogs$Time), "%d/%m/%Y")
#datausers$Started <- strptime(as.character(datausers$Started),format = "%d-%b")
#year(datausers$Started)<-2017
#daylist<-preprocessing(datalogs,datausers)

#don<-xts(x = daylist$Friend, order.by = daylist$date)
varUser<- ""
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
                  #dygraph(don) %>%
                   # dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
                    #dyRangeSelector() %>%
                    #dyCrosshair(direction = "vertical") %>%
                    #dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
                    #dyRoller(rollPeriod = 1)
                  
                  )
              )),
      tabItem(tabName = "singleUsers",
              h2("single users: "),
              h3(varUser),
              fluidRow(
                box(selectInput("varUser", 
                                label = "Choose a type",
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
  
  
} 

shinyApp(ui = ui, server = server)