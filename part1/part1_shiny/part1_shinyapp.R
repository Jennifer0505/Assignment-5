#Shiney Dashboard
library(shinydashboard)
library(shiny)
library(ggplot2)

wholedata<-read.csv("wholedata.csv", header=TRUE)
whole_data_ATMP <- read.csv(file="whole_data_ATMP.csv", header=TRUE)
whole_data_WTMP <- read.csv(file="whole_data_WTMP.csv", header=TRUE)
wholedata_ATMP_Linear <- read.csv(file="wholedata_ATMP_Linear.csv", header=TRUE)
wholedata_WTMP_Linear <- read.csv(file="wholedata_WTMP_Linear.csv", header=TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Temperature Change"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series-Air Temperature", tabName = "ATMP", icon = icon("th")),
      menuItem("Time Series-Water Temperature", tabName = "WTMP", icon = icon("th")),
      menuItem("WTMP against ATMP", tabName = "WTMP-ATMP", icon = icon("th")),
      menuItem("Linear regression for WTMP", tabName = "lm-WTMP", icon = icon("th")),
      menuItem("Linear regression for ATMP", tabName = "lm-ATMP", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "ATMP",
              fluidRow(
                box(plotOutput("plot1"))
              )
      ),
      tabItem(tabName = "WTMP",
              fluidRow(
                box(plotOutput("plot2"))
              )
      ),
      tabItem(tabName = "WTMP-ATMP",
              fluidRow(
                box(plotOutput("plot3"))
              )
      ),
      tabItem(tabName = "lm-WTMP",
              fluidRow(
                box(plotOutput("plot4"))
              )
      ),
      tabItem(tabName = "lm-ATMP",
              fluidRow(
                box(plotOutput("plot5"))
              )
      )
    )
  ))


server <- function(input, output) { 
  output$plot1 <- renderPlot({
    ts1 <- as.vector(t(whole_data_ATMP[,-1]))
    ts2 <- ts(rev(ts1), start= c(1985,1), end=c(2017,12),frequency=12)
    ts3 <- ts.plot(ts2, gpars=list(xlab="year", ylab="Air_Temperature", lty=c(1:3)),col='purple')
  })
  output$plot2 <- renderPlot({
    ts4 <- as.vector(t(whole_data_WTMP[,-1]))
    ts5 <- ts(rev(ts4), start= c(1985,1), end=c(2017,12),frequency=12)
    ts6 <- ts.plot(ts5, gpars=list(xlab="year", ylab="Water_Temperature", lty=c(1:3)),col='blue')
  })
  output$plot3 <- renderPlot({
    plot(wholedata$ATMP, wholedata$WTMP)
    abline(lm(wholedata$ATMP ~ wholedata$WTMP))
  })
  output$plot4 <- renderPlot({
    plot(WTMP ~ YYYY, data=wholedata_WTMP_Linear)
    abline(lm(wholedata_WTMP_Linear$WTMP~wholedata_WTMP_Linear$YYYY))
  })
  output$plot5 <- renderPlot({
    plot(ATMP~YYYY, data=wholedata_ATMP_Linear)
    abline(lm(wholedata_ATMP_Linear$ATMP~wholedata_ATMP_Linear$YYYY))
  })
}


shinyApp(ui, server)

