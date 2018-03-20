library(shiny)
library(shinydashboard)
library(readxl)

Toxicity<-read_xlsx("Toxicity_Tidy_Table.xlsx")
Year_2006 <- read.csv(file="Year_2006.csv", header=TRUE)
Year_2010 <- read.csv(file="Year_2010.csv", header=TRUE)
Year_2014 <- read.csv(file="Year_2014.csv", header=TRUE)
Year_2016 <- read.csv(file="Year_2016.csv", header=TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Chemical Evaluation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("MethodA", tabName = "M1", icon = icon("th")),
      menuItem("MethodB", tabName = "M2", icon = icon("th")),
      menuItem("MethodC", tabName = "M3", icon = icon("th")),
      menuItem("MethodD", tabName = "M4", icon = icon("th")),
      menuItem("MethodE", tabName = "M5", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "M1",
              fluidRow(
                tabBox(
                  title = "View by year",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("2006", "Applications, Measured In Lb", plotOutput("plot11")),
                  tabPanel("2010", "Applications, Measured In Lb", plotOutput("plot12")),
                  tabPanel("2014", "Applications, Measured In Lb", plotOutput("plot13")),
                  tabPanel("2016", "Applications, Measured In Lb", plotOutput("plot14"))
                ),
                box(title="View as a whole", "Applications, Measured In Lb", plotOutput("plot1"))
              )
      ),
      tabItem(tabName = "M2",
              fluidRow(
                tabBox(
                  title = "View by year",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("2006", "Applications, Measured In Lb", plotOutput("plot21")),
                  tabPanel("2010", "Applications, Measured In Lb", plotOutput("plot22")),
                  tabPanel("2014", "Applications, Measured In Lb", plotOutput("plot23")),
                  tabPanel("2016", "Applications, Measured In Lb", plotOutput("plot24"))
                ),
                box(title="View as a whole", "Applications, Measured In Lb/Acre/Application, Avg", plotOutput("plot2") )
              )
      ),
      tabItem(tabName = "M3",
              fluidRow(
                tabBox(
                  title = "View by year",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("2006", "Applications, Measured In Lb", plotOutput("plot31")),
                  tabPanel("2010", "Applications, Measured In Lb", plotOutput("plot32")),
                  tabPanel("2014", "Applications, Measured In Lb", plotOutput("plot33")),
                  tabPanel("2016", "Applications, Measured In Lb", plotOutput("plot34"))
                ),
                box(title="View as a whole", "Applications, Measured In Lb/Acre/Year, Avg:", plotOutput("plot3"))
              )
      ),
      tabItem(tabName = "M4",
              fluidRow(
                tabBox(
                  title = "View by year",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("2006", "Applications, Measured In Lb", plotOutput("plot41")),
                  tabPanel("2010", "Applications, Measured In Lb", plotOutput("plot42")),
                  tabPanel("2014", "Applications, Measured In Lb", plotOutput("plot43")),
                  tabPanel("2016", "Applications, Measured In Lb", plotOutput("plot44"))
                ),
                box(title="View as a whole", "Applications, Measured In Number, Avg", plotOutput("plot4"))
              )
      ),
      tabItem(tabName = "M5",
              fluidRow(
                tabBox(
                  title = "View by year",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "250px",
                  tabPanel("2006", "Applications, Measured In Lb", plotOutput("plot51")),
                  tabPanel("2010", "Applications, Measured In Lb", plotOutput("plot52")),
                  tabPanel("2014", "Applications, Measured In Lb", plotOutput("plot53")),
                  tabPanel("2016", "Applications, Measured In Lb", plotOutput("plot54"))
                ),
                box(title="View as a whole", "Treated, Measured in Pct of Area Planted, Avg", plotOutput("plot5"))
              )
      )
    )
  ))

server <- function(input, output) { 
  output$plot1 <- renderPlot({
    plot(Toxicity$LD50, Toxicity$A)
  })
  output$plot11 <- renderPlot({
    plot(Year_2006$LD50, Year_2006$A)
  })
  output$plot12 <- renderPlot({
    plot(Year_2010$LD50, Year_2010$A)
  })
  output$plot13 <- renderPlot({
    plot(Year_2014$LD50, Year_2014$A)
  })
  output$plot14 <- renderPlot({
    plot(Year_2016$LD50, Year_2016$A)
  })
  
  output$plot2 <- renderPlot({
    plot(Toxicity$LD50, Toxicity$B)
  })
  output$plot21 <- renderPlot({
    plot(Year_2006$LD50, Year_2006$B)
  })
  output$plot22 <- renderPlot({
    plot(Year_2010$LD50, Year_2010$B)
  })
  output$plot23 <- renderPlot({
    plot(Year_2014$LD50, Year_2014$B)
  })
  output$plot24 <- renderPlot({
    plot(Year_2016$LD50, Year_2016$B)
  })
  
  
  output$plot3 <- renderPlot({
    plot(Toxicity$LD50, Toxicity$C)
  })
  output$plot31 <- renderPlot({
    plot(Year_2006$LD50, Year_2006$C)
  })
  output$plot32 <- renderPlot({
    plot(Year_2010$LD50, Year_2010$C)
  })
  output$plot33 <- renderPlot({
    plot(Year_2014$LD50, Year_2014$C)
  })
  output$plot34 <- renderPlot({
    plot(Year_2016$LD50, Year_2016$C)
  })
  
  output$plot4 <- renderPlot({
    plot(Toxicity$LD50, Toxicity$D)
  })
  output$plot41 <- renderPlot({
    plot(Year_2006$LD50, Year_2006$D)
  })
  output$plot42 <- renderPlot({
    plot(Year_2010$LD50, Year_2010$D)
  })
  output$plot43 <- renderPlot({
    plot(Year_2014$LD50, Year_2014$D)
  })
  output$plot44 <- renderPlot({
    plot(Year_2016$LD50, Year_2016$D)
  })
  
  output$plot5 <- renderPlot({
    plot(Toxicity$LD50, Toxicity$E)
  })
  output$plot51 <- renderPlot({
    plot(Year_2006$LD50, Year_2006$E)
  })
  output$plot52 <- renderPlot({
    plot(Year_2010$LD50, Year_2010$E)
  })
  output$plot53 <- renderPlot({
    plot(Year_2014$LD50, Year_2014$E)
  })
  output$plot54 <- renderPlot({
    plot(Year_2016$LD50, Year_2016$E)
  })
}

shinyApp(ui, server)

