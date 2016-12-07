#install.packages('rsconnect')
#install.packages(c('ggplot2', 'shiny'))

library(rsconnect)
library(ggplot2)
library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("National Map", mainPanel(h1("Location Based Participant Data")), fluidRow(plotOutput("nationPlot")),
                              sidebarPanel(
                                h5("Just a few things to get a feel for where the data comes from"),
                                sliderInput("ageRange","Age",min(user.data$PRTAGE),max(user.data$PRTAGE),c(min(user.data$PRTAGE),max(user.data$PRTAGE))),
                                selectInput("mapData", 
                                            "Select what you would like to look at",
                                            choices=c("Percentage that hold a bachelors"
                                                      ,"General Number of Participants")
                                            , selected="General Number of Participants")
                                )
                              
                              
             ),
    tabPanel("Tab2",
             sidebarPanel("side panel"),
             mainPanel("main panel"),
             fluidRow(plotOutput("statePlot"),
                      
                      wellPanel(
                        sliderInput(inputId = "nlabels",
                                    label = "Top n States:",
                                    min = 1,
                                    max = 10,
                                    value = 6,
                                    step = 1)
                      )
             )
    ),
    tabPanel("Tab3",
             fluidRow(plotOutput("iStatePlot"),
                      wellPanel(
                        htmlOutput("selectState"))
             )
    )
  )
  
)

source("MappingStuff.R")

server <- function(input,output) {
  
  output$nationPlot <- renderPlot({
    
    #todo input$ageRange[1] = lower , input$ageRange[2]=upper 
    plotNation(input$mapData, input$ageRange)
  })
  
}

shinyApp(ui=ui, server=server)