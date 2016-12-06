#install.packages('rsconnect')
#install.packages(c('ggplot2', 'shiny'))

library(rsconnect)
library(ggplot2)
library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Tab1", fluidRow(plotOutput("nationPlot"))),
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



server <- function(input,output) {
  
  
}

shinyApp(ui=ui, server=server)