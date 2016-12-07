#install.packages('rsconnect')
#install.packages(c('ggplot2', 'shiny'))

library(rsconnect)
library(ggplot2)
library(shiny)
#source("ReadingData.R")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("National Map", mainPanel(h1("Location Based Participant Data")), fluidRow(plotOutput("nationPlot")),
                              sidebarPanel(
                                h5("Just a few things to get a feel for where the data comes from"),
                                sliderInput("ageRange","Age",min(user.data$PRTAGE),max(user.data$PRTAGE),c(min(user.data$PRTAGE),max(user.data$PRTAGE))),
                                selectInput("mapData", 
                                            "Select what you would like to look at",
                                            choices=c("Percentage that hold a bachelors"
                                                      ,"General Number of Participants",
                                                      "Average Age of Participants")
                                            , selected="General Number of Participants")
                                )
                              
                              
             ),
    tabPanel("Tab2",
             sidebarPanel("side panel"),
             mainPanel("main panel"),
             fluidRow(plotOutput("hoursWorkedPlot"),
                      
                      wellPanel(
                        sliderInput(inputId = "nlabels",
                                    label = "Choose color:",
                                    min = 1,
                                    max = 3,
                                    value = 2,
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
  colors <- c("darkblue", "darkgreen", "blueviolet")
  output$nationPlot <- renderPlot({
    
    #todo input$ageRange[1] = lower , input$ageRange[2]=upper 
    plotNation(input$mapData, input$ageRange)
  })
  
  output$hoursWorkedPlot <- renderPlot({
    gg <- ggplot()
    gg <- gg + geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15)
    ### FILL HERE WITH WHAT WE want to see 
    gg <- gg + geom_map(data=counts, map=us,
                        aes(fill=avgWorkedHours, map_id=region),
                        color="#ffffff", size=0.15)
    gg <- gg + scale_fill_continuous(low='darkolivegreen1', high=colors[input$nlabels], 
                                     guide='colorbar')
    gg <- gg + labs(x=NULL, y=NULL)
    gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
    gg <- gg + theme(panel.border = element_blank(),panel.background = element_blank(),axis.ticks = element_blank(), axis.text = element_blank())
    gg
    
    
    
  })
  
}

shinyApp(ui=ui, server=server)