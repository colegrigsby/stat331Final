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
                                                      "Average Age of Participants",
                                                      "Percent of State Population Participating")
                                            , selected="General Number of Participants")
                                )
                              
                              
             ),
    tabPanel("Tab2",
             
             fluidRow(h1("Examing Data by Education Level"),
               checkboxGroupInput("educationLevel", "Select levels:",
                                                                   c("High School Incomplete" = 1,
                                                                     "High School Diploma" = 2,
                                                                     "Some College" = 3,
                                                                     "Bachelor's/Associate's" = 4,
                                                                     "Masters and Above" = 5), selected = 1),
                  plotOutput("hoursWorkedPlot")),
                      
                      wellPanel(
                        sliderInput(inputId = "nlabels",
                                    label = "Choose color:",
                                    min = 1,
                                    max = 3,
                                    value = 2,
                                    step = 1)
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
    
    selected <- input$educationLevel
    
    user.data.temp <- user.data[user.data$PEHRACTT != -1 & user.data$PEEDUCA != -1,]   


    if (1 %in% selected == FALSE) {
      user.data.temp <- user.data.temp[user.data.temp$PEEDUCA > 38,]
    }
    if ("2" %in% selected == FALSE) {
      user.data.temp <- user.data.temp[user.data.temp$PEEDUCA != 39,]
    }
    if ("3" %in% selected == FALSE) {
      user.data.temp <- user.data.temp[user.data.temp$PEEDUCA != 40,]
    }

    if ("5" %in% selected == FALSE) {
      user.data.temp <- user.data.temp[user.data.temp$PEEDUCA < 44,]
    }

    

    hours.data <- aggregate(PRTAGE ~ state, user.data.temp, mean)
    counts$avgWorkedHours <- hours.data[-9,]$PRTAGE
    

    
    gg <- ggplot()
    gg <- gg + geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15)
    gg <- gg + geom_map(data=counts, map=us,
                        aes(fill=avgWorkedHours, map_id=region),
                        color="#ffffff", size=0.15)
    gg <- gg + scale_fill_continuous(low='darkseagreen1', high=colors[input$nlabels], 
                                     guide='colorbar')
    gg <- gg + labs(x=NULL, y=NULL)
    gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
    gg <- gg + theme(panel.border = element_blank(),panel.background = element_blank(),axis.ticks = element_blank(), axis.text = element_blank())
    gg
    
    
    
  })
  
}

shinyApp(ui=ui, server=server)